module Patchwork.Logic where

import Prelude

import Control.Monad.Reader (ReaderT, asks)
import Control.Monad.State (StateT, gets)
import Data.Foldable (all, and, minimumBy)
import Data.Lens (_2, to, view, (%=), (.=), (^.))
import Data.Map as Map
import Data.Maybe (maybe, maybe')
import Data.Set as Set
import Data.TotalMap as TotalMap
import Data.Tuple (fst)
import Data.Tuple.Nested ((/\))
import Effect.Aff.Class (class MonadAff)
import Patchwork.Interaction (InteractionT, choosePatchFromMarketUnsafe, chooseTurnActionUnsafe, chooseWaitTimeUnsafe, placePatchUnsafe, printGameMessage)
import Patchwork.Model (Config, GameMessage(..), GameResult(..), Model, Patch, PatchId, PatchLayout, TurnAction(..), _Config, _Model, _Patch, _Player, _activePlayerId, _buttonPrice, _buttons, _calcWaitResult, _circle, _patchLayout, _players, _previousTurn, _quilt, _time, _timePrice, activePlayer, adjustPatchLayout, canAfford, extractPatchFromCircle, fromPatchLayoutToQuilt, getPatch, getPlayerScore, isOnQuilt)
import Patchwork.Util (bug, bug', fromSingletonList, minimumsBy)

--------------------------------------------------------------------------------
-- T
--------------------------------------------------------------------------------

type T m = InteractionT (StateT Model (ReaderT Config m))

type M a = forall m. MonadAff m => T m a

--------------------------------------------------------------------------------
-- main
--------------------------------------------------------------------------------

main :: M GameResult
main = do
  loop unit

loop :: Unit -> M GameResult
loop _ = do
  do -- beginning phase
    players :: Array _ <- gets (view (_Model <<< _players <<< to TotalMap.toUnfoldable))
    let
      activePlayer = players
        # minimumBy
            ( \(_ /\ p1) (_ /\ p2) ->
                -- lower time, with tie broken by higher previousTurn
                compare
                  ((p1 ^. _Player <<< _time) /\ (p2 ^. _Player <<< _previousTurn))
                  ((p2 ^. _Player <<< _time) /\ (p1 ^. _Player <<< _previousTurn))
            )
        # maybe' (bug' "there must be at least 1 player") fst
    _Model <<< _activePlayerId .= activePlayer
  do -- main phase
    chooseTurnAction >>= case _ of
      Wait -> do
        waitTime <- chooseWaitTime
        calcWaitResult <- asks (view (_Config <<< _calcWaitResult))
        let { rewardButtons } = calcWaitResult { waitTime }
        modifyButtons (_ + rewardButtons)
        modifyTime (_ + waitTime)
      Buy -> do
        chosenPatchId <- choosePatchFromMarket
        chosenPatch <- gets (getPatch chosenPatchId)
        modifyButtons (_ - (chosenPatch ^. _Patch <<< _buttonPrice))
        modifyTime (_ - (chosenPatch ^. _Patch <<< _timePrice))
        placePatch chosenPatchId chosenPatch
  do -- ending phase
    players :: Array _ <- gets (view (_Model <<< _players <<< to TotalMap.toUnfoldable))
    if players # all (view (_2 <<< _Player <<< _time <<< to (_ == 0))) then do
      let
        mb_winnerId = players
          # minimumsBy (\(_ /\ p) (_ /\ p') -> compare (p # getPlayerScore) (p' # getPlayerScore))
          # fromSingletonList
          # map fst
      pure $ mb_winnerId # maybe Tie Win
    else
      loop unit

--------------------------------------------------------------------------------
-- basic actions
--------------------------------------------------------------------------------

modifyButtons :: (Int -> Int) -> M Unit
modifyButtons f = do
  buttons <- gets $ view $ activePlayer <<< _Player <<< _buttons <<< to f
  if buttons < 0 then bug "tried to set buttons to negative" else pure unit
  activePlayer <<< _Player <<< _buttons .= buttons

-- | applies effect of passing certain time thresholds first
-- | if modification results in negative time, set to 0 time instead
modifyTime :: (Int -> Int) -> M Unit
modifyTime f = do
  time <- gets $ view $ activePlayer <<< _Player <<< _time
  let time' = time # f >>> max 0
  -- TODO: apply effects of passing certain time thresholds first
  activePlayer <<< _Player <<< _time .= time'

--------------------------------------------------------------------------------
-- external actions
--------------------------------------------------------------------------------

chooseTurnAction :: M TurnAction
chooseTurnAction = do
  { turnAction } <- chooseTurnActionUnsafe unit
  -- TODO: make sure the player has a possible move
  pure turnAction

-- | - player picks patch from market
-- | - patch is removed from market
-- | - move the market token to where the patch was
-- | - the chosen patch's PatchId is returned
choosePatchFromMarket :: M PatchId
choosePatchFromMarket = do
  { patchIndex } <- choosePatchFromMarketUnsafe unit
  if not (patchIndex < 3) then do
    printGameMessage { gameMessage: WarningGameMessage "you can't choose that patch" }
    choosePatchFromMarket
  else do
    circle <- gets (view (_Model <<< _circle))
    let circle' /\ patchId = circle # extractPatchFromCircle patchIndex
    patch <- gets (getPatch patchId)
    player <- gets (view activePlayer)
    if not (player `canAfford` patch) then do
      printGameMessage { gameMessage: WarningGameMessage "you can't afford that patch" }
      choosePatchFromMarket
    else do
      _Model <<< _circle .= circle'
      pure patchId

chooseWaitTime :: M Int
chooseWaitTime = do
  { time } <- chooseWaitTimeUnsafe unit
  if not (0 < time) then do
    printGameMessage { gameMessage: WarningGameMessage "you must wait for a positive amount of time" }
    chooseWaitTime
  else
    pure time

placePatch :: PatchId -> Patch -> M Unit
placePatch patchId patch = do
  { face, orientation, position } <- placePatchUnsafe { patch }
  let layout = (patch ^. _Patch <<< _patchLayout) # adjustPatchLayout position orientation face
  isPlaceablePatchLayout layout >>= case _ of
    false -> do
      printGameMessage { gameMessage: WarningGameMessage "you can't place the patch there" }
      placePatch patchId patch
    true -> do
      activePlayer <<< _Player <<< _quilt %= Map.union (layout # fromPatchLayoutToQuilt patchId)

isPlaceablePatchLayout :: PatchLayout -> M Boolean
isPlaceablePatchLayout patchLayout = do
  quilt <- gets $ view $ activePlayer <<< _Player <<< _quilt
  let overlapsWithPatchLayout p = patchLayout # Set.filter (fst >>> (_ == p)) # not <<< Set.isEmpty
  let entirelyOnQuilt layout = layout # Set.filter (fst >>> isOnQuilt) # not <<< Set.isEmpty
  pure $ and
    [ patchLayout # entirelyOnQuilt
    , quilt # Map.keys # Set.filter overlapsWithPatchLayout # Set.isEmpty
    ]

