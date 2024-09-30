module Patchwork.Logic where

import Prelude

import Control.Monad.Reader (ReaderT, asks)
import Control.Monad.State (StateT, gets)
import Data.Foldable (all, minimumBy)
import Data.Lens (_2, to, view, (.=), (^.))
import Data.Maybe (maybe, maybe')
import Data.TotalMap as TotalMap
import Data.Tuple (fst)
import Data.Tuple.Nested ((/\))
import Effect.Aff.Class (class MonadAff)
import Patchwork.Interaction (InteractionT, choosePatchFromMarketUnsafe, chooseTurnActionUnsafe, chooseWaitTimeUnsafe, placePatchUnsafe, printGameMessage, setGameResult)
import Patchwork.Model (Config, GameMessage(..), GameResult(..), Model, PatchId, TurnAction(..), _Config, _Model, _Patch, _Player, _activePlayerId, _buttonPrice, _calcWaitResult, _circle, _players, _previousTurn, _time, _timePrice, activePlayer, extractPatchFromCircle, getPatch, getPlayerScore)
import Patchwork.Util (bug', fromSingletonList, minimumsBy, todo)

--------------------------------------------------------------------------------
-- T
--------------------------------------------------------------------------------

type T m = InteractionT (StateT Model (ReaderT Config m))

type M a = forall m. MonadAff m => T m a

--------------------------------------------------------------------------------
-- main
--------------------------------------------------------------------------------

main :: Unit -> M Unit
main _ = do
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
        placePatch chosenPatchId
  do -- ending phase
    players :: Array _ <- gets (view (_Model <<< _players <<< to TotalMap.toUnfoldable))
    if players # all (view (_2 <<< _Player <<< _time <<< to (_ == 0))) then do
      let
        mb_winnerId = players
          # minimumsBy (\(_ /\ p) (_ /\ p') -> compare (p # getPlayerScore) (p' # getPlayerScore))
          # fromSingletonList
          # map fst
      setGameResult { gameResult: mb_winnerId # maybe Tie Win }
    else
      main unit

--------------------------------------------------------------------------------
-- basic actions
--------------------------------------------------------------------------------

modifyButtons :: (Int -> Int) -> M Unit
modifyButtons = todo "" {}

-- | applies effect of passing certain time spaces
-- | if modification results in negative time, set to 0 time instead
modifyTime :: (Int -> Int) -> M Unit
modifyTime = todo "" {}

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
    -- ensure that active player can buy patch
    if false then do
      printGameMessage { gameMessage: WarningGameMessage "you can't afford that patch" }
      choosePatchFromMarket
    else do
      _Model <<< _circle .= circle'
      pure patchId

chooseWaitTime :: M Int
chooseWaitTime = do
  { time } <- chooseWaitTimeUnsafe unit
  pure time

placePatch :: PatchId -> M Unit
placePatch patchId = do
  { face, orientation, position } <- placePatchUnsafe { patchId }
  -- check is a valid placement
  -- update player's board
  pure unit
