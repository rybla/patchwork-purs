module Patchwork.LogicOld where

import Patchwork.Interaction
import Patchwork.Model
import Prelude

import Control.Monad.State (StateT, get, gets)
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Lens (view, (%=), (.=))
import Data.Lens.Record (prop)
import Data.List (List)
import Data.List as List
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe', maybe')
import Data.Set as Set
import Data.Three as Three
import Data.TotalMap (at')
import Data.TotalMap as TotalMap
import Data.Traversable (sequence)
import Data.Tuple (fst, snd)
import Data.Tuple.Nested ((/\))
import Data.Unfoldable (replicate)
import Effect.Aff.Class (class MonadAff)
import Effect.Class.Console as Console
import Partial.Unsafe (unsafeCrashWith)
import Patchwork.Util (bug, todo, (∘))
import Type.Prelude (Proxy(..))

--------------------------------------------------------------------------------
-- M
--------------------------------------------------------------------------------

type M m = InteractionT (StateT Model m)

--------------------------------------------------------------------------------
-- Main
--------------------------------------------------------------------------------

main :: forall m. MonadAff m => Unit -> M m Unit
main _ = do
  Console.log "[main]"
  _Model ∘ prop _turn %= (_ + 1)
  updateActivePlayer >>= case _ of
    false -> do
      -- no next active player, since game is over, and there is a winner
      winner <- gets (view (_Model ∘ prop _winner))
      Console.log ("[main] winer = " <> show winner)
    true -> do
      turn <- gets (view (_Model ∘ prop _turn))
      inject (ChooseTurnAction { k: pure }) >>= case _ of
        { selection: Buy } -> buy
        { selection: Wait } -> wait
      activePlayer ∘ _Player ∘ prop _lastTurnPlayed .= turn
      main unit

-- -- check if there is a winner
-- gets (view (_Model ∘ prop _winner)) >>= case _ of
--   Nothing -> do
--     Console.log "[main] no winner"

--   Just _playerId -> do
--     Console.log "[main] yes winner"
--     -- yes winner, so game is over
--     todo "how to indicate winner?"
-- winner <- gets (view (_Model ∘ prop _winner))
-- Console.log ("[main] winer = " <> show winner)

-- | Set model.activePlayer to be the player with the most time, tie broken by
-- | last turn played, and return true. But, if that player cannot play, then
-- | activePlayer to be the other player and return true. If neither player
-- | cannot play, then game is over, and return false.
updateActivePlayer
  :: forall m. MonadAff m => M m Boolean
updateActivePlayer = do
  Console.log "[updateActivePlayer]"
  playerRanks <- TotalMap.fromFunctionM \playerId -> do
    player <- gets (view (_Model ∘ prop _players ∘ at' playerId ∘ _Player))
    pure (player.time /\ player.lastTurnPlayed)
  let
    activePlayer =
      playerRanks
        # TotalMap.toUnfoldable
        # Array.sortBy (\(_ /\ time1) (_ /\ time2) -> compare time1 time2)
        # Array.last
        # maybe' (\_ -> unsafeCrashWith "impossible: 0 players") identity
        # fst
  _Model ∘ prop _activePlayer .= activePlayer
  -- TODO: 
  --  - check if this player can actually play
  --  - if not, then try other player
  --  - if not, then game over: updateWinner, return false
  pure true

--------------------------------------------------------------------------------
-- Functions
--------------------------------------------------------------------------------

buy
  :: forall m. MonadAff m => M m Unit
buy = do
  -- TODO: check for any timemarks passed when spending time
  Console.log "[buy]"
  { selection } <- inject (ChoosePatchFromCircle { k: pure })
  circle <- gets (view (_Model ∘ prop _circle))
  let
    newCircle /\ patchId =
      circle
        # flip (List.foldr identity) (replicate (selection # Three.toInt) shiftCircle :: List _)
        # removeCircleFocus
        # lmap (flip (List.foldr identity) (replicate (3 - (selection # Three.toInt)) shiftCircle :: List _))
  _Model ∘ prop _circle .= newCircle
  Patch patch <- gets (getPatch patchId)
  spendButtons patch.buttonPrice
  spendDuration patch.durationPrice
  placePatch patchId

wait
  :: forall m. MonadAff m => M m Unit
wait = do
  -- TODO: check for any timemarks passed when waiting
  Console.log "[wait]"
  target <- gets (view (_Model ∘ prop _activePlayer))
  { duration } <- inject (ChooseWaitDuration { k: pure })
  spendDuration duration
  _Model ∘ prop _players ∘ at' target ∘ _Player ∘ prop _buttons %= (_ + (duration + 1))

-- | Run interaction where player places patch on their quilt.
placePatch
  :: forall m. MonadAff m => PatchId -> M m Unit
placePatch patchId = do
  Console.log "[placePatch]"
  playerId <- gets (view (_Model ∘ prop _activePlayer))
  Patch patch <- gets (getPatch patchId)
  { pos, ori, face } <- inject (PlacePatch { patchId, k: pure })
  let quiltLayout = patch.quiltLayout # adjustQuiltLayout pos ori face
  _Model ∘ prop _players ∘ at' playerId ∘ _Player ∘ prop _quilt %=
    Map.union (quiltLayout # Set.map (\(pos' /\ btn) -> (pos' /\ patchId /\ btn)) # Map.fromFoldable)

spendButtons
  :: forall m. MonadAff m => Int -> M m Unit
spendButtons buttons = do
  playerId <- gets (view (_Model ∘ prop _activePlayer))
  _Model ∘ prop _players ∘ at' playerId ∘ _Player ∘ prop _buttons %= (_ - buttons)

spendDuration
  :: forall m. MonadAff m => Int -> M m Unit
spendDuration duration = do
  playerId <- gets (view (_Model ∘ prop _activePlayer))
  _Model ∘ prop _players ∘ at' playerId ∘ _Player ∘ prop _time %= (_ - duration)
  -- special actions when certain timemarks passed
  -- collect leathers
  -- earn income
  todo ""

inRange :: Int -> { min :: Int, max :: Int } -> Boolean
inRange x { min, max } = min <= x && x < max

updateWinner
  :: forall m. MonadAff m => M m Unit
updateWinner = do
  model <- get
  let
    scores = TotalMap.fromFunction \playerId ->
      playerId # playerScore model
  let
    winnerId = scores # TotalMap.toUnfoldable # Array.sortBy (\(_ /\ score1) (_ /\ score2) -> score1 `compare` score2)
      # Array.last
      # fromMaybe' (\_ -> bug "impossible: 0 players")
      # fst
  _Model ∘ prop _winner .= Just winnerId
