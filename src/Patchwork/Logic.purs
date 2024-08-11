module Patchwork.Logic where

import Patchwork.Interaction
import Patchwork.Model
import Prelude

import Control.Monad.State (StateT, gets)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Lens (view, (%=), (.=))
import Data.Lens.Record (prop)
import Data.List (List)
import Data.List as List
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe')
import Data.Set as Set
import Data.Three as Three
import Data.TotalMap (at')
import Data.TotalMap as TotalMap
import Data.Traversable (sequence)
import Data.Tuple (fst)
import Data.Tuple.Nested ((/\))
import Data.Unfoldable (replicate)
import Effect.Aff.Class (class MonadAff)
import Effect.Class.Console as Console
import Partial.Unsafe (unsafeCrashWith)
import Patchwork.Util (todo, (∘))
import Type.Prelude (Proxy(..))

--------------------------------------------------------------------------------
-- M
--------------------------------------------------------------------------------

type M m = InteractionT (StateT Model m)

_model = Proxy :: Proxy "model"

--------------------------------------------------------------------------------
-- Main
--------------------------------------------------------------------------------

main :: forall m. MonadAff m => Unit -> M m Unit
main _ = do
  Console.log "[main]"
  -- check if there is a winner
  gets (view (_Model ∘ prop _winner)) >>= case _ of
    Nothing -> do
      Console.log "[main] no winner"
      -- no winner, so active player takes action
      inject (ChooseTurnAction { k: pure }) >>= case _ of
        { selection: Buy } -> buy
        { selection: Wait } -> wait
        { selection: Pass } -> pure unit
      updateActivePlayer
      main unit
    Just _playerId -> do
      Console.log "[main] yes winner"
      -- yes winner, so game is over
      todo "how to indicate winner?"
  winner <- gets (view (_Model ∘ prop _winner))
  Console.log ("[main] winer = " <> show winner)

-- | Set model.activePlayer to be the player with the most time.
updateActivePlayer
  :: forall m. MonadAff m => M m Unit
updateActivePlayer = do
  Console.log "[updateActivePlayer]"
  playersTimes <- TotalMap.fromFunctionM \playerId -> do
    player <- gets (view (_Model ∘ prop _players ∘ at' playerId ∘ _Player))
    pure player.time
  let
    activePlayer =
      playersTimes
        # TotalMap.toUnfoldable
        # Array.sortBy (\(_ /\ time1) (_ /\ time2) -> compare time1 time2)
        # Array.last
        # maybe' (\_ -> unsafeCrashWith "impossible: 0 players") identity
        # fst
  _Model ∘ prop _activePlayer .= activePlayer

--------------------------------------------------------------------------------
-- Functions
--------------------------------------------------------------------------------

buy
  :: forall m. MonadAff m => M m Unit
buy = do
  Console.log "[buy]"
  { selection } <- inject (ChoosePatchFromCircle { k: pure })
  circle <- gets (view (_Model ∘ prop _circle))
  let
    newCircle /\ patchId =
      circle
        # flip (List.foldr identity) (replicate (selection # Three.toInt) shiftCircle :: List _)
        # removeCircleFocus
  _Model ∘ prop _circle .= newCircle
  Patch patch <- getPatch patchId
  spendButtons patch.buttonPrice
  spendDuration patch.durationPrice
  placePatch patchId
  updateWinner

wait
  :: forall m. MonadAff m => M m Unit
wait = do
  Console.log "[wait]"
  target <- gets (view (_Model ∘ prop _activePlayer))
  { duration } <- inject (ChooseWaitDuration { k: pure })
  _Model ∘ prop _players ∘ at' target ∘ _Player ∘ prop _time %= (_ + duration)

-- | Run interaction where player places patch on their quilt.
placePatch
  :: forall m. MonadAff m => PatchId -> M m Unit
placePatch patchId = do
  Console.log "[placePatch]"
  playerId <- gets (view (_Model ∘ prop _activePlayer))
  Patch patch <- getPatch patchId
  { pos, ori } <- inject (PlacePatch { patchId, k: pure })
  let quiltLayout = patch.quiltLayout # adjustQuiltLayout pos ori
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

getPatch
  :: forall m. MonadAff m => PatchId -> M m Patch
getPatch patchId = do
  patches <- gets (view (_Model ∘ prop _patches))
  patches
    # Map.lookup patchId
    # maybe' (\_ -> unsafeCrashWith $ "invalid PatchId; patchId = " <> show patchId <> "; patches.keys = " <> show (patches # Map.keys)) pure

updateWinner
  :: forall m. MonadAff m => M m Unit
updateWinner = do
  players <- TotalMap.fromFunctionM \playerId -> do
    player <- gets (view (_Model ∘ prop _players ∘ at' playerId ∘ _Player))
    if player.time == 0 then
      pure (Right player.buttons)
    else
      pure (Left unit)
  case players # sequence of
    Left _ -> pure unit
    Right results ->
      let
        winningPlayerId /\ _ = TotalMap.toUnfoldable results
          # Array.sortBy (\(_ /\ buttons1) (_ /\ buttons2) -> compare buttons1 buttons2)
          # Array.head
          # maybe' (\_ -> unsafeCrashWith "impossible: 0 players") identity
      in
        _Model ∘ prop _winner .= Just winningPlayerId

