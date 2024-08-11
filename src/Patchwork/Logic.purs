module Patchwork.Logic where

import Patchwork.Interaction
import Patchwork.Model
import Prelude

import Control.Monad.State (StateT, gets)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Lens (view, (%=), (.=))
import Data.Lens.At (at)
import Data.Lens.Record (prop)
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe, maybe')
import Data.TotalMap (at')
import Data.TotalMap as TotalMap
import Data.Traversable (sequence)
import Data.Tuple.Nested ((/\))
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

main :: forall m. MonadAff m => M m Unit
main = do
  -- check if there is a winner
  gets (view (_Model ∘ prop _winner)) >>= case _ of
    Nothing -> do
      -- no winner, so active player takes action
      inject (ChooseTurnAction { k: pure }) >>= case _ of
        { selection: Buy } -> buy
        { selection: Wait } -> wait
        { selection: Pass } -> pure unit
      _Model ∘ prop _activePlayer %= nextPlayerId
      main
    Just _playerId -> do
      -- yes winner, so game is over
      todo "how to indicate winner?"

--------------------------------------------------------------------------------
-- Functions
--------------------------------------------------------------------------------

buy
  :: forall m. MonadAff m => M m Unit
buy = do
  Console.log "[buy]"
  patchId <- inject (ChoosePatchFromCircle { k: pure })
  Patch patch <- removePatchFromPatchCircle (todo "")
  target <- gets (view (_Model ∘ prop _activePlayer))
  spendButtons target patch.buttonPrice
  spendDuration target patch.durationPrice
  placePatch target (Patch patch)
  updateWinner
  pure unit

wait
  :: forall m. MonadAff m => M m Unit
wait = do
  Console.log "[wait]"
  target <- gets (view (_Model ∘ prop _activePlayer))
  { duration } <- inject (ChooseWaitDuration { k: pure })
  _Model ∘ prop _players ∘ at' target ∘ _Player ∘ prop _time %= (_ + duration)

-- | Run interaction where player places patch on their quilt.
placePatch
  :: forall m. MonadAff m => PlayerId -> Patch -> M m Unit
placePatch target patch = do
  Console.log "[placePatch]"
  quilt <- gets (view (_Model ∘ prop _players ∘ at' target ∘ _Player ∘ prop _quilt))
  { quilt' } <- inject (PlacePatch { patch, quilt, k: pure })
  _Model ∘ prop _players ∘ at' target ∘ _Player ∘ prop _quilt .= quilt'

removePatchFromPatchCircle
  :: forall m. MonadAff m => CirclePos -> M m Patch
removePatchFromPatchCircle i = do
  Console.log "[removePatchFromPatchCircle]"
  patch <-
    gets (view (_Model ∘ prop _patchCircle ∘ at i))
      >>= maybe (pure (unsafeCrashWith "invalid CirclePos")) pure
      >>= getPatch
  _Model ∘ prop _patchCircle %=
    Map.delete i
  pure patch

spendButtons
  :: forall m. MonadAff m => PlayerId -> Int -> M m Unit
spendButtons playerId buttons = do
  _Model ∘ prop _players ∘ at' playerId ∘ _Player ∘ prop _buttons %= (_ - buttons)

spendDuration
  :: forall m. MonadAff m => PlayerId -> Int -> M m Unit
spendDuration playerId duration = do
  _Model ∘ prop _players ∘ at' playerId ∘ _Player ∘ prop _time %= (_ - duration)

getPatch
  :: forall m. MonadAff m => PatchId -> M m Patch
getPatch patchId = do
  patches <- gets (view (_Model ∘ prop _patches))
  patches
    # Map.lookup patchId
    # maybe' (unsafeCrashWith $ "invalid PatchId: " <> show patchId) pure

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

