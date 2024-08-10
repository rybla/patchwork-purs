module Patchwork.Logic where

import Patchwork.Interaction
import Patchwork.Model
import Prelude

import Control.Monad.Reader (ReaderT)
import Control.Monad.State (StateT, gets)
import Data.Lens (view, (%=), (.=))
import Data.Lens.At (at)
import Data.Lens.Record (prop)
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.TotalMap (at')
import Partial.Unsafe (unsafeCrashWith)
import Patchwork.Util (todo, (∘))
import Type.Prelude (Proxy(..))

--------------------------------------------------------------------------------
-- M
--------------------------------------------------------------------------------

type M m = InteractionT (ReaderT Ctx (StateT Env m))

type Ctx = {}

type Env =
  { model :: Model
  }

_model = Proxy :: Proxy "model"

--------------------------------------------------------------------------------
-- Main
--------------------------------------------------------------------------------

main :: forall m. Monad m => M m Unit
main = do
  -- check if there is a winner
  gets (view (prop _model ∘ _Model ∘ prop _winner)) >>= case _ of
    Nothing -> do
      -- no winner, so active player takes action
      target <- gets (view (prop _model ∘ _Model ∘ prop _activePlayer))
      inject (ChooseTurnAction { target, k: pure }) >>= case _ of
        { selection: Buy } -> buy
        { selection: Wait } -> wait
        { selection: Pass } -> pure unit
      prop _model ∘ _Model ∘ prop _activePlayer %= nextPlayerId
      main
    Just _playerId -> do
      -- yes winner, so game is over
      todo "how to indicate winner?"

--------------------------------------------------------------------------------
-- Functions
--------------------------------------------------------------------------------

buy
  :: forall m. Monad m => M m Unit
buy = do
  Patch patch <- removePatchFromPatchCircle (todo "")
  spendButtons patch.buttonPrice
  spendDuration patch.durationPrice
  placePatch (Patch patch)
  updateWinner
  pure unit

wait
  :: forall m. Monad m => M m Unit
wait = do
  target <- gets (view (prop _model ∘ _Model ∘ prop _activePlayer))
  time <- gets (view (prop _model ∘ _Model ∘ prop _players ∘ at' target ∘ _Player ∘ prop _time))
  maxTime <- gets (view (prop _model ∘ _Model ∘ prop _maxTime))
  { duration } <- inject (ChooseWaitDuration { target, time, maxTime, k: pure })
  prop _model ∘ _Model ∘ prop _players ∘ at' target ∘ _Player ∘ prop _time %= (_ + duration)

-- | Run interaction where player places patch on their quilt.
placePatch
  :: forall m. Monad m => Patch -> M m Unit
placePatch patch = do
  target <- gets (view (prop _model ∘ _Model ∘ prop _activePlayer))
  quilt <- gets (view (prop _model ∘ _Model ∘ prop _players ∘ at' target ∘ _Player ∘ prop _quilt))
  { quilt' } <- inject (PlacePatch { target, patch, quilt, k: pure })
  prop _model ∘ _Model ∘ prop _players ∘ at' target ∘ _Player ∘ prop _quilt .= quilt'

removePatchFromPatchCircle
  :: forall m. Monad m => CirclePos -> M m Patch
removePatchFromPatchCircle i = do
  patch <-
    gets (view (prop _model ∘ _Model ∘ prop _patchCircle ∘ at i))
      >>= maybe (pure (unsafeCrashWith "invalid CirclePos")) pure
      >>= getPatch
  prop _model ∘ _Model ∘ prop _patchCircle %=
    Map.delete i
  pure patch

spendButtons
  :: forall m. Monad m => Int -> M m Unit
spendButtons _ = pure (todo "spendButtons")

spendDuration
  :: forall m. Monad m => Int -> M m Unit
spendDuration _ = pure (todo "spendDuration")

getPatch
  :: forall m. Monad m => PatchId -> M m Patch
getPatch _ = pure (todo "getPatch")

updateWinner
  :: forall m. Monad m => M m Unit
updateWinner = pure (todo "updateWinner")

