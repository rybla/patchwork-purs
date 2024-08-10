module Patchwork.Update where

import Patchwork.Model
import Prelude

import Control.Monad.Except (ExceptT, throwError)
import Control.Monad.Reader (ReaderT)
import Control.Monad.State (StateT, gets)
import Data.Lens (view, (%=), (.=))
import Data.Lens.At (at)
import Data.Lens.Record (prop)
import Data.Map as Map
import Data.Maybe (maybe)
import Data.TotalMap (at')
import Patchwork.Interaction (InteractionT)
import Patchwork.Interaction as Interaction
import Patchwork.Util (todo, (∘))
import Type.Prelude (Proxy(..))

--------------------------------------------------------------------------------
-- M
--------------------------------------------------------------------------------

type M m = InteractionT (ExceptT Err (ReaderT Ctx (StateT Env m)))

type Ctx = {}

type Env =
  { model :: Model
  }

_model = Proxy :: Proxy "model"

type Err = String

--------------------------------------------------------------------------------
-- Update
--------------------------------------------------------------------------------

update
  :: forall m. Monad m => Action -> M m Unit
update (Buy buy) = do
  Patch patch <- removePatchFromPatchCircle buy.selection
  spendButtons patch.buttonPrice
  spendDuration patch.durationPrice
  placePatch (Patch patch)
  updateWinner
  pure unit
update (Wait wait) = do
  pure (todo "wait")

-- | Run interaction where player places patch on their quilt.
placePatch
  :: forall m. Monad m => Patch -> M m Unit
placePatch patch = do
  playerId <- gets (view (prop _model ∘ _Model ∘ prop _activePlayer))
  quilt <- gets (view (prop _model ∘ _Model ∘ prop _players ∘ at' playerId ∘ _Player ∘ prop _quilt))
  quilt' <- Interaction.placePatch playerId patch quilt
  prop _model ∘ _Model ∘ prop _players ∘ at' playerId ∘ _Player ∘ prop _quilt .= quilt'

removePatchFromPatchCircle
  :: forall m. Monad m => CirclePos -> M m Patch
removePatchFromPatchCircle i = do
  patch <-
    gets (view (prop _model ∘ _Model ∘ prop _patchCircle ∘ at i))
      >>= maybe (throwError "invalid CirclePos") pure
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

