module Patchwork.Logic where

import Prelude

import Control.Monad.State (StateT)
import Data.Either (Either(..))
import Data.Either.Nested (type (\/))
import Data.Three (Three)
import Effect.Aff.Class (class MonadAff)
import Patchwork.Interaction (InteractionT, chooseTurnAction)
import Patchwork.Model (Model)
import Patchwork.Model as Model
import Patchwork.Util (todo)

--------------------------------------------------------------------------------
-- T
--------------------------------------------------------------------------------
type T m = InteractionT (StateT Model m)

type M a = forall m. MonadAff m => T m a

--------------------------------------------------------------------------------
-- main
--------------------------------------------------------------------------------
main :: M Unit
main = do
  turn unit

turn :: Unit -> M Unit
turn _ = do
  startTurnPhase
  mainTurnPhase
  endTurnPhase

startTurnPhase :: M Unit
startTurnPhase = pure unit

mainTurnPhase :: M Unit
mainTurnPhase = do
  chooseTurnAction_safe unit
    >>= case _ of
      { selection: Model.Buy } -> buy
      { selection: Model.Wait } -> wait

wait :: M Unit
wait = do
  { duration } <- chooseWaitDuration_safe unit
  todo "wait" duration # void
  pure unit

buy :: M Unit
buy = do
  pure unit

endTurnPhase :: M Unit
endTurnPhase = do
  -- check for end of game (no valid active player)
  -- update active player
  -- next turn
  turn unit

--------------------------------------------------------------------------------
-- safe interactions
--
-- These versions of the interactions are _safe_ in that they validate
-- something, requiring backtracking upon an invalid state, before returning.
-- Mostly used for validating user input, and re-prompting if they input
-- something invalid.
--------------------------------------------------------------------------------

chooseTurnAction_safe :: Unit -> M { selection :: Model.TurnAction }
chooseTurnAction_safe _ = do
  { selection } <- chooseTurnAction
  isValidTurnAction selection
    >>= case _ of
      Left _msg -> chooseTurnAction_safe unit
      Right _ -> pure { selection }

choosePatchFromCircle_safe :: Unit -> M { selection :: Three }
choosePatchFromCircle_safe = todo "choosePatchFromCircle_safe" {}

choosePatchPlacement_safe :: Unit -> M { position :: Model.QuiltPos, orientation :: Model.PatchOrientation, face :: Model.PatchFace }
choosePatchPlacement_safe = todo "choosePatchPlacement_safe" {}

chooseWaitDuration_safe :: Unit -> M { duration :: Int }
chooseWaitDuration_safe = do todo "chooseWaitDuration_safe" {}

--------------------------------------------------------------------------------
-- utilities
--------------------------------------------------------------------------------
isValidTurnAction :: Model.TurnAction -> M (String \/ Unit)
isValidTurnAction = todo "make sure is valid"
