module Patchwork.Logic where

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
  turn unit

turn :: forall m. MonadAff m => Unit -> M m Unit
turn _ = do
  inject (ChooseTurnAction { k: pure }) >>= case _ of
    { selection: Buy } -> buy
    { selection: Wait } -> wait
  endTurn

wait :: forall m. MonadAff m => M m Unit
wait = do
  pure unit

buy :: forall m. MonadAff m => M m Unit
buy = do
  pure unit

endTurn :: forall m. MonadAff m => M m Unit
endTurn = do
  -- check for end of game (no valid active player)
  -- update active player
  -- next turn
  turn unit
