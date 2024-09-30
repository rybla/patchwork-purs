module Patchwork.Interaction
  ( InteractionT(..)
  , InteractionF(..)
  , labelInteractionF
  , lift
  , chooseTurnAction
  , choosePatchFromMarket
  , choosePatchPlacement
  , chooseWaitDuration
  , setWinner
  ) where

import Prelude

import Control.Monad.Error.Class (class MonadThrow, throwError)
import Control.Monad.Free (Free, liftF)
import Control.Monad.Reader.Class (class MonadAsk, ask)
import Control.Monad.Rec.Class (class MonadRec, Step(..), tailRecM)
import Control.Monad.State.Class (class MonadState, state)
import Control.Monad.Trans.Class (class MonadTrans)
import Control.Monad.Writer.Class (class MonadTell, tell)
import Data.Identity (Identity)
import Data.Three (Three)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Patchwork.Model (PatchFace, PatchId, PatchOrientation, PlayerId, QuiltPos, TurnAction)

--------------------------------------------------------------------------------
-- InteractionF
--------------------------------------------------------------------------------

newtype InteractionT m a = InteractionT (Free (InteractionF m) a)
type Interaction = InteractionT Identity

data InteractionF m (a :: Type)
  = Lift (m a)
  | ChooseTurnAction ({ selection :: TurnAction } -> m a)
  | ChoosePatchFromMarket ({ selection :: Three } -> m a)
  | ChoosePatchPlacement { patchId :: PatchId } ({ position :: QuiltPos, orientation :: PatchOrientation, face :: PatchFace } -> m a)
  | ChooseWaitDuration ({ duration :: Int } -> m a)
  | SetWinner { winner :: PlayerId } (m a)

labelInteractionF :: forall m a. InteractionF m a -> String
labelInteractionF = case _ of
  Lift _ -> "Lift"
  ChooseTurnAction _ -> "ChooseTurnAction"
  ChoosePatchFromMarket _ -> "ChoosePatchFromMarket"
  ChoosePatchPlacement _ _ -> "ChoosePatchPlacement"
  ChooseWaitDuration _ -> "ChooseWaitDuration"
  SetWinner _ _ -> "SetWinner"

lift = InteractionT <<< liftF <<< Lift
chooseTurnAction = InteractionT <<< liftF <<< ChooseTurnAction $ pure
choosePatchFromMarket = InteractionT <<< liftF <<< ChoosePatchFromMarket $ pure
choosePatchPlacement args = InteractionT <<< liftF <<< ChoosePatchPlacement args $ pure
chooseWaitDuration = InteractionT <<< liftF <<< ChooseWaitDuration $ pure
setWinner args = InteractionT <<< liftF <<< SetWinner args $ pure

derive newtype instance Functor (InteractionT m)
derive newtype instance Apply (InteractionT m)
derive newtype instance Applicative (InteractionT m)
derive newtype instance Bind (InteractionT m)
derive newtype instance Monad (InteractionT m)
derive newtype instance Semigroup a => Semigroup (InteractionT m a)
derive newtype instance Monoid a => Monoid (InteractionT m a)

instance MonadEffect m => MonadEffect (InteractionT m) where
  liftEffect = InteractionT <<< liftF <<< Lift <<< liftEffect

instance MonadAff m => MonadAff (InteractionT m) where
  liftAff = InteractionT <<< liftF <<< Lift <<< liftAff

instance MonadTrans InteractionT where
  lift = InteractionT <<< liftF <<< Lift

instance MonadRec (InteractionT m) where
  tailRecM k a = k a >>= case _ of
    Loop x -> tailRecM k x
    Done y -> pure y

instance MonadState state m => MonadState state (InteractionT m) where
  state = InteractionT <<< liftF <<< Lift <<< state

instance MonadAsk r m => MonadAsk r (InteractionT m) where
  ask = InteractionT $ liftF $ Lift ask

instance MonadTell w m => MonadTell w (InteractionT m) where
  tell = InteractionT <<< liftF <<< Lift <<< tell

instance MonadThrow e m => MonadThrow e (InteractionT m) where
  throwError = InteractionT <<< liftF <<< Lift <<< throwError

derive instance Functor m => Functor (InteractionF m)

