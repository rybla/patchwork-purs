module Patchwork.Interaction where

import Prelude

import Control.Monad.Error.Class (class MonadThrow, throwError)
import Control.Monad.Free (Free, liftF)
import Control.Monad.Reader.Class (class MonadAsk, ask)
import Control.Monad.Rec.Class (class MonadRec, Step(..), tailRecM)
import Control.Monad.State.Class (class MonadState, state)
import Control.Monad.Trans.Class (class MonadTrans)
import Control.Monad.Writer.Class (class MonadTell, tell)
import Data.Identity (Identity)
import Data.Newtype (class Newtype)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Patchwork.Model (Patch, PlayerId, Quilt)

--------------------------------------------------------------------------------
-- InteractionF
--------------------------------------------------------------------------------

-- | Monad that models player interactions.
data InteractionF m (a :: Type)
  = Lift (m a)
  | PlacePatch
      PlayerId -- target player
      Patch -- patch to place
      Quilt -- target player's current quilt
      ( Quilt -- player's new quilt
        -> m a
      )
  | ChooseWaitDuration
      PlayerId -- target player
      Int -- target player's current time
      ( Int -- target player's new time
        -> m a
      )

placePatch
  :: forall m. Monad m => PlayerId -> Patch -> Quilt -> InteractionT m Quilt
placePatch playerId patch quilt = InteractionT (liftF (PlacePatch playerId patch quilt pure))

derive instance Newtype (InteractionT m a) _

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

--------------------------------------------------------------------------------
-- Related Types
--------------------------------------------------------------------------------

type Interaction = InteractionT Identity

newtype InteractionT m a = InteractionT (Free (InteractionF m) a)
