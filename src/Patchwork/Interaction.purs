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
import Data.Three (Three(..))
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Patchwork.Model (Circle(..), Patch, PatchFace, PatchId, PatchOrientation, PlayerId, Quilt, QuiltPos(..), TurnAction)

--------------------------------------------------------------------------------
-- InteractionF
--------------------------------------------------------------------------------

newtype InteractionT m a = InteractionT (Free (InteractionF m) a)
type Interaction = InteractionT Identity

data InteractionF m a
  = Lift_InteractionF (Lift m a)
  | ChooseTurnAction_InteractionF (ChooseTurnAction m a)
  | ChoosePatchFromCircle_InteractionF (ChoosePatchFromCircle m a)
  | PlacePatch_InteractionF (PlacePatch m a)
  | ChooseWaitDuration_InteractionF (ChooseWaitDuration m a)
  | SetWinner_InteractionF (SetWinner m a)

labelInteractionF :: forall m a. InteractionF m a -> String
labelInteractionF = case _ of
  Lift_InteractionF _ -> "Lift"
  ChooseTurnAction_InteractionF _ -> "ChooseTurnAction"
  ChoosePatchFromCircle_InteractionF _ -> "ChoosePatchFromCircle"
  PlacePatch_InteractionF _ -> "PlacePatch"
  ChooseWaitDuration_InteractionF _ -> "ChooseWaitDuration"
  SetWinner_InteractionF _ -> "SetWinner"

class Inject f where
  inject :: forall m a. Monad m => f m a -> InteractionT m a

newtype Lift m (a :: Type) = Lift (m a)

derive instance Functor m => Functor (Lift m)
instance Inject Lift where
  inject = InteractionT <<< liftF <<< Lift_InteractionF

newtype ChooseTurnAction m (a :: Type) = ChooseTurnAction { k :: ChooseTurnAction_Result -> m a }
type ChooseTurnAction_Result = { selection :: TurnAction }

derive instance Functor m => Functor (ChooseTurnAction m)
instance Inject ChooseTurnAction where
  inject = InteractionT <<< liftF <<< ChooseTurnAction_InteractionF

newtype ChoosePatchFromCircle m (a :: Type) = ChoosePatchFromCircle { k :: ChoosePatchFromCircle_Result -> m a }
type ChoosePatchFromCircle_Result = { selection :: Three }

derive instance Functor m => Functor (ChoosePatchFromCircle m)
instance Inject ChoosePatchFromCircle where
  inject = InteractionT <<< liftF <<< ChoosePatchFromCircle_InteractionF

newtype PlacePatch m (a :: Type) = PlacePatch { patchId :: PatchId, k :: PlacePatch_Result -> m a }
type PlacePatch_Result = { pos :: QuiltPos, ori :: PatchOrientation, face :: PatchFace }

derive instance Functor m => Functor (PlacePatch m)
instance Inject PlacePatch where
  inject = InteractionT <<< liftF <<< PlacePatch_InteractionF

newtype ChooseWaitDuration m (a :: Type) = ChooseWaitDuration { k :: ChooseWaitDuration_Result -> m a }
type ChooseWaitDuration_Result = { duration :: Int }

derive instance Functor m => Functor (ChooseWaitDuration m)
instance Inject ChooseWaitDuration where
  inject = InteractionT <<< liftF <<< ChooseWaitDuration_InteractionF

newtype SetWinner m (a :: Type) = SetWinner { winner :: PlayerId, k :: {} -> m a }

derive instance Functor m => Functor (SetWinner m)
instance Inject SetWinner where
  inject = InteractionT <<< liftF <<< SetWinner_InteractionF

derive instance Newtype (InteractionT m a) _

derive newtype instance Functor (InteractionT m)
derive newtype instance Apply (InteractionT m)
derive newtype instance Applicative (InteractionT m)
derive newtype instance Bind (InteractionT m)
derive newtype instance Monad (InteractionT m)
derive newtype instance Semigroup a => Semigroup (InteractionT m a)
derive newtype instance Monoid a => Monoid (InteractionT m a)

instance MonadEffect m => MonadEffect (InteractionT m) where
  liftEffect = InteractionT <<< liftF <<< Lift_InteractionF <<< Lift <<< liftEffect

instance MonadAff m => MonadAff (InteractionT m) where
  liftAff = InteractionT <<< liftF <<< Lift_InteractionF <<< Lift <<< liftAff

instance MonadTrans InteractionT where
  lift = InteractionT <<< liftF <<< Lift_InteractionF <<< Lift

instance MonadRec (InteractionT m) where
  tailRecM k a = k a >>= case _ of
    Loop x -> tailRecM k x
    Done y -> pure y

instance MonadState state m => MonadState state (InteractionT m) where
  state = InteractionT <<< liftF <<< Lift_InteractionF <<< Lift <<< state

instance MonadAsk r m => MonadAsk r (InteractionT m) where
  ask = InteractionT $ liftF $ Lift_InteractionF $ Lift ask

instance MonadTell w m => MonadTell w (InteractionT m) where
  tell = InteractionT <<< liftF <<< Lift_InteractionF <<< Lift <<< tell

instance MonadThrow e m => MonadThrow e (InteractionT m) where
  throwError = InteractionT <<< liftF <<< Lift_InteractionF <<< Lift <<< throwError

derive instance Functor m => Functor (InteractionF m)

