module Patchwork.Model where

import Prelude

import Data.Enum (class Enum)
import Data.Lens (Iso')
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Map (Map)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Set (Set)
import Data.TotalMap (TotalMap)
import Data.Tuple.Nested (type (/\))
import Type.Prelude (Proxy(..))
import Type.Proxy (Proxy)

--------------------------------------------------------------------------------
-- Model
--------------------------------------------------------------------------------

newtype Model = Model
  { patches :: Map PatchId Patch
  , patchCircle :: Map CirclePos PatchId
  , currentCirclePos :: CirclePos
  , players :: TotalMap PlayerId Player
  , activePlayer :: PlayerId
  , winner :: Maybe PlayerId
  }

_Model = _Newtype :: Iso' Model _
_patches = Proxy :: Proxy "patches"
_patchCircle = Proxy :: Proxy "patchCircle"
_currentCirclePos = Proxy :: Proxy "currentCirclePos"
_players = Proxy :: Proxy "players"
_activePlayer = Proxy :: Proxy "activePlayer"

derive instance Newtype Model _

--------------------------------------------------------------------------------
-- PlayerId
--------------------------------------------------------------------------------

newtype PlayerId = PlayerId Boolean

derive instance Newtype PlayerId _
derive newtype instance Eq PlayerId
derive newtype instance Ord PlayerId

instance Enum PlayerId where
  succ (PlayerId b) = Just (PlayerId (not b))
  pred (PlayerId b) = Just (PlayerId (not b))

--------------------------------------------------------------------------------
-- PatchId
--------------------------------------------------------------------------------

newtype PatchId = PatchId Int

derive instance Newtype PatchId _

--------------------------------------------------------------------------------
-- PatchCircleIndex
--------------------------------------------------------------------------------

newtype PatchCircleIndex = PatchCircleIndex Int

derive instance Newtype PatchCircleIndex _

--------------------------------------------------------------------------------
-- Player
--------------------------------------------------------------------------------

newtype Player = Player
  { name :: String
  , time :: Int
  , quilt :: Quilt
  , buttons :: Int
  }

_Player = _Newtype :: Iso' Player _
_name = Proxy :: Proxy "name"
_quilt = Proxy :: Proxy "quilt"
_buttons = Proxy :: Proxy "buttons"

derive instance Newtype Player _

type Quilt = Map QuiltPos PatchId

--------------------------------------------------------------------------------
-- Patch
--------------------------------------------------------------------------------

newtype Patch = Patch
  { buttonPrice :: Int
  , durationPrice :: Int
  , layout :: Set QuiltPos
  , buttonLayout :: Set QuiltPos
  }

_Patch = _Newtype :: Iso' Patch _
_buttonPrice = Proxy :: Proxy "buttonPrice"
_durationPrice = Proxy :: Proxy "durationPrice"
_layout = Proxy :: Proxy "layout"
_buttonLayout = Proxy :: Proxy "buttonLayout"

derive instance Newtype Patch _

--------------------------------------------------------------------------------
-- QuiltPos
--------------------------------------------------------------------------------

newtype QuiltPos = QuiltPos (Int /\ Int)

derive instance Newtype QuiltPos _

--------------------------------------------------------------------------------
-- CirclePos
--------------------------------------------------------------------------------

newtype CirclePos = CirclePos Int

derive instance Newtype CirclePos _
derive newtype instance Eq CirclePos
derive newtype instance Ord CirclePos

--------------------------------------------------------------------------------
-- Action
--------------------------------------------------------------------------------

data Action
  = Buy { selection :: CirclePos }
  | Wait { duration :: Int }
