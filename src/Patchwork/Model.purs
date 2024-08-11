module Patchwork.Model where

import Prelude

import Data.Argonaut (class EncodeJson)
import Data.Argonaut.Encode.Generic (genericEncodeJson)
import Data.Enum (class BoundedEnum, class Enum)
import Data.Generic.Rep (class Generic)
import Data.Lens (Iso')
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Map (Map)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Data.Set (Set)
import Data.Show.Generic (genericShow)
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
_maxTime = Proxy :: Proxy "maxTime"
_winner = Proxy :: Proxy "winner"

derive instance Newtype Model _
derive instance Generic Model _
derive newtype instance Show Model
instance EncodeJson Model where
  encodeJson x = genericEncodeJson x

--------------------------------------------------------------------------------
-- PlayerId
--------------------------------------------------------------------------------

newtype PlayerId = PlayerId Boolean

derive instance Newtype PlayerId _
derive instance Generic PlayerId _
derive newtype instance Eq PlayerId
derive newtype instance Ord PlayerId
derive newtype instance Enum PlayerId
derive newtype instance Bounded PlayerId
derive newtype instance BoundedEnum PlayerId

instance Show PlayerId where
  show = case _ of
    PlayerId false -> "Player 1"
    PlayerId true -> "Player 2"

instance EncodeJson PlayerId where
  encodeJson x = genericEncodeJson x

nextPlayerId (PlayerId b) = PlayerId (not b)

--------------------------------------------------------------------------------
-- PatchId
--------------------------------------------------------------------------------

newtype PatchId = PatchId Int

derive instance Newtype PatchId _
derive instance Generic PatchId _
derive newtype instance Show PatchId
derive newtype instance Eq PatchId
derive newtype instance Ord PatchId

instance EncodeJson PatchId where
  encodeJson x = genericEncodeJson x

--------------------------------------------------------------------------------
-- PatchCircleIndex
--------------------------------------------------------------------------------

newtype PatchCircleIndex = PatchCircleIndex Int

derive instance Newtype PatchCircleIndex _
derive instance Generic PatchCircleIndex _
derive newtype instance Show PatchCircleIndex

instance EncodeJson PatchCircleIndex where
  encodeJson x = genericEncodeJson x

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
_time = Proxy :: Proxy "time"
_name = Proxy :: Proxy "name"
_quilt = Proxy :: Proxy "quilt"
_buttons = Proxy :: Proxy "buttons"

derive instance Newtype Player _
derive instance Generic Player _
derive newtype instance Show Player

instance EncodeJson Player where
  encodeJson x = genericEncodeJson x

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
derive instance Generic Patch _
derive newtype instance Show Patch

instance EncodeJson Patch where
  encodeJson x = genericEncodeJson x

--------------------------------------------------------------------------------
-- QuiltPos
--------------------------------------------------------------------------------

newtype QuiltPos = QuiltPos (Int /\ Int)

derive instance Newtype QuiltPos _
derive instance Generic QuiltPos _
derive newtype instance Show QuiltPos
derive newtype instance Eq QuiltPos
derive newtype instance Ord QuiltPos

instance EncodeJson QuiltPos where
  encodeJson x = genericEncodeJson x

--------------------------------------------------------------------------------
-- CirclePos
--------------------------------------------------------------------------------

newtype CirclePos = CirclePos Int

derive instance Newtype CirclePos _
derive instance Generic CirclePos _
derive newtype instance Show CirclePos
derive newtype instance Eq CirclePos
derive newtype instance Ord CirclePos

instance EncodeJson CirclePos where
  encodeJson x = genericEncodeJson x

--------------------------------------------------------------------------------
-- TurnAction
--------------------------------------------------------------------------------

data TurnAction = Buy | Wait | Pass

derive instance Generic TurnAction _

instance Show TurnAction where
  show x = genericShow x

instance EncodeJson TurnAction where
  encodeJson x = genericEncodeJson x