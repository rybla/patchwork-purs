module Patchwork.Model where

import Prelude

import Data.Argonaut (class EncodeJson)
import Data.Argonaut.Encode.Generic (genericEncodeJson)
import Data.Bifunctor (lmap)
import Data.Enum (class BoundedEnum, class Enum)
import Data.Generic.Rep (class Generic)
import Data.Lens (Iso', Lens')
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.List (List(..))
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype, over)
import Data.Set (Set)
import Data.Set as Set
import Data.Show.Generic (genericShow)
import Data.TotalMap (TotalMap)
import Data.Tuple.Nested (type (/\), (/\))
import Halogen.Svg.Attributes (Color)
import Halogen.Svg.Attributes as HSvgA
import Partial.Unsafe (unsafeCrashWith)
import Patchwork.Util (todo, (∘))
import Type.Prelude (Proxy(..))
import Type.Proxy (Proxy)

--------------------------------------------------------------------------------
-- Model
--------------------------------------------------------------------------------

newtype Model = Model
  { patches :: Map PatchId Patch
  , circle :: Circle
  , players :: TotalMap PlayerId Player
  , activePlayer :: PlayerId
  , winner :: Maybe PlayerId
  }

_Model = _Newtype :: Iso' Model _
_patches = Proxy :: Proxy "patches"
_circle = Proxy :: Proxy "circle"
_players = Proxy :: Proxy "players"
_activePlayer = Proxy :: Proxy "activePlayer"
_maxTime = Proxy :: Proxy "maxTime"
_winner = Proxy :: Proxy "winner"

derive instance Newtype Model _
derive instance Generic Model _
derive newtype instance Show Model

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

--------------------------------------------------------------------------------
-- Player
--------------------------------------------------------------------------------

newtype Player = Player
  { name :: String
  , time :: Int
  , buttons :: Int
  , quilt :: Quilt
  }

_Player = _Newtype :: Iso' Player _
_time = Proxy :: Proxy "time"
_name = Proxy :: Proxy "name"
_quilt = Proxy :: Proxy "quilt"
_buttons = Proxy :: Proxy "buttons"

derive instance Newtype Player _
derive instance Generic Player _
derive newtype instance Show Player

type Quilt = Map QuiltPos (PatchId /\ Boolean)

--------------------------------------------------------------------------------
-- Patch
--------------------------------------------------------------------------------

newtype Patch = Patch
  { buttonPrice :: Int
  , durationPrice :: Int
  , quiltLayout :: QuiltLayout
  , patchStyle :: PatchStyle
  }

-- implicitly, pivot is around (0, 0)
type QuiltLayout = Set (QuiltPos /\ Boolean)

_Patch = _Newtype :: Iso' Patch _
_buttonPrice = Proxy :: Proxy "buttonPrice"
_durationPrice = Proxy :: Proxy "durationPrice"
_quiltLayout = Proxy :: Proxy "quiltLayout"
_buttonLayout = Proxy :: Proxy "buttonLayout"

derive instance Newtype Patch _
derive instance Generic Patch _
derive newtype instance Show Patch

--------------------------------------------------------------------------------
-- PatchStyle
--------------------------------------------------------------------------------

data PatchStyle 
  = SolidColorPatchStyle Color
  | BackgroundPatchStyle

derive instance Generic PatchStyle _

instance Show PatchStyle where
  show x = genericShow x

--------------------------------------------------------------------------------
-- QuiltPos
--------------------------------------------------------------------------------

newtype QuiltPos = QuiltPos (Int /\ Int)

derive instance Newtype QuiltPos _
derive instance Generic QuiltPos _
derive newtype instance Show QuiltPos
derive newtype instance Eq QuiltPos
derive newtype instance Ord QuiltPos

--------------------------------------------------------------------------------
-- Circle
--------------------------------------------------------------------------------

newtype Circle = Circle { focus :: PatchId, items :: List PatchId }

derive instance Newtype Circle _
derive instance Generic Circle _
derive newtype instance Show Circle

focus :: Lens' Circle PatchId
focus = _Newtype ∘ prop (Proxy :: Proxy "focus")

-- items :: Lens' Circle (List PatchId)
-- items = _Newtype ∘ prop (Proxy :: Proxy "items")

shiftCircle :: Circle -> Circle
shiftCircle (Circle pc) = case pc.items of
  Nil -> Circle pc
  Cons item items -> Circle { focus: item, items: List.snoc items pc.focus }

removeCircleFocus :: Circle -> Circle /\ PatchId
removeCircleFocus (Circle pc) = case pc.items of
  Nil -> unsafeCrashWith "removeCircleFocus on Circle with only 1 item"
  Cons item items -> Circle { focus: item, items } /\ pc.focus

--------------------------------------------------------------------------------
-- TurnAction
--------------------------------------------------------------------------------

data TurnAction = Buy | Wait | Pass

derive instance Generic TurnAction _

instance Show TurnAction where
  show x = genericShow x

--------------------------------------------------------------------------------
-- PatchOrientation
--------------------------------------------------------------------------------

data PatchOrientation = North | South | East | West

adjustQuiltLayout :: QuiltPos -> PatchOrientation -> QuiltLayout -> QuiltLayout
adjustQuiltLayout pos ori = shiftQuiltLayout pos ∘ orientQuiltLayout ori

orientQuiltLayout :: PatchOrientation -> QuiltLayout -> QuiltLayout
orientQuiltLayout North = identity
orientQuiltLayout East = Set.map $ lmap $ over QuiltPos \(x /\ y) -> y /\ -x
orientQuiltLayout South = Set.map $ lmap $ over QuiltPos \(x /\ y) -> x /\ -y
orientQuiltLayout West = Set.map $ lmap $ over QuiltPos \(x /\ y) -> y /\ x

shiftQuiltLayout :: QuiltPos -> QuiltLayout -> QuiltLayout
shiftQuiltLayout (QuiltPos (dx /\ dy)) = Set.map $ lmap $ over QuiltPos \(x /\ y) -> (x + dx) /\ (y + dy)

--------------------------------------------------------------------------------
-- standardPatches
--------------------------------------------------------------------------------

standardPatches :: Map PatchId Patch
standardPatches = Map.fromFoldable
  [ PatchId 0 /\ Patch
      { durationPrice: 1
      , buttonPrice: 1
      , quiltLayout: Set.fromFoldable [ QuiltPos (0 /\ 0) /\ true ]
      , patchStyle: SolidColorPatchStyle (HSvgA.RGB 255 100 100)
      }
  , PatchId 1 /\ Patch
      { durationPrice: 2
      , buttonPrice: 2
      , quiltLayout: Set.fromFoldable [ QuiltPos (0 /\ 0) /\ false, QuiltPos (0 /\ 1) /\ true ]
      , patchStyle: SolidColorPatchStyle (HSvgA.RGB 100 255 100)
      }
  , PatchId 2 /\ Patch
      { durationPrice: 3
      , buttonPrice: 3
      , quiltLayout: Set.fromFoldable [ QuiltPos (0 /\ 0) /\ false, QuiltPos (0 /\ 1) /\ false, QuiltPos (0 /\ 2) /\ true ]
      , patchStyle: SolidColorPatchStyle (HSvgA.RGB 100 100 255)
      }
  , PatchId 3 /\ Patch
      { durationPrice: 3
      , buttonPrice: 3
      , quiltLayout: Set.fromFoldable [ QuiltPos (0 /\ 0) /\ false, QuiltPos (0 /\ 1) /\ false, QuiltPos (0 /\ 2) /\ true ]
      , patchStyle: SolidColorPatchStyle (HSvgA.RGB 100 100 255)
      }
  , PatchId 4 /\ Patch
      { durationPrice: 3
      , buttonPrice: 3
      , quiltLayout: Set.fromFoldable [ QuiltPos (0 /\ 0) /\ false, QuiltPos (0 /\ 1) /\ false, QuiltPos (0 /\ 2) /\ true ]
      , patchStyle: SolidColorPatchStyle (HSvgA.RGB 100 100 255)
      }
  , PatchId 5 /\ Patch
      { durationPrice: 3
      , buttonPrice: 3
      , quiltLayout: Set.fromFoldable [ QuiltPos (0 /\ 0) /\ false, QuiltPos (0 /\ 1) /\ false, QuiltPos (0 /\ 2) /\ true ]
      , patchStyle: SolidColorPatchStyle (HSvgA.RGB 100 100 255)
      }
  , PatchId 6 /\ Patch
      { durationPrice: 3
      , buttonPrice: 3
      , quiltLayout: Set.fromFoldable [ QuiltPos (0 /\ 0) /\ false, QuiltPos (0 /\ 1) /\ false, QuiltPos (0 /\ 2) /\ true ]
      , patchStyle: SolidColorPatchStyle (HSvgA.RGB 100 100 255)
      }
  , PatchId 7 /\ Patch
      { durationPrice: 3
      , buttonPrice: 3
      , quiltLayout: Set.fromFoldable [ QuiltPos (0 /\ 0) /\ false, QuiltPos (0 /\ 1) /\ false, QuiltPos (0 /\ 2) /\ true ]
      , patchStyle: SolidColorPatchStyle (HSvgA.RGB 100 100 255)
      }
  , PatchId 8 /\ Patch
      { durationPrice: 3
      , buttonPrice: 3
      , quiltLayout: Set.fromFoldable [ QuiltPos (0 /\ 0) /\ false, QuiltPos (0 /\ 1) /\ false, QuiltPos (0 /\ 2) /\ true ]
      , patchStyle: SolidColorPatchStyle (HSvgA.RGB 100 100 255)
      }
  , PatchId 9 /\ Patch
      { durationPrice: 3
      , buttonPrice: 3
      , quiltLayout: Set.fromFoldable [ QuiltPos (0 /\ 0) /\ false, QuiltPos (0 /\ 1) /\ false, QuiltPos (0 /\ 2) /\ true ]
      , patchStyle: SolidColorPatchStyle (HSvgA.RGB 100 100 255)
      }
  , PatchId 10 /\ Patch
      { durationPrice: 3
      , buttonPrice: 3
      , quiltLayout: Set.fromFoldable [ QuiltPos (0 /\ 0) /\ false, QuiltPos (0 /\ 1) /\ false, QuiltPos (0 /\ 2) /\ true ]
      , patchStyle: SolidColorPatchStyle (HSvgA.RGB 100 100 255)
      }
  , PatchId 11 /\ Patch
      { durationPrice: 3
      , buttonPrice: 3
      , quiltLayout: Set.fromFoldable [ QuiltPos (0 /\ 0) /\ false, QuiltPos (0 /\ 1) /\ false, QuiltPos (0 /\ 2) /\ true ]
      , patchStyle: SolidColorPatchStyle (HSvgA.RGB 100 100 255)
      }
  , PatchId 12 /\ Patch
      { durationPrice: 3
      , buttonPrice: 3
      , quiltLayout: Set.fromFoldable [ QuiltPos (0 /\ 0) /\ false, QuiltPos (0 /\ 1) /\ false, QuiltPos (0 /\ 2) /\ true ]
      , patchStyle: SolidColorPatchStyle (HSvgA.RGB 100 100 255)
      }
  , PatchId 13 /\ Patch
      { durationPrice: 3
      , buttonPrice: 3
      , quiltLayout: Set.fromFoldable [ QuiltPos (0 /\ 0) /\ false, QuiltPos (0 /\ 1) /\ false, QuiltPos (0 /\ 2) /\ true ]
      , patchStyle: SolidColorPatchStyle (HSvgA.RGB 100 100 255)
      }
  , PatchId 14 /\ Patch
      { durationPrice: 3
      , buttonPrice: 3
      , quiltLayout: Set.fromFoldable [ QuiltPos (0 /\ 0) /\ false, QuiltPos (0 /\ 1) /\ false, QuiltPos (0 /\ 2) /\ true ]
      , patchStyle: SolidColorPatchStyle (HSvgA.RGB 100 100 255)
      }
  , PatchId 15 /\ Patch
      { durationPrice: 3
      , buttonPrice: 3
      , quiltLayout: Set.fromFoldable [ QuiltPos (0 /\ 0) /\ false, QuiltPos (0 /\ 1) /\ false, QuiltPos (0 /\ 2) /\ true ]
      , patchStyle: SolidColorPatchStyle (HSvgA.RGB 100 100 255)
      }
  ]

initialCircle :: Int -> Map PatchId Patch -> Circle
initialCircle _seed patches =
  case patches # Map.keys # List.fromFoldable of
    Nil -> unsafeCrashWith "initialCircle with no patches"
    Cons focus_ items -> Circle { focus: focus_, items }

