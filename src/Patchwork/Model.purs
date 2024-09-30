module Patchwork.Model where

import Prelude

import Data.Array ((..))
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Bounded.Generic (genericBottom, genericTop)
import Data.Enum (class BoundedEnum, class Enum)
import Data.Enum.Generic (genericCardinality, genericFromEnum, genericPred, genericSucc, genericToEnum)
import Data.Eq.Generic (genericEq)
import Data.Foldable (foldMap, or, sum)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Generic.Rep (class Generic)
import Data.Lens (Iso', Lens', lens', set, (^.))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.List (List(..), (:))
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe')
import Data.Newtype (class Newtype, over)
import Data.Ord.Generic (genericCompare)
import Data.Set (Set)
import Data.Set as Set
import Data.Show.Generic (genericShow)
import Data.Three (Three)
import Data.Three as Three
import Data.TotalMap (TotalMap, at')
import Data.Tuple.Nested (type (/\), (/\))
import Halogen.Svg.Attributes (Color)
import Halogen.Svg.Attributes as HSvgA
import Partial.Unsafe (unsafeCrashWith)
import Patchwork.Util (bug, bug', rotateArray, todo, (∘))
import Type.Prelude (Proxy(..))
import Type.Proxy (Proxy)

quiltSize = 9

--------------------------------------------------------------------------------
-- Model
--------------------------------------------------------------------------------

newtype Model = Model
  { patches :: Map PatchId Patch
  , circle :: Circle
  , players :: TotalMap PlayerId Player
  , activePlayerId :: PlayerId
  , winner :: Maybe PlayerId
  , turn :: Int
  }

_Model = _Newtype :: Iso' Model _
_patches = prop (Proxy :: Proxy "patches")
_circle = prop (Proxy :: Proxy "circle")
_players = prop (Proxy :: Proxy "players")
_activePlayerId = prop (Proxy :: Proxy "activePlayerId")
_maxTime = prop (Proxy :: Proxy "maxTime")
_winner = prop (Proxy :: Proxy "winner")
_turn = prop (Proxy :: Proxy "turn")

derive instance Newtype Model _
derive instance Generic Model _
derive newtype instance Show Model

activePlayer :: Lens' Model Player
activePlayer = lens' \(Model model) ->
  model.players ^. at' model.activePlayerId /\
    \player -> Model model { players = model.players # set (at' model.activePlayerId) player }

getActivePlayer :: Model -> Player
getActivePlayer (Model model) = model.players ^. at' model.activePlayerId

getPatch :: PatchId -> Model -> Patch
getPatch pId (Model model) = model.patches
  # Map.lookup pId
  # fromMaybe' \_ -> bug $ "invalid PatchId: " <> show pId

-- | A player's score is:
-- |  + number of buttons
-- |  + number of bonus buttons
-- |  - 2*(number of squares on board NOT covered by quilt)
playerScore :: Model -> PlayerId -> Int
playerScore (Model model) playerId = buttons - (2 * uncoveredSquares)
  where
  Player player = model.players ^. at' playerId
  buttons = player.buttons
  uncoveredSquares = Set.difference board (player.quilt # Map.keys) # Set.size

quiltButtons :: Quilt -> Int
quiltButtons = Map.values >>> map (\(_ /\ btn) -> if btn then 1 else 0) >>> sum

board :: Set QuiltPos
board = Set.fromFoldable $
  0 .. quiltSize # foldMap \x ->
    0 .. quiltSize # map \y ->
      QuiltPos (x /\ y)

--------------------------------------------------------------------------------
-- PlayerId
--------------------------------------------------------------------------------

data PlayerId = Player1 | Player2

derive instance Generic PlayerId _

instance Show PlayerId where
  show x = genericShow x

instance Eq PlayerId where
  eq x = genericEq x

instance Ord PlayerId where
  compare x = genericCompare x

instance Enum PlayerId where
  succ x = genericSucc x
  pred x = genericPred x

instance Bounded PlayerId where
  top = genericTop
  bottom = genericBottom

instance BoundedEnum PlayerId where
  cardinality = genericCardinality
  fromEnum x = genericFromEnum x
  toEnum x = genericToEnum x

nextPlayerId :: PlayerId -> PlayerId
nextPlayerId Player1 = Player2
nextPlayerId Player2 = Player1

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
  , previousTurn :: Int
  , buttons :: Int
  , quilt :: Quilt
  }

_Player = _Newtype :: Iso' Player _
_name = prop (Proxy :: Proxy "name")
_time = prop (Proxy :: Proxy "time")
_buttons = prop (Proxy :: Proxy "buttons")
_quilt = prop (Proxy :: Proxy "quilt")
_previousTurn = prop (Proxy :: Proxy "previousTurn")

derive instance Newtype Player _
derive instance Generic Player _
derive newtype instance Show Player

type Quilt = Map QuiltPos (PatchId /\ Boolean)

canAfford :: Player -> Patch -> Boolean
canAfford (Player player) (Patch patch) =
  player.buttons >= patch.buttonPrice &&
    player.time >= patch.timePrice

getPlayerScore :: Player -> Int
getPlayerScore player = todo "getPlayerScore" {}

--------------------------------------------------------------------------------
-- Patch
--------------------------------------------------------------------------------

newtype Patch = Patch
  { buttonPrice :: Int
  , timePrice :: Int
  , patchLayout :: PatchLayout
  , patchStyle :: PatchStyle
  }

-- implicitly, pivot is around (0, 0)
-- Boolean indicates if there is a Button present or not
type PatchLayout = Set (QuiltPos /\ Boolean)

_Patch = _Newtype :: Iso' Patch _

_buttonPrice = prop (Proxy :: Proxy "buttonPrice")
_timePrice = prop (Proxy :: Proxy "timePrice")
_patchLayout = prop (Proxy :: Proxy "patchLayout")

derive instance Newtype Patch _
derive instance Generic Patch _
derive newtype instance Show Patch

fromPatchLayoutToQuilt :: PatchId -> PatchLayout -> Quilt
fromPatchLayoutToQuilt id = Set.map (\(p /\ btn) -> p /\ id /\ btn) >>> Map.fromFoldable

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

addQuiltPos :: QuiltPos -> QuiltPos -> QuiltPos
addQuiltPos (QuiltPos (x1 /\ y1)) (QuiltPos (x2 /\ y2)) = QuiltPos ((x1 + x2) /\ (y1 + y2))

isOffQuilt :: QuiltPos -> Boolean
isOffQuilt (QuiltPos (x /\ y)) = or
  [ x < 0
  , x >= quiltSize
  , y < 0
  , y >= quiltSize
  ]

isOnQuilt :: QuiltPos -> Boolean
isOnQuilt = not <<< isOffQuilt

--------------------------------------------------------------------------------
-- Circle
--------------------------------------------------------------------------------

newtype Circle = Circle { patches :: Array PatchId }

derive instance Newtype Circle _
derive instance Generic Circle _
derive newtype instance Show Circle

extractPatchFromCircle :: Int -> Circle -> Circle /\ PatchId
extractPatchFromCircle i (Circle circle) = fromMaybe' (bug' "circle is empty") do
  { head: patchId, tail: patches } <- circle.patches # rotateArray i # Array.uncons
  pure (Circle circle { patches = patches } /\ patchId)

-- newtype Circle = Circle { focus :: PatchId, items :: List PatchId }

-- focus :: Lens' Circle PatchId
-- focus = _Newtype ∘ prop (Proxy :: Proxy "focus")

-- nextThreePatches :: Circle -> PatchId /\ PatchId /\ PatchId
-- nextThreePatches (Circle circle) = case circle.items # List.take 3 of
--   a : b : c : _ -> a /\ b /\ c
--   _ -> bug "the Circle cannot have less than 3 next patches"

-- getOneOfThreePatches :: Three -> Circle -> PatchId
-- getOneOfThreePatches three (Circle circle) = case three of
--   Three.One -> List.index circle.items 0 # fromMaybe' \_ -> bug "the Circle cannot have less than 3 next patches"
--   Three.Two -> List.index circle.items 1 # fromMaybe' \_ -> bug "the Circle cannot have less than 3 next patches"
--   Three.Three -> List.index circle.items 2 # fromMaybe' \_ -> bug "the Circle cannot have less than 3 next patches"

-- -- items :: Lens' Circle (List PatchId)
-- -- items = _Newtype ∘ prop (Proxy :: Proxy "items")

-- shiftCircle :: Circle -> Circle
-- shiftCircle (Circle pc) = case pc.items of
--   Nil -> Circle pc
--   Cons item items -> Circle { focus: item, items: List.snoc items pc.focus }

-- removeCircleFocus :: Circle -> Circle /\ PatchId
-- removeCircleFocus (Circle pc) = case pc.items of
--   Nil -> unsafeCrashWith "removeCircleFocus on Circle with only 1 item"
--   Cons item items -> Circle { focus: item, items } /\ pc.focus

--------------------------------------------------------------------------------
-- TurnAction
--------------------------------------------------------------------------------

data TurnAction = Buy | Wait

derive instance Generic TurnAction _

instance Show TurnAction where
  show x = genericShow x

--------------------------------------------------------------------------------
-- PatchOrientation
--------------------------------------------------------------------------------

data PatchOrientation = North | South | East | West

derive instance Generic PatchOrientation _

instance Show PatchOrientation where
  show x = genericShow x

instance Eq PatchOrientation where
  eq x = genericEq x

nextPatchOrientation :: PatchOrientation -> PatchOrientation
nextPatchOrientation North = East
nextPatchOrientation East = South
nextPatchOrientation South = West
nextPatchOrientation West = North

prevPatchOrientation :: PatchOrientation -> PatchOrientation
prevPatchOrientation East = North
prevPatchOrientation South = East
prevPatchOrientation West = South
prevPatchOrientation North = West

--------------------------------------------------------------------------------
-- PatchFace
--------------------------------------------------------------------------------

data PatchFace = FaceUp | FaceDown

derive instance Generic PatchFace _

instance Show PatchFace where
  show x = genericShow x

instance Eq PatchFace where
  eq x = genericEq x

nextPathFace :: PatchFace -> PatchFace
nextPathFace FaceUp = FaceDown
nextPathFace FaceDown = FaceUp

prevPathFace :: PatchFace -> PatchFace
prevPathFace FaceUp = FaceDown
prevPathFace FaceDown = FaceUp

--------------------------------------------------------------------------------
-- adjustPatchLayout
--------------------------------------------------------------------------------

adjustPatchLayout :: QuiltPos -> PatchOrientation -> PatchFace -> PatchLayout -> PatchLayout
adjustPatchLayout pos ori face = shiftPatchLayout pos ∘ orientPatchLayout ori ∘ facePatchLayout face

orientPatchLayout :: PatchOrientation -> PatchLayout -> PatchLayout
orientPatchLayout North = identity
orientPatchLayout East = Set.map $ lmap $ over QuiltPos \(x /\ y) -> y /\ -x
orientPatchLayout West = Set.map $ lmap $ over QuiltPos \(x /\ y) -> -y /\ x
orientPatchLayout South = Set.map $ lmap $ over QuiltPos \(x /\ y) -> -x /\ -y

facePatchLayout :: PatchFace -> PatchLayout -> PatchLayout
facePatchLayout FaceUp = identity
facePatchLayout FaceDown = Set.map $ lmap $ over QuiltPos \(x /\ y) -> -x /\ -y

shiftPatchLayout :: QuiltPos -> PatchLayout -> PatchLayout
shiftPatchLayout (QuiltPos (dx /\ dy)) = Set.map $ lmap $ over QuiltPos \(x /\ y) -> (x + dx) /\ (y + dy)

--------------------------------------------------------------------------------
-- standardPatches
--------------------------------------------------------------------------------

standardPatches :: Map PatchId Patch
standardPatches = Map.fromFoldable $ mapWithIndex (\i patch -> (PatchId i /\ patch)) $
  [ Patch
      { timePrice: 2
      , buttonPrice: 2
      , patchLayout: Set.fromFoldable
          [ QuiltPos (0 /\ 0) /\ false
          , QuiltPos (0 /\ 1) /\ true
          ]
      , patchStyle: SolidColorPatchStyle (HSvgA.RGB 100 255 100)
      }
  , Patch
      { timePrice: 2
      , buttonPrice: 2
      , patchLayout: Set.fromFoldable
          [ QuiltPos (0 /\ 0) /\ false
          , QuiltPos (0 /\ 1) /\ true
          ]
      , patchStyle: SolidColorPatchStyle (HSvgA.RGB 100 255 100)
      }
  , Patch
      { timePrice: 2
      , buttonPrice: 2
      , patchLayout: Set.fromFoldable
          [ QuiltPos (0 /\ 0) /\ false
          , QuiltPos (0 /\ 1) /\ true
          ]
      , patchStyle: SolidColorPatchStyle (HSvgA.RGB 100 255 100)
      }
  , Patch
      { timePrice: 2
      , buttonPrice: 2
      , patchLayout: Set.fromFoldable
          [ QuiltPos (0 /\ 0) /\ false
          , QuiltPos (0 /\ 1) /\ true
          ]
      , patchStyle: SolidColorPatchStyle (HSvgA.RGB 100 255 100)
      }
  , Patch
      { timePrice: 2
      , buttonPrice: 2
      , patchLayout: Set.fromFoldable
          [ QuiltPos (0 /\ 0) /\ false
          , QuiltPos (0 /\ 1) /\ true
          ]
      , patchStyle: SolidColorPatchStyle (HSvgA.RGB 100 255 100)
      }
  , Patch
      { timePrice: 2
      , buttonPrice: 2
      , patchLayout: Set.fromFoldable
          [ QuiltPos (0 /\ 0) /\ false
          , QuiltPos (0 /\ 1) /\ true
          ]
      , patchStyle: SolidColorPatchStyle (HSvgA.RGB 100 255 100)
      }
  , Patch
      { timePrice: 1
      , buttonPrice: 1
      , patchLayout: Set.fromFoldable
          [ QuiltPos (0 /\ 0) /\ true
          , QuiltPos (1 /\ 0) /\ false
          , QuiltPos (0 /\ 1) /\ false
          ]
      , patchStyle: SolidColorPatchStyle (HSvgA.RGB 255 100 100)
      }
  , Patch
      { timePrice: 2
      , buttonPrice: 2
      , patchLayout: Set.fromFoldable
          [ QuiltPos (0 /\ 0) /\ false
          , QuiltPos (0 /\ 1) /\ true
          ]
      , patchStyle: SolidColorPatchStyle (HSvgA.RGB 100 255 100)
      }
  , Patch
      { timePrice: 2
      , buttonPrice: 2
      , patchLayout: Set.fromFoldable
          [ QuiltPos (0 /\ 0) /\ false
          , QuiltPos (0 /\ 1) /\ true
          ]
      , patchStyle: SolidColorPatchStyle (HSvgA.RGB 100 255 100)
      }
  , Patch
      { timePrice: 2
      , buttonPrice: 2
      , patchLayout: Set.fromFoldable
          [ QuiltPos (0 /\ 0) /\ false
          , QuiltPos (0 /\ 1) /\ true
          ]
      , patchStyle: SolidColorPatchStyle (HSvgA.RGB 100 255 100)
      }
  ]

initialCircle :: Int -> Map PatchId Patch -> Circle
initialCircle _seed patches =
  case patches # Map.keys # List.fromFoldable of
    Nil -> unsafeCrashWith "initialCircle with no patches"
    Cons focus_ items -> todo "Circle { focus: focus_, items }" {}

--------------------------------------------------------------------------------
-- GameResult
--------------------------------------------------------------------------------

data GameResult = Win PlayerId | Tie

derive instance Generic GameResult _

instance Show GameResult where
  show x = genericShow x

instance Eq GameResult where
  eq x = genericEq x

--------------------------------------------------------------------------------
-- GameMessage
--------------------------------------------------------------------------------

data GameMessage
  = LogGameMessage String
  | WarningGameMessage String

--------------------------------------------------------------------------------
-- Config
--------------------------------------------------------------------------------

newtype Config = Config
  { quiltSize :: Int
  , calcWaitResult :: { waitTime :: Int } -> { rewardButtons :: Int }
  }

_Config = _Newtype :: Iso' Config _
_quiltSize = prop (Proxy :: Proxy "quiltSize")
_calcWaitResult = prop (Proxy :: Proxy "calcWaitResult")

derive instance Newtype Config _
-- derive newtype instance Show Config
