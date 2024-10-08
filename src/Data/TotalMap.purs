module Data.TotalMap
  ( TotalMap
  , fromFunction
  , fromFunctionWithIndex
  , fromFunctionM
  , toUnfoldable
  , at'
  ) where

import Prelude

import Data.Argonaut (class EncodeJson)
import Data.Enum (class BoundedEnum, enumFromTo)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Lens (Lens', lens')
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (maybe')
import Data.Traversable (class Foldable, class Traversable, traverse)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested (type (/\), (/\))
import Data.Unfoldable (class Unfoldable)
import Partial.Unsafe (unsafeCrashWith)
import Patchwork.Util ((∘))

newtype TotalMap k v = TotalMap (Map k v)

derive newtype instance (Show k, Show v) => Show (TotalMap k v)
derive newtype instance Functor (TotalMap k)
derive newtype instance Foldable (TotalMap k)
derive newtype instance Traversable (TotalMap k)

derive newtype instance (EncodeJson k, Ord k, EncodeJson v) => EncodeJson (TotalMap k v)

toUnfoldable :: forall f k v. Unfoldable f => TotalMap k v -> f (k /\ v)
toUnfoldable (TotalMap m) = Map.toUnfoldable m

fromFunction :: forall k v. BoundedEnum k => (k -> v) -> TotalMap k v
fromFunction f =
  (enumFromTo bottom top :: Array k)
    # map (\k -> k /\ f k)
    # Map.fromFoldable
    # TotalMap

fromFunctionWithIndex :: forall k v. BoundedEnum k => (Int -> k -> v) -> TotalMap k v
fromFunctionWithIndex f =
  (enumFromTo bottom top :: Array k)
    # mapWithIndex (\i k -> k /\ f i k)
    # Map.fromFoldable
    # TotalMap

fromFunctionM :: forall m k v. Monad m => BoundedEnum k => (k -> m v) -> m (TotalMap k v)
fromFunctionM f =
  (enumFromTo bottom top :: Array k)
    # traverse (\k -> (k /\ _) <$> f k)
    # map (TotalMap ∘ Map.fromFoldable)

at' :: forall k v. Ord k => k -> Lens' (TotalMap k v) v
at' k = lens' \(TotalMap m) ->
  Tuple
    (m # Map.lookup k # maybe' (\_ -> unsafeCrashWith "impossible: key not found in TotalMap") identity)
    (\v -> TotalMap (m # Map.insert k v))

