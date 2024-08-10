module Data.TotalMap
  ( TotalMap
  , new
  , at'
  ) where

import Prelude

import Data.Enum (class BoundedEnum, enumFromTo)
import Data.Lens (Lens, Lens', lens')
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (maybe')
import Data.Newtype (over)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Partial.Unsafe (unsafeCrashWith)

newtype TotalMap k v = TotalMap (Map k v)

new :: forall k v. BoundedEnum k => (k -> v) -> TotalMap k v
new f =
  (enumFromTo bottom top :: Array k)
    # map (\k -> k /\ f k)
    # Map.fromFoldable
    # TotalMap

at' :: forall k v. Ord k => k -> Lens' (TotalMap k v) v
at' k = lens' \(TotalMap m) ->
  Tuple
    (m # Map.lookup k # maybe' (\_ -> unsafeCrashWith "impossible: key not found in TotalMap") identity)
    (\v -> TotalMap (m # Map.insert k v))

