module Patchwork.Util where

import Prelude

import Control.Bind (bindFlipped)
import Control.Plus (empty)
import Data.Array as Array
import Data.Foldable (class Foldable, all, foldMap, foldr, minimumBy)
import Data.List (List(..))
import Data.List as List
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple.Nested ((/\))
import Partial.Unsafe (unsafeCrashWith)

infixr 9 compose as ∘

todo :: forall a b. String -> a -> b
todo msg = unsafeCrashWith ("TODO: " <> msg)

bug :: forall a. String -> a
bug msg = unsafeCrashWith ("BUG: " <> msg)

bug' :: forall a. String -> Unit -> a
bug' msg _ = bug msg

minimumsBy
  :: forall a f1 f2
   . Foldable f1
  => Functor f2
  => Applicative f2
  => Foldable f2
  => Monoid (f2 a)
  => (a -> a -> Ordering)
  -> f1 a
  -> f2 a
minimumsBy f = foldr
  ( \x ys -> ys # map (\y -> y /\ f x y) # foldMap case _ of
      z /\ LT -> pure z
      z /\ EQ -> pure z
      _ /\ GT -> mempty
  )
  mempty

fromSingletonList :: forall a. List a -> Maybe a
fromSingletonList (Cons x Nil) = pure x
fromSingletonList _ = empty

rotateArray :: forall a. Int -> Array a -> Array a
rotateArray n = Array.splitAt n >>> \{ before, after } -> after <> before
