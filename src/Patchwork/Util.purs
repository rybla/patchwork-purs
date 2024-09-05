module Patchwork.Util where

import Prelude

import Partial.Unsafe (unsafeCrashWith)

infixr 9 compose as ∘

todo :: forall a b. String -> a -> b
todo msg = unsafeCrashWith ("TODO: " <> msg)

bug :: forall a. String -> a
bug msg = unsafeCrashWith ("BUG: " <> msg)
