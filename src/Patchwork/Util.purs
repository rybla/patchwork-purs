module Patchwork.Util where

import Prelude

import Partial.Unsafe (unsafeCrashWith)

infixr 9 compose as ∘

todo :: forall a. String -> a
todo msg = unsafeCrashWith ("TODO: " <> msg)

