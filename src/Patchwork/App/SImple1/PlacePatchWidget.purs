module Patchwork.App.SImple1.PlacePatchWidget where

import Patchwork.Interaction
import Patchwork.Model
import Prelude

import Control.Monad.Free (Free, runFreeM)
import Control.Monad.State (StateT, get, modify_, runStateT)
import Control.Monad.Trans.Class (lift)
import Data.Array as Array
import Data.Lens ((^.))
import Data.Lens.Record (prop)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe', maybe)
import Data.Newtype (wrap)
import Data.Three as Three
import Data.TotalMap (at')
import Data.TotalMap as TotalMap
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff as Aff
import Effect.Class.Console as Console
import Halogen (liftAff)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver as HVD
import Partial.Unsafe (unsafeCrashWith)
import Patchwork.App.Common (renderPatch, renderPlayer)
import Patchwork.Logic as Logic
import Patchwork.Util (todo, (∘))
import Type.Proxy (Proxy(..))

