module Patchwork.App.Common where

import Patchwork.Model
import Prelude

import Data.Array as Array
import Data.Int as Int
import Data.Maybe (fromMaybe')
import Data.Set as Set
import Data.Tuple.Nested ((/\))
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Svg.Attributes as HSvgP
import Halogen.Svg.Attributes.Color as Color
import Halogen.Svg.Elements as HSvg
import Patchwork.Util (bug)

renderPatch :: forall w i. Patch -> HH.HTML w i
renderPatch (Patch patch) =
  HH.div
    [ HP.style "display: flex; flex-direction: column; gap: 0.5em;" ]
    [ HH.div [] [ HH.text $ "button price: " <> show patch.buttonPrice ]
    , HH.div [] [ HH.text $ "duration price: " <> show patch.durationPrice ]
    , renderQuiltLayout patch.quiltLayout
    ]

renderQuiltLayout :: forall w i. QuiltLayout -> HH.HTML w i
renderQuiltLayout quiltLayout =
  let
    QuiltPos (x_max /\ _) /\ _ = quiltLayout
      # Set.toUnfoldable
      # Array.sortBy (\((QuiltPos (x1 /\ _)) /\ _) ((QuiltPos (x2 /\ _)) /\ _) -> compare x1 x2)
      # Array.last
      # fromMaybe' (\_ -> bug "empty quiltLayout")
    QuiltPos (_ /\ y_max) /\ _ = quiltLayout
      # Set.toUnfoldable
      # Array.sortBy (\((QuiltPos (_ /\ y1)) /\ _) ((QuiltPos (_ /\ y2)) /\ _) -> compare y1 y2)
      # Array.last
      # fromMaybe' (\_ -> bug "empty quiltLayout")
  in
    HSvg.svg
      [ HSvgP.width (Int.toNumber (x_max + 1) * size)
      , HSvgP.height (Int.toNumber (y_max + 1) * size)
      ]
      ( quiltLayout
          # Set.toUnfoldable
          # map
              ( \((QuiltPos (x /\ y)) /\ btn) ->
                  HSvg.rect
                    [ HSvgP.x (Int.toNumber x * size)
                    , HSvgP.y (Int.toNumber y * size)
                    , HSvgP.width size
                    , HSvgP.height size
                    , HSvgP.fill
                        if btn then
                          Color.RGB 0 255 0
                        else
                          Color.RGB 0 0 255
                    ]
              )
      )
  where
  size = 40.0
