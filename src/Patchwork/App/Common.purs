module Patchwork.App.Common where

import Patchwork.Model
import Prelude

import Data.Array ((..))
import Data.Array as Array
import Data.Foldable (foldMap)
import Data.Int as Int
import Data.Lens ((^.))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (fromMaybe')
import Data.Set as Set
import Data.TotalMap (at')
import Data.Tuple.Nested ((/\))
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Svg.Attributes as HSvgP
import Halogen.Svg.Elements as HSvg
import Patchwork.Util (bug, todo)

patchSquareSize = 20.0

renderPlayer :: forall w i. Model -> PlayerId -> HH.HTML w i
renderPlayer (Model model) playerId =
  HH.div
    [ HP.style "padding: 1em; border: 0.1em solid black; display: flex; flex-direction: column; gap: 1em;" ]
    [ HH.div [] [ HH.text player.name ]
    , HH.div [] [ HH.text ("time: " <> show player.time) ]
    , HH.div [] [ HH.text ("buttons: " <> show player.buttons) ]
    , HH.div [] [ HH.text ("bonus buttons: " <> show player.bonusButtons) ]
    , HH.div [] [ HH.text ("quilt buttons: " <> show (player.quilt # quiltButtons)) ]
    , HH.div [] [ HH.text ("previous turn: " <> show player.previousTurn) ]
    , HH.div [] [ HH.text ("score: " <> show (playerId # playerScore (Model model))) ]
    , renderQuilt model.patches player.quilt
    ]
  where
  Player player = model.players ^. at' playerId

renderQuilt :: forall w i. Map PatchId Patch -> Quilt -> HH.HTML w i
renderQuilt patches quilt =
  let
    x_max /\ y_max = (boardSize - 1) /\ (boardSize - 1)
  in
    HSvg.svg
      [ HSvgP.width (Int.toNumber (x_max + 1) * patchSquareSize)
      , HSvgP.height (Int.toNumber (y_max + 1) * patchSquareSize)
      ]
      ( [ (0 .. boardSize # foldMap \x -> 0 .. boardSize # map \y -> x /\ y)
            # foldMap \(x /\ y) ->
                renderPatchSquare BackgroundPatchStyle
                  { x: Int.toNumber x * patchSquareSize
                  , y: Int.toNumber y * patchSquareSize
                  , size: patchSquareSize
                  , btn: false
                  }
        , quilt
            # (Map.toUnfoldable :: _ -> Array _)
            # foldMap
                ( \(QuiltPos (x /\ y) /\ patchId /\ btn) ->
                    let
                      Patch patch = patches # Map.lookup patchId # fromMaybe' (\_ -> bug $ "invalid PatchId: " <> show patchId)
                    in
                      renderPatchSquare patch.patchStyle
                        { x: Int.toNumber x * patchSquareSize
                        , y: Int.toNumber y * patchSquareSize
                        , size: patchSquareSize
                        , btn
                        }
                )
        ] # Array.fold
      )

renderPatch :: forall w i. Patch -> HH.HTML w i
renderPatch (Patch patch) =
  HH.div
    [ HP.style "display: flex; flex-direction: column; gap: 0.5em;" ]
    [ HH.div [] [ HH.text $ "button price: " <> show patch.buttonPrice ]
    , HH.div [] [ HH.text $ "time price: " <> show patch.timePrice ]
    , renderPatchLayout patch.patchStyle patch.patchLayout
    ]

renderPatchLayout :: forall w i. PatchStyle -> PatchLayout -> HH.HTML w i
renderPatchLayout patchStyle patchLayout =
  let
    QuiltPos (x_max /\ _) /\ _ = patchLayout
      # Set.toUnfoldable
      # Array.sortBy (\((QuiltPos (x1 /\ _)) /\ _) ((QuiltPos (x2 /\ _)) /\ _) -> compare x1 x2)
      # Array.last
      # fromMaybe' (\_ -> bug "empty patchLayout")
    QuiltPos (_ /\ y_max) /\ _ = patchLayout
      # Set.toUnfoldable
      # Array.sortBy (\((QuiltPos (_ /\ y1)) /\ _) ((QuiltPos (_ /\ y2)) /\ _) -> compare y1 y2)
      # Array.last
      # fromMaybe' (\_ -> bug "empty patchLayout")
  in
    HSvg.svg
      [ HSvgP.width (Int.toNumber (x_max + 1) * patchSquareSize)
      , HSvgP.height (Int.toNumber (y_max + 1) * patchSquareSize)
      ]
      ( patchLayout
          # Set.toUnfoldable
          # map
              ( \((QuiltPos (x /\ y)) /\ btn) ->
                  renderPatchSquare patchStyle
                    { x: Int.toNumber x * patchSquareSize
                    , y: Int.toNumber y * patchSquareSize
                    , size: patchSquareSize
                    , btn
                    }
              )
          # Array.fold
      )

renderPatchSquare
  :: forall w i
   . PatchStyle
  -> { x :: Number
     , y :: Number
     , size :: Number
     , btn :: Boolean
     }
  -> Array (HH.HTML w i)
renderPatchSquare (SolidColorPatchStyle color) { x, y, size, btn } =
  [ [ HSvg.rect
        [ HSvgP.x x
        , HSvgP.y y
        , HSvgP.width size
        , HSvgP.height size
        , HSvgP.fill color
        ]
    ]
  , if not btn then []
    else
      let
        cx = x + (size / 2.0)
        cy = y + (size / 2.0)
        strokeWidth = size / 10.0
      in
        [ HSvg.circle
            [ HSvgP.cx cx
            , HSvgP.cy cy
            , HSvgP.r (size / 2.8)
            , HSvgP.fill (HSvgP.RGB 70 170 219)
            , HSvgP.stroke (HSvgP.RGB 0 0 0)
            , HSvgP.strokeWidth strokeWidth
            ]
        , HSvg.line
            [ HSvgP.x1 (cx - 1.5 * strokeWidth)
            , HSvgP.y1 cy
            , HSvgP.x2 (cx + 1.5 * strokeWidth)
            , HSvgP.y2 cy
            , HSvgP.stroke (HSvgP.RGB 0 0 0)
            , HSvgP.strokeWidth strokeWidth
            ]
        , HSvg.line
            [ HSvgP.x1 cx
            , HSvgP.y1 (cy - 1.5 * strokeWidth)
            , HSvgP.x2 cx
            , HSvgP.y2 (cy + 1.5 * strokeWidth)
            , HSvgP.stroke (HSvgP.RGB 0 0 0)
            , HSvgP.strokeWidth strokeWidth
            ]
        ]
  ] # Array.fold

renderPatchSquare BackgroundPatchStyle { x, y, size } =
  let
    strokeWidth = size / 10.0
  in
    [ HSvg.rect
        [ HSvgP.x x
        , HSvgP.y y
        , HSvgP.width size
        , HSvgP.height size
        , HSvgP.fill (HSvgP.RGB 234 182 118)
        -- , HSvgP.stroke (HSvgP.RGB 0 0 0)
        , HSvgP.stroke (HSvgP.RGB 158 113 62)
        , HSvgP.strokeWidth strokeWidth
        ]
    ]

