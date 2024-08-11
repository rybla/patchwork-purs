module Patchwork.App.Simple1 where

import Patchwork.Interaction
import Prelude

import Control.Bind (bindFlipped)
import Control.Monad.Free (Free, runFreeM)
import Control.Monad.State (modify_)
import Control.Monad.Trans.Class (lift)
import Data.Array ((..))
import Data.Array as Array
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (unwrap)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class.Console as Console
import Halogen (HalogenM, Slot, liftAff)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver as HVD
import Patchwork.Model (PlayerId(..), TurnAction(..))
import Patchwork.Util (todo, (∘))
import Type.Proxy (Proxy(..))
import Web.UIEvent.MouseEvent (MouseEvent)

main :: Effect Unit
main = HA.runHalogenAff (HVD.runUI component {} =<< HA.awaitBody)

type Input = {}

type State =
  { count :: Int
  , activePlayer :: PlayerId
  , mb_widget :: Maybe Widget
  }

data Action
  = Start MouseEvent
  | WidgetOutput_Action WidgetOutput

type Slots =
  ( widget :: WidgetSlot Unit
  )

type Output = Void

type WidgetSlot = Slot WidgetQuery WidgetOutput

type Widget = H.Component WidgetQuery WidgetInput WidgetOutput Aff
data WidgetQuery a = WidgetQuery a
type WidgetInput = {}
data WidgetOutput = WidgetOutput (Aff (Free (InteractionF Aff) Unit))

component :: forall query. H.Component query Input Output Aff
component = H.mkComponent { initialState, eval, render }
  where
  initialState :: Input -> State
  initialState _ =
    { count: 0
    , activePlayer: bottom
    , mb_widget: Nothing
    }

  eval = H.mkEval H.defaultEval { handleAction = handleAction }

  handleAction = case _ of
    Start _event -> do
      -- blocking sequencing works!
      runInteraction $ void $ do
        void $ inject $ ChooseTurnAction { target: PlayerId false, k: pure }
        void $ inject $ ChooseTurnAction { target: PlayerId true, k: pure }
      pure unit
    WidgetOutput_Action (WidgetOutput m) -> do
      modify_ _ { mb_widget = Nothing }
      m
        # liftAff
        # bindFlipped (runInteraction ∘ InteractionT)
      pure unit

  render { count, mb_widget, activePlayer } =
    HH.div
      [ HP.style "display: flex; flex-direction: column; gap: 0.5em" ]
      ( [ [ HH.div [] [ HH.text $ "activePlayer: " <> show activePlayer ] ]
        , [ HH.button [ HE.onClick Start ] [ HH.text "Start" ] ]
        , [ HH.div [] [ HH.text $ "count: " <> show count ] ]
        , mb_widget # maybe [] \widget ->
            [ HH.slot (Proxy :: Proxy "widget") unit widget {} WidgetOutput_Action
            ]
        ] # Array.fold
      )

runInteraction
  :: InteractionT Aff Unit
  -> HalogenM State Action Slots Output Aff Unit
runInteraction = unwrap >>> runFreeM
  ( case _ of
      Lift_InteractionF (Lift ma) -> lift ma
      ChooseTurnAction_InteractionF (ChooseTurnAction { target, k }) -> do
        modify_ _ { activePlayer = target }
        let
          widget = H.mkComponent { initialState, eval, render }
            where
            initialState _ = {}
            eval = H.mkEval H.defaultEval
              { handleAction = \selection -> do
                  Console.log $ "selection: " <> show selection
                  H.raise (WidgetOutput $ k { selection })
              }
            render {} =
              HH.div
                [ HP.style "display: flex; flex-direction: column: gap: 0.5em; border: 0.1em solid black; padding: 0.5em;" ]
                [ HH.text "this is a widget"
                , HH.button [ HE.onClick (const Buy) ] [ HH.text "Buy" ]
                , HH.button [ HE.onClick (const Wait) ] [ HH.text "Wait" ]
                , HH.button [ HE.onClick (const Pass) ] [ HH.text "Pass" ]
                ]
        modify_ _ { mb_widget = Just widget }
        pure (pure unit)
      _ -> todo ""
  )
