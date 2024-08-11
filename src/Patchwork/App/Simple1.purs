module Patchwork.App.Simple1 where

import Patchwork.Interaction
import Patchwork.Model
import Prelude

import Control.Monad.Free (Free, runFreeM)
import Control.Monad.State (StateT, get, modify_, runStateT)
import Control.Monad.Trans.Class (lift)
import Data.Array as Array
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (wrap)
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
import Patchwork.Logic as Logic
import Patchwork.Util (todo)
import Type.Proxy (Proxy(..))
import Web.UIEvent.MouseEvent (MouseEvent)

main :: Effect Unit
main = HA.runHalogenAff (HVD.runUI component {} =<< HA.awaitBody)

type Input = {}

type State =
  { mb_widget :: Maybe Widget
  , model :: Model
  }

data Action
  = Start MouseEvent
  | WidgetOutput_Action WidgetOutput

type Slots =
  ( widget :: WidgetSlot Unit
  )

type Output = Void

type WidgetSlot = H.Slot WidgetQuery WidgetOutput

type Widget = H.Component WidgetQuery WidgetInput WidgetOutput Aff
data WidgetQuery a = WidgetQuery a
type WidgetInput = {}
-- data WidgetOutput = WidgetOutput (Aff (Free (InteractionF Aff) Unit))
data WidgetOutput = WidgetOutput (StateT Model Aff (Free (InteractionF (StateT Model Aff)) Unit))

component :: forall query. H.Component query Input Output Aff
component = H.mkComponent { initialState, eval, render }
  where
  initialState :: Input -> State
  initialState _ =
    { model: Model
        { patches: Map.empty
        , patchCircle: Map.empty
        , currentCirclePos: wrap 0
        , activePlayer: bottom
        , players: TotalMap.fromFunction \_ ->
            Player
              { name: "Bon"
              , buttons: 0
              , quilt: Map.empty
              , time: 0
              }
        , winner: Nothing
        }
    , mb_widget: Nothing
    }

  eval = H.mkEval H.defaultEval { handleAction = handleAction }

  handleAction = case _ of
    Start _event -> do
      -- let
      --   m _ = do
      --     void $ inject $ ChooseTurnAction { k: pure }
      --     void $ inject $ ChooseTurnAction { k: pure }
      --     m unit
      -- runInteraction $ void $ m unit
      runInteraction $ Logic.main unit
    WidgetOutput_Action (WidgetOutput m) -> do
      modify_ _ { mb_widget = Nothing }
      Aff.delay (wrap 100.0) # liftAff -- delay for 100ms to make it feel like things are happening, ya know
      { model } <- get
      fm /\ model' <- m # flip runStateT model # H.liftAff
      modify_ _ { model = model' }
      runInteraction (InteractionT fm)
      pure unit

  render { mb_widget, model: Model model } =
    HH.div
      [ HP.style "display: flex; flex-direction: column; gap: 0.5em" ]
      ( [ [ HH.div [] [ HH.text $ "activePlayer: " <> show model.activePlayer ] ]
        , [ HH.div [] [ HH.pre [ HP.style "white-space: pre-wrap;" ] [ HH.text (show (Model model)) ] ] ]
        , [ HH.button [ HE.onClick Start ] [ HH.text "Start" ] ]
        , mb_widget # maybe [] \widget ->
            [ HH.slot (Proxy :: Proxy "widget") unit widget {} WidgetOutput_Action
            ]
        ] # Array.fold
      )

runInteraction
  :: InteractionT (StateT Model Aff) Unit
  -> H.HalogenM State Action Slots Output Aff Unit
runInteraction (InteractionT fm) = do
  fm # runFreeM \interaction -> do
    Console.log $ "[runInteraction] " <> labelInteractionF interaction
    case interaction of
      Lift_InteractionF (Lift ma) -> do
        { model } <- get
        ma' /\ model' <- runStateT ma model # lift
        modify_ _ { model = model' }
        pure ma'
      ChooseTurnAction_InteractionF (ChooseTurnAction { k }) -> spawnWidget (H.mkComponent { initialState, eval, render })
        where
        initialState _ = {}
        eval = H.mkEval H.defaultEval
          { handleAction = \{ selection } -> do
              Console.log $ "selection = " <> show selection
              H.raise (WidgetOutput $ k { selection })
          }
        render {} =
          HH.div
            [ HP.style "display: flex; flex-direction: row; gap: 0.5em; border: 0.1em solid black; padding: 0.5em;" ]
            [ HH.text "choose turn action:"
            , HH.button [ HE.onClick (const { selection: Buy }) ] [ HH.text "Buy" ]
            , HH.button [ HE.onClick (const { selection: Wait }) ] [ HH.text "Wait" ]
            , HH.button [ HE.onClick (const { selection: Pass }) ] [ HH.text "Pass" ]
            ]
      ChoosePatchFromCircle_InteractionF (ChoosePatchFromCircle { k }) -> spawnWidget (H.mkComponent { initialState, eval, render })
        where
        initialState _ = {}
        eval = H.mkEval H.defaultEval
          { handleAction = \{ pos } -> do
              Console.log $ "pos = " <> show pos
              H.raise (WidgetOutput $ k { pos })
          }
        render {} =
          HH.div
            [ HP.style "display: flex; flex-direction: row; gap: 0.5em; border: 0.1em solid black; padding: 0.5em;" ]
            [ HH.text "choose patch from circle:"
            -- , HH.button [ HE.onClick (const ?a) ] [ HH.text "Buy" ]
            ]
      _ -> todo "interpretation"

spawnWidget :: Widget -> H.HalogenM State Action Slots Output Aff (Free (InteractionF (StateT Model Aff)) Unit)
spawnWidget widget = do
  modify_ _ { mb_widget = Just widget }
  pure (pure unit)
