module Patchwork.App.Simple1 where

import Prelude

import Control.Monad.Free (Free, runFreeM)
import Control.Monad.State (StateT, execStateT, get, modify_, runStateT)
import Control.Monad.Trans.Class (lift)
import Data.Array as Array
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (over, wrap)
import Data.TotalMap as TotalMap
import Data.Tuple.Nested ((/\))
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
import Patchwork.Interaction (ChooseTurnAction(..), ChooseWaitDuration(..), InteractionF(..), InteractionT(..), Lift(..), PlacePatch(..), SetWinner(..), inject)
import Patchwork.Model (Model(..), Player(..), PlayerId(..), TurnAction(..))
import Patchwork.Util (todo)
import Type.Proxy (Proxy(..))
import Web.UIEvent.MouseEvent (MouseEvent)

main :: Effect Unit
main = HA.runHalogenAff (HVD.runUI component {} =<< HA.awaitBody)

type Input = {}

type State =
  { count :: Int
  , mb_widget :: Maybe Widget
  , model :: Model
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
-- data WidgetOutput = WidgetOutput (Aff (Free (InteractionF Aff) Unit))
data WidgetOutput = WidgetOutput (StateT Model Aff (Free (InteractionF (StateT Model Aff)) Unit))

component :: forall query. H.Component query Input Output Aff
component = H.mkComponent { initialState, eval, render }
  where
  initialState :: Input -> State
  initialState _ =
    { count: 0
    , model: Model
        { patches: Map.empty
        , maxTime: 0
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
      -- blocking sequencing works!
      runInteraction $ void $ do
        void $ inject $ ChooseTurnAction { target: PlayerId false, k: pure }
        void $ inject $ ChooseTurnAction { target: PlayerId true, k: pure }
      pure unit
    WidgetOutput_Action (WidgetOutput m) -> do
      modify_ _ { mb_widget = Nothing }
      { model } <- get
      fm /\ model' <- m # flip runStateT model # liftAff
      modify_ _ { model = model' }
      runInteraction (InteractionT fm)
      pure unit

  render { count, mb_widget, model: Model model } =
    HH.div
      [ HP.style "display: flex; flex-direction: column; gap: 0.5em" ]
      ( [ [ HH.div [] [ HH.text $ "activePlayer: " <> show model.activePlayer ] ]
        , [ HH.button [ HE.onClick Start ] [ HH.text "Start" ] ]
        , [ HH.div [] [ HH.text $ "count: " <> show count ] ]
        , mb_widget # maybe [] \widget ->
            [ HH.slot (Proxy :: Proxy "widget") unit widget {} WidgetOutput_Action
            ]
        ] # Array.fold
      )

runInteraction
  :: InteractionT (StateT Model Aff) Unit
  -> HalogenM State Action Slots Output Aff Unit
runInteraction (InteractionT fm) = do
  { model } <- get
  fm # runFreeM case _ of
    Lift_InteractionF (Lift ma) -> do
      model' <- execStateT ma model # lift
      modify_ _ { model = model' }
      pure (pure unit)
    ChooseTurnAction_InteractionF (ChooseTurnAction { target, k }) -> do
      modify_ \env -> env { model = env.model # over Model _ { activePlayer = target } }
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
    PlacePatch_InteractionF (PlacePatch {}) -> todo ""
    ChooseWaitDuration_InteractionF (ChooseWaitDuration {}) -> todo ""
    SetWinner_InteractionF (SetWinner {}) -> todo ""
