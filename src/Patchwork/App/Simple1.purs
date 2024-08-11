module Patchwork.App.Simple1 where

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
import Data.Maybe (Maybe(..), fromMaybe', maybe, maybe')
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
import Patchwork.Logic as Logic
import Patchwork.Util (todo, (∘))
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
  = Initialize
  | WidgetOutput_Action WidgetOutput

type Slots =
  ( widget :: WidgetSlot Unit
  )

type Output = Void

type WidgetSlot = H.Slot WidgetQuery WidgetOutput

type Widget = H.Component WidgetQuery WidgetInput WidgetOutput Aff
data WidgetQuery a = WidgetQuery a
type WidgetInput = {}
data WidgetOutput = WidgetOutput (StateT Model Aff (Free (InteractionF (StateT Model Aff)) Unit))

component :: forall query. H.Component query Input Output Aff
component = H.mkComponent { initialState, eval, render }
  where
  initialState :: Input -> State
  initialState _ =
    let
      patches = standardPatches
    in
      { model: Model
          { patches
          , circle: patches # initialCircle 0
          , activePlayer: bottom
          , players: TotalMap.fromFunctionWithIndex \i _ ->
              Player
                { name: [ "Alice", "Bob" ] Array.!! i # fromMaybe' (\_ -> unsafeCrashWith "impossible")
                , buttons: 0
                , quilt: Map.empty
                , time: 40
                }
          , winner: Nothing
          }
      , mb_widget: Nothing
      }

  eval = H.mkEval H.defaultEval { initialize = Just Initialize, handleAction = handleAction }

  handleAction = case _ of
    Initialize -> do
      -- let
      --   m _ = do
      --     void $ inject $ ChooseTurnAction { k: pure }
      --     void $ inject $ ChooseTurnAction { k: pure }
      --     m unit
      -- runInteraction $ void $ m unit
      runInteraction $ Logic.main unit
    WidgetOutput_Action (WidgetOutput m) -> do
      modify_ _ { mb_widget = Nothing }
      Aff.delay (wrap 100.0) # liftAff -- delay for 100ms to make it feel like things are happening when you click a button, ya know
      { model } <- get
      fm /\ model' <- m # flip runStateT model # H.liftAff
      modify_ _ { model = model' }
      runInteraction (InteractionT fm)
      pure unit

  render { mb_widget, model: Model model } =
    HH.div
      [ HP.style "display: flex; flex-direction: column; gap: 0.5em" ]
      ( [
          --
          -- game state 
          --
          [ HH.div [] [ HH.text $ "Player 1: " <> show (model.players ^. at' bottom) ] ]
        , [ HH.div [] [ HH.text $ "Player 2: " <> show (model.players ^. at' top) ] ]
        , [ HH.div [] [ HH.text $ "activePlayer: " <> show (model.players ^. at' model.activePlayer ∘ _Player ∘ prop _name) ] ]
        , [ HH.div [] [ HH.text $ "circle: " <> show model.circle ] ]
        , [ HH.div [] [ HH.text $ "winner: " <> show model.winner ] ]
        -- , [ HH.div [] [ HH.pre [ HP.style "white-space: pre-wrap;" ] [ HH.text (show (Model model)) ] ] ]
        --
        -- widget
        --
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
      ChooseTurnAction_InteractionF (ChooseTurnAction { k }) -> do
        let
          initialState _ = {}
          eval = H.mkEval H.defaultEval
            { handleAction = \{ selection } -> do
                Console.log $ "selection = " <> show selection
                H.raise (WidgetOutput $ k { selection })
            }
          render {} =
            HH.div
              [ HP.style "display: flex; flex-direction: column; gap: 0.5em; border: 0.1em solid black; padding: 0.5em;" ]
              [ HH.div [] [ HH.text "Choose what to do on your turn." ]
              , HH.div
                  [ HP.style "display: flex; flex-direction: row; gap: 0.5em;" ]
                  [ HH.button [ HE.onClick (const { selection: Buy }) ] [ HH.text "Buy" ]
                  , HH.button [ HE.onClick (const { selection: Wait }) ] [ HH.text "Wait" ]
                  , HH.button [ HE.onClick (const { selection: Pass }) ] [ HH.text "Pass" ]
                  ]
              ]
        spawnWidget (H.mkComponent { initialState, eval, render })
      ChoosePatchFromCircle_InteractionF (ChoosePatchFromCircle { k }) -> do
        -- { model: Model model } <- get
        let
          initialState _ = {}
          eval = H.mkEval H.defaultEval
            { handleAction = \{ selection } -> do
                Console.log $ "selection = " <> show selection
                H.raise (WidgetOutput $ k { selection })
            }
          render {} =
            HH.div
              [ HP.style "display: flex; flex-direction: column; gap: 0.5em; border: 0.1em solid black; padding: 0.5em;" ]
              [ HH.div [] [ HH.text "Choose a patch from the circle." ]
              , HH.div
                  [ HP.style "display: flex; flex-direction: row; gap: 0.5em;" ]
                  [ HH.button [ HE.onClick (const { selection: Three.One }) ] [ HH.text "#1" ]
                  , HH.button [ HE.onClick (const { selection: Three.Two }) ] [ HH.text "#2" ]
                  , HH.button [ HE.onClick (const { selection: Three.Three }) ] [ HH.text "#3" ]
                  ]
              ]
        spawnWidget (H.mkComponent { initialState, eval, render })
      PlacePatch_InteractionF (PlacePatch { patchId, k }) -> do
        -- { model: Model model } <- get
        let
          initialState _ = {}
          eval = H.mkEval H.defaultEval
            { handleAction = \{ pos, ori } -> do
                -- Console.log $ " = " <> show 
                H.raise (WidgetOutput $ k { pos, ori })
            }
          render {} =
            HH.div
              [ HP.style "display: flex; flex-direction: column; gap: 0.5em; border: 0.1em solid black; padding: 0.5em;" ]
              [ HH.div [] [ HH.text "Place the patch on your quilt." ]
              , HH.div
                  [ HP.style "display: flex; flex-direction: row; gap: 0.5em;" ]
                  [ HH.button [ HE.onClick (const { pos: QuiltPos (0 /\ 0), ori: North }) ] [ HH.text "example placement" ]
                  ]
              ]
        spawnWidget (H.mkComponent { initialState, eval, render })
      _ -> todo "interpretation"

spawnWidget :: Widget -> H.HalogenM State Action Slots Output Aff (Free (InteractionF (StateT Model Aff)) Unit)
spawnWidget widget = do
  modify_ _ { mb_widget = Just widget }
  pure (pure unit)
