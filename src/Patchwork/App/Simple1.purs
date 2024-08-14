module Patchwork.App.Simple1 where

import Patchwork.Interaction
import Patchwork.Model
import Prelude

import Control.Monad.Free (Free, runFreeM)
import Control.Monad.State (StateT, get, modify, modify_, runStateT)
import Control.Monad.Trans.Class (lift)
import Data.Array as Array
import Data.Foldable (any)
import Data.Lens ((%=), (^.))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe', maybe)
import Data.Newtype (wrap)
import Data.Set as Set
import Data.Three as Three
import Data.TotalMap (at')
import Data.TotalMap as TotalMap
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff as Aff
import Effect.Class.Console as Console
import Halogen (liftAff, liftEffect)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Query.Event as HQE
import Halogen.VDom.Driver as HVD
import Partial.Unsafe (unsafeCrashWith)
import Patchwork.App.Common (renderPatch, renderPlayer, renderQuilt)
import Patchwork.Logic as Logic
import Patchwork.Util (bug, todo, (∘))
import Type.Proxy (Proxy(..))
import Web.Event.Event as Event
import Web.HTML as Web
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.Window as Window
import Web.UIEvent.KeyboardEvent (KeyboardEvent)
import Web.UIEvent.KeyboardEvent as KE
import Web.UIEvent.KeyboardEvent.EventTypes as KET

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
  | Keyboard_Action KeyboardEvent

type Slots =
  ( widget :: WidgetSlot Unit
  )

type Output = Void

type WidgetSlot = H.Slot WidgetQuery WidgetOutput

type Widget = H.Component WidgetQuery WidgetInput WidgetOutput Aff

data WidgetQuery a
  = Pure_WidgetQuery a
  | Keyboard_WidgetQuery KeyboardEvent a

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
                , buttons: 5
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
      -- registry event listener for keyboard
      document <- Web.window >>= Window.document # liftEffect
      H.subscribe' \_ ->
        HQE.eventListener
          KET.keyup
          (HTMLDocument.toEventTarget document)
          (map Keyboard_Action <<< KE.fromEvent)
      -- start main game logic
      runInteraction $ Logic.main unit
    WidgetOutput_Action (WidgetOutput m) -> do
      modify_ _ { mb_widget = Nothing }
      Aff.delay (wrap 100.0) # liftAff -- delay for 100ms to make it feel like things are happening when you click a button, ya know
      { model } <- get
      fm /\ model' <- m # flip runStateT model # H.liftAff
      modify_ _ { model = model' }
      runInteraction (InteractionT fm)
      pure unit
    Keyboard_Action ke -> do
      ke # KE.toEvent # Event.preventDefault # liftEffect
      H.tell (Proxy :: Proxy "widget") unit $ Keyboard_WidgetQuery ke

  render { mb_widget, model: Model model } =
    HH.div
      [ HP.style "padding: 1em; display: flex; flex-direction: column; gap: 1.0em;" ]
      ( [ [ HH.div [] [ HH.text $ "activePlayer: " <> show (Model model ^. activePlayer ∘ _Player ∘ prop _name) ] ]
        , [ HH.div
              [ HP.style "min-height: 6em;" ]
              ( mb_widget # maybe
                  []
                  \widget ->
                    [ HH.slot (Proxy :: Proxy "widget") unit widget {} WidgetOutput_Action
                    ]
              )
          ]
        , [ HH.div
              [ HP.style "display: flex; flex-direction: row; gap: 1.0em;" ]
              [ renderPlayer model.patches (model.players ^. at' bottom)
              , renderPlayer model.patches (model.players ^. at' top)
              ]
          ]
        , [ HH.div
              [ HP.style "display: flex; flex-direction: row; gap: 1.0em; flex-wrap: wrap;" ]
              let
                Circle { focus, items } = model.circle
                renderPatchId patchId =
                  let
                    patch = model.patches # Map.lookup patchId # fromMaybe' (\_ -> unsafeCrashWith "impossible: invalid patchId")
                  in
                    HH.div
                      [ HP.style "border: 0.1em solid black; padding: 1.0em;" ]
                      [ renderPatch patch ]
              in
                [ items
                    # map renderPatchId
                    # Array.fromFoldable
                , [ renderPatchId focus ]
                ]
                  # Array.fold
          ]
        , case model.winner of
            Nothing -> []
            Just winner -> [ HH.div [] [ HH.text $ "winner: " <> show winner ] ]
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
        { model } <- get
        spawnWidget (widget_ChooseTurnAction { model } k)
      ChoosePatchFromCircle_InteractionF (ChoosePatchFromCircle { k }) -> do
        { model } <- get
        spawnWidget (widget_ChoosePatchFromCircle { model } k)
      PlacePatch_InteractionF (PlacePatch { patchId, k }) -> do
        { model } <- get
        spawnWidget (widget_PlacePatch { model, patchId } k)
      _ -> todo "interpretation"

spawnWidget :: Widget -> H.HalogenM State Action Slots Output Aff (Free (InteractionF (StateT Model Aff)) Unit)
spawnWidget widget = do
  modify_ _ { mb_widget = Just widget }
  pure (pure unit)

widget_ChooseTurnAction
  :: { model :: Model }
  -> (ChooseTurnAction_Result -> StateT Model Aff (Free (InteractionF (StateT Model Aff)) Unit))
  -> Widget
widget_ChooseTurnAction { model: Model model } k = H.mkComponent { initialState, eval, render }
  where
  initialState _ =
    { mb_err: Nothing :: Maybe String
    }
  eval = H.mkEval H.defaultEval
    { handleAction = \{ selection } -> do
        case selection of
          Buy -> do
            -- in order to Buy, must be able to afford at least one of the next 3 patches
            let
              Player player = Model model # getActivePlayer
              canAfford (Patch patch) =
                player.buttons >= patch.buttonPrice &&
                  player.time >= patch.durationPrice
              p1 /\ p2 /\ p3 = model.circle # nextThreePatches
            if
              not
                ([ p1, p2, p3 ] # any \patchId -> canAfford (Model model # getPatch patchId)) then
              modify_ _ { mb_err = Just "You can't afford any of the available patches!" }
            else
              H.raise (WidgetOutput $ k { selection })
          Wait ->
            H.raise (WidgetOutput $ k { selection })
          Pass ->
            H.raise (WidgetOutput $ k { selection })
    }
  render { mb_err } =
    HH.div
      [ HP.style "display: flex; flex-direction: column; gap: 1.0em; border: 0.1em solid black; padding: 1.0em;" ]
      ( [ [ HH.div [] [ HH.text "Choose what to do on your turn." ]
          , HH.div
              [ HP.style "display: flex; flex-direction: row; gap: 1.0em;" ]
              [ HH.button [ HE.onClick (const { selection: Buy }) ] [ HH.text "Buy" ]
              , HH.button [ HE.onClick (const { selection: Wait }) ] [ HH.text "Wait" ]
              , HH.button [ HE.onClick (const { selection: Pass }) ] [ HH.text "Pass" ]
              ]
          ]
        , mb_err # maybe [] \err ->
            [ HH.div [ HP.style "color: red" ] [ HH.text err ] ]
        ] # Array.fold
      )

widget_ChoosePatchFromCircle
  :: { model :: Model }
  -> (ChoosePatchFromCircle_Result -> StateT Model Aff (Free (InteractionF (StateT Model Aff)) Unit))
  -> Widget
widget_ChoosePatchFromCircle { model: _ } k = H.mkComponent { initialState, eval, render }
  where
  initialState _ = {}
  eval = H.mkEval H.defaultEval
    { handleAction = \{ selection } -> do
        -- TODO: validate
        H.raise (WidgetOutput $ k { selection })
    }
  render {} =
    HH.div
      [ HP.style "display: flex; flex-direction: column; gap: 1.0em; border: 0.1em solid black; padding: 1.0em;" ]
      [ HH.div [] [ HH.text "Choose a patch from the circle." ]
      , HH.div
          [ HP.style "display: flex; flex-direction: row; gap: 1.0em;" ]
          [ HH.button [ HE.onClick (const { selection: Three.One }) ] [ HH.text "#1" ]
          , HH.button [ HE.onClick (const { selection: Three.Two }) ] [ HH.text "#2" ]
          , HH.button [ HE.onClick (const { selection: Three.Three }) ] [ HH.text "#3" ]
          ]
      ]

widget_PlacePatch
  :: { model :: Model, patchId :: PatchId }
  -> (PlacePatch_Result -> StateT Model Aff (Free (InteractionF (StateT Model Aff)) Unit))
  -> Widget
widget_PlacePatch { model: Model model, patchId } k = H.mkComponent { initialState, eval, render }
  where
  Patch patch = model.patches # Map.lookup patchId # fromMaybe' \_ -> bug "invalid PatchId"

  initialState _ =
    { quilt: Model model ^. activePlayer ∘ _Player ∘ prop _quilt
    , pos: QuiltPos (0 /\ 0)
    , ori: North
    , face: FaceUp
    }

  eval = H.mkEval H.defaultEval
    { handleQuery = case _ of
        Pure_WidgetQuery a -> pure (Just a)
        Keyboard_WidgetQuery ke a -> do
          case KE.key ke of
            -- flip
            "ArrowUp" | KE.shiftKey ke -> prop (Proxy :: Proxy "face") %= nextPathFace
            "ArrowDown" | KE.shiftKey ke -> prop (Proxy :: Proxy "face") %= nextPathFace
            -- turn
            "ArrowLeft" | KE.shiftKey ke -> prop (Proxy :: Proxy "ori") %= nextPatchOrientation
            "ArrowRight" | KE.shiftKey ke -> prop (Proxy :: Proxy "ori") %= prevPatchOrientation
            -- shift
            "ArrowUp" -> prop (Proxy :: Proxy "pos") ∘ _Newtype %= \(x /\ y) -> (x /\ (y - 1))
            "ArrowDown" -> prop (Proxy :: Proxy "pos") ∘ _Newtype %= \(x /\ y) -> (x /\ (y + 1))
            "ArrowLeft" -> prop (Proxy :: Proxy "pos") ∘ _Newtype %= \(x /\ y) -> ((x - 1) /\ y)
            "ArrowRight" -> prop (Proxy :: Proxy "pos") ∘ _Newtype %= \(x /\ y) -> ((x + 1) /\ y)
            _ -> pure unit
          pure (Just a)
    , handleAction = \{ pos, ori, face } -> do
        -- TODO: validate
        H.raise (WidgetOutput $ k { pos, ori, face })
    }

  render { quilt, pos, ori, face } =
    HH.div
      [ HP.style "display: flex; flex-direction: column; gap: 1.0em; border: 0.1em solid black; padding: 1.0em;" ]
      [ HH.div [] [ HH.text "Place the patch on your quilt." ]
      , HH.div []
          [ renderQuilt model.patches
              ( quilt # Map.union
                  ( patch.quiltLayout
                      # adjustQuiltLayout pos ori face
                      # Set.map (\(pos' /\ btn) -> (pos' /\ patchId /\ btn))
                      # Map.fromFoldable
                  )
              )
          ]
      , HH.div []
          [ HH.button
              [ HE.onClick (const { ori, pos, face }) ]
              [ HH.text "Submit" ]
          ]

      ]
