module Patchwork.App.Simple1 where

import Patchwork.Interaction
import Prelude

import Control.Monad.Free (runFreeM)
import Data.Array ((..))
import Data.Foldable (traverse_)
import Data.Newtype (unwrap, wrap)
import Effect (Effect)
import Effect.Aff (delay)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class.Console as Console
import Halogen (HalogenM, modify_)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver as HVD
import Patchwork.Model (PlayerId(..), TurnAction(..))
import Patchwork.Util (todo)
import Web.UIEvent.MouseEvent (MouseEvent)

main :: Effect Unit
main = HA.runHalogenAff (HVD.runUI component {} =<< HA.awaitBody)

type Input = {}

type State =
  { count :: Int
  }

data Action = Start MouseEvent

component :: forall query output m. MonadAff m => H.Component query Input output m
component = H.mkComponent { initialState, eval, render }
  where
  initialState :: Input -> State
  initialState _ =
    { count: 0
    }

  eval = H.mkEval H.defaultEval { handleAction = handleAction }

  handleAction = case _ of
    Start _event -> do
      void $ runInteractionT (inject (ChooseTurnAction { target: PlayerId true, k: pure }))
      pure unit

  render { count } =
    HH.div
      [ HP.style "display: flex; flex-direction: column; gap: 0.5em" ]
      [ HH.button
          [ HE.onClick Start ]
          [ HH.text "Start" ]
      , HH.div [] [ HH.text $ "count: " <> show count ]
      ]

runInteractionT
  :: forall action slots output m a
   . MonadAff m
  => InteractionT (HalogenM State action slots output m) a
  -> HalogenM State action slots output m a
runInteractionT = unwrap >>> runFreeM
  ( case _ of
      Lift_InteractionF (Lift ma) -> ma
      ChooseTurnAction_InteractionF (ChooseTurnAction { target: _, k }) -> do
        0 .. 10 # traverse_ \i -> do
          Console.log $ "count: " <> show i
          modify_ \env -> env { count = env.count + 1 }
          liftAff (delay (1000.0 # wrap))
        k { selection: Pass }
      PlacePatch_InteractionF (PlacePatch {}) -> todo ""
      ChooseWaitDuration_InteractionF (ChooseWaitDuration {}) -> todo ""
      SetWinner_InteractionF (SetWinner {}) -> todo ""
  )
