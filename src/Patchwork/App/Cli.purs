-- | simple CLI version for testing
module Patchwork.App.Cli where

import Prelude

import Control.Monad.Free (runFreeM)
import Control.Monad.State (class MonadState, StateT, evalStateT, gets)
import Control.Plus (empty)
import Data.Array as Array
import Data.Foldable (fold)
import Data.Lens (view)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Options ((:=))
import Data.Set as Set
import Data.String as String
import Data.TotalMap as TotalMap
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Class.Console as Console
import Node.Process as Process
import Node.ReadLine as RL
import Node.ReadLine.Aff as RLA
import Patchwork.Interaction (InteractionF(..), InteractionT(..), chooseTurnActionUnsafe)
import Patchwork.Model (Circle(..), Model(..), Player(..), TurnAction(..), _Player, _name, activePlayer, standardPatches)
import Patchwork.Util (todo)

main :: Effect Unit
main = launchAff_ do
  let
    m :: forall m. MonadEffect m => InteractionT (StateT Model m) Unit
    m = do
      action <- chooseTurnActionUnsafe unit
      Console.logShow { action }
      pure unit
  let
    model =
      Model
        { patches
        , activePlayerId: bottom
        , circle: Circle { patches: patches # Map.keys # Set.toUnfoldable }
        , players: TotalMap.fromFunction \id -> Player
            { name: show id
            , buttons: 5
            , previousTurn: 0
            , quilt: empty
            , time: 50 -- what is it actually?
            }
        , turn: 0
        , winner: empty
        }
      where
      patches = standardPatches
  _ <-
    m
      # runInteractionT
      # flip evalStateT model
  pure unit

runInteractionT :: InteractionT (StateT Model Aff) Unit -> StateT Model Aff Unit
runInteractionT (InteractionT fm) = do
  fm # runFreeM \interaction -> do
    case interaction of
      Lift ma -> ma
      ChooseTurnActionUnsafe _args k -> do
        turnAction <- getInput
          { choices: [ "buy", "wait" ]
          , defaultChoice: "buy"
          }
          "turn action: "
          case _ of
            "buy" -> pure Buy
            "wait" -> pure Wait
            _ -> empty
        k { turnAction }
      ChoosePatchFromMarketUnsafe args k -> todo "" {}
      PlacePatchUnsafe args k -> todo "" {}
      ChooseWaitTimeUnsafe args k -> todo "" {}
      PrintGameMessage args k -> todo "" {}

getInput
  :: forall m a
   . MonadAff m
  => MonadState Model m
  => { choices :: Array String
     , defaultChoice :: String
     }
  -> String
  -> (String -> Maybe a)
  -> m a
getInput args question parse = do
  interface <-
    RL.createInterface Process.stdin
      ( fold
          [ RL.output := Process.stdout
          , RL.completer := \input -> do
              let completions = args.choices # Array.filter \pre -> input # String.stripPrefix (String.Pattern pre) # isJust
              pure { completions, matched: completions # Array.head # fromMaybe args.defaultChoice }
          ]
      )
      # liftEffect
  name <- gets $ view $ activePlayer <<< _Player <<< _name
  input <- interface # RLA.question ("[" <> name <> "] " <> question) # liftAff
  case parse input of
    Nothing -> do
      Console.log $ "invalid choice; valid choices are: " <> show args.choices
      getInput args question parse
    Just result -> do
      interface # RL.close # liftEffect
      pure result

