module Patchwork.App where

import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH

component :: forall query input output m. MonadAff m => H.Component query input output m
component = H.mkComponent { initialState, eval, render }
  where
  initialState _ = {}

  eval = H.mkEval H.defaultEval

  render _ = HH.div [] [ HH.text "<App/>" ]
