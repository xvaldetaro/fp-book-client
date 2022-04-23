module Main where

import Prelude

import Component.Router as Router
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Halogen.Aff as HA
import Halogen.Subscription as HS
import Halogen.VDom.Driver (runUI)

main :: Effect Unit
main = do
  HA.runHalogenAff do
    body <- HA.awaitBody
    runUI Router.component unit body
