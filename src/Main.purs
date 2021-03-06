module Main where

import Prelude

import AppM (runAppM)
import Component.ChangePassword as ChangePassword
import Component.Modal.Message as Message
import Component.Modal.Modal as Modal
import Component.Router as Router
import Data.Maybe (Maybe(..))
import Data.Route (routeCodec)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Ref as Ref
import Halogen as H
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Routing.Duplex as Duplex
import Routing.Hash (matchesWith)

main :: Effect Unit
main = do
  HA.runHalogenAff do
    body <- HA.awaitBody
    userRef <- H.liftEffect $ Ref.new Nothing
    let router = H.hoist (runAppM { userRef }) Router.component
    io <- runUI router unit body
    H.liftEffect $ matchesWith (Duplex.parse routeCodec) $
      \_ new -> launchAff_ $ io.query (H.mkTell $ Router.Navigate new)
    -- let other = H.hoist (runAppM { userRef }) (Modal.component CreateUser.component)
    -- io <- runUI other unit body
    -- pure unit


