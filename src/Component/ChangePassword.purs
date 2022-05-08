module Component.ChangePassword where

import Prelude

import Affjax (Error, Response, printError)
import Affjax as Ajax
import Affjax.RequestBody as RequestBody
import Affjax.ResponseFormat as ResponseFormat
import Api.Logon (LogonRequest(..), LogonResponse(..), LogonResults(..))
import AppTheme (paperColor, themeColor, themeFont)
import CSS (borderRadius, column, gray, paddingBottom, pct, px, rem, vw)
import CSS.Background (backgroundColor)
import CSS.Box (boxShadow)
import CSS.Color (rgba, white)
import CSS.Common (center)
import CSS.Cursor (cursor, notAllowed, pointer)
import CSS.Display (display, flex)
import CSS.Flexbox (alignItems, flexDirection, flexGrow, justifyContent, row)
import CSS.Font (FontWeight(..), color, fontSize, fontWeight)
import CSS.Geometry (height, paddingLeft, paddingRight, paddingTop, width)
import CSS.Missing (spaceEvenly)
import CSS.Property (value)
import Capability.Log (class Log, log, logD)
import Capability.LogonRoute (class LogonRoute, PasswordType(..), logonRoute)
import Capability.Navigate (class Navigate, navigate)
import Control.Monad.Except (runExcept)
import Control.Monad.Reader (class MonadAsk, ask)
import Data.Bifunctor (lmap)
import Data.Const (Const)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Route (Route)
import Data.Route as Component
import Data.Route as Route
import Data.String (trim)
import Effect.Aff.Class (class MonadAff)
import Effect.Ref as Ref
import Env (Env)
import Foreign.Generic (class Encode, decodeJSON, encodeJSON)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS as HC
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties (InputType(..))
import Halogen.HTML.Properties as HP
import Image.BookCover (bookCover)
import Web.HTML as HTML
import Web.HTML.Window as Window

type Input = Unit
type Output = Void

type Slots :: ∀ k. Row k
type Slots = ()

type State = {}

type Query :: ∀ k. k -> Type
type Query = Const Void

data Action = DoChangePassword

component :: ∀ m
    . MonadAff m
    => MonadAsk Env m
    => Navigate m Route
    => Log m
    => H.Component Query Input Output m
component = H.mkComponent
  { initialState: \_ -> {}
  , render
  , eval: H.mkEval H.defaultEval {
      handleAction = handleAction
    }
  }
  where
    handleAction :: Action -> H.HalogenM State Action Slots Output m Unit
    handleAction = case _ of
      DoChangePassword -> do
        logD "Clicked on Change Password"
        navigate $ Route.Users Nothing

    render :: State -> H.ComponentHTML Action Slots m
    render _ =
      HH.div [
        HC.style do
          display flex
          flexDirection row
          alignItems center
          justifyContent center
          paddingTop $ vw 0.65
      ]
      [ HH.button
        [
          HC.style do
            backgroundColor themeColor
            themeFont
            fontWeight $ FontWeight $ value "500"
            fontSize $ vw 1.0
            width (rem 20.0)
            height $ vw 3.0
            color gray
            cursor pointer
          , HE.onClick $ const DoChangePassword
          ]
          [ HH.text "CHANGE PASS" ]
      ]