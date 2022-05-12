module Component.Message where

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
import Halogen.HTML.Core (ClassName(..))
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties (InputType(..))
import Halogen.HTML.Properties as HP
import Image.BookCover (bookCover)
import Web.HTML as HTML
import Web.HTML.Window as Window

type Input = String
type Output = Void

type Slots :: ∀ k. Row k
type Slots = ()

type State = { message :: String }

type Query :: ∀ k. k -> Type
type Query = Const Void

data Action = Input String

component :: ∀ m
    . MonadAff m
    => MonadAsk Env m
    => Navigate m Route
    => Log m
    => H.Component Query Input Output m
component = H.mkComponent
  { initialState: \message -> {message}
  , render
  , eval: H.mkEval H.defaultEval {
      handleAction = handleAction,
      receive = Just <<< Input
    }
  }
  where
    handleAction :: Action -> H.HalogenM State Action Slots Output m Unit
    handleAction = case _ of
      Input message -> do
        H.modify_ _ { message = message }

    render :: State -> H.ComponentHTML Action Slots m
    render { message } =
      HH.p
        [ HP.class_ $ ClassName "card-text" ]
        [ HH.text message ]