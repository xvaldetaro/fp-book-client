module Component.Logon where

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
import Capability.Log (class Log, logD)
import Capability.LogonRoute (class LogonRoute, PasswordType(..), logonRoute)
import Capability.Navigate (class Navigate, navigate)
import Control.Monad.Except (runExcept)
import Control.Monad.Reader (class MonadAsk, ask)
import Data.Bifunctor (lmap)
import Data.Const (Const)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.String (trim)
import Data.UUID (UUID)
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
import Util as Utils
import Web.HTML as HTML
import Web.HTML.Window as Window

type Input
  = Unit

type Output
  = Void

type Slots :: ∀ k. Row k
type Slots
  = ()

type State
  = { userName :: String, password :: String }

type Query :: ∀ k. k -> Type
type Query
  = Const Void

data Action
  = Logon
  | Input (State -> State)

type LogonSuccessPayload
  = { authToken :: UUID, admin :: Boolean , mustChangePassword :: Boolean}

component ::
  ∀ m route.
  MonadAff m =>
  LogonRoute m route =>
  MonadAsk Env m =>
  Navigate m route =>
  Log m =>
  H.Component Query Input Output m
component =
  H.mkComponent
    { initialState: \_ -> { userName: "", password: "" }
    , render
    , eval:
        H.mkEval
          H.defaultEval
            { handleAction = handleAction
            }
    }
  where
  handleAction :: Action -> H.HalogenM State Action Slots Output m Unit
  handleAction = case _ of
    Input f -> H.modify_ f
    Logon -> do
      { userName, password } <- H.get
      response <- Utils.postJson $ LogonRequest $ { userName, password }
      case unpackResponse response of
        Left errString -> alertError errString
        Right { authToken, mustChangePassword, admin } -> do
          logD $ "successful logon:" <> show authToken
          { userRef } <- ask
          H.liftEffect $ Ref.write (Just { authToken, admin }) userRef
          navigate <=< logonRoute
            $ if mustChangePassword then PasswordTemporary else PasswordPermanent
      where
      unpackResponse ::
        Either String LogonResponse ->
        Either String LogonSuccessPayload
      unpackResponse response =
        response
          >>= case _ of
              LogonResponse LogonResultsFailure -> Left "Invalid logon credentials"
              LogonResponse (LogonResultsSuccess x) -> Right x

      alertError :: String -> H.HalogenM State Action Slots Output m Unit
      alertError msg = H.liftEffect $ HTML.window >>= Window.alert msg

  render :: State -> H.ComponentHTML Action Slots m
  render { userName, password } =
    HH.div
      [ HC.style do
          display flex
          flexDirection column
          width $ vw 22.0
          height $ vw 25.0
          paddingTop $ vw 1.25
          paddingBottom $ vw 1.25
          paddingLeft $ vw 1.50
          paddingRight $ vw 1.50
          backgroundColor paperColor
          borderRadius (px 7.0) (px 7.0) (px 7.0) (px 7.0)
          boxShadow (px 10.0) (px 10.0) (px 20.0) (rgba 0 0 24 0.75)
      ]
      [ HH.div
          [ HC.style do
              paddingRight $ vw 1.0
              display flex
              flexDirection column
              alignItems center
              justifyContent center
              flexGrow 10.0
          ]
          [ HH.img
              [ HC.style do
                  width $ pct 40.0
              , HP.src bookCover
              ]
          ]
      , HH.div
          [ HC.style do
              display flex
              flexDirection column
              justifyContent spaceEvenly
              alignItems center
              flexGrow 3.0
          ]
          [ HH.div
              [ HC.style do
                  display flex
                  flexDirection row
                  alignItems center
                  justifyContent center
                  themeFont
              ]
              [ HH.label
                  [ HC.style do
                      width (rem 6.0)
                  ]
                  [ HH.text "Username: " ]
              , HH.input
                  [ HC.style do
                      backgroundColor paperColor
                      width (vw 8.3)
                      paddingLeft (rem 0.5)
                      paddingRight (rem 0.5)
                      fontSize (vw 1.0)
                  , HE.onValueInput $ Input <<< \s -> _ { userName = s }
                  ]
              ]
          , HH.div
              [ HC.style do
                  display flex
                  flexDirection row
                  alignItems center
                  justifyContent center
                  themeFont
              ]
              [ HH.label
                  [ HC.style do
                      width (rem 6.0)
                  ]
                  [ HH.text "Password: " ]
              , HH.input
                  [ HP.type_ InputPassword
                  , HC.style do
                      backgroundColor paperColor
                      width (vw 8.3)
                      paddingLeft (rem 0.5)
                      paddingRight (rem 0.5)
                      fontSize (vw 1.0)
                  , HE.onValueInput $ Input <<< \s -> _ { password = s }
                  ]
              ]
          ]
      , HH.div
          [ HC.style do
              display flex
              flexDirection row
              alignItems center
              justifyContent center
              paddingTop $ vw 0.65
          ]
          [ HH.button
              [ HC.style do
                  backgroundColor themeColor
                  themeFont
                  fontWeight $ FontWeight $ value "500"
                  fontSize $ vw 1.0
                  width (rem 20.0)
                  height $ vw 3.0
                  color if logonDisabled then gray else white
                  cursor if logonDisabled then notAllowed else pointer
              , HE.onClick $ const Logon
              , HP.disabled logonDisabled
              ]
              [ HH.text "LOGON" ]
          ]
      ]
    where
    logonDisabled = trim userName == "" || trim password == ""
