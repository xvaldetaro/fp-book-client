module Component.Logon where

import Prelude

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
import Component.Message as Message
import Component.Modal as Modal
import Control.Monad.Reader (class MonadAsk, ask)
import Data.Const (Const)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.String (trim)
import Data.UUID (UUID)
import Effect.Aff.Class (class MonadAff)
import Effect.Ref as Ref
import Env (Env)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS as HC
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties (InputType(..))
import Halogen.HTML.Properties as HP
import Image.BookCover (bookCover)
import Type.Proxy (Proxy(..))
import Util as Utils

type Input = Unit

type Output = Void

type Slots = (modal :: H.Slot Query (Modal.Output Message.Output) Unit)
_modal = Proxy :: Proxy "modal"

type State = { userName :: String, password :: String, errorMessage :: Maybe String }

type Query :: ∀ k. k -> Type
type Query = Const Void

data Action
  = Logon
  | Input (State -> State)
  | DidReceiveModalOutput (Modal.Output Message.Output)

type LogonSuccessPayload = { authToken :: UUID, admin :: Boolean, mustChangePassword :: Boolean }

component
  :: ∀ m route
   . MonadAff m
  => LogonRoute m route
  => MonadAsk Env m
  => Navigate m route
  => Log m
  => H.Component Query Input Output m
component =
  H.mkComponent
    { initialState: \_ -> { userName: "", password: "", errorMessage: Nothing }
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
    DidReceiveModalOutput output ->
      case output of
        _ -> H.modify_ _ { errorMessage = Nothing }
    Logon -> do
      { userName, password } <- H.get
      response <- Utils.postJson $ LogonRequest $ { userName, password }
      case unpackResponse response of
        Left errString -> H.modify_ _ { errorMessage = Just errString }
        Right { authToken, mustChangePassword, admin } -> do
          logD $ "successful logon:" <> show authToken
          { userRef } <- ask
          H.liftEffect $ Ref.write (Just { authToken, admin }) userRef
          navigate <=< logonRoute
            $ if mustChangePassword then PasswordTemporary else PasswordPermanent
      where
      unpackResponse
        :: Either String LogonResponse
        -> Either String LogonSuccessPayload
      unpackResponse response =
        response
          >>= case _ of
            LogonResponse LogonResultsFailure -> Left "Invalid logon credentials"
            LogonResponse (LogonResultsSuccess x) -> Right x

  render :: State -> H.ComponentHTML Action Slots m
  render { userName, password, errorMessage } =
    HH.div_
      [ mainRender
      , case errorMessage of
          Nothing -> HH.text ""
          Just x -> HH.slot _modal unit (Modal.component Message.component) x DidReceiveModalOutput
      ]
    where
    mainRender =
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

    logonDisabled = trim userName == "" || trim password == ""
