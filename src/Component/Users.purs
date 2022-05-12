module Component.Users where

import Prelude

import Affjax (Error, Response, printError)
import Affjax as Ajax
import Affjax.RequestBody as RequestBody
import Affjax.ResponseFormat as ResponseFormat
import Api.Logon (LogonRequest(..), LogonResponse(..), LogonResults(..))
import Api.QueryUsers (QueryUsersRequest(..), QueryUsersResponse(..), QueryUsersResults(..))
import Api.User (User(..))
import AppTheme (paperColor, selectedColor, themeColor, themeFont)
import CSS (borderRadius, column, flex, gray, paddingBottom, pct, px, rem, vw)
import CSS.Background (backgroundColor)
import CSS.Box (boxShadow)
import CSS.Color (rgba, white)
import CSS.Common (center)
import CSS.Cursor (cursor, notAllowed, pointer)
import CSS.Display (display, flex)
import CSS.Flexbox (alignItems, flexBasis, flexDirection, flexGrow, flexStart, justifyContent, row)
import CSS.Font (FontWeight(..), color, fontSize, fontWeight)
import CSS.Geometry (height, minWidth, padding, paddingLeft, paddingRight, paddingTop, width)
import CSS.Missing (spaceEvenly)
import CSS.Property (value)
import Capability.Log (class Log, log, logD)
import Capability.LogonRoute (class LogonRoute, PasswordType(..), logonRoute)
import Capability.Navigate (class Navigate, navigate)
import Control.Monad.Except (runExcept, runExceptT, throwError)
import Control.Monad.Reader (class MonadAsk, ask)
import Data.Array (foldr)
import Data.Array as Array
import Data.Bifunctor (bimap, lmap, rmap)
import Data.Const (Const)
import Data.Either (Either(..), hush, note)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.Route (Route, routeCodec)
import Data.Route as Component
import Data.Route as Route
import Data.String (trim)
import Data.Tuple (Tuple(..))
import Data.UUID (UUID)
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
import Routing.Duplex (parse)
import Routing.Hash (getHash)
import Undefined (undefined)
import Util (getUserToken, liftSuccess, postJson)
import Web.HTML (window)
import Web.HTML as HTML
import Web.HTML.Window (alert)
import Web.HTML.Window as Window

-- The userName for this route
type Input = Maybe String

type Output = Void

type Slots :: ∀ k. Row k
type Slots = ()

type State =
  { authorized :: Boolean
  , selectedUser :: Maybe User
  , users :: Map String User
  , initUserName :: Maybe String
  }

type Query :: ∀ k. k -> Type
type Query = Const Void

data Action
  = DidTapUserRow User
  | DidReceiveSelectedUserInput (Maybe String)
  | Initialize

component
  :: ∀ m
   . MonadAff m
  => MonadAsk Env m
  => Navigate m Route
  => Log m
  => H.Component Query Input Output m
component =
  H.mkComponent
    { initialState: \initUserName ->
        { users: Map.empty, authorized: false, selectedUser: Nothing, initUserName }
    , render
    , eval:
        H.mkEval
          H.defaultEval
            { handleAction = handleAction
            , initialize = Just Initialize
            , receive = (\selectedName -> Just $ DidReceiveSelectedUserInput $ selectedName)
            }
    }
  where
  handleAction :: Action -> H.HalogenM State Action Slots Output m Unit
  handleAction = case _ of
    DidTapUserRow (User { userName }) -> do
      logD $ "Tapped user: " <> userName
      navigate $ Route.Users $ Just userName
    DidReceiveSelectedUserInput userNameMaybe -> do
      { users } <- H.get
      let userMaybe = userNameMaybe >>= flip Map.lookup users
      H.modify_ _ { selectedUser = userMaybe }
    Initialize -> do
      usersEither <- H.lift getUsers
      logD $ "Users: " <> show usersEither
      case usersEither of
        Left err -> do
          H.modify_ _ { authorized = false }
          alertError err
        Right users -> do
          { initUserName } <- H.get
          let userMap = Map.fromFoldable $ users <#> \u@(User u') -> Tuple u'.userName u
          let selectedUser = initUserName >>= flip Map.lookup userMap
          H.modify_ _ { users = userMap, authorized = true, selectedUser = selectedUser }
      where
      getUsers :: m (Either String (Array User))
      getUsers =
        runExceptT do
          { authToken, admin } <- getUserToken <#> note "Missing authtoken" #
            liftSuccess
          unless admin $ throwError "Not authorized"
          QueryUsersResponse results <-
            postJson (QueryUsersRequest { authToken })
              # liftSuccess
          case results of
            QueryUsersResultsFailure { reason } -> throwError $ show reason
            QueryUsersResultsSuccess { users } -> pure users
    where
    alertError :: String -> H.HalogenM State Action Slots Output m Unit
    alertError msg = H.liftEffect $ window >>= alert msg

  render :: State -> H.ComponentHTML Action Slots m
  render { authorized, users, selectedUser } =
    if not authorized then
      HH.text "NOT AUTHORIZED"
    else
      page
    where
    mainContent = case selectedUser of
      Nothing -> HH.text "No user"
      Just (User { userName }) -> HH.text $ "user: " <> userName
    page =
      HH.div
        [ HC.style do
            display flex
            flexDirection row
            flexGrow 1.0
        ]
        [ HH.div
            [ HC.style do
                display flex
                flexDirection column
                minWidth (rem 20.0)
                paddingRight (rem 2.0)
            ]
            [ HH.ul_
                ( (Array.fromFoldable $ Map.values users)
                    <#> \user@(User { userName }) ->
                      HH.li
                        [ HP.class_ $ ClassName "list-group-item"
                        , HC.style do
                            backgroundColor
                              if Just user == selectedUser then selectedColor
                              else paperColor
                            cursor pointer
                        , HE.onClick $ const $ DidTapUserRow user
                        ]
                        [ HH.text userName ]
                )
            ]
        , selectedUser # maybe (HH.text "") \(User user) ->
            let
              item h = HH.div [ HC.style $ paddingBottom (rem 0.5) ] [ h ]
            in
              HH.div
                [ HC.style do
                    display flex
                    flexDirection row
                    flexBasis (pct 100.0)
                    backgroundColor paperColor
                    padding (rem 2.0) (rem 2.0) (rem 2.0) (rem 2.0)
                ]
                [ HH.div
                    [ HC.style do
                        display flex
                        flexDirection column
                        justifyContent flexStart
                        alignItems flexStart
                        minWidth (rem 10.0)

                    ]
                    [ item $ HH.text "User Name:"

                    , item $ HH.text "Name:"

                    , item $ HH.text "Administrator:"

                    ]
                , HH.div
                    [ HC.style do
                        display flex
                        flexDirection column
                        justifyContent flexStart
                        alignItems flexStart
                    ]
                    [ item $ HH.text user.userName
                    , item $ HH.text $ user.firstName <> " " <> user.lastName
                    , item $ HH.text if user.admin then "Yes" else "No"
                    ]

                ]
        ]

-- mainLayoutWithSidePanel sidePanelLayout mainContentLayout
--   where
--   mainContentLayout =
--     HH.div
--       [ HC.style do
--           display flex
--           flexDirection row
--           alignItems center
--           justifyContent center
--           paddingTop $ vw 0.65
--       ]
--       [ HH.button
--           [ HC.style do
--               backgroundColor themeColor
--               themeFont
--               fontWeight $ FontWeight $ value "500"
--               fontSize $ vw 1.0
--               width (rem 20.0)
--               height $ vw 3.0
--               color gray
--               cursor pointer
--           , HE.onClick $ const DoSelectUser
--           ]
--           [ HH.text "User Profile" ]
--       ]
--   sidePanelLayout =
--     HH.div
--       [ HC.style do
--           display flex
--           flexDirection column
--       ]
--       $ HH.text <<< show <$> users
--   mainLayoutWithSidePanel panelHtml contentHtml =
--     HH.div
--       [ HC.style do
--           display flex
--           flexDirection row
--       ]
--       [ panelHtml
--       , contentHtml
--       ]
