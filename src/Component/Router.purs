module Component.Router where

import Prelude

import CSS (color, white)
import CSS.Flexbox (alignItems, flexStart, justifyContent, stretch)
import CSS.Geometry (padding)
import CSS.Size (rem)
import Capability.Log (class Log, logD)
import Capability.LogonRoute (class LogonRoute)
import Capability.Navigate (class Navigate, navigate)
import Component.ChangePassword as ChangePassword
import Component.Logon as Logon
import Component.Page as Page
import Component.Users as Users
import Control.Monad.Reader (class MonadAsk, ask)
import Data.Const (Const)
import Data.Maybe (Maybe(..), isNothing, maybe)
import Data.Route (Route(..))
import Data.Route as Route
import Effect.Aff.Class (class MonadAff)
import Effect.Ref as Ref
import Env (Env)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS as HC
import Type.Proxy (Proxy(..))

type Input = Unit

type Output = Void

type State = { route :: Route }

data Query a = Navigate Route a

type PageSlot = H.Slot (Const Void) Void Unit

type Slots =
  ( logon :: PageSlot
  , logoff :: PageSlot
  , users :: PageSlot
  , changePassword :: PageSlot
  )

_logon = Proxy :: Proxy "logon"

_logoff = Proxy :: Proxy "logoff"

_users = Proxy :: Proxy "users"

_changePassword = Proxy :: Proxy "changePassword"

data Action = Void

component
  :: ∀ m
   . MonadAff m
  => Navigate m Route
  => MonadAsk Env m
  => LogonRoute m Route
  => Log m
  => H.Component Query Input Output m
component =
  H.mkComponent
    { initialState: \_ -> { route: Logon }
    , render
    , eval:
        H.mkEval
          H.defaultEval
            { handleQuery = handleQuery
            }
    }
  where
  handleQuery :: ∀ a. Query a -> H.HalogenM State Action Slots Output m (Maybe a)
  handleQuery = case _ of
    Navigate route a -> do
      { userRef } <- ask
      maybeUser <- H.liftEffect $ Ref.read userRef
      if isNothing maybeUser then
        navigate Route.Logon
      else
        H.modify_ _ { route = route }
      pure (Just a)

  render { route } = case route of
    Logon -> HH.slot_ _logon unit (defaultPage Logon.component) unit
    Logoff -> HH.span [ HC.style $ color white ] [ HH.text "Logoff" ]
    Users userName -> HH.slot_ _users unit (wholePage Users.component) userName
    ChangePassword ->
      HH.slot_ _changePassword unit
        (defaultPage ChangePassword.component)
        unit
    where
    defaultPage = Page.component $ pure unit
    wholePage =
      Page.component do
        alignItems stretch
        justifyContent flexStart
        padding (rem 2.0) (rem 2.0) (rem 2.0) (rem 2.0)
