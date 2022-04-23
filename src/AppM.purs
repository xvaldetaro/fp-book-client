module AppM where

import Prelude

import Capability.Log (class Log, LogLevel)
import Capability.LogonRoute (class LogonRoute, PasswordType(..), logonRoute)
import Capability.Navigate (class Navigate)
import Control.Monad.Reader (class MonadAsk, ReaderT, runReaderT)
import Data.JSDate (getTime, now)
import Data.Maybe (Maybe(..))
import Data.Route (Route, routeCodec)
import Data.Route as Route
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Class.Console as Console
import Effect.Now (nowDateTime)
import Env (Env)
import Routing.Duplex (RouteDuplex', print)
import Routing.Hash (setHash)

newtype AppM a = AppM (ReaderT Env Aff a)
derive newtype instance functorAppM :: Functor AppM
derive newtype instance applyAppM :: Apply AppM
derive newtype instance applicativeAppM :: Applicative AppM
derive newtype instance bindAppM :: Bind AppM
derive newtype instance monadAppM :: Monad AppM
derive newtype instance monadEffectAppM :: MonadEffect AppM
derive newtype instance monadAffAppM :: MonadAff AppM
derive newtype instance monadAskAppM :: MonadAsk Env AppM

runAppM :: ∀ a. Env -> AppM a -> Aff a
runAppM env (AppM reader) = runReaderT reader env

instance logonRouteAppM :: LogonRoute AppM Route where
  logonRoute PasswordPermanent = pure $ Route.Users Nothing
  logonRoute PasswordTemporary = pure Route.ChangePassword

instance navigateAppM :: Navigate AppM Route where
  navigate = liftEffect <<< setHash <<< print routeCodec

instance logAppM :: Log AppM where
  log = Console.log <<< show
  logEntry level message =
    liftEffect
      $ { level, message, timestamp: _ }
      <$> nowDateTime



