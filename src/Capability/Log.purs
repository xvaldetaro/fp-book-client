module Capability.Log where

import Prelude

import Data.DateTime (DateTime)
import Data.Generic.Rep (class Generic)
import Data.JSDate (getTime, now)
import Data.Show.Generic (genericShow)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Class.Console as Console
import Halogen (HalogenM, lift)

data LogLevel = Debug | Info | Warning | Error
derive instance genericLogLevel :: Generic LogLevel _
instance showLogLevel :: Show LogLevel where
  show = genericShow

type LogEntry =
  { level :: LogLevel
  , message :: String
  , timestamp :: DateTime
  }

class Monad m <= Log m where
  logEntry :: LogLevel -> String -> m LogEntry
  log :: LogEntry -> m Unit

instance logHalogenM :: Log m => Log (HalogenM s a sl o m) where
  log = lift <<< log
  logEntry level = lift <<< logEntry level