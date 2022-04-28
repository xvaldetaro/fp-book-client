module Capability.Log where

import Prelude

import Data.DateTime (DateTime)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
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

logD :: ∀ m. Log m => String -> m Unit
logD = log <=< logEntry Debug

instance logHalogenM :: Log m => Log (HalogenM s a sl o m) where
  log = lift <<< log
  logEntry level = lift <<< logEntry level