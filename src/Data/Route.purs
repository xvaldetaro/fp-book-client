module Data.Route where

import Prelude hiding ((/))

import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import Routing.Duplex (RouteDuplex', optional, path, root, segment)
import Routing.Duplex.Generic (noArgs, sum)
import Routing.Duplex.Generic.Syntax ((/))

data Route
  = Logon
  | Logoff
  | Users (Maybe String)
  | ChangePassword

derive instance genericRoute :: Generic Route _

-- userId :: RouteDuplex' String -> RouteDuplex' UserId
-- userId = as printer parser
--   where
--     printer :: UserId -> String
--     printer (UserId uuid) = UUID.toString uuid
--     parser :: String -> Either String UserId
--     parser = UUID.parseUUID >>> map UserId >>> note "invalid uuid"

routeCodec :: RouteDuplex' Route
routeCodec = root $ sum
  { "Logon": path "logon" noArgs
  , "Logoff": path "logoff" noArgs
  -- , "Users": "users" / (optional $ userId segment)
  , "Users": "users" / (optional segment)
  , "ChangePassword": path "change-password" noArgs
  }