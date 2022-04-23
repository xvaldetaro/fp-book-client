module Capability.LogonRoute where

import Prelude

import Data.UUID (UUID)
import Halogen (HalogenM, lift)

data PasswordType = PasswordPermanent | PasswordTemporary

class Monad m <= LogonRoute m route where
  logonRoute :: PasswordType -> m route

instance logonRouteHalogenM
    :: LogonRoute m route
    => LogonRoute (HalogenM s a sl o m) route where
  logonRoute = lift <<< logonRoute