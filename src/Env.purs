module Env where

import Prelude

import Data.LoggedOnUser (LoggedOnUser)
import Data.Maybe (Maybe)
import Effect.Ref (Ref)

type Env =
  { userRef :: Ref (Maybe LoggedOnUser)
  }