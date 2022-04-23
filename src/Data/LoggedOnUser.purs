module Data.LoggedOnUser where

import Prelude

import Data.UUID (UUID)

type LoggedOnUser =
  { authToken :: UUID
  }