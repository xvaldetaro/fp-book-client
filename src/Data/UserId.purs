module Data.UserId where

import Data.UUID (UUID)

newtype UserId = UserId UUID
