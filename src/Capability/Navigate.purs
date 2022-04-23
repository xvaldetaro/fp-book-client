module Capability.Navigate where

import Prelude

import Data.Route (Route)
import Halogen (HalogenM, lift)

class Monad m <= Navigate m route where
  navigate :: route -> m Unit

instance navigateHalogenM
    :: Navigate m route
    => Navigate (HalogenM s a sl o m) route where
  navigate = lift <<< navigate