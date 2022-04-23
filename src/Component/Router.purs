module Component.Router where

import Prelude

import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS as HC
import Halogen.HTML.Events as HE

type Input = Unit
type Output = Unit
type State = { }


data Query :: ∀ k. k -> Type
data Query a

type Slots :: ∀ k. Row k
type Slots = ()

data Action = Initialize

component :: ∀ m. H.Component Query Input Output m
component = H.mkComponent
  { initialState: \_ -> {}
  , render
  , eval: H.mkEval H.defaultEval
    { initialize = Just Initialize
    , handleAction = handleAction
    }
  }
  where
    handleAction :: Action -> H.HalogenM State Action Slots Output m Unit
    handleAction _ = pure unit

    render :: State -> H.ComponentHTML Action Slots m
    render _ =
      HH.text "Hello world"