module Component.Modal.Message where

import Prelude

import CSS (spaceBetween)
import CSS.Display (display, flex)
import CSS.Flexbox (flexDirection, justifyContent, row)
import Capability.Log (class Log)
import Capability.Navigate (class Navigate)
import Component.Modal.Modal as Modal
import Control.Monad.Reader (class MonadAsk)
import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Env (Env)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS as HC
import Halogen.HTML.Core (ClassName(..))
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as Halogen.HTML.Properties.ARIA

type Input = String
type Output = Void
type InternalOutput = Modal.InnerOutputInternal Output

type Slots :: ∀ k. Row k
type Slots = ()

type State = { message :: String }

data Action
  = Input String
  | Initialize
  | DidTapCloseButton

component
  :: ∀ m route
   . MonadAff m
  => MonadAsk Env m
  => Navigate m route
  => Log m
  => H.Component (Modal.InnerQuery Void) Input InternalOutput m
component = H.mkComponent
  { initialState: \message -> { message }
  , render
  , eval: H.mkEval H.defaultEval
      { handleAction = handleAction
      , initialize = Just Initialize
      , receive = Just <<< Input
      }
  }
  where
  handleAction :: Action -> H.HalogenM State Action Slots InternalOutput m Unit
  handleAction = case _ of
    Input message -> do
      H.modify_ _ { message = message }
<<<<<<< Updated upstream
    Initialize -> H.raise $ Modal.SetModalConfig $ Modal.defaultConfig
      { buttonDisplay = Modal.DisplayAffirmative, isAffirmativeDisabled = true }
=======
    Initialize -> H.raise $ Modal.SetModalConfig _
      { buttonDisplay = Modal.DisplayAffirmative, affirmativeLabel = "Close" }
>>>>>>> Stashed changes
    DidTapCloseButton -> H.raise $ Modal.CloseAffirmative

  render :: State -> H.ComponentHTML Action Slots m
  render { message } =
    HH.div
      [ HC.style $ display flex *> flexDirection row *> justifyContent spaceBetween ]
      [ HH.p
          [ HP.class_ $ ClassName "card-text" ]
          [ HH.text message ]
      , HH.button
          [ HP.class_ $ ClassName "close"
          , HE.onClick $ const DidTapCloseButton
          ]
          [ HH.span [ Halogen.HTML.Properties.ARIA.hidden "true" ] [ HH.text "x" ] ]
      ]