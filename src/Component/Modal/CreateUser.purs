module Component.Modal.CreateUser where

import Prelude

import Api.User (User(..))
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
import Halogen (ClassName(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS as HC
import Halogen.HTML.Core (ClassName(..))
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as Halogen.HTML.Properties.ARIA

type Input = Unit
type Output = Maybe User

type InternalOutput = Modal.InnerOutputInternal Output

type Slots :: ∀ k. Row k
type Slots = ()

type UserFields =
  { userName :: String
  , admin :: Boolean
  , firstName :: String
  , lastName :: String
  }

type State = { userFields :: UserFields }

type Query :: ∀ k. k -> Type
type Query = Const Void

data Action
  = Initialize
  | FormInput (UserFields -> UserFields)

component
  :: ∀ m route
   . MonadAff m
  => MonadAsk Env m
  => Navigate m route
  => Log m
  => H.Component Query Input InternalOutput m
component = H.mkComponent
  { initialState: \_ ->
      { userFields:
          { userName: ""
          , firstName: ""
          , lastName: ""
          , admin: false
          }
      }
  , render
  , eval: H.mkEval H.defaultEval
      { handleAction = handleAction
      , initialize = Just Initialize
      }
  }
  where
  handleAction :: Action -> H.HalogenM State Action Slots InternalOutput m Unit
  handleAction = case _ of
    Initialize -> H.raise $ Modal.SetModalConfig $ Modal.defaultConfig
      { buttonDisplay = Modal.DisplayBothButtons, isAffirmativeDisabled = true }
    FormInput f -> H.modify_ (\s -> s { userFields = f s.userFields })

  render :: State -> H.ComponentHTML Action Slots m
  render _ =
    HH.form_
      [ textFormGroup "First Name" "John" HP.InputText (\v uf -> uf { firstName = v }) ]
    where
    textFormGroup
      :: ∀ w
       . String
      -> String
      -> HP.InputType
      -> (String -> UserFields -> UserFields)
      -> HH.HTML w Action
    textFormGroup label placeholder inputType mutateFields =
      HH.div
        [ HP.class_ $ ClassName "form-group" ]
        [ HH.label_ [ HH.text label ]
        , HH.input
            [ HP.type_ inputType
            , HP.class_ $ ClassName "form-control"
            , HP.placeholder placeholder
            , HE.onValueInput $ \v -> FormInput $ mutateFields v
            ]
        ]
