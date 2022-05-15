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
<<<<<<< Updated upstream
import Data.Maybe (Maybe(..))
=======
import Data.Either (Either(..), note)
import Data.Maybe (Maybe(..), maybe)
import Data.Route (Route)
import Data.Route as Route
import Data.String (null)
import Data.Tuple (Tuple(..))
import Data.UUID (UUID)
>>>>>>> Stashed changes
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

<<<<<<< Updated upstream
type Slots :: ∀ k. Row k
type Slots = ()
=======
type Slots = (errorModal :: H.Slot (Modal.InnerQuery Void) (Modal.Output Message.Output) Unit)
_errorModal = Proxy :: Proxy "errorModal"
>>>>>>> Stashed changes

type UserFields =
  { userName :: String
  , admin :: Boolean
  , firstName :: String
  , lastName :: String
  }

<<<<<<< Updated upstream
type State = { userFields :: UserFields }

type Query :: ∀ k. k -> Type
type Query = Const Void

data Action
  = Initialize
  | FormInput (UserFields -> UserFields)
=======
data Action
  = Initialize
  | FormInput (State -> State)
  | ErrorModalExit (Modal.Output Message.Output)
  | RouteToLogon

data CreateUserErrors
  = MissingAuthToken
  | NotAdmin
  | ServerReturnedFailure String
  | DecodeError String
>>>>>>> Stashed changes

component
  :: ∀ m route
   . MonadAff m
  => MonadAsk Env m
  => Navigate m route
  => Log m
  => H.Component (Modal.InnerQuery Void) Input InternalOutput m
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
      , handleQuery = handleQuery
      }
  }
  where
  handleQuery
    :: ∀ a. Modal.InnerQuery Void a -> H.HalogenM State Action Slots InternalOutput m (Maybe a)
  handleQuery (Modal.AffirmativeClicked a) = do
    state <- H.get
    successEither <- H.lift $ runExceptT do
      { authToken, admin } <- getUserToken <#> note MissingAuthToken # liftSuccess
      unless admin $ throwError NotAdmin
      CreateUserResponse results <- postJson (createRequest authToken state)
        <#> (lmap \s -> DecodeError s)
        # liftSuccess
      case results of
        CreateUserResultsSuccess -> pure unit
        CreateUserResultsFailure { reason } -> throwError $ ServerReturnedFailure $ show reason
    case successEither of
      Left err ->
        let
          (Tuple action errString) = case err of
            MissingAuthToken -> Tuple (Just RouteToLogon) "Session Expired"
            NotAdmin -> Tuple Nothing "User is not admin"
            ServerReturnedFailure s -> Tuple Nothing $ "Failed request: " <> s
            DecodeError s -> Tuple Nothing $ "Decoding response failed: " <> s
        in
          H.modify_ _ { errorMessage = Just errString, postErrorModalExitAction = action }
      Right _ -> H.raise $ Modal.PassThrough $ createUser state
    pure $ Just a
  handleQuery (Modal.NegativeClicked a) = H.raise Modal.CloseNegative *> pure (Just a)
  handleQuery _ = pure Nothing

  handleAction :: Action -> H.HalogenM State Action Slots InternalOutput m Unit
  handleAction = case _ of
<<<<<<< Updated upstream
    Initialize -> H.raise $ Modal.SetModalConfig $ Modal.defaultConfig
      { buttonDisplay = Modal.DisplayBothButtons, isAffirmativeDisabled = true }
    FormInput f -> H.modify_ (\s -> s { userFields = f s.userFields })
=======
    Initialize -> H.raise $ Modal.SetModalConfig _
      { buttonDisplay = Modal.DisplayBothButtons
      , isAffirmativeDisabled = true
      , affirmativeLabel = "Create User"
      }
    FormInput f -> do
      H.modify_ f
      s <- H.get
      if shouldEnableAffirmative s then H.raise $ Modal.SetModalConfig _
        { isAffirmativeDisabled = false }
      else H.raise $ Modal.SetModalConfig _ { isAffirmativeDisabled = shouldEnableAffirmative s }
    RouteToLogon -> navigate Route.Logon
    ErrorModalExit _ -> do
      { postErrorModalExitAction } <- H.get
      H.modify_ _ { errorMessage = Nothing }
      maybe (pure unit) handleAction postErrorModalExitAction
    where
    shouldEnableAffirmative s = null s.firstName && null s.lastName
>>>>>>> Stashed changes

  render :: State -> H.ComponentHTML Action Slots m
  render _ =
    HH.form_
<<<<<<< Updated upstream
      [ textFormGroup "First Name" "John" HP.InputText (\v uf -> uf { firstName = v }) ]
=======
      [ textFormGroup "First Name" "John" HP.InputText (\v uf -> uf { firstName = v })
      , textFormGroup "Last Name" "Appleseed" HP.InputText (\v uf -> uf { lastName = v })
      , textFormGroup "Username" "johnappleseed" HP.InputText (\v uf -> uf { userName = v })
      , textFormGroup "Password" "" HP.InputPassword (\v uf -> uf { password = v })
      , HH.div
          [ HP.class_ $ ClassName "form-check" ]
          [ HH.input
              [ HP.type_ InputCheckbox
              , HP.class_ $ ClassName "form-check-input"
              , HE.onChecked $ \v -> FormInput $ \uf -> uf { admin = v }
              ]
          , HH.label
              [ HP.class_ $ ClassName "form-check-label" ]
              [ HH.text "Is Admin" ]
          ]
      , case errorMessage of
          Nothing -> HH.text ""
          Just x -> HH.slot _errorModal unit (Modal.component Message.component) x ErrorModalExit
      ]
>>>>>>> Stashed changes
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
