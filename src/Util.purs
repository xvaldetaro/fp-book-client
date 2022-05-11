module Util where

import Prelude

import Affjax (Error, Response, printError)
import Affjax as Ajax
import Affjax.RequestBody as RequestBody
import Affjax.ResponseFormat as ResponseFormat
import Control.Monad.Error.Class (class MonadError, throwError)
import Control.Monad.Except (class MonadTrans, runExcept)
import Control.Monad.Reader (class MonadAsk, ask)
import Control.Monad.Trans.Class (lift)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.LoggedOnUser (LoggedOnUser)
import Data.Maybe (Maybe(..))
import Data.UUID (UUID)
import Effect.Aff.Class (class MonadAff)
import Effect.Ref as Ref
import Env (Env)
import Foreign.Generic (class Decode, class Encode, decodeJSON, encodeJSON)
import Halogen as H

endpoint :: String
endpoint = "http://localhost:3000/"

postJson
  :: ∀ request response m
  .  Encode request
  => Decode response
  => MonadAff m
  => request
  -> m (Either String response)
postJson str = Ajax.post ResponseFormat.string endpoint (mkRequest str)
  <#> unpackResponse
  # H.liftAff
  where
    mkRequest = Just <<< RequestBody.String <<< encodeJSON
    unpackResponse :: Either Error (Response String) -> Either String response
    unpackResponse ajaxResponse = do
      {body} <- lmap printError ajaxResponse
      lmap show <<< runExcept <<< decodeJSON $ body

getUserToken :: ∀ m. MonadAff m => MonadAsk Env m => m (Maybe LoggedOnUser)
getUserToken = do
  { userRef } <- ask
  H.liftEffect $ Ref.read userRef

liftEither :: ∀ e a m. MonadError e m => Either e a -> m a
liftEither (Left x) = throwError x
liftEither (Right x) = pure x

liftSuccess
  :: ∀ e a m t
  .Monad m
  => MonadTrans t
  => MonadError e (t m)
  => m (Either e a)
  -> t m a
liftSuccess x = lift x >>= liftEither