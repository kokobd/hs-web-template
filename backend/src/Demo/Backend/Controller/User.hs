{-# LANGUAGE RecordWildCards #-}

module Demo.Backend.Controller.User where

import Control.Monad.Except
import Demo.Backend.Service.User (UserService)
import qualified Demo.Backend.Service.User as UserService
import Demo.Common.API.User (UserRegistrationForm (..))
import Effectful
import Effectful.Dispatch.Dynamic
import Servant.API (NoContent (..))
import Servant.Server (ServerError)

data UserController :: Effect where
  Register :: UserRegistrationForm -> UserController m (Either ServerError NoContent)

type instance DispatchOf UserController = 'Dynamic

register ::
  UserController :> es =>
  UserRegistrationForm ->
  ExceptT ServerError (Eff es) NoContent
register = ExceptT . send . Register

runUserController ::
  UserService :> es =>
  Eff (UserController : es) a ->
  Eff es a
runUserController = interpret $ \_ -> \case
  Register UserRegistrationForm {..} -> do
    UserService.register _userRegistrationForm_email _userRegistrationForm_password
    pure $ Right NoContent
