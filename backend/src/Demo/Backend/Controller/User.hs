{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Demo.Backend.Controller.User where

import Demo.Backend.Service.User (UserService)
import qualified Demo.Backend.Service.User as UserService
import Demo.Common.API.User (UserRegistrationForm (..))
import Effectful
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.TH
import Servant.API (NoContent (..))

data UserController :: Effect where
  Register :: UserRegistrationForm -> UserController m NoContent

makeEffect ''UserController

runUserController ::
  UserService :> es =>
  Eff (UserController : es) a ->
  Eff es a
runUserController = interpret $ \_ -> \case
  Register UserRegistrationForm {..} -> do
    UserService.register _userRegistrationForm_email _userRegistrationForm_password
    pure NoContent
