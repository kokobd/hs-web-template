{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Demo.Backend.Service.User where

import Data.ByteString (ByteString)
import Data.Text (Text)
import qualified Data.Text.Encoding as T
import Effectful
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.TH

data User = User
  { _user_email :: !Text,
    _user_hashedPassword :: !ByteString,
    _user_nickname :: !Text
  }
  deriving (Show, Eq)

data UserRepo :: Effect where
  CreateUser :: User -> UserRepo m ()

data UserService :: Effect where
  -- | Given email and password (in plain text), request to register a new user
  -- we may introduce a new type called @UserRegistrationForm@ if desired
  Register ::
    -- | email
    Text ->
    -- | password
    Text ->
    UserService m ()

makeEffect ''UserRepo
makeEffect ''UserService

runUserService ::
  UserRepo :> es =>
  Eff (UserService : es) a ->
  Eff es a
runUserService = interpret $ \_ -> \case
  Register email password ->
    createUser $
      User
        { _user_email = email,
          _user_hashedPassword = hashPassword password,
          _user_nickname = "" -- default nick name
        }

hashPassword :: Text -> ByteString
hashPassword = T.encodeUtf8 -- dummy implementation. we should generate a salt, and calculate sha512(salt <> password)
