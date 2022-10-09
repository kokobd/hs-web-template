{-# LANGUAGE DeriveGeneric #-}

module Demo.Common.API.User where

import Demo.Common.API.Prelude

type API = ReqBody '[JSON] UserRegistrationForm :> Post '[JSON] NoContent

data UserRegistrationForm = UserRegistrationForm
  { _userRegistrationForm_email :: !Text,
    _userRegistrationForm_password :: !Text
  }
  deriving (Show, Eq, Generic)

instance FromJSON UserRegistrationForm where
  parseJSON = genericParseJSON aesonOptions

instance ToJSON UserRegistrationForm where
  toJSON = genericToJSON aesonOptions
  toEncoding = genericToEncoding aesonOptions
