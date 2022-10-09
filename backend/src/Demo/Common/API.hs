module Demo.Common.API where

import qualified Demo.Common.API.User as User
import Servant.API

type API = "api" :> "user" :> User.API
