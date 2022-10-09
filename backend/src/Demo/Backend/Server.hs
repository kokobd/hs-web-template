module Demo.Backend.Server where

import Data.Proxy (Proxy (..))
import Demo.Backend.Controller.User (UserController)
import qualified Demo.Backend.Controller.User as UserController
import Demo.Backend.Service.User (UserRepo, UserService)
import Demo.Common.API.User (API)
import Effectful
import Servant.Server

server ::
  UserController :> es =>
  ServerT API (Eff es)
-- wire up all controllers here
server = UserController.register

apiProxy :: Proxy API
apiProxy = Proxy

type AppM =
  Eff
    '[ UserController,
       UserService,
       UserRepo,
       IOE
     ]

runAppM :: AppM a -> Handler a
runAppM = undefined
