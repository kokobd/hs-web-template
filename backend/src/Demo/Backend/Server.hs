{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Demo.Backend.Server
  ( Server,
    runServerEffect,
    startServer,
    getWaiApplication,
  )
where

import Control.Monad.Except
import Data.Proxy (Proxy (..))
import Data.String (fromString)
import qualified Demo.Backend.Config as Config
import Demo.Backend.Controller.User (UserController)
import qualified Demo.Backend.Controller.User as UserController
import Demo.Backend.External.Logger
import Demo.Common.API (API)
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.TH
import Network.Wai (Application)
import qualified Network.Wai.Handler.Warp as Warp
import qualified Servant.Server as Servant

server ::
  UserController :> es =>
  Servant.ServerT API (ExceptT Servant.ServerError (Eff es))
-- wire up all controllers here
server = UserController.register

apiProxy :: Proxy API
apiProxy = Proxy

data Server :: Effect where
  GetWaiApplication :: Server m Application
  StartServer :: Server m ()

makeEffect ''Server

type Constraints es =
  '[ IOE,
     Config.Loader,
     Logger,
     UserController
   ]
    :>> es

runServerEffect ::
  forall es a.
  Constraints es =>
  Eff (Server : es) a ->
  Eff es a
runServerEffect = interpret $ \_ -> \case
  StartServer -> do
    port <- Config.getConfig (Config._web_port . Config._app_web)
    waiApp <- getWaiApplication'
    withLogger . logInfo . fromString $ "listening at port " <> show port
    liftIO $ Warp.run port waiApp
  GetWaiApplication -> getWaiApplication'

getWaiApplication' :: forall es. Constraints es => Eff es Application
getWaiApplication' = do
  withUnliftStrategy (ConcUnlift Ephemeral Unlimited) $ withEffToIO $ \unlift -> do
    let serverToHandler :: ExceptT Servant.ServerError (Eff es) x -> Servant.Handler x
        serverToHandler = Servant.Handler . ExceptT . liftIO . unlift . runExceptT
        waiApp = Servant.serve apiProxy (Servant.hoistServer apiProxy serverToHandler server)
    pure waiApp
