{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}

module Demo.Backend.Config where

import Control.Lens hiding ((<.>))
import Data.Aeson
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Yaml.Aeson (decodeFileThrow)
import Demo.Common.API.Prelude (aesonOptions)
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Reader.Static
import Effectful.TH (makeEffect)
import GHC.Generics (Generic)
import System.Environment (lookupEnv)
import System.FilePath

data App = App
  { _app_env :: Text,
    _app_db :: DB,
    _app_web :: Web
  }
  deriving (Show, Generic)

data DB = DB
  { _db_connStr :: Text,
    _db_numConns :: Int
  }
  deriving (Show, Generic)

newtype Web = Web
  { _web_port :: Int
  }
  deriving (Show, Generic)

instance FromJSON App where
  parseJSON = genericParseJSON aesonOptions

instance FromJSON DB where
  parseJSON = genericParseJSON aesonOptions

instance FromJSON Web where
  parseJSON = genericParseJSON aesonOptions

makeLenses ''App
makeLenses ''DB
makeLenses ''Web

data Loader :: Effect where
  GetConfig :: (App -> a) -> Loader m a

makeEffect ''Loader

runLoader :: IOE :> es => Eff (Loader : es) a -> Eff es a
runLoader = reinterpret loadConfigToReader $ \_ -> \case
  GetConfig f -> asks f

loadConfigToReader :: IOE :> es => Eff (Reader App : es) a -> Eff es a
loadConfigToReader action = do
  conf <- liftIO $ do
    confDir <- fromMaybe "conf" <$> lookupEnv "CONF_DIR"
    env <- fromMaybe "dev" <$> lookupEnv "ENV"
    let confFile = confDir </> "config" <.> env <.> "yaml"
    decodeFileThrow confFile
  runReader conf action
