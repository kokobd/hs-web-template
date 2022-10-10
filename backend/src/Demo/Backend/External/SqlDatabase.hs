{-# LANGUAGE TemplateHaskell #-}

module Demo.Backend.External.SqlDatabase
  ( runSqlBackendPool,
    SqlBackendPool,
    withConn,
  )
where

import Control.Lens
import Control.Monad.Reader (ReaderT (runReaderT))
import Control.Monad.Trans.Class (lift)
import Data.Pool (Pool, withResource)
import Database.Persist.Sql
import Database.Persist.Sqlite
import qualified Demo.Backend.Config as Config
import Demo.Backend.External.Logger
import Demo.Backend.Persist.Model (migrateAll)
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Reader.Static
import Effectful.TH (makeEffect)

data SqlBackendPool :: Effect where
  WithConn :: ReaderT SqlBackend m a -> SqlBackendPool m a

makeEffect ''SqlBackendPool

runSqlBackendPool ::
  (IOE :> es, Config.Loader :> es, Logger :> es) =>
  Eff (SqlBackendPool : es) a ->
  Eff es a
runSqlBackendPool = reinterpret initSqlBackendPool $ \env -> \case
  WithConn action -> do
    pool <- ask
    localSeqUnliftIO env $ \unlift -> do
      withResource pool $ unlift . runReaderT action

initSqlBackendPool ::
  (IOE :> es, Config.Loader :> es, Logger :> es) =>
  Eff (Reader (Pool SqlBackend) : es) a ->
  Eff es a
initSqlBackendPool action = do
  conf <- Config.getConfig Config._app_db
  withLogger
    $ withSqlitePool
      (conf ^. Config.db_connStr)
      (conf ^. Config.db_numConns)
    $ \pool -> do
      lift $ withUnliftStrategy (ConcUnlift Ephemeral Unlimited) $ withEffToIO $ \unlift -> do
        liftIO $ withResource pool $ runReaderT (runMigration migrateAll)
        unlift $ runReader pool action
