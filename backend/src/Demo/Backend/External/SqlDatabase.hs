{-# LANGUAGE TemplateHaskell #-}

module Demo.Backend.External.SqlDatabase
  ( runSqlBackendPool,
    SqlBackendPool,
    withConn,
  )
where

import Control.Lens
import Control.Monad.Logger (Loc, LogLevel (..), LogSource, LogStr, LoggingT (runLoggingT), fromLogStr)
import Control.Monad.Reader (ReaderT (runReaderT))
import Control.Monad.Trans.Class (lift)
import Data.Pool (Pool, withResource)
import Database.Persist.Sql
import Database.Persist.Sqlite
import qualified Demo.Backend.Config as Config
import Demo.Backend.External.Logger (KatipE)
import Demo.Backend.Persist.Model (migrateAll)
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Reader.Static
import Effectful.TH (makeEffect)
import qualified Katip

data SqlBackendPool :: Effect where
  WithConn :: ReaderT SqlBackend m a -> SqlBackendPool m a

makeEffect ''SqlBackendPool

runSqlBackendPool ::
  (IOE :> es, Config.Loader :> es, KatipE :> es) =>
  Eff (SqlBackendPool : es) a ->
  Eff es a
runSqlBackendPool = reinterpret initSqlBackendPool $ \env -> \case
  WithConn action -> do
    pool <- ask
    localSeqUnliftIO env $ \unlift -> do
      withResource pool $ unlift . runReaderT action

initSqlBackendPool ::
  (IOE :> es, Config.Loader :> es, KatipE :> es) =>
  Eff (Reader (Pool SqlBackend) : es) a ->
  Eff es a
initSqlBackendPool action = do
  conf <- Config.getConfig Config._app_db
  withUnliftStrategy (ConcUnlift Ephemeral Unlimited) $ withEffToIO $ \unlift -> do
    let logger :: Loc -> LogSource -> LogLevel -> LogStr -> IO ()
        logger loc _ level str =
          unlift $
            Katip.logItemM (Just loc) (logLevelToKatip level) (Katip.logStr $ fromLogStr str)
    flip runLoggingT logger
      $ withSqlitePool
        (conf ^. Config.db_connStr)
        (conf ^. Config.db_numConns)
      $ \pool -> do
        liftIO $ withResource pool $ runReaderT (runMigration migrateAll)
        lift . unlift $ runReader pool action

logLevelToKatip :: LogLevel -> Katip.Severity
logLevelToKatip LevelDebug = Katip.DebugS
logLevelToKatip LevelInfo = Katip.InfoS
logLevelToKatip LevelWarn = Katip.WarningS
logLevelToKatip LevelError = Katip.ErrorS
logLevelToKatip (LevelOther _) = Katip.InfoS -- katip doesn't allow custom levels so we map them to info
