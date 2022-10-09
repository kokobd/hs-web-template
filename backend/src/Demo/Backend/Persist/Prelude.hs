module Demo.Backend.Persist.Prelude
  ( withConn,
    module Effectful,
    module Database.Persist,
    SqlBackend,
    Pool,
    Reader,
    interpret,
  )
where

import Control.Monad.IO.Unlift (askRunInIO)
import Control.Monad.Reader (ReaderT, runReaderT)
import Data.Pool (Pool, withResource)
import Database.Persist
import Database.Persist.Sql (SqlBackend)
import Effectful
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.Reader.Static

withConn ::
  (Reader (Pool SqlBackend) :> es, IOE :> es) =>
  ReaderT SqlBackend (Eff es) a ->
  Eff es a
withConn action = do
  pool :: Pool SqlBackend <- ask
  runInIO <- askRunInIO
  liftIO . withResource pool $ runInIO . runReaderT action
