module Demo.Backend.External.SqlDatabase (runSqlBackendPool) where

import Data.Pool (Pool)
import Database.Persist.Sql
import Database.Persist.Sqlite
import Effectful
import Effectful.Reader.Static

runSqlBackendPool ::
  IOE :> es =>
  Eff (Reader (Pool SqlBackend) : es) a ->
  Eff es a
runSqlBackendPool action = do
  let pool = undefined -- FIXME
  runReader pool action
