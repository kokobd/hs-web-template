module Demo.Backend.Persist.User
  ( runUserRepo,
  )
where

import Control.Monad (void)
import qualified Demo.Backend.Persist.Model as Model
import Demo.Backend.Persist.Prelude
import Demo.Backend.Service.User

runUserRepo ::
  (IOE :> es, SqlBackendPool :> es) =>
  Eff (UserRepo : es) a ->
  Eff es a
runUserRepo = interpret $ \_ -> \case
  CreateUser user -> withConn $ do
    void $ insert $ userToModel user

userToModel :: User -> Model.User
userToModel user =
  Model.User -- this might be more complex in production code
    { Model.userEmail = _user_email user,
      Model.userHashedPassword = _user_hashedPassword user,
      Model.userNickname = _user_nickname user
    }
