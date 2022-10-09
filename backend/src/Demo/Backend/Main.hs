module Demo.Backend.Main
  ( main,
  )
where

import Data.Function ((&))
import qualified Demo.Backend.Config as Config
import Demo.Backend.Controller.User (UserController, runUserController)
import Demo.Backend.External.Logger (KatipE, runKatipE)
import Demo.Backend.Persist.Prelude (SqlBackendPool, runSqlBackendPool)
import Demo.Backend.Persist.User (runUserRepo)
import Demo.Backend.Server
import Demo.Backend.Service.User (UserRepo, UserService, runUserService)
import Effectful

main :: IO ()
main = runAppM startServer

type AppM =
  Eff
    '[ Server,
       UserController,
       UserService,
       UserRepo,
       SqlBackendPool,
       KatipE,
       Config.Loader,
       IOE
     ]

runAppM :: AppM a -> IO a
runAppM appM =
  runServerEffect appM
    & runUserController
    & runUserService
    & runUserRepo
    & runSqlBackendPool
    & runKatipE
    & Config.runLoader
    & runEff
