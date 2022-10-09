{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Demo.Backend.External.Logger (KatipE, runKatipE) where

import qualified Demo.Backend.Config as Config
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Reader.Static
import Katip
import System.IO (stdout)

data KatipE :: Effect where
  GetLogEnv :: KatipE m LogEnv
  LocalLogEnv :: (LogEnv -> LogEnv) -> m a -> KatipE m a
  GetKatipContext :: KatipE m LogContexts
  LocalKatipContext :: (LogContexts -> LogContexts) -> m a -> KatipE m a
  GetKatipNamespace :: KatipE m Namespace
  LocalKatipNamespace :: (Namespace -> Namespace) -> m a -> KatipE m a

type instance DispatchOf KatipE = 'Dynamic

instance (IOE :> es, KatipE :> es) => Katip (Eff es) where
  getLogEnv = send GetLogEnv
  localLogEnv x = send . LocalLogEnv x

instance (IOE :> es, KatipE :> es) => KatipContext (Eff es) where
  getKatipContext = send GetKatipContext
  localKatipContext x = send . LocalKatipContext x
  getKatipNamespace = send GetKatipNamespace
  localKatipNamespace x = send . LocalKatipNamespace x

runKatipE ::
  (IOE :> es, Config.Loader :> es) =>
  Eff (KatipE : es) a ->
  Eff es a
runKatipE = reinterpret convEff $ \env -> \case
  GetLogEnv -> ask
  LocalLogEnv a m -> localSeqUnlift env $ \unlift -> local a (unlift m)
  GetKatipContext -> ask
  LocalKatipContext a m -> localSeqUnlift env $ \unlift -> local a (unlift m)
  GetKatipNamespace -> ask
  LocalKatipNamespace a m -> localSeqUnlift env $ \unlift -> local a (unlift m)

convEff ::
  (IOE :> es, Config.Loader :> es) =>
  Eff
    ( Reader LogEnv
        : Reader LogContexts
        : Reader Namespace
        : es
    )
    a ->
  Eff es a
convEff action = do
  handleScribe <- liftIO $ mkHandleScribe ColorIfTerminal stdout (permitItem InfoS) V2
  env <- Config.getConfig Config._app_env
  logEnv <-
    liftIO $
      registerScribe "stdout" handleScribe defaultScribeSettings
        =<< initLogEnv "Demo" (Environment env)
  let initialContext = liftPayload ()
      initialNamespace = "main"
  runReader initialNamespace . runReader initialContext . runReader logEnv $ action
