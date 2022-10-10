{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Demo.Backend.External.Logger
  ( Logger,
    withLogger,
    runLogger,
    module Control.Monad.Logger.Aeson,
  )
where

import Control.Monad.Logger
import Control.Monad.Logger.Aeson (logDebug, logError, logInfo, logWarn)
import Control.Monad.Reader (ReaderT (..))
import Control.Monad.Trans.Class
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Reader.Static
import Effectful.TH
import System.IO (stdout)

data Logger :: Effect where
  WithLogger :: PureLoggingT m a -> Logger m a

type LogFunc m = Loc -> LogSource -> LogLevel -> LogStr -> m ()

newtype PureLoggingT m a = PureLoggingT
  { runPureLoggingT :: ReaderT (LogFunc m) m a
  }
  deriving (Functor, Applicative, Monad)

instance Monad m => MonadLogger (PureLoggingT m) where
  monadLoggerLog :: ToLogStr msg => Loc -> LogSource -> LogLevel -> msg -> PureLoggingT m ()
  monadLoggerLog loc source level msg = PureLoggingT $ ReaderT $ \logFunc ->
    logFunc loc source level (toLogStr msg)

instance MonadIO m => MonadIO (PureLoggingT m) where
  liftIO :: IO a -> PureLoggingT m a
  liftIO = PureLoggingT . liftIO

instance MonadUnliftIO m => MonadUnliftIO (PureLoggingT m) where
  withRunInIO :: ((forall a. PureLoggingT m a -> IO a) -> IO b) -> PureLoggingT m b
  withRunInIO action = PureLoggingT $ withRunInIO $ \runInIO -> action $ runInIO . runPureLoggingT

instance IOE :> es => MonadLoggerIO (PureLoggingT (Eff es)) where
  askLoggerIO :: PureLoggingT (Eff es) (LogFunc IO)
  askLoggerIO = PureLoggingT $ ReaderT $ \logFunc -> do
    withUnliftStrategy (ConcUnlift Ephemeral Unlimited) $ withEffToIO $ \unlift ->
      pure $ \loc source level msg -> unlift (logFunc loc source level msg)

instance MonadTrans PureLoggingT where
  lift :: Monad m => m a -> PureLoggingT m a
  lift = PureLoggingT . lift

makeEffect ''Logger

runLogger ::
  IOE :> es =>
  Eff (Logger : es) a ->
  Eff es a
runLogger = reinterpret initLogger $ \env -> \case
  WithLogger action -> do
    logFunc :: LogFunc IO <- ask
    localLiftUnliftIO env SeqUnlift $ \liftEff unlift -> do
      unlift $ runReaderT (runPureLoggingT action) $ \loc source level msg -> liftEff $ logFunc loc source level msg

initLogger :: Eff (Reader (LogFunc IO) : es) a -> Eff es a
initLogger action = do
  let logFunc = defaultOutput stdout -- we may allow using other loggers based on config
  runReader logFunc action
