{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import Control.Concurrent (forkIO, newEmptyMVar)
import Control.Concurrent.MVar
import Control.Monad.IO.Class (liftIO)
import Demo.Backend.Main (runAppM)
import Demo.Backend.Server (getWaiApplication)
import Network.Wai (Application)
import Test.Tasty
import User (userTests)

main :: IO ()
main =
  defaultMain $
    withResource initApp releaseApp $ \(fmap fst -> getApp) ->
      testGroup
        "Integration Tests"
        [ userTests getApp
        ]

initApp :: IO (Application, MVar ())
initApp = do
  releaseSignal <- newEmptyMVar
  waiAppMVar <- newEmptyMVar
  forkIO $ runAppM $ do
    waiApp <- getWaiApplication
    liftIO $ putMVar waiAppMVar waiApp
    liftIO $ takeMVar releaseSignal
  (,releaseSignal) <$> takeMVar waiAppMVar

releaseApp :: (Application, MVar ()) -> IO ()
releaseApp (_, releaseSignal) = do
  putMVar releaseSignal ()
