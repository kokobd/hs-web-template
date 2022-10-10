module User where

import Test.Tasty
import Test.Tasty.Wai
import Demo.Backend.Main (runAppM)
import Demo.Backend.Server (getWaiApplication)

test_registration :: TestTree
test_registration = undefined
  -- withResource initWaiApp releaseWaiApp $ \waiApp -> do
  --   liftIO $ waiApp
  --   testWai waiApp "hello" $ do
  --     pure ()