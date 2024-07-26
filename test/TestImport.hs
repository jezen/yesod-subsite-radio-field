module TestImport
  ( module TestImport
  , module X
  ) where

import ClassyPrelude as X hiding (Handler, delete, deleteBy, exp)
import ExampleApp as X
import Faker as X
import SubApp as X
import System.Log.FastLogger.LoggerSet (newStdoutLoggerSet)
import Test.Hspec as X
import Yesod.Core.Dispatch (defaultMiddlewaresNoLogging)
import Yesod.Core.Unsafe (fakeHandlerGetLogger)
import Yesod.Default.Config2 (makeYesodLogger)
import Yesod.Test as X

withApp :: SpecWith (TestApp App) -> Spec
withApp = around (bracket beforeEach afterEach) . aroundWith (. fst)
  where
  beforeEach :: IO (TestApp App, ())
  beforeEach = do
    app <- makeFoundation
    pure ((app, defaultMiddlewaresNoLogging), ())

  afterEach :: (TestApp App, ()) -> IO ()
  afterEach ((app, _), _) = shutdownApp app

runHandler :: Handler a -> YesodExample App a
runHandler handler = do
  app <- getTestYesod
  logger <- liftIO (newStdoutLoggerSet 1 >>= makeYesodLogger)
  fakeHandlerGetLogger (const logger) app handler
