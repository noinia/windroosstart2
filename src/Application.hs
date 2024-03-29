{-# OPTIONS_GHC -fno-warn-orphans #-}
module Application
    ( makeApplication
    , getApplicationDev
    , makeFoundation
    , createUserApp
    ) where

import qualified Database.Persist
import           Database.Persist.Sql(runMigration)

import           Import
import qualified Network.Wai.Middleware.RequestLogger as RequestLogger
import           Network.Wai.Middleware.RequestLogger( mkRequestLogger
                                                     , outputFormat
                                                     , OutputFormat (..)
                                                     , IPAddrSource (..)
                                                     , destination)
import           Yesod.Auth
import           Yesod.Default.Config
import           Yesod.Default.Handlers
import           Yesod.Default.Main

import           Database.Persist.Sqlite (createSqlitePool, sqlDatabase, sqlPoolSize)
import           Network.HTTP.Client.Conduit (newManager)
import           Control.Monad.Logger (runLoggingT, runStdoutLoggingT)
import           System.Log.FastLogger (newStdoutLoggerSet, defaultBufSize)
import           Network.Wai.Logger (clockDateCacher)
import           Data.Default (def)
import           Yesod.Core.Types (loggerSet, Logger (Logger))

-- Import all relevant handler modules here.
-- Don't forget to add new modules to your cabal file!
import           Handler.Home
import           Handler.Node
import           Handler.Tag
import           Handler.Post
import           Handler.User




-- This line actually creates our YesodDispatch instance. It is the second half
-- of the call to mkYesodData which occurs in Foundation.hs. Please see the
-- comments there for more details.
mkYesodDispatch "App" resourcesApp

-- This function allocates resources (such as a database connection pool),
-- performs initialization and creates a WAI application. This is also the
-- place to put your migrate statements to have automatic database
-- migrations handled by Yesod.
makeApplication :: AppConfig DefaultEnv Extra -> IO (Application, LogFunc)
makeApplication conf = do
    foundation <- makeFoundation conf

    -- Initialize the logging middleware
    logWare <- mkRequestLogger def
        { outputFormat =
            if development
                then Detailed True
                else Apache FromSocket
        , destination = RequestLogger.Logger $ loggerSet $ appLogger foundation
        }

    -- Create the WAI application and apply middlewares
    app <- toWaiAppPlain foundation
    let logFunc = messageLoggerSource foundation (appLogger foundation)
    return (logWare $ defaultMiddlewaresNoLogging app, logFunc)

-- | Loads up any necessary settings, creates your foundation datatype, and
-- performs some initialization.
makeFoundation :: AppConfig DefaultEnv Extra -> IO App
makeFoundation conf = do
    manager <- newManager
    s <- staticSite
    dbconf <- withYamlEnvironment "config/sqlite.yml" (appEnv conf)
              Database.Persist.loadConfig >>=
              Database.Persist.applyEnv

    loggerSet' <- newStdoutLoggerSet defaultBufSize
    (getter, _) <- clockDateCacher

    let logger = Yesod.Core.Types.Logger loggerSet' getter
        mkFoundation p = App
            { settings = conf
            , getStatic = s
            , connPool = p
            , httpManager = manager
            , persistConfig = dbconf
            , appLogger = logger
            }
        tempFoundation = mkFoundation $ error "connPool forced in tempFoundation"
        logFunc = messageLoggerSource tempFoundation logger

    p <- flip runLoggingT logFunc
       $ createSqlitePool (sqlDatabase dbconf) (sqlPoolSize dbconf)
    let foundation = mkFoundation p

    -- Perform database migration using our application's logging settings.
    -- flip runLoggingT logFunc
    --     (Database.Persist.runPool dbconf (runMigration migrateAll) p)
    -- initialize the DB
    flip runLoggingT logFunc
        (Database.Persist.runPool dbconf initializeDB p)

    return foundation








-- for yesod devel
getApplicationDev :: IO (Int, Application)
getApplicationDev =
    defaultDevelApp loader (fmap fst . makeApplication)
  where
    loader = Yesod.Default.Config.loadConfig (configSettings Development)
        { csParseExtra = parseExtra
        }




--------------------------------------------------------------------------------


-- | Loads up any necessary settings, creates your foundation datatype, and
-- performs some initialization.
createUserApp :: AppConfig DefaultEnv Extra -> IO ()
createUserApp conf = do
    dbconf <- withYamlEnvironment "config/sqlite.yml" (appEnv conf)
              Database.Persist.loadConfig >>=
              Database.Persist.applyEnv

    -- loggerSet' <- newStdoutLoggerSet defaultBufSize
    -- (getter, _) <- clockDateCacher

    -- let logger = Yesod.Core.Types.Logger loggerSet' getter
    --     logFunc = messageLoggerSource undefined logger

    p <- runStdoutLoggingT
       $ createSqlitePool (sqlDatabase dbconf) (sqlPoolSize dbconf)

    runStdoutLoggingT
        (Database.Persist.runPool dbconf createUser p)

    return ()
