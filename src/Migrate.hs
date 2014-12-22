module Migrate where

import           Control.Monad.Trans.Reader(ReaderT)


import qualified Data.Text as T
import qualified Database.Persist
import           Database.Persist.Sql(SqlBackend, Single(..), rawSql)

import           Import

import           Database.Persist.Sqlite (createSqlitePool, sqlDatabase, sqlPoolSize)
import           Control.Monad.Logger (runStderrLoggingT)

import           System.Directory(renameFile)
import           Yesod.Default.Config


import           Debug.Trace



type OldNode = ( Single Int, Single Int, Single Text
               , Single Text, Single String
               , Maybe (Single Bool)
               )

-- queryOld      :: AppConfig DefaultEnv Extra -> (LogFunc -> IO SqlBackend)
--               -> IO [(NodeId, DBNode, Bool)]
queryOld conf = do
    dbconf <- withYamlEnvironment "config/migrate_old_db.yml" (appEnv conf)
                Database.Persist.loadConfig >>=
                Database.Persist.applyEnv
    p <- runStderrLoggingT $
         createSqlitePool (sqlDatabase dbconf) (sqlPoolSize dbconf)

    oldNodes <- runStderrLoggingT $
                Database.Persist.runPool dbconf query p

    -- mapM_ moveFig oldNodes
    return $ map f oldNodes
  where
    query :: MonadIO m => ReaderT SqlBackend m [OldNode]
    query = rawSql "SELECT id, parent, name, url, img, priv FROM node" []

    fromInt :: Int -> NodeId
    fromInt = DBNodeKey . fromIntegral



    mkUrl u
      | T.length u < 1 = Nothing
      | otherwise      = Just u

    mkImg i
      | traceShow ("'" <> i <> "'") False= undefined
      | length i < 1 = Nothing
      | otherwise = Just  "image/jpeg" -- All but one img. were jpgs. so just
                                       -- assume it is JPG

    f :: OldNode -> (NodeId, DBNode, Bool)
    f (Single i, Single p, Single n, Single murl, Single mimg, mteach) =
      ( fromInt i,
        DBNode (fromInt p) n (mkUrl murl) (mkImg mimg)
      , maybe False unSingle mteach)

    -- moveFig (Single i, _, _, _, Just (Single fp), _) = return ()
    -- moveFig _                                    = return ()


insertNew conf oldNodes = do
    dbconf <- withYamlEnvironment "config/sqlite.yml" (appEnv conf)
              Database.Persist.loadConfig >>=
              Database.Persist.applyEnv
    p <- runStderrLoggingT $
         createSqlitePool (sqlDatabase dbconf) (sqlPoolSize dbconf)

    runStderrLoggingT
      (Database.Persist.runPool dbconf query p)
  where
    teacherTagId = TagKey 3

    query = do
      mapM_ (\(i,n,_) -> repsert i n) oldNodes
      mapM_ (\(i,_,_) -> insert_ (TagNodeStore teacherTagId i))
           . filter (\(_,_,b) -> b) $ oldNodes

migrateFromOld :: IO ()
migrateFromOld = do
  conf <- loadDevelopmentConfig
  queryOld conf >>= insertNew conf -- mapM_ (print . dBNodeImage . (\(_,n,_) -> n))

    -- const (return ()) --  insertNew conf
