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



withOldDb conf query = do
    dbconf <- withYamlEnvironment "config/migrate_old_db.yml" (appEnv conf)
                Database.Persist.loadConfig >>=
                Database.Persist.applyEnv
    p <- runStderrLoggingT $
         createSqlitePool (sqlDatabase dbconf) (sqlPoolSize dbconf)
    runStderrLoggingT $
                Database.Persist.runPool dbconf query p

moveFigs conf = do
    xs <- withOldDb conf $ rawSql "SELECT id, img FROM node" []
    mapM_ moveFig xs
  where
    moveFig :: (Single Int, Single FilePath) -> IO ()
    moveFig (Single i, Single fp)
      | not $ null fp = let path   = "uploaded_images/"
                            oldFig = path <> fp
                            newFig = path <> show i <> ".jpg"
                        in renameFile oldFig newFig
      | otherwise = return ()





queryOld conf = map f <$> withOldDb conf query
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
