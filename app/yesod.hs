{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}
module Main where

import           Data.Text                (Text)
import           Network.Wai.Handler.Warp (run)
-- import           Yesod.Core               (RenderRoute (..), Yesod, mkYesod,
--                                            parseRoutes, toWaiApp
--                                           , ParseRoute(..)
--                                           )
import Yesod.Core
import Network.Wai
import Data.Functor
import Yesod.Routes.TH.Types

import Debug.Trace


-- | This is my data type. There are many like it, but this one is mine.
data Minimal = Minimal

-- mkYesod "Minimal" [parseRoutes|
--     / RootR GET
-- |]

instance Yesod Minimal

getRootR :: HandlerFor Minimal Text
getRootR = pure "Hello, world!"

main :: IO ()
main = run 3000 =<< toWaiApp Minimal


--------------------------------------------------------------------------------
-- | the mkYesod stuff expands to the following:


instance ParseRoute Minimal where
  parseRoute _ = Just RootR

    -- = anonF parseGo
    -- where
    --     -- anonF :: (() -> ([Text], [(Text, Text)]) -> Maybe (Route a))
    --     --       -> ([Text], [(Text, Text)]) -> Maybe (Route a)
    --     anonF = \ f thePath0 -> (f ()) thePath0

    --     parseGo env2437_a3EC thePath
    --       = go (fst thePath)
    --       where
    --           go []
    --             = ((((\ _ _ x_a3EG _ -> x_a3EG) (error "mdsGetHandler"))
    --                   env2437_a3EC)
    --                  (traceShowId $ Just RootR))
    --                 thePath
    --           go _
    --             = error "don't parse paths"
    --             -- ((((\ _ _ x_a3EF _ -> x_a3EF) (error "mds404")) env2437_a3EC)
    --             --      Nothing)
    --             --     thePath

instance RenderRoute Minimal where
  data Route Minimal = RootR  deriving (Show, Eq, Read)
  renderRoute _ = ([], [])

instance RouteAttrs Minimal where
  routeAttrs _ = mempty



instance YesodDispatch Minimal where
  yesodDispatch runnerEnv req = case requestMethod req of
              "GET" -> (((yesodRunner getRootR) runnerEnv) (Just RootR)) req
              _     -> error "don't care about other req types"


    -- go (pathInfo req)
    -- where
    --     go []
    --       = case requestMethod req of
    --           "GET" -> (((yesodRunner getRootR) runnerEnv) (Just RootR)) req
    --           _     -> error "don't care about other req types"

    --             -- (((yesodRunner (void badMethod)) runnerEnv)
    --             --       (Just RootR))
    --             --      req
    --     go _
    --       = error "pathInfo better be empty"
    --       -- (((yesodRunner (void notFound)) runnerEnv)
    --       --      Nothing)
    --       --     req
