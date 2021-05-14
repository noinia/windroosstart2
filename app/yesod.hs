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
import Data.Set(fromList)

-- | This is my data type. There are many like it, but this one is mine.
data Minimal = Minimal

-- mkYesod "Minimal" [parseRoutes|
--     / RootR GET
-- |]

instance Yesod Minimal

getRootR :: Handler Text
getRootR = pure "Hello, world!"

main :: IO ()
main = run 3000 =<< toWaiApp Minimal


--------------------------------------------------------------------------------

instance ParseRoute Minimal where
  parseRoute
    = ((\ f_a3EI x_a3EJ -> (f_a3EI ()) x_a3EJ) ::
         (() -> ([Text], [(Text, Text)]) -> Maybe (Route a_a3EK))
         -> ([Text], [(Text, Text)]) -> Maybe (Route a_a3EK))
        helper_a3EH
    where
        helper_a3EH env2437_a3EC req2437_a3ED
          = helper2437_a3EE (fst req2437_a3ED)
          where
              helper2437_a3EE []
                = ((((\ _ _ x_a3EG _ -> x_a3EG) (error "mdsGetHandler"))
                      env2437_a3EC)
                     (Just RootR))
                    req2437_a3ED
              helper2437_a3EE _
                = ((((\ _ _ x_a3EF _ -> x_a3EF) (error "mds404")) env2437_a3EC)
                     Nothing)
                    req2437_a3ED

instance RenderRoute Minimal where
  data Route Minimal
    = RootR
    deriving (Show, Eq, Read)
  renderRoute RootR = ([], [])
instance RouteAttrs Minimal where
  routeAttrs RootR {}
    = fromList []

resourcesMinimal :: [ResourceTree String]
resourcesMinimal
  = [ResourceLeaf
       (((((Resource "RootR") [])
            ((Methods Nothing) ["GET"]))
           [])
          True)]

type Handler = HandlerFor Minimal
type Widget = WidgetFor Minimal ()
instance YesodDispatch Minimal where
  yesodDispatch env9720_a3Ez req9720_a3EA
    = helper9720_a3EB (pathInfo req9720_a3EA)
    where
        helper9720_a3EB []
          = case requestMethod req9720_a3EA of
              "GET"
                -> (((yesodRunner getRootR) env9720_a3Ez) (Just RootR))
                     req9720_a3EA
              _ -> (((yesodRunner (void badMethod)) env9720_a3Ez)
                      (Just RootR))
                     req9720_a3EA
        helper9720_a3EB _
          = (((yesodRunner (void notFound)) env9720_a3Ez)
               Nothing)
              req9720_a3EA
