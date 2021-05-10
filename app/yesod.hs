{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

module Main where

import           Data.Text                (Text)
import           Network.Wai.Handler.Warp (run)
import           Yesod.Core               (RenderRoute (..), Yesod, mkYesod,
                                           parseRoutes, toWaiApp)

-- | This is my data type. There are many like it, but this one is mine.
data Minimal = Minimal

mkYesod "Minimal" [parseRoutes|
    / RootR GET
|]

instance Yesod Minimal

getRootR :: Handler Text
getRootR = pure "Hello, world!"

main :: IO ()
main = run 3000 =<< toWaiApp Minimal
