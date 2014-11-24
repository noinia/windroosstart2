module Model.Node where

import Control.Applicative
import Data.Text(Text)
import Database.Persist((==.),(!=.))
import Database.Persist.Class(get,selectList)
import Database.Persist.Types(Entity(..))
import Foundation
import Model
import Prelude
import Yesod.Persist.Core

data Node a = Node { dbNode    :: DBNode
                   , dataValue :: a
                   , tags      :: [Tag]
                   , children  :: [Node a]
                   }

description :: Node a -> Text
description = dBNodeDescription . dbNode

parentId    :: Node a -> NodeId
parentId    = dBNodeParent . dbNode

url         :: Node a -> Maybe URL
url         = dBNodeUrl . dbNode

image       :: Node a -> Maybe FilePath
image       = dBNodeImage . dbNode

getTree   :: NodeId -> YesodDB App (Maybe (Node ()))
getTree i = node <$> get i
                 <*> (mapM get'     =<< selectList [ TagNodeStoreNodeId ==. i] [])
                 <*> (mapM getTree' =<< selectList [ DBNodeParent      ==. i
                                                   , DBNodeId          !=. rootId
                                                   ] [])
  where
    node mi mTgs mChs = Node <$> mi <*> pure () <*> sequence mTgs <*> sequence mChs
    get'        = get . tagNodeStoreTagId . entityVal
    getTree'    = getTree . dBNodeParent . entityVal

rootId :: NodeId
rootId = DBNodeKey 0
