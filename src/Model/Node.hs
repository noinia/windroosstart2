{-# LANGUAGE TupleSections #-}
module Model.Node where


import Settings(allowedContentTypes)
import Yesod.Core.Content(ContentType)
import Control.Monad
import Control.Applicative
import Data.Text(Text)
import Database.Persist((==.),(!=.))
import Database.Persist.Class(get,selectList)
import Database.Persist.Types(Entity(..))
import Foundation
import Model
import Prelude
import Yesod.Persist.Core
import System.FilePath((<.>))
import qualified Data.ByteString.Char8 as B

import qualified Data.Map as M
import qualified Data.Set as S

--------------------------------------------------------------------------------

data Node a = Node { nodeId    :: NodeId
                   , dbNode    :: DBNode
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

imageContentType :: Node a -> Maybe ContentType
imageContentType = fmap B.pack . dBNodeImage . dbNode

image   :: Node a -> Maybe (ContentType, FilePath)
image n = (,) <$> imageContentType n
              <*> imageFileName n

imageFileName   :: Node a -> Maybe FilePath
imageFileName n = (imageName (nodeId n) <.>) <$> imageExt n

imageExt   :: Node a -> Maybe String
imageExt n = imageContentType n >>= flip lookup allowedContentTypes

imageName :: NodeId -> FilePath
imageName = show . toInteger . unDBNodeKey


getTree   :: NodeId -> YesodDB App (Maybe (Node ()))
getTree i = node <$> get i
                 <*> (mapM get'     =<< selectList [ TagNodeStoreNodeId ==. i] [])
                 <*> (mapM getTree' =<< selectList [ DBNodeParent       ==. i
                                                   , DBNodeId           !=. rootId
                                                   ] [])
  where
    node mi mTgs mChs = Node i <$> mi <*> pure () <*> sequence mTgs <*> sequence mChs
    get'        = get . tagNodeStoreTagId . entityVal
    getTree'    = getTree . entityKey

rootId :: NodeId
rootId = DBNodeKey 0

withId   :: NodeId -> Node a -> Maybe (Node a)
withId i = findNode ((== i) . nodeId)


findNode      :: (Node a -> Bool) -> Node a -> Maybe (Node a)
findNode p n
  | p n       = Just n
  | otherwise = msum . map (findNode p) . children $ n


tagIds       :: M.Map TagId Tag -> Node a -> [TagId]
tagIds tgs n = let myTgs = S.fromList $ tags n
               in M.keys . M.filter (`S.member` myTgs) $ tgs
