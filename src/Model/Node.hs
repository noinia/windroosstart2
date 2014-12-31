{-# LANGUAGE TupleSections #-}
module Model.Node where

import           Control.Applicative
import           Control.Monad
import qualified Data.ByteString.Char8 as B
import           Data.List((\\), nub)
import           Data.Maybe
import           Data.Text(Text)
import           Database.Persist((==.),(!=.))
import           Database.Persist.Class(get,selectList, replace, deleteWhere, insertMany_)
import           Database.Persist.Types(SelectOpt(..), Entity(..))
import           Foundation
import           Model
import           Prelude
import           Settings(allowedContentTypes)
import           System.FilePath((<.>))
import           Yesod.Core.Content(ContentType)
import           Yesod.Persist.Core


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
                                                   ] [Asc DBNodeDescription])
  where
    node mi mTgs mChs = Node i <$> mi <*> pure () <*> sequence mTgs
                               <*> Just (catMaybes mChs) -- makes getting trees lazy
                                                         --
    get'        = get . tagNodeStoreTagId . entityVal
    getTree'    = getTree . entityKey

withId   :: NodeId -> Node a -> Maybe (Node a)
withId i = findNode ((== i) . nodeId)


findNode      :: (Node a -> Bool) -> Node a -> Maybe (Node a)
findNode p n
  | p n       = Just n
  | otherwise = msum . map (findNode p) . children $ n


tagIds       :: M.Map TagId Tag -> Node a -> [TagId]
tagIds tgs n = let myTgs = S.fromList $ tags n
               in M.keys . M.filter (`S.member` myTgs) $ tgs

filterNode :: (Node a -> Bool) -> Node a -> Maybe (Node a)
filterNode p n
  | p n       = Just $ n { children = mapMaybe (filterNode p) $ children n }
  | otherwise = Nothing

-- | Match all tags (or if the first arg is False, avoid all tags)
filterByTags       :: Bool -> [Tag] -> Node a -> Maybe (Node a)
filterByTags b tgs = filterNode (\n -> allTags n || p (tagsMatch n))
  where
    p           = if b then id else not
    allTags     = null . tags
      -- Having no tags counts as matching everything
    tagsMatch n = null $ tgs \\ (tags n)
      -- If it has tags, then it should match all of the selected ones.

hasChildren :: Node a -> Bool
hasChildren = not . null . children


recursively     :: (Node a -> Node a) -> Node a -> Node a
recursively f n = let n' = f n in n' { children = map f $ children n' }

addTags       :: [Tag] -> Node a -> Node a
addTags tgs n = n { tags = nub $ tgs ++ tags n }


storeTree   :: Node () -> YesodDB App ()
storeTree t = storeNode t >> mapM_ storeNode (children t)


storeNode   :: Node () -> YesodDB App ()
storeNode t = do
    let i    = nodeId t
        tagSet = S.fromList $ tags t
    tagList <- filter (\e -> entityVal e `S.member` tagSet) <$> selectList [] []
    deleteWhere [TagNodeStoreNodeId ==. i]
    insertMany_ . map (\e -> TagNodeStore (entityKey e) i) $ tagList
    replace i (dbNode t)

size :: Node a -> Int
size = (1 +) . sum . map size . children
