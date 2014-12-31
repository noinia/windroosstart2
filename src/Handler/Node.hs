module Handler.Node where


import qualified Data.Map as M
import           Control.Applicative
import           Data.Maybe(fromJust, fromMaybe)
import           Data.Text(unpack)
import           Import
import           System.Directory(removeFile)
import           System.FilePath
import           Text.Julius(RawJS(..))

import           Debug.Trace


import qualified Data.Text as T
import qualified Data.ByteString.Char8 as B

import Handler.Tag(allTags)
import Handler.Post(visiblePosts)

--------------------------------------------------------------------------------

getChildrenR   :: NodeId -> Handler Html
getChildrenR i = getTreeFromDB i "getChildren" $ \ft -> do
  let t   = ft { children = map f $ children ft }
      f n = n { children = [] }
  withPosts t


withPosts   :: Node a -> Handler Html
withPosts t = do
               posts <- visiblePosts
               adminLayout $ $(widgetFile "tree")


getTreeR   :: NodeId -> Handler Html
getTreeR i = getTreeFromDB i "getTree" withPosts

getTreeNotTagR                :: NodeId -> TagText -> Handler Html
getTreeNotTagR i (TagText tg) = getTreeFromDB i "getTree" (byTags False tg)

getTreeTagR                :: NodeId -> TagText -> Handler Html
getTreeTagR i (TagText tg) = getTreeFromDB i "getTree" (byTags True tg)


byTags         :: Bool -> Text -> Node a -> Handler Html
byTags b tg t' = case filterByTags b [Tag tg] t' of
    Nothing -> notFound
    Just t  -> withPosts t

getTreeTagRootR :: TagText -> Handler Html
getTreeTagRootR = getTreeTagR rootId

--------------------------------------------------------------------------------

getNodeAddR   :: NodeId -> Handler Html
getNodeAddR i = getTreeFromDB i "add" $ \t -> do
  tags <- allTags
  (formWidget, enctype) <- generateFormPost $ nodeForm t Nothing tags
  let act = NodeAddR i
  adminLayout $ $(widgetFile "nodeAdd")

postNodeAddR   :: NodeId -> Handler Html
postNodeAddR i = getTreeFromDB i "add" $ \t -> do
  tags <- allTags
  ((result,_), _) <- runFormPost $ nodeForm t Nothing tags
  case result of
    FormSuccess (nc,fi,tgs) -> do
                                 let dbNodeJ = nc Nothing
                                 j <- runDB $ insert dbNodeJ
                                 mfp <- moveImageFile (Node j dbNodeJ () [] []) fi
                                 updateImage j mfp
                                 runDB . insertMany_ . map (flip TagNodeStore j) $ tgs
                                 setMessage "Node toegevoegd."
                                 redirect (NodeAddR i)
    _                       -> do
                                 setMessage "Fout bij het toevoegen."
                                 redirect (NodeAddR i)


updateImage   :: NodeId -> Maybe ContentType -> Handler ()
updateImage j = maybe (return ()) (runDB . updateContentType)
  where
    updateContentType ct = update j [DBNodeImage =. Just (B.unpack ct)]


-- | Given a node ID and a maybe file info of the uploaded file, move the file
-- , scale it etc, and return the extension of the new file.
moveImageFile                :: Node a -> Maybe FileInfo -> Handler (Maybe ContentType)
moveImageFile _    Nothing   = return Nothing
moveImageFile node (Just fi)
  | isAllowed fi     = do
                         basePath <- getFilesPath
                         let ct     = fileContentType' fi
                             node'  = node { dbNode = (dbNode node) {
                                                dBNodeImage = Just . B.unpack $ ct}}
                         deleteImage node -- remove the old image
                         let fp = basePath </> fromJust (imageFileName node')
                         liftIO $ fileMove fi fp -- put the new image in place
                         return $ Just ct
  | otherwise      = do
                       setMessage "Bestandsformaat niet toegestaan"
                       return Nothing


fileContentType' :: FileInfo -> ContentType
fileContentType' = B.pack . T.unpack . fileContentType

isAllowed    :: FileInfo -> Bool
isAllowed fi = fileContentType' fi `elem` (map fst allowedContentTypes)


--------

getNodeRemoveR   :: NodeId -> Handler Html
getNodeRemoveR i
  | i == rootId     = do setMessage "Het is niet mogelijk de root te verwijderen"
                         redirect (NodeUpdateR i)
  | otherwise       = getTreeFromDB i "edit" $ \t -> do
                        deleteImage t
                        runDB $ deleteWhere [TagNodeStoreNodeId ==. i]
                                >>
                                delete i
                        setMessage $ "Node " <> toHtml (description t) <> " verwijderd."
                        redirect (NodeUpdateR $ parentId t)


deleteImage :: Node a -> Handler ()
deleteImage = maybe (return ()) rm . imageFileName
  where
    rm n = getFilesPath >>= \fp -> liftIO . removeFile $ fp </> n

--------------------------------------------------------------------------------

getNodeUpdateR    :: NodeId -> Handler Html
getNodeUpdateR i = nodeAdminWidget i >>= adminLayout

nodeAdminWidget   :: NodeId -> Handler Widget
nodeAdminWidget i = getTreeFromDB rootId (errorW "edit") $ \r -> case withId i r of
  Nothing -> return $ errorW "Node niet gevonden."
  Just t  -> do
    tags <- allTags
    (formWidget, enctype) <- generateFormPost $ nodeForm r (Just t) tags
    let act        = NodeUpdateR i
        editWidget = $(widgetFile "nodeAdd")
    return $(widgetFile "nodeUpdate")




postNodeUpdateR    :: NodeId -> Handler Html
postNodeUpdateR i
  | i == rootId    = do setMessage "Het is niet mogelijk de root node aan te passen"
                        redirect (NodeUpdateR i)
  | otherwise      = getTreeFromDB rootId "edit" $ \t -> do
    tags <- allTags
    let mNodeI = withId i t
    ((result,_), _) <- runFormPost $ nodeForm t mNodeI tags
    case (mNodeI,result) of
      (Just nodeI, FormSuccess (nc,fi,tgs)) -> do
            mfp <- moveImageFile nodeI fi
            let t'  = nc mfp
                msg = "Node " <> dBNodeDescription t' <> " aangepast."
            runDB $ deleteWhere [TagNodeStoreNodeId ==. i]
                    >> (insertMany_ . map (flip TagNodeStore i)) tgs
                    >> replace i t'
            setMessage (toHtml msg)
            redirect (NodeUpdateR i)
      _                                 -> do
            setMessage "Fout bij het aanpassen."
            redirect (NodeUpdateR i)



--------------------------------------------------------------------------------


getImageR   :: NodeId -> Handler Html
getImageR i = getTreeFromDB i "image" $ \t -> do
    fp   <- getFilesPath
    case image t of
      Nothing      -> notFound
      Just (ct,fn) -> do
        liftIO . putStrLn $ (fp </> fn)
        sendFile ct (fp </> fn)


--------------------------------------------------------------------------------

getTreeFromDB       :: NodeId -> a -> (Node () -> Handler a) -> Handler a
getTreeFromDB i m h = runDB (getTree i) >>= \x -> case x of
    Nothing -> return m
    Just t  -> h t


--------------------------------------------------------------------------------

type DBNodeConstructor = (Maybe ContentType -> DBNode,Maybe FileInfo,[TagId])


dbNodeC              :: NodeId -> Text -> Maybe URL -> Maybe FileInfo -> Maybe [TagId]
                     -> DBNodeConstructor
dbNodeC p n l fi tgs = (\ct -> DBNode p n l (B.unpack <$> ct), fi, fromMaybe [] tgs)

-- | First param is the root of the tree, This is assumed to be the default parent
-- for a new node.
nodeForm               :: Node a -> Maybe (Node a) -> (M.Map TagId Tag)
                       -> Form DBNodeConstructor
nodeForm root n allTgs = renderDivs $
    dbNodeC <$> areq (nodeField root)          "Ouder" parent
            <*> areq textField                 "Naam"  (description  <$> n)
            <*> aopt urlField                  "Link"  (url          <$> n)
            <*> fileAFormOpt                   "Afbeelding"
            <*> aopt (checkboxesFieldList tgs) "Tags"  (Just (tagIds allTgs <$> n))
  where
    parent = (parentId <$> n) <|> (Just $ nodeId root)
    tgs    :: [(Text,TagId)]
    tgs    = map (\(i,(Tag t)) -> (t,i)) . M.toList $ allTgs



nodeField   :: Node a -> Field Handler NodeId
nodeField t = selectFieldList $ asList t
  where
    asList n = (description n, nodeId n) : concatMap asList (children n)


--------------------------------------------------------------------------------

mImage  :: Node a -> Widget
mImage t = $(widgetFile "mimage")

mLink    :: Node a -> Widget
mLink t = $(widgetFile "mlink")


level3    :: Node a -> Widget
level3 t = $(widgetFile "level3")

level2    :: Node a -> Widget
level2 t = $(widgetFile "level2")

level1    :: Node a -> Widget
level1 t = $(widgetFile "level1")

errorW   :: Text -> Widget
errorW e = $(widgetFile "error")

nodeChildrenHtmlId   :: Node a -> Text
nodeChildrenHtmlId t =  nodeHtmlId t <> "_children"

nodeHtmlId :: Node a -> Text
nodeHtmlId = T.pack . show . toInteger . unDBNodeKey . nodeId
