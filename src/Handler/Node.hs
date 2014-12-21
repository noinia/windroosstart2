module Handler.Node where

import qualified Data.Map as M
import           Control.Applicative
import           Data.Maybe(fromJust)
import           Data.Text(unpack)
import           Import
import           System.Directory(removeFile)
import           System.FilePath

import           Debug.Trace


import qualified Data.Text as T
import qualified Data.ByteString.Char8 as B

import Handler.Tag(allTags)


--------------------------------------------------------------------------------

getTreeR   :: NodeId -> Handler Html
getTreeR i = getTreeFromDB i "getTree" $ \t ->
             (defaultLayout $ $(widgetFile "tree"))


--------------------------------------------------------------------------------

getNodeAddR   :: NodeId -> Handler Html
getNodeAddR i = getTreeFromDB i "add" $ \t -> do
  tags <- allTags
  (formWidget, enctype) <- generateFormPost $ nodeForm t Nothing tags
  let act = NodeAddR i
  defaultLayout $ $(widgetFile "nodeAdd")

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

deleteNodeRemoveR   :: NodeId -> Handler Html
deleteNodeRemoveR i
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
getNodeUpdateR i = getTreeFromDB rootId "edit" $ \r -> case withId i r of
  Nothing -> return "error"
  Just t  -> do
    tags <- allTags
    (formWidget, enctype) <- generateFormPost $ nodeForm r (Just t) tags
    let act        = NodeUpdateR i
        editWidget = $(widgetFile "nodeAdd")
    defaultLayout $ $(widgetFile "nodeUpdate")

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

getTreeFromDB       :: NodeId -> Html -> (Node () -> Handler Html) -> Handler Html
getTreeFromDB i m h = runDB (getTree i) >>= \x -> case x of
    Nothing -> return $ "error: " <> m
    Just t  -> h t


--------------------------------------------------------------------------------

type DBNodeConstructor = (Maybe ContentType -> DBNode,Maybe FileInfo,[TagId])


dbNodeC              :: NodeId -> Text -> Maybe URL -> Maybe FileInfo -> [TagId]
                     -> DBNodeConstructor
dbNodeC p n l fi tgs = (\ct -> DBNode p n l (B.unpack <$> ct), fi, tgs)

-- | First param is the root of the tree, This is assumed to be the default parent
-- for a new node.
nodeForm               :: Node a -> Maybe (Node a) -> (M.Map TagId Tag)
                       -> Form DBNodeConstructor
nodeForm root n allTgs = renderDivs $
    dbNodeC <$> areq (nodeField root)          "Ouder" parent
            <*> areq textField                 "Naam"  (description  <$> n)
            <*> aopt urlField                  "Link"  (url          <$> n)
            <*> fileAFormOpt                   "Afbeelding"
            <*> areq (checkboxesFieldList tgs) "Tags"  (tagIds allTgs <$> n)
  where
    parent = (parentId <$> n) <|> (Just $ nodeId root)
    tgs    :: [(Text,TagId)]
    tgs    = map (\(i,(Tag t)) -> (t,i)) . M.toList $ allTgs



nodeField   :: Node a -> Field Handler NodeId
nodeField t = selectFieldList $ asList t
  where
    asList n = (description n, nodeId n) : concatMap asList (children n)


--------------------------------------------------------------------------------

mImage t = $(widgetFile "mimage")

mLink t = $(widgetFile "mlink")

level3 t = $(widgetFile "level3")

level2 t = $(widgetFile "level2")

level1 t = $(widgetFile "level1")
