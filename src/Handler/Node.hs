module Handler.Node where

import           Control.Applicative
import           Data.Maybe(fromJust)
import           Data.Text(unpack)
import           Import
import           System.FilePath

import           Debug.Trace


import qualified Data.Text as T
import qualified Data.ByteString.Char8 as B

--------------------------------------------------------------------------------

getTreeR   :: NodeId -> Handler Html
getTreeR i = getTreeFromDB i "getTree" $ \t ->
             (defaultLayout $ $(widgetFile "tree"))


--------------------------------------------------------------------------------

getNodeAddR   :: NodeId -> Handler Html
getNodeAddR i = getTreeFromDB i "add" $ \t -> do
  (formWidget, enctype) <- generateFormPost $ nodeForm t Nothing
  let act = NodeAddR i
  defaultLayout $ $(widgetFile "nodeAdd")

postNodeAddR   :: NodeId -> Handler Html
postNodeAddR i = getTreeFromDB i "add" $ \t -> do
  ((result,_), _) <- runFormPost $ nodeForm t Nothing
  case result of
    FormSuccess (nc,fi) -> do
                             j <- runDB $ insert (nc Nothing)
                             mfp <- moveFile j fi
                             updateImage j mfp
                             setMessage "Node toegevoegd."
                             redirect (NodeAddR i)
    _                   -> do
                             setMessage "Fout bij het toevoegen."
                             redirect (NodeAddR i)


updateImage   :: NodeId -> Maybe ContentType -> Handler ()
updateImage j = maybe (return ()) (runDB . updateContentType)
  where
    updateContentType ct = update j [DBNodeImage =. Just (B.unpack ct)]


-- | Given a node ID and a maybe file info of the uploaded file, move the file
-- , scale it etc, and return the extension of the new file.
moveFile             :: NodeId -> Maybe FileInfo -> Handler (Maybe ContentType)
moveFile i Nothing   = return Nothing
moveFile i (Just fi)
  | isAllowed fi     = getFilesPath >>= \p ->
                       let
                           contType = fileContentType' fi
                           ext = fromJust $ lookup contType allowedContentTypes
                           n   = imageName i
                           fp  = p </> n <.> ext
                       in liftIO $ fileMove fi fp >> return (Just contType)
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
  | otherwise       = undefined


--------------------------------------------------------------------------------

getNodeUpdateR    :: NodeId -> Handler Html
getNodeUpdateR i = getTreeFromDB rootId "edit" $ \r -> case withId i r of
  Nothing -> return "error"
  Just t  -> do
    (formWidget, enctype) <- generateFormPost $ nodeForm r (Just t)
    let act        = NodeUpdateR i
        editWidget = $(widgetFile "nodeAdd")
    defaultLayout $ $(widgetFile "nodeUpdate")

postNodeUpdateR    :: NodeId -> Handler Html
postNodeUpdateR i
  | i == rootId    = do setMessage "Het is niet mogelijk de root node aan te passen"
                        redirect (NodeUpdateR i)
  | otherwise      = getTreeFromDB rootId "edit" $ \t -> do
    ((result,_), _) <- runFormPost $ nodeForm t (withId i t)
    case result of
      FormSuccess (nc,fi) -> do
                               mfp <- moveFile i fi
                               let t' = nc mfp
                               runDB $ replace i t'
                               let msg = "Node " <> dBNodeDescription t' <> " aangepast."
                               setMessage (toHtml msg)
                               redirect (NodeUpdateR i)
      _                   -> do
                               setMessage "Fout bij het aanpassen."
                               redirect (NodeUpdateR i)



--------------------------------------------------------------------------------

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

type DBNodeConstructor = (Maybe ContentType -> DBNode,Maybe FileInfo)


dbNodeC           :: NodeId -> Text -> Maybe URL -> Maybe FileInfo -> DBNodeConstructor
dbNodeC p n l fi = (\ct -> DBNode p n l (B.unpack <$> ct), fi)

-- | First param is the root of the tree, This is assumed to be the default parent
-- for a new node.
nodeForm       :: Node a -> Maybe (Node a) -> Form DBNodeConstructor
nodeForm root n = renderDivs $
                  dbNodeC <$> areq (nodeField root) "Ouder" parent
                          <*> areq textField        "Naam"  (description <$> n)
                          <*> aopt urlField         "Link"  (url         <$> n)
                          <*> fileAFormOpt          "Afbeelding"
  where
    parent = (parentId <$> n) <|> (Just $ nodeId root)

nodeField   :: Node a -> Field Handler NodeId
nodeField t = selectFieldList $ asList t
  where
    asList n = (description n, nodeId n) : concatMap asList (children n)
