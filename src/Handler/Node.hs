module Handler.Node where

import Control.Applicative
import Data.Text(unpack)
import Import

import Debug.Trace

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
                             mfp <- moveFile fi
                             updateImage j mfp
                             redirect (NodeAddR i)
    _                   -> do
                             setMessage "Fout bij het toevoegen."
                             redirect (NodeAddR i)



updateImage   :: NodeId -> Maybe FilePath -> Handler ()
updateImage j = maybe (return ()) (runDB . updateFp)
  where
    updateFp fp = update j [DBNodeImage =. Just fp]


moveFile           :: Maybe FileInfo -> Handler (Maybe String)
moveFile Nothing   = return Nothing
moveFile (Just fi)
  | isAllowed fi   = getFilesPath >>= \p ->
                       let n  = unpack $ fileName fi
                           fp = p <> "/" <> n
                       in liftIO $ fileMove fi fp >> return (Just fp)
  | otherwise      = do
                       setMessage "Bestandsformaat niet toegestaan"
                       return Nothing


allowedContentTypes :: [Text]
allowedContentTypes = ["image/png", "image/jpg", "image/jpeg"]

isAllowed    :: FileInfo -> Bool
isAllowed fi | traceShow (fileContentType fi) False = undefined
isAllowed fi = fileContentType fi `elem` allowedContentTypes


--------

getNodeRemoveR   :: NodeId -> Handler Html
getNodeRemoveR i = undefined


deleteNodeRemoveR   :: NodeId -> Handler Html
deleteNodeRemoveR i = undefined


--------------------------------------------------------------------------------

getNodeUpdateR    :: NodeId -> Handler Html
getNodeUpdateR i = getTreeFromDB i "edit" $ \t -> do
    (formWidget, enctype) <- generateFormPost $ nodeForm t (Just t)
    let act        = NodeUpdateR i
        editWidget = $(widgetFile "nodeAdd")
    defaultLayout $ $(widgetFile "nodeUpdate")

postNodeUpdateR   :: NodeId -> Handler Html
postNodeUpdateR i = getTreeFromDB i "edit" $ \t -> do
    ((result,_), _) <- runFormPost $ nodeForm t (Just t)
    case result of
      FormSuccess (nc,fi) -> do
                               mfp <- moveFile fi
                               runDB $ replace i (nc mfp)
                               redirect (NodeAddR i)
      _                   -> do
                               setMessage "Fout bij het aanpassen."
                               redirect (NodeAddR i)

--------------------------------------------------------------------------------

getTreeFromDB       :: NodeId -> Html -> (Node () -> Handler Html) -> Handler Html
getTreeFromDB i m h = runDB (getTree i) >>= \x -> case x of
    Nothing -> return $ "error: " <> m
    Just t  -> h t

--------------------------------------------------------------------------------

type DBNodeConstructor = (Maybe FilePath -> DBNode,Maybe FileInfo)


dbNodeC           :: NodeId -> Text -> Maybe URL -> Maybe FileInfo -> DBNodeConstructor
dbNodeC p n l fi = (\fp -> DBNode p n l fp, fi)

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
