module Handler.Node where

import Import


getTreeFromDB i m h = runDB (getTree i) >>= \x -> case x of
    Nothing -> return $ "error: " <> m
    Just t  -> h t


getTreeR   :: NodeId -> Handler Html
getTreeR i = getTreeFromDB i "getTree" $ \t ->
             (defaultLayout $ $(widgetFile "tree"))

getNodeAddR    :: NodeId -> Handler Html
getNodeAddR pi = getTreeFromDB pi "add" $ \t -> do
  (formWidget, enctype) <- generateFormPost $ nodeForm t t
  defaultLayout [whamlet|
                 <form enctype=#{enctype}>
                 ^{formWidget}
                 |]



type DBNodeConstructor = (Maybe FilePath -> DBNode,Maybe FileInfo)


dbNodeC p n l fi = (\fp -> DBNode p n l fp, fi)

nodeForm       :: Node a -> Node a -> Form DBNodeConstructor
nodeForm root p = renderDivs $
                  dbNodeC <$> areq (nodeField root) "Ouder" (Just $ nodeId p)
                          <*> areq textField        "Naam"  Nothing
                          <*> aopt urlField         "Link"  Nothing
                          <*> fileAFormOpt          "Afbeelding"

nodeField   :: Node a -> Field Handler NodeId
nodeField t = selectFieldList $ asList t
  where
    asList n = (description n, nodeId n) : concatMap asList (children n)


postNodeAddR    :: NodeId -> Handler Html
postNodeAddR pi = undefined

getNodeRemoveR   :: NodeId -> Handler Html
getNodeRemoveR i = undefined


deleteNodeRemoveR   :: NodeId -> Handler Html
deleteNodeRemoveR i = undefined


getNodeUpdateR   :: NodeId -> Handler Html
getNodeUpdateR i = undefined

postNodeUpdateR   :: NodeId -> Handler Html
postNodeUpdateR i = undefined
