module Handler.Node where

import Import


getTreeR   :: NodeId -> Handler Html
getTreeR i = runDB (getTree i) >>= \x -> case x of
    Nothing -> return "error"
    Just t  -> defaultLayout $ $(widgetFile "tree")

getNodeAddR :: Handler Html
getNodeAddR = undefined

postNodeAddR :: Handler Html
postNodeAddR = undefined

getNodeRemoveR   :: NodeId -> Handler Html
getNodeRemoveR i = undefined


deleteNodeRemoveR   :: NodeId -> Handler Html
deleteNodeRemoveR i = undefined


getNodeUpdateR   :: NodeId -> Handler Html
getNodeUpdateR i = undefined

postNodeUpdateR   :: NodeId -> Handler Html
postNodeUpdateR i = undefined
