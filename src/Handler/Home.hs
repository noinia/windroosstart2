module Handler.Home where

import Import
import Handler.Node
import Handler.Tag
import Handler.Post

--------------------------------------------------------------------------------

getHomeR  :: Handler Html
getHomeR = getTreeR rootId

getOnderbouwR :: Handler Html
getOnderbouwR = getTreeTagRootR $ TagText "onderbouw"


getBovenbouwR :: Handler Html
getBovenbouwR = getTreeTagRootR $ TagText "bovenbouw"


getTeacherR :: Handler Html
getTeacherR = getTreeTagRootR $ TagText "leraar"


getAdminR :: Handler Html
getAdminR = do
    tagAdmin  <- tagAdminWidget
    postAdmin <- postAdminWidget
    nodeAdmin <- nodeAdminWidget rootId
    defaultLayout $ $(widgetFile "admin")
