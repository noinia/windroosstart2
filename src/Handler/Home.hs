module Handler.Home where

import Import
import Handler.Node
import Handler.Tag
import Handler.Post
import Handler.User

--------------------------------------------------------------------------------

bovenbouwId :: Key DBNode
bovenbouwId = DBNodeKey 462

onderbouwId :: Key DBNode
onderbouwId = DBNodeKey 463

teacherTag :: TagText
teacherTag = TagText "leraar"


getHomeR  :: Handler Html
getHomeR = getTreeNotTagR rootId teacherTag

getOnderbouwR :: Handler Html
getOnderbouwR = getTreeNotTagR onderbouwId teacherTag

getBovenbouwR :: Handler Html
getBovenbouwR = getTreeNotTagR bovenbouwId teacherTag


getTeacherR :: Handler Html
getTeacherR = getTreeTagRootR $ TagText "leraar"


getAdminR :: Handler Html
getAdminR = do
    tagAdmin  <- tagAdminWidget
    postAdmin <- postAdminWidget
    nodeAdmin <- nodeAdminWidget rootId
    userAdmin <- userAdminWidget
    defaultLayout $ $(widgetFile "admin")
