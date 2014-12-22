module Handler.Post where

import Import
import Yesod.Core(Route)


getPostAddR :: Handler Html
getPostAddR = getForm PostAddR Nothing

getForm        :: Route App -> Maybe Post -> Handler Html
getForm act mp = do
  (formWidget, enctype) <- generateFormPost $ postForm mp
  (defaultLayout $ $(widgetFile "post"))

postPostAddR :: Handler Html
postPostAddR = do
  ((result, _), _) <- runFormPost $ postForm Nothing
  case result of
    FormSuccess p -> do runDB $ insert_ p
                        setMessage "Post toegevoegd"
                        redirect HomeR
    _             -> do setMessage "Fout bij het toevoegen."
                        redirect PostAddR


getPostUpdateR   :: PostId -> Handler Html
getPostUpdateR i = runDB (get i) >>= getForm (PostUpdateR i)

postPostUpdateR   :: PostId -> Handler Html
postPostUpdateR i = do
  mp <- runDB $ get i
  ((result, _), _) <- runFormPost $ postForm mp
  case result of
    FormSuccess p -> do runDB $ replace i p
                        setMessage "Post geupdate."
                        redirect HomeR
    _             -> do setMessage "Fout bij het toevoegen."
                        redirect PostAddR

getPostRemoveR   :: PostId -> Handler Html
getPostRemoveR i = do runDB $ delete i
                      setMessage "Post verwijderd."
                      redirect AdminR

postForm    :: Maybe Post -> Form Post
postForm mp = renderDivs $ (\t b (Textarea c) -> Post t b c)
     <$> areq textField     "Titel"      (postTitle              <$> mp)
     <*> areq checkBoxField "Zichtbaar"  (postVisible            <$> mp)
     <*> areq textareaField "Inhoud"     (Textarea . postContent <$> mp)


visiblePosts :: Handler [Post]
visiblePosts = runDB . fmap (map entityVal) $ selectList [PostVisible ==. True] []


postAdminWidget :: Handler Widget
postAdminWidget = do
    posts <- runDB . fmap (map f) $ selectList [] []
    return $(widgetFile "postAdmin")
  where
    f e = (entityKey e, entityVal e)
