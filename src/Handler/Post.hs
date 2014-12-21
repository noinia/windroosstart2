module Handler.Post where

import Import

getPostAddR :: Handler Html
getPostAddR = getForm Nothing


getForm    :: Maybe Post -> Handler Html
getForm mp = do
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
getPostUpdateR i = runDB (get i) >>= getForm

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

deletePostRemoveR   :: PostId -> Handler Html
deletePostRemoveR i = do runDB $ delete i
                         setMessage "Post verwijderd."
                         redirect HomeR

postForm    :: Maybe Post -> Form Post
postForm mp = renderDivs $ (\t b (Textarea c) -> Post t b c)
     <$> areq textField     "Titel"      (postTitle              <$> mp)
     <*> areq checkBoxField "Zichtbaar"  (postVisible            <$> mp)
     <*> areq textareaField "Inhoud"     (Textarea . postContent <$> mp)


visiblePosts :: Handler [Post]
visiblePosts = runDB . fmap (map entityVal) $ selectList [PostVisible ==. True] []
