module Handler.Tag where

import Import


allTags :: Handler [Tag]
allTags = runDB . fmap (map entityVal) $ selectList [] []

getTagAdminR :: Handler Html
getTagAdminR = do
    tags <- allTags
    (formWidget, enctype) <- generateFormPost tagForm
    defaultLayout $ $(widgetFile "tagAdmin")



-- tagForm :: AForm Handler TagText
tagForm = renderDivs $ TagText <$> areq tagField "tag" Nothing
  where
    tagField = check (fmap unTT . tagText) textField
    -- the unwrapping + rewrapping is a bit silly.


postTagCreateR :: Handler Html
postTagCreateR = do
    ((result,_), _) <- runFormPost tagForm
    case result of
      FormSuccess (TagText t) -> do _ <- runDB $ insert (Tag t)
                                    setMessage "Tag toegevoegd."
      _                       -> setMessage "Fout bij het toevoegen."
    redirect TagAdminR
