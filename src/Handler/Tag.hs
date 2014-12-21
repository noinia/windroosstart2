module Handler.Tag where

import Import
import qualified Data.Map as M

allTags :: Handler (M.Map TagId Tag)
allTags = runDB . fmap (M.fromList . map f) $ selectList [] []
  where
    f e = (entityKey e, entityVal e)

getTagAdminR :: Handler Html
getTagAdminR = do
    tags <- M.elems <$> allTags
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
      FormSuccess (TagText t) -> do runDB $ insert_ (Tag t)
                                    setMessage "Tag toegevoegd."
      _                       -> setMessage "Fout bij het toevoegen."
    redirect TagAdminR
