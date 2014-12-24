module Handler.User where

import           Import
import           Yesod.Auth.HashDB(setPassword)

-- getAddUserR = do

userAdminWidget :: Handler Widget
userAdminWidget = do
  (formWidget, enctype) <- generateFormPost addUserForm
  let act = UserAddR
  return $(widgetFile "userAdd")


postUserAddR :: Handler Html
postUserAddR = do
  ((result, _), _) <- runFormPost addUserForm
  case result of
    FormSuccess (n,p) -> do u <- setPassword p $ User n Nothing
                            runDB $ insert_ u
                            setMessage "Gebruiker toegevoegd"
                            redirect AdminR
    _                 -> do setMessage "Fout bij het toevoegen."
                            redirect AdminR



addUserForm :: Form (Text,Text)
addUserForm = renderDivs $ (,) <$> areq textField     "Gebruikersnaam"      Nothing
                               <*> areq passwordField "Wachtwoord"          Nothing
