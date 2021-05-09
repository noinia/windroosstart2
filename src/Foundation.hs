{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE InstanceSigs #-}
module Foundation where


import           Control.Applicative
import           Control.Monad(when)
import           Control.Monad.Trans.Reader(ReaderT)
import           Data.Monoid
import           Data.Proxy
import qualified Data.Text.IO as TIO
import qualified Database.Persist
import           Database.Persist.Sql (SqlBackend)
import           Model
import           Network.HTTP.Client.Conduit (Manager, HasHttpManager (getHttpManager))
import           Prelude
import qualified Settings
import           Settings (widgetFile, Extra (..))
import           Settings.Development (development)
import           Settings.StaticFiles
import           Text.Hamlet (hamletFile)
import           Text.Jasmine (minifym)
import           Yesod
import           Yesod.Auth
import           Yesod.Auth.HashDB(authHashDB, HashDBUser(..), setPassword)
import           Yesod.Core.Types (Logger)
import           Yesod.Default.Config
import           Yesod.Default.Util (addStaticContentExternal)
import           Yesod.Static
import qualified Yesod.Auth.Message as Msg


-- | The site argument for your application. This can be a good place to
-- keep settings and values requiring initialization before your application
-- starts running, such as database connections. Every handler will have
-- access to the data present here.
data App = App
    { settings :: AppConfig DefaultEnv Extra
    , getStatic :: Static -- ^ Settings for static file serving.
    , connPool :: Database.Persist.PersistConfigPool Settings.PersistConf -- ^ Database connection pool.
    , httpManager :: Manager
    , persistConfig :: Settings.PersistConf
    , appLogger :: Logger
    }

instance HasHttpManager App where
    getHttpManager = httpManager

-- Set up i18n messages. See the message folder.
mkMessage "App" "messages" "en"

-- This is where we define all of the routes in our application. For a full
-- explanation of the syntax, please see:
-- http://www.yesodweb.com/book/routing-and-handlers
--
-- Note that this is really half the story; in Application.hs, mkYesodDispatch
-- generates the rest of the code. Please see the linked documentation for an
-- explanation for this split.
mkYesodData "App" $(parseRoutesFile "config/routes")

type Form x = Html -> MForm (HandlerT App IO) (FormResult x, Widget)

instance YesodAuthPersist App where
  type AuthEntity App = User

-- Please see the documentation for the Yesod typeclass. There are a number
-- of settings which can be configured by overriding methods here.
instance Yesod App where
    approot = ApprootMaster $ appRoot . settings

    -- Store session data on the client in encrypted cookies,
    -- default session idle timeout is 120 minutes
    makeSessionBackend _ = fmap Just $ defaultClientSessionBackend
        120    -- timeout in minutes
        "config/client_session_key.aes"

    defaultLayout widget = do
        master <- getYesod
        mmsg   <- getMessage
        muser  <- maybeAuthId >>= maybe (return Nothing) getAuthEntity

        -- We break up the default layout into two components:
        -- default-layout is the contents of the body tag, and
        -- default-layout-wrapper is the entire page. Since the final
        -- value passed to hamletToRepHtml cannot be a widget, this allows
        -- you to use normal widget features in default-layout.

        pc <- widgetToPageContent $ do
            addScript $ StaticR js_jquery_js
            addScript $ StaticR js_bootstrap_js
            addScript $ StaticR js_bootstrap_confirmation_js
            addStylesheet $ StaticR css_bootstrap_css
            $(widgetFile "default-layout")
        withUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")

    -- This is done to provide an optimization for serving static files from
    -- a separate domain. Please see the staticRoot setting in Settings.hs
    -- urlRenderOverride y (StaticR s) =
    --     Just $ uncurry (joinPath y (Settings.staticRoot $ settings y)) $ renderRoute s
    -- urlRenderOverride _ _ = Nothing

    -- The page to be redirected to when authentication is required.
    authRoute _ = Just $ AuthR LoginR

    -- Routes not requiring authentication.
    isAuthorized (AuthR _) _ = return Authorized
    isAuthorized FaviconR  _ = return Authorized
    isAuthorized RobotsR   _ = return Authorized

    -- Freely allow accessing the public pages, block the rest
    isAuthorized HomeR      _ = return Authorized
    isAuthorized OnderbouwR _ = return Authorized
    isAuthorized BovenbouwR _ = return Authorized
    -- not sure we should make this one publically available
    isAuthorized TeacherR   _ = return Authorized
    -- Allow access to the tree pages
    isAuthorized (TreeR _) _         = return Authorized
    isAuthorized (ChildrenR _) _     = return Authorized
    isAuthorized (TreeTagR _ _) _    = return Authorized
    isAuthorized (TreeNotTagR _ _) _ = return Authorized
    isAuthorized (TreeTagRootR _) _  = return Authorized
    -- Allow access to the images
    isAuthorized (ImageR _)       _ = return Authorized

    -- For the remaining pages, require authorization
    isAuthorized _ _ = maybe AuthenticationRequired (const Authorized) <$> maybeAuthId

    -- This function creates static content files in the static folder
    -- and names them based on a hash of their content. This allows
    -- expiration dates to be set far in the future without worry of
    -- users receiving stale content.
    addStaticContent =
        addStaticContentExternal minifym genFileName Settings.staticDir (StaticR . flip StaticRoute [])
      where
        -- Generate a unique filename based on the content itself
        genFileName lbs
            | development = "autogen-" ++ base64md5 lbs
            | otherwise   = base64md5 lbs

    -- Place Javascript at bottom of the body tag so the rest of the page loads first
    jsLoader _ = BottomOfBody

    -- What messages should be logged. The following includes all messages when
    -- in development, and warnings and errors in production.
    -- shouldLog _ _source level =
    --     development || level == LevelWarn || level == LevelError

    makeLogger = return . appLogger

-- How to run database actions.
instance YesodPersist App where
    type YesodPersistBackend App = SqlBackend
    runDB = defaultRunDB persistConfig connPool
instance YesodPersistRunner App where
    getDBRunner = defaultGetDBRunner connPool

instance HashDBUser User where
    userPasswordHash = userPassword
    setPasswordHash h u = u { userPassword = Just h }

instance YesodAuth App where
    type AuthId App = UserId

    -- Where to send a user after successful login
    loginDest _ = AdminR
    -- Where to send a user after logout
    logoutDest _ = HomeR

    authenticate       :: (MonadHandler m, HandlerSite m ~ App)
                       => Creds App -> m (AuthenticationResult App)
    authenticate creds = liftHandler $
                         maybeAuthId >>= \case
                           Just uid -> pure $ Authenticated uid
                           Nothing  -> lookupInDB
      where
        lookupInDB = let ci = credsIdent creds
                         uu = UniqueUser ci
                     in runDB (getBy uu) >>= \case
                          Just (Entity uid _) -> pure $ Authenticated uid
                          Nothing             -> pure $ UserError Msg.InvalidLogin


    -- You can add other plugins like BrowserID, email or OAuth here
    authPlugins _ = [ authHashDB (Just . UniqueUser) ]

    -- authHttpManager = httpManager


-- This instance is required to use forms. You can modify renderMessage to
-- achieve customized and internationalized form validation messages.
instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

-- | Get the 'Extra' value, used to hold data from the settings.yml file.
getExtra :: Handler Extra
getExtra = fmap (appExtra . settings) getYesod

-- Note: previous versions of the scaffolding included a deliver function to
-- send emails. Unfortunately, there are too many different options for us to
-- give a reasonable default. Instead, the information is available on the
-- wiki:
--
-- https://github.com/yesodweb/yesod/wiki/Sending-email

getFilesPath :: Handler FilePath
getFilesPath = return "uploaded_images"


adminLayout :: Widget -> Handler Html
adminLayout = defaultLayout


-- | Initialize the database, i.e. make sure that we have a root node, at least one user
--   and a bunch of tags.
initializeDB :: MonadIO m => ReaderT SqlBackend m ()
initializeDB = do
    whenEmpty (Proxy :: Proxy User) $ do
      u <- liftIO createNewUser
      insert_ u
    whenEmpty (Proxy :: Proxy DBNode) $
      repsert rootId (DBNode rootId "root" Nothing Nothing)
    whenEmpty (Proxy :: Proxy Tag) $
      insertMany_ [ Tag "leraar"
                  ]
  where
    createNewUser :: IO User
    createNewUser = do
      putStrLn "No users found, adding new user"
      putStrLn "username: "
      un <- TIO.getLine
      putStrLn "password: "
      pw <- TIO.getLine
      TIO.putStrLn $ "Creating a new user '" <> un <> "' with password '" <> pw <> "'."
      setPassword pw (User un Nothing)

    whenEmpty (Proxy :: Proxy a) h = do
      num <- count ([] :: [Filter a])
      when (num == 0) h


rootId :: NodeId
rootId = DBNodeKey 0
