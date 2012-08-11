{-# LANGUAGE RankNTypes #-}
module Foundation
    ( App (..)
    , Route (..)
    , AppMessage (..)
    , resourcesApp
    , Handler
    , Widget
    , Form
    , maybeAuth
    , requireAuth
    , requireAuthId
    , module Settings
    , module Model
    ) where

import Prelude
import Yesod
import Yesod.Static
import Yesod.Auth
import Yesod.Auth.BrowserId
import Yesod.Auth.Email
import Yesod.Auth.GoogleEmail
import Yesod.Default.Config
import Yesod.Default.Util (addStaticContentExternal)
import Network.HTTP.Conduit (Manager)
import qualified Settings
import qualified Database.Persist.Store
import Settings.StaticFiles
import Database.Persist.GenericSql
import Settings (widgetFile, Extra (..))
import Model
import Text.Jasmine (minifym)
import Web.ClientSession (getKey)
import Text.Hamlet (hamletFile, shamlet)
import Data.Text (Text)
import Data.Maybe (isJust)

import Network.Mail.Mime
import qualified Data.Text.Lazy.Encoding
import Text.Shakespeare.Text (stext)
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import Control.Monad (join)
-- | The site argument for your application. This can be a good place to
-- keep settings and values requiring initialization before your application
-- starts running, such as database connections. Every handler will have
-- access to the data present here.
data App = App
    { settings :: AppConfig DefaultEnv Extra
    , getStatic :: Static -- ^ Settings for static file serving.
    , connPool :: Database.Persist.Store.PersistConfigPool Settings.PersistConfig -- ^ Database connection pool.
    , httpManager :: Manager
    , persistConfig :: Settings.PersistConfig
    }

-- Set up i18n messages. See the message folder.
mkMessage "App" "messages" "en"

-- This is where we define all of the routes in our application. For a full
-- explanation of the syntax, please see:
-- http://www.yesodweb.com/book/handler
--
-- This function does three things:
--
-- * Creates the route datatype AppRoute. Every valid URL in your
--   application can be represented as a value of this type.
-- * Creates the associated type:
--       type instance Route App = AppRoute
-- * Creates the value resourcesApp which contains information on the
--   resources declared below. This is used in Handler.hs by the call to
--   mkYesodDispatch
--
-- What this function does *not* do is create a YesodSite instance for
-- App. Creating that instance requires all of the handler functions
-- for our application to be in scope. However, the handler functions
-- usually require access to the AppRoute datatype. Therefore, we
-- split these actions into two functions and place them in separate files.
mkYesodData "App" $(parseRoutesFile "config/routes")

type Form x = Html -> MForm App App (FormResult x, Widget)

reqAdmin :: forall m s.
            (PersistStore (YesodPersistBackend m) (GHandler s m),
             YesodPersist m, YesodAuth m,
             AuthId m
             ~ Key
                 (YesodPersistBackend m) (UserGeneric (YesodPersistBackend m))) =>
            GHandler s m AuthResult
reqAdmin = do
    mu <- maybeAuth
    return $ case mu of
        Nothing -> AuthenticationRequired
        Just u -> if userAdmin $ entityVal u then
                    Authorized
                  else
                    Unauthorized "You must be an admin"

-- Please see the documentation for the Yesod typeclass. There are a number
-- of settings which can be configured by overriding methods here.
instance Yesod App where
    approot = ApprootMaster $ appRoot . settings

    -- Store session data on the client in encrypted cookies,
    -- default session idle timeout is 120 minutes
    makeSessionBackend _ = do
        key <- getKey "config/client_session_key.aes"
        return . Just $ clientSessionBackend key 120

    defaultLayout widget = do
        master <- getYesod
        mmsg <- getMessage

        -- We break up the default layout into two components:
        -- default-layout is the contents of the body tag, and
        -- default-layout-wrapper is the entire page. Since the final
        -- value passed to hamletToRepHtml cannot be a widget, this allows
        -- you to use normal widget features in default-layout.

        current <- getCurrentRoute
        tm <- getRouteToMaster

        (title', parents) <- breadcrumbs
        muser <- maybeAuth
        
        let langs :: [(Text, Text)]
            langs = [ ("bg", "Bulgarian")
                    , ("en", "English")
                    ]

        let isCurrent :: Route App -> Bool
            isCurrent HomeR = fmap tm current == Just HomeR
            isCurrent x = Just x == fmap tm current || x `elem` map fst parents
        
        let aIsCurrent :: Route App -> Bool
            aIsCurrent x = Just x == fmap tm current

        let isInAdmin :: Bool
            isInAdmin = case fmap tm current of
                             Just x -> x == AdminR || AdminR `elem` map fst parents
                             _ -> False

        let menus :: [ (Route App, AppMessage) ]
            menus = [ (HomeR, MsgHome)
                    , (BlogR, MsgBlog)
                    ]

        let adminContentMenu :: [ (Route App,    AppMessage) ]
            adminContentMenu =  [ (AdminR,       MsgAdminPanel)
                                , (AddPluginR,   MsgAddPlugin) 
                                , (NewEntryR,    MsgAddEntry)
                                ]
        let isCurrentUserAdmin :: Bool
            isCurrentUserAdmin =
                case muser of
                     Just (Entity _ user) -> if userAdmin user then True else False
                     _ -> False
        hc <- widgetToPageContent $ do
            $(widgetFile "header-layout")

        pc <- widgetToPageContent $ do
            addStylesheet $ StaticR css_bootstrap_min_css
            addScript $ StaticR js_jquery_js
            addScript $ StaticR js_bootstrap_min_js
            addScript $ StaticR js_bootstrap_scrollspy_js
            addScript $ StaticR js_bootstrap_collapse_js
            case isInAdmin of
                True -> $(widgetFile "admin/wrapper")
                False -> $(widgetFile "default-layout")

        hamletToRepHtml $(hamletFile "templates/default-layout-wrapper.hamlet")

    -- This is done to provide an optimization for serving static files from
    -- a separate domain. Please see the staticRoot setting in Settings.hs
    urlRenderOverride y (StaticR s) =
        Just $ uncurry (joinPath y (Settings.staticRoot $ settings y)) $ renderRoute s
    urlRenderOverride _ _ = Nothing

    -- The page to be redirected to when authentication is required.
    authRoute _ = Just $ AuthR LoginR

    isAuthorized AdminR           _ = reqAdmin
    isAuthorized AddPluginR       _ = reqAdmin
    isAuthorized AdminListUsersR  _ = reqAdmin
    isAuthorized (DeleteEntryR _) _ = reqAdmin
    isAuthorized NewEntryR        _ = reqAdmin
    isAuthorized (EditEntryR _)   _ = reqAdmin
    isAuthorized _ _ = return Authorized

    -- This function creates static content files in the static folder
    -- and names them based on a hash of their content. This allows
    -- expiration dates to be set far in the future without worry of
    -- users receiving stale content.
    addStaticContent = addStaticContentExternal minifym base64md5 Settings.staticDir (StaticR . flip StaticRoute [])

    -- Place Javascript at bottom of the body tag so the rest of the page loads first
    jsLoader _ = BottomOfBody

    maximumContentLength _ (Just AddPluginR) = 1024 * 1024 * 32 -- 32mb
    maximumContentLength _ _ = 1024 * 1024 * 2 -- 2 mb

instance YesodBreadcrumbs App where
    breadcrumb (EntryR slug) = do
        (Entity _ e) <- runDB $ getBy404 $ UniquePost slug
        return (entryTitle e, Just BlogR)

    -- Admin pages
    breadcrumb AdminListUsersR   = return ("", Just AdminR)
    breadcrumb NewEntryR         = return ("", Just AdminR)
    breadcrumb AddPluginR        = return ("", Just AdminR)
    breadcrumb (EditEntryR _)    = return ("", Just AdminR)

    breadcrumb _                 = return ("", Nothing)

-- How to run database actions.
instance YesodPersist App where
    type YesodPersistBackend App = SqlPersist
    runDB f = do
        master <- getYesod
        Database.Persist.Store.runPool
            (persistConfig master)
            f
            (connPool master)

instance YesodAuth App where
    type AuthId App = UserId

    -- Where to send a user after successful login
    loginDest _ = HomeR
    -- Where to send a user after logout
    logoutDest _ = HomeR
    getAuthId creds = runDB $ do
        x <- getBy $ UniqueUser $ credsIdent creds
        case x of
            Just (Entity uid _) -> return $ Just uid
            Nothing -> do
                fmap Just $ insert $ User (credsIdent creds) Nothing Nothing False False

    -- You can add other plugins like BrowserID, email or OAuth here
    authPlugins _ = [authBrowserId, authGoogleEmail, authEmail]
    authHttpManager = httpManager

instance YesodAuthEmail App where
    type AuthEmailId App = UserId

    addUnverified email verkey =
        runDB $ insert $ User email Nothing (Just verkey) False False

    sendVerifyEmail email _ verurl =
        liftIO $ renderSendMail (emptyMail $ Address Nothing "noreply")
            { mailTo = [Address Nothing email]
            , mailHeaders =
                [ ("Subject", "Verify your email address")
                ]
            , mailParts = [[textPart, htmlPart]]
            }
      where
        textPart = Part
            { partType = "text/plain; charset=utf-8"
            , partEncoding = None
            , partFilename = Nothing
            , partContent = Data.Text.Lazy.Encoding.encodeUtf8 [stext|
Please confirm your email address by clicking on the link below.

\#{verurl}

Thank you
|]
            , partHeaders = []
            }
        htmlPart = Part
            { partType = "text/html; charset=utf-8"
            , partEncoding = None
            , partFilename = Nothing
            , partContent = renderHtml [shamlet|
$newline always
<p>Please confirm your email address by clicking on the link below.
<p>
    <a href=#{verurl}>#{verurl}
<p>Thank you
|]
            , partHeaders = []
            }
    getVerifyKey = runDB . fmap (join . fmap userVerkey) . get
    setVerifyKey uid key = runDB $ update uid [UserVerkey =. Just key]
    verifyAccount uid = runDB $ do
        mu <- get uid
        case mu of
            Nothing -> return Nothing
            Just _ -> do
                update uid [UserVerified =. True]
                return $ Just uid
    getPassword = runDB . fmap (join . fmap userPassword) . get
    setPassword uid pass = runDB $ update uid [UserPassword =. Just pass]
    getEmailCreds email = runDB $ do
        mu <- getBy $ UniqueUser email
        case mu of
            Nothing -> return Nothing
            Just (Entity uid u) -> return $ Just EmailCreds
                { emailCredsId = uid
                , emailCredsAuthId = Just uid
                , emailCredsStatus = isJust $ userPassword u
                , emailCredsVerkey = userVerkey u
                }
    getEmail = runDB . fmap (fmap userIdent) . get

-- This instance is required to use forms. You can modify renderMessage to
-- achieve customized and internationalized form validation messages.
instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

-- Note: previous versions of the scaffolding included a deliver function to
-- send emails. Unfortunately, there are too many different options for us to
-- give a reasonable default. Instead, the information is available on the
-- wiki:
--
-- https://github.com/yesodweb/yesod/wiki/Sending-email
