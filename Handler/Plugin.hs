{-# LANGUAGE DoAndIfThenElse #-}
module Handler.Plugin where

import Import
import Helpers.Plugin
import Helpers.Messages
import qualified Data.Text as T
import Data.Aeson.Types (toJSON)
import Data.List (find)
import System.Directory (removeFile)

data PluginForm = PluginForm { pfTitle :: Text
                             , pfDescription :: Textarea
                             , pfCommands :: Textarea
                             , pfFile :: FileInfo
                             }
instance Show PluginForm where
    show pf = show (pfTitle pf) ++ " " ++ show (pfDescription pf)
pluginForm :: Maybe PluginForm -> Form PluginForm
pluginForm plugin = renderBootstrap $ PluginForm
    <$> areq textField (setLabel MsgPluginName) (fmap pfTitle plugin)
    <*> areq textareaField (setLabel MsgPluginDescription) (fmap pfDescription plugin)
    <*> areq textareaField (setLabel MsgCommands) (fmap pfCommands plugin)
    <*> fileAFormReq (setLabel MsgPluginFile)

getAddPluginR :: Handler RepHtml
getAddPluginR = do
    (formWidget, formEnctype) <- generateFormPost $ pluginForm Nothing
    defaultLayout $ do
        setTitle "Add plugin"
        $(widgetFile "plugins/new-plugin")

postAddPluginR :: Handler RepHtml
postAddPluginR = do
    ((result, _), _) <- runFormPost $ pluginForm Nothing
    case result of
            FormSuccess res ->
              if (fileContentType $ pfFile res) == ("application/zip" :: T.Text) ||
                 (fileContentType $ pfFile res) == ("application/octet-stream" :: T.Text) ||
                 (fileContentType $ pfFile res) == ("application/x-zip-compressed" :: T.Text)
              then do
                      name <- liftIO $ saveFile $ pfFile res
                      key <- runDB $ do
                          insert $
                            Plugin (pfTitle res)
                                   (pfDescription res)
                                   (fileContentType $ pfFile res)
                                   (T.pack name)
                      _ <- runDB $ insertCommands key $ filter (not . T.null) $ T.lines $ T.filter ((/=) '\r') $ unTextarea $ pfCommands res
                      setMessage $ successMessage $ T.pack ("Successfully added a new plugin." ++ name)
                      redirect HomeR
              else do
                    setMessage $ failureMessage $ "Wrong file type:" `T.append` (fileContentType $ pfFile res)
                    redirect HomeR
                       -- $(widgetFile "plugins/new-plugin")

            FormFailure xs -> do
                setMessage $ failureMessage $ T.concat xs
                redirect AddPluginR
                
            FormMissing -> do
                setMessage $ failureMessage "Form Missing"
                redirect AddPluginR
  where insertCommands _ [] = return ()
        insertCommands k (x:xs) = do
            _ <- insert $ Command k x
            insertCommands k xs

getDeletePluginR :: PluginId -> Handler RepHtml
getDeletePluginR pid = do
    (msgType, msg, filePath) <- runDB $ do
        mentity <- get pid

        case mentity of
            Just plugin -> do
                deleteWhere [ InstallationPlugin ==. pid ]
                deleteWhere [ PermissionMaster ==. pid ]
                deleteWhere [ PermissionSlave ==. pid ]
                deleteWhere [ CommandPluginId ==. pid ]
                deleteWhere [ ClickPluginId ==. pid ]
                delete pid
                return (Success, "plugin deleted!", pluginFilePath plugin)
            Nothing -> return (Error, "plugin not found", "")
    case msgType of
        Success -> do
            liftIO $ removeFile $ T.unpack filePath
            setMessage $ successMessage msg
        _ -> setMessage $ failureMessage msg
    redirect AdminR
            
getDownloadPluginR :: PluginId -> Handler ChooseRep
getDownloadPluginR pid = do
    plugin <- runDB $ get404 pid
    setHeader "Content-Disposition" "attachment; filename=plugin.zip"
    sendFile "text/plain" (T.unpack $ pluginFilePath plugin)

getInfoPluginR :: PluginId -> Handler RepJson
getInfoPluginR pid = do
    plugin <- runDB $ get404 pid
    jsonToRepJson $ pluginToJson (Entity pid plugin)

getListPluginsR :: Handler RepJson
getListPluginsR = do
    plugins <- runDB $ selectList [] []
    jsonToRepJson $ array [pluginToJson plugin | plugin <- plugins]

getListInstalledR :: Handler RepJson
getListInstalledR = do
    uid <- requireAuthId
    i <- runDB $ selectList [ InstallationUser ==. uid ] []
    jsonToRepJson $ array [toJSON $ installationPlugin p | Entity _ p <- i]

getInstallPluginR :: PluginId -> Handler RepHtml
getInstallPluginR pid = do
    Entity uid _ <- requireAuth
    (msgType, msg) <- runDB $ do
        i <- getBy $ UniqueInstallation uid pid
        
        case i of
             Just _ -> do
                return (Alert, "Plugin already installed")
             Nothing -> do
                _ <- insert $ Installation uid pid
                return (Success, "Successfully installed the plugin.")
    case msgType of
        Success -> setMessage $ successMessage msg
        _ -> setMessage $ warningMessage msg
    redirect BrowsePluginsR

getUninstallPluginR :: PluginId -> Handler RepHtml
getUninstallPluginR pid = do
    Entity uid _ <- requireAuth
    (msgType, msg) <- runDB $ do
        mentity <- getBy $ UniqueInstallation uid pid

        case mentity of
            Just _ -> do
                deleteBy $ UniqueInstallation uid pid
                deleteWhere [ ClickPluginId ==. pid ]
                return (Success, "Plugin uninstalled.")
            Nothing -> return (Error, "plugin not found")
    case msgType of
        Success -> do
            setMessage $ successMessage msg
        _ -> setMessage $ failureMessage msg
    redirect BrowsePluginsR

getBrowsePluginsR :: Handler RepHtml
getBrowsePluginsR = do
    Entity uid user <- requireAuth
    (plugins', installed) <- runDB $ do
        plugins <- selectList [] [ Desc PluginId ]
        installed <- selectList [ InstallationUser ==. uid ] []
        return (plugins, installed)
    let installed' = map (\(Entity _ x) -> installationPlugin x) installed
        keys = map (\(Entity k _) -> k) plugins'
        plugins = zip4 [(1 :: Int)..]
                       (map (\(Entity _ p) -> p) plugins')
                       keys
                       (map (\k -> k `elem` installed') keys)
        isCurrentUserAdmin = userAdmin user
    defaultLayout $ do
        setTitle "Plugins"
        $(widgetFile "browse-plugins")

zip4 :: [a] -> [b] -> [c] -> [d] -> [(a, b, c, d)]
zip4 [] _ _ _ = []
zip4 _ [] _ _ = []
zip4 _ _ [] _ = []
zip4 _ _ _ [] = []
zip4 (a:as) (b:bs) (c:cs) (d:ds) = (a, b, c, d) : zip4 as bs cs ds

permissionForm :: [ (Text, PluginId) ] -> Form (PluginId, PluginId)
permissionForm plugins = renderBootstrap $ (,)
    <$> areq (selectField $ optionsPairs plugins) "Master" Nothing
    <*> areq (selectField $ optionsPairs plugins) "Slave" Nothing

getPermissionsR :: Handler RepHtml
getPermissionsR = do
    uid <- requireAuthId
    (plugins', installed') <- runDB $ do
        p <- selectList [] [ Asc PluginId ]
        i <- selectList [ InstallationUser ==. uid ] []
        return (p, i)

    let plugins = filter (\(Entity k _) -> k `elem` installed) plugins'
        installed = map (\(Entity _ i) -> installationPlugin i) installed'

    (formWidget, formEnctype) <- generateFormPost $ permissionForm $
        zip (map (\(Entity _ p) -> pluginTitle p) plugins)
            (map (\(Entity k _) -> k) plugins)

    permissions' <- runDB $ selectList [ PermissionUser ==. uid ] [ Asc PermissionId ]
    let permissions = zip4 (map (\(Entity _ p) -> value $ permissionMaster p) permissions')
                           (map (\(Entity _ p) -> value $ permissionSlave p) permissions')
                           [(1 :: Int)..]
                           (map entityKey permissions')
        value x = case find (\(Entity k _) -> x == k) plugins of
                       Just y -> y
                       Nothing -> undefined
    defaultLayout $ do
        $(widgetFile "permissions")

postPermissionsR :: Handler RepHtml
postPermissionsR = do
    user <- requireAuthId
    plugins <- runDB $ selectList [] [ Asc PluginId ]
    ((result, _), _) <- runFormPost $ permissionForm $
        zip (map (\(Entity _ p) -> pluginTitle p) plugins)
            (map (\(Entity k _) -> k) plugins)
    case result of
        FormSuccess (master, slave) -> do
            if master == slave then do
                setMessage $ failureMessage $ "Master and Slave should be different."
            else do
                (msgType, msg) <- runDB $ do
                    res <- insertBy $ Permission user master slave
                    case res of
                        Left _ -> return (Error, "This relation has been added.")
                        Right _ -> return (Success, "Added a new permission.")
                case msgType of
                     Success -> setMessage $ successMessage msg
                     _ -> setMessage $ failureMessage msg
            redirect PermissionsR
        _ -> do
            setMessage $ failureMessage "error"
            redirect PermissionsR

getRemovePermissionR :: PermissionId -> Handler RepHtml
getRemovePermissionR perId = do
    uid <- requireAuthId
    (msgType, msg) <- runDB $ do
        mentity <- get perId
        case mentity of
            Just entity ->
                if uid == permissionUser entity then do
                    delete perId
                    return (Success, "Plugin uninstalled.")
                else return (Error, "This permission does not belong to you.")
            Nothing -> return (Error, "Permission id not found")
    case msgType of
        Success -> do
            setMessage $ successMessage msg
        _ -> setMessage $ failureMessage msg
    redirect PermissionsR

getHasPermissionR :: PluginId -> PluginId -> Handler RepJson
getHasPermissionR master slave = do
    uid <- requireAuthId
    result <- runDB $ getBy $ UniquePermission uid master slave
    case result of
         Just _ -> jsonToRepJson $ toJSON True
         Nothing -> jsonToRepJson $ toJSON False

