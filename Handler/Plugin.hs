module Handler.Plugin where

import Import
import Helpers.Plugin
import Helpers.Messages
import qualified Data.Text as T

data PluginForm = PluginForm { pfTitle :: Text
                             , pfDescription :: Textarea
                             , pfFile :: FileInfo
                             }
instance Show PluginForm where
    show pf = show (pfTitle pf) ++ " " ++ show (pfDescription pf)
pluginForm :: Maybe PluginForm -> Form PluginForm
pluginForm plugin = renderBootstrap $ PluginForm
    <$> areq textField (setLabel MsgPluginName) (fmap pfTitle plugin)
    <*> areq textareaField (setLabel MsgPluginDescription) (fmap pfDescription plugin)
    <*> fileAFormReq (setLabel MsgPluginFile)

getAddPluginR :: Handler RepHtml
getAddPluginR = do
    (formWidget, formEnctype) <- generateFormPost $ pluginForm Nothing
    defaultLayout $ do
        setTitle "Add plugin"
        $(widgetFile "plugins/new-plugin")

postAddPluginR :: Handler RepHtml
postAddPluginR = do
    ((result, formWidget), formEnctype) <- runFormPost $ pluginForm Nothing
    case result of
            FormSuccess res ->
              if (fileContentType $ pfFile res) == ("application/zip" :: T.Text)
              then do
                      name <- liftIO $ saveFile $ pfFile res
                      _ <- runDB $ insert $
                          Plugin (pfTitle res)
                                 (pfDescription res)
                                 (fileContentType $ pfFile res)
                                 (T.pack name)
                      setMessage $ successMessage $ T.pack ("Successfully added a new plugin." ++ name)
                      redirect HomeR
              else defaultLayout $ do
                           $(widgetFile "plugins/new-plugin")

            _ -> defaultLayout $ do
                $(widgetFile "plugins/new-plugin")
            
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
