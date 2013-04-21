module Handler.Crash where

import Import
import Data.Aeson.Types (toJSON)

crashForm :: Form Crash
crashForm = renderBootstrap $ Crash
    <$> areq textField (FieldSettings "Error" Nothing Nothing (Just "error") []) Nothing
    <*> areq textField (FieldSettings "StackTrace" Nothing Nothing (Just "stacktrace") []) Nothing
    <*> aformM (liftIO getCurrentTime)

postCrashR :: Handler RepJson
postCrashR = do
    ((result, _), _) <- runFormPostNoToken crashForm
    case result of
        FormSuccess crash -> do
            _ <- runDB $ insert crash
            jsonToRepJson $ toJSON ("success" :: Text)
        _ -> do
            jsonToRepJson $ toJSON ("error" :: Text)
