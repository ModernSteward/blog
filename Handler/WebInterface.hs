{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.WebInterface where

import Import
import qualified Data.Text as T (pack, unpack)
import Data.Int
import Helpers.Messages

getSendR :: PluginId -> Text -> Handler RepHtml
getSendR pid txt = do
    uid <- requireAuthId
    posted <- liftIO getCurrentTime
    let click = Click { clickText = txt
                      , clickUser = uid
                      , clickTime = posted
                      , clickPluginId = pid
                      }
    _ <- runDB $ insert click -- TODO: check for error
    setMessage "Successfully sent a command."
    redirect CommandsR
   
getListR :: Handler RepJson
getListR = do
    uid <- requireAuthId
    result <- runDB $ do
        selectList [ ClickUser ==. uid] [ Asc ClickTime ]
    jsonToRepJson $ array [clickToJson click | click <- result]
  where clickToJson (Entity _ click) =
            object [ ("text", clickText click)
                   , ("time", T.pack $ show $ clickTime click)
--                   , ("pluginid", T.pack $ show $ unKey $ clickPluginId click)
                   , ("pluginid", case fromPersistValue $ unKey $ clickPluginId click of
                                       Left err -> error $ T.unpack err
                                       Right x -> T.pack $ show (x :: Int64))
                   ]

getAccumulateR :: Handler RepHtml
getAccumulateR = do
    uid <- requireAuthId
    _ <- runDB $ deleteWhere [ ClickUser ==. uid ]
    redirect HomeR

getCommandsR :: Handler RepHtml
getCommandsR = do
    uid <- requireAuthId
    installs <- runDB $ selectList [ InstallationUser ==. uid ] []
    commands' <- listCommands $ map (\(Entity _ v) -> installationPlugin v) installs
    let commands = map (\(x, (y, z, k)) -> (x, y, z, k)) $ zip [(1 :: Int)..] commands'
    defaultLayout $ do
        setTitle "Commands"
        $(widgetFile "commands")
  where listCommands [] = return []
        listCommands (pid:pids) = do
            commands <- runDB $ selectList [ CommandPluginId ==. pid] []
            mplugin <- runDB $ get pid
            
            let plugin = case mplugin of
                              Just p -> p
                              Nothing -> undefined

            res <- listCommands pids
            return $ (pid, plugin, map (\(Entity _ v) -> commandText v) commands) : res
