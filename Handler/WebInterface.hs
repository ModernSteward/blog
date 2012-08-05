{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.WebInterface where

import Import
import qualified Data.Text as T (pack)

getSendR :: Text -> Handler RepHtml
getSendR txt = do
    uid <- requireAuthId
    posted <- liftIO getCurrentTime
    let click = Click { clickText = txt
                      , clickUser = uid
                      , clickTime = posted
                      }
    _ <- runDB $ insert click -- TODO: check for error
    setMessage "Successfully sent a command."
    redirect HomeR
   
getListR :: Handler RepJson
getListR = do
    uid <- requireAuthId
    result <- runDB $ do
        selectList [ ClickUser ==. uid] [ Asc ClickTime ]
    jsonToRepJson $ array [clickToJson click | click <- result]
  where clickToJson (Entity _ click) =
            object [ ("text", clickText click)
                   , ("time", T.pack $ show $ clickTime click)
                   ]

getAccumulateR :: Handler RepHtml
getAccumulateR = do
    uid <- requireAuthId
    _ <- runDB $ deleteWhere [ ClickUser ==. uid ]
    redirect HomeR
