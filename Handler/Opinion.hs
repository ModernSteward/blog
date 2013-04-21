module Handler.Opinion where

import Import

getOpinionR :: Handler RepHtml
getOpinionR = do
    defaultLayout $ do
        setTitle "Opinions - ModernSteward"
        $(widgetFile "opinions")
