{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Admin where

import Import

getAdminR :: Handler RepHtml
getAdminR = do
    (plugins, posts, users) <- runDB $ do
        plugins <- count [PluginTitle !=. ""]
        posts <- count [EntrySlug !=. ""]
        users <- count [UserIdent !=. ""]
        return (plugins, posts, users)

    defaultLayout $ do
        setTitle "Admin page"
        $(widgetFile "admin/homepage")

