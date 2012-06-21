{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Blog
    ( getEntryR
    , getEditEntryR
    , getBlogPageR
    , getBlogR
    , getNewEntryR
    , postNewEntryR
    , getDeleteEntryR
    ) where

import Helpers.Messages
import Helpers.Posts
import Import
import Yesod.Markdown

getEntryR :: Text -> Handler RepHtml
getEntryR slug = do
    entry <- runDB $ getBySlug404 slug
    defaultLayout $ do
        setTitleI $ entryTitle entry
        $(widgetFile "blog-entry")

resultsForPage :: (PersistEntity Entry, PersistQuery b m, PersistEntityBackend Entry ~ b) => Int -> b m [Entity Entry]
resultsForPage pageNumber = do
    let resultsPerPage = 30
    selectList [] [ Desc EntryPosted
                  , LimitTo resultsPerPage
                  , OffsetBy $ (pageNumber - 1) * resultsPerPage
                  ]

getBlogPageR :: Int -> Handler RepHtml
getBlogPageR page = do
    entries <- runDB $ resultsForPage page
    now <- liftIO $ getCurrentTime
    muser <- maybeAuth
    let isCurrentUserAdmin :: Bool
        isCurrentUserAdmin =
            case muser of
                 Just (Entity _ user) -> if userAdmin user then True else False
                 _ -> False
    defaultLayout $ do
        setTitleI MsgBlog
        $(widgetFile "blog-entries")

getBlogR :: Handler RepHtml
getBlogR = getBlogPageR 1

entryHandler :: Maybe Entry -> Handler RepHtml
entryHandler entry = do
    (formWidget, formEnctype) <- generateFormPost $ newEntryForm entry
    defaultLayout $ do
        setTitle "New post"
        $(widgetFile "admin/new-post")

getNewEntryR :: Handler RepHtml
getNewEntryR = entryHandler Nothing

postNewEntryR :: Handler RepHtml
postNewEntryR = do
    ((result, formWidget), formEnctype) <- runFormPost $ newEntryForm Nothing
    case result of
        FormSuccess entry -> do
            (msg, redirection) <- runDB $ do

                res <- insertBy entry

                case res of
                    Right _ ->
                        return ( successMessage "Successfully added a new entry."
                               , EntryR $ entrySlug entry)
                    Left (Entity k _) -> do
                        update k
                            [ EntrySlug =. entrySlug entry
                            , EntryTitle =. entryTitle entry
                            , EntryPosted =. entryPosted entry
                            , EntryDescr =. entryDescr entry
                            , EntryPost =. entryPost entry
                            ]
                        return ( successMessage "Successfully updated the entry."
                               , EntryR $ entrySlug entry)
            setMessage msg
            redirect redirection
{-
            case iResult of
                 Nothing -> defaultLayout $ do
                              setTitle "New post"
                              $(widgetFile "admin/new-post")
                 Just _ -> do
                      setMessage $ successMessage "Successfully added a new entry."
                      redirect $ EntryR $ entrySlug entry
 -}
        _ -> defaultLayout $ do
                setTitle "New post"
                $(widgetFile "admin/new-post")
        
newEntryForm :: Maybe Entry -> Form Entry
newEntryForm entry = renderBootstrap $ Entry
                        <$> areq textField (setLabel MsgBlogSlug) (fmap entrySlug entry)
                        <*> areq textField (setLabel MsgBlogTitle) (fmap entryTitle entry)
                        <*> aformM (liftIO getCurrentTime)
                        <*> aopt markdownField (setLabel MsgBlogDescription) (fmap entryDescr entry)
                        <*> areq markdownField (setLabel MsgBlogPost) (fmap entryPost entry)

getEditEntryR :: Text -> Handler RepHtml
getEditEntryR slug = do
    (Entity _ entity) <- runDB $ getBy404 $ UniquePost slug
    entryHandler $ Just entity

getDeleteEntryR :: Text -> Handler RepHtml
getDeleteEntryR slug = do
    (msgType, msg) <- runDB $ do
        mentity <- getBy $ UniquePost slug

        case mentity of
            Just (Entity key _) -> do
                delete key
                return (Success, "post deleted!")
            Nothing -> return (Error, "post not found")
    case msgType of
        Success -> setMessage $ successMessage msg
        _ -> setMessage "failure"
    redirect AdminR
