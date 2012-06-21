module Handler.AdminUsers where

import Import
import Helpers.Messages

listUsers :: (PersistEntity User, PersistQuery b m, PersistEntityBackend Entry ~ b) => b m [Entity User]
listUsers = selectList [] [Desc UserIdent]

getAdminListUsersR :: Handler RepHtml
getAdminListUsersR = do
    users <- runDB listUsers
    defaultLayout $ do
        setTitle "Users page"
        $(widgetFile "admin/users")

getMakeUserAdminR :: UserId -> Handler RepHtml
getMakeUserAdminR uid = do
    _ <- runDB $ update uid [UserAdmin =. True]
    setMessage $ successMessage "Gave the user admin access"
    redirect AdminListUsersR

getMakeUserNormalR :: UserId -> Handler RepHtml
getMakeUserNormalR uid = do
    _ <- runDB $ update uid [UserAdmin =. False]
    setMessage $ successMessage "Took the user's admin access"
    redirect AdminListUsersR
