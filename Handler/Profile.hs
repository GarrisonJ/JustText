module Handler.Profile where

import Import

getProfileR :: UserId -> Handler Html
getProfileR userId = do
    messages <- runDB $ selectList [MessageUser ==. userId] [Desc MessageTimestamp]
    profile <- runDB $ selectFirst [ProfileUser ==. userId] []
    mauth <- maybeAuth
    defaultLayout $ do
        aDomId <- newIdent
        setTitle "Just text"
        $(widgetFile "profile")

