module Handler.Profile where

import Import

getProfileR :: UserId -> Handler Html
getProfileR userId = do
    -- Get every message posted by user
    messages <- runDB $ selectList [MessageUser ==. userId] [Desc MessageTimestamp]
    -- Get profile info
    profile <- runDB $ selectFirst [ProfileUser ==. userId] []
    -- Check if user is logged in
    mauth <- maybeAuth
    -- Render layout
    defaultLayout $ do
        aDomId <- newIdent
        setTitle "Just text"
        $(widgetFile "profile")

