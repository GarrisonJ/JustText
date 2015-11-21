module Handler.Message where

import Import
import Handler.Widgets
import Handler.Like

getMessageR :: MessageId -> Handler Html
getMessageR messageId = do
    mauth     <- maybeAuth
    message   <- runDB $ get404 messageId
    numLikes  <- runDB $ count [LikeMessage ==. messageId]
    userLiked <- case mauth of
                   Nothing  -> didUnauthLike messageId
                   (Just (Entity m _)) -> do
                              l <- runDB $ selectFirst [LikeMessage ==. messageId, LikeLover ==. m] []
                              case l of
                                Nothing -> return False
                                _       -> return True
    creator <- runDB $ selectFirst [ProfileUser ==. messageUser message] []
    defaultLayout $ do
        setTitle "TTxTT"
        $(widgetFile "message")


deleteMessageR :: MessageId -> Handler Html
deleteMessageR messageId = do
        userId <- requireAuthId
        runDB $ deleteWhere [LikeMessage ==. messageId]
        runDB $ deleteWhere [MessageId ==. messageId, MessageUser ==. userId]
        redirect HomeR
