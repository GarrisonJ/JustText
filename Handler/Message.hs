module Handler.Message where

import Import
import Handler.Widgets

getMessageR :: MessageId -> Handler Html
getMessageR messageId = do
    mauth     <- maybeAuth
    message   <- runDB $ selectFirst [MessageId ==. messageId] []
    numLikes  <- runDB $ count [LikeMessage ==. messageId]
    userLiked <- case mauth of
                   Nothing  -> return False
                   (Just (Entity m _)) -> do
                              l <- runDB $ selectFirst [LikeMessage ==. messageId, LikeLover ==. m] []
                              case l of
                                Nothing -> return False
                                _       -> return True
    creator <- case message of
                Nothing             -> return Nothing
                (Just (Entity _ m)) -> runDB $ selectFirst [ProfileUser ==. messageUser m] []
    defaultLayout $ do
        setTitle "TTxTT"
        $(widgetFile "message")

deleteMessageR :: MessageId -> Handler Html
deleteMessageR messageId = do
        userId <- requireAuthId
        runDB $ deleteWhere [LikeMessage ==. messageId]
        runDB $ deleteWhere [MessageId ==. messageId, MessageUser ==. userId]
        redirect HomeR
