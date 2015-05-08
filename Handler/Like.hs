{-# LANGUAGE OverloadedStrings #-}
module Handler.Like where

import Import

postLikeR :: MessageId -> Handler ()
postLikeR messageId = do
    user <- requireAuthId
    like <- runDB $ selectFirst [LikeLover ==. user, LikeMessage ==. messageId] []
    message <- runDB $ selectFirst [MessageId ==. messageId] []
    case message of
      Nothing -> return ()
      _ -> case like of
              (Just _) -> deleteLikeR messageId
              _       -> do
                          _ <- runDB $ insert (Like user messageId)
                          return ()

deleteLikeR :: MessageId -> Handler ()
deleteLikeR messageId = do
    user <- requireAuthId
    _ <- runDB $ deleteWhere [LikeLover ==. user, LikeMessage ==. messageId]
    return ()
