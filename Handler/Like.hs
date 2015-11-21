{-# LANGUAGE OverloadedStrings #-}
module Handler.Like where

import Import
import qualified Data.List as L
import Text.Read

likesSessionKey :: Text
likesSessionKey = "Likes"

deleteAllUnauthLikes :: Handler ()
deleteAllUnauthLikes = deleteSession likesSessionKey

didLike :: Maybe a -> Bool -> Bool -> Bool
didLike mauth unAuthLiked authLiked =
            case mauth of
                   Nothing  -> unAuthLiked
                   (Just _) -> authLiked

getUnauthLikes :: Handler ([MessageId])
getUnauthLikes = do
          likes' <- lookupSession likesSessionKey
          case likes' of
            Nothing -> return []
            (Just likes) -> case readMaybe (unpack likes) of
                                Nothing -> deleteSession likesSessionKey >> return []
                                (Just l) -> return l

didUnauthLike :: MessageId -> Handler (Bool)
didUnauthLike messageId = do
        alreadyLike <- getUnauthLikes
        return $ messageId `elem` alreadyLike

postLikeR :: MessageId -> Handler ()
postLikeR messageId = do
    mUser <- maybeAuthId
    message <- runDB $ get404 messageId
    case mUser of
      Nothing     -> handleNonauthLike messageId
      (Just user) -> handleAuthUserLike user messageId
  where
    handleNonauthLike messageId = do
        alreadyLike' <- lookupSession likesSessionKey
        let alreadyLike = case alreadyLike' of
                                  Nothing -> [] :: [MessageId]
                                  (Just l) -> read $ unpack l :: [MessageId]
        case messageId `elem` alreadyLike of
          False -> do
            setSession likesSessionKey (pack $ show $ messageId:alreadyLike)
            runDB $ update messageId [MessageNonAuthLikeCount +=. 1]
          True -> do
            setSession likesSessionKey (pack $ show $ L.delete messageId alreadyLike)
            runDB $ update messageId [MessageNonAuthLikeCount -=. 1]

    handleAuthUserLike user messageId = do
        like <- runDB $ selectFirst [LikeLover ==. user, LikeMessage ==. messageId] []
        case like of
          (Just _) -> void $ runDB $ deleteWhere [LikeLover ==. user, LikeMessage ==. messageId]
          _        -> void $ runDB $ insert (Like user messageId)


likeButton :: MessageId -> Bool -> Int -> Widget
likeButton messageId userLiked numLikes = do
  likeButtonJulius
  likeButtonHamlet messageId userLiked numLikes

getUserLiked :: MessageId -> UserId -> Handler Bool
getUserLiked messageId userId = do
  like <- runDB $ selectFirst [LikeLover ==. userId, LikeMessage ==. messageId] []
  case like of
    Nothing -> return False
    Just _ -> return True

getNumberOfLikes :: MessageId -> Handler Int
getNumberOfLikes messageId = runDB $ count [LikeMessage ==. messageId]

likeButtonHamlet :: MessageId -> Bool -> Int -> Widget
likeButtonHamlet messageId userLiked numLikes =
    toWidget
        [hamlet|
          <li>
            $if userLiked
              <a href=# message-url=@{LikeR messageId} .waves-effect .waves-light .red-text .like>
                <span .likeCount>#{numLikes}
                <i .mdi-action-favorite>
            $else
              <a href=# message-url=@{LikeR messageId} .waves-effect .waves-light .teal-text .like>
                <span .likeCount>#{numLikes}
                <i .mdi-action-favorite>
        |]

likeButtonJulius :: Widget
likeButtonJulius = do
    toWidget
        [julius|
                $(function(){
                    function likeMessage(link) {
                        $.ajax({
                            type: "POST",
                            url: link.attr("message-url"),
                        }).done(function(msg) {
                            if (link.hasClass("red-text")) {
                              link.removeClass("red-text");
                              link.addClass("teal-text");
                              link.children(".likeCount").html(parseInt(link.children(".likeCount").html(), 10)-1);
                            } else {
                              link.removeClass("teal-text");
                              link.addClass("red-text");
                              link.children(".likeCount").html(parseInt(link.children(".likeCount").html(), 10)+1);
                            }
                        });
                    }
                    $("a.like").click(function() {
                        likeMessage($(this));
                        return false;
                    });
                });
        |]
