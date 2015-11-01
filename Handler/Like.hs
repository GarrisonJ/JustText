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
