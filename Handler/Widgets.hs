{-# LANGUAGE OverloadedStrings #-}
module Handler.Widgets where

import Import
import Yesod.Markdown
import Network.Gravatar
import Text.Pandoc
import Text.Pandoc.Shared

data TwitterMeta = TwitterMeta { creator :: Text
                               , title :: String
                               , description :: String
                               , image :: String
                               }

renderTwitterMetta :: Message -> Profile -> Widget
renderTwitterMetta m p = do
  let tm = getTwitterMetta m p
    in toWidget [whamlet|
                <meta name="twitter:card" content="summary_large_image">
                <meta name="twitter:site" content="ttxtt.io">
                <meta name="twitter:creator" content="#{creator tm}">
                <meta name="twitter:title" content="#{title tm}">
                <meta name="twitter:description" content="#{description tm}">
                <meta name="twitter:image" content="#{image tm}">
                |]

getTwitterMetta :: Message -> Profile -> TwitterMeta
getTwitterMetta m creator =
            TwitterMeta {
              creator     = profileUsername creator
            , title       = getTitle (messageContent m)
            , description = "ttxtt.io post"
            , image       = extractImages (parseMarkdown def (messageContent m))
            }

extractImage :: Inline -> [String]
extractImage (Image _ (u,_)) = [u]
extractImage _ = []

extractImages :: Pandoc -> String
extractImages m = case queryWith extractImage m of
                    (x:_) -> x
                    _ -> ""

getTitle :: Markdown -> String
getTitle m = case (parseMarkdown def m) of
              (Pandoc meta _) -> stringify $ docTitle meta

messageWhamlet :: Markdown -> Text -> UserId -> Text -> UTCTime -> MessageId -> Bool -> Int -> Widget
messageWhamlet message email userId username timestamp messageId userLiked numLikes =
    let gravatarSettings = def{gDefault=Just MM}
        in toWidget [whamlet|
            <ul .collection .z-depth-1>
              <li .collection-item>
                <div #message>
                  ^{renderMarkdown message}
              <li .collection-item .avatar>
                <a href=@{ProfileR userId}>
                  <img src=#{gravatar gravatarSettings email} alt="" class="circle">
                <span .title>
                  <a href=@{ProfileR userId} .grey-text .text-lighten-1>
                    #{username}
                <p>
                  <a href=@{MessageR messageId}> #{getTitle message} <br>
                    #{formatTime defaultTimeLocale "%D" timestamp}
                <span .secondary-content>
                  <ul>
                    ^{likeButtonHamlet messageId userLiked numLikes}
          |]

getUserLiked :: MessageId -> UserId -> Handler Bool
getUserLiked messageId userId = do
  like <- runDB $ selectFirst [LikeLover ==. userId, LikeMessage ==. messageId] []
  case like of
    Nothing -> return False
    Just _ -> return True

getNumberOfLikes :: MessageId -> Handler Int
getNumberOfLikes messageId = runDB $ count [LikeMessage ==. messageId]

renderMessageW' :: Text -> Text -> UserId -> Maybe Text -> Markdown -> UTCTime -> MessageId -> Int -> Int -> Widget
renderMessageW' email username userId mBio content timestamp messageId numLikes userLiked' = do
    let gravatarSettings = def{gDefault=Just MM}
        message          = content
        userLiked = userLiked' > 0
    messageWhamlet message
                   email
                   userId
                   username
                   timestamp
                   messageId
                   userLiked
                   numLikes

renderMessageW :: Entity Message -> Profile -> Bool -> Int -> Widget
renderMessageW em creator userLiked numLikes =
  let gravatarSettings = def{gDefault=Just MM}
      messageId        = entityKey em
      message          = entityVal em
  in do
    renderTwitterMetta message creator
    likeButtonJulius
    messageWhamlet (messageContent message)
                   (profileEmail creator)
                   (profileUser creator)
                   (profileUsername creator)
                   (messageTimestamp message)
                   messageId
                   userLiked
                   numLikes

renderMarkdown :: Markdown -> Widget
renderMarkdown m = do
              addStylesheet $ StaticR css_code_css
              addStylesheetRemote $ "https://cdn.jsdelivr.net/font-hack/2.010/css/hack-extended.min.css"
              addScriptRemote "http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML"
              toWidget $ writePandoc yesodDefaultWriterOptions{writerHTMLMathMethod=(MathJax "http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML")} pan
        where
          pan = parseMarkdown yesodDefaultReaderOptions m

navbar :: Maybe (Entity User) -> Widget
navbar mauth = toWidget [hamlet|
          <div class="row">
            <nav>
              <div .nav-wrapper .grey .lighten-3>
                <a href=@{HomeR} .brand-logo>&nbsp;&#9646;&#9646;&#9646;
                <ul .right>
                  $maybe (Entity id _) <- mauth
                    <li>
                      <a href=@{ProfileR id}>Profile
                    <li>
                      <a href=@{SettingsR}>Settings
                    <li>
                      <a href=@{AuthR LogoutR}>Logout
                  $nothing
                    <li>
                      <a href=@{LandingR}><i>Login</i>
         |]


likeButton :: MessageId -> Bool -> Int -> Widget
likeButton messageId userLiked numLikes = do
  likeButtonJulius
  likeButtonHamlet messageId userLiked numLikes

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
