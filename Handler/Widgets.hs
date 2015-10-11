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


renderMessageW :: Message -> MessageId -> Maybe a -> Profile -> Bool -> Int -> Widget
renderMessageW message messageId mauth creator userLiked numLikes =
  let gravatarSettings = def{gDefault=Just MM}
  in do
    renderTwitterMetta message creator
    toWidget [whamlet|
      <ul .collection .z-depth-1>
        <li .collection-item>
          <div #message>
            ^{renderMarkdown (messageContent message)}
        <li .collection-item .avatar>
          <img src=#{gravatar gravatarSettings (profileEmail creator)} alt="" class="circle">
          <span .title>
            <a href=@{ProfileR (profileUser creator)} .grey-text .text-lighten-1>
              #{profileUsername creator}
          <p> #{getTitle (messageContent message)} <br>
              #{formatTime defaultTimeLocale "%D" (messageTimestamp message)}
          <span .secondary-content>
            <ul>
              ^{likeButton messageId userLiked numLikes}
    |]

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
                              $(".likeCount").html(parseInt($('.likeCount').html(), 10)-1);
                            } else {
                              link.removeClass("teal-text");
                              link.addClass("red-text");
                              $(".likeCount").html(parseInt($('.likeCount').html(), 10)+1);
                            }
                        });
                    }
                    $("a.like").click(function() {
                        likeMessage($(this));
                        return false;
                    });
                });
        |]
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
