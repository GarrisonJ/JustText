{-# LANGUAGE OverloadedStrings #-}
module Handler.Widgets where

import Import
import Yesod.Markdown
import Network.Gravatar
import Text.Pandoc.Options
import Text.Pandoc.Definition
import Text.Pandoc.Shared

renderMessageW :: Message -> MessageId -> Maybe a -> Profile -> Bool -> Int -> Widget
renderMessageW message messageId mauth creator userLiked numLikes =
  let gravatarSettings = def{gDefault=Just MM}
  in toWidget [whamlet|
    <ul .collection .z-depth-1>
      <li class="collection-item">
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
            <!--
            $maybe _ <- mauth
              <li>
                <a href=# message-url=@{MessageR messageId} .delete .secondary-content .right-align>
                  <i class="mdi-action-delete">
            $nothing
            -->
  |]

-- TODO: Currenty we parse markdown twice, do it once
renderMarkdown :: Markdown -> Widget
renderMarkdown m = do
              addStylesheet $ StaticR css_code_css
              addScriptRemote "http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML"
              toWidget $ writePandoc yesodDefaultWriterOptions{writerHTMLMathMethod=(MathJax "http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML")} pan
        where
          pan = parseMarkdown yesodDefaultReaderOptions m

getTitle :: Markdown -> String
getTitle m = case (parseMarkdown def m) of
              (Pandoc meta _) -> stringify $ docTitle meta

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
