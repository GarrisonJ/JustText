{-# LANGUAGE OverloadedStrings #-}
module Handler.Widgets where

import Import
import Yesod.Markdown
import Network.Gravatar
import Text.Pandoc
import Text.Pandoc.Shared
import Handler.Like

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

messageWhamlet :: Markdown -> Text -> UserId -> Text -> UTCTime -> MessageId -> Bool -> Int -> Maybe (Entity User) -> Widget
messageWhamlet message email userId username timestamp messageId userLiked numLikes mauth =
    let gravatarSettings = def{gDefault=Just MM}
        in toWidget [whamlet|
          <div .messageBox>
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
                    <li>
                    <li>
                    <li>
                      $maybe (Entity uid _) <- mauth
                        $if uid == userId
                          <a href=# message-url=@{MessageR messageId} .grey-text .text-lighten-1 .delete style="display:none;">
                            <i class="mdi-action-delete">
          |]

renderMessageW' :: Text -> Text -> UserId -> Maybe Text -> Markdown -> UTCTime -> MessageId -> Int -> Int -> Maybe (Entity User) -> Widget
renderMessageW' email username userId mBio content timestamp messageId numLikes userLiked mauth = do
    let gravatarSettings = def{gDefault=Just MM}
        message          = content
        userLiked' = userLiked > 0
    messageWhamlet message
                   email
                   userId
                   username
                   timestamp
                   messageId
                   userLiked'
                   numLikes
                   mauth

renderMessageW :: Entity Message -> Profile -> Bool -> Int -> Maybe (Entity User) -> Widget
renderMessageW em creator userLiked numLikes mauth =
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
                   mauth

renderMarkdown :: Markdown -> Widget
renderMarkdown m = do
              addStylesheet $ StaticR css_code_css
              addStylesheetRemote $ "https://cdn.jsdelivr.net/font-hack/2.010/css/hack-extended.min.css"
              addScriptRemote "http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML"
              toWidget $ writePandoc yesodDefaultWriterOptions{writerHTMLMathMethod=(MathJax "http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML")} pan
        where
          pan = parseMarkdown yesodDefaultReaderOptions m
