{-# LANGUAGE OverloadedStrings #-}
module Handler.Paginates where

import           Import
import qualified Database.Esqueleto as E
import           Database.Esqueleto ((^.))
import           Yesod.Markdown
import           Yesod.Paginate
import           Handler.Widgets

getPaginatesR :: Int -> Handler Html
getPaginatesR page = do
    uid <- requireAuthId
    mauth <- maybeAuth
    followSelf <- runDB $ selectFirst [FollowFollower ==. uid, FollowFollowee ==. uid] []
    case followSelf of
      Nothing -> runDB $ insert_ $ Follow uid uid
      (Just _) -> return ()
    messages <- paginateWith (PageConfig 10 page HomeR PaginatesR)$
                \((message `E.CrossJoin` profile) `E.CrossJoin` follow) -> do
                  E.where_ $ (message ^. MessageUser E.==. profile ^. ProfileUser) E.&&.
                         (follow  ^. FollowFollowee E.==. profile ^. ProfileUser) E.&&.
                         (follow  ^. FollowFollower E.==. E.val uid)
                  E.orderBy [E.desc (message ^. MessageTimestamp)]
                  return
                      ( message   ^. MessageContent
                      , message   ^. MessageTimestamp
                      , message   ^. MessageId
                      , profile   ^. ProfileUsername
                      , profile   ^. ProfileUser
                      )
    (formWidget, formEnctype) <- generateFormPost $ messageForm uid
    -- Get profile info
    uProfile <- runDB $ selectFirst [ProfileUser ==. uid] []
    -- If user has profile render homepage, else render settings page
    case uProfile of
        Nothing -> do setMessage "You need a username before you can post messages."
                      redirect SettingsR
        Just _ -> defaultLayout $ do
                              aDomId <- newIdent
                              addScript $ StaticR vue_min_js
                              addScript $ StaticR node_modules_marked_marked_min_js
                              addScript $ StaticR node_modules_highlightjs_highlight_pack_min_js
                              addStylesheet $ StaticR node_modules_highlightjs_styles_arta_css
                              setTitle "Just text"
                              $(widgetFile "homepage")

messageForm :: UserId -> Form Message
messageForm user = renderDivs $ Message
    <$> areq markdownField markSettings Nothing
    <*> pure user
    <*> lift (liftIO getCurrentTime)
  where
    markSettings = "" {fsAttrs = [("v-model", "input"), ("debounce", "300")], fsId = Just "textInput"}
