module Handler.Profile where

import           Import
import           Network.Gravatar
import           Handler.Widgets
import           Handler.Like
import           Handler.Navbar
import qualified Database.Esqueleto      as E
import           Database.Esqueleto      ((^.))
import           Yesod.Paginate
import           Yesod.Markdown

getUserProfileR :: Text -> Handler Html
getUserProfileR username = do
  user <- runDB $ selectFirst [ProfileUsername ==. username] []
  case user of
    Nothing -> do
              setMessage "That profile does not exist"
              redirect HomeR
    (Just (Entity _ profile)) -> getProfilePageR (profileUser profile) 0

getProfileR :: UserId -> Handler Html
getProfileR userId = getProfilePageR userId 0


getProfilePageR :: UserId -> Int -> Handler Html
getProfilePageR userProfileId page = do
    mauth <- maybeAuth
    (currentPage, messages) <- case mauth of
                  Nothing                  -> getMessagesForUnauthUser userProfileId        page
                  (Just (Entity userId _)) -> getMessagesForAuthUser   userProfileId userId page

    -- Get profile info
    let gravatarSettings = def{gSize=Just (Size 200), gDefault=Just MM}
    profile <- runDB $ selectFirst [ProfileUser ==. userProfileId] []

    -- Follow status
    oldFollowStatus <- case mauth of
                        (Just (Entity uid _ )) -> runDB $ selectFirst [FollowFollower ==. uid, FollowFollowee ==. userProfileId] []
                        Nothing -> return Nothing
    -- Create follow button
    (formWidget, formEnctype) <- case mauth of
                                (Just (Entity uid _ )) -> generateFormPost $ followForm uid userProfileId
                                Nothing -> generateFormPost $ followForm userProfileId userProfileId -- invald user
    -- Render layout
    defaultLayout $ do
        setTitle "TTxTT"
        likeButtonJulius
        $(widgetFile "profile")

postProfileR :: UserId -> Handler Html
postProfileR userId = do
    user <- requireAuthId
    oldFollowStatus <- runDB $ selectFirst [FollowFollower ==. user, FollowFollowee ==. userId] []
    case oldFollowStatus of
      (Just _) -> do
        runDB $ deleteWhere [FollowFollower ==. user, FollowFollowee ==. userId]
        redirect (ProfileR userId)
      Nothing -> do
        ((follow, _), _) <- runFormPost $ followForm user userId
        case follow of
            FormSuccess a -> do _ <- runDB $ insert a
                                return ()
            FormFailure t -> setMessage $ toHtml $ unwords t
            _             -> setMessage "Error"
        redirect (ProfileR userId)

followForm :: UserId -> UserId -> Form Follow
followForm follower followee = renderDivs $ Follow
    <$> pure follower
    <*> pure followee

getMessagesForAuthUser :: (YesodPersist site, YesodPersistBackend site ~ SqlBackend) =>
                          Key User
                          -> Key User
                          -> Int
                          -> HandlerT
                               site
                               IO
                               (Page
                                  (Route App)
                                  (E.Value Markdown,
                                   E.Value UTCTime,
                                   E.Value (Key Message),
                                   E.Value Int,
                                   E.Value Int),
                                 [(Yesod.Markdown.Markdown,
                                   UTCTime,
                                   Key Message,
                                   Int,
                                   Bool)])
getMessagesForAuthUser userProfileId userId page = do
    currentPage <- paginateWith (PageConfig 10 page HomeR (ProfilePageR userProfileId))$
                \(message `E.LeftOuterJoin` likes) -> do
                  E.on $ message ^. MessageId E.==. likes ^. LikeMessage
                  E.groupBy ( message   ^. MessageContent
                            , message   ^. MessageTimestamp
                            , message   ^. MessageUser
                            , message   ^. MessageId
                            )
                  let likeCount = E.count (likes ^. LikeLover)
                  E.where_ (message ^. MessageUser E.==. E.val userProfileId)
                  E.orderBy [E.desc (message ^. MessageTimestamp)]
                  return ( message   ^. MessageContent
                         , message   ^. MessageTimestamp
                         , message   ^. MessageId
                         , message   ^. MessageNonAuthLikeCount
                         , likeCount :: E.SqlExpr (E.Value Int)
                         )


    messages <- mapM (\(E.Value content
                         , E.Value timestamp
                         , E.Value msgId
                         , E.Value nonAuthLikeCount
                         , E.Value likeCount) -> do
                              userLiked <- runDB $ selectFirst [LikeMessage ==. msgId, LikeLover ==. userId] []
                              return (content, timestamp, msgId, nonAuthLikeCount + likeCount, maybeToBool userLiked))
                        (pageResults currentPage)

    return (currentPage, messages)
  where
    maybeToBool :: Maybe a -> Bool
    maybeToBool (Just _ ) = True
    maybeToBool Nothing   = False

getMessagesForUnauthUser :: Key User -> Int
         -> HandlerT
            App
            IO
            (Page
               (Route App)
               (E.Value Yesod.Markdown.Markdown,
                E.Value UTCTime,
                E.Value (Key Message),
                E.Value Int,
                E.Value Int),
             [(Yesod.Markdown.Markdown,
               UTCTime,
               Key Message,
               Int,
               Bool)])
getMessagesForUnauthUser userProfileId page = do
    currentPage <- paginateWith (PageConfig 10 page HomeR (ProfilePageR userProfileId))$
                \(message `E.LeftOuterJoin` likes) -> do
                  E.on $ message ^. MessageId E.==. likes ^. LikeMessage
                  E.groupBy ( message   ^. MessageContent
                            , message   ^. MessageTimestamp
                            , message   ^. MessageUser
                            , message   ^. MessageId
                            )
                  let likeCount = E.count (likes ^. LikeLover)
                  E.where_ (message ^. MessageUser E.==. E.val userProfileId)
                  E.orderBy [E.desc (message ^. MessageTimestamp)]
                  return ( message   ^. MessageContent
                         , message   ^. MessageTimestamp
                         , message   ^. MessageId
                         , message   ^. MessageNonAuthLikeCount
                         , likeCount :: E.SqlExpr (E.Value Int)
                         )

    likes <- getUnauthLikes

    let messages = map (\(E.Value content, E.Value timestamp, E.Value msgId, E.Value nonAuthLikeCount, E.Value likeCount) ->
                                (content, timestamp, msgId, nonAuthLikeCount + likeCount, msgId `elem` likes))
                        (pageResults currentPage)

    return (currentPage, messages)

