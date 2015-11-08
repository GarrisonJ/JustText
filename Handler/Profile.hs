module Handler.Profile where

import           Import
import           Network.Gravatar
import           Handler.Widgets
import           Handler.Like
import           Handler.Navbar
import qualified Database.Esqueleto      as E
import           Database.Esqueleto      ((^.))
import           Yesod.Paginate

getUserProfileR :: Text -> Handler Html
getUserProfileR username = do
  user <- runDB $ selectFirst [ProfileUsername ==. username] []
  case user of
    Nothing -> do
              setMessage "That profile does not exist"
              redirect HomeR
    (Just (Entity _ profile)) -> do
              getProfilePageR (profileUser profile) 0

getProfileR :: UserId -> Handler Html
getProfileR userId = getProfilePageR userId 0

getProfilePageR :: UserId -> Int -> Handler Html
getProfilePageR userId page = do
    -- Get every message posted by user
    messages <- paginateWith (PageConfig 10 page HomeR PaginatesR)$
                \(message `E.LeftOuterJoin` likes) -> do
                  E.on (message ^. MessageId E.==. likes ^. LikeMessage)
                  E.groupBy ( message   ^. MessageContent
                            , message   ^. MessageTimestamp
                            , message   ^. MessageUser
                            , message   ^. MessageId
                            )
                  let likeCount = E.count (likes ^. LikeLover)
                  let userLiked = E.count (likes ^. LikeLover E.==. E.val userId)
                  E.where_ $ (message ^. MessageUser E.==. E.val userId)
                  E.orderBy [E.desc (message ^. MessageTimestamp)]
                  return ( message   ^. MessageContent
                         , message   ^. MessageTimestamp
                         , message   ^. MessageId
                         , likeCount
                         , userLiked
                         )
    -- Get profile info
    let gravatarSettings = def{gSize=Just (Size 200), gDefault=Just MM}
    profile <- runDB $ selectFirst [ProfileUser ==. userId] []
    -- Check if user is logged in
    mauth <- maybeAuth
    -- Follow status
    oldFollowStatus <- case mauth of
                          (Just (Entity uid _ )) -> runDB $ selectFirst [FollowFollower ==. uid, FollowFollowee ==. userId] []
                          Nothing -> return Nothing
    -- Create follow button
    (formWidget, formEnctype) <- case mauth of
                                (Just (Entity uid _ )) -> generateFormPost $ followForm uid userId
                                Nothing -> generateFormPost $ followForm userId userId -- invald user
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
