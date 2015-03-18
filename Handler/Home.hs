{-# LANGUAGE OverloadedStrings #-}
module Handler.Home where

import Import
import qualified Database.Esqueleto      as E
import           Database.Esqueleto      ((^.))

-- This is a handler function for the GET request method on the HomeR
-- resource pattern. All of your resource patterns are defined in
-- config/routes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
--

getHomeR :: Handler Html
getHomeR = do
    -- Get userid if user is authenticated, else Nothing
    mauth <- maybeAuth
    -- If user is authenticated render homepage, otherwise render login page
    case mauth of
      Nothing -> defaultLayout $ do
            aDomId <- newIdent
            setTitle "Just text"
            $(widgetFile "loginpage")
      Just (Entity uid _) -> do
        -- Get all messages joined with profiles
        messages <- runDB
            $ E.select
            $ E.from $ \(message `E.InnerJoin` profile) -> do
                    E.on $ message ^. MessageUser E.==. profile ^. ProfileUser
                    E.orderBy [E.desc (message ^. MessageTimestamp)]
                    return
                        ( message   ^. MessageContent
                        , message   ^. MessageTimestamp
                        , profile   ^. ProfileUsername
                        , profile   ^. ProfileUser
                        )
        -- Create message form
        (formWidget, formEnctype) <- generateFormPost $ messageForm uid
        -- Get profile info
        uProfile <- runDB $ selectFirst [ProfileUser ==. uid] []
        -- If user has profile render homepage, else render settings page
        case uProfile of
            Nothing -> do setMessage "You need a username before you can post messages."
                          redirect SettingsR 
            Just (Entity _ p) -> defaultLayout $ do
                                  aDomId <- newIdent
                                  setTitle "Just text"
                                  $(widgetFile "homepage")
postHomeR :: Handler Html
postHomeR = do
    -- User must be authenticated 
    user <- requireAuthId
    -- Get message from form
    ((message, _), _) <- runFormPost $ messageForm user
    -- Insert message into database, or display error to user
    case message of 
        FormSuccess a -> do _ <- runDB $ insert a
                            return ()
        FormFailure t -> setMessage $ toHtml $ show t ++ " D:"
        _             -> setMessage "Error"
    -- Redirect to homepage
    redirect HomeR
 
messageForm :: UserId -> Form Message
messageForm user = renderDivs $ Message
    <$> areq textareaField "" Nothing
    <*> pure user
    <*> lift (liftIO getCurrentTime)
