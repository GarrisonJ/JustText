{-# LANGUAGE OverloadedStrings #-}
module Handler.Home where

import Import

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
    messages <- runDB $ selectList [] [Desc MessageTimestamp]
    mauth <- maybeAuth
    let submission = Nothing :: Maybe Message
    case mauth of
      Just (Entity uid _) -> do
        (formWidget, formEnctype) <- generateFormPost $ messageForm uid
        defaultLayout $ do
            aDomId <- newIdent
            setTitle "Just text"
            $(widgetFile "homepage")
      Nothing -> defaultLayout $ do
            aDomId <- newIdent
            setTitle "Just text"
            $(widgetFile "loginpage")

postHomeR ::  Handler ()
postHomeR = do
    user <- requireAuthId
    ((message, _), _) <- runFormPost $ messageForm user
    case message of 
        FormSuccess a -> do 
                    _ <- runDB $ insert a
                    setMessage "Message posted :)"
        FormFailure t -> setMessage $ toHtml $ show t ++ " D:"
        _             -> setMessage "Error"
    redirect HomeR
    
messageForm :: UserId -> Form Message
messageForm user = renderDivs $ Message
    <$> areq textareaField "" Nothing
    <*> pure user
    <*> lift (liftIO getCurrentTime)
