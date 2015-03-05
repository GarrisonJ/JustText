{-# LANGUAGE OverloadedStrings #-}
module Handler.Home where

import Import
-- import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3,
--                               withSmallInput)

-- This is a handler function for the GET request method on the HomeR
-- resource pattern. All of your resource patterns are defined in
-- config/routes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
--
--
-- data FormResult a = FormMissing
--                   | FormFailure [Text]
--                   | FormSuccess a
--     deriving Show
-- instance Functor FormResult where
--     fmap _ FormMissing = FormMissing
--     fmap _ (FormFailure errs) = FormFailure errs
--     fmap f (FormSuccess a) = FormSuccess $ f a
-- instance Applicative FormResult where
--     pure = FormSuccess
--     (FormSuccess f) <*> (FormSuccess g) = FormSuccess $ f g
--     (FormFailure x) <*> (FormFailure y) = FormFailure $ x ++ y
--     (FormFailure x) <*> _ = FormFailure x
--     _ <*> (FormFailure y) = FormFailure y
--     _ <*> _ = FormMissing
-- instance Monoid m => Monoid (FormResult m) where
--     mempty = pure mempty
--     mappend x y = mappend <$> x <*> y
-- instance Semigroup m => Semigroup (FormResult m) where
--     x <> y = (<>) <$> x <*> y

getHomeR :: Handler Html
getHomeR = do
    -- mmsg <- getMessage
    messages <- runDB $ selectList [] [Desc MessageTimestamp]
    mauth <- maybeAuth
    (formWidget, formEnctype) <- generateFormPost messageForm
    let submission = Nothing :: Maybe Message
        handlerName = "getHomeR" :: Text
    defaultLayout $ do
        aDomId <- newIdent
        setTitle "Just text"
        $(widgetFile "homepage")

postHomeR ::  Handler ()
postHomeR = do
    ((message, _), _) <- runFormPost messageForm
    case message of 
        FormSuccess a -> do 
                    _ <- runDB $ insert a
                    setMessage "Message posted :)"
        FormFailure t -> setMessage $ toHtml $ show t ++ "D:"
        _             -> setMessage "Error"
    redirect HomeR
    
-- now <- liftIO getCurrentTime
-- _ <- runDB $ insert $ Message (Textarea mes) now
-- mes <- runInputPost $ ireq textField "message"
--     mmsg <- getMessage
--     -- (uid, _) <- requireAuth
--     ((FormResult a0, xml0), Enctype) <- runFormPost "message"
--     now <- liftIO getCurrentTime
--     runDB $ insert $ Message 3 message now
--     setMessage "Message posted :)"
--     redirect getHomeR

messageForm :: Form Message
messageForm = renderDivs $ Message
    <$> areq textareaField "" Nothing
    <*> lift (liftIO getCurrentTime)

-- Message
--     user UserId Eq In
--     content String
--     timestamp UTCTime Desc
