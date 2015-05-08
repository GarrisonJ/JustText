{-# LANGUAGE OverloadedStrings #-}
module Handler.Home where

import Import
import Yesod.Markdown

getHomeR :: Handler Html
getHomeR = redirect (PaginatesR 0)

postHomeR :: Handler Html
postHomeR = do
    user <- requireAuthId
    ((message, _), _) <- runFormPost $ messageForm user
    case message of
        FormSuccess a -> do _ <- runDB $ insert a
                            return ()
        FormFailure t -> setMessage $ toHtml $ show t ++ " D:"
        _             -> setMessage "Error"
    redirect (PaginatesR 0)

messageForm :: UserId -> Form Message
messageForm user = renderDivs $ Message
    <$> areq markdownField "" Nothing
    <*> pure user
    <*> lift (liftIO getCurrentTime)
