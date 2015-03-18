module Handler.Settings where

import Import

getSettingsR :: Handler Html
getSettingsR = do
    -- User must be logged in
    userId <- requireAuthId
    -- Get user info for header
    mauth <- maybeAuth
    -- Get profile info
    profile <- runDB $ selectFirst [ProfileUser ==. userId] []
    -- Get profile for defaults in form 
    let mprofile = case profile of 
            Nothing -> Nothing 
            Just (Entity _ p) -> Just p 
    -- Create settings form
    (formWidget, formEnctype) <- generateFormPost $ settingsForm userId mprofile
    -- Render layout
    defaultLayout $ do
        aDomId <- newIdent
        setTitle "Just text"
        $(widgetFile "settings")

postSettingsR :: Handler ()
postSettingsR = do
    -- User must be logged in
    userId <- requireAuthId
    -- Get data from form
    ((pdeta, _), _) <- runFormPost $ settingsForm userId Nothing
    -- Check if data is well formed, and display errors to user
    case pdeta of 
        FormSuccess a -> do 
                    _ <- runDB $ deleteWhere [ProfileUser ==. userId]
                    _ <- runDB $ insert a
                    setMessage "Profile updated"
        FormFailure t -> setMessage $ toHtml $ show t ++ " D:"
        _             -> setMessage "Error"
    redirect SettingsR 

settingsForm :: UserId -> Maybe Profile -> Form Profile
settingsForm user mprofile = renderDivs $ Profile
    <$> areq textField "Email"    (profileEmail    <$> mprofile)
    <*> areq textField "Username" (profileUsername <$> mprofile)
    <*> pure user
