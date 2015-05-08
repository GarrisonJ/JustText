module Handler.Settings where

import Import
import Handler.Widgets

getSettingsR :: Handler Html
getSettingsR = do
    userId <- requireAuthId
    mauth <- maybeAuth
    profile <- runDB $ selectFirst [ProfileUser ==. userId] []
    let mprofile = case profile of
            Nothing -> Nothing
            Just (Entity _ p) -> Just p
    (formWidget, formEnctype) <- generateFormPost $ settingsForm userId mprofile
    defaultLayout $ do
        setTitle "Just text"
        $(widgetFile "settings")

postSettingsR :: Handler ()
postSettingsR = do
    userId <- requireAuthId
    profile <- runDB $ selectFirst [ProfileUser ==. userId] []
    let mprofile = case profile of
            Nothing -> Nothing
            Just (Entity _ p) -> Just p
    ((pdeta, _), _) <- runFormPost $ settingsForm userId mprofile
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
