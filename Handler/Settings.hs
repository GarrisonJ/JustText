module Handler.Settings where

import Import
import Handler.Widgets
import Network.Gravatar

getSettingsR :: Handler Html
getSettingsR = do
    userId <- requireAuthId
    mauth <- maybeAuth
    profile <- runDB $ selectFirst [ProfileUser ==. userId] []
    let gravatarSettings = def{gSize=Just (Size 200), gDefault=Just MM}
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
                    uniq <- runDB $ insertUnique a
                    case uniq of
                      Nothing -> case mprofile of
                                    Nothing -> setMessage "Username taken"
                                    (Just p) -> do _ <- runDB $ insertUnique p
                                                   setMessage "Username taken"
                      (Just _ ) -> setMessage "Profile updated"
        FormFailure t -> setMessage $ toHtml $ unwords t
        _             -> setMessage "Error"

    redirect SettingsR

settingsForm :: UserId -> Maybe Profile -> Form Profile
settingsForm user mprofile = renderDivs $ Profile
    <$> areq textField         "Email"    (profileEmail <$> mprofile)
    <*> areq usernameTextField "Username" (profileUsername <$> mprofile)
    <*> pure user
    <*> aopt textField         "Bio"      (profileBio <$> mprofile)
  where
    errorMessage :: Text
    errorMessage = "Username can only contain only letters, numbers, and underscores. And they can only be at most 16 characters long."

    usernameTextField = check validateUsername textField

    validCharacters = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ ['_']

    validateUsername u = if (all (\x -> x `elem` validCharacters) u) && (length u <= 16)
                            then Right u
                            else Left errorMessage
