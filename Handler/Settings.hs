module Handler.Settings where

import Import

getSettingsR :: Handler Html
getSettingsR = do
    userId <- requireAuthId
    profile <- runDB $ selectFirst [ProfileUser ==. userId] []
    (formWidget, formEnctype) <- generateFormPost $ settingsForm userId
    defaultLayout $ do
        aDomId <- newIdent
        setTitle "Just text"
        $(widgetFile "settings")

postSettingsR :: Handler ()
postSettingsR = do
    userId <- requireAuthId
    ((pdeta, _), _) <- runFormPost $ settingsForm userId
    case pdeta of 
        FormSuccess a -> do 
                    let e = profileEmail a
                    _ <- runDB $ deleteWhere [ProfileEmail ==. e]
                    _ <- runDB $ insert a
                    setMessage "Profile updated"
        FormFailure t -> setMessage $ toHtml $ show t ++ " D:"
        _             -> setMessage "Error"
    redirect SettingsR 

settingsForm :: UserId -> Form Profile
settingsForm user = renderDivs $ Profile
    <$> areq textField "email" Nothing
    <*> areq textField "username" Nothing
    <*> pure user
