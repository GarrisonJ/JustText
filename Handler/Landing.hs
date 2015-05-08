module Handler.Landing where

import Import
import Yesod.Auth.BrowserId

authLinkWidget :: Widget
authLinkWidget = do
    onclick <- createOnClick def AuthR
    loginIcon <- return $ PluginR "browserid" ["static", "sign-in.png"]
    [whamlet|
      <div><a href="javascript:#{onclick}()"><img src=@{AuthR loginIcon}></a>
    |]

getLandingR :: Handler Html
getLandingR = do
  user <- maybeAuth
  defaultLayout $ do
    setTitle "Just Text"
    $(widgetFile "welcome")
