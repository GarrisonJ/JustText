module Handler.Landing where

import Import
import Yesod.Auth.BrowserId

authLinkWidget :: Widget
authLinkWidget = do
    onclick <- createOnClick def AuthR
    let loginIcon = PluginR "browserid" ["static", "sign-in.png"]
    [whamlet|
      <div .center><a href="javascript:#{onclick}()" class="waves-effect waves-light btn-large">Sign up / Log in</a>
    |]

getLandingR :: Handler Html
getLandingR = do
  user <- maybeAuth
  defaultLayout $ do
    setTitle "TTxTT"
    $(widgetFile "welcome")
