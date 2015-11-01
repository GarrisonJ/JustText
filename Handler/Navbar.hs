module Handler.Navbar where

import Import

navbar :: Maybe (Entity User) -> Widget
navbar mauth = toWidget [hamlet|
          <div class="row">
            <nav>
              <div .nav-wrapper .grey .lighten-3>
                <a href=@{HomeR} .brand-logo>&nbsp;&#9646;&#9646;&#9646;
                <ul .right>
                  $maybe (Entity id _) <- mauth
                    <li>
                      <a href=@{ProfileR id}>Profile
                    <li>
                      <a href=@{SettingsR}>Settings
                    <li>
                      <a href=@{AuthR LogoutR}>Logout
                  $nothing
                    <li>
                      <a href=@{LandingR}><i>Login</i>
         |]


