module Handler.Navbar where

import Import

-- 626262
navbar :: Maybe (Entity User) -> Widget
navbar mauth = do
        addStylesheetRemote "https://fonts.googleapis.com/icon?family=Material+Icons"
        toWidget [julius|
          $(".button-collapse").sideNav();
        |]
        toWidget [hamlet|
          <div class="navbar-fixed">
            <nav .transparent style="box-shadow: none;">
              <div .nav-wrapper .transparent>
                <a href=@{HomeR} .brand-logo .white-text>&nbsp;&#9646;&#9646;&#9646;
                $maybe (Entity id _) <- mauth
                  <a href="#" data-activates="mobile-demo" class="button-collapse"><i class="material-icons">menu</i></a>
                  <ul .right .hide-on-med-and-down>
                      <li>
                        <a href=@{ProfileR id}>Profile
                      <li>
                        <a href=@{SettingsR}>Settings
                      <li>
                        <a href=@{AuthR LogoutR}>Logout
                  <ul .side-nav #mobile-demo>
                      <li>
                        <a href=@{ProfileR id}>Profile
                      <li>
                        <a href=@{SettingsR}>Settings
                      <li>
                        <a href=@{AuthR LogoutR}>Logout
                $nothing
                  <ul .right>
                    <li>
                      <a href=@{LandingR}>Login
         |]


