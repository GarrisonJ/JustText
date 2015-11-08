module Handler.HomeSpec (spec) where

import TestImport

spec :: Spec
spec = withApp $ do
    it "loads the index and checks it looks right" $ do
        get HomeR
        statusIs 200
        htmlAllContain ".navbar-brand" "TTxTT"

    it "loads and displays all messages" $ do
        get HomeR
        statusIs 200
        msg <- runDB $ selectList ([] :: [Filter Message]) []
        htmlCount ".message" $ length msg
