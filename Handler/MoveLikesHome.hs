module Handler.MoveLikesHome where

import Import
import Handler.Like
import Handler.Home

getMoveLikesHomeR :: Handler Html
getMoveLikesHomeR = do
    deleteAllUnauthLikes
    getPaginatesR 0
