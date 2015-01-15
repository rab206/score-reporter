{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Handler.DeletePerson where

import Yesod

import Foundation
import Model

getDeletePersonR :: PersonId -> Handler Html
getDeletePersonR personId = do
    deletePerson personId
    redirect HomeR
