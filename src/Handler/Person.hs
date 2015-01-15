{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

module Handler.Person where

import           Control.Exception  hiding (Handler)

import           Data.Default
import           Data.Text          (Text)
import qualified Data.Text          as Text
import qualified Data.Text.Encoding as Text
import           Data.Time          (UTCTime, getCurrentTime)
import           Text.Blaze
import           Yesod
import           Yesod.Default.Util

import           Foundation
import           Model

getPersonR :: PersonId -> Handler Html
getPersonR personId = do
    Person name time <- getPersonById personId
    files <- getFilesByPersonId personId
    defaultLayout $ do
        setTitle . toMarkup $ "Job Applicant " `Text.append` name
        $(widgetFileNoReload def "person")

