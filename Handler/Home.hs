{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

module Handler.Home where

import           Control.Applicative
import           Data.Default
import           Data.Text           (Text)
import           Data.Time           (UTCTime, getCurrentTime)
import           Yesod
import           Yesod.Default.Util

import           Foundation
import           Model

getHomeR :: Handler Html
getHomeR = do
    defaultLayout $ do
        setTitle "Score Reporter Home"
        $(widgetFileNoReload def "home")
