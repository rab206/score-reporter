{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

module Handler.Home where

import           Data.Default
import           Yesod
import           Yesod.Default.Util

import           Foundation

getHomeR :: Handler Html
getHomeR =
    defaultLayout $ do
        setTitle "Score Reporter Home"
        $(widgetFileNoReload def "home")
