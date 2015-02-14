{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

module Dispatch where

import           Yesod

import           Foundation
import           Handler.Home
import           Handler.Tournament
import           Handler.Tournaments
import           Handler.Game
import           Handler.Games
import           Handler.Score
import           Handler.Teams


mkYesodDispatch "App" resourcesApp
