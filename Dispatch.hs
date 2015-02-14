{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

module Dispatch where

import           Yesod

import           Foundation
import           Handler.Home
import           Handler.Tournament
import           Handler.Tournaments
import           Handler.TournamentGames


mkYesodDispatch "App" resourcesApp
