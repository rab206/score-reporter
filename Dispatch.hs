{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

module Dispatch where

import           Yesod

import           Foundation
import           Handler.DeleteFile
import           Handler.DeletePerson
import           Handler.Download
import           Handler.Home
import           Handler.Person
import           Handler.PreviewFIle

mkYesodDispatch "App" resourcesApp
