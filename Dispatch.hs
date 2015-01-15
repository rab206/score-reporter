{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Dispatch where

import Yesod

import Foundation
import Handler.Download
import Handler.Home
import Handler.PreviewFIle
import Handler.DeleteFile
import Handler.Person
import Handler.DeletePerson

mkYesodDispatch "App" resourcesApp
