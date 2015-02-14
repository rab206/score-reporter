{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

module Foundation where

import           Control.Applicative
import           Control.Monad.Trans.Resource
import qualified Data.ByteString              as S
import qualified Data.ByteString.Lazy         as L
import           Data.Conduit
import           Data.Conduit.Binary
import           Data.Default
import           Data.Text                    (Text)
import           Data.Time                    (UTCTime)
import           Database.Persist.Sql
import           Text.Hamlet
import           Yesod
import           Yesod.Default.Util

import           Config
import           Model

data App = App
    { connPool :: ConnectionPool
    }

instance Yesod App where
  defaultLayout widget = do
    pc <- widgetToPageContent $ $(widgetFileNoReload def "default-layout")
    withUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")

instance RenderMessage App FormMessage where
  renderMessage _ _ = defaultFormMessage

instance YesodPersist App where
  type YesodPersistBackend App = SqlBackend
  runDB = defaultRunDB (const persistConfig) connPool

instance YesodPersistRunner App where
  getDBRunner = defaultGetDBRunner connPool


mkYesodData "App" $(parseRoutesFile "config/routes")

-- put helper methods and db methods here