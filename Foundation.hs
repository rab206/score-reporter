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
import           Model.FileType

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

getList :: Handler [Entity Person]
getList = runDB $ selectList [] [Asc PersonId]

addPerson :: Text -> FileInfo -> FileInfo -> UTCTime -> Handler ()
addPerson name cv resume time = do
    cvBytes <- extractBytes cv
    resumeBytes <- extractBytes resume
    runDB $ do
      personId <- insert (Person name time)
      insert_ $ StoredFile (fileName cv) CV (fileContentType cv) cvBytes personId time
      insert_ $ StoredFile (fileName resume) Resume (fileContentType cv) resumeBytes personId time
    return ()

deletePerson :: PersonId -> Handler ()
deletePerson personId = runDB $ delete personId

updateFile :: StoredFileId -> FileInfo -> Handler ()
updateFile fileId fi = do
    bytes <- extractBytes fi
    runDB $ update fileId [StoredFileName =. fileName fi,
                           StoredFileContentType =. fileContentType fi,
                           StoredFileContent =. bytes]

deleteFile :: StoredFileId -> Handler ()
deleteFile ident = runDB $ delete ident

getFileById :: StoredFileId -> Handler StoredFile
getFileById ident = do
    mfile <- runDB $ get ident
    case mfile of
      Nothing -> notFound
      Just file -> return file

getFilesByPersonId :: Key Person -> Handler [Entity StoredFile]
getFilesByPersonId personId = runDB $ selectList [StoredFilePersonId <-. [personId]] []

getPersonById :: PersonId -> Handler Person
getPersonById ident = do
    mperson <- runDB $ get ident
    case mperson of
      Nothing -> notFound
      Just person -> return person

extractBytes :: FileInfo ->  Handler S.ByteString
extractBytes fi = do
    fileBytes <- runResourceT $ fileSource fi $$ sinkLbs
    return $ S.pack . L.unpack $ fileBytes
