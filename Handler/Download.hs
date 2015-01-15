{-# LANGUAGE OverloadedStrings #-}

module Handler.Download where

import qualified Data.Text          as Text
import qualified Data.Text.Encoding as Text

import           Yesod

import           Foundation
import           Model

getDownloadR :: Key StoredFile -> Handler TypedContent
getDownloadR ident = do
    StoredFile filename fileType contentType bytes personId time <- getFileById ident
    addHeader "Content-Disposition" $ Text.concat
        [ "attachment; filename=\"", filename, "\""]
    sendResponse (Text.encodeUtf8 contentType, toContent bytes)
