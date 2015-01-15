{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}

module Handler.PreviewFIle where

import           Control.Applicative
import           Control.Exception   hiding (Handler)
import qualified Data.ByteString     as SB
import           Data.Default
import           Data.Text           (Text)
import qualified Data.Text           as Text
import qualified Data.Text.Encoding  as Text
import           Data.Time           (UTCTime, getCurrentTime)
import           Text.Blaze
import           Yesod
import           Yesod.Default.Util

import           Foundation
import           Model

getPreviewR :: Key StoredFile -> Handler Html
getPreviewR fileId = do
    (formWidget, formEncType) <- generateFormPost uploadForm
    StoredFile filename fileType contentType bytes personId time <- getFileById fileId
    Person name time <- getPersonById personId
    defaultLayout $ do
        setTitle . toMarkup $ "File Processor - " `Text.append` filename
        previewBlock <- liftIO $ preview fileId contentType bytes
        $(widgetFileNoReload def "preview")

preview :: Key StoredFile -> Text -> SB.ByteString -> IO Widget
preview ident contentType bytes
  | "image/" `Text.isPrefixOf` contentType =
    return [whamlet|<img src=@{DownloadR ident}>|]
  | otherwise = do
     eText <- try . evaluate $ Text.decodeUtf8 bytes :: IO (Either SomeException Text)
     return $ case eText of
      Left _ -> errorMessage
      Right text -> [whamlet|<pre>#{text}|]
  where
    errorMessage = [whamlet|<pre>Unable to display 1

{-# LANGUAGE OverloadedStrings #-}file contents.|]

postPreviewR :: StoredFileId -> Handler Html
postPreviewR fileId = do
    ((result, _), _) <- runFormPost uploadForm
    case result of
      FormSuccess (FormFile fi time) -> do
          updateFile fileId fi
          return ()
      _ -> return ()
    redirect HomeR

data FormFile = FormFile{
      file :: FileInfo
    , time :: UTCTime
   }
uploadForm :: Html -> MForm Handler (FormResult FormFile, Widget)
uploadForm = renderDivs $ FormFile
    <$> areq fileField "" Nothing
    <*> lift (liftIO getCurrentTime)
