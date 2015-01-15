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
    (formWidget, formEncType) <- generateFormPost uploadForm
    people <- getList
    defaultLayout $ do
        setTitle "Job Application Form"
        $(widgetFileNoReload def "home")

postHomeR :: Handler Html
postHomeR = do
    ((result, _), _) <- runFormPost uploadForm
    case result of
      FormSuccess (FormPerson name cv resume time) -> do
          addPerson name cv resume time
          return ()
      _ -> return ()
    redirect HomeR

data FormPerson = FormPerson
   { personName :: Text
    , cv        :: FileInfo
    , resume    :: FileInfo
    , time      :: UTCTime
   }

uploadForm :: Html -> MForm Handler  (FormResult FormPerson, Widget)
uploadForm = renderDivs $ FormPerson
    <$> areq textField "Name" Nothing
    <*> areq fileField  "CV" Nothing
    <*> areq fileField  "Resume" Nothing
    <*> lift (liftIO getCurrentTime)
