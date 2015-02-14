{-# LANGUAGE OverloadedStrings     #-}
module Handler.Score where

import Import

getScoreR :: GameId -> ScoreId -> Handler Value
getScoreR gid sid = do
  score <- runDB $ get404 sid
  return $ toJSON score

postPostScoreR :: GameId -> Handler ()
postPostScoreR gid = do
  scoreAttr <- requireJsonBody :: Handler ScoreAttrs
  insertScore gid scoreAttr
  sendResponseStatus status201 $ object ["gameId" .= gid]