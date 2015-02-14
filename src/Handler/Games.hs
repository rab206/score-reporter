{-# LANGUAGE OverloadedStrings     #-}
module Handler.Games where

import Import

getGamesR :: TournamentId -> Handler Value
getGamesR tid = do
  t <- runDB $ selectList [GameTournamentId ==. tid] []
  return $ object ["games" .= t]

postGamesR :: TournamentId -> Handler ()
postGamesR tid = do
  gameAttrs <- requireJsonBody :: Handler [GameAttrs]
  insertGames tid gameAttrs []
  sendResponseStatus status201 $ object ["tournamentId" .= tid]
