{-# LANGUAGE OverloadedStrings     #-}
module Handler.Teams where

import Import

getTeamsR :: TournamentId -> Handler Value
getTeamsR tid = do
  t <- runDB $ selectList [TeamTournamentId ==. tid] []
  return $ object ["teams" .= t]

postTeamsR :: TournamentId -> Handler ()
postTeamsR tid = do
  teamAttrs <- requireJsonBody :: Handler [TeamAttrs]
  insertTeams tid teamAttrs []
  sendResponseStatus status201 $ object ["tournamentId" .= tid]
