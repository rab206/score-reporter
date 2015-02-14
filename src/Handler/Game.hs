{-# LANGUAGE OverloadedStrings     #-}
module Handler.Game where

import Import

getGameR :: TournamentId -> GameId -> Handler Value
getGameR tid gid = do
  game <- runDB $ get404 gid
  team1Score <- selectTeamScore gid (gameTournamentId game) (gameSeed1 game)
  team2Score <- selectTeamScore gid (gameTournamentId game) (gameSeed2 game)
  sendResponseStatus status201 $ object ["game" .= game,
                                         "team1Score" .= team1Score,
                                         "team2Score" .= team2Score]

putGameR :: TournamentId -> GameId -> Handler ()
putGameR tid gid = do
  gameAttrs <- requireJsonBody :: Handler [GameAttrs]
  insertGames tid gameAttrs []
  sendResponseStatus status201 $ object ["tournamentId" .= tid]

deleteGameR :: TournamentId -> GameId -> Handler ()
deleteGameR tid gid = do
  runDB $ delete gid
  sendResponseStatus status200 $ object ["tournamentId" .= tid]