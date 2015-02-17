{-# LANGUAGE OverloadedStrings     #-}
module Handler.Tournament where

import Import

-- Unlike Handler.Home, our getFibR returns a Value result, which is the datatype
-- used for JSON values. We return our result as a JSON object, and place
-- our integral result under the "value" key.
getTournamentR :: TournamentId -> Handler Value
getTournamentR tid = do
  t <- runDB $ get404 tid
  gs <- runDB $ selectList [GameTournamentId ==. tid] []
  teams <- runDB $ selectList [TeamTournamentId ==. tid] [Asc TeamSeeding]
  return $ object ["tournament" .= t,
                    "games" .= gs,
                    "teams" .= teams]
                    
putTournamentR :: TournamentId -> Handler Value
putTournamentR tid = do
    tournament <- requireJsonBody :: Handler Tournament
    runDB $ replace tid tournament
    sendResponseStatus status200 ("UPDATED" :: Text)

postPostTournamentR :: Handler ()
postPostTournamentR = do
    t <- requireJsonBody :: Handler Tournament
    _    <- runDB $ insert t
    sendResponseStatus status201 ("CREATED" :: Text)