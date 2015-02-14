{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE DeriveGeneric #-}
module Handler.TournamentGames where


import GHC.Generics
import Import

data GameAttrs = GameAttrs {pitchNumber :: Int, time :: UTCTime, seeding1 :: Int, seeding2 :: Int} deriving Generic

instance FromJSON GameAttrs


getTournamentGamesR :: TournamentId -> Handler Value
getTournamentGamesR id = do
  t <- runDB $ selectList [GameTournamentId ==. id] []
  return $ object ["games" .= t]

postTournamentGamesR :: TournamentId -> Handler ()
postTournamentGamesR tid = do
  gameAttrs <- requireJsonBody :: Handler [GameAttrs]
  _ <- insertGames tid gameAttrs []
  sendResponseStatus status201 $ object ["tournamentId" .= tid]

insertGames tid (g:gs) rs = insertGames tid gs ((Game tid (pitchNumber g) (time g) (seeding1 g) (seeding2 g)):rs)
insertGames _ [] rs = do runDB $ insertMany rs
