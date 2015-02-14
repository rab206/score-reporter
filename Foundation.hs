{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

module Foundation where

import           Data.Default
import           Data.Time (UTCTime, getCurrentTime)
import           Data.Text
import           GHC.Generics
import           Database.Persist.Sql
import           Text.Hamlet
import           Yesod
import           Yesod.Default.Util
import Data.Conduit
import qualified Data.Conduit.List as CL

import           Config
import           Model

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

-- put helper methods and db methods here


data GameAttrs = GameAttrs {pitchNumber :: Int, time :: UTCTime, seeding1 :: Int, seeding2 :: Int} deriving Generic

instance FromJSON GameAttrs

data TeamAttrs = TeamAttrs {name :: Text, twitterHandle :: Maybe Text, seeding :: Int} deriving Generic

instance FromJSON TeamAttrs

data ScoreAttrs = ScoreAttrs {scoreTeam :: TeamId, scorerName :: Maybe Text,  assisterName :: Maybe Text} deriving Generic

instance FromJSON ScoreAttrs

insertGames :: TournamentId -> [GameAttrs] -> [Game] -> Handler ()
insertGames tid (g:gs) rs = insertGames tid gs (Game tid (pitchNumber g) (time g) (seeding1 g) (seeding2 g):rs)
insertGames _ [] rs = runDB $ insertMany_ rs

insertTeams :: TournamentId -> [TeamAttrs] -> [Team] -> Handler ()
insertTeams tid (t:ts) rs = insertTeams tid ts (Team tid (name t) (twitterHandle t) (seeding t):rs)
insertTeams _ [] rs = runDB $ insertMany_ rs

insertScore :: GameId -> ScoreAttrs -> Handler ()
insertScore gid s = do
  time <- liftIO getCurrentTime
  runDB $ insert_ (Score gid (scoreTeam s) (scorerName s) (assisterName s) time)

selectTeamScore :: GameId -> TournamentId -> Int -> Handler Int
selectTeamScore gid tid seed = do
    let sql = "select count(*) from score join team on score.team_id = team.id where score.game_id = ? and team.tournament_id = ? and team.seeding = ?"
    res <- runDB $ rawQuery sql ((toPersistValue gid):(toPersistValue tid):(toPersistValue seed):[]) $$ CL.consume
    return $ toVal res

toVal::[[PersistValue]] -> (Int)
toVal [[total]] = (toInt total)
toInt :: PersistValue -> Int
toInt PersistNull = 0
toInt v = either (error . unpack) id $ fromPersistValue v