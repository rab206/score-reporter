{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad
import           Control.Monad.Logger
import           Control.Monad.Trans.Resource
import           Database.Persist.Sql
import           Data.Text
import           Yesod

import           Config
import           Dispatch                     ()
import           Foundation
import           Model                        (migrateAll)
import qualified Database.Sqlite as Sqlite
import Database.Persist.Sqlite (createSqlitePool, runSqlPool, sqlDatabase, sqlPoolSize, wrapConnection)

-- insert the tournament id into the game as per 
-- http://pbrisbin.com/posts/writing_json_apis_with_yesod/

--https://www.fpcomplete.com/user/snoyberg/yesod/typesafe-urls-in-client-side-code

-- simple example REST app
-- https://github.com/MrRacoon/ExampleRESTYesod/blob/master/Main.lhs

-- example complex yesod app
-- https://github.com/snoyberg/haskellers/blob/4a039fd69a3ce5bc4f6fc663658f083487a51003/tests/Application.hs

--example basic yesod app
-- https://github.com/lhuang7/yesod-basic-web-template

--http://stackoverflow.com/questions/19272356/override-instance-behaviour

--raw sql https://github.com/yesodweb/yesod/wiki/RawSQL

--enable foreign keys as per https://github.com/yesodweb/yesod/wiki/Activate-foreign-key-checking-in-Sqlite
rawConnection :: Text -> IO Sqlite.Connection
rawConnection t = Sqlite.open t

disableForeignKeys :: Sqlite.Connection -> IO ()
disableForeignKeys conn = Sqlite.prepare conn "PRAGMA foreign_keys = ON;" >>= void . Sqlite.step

main :: IO ()
main = do
--  sqliteConn <- rawConnection (sqlDatabase $ persistConfig)
--  disableForeignKeys sqliteConn
--  pool <- runStdoutLoggingT $ createSqlPool
--        (wrapConnection sqliteConn)
--        (sqlPoolSize $ persistConfig)
  pool <- createPoolConfig persistConfig
  runResourceT $ runStderrLoggingT $ flip runSqlPool pool
      $ runMigration migrateAll
  warpEnv $ App pool
