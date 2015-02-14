{-# LANGUAGE OverloadedStrings #-}

module Main where


import           Control.Monad.Logger
import           Control.Monad.Trans.Resource
import           Database.Persist.Sql
import           Yesod

import           Config
import           Dispatch                     ()
import           Foundation
import           Model                        (migrateAll)

-- insert the tournament id into the game as per 
-- http://pbrisbin.com/posts/writing_json_apis_with_yesod/

--https://www.fpcomplete.com/user/snoyberg/yesod/typesafe-urls-in-client-side-code

-- simple example REST app
-- https://github.com/MrRacoon/ExampleRESTYesod/blob/master/Main.lhs

--http://stackoverflow.com/questions/19272356/override-instance-behaviour
main :: IO ()
main = do
    pool <- createPoolConfig persistConfig
    runResourceT $ runStderrLoggingT $ flip runSqlPool pool
        $ runMigration migrateAll
    warpEnv $ App pool
