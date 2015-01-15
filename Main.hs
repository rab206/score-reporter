{-# LANGUAGE OverloadedStrings #-}

module Main where


import Control.Monad.Logger
import Control.Monad.Trans.Resource
import Database.Persist.Sql
import Yesod

import Config
import Dispatch ()
import Foundation
import Model (migrateAll)

main :: IO ()
main = do
    pool <- createPoolConfig persistConfig
    runResourceT $ runStderrLoggingT $ flip runSqlPool pool
        $ runMigration migrateAll
    warpEnv $ App pool