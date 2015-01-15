{-# LANGUAGE OverloadedStrings #-}
module Config where

import Database.Persist.Sqlite

persistConfig :: SqliteConf
persistConfig = SqliteConf "database" 100