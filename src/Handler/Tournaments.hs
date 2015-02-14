{-# LANGUAGE OverloadedStrings     #-}
module Handler.Tournaments where

import Import

-- Unlike Handler.Home, our getFibR returns a Value result, which is the datatype
-- used for JSON values. We return our result as a JSON object, and place
-- our integral result under the "value" key.
getTournamentsR :: Handler Value
getTournamentsR = do
    ts <- runDB $ selectList [] [] :: Handler [Entity Tournament]
    return $ object ["tournaments" .= ts]