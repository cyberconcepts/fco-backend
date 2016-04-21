{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

module Fco.Backend.Database (
          Connection, DBSettings, 
          connect, disconnect, 
          dbSettings, dbName, credentials, getRow)
where

import BasicPrelude
import Data.Text (unpack)
import Database.HDBC.PostgreSQL (Connection, connectPostgreSQL)
import Database.HDBC (IConnection, SqlValue,
                      disconnect, execute, fetchRow, prepare)


data DBSettings = DBSettings {
                      dbName :: Text,
                      credentials :: (Text, Text) }

dbSettings = DBSettings "fco01" ("fco", "funky")


connect :: DBSettings -> IO Connection
connect settings = connectPostgreSQL $ 
              "dbname=" ++ unpack (dbName settings) ++ 
              " user=" ++ unpack userName ++
              " password=" ++ unpack password
          where (userName, password) = credentials settings


getRow :: IConnection conn => conn -> String -> IO (Maybe [SqlValue])
getRow conn sql = do
    stmt <- prepare conn sql
    execute stmt []
    row <- fetchRow stmt
    return row
