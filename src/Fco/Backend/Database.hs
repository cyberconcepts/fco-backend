module Fco.Backend.Database where

import Database.HDBC.PostgreSQL (Connection, connectPostgreSQL)
import Database.HDBC (IConnection, SqlValue,
                      execute, fetchRow, prepare)


connect :: IO Connection
connect = connectPostgreSQL "dbname=fco01 user=fco password=funky"


getRow :: IConnection conn => conn -> String -> IO (Maybe [SqlValue])
getRow conn sql = do
    stmt <- prepare conn sql
    execute stmt []
    row <- fetchRow stmt
    return row
