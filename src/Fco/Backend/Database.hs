{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

module Fco.Backend.Database (
          Connection, DBSettings, 
          connect, disconnect, 
          dbSettings, dbName, credentials, getRow,
          getNamespaces) where

import BasicPrelude
import Data.Text (unpack)
import Database.HDBC.PostgreSQL (Connection, connectPostgreSQL)
import Database.HDBC (IConnection, SqlValue,
                      disconnect, execute, fetchAllRows, fetchRow, fromSql,
                      prepare)

import Fco.Backend.Types (Namespace (..))

-- settings

data DBSettings = DBSettings {
                      dbName :: Text,
                      credentials :: (Text, Text) }

dbSettings = DBSettings "fco01" ("fco", "funky")

-- connect

connect :: DBSettings -> IO Connection
connect settings = connectPostgreSQL $ 
              "dbname=" ++ unpack (dbName settings) ++ 
              " user=" ++ unpack userName ++
              " password=" ++ unpack password
          where (userName, password) = credentials settings

-- API

getNamespaces :: IConnection conn => conn -> IO [Namespace]
getNamespaces conn = do
    rows <- getRows conn "select id, iri, prefix from namespaces" []
    return $ map mkns rows
  where mkns [id, iri, prefix] = 
          Namespace (fromSql id) (fromSql iri) (fromSql prefix)

-- queryNodes :: Connection -> NodeQuery -> [Node]
-- getNode :: Connection -> Id -> Node
-- addNode :: Connection -> Node -> Id
-- updateNode :: Connection -> Node -> Node

-- helper functions

getRows :: IConnection conn => conn -> String -> [SqlValue] -> IO [[SqlValue]]
getRows conn sql params = do
    stmt <- prepare conn sql
    execute stmt params
    fetchAllRows stmt

getRow :: IConnection conn => conn -> String -> IO (Maybe [SqlValue])
getRow conn sql = do
    stmt <- prepare conn sql
    execute stmt []
    fetchRow stmt

