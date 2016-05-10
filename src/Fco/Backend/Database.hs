{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

module Fco.Backend.Database (
          Connection, DBSettings, 
          connect, disconnect, 
          dbSettings, dbName, credentials, getRow,
          addNode, getNamespaces) where

import BasicPrelude
import Data.Text (unpack)
import Database.HDBC.PostgreSQL (Connection, connectPostgreSQL)
import Database.HDBC (IConnection, SqlValue,
                      commit, disconnect, execute, fetchAllRows, fetchRow, fromSql,
                      prepare, run, runRaw, toSql)

import Fco.Backend.Types (Identifier, Namespace (..), Node (..),
                      QueryCrit (..), NodeQuery (..))

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

addNode :: IConnection conn => conn -> Node -> IO Identifier
addNode conn (Node 0 nsid name) = do
    let ins = "insert into nodes (namespace, name) values (?, ?)"
    run conn ins [toSql nsid, toSql name]
    commit conn
    let sel = "select id from nodes where namespace = ? and name = ?"
    Just [id] <- getRow conn sel [toSql nsid, toSql name]
    return $ fromSql id

-- updateNode :: Connection -> Node -> Node

-- helper functions

getRows :: IConnection conn => conn -> String -> [SqlValue] -> IO [[SqlValue]]
getRows conn sql params = do
    stmt <- prepare conn sql
    execute stmt params
    fetchAllRows stmt

getRow :: IConnection conn => conn -> String -> [SqlValue] -> IO (Maybe [SqlValue])
getRow conn sql params = do
    stmt <- prepare conn sql
    execute stmt params
    fetchRow stmt

