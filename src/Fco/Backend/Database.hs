{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

module Fco.Backend.Database (
          Connection, DBSettings, 
          connect, disconnect, 
          dbSettings, dbName, credentials, getRow,
          addNode, addTriple, getNamespaces) where

import BasicPrelude
import Data.Text (unpack)
import Database.HDBC.PostgreSQL (Connection, connectPostgreSQL)
import Database.HDBC (IConnection, SqlValue,
                      commit, disconnect, execute, fetchAllRows, fetchRow, fromSql,
                      prepare, run, runRaw, toSql)

import Fco.Backend.Types (
          Name, NamespaceId, NodeId, ContextId, TripleId,
          Namespace (..), Node (..), Triple (..), Object (..),  
          QueryCrit (..), NodeQuery (..), TripleQuery (..))

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

-- getNode ::  -> NodeId -> IO Node
-- queryNodes :: IConnection conn => conn -> NodeQuery -> IO [Node]

-- getTriple :: IConnection conn => conn -> TripleId -> IO Triple
-- queryTriples :: IConnection conn => conn -> TripleQuery -> IO [Triple]

addNode :: IConnection conn => conn -> Node -> IO NodeId
addNode conn (Node nsid name) = do
    let ins = "insert into nodes (namespace, name) values (?, ?) returning (id)"
    Just [id] <- getRow conn ins [toSql nsid, toSql name]
    commit conn
    return $ fromSql id

addTriple :: IConnection conn => conn -> Triple -> IO TripleId
--addTriple :: IConnection conn => conn -> NodeId -> NodeId -> Object -> ContextId
--                 -> IO TripleId
addTriple conn (Triple subject predicate (NodeRef obj) context) = do
    let ins = "insert into triples (subject, predicate, datatype, value) \
              \values (?, ?, ?, ?) returning id"
    Just [id] <- getRow conn ins [
                    toSql subject, toSql predicate, toSql (1 :: Int), toSql obj]
    commit conn
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

