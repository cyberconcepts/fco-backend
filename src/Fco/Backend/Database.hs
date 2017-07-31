{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

module Fco.Backend.Database (
          Connection, DBSettings,
          connect, disconnect, 
          dbSettings, dbName, credentials,
          addNode, getNode, queryNode,
          addTriple, getTriple, queryTriple, queryTriples,
          getNamespaces) where

import BasicPrelude
import Data.Text (unpack)
import Database.HDBC.PostgreSQL (Connection, connectPostgreSQL)
import Database.HDBC (IConnection, SqlValue,
                      commit, disconnect, execute, fetchAllRows, fetchRow, fromSql,
                      prepare, run, runRaw, toSql)

import Fco.Backend.Types (
          Name, NamespaceId, NodeId, ContextId, TripleId,
          Namespace (..), Node (..), Triple (..), Object (..),  
          QueryCrit (..), TripleQuery (..))

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

-- namespaces

getNamespaces :: IConnection conn => conn -> IO [(NamespaceId, Namespace)]
getNamespaces conn = do
    rows <- getRows conn "select id, iri, prefix from namespaces" []
    return $ map mkns rows
  where mkns :: [SqlValue] -> (NamespaceId, Namespace)
        mkns [id, iri, prefix] = 
          ((fromSql id), Namespace (fromSql iri) (fromSql prefix))

-- nodes

addNode :: IConnection conn => conn -> Node -> IO NodeId
addNode conn (Node nsid name) = do
    let ins = "insert into nodes (namespace, name) values (?, ?) returning (id)"
    Just [id] <- getRow conn ins [toSql nsid, toSql name]
    commit conn
    return $ fromSql id

getNode :: IConnection conn => conn -> NodeId -> IO Node
getNode conn id = do
    let sql = "select namespace, name from nodes where id = ?"
    Just [nsid, name] <- getRow conn sql [toSql id]
    return $ Node (fromSql nsid) (fromSql name)

queryNode :: IConnection conn => conn -> NamespaceId -> Name -> IO (Maybe NodeId)
queryNode conn nsId name = do
    let sql = "select id from nodes where namespace = ? and name = ?"
    ids <- getRows conn sql [toSql nsId, toSql name]
    case ids of
      [] -> return Nothing
      _  -> return $ Just $ fromSql (head (head ids))

-- queryNodesInNS conn nsId
-- update?
-- delete

-- triples

addTriple :: IConnection conn => conn -> Triple -> IO TripleId
addTriple conn (Triple subject predicate (NodeRef obj) context) = do
    let ins = "insert into triples (subject, predicate, datatype, value) \
              \values (?, ?, ?, ?) returning id"
    Just [id] <- getRow conn ins [
                    toSql subject, toSql predicate, toSql (1 :: Int), toSql obj]
    commit conn
    return $ fromSql id

getTriple :: IConnection conn => conn -> TripleId -> IO Triple
getTriple conn id = do
    let sql = "select subject, predicate, datatype, value \
              \from triples where id = ?"
    Just [sId, pId, dt, value] <- getRow conn sql [toSql id]
    return $ Triple (fromSql sId) (fromSql pId) (NodeRef $ fromSql value) Nothing

queryTriple :: IConnection conn => conn -> NodeId -> NodeId -> Object -> ContextId
                  -> IO (Maybe TripleId)
queryTriple conn subject predicate (NodeRef obj) context = do
    let sql = "select id from triples where subject = ? and predicate = ? \
                 \and datatype = ? and value = ?"
    ids <- getRows conn sql [toSql subject, toSql predicate, 
                              toSql (1 :: Int), toSql obj]
    case ids of
      [] -> return Nothing
      _  -> return $ Just $ fromSql (head (head ids))

queryTriples :: IConnection conn => conn -> TripleQuery -> IO [(TripleId, Triple)]
queryTriples conn query = do
    let sql0 = "select id, subject, predicate, datatype, value from triples where "
    let (qu0, par0) = ([], [])
    let TripleQuery sub pred obj ctx = query
    let (qu1, par1) = getQuPar "subject" sub qu0 par0
    let (qu2, par2) = getQuPar "predicate" pred qu1 par1
    let (qu3, par3) = getQuParOb "object" obj qu2 par2
    let sql = sql0 ++ (intercalate " and " qu3)
    result <- getRows conn (unpack sql) (map toSql par3)
    return $ map makeTriple result 
  where 
      getQuPar col crit qu par = case crit of 
          IsEqual id -> ((col ++ " = ?"):qu, id:par)
          Ignore -> (qu, par)
      getQuParOb col crit qu par = case crit of 
          IsEqual (NodeRef id) -> ((col ++ " = ?"):qu, id:par)
          Ignore -> (qu, par)
      makeTriple :: [SqlValue] -> (TripleId, Triple)
      makeTriple [id, sub, pred, dt, val] = (
          fromSql id, 
          Triple (fromSql sub) (fromSql pred) (NodeRef (fromSql val)) Nothing)

-- update?
-- delete

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
