{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

module Fco.Backend.Database (
    Connection, DBSettings (..), Environment (..),
    connect, disconnect, 
    dbSettings, environment, setEnvDBPool, withDBPool,
    addNode, getNode, queryNode,
    addText, getText, queryText,
    addTriple, getTriple, queryTriple, queryTriples,
    getNamespaces) where

import BasicPrelude
import Data.Pool (Pool, createPool, withResource)
import Data.Text (unpack)
import Database.HDBC.PostgreSQL (Connection, connectPostgreSQL)
import Database.HDBC (
    IConnection, SqlValue,
    commit, disconnect, execute, fetchAllRows', fetchRow, fromSql,
    prepare, run, runRaw, toSql)

import Fco.Backend.Types (
    NamespaceId, NodeId, TripleId, TextId,
    Node (..), Triple (..), Object (..),  
    QueryCrit (..), TripleQuery (..))
import Fco.Core.Types (Namespace (..), NodeName)


-- * database connections

data Environment = Environment {
                      envDB :: DBSettings,
                      envPool :: Pool Connection,
                      envNamespaces :: [(NamespaceId, Namespace)] }
                    deriving Show

environment = Environment dbSettings undefined []

data DBSettings = DBSettings {
                      dbName :: Text,
                      credentials :: (Text, Text) }
                    deriving Show

dbSettings = DBSettings "fco01" ("fco", "funky")

connect :: DBSettings -> IO Connection
connect settings = connectPostgreSQL $ 
              "dbname=" ++ unpack (dbName settings) ++ 
              " user=" ++ unpack userName ++
              " password=" ++ unpack password
          where (userName, password) = credentials settings

dbPool :: DBSettings -> IO (Pool Connection)
dbPool settings = createPool (connect settings) disconnect 3 600 20

setEnvDBPool :: Environment -> DBSettings -> IO Environment
setEnvDBPool env settings = do
    pool <- dbPool settings
    return env { envPool = pool }

withDBPool :: Environment -> (Connection -> IO c) -> IO c
withDBPool env = withResource $ envPool env


-- * API

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

queryNode :: IConnection conn => conn -> NamespaceId -> NodeName 
      -> IO (Maybe NodeId)
queryNode conn nsId name = do
    let sql = "select id from nodes where namespace = ? and name = ?"
    ids <- getRows conn sql [toSql nsId, toSql name]
    case ids of
      [] -> return Nothing
      _  -> return $ Just $ fromSql (head (head ids))

addText :: IConnection conn => conn -> Text -> IO TextId
addText conn txt = do
    let ins = "insert into texts (text) values (?) returning (id)"
    Just [id] <- getRow conn ins [toSql txt]
    commit conn
    return $ fromSql id

getText :: IConnection conn => conn -> TextId -> IO Text
getText conn id = do
    let sql = "select text from texts where id = ?"
    Just [txt] <- getRow conn sql [toSql id]
    return $ fromSql txt

queryText :: IConnection conn => conn -> Text -> IO (Maybe TextId)
queryText conn txt = do
    let sql = "select id from texts where text = ?"
    ids <- getRows conn sql [toSql txt]
    case ids of
      [] -> return Nothing
      _  -> return $ Just $ fromSql (head (head ids))

-- queryNodesInNS conn nsId
-- update?
-- delete

-- triples

addTriple :: IConnection conn => conn -> Triple -> IO TripleId
addTriple conn (Triple subject predicate (Object dt val)) = do
    let ins = "insert into triples (subject, predicate, datatype, value) \
              \values (?, ?, ?, ?) returning id"
    Just [id] <- getRow conn ins [
                    toSql subject, toSql predicate, toSql dt, toSql val]
    commit conn
    return $ fromSql id

getTriple :: IConnection conn => conn -> TripleId -> IO Triple
getTriple conn id = do
    let sql = "select subject, predicate, datatype, value \
              \from triples where id = ?"
    Just [sId, pId, dt, value] <- getRow conn sql [toSql id]
    return $ Triple (fromSql sId) (fromSql pId) 
                    $ Object (fromSql dt) (fromSql value)

queryTriple :: IConnection conn => conn -> NodeId -> NodeId -> Object
                  -> IO (Maybe TripleId)
queryTriple conn subject predicate (Object dt val) = do
    let sql = "select id from triples where subject = ? and predicate = ? \
                 \and datatype = ? and value = ?"
    ids <- getRows conn sql [toSql subject, toSql predicate, 
                              toSql dt, toSql val]
    case ids of
      [] -> return Nothing
      _  -> return $ Just $ fromSql (head (head ids))

queryTriples :: IConnection conn => conn -> TripleQuery -> IO [(TripleId, Triple)]
queryTriples conn query = do
    let sql0 = "select id, subject, predicate, datatype, value from triples"
        (sqlW, par) = setupTriplesQuery query
        sqlS = case par of
            [] -> sql0
            _  -> sql0 ++ " where "
    result <- getRows conn (unpack (sqlS ++ sqlW)) par
    return $ map makeTriple result 
  where 
      makeTriple :: [SqlValue] -> (TripleId, Triple)
      makeTriple [id, sub, pred, dt, val] = (
          fromSql id, 
          Triple (fromSql sub) (fromSql pred) 
                 $ Object (fromSql dt) (fromSql val))

-- update?
-- delete


-- helper functions

setupTriplesQuery :: TripleQuery -> (Text, [SqlValue])
setupTriplesQuery query = 
    (intercalate " and " qu3, map toSql par3)
  where
    getQuPar col crit qu par = case crit of 
        IsEqual id -> ((col ++ " = ?"):qu, id:par)
        Ignore -> (qu, par)
    getQuParOb crit qu par = case crit of 
        IsEqual (Object dt val) -> 
          (("datatype = ?"):("value = ?"):qu, dt:val:par)
        Ignore -> (qu, par)
    (qu0, par0) = ([], [])
    TripleQuery sub pred obj = query
    (qu1, par1) = getQuPar "subject" sub qu0 par0
    (qu2, par2) = getQuPar "predicate" pred qu1 par1
    (qu3, par3) = getQuParOb obj qu2 par2


getRows :: IConnection conn => conn -> String -> [SqlValue] -> IO [[SqlValue]]
getRows conn sql params = do
    stmt <- prepare conn sql
    execute stmt params
    fetchAllRows' stmt

getRow :: IConnection conn => conn -> String -> [SqlValue] -> IO (Maybe [SqlValue])
getRow conn sql params = do
    stmt <- prepare conn sql
    execute stmt params
    fetchRow stmt

