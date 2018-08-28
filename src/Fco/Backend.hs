{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

module Fco.Backend where

import BasicPrelude
import Control.Exception (bracket)
import Data.List (lookup)
import qualified Data.Text as T
import Data.IntMap (fromList)

import Fco.Backend.Database (
                Connection,
                connect, disconnect, 
                getNamespaces,
                addNode, getNode, queryNode,
                addTriple, queryTriple)
import Fco.Backend.Database as DB
import Fco.Backend.Types (
                DBSettings, Environment,
                Namespace (..), NamespaceId, 
                NodeId, Node (..), ContextId,
                TripleId, Triple (..),
                Object (..), 
                QueryCrit (..), TripleQuery (..),
                dbSettings, envDB, envNamespaces)
import Fco.Core.Types (
                NodeName)


-- format nodes and triples for display

showNode :: Environment -> NodeId -> IO Text
showNode env nodeId = 
    withConnection (envDB env) $ \conn -> do
        Node nsId name <- getNode conn nodeId
        return $ (getNamespacePrefix env nsId) ++ ":" ++ name

showObject :: Environment -> Object -> IO Text
showObject env (NodeRef id) = showNode env id
showObject env (TextVal txt) = return $ "\"" ++ txt ++ "\""

showTriple :: Environment -> Triple -> IO Text
showTriple env (Triple subject predicate object Nothing) = do
    s <- showNode env subject 
    p <- showNode env predicate
    o <- showObject env object
    return $ unwords [s, p, o]


-- parse nodes and triples using backend store

parseNode :: Environment -> Text -> IO NodeId
parseNode env txt = do
    let (ns, rname) = T.breakOn ":" txt
        nsId = findNameSpaceByPrefix env ns
    withConnection (envDB env) $ \conn ->
        getOrCreateNode conn nsId $ T.tail rname

--parseTriple :: Environment -> Text -> IO Triple
parseTriple :: Environment -> Text -> IO TripleId
parseTriple env txt = do
    let stripSpace = snd . T.span (== ' ')
        txt1 = stripSpace txt
        (st, r1) = T.breakOn " " txt1
        r2 = stripSpace r1
        (pt, r3) = T.breakOn " " r2
        ot = stripSpace r3
    s <- parseNode env st
    p <- parseNode env pt
    o <- parseNode env ot
    --return $ Triple s p (NodeRef o) Nothing
    withConnection (envDB env) $ \conn ->
        getOrCreateTriple conn s p (NodeRef o) Nothing


-- helper functions

getNamespacePrefix :: Environment -> NamespaceId -> Text
getNamespacePrefix env nsId = 
    case lookup nsId (envNamespaces env) of
        Just (Namespace iri prefix) -> prefix
        Nothing -> ""

findNameSpaceByPrefix :: Environment -> Text -> NamespaceId
findNameSpaceByPrefix env prefix = 
    case find checkPrefix (envNamespaces env) of
        Just (id, Namespace iri pf) -> id
        Nothing -> error $ "Namespace " ++ (show prefix) ++ " not found!"
    where checkPrefix (id, Namespace iri pf) = pf == prefix

setupEnv :: Environment -> IO Environment
setupEnv env = 
    withConnection (envDB env) $ \conn -> do
        nss <- getNamespaces conn
        return $ env { envNamespaces = nss }


-- lower-level database-related stuff

fcoConnect :: DBSettings -> IO Connection
fcoConnect settings = connect settings

withConnection :: DBSettings -> (Connection -> IO c) -> IO c
withConnection settings = bracket (fcoConnect settings) disconnect


getOrCreateNode :: Connection -> NamespaceId -> Text -> IO NodeId
getOrCreateNode conn nsId name = do
    result <- queryNode conn nsId name
    case result of
      Nothing -> addNode conn (Node nsId name)
      Just id -> return id

getOrCreateTriple :: Connection -> NodeId -> NodeId -> Object -> ContextId -> 
          IO TripleId
getOrCreateTriple conn subject predicate object context = do
    result <- queryTriple conn subject predicate object context
    case result of
      Nothing -> addTriple conn (Triple subject predicate object context)
      Just id -> return id

queryTriples :: Connection -> TripleQuery -> IO (IntMap Triple)
queryTriples conn query = do
  triples <- DB.queryTriples conn query
  return $ fromList triples

-- load bootstrap definitions (obsolete):

-- withConnection settings $ \conn do

-- let sys = 1
-- let rdf = 2

-- sys_node <- node sys "Node"
-- sys_datatype <- node sys "Datatype"
-- rdf_type <- node rdf "type"

-- sys_string <- node "sys:String"
-- rdf_Property <- node "rdf:Property"

-- addTriple $ Triple sys_datatype rdf_type (NodeRef sys_datatype)
-- = triple sys_datatype rdf_type (NodeRef sys_datatype)
-- ? triple "sys:datatype" "rdf:type" "sys:datatype"

-- triple sys_int rdf_type (NodeRef sys_datatype)
-- triple sys_string rdf_type (NodeRef sys_datatype)

-- triple rdf_type rdf_type (NodeRef rdf_Property)
-- triple rdf_Property rdf_type (NodeRef rdf_type)
