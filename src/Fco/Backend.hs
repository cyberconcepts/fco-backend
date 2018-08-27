{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

module Fco.Backend where

import BasicPrelude
import Control.Exception (bracket)
import Data.List (lookup)
import Data.Text (pack, unpack)
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
                dbSettings, envDB)
import Fco.Core.Types (
                NodeName)


showNode :: Environment -> NodeId -> IO Text
showNode env nodeId = 
    withConnection (envDB env) $ \conn -> do
      Node nsId name <- getNode conn nodeId
      nss <- getNamespaces conn
      let nsp = case lookup nsId nss of
                Just (Namespace iri prefix) -> prefix
                Nothing -> ""
      return $ nsp ++ ":" ++ name


--instance Read Node where
--  readPrec = readNode

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

getOrCreateTriple :: Connection -> NodeId -> NodeId -> Object -> ContextId -> IO TripleId
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
