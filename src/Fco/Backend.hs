{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

module Fco.Backend where

import BasicPrelude
import Data.Text (unpack)
import Fco.Backend.Database (
                Connection, DBSettings,
                connect, disconnect, dbSettings,
                addNode, queryNode,
                addTriple, queryTriple)
import Fco.Backend.Types (
                NamespaceId, 
                NodeId, Node (..), ContextId,
                TripleId, Triple (..),
                Object (..), 
                QueryCrit (..), TripleQuery (..))


fcoConnect :: DBSettings -> IO Connection
fcoConnect settings = connect settings

withConnection :: DBSettings -> (Connection -> IO c) -> IO c
withConnection settings = bracket (fcoConnect settings) disconnect


node :: Connection -> NamespaceId -> Text -> IO NodeId
node conn nsId name = do
    result <- queryNode conn nsId name
    case result of
      Nothing -> addNode conn (Node nsId name)
      Just id -> return id

triple :: Connection -> NodeId -> NodeId -> Object -> ContextId -> IO TripleId
triple conn subject predicate object context = do
    result <- queryTriple conn subject predicate object context
    case result of
      Nothing -> addTriple conn (Triple subject predicate object context)
      Just id -> return id

-- load bootstrap definitions:

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
