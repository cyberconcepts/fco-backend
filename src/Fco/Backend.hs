{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

module Fco.Backend where

import BasicPrelude
import Data.Text (unpack)
import Fco.Backend.Database (
                Connection, DBSettings,
                connect, disconnect, dbSettings)


fcoConnect :: DBSettings -> IO Connection
fcoConnect settings = connect settings

withConnection :: DBSettings -> (Connection -> IO c) -> IO c
withConnection settings = bracket (fcoConnect settings) disconnect


-- load bootstrap definitions:

-- withConnection settings $ do

-- sys_node <- node "sys:node"
-- sys_datatype <- node "sys:datatype"
-- sys_string <- node "sys:string"
-- rdf_type <- node "rdf:type"
-- rdf_Property <- node "rdf:Property"

-- triple sys_datatype rdf_type (Node sys_datatype)
-- triple sys_node rdf_type (Node sys_datatype)
-- triple sys_string rdf_type (Node sys_datatype)

-- triple rdf_type rdf_type (Node rdf_Property)
-- triple rdf_Property rdf_type (Node rdf_type)
