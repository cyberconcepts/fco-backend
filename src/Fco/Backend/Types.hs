{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

module Fco.Backend.Types where

import BasicPrelude
import Data.Text

import Fco.Core.Types (Namespace (..), NodeName)

-- basic types (type synonyms)

type Identifier = Int
type DataTypeId = Identifier
type NamespaceId = Identifier
type NodeId = Identifier
type TripleId = Identifier
type TextId = Identifier

-- complex types

data Node = Node NamespaceId NodeName deriving (Eq, Show)

data Triple = Triple NodeId NodeId Object deriving (Eq, Show)

--data Object = NodeRef NodeId | IntVal Int | TextVal Text
--                    deriving (Eq, Ord, Show)
data Object = Object DataTypeId Int deriving (Eq, Show)

-- query criteria

data QueryCrit a = IsEqual a | Ignore deriving (Eq, Show)

data TripleQuery = TripleQuery 
                        (QueryCrit NodeId) 
                        (QueryCrit NodeId)
                        (QueryCrit Object)
                    deriving (Eq, Show)


-- settings / environment / graph state

data Environment = Environment {
                      envDB :: DBSettings,
                      envNamespaces :: [(NamespaceId, Namespace)] }
                    deriving Show

environment = Environment dbSettings []

-- database settings

data DBSettings = DBSettings {
                      dbName :: Text,
                      credentials :: (Text, Text) }
                    deriving Show

dbSettings = DBSettings "fco01" ("fco", "funky")
