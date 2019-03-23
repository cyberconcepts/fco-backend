{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

module Fco.Backend.Types where

import BasicPrelude
import Data.Text

import Fco.Core.Types (Namespace (..), NodeName)


-- * RDF-related types

-- basic types (type synonyms)

type Identifier = Int
type DataTypeId = Identifier
type NamespaceId = Identifier
type NodeId = Identifier
type TripleId = Identifier
type TextId = Identifier

data Node = Node NamespaceId NodeName deriving (Eq, Show)

data Triple = Triple NodeId NodeId Object deriving (Eq, Show)

--data Object = NodeRef NodeId | IntVal Int | TextVal Text
--                    deriving (Eq, Ord, Show)
data Object = Object DataTypeId Int deriving (Eq, Show)


data QueryCrit a = IsEqual a | Ignore deriving (Eq, Show)

data TripleQuery = TripleQuery 
                        (QueryCrit NodeId) 
                        (QueryCrit NodeId)
                        (QueryCrit Object)
                    deriving (Eq, Show)

