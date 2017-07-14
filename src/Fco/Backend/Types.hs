{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

module Fco.Backend.Types where

import BasicPrelude
import Data.Text

-- basic types (type synonyms)

type Identifier = Int64
type NamespaceId = Identifier
type NodeId = Identifier
type TripleId = Identifier
type ContextId = Maybe NodeId

type IRI = Text
type Prefix = Text
type Name = Text

-- complex types

data Namespace = Namespace NamespaceId IRI Prefix 
                    deriving (Eq, Ord, Show)

data Node = Node NamespaceId Text deriving (Eq, Show)

data Triple = Triple NodeId NodeId Object ContextId deriving (Eq, Show)

data Object = NodeRef NodeId | IntVal Int64 | TextVal Text
                    deriving (Eq, Ord, Show)

-- query criteria

data QueryCrit a = IsEqual a | Ignore deriving (Eq, Show)

data NodeQuery = NodeQuery (QueryCrit NamespaceId) (QueryCrit Name)
                    deriving (Eq, Show)

data TripleQuery = TripleQuery 
                        (QueryCrit NodeId) 
                        (QueryCrit NodeId)
                        (QueryCrit Object)
                        (QueryCrit ContextId)
                    deriving (Eq, Show)
