{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

module Fco.Backend.Types where

import BasicPrelude
import Data.Text


type Identifier = Int

type IRI = Text
type Prefix = Text

type NamespaceId = Identifier
type Name = Text

type ContextId = Maybe Identifier

data Namespace = Namespace Identifier IRI Prefix 
                    deriving (Eq, Ord, Show)

data Node = Node Identifier NamespaceId Name
                    deriving (Eq, Ord, Show)


data QueryCrit a = Equal a | Null | None

data NodeQuery = NodeQuery (QueryCrit NamespaceId) (QueryCrit Name)
