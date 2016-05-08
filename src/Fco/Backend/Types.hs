{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

module Fco.Backend.Types where

import BasicPrelude
import Data.Text

type Identifier = Int

type IRI = Text
type Prefix = Text

data Namespace = Namespace Identifier IRI Prefix 
                    deriving (Show, Eq)
