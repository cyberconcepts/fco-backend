{-# LANGUAGE OverloadedStrings #-}
module Fco.Backend.DatabaseSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck

import Control.Exception (bracket)
import Data.List (sort)
import Database.HDBC (disconnect, getTables, hdbcDriverName)

import Fco.Backend.Database (connect, dbSettings, dbName, getNamespaces)
import Fco.Backend.Types (Namespace (..))

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec


withConnection = bracket (connect settings) disconnect
      where settings = dbSettings { dbName = "fco_test" }

getTablesSorted conn = do 
          tables <- getTables conn
          return $ sort tables


spec :: Spec
spec = do

  describe "db connection" $ do
    it "connects to PostgreSQL database" $ withConnection $ \conn -> do
      hdbcDriverName conn `shouldBe` "postgresql"
    it "has access to fco standard tables" $ withConnection $ \conn -> do
      getTablesSorted conn `shouldReturn` 
          ["datatypes", "namespaces", "nodes", "strings", "triples"]

  describe "triple store access" $ do
    it "loads namespaces" $ withConnection $ \conn -> do
      getNamespaces conn `shouldReturn`
        [Namespace 1 "http://functionalconcepts.org/system#" "sys",
         Namespace 2 "http://www.w3.org/1999/02/22-rdf-syntax-ns#" "rdf",
         Namespace 3 "http://www.w3.org/2000/01/rdf-schema#" "rdfs"]
