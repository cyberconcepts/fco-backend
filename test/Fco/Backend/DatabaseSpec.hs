{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
module Fco.Backend.DatabaseSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck

import BasicPrelude
import Data.List (sort)
import Data.Text (unpack)
import Database.HDBC (commit, disconnect, getTables, hdbcDriverName, runRaw)

import Fco.Backend.Database (Connection,
        addNode, addTriple, connect, dbSettings, dbName, getNamespaces)
import Fco.Backend.Types (Namespace (..), Object(..))


-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec


withConnection :: (Connection -> IO c) -> IO c
withConnection = bracket (connect settings) disconnect
      where settings = dbSettings { dbName = "fco_test" }

initTestDB :: IO ()
initTestDB = do
    withConnection $ \conn -> do
        let path = "database" </> "postgres" </> "testinit.sql"
        sql <- readFile path
        runRaw conn $ unpack sql
        commit conn


spec :: Spec
spec = do

  describe "initialization" $ do
    it "creates test database" $ do 
      initTestDB `shouldReturn` ()

  describe "db connection" $ do
    it "connects to PostgreSQL database" $ withConnection $ \conn -> do
      hdbcDriverName conn `shouldBe` "postgresql"
    it "has access to fco standard tables" $ withConnection $ \conn -> do
        tables <- getTables conn
        sort tables `shouldBe` 
            ["datatypes", "namespaces", "nodes", "strings", "triples"]

  describe "triple store access" $ do
    it "loads namespaces" $ withConnection $ \conn -> do
      getNamespaces conn `shouldReturn`
        [Namespace 1 "http://functionalconcepts.org/system#" "sys",
         Namespace 2 "http://www.w3.org/1999/02/22-rdf-syntax-ns#" "rdf",
         Namespace 3 "http://www.w3.org/2000/01/rdf-schema#" "rdfs"]
    it "adds nodes" $ withConnection $ \conn -> do
      addNode conn 1 "Node" `shouldReturn` 1
      addNode conn 1 "Datatype" `shouldReturn` 2
      addNode conn 2 "type" `shouldReturn` 3
    it "adds triples" $ withConnection $ \conn -> do
      addTriple conn 1 2 (Node 3) Nothing `shouldReturn` 1
