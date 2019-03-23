{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
module Fco.Backend.DatabaseSpec (main, spec, initTestDB) where

import Test.Hspec
import Test.QuickCheck

import BasicPrelude
import Control.Exception (bracket)
import Data.List (sort)
import Data.Text (unpack)
import Database.HDBC (commit, disconnect, getTables, hdbcDriverName, runRaw)

import Fco.Backend.Database (
        Connection, connect,
        dbName, dbSettings, setEnvDBPool, withDBPool,
        getNamespaces,
        addNode, getNode, queryNode, addTriple, getTriple, 
        queryTriple, queryTriples)
import Fco.Backend.Types (
        Node (..), Triple (..), Object(..),
        TripleQuery (..), QueryCrit (..))
import Fco.Core.Types (Namespace (..))


-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec


withConnection :: (Connection -> IO c) -> IO c
withConnection = bracket (connect settings) disconnect
      where settings = dbSettings { dbName = "fco_test" }

runSqlFromFile :: Connection -> FilePath -> IO ()
runSqlFromFile conn path = do
    sql <- readFile path
    runRaw conn $ unpack sql
    commit conn


initTestDB :: IO ()
initTestDB = do
    withConnection $ \conn -> do
        runSqlFromFile conn $ "database" </> "postgres" </> "drop_tables.sql"
        runSqlFromFile conn $ "database" </> "postgres" </> "init.sql"


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
            ["datatypes", "namespaces", "nodes", "texts", "triples"]

  describe "triple database" $ do

    it "loads namespaces" $ withConnection $ \conn -> do
      getNamespaces conn `shouldReturn`
        [(1, Namespace "http://functionalconcepts.org/fco-common#" "fco"),
         (2, Namespace "http://www.w3.org/1999/02/22-rdf-syntax-ns#" "rdf"),
         (3, Namespace "http://www.w3.org/2000/01/rdf-schema#" "rdfs")]

    it "adds nodes" $ withConnection $ \conn -> do
      addNode conn (Node 1 "topic") `shouldReturn` 6
    it "gets a node" $ withConnection $ \conn -> do
      getNode conn 5 `shouldReturn` (Node 1 "Datatype")
    it "gets a node by namespace and name" $ withConnection $ \conn -> do
      queryNode conn 2 "type" `shouldReturn` (Just 2)

    -- sys:Node rdf:type sys:DataType
    it "adds triples" $ withConnection $ \conn -> do
      addTriple conn (Triple 6 2 (Object 1 4)) `shouldReturn` 6
    it "gets a triple" $ withConnection $ \conn -> do
      getTriple conn 1 `shouldReturn` (Triple 1 2 (Object 1 5))
    it "gets a triple by its components" $ withConnection $ \conn -> do
      queryTriple conn 1 2 (Object 1 5) `shouldReturn` (Just 1)

    it "query: finds triples" $ withConnection $ \conn -> do
      queryTriples conn (TripleQuery (IsEqual 1) Ignore Ignore)
          `shouldReturn` [(1, Triple 1 2 (Object 1 5))]
    it "query: gives empty list if nothing is found" $ withConnection $ \conn -> do
      queryTriples conn (TripleQuery Ignore (IsEqual 1) Ignore)
          `shouldReturn` []

