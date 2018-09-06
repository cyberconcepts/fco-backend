{-# LANGUAGE OverloadedStrings #-}

module Fco.BackendSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck

import Data.IntMap (elems, fromList)

import Fco.Backend (
            getOrCreateNode, getOrCreateTriple, 
            parseQuery, parseTriple, queryTriples,
            setupEnv,
            showTriple, withConnection)
import Fco.Backend.Types (
            Object (..), Triple (..), TripleQuery (..), QueryCrit (..),
            dbSettings, dbName, environment, envDB)


main :: IO ()
main = hspec spec

spec :: Spec
spec = do

  describe "triple store using explicit database connection" $ do

    let settings = dbSettings { dbName = "fco_test" }
        fco = 1
        rdf = 2
        rdfs = 3

    it "finds an existing node" $ withConnection settings $ \conn -> do
        getOrCreateNode conn fco "Node" `shouldReturn` 1
    it "adds a new node" $ withConnection settings $ \conn -> do
        getOrCreateNode conn fco "haskell" `shouldReturn` 7

    it "finds an existing triple" $ withConnection settings $ \conn -> do
        getOrCreateTriple conn 1 2 (NodeRef 5) `shouldReturn` 1
    it "adds a new triple" $ withConnection settings $ \conn -> do
        getOrCreateTriple conn 7 2 (NodeRef 6) `shouldReturn` 7

    it "finds triples when queried" $ withConnection settings $ \conn -> do
        queryTriples conn (TripleQuery (IsEqual 3) Ignore Ignore)
          `shouldReturn` fromList [(3,Triple 3 2 (NodeRef 4))]


  describe "representation of nodes, triples, and graphs" $ do

    let db = dbSettings { dbName = "fco_test" }

    it "displays a triple" $ do
        env <- setupEnv $ environment { envDB = db }
        showTriple env (Triple 3 2 (NodeRef 4)) `shouldReturn`
                "rdf:Property rdf:type rdf:Class"

    it "parses a triple and stores it if it does not exist" $ do
        env <- setupEnv $ environment { envDB = db }
        parseTriple env "rdf:Property rdf:type rdf:Class" `shouldReturn` 3
                --(Triple 3 2 (NodeRef 4) Nothing) 
        parseTriple env "fco:javascript rdf:type fco:topic" `shouldReturn` 8
        parseTriple env "fco:python rdf:type fco:topic" `shouldReturn` 9

    it "queries the triple store" $ do
        env <- setupEnv $ environment { envDB = db }
        withConnection (envDB env) $ \conn -> do 
            qu <- parseQuery env "fco:haskell ? ?"
            tr <- queryTriples conn qu
            mapM (showTriple env) (elems tr) 
                `shouldReturn` ["fco:haskell rdf:type fco:topic"]
