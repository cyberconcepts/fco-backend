{-# LANGUAGE OverloadedStrings #-}

module Fco.BackendSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck

import Data.IntMap (fromList)

import Fco.Backend (
            getOrCreateNode, getOrCreateTriple, 
            parseNode, parseTriple, queryTriples,
            setupEnv,
            showNode, showTriple, withConnection)
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
        getOrCreateTriple conn 1 2 (NodeRef 5) Nothing `shouldReturn` 1
    it "adds a new triple" $ withConnection settings $ \conn -> do
        getOrCreateTriple conn 7 2 (NodeRef 6) Nothing `shouldReturn` 7

    it "finds triples when queried" $ withConnection settings $ \conn -> do
      queryTriples conn (TripleQuery (IsEqual 3) Ignore Ignore Ignore)
          `shouldReturn` fromList [(3,Triple 3 2 (NodeRef 4) Nothing)]


  describe "representation of nodes, triples, and graphs" $ do

    let db = dbSettings { dbName = "fco_test" }

    it "shows name of a node" $ do
        env <- setupEnv $ environment { envDB = db }
        showNode env 6 `shouldReturn` "fco:topic"

    it "displays a triple" $ do
        env <- setupEnv $ environment { envDB = db }
        showTriple env (Triple 3 2 (NodeRef 4) Nothing) `shouldReturn`
                "rdf:Property rdf:type rdf:Class"

    it "parses a node name and finds (or creates) the node" $ do
        env <- setupEnv $ environment { envDB = db }
        parseNode env "fco:topic" `shouldReturn` 6
        parseNode env "fco:haskell" `shouldReturn` 7
        parseNode env "fco:javascript" `shouldReturn` 8

    it "parses a triple and stores it if it does not exist" $ do
        env <- setupEnv $ environment { envDB = db }
        parseTriple env "rdf:Property rdf:type rdf:Class" `shouldReturn` 3
                --(Triple 3 2 (NodeRef 4) Nothing) 
        parseTriple env "fco:javascript rdf:type fco:topic" `shouldReturn` 8
        parseTriple env "fco:python rdf:type fco:topic" `shouldReturn` 9
