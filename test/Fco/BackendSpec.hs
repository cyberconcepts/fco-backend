{-# LANGUAGE OverloadedStrings #-}
module Fco.BackendSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck

import Fco.Backend (withConnection, node, triple)
import Fco.Backend.Database (DBSettings(..), dbSettings)
import Fco.Backend.Types (Object (..))


main :: IO ()
main = hspec spec

spec :: Spec
spec = do

  describe "triple store" $ do

    let settings = dbSettings { dbName = "fco_test" }
    let sys = 1
    let rdf = 2
    let rdfs = 3

    it "finds an existing node" $ withConnection settings $ \conn -> do
        node conn sys "Node" `shouldReturn` 1
    it "adds a new node" $ withConnection settings $ \conn -> do
        node conn rdf "Property" `shouldReturn` 4

    it "finds an existing triple" $ withConnection settings $ \conn -> do
        triple conn 1 3 (NodeRef 2) Nothing `shouldReturn` 1
    it "adds a new triple" $ withConnection settings $ \conn -> do
        triple conn 3 3 (NodeRef 4) Nothing `shouldReturn` 2