{-# LANGUAGE OverloadedStrings #-}
module Fco.BackendSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck

import Fco.Backend (withConnection, node)
import Fco.Backend.Database (DBSettings(..), dbSettings)
import Fco.Backend.DatabaseSpec (initTestDB)


main :: IO ()
main = hspec spec

spec :: Spec
spec = do

  describe "triple store" $ do

    let settings = dbSettings { dbName = "fco_test" }

    it "finds an existing node" $ withConnection settings $ \conn -> do
        node conn 1 "Node" `shouldReturn` 1
    it "adds a new node" $ withConnection settings $ \conn -> do
        node conn 2 "Property" `shouldReturn` 4

