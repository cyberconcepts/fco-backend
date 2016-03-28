module Fco.Backend.DatabaseSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck

import Control.Exception (bracket)
import Database.HDBC (disconnect, getTables, hdbcDriverName)

import Fco.Backend.Database (connect)

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

withConnection = bracket connect disconnect

spec :: Spec
spec = do
  describe "db connection" $ do
    it "connects to PostgreSQL database" $ withConnection $ \conn -> do
      hdbcDriverName conn `shouldBe` "postgresql"
    it "has access to fco standard tables" $ withConnection $ \conn -> do
      getTables conn `shouldReturn` 
          ["nodes", "datatypes", "iris", "strings", "triples"]
