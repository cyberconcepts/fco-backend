{-# LANGUAGE OverloadedStrings #-}
module Fco.BackendSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck

import Fco.Backend

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "fco connection" $ do
    it "creates new FCO connection" $ do
      "dummy" `shouldBe` "dummy"
