{-# LANGUAGE OverloadedStrings #-}
module Data.JSON.QQSpec (main, spec) where

import           Test.Hspec

import           Data.JSON.QQ

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "parsedJson" $ do
    it "parses JSON" $ do
      let Right value = parsedJson "{foo: 23}"
      value `shouldBe` JsonObject [(HashStringKey "foo", JsonNumber False 23)]

    it "parses decimal numbers" $ do
      let Right value = parsedJson "{foo: 5.97}"
      value `shouldBe` JsonObject [(HashStringKey "foo", JsonNumber False 5.97)]

    it "parses empty objects (regression test)" $ do
      let Right value = parsedJson "{}"
      value `shouldBe` JsonObject []

    it "fails on excess input" $ do
      let Left err = parsedJson "{foo: 23} some excess input"
      show err `shouldBe` "\"txt\" (line 1, column 11):\nunexpected 's'\nexpecting space or end of input"
