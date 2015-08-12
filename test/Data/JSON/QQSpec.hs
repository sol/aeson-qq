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
      value `shouldBe` JsonObject [(HashStringKey "foo", JsonNumber 23)]

    it "parses decimal numbers" $ do
      let Right value = parsedJson "{foo: 5.97}"
      value `shouldBe` JsonObject [(HashStringKey "foo", JsonNumber 5.97)]

    context "empty objects" $ do
      it "parses empty objects (regression test)" $ do
        let Right value = parsedJson "{}"
        value `shouldBe` JsonObject []

      it "parses empty objects that include whitespace (regression test)" $ do
        let Right value = parsedJson "{ }"
        value `shouldBe` JsonObject []

      it "parses empty objects that include newlines (regression test)" $ do
        let Right value = parsedJson "{\n}"
        value `shouldBe` JsonObject []

    context "empty arrays" $ do
      it "parses empty arrays" $ do
        let Right value = parsedJson "[   ]"
        value `shouldBe` JsonArray []

      it "parses empty arrays that include whitespace (regression test)" $ do
        let Right value = parsedJson "[   ]"
        value `shouldBe` JsonArray []

      it "parses empty objects that include newlines (regression test)" $ do
        let Right value = parsedJson "[\n]"
        value `shouldBe` JsonArray []

    it "fails on excess input" $ do
      let Left err = parsedJson "{foo: 23} some excess input"
      show err `shouldBe` "\"txt\" (line 1, column 11):\nunexpected 's'\nexpecting space or end of input"
