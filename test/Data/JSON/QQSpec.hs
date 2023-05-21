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
      let value = parsedJson "{foo: 23}"
      value `shouldSatisfy` either
        (const False)
        (== JsonObject [(HashStringKey "foo", JsonNumber 23)])

    it "parses decimal numbers" $ do
      let value = parsedJson "{foo: 5.97}"
      value `shouldSatisfy` either
        (const False)
        (== JsonObject [(HashStringKey "foo", JsonNumber 5.97)])

    context "empty objects" $ do
      it "parses empty objects (regression test)" $ do
        let value = parsedJson "{}"
        value `shouldSatisfy` either (const False) (== JsonObject [])

      it "parses empty objects that include whitespace (regression test)" $ do
        let value = parsedJson "{ }"
        value `shouldSatisfy` either (const False) (== JsonObject [])

      it "parses empty objects that include newlines (regression test)" $ do
        let value = parsedJson "{\n}"
        value `shouldSatisfy` either (const False) (== JsonObject [])

    context "empty arrays" $ do
      it "parses empty arrays" $ do
        let value = parsedJson "[   ]"
        value `shouldSatisfy` either (const False) (== JsonArray [])

      it "parses empty arrays that include whitespace (regression test)" $ do
        let value = parsedJson "[   ]"
        value `shouldSatisfy` either (const False) (== JsonArray [])

      it "parses empty objects that include newlines (regression test)" $ do
        let value = parsedJson "[\n]"
        value `shouldSatisfy` either (const False) (== JsonArray [])

    it "fails on excess input" $ do
      let value = parsedJson "{foo: 23} some excess input"
      value `shouldSatisfy` either
        ((== "\"txt\" (line 1, column 11):\nunexpected 's'\nexpecting space or end of input") . show)
        (const False)
