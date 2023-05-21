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
      parsedJson "{foo: 23}" `shouldBe` Right (JsonObject [(HashStringKey "foo", JsonNumber 23)])

    it "parses decimal numbers" $ do
      parsedJson "{foo: 5.97}" `shouldBe` Right (JsonObject [(HashStringKey "foo", JsonNumber 5.97)])

    context "empty objects" $ do
      it "parses empty objects (regression test)" $ do
        parsedJson "{}" `shouldBe` Right (JsonObject [])

      it "parses empty objects that include whitespace (regression test)" $ do
        parsedJson "{ }" `shouldBe` Right (JsonObject [])

      it "parses empty objects that include newlines (regression test)" $ do
        parsedJson "{\n}" `shouldBe` Right (JsonObject [])

    context "empty arrays" $ do
      it "parses empty arrays" $ do
        parsedJson "[   ]" `shouldBe` Right (JsonArray [])

      it "parses empty arrays that include whitespace (regression test)" $ do
        parsedJson "[   ]" `shouldBe` Right (JsonArray [])

      it "parses empty objects that include newlines (regression test)" $ do
        parsedJson "[\n]" `shouldBe` Right (JsonArray [])

    it "fails on excess input" $ do
        let Left err = parsedJson "{foo: 23} some excess input"
        show err `shouldBe` "\"txt\" (line 1, column 11):\nunexpected 's'\nexpecting space or end of input"
