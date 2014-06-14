{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module Data.Aeson.QQSpec (main, spec) where

import           Test.Hspec

import           Data.Char
import           Data.Aeson

import qualified Person
import           Data.Aeson.QQ

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "aesonQQ" $ do
    it "handles escape sequences" $ do
      [aesonQQ|{foo: "ba r.\".\\.r\n"}|] `shouldBe` object [("foo", "ba r.\".\\.r\n")]

    it "can construct arrays" $ do
      [aesonQQ|[null, {foo: 23}]|] `shouldBe` toJSON [Null, object [("foo", Number 23.0)]]

    it "can construct true, false and null" $ do
      [aesonQQ|[true, false, null]|] `shouldBe` toJSON [Bool True, Bool False, Null]

    it "accepts quotes around field names" $ do
      [aesonQQ|{"foo": "bar"}|] `shouldBe` object [("foo", "bar")]

    it "can parse multiline strings" $ do
      [aesonQQ|
        [   {
          user:
            "Joe"},
         {user: "John"}]
       |] `shouldBe` toJSON [object [("user", "Joe")], object [("user", "John")]]

    it "can interpolate JSON values" $ do
      let x = object [("foo", Number 23)]
      [aesonQQ|[null, #{x}]|] `shouldBe` toJSON [Null, x]

    it "can interpolate field names" $ do
      let foo = "zoo"
      [aesonQQ|{$foo: "bar"}|] `shouldBe` object [("zoo", "bar")]

    it "can interpolate numbers" $ do
      let x = 23 :: Int
      [aesonQQ|[null, {foo: #{x}}]|] `shouldBe` toJSON [Null, object [("foo", Number 23)]]

    it "can interpolate strings" $ do
      let foo = "bar" :: String
      [aesonQQ|{foo: #{foo}}|] `shouldBe` object [("foo", "bar")]

    it "can interpolate data types" $ do
      let foo = Person.Person "Joe" 23
      [aesonQQ|#{foo}|] `shouldBe` object [("name", "Joe"), ("age", Number 23)]

    it "can interpolate simple expressions" $ do
      let x = 23 :: Int
          y = 42
      [aesonQQ|{foo: #{x + y}}|] `shouldBe` object [("foo", Number 65)]

    it "can interpolate more complicated expressions" $ do
      let name = "Joe"
      [aesonQQ|{name: #{map toUpper name}}|] `shouldBe` object [("name", "JOE")]
