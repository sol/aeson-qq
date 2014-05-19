{-# LANGUAGE TemplateHaskell, QuasiQuotes, DeriveGeneric #-}
module Main where

import Data.Aeson as A
import Data.Aeson.QQ
import Data.Aeson.Types

-- for test
import Test.Framework.TH
import Test.Framework.Providers.HUnit
import Test.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.Framework (defaultMain)

import Data.Ratio
import Data.Vector
import Data.Text
import Data.Attoparsec
import Data.Attoparsec.Number

import Data.Char

import Language.Haskell.TH
import Data.Scientific

import GHC.Generics

main = $defaultMainGenerator

case_get_QQ_to_compile = do
  let actual = [aesonQQ| {foo: "ba r.\".\\.r\n"} |]
      expected = A.object [(pack "foo", string' "ba r.\\\".\\\\.r\\n")]
  expected @=? actual

case_arrays = do
  let actual = [aesonQQ| [null,{foo: -42}] |]
      expected = arrays [A.Null, A.object [(pack "foo", number (-42.0))] ]
  expected @=? actual

case_code = do
  let actual = [aesonQQ| [null,{foo: <|x|>}] |]
      expected = arrays [A.Null, object [(pack "foo", number 42)] ]
      x = 42 :: Integer
  expected @=? actual

case_true = do
  let actual = [aesonQQ| [true,false,null] |]
      expected = arrays [A.Bool True, A.Bool False, A.Null]
  expected @=? actual

case_json_var = do
  let actual = [aesonQQ| [null,{foo: <<x>>}] |]
      expected = arrays [A.Null, object [(pack "foo", number 42 )] ]
      x = A.toJSON ( 42 :: Integer)
  expected @=? actual

case_foo = do
  let actual = [aesonQQ| <|foo|> |]
      expected = object [(pack "age", number 42 ) ]
      foo = Bar 42
  expected @=? actual

case_quoted_name = do
  let actual = [aesonQQ| {"foo": "bar"} |]
      expected = object [(pack "foo", string' "bar")]
      foo = "zoo"
  expected @=? actual

case_var_name = do
  let actual = [aesonQQ| {$foo: "bar"} |]
      expected = object [(pack "zoo", string' "bar")]
      foo = "zoo"
  expected @=? actual

case_multiline = do
  let actual =
        [aesonQQ|
          [   {
            user:
              "Pelle"},
           {user: "Arne"}]
         |]
      expected = arrays [object [(pack "user", string' "Pelle")], object [ (pack "user", string' "Arne")] ]
  expected @=? actual

case_simple_code = do
  let actual = [aesonQQ| { foo: <| foo |> } |]
      expected = object [(pack "foo", string' "zoo")]
      foo = "zoo"
  expected @=? actual


case_semi_advanced_code = do
  let actual = [aesonQQ| { foo: <| foo + 45 |> } |]
      expected = object [(pack "foo", number 133)]
      foo = 88 :: Integer
  expected @=? actual


case_semi_advanced_char = do
  let actual = [aesonQQ| { name: <| Prelude.map Data.Char.toUpper name |> } |]
      expected = object [(pack "name", string' "PELLE")]
      name = "Pelle"
  expected @=? actual


-- Data types

data Foo = Bar { age :: Integer}
  deriving (Eq, Show, Generic)

instance ToJSON Foo


-- Helpers

arrays = A.Array . fromList

string' s = A.String $ pack s

number = A.Number
