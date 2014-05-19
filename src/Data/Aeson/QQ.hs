-- | This package expose the function @aesonQQ@ that compile time converts json code into a @Data.Aeson.Value@.
--    @aesonQQ@ got the signature
--
--    > aesonQQ :: QuasiQuoter
--
--    and is used like
--
--    > myCode = [aesonQQ| {age: 23, name: "Pelle", likes: ["mac","Haskell"] } |]
--
--    where it is important that
--
--    * you got no space in @[aesonQQ|@ and
--
--    * no additional code after @|]@.
--
--    The quasiquatation can also bind to variables like
--
--    > myCode = [aesonQQ| {age: <|age|>, name: <|name|>} |]
--    > where age = 34 :: Integer
--    >       name = "Pelle"
--
--    where the function  @toJSON@ will be called on @age@ and @name@ runtime.
--
--    You can also insert Haskell code:
--
--    > myCode = [aesonQQ| {age: <|age + 34 :: Integer|>, name: <|map toUpper name|>} |]
--    > where age = 34 :: Integer
--    >       name = "Pelle"
--
--    You can use a similar syntax if you want to insert a value of type Data.Aeson.Value like
--
--    > myCode = [aesonQQ| {"age": <<age>>} |]
--
--    If you want to replace the name of the key in a hash you'll use the $-syntax:
--
--    > foo = [aesonQQ| {$bar: 42} |]
--    > bar = "age"
--

module Data.Aeson.QQ (
  aesonQQ
) where

import Language.Haskell.TH
import Language.Haskell.TH.Quote

import Data.Data
import Data.Maybe
import Control.Applicative

import Data.JSON.QQ as QQ

import Data.Aeson as A
import Data.Aeson.Generic

import Data.Ratio
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Error

import Language.Haskell.Meta.Parse
import Data.Scientific

aesonQQ :: QuasiQuoter
aesonQQ = QuasiQuoter {
  quoteExp = jsonExp,
  quotePat = \s -> error "No quotePat defined for jsonQQ",
  quoteType = \s -> error "No quoteType defined for jsonQQ",
  quoteDec = \s -> error "No quoteDec defined for jsonQQ"
}


jsonExp :: String -> ExpQ
jsonExp txt =
  case parsed' of
    Left err -> error $ "Error in aesonExp: " ++ show err
    Right val -> toExp val
  where
    parsed' = QQ.parsedJson txt

----
-- JSValue etc to ExpQ
---------
toExp :: QQ.JsonValue -> ExpQ

toExp (JsonString str) = return $
    AppE (ConE $ mkName "Data.Aeson.Types.String") (packE (LitE (StringL $ str)))

toExp (JsonNull) = return $ ConE $ mkName "Data.Aeson.Types.Null"

toExp (JsonObject objs) =
    AppE (VarE $ mkName "Data.Aeson.Types.object") . ListE <$> jsList
    where
      jsList :: Q [Exp] -- [(String,JSValue)]
      jsList = mapM objs2list (objs)
      objs2list :: (HashKey, JsonValue) -> ExpQ
      objs2list (key, value) = do
        v <- toExp value
        return $ case key of
          HashStringKey k -> TupE [packE (LitE (StringL k)), v]
          HashVarKey k -> TupE [packE (VarE $ mkName k), v]

toExp (JsonArray arr) =
    AppE (ConE $ mkName "Data.Aeson.Types.Array") . AppE (VarE $ mkName "Data.Vector.fromList") . ListE <$> mapM toExp arr

toExp (JsonNumber b rat) = return $
    AppE (ConE $ mkName "Data.Aeson.Types.Number") (AppE (VarE $ mkName "fromFloatDigits") (LitE (RationalL rat)))
toExp (JsonIdVar v) = return $
    VarE $ mkName v

toExp (JsonBool b) = return $
    AppE (ConE $ mkName "Data.Aeson.Types.Bool") (ConE $ mkName (if b then "True" else "False"))

toExp (JsonCode exp) = return $
    AppE (VarE $ mkName "Data.Aeson.Generic.toJSON") exp

-- Helpers
packE = AppE (VarE $ mkName "Data.Text.pack")

