{-# LANGUAGE TemplateHaskell #-}
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

import Control.Applicative
import qualified Data.Vector as V
import qualified Data.Text as T
import Data.Aeson

import Data.JSON.QQ as QQ

aesonQQ :: QuasiQuoter
aesonQQ = QuasiQuoter {
  quoteExp = jsonExp,
  quotePat = const $ error "No quotePat defined for jsonQQ",
  quoteType = const $ error "No quoteType defined for jsonQQ",
  quoteDec = const $ error "No quoteDec defined for jsonQQ"
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
toExp (JsonString str) = [|String (T.pack str)|]
toExp (JsonNull) = [|Null|]
toExp (JsonObject objs) = [|object $jsList|]
    where
      jsList :: ExpQ
      jsList = ListE <$> mapM objs2list (objs)

      objs2list :: (HashKey, JsonValue) -> ExpQ
      objs2list (key, value) = do
        case key of
          HashStringKey k -> [|(T.pack k, $(toExp value))|]
          HashVarKey k -> [|(T.pack $(dyn k), $(toExp value))|]
toExp (JsonArray arr) = [|Array $ V.fromList $(ListE <$> mapM toExp arr)|]
toExp (JsonNumber _ rat) = [|Number (fromRational $(return $ LitE $ RationalL rat))|]
toExp (JsonIdVar v) = dyn v
toExp (JsonBool b) = [|Bool b|]
toExp (JsonCode e) = [|toJSON $(return e)|]

-- Helpers
packE :: Exp -> ExpQ
packE e = [|T.pack $(return e)|]
