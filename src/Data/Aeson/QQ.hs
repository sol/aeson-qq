{-# LANGUAGE TemplateHaskell #-}
-- | Have a look at the <https://github.com/sol/aeson-qq#readme README> for
-- documentation.
module Data.Aeson.QQ (aesonQQ) where

import Prelude ()
import Prelude.Compat

import Language.Haskell.TH
import Language.Haskell.TH.Quote

import qualified Data.Vector as V
import Data.String (fromString)
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
          HashStringKey k -> [|(fromString k, $(toExp value))|]
          HashVarKey k -> [|(fromString $(dyn k), $(toExp value))|]
toExp (JsonArray arr) = [|Array $ V.fromList $(ListE <$> mapM toExp arr)|]
toExp (JsonNumber n) = [|Number (fromRational $(return $ LitE $ RationalL (toRational n)))|]
toExp (JsonBool b) = [|Bool b|]
toExp (JsonCode e) = [|toJSON $(return e)|]
