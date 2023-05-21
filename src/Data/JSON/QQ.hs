{-# LANGUAGE CPP #-}
module Data.JSON.QQ (JsonValue (..), HashKey (..), parsedJson) where

import           Control.Applicative
import           Language.Haskell.TH
import           Text.ParserCombinators.Parsec hiding (many, (<|>))
import           Language.Haskell.Meta.Parse
import qualified Data.Attoparsec.Text as A
import           Data.Scientific (Scientific)
import qualified Data.Text as T

#if MIN_VERSION_base(4,11,0)
import           Data.Functor ((<&>), ($>))
#else
import           Data.Functor (($>))

(<&>) :: Functor f => f a -> (a -> b) -> f b
(<&>) = flip fmap

infixl 1 <&>
#endif

parsedJson :: String -> Either ParseError JsonValue
parsedJson = parse (jpValue <* eof) "txt"

-------
-- Internal representation

data JsonValue =
    JsonNull
  | JsonString String
  | JsonNumber Scientific
  | JsonObject [(HashKey,JsonValue)]
  | JsonArray [JsonValue]
  | JsonBool Bool
  | JsonCode Exp
  deriving (Eq, Show)

data HashKey =
    HashVarKey String
  | HashStringKey String
  deriving (Eq, Show)

type JsonParser = Parser JsonValue

jpValue :: JsonParser
jpValue = do
  spaces
  res <- jpBool <|> jpNull <|> jpString <|> jpObject <|> jpNumber  <|> jpArray <|> jpCode
  spaces
  return res

jpBool :: JsonParser
jpBool = JsonBool <$> ((string "true" $> True) <|> (string "false" $> False))

jpCode :: JsonParser
jpCode = JsonCode <$> (string "#{" *> parseExp')
  where
    parseExp' = do
      str <- many1 (noneOf "}") <* char '}'
      case parseExp str of
        Left l -> fail l
        Right r -> return r

jpNull :: JsonParser
jpNull = string "null" $> JsonNull

jpString :: JsonParser
jpString = between (char '"') (char '"') (option [""] $ many chars) <&> (JsonString . concat) -- do

jpNumber :: JsonParser
jpNumber = JsonNumber <$> do
  isMinus <- option "" (string "-")
  d <- many1 digit
  o <- option "" withDot
  e <- option "" withE
  convert (isMinus ++ d ++ o ++ e)
  where
    withE = do
      e <- char 'e' <|> char 'E'
      plusMinus <- option "" (string "+" <|> string "-")
      d <- many digit
      return $ e : plusMinus ++ d

    withDot = do
      o <- char '.'
      d <- many digit
      return $ o:d

    convert :: String -> Parser Scientific
    convert = either fail return . A.parseOnly (A.scientific <* A.endOfInput) . T.pack

jpObject :: JsonParser
jpObject = do
  list <- between (char '{') (char '}') (spaces *> commaSep jpHash)
  return $ JsonObject list
  where
    jpHash :: CharParser () (HashKey,JsonValue) -- (String,JsonValue)
    jpHash = do
      spaces
      name <- varKey <|> symbolKey <|> quotedStringKey
      spaces
      _ <- char ':'
      spaces
      value <- jpValue
      spaces
      return (name,value)

symbolKey :: CharParser () HashKey
symbolKey = HashStringKey <$> symbol

quotedStringKey :: CharParser () HashKey
quotedStringKey = HashStringKey <$> quotedString

varKey :: CharParser () HashKey
varKey = HashVarKey <$> (char '$' *> symbol)

jpArray :: CharParser () JsonValue
jpArray = JsonArray <$> between (char '[') (char ']') (spaces *> commaSep jpValue)

-------
-- helpers for parser/grammar
quotedString :: CharParser () String
quotedString = concat <$> between (char '"') (char '"') (option [""] $ many chars)

symbol :: CharParser () String
symbol = many1 (noneOf "\\ \":;><${}")

commaSep :: CharParser () a -> CharParser () [a]
commaSep p  = p `sepBy` char ','

chars :: CharParser () String
chars = do
       try (string "\\\"" $> "\"")
   <|> try (string "\\\\" $> "\\")
   <|> try (string "\\/" $> "/")
   <|> try (string "\\b" $> "\b")
   <|> try (string "\\f" $> "\f")
   <|> try (string "\\n" $> "\n")
   <|> try (string "\\r" $> "\r")
   <|> try (string "\\t" $> "\t")
   <|> try unicodeChars
   <|> many1 (noneOf "\\\"")

unicodeChars :: CharParser () String
unicodeChars = do
  u <- string "\\u"
  d1 <- hexDigit
  d2 <- hexDigit
  d3 <- hexDigit
  d4 <- hexDigit
  return $ u ++ [d1] ++ [d2] ++ [d3] ++ [d4]
