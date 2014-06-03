module Data.JSON.QQ (JsonValue (..), HashKey (..), parsedJson) where

import Control.Applicative

import Language.Haskell.TH
import Language.Haskell.TH.Quote

import Data.Data
import Data.Maybe

import Data.Ratio
import Text.ParserCombinators.Parsec hiding (many, (<|>))
import Text.ParserCombinators.Parsec.Error

import Language.Haskell.Meta.Parse

parsedJson :: String -> Either ParseError JsonValue
parsedJson txt = parse jpValue "txt" txt

-------
-- Internal representation

data JsonValue =
  JsonNull
  | JsonString String
  | JsonNumber Bool Rational
  | JsonObject [(HashKey,JsonValue)]
  | JsonArray [JsonValue]
  | JsonBool Bool
  | JsonCode Exp

data HashKey =
  HashVarKey String
  | HashStringKey String

type JsonParser = Parser JsonValue

jpValue :: JsonParser
jpValue = do
  spaces
  res <- jpBool <|> jpNull <|> jpString <|> jpObject <|> jpNumber  <|> jpArray <|> jpCode
  spaces
  return res

jpBool :: JsonParser
jpBool = JsonBool <$> (string "true" *> pure True <|> string "false" *> pure False)

jpCode :: JsonParser
jpCode = JsonCode <$> (string "<|" *> parseExp')
  where
    parseExp' = do
      str <- untilString
      case (parseExp str) of
        Left l -> fail l
        Right r -> return r

jpNull :: JsonParser
jpNull = string "null" *> pure JsonNull

jpString :: JsonParser
jpString = between (char '"') (char '"') (option [""] $ many chars) >>= return . JsonString . concat -- do

jpNumber :: JsonParser
jpNumber = do
  val <- float
  return $ JsonNumber False (toRational val)

jpObject :: JsonParser
jpObject = do
  list <- between (char '{') (char '}') (commaSep jpHash)
  return $ JsonObject $ list
  where
    jpHash :: CharParser () (HashKey,JsonValue) -- (String,JsonValue)
    jpHash = do
      spaces
      name <- varKey <|> symbolKey <|> quotedStringKey
      spaces
      char ':'
      spaces
      value <- jpValue
      spaces
      return (name,value)

symbolKey :: CharParser () HashKey
symbolKey = HashStringKey <$> symbol

quotedStringKey :: CharParser () HashKey
quotedStringKey = HashStringKey <$> quotedString

varKey :: CharParser () HashKey
varKey = do
  char '$'
  sym <- symbol
  return $ HashVarKey sym

jpArray :: CharParser () JsonValue
jpArray = JsonArray <$> between (char '[') (char ']') (commaSep jpValue)

-------
-- helpers for parser/grammar

untilString :: Parser String
untilString = do
      n0 <- option "" $ many1 (noneOf "|")
      char '|'
      n1 <- option "" $ many1 (noneOf ">")
      char '>'
      if not $ null n1
        then do n2 <- untilString
                return $ concat [n0,n1,n2]
        else return $ concat [n0,n1]



float :: CharParser st Double
float = do
  isMinus <- option ' ' (char '-')
  d <- many1 digit
  o <- option "" withDot
  e <- option "" withE
  return $ (read $ isMinus : d ++ o ++ e :: Double)

withE = do
  e <- char 'e' <|> char 'E'
  plusMinus <- option "" (string "+" <|> string "-")
  d <- many digit
  return $ e : plusMinus ++ d

withDot = do
  o <- char '.'
  d <- many digit
  return $ o:d

quotedString :: CharParser () String
quotedString = concat <$> between (char '"') (char '"') (option [""] $ many chars)

symbol :: CharParser () String
symbol = many1 (noneOf "\\ \":;><$")

commaSep p  = p `sepBy` (char ',')

chars :: CharParser () String
chars = do
   try (string "\\\"")
   <|> try (string "\\/")
   <|> try (string "\\\\")
   <|> try (string "\\b")
   <|> try (string "\\f")
   <|> try (string "\\n")
   <|> try (string "\\r")
   <|> try (string "\\t")
   <|> try (unicodeChars)
   <|> many1 (noneOf "\\\"")

unicodeChars :: CharParser () String
unicodeChars = do
  u <- string "\\u"
  d1 <- hexDigit
  d2 <- hexDigit
  d3 <- hexDigit
  d4 <- hexDigit
  return $ u ++ [d1] ++ [d2] ++ [d3] ++ [d4]
