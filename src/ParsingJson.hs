{-
-- EPITECH PROJECT, 2024
-- mypandoc
-- File description:
-- ParsingJson.hs
-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use $>" #-}

module ParsingJson (parseJsonValue, JsonValue(..)) where
{-
  Module : ParsingJson
  Description : This Haskell module provides functions for parsing JSON data into a structured format.

  This module exports the following functions:

  1. parseJsonValue :: Parser JsonValue
    Takes a String and returns a `Maybe` `JsonValue`. It is used to parse a string of JSON data into a structured format.

  The module also includes several helper functions for parsing different parts of JSON data. These functions are not exported and are used internally by the `parseJsonValue` function.

  Helper functions:

  1. parseNull :: Parser JsonValue
    A `Parser` for `JsonNull`.

  2. parseBool :: Parser JsonValue
    A `Parser` for `JsonBool`.

  3. parseNumber :: Parser JsonValue
    A `Parser` for `JsonNumber`.

  4. parseString :: Parser JsonValue
    A `Parser` for `JsonString`.

  5. parseComma :: Parser Char
    A `Parser` for comma.

  6. parseJsonArray :: Parser JsonValue
    A `Parser` for `JsonArray`.

  7. parseJsonObject :: Parser JsonValue
    A `Parser` for `JsonObject`.

  8. parsePair :: Parser (String, JsonValue)
    A `Parser` for pairs.

  9. parseKey :: Parser String
    A `Parser` for keys.

  10. parseColon :: Parser Char
     A `Parser` for colons.

  The module also defines the `JsonValue` data type and makes it an instance of the `Show` typeclass:

  1. JsonValue
    Represents parsed JSON data. It can either be a `JsonNull`, `JsonBool`, `JsonNumber`, `JsonString`, `JsonArray`, `JsonArrayS`, or `JsonObject`.

  2. instance Show JsonValue
    Defines how `JsonValue`s are converted to strings.
-}

import Lib (string, parseWhitespace, parseChar, parseMany, parseAnyChar, parseSome, Parser(..), Alternative(..))
import Data.List ((\\))
import Control.Applicative (optional)

-- `JsonValue` represents a JSON value.
data JsonValue
  = JsonNull
  | JsonBool Bool
  | JsonNumber Double
  | JsonString String
  | JsonArray [JsonValue]
  | JsonArrayS [JsonValue]
  | JsonObject [(String, JsonValue)] deriving (Eq)

-- `Show` instance for `JsonValue`.
instance Show JsonValue where
  show JsonNull = "JsonNull"
  show (JsonBool b) = "JsonBool " ++ show b
  show (JsonNumber n) = "JsonNumber " ++ show n
  show (JsonString s) = "JsonString " ++ show s
  show (JsonArray a) = "JsonArray " ++ show a
  show (JsonObject o) = "JsonObject " ++ show o
  show (JsonArrayS a) = "JsonArrayS " ++ show a

-- `parseJsonValue` function takes a String and returns a `Maybe` `JsonValue`.
parseJsonValue :: Parser JsonValue
parseJsonValue = parseNull
  <|> parseBool
  <|> parseNumber
  <|> parseString
  <|> parseJsonArray
  <|> parseJsonObject

-- `parseNull` function is a `Parser` for `JsonNull`.
parseNull :: Parser JsonValue
parseNull = string "null" *> pure JsonNull

-- `parseBool` function is a `Parser` for `JsonBool`.
parseBool :: Parser JsonValue
parseBool = (string "true" *> pure (JsonBool True))
  <|> (string "false" *> pure (JsonBool False))

-- `parseNumber` function is a `Parser` for `JsonNumber`.
parseNumber :: Parser JsonValue
parseNumber = fmap (JsonNumber . read)
  (parseSome (parseAnyChar "-.0123456789"))

-- `parseString` function is a `Parser` for `JsonString`.
parseString :: Parser JsonValue
parseString = fmap JsonString (parseChar '"'
  *> parseMany (parseAnyChar ([' '..'~'] \\ ['"'])) <* parseChar '"')

-- `parseWhitespace` function is a `Parser` for whitespace.
parseComma :: Parser Char
parseComma = do
  _ <- parseChar ','
  _ <- parseWhitespace
  return ','

-- `parseJsonArray` function is a `Parser` for `JsonArray`.
parseJsonArray :: Parser JsonValue
parseJsonArray = do
  _ <- parseWhitespace
  _ <- parseChar '['
  _ <- parseWhitespace
  values <- parseMany (parseJsonValue <* optional parseComma)
  _ <- parseWhitespace
  _ <- parseChar ']'
  _ <- parseWhitespace
  return (JsonArray values)

-- `parseJsonObject` function is a `Parser` for `JsonObject`.
parseJsonObject :: Parser JsonValue
parseJsonObject = do
  _ <- parseWhitespace
  _ <- parseChar '{'
  _ <- parseWhitespace
  pairs <- parseMany parsePair
  _ <- parseWhitespace
  _ <- parseChar '}'
  _ <- parseWhitespace
  return (JsonObject (map checkContent pairs))

-- `checkContent` function takes a pair and returns a pair.
checkContent :: (String, JsonValue) -> (String, JsonValue)
checkContent ("content", JsonArray arr) = ("content", JsonArrayS arr)
checkContent ("body", JsonArray arr) = ("body", JsonArrayS arr)
checkContent ("codeblock", JsonArray arr) = ("codeblock", JsonArrayS arr)
checkContent pair = pair

-- `parsePair` function is a `Parser` for pairs.
parsePair :: Parser (String, JsonValue)
parsePair = do
  key <- parseKey
  _ <- parseColon
  value <- parseJsonValue
  _ <- optional parseComma
  _ <- parseWhitespace
  return (key, value)

-- `parseKey` function is a `Parser` for keys.
parseKey :: Parser String
parseKey = do
  keyJson <- parseString
  case keyJson of
    JsonString str -> return str
    _ -> error "Expected a JsonString"

-- `parseColon` function is a `Parser` for colons.
parseColon :: Parser Char
parseColon = parseWhitespace
  *> parseChar ':'
  <* parseWhitespace
