{-
-- EPITECH PROJECT, 2024
-- mypandoc
-- File description:
-- ParsingJson.hs
-}

module ParsingXml (parseXmlValue, XmlValue(..)) where
{-
  Module : ParsingXml
  Description : This Haskell module provides functions for parsing XML data into a structured format.

  This module exports the following functions:

  1. parseXmlValue :: Parser XmlValue
    Takes a String and returns a `Maybe` `XmlValue`. It is used to parse a string of XML data into a structured format.

  The module also includes several helper functions for parsing different parts of XML data. These functions are not exported and are used internally by the `parseXmlValue` function.

  Helper functions:

  1. parseTag :: Parser XmlValue
    A `Parser` for `XmlValue` tags.

  2. parseManyUntil :: Parser a -> Parser b -> Parser [a]
    Takes two parsers and returns a parser that parses many instances of the first parser until the second parser succeeds.

  3. parseOpenTag :: Parser (String, [(String, String)])
    A parser for open tags.

  4. parseCloseTag :: Parser ()
    A parser for close tags.

  5. parseClosingTag :: String -> Parser String
    A parser for closing tags.

  6. parseContent :: Parser XmlValue
    A parser for content.

  7. parseTagName :: Parser String
    A parser for tag names.

  8. parseAttributes :: Parser [(String, String)]
    A parser for attributes.

  9. parseAttribute :: Parser (String, String)
    A parser for attributes.

  The module also defines the `XmlValue` data type and makes it an instance of the `Show` typeclass:

  1. XmlValue
    Represents parsed XML data. It can either be an `XmlTag` with a name, a list of attributes, and a list of child `XmlValue`s, or an `XmlContent` with a string of content.

  2. instance Show XmlValue
    Defines how `XmlValue`s are converted to strings.
-}

import Lib (string, parseWhitespace, parseChar, parseMany, parseAnyChar, Parser(..), Alternative(..))
import Data.List ((\\))

-- `XmlValue` data type represents parsed XML data.
data XmlValue
  = XmlTag String [(String, String)] [XmlValue]
  | XmlContent String

-- `XmlValue` data type is an instance of the `Show` typeclass.
instance Show XmlValue where
  show (XmlTag name attrs children) =
    "XmlTag " ++ show name ++ " " ++ show attrs ++ " " ++ show children
  show (XmlContent content) = "XmlContent " ++ show content

-- `parseXmlValue` function takes a String and returns a `Maybe` `XmlValue`.
parseXmlValue :: Parser XmlValue
parseXmlValue = parseTag <|> parseContent

-- `parseTag` function is a `Parser` for `XmlValue` tags.
parseManyUntil :: Parser a -> Parser b -> Parser [a]
parseManyUntil parser end = Parser parse
  where
    parse str = case runParser end str of
      Just _ -> Just ([], str)
      Nothing -> case runParser parser str of
        Nothing -> Just ([], str)
        Just (result, rest) -> case runParser (parseManyUntil parser end) rest of
          Nothing -> Just ([result], rest)
          Just (results, finalRest) -> Just (result:results, finalRest)

-- `parseOpenTag` function is for parsing open tags.
parseOpenTag :: Parser (String, [(String, String)])
parseOpenTag = do
  _ <- parseWhitespace
  _ <- parseChar '<'
  _ <- parseWhitespace
  name <- parseTagName
  _ <- parseWhitespace
  attrs <- parseAttributes
  _ <- parseWhitespace
  _ <- parseChar '>'
  return (name, attrs)

-- `parseCloseTag` function is for parsing close tags.
parseCloseTag :: Parser ()
parseCloseTag = do
  _ <- parseChar '<'
  _ <- parseChar '/'
  _ <- parseTagName
  _ <- parseChar '>'
  return ()

-- `parseTag` function is for parsing tags.
parseTag :: Parser XmlValue
parseTag = do
  (name, attrs) <- parseOpenTag
  children <- parseManyUntil (parseTag <|> parseContent) (parseClosingTag name)
  _ <- parseWhitespace
  _ <- parseCloseTag
  return (XmlTag name attrs children)

-- `parseClosingTag` function is for parsing closing tags.
parseClosingTag :: String -> Parser String
parseClosingTag name = do
  _ <- parseChar '<'
  _ <- parseChar '/'
  _ <- parseWhitespace
  _ <- string name
  _ <- parseWhitespace
  _ <- parseChar '>'
  return name

-- `parseContent` function is for parsing content.
parseContent :: Parser XmlValue
parseContent = fmap XmlContent (
    parseMany (parseAnyChar (([' '..'~'] ++ ['\n']) \\ ['<']))
  )

-- `parseTagName` function is for parsing tag names.
parseTagName :: Parser String
parseTagName = parseMany (parseAnyChar ['a'..'z'])

-- `parseAttributes` function is for parsing attributes.
parseAttributes :: Parser [(String, String)]
parseAttributes = parseMany parseAttribute

-- `parseAttribute` function is for parsing attributes.
parseAttribute :: Parser (String, String)
parseAttribute = do
  _ <- parseWhitespace
  key <- parseTagName
  _ <- parseChar '='
  _ <- parseChar '"'
  value <- parseMany (parseAnyChar ([' '..'~'] \\ ['"']))
  _ <- parseChar '"'
  return (key, value)
