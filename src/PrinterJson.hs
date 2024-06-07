{-
-- EPITECH PROJECT, 2024
-- mypandoc
-- File description:
-- PrinterJson
-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use foldr" #-}

module PrinterJson (jsonFJson, jsonFXml) where
{-
Module : PrinterJson
Description : This Haskell module provides functions for converting JSON and XML data to strings in different formats.

This module exports the following functions:

1. jsonFJson :: JsonValue -> String
  Takes a JsonValue and returns a String. It is used to convert a JsonValue to a String.

2. jsonFXml :: XmlValue -> String
  Takes an XmlValue and returns a String. It is used to convert an XmlValue to a JSON formatted String.

The module also includes several helper functions for printing JSON and XML data in different formats. These functions are not exported and are used internally by the above functions.

Note: This module uses the JsonValue and XmlValue data types from the ParsingJson and ParsingXml modules respectively. These data types represent parsed JSON and XML data.

Helper functions:

1. replaceNew :: Char -> String
  Takes a Char and returns a String. It is used to replace newline characters with their escape sequence.

2. printAttrs :: [(String, String)] -> String
  Takes a list of attribute pairs and returns a String. It is used to convert a list of attribute pairs to a JSON formatted String.

3. printChildren :: [XmlValue] -> String
  Takes a list of XmlValues and returns a String. It is used to convert a list of XmlValues to a JSON formatted String.
-}

import ParsingJson (JsonValue(..))
import ParsingXml (XmlValue(..))
import Data.List (intercalate)

-- `jsonFJson` function takes a JsonValue and returns a String.
jsonFJson :: JsonValue -> String
jsonFJson JsonNull = "null"
jsonFJson (JsonBool True) = "true"
jsonFJson (JsonBool False) = "false"
jsonFJson (JsonNumber num) = show num
jsonFJson (JsonString str) = "\"" ++ str ++ "\""
jsonFJson (JsonArray arr) = "[" ++ intercalate "," (map jsonFJson arr) ++ "]"
jsonFJson (JsonArrayS arr) = "[" ++ intercalate "," (map jsonFJson arr) ++ "]"
jsonFJson (JsonObject obj) = "{" ++ intercalate "," (map printPair obj) ++ "}"
  where
    printPair (key, value) = "\"" ++ key ++ "\": " ++ jsonFJson value

-- `jsonFXml` function takes an XmlValue and returns a String.
jsonFXml :: XmlValue -> String
jsonFXml (XmlTag "document" _ children) = "{" ++ printChildren children ++ "}"
jsonFXml (XmlTag "paragraph" _ children) = "[" ++ printChildren children ++ "]"
jsonFXml (XmlTag "italic" _ children) = "{\"italic\": " ++
  printChildren children ++ "}"
jsonFXml (XmlTag "bold" _ children) = "{\"bold\": " ++
  printChildren children ++ "}"
jsonFXml (XmlTag "code" _ children) = "{\"code\": " ++
  printChildren children ++ "}"
jsonFXml (XmlTag "section" attrs children) = "{\"section\": {"
  ++ printAttrs attrs ++ ",\"content\": [" ++ printChildren children ++ "]}}"
jsonFXml (XmlTag "link" attrs children) = "{\"link\": {"
  ++ printAttrs attrs ++ ",\"content\": [" ++ printChildren children ++ "]}}"
jsonFXml (XmlTag "image" attrs children) = "{\"image\": {"
  ++ printAttrs attrs ++ ",\"alt\": [" ++ printChildren children ++ "]}}"
jsonFXml (XmlTag "body" attrs children) = "\"body\": [" ++
  printAttrs attrs ++ printChildren children ++ "]"
jsonFXml (XmlTag "header" attrs children) = "\"header\": {"
  ++ printAttrs attrs ++ "," ++ printChildren children ++ "}"
jsonFXml (XmlTag "codeblock" attrs children) = "{\"codeblock\": ["
  ++ printAttrs attrs ++ printChildren children ++ "]}"
jsonFXml (XmlTag "list" attrs children) = "{\"list\": ["
  ++ printAttrs attrs ++ printChildren children ++ "]}"
jsonFXml (XmlTag name attrs children) =
  "\"" ++ name ++ "\": "
  ++ printAttrs attrs ++ printChildren children
jsonFXml (XmlContent content) = "\"" ++ concatMap replaceNew content ++ "\""

-- `replaceNew` function takes a Char and returns a String.
replaceNew :: Char -> String
replaceNew '\n' = "\\n"
replaceNew c = [c]

-- `printAttrs` function takes a list of attribute pairs and returns a String.
printAttrs :: [(String, String)] -> String
printAttrs [] = ""
printAttrs ((attrName, attrValue):attrs) =
  "\"" ++ attrName ++ "\": \"" ++ attrValue
  ++ "\"" ++ (if not (null attrs) then "," else "") ++ printAttrs attrs

-- `printChildren` function takes a list of XmlValues and returns a String.
printChildren :: [XmlValue] -> String
printChildren [] = ""
printChildren [x] = jsonFXml x
printChildren (x:xs) = jsonFXml x ++ "," ++ printChildren xs
