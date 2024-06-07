{-
-- EPITECH PROJECT, 2024
-- mypandoc
-- File description:
-- PrinterXml
-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use foldr" #-}

module PrinterXml (xmlFXml, mdFXml, xmlFJson) where
{-
Module : PrinterXml
Description : This Haskell module provides functions for converting XML and JSON data to strings in different formats.

This module exports the following functions:

1. xmlFXml :: XmlValue -> String
   Takes an XmlValue and returns a String. It is used to convert an XmlValue to a String.

2. mdFXml :: Int -> XmlValue -> String
   Takes an Int and an XmlValue and returns a String. It is used to convert an XmlValue to a Markdown formatted String.

3. xmlFJson :: Bool -> JsonValue -> String
   Takes a Bool and a JsonValue and returns a String. It is used to convert a JsonValue to a XML formatted String.

The module also includes several helper functions for printing XML and JSON data in different formats. These functions are not exported and are used internally by the above functions.

Note: This module uses the XmlValue and JsonValue data types from the ParsingXml and ParsingJson modules respectively. These data types represent parsed XML and JSON data.
-}

import ParsingXml (XmlValue(..))
import ParsingJson (JsonValue(..))
import Data.List (intercalate)
import Data.Maybe (fromMaybe)

-- `xmlFXml` function takes an XmlValue and returns a String.
-- It is used to convert an XmlValue to a String.
xmlFXml :: XmlValue -> String
xmlFXml (XmlTag "document" _ children) = printChildrenXml children
xmlFXml (XmlTag "paragraph" attrs children) =
  "<" ++ "paragraph" ++ printAttrsXml attrs ++ ">"
  ++ printWithNewLine children ++ "</" ++ "paragraph" ++ ">\n"
xmlFXml (XmlTag name attrs children) =
  "<" ++ name ++ printAttrsXml attrs ++ ">"
  ++ printChildrenXml children ++ "</" ++ name ++ ">"
xmlFXml (XmlContent content) = content

-- `printAttrsXml` function takes a list of attribute pairs and returns a String.
printAttrsXml :: [(String, String)] -> String
printAttrsXml [] = ""
printAttrsXml ((attrName, attrValue):attrs) =
  " " ++ attrName ++ "=\"" ++ attrValue ++ "\"" ++ printAttrsXml attrs

-- `printChildrenXml` function takes a list of XmlValues and returns a String.
printChildrenXml :: [XmlValue] -> String
printChildrenXml [] = ""
printChildrenXml (x:xs) = xmlFXml x ++ printChildrenXml xs

-- `printWithNewLine` function takes a list of XmlValues and returns a String.
printWithNewLine :: [XmlValue] -> String
printWithNewLine [] = ""
printWithNewLine [x] = xmlFXml x
printWithNewLine (x:xs) = xmlFXml x ++ "\n" ++ printWithNewLine xs

-- `mdFXml` function takes an Int and an XmlValue and returns a String.
mdFXml :: Int -> XmlValue -> String
mdFXml _ (XmlTag "document" _ children) = printChildrenMd 1 children
mdFXml _ (XmlTag "header" attrs children) = "---\n"
  ++ printHeaderMd attrs ++ printChildrenMd 1 children ++ "---\n\n"
mdFXml _ (XmlTag "author" _ children) = "author: "
  ++ printChildrenMd 1 children ++ "\n"
mdFXml _ (XmlTag "date" _ children) = "date: "
  ++ printChildrenMd 1 children ++ "\n"
mdFXml _ (XmlTag "body" _ children) =
  printChildrenMd 1 children ++ "\n"
mdFXml sectionNumber (XmlTag "section" attrs children) = "\n"
  ++ printSectionMd sectionNumber attrs
  ++ printChildrenMd (sectionNumber + 1) children
mdFXml _ (XmlTag "paragraph" _ children) =
  printChildrenMd 1 children ++ "\n"
mdFXml _ (XmlTag "bold" _ children) = "**"
  ++ printChildrenMd 1 children ++ "**"
mdFXml _ (XmlTag "italic" _ children) = "*"
  ++ printChildrenMd 1 children ++ "* "
mdFXml _ (XmlTag "code" _ children) = "`"
  ++ printChildrenMd 1 children ++ "` "
mdFXml _ (XmlTag "link" attrs children) = "["
  ++ printChildrenMd 1 children ++ "](" ++ printLinkMd attrs ++ ")"
mdFXml _ (XmlTag "image" attrs children) = " !["
  ++ printChildrenMd 1 children ++ "](" ++ printImageMd attrs ++ ")"
mdFXml _ (XmlTag "codeblock" _ children) = "```\n"
  ++ printChildrenMd 1 children ++ "\n\n```\n"
mdFXml _ (XmlTag "list" _ children) = printListMd children
mdFXml _ (XmlTag _ _ children) = printChildrenMd 1 children
mdFXml _ (XmlContent content) | null content = ""
                              | otherwise    = content

-- `printChildrenMd` function takes an Int and a list of XmlValues and returns a String.
printChildrenMd :: Int -> [XmlValue] -> String
printChildrenMd _ [] = ""
printChildrenMd sectionNumber [x] = mdFXml sectionNumber x
printChildrenMd sectionNumber (x:xs) = mdFXml sectionNumber x
  ++ (if any isParagraph xs then "\n" else "")
  ++ printChildrenMd sectionNumber xs
  where
    isParagraph (XmlTag "paragraph" _ _) = True
    isParagraph _ = False

-- `printListMd` function takes a list of XmlValues and returns a String.
printListMd :: [XmlValue] -> String
printListMd [] = ""
printListMd [x] = "-" ++ mdFXml 1 x
printListMd (x:xs) = "-" ++ mdFXml 1 x ++ "\n" ++ printListMd xs

-- `printHeaderMd` function takes a list of attribute pairs and returns a String.
printHeaderMd :: [(String, String)] -> String
printHeaderMd [] = ""
printHeaderMd (("title", value):xs) = "title: " ++ value
  ++ "\n" ++ printHeaderMd xs
printHeaderMd (_:xs) = printHeaderMd xs

-- `printSectionMd` function takes an Int and a list of attribute pairs and returns a String.
printSectionMd :: Int -> [(String, String)] -> String
printSectionMd _ [] = ""
printSectionMd num (("title", value):xs) 
  | null value = replicate num '#' ++ " \n" ++ printSectionMd num xs
  | otherwise = replicate num '#' ++ " " ++ value ++ "\n\n"
    ++ printSectionMd num xs
printSectionMd num (_:xs) = printSectionMd num xs

-- `printLinkMd` function takes a list of attribute pairs and returns a String.
printLinkMd :: [(String, String)] -> String
printLinkMd [] = ""
printLinkMd (("url", value):_) = value
printLinkMd (_:xs) = printLinkMd xs

-- `printImageMd` function takes a list of attribute pairs and returns a String.
printImageMd :: [(String, String)] -> String
printImageMd [] = ""
printImageMd (("url", value):_) = value
printImageMd (_:xs) = printImageMd xs

-- `xmlFJson` function takes a Bool and a JsonValue and returns a String.
xmlFJson :: Bool -> JsonValue -> String
xmlFJson _ JsonNull = "null"
xmlFJson _ (JsonBool True) = "true"
xmlFJson _ (JsonBool False) = "false"
xmlFJson _ (JsonNumber num) = show num
xmlFJson _ (JsonString str) = str
xmlFJson inParagraph (JsonArray arr)
  | not inParagraph && not (printParagraph arr) =
  "<paragraph>" ++ intercalate "\n" (map (xmlFJson True) arr) ++ "</paragraph>"
  | otherwise = intercalate "\n" (map (xmlFJson inParagraph) arr)
xmlFJson _ (JsonArrayS arr) = intercalate "\n" (map (xmlFJson False) arr)
xmlFJson inParagraph (JsonObject obj) =
  intercalate "\n" (map (printPair inParagraph) obj)

-- `printHeader` function takes a Bool and a list of attribute pairs and returns a String.
printHeader :: Bool -> [(String, JsonValue)] -> String
printHeader inParagraph pairs =
  "<header title=\"" ++ xmlFJson inParagraph title ++ "\">" ++
  (if author == JsonString "none" then "" else "<author>"
  ++ xmlFJson inParagraph author ++ "</author>") ++
  (if date == JsonString "none" then "" else "<date>"
  ++ xmlFJson inParagraph date ++ "</date>") ++ "</header>"
  where
    title = fromMaybe (JsonString "none") (lookup "title" pairs)
    author = fromMaybe (JsonString "none") (lookup "author" pairs)
    date = fromMaybe (JsonString "none") (lookup "date" pairs)

-- `printSection` function takes a Bool and a list of attribute pairs and returns a String.
printSection :: Bool -> [(String, JsonValue)] -> String
printSection inParagraph pairs =
  "<section" ++ titlePart ++ ">" ++
  xmlFJson inParagraph content ++ "</section>"
  where
    title = fromMaybe (JsonString "none") (lookup "title" pairs)
    content = fromMaybe (JsonString "none") (lookup "content" pairs)
    titlePart | title == JsonString "none" = ""
              | otherwise = " title=\"" ++ xmlFJson inParagraph title ++ "\""

-- `printPair` function takes a Bool and a pair of String and JsonValue and returns a String.
printPair :: Bool -> (String, JsonValue) -> String
printPair inParagraph ("url", value) =
  "url=\"" ++ xmlFJson inParagraph value ++ "\">"
printPair inParagraph ("header", JsonObject pairs) =
  printHeader inParagraph pairs
printPair inParagraph ("section", JsonObject pairs) =
  printSection inParagraph pairs
printPair inParagraph ("link", value) =
  "<link " ++ xmlFJson inParagraph value ++ "</link>"
printPair inParagraph ("image", value) =
  "<image " ++ xmlFJson inParagraph value ++ "</image>"
printPair inParagraph ("content", value) = xmlFJson inParagraph value
printPair inParagraph ("alt", value) = xmlFJson inParagraph value
printPair inParagraph (key, v) =
  "<" ++ key ++ ">" ++ xmlFJson inParagraph v ++ "</" ++ key ++ ">"

-- `printParagraph` function takes a list of JsonValues and returns a Bool.
printParagraph :: [JsonValue] -> Bool
printParagraph ((JsonString _):_) = False
printParagraph ((JsonObject _):_) = False
printParagraph _ = True
