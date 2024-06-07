{-
-- EPITECH PROJECT, 2024
-- mypandoc
-- File description:
-- Main
-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# HLINT ignore "Use <&>" #-}

module Main (main) where
{-
  Module : Main
  Description : This Haskell module provides the main entry point for the application. It handles command line arguments, file reading, format detection, parsing, and output generation.

  This module exports the following functions:

  1. main :: IO ()
    The main entry point for the application.

  The module also includes several helper functions:

  1. getOpts :: Conf -> [String] -> Maybe Conf
    Processes command line arguments and updates the configuration accordingly.

  2. detectFormat :: String -> String
    Detects the format of a file based on its extension.

  3. exitWithError :: String -> IO ()
    Prints an error message and exits the program with a failure status.

  4. selectParser :: String -> Parser (Either JsonValue XmlValue)
    Selects the appropriate parser based on the input format.

  5. selectOtp :: String -> String -> (Either JsonValue XmlValue -> String)
    Selects the appropriate output generator based on the input and output formats.

  6. parseFile :: Conf -> Parser (Either JsonValue XmlValue) -> IO (Maybe (Either JsonValue XmlValue, String))
    Reads a file and parses its content.

  7. printOutput :: Conf -> (Either JsonValue XmlValue -> String) -> Either JsonValue XmlValue -> IO ()
    Generates output and either prints it to the console or writes it to a file.

  8. detectFormatFromContent :: String -> String
    Detects the format of a file based on its content.

  9. processMarkdown :: Conf -> String -> IO ()
    Processes a Markdown file.

  10. determineFormat :: Conf -> IO String
    Determines the format of a file.

  11. handleFormat :: Conf -> String -> String -> IO ()
    Handles a file based on its format.

  12. processFile :: Conf -> IO ()
    Processes a file.

  13. exceptionHandler :: IOException -> IO ()
    Handles exceptions.

  The module also defines several data types:

  1. Document
    Represents a document with a header and a body.

  2. Header
    Represents a header with a title, an optional author, and an optional date.

  3. Body
    Represents a body with content.

  4. Content
    Represents different types of content.

  5. Item
    Represents an item in a list.
-}

import Conf (Conf(ifile, oformat, ofile, iformat), defaultConf)
import Control.Exception (catch, IOException)
import Data.List (isSuffixOf, isPrefixOf)
import Lib (Parser(..))
import ParsingJson (JsonValue(..), parseJsonValue)
import ParsingXml (XmlValue(..), parseXmlValue)
import PrinterJson (jsonFJson, jsonFXml)
import PrinterMarkdown (mdFJson)
import PrinterXml (xmlFXml, mdFXml, xmlFJson)
import System.Environment (getArgs)
import System.Exit (exitWith, ExitCode(ExitFailure))

data Document = Document {
    header :: Header,
    body :: Body
} deriving (Show, Eq)

data Header = Header {
    title :: String,
    author :: Maybe String,
    date :: Maybe String
} deriving (Show, Eq)

data Body = Body {
    content :: [Content]
} deriving (Show, Eq)

data Content =
  Text String |
  Italic String |
  Bold String |
  Code String |
  Link String String |
  Image String String |
  Paragraph [Content] |
  Section (Maybe String) [Content] |
  List [Item]
  deriving (Show, Eq)

data Item = Item [Content]
  deriving (Show, Eq)

getOpts :: Conf -> [String] -> Maybe Conf
getOpts conf []
  | ifile conf == ""                                       = Nothing
  | oformat conf `notElem` ["xml", "json", "markdown"]     = Nothing
  | iformat conf `notElem` ["", "xml", "json", "markdown"] = Nothing
  | otherwise                                              = Just conf
getOpts conf ("-i":y:ys) = getOpts (conf {ifile = y}) ys
getOpts conf ("-f":y:ys) = getOpts (conf {oformat = y}) ys
getOpts conf ("-o":y:ys) = getOpts (conf {ofile = y}) ys
getOpts conf ("-e":y:ys) = getOpts (conf {iformat = y}) ys
getOpts _ _ = Nothing

detectFormat :: String -> String
detectFormat filename
  | ".json" `isSuffixOf` filename = "json"
  | ".xml" `isSuffixOf` filename = "xml"
  | ".md" `isSuffixOf` filename = "markdown"
  | otherwise = "unknown"

exitWithError :: String -> IO ()
exitWithError msg = putStrLn msg >> exitWith (ExitFailure 84)

selectParser :: String -> Parser (Either JsonValue XmlValue)
selectParser format = case format of
  "json" -> fmap Left parseJsonValue
  "xml" -> fmap Right parseXmlValue
  _ -> error "Error: format not supported"

selectOtp :: String -> String -> (Either JsonValue XmlValue -> String)
selectOtp "json" "json" = either jsonFJson (const "")
selectOtp "json" "xml" = either (xmlFJson False) (const "")
selectOtp "json" "markdown" = either mdFJson (const "")
selectOtp "xml" "json" = either (const "") jsonFXml
selectOtp "xml" "xml" = either (const "") xmlFXml
selectOtp "xml" "markdown" = either (const "") (mdFXml 1)
selectOtp _ _ = error "Error: format not supported"

parseFile :: Conf -> Parser (Either JsonValue XmlValue) -> IO (Maybe (Either JsonValue XmlValue, String))
parseFile conf parser = readFile (ifile conf) >>= return . runParser parser

printOutput :: Conf -> (Either JsonValue XmlValue -> String) -> Either JsonValue XmlValue -> IO ()
printOutput conf printer value
  | oformat conf == "xml" =
    case ofile conf of
      "" -> putStrLn ("<document>" ++ output ++ "</document>")
      _  -> writeFile (ofile conf) ("<document>" ++ output ++ "</document>")
  | otherwise =
    case ofile conf of
      "" -> putStrLn output
      _  -> writeFile (ofile conf) output
  where output = printer value

detectFormatFromContent :: String -> String
detectFormatFromContent fileContent
  | "{" `isPrefixOf` fileContent || "[" `isPrefixOf` fileContent = "json"
  | "<" `isPrefixOf` fileContent = "xml"
  | "---" `isPrefixOf` fileContent = "markdown"
  | otherwise = "unknown"

processMarkdown :: Conf -> String -> IO ()
processMarkdown conf fileContent = 
  case ofile conf of
    "" -> putStrLn fileContent
    _  -> writeFile (ofile conf) fileContent

determineFormat :: Conf -> IO String
determineFormat conf
  | format == "unknown" = fmap detectFormatFromContent (readFile (ifile conf))
  | otherwise = return format
  where format | iformat conf == "" = detectFormat (ifile conf)
               | otherwise = iformat conf

handleFormat :: Conf -> String -> String -> IO ()
handleFormat conf format fileContent = case format of
  "unknown" -> exitWithError "Error: unknown format"
  "markdown" -> processMarkdown conf fileContent
  _ -> do
    result <- parseFile conf (selectParser format)
    case result of
      Just (v, _) -> printOutput conf (selectOtp format (oformat conf)) v
      Nothing -> exitWithError "Error: invalid format"

processFile :: Conf -> IO ()
processFile conf = do
  fileContent <- readFile (ifile conf)
  format <- determineFormat conf
  handleFormat conf format fileContent

main :: IO ()
main = catch (
  do
    args <- getArgs
    case getOpts defaultConf args of
      Just conf -> processFile conf
      Nothing -> exitWithError "Error: arguments are invalid"
    -- readFile "examples/syntaxe.xml" >>= print . runParser (parseXmlValue)
  ) exceptionHandler

exceptionHandler :: IOException -> IO ()
exceptionHandler _ = exitWithError "Error: file not found"
