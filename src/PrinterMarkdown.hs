{-
-- EPITECH PROJECT, 2024
-- mypandoc
-- File description:
-- PrinterMarkdown
-}

module PrinterMarkdown (mdFJson) where

import Lib (contains)
import ParsingJson (JsonValue(..))

-- `mdFJson` function takes a JsonValue and returns a String.
mdFJson :: JsonValue -> String
mdFJson (JsonString x) = x
mdFJson (JsonNumber x) = show x
mdFJson (JsonBool x) | x = "true"
               | otherwise = "false"
mdFJson (JsonArray xs) = concatMap mdFJson xs
mdFJson (JsonArrayS xs) = concatMap mdFJson xs
mdFJson (JsonObject [("bold", JsonString x)]) = "**" ++ x ++ "**"
mdFJson (JsonObject [("italic", JsonString x)]) = "*" ++ x ++ "*"
mdFJson (JsonObject [("code", JsonString x)]) = "`" ++ x ++ "`"
mdFJson (JsonObject x) = printObject x 0
mdFJson JsonNull = "null"

-- `printObject` function takes a list of attribute pairs and an Int and returns a String.
printObject :: [(String, JsonValue)] -> Int -> String
printObject [] _ = ""
printObject ((key, value):xs) n =
  printKeyValue key value n ++ printObject xs n

-- `printKeyValue` function takes a String, a JsonValue, and an Int and returns a String.
printKeyValue :: String -> JsonValue -> Int -> String
printKeyValue "header" (JsonObject header) _ =
  "---\n" ++ printPairs header ++ "---\n\n"
printKeyValue "body" (JsonArray body) n = printBody body n
printKeyValue "link" (JsonObject link) n = printLink link n
printKeyValue "image" (JsonObject image) n = printImage image n
printKeyValue "codeblock" (JsonObject block) _ =
  case lookup "codeblock" block of
    Just (JsonArray code) -> printCodeBlock code
    _ -> ""
printKeyValue _ (JsonObject obj) n = printObject obj n
printKeyValue _ value _ = mdFJson value

-- `printLink` function takes a list of attribute pairs and an Int and returns a String.
printLink :: [(String, JsonValue)] -> Int -> String
printLink link _ = case (lookup "url" link, lookup "content" link) of
  (Just (JsonString url), Just (JsonArray content)) ->
    "[" ++ concatMap mdFJson content ++ "](" ++ url ++ ")"
  _ -> ""

-- `printImage` function takes a list of attribute pairs and an Int and returns a String.
printImage :: [(String, JsonValue)] -> Int -> String
printImage image _ = case (lookup "url" image, lookup "alt" image) of
  (Just (JsonString url), Just (JsonArray alt)) ->
    "![" ++ concatMap mdFJson alt ++ "](" ++ url ++ ")"
  _ -> ""

-- `printPairs` function takes a list of attribute pairs and returns a String.
printPairs :: [(String, JsonValue)] -> String
printPairs [] = ""
printPairs ((key, value):xs) =
  key ++ ": " ++ mdFJson value ++ "\n" ++ printPairs xs

-- `printBody` function takes a list of JsonValues and an Int and returns a String.
printBody :: [JsonValue] -> Int -> String
printBody [] _ = ""
printBody ((JsonArray paragraph):xs) n =
  concatMap mdFJson paragraph ++ "\n\n" ++ printBody xs n
printBody ((JsonArrayS paragraph):xs) n =
  concatMap mdFJson paragraph ++ "\n\n" ++ printBody xs n
printBody ((JsonObject section):xs) n =
  printSection section (n+1) ++ printBody xs n
printBody (JsonNull:xs) n = printBody xs n
printBody ((JsonBool _):xs) n = printBody xs n
printBody ((JsonNumber _):xs) n = printBody xs n
printBody ((JsonString _):xs) n = printBody xs n

-- `printSection` function takes a list of attribute pairs and an Int and returns a String.
printSection :: [(String, JsonValue)] -> Int -> String
printSection [("section", JsonObject section)] n =
  printSectionContent section n
printSection (("codeblock", JsonArray code):xs) n =
  printCodeBlock code ++ printSection xs n ++ "\n"
printSection (("list", JsonArray list):xs) n =
  printList list ++ "\n" ++ printSection xs n
printSection (("link", JsonString url):xs) n =
  "[" ++ url ++ "](" ++ url ++ ")" ++ printSection xs n ++ "\n"
printSection (("image", JsonString url):xs) n =
  "![" ++ url ++ "](" ++ url ++ ")" ++ printSection xs n ++ "\n"
printSection _ _ = ""

-- `printSectionContent` function takes a list of attribute pairs and an Int and returns a String.
printSectionContent :: [(String, JsonValue)] -> Int -> String
printSectionContent section n =
  case (lookup "title" section, lookup "content" section) of
    (Just (JsonString title), Just (JsonArray content)) ->
      replicate n '#' ++ " " ++ title ++ "\n\n" ++ printBody content n
    _ -> ""

-- `printCodeBlock` function takes a list of JsonValues and returns a String.
printCodeBlock :: [JsonValue] -> String
printCodeBlock [] = ""
printCodeBlock ((JsonArray block):xs) =
  "```\n" ++ concatMap printMdWithoutCodeFormat block ++ "\n\n\n```"
  ++ printCodeBlock xs
printCodeBlock ((JsonObject block):xs) =
  case lookup "code" block of
    Just (JsonString code) -> "```\n" ++ code ++ "\n```"
                              ++ printCodeBlock xs
    _ -> printCodeBlock xs
printCodeBlock ((JsonArrayS block):xs) =
  "```\n" ++ concatMap printMdWithoutCodeFormat block ++ "\n\n\n```"
  ++ printCodeBlock xs
printCodeBlock (JsonNull:xs) = printCodeBlock xs
printCodeBlock ((JsonBool _):xs) = printCodeBlock xs
printCodeBlock ((JsonNumber _):xs) = printCodeBlock xs
printCodeBlock ((JsonString _):xs) = printCodeBlock xs

-- `printMdWithoutCodeFormat` function takes a JsonValue and returns a String.
printMdWithoutCodeFormat :: JsonValue -> String
printMdWithoutCodeFormat (JsonString x)
  | contains "bold" x = "**" ++ x ++ "**"
  | contains "italic" x = "*" ++ x ++ "*"
  | otherwise = x
printMdWithoutCodeFormat (JsonNumber x) = show x
printMdWithoutCodeFormat (JsonBool x)
  | x = "true"
  | otherwise = "false"
printMdWithoutCodeFormat (JsonArray xs) =
  concatMap printMdWithoutCodeFormat xs
printMdWithoutCodeFormat (JsonArrayS xs) =
  concatMap printMdWithoutCodeFormat xs
printMdWithoutCodeFormat (JsonObject x) = printObject x 0
printMdWithoutCodeFormat JsonNull = "null"

-- `printList` function takes a list of JsonValues and returns a String.
printList :: [JsonValue] -> String
printList [] = ""
printList ((JsonArray item):xs) =
  "\n-" ++ concatMap mdFJson item ++ "\n" ++ printList xs
printList ((JsonArrayS item):xs) =
  "\n-" ++ concatMap mdFJson item ++ "\n" ++ printList xs
printList ((JsonObject _):xs) = printList xs
printList (JsonNull:xs) = printList xs
printList ((JsonBool _):xs) = printList xs
printList ((JsonNumber _):xs) = printList xs
printList ((JsonString _):xs) = printList xs
