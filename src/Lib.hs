{-
-- EPITECH PROJECT, 2024
-- mypandoc
-- File description:
-- Lib
-}

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# HLINT ignore "Use $>" #-}
{-# HLINT ignore "Use foldr" #-}

module Lib (
  string,
  parseWhitespace,
  parseChar,
  parseMany,
  parseAnyChar,
  parseSome,
  contains,
  Parser(..),
  Functor(..),
  Applicative(..),
  Alternative(..),
) where

import Data.List (isInfixOf)
import Data.Char (toLower)
import Control.Applicative

-- `<*>` is an operator that applies a function within a context
-- to a value in the same context.
-- For example, if you have a function that adds 3 to a number,
-- and you want to apply it to 2, you would normally do `(+3) 2`.
-- But if both the function and the number are in a context (like `Just`),
-- you can use `<*>` : `Just (+3) <*> Just 2` gives `Just 5`.

-- `<*` and `*>` are used when you have two actions, but you don't care about the result of one of them.
-- For example, `Just 3 <* Just 2` gives `Just 3` because you ignore the result of `Just 2`.
-- And `Just 3 *> Just 2` gives `Just 2` because you ignore the result of `Just 3`.

-- `<|>` is used when you have two actions and you want to try the first one,
-- but if it fails, you want to try the second one.
-- For example, `Nothing <|> Just 3` gives `Just 3` because `Nothing` is considered a failure.

-- `\str` is a way to define a function without giving it a name.
-- It's useful when you need a function just once.
-- For example, `\x -> x + 2` is a function that takes a number and adds 2 to it.

-- `>>=` is used to chain actions where each action depends on the result of the previous one.
-- For example, `Just 3 >>= \x -> Just (x + 2)` gives `Just 5` because you take the 3,
-- you add 2 to it, and you put the result in a new box.

-- `(.)` is an operator that takes two functions and combines them into one that, when called,
-- calls the first function on the result of the second.
-- For example, `(not . even) 2` gives `True`, because `even 2` is `True`, and `not True` is `False`.
-- It's a very efficient way to combine functions in Haskell.

-- `..` is used to create a list of values in a certain range.
-- For example, `[1..5]` gives `[1, 2, 3, 4, 5]`.
-- You can also use `..` with characters.
-- For example, `['a'..'e']` gives `['a', 'b', 'c', 'd', 'e']`.
-- In the code, `[' '..'~']` creates a list of all printable ASCII characters.


-- `Parser` is a data structure that represents a parser.
-- `runParser` is a function that takes a string and returns a result and the remainder of the unparsed string.

data Parser a = Parser {
  runParser :: String -> Maybe (a , String )
}

-- `Functor` is an instance for `Parser` that allows applying a function to a result in a `Parser` context.

instance Functor Parser where
  fmap fct parser = Parser parse
    where
      parse str = case runParser parser str of
        Nothing -> Nothing
        Just (result, rest) -> Just (fct result, rest)

-- `Applicative` is an instance for `Parser` that allows applying a function in a `Parser` context to a value in the same context.

instance Applicative Parser where
  pure x = Parser (\str -> Just (x, str))
  (Parser p1) <*> (Parser p2) = Parser (\str -> do
    (f, rest1) <- p1 str
    (x, rest2) <- p2 rest1
    return (f x, rest2))

-- `Alternative` is an instance for `Parser` that allows choosing between two parsers.

instance Alternative Parser where
  empty = Parser (const Nothing)
  (Parser p1) <|> (Parser p2) = Parser (\str -> p1 str <|> p2 str)

-- `Monad` is an instance for `Parser` that allows chaining parsers where each parser depends on the result of the previous one.

instance Monad Parser where
  return = pure
  (Parser p) >>= f = Parser (\str -> do
    (result, rest) <- p str
    runParser (f result) rest)

-- parse (x:xs) | x == c = trace ("xs: " ++ show xs) (Just (c, xs))
parseChar :: Char -> Parser Char
parseChar c = Parser parse
  where
    parse (x:xs) | x == c = Just (c, xs)
    parse _ = Nothing

parseMany :: Parser a -> Parser [a]
parseMany parser = Parser parse
  where
    parse str = case runParser parser str of
      Nothing -> Just ([], str)
      Just (result, rest) -> case runParser (parseMany parser) rest of
        Nothing -> Just ([result], rest)
        Just (results, finalRest) -> Just (result:results, finalRest)

parseAnyChar :: String -> Parser Char
parseAnyChar [] = empty
parseAnyChar (c:cs) = parseChar c <|> parseAnyChar cs

parseSome :: Parser a -> Parser [a]
parseSome parser = fmap (:) parser <*> parseMany parser

contains :: String -> String -> Bool
contains = isInfixOf . map toLower

parseWhitespace :: Parser String
parseWhitespace = parseMany
  (parseChar ' ' <|> parseChar '\n' <|> parseChar '\r' <|> parseChar '\t')

string :: String -> Parser String
string = traverse parseChar
