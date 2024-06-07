{-
-- EPITECH PROJECT, 2024
-- mypandoc
-- File description:
-- Conf
-}

module Conf (Conf(ifile, oformat, ofile, iformat), defaultConf) where

data Conf = Conf {
  ifile   :: String,
  oformat :: String,
  ofile   :: String,
  iformat :: String
} deriving (Show)

defaultConf :: Conf
defaultConf = Conf {
  ifile   = "",
  oformat = "",
  ofile   = "",
  iformat = ""
}
