module Main where
import Parser.Parser
import Semantic.Semantic
import Text.ParserCombinators.Parsec
import System.IO
import System.Directory

main = do
  src <- getContents
  let parsed = parse eislerFile "input" src
  print parsed
