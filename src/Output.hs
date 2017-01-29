module Output where

import Common
import Parser
import Semantics
import Data.List
import qualified Data.Time as Time

header filename = "!Eisler 0.1!\n*REMARK* "
               ++ filename
               ++ " -- "
               ++ "*REMARK*\n"

outputPart :: [(Port,Port)] -> String
outputPart ps = ("*PART*       ITEMS\n" ++)
  . outputPart_
  . map (\(c,_,r)->(c,r))
  . sortBy ordRef
  . nubBy (\x y -> fst3 x == fst3 y)
  $ expandTuple ps

outputPart_ :: [(CompName,Reference)] -> String
outputPart_ [] = ""
outputPart_ ((c,r):ts) = r ++ "    " ++ c ++ "\n" ++ (outputPart_ ts)
