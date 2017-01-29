module Output where

import Common
import Parser
import Semantics
import Data.List
import qualified Data.Time as Time
import qualified Data.Set as Set

header = "!Eisler 0.1!\n*REMARK* "
       ++ "untitled"
       ++ " -- \n"
       ++ "*REMARK*\n"

outputPart :: [(Port,Port)] -> String
outputPart ps = ("*PART*       ITEMS\n" ++)
  . outputPart_
  . map (\(c,_,r,pa)->(c,r,pa))
  . sortBy ordRef
  . nubBy (\x y -> fst4 x == fst4 y)
  $ expandTuple ps

outputPart_ :: [(CompName,Reference,PartName)] -> String
outputPart_ [] = ""
outputPart_ ((c,r,pa):ts) = r ++ "    " ++ c ++ "@" ++ pa ++ "\n" ++ (outputPart_ ts)

outputSignal :: [Port] -> String
outputSignal [] = ""
outputSignal ((_,p,r,_):ps) = r ++ "." ++ (show p) ++ " " ++ (outputSignal ps)

outputSignals :: [Set.Set Port] -> Int -> String
outputSignals [] _ = ""
outputSignals (set:sets) n =
  "*SIGNAL* $$$" ++ (replicate (5 - len) '0') ++ num ++ "\n" ++
    (outputSignal $ Set.elems set) ++ "\n" ++
    outputSignals sets (n+1)
  where
    len = length num
    num = show n

outputNet :: [Set.Set Port] -> String
outputNet set = "*NET*\n" ++ outputSignals set 1
