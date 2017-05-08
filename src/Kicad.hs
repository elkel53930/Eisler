module Kicad where

import Parser
import Common
import Semantics
import Data.Set
import qualified Data.List as L

data PortInfo = PortInfo Reference CompName PartName PortNum WireName deriving (Show,Eq,Ord)

output :: [Net] -> String
output net =
  "# EESchema Netlist\n(\n" ++
  outputNet pss 1 ++
  ")\n*\n" where
    pss = L.groupBy Kicad.eqRef . L.sort $ changeStyles [] net

outputNet :: [[PortInfo]] -> Int -> String
outputNet [] _ = ""
outputNet (ps:pss) n =
  "  ( " ++ format n 4 ++ " $noname " ++ ref ++ " " ++ partName ++ "{Lib=" ++ compName ++ "}\n" ++
  outputPort ps ++
  "  )\n" ++ outputNet pss (n+1) where
    (PortInfo ref compName partName _ _) = head ps

outputPort :: [PortInfo] -> String
outputPort [] = ""
outputPort (p:ps) =
  "    ( " ++ show portNum ++ " " ++ wireName ++ ")\n" ++ outputPort ps where
    (PortInfo _ _ _ portNum wireName) = p

eqRef :: PortInfo -> PortInfo -> Bool
eqRef (PortInfo a _ _ _ _) (PortInfo b _ _ _ _) = a == b

changeStyles :: [PortInfo] -> [Net] -> [PortInfo]
changeStyles origin [] = origin
changeStyles origin (n:ns) = changeStyles (changeStyle origin n) ns

changeStyle :: [PortInfo] -> Net -> [PortInfo]
changeStyle origin net = origin ++ (netToPortInfo wire . elems . snd $ getNet net) where
  wire = getToken . fst $ getNet net

netToPortInfo :: WireName -> [Connectable] -> [PortInfo]
netToPortInfo _ [] = []
netToPortInfo wire ((ConItfc _):cs) = netToPortInfo wire cs
netToPortInfo wire (c:cs) = (PortInfo ref comp part port wire) : netToPortInfo wire cs where
  comp = getToken compIden
  port = getToken portIntLit
  part = getToken partIden
  ConPort compIden portIntLit ref partIden _ = c
