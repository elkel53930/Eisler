module ConInfo(output) where

import Types
import Common
import Data.Set
import qualified Data.List as L

data PortInfo = PortInfo Reference CompName PartName PortNum PortName WireName deriving (Show,Eq,Ord)

output :: [Net] -> String
output net =
  "# Pin connection information\n\n" ++ outputNet pss
  where pss = L.groupBy ConInfo.eqRef . L.sort $ changeStyles [] net

outputNet :: [[PortInfo]] -> String
outputNet [] = ""
outputNet (ps:pss) =
  concat[ "## ", ref, " (", compName, ",", partName, ")\n"
        , "|Pin no.|Pin name|Net name|\n|-:|--|--|\n"
        , outputPort ps
        , "\n"
        , outputNet pss
        , "\n\n"] where
          (PortInfo ref compName partName _ _ _) = head ps

outputPort :: [PortInfo] -> String
outputPort [] = ""
outputPort (p:ps) =
  concat[ "|", show portNum
        , "|", portName
        , "|", wireName
        , "|\n", outputPort ps] where
          (PortInfo _ _ _ portNum portName wireName) = p

eqRef :: PortInfo -> PortInfo -> Bool
eqRef (PortInfo a _ _ _ _ _) (PortInfo b _ _ _ _ _) = a == b

changeStyles :: [PortInfo] -> [Net] -> [PortInfo]
changeStyles origin [] = origin
changeStyles origin (n:ns) = changeStyles (changeStyle origin n) ns

changeStyle :: [PortInfo] -> Net -> [PortInfo]
changeStyle origin net = origin ++ (netToPortInfo wire . elems . snd $ getNet net) where
  wire = getToken . fst $ getNet net

netToPortInfo :: WireName -> [Connectable] -> [PortInfo]
netToPortInfo _ [] = []
netToPortInfo wire ((ConItfc _):cs) = netToPortInfo wire cs
netToPortInfo wire (c:cs) = (PortInfo ref comp part port portname wire) : netToPortInfo wire cs where
  comp = getToken compIden
  port = getToken portIntLit
  part = getToken partIden
  portname = getToken portIden
  ConPort compIden portIntLit portIden ref partIden _ = c
