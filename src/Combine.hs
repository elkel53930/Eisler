module Combine(combine) where

import Types
import Parser
import Text.ParserCombinators.Parsec
import Control.Applicative
import Data.List
import qualified Data.Set as Set
import Common

combine :: [Net] -> [(Connectable,Connectable)] -> Result [Net]
combine nets cs = namingWire 1 <$> combineConnectables nets cs


combineConnectables :: [Net] -> [(Connectable,Connectable)] -> Result [Net]
combineConnectables _ [] = Right []
combineConnectables origin (c:cs) = do
  newNets <- combineConnectables origin cs
  combineConnectable newNets c

combineConnectable :: [Net] -> (Connectable,Connectable) -> Result [Net]
combineConnectable origin (c1,c2) = do
  newNet <- combineNet net1 net2
  Right (newNet : ((origin \\ [net1]) \\ [net2]))
  where
    net1 = searchConnectable c1 origin
    net2 = searchConnectable c2 origin

searchConnectable :: Connectable -> [Net] -> Net
searchConnectable (ConWire name) [] = Net (name, Set.empty)
searchConnectable w@(ConWire name) (s:ss) =
  if (fst $ getNet s) == name
    then s
    else searchConnectable w ss
searchConnectable i@(ConItfc _) [] = Net (newToken "", Set.singleton i)
searchConnectable i@(ConItfc _) (s:ss) =
  if Set.member i (snd $ getNet s)
    then s
    else searchConnectable i ss
searchConnectable p@(ConPort _ _ _ _ _ _) [] = Net (newToken "", Set.singleton p)
searchConnectable p@(ConPort _ _ _ _ _ _) (s:ss) =
  if Set.member p (snd $ getNet s)
    then s
    else searchConnectable p ss

combineNet :: Net -> Net -> Result Net
combineNet n1@(Net (wire1,set1)) n2@(Net (wire2,set2)) =
  case (wire1,wire2) of
    ((Token [] _),(Token [] _)) -> Right $ Net (newToken "", Set.union set1 set2)
    ((Token [] _),name) -> Right $ Net (name, Set.union set1 set2)
    (name,(Token [] _)) -> Right $ Net (name, Set.union set1 set2)
    otherwise -> if wire1 == wire2
                 then Right $ Net (wire1, Set.union set1 set2)
                 else Left $ (showPos wire1)
                         ++ "\n\tConnection between different wires. '"
                         ++ (getToken wire1) ++ "' and '"
                         ++ (getToken wire2) ++ "'."

namingWire :: Int -> [Net] -> [Net]
namingWire _ [] = []
namingWire n (Net(w,s):ns) =
  if w .== ""
    then Net(newToken ("$$$" ++ format n 5),s) : namingWire (n+1) ns
    else Net(w,s) : namingWire n ns
