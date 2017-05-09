module Bom where

import Parser
import Semantics
import qualified Data.Set as Set
import Data.List

bomShow :: [Net] -> String
bomShow =
  (++) "No.,Ref.,Type\n"
  . concat
  . zipWith (++) index
  . sort
  . map showBom
  . groupBom
  . nub
  . sort
  . map (\(ConPort _ _ ref _ pt) -> (pt,ref))
  . filter isConPort
  . Set.elems
  . foldl Set.union Set.empty
  . map (\x -> snd . getNet $ x)

groupBom :: [(Maybe PartType, Reference)] -> [[(Maybe PartType, Reference)]]
groupBom = groupBy (\(pt1,_) (pt2,_) -> pt1 == pt2)

showBom :: [(Maybe PartType, Reference)] -> String
showBom all@((pt,_):xs) = (concat $ map (flip(++) " " . snd) all) ++ case pt of
  Nothing -> ",\n"
  Just t  -> "," ++ (getToken t) ++ "\n"

index :: [String]
index = map (flip(++) ".," . show) [1..]

isConPort :: Connectable -> Bool
isConPort x = case x of
  ConPort _ _ _ _ _ -> True
  otherwise -> False