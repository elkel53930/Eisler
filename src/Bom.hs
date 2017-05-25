module Bom(output) where

import Types
import qualified Data.Set as Set
import Data.List

output :: [Net] -> String
output =
  (++) "No.,Ref.,Type,Qty.\n"
  . concat
  . zipWith (++) index
  . sort
  . map showBom
  . groupBom
  . nub
  . sort
  . map (\(ConPort _ _ _ ref _ pt) -> (pt,ref))
  . filter isConPort
  . Set.elems
  . foldl Set.union Set.empty
  . map (\x -> snd . getNet $ x)

groupBom :: [(Maybe PartType, Reference)] -> [[(Maybe PartType, Reference)]]
groupBom = groupBy (\(pt1,_) (pt2,_) -> pt1 == pt2)

showBom :: [(Maybe PartType, Reference)] -> String
showBom all@((pt,_):xs) = (concat $ map (flip(++) " " . snd) all) ++ case pt of
  Nothing -> concat [",,", (show $ length all), "\n"]
  Just t  -> concat [",", (getToken t), ",", show $ length all, "\n"]

index :: [String]
index = map (flip(++) ".," . show) [1..]

isConPort :: Connectable -> Bool
isConPort x = case x of
  ConPort _ _ _ _ _ _ -> True
  otherwise -> False
