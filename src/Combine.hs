module Combine where

import qualified Data.Set as S
import qualified Data.List as L

src = [(1,2),(3,4),(4,5),(6,7)]

adds :: Ord a => [S.Set a] -> [(a,a)] -> [S.Set a]
adds _ [] = []
adds origin (a:as) = add (adds origin as) a

add :: Ord a => [S.Set a] -> (a,a) -> [S.Set a]
add origin (a1,a2) =
  (S.union (S.insert a1 set1) (S.insert a2 set2)) : ((origin L.\\ [set1]) L.\\ [set2])
  where
    set1 = search a1 origin
    set2 = search a2 origin


search :: Ord a => a -> [S.Set a] -> S.Set a
search _ [] = S.empty
search key (s:ss) =
  if S.member key s
    then s
    else search key ss
