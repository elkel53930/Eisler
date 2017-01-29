module Common where

import Data.Tuple
import Data.List

lookupWith :: (c -> a ->Bool) -> c -> [(a,b)] -> Maybe (a,b)
lookupWith _ _ [] = Nothing
lookupWith f a ((x,y):ts) =
  if f a x
    then Just (x,y)
    else lookupWith f a ts


lookupWith_ :: (c -> a ->Bool) -> c -> [(b,a)] -> Maybe (b,a)
lookupWith_ _ _ [] = Nothing
lookupWith_ f a ((x,y):ts) =
  if f a y
    then Just (x,y)
    else lookupWith_ f a ts

justSnd = Just . snd
justFst = Just .fst

eqFst :: Eq a => a -> (a,b) -> Bool
eqFst x (y,_) = x == y

eqSnd :: Eq a => a -> (b,a) -> Bool
eqSnd x (_,y) = x == y

deleteBy' :: (a->b->Bool) -> b -> [a] -> [a]
deleteBy' f b (a:as) =
  if f a b
    then a : deleteBy' f b as
    else deleteBy' f b as

elemBy :: (a->b->Bool) -> a -> [b] -> Bool
elemBy f x xs = or $ map (f x) xs

pickupBy :: Eq a => (a->a->Bool) -> a -> [a] -> Maybe (a,[a])
pickupBy f x xs =
  if elemBy f x xs
    then Just (x, (xs\\[x]))
    else Nothing

pickup :: Eq a => a -> [a] -> Maybe (a,[a])
pickup = pickupBy (==)

fst3 (x,_,_) = x
snd3 (_,x,_) = x
thd3 (_,_,x) = x
