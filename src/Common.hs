module Common where

import Data.Tuple
import Data.List
import Control.Applicative

lookupWith :: (a->Bool) -> [a] -> Maybe a
lookupWith _ [] = Nothing
lookupWith f (a:as) =
  if f a
    then Just a
    else lookupWith f as

justSnd = Just . snd
justFst = Just . fst

cutFst3 (_,x,y) = (x,y)
justCutFst3 = Just . cutFst3

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

fst4 (x,_,_,_) = x
snd4 (_,x,_,_) = x
thd4 (_,_,x,_) = x

format :: Int -> Int -> String
format n digit = (replicate (digit - len) '0') ++ num where
  len = length num
  num = show n

split :: Int -> [a] -> [[a]]
split _ [] = []
split n xs = (take n xs) : (split n $ drop n xs)

replace :: Eq a => a -> a -> [a] -> [a]
replace x y = map (\a -> if a==x then y else a)
