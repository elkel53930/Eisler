import Data.List

type T = (Int,(Int,String))

test = [ (1,(1,"1")), (2,(2,"2")), (3,(3,"3")), (4,(4,"4")), (5,(5,"5"))] :: [T]


justFst (x,_) = Just x

--fn = lookup 1 test >>= (\(x,_) -> Just x) >>= (\x -> Just $ x*2)

{-
fn = do
  a <- lookup 1 test
  b <- justFst a
  Just $ (*2) b
-}

fn = do
  b <- lookup 1 test >>= justFst
  Just $ (*2) b

f2 :: Int -> Maybe Int
f2 x = Just x
f3 x = Nothing
f4 :: Int -> Int
f4 x = x

f5 = do
  x1 <- f2 1
  x2 <- f3 x1
  let x3 = f4 x2
  Just x3

f6 :: [Int] -> Int
f6 [] = 0
f6 [n] = n+1
f6 (n:ns) = n + f6 ns

alphabet x =
  lookup x $ zip [1..] ['A'..'Z']

toSmall c =
  lookup c $ zip ['A'..'Z'] ['a'..]

alphabetSmall x = do
  c <- alphabet x
  toSmall c


main = do
  sequence_ [print 123,putStrLn "hello"]

data Parent = Child1 Int | Child2 String deriving Show

pickup :: [Parent] -> [Int]
pickup parents =
  map (\(Child1 x) -> x) $ filter (\x -> case x of
    Child1 _ -> True
    otherwise -> False) parents
