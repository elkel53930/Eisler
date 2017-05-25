module Referencing(referencing) where

import Types
import Common
import Data.List

referencing :: [(Connectable,Connectable)] -> [(Connectable,Connectable)]
referencing ps = map (renameRef dic) ps
  where
    dic = termRef
        . groupBy eqRef  -- リファレンスプリフィックスでグルーピング
        . sortBy ordRef  -- リファレンスプリフィックスでソート
        . nubBy eqComp   -- 同じコンポーネントは削除
        $ expandTuple ps -- タプルを分解


-- Port > Wire > Itfc
-- リファレンスプリフィックスでOrdering
ordRef :: Connectable -> Connectable -> Ordering
ordRef x@(ConPort _ _ _ _ _ _) y@(ConPort _ _ _ _ _ _) = compare (getRef x) (getRef y)
ordRef (ConPort _ _ _ _ _ _) (ConWire _) = GT
ordRef (ConWire _) (ConPort _ _ _ _ _ _) = LT
ordRef (ConWire x) (ConWire y) = compare x y
ordRef (ConItfc _) (ConWire _) = LT
ordRef (ConWire _) (ConItfc _) = GT
ordRef (ConItfc _) (ConPort _ _ _ _ _ _) = LT
ordRef (ConPort _ _ _ _ _ _) (ConItfc _) = GT
ordRef (ConItfc x) (ConItfc y) = compare x y

-- リファレンスプリフィックスが等しいか
eqRef :: Connectable -> Connectable -> Bool
eqRef pl pr = (==EQ) $ ordRef pl pr

-- コンポーネントが等しいか
eqComp :: Connectable -> Connectable -> Bool
eqComp (ConPort x _ _ _ _ _) (ConPort y _ _ _ _ _) = x == y
eqComp (ConPort _ _ _ _ _ _) (ConWire _) = False
eqComp (ConWire _) (ConPort _ _ _ _ _ _) = False
eqComp (ConWire x) (ConWire y) = x == y
eqComp (ConItfc _) (ConWire _) = False
eqComp (ConWire _) (ConItfc _) = False
eqComp (ConItfc _) (ConPort _ _ _ _ _ _) = False
eqComp (ConPort _ _ _ _ _ _) (ConItfc _) = False
eqComp (ConItfc x) (ConItfc y) = x == y

getRef :: Connectable -> Reference
getRef (ConPort _ _ _ ref _ _) = ref


termRef :: [[Connectable]] -> [(CompIden,Reference)]
termRef [] = []
termRef (ps:pss) = termRef_ ps 1 ++ (termRef pss)

termRef_ :: [Connectable] -> Int -> [(CompIden,Reference)]
termRef_ [] _ = []
termRef_ ((ConPort c _ _ r _ _):ps) n = (c,r ++ (show n)) : (termRef_ ps $ n + 1)
termRef_ (_:ps) n = termRef_ ps n

expandTuple :: [(a,a)] -> [a]
expandTuple [] = []
expandTuple ((xl,xr):xs) = xl : xr : expandTuple xs

renameRef :: [(CompIden,Reference)] -> (Connectable,Connectable) -> (Connectable,Connectable)
renameRef dic (p1,p2) = (renameRefSingle dic p1, renameRefSingle dic p2)

renameRefSingle :: [(CompIden,Reference)] -> Connectable -> Connectable
renameRefSingle [] port = port
renameRefSingle ((c,r):ts) port@(ConPort pc pp po pr ppa pt) =
  if pc == c
    then ConPort pc pp po r ppa pt
    else renameRefSingle ts port
renameRefSingle _ x = id x  -- Wire and Itfc
