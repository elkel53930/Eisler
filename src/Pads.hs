module Pads(output) where

import Types
import Common
import Data.List
import qualified Data.Time as Time
import qualified Data.Set as Set

header _ = "!Eisler 0.1!\n*REMARK* untitled -- \n*REMARK*\n"
footer _ = "*END*     OF ASCII OUTPUT FILE"

output :: [Net] -> String
output ns = concat $ fmap ($ns) [header,parts,signals,footer]

parts :: [Net] -> String
parts = ("*PART*\n"++)
      . concatMap part
      . nub
      . sort
      . map (\(ConPort c _ _ r p _) -> (r,getToken c,getToken p))
      . filter isConPort
      . Set.elems
      . foldl Set.union Set.empty
      . map (\x -> snd . getNet $ x)


part :: (Reference, CompName, PartName) -> String
part (r,c,p) = concat [r, "    ", c, "@", p, "\n"]

signals :: [Net] -> String
signals = ("*NET*\n"++) . concatMap signal

signal :: Net -> String
signal net =
  concat [ "*SIGNAL* "
         , getToken w
         , "\n"
         , ( concatMap (\xs -> concat xs ++ "\n")
           . split 5
           $ map Pads.port ns )]
  where
    ns = Set.toList set
    (w,set) = getNet net

port :: Connectable -> String
port (ConPort _ n _ r _ _) =
  concat [r, ".", show $ getToken n, " "]
port _ = ""


isConPort :: Connectable -> Bool
isConPort x = case x of
  ConPort _ _ _ _ _ _ -> True
  otherwise -> False
