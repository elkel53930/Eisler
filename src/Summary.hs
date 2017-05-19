module Summary where

import Parser
import Semantics
import Common
import qualified Data.Set as Set
import Data.List
import Data.Time

output :: FilePath -> [Net] -> IO()
output fp net = do
  time <- getZonedTime
  let timeInfo = show time
  writeFile (changeExt "md" fp) $ concat
    [ "# ["
    , fp
    , "] Translation Summary\n\n"
    , timeInfo
    , "\n\n"
    , partsInformation net
    , netsInformation net
    ]

partsInformation :: [Net] -> String
partsInformation =
  (++) "## Parts information\n\n|Ref.|Decl.|Type|\n|--|--|--|\n"
  . concatMap showPart
  . nub
  . sort
  . map (\(ConPort comp _ _ ref _ pt) -> (ref,comp,pt))
  . filter isConPort
  . Set.elems
  . foldl Set.union Set.empty
  . map (\x -> snd . getNet $ x)

isConPort :: Connectable -> Bool
isConPort x = case x of
  ConPort _ _ _ _ _ _ -> True
  otherwise -> False

showPart :: (Reference,CompIden,Maybe PartType) -> String
showPart (r,c,pt) = concat
  [ "|", r
  , "|", atmark $ getToken c
  , "|", case pt of
    Nothing -> ""
    Just t  -> getToken t
  , "|\n"
  ]

netsInformation :: [Net] -> String
netsInformation =
  (++) "## Nets information\n\n|Net name|Pins|\n|--|--|\n"
  . concat
  . sort
  . map showNet

showNet :: Net -> String
showNet net = concat
  [ "|", getToken wire
  , "|", showConnectables set
  , "|\n"
  ] where
    (wire,set) = getNet net

showConnectables :: Set.Set Connectable -> String
showConnectables =
  concat
  . intersperse "<br>"
  . filter (/="")
  . sort
  . map showConnectable
  . Set.elems

showConnectable :: Connectable -> String
showConnectable (ConPort c _ po _ _ _) = concat
  [ atmark $ getToken c
  , " . "
  , getToken po
  ]
showConnectable _ = ""
{-
showConnectable (ConWire w) =  "Wire : " ++ (atmark $ getToken w)
showConnectable (ConItfc i) =  "Interface : " ++ (atmark $ getToken i)
-}
atmark :: String -> String
atmark = replace '-' '@'
