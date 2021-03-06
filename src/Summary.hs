{-# LANGUAGE TemplateHaskell #-}

module Summary(output) where

import Development.GitRev
import Types
import Common
import qualified Data.Set as Set
import Data.List
import Data.Time
import Control.Applicative
import Network.HTTP.Base (urlEncode)


type Markdown = String

output :: FilePath -> FilePath -> [Net] -> IO()
output eis fp net = do
  time <- getZonedTime
  let timeInfo = concat [ show $ zonedTimeToLocalTime time
                        , " "
                        , show . minutesToTimeZone . timeZoneMinutes $ zonedTimeZone time
                        ]
  writeFile (changeExt "md" fp) $ concat
    [ "# ["
    , eis
    , "] Translation Summary\n\n* Time-stamp : "
    , timeInfo
    , "\n*  "
    , gitinfo
    , "\n\n"
    , bom net
    , partsInformation net
    , netsInformation net
    ]

gitinfo :: String
gitinfo = concat [ "Git revision : ", $(gitBranch), "@", $(gitHash)
       , " (", $(gitCommitDate), ")\n" ]

{-
  BOMを出力
  リファレンス、型番、数量、Digikeyで検索、RSで検索、Chip1Stopで検索
-}
bom :: [Net] -> Markdown
bom =
  (++) "## BOM\n\n|Ref.|Type|Qty|Search|\n|--|--|--:|--|\n"
  . concat
  . sort
  . map showBom
  . groupBy (\(pt1,_) (pt2,_) -> pt1 == pt2)
  . nub
  . sortBy (\x y -> compare (fst x) (fst y))
  . map (\(ConPort _ _ _ ref _ pt) -> (pt,ref))
  . filter isConPort
  . Set.elems
  . foldl Set.union Set.empty
  . map (\x -> snd . getNet $ x)

showBom :: [(Maybe PartType, Reference)] -> String
showBom all@((pt,_):xs) = concat
  [ "|", concat
       . map (concat . intersperse " ")
       . intersperse ["<br>"]
       . split 10
       . sort
       $ map snd all -- References
  , "|", mb $ getToken <$> pt                   -- Part Type
  , "|", show $ length all                      -- Qty.
  , "|", mb $ rsSearch <$> pt                   -- RS
  , ", ", mb $ googleSearch <$> pt              -- Google
  , ", ", mb $ digiKeySearch <$> pt              -- DigiKey
  , ", ", mb $ chip1Search <$> pt                -- Chip1Stop
  , "|\n"
  ] where mb x = case x of Nothing -> ""; Just t -> t;

makeSearch :: String -> String -> PartType -> Markdown
makeSearch title url part = concat
  [ "["
  , title
  , "]("
  , url
  , urlEncode $ getToken part
  , ")"
  ]

digiKeySearch = makeSearch "Digikey" "https://www.digikey.jp/products/ja?keywords="
rsSearch = makeSearch "RS" "http://jp.rs-online.com/web/c/?sra=oss&r=t&searchTerm="
chip1Search = makeSearch "Chip1Stop" "http://www.chip1stop.com/search.do?classCd=&did=&keyword="
googleSearch = makeSearch "Google" "https://www.google.co.jp/search?q="

{-
  パーツインフォメーション
  eisファイル内のどのpartのリファレンスがいくつで、その型番がなんであるかの一覧
-}

partsInformation :: [Net] -> Markdown
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

showPart :: (Reference,CompIden,Maybe PartType) -> Markdown
showPart (r,c,pt) = concat
  [ "|", r
  , "|", getToken c
  , "|", case pt of
    Nothing -> ""
    Just t  -> getToken t
  , "|\n"
  ]

{-
  ネットインフォメーション
  ネット名と、そのネットに繋がっているピンの一覧
-}

netsInformation :: [Net] -> Markdown
netsInformation =
  (++) "## Nets information\n\n|Net name|Pins|\n|--|--|\n"
  . concat
  . sort
  . map showNet

showNet :: Net -> Markdown
showNet net = concat
  [ "|", getToken wire
  , "|", showConnectables set
  , "|\n"
  ] where
    (wire,set) = getNet net

showConnectables :: Set.Set Connectable -> Markdown
showConnectables =
  concat
  . intersperse "<br>"
  . filter (/="")
  . sort
  . map showConnectable
  . Set.elems

showConnectable :: Connectable -> Markdown
showConnectable (ConPort c _ po _ _ _) = concat
  [ getToken c
  , " . "
  , getToken po
  ]
showConnectable _ = ""
