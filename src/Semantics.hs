module Semantics where

import Parser
import Text.ParserCombinators.Parsec
import Control.Applicative
import Data.List
import qualified Data.Set as Set
import Common

--type Module = (ModuleIden, [ Net ])
newtype Net = Net { getNet :: (WireIden, Set.Set Connectable)} deriving (Show,Eq)
data Connectable = ConWire WireIden
                 | ConItfc ItfcIden
                 | ConPort CompIden PortIntLit Reference PartIden deriving (Show,Eq,Ord)
type ErrorMsg = String
type Result = Either ErrorMsg

expandModules :: [ModuleElement] -> String -> [ModuleElement]
expandModules [] _ = []
expandModules (m:ms) suf =
  case m of
    DecPart (comp,part) -> DecPart (addSuffix comp suf,part) : expandModules ms suf
    DecWire w           -> (DecWire $ addSuffix w suf) : expandModules ms suf
    ConExpr c           -> (ConExpr $ mangleCe suf c) : expandModules ms suf

mangleCe :: String -> ConnectExpression -> ConnectExpression
mangleCe suf (cl,bs,cr) = (mangleC suf cl, mangleBcs suf bs, mangleC suf cr)

mangleBcs :: String -> [BCnct] -> [BCnct]
mangleBcs _ [] = []
mangleBcs suf (b:bs) =
  case b of
    BPin pl c pr -> BPin pl (addSuffix c suf) pr : mangleBcs suf bs
    BWire w      -> BWire (addSuffix w suf)      : mangleBcs suf bs

mangleC :: String -> Cnct -> Cnct
mangleC suf c =
  case c of
    Pin c p -> Pin (addSuffix c suf) p
    Wire w  -> Wire $ addSuffix w suf

addSuffix :: Identify -> String -> Identify
addSuffix (Token name pos) suf = Token (name ++ "@" ++ suf) pos

moduleNet :: [SourceElement] -> ModuleName -> Result [(Connectable,Connectable)]
moduleNet parsed name = do
  srcElems <- checkOverlap parsed name
  (_,(_,modElems)) <- searchMod (snd $ divideSrc srcElems) name
  let cncts = expansionaCnct . pickupConExpr $ modElems
  convertToPort modElems (fst $ divideSrc srcElems) cncts

getRef :: Connectable -> Reference
getRef (ConPort _ _ ref _) = ref

checkOverlap :: [SourceElement] -> ModuleName -> Result [SourceElement]
checkOverlap srcElems mname = do
  case (\input -> input \\ nub input) $ getIdens srcElems mname of
    [] -> Right srcElems
    overlaps -> Left $ getOverlapsError overlaps

-- [SourceElement]内の識別子の一覧を返す。
getIdens :: [SourceElement] -> ModuleName -> [Token String]
getIdens [] _ = []
getIdens (t:ts) tagMod = name ++ getIdens ts tagMod where
  name =
    case t of
      DefPart (pname, (_, _)) -> [pname]
      DefMod (mname, (_, m)) ->
        if mname .== tagMod
          then (mname) : getIdensMod m
          else []

-- [ModuleElement]内の識別子の一覧を返す
getIdensMod :: [ModuleElement] -> [Token String]
getIdensMod [] = []
getIdensMod (t:ts) =
  case t of
    DecPart (cname, _) -> cname : (getIdensMod ts)
    DecWire (wname   ) -> wname : (getIdensMod ts)
    DecMod  (mname, _) -> mname : (getIdensMod ts)
    DecItfc (iname   ) -> iname : (getIdensMod ts)
    ConExpr _ -> getIdensMod ts

getOverlapsError :: [Token String] -> String
getOverlapsError ts =
  concat $ map (\token -> (showPos token)
                   ++ "\n\tMultiple Declarations of '"
                   ++ getToken token ++ "'\n" ) ts

-- -- -- -- divide -- -- -- --

divideSrc :: [SourceElement] -> ([DefinePart],[DefineModule])
divideSrc [] = ([],[])
divideSrc ((DefPart p):ts) = (p:ps,ms) where
  (ps,ms) = divideSrc ts
divideSrc ((DefMod m):ts) = (ps,m:ms) where
  (ps,ms) = divideSrc ts

pickupDecPart :: [ModuleElement] -> [DeclarePart]
pickupDecPart [] = []
pickupDecPart ((DecPart p):ts) = p : pickupDecPart ts
pickupDecPart (_:ts) = pickupDecPart ts

pickupConExpr :: [ModuleElement] -> [ConnectExpression]
pickupConExpr [] = []
pickupConExpr ((ConExpr c):ts) = c : pickupConExpr ts
pickupConExpr (_:ts) = pickupConExpr ts

pickupDecWire :: [ModuleElement] -> [WireIden]
pickupDecWire [] = []
pickupDecWire ((DecWire w):ts) = w : pickupDecWire ts
pickupDecWire (_:ts) = pickupDecWire ts
{-
pickupDecMod :: [ModuleElement] -> [DeclareModule]
pickupDecMod [] = []
pickupDecMod ((DecMod m):ts) = m : pickupDecMod ts
pickupDecMod (_:ts) = pickupDecMod ts
-}

pickupDecItfc :: [ModuleElement] -> [ItfcIden]
pickupDecItfc [] = []
pickupDecItfc ((DecItfc i):ts) = i : pickupDecItfc ts
pickupDecItfc (_:ts) = pickupDecItfc ts

-- -- -- --  search -- -- -- --

searchMod :: [DefineModule] -> String -> Result DefineModule
searchMod defMods name =
  case lookupWith (\(x,_) -> x.==name) defMods of
    Just defmod -> Right defmod
    Nothing -> Left $ "Module '" ++ name ++ "' is not defined."

-- -- -- -- expansion -- -- -- --

expansionaCnct :: [ConnectExpression] -> [(Cnct,Cnct)]
expansionaCnct [] = []
expansionaCnct (c:cs) = convertCnct c ++ expansionaCnct cs

convertCnct :: ConnectExpression -> [(Cnct,Cnct)]
convertCnct (cl, [], cr) = [(cl,cr)]
convertCnct (cl, [cb], cr) = [(cl,leftCnct cb),(rightCnct cb,cr)]
convertCnct (cl, cbs, cr) = (cl, leftCnct $ head cbs) : (convertBCnct cbs) ++ [(rightCnct $ last cbs, cr)]

convertBCnct :: [BCnct] -> [(Cnct,Cnct)]
convertBCnct [] = []
convertBCnct (b1:b2:[]) = [(rightCnct b1,leftCnct b2)]
convertBCnct (b1:b2:bs) = (rightCnct b1,leftCnct b2) : convertBCnct (b2:bs) where

leftCnct :: BCnct -> Cnct
leftCnct (BPin port comp _) = Pin comp port
leftCnct (BWire wire) = Wire wire

rightCnct :: BCnct -> Cnct
rightCnct (BPin _ comp port) = Pin comp port
rightCnct (BWire wire) = Wire wire

-- -- -- -- convert from Cnct to Connectable -- -- -- --
convertToPort :: [ModuleElement] -> [DefinePart] -> [(Cnct,Cnct)] -> Result [(Connectable,Connectable)]
convertToPort _ _ [] = Right []
convertToPort modElems defParts ((cl,cr):cs) = do
  pl <- cnctToConnectable modElems defParts cl
  pr <- cnctToConnectable modElems defParts cr
  convertToPort modElems defParts cs >>= (\x xs -> Right (x:xs)) (pl,pr)

searchComp :: CompIden -> [DeclarePart] -> Result PartIden
searchComp comp decParts =
  case lookupWith (\(x,_)->x==comp) decParts >>= justSnd of
    Just name -> Right name
    Nothing -> Left $ showPos comp ++ "\n\tComponent '" ++ getToken comp ++ "' is not declared."

searchPart :: PartIden -> [DefinePart] -> Result ([(PortIntLit,PortIden)],Reference)
searchPart part defParts =
  case lookupWith (\(x,_)->x==part) defParts >>= justSnd of
    Just partInfo -> Right partInfo
    Nothing -> Left $ showPos part ++ "\n\tPart '" ++ getToken part ++ "' is not defined."

searchPort :: PortIden -> [(PortIntLit,PortIden)] -> Result PortIntLit
searchPort port partInfos =
  case lookupWith (\(_,x) -> x==port) partInfos >>= justFst of
    Just portIntLit -> Right portIntLit
    Nothing -> Left $ (showPos port) ++ "\n\tPort '" ++ getToken port ++ "' is not defined."

searchItfc :: WireIden -> [ModuleElement] -> Maybe ItfcIden
searchItfc wire modElems =
  case elem wire decItfc of
    True  -> Just wire
    False -> Nothing
  where decItfc = pickupDecItfc modElems

searchWire :: WireIden -> [ModuleElement] -> Maybe WireIden
searchWire wire modElems =
  case elem wire decWire of
    True  -> Just wire
    False -> Nothing
  where decWire = pickupDecWire modElems

-- Connectable = (CompName, PortNum)
{-
  CnctのCompIden内のCompNameをキーに、[DeclarePart]から、PartIdenを探し出す。
  このPartIdenをキーに、[DefinePart]から[(PortIntLit,PortIden)]を探し出す。
  この[(PortIntLit,PortIden)]から、CnctのPortIden内のPortNameをキーにPortNumを探し出す。
  CompNameとPortNumのタプル = Connectableを返す。
-}
cnctToConnectable :: [ModuleElement] -> [DefinePart] -> Cnct -> Result Connectable
cnctToConnectable modElems defParts (Pin compIden portIden) = do
  partIden <- searchComp compIden $ pickupDecPart modElems
  partInfo <- searchPart partIden defParts
  portIntLit <- searchPort portIden (fst partInfo)
  Right (ConPort compIden portIntLit (snd partInfo) partIden)
cnctToConnectable modElems _ (Wire wireIden) = do
  -- DecWireとDecItfcの中から該当するものがあるかどうか探す
  case searchWire wireIden modElems of
    Nothing -> case searchItfc wireIden modElems of
      Nothing -> Left $ showPos wireIden ++  "\n\t'" ++ getToken wireIden ++ " is not definded."
      Just i  -> Right $ ConItfc wireIden
    Just w  -> Right $ ConWire wireIden

-- Port > Wire > Itfc
ordRef :: Connectable -> Connectable -> Ordering
ordRef x@(ConPort _ _ _ _) y@(ConPort _ _ _ _) = compare (getRef x) (getRef y)
ordRef (ConPort _ _ _ _) (ConWire _) = GT
ordRef (ConWire _) (ConPort _ _ _ _) = LT
ordRef (ConWire x) (ConWire y) = compare x y
ordRef (ConItfc _) (ConWire _) = LT
ordRef (ConWire _) (ConItfc _) = GT
ordRef (ConItfc _) (ConPort _ _ _ _) = LT
ordRef (ConPort _ _ _ _) (ConItfc _) = GT
ordRef (ConItfc x) (ConItfc y) = compare x y

eqRef :: Connectable -> Connectable -> Bool
eqRef pl pr = (==EQ) $ ordRef pl pr

eqComp :: Connectable -> Connectable -> Bool
eqComp (ConPort x _ _ _) (ConPort y _ _ _) = x == y
eqComp (ConPort _ _ _ _) (ConWire _) = False
eqComp (ConWire _) (ConPort _ _ _ _) = False
eqComp (ConWire x) (ConWire y) = x == y
eqComp (ConItfc _) (ConWire _) = False
eqComp (ConWire _) (ConItfc _) = False
eqComp (ConItfc _) (ConPort _ _ _ _) = False
eqComp (ConPort _ _ _ _) (ConItfc _) = False
eqComp (ConItfc x) (ConItfc y) = x == y


referencing :: [(Connectable,Connectable)] -> [(Connectable,Connectable)]
referencing ps = map (renameRef dic) ps
  where
    dic = termRef
        . groupBy eqRef
        . sortBy ordRef
        . nubBy eqComp
        $ expandTuple ps

termRef :: [[Connectable]] -> [(CompIden,Reference)]
termRef [] = []
termRef (ps:pss) = termRef_ ps 1 ++ (termRef pss)

termRef_ :: [Connectable] -> Int -> [(CompIden,Reference)]
termRef_ [] _ = []
termRef_ ((ConPort c _ r _):ps) n = (c,r ++ (show n)) : (termRef_ ps $ n + 1)
termRef_ (ConWire w:ps) n = termRef_ ps $ n
termRef_ (ConItfc i:ps) n = termRef_ ps $ n

expandTuple :: [(a,a)] -> [a]
expandTuple [] = []
expandTuple ((xl,xr):xs) = xl : xr : expandTuple xs

renameRef :: [(CompIden,Reference)] -> (Connectable,Connectable) -> (Connectable,Connectable)
renameRef dic (p1,p2) = (renameRefSingle dic p1, renameRefSingle dic p2)

renameRefSingle :: [(CompIden,Reference)] -> Connectable -> Connectable
renameRefSingle [] port = port
renameRefSingle ((c,r):ts) port@(ConPort pc pp pr ppa) =
  if pc == c
    then ConPort pc pp r ppa
    else renameRefSingle ts port
renameRefSingle _ (ConWire w) = ConWire w
renameRefSingle _ (ConItfc i) = ConItfc i

combineConnectables :: [Net] -> [(Connectable,Connectable)] -> Result [Net]
combineConnectables _ [] = Right []
combineConnectables origin (c:cs) = do
  newNets <- combineConnectables origin cs
  combineConnectable newNets c

combineConnectable :: [Net] -> (Connectable,Connectable) -> Result [Net]
combineConnectable origin (c1,c2) = do
  newNet <- combineNet net1 net2
  Right (newNet : ((origin \\ [net1]) \\ [net2]))
  where
    net1 = searchConnectable c1 origin
    net2 = searchConnectable c2 origin

searchConnectable :: Connectable -> [Net] -> Net
searchConnectable (ConWire name) [] = Net (name, Set.empty)
searchConnectable w@(ConWire name) (s:ss) =
  if (fst $ getNet s) == name
    then s
    else searchConnectable w ss
searchConnectable i@(ConItfc _) [] = Net (newToken "", Set.singleton i)
searchConnectable i@(ConItfc _) (s:ss) =
  if Set.member i (snd $ getNet s)
    then s
    else searchConnectable i ss
searchConnectable p@(ConPort _ _ _ _) [] = Net (newToken "", Set.singleton p)
searchConnectable p@(ConPort _ _ _ _) (s:ss) =
  if Set.member p (snd $ getNet s)
    then s
    else searchConnectable p ss

combineNet :: Net -> Net -> Result Net
combineNet n1@(Net (wire1,set1)) n2@(Net (wire2,set2)) =
  case (wire1,wire2) of
    ((Token [] _),(Token [] _)) -> Right $ Net (newToken "", Set.union set1 set2)
    ((Token [] _),name) -> Right $ Net (name, Set.union set1 set2)
    (name,(Token [] _)) -> Right $ Net (name, Set.union set1 set2)
    otherwise -> if wire1 == wire2
                 then Right $ Net (wire1, Set.union set1 set2)
                 else Left $ (showPos wire1)
                         ++ "\n\tConnection between different wires. '"
                         ++ (getToken wire1) ++ "' and '"
                         ++ (getToken wire2) ++ "'."

namingWire :: [Net] -> Int -> [Net]
namingWire [] _ = []
namingWire (Net(w,s):ns) n =
  if w .== ""
    then Net(newToken ("$$$" ++ format n 5),s) : namingWire ns (n+1)
    else Net(w,s) : namingWire ns n
