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
                 | ConPort CompIden PortIntLit Reference PartIden deriving (Show,Eq,Ord)
type ErrorMsg = String

type Result = Either ErrorMsg

moduleNet :: [SourceElement] -> ModuleName -> Result [Net]
moduleNet parsed name = do
  srcElems <- checkOverlap parsed
  let (defParts,defMods) = divideSrc srcElems
  (_,(_,modElems)) <- searchMod defMods name
  let (wires,decParts,conExprs) = divideMod modElems
  let cncts = expansionaCnct conExprs
  ports <- convertToPort decParts defParts cncts
  let refed = referencing ports
  nets <- combineConnectables [] refed
  Right $ namingWire nets 1

getRef :: Connectable -> Reference
getRef (ConPort _ _ ref _) = ref

checkOverlap :: [SourceElement] -> Result [SourceElement]
checkOverlap srcElems = do
  case (\input -> input \\ nub input) $ getIdens srcElems of
    [] -> Right srcElems
    overlaps -> Left $ getOverlapsError overlaps

-- [SourceElement]内の識別子の一覧を返す。
getIdens :: [SourceElement] -> [Token String]
getIdens [] = []
getIdens (t:ts) = name ++ getIdens ts where
  name =
    case t of
      DefPart (pname, (_, _)) -> [pname]
      DefMod (mname, (_, m)) -> (mname) : getIdensMod m

-- [ModuleElement]内の識別子の一覧を返す
getIdensMod :: [ModuleElement] -> [Token String]
getIdensMod [] = []
getIdensMod (t:ts) =
  case t of
    DecPart (cname, _) -> cname : (getIdensMod ts)
    DecWire (wname) -> wname : (getIdensMod ts)
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

divideMod :: [ModuleElement] -> ([WireIden],[DeclarePart],[ConnectExpression])
divideMod [] = ([],[],[])
divideMod ((DecPart p):ts) = (ws,p:ps,cs) where
  (ws,ps,cs) = divideMod ts
divideMod ((ConExpr c):ts) = (ws,ps,c:cs) where
  (ws,ps,cs) = divideMod ts
divideMod ((DecWire w):ts) = (w:ws,ps,cs) where
  (ws,ps,cs) = divideMod ts

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
convertToPort :: [DeclarePart] -> [DefinePart] -> [(Cnct,Cnct)] -> Result [(Connectable,Connectable)]
convertToPort _ _ [] = Right []
convertToPort decParts defParts ((cl,cr):cs) = do
  pl <- cnctToConnectable decParts defParts cl
  pr <- cnctToConnectable decParts defParts cr
  convertToPort decParts defParts cs >>= (\x xs -> Right (x:xs)) (pl,pr)


searchComp :: CompIden -> [DeclarePart] -> Result PartIden
searchComp comp decParts =
  case lookupWith (\(x,_)->x==comp)  decParts >>= justSnd of
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

-- Connectable = (CompName, PortNum)
{-
  CnctのCompIden内のCompNameをキーに、[DeclarePart]から、PartIdenを探し出す。
  このPartIdenをキーに、[DefinePart]から[(PortIntLit,PortIden)]を探し出す。
  この[(PortIntLit,PortIden)]から、CnctのPortIden内のPortNameをキーにPortNumを探し出す。
  CompNameとPortNumのタプル = Connectableを返す。
-}
cnctToConnectable :: [DeclarePart] -> [DefinePart] -> Cnct -> Result Connectable
cnctToConnectable decParts defParts (Pin compIden portIden) = do
  partIden <- searchComp compIden decParts
  partInfo <- searchPart partIden defParts
  portIntLit <- searchPort portIden (fst partInfo)
  Right (ConPort compIden portIntLit (snd partInfo) partIden)
cnctToConnectable decParts defParts (Wire wireIden) = do
  Right $ ConWire wireIden



ordRef :: Connectable -> Connectable -> Ordering
ordRef x@(ConPort _ _ _ _) y@(ConPort _ _ _ _) = compare (getRef x) (getRef y)
ordRef (ConPort _ _ _ _) (ConWire _) = GT
ordRef (ConWire _) (ConPort _ _ _ _) = LT
ordRef (ConWire x) (ConWire y) = compare x y

eqRef :: Connectable -> Connectable -> Bool
eqRef pl pr = (==EQ) $ ordRef pl pr

eqComp :: Connectable -> Connectable -> Bool
eqComp (ConPort x _ _ _) (ConPort y _ _ _) = x == y
eqComp (ConPort _ _ _ _) (ConWire _) = False
eqComp (ConWire _) (ConPort _ _ _ _) = False
eqComp (ConWire x) (ConWire y) = x == y

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
