module Semantics where

import Parser
import Text.ParserCombinators.Parsec
import Control.Applicative
import Data.List
import Common

type Module = (ModuleIden, [ Wire ])
type Wire = (WireName, [Port])
type Port = (CompName, PortNum, Reference)
type WireName = String
type ErrorMsg = String

type Result = Either ErrorMsg

-- 重複しているタプルを返す。
-- 重複しているかどうかはaだけで判断
{-checkOverlap :: Eq a => Eq b => [(a,b)] -> [(a,b)]
checkOverlap input = input \\ nubBy (\(x,_) (y,_)->x==y) input-}

checkOverlap :: [SourceElement] -> Result [SourceElement]
checkOverlap srcElems = do
  case (\input -> input \\ nubBy (\(x,_) (y,_)->x==y) input) $ getNames srcElems of
    [] -> Right srcElems
    overlaps -> Left $ getOverlapsError overlaps


-- [SourceElement]内で重複している識別子の一覧を返す。
getNames :: [SourceElement] -> [(String,SourcePos)]
getNames [] = []
getNames (t:ts) = name ++ getNames ts where
  name =
    case t of
      DefPart (pname, (_, _)) -> [pname]
      DefMod (mname, (_, m)) -> mname : getNamesFromModule m

-- [ModuleElement]内で重複している識別子の一覧を返す
getNamesFromModule :: [ModuleElement] -> [(String,SourcePos)]
getNamesFromModule [] = []
getNamesFromModule (t:ts) =
  case t of
    DecPart (cname, _) -> cname : (getNamesFromModule ts)
    ConExpr _ -> getNamesFromModule ts

getOverlapsError :: [(String, SourcePos)] -> String
getOverlapsError ts =
  concat $ map (\(name,pos) -> (show pos)
                   ++ "\n\tMultiple Declarations of '"
                   ++ name ++ "'\n" ) ts


-- -- -- -- divide -- -- -- --

divideSrc :: [SourceElement] -> ([DefinePart],[DefineModule])
divideSrc [] = ([],[])
divideSrc ((DefPart p):ts) = (p:ps,ms) where
  (ps,ms) = divideSrc ts
divideSrc ((DefMod m):ts) = (ps,m:ms) where
  (ps,ms) = divideSrc ts

divideMod :: [ModuleElement] -> ([DeclarePart],[ConnectExpression])
divideMod [] = ([],[])
divideMod ((DecPart p):ts) = (p:ps,cs) where
  (ps,cs) = divideMod ts
divideMod ((ConExpr c):ts) = (ps,c:cs) where
  (ps,cs) = divideMod ts

-- -- -- --  search -- -- -- --

searchMod :: [DefineModule] -> ModuleName -> Result DefineModule
searchMod defMods name =
  case lookupWith eqFst name defMods of
    Just defmod -> Right defmod
    Nothing -> Left $ "Module '" ++ name ++ "' is not defined."

-- -- -- -- expansion -- -- -- --

expansionaCnct :: [ConnectExpression] -> [(Cnct,Cnct)]
expansionaCnct [] = []
expansionaCnct (c:cs) = convertCnct c ++ expansionaCnct cs

convertCnct :: ConnectExpression -> [(Cnct,Cnct)]
convertCnct (cl, [], cr) = [(cl,cr)]
convertCnct (cl, [cb], cr) = [(cl,Pin comp lPort),(Pin comp rPort,cr)] where
  BPin lPort comp rPort = cb
convertCnct (cl, cbs, cr) = (cl, Pin compHead lPortHead) : (convertBCnct cbs) ++ [(Pin compLast rPortLast, cr)] where
  BPin lPortHead compHead _ = head cbs
  BPin _ compLast rPortLast = last cbs

convertBCnct :: [BCnct] -> [(Cnct,Cnct)]
convertBCnct (b1:b2:[]) = [(Pin comp1 rPort1, Pin comp2 lPort2)] where
  BPin _ comp1 rPort1 = b1
  BPin lPort2 comp2 _ = b2
convertBCnct (b1:b2:bs) = (Pin comp1 rPort1, Pin comp2 lPort2) : convertBCnct bs where
  BPin _ comp1 rPort1 = b1
  BPin lPort2 comp2 _ = b2


-- -- -- -- convert To Port -- -- -- --
convertToPort :: [DeclarePart] -> [DefinePart] -> [(Cnct,Cnct)] -> Result [(Port,Port)]
convertToPort _ _ [] = Right []
convertToPort decParts defParts ((cl,cr):cs) = do
  pl <- retraceComp decParts defParts cl
  pr <- retraceComp decParts defParts cr
  convertToPort decParts defParts cs >>= (\x xs -> Right (x:xs)) (pl,pr)


searchComp :: CompIden -> [DeclarePart] -> Result PartIden
searchComp (compName, srcPos) decParts =
  case lookupWith eqFst compName decParts >>= justSnd of
    Just name -> Right name
    Nothing -> Left $ (show srcPos) ++ "\n\tComponent '" ++ compName ++ "' is not declared."

searchPart :: PartIden -> [DefinePart] -> Result ([(PortIntLit,PortIden)],Reference)
searchPart (partName,srcPos) defParts =
  case lookupWith eqFst partName defParts >>= justSnd of
    Just partInfo -> Right partInfo
    Nothing -> Left $ (show srcPos) ++ "\n\tPart '" ++ partName ++ "' is not defined."

searchPort :: PortIden -> [(PortIntLit,PortIden)] -> Result PortIntLit
searchPort (portName,srcPos) partInfos =
  case lookupWith_ eqFst portName partInfos >>= justFst of
    Just portIntLit -> Right portIntLit
    Nothing -> Left $ (show srcPos) ++ "\n\tPort '" ++ portName ++ "' is not defined."

-- Port = (CompName, PortNum)
{-
  CnctのCompIden内のCompNameをキーに、[DeclarePart]から、PartIdenを探し出す。
  このPartIdenをキーに、[DefinePart]から[(PortIntLit,PortIden)]を探し出す。
  この[(PortIntLit,PortIden)]から、CnctのPortIden内のPortNameをキーにPortNumを探し出す。
  CompNameとPortNumのタプル = Portを返す。
-}
retraceComp :: [DeclarePart] -> [DefinePart] -> Cnct -> Result Port
retraceComp decParts defParts cnct = do
  partIden <- searchComp compIden decParts
  partInfo <- searchPart partIden defParts
  portIntLit <- searchPort portIden (fst partInfo)
  Right (fst compIden, fst portIntLit, snd partInfo)
  where
    Pin compIden portIden = cnct

--collectUpPort :: [(Port,Port)] -> Maybe Wire
--collectUpPort ((pl,pr):ps) =

--eqPortPort (x1,x2) (y1,y2) = (x1==y1&&x2==y2) || (x1==y2&&x2==y1)
