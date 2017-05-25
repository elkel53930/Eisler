module Organize(organize) where

import Types
import Common

import Control.Applicative
import Data.List

organize :: [SourceElement] -> ModuleName -> Result [(Connectable,Connectable)]
organize parsed name = do
  srcElems <- checkOverlap parsed name
  (_,(_,modElems)) <- searchMod (snd $ divideSrc srcElems) name
  modElems' <- expandModuleElements srcElems [name] modElems ( '-' : name )
--  Left $ show modElems'
  convertToPort modElems' (fst $ divideSrc srcElems) $ expansionCnct . pickupConExpr $ modElems'

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
    DecPart (cname, _, _) -> cname ++ (getIdensMod ts)
    DecWire (wname   ) -> wname ++ (getIdensMod ts)
    DecMod  (mname, _) -> mname ++ (getIdensMod ts)
    DecItfc (iname   ) -> iname ++ (getIdensMod ts)
    ConExpr _ -> getIdensMod ts

getOverlapsError :: [Token String] -> String
getOverlapsError ts =
  concat $ map (\token -> (showPos token)
                   ++ "\n\tMultiple Declarations of '"
                   ++ getToken token ++ "'\n" ) ts

expandSubModules :: Suffix -> [ModuleName] -> [SourceElement] -> DefineModule -> Result [ModuleElement]
expandSubModules suf mns srcElems (iden, (ports,modElems)) = do
  case elem (getToken iden) mns of
    True -> Left $ (showPos iden) ++ "\n\tModule '" ++ (getToken iden) ++ "' is used recursively."
    False -> (++) <$> (Right $ expandPort suf ports) <*> (expandModuleElements srcElems (getToken iden : mns) modElems suf)

expandPort :: Suffix -> [(PortIntLit,PortIden)] -> [ModuleElement]
expandPort _ [] = []
expandPort suf ((num,iden):ps) =
  (DecItfc $ [suffixIden suf iden]) : expandPort suf ps

for :: (a -> Result [b]) -> [a] -> Result [b]
for _ [] = Right []
for f (c:cs) = (++) <$> f c <*> for f cs

-- Module内の識別子にサフィックスをつける
expandModuleElements :: [SourceElement] -> [ModuleName] -> [ModuleElement] -> Suffix -> Result [ModuleElement]
expandModuleElements _ _ [] _ = Right []
expandModuleElements srcElems mns (m:ms) suf = (++) <$> m' <*> ( expandModuleElements srcElems mns ms suf ) where
   m' = case m of
      DecMod  (cs,m)  -> for (\x -> do
        defMod <- searchMod (snd $ divideSrc srcElems) (getToken m)
        expandSubModules ("-" ++ (getToken x) ++ suf) mns srcElems defMod) cs
      DecPart (c,p,t)-> Right [DecPart ( map (suffixIden suf) c,p,t )]
      DecWire w      -> Right [DecWire $ map (suffixIden suf) w]
      DecItfc i      -> Right [DecItfc $ map( suffixIden suf) i]
      ConExpr (cr,bs,cl) ->
        Right [ConExpr (suffixCnct cr suf, suffixBCnct bs suf, suffixCnct cl suf)]

suffixCnct :: Cnct -> Suffix -> Cnct
suffixCnct (Pin c p) suf = Pin (suffixIden suf c) p
suffixCnct (Wire w)  suf = Wire $ suffixIden suf w

suffixBCnct :: [BCnct] -> Suffix -> [BCnct]
suffixBCnct [] _ = []
suffixBCnct (b:bs) suf = b' : (suffixBCnct bs suf) where
  b' = case b of
    BPin pr c pl -> BPin pr (suffixIden suf c) pl
    BWire w      -> BWire $ suffixIden suf w

suffixIden :: Suffix -> Identify -> Identify
suffixIden suff (Token name pos) = Token (name++suff) pos

-- -- -- -- divide -- -- -- --

divideSrc :: [SourceElement] -> ([DefinePart],[DefineModule])
divideSrc [] = ([],[])
divideSrc ((DefPart p):ts) = (p:ps,ms) where
  (ps,ms) = divideSrc ts
divideSrc ((DefMod m):ts) = (ps,m:ms) where
  (ps,ms) = divideSrc ts

pickupDecPart :: [ModuleElement] -> [DeclarePart]
pickupDecPart modElems = map(\(DecPart p) -> p) $ filter (\x -> case x of
  DecPart p -> True
  otherwise -> False) modElems

pickupConExpr :: [ModuleElement] -> [ConnectExpression]
pickupConExpr modElems = map(\(ConExpr ce) -> ce) $ filter (\x -> case x of
  ConExpr _ -> True
  otherwise -> False) modElems

pickupDecWire :: [ModuleElement] -> [WireIden]
pickupDecWire modElems = concatMap(\(DecWire w) -> w) $ filter (\x -> case x of
  DecWire _ -> True
  otherwise -> False) modElems

pickupDecItfc :: [ModuleElement] -> [ItfcIden]
pickupDecItfc modElems = concatMap(\(DecItfc i) -> i) $ filter (\x -> case x of
  DecItfc _ -> True
  otherwise -> False) modElems

-- -- -- --  search -- -- -- --

searchMod :: [DefineModule] -> String -> Result DefineModule
searchMod defMods name =
  case lookupWith (\(x,_) -> x.==name) defMods of
    Just defmod -> Right defmod
    Nothing -> Left $ "Module '" ++ name ++ "' is not defined."

-- -- -- -- expansion -- -- -- --

-- CnctとBCnctが混在したConnectExpressionを、Cnct x2のタプルに変換する
expansionCnct :: [ConnectExpression] -> [(Cnct,Cnct)]
expansionCnct = foldl (\cncts ce -> cncts ++ convertCnct ce) []

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

searchComp :: CompIden -> [DeclarePart] -> Result (PartIden,Maybe PartType)
searchComp comp decParts =
  case lookupWith (\(x,_,_)->elem comp x) decParts >>= justCutFst3 of
    Just part -> Right part
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
  case searchComp compIden $ pickupDecPart modElems of
    Left msg -> case searchItfc (suffixIden ('-' : getToken compIden) portIden) modElems of
      Nothing -> Left msg
      Just i  -> Right $ ConItfc (suffixIden ('-' : getToken compIden) portIden)
    Right p -> do
      (partIden, partType) <- searchComp compIden $ pickupDecPart modElems
      partInfo <- searchPart partIden defParts
      portIntLit <- searchPort portIden (fst partInfo)
      Right (ConPort compIden portIntLit portIden (snd partInfo) partIden partType)
cnctToConnectable modElems _ (Wire wireIden) = do
  -- DecWireとDecItfcの中から該当するものがあるかどうか探す
  case searchWire wireIden modElems of
    Nothing -> case searchItfc wireIden modElems of
      Nothing -> Left $ showPos wireIden ++  "\n\t'" ++ getToken wireIden ++ "' is not defined."
      Just i  -> Right $ ConItfc wireIden
    Just w  -> Right $ ConWire wireIden
