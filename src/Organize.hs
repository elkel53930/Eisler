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
  let modElems'' = mergeDeclare parsed modElems'
--  Left $ show modElems'
  convertToPort modElems'' (fst $ divideSrc srcElems) $ expansionCnct . pickupConExpr $ modElems'

mergeDeclare :: [SourceElement] -> [ModuleElement] -> [ModuleElement]
--mergeDeclare _ (Left err) = Left err
mergeDeclare [] modElems = modElems
mergeDeclare (t:ts) ms =
  case t of
    DecGMod  m -> (DecLMod  m) : ms
    DecGWire w -> (DecLWire w) : ms
    DecGPart p -> (DecLPart p) : ms
    DecGItfc i -> (DecLItfc i) : ms
    otherwise  -> (mergeDeclare ts ms)

checkOverlap :: [SourceElement] -> ModuleName -> Result [SourceElement]
checkOverlap srcElems mname = do
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
      DefMod (mname, (_, m))  -> mname : getIdensMod (getToken mname) m
      Import _                -> []
      DecGMod (mname,_)       -> mname
      DecGPart (pname,_,_)    -> pname
      DecGWire (wname)        -> wname
      DecGItfc (iname)        -> iname

-- [ModuleElement]内の識別子の一覧を返す
getIdensMod :: String -> [ModuleElement] -> [Token String]
getIdensMod _ [] = []
getIdensMod postfix (t:ts) =
  case t of
    DecLPart (cname, _, _) -> (pre cname) ++ (getIdensMod postfix ts)
    DecLWire (wname   ) -> (pre wname) ++ (getIdensMod postfix ts)
    DecLMod  (mname, _) -> (pre mname) ++ (getIdensMod postfix ts)
    DecLItfc (iname   ) -> (pre iname) ++ (getIdensMod postfix ts)
    ConExpr _ -> getIdensMod postfix ts
    where
      pre = map (\(Token name pos) -> Token (name++"@"++postfix) pos)

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
  (DecLItfc $ [suffixIden suf iden]) : expandPort suf ps

for :: (a -> Result [b]) -> [a] -> Result [b]
for _ [] = Right []
for f (c:cs) = (++) <$> f c <*> for f cs

-- Module内で宣言された識別子にサフィックスをつける
expandModuleElements :: [SourceElement] -> [ModuleName] -> [ModuleElement] -> Suffix -> Result [ModuleElement]
expandModuleElements _ _ [] _ = Right []
expandModuleElements srcElems mns modElems@(m:ms) suf = (++) <$> m' <*> ( expandModuleElements srcElems mns ms suf ) where
   m' = case m of
      DecLMod  (cs,m)  -> for (\x -> do
        defMod <- searchMod (snd $ divideSrc srcElems) (getToken m)
        expandSubModules ("-" ++ (getToken x) ++ suf) mns srcElems defMod) cs
      DecLPart (c,p,t)-> Right [DecLPart ( map (suffixIden suf) c,p,t )]
      DecLWire w      -> Right [DecLWire $ map (suffixIden suf) w]
      DecLItfc i      -> Right [DecLItfc $ map( suffixIden suf) i]
      ConExpr (cr,bs,cl) ->
          Left $ show modElems
--        Right [ConExpr (suffixCnct target suf cr, suffixBCnct target suf bs, suffixCnct target suf cl)]
        where target = localComp modElems

localComp :: [ModuleElement] -> [CompIden]
localComp [] = []
localComp (m:ms) =
  case m of
    DecLMod  (names,_)   -> names ++ localComp ms
    DecLPart (names,_,_) -> names ++ localComp ms
    DecLWire names       -> names ++ localComp ms
    DecLItfc names       -> names ++ localComp ms
    otherwise            -> localComp ms

suffixCnct :: [CompIden] -> Suffix -> Cnct -> Cnct
suffixCnct target suf (Pin c p) = Pin (suffixIden suf c) p
{-  case lookupWith ((== getToken c) . getToken) target of
    Just _  -> Pin (suffixIden suf c) p
    Nothing -> (Pin c p)-}
suffixCnct target suf(Wire w)  = Wire $ suffixIden suf w

suffixBCnct :: [CompIden] -> Suffix -> [BCnct] -> [BCnct]
suffixBCnct _ _ [] = []
suffixBCnct target suf (b:bs) = b' : (suffixBCnct target suf bs) where
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
divideSrc ((Import _):ts) = divideSrc ts
divideSrc ((DecGPart _):ts) =divideSrc ts
divideSrc ((DecGMod _):ts) =divideSrc ts
divideSrc ((DecGWire _):ts) =divideSrc ts
divideSrc ((DecGItfc _):ts) =divideSrc ts

pickupDecLPart :: [ModuleElement] -> [DeclarePart]
pickupDecLPart modElems = map(\(DecLPart p) -> p) $ filter (\x -> case x of
  DecLPart p -> True
  otherwise -> False) modElems

pickupConExpr :: [ModuleElement] -> [ConnectExpression]
pickupConExpr modElems = map(\(ConExpr ce) -> ce) $ filter (\x -> case x of
  ConExpr _ -> True
  otherwise -> False) modElems

pickupDecLWire :: [ModuleElement] -> [WireIden]
pickupDecLWire modElems = concatMap(\(DecLWire w) -> w) $ filter (\x -> case x of
  DecLWire _ -> True
  otherwise -> False) modElems

pickupDecLItfc :: [ModuleElement] -> [ItfcIden]
pickupDecLItfc modElems = concatMap(\(DecLItfc i) -> i) $ filter (\x -> case x of
  DecLItfc _ -> True
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
  case elem wire decLItfc of
    True  -> Just wire
    False -> Nothing
  where decLItfc = pickupDecLItfc modElems

searchWire :: WireIden -> [ModuleElement] -> Maybe WireIden
searchWire wire modElems =
  case elem wire decWire of
    True  -> Just wire
    False -> Nothing
  where decWire = pickupDecLWire modElems

-- Connectable = (CompName, PortNum)
{-
  CnctのCompIden内のCompNameをキーに、[DeclarePart]から、PartIdenを探し出す。
  このPartIdenをキーに、[DefinePart]から[(PortIntLit,PortIden)]を探し出す。
  この[(PortIntLit,PortIden)]から、CnctのPortIden内のPortNameをキーにPortNumを探し出す。
  CompNameとPortNumのタプル = Connectableを返す。
-}
cnctToConnectable :: [ModuleElement] -> [DefinePart] -> Cnct -> Result Connectable
cnctToConnectable modElems defParts (Pin compIden portIden) = do
  case searchComp compIden $ pickupDecLPart modElems of
    Left msg -> case searchItfc (suffixIden ('-' : getToken compIden) portIden) modElems of
      Nothing -> Left msg
      Just i  -> Right $ ConItfc (suffixIden ('-' : getToken compIden) portIden)
    Right p -> do
      (partIden, partType) <- searchComp compIden $ pickupDecLPart modElems
      partInfo <- searchPart partIden defParts
      portIntLit <- searchPort portIden (fst partInfo)
      Right (ConPort compIden portIntLit portIden (snd partInfo) partIden partType)
cnctToConnectable modElems _ (Wire wireIden) = do
  -- DecLWireとDecLItfcの中から該当するものがあるかどうか探す
  case searchWire wireIden modElems of
    Nothing -> case searchItfc wireIden modElems of
      Nothing -> Left $ showPos wireIden ++  "\n\t'" ++ getToken wireIden ++ "' is not defined."
      Just i  -> Right $ ConItfc wireIden
    Just w  -> Right $ ConWire wireIden
