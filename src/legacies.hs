
eqFstFst :: Eq a => a -> ((a, b), c) -> Bool
eqFstFst x t = x == (fst $ fst t)

getCompFromModule :: [P.ModuleElement] -> P.CompName -> Maybe P.CompIden
getCompFromModule modElems name = lookupWith eqFst name comps where
  (comps,_) = divideModuleElement modElems

getModuleFromSource :: [P.SourceElement] -> P.ModuleName -> Maybe ([(P.PortIntLit, P.PortIden)], [P.ModuleElement])
getModuleFromSource srcElems name = lookupWith eqFst name mods where
  (_,mods) = divideSoureElement srcElems

getPartFromComp :: [P.ModuleElement] -> P.PartName -> Maybe P.PartIden
getPartFromComp modElems name = lookupWith eqFst name decs where
  (decs,_) = divideModuleElement modElems

getPartFromSource :: [P.SourceElement] -> P.PartName -> Maybe ([(P.PortIntLit, P.PortIden)], P.Reference)
getPartFromSource srcElems name = lookupWith eqFst name parts where
  (parts,_) = divideSoureElement srcElems

getPortNumInPort :: [(P.PortIntLit,P.PortIden)] -> P.PortName -> Maybe P.PortIntLit
getPortNumInPort [] _ = Nothing
getPortNumInPort ((num,name):ts) target =
  if fst name == target then Just num else getPortNumInPort ts target

getPortNum :: [P.SourceElement] -> P.ModuleName -> P.CompName -> P.PortName -> Maybe P.PortNum
getPortNum srcElems modu comp port = do
  modElems <- getModuleFromSource srcElems modu >>= justSnd
  partName <- getPartFromComp modElems comp >>= justFst
  ports <- getPartFromSource srcElems partName >>= justFst
  getPortNumInPort ports port >>= justFst

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

--convertModule :: P.DefineModule -> Module
--convertModule defMod =
{-
convertWire ::P.ModuleElement -> P.ConnectExpression -> [Wire] -> [Wire]
convertWire modElems (rc,[bcs],lc) ws =
  portr = (
    getCompFromModule modElems rcName
    )
-}

convertToPort :: [P.SourceElement] -> P.ModuleName -> P.Cnct -> Maybe Port
convertToPort srcElems mName (P.Pin compIden portIden) =
  case portNum of
    Just n -> Just ( compName, portName, n )
    Nothing -> Nothing
  where
    compName = fst compIden
    portName = fst portIden
    portNum = getPortNum srcElems mName compName portName

extractConnectedPorts :: [Wire] -> Port -> (Wire,[Wire])
extractConnectedPorts ws port =
  case searchWire port ws of
    Nothing -> ( ("", [port]), ws )
    Just w -> ( w, delete w ws )

concatWire :: Wire -> Wire -> Result Wire
concatWire w1 w2 =
  case (name1,name2) of
    ([],[]) -> Right ("",ps1 ++ ps2)
    ([],str) -> Right (str,ps1 ++ ps2)
    (str,[]) -> Right (str,ps1 ++ ps2)
    (str1,str2) -> if str1 == str2
      then Right (str1,ps1++ps2)
      else Left $ "Conflict wire name : " ++ str1 ++ " and " ++ str2
  where
    (name1,ps1) = w1
    (name2,ps2) = w2

addWire :: [Wire] -> Port -> Port -> Result [Wire]
addWire wires p1 p2 =
  case newWire of
    Right newW -> Right $ newW : ws'
    Left errmsg -> Left errmsg
  where
    (w1,ws1) = extractConnectedPorts wires  p1
    (w2,ws') = extractConnectedPorts ws1    p2
    newWire = concatWire w1 w2

searchWire :: Port -> [Wire] -> Maybe Wire
searchWire _ [] = Nothing
searchWire port (w:ws) =
  if port `elem` ports
    then Just w
    else searchWire port ws
  where
    (_,ports) = w



-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

divideSoureElement :: [P.SourceElement] -> ([P.DefinePart],[P.DefineModule])
divideSoureElement [] = ([],[])
divideSoureElement ((P.DefPart p):ts) = (p:ps,ms) where
  (ps,ms) = divideSoureElement ts
divideSoureElement ((P.DefMod m):ts) = (ps,m:ms) where
  (ps,ms) = divideSoureElement ts

divideModuleElement :: [P.ModuleElement] -> ([P.DeclarePart],[P.ConnectExpression])
divideModuleElement [] = ([],[])
divideModuleElement ((P.DecPart p):ts) = (p:ps,cs) where
  (ps,cs) = divideModuleElement ts
divideModuleElement ((P.ConExpr c):ts) = (ps,c:cs) where
  (ps,cs) = divideModuleElement ts







  getModuleFromSource :: [P.SourceElement] -> P.ModuleName -> Maybe ([(P.PortIntLit, P.PortIden)], [P.ModuleElement])
  getModuleFromSource srcElems name = lookupWith eqFst name mods where
    (_,mods) = divideSoureElement srcElems

  getPartFromComp :: [P.ModuleElement] -> P.PartName -> Maybe P.PartIden
  getPartFromComp modElems name = lookupWith eqFst name decs where
    (decs,_) = divideModuleElement modElems

  getPartFromSource :: [P.SourceElement] -> P.PartName -> Maybe ([(P.PortIntLit, P.PortIden)], P.Reference)
  getPartFromSource srcElems name = lookupWith eqFst name parts where
    (parts,_) = divideSoureElement srcElems

  getPortNumInPort :: [(P.PortIntLit,P.PortIden)] -> P.PortName -> Maybe P.PortIntLit
  getPortNumInPort [] _ = Nothing
  getPortNumInPort ((num,name):ts) target =
    if fst name == target then Just num else getPortNumInPort ts target

  divideSoureElement :: [P.SourceElement] -> ([P.DefinePart],[P.DefineModule])
  divideSoureElement [] = ([],[])
  divideSoureElement ((P.DefPart p):ts) = (p:ps,ms) where
    (ps,ms) = divideSoureElement ts
  divideSoureElement ((P.DefMod m):ts) = (ps,m:ms) where
    (ps,ms) = divideSoureElement ts

  divideModuleElement :: [P.ModuleElement] -> ([P.DeclarePart],[P.ConnectExpression])
  divideModuleElement [] = ([],[])
  divideModuleElement ((P.DecPart p):ts) = (p:ps,cs) where
    (ps,cs) = divideModuleElement ts
  divideModuleElement ((P.ConExpr c):ts) = (ps,c:cs) where
    (ps,cs) = divideModuleElement ts

  getPort :: [P.SourceElement] -> [P.ModuleElement] -> P.Cnct -> Maybe Port
  getPort srcElems modElems cnct = do
  --  modElems <- getModuleFromSource srcElems modName >>= justSnd
    partName <- getPartFromComp modElems comp >>= justFst
    ports <- getPartFromSource srcElems partName >>= justFst
    portNum <- getPortNumInPort ports port >>= justFst
    (\name num -> Just (name,num)) comp portNum where
      P.Pin (comp,_) (port,_) = cnct

  pickupWire' :: Port -> [Wire] -> [Wire] -> ([Wire],Wire)
  pickupWire' port origins [] = (origins,("",[]))
  pickupWire' port origins (w:ws) =
    if port `elem` ports
      then (delete w origins, w)
      else pickupWire' port origins ws
    where
      (_,ports) = w

  pickupWire :: Port -> [Wire] -> ([Wire],Wire)
  pickupWire port ws = pickupWire' port ws ws

  concatWire :: Wire -> Wire -> Result Wire
  concatWire w1 w2 =
    case (name1,name2) of
      ([],[]) -> Right ("",ps1 ++ ps2)
      ([],str) -> Right (str,ps1 ++ ps2)
      (str,[]) -> Right (str,ps1 ++ ps2)
      (str1,str2) -> if str1 == str2
        then Right (str1,ps1++ps2)
        else Left $ "Conflict wire name : '" ++ str1 ++ "' and '" ++ str2 ++ "'"
    where
      (name1,ps1) = w1
      (name2,ps2) = w2

  convertCnct :: P.ConnectExpression -> [(P.Cnct,P.Cnct)]
  convertCnct (cl, [], cr) = [(cl,cr)]
  convertCnct (cl, [cb], cr) = [(cl,P.Pin comp lPort),(P.Pin comp rPort,cr)] where
    P.BPin lPort comp rPort = cb
  convertCnct (cl, cbs, cr) = (cl, P.Pin compHead lPortHead) : (convertBCnct cbs) ++ [(P.Pin compLast rPortLast, cr)] where
    P.BPin lPortHead compHead _ = head cbs
    P.BPin _ compLast rPortLast = last cbs

  convertBCnct :: [P.BCnct] -> [(P.Cnct,P.Cnct)]
  convertBCnct (b1:b2:[]) = [(P.Pin comp1 rPort1, P.Pin comp2 lPort2)] where
    P.BPin _ comp1 rPort1 = b1
    P.BPin lPort2 comp2 _ = b2
  convertBCnct (b1:b2:bs) = (P.Pin comp1 rPort1, P.Pin comp2 lPort2) : convertBCnct bs where
    P.BPin _ comp1 rPort1 = b1
    P.BPin lPort2 comp2 _ = b2


    {-
  -- P.ModuleElement はConExprだけの前提
  convertCnctToWire :: [P.SourceElement] -> [P.ModuleElement] -> [Wire] -> [Wire]
  convertCnctToWire _ _ ws [] = ws
  convertCnctToWire srcElems modElems ws e:es =
    portConnects = convertCnct conExpr
    P.ConExpr conExpr = e

  convertModule :: P.DefineModule -> Module
  convertModule (iden, _, modElems) = (iden,ws) where
    ws =
  -}

  addPortsConnect :: [P.SourceElement] -> [P.ModuleElement] -> [Wire] -> [(P.Cnct,P.Cnct)] -> [Wire]
  addPortsConnect _ _ ws [] = ws
  addPortsConnect srcElems modElems ws (p:ps) =
    addPortConnect srcElems modElems ws' p where
      ws' = addPortsConnect srcElems modElems ws ps

  addPortConnect :: [P.SourceElement] -> [P.ModuleElement] -> [Wire] -> (P.Cnct,P.Cnct) -> [Wire]
  addPortConnect srcElems modElems ws (cl,cr) = newWire : wsr where
    Right newWire = concatWire wl wr
    (wsl,wl) = pickupWire lPort ws
    (wsr,wr) = pickupWire rPort wsl
    Just lPort = getPort srcElems modElems cl
    Just rPort = getPort srcElems modElems cr

  genWire wName cName pNum = (wName,[(cName,pNum)])
