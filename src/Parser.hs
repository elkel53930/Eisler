module Parser where

import Text.ParserCombinators.Parsec
import System.IO

data Cnct = Pin CompIden PortIden deriving Show
data BCnct = BPin PortIden CompIden PortIden deriving Show

newtype PartIden = PartIden {getPartIden :: (PartName,SourcePos)} deriving(Show,Eq)
newtype PortIntLit = PortIntLit {getPortIntLit :: (PortNum,SourcePos)} deriving(Show,Eq)
newtype PortIden = PortIden {getPortIden :: (PortName,SourcePos)} deriving(Show,Eq)
newtype ModuleIden = ModuleIden {getModuleIden :: (ModuleName,SourcePos)} deriving(Show,Eq)
newtype CompIden = CompIden {getCompIden :: (CompName,SourcePos)} deriving(Show,Eq)

type Reference = String
type PartName = String
type PortNum = Int
type PortName = String
type ModuleName = String
type CompName = String

type DefinePart = (PartIden, ([(PortIntLit,PortIden)], Reference))
type DefineModule = (ModuleIden, ([(PortIntLit,PortIden)], [ModuleElement]))
type DeclarePart = (CompIden, PartIden)
type ConnectExpression = (Cnct, [BCnct], Cnct)

data SourceElement = Import FilePath
                   | DefPart DefinePart
                   | DefMod DefineModule deriving Show

data ModuleElement = DecPart DeclarePart
                   | ConExpr ConnectExpression deriving Show

kwdDefModule = "defmodule"
kwdDefPart = "defpart"
kwdDecPart = "part"
kwdAs = "as"
kwdImport = "import"

eqPartIden name (PartIden(partName,_)) = name == partName
eqPortIden name (PortIden(portName,_)) = name == portName
eqModuleIden name (ModuleIden(modName,_)) = name == modName
eqCompIden name (CompIden(compName,_)) = name == compName

parseEisFile :: FilePath -> IO(Either ParseError [SourceElement])
parseEisFile filepath = parseEisFiles [filepath] []

parseEisFiles :: [FilePath] -> [SourceElement] -> IO(Either ParseError [SourceElement])
parseEisFiles [] srcElems = return $ Right srcElems
parseEisFiles (file:fs) srcElems = do
  handle <- openFile file ReadMode
  source <- hGetContents handle
  case parse eislerFile file source of
    Right parsed -> do
      childlenResult <- parseEisFiles childlen elements
      case childlenResult of
        Right childlenElements -> parseEisFiles fs childlenElements
        Left err -> return $ Left err
      where
        childlen = pickupImport (parsed++srcElems)
        elements = pickupNotImport (parsed++srcElems)
    Left err -> return $ Left err


pickupImport :: [SourceElement] -> [FilePath]
pickupImport srcElems =
  map (\(Import file) -> file) $ filter (\x -> case x of
    Import file -> True
    otherwise -> False) srcElems

pickupNotImport :: [SourceElement] -> [SourceElement]
pickupNotImport srcElems =
  filter (\x -> case x of
    Import file -> False
    otherwise -> True) srcElems


eislerFile :: Parser [SourceElement]
eislerFile = many ( try imp <|>
                    try defPart <|>
                    defModule )
{-
  module/part define
-}

defModule :: Parser SourceElement
defModule = do
  stringSp kwdDefModule
  m <- iden
  charSp '('
  ps <- many port
  charSp ')'
  charSp '{'
  elems <- many ( try decPart <|>
                  try conExpr)
  charSp '}'
  return $ DefMod (ModuleIden m, (ps, elems))

defPart :: Parser SourceElement
defPart = do
  stringSp kwdDefPart
  p <- iden
  charSp '('
  ps <- many1 port
  charSp ')'
  charSp '{'
  ref <- partRef
  charSp '}'
  return $ DefPart (PartIden p, (ps, ref))

imp :: Parser SourceElement
imp = do
  stringSp kwdImport
  result <- strLit
  return $ Import result

partRef :: Parser String
partRef = do
  stringSp "ref"
  result <- strLit
  charSp ';'
  return result

port :: Parser (PortIntLit,PortIden)
port = do
  n <- intLit
  charSp ':'
  s <- iden
  return (PortIntLit n,PortIden s)

{-
  part/module/wire declare
-}

decPart :: Parser ModuleElement
decPart = do
  stringSp kwdDecPart
  c <- iden
  stringSp kwdAs
  p <- iden
  charSp ';'
  return $ DecPart (CompIden c, PartIden p)

{-
  ConExpr
-}

rCnctCompPort :: Parser Cnct
rCnctCompPort = do
  c <- iden
  char '.'
  p <- iden
  return $ Pin (CompIden c) (PortIden p)

lCnctCompPort :: Parser Cnct
lCnctCompPort = do
  p <- iden
  char '.'
  c <- iden
  return $ Pin (CompIden c) (PortIden p)

bCnctCompPort :: Parser BCnct
bCnctCompPort = do
  pl <- iden
  char '.'
  c <- iden
  char '.'
  pr <- iden
  return $ BPin (PortIden pl) (CompIden c) (PortIden pr)

rCnct :: Parser Cnct
rCnct = do
  result <- try rCnctCompPort
  charSp '-'
  return result

lCnct :: Parser Cnct
lCnct = try lCnctCompPort

bCnct :: Parser BCnct
bCnct = do
  result <- try bCnctCompPort
  charSp '-'
  return result

conExpr :: Parser ModuleElement
conExpr = do
  r  <- rCnct
  bs <- many $ try bCnct
  l <- lCnct
  charSp ';'
  return $ ConExpr (r, bs, l)

{-
  Common
-}

strLit :: Parser String
strLit = do
  spaces
  char '"'
  result <- many $ noneOf "\""
  char '"'
  spaces
  return result

intLit :: Parser (Int,SourcePos)
intLit = do
  spaces
  pos <- getPosition
  result <- many1 digit
  spaces
  return $ (read result, pos)

iden :: Parser (String,SourcePos)
iden = do
  spaces
  pos <- getPosition
  headLetter <- letter_
  tails <- many (letter_ <|> digit)
  spaces
  return $ (headLetter : tails, pos)

letter_ :: Parser Char
letter_ = letter <|> char '_'

charSp :: Char -> Parser String
charSp c = do
  spaces
  result <- char c
  spaces
  return [c]

stringSp :: String -> Parser String
stringSp s = do
  spaces
  result <- string s
  spaces
  return s
