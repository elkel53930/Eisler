module Parser.Parser ( eislerFile
              , RCnct
              , LCnct
              , BCnct
              , Comp
              , Port
              , Part
              , Module
              , SourceElement
              , ModuleElement
              ) where

import Text.ParserCombinators.Parsec

data RCnct = RWire String
           | RPin Comp Port
           deriving Show
data LCnct = LWire String
           | LPin Port Comp
           deriving Show
data BCnct = BWire String
           | BPin Port Comp Port
           deriving Show

data Comp = Comp String deriving Show
data Port = Port String deriving Show
data Part = Part String deriving Show
data Module = Module String deriving Show

data SourceElement = ImportSource { fileName :: String
                                  , importSrcPos :: SourcePos}
                   | DefinePart { defPartName :: String
                                , defPartPorts :: [(Int,String,SourcePos)]
                                , defPartRef :: String
                                }
                   | DefineModule { defModuleName :: String
                                  , defModPorts :: [(Int,String,SourcePos)]
                                  , defModElements :: [(ModuleElement,SourcePos)]
                                  } deriving Show

data ModuleElement = DeclarePart Comp Part
                   | DeclareWire String
                   | DeclareModule Comp Module
                   | ConExpr RCnct [BCnct] LCnct
                   deriving Show

kwdDefModule = "defmodule"
kwdDefPart = "defpart"
kwdDecModule = "module"
kwdDecPart = "part"
kwdDecWire = "wire"
kwdAs = "as"
kwdImport = "import"
keywords = [kwdDecPart,kwdDefModule,kwdDefPart,kwdDefModule,kwdAs,kwdDecWire,kwdImport]

eislerFile :: Parser [SourceElement]
eislerFile = many ( try importSrc <|>
                    try defPart <|>
                    defModule )

{-
  source import
-}
importSrc :: Parser SourceElement
importSrc = do
  p<-getPosition
  stringSp kwdImport
  s <- strLit
  charSp ';'
  return ImportSource {fileName = s, importSrcPos = p}

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
                  try decWire <|>
                  try decModule <|>
                  try conExpr)
  charSp '}'
  return DefineModule{ defModuleName = m
                     , defModPorts = ps
                     , defModElements = elems}

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
  return DefinePart{ defPartName = p
                   , defPartPorts = ps
                   , defPartRef = ref}

partRef :: Parser String
partRef = do
  stringSp "ref"
  result <- strLit
  charSp ';'
  return result

port :: Parser (Int,String,SourcePos)
port = do
  p <- getPosition
  n <- intLit
  charSp ':'
  s <- iden
  return (n,s,p)

{-
  part/module/wire declare
-}

decPart :: Parser (ModuleElement,SourcePos)
decPart = do
  pos <- getPosition
  stringSp kwdDecPart
  c <- iden
  stringSp kwdAs
  p <- iden
  charSp ';'
  return $ ( (DeclarePart (Comp c) (Part p)), pos)

decModule :: Parser (ModuleElement,SourcePos)
decModule = do
  pos <- getPosition
  stringSp kwdDecModule
  c <- iden
  stringSp kwdAs
  m <- iden
  charSp ';'
  return $ ((DeclareModule (Comp c) (Module m)), pos)

decWire :: Parser (ModuleElement,SourcePos)
decWire = do
  pos <- getPosition
  stringSp kwdDecWire
  w <- iden
  charSp ';'
  return $ ((DeclareWire w),pos)

{-
  ConExpr
-}

rCnctCompPort :: Parser RCnct
rCnctCompPort = do
  c <- iden
  char '.'
  p <- iden
  return $ RPin (Comp c) (Port p)

rCnctWire :: Parser RCnct
rCnctWire = do
  w <- iden
  return $ RWire w

lCnctCompPort :: Parser LCnct
lCnctCompPort = do
  p <- iden
  char '.'
  c <- iden
  return $ LPin (Port p) (Comp c)

lCnctWire :: Parser LCnct
lCnctWire = do
  w <- iden
  return $ LWire w

bCnctCompPort :: Parser BCnct
bCnctCompPort = do
  pl <- iden
  char '.'
  c <- iden
  char '.'
  pr <- iden
  return $ BPin (Port pl) (Comp c) (Port pr)

bCnctWire :: Parser BCnct
bCnctWire = do
  w <- iden
  return $ BWire w

rCnct :: Parser RCnct
rCnct = do
  result <- try rCnctCompPort <|> rCnctWire
  charSp '-'
  return result

lCnct :: Parser LCnct
lCnct = try lCnctCompPort <|> lCnctWire

bCnct :: Parser BCnct
bCnct = do
  result <- try bCnctCompPort <|> bCnctWire
  charSp '-'
  return result

conExpr :: Parser (ModuleElement,SourcePos)
conExpr = do
  pos <- getPosition
  r  <- rCnct
  bs <- many $ try bCnct
  l <- lCnct
  charSp ';'
  return $ ((ConExpr r bs l),pos)

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

intLit :: Parser Int
intLit = do
  spaces
  result <- many1 digit
  spaces
  return $ read result

iden :: Parser String
iden = do
  spaces
  headLetter <- letter_
  tails <- many (letter_ <|> digit)
  spaces
  return (headLetter : tails)

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