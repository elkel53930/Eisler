module Parser where

import Text.ParserCombinators.Parsec
import System.IO
import Text.Parsec.Pos
import Text.Parsec.Char

data Cnct = Pin CompIden PortIden
          | Wire WireIden deriving Show
data BCnct = BPin PortIden CompIden PortIden
           | BWire WireIden deriving Show

data Token a = Token a SourcePos deriving Show
type Identify = Token String
type StrLit = Token String
type IntLit = Token Int
type WireIden = Identify
type ItfcIden = Identify
type PartIden = Identify
type PortIntLit = IntLit
type PortIden = Identify
type ModuleIden = Identify
type CompIden = Identify
type PartType = StrLit
type ImpFile = StrLit

type Reference = String
type WireName = String
type PartName = String
type PortNum = Int
type PortName = String
type ModuleName = String
type CompName = String

type DefinePart = (PartIden, ([(PortIntLit,PortIden)], Reference))
type DefineModule = (ModuleIden, ([(PortIntLit,PortIden)], [ModuleElement]))
type DeclarePart = ([CompIden], PartIden, Maybe PartType)
type DeclareModule = ([CompIden], ModuleIden)
type ConnectExpression = (Cnct, [BCnct], Cnct)

data SourceElement = Import ImpFile
                   | DefPart DefinePart
                   | DefMod DefineModule deriving Show

data ModuleElement = DecMod DeclareModule
                   | DecPart DeclarePart
                   | DecWire [WireIden]
                   | DecItfc [ItfcIden]
                   | ConExpr ConnectExpression deriving Show

instance Eq a => Eq (Token a) where
  Token name1 _ == Token name2 _ = name1 == name2
instance Ord a => Ord (Token a) where
  (Token name1 _) < (Token name2 _) = name1 < name2
  (Token name1 _) > (Token name2 _) = name1 > name2
  (Token name1 _) <= (Token name2 _) = name1 <= name2
  (Token name1 _) >= (Token name2 _) = name1 >= name2

kwdDefModule = "defmodule"
kwdDefPart = "defpart"
kwdDecPart = "part"
kwdDecMod = "module"
kwdAs = "as"
kwdImport = "import"
kwdDecWire = "wire"
kwdDecItfc = "interface"

showPos :: Token a -> String
showPos (Token _ pos) = show pos

getToken :: Token a -> a
getToken (Token a _) = a

newToken :: a -> Token a
newToken a = Token a $ newPos "Internal" 0 0

(.==) :: Eq a => Token a -> a -> Bool
(Token x _) .== y = x == y

parseEisFile :: FilePath -> IO(Either ParseError [SourceElement])
parseEisFile filepath = parseEisFiles [newToken filepath] []

parseEisFiles :: [ImpFile] -> [SourceElement] -> IO(Either ParseError [SourceElement])
parseEisFiles [] srcElems = return $ Right srcElems
parseEisFiles (f:fs) srcElems = do
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
  where file = getToken f


pickupImport :: [SourceElement] -> [ImpFile]
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
  elems <- many ( try decWire <|>
                  try decPart <|>
                  try decItfc <|>
                  try decModule <|>
                  conExpr)
  charSp '}'
  return $ DefMod (m, (ps, elems))

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
  return $ DefPart (p, (ps, ref))

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
  return $ getToken result

port :: Parser (PortIntLit,PortIden)
port = do
  n <- intLit
  charSp ':'
  s <- iden
  return (n,s)

{-
  part/module/wire declare
-}

decPart :: Parser ModuleElement
decPart = do
  stringSp kwdDecPart
  c <- sepBy1 iden $ char ','
  t <- optionMaybe strLit
  stringSp kwdAs
  p <- iden
  charSp ';'
  return $ DecPart (c,p,t)

decWire :: Parser ModuleElement
decWire = do
  stringSp kwdDecWire
  w <- sepBy1 iden $ char ','
  charSp ';'
  return $ DecWire w

decItfc :: Parser ModuleElement
decItfc = do
  stringSp kwdDecItfc
  i <- sepBy1 iden $ char ','
  charSp ';'
  return $ DecItfc i

decModule :: Parser ModuleElement
decModule = do
  stringSp kwdDecMod
  c <- sepBy1 iden $ char ','
  stringSp kwdAs
  m <- iden
  charSp ';'
  return $ DecMod (c,m)

{-
  ConExpr
-}

rCnctCompPort :: Parser Cnct
rCnctCompPort = do
  c <- iden
  char '.'
  p <- iden
  return $ Pin c p

lCnctCompPort :: Parser Cnct
lCnctCompPort = do
  p <- iden
  char '.'
  c <- iden
  return $ Pin c p

wire :: Parser Cnct
wire = do
  l <- iden
  return $ Wire l

bCnctCompPort :: Parser BCnct
bCnctCompPort = do
  pl <- iden
  char '.'
  c <- iden
  char '.'
  pr <- iden
  return $ BPin pl c pr

bWire :: Parser BCnct
bWire = do
  l <- iden
  return $ BWire l

rCnct :: Parser Cnct
rCnct = do
  result <- try rCnctCompPort <|> wire
  charSp '-'
  return result

lCnct :: Parser Cnct
lCnct = try lCnctCompPort <|> wire

bCnct :: Parser BCnct
bCnct = do
  result <- try bCnctCompPort <|> bWire
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

strLit :: Parser StrLit
strLit = do
  spcmnt
  pos <- getPosition
  char '"'
  result <- many $ noneOf "\""
  char '"'
  spcmnt
  return $ Token result pos

intLit :: Parser IntLit
intLit = do
  spcmnt
  pos <- getPosition
  result <- many1 digit
  spcmnt
  return $ Token (read result) pos

iden :: Parser Identify
iden = do
  spcmnt
  pos <- getPosition
  headLetter <- letter_
  tails <- many (letter_ <|> digit)
  spcmnt
  return $ Token (headLetter : tails) pos

letter_ :: Parser Char
letter_ = letter <|> char '_'

charSp :: Char -> Parser String
charSp c = do
  spcmnt
  result <- char c
  spcmnt
  return [c]

stringSp :: String -> Parser String
stringSp s = do
  spcmnt
  result <- string s
  spcmnt
  return s

spcmnt :: Parser String
spcmnt = do
  result <- many (sp <|> try blockCmnt <|> try lineCmnt)
  return $ concat result

sp :: Parser String
sp = do
  result <- many1 space
  return result

blockCmnt :: Parser String
blockCmnt = do
  string "/*"
  cmnt <- blockCmntEnd
  return cmnt

blockCmntEnd :: Parser String
blockCmntEnd = do
  cmnt <-
    try( do{
      string "*/";
      return "";
      }) <|>
    do{
      c<-anyChar;
      cs<-blockCmntEnd;
      return $ c:cs;
    }
  return cmnt

lineCmnt :: Parser String
lineCmnt = do
  string "//"
  cmnt <- lineCmntEnd
  return cmnt


lineCmntEnd :: Parser String
lineCmntEnd = do
  cmnt <-
    try( do{
      eol;
      return "";
      }) <|>
    do{
      c<-anyChar;
      cs<-lineCmntEnd;
      return $ c:cs;
    }
  return cmnt

eol :: Parser String
eol =   try (string "\n\r")
    <|> try (string "\r\n")
    <|> string "\n"
    <|> string "\r"
