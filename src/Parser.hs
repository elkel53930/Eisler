module Parser(parseEis) where

import Types
import Text.ParserCombinators.Parsec
import System.IO
import Text.Parsec.Pos
import Text.Parsec.Char

kwdDefModule = "defmodule"
kwdDefPart = "defpart"
kwdDecLPart = "part"
kwdDecLMod = "module"
kwdAs = "as"
kwdImport = "import"
kwdDecLWire = "wire"
kwdDecLItfc = "interface"

parseEis :: FilePath -> IO(Either ParseError [SourceElement])
--parseEis filepath = parseEisFiles [newToken filepath] []
parseEis file = do
  handle <- openFile file ReadMode
  source <- hGetContents handle
  return $ parse eislerFile file source
{-
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
-}

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
  elems <- many ( try decLWire <|>
                  try decLPart <|>
                  try decLItfc <|>
                  try decLModule <|>
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

decPart :: Parser DeclarePart
decPart = do
  stringSp kwdDecLPart
  c <- sepBy1 iden $ char ','
  t <- optionMaybe strLit
  stringSp kwdAs
  p <- iden
  charSp ';'
  return (c,p,t)

decLPart :: Parser ModuleElement
decLPart = do
  dp <- decPart
  return $ DecLPart dp

decWire :: Parser [WireIden]
decWire = do
  stringSp kwdDecLWire
  w <- sepBy1 iden $ char ','
  charSp ';'
  return w

decLWire :: Parser ModuleElement
decLWire = do
  w <- decWire
  return $ DecLWire w


decItfc :: Parser [ItfcIden]
decItfc = do
  stringSp kwdDecLItfc
  i <- sepBy1 iden $ char ','
  charSp ';'
  return i

decLItfc :: Parser ModuleElement
decLItfc = do
  i <- decItfc
  return $ DecLItfc i

decModule :: Parser DeclareModule
decModule = do
  stringSp kwdDecLMod
  c <- sepBy1 iden $ char ','
  stringSp kwdAs
  m <- iden
  charSp ';'
  return (c,m)

decLModule :: Parser ModuleElement
decLModule = do
  dm <- decModule
  return $ DecLMod dm

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
