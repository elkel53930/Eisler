import Types
import qualified Data.Map as Map
import Text.ParserCombinators.Parsec hiding(try)
import Text.Parsec
import Text.Parsec.Pos
import Text.Parsec.Char
import Control.Lens
import System.IO

kwdDefModule = "defmodule"
kwdDefPart = "defpart"
kwdDecPart = "part"
kwdDecMod = "module"
kwdAs = "as"
kwdImport = "import"
kwdDecWire = "wire"
kwdDecItfc = "interface"
kwdPropertyRef = "ref"
kwdPropertyType = "type"
kwdPropertyValue = "value"
kwdPropertyModel = "model"
kwdPropertyMani = "mani"
kwdPropertyDscr = "dscr"

data SourceElement = SeDefPart DfPa
                   | SeDefMod DfMo
                   | SeDecMod DcMo
                   | SeDecWire DcWi
                   | SeDecItfc DcIf
                   | SeDecPart DcPa deriving (Show)

data ModuleElement = MeDecMod DcMo
                   | MeDecPart DcPa
                   | MeDecWire DcWi
                   | MeDecItfc DcIf
                   | MeExpr Expr deriving (Show)


--parseEisler :: FilePath -> IO(Either ParseError Src)
parseEisler file = do
  handle <- openFile file ReadMode
  source <- hGetContents handle
  return $ parse parseSrc file source

-- Src
assortSourceElement :: [SourceElement] -> ([DfPa],[DfMo],[DcPa],[DcMo],[DcWi],[DcIf])
assortSourceElement [] = ([],[],[],[],[],[])
assortSourceElement ((SeDefPart dfpa):xs) = (assortSourceElement xs) & _1 %~ (dfpa:)
assortSourceElement ((SeDefMod  dfmo):xs) = (assortSourceElement xs) & _2 %~ (dfmo:)
assortSourceElement ((SeDecPart dcpa):xs) = (assortSourceElement xs) & _3 %~ (dcpa:)
assortSourceElement ((SeDecMod  dcmo):xs) = (assortSourceElement xs) & _4 %~ (dcmo:)
assortSourceElement ((SeDecWire dcwi):xs) = (assortSourceElement xs) & _5 %~ (dcwi:)
assortSourceElement ((SeDecItfc dcif):xs) = (assortSourceElement xs) & _6 %~ (dcif:)

parseSrc :: Parser [SourceElement]
parseSrc =    many ( parseDfPa <|>
                     parseDfMo <|>
                     parseGDcPa <|>
                     parseGDcWi <|>
                     parseGDcIf <|>
                     parseGDcMo )


{-
parseSrc :: Parser Src
parseSrc = do
  srcElems <- many1(try parseDfMo <|>
                    try parseDfPa <|>
                    try parseGDcPa <|>
                    try parseGDcWi <|>
                    try parseGDcIf <|>
                     parseGDcMo )
  let (dfpas, dfmos, dcpas, dcmos, dcwis, dcifs) = assortSourceElement srcElems
  return $ Src dfpas dfmos dcpas dcmos dcwis dcifs
-}

-- Define Part

parseDfPa :: Parser SourceElement
parseDfPa = do
  try $ stringSp kwdDefPart
  name <- iden
  charSp '('
  ports <- sepBy parsePortAliases $ char ','
  charSp ')'
  props <- option [] parseProperties
  return . SeDefPart $ DfPa name ports props

-- Define Module

assortModuleElement :: [ModuleElement] -> ([DcPa],[DcMo],[DcWi],[DcIf],[Expr])
assortModuleElement [] = ([],[],[],[],[])
assortModuleElement ((MeDecPart dcpa):xs) = (assortModuleElement xs) & _1 %~ (dcpa:)
assortModuleElement ((MeDecMod  dcmo):xs) = (assortModuleElement xs) & _2 %~ (dcmo:)
assortModuleElement ((MeDecWire dcwi):xs) = (assortModuleElement xs) & _3 %~ (dcwi:)
assortModuleElement ((MeDecItfc dcif):xs) = (assortModuleElement xs) & _4 %~ (dcif:)
assortModuleElement ((MeExpr    expr):xs) = (assortModuleElement xs) & _5 %~ (expr:)

parseDfMo :: Parser SourceElement
parseDfMo = do
  try $ stringSp kwdDefModule
  name <- iden
  charSp '('
  ports <- sepBy parsePortAliases $ char ','
  charSp ')'
  charSp '{'
  modElems <- many parseModuleElements
  charSp '}'
  let (dcpas, dcmos, dcwis, dcifs, exprs) = assortModuleElement modElems
  return . SeDefMod $ DfMo name ports dcpas dcmos dcwis dcifs exprs
  where
    parseModuleElements = try parseExpr <|>
                          ( parseLDcMo <|>
                            parseLDcPa <|>
                            parseLDcWi <|>
                            parseLDcIf )


-- Declare wire
parseLDcWi :: Parser ModuleElement
parseLDcWi = do
  res <- parseDcWi
  return $ MeDecWire res

parseGDcWi :: Parser SourceElement
parseGDcWi = do
  res <- parseDcWi
  return $ SeDecWire res

parseDcWi :: Parser DcWi
parseDcWi = do
  try $ stringSp kwdDecWire
  names <- sepBy1 iden $ char ','
  charSp ';'
  return $ DcWi names


-- Declare Interface
parseLDcIf :: Parser ModuleElement
parseLDcIf = do
  res <- parseDcIf
  return $ MeDecItfc res

parseGDcIf :: Parser SourceElement
parseGDcIf = do
  res <- parseDcIf
  return $ SeDecItfc res


parseDcIf :: Parser DcIf
parseDcIf = do
  try $ stringSp kwdDecItfc
  names <- sepBy1 iden $ char ','
  charSp ';'
  return $ DcIf names

-- Declare part

-- for Glocal
parseGDcPa :: Parser SourceElement
parseGDcPa = do
  res <- parseDcPa
  return $ SeDecPart res

-- for Local
parseLDcPa :: Parser ModuleElement
parseLDcPa = do
  res <- parseDcPa
  return $ MeDecPart res

parseDcPa :: Parser DcPa
parseDcPa = do
  try $ stringSp kwdDecPart
  names <- sepBy1 iden $ char ','
  props <- option [] parseProperties
  stringSp kwdAs
  part <- iden;
  charSp ';'
  return $ DcPa names part props

-- Declare module

-- for Global
parseGDcMo :: Parser SourceElement
parseGDcMo = do
  res <- parseDcMo
  return $ SeDecMod res

-- for Local
parseLDcMo :: Parser ModuleElement
parseLDcMo = do
  res <- parseDcMo
  return $ MeDecMod res

parseDcMo :: Parser DcMo
parseDcMo = do
  try $ stringSp kwdDecMod
  names <- sepBy1 iden $ char ','
  stringSp kwdAs
  m <- iden
  charSp ';'
  return $ DcMo names m

-- Properties

parseProperties :: Parser [Property]
parseProperties = do
  charSp '{'
  props <- sepBy parseProperty $ char ','
  charSp '}'
  return props
  where
    parseProperty = do
      iden <- parsePropertyItem
      charSp '='
      value <- strLit
      return $ (iden,value)
    parsePropertyItem = try (stringSp kwdPropertyModel) <|>
                        try (stringSp kwdPropertyMani) <|>
                        try (stringSp kwdPropertyType) <|>
                        try (stringSp kwdPropertyValue) <|>
                        try (stringSp kwdPropertyRef) <|>
                        (stringSp kwdPropertyDscr)

-- Port Aliases

parsePortAliases :: Parser PortAlias
parsePortAliases = sepBy iden $ char ':'

-- Expression

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

parseExpr :: Parser ModuleElement
parseExpr = do
  r  <- rCnct
  bs <- many $ try bCnct
  l <- lCnct
  charSp ';'
  return . MeExpr $ Expr r bs l

{-
  Common
-}

strLit :: Parser StrLit
strLit = do
  spcmnt
  pos <- getPosition
  char '"'
  result <- many $ Text.Parsec.Char.noneOf "\""
  char '"'
  spcmnt
  return $ Token result pos

iden :: Parser Identify
iden = do
  spcmnt
  pos <- getPosition
  idens <- many1 (letter <|> digit <|> char '_')
  spcmnt
  return $ Token idens pos

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
