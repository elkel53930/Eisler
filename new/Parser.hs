import Types
import qualified Data.Map as Map
import Text.ParserCombinators.Parsec
import Text.Parsec.Pos
import Text.Parsec.Char

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

{-
parseEisler :: FilePath -> IO(Either ParseError Src)
parseEisler file = do
  handle <- openFile file ReadMode
  source <- hGetContents handle
  return $ parse eislerFile file source

eislerFile :: Parser [SourceElement]
eislerFile = many ( try parseDfPa <|>
                    try parseDfMo <|>
                    try parseDcMo <|>
                    try parseDcWi <|>
                    try parseDcIf <|>
                    try parseDcPa )
-}
parseDfPa :: Parser SourceElement
parseDfPa = do
  stringSp kwdDefPart
  name <- iden
  charSp '('
  ports <- sepBy parsePortAliases $ char ','
  charSp ')'
  charSp '{'
  props <- parseProperties
  charSp '}'
  charSp ';'
  return . SeDefPart $ DfPa name ports props

parseProperties :: Parser [Property]
parseProperties = sepBy parseProperty $ char ','
  where
    parseProperty = do
      iden <- parsePropertyItem
      charSp '='
      value <- strLit
      return $ (iden,value)
    parsePropertyItem = try (stringSp kwdPropertyRef) <|>
                        try (stringSp kwdPropertyType) <|>
                        try (stringSp kwdPropertyValue) <|>
                        try (stringSp kwdPropertyModel) <|>
                        try (stringSp kwdPropertyMani) <|>
                        (stringSp kwdPropertyDscr)

parsePortAliases :: Parser PortAlias
parsePortAliases = sepBy iden $ char ':'

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
  idens <- many (letter_ <|> digit)
  spcmnt
  return $ Token idens pos

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
