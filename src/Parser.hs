module Parser where

import Text.ParserCombinators.Parsec

data Cnct = Pin CompIden PortIden deriving Show
data BCnct = BPin PortIden CompIden PortIden deriving Show

type PartIden = (PartName,SourcePos)
type PortIntLit = (PortNum,SourcePos)
type PortIden = (PortName,SourcePos)
type ModuleIden = (ModuleName,SourcePos)
type Reference = String
type CompIden = (CompName,SourcePos)

type PartName = String
type PortNum = Int
type PortName = String
type ModuleName = String
type CompName = String

type DefinePart = (PartIden, ([(PortIntLit,PortIden)], Reference))
type DefineModule = (ModuleIden, ([(PortIntLit,PortIden)], [ModuleElement]))
type DeclarePart = (CompIden, PartIden)
type ConnectExpression = (Cnct, [BCnct], Cnct)

data SourceElement = DefPart DefinePart
                   | DefMod DefineModule deriving Show

data ModuleElement = DecPart DeclarePart
                   | ConExpr ConnectExpression deriving Show

kwdDefModule = "defmodule"
kwdDefPart = "defpart"
kwdDecPart = "part"
kwdAs = "as"

parseEisFile filename src =
  case parse eislerFile filename src of
    Right parsed -> Right parsed
    Left err -> Left $ show err

eislerFile :: Parser [SourceElement]
eislerFile = many ( try defPart <|>
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
  return (n,s)

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
  return $ DecPart (c, p)

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

bCnctCompPort :: Parser BCnct
bCnctCompPort = do
  pl <- iden
  char '.'
  c <- iden
  char '.'
  pr <- iden
  return $ BPin pl c pr

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
