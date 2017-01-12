import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Pos


testParser :: Parser (String,SourcePos)
testParser = do
  str <- many1 alphaNum
  pos <- getPosition
  return (str,pos)
