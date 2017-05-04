{-
import System.Environment
import System.FilePath
import System.IO

main = do
  args <- getArgs
  let file = head args
  prog <- getExecutablePath
  let path = splitPath prog
  let filename = (++file) . concat $ init path
  handle <- openFile filename ReadMode
  source <- hGetContents handle
  putStrLn source
-}

import Text.ParserCombinators.Parsec

comment :: Parser String
comment = do
  string "/*"
  cmnt <- commentEnd
  return cmnt

commentEnd :: Parser String
commentEnd = do
  cmnt <-
    try( do{
      string "*/";
      return "";
      }) <|>
    do{
      c<-anyChar;
      cs<-commentEnd;
--      return cs;
      return $ c:cs;
    }
  return cmnt
