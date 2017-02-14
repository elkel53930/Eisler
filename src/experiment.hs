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
