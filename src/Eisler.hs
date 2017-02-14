import qualified Parser as P
import qualified Semantics as S
import qualified Kicad as Kicad
--import qualified Output as O
import Common
import Text.ParserCombinators.Parsec
import System.Environment
import System.IO
import Data.List
import qualified Data.Set as Set

main = do
  args <- getArgs
  case (length args) of
    0 -> putStrLn "Eisler translator to PADS Logic netlist."
    otherwise -> translate args

translate args = do
  -- IO
  let sourceName = head args
  result <- P.parseEisFile sourceName
  case result of
    Right parsed -> case do
      -- Result = Either ErrorMsg
      named <- S.moduleNet parsed "main"
      Right $ Kicad.output named of
        Right kicad -> putStrLn kicad
        Left l -> putStrLn l
    Left err -> putStrLn $ show err
