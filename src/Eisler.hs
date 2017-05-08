import qualified Parser as P
import qualified Semantics as S
import qualified Kicad as Kicad
import qualified Bom as Bom
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
    0 -> putStrLn "Eisler translator to KiCad legacy netlist."
    otherwise -> translate args

translate args = do
  -- IO
  result <- P.parseEisFile $ head args
  case result of
    Right parsed -> case do
      -- Result = Either ErrorMsg
      ports <- S.moduleNet parsed "main"
      nets <- S.combineConnectables [] $ S.referencing ports
      Left . show $ Bom.bomShow nets
      Right $ Kicad.output ( S.namingWire nets 1 ) of
        Right kicad -> putStrLn kicad
        Left l      -> putStrLn l
    Left err -> putStrLn $ show err
