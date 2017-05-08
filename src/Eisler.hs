import qualified Parser as P
import qualified Semantics as S
import qualified Kicad as Kicad
import qualified Bom as Bom
--import qualified Output as O
import Common
import Text.ParserCombinators.Parsec
import System.Environment
import System.IO
import System.Directory
import Data.List
import qualified Data.Set as Set
import Control.Exception

main = do
  args <- getArgs
  case (length args) of
    0 -> putStrLn "Eisler translator to KiCad legacy netlist."
    otherwise -> translate args

translate args = do
  -- IO
  result <- P.parseEisFile $ eisFile
  case result of
    Right parsed -> case do
      -- Result = Either ErrorMsg
      ports <- S.moduleNet parsed "main"
      nets <- S.combineConnectables [] $ S.referencing ports
      return nets
      of
        Right nets -> do
          writeFile (changeExt "net" eisFile) $ Kicad.output ( S.namingWire nets 1 )
          writeFile (changeExt "csv" eisFile) $ Bom.bomShow nets
        Left l      -> putStrLn l
    Left err -> putStrLn $ show err
  where
    eisFile = head args

changeExt :: String -> FilePath -> FilePath
changeExt newExt file = (reverse $ drop len rfile) ++ newExt where
  len = length $ takeWhile (/='.') rfile
  rfile = reverse file
