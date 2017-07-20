import Types
import qualified Parser as P
import qualified Combine as C
import qualified Organize as O
import qualified Referencing as R
import qualified Kicad as Kicad
import qualified Pads as Pads
import qualified Bom as Bom
import qualified Summary as Summary
--import qualified Output as O
import Common
import Text.ParserCombinators.Parsec
import System.Environment
import System.IO
import System.Directory
import System.FilePath
import Data.List
import qualified Data.Set as Set
import Control.Exception

main = do
  args <- getArgs
  case (length args) of
    0 -> putStrLn "Eisler translator to KiCad legacy netlist."
    otherwise -> translate args

concatResult :: [Either ParseError [SourceElement]] -> Either ParseError [SourceElement]
concatResult [] = Right []
concatResult (Right elements:ss) =
  case concatResult ss of
    Left err -> Left err
    Right r -> Right $ elements ++ r
concatResult (Left err:ss) = Left err

translate args = do
  -- IO
  dirFiles <- getDirectoryContents $ takeDirectory eisFile
  let eisFiles = extensionFilter "eis" dirFiles
  result <- concatResult <$> mapM P.parseEis eisFiles
--  result <- P.parseEis $ eisFile
  case result of
    Right parsed -> case do
      -- Result = Either ErrorMsg
      ports <- O.organize parsed "main"
      nets <- C.combine [] $ R.referencing ports
      return nets
      of
        Right nets -> do
          writeFile (changeExt "net" eisFile) $ Kicad.output nets
          writeFile (changeExt "asc" eisFile) $ Pads.output nets
          writeFile (changeExt "csv" eisFile) $ Bom.output nets
          Summary.output eisFile "README.md" nets
        Left l      -> putStrLn l
    Left err -> putStrLn $ show err
  where
    eisFile = head args

getExtension :: String -> String
getExtension = reverse . takeWhile (/='.') . reverse

extensionFilter :: String -> [FilePath] -> [FilePath]
extensionFilter ext = filter ((==ext) . getExtension)
