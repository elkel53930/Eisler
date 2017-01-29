import qualified Parser as P
import qualified Semantics as S
import qualified Output as O
import Common
import Text.ParserCombinators.Parsec
import System.Environment
import System.IO
import qualified Combine as Comb

main = do
  args <- getArgs
  case (length args) of
    0 -> putStrLn "Eisler translator to PADS Logic netlist."
    otherwise -> translate args

translate args = do
  -- IO
  let sourceName = head args
  handle <- openFile sourceName ReadMode
  source <- hGetContents handle
  case do
    -- Result = Either ErrorMsg
    parsed  <- P.parseEisFile sourceName source
    srcElems <- S.checkOverlap parsed
    let (defParts,defMods) = S.divideSrc srcElems
    (_,(_,modElems)) <- S.searchMod defMods "main"
    let (decParts,conExprs) = S.divideMod modElems
    let cncts = S.expansionaCnct conExprs
    ports <- S.convertToPort decParts defParts cncts
    let refed = S.referencing ports
    Right $ (Comb.adds [] refed, refed) of
      Right (combined,refed) -> do
        putStrLn $ O.header
        putStrLn $ O.outputPart refed
        putStrLn $ O.outputNet combined
        putStrLn "*END*     OF ASCII OUTPUT FILE"
      Left l -> putStrLn l
