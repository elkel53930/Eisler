import qualified Parser as P
import qualified Semantics as S
import qualified Output as O
import Common
import Text.ParserCombinators.Parsec
import System.Environment
import System.IO
import qualified Combine as Comb
import qualified Data.Time as Time

main = do
  args <- getArgs
  now <- Time.getZonedTime
  putStrLn $ show now
  case (length args) of
    0 -> putStrLn "Eisler translator to PADS Logic netlist."
    otherwise -> translate args

translate args = do
  -- IO()
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
    Right $ Comb.adds [] ports of
      Right r -> print r
      Left l -> putStrLn l
