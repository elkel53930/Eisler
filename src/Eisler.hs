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
    Right parsed ->
      case do
        -- Result = Either ErrorMsg
        srcElems <- S.checkOverlap parsed
        let (defParts,defMods) = S.divideSrc srcElems
        (_,(_,modElems)) <- S.searchMod defMods "main"
        let (wires,decParts,conExprs) = S.divideMod modElems
        let cncts = S.expansionaCnct conExprs
        ports <- S.convertToPort decParts defParts cncts
        let refed = S.referencing ports
        nets <- S.combineConnectables [] refed
        let named = S.namingWire nets 1
        Right $ Kicad.output named
          of
          Right kicad -> do
            putStrLn kicad
          Left l -> putStrLn l
    Left err -> putStrLn $ show err

{-
printNet :: [S.Net] -> String
printNet [] = ""
printNet (n:ns) = wire ++ "\n" ++ printConnectables cs ++ printNet ns where
  wire = P.getToken wireIden
  cs = Set.elems cSet
  S.Net(wireIden,cSet) = n

printCC :: [(S.Connectable,S.Connectable)] -> String
printCC [] = ""
printCC ((c1,c2):cs) = printConnectable c1 ++ printConnectable c2 ++ "\n" ++ printCC cs

printConnectables :: [S.Connectable] -> String
printConnectables [] = ""
printConnectables (c:cs) =printConnectable c ++ printConnectables cs

printConnectable :: S.Connectable -> String
printConnectable (S.ConPort compIden portIntLit ref partIden) =
  "  (" ++ comp ++
  " " ++ port ++
  " " ++ ref ++
  " " ++ part ++
  ")\n" where
    comp = P.getToken compIden
    port = show $ P.getToken portIntLit
    part = P.getToken partIden

printConnectable (S.ConWire wireIden) =
  "  (" ++ wire ++ ")\n" where wire = P.getToken wireIden
-}
