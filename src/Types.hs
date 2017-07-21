module Types where

import Text.ParserCombinators.Parsec
import Text.Parsec.Pos
import Data.List
import qualified Data.Set as Set

data Cnct = Pin CompIden PortIden
          | Wire WireIden deriving Show
data BCnct = BPin PortIden CompIden PortIden
           | BWire WireIden deriving Show

data Token a = Token a SourcePos deriving Show
type Identify = Token String
type StrLit = Token String
type IntLit = Token Int
type WireIden = Identify
type ItfcIden = Identify
type PartIden = Identify
type PortIntLit = IntLit
type PortIden = Identify
type ModuleIden = Identify
type CompIden = Identify
type PartType = StrLit
type ImpFile = StrLit

type Reference = String
type WireName = String
type PartName = String
type PortNum = Int
type PortName = String
type ModuleName = String
type CompName = String

type DefinePart = (PartIden, ([(PortIntLit,PortIden)], Reference))
type DefineModule = (ModuleIden, ([(PortIntLit,PortIden)], [ModuleElement]))
type DeclarePart = ([CompIden], PartIden, Maybe PartType)
type DeclareModule = ([CompIden], ModuleIden)
type ConnectExpression = (Cnct, [BCnct], Cnct)

data SourceElement = Import ImpFile
                   | DefPart DefinePart
                   | DefMod DefineModule
                   | DecGPart DeclarePart deriving Show

data ModuleElement = DecLMod DeclareModule
                   | DecLPart DeclarePart
                   | DecLWire [WireIden]
                   | DecLItfc [ItfcIden]
                   | ConExpr ConnectExpression deriving Show

newtype Net = Net { getNet :: (WireIden, Set.Set Connectable)} deriving (Show,Eq)
data Connectable = ConWire WireIden
                 | ConItfc ItfcIden
                 | ConPort CompIden PortIntLit PortIden Reference PartIden (Maybe PartType) deriving (Show,Eq,Ord)
type ErrorMsg = String
type Result = Either ErrorMsg
type Suffix = String


instance Eq a => Eq (Token a) where
  Token name1 _ == Token name2 _ = name1 == name2
instance Ord a => Ord (Token a) where
  (Token name1 _) < (Token name2 _) = name1 < name2
  (Token name1 _) > (Token name2 _) = name1 > name2
  (Token name1 _) <= (Token name2 _) = name1 <= name2
  (Token name1 _) >= (Token name2 _) = name1 >= name2


showPos :: Token a -> String
showPos (Token _ pos) = show pos

getToken :: Token a -> a
getToken (Token a _) = a

newToken :: a -> Token a
newToken a = Token a $ newPos "Internal" 0 0

(.==) :: Eq a => Token a -> a -> Bool
(Token x _) .== y = x == y
