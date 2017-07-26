{-# LANGUAGE TemplateHaskell #-}

module Types where

import Text.ParserCombinators.Parsec
import Text.Parsec.Pos
import Data.List
import Control.Lens
import qualified Data.Set as Set
import qualified Data.Map as Map

data Token a = Token a SourcePos deriving Show
type Identify = Token String
type StrLit = Token String
type IntLit = Token Int

type WireIden = Identify
type ItfcIden = Identify
type PartIden = Identify
type PortIntLit = IntLit
type PortIden = Identify
type ModIden = Identify
type CompIden = Identify
type PartType = StrLit
type ImpFile = StrLit


data Cnct = Pin { pinCompName :: CompIden
                , pinPortName :: PortIden }
          | Wire { wireWireName :: WireIden } deriving Show

data BCnct = BPin { bpinLeftPortName :: PortIden
                  , bpinCompName :: CompIden
                  , bpinRightPortName :: PortIden }
           | BWire { bwireWireName :: WireIden } deriving Show

--data PropertyItem = PropRef | PropType | PropValue | PropModel | PropMani | PropDscr deriving (Show, Eq, Ord)

type PropertyValue = StrLit
type PropertyIden = String
type Property = (PropertyIden, PropertyValue)

type Reference = String
type WireName = String
type PartName = String
type PortName = String
type ModName = String
type CompName = String
type Prefix = String

type PortAlias = [PortIden]

data DfPa = DfPa { _dfpaName :: PartIden
                 , _dfpaPortList :: [PortAlias]
                 , _dfpaProperties :: [Property] } deriving Show
makeLenses ''DfPa

data DcPa = DcPa { _dcpaNames :: [CompIden]
                 , _dcpaPartName :: PartIden
                 , _dcpaProperties :: [Property] } deriving Show
makeLenses ''DcPa

data DcMo = DcMo { _dcmoNames :: [CompIden]
                 , _dcmoModName :: ModIden } deriving Show
makeLenses ''DcMo

data DcWi = DcWi { _dcwiNames :: [Identify]} deriving Show
makeLenses ''DcWi

data DcIf = DcIf { _dcifNames :: [ItfcIden]} deriving Show
makeLenses ''DcIf

data Expr = Expr { _exprRight :: Cnct
                 , _exprBoth :: [BCnct]
                 , _exprLeft :: Cnct } deriving Show
makeLenses ''Expr

data DfMo = DfMo { _dfmoName :: ModIden
, _dfmoPortList :: [PortAlias]
, _dfmoDecPart :: [DcPa]
, _dfmoDecMod :: [DcMo]
, _dfmoDecWire :: [DcWi]
, _dfmoDecItfc :: [DcIf]
, _dfmoExpr :: [Expr] } deriving Show
makeLenses ''DfMo

data Src = Src { _srcDefPart :: [DfPa]
               , _srcDefMod :: [DfMo]
               , _srcDecPart :: [DcPa]
               , _srcDecMod :: [DcMo]
               , _srcDecWire :: [DcWi]
               , _srcDecItfc :: [DcIf]
               } deriving Show
makeLenses ''Src

data ExSrc = ExpSrc { _exsrcDefPart :: [DfPa]
                    , _exsrcDecPart :: [DcPa]
                    , _exsrcDecWire :: [DcWi]
                    , _exsrcDcItfc :: [DcIf]
                    , _exsrcExpr :: [Expr]
                    } deriving Show
makeLenses ''ExSrc

data Connectable = ConWire WireIden
                 | ConItfc ItfcIden
                 | ConPort CompIden PortIntLit PortIden Reference PartIden (Maybe PartType) deriving (Show)

data Net = Net { _netWireName :: WireIden
               , _netConnectableSet :: Set.Set Connectable } deriving (Show)
makeLenses ''Net

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

(==.) :: Eq a => a -> Token a -> Bool
y ==. (Token x _) = y == x
