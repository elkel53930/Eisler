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

type PortAlias = [PortIden]

data DfPa = DfPa { dfpaName :: PartIden
                 , dfpaPortList :: [PortAlias]
                 , dfpaProperties :: [Property] } deriving Show

data DfMo = DfMo { dfmoName :: ModIden
                 , dfmoPortList :: [PortAlias]
                 , dfmoDecPart :: [DcPa]
                 , dfmoDecMod :: [DcMo]
                 , dfmoDecWire :: [DcWi]
                 , dfmoDecItfc :: [DcIf]
                 , dfmoExpr :: [Expr] } deriving Show

data DcPa = DcPa { dcpaNames :: [CompIden]
                 , dcpaPartName :: PartIden
                 , dcpaProperties :: [Property] } deriving Show

data DcMo = DcMo { dcmoNames :: [CompIden]
                 , dcmoModName :: ModIden } deriving Show

data DcWi = DcWi { dcwiNames :: [Identify]} deriving Show

data DcIf = DcIf { dcifNames :: [ItfcIden]} deriving Show

data Expr = Expr { exprRight :: Cnct
                 , exprBoth :: [BCnct]
                 , exprLeft :: Cnct } deriving Show

data Src = Src { srcDefPart :: [DfPa]
               , srcDefMod :: [DfMo]
               , srcDecMod :: [DcMo]
               , srcDecWire :: [DcWi]
               , srcDecItfc :: [DcIf]
               , srcDecPart :: [DcPa]
               } deriving Show

data Net = Net { netWireName :: WireIden
               , netConnectableSet :: Set.Set Connectable } deriving (Show,Eq)

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
