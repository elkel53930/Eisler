module Semantic.Semantic ( Module
                , Part
                , Port
                , Wire
                , PartName
                , ModuleName
                ) where

data Module = Module ModuleName [ ( Wire, [(Part,Port)], [(Module,Port)] ) ]
data Part = Part PartName [ Port ]
type Port = Int
type Wire = String
type PartName = String
type ModuleName = String
