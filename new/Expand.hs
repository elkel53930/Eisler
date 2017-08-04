module Expand where

{-
  サブモジュールを展開する。
  Src -> Src

  mainモジュールからスタートして、
    モジュール内のDcMoの内容をDcPa, DcWi, DcIf, Exprに展開する
    このとき、識別子に"@{サブモジュール名}"を追加する
-}

import Types
import Control.Lens
import Data.List

{-
  SrcのDfMoの中からModNameという名前のモジュールを見つける。
  そのモジュールの中にあるDcMoの中で宣言されている
  Part, Wire, Interface,
  その際、識別子(DcPa, DcWi, DcIf, )
-}
expand ::　Prefix -> ModName -> Src -> ExSrc
expand p m s = ExSrc (s^.srcDefPart) dcpas dcwis dcifs exprs
where



expandDcPa :: [DfMo] -> DfMo -> [DcPa]
expandDcPa ms m =
