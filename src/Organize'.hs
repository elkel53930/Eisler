{-
このモジュールがやること。
mainモジュールを起点にして[SourceElement]からResult [(Connectable,Connectable)]への変換

* すべてのモジュール内で、
** そのモジュール内で識別子の宣言が重複していないかをチェック
** その識別子がさしているものがWireなのか、Itfcなのか、Partなのか、Moduleなのかを確定する
*** PartやModuleだった場合は、Portの一覧など宣言に関する情報もまとめて保持する


-}

module Organize'(organize) where

import Types
import Common

type CallFrom = String  -- どのモジュールから呼ばれているか
data Compoment = Wire WireIden [CallFrom]
               | Itfc ItfcIden [CallFrom]
               | Part PartIden [CallFrom] DeclarePart DefinePart
               | Module ModuleIden [CallFrom] DeclareModule DefineModule
{-
CallFromのリストは、どのモジュールから呼ばれているかを格納している
mainモジュール内のledモジュール内のrなら[CallFrom]の中身は["led","main"]
-}
