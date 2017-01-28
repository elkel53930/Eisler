```
type CompName = String
type PortNum = Int
type WireName = String
```

モジュール内の話に限れば、
「コンポーネント名、ポート番号」があればOK。`[P.CompName,P.PortNum]`

これとConExprの情報を合わせて、
「配線名、「コンポ名、ポート番号」 × たくさん」を作る。`[P.WireName,[P.CompName,P.PortNum]]`

```
type ConnectExpression = (Cnct, [BCnct], Cnct)
data Cnct = Pin CompIden PortIden
data BCnct = BPin PortIden CompIden PortIden
type Port = [CompName,PortNum]
type Wire = [WireName,[Port]]
```

`[Wire]`から`Port`を含む`Wire`を探して、その`Wire`とそれを取り除いた`[Wire]`を返す関数

```
pickupBy :: (Wire -> Bool) -> [Wire] -> ([Wire],Wire)
```

`Wire`どうしを結合する関数

```
concatWire :: Wire -> Wire -> Wire
```

[SourceElement]、モジュール名、コンポ名、ポート名からポートを返す関数

```
getPort :: [SourceElement] -> ModuleName -> CompName -> PortName -> Port
```

あー、コレMaybeかなぁ…。
エラーが出ない前提であればMaybeである必要はないんだけどなぁ…。
忰山さんに相談しよう。
相談した。エラーが出ない前提ならMaybeじゃなくてOK。事前条件だよね。

getPortはCnctからにしよう。

```
getPort :: [SourceElement] -> ModuleName -> Cnct -> Port
```

というか、SourceElementとModuleNameやめてModuleElementにしよう

```
getPort :: [ModuleElement] -> Cnct -> Port
```
