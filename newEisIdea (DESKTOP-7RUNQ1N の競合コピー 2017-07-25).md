# Eisler2の文法

## 拡張子

「.e」

## import

importは廃止。同じディレクトリののeファイルは全部処理される

## コメント

```
// スラスラは行コメント
/* スラアスタはブロックコメント */
```

## defpart

### partの定義

```
defpart R(1,2){ref="R"} // giditのみからなる文字列は識別子扱い。
defpart Btt(Plus,Minus){ref="BT"} // なのでこういう書き方もOK
defpart SOP4(1:VCC, 2:IN, 3:OUT, 4:GND){ref="IC"} // ピン名のエイリアスは:でつなげて書く
```

複数のエイリアスがあってもOK。ex. `1:P14:MTIOC`

KiCad用のネットリストでは、最初の名前が「ピン番号」２つめの名前が「ピン名」として扱われる。
ピン番号は数字ではなく文字列なので注意。
ピン名がない場合は、ピン番号がピン名になる。

partにはプロパティがあって、それを`項目名="値"`の形で指定する。
プロパティには必須のものと任意のものがある。

* 必須のプロパティ
 * ref : リファレンス番号の接頭辞 ex. "R", IC", "LD", "Q"
* 任意のプロパティ
 * type : 部品のタイプ ex. "Chip resigter", "Linear regulater", "Mounting hole"
 * value : 部品の定数 ex. "110k 5% 1608", ""
 * model : 部品の型番 ex. "LTC4054L-4.2", "R5F5631PDDFL"
 * mani : 部品のメーカー ex. "JST", "Texas Instruments"
 * dscr : 備考 ex. "Not mount."

```
defpart LTC4054(1:CHRG, 2:GND, 3:BAT, 4:VCC, 5:PROG){
  ref   = "IC",
  type  = "LiPo charger",
  model = "LTC4054L-4.2",
  mani  = "Linear Technology",
  // 項目を省略すると、その項目はNothingになる。空白文字列ではない。
}
```

### portを持たないpart

portをもたないpartも定義できる。

```
defpart NTH2R2(){ref="NTH", type="Non-through hole", value="fai2.2"}
//            ^^ portがない
```


## defmodule

defmodule云々の部分はdefpartと同じ。

### partの宣言

次のように宣言する。

```
part r {項目1="値1" 項目2="値2" 項目3="値3"} as R;
```

たとえば

```
defpart ChipR(1,2){ref="R", type="Chip Register"}
part r {value="110k 5% 1608"} as ChipR;
```

プロパティで指定できる内容はdefpartと同じ。
defpartですでに指定されている項目は上書きされる。

```
defpart ChipR(1,2){ref="R", type="Chip Register"}
part r {ref="CR", value="110k 5% 1608"} as ChipR;
//      ^^^^^^^上書きされる。
```

複数まとめて宣言もOK

```
part r1, r2 {value="110k 5% 1608"} as ChipR;
```

でもプロパティが違う場合は分けて宣言が必要

```
part r1 {value="110k 5% 1608"}, r2 {value="39.1 1% 1005"} as ChipR; // NG
part r2 {value="39.1 1% 1005"} as ChipR; // OK
part r1 {value="110k 5% 1608"} as ChipR; // OK
```

### moduleの宣言

旧Eislerと同じ。

### 結線の記述

#### ポートの指定

ポートの指定には`<`と`>`を使う。
rというpartの1というピンを左側に結線するなら`1<r`、右側なら`r>1`、両方なら`1<r>1`

ピン名はエイリアスで宣言されているどの名前でもいい。
`1:P14:MTIOC`と定義されていれば`1<ic` `P14<ic` `MTIOC<ic`は同じ意味。

#### ポートの省略

ポート指定を省略すると、左側の場合は定義で一番最初に書かれているポートとして、右側の場合は2番目に書かれているポートとして扱われる。

```
defpart ChipR(1,2){ref="R", type="Chip Register"}
defpart ChipLED(2:A,1:K){ref="LD", type="Chip LED"}
...
part r as ChipR;
part ld as ChipLED;
...
VCC - r - ld - GND;
VCC - 1<r>2 - ld>K - GND;
// ↑この２行は同じ意味
```

#### WireとInterface

旧Eislerと同じ。
ただし、グローバルにも宣言できる。

#### 名前解決

グローバルな識別子とローカルな識別子が衝突した場合、ローカルが優先される。

```
wire VCC;
...
defmodule sub(1,2){
  wire VCC;
  1-VCC; // 一行上で宣言されているVCCにつながり、グローバルのVCCにはつながらない。
}
```

ローカルな識別子同士、グローバルな識別子同士が衝突した場合はエラーになる。
```
wire VCC;
interface VCC; // エラー
```
```
part reg as Reg;
part reg as ChipR;  // エラー
```

#### 未結線portの扱い

未結線のportはNCという予約語に接続する。これをしないと警告が出る。
NCはWireやItfcと同じように使える。

'''
reg>NC - NC;
ctrler>NC - NC - 4<reg_array
'''
