## モジュールに分割する

回路には、同じパターンの繰り返しが現れることがあります。

例えば、LEDで4桁の数字を表示させようとすると、7セグメントLEDと74HC4511(7セグメントLEDドライバ)のセットを4つ用いることになるでしょう。
回路図であれば、同じ回路のパターンが現れた時にはCAD上でコピー＆ペーストしたり、もしかしたら、1つ1つ作画したりするかもしれません。

ですが、Eislerにはモジュールという機能があります。
モジュールはある機能を持った回路ブロックで、1度定義すれば、あたかもそれが1つのパーツであるかのように使うことができます。
先の例でいうと、7セグメントLEDとそのドライバ、そして電流制限抵抗や、必要ならプルアップ・プルダウン抵抗をひとまとまりのモジュールとして定義し、それをパーツのように複数個宣言して使うことができます。

モジュールを使えば、同じ回路を何度も書かずに済みますし、もしその回路に変更が生じた際、モジュールの中身を修正してやればその回路ブロックが使われているすべての箇所が修正されます。

### モジュールを使った例

7セグメントLEDの例は、ソースコードが長くなりすぎてしまうので、ここでは下図のような回路を例にします。

![Schematic](resource\Module\Schematic.png)

ソースコードは次のようになります。
今回はModuleSample.eisというファイル名にしました。

```ModuleSample.eis
defpart R(1:1 2:2){ref "R";}
defpart LED(1:A 2:K){ref "LD";}
defpart NPNTransister(1:E 2:C 3:B){ref "TR";}

defmodule LedWithDriver(1:VPLUS 2:GND 3:IN)
{
  part r_led "330 5% 1608" as R;
  part r_base "1K 5% 1608" as R;
  part led "BR1111C" as LED;
  part tr "2SC1815" as NPNTransister;

  VPLUS - 1.r_led.2 - A.led.K - C.tr.E - GND;
  IN - 1.r_base.2 - B.tr;
}

defpart CONN5(1:1 2:2 3:3 4:4 5:5){ref "CN";}

defmodule main()
{
  part cn "PSS-410153-05" as CONN5;
  module l1,l2,l3 as LedWithDriver;

  wire VIN, GND, LED1, LED2, LED3;

  cn.1 - VIN;
  cn.2 - LED1;
  cn.3 - LED2;
  cn.4 - LED3;
  cn.5 - GND;

  VIN - VPLUS.l1.GND - GND;
  VIN - VPLUS.l2.GND - GND;
  VIN - VPLUS.l3.GND - GND;

  LED1 - IN.l1;
  LED2 - IN.l2;
  LED3 - IN.l3;
}
```

### 翻訳結果を見てみよう

`ket ModuleSample.eis`でソースコードを翻訳し、README.mdを開いて部品の一覧を確認してみます。

LEDと抵抗とトランジスタがそれぞれ3つずつ使われているのが分かります。
