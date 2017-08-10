## 複数のソースコードに分割する

いままでのサンプルは比較的規模の小さい回路ばかりでしたが、実際の開発ではより大規模なものが必要になることがあります。
そんな時、複数のモジュールや、たくさんのパーツの定義を１つのeisファイルにまとめて書くと、あとから読み返したときに理解しづらくなってしまいます。

そこで、この章では複数のeisファイルに分割する方法を説明します。

といっても簡単で、単純に複数のファイルに分けて回路を書き、メインとなるソースコードと同じディレクトリに入れておくだけでOKです。
さっそく試してみましょう。

例としてモジュールについて解説した時のサンプルModuleSample.eisと同じ回路を使います。
もともと１つだったファイルの中身を次の３つのファイルに分けて書きます。

まずは、パーツの定義だけを集めたdefpart.eisです。

```defpart.eis
defpart R(1:1 2:2){ref "R";}
defpart LED(1:A 2:K){ref "LD";}
defpart NPNTransister(1:E 2:C 3:B){ref "TR";}
defpart CONN5(1:1 2:2 3:3 4:4 5:5){ref "CN";}
```

次に、LedWithDriverモジュールだけを書いた、LedWithDriver.eisです。

```LedWithDriver.eis
defmodule LedWithDriver(1:VPLUS 2:GND 3:IN)
{
  part r_led "330 5% 1608" as R;
  part r_base "1K 5% 1608" as R;
  part led "BR1111C" as LED;
  part tr "2SC1815" as NPNTransister;

  VPLUS - 1.r_led.2 - A.led.K - C.tr.E - GND;
  IN - 1.r_base.2 - B.tr;
}
```

最後にmainモジュールだけを書いた、DivideSample.eisです。

```DivideSample.eis
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

この３つのファイルを同じディレクトリに置き、コマンドラインシェルから`ket DivideSample.eis`を実行してください。
ModuleSample.eisの時と同じ結果が得られるはずです。

このようにEislerは、メインのファイルと同じディレクトリに置かれた拡張子が`*.eis`のファイルをひとまとめにして翻訳します。
importやincludeといった操作は必要ありません。
ただし、サブディレクトリの中にあるファイルは翻訳の対象とならないので注意してください。
