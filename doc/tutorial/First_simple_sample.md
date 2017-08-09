## シンプルなサンプル

コネクタから電源を受け取り、LEDを点灯させるだけの回路を作りましょう。
回路図で書くと、以下のようになります。

![First_simple_sample_schematic](resource\First_simple_sample\schematic.png)

テキストエディタに以下のソースコードを打ち込み、「Sample1.eis」というファイル名で保存します。

```tu01.eis
defpart CN(1:VIN 2:GND){ref "CN";}
defpart R(1:1 2:2){ref "R";}
defpart LED(1:A 2:K){ref "LD";}

defmodule main()
{
  part cn "B2B-EH" as CN;
  part led "BR1111C" as LED;
  part r "330 5% 1608" as R;

  wire VIN, GND;

  cn.VIN - VIN;
  cn.GND - GND;

  VIN - A.led.K - 1.r.2 - GND;
}
```

次に、お好みのコマンドラインシェルを開き、上記ファイルを保存したディレクトリに移動します。
そして以下のコマンドを実行してください。

```
> ket Sample1.eis
```

エラーがなければ何も表示されずに処理は完了し、以下の５つのファイルが作られます。

* ConInfo.md (各部品ごとの結線情報)
* README.md (翻訳結果のサマリ)
* Sample1.asc (PADS用のネットリスト)
* Sample1.csv (部品表)
* Sample1.net (KiCad用のネットリスト)
