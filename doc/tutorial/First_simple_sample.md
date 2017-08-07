## シンプルなサンプル

コネクタから電源を受け取り、LEDを点灯させるだけの回路を作りましょう。

テキストエディタに以下のソースコードを打ち込み、お好みのファイル名で保存します。
ただし、拡張子は「.eis」としてください。

```tu01.eis
defpart CN(1: VIN 2:GND){ref "CN";}
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
