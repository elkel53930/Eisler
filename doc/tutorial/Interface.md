## インターフェイス

インターフェイスはワイヤによく似た機能で、ワイヤと異なる点は以下の２つのみです。
* 異なる名前のインターフェイスやワイヤとも結線できる
* 出力されるネットリストの名前に、インターフェイス名は使われない

### ワイヤとインターフェイスをつなぐ

例を挙げてみましょう。

```Interface.eis
defpart R(1:1 2:2){ref "R";}
defpart NchFET(1:G 2:S 3:D){ref "Q";}
defpart LED(1:A 2:K){ref "LD";}
defpart CONN3(1:1 2:2 3:3){ref "CN";}

defmodule main()
{
  part pulldownR "10k 5% 1608" as R;
  part r_led "330 5% 1608" as R;
  part led "BR1111C" as LED;
  part fet "BSS138" as NchFET;
  part cn "PSS-410153-03" as CONN3;

  wire VIN, GND, SIGNAL;
  interface PULLDOWN;

  GND - 1.pulldownR.2 - PULLDOWN;

  VIN - 1.r_led.2 - A.led.K - D.fet.S - GND;
  SIGNAL - PULLDOWN - G.fet;
}
```

`PULLDOWN`というインターフェイスはプルダウン抵抗`pulldownR`を介してGNDにつながっています。
この`PULLDOWN`は`SIGNAL`に接続され、プルダウンを実現しています。
もしPULLDOWNをワイヤとして宣言してしまうと(`wire PULLDOWN;`)、次のようなエラーが発生します。
```
"Interface.eis" (line 20, column 3)
        Connection between different wires. 'SIGNAL-main' and 'PULLDOWN-main'.
```

### ネット名の確認

ワイヤを使用すると、Eislerが出力するネットリストのネット名そのワイヤ名が使われます。
