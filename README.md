# Eisler 0.1

## Overview

Eisler(アイスラー)は、テキストベースの回路記述言語です。
Eislerで回路を記述し、それを処理系に通すとネットリストと部品表が出力される、
そんなソフトウェアを目指しています。


## Sample

```
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

## License

Copyright 2017 elkel53930

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

[The MIT License](https://opensource.org/licenses/mit-license.php)

## Author

[elkel53930](https://github.com/elkel53930/)
