defpart ChipR(1,2) {ref = "R"}
defpart ChipLed(1:A,2:K) {ref ="LD"}
defpart CONN2 (1,2) {ref = "CN"}

interface PullUp;
wire VCC;

defmodule main()
{
  part r {value = "100 5% 1608"} as ChipR;
  part led {mani = "Stanley", type = "BR1111C"} as ChipLED;
  part cn {mani = "JST", type = "B2B-EH"} as CONN2;
  wire VCC,GND;
  cn.1 - VCC;
  cn.2 - GND;
  VCC - 1.r.2 - A.led.K - GND;
}
