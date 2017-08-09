EESchema Schematic File Version 2
LIBS:power
LIBS:device
LIBS:transistors
LIBS:conn
LIBS:linear
LIBS:regul
LIBS:74xx
LIBS:cmos4000
LIBS:adc-dac
LIBS:memory
LIBS:xilinx
LIBS:special
LIBS:microcontrollers
LIBS:dsp
LIBS:microchip
LIBS:analog_switches
LIBS:motorola
LIBS:texas
LIBS:intel
LIBS:audio
LIBS:interface
LIBS:digital-audio
LIBS:philips
LIBS:display
LIBS:cypress
LIBS:siliconi
LIBS:opto
LIBS:atmel
LIBS:contrib
LIBS:valves
LIBS:First_simple_sample-cache
EELAYER 27 0
EELAYER END
$Descr A4 11693 8268
encoding utf-8
Sheet 1 1
Title ""
Date "9 aug 2017"
Rev ""
Comp ""
Comment1 ""
Comment2 ""
Comment3 ""
Comment4 ""
$EndDescr
$Comp
L LED D1
U 1 1 59891967
P 5050 3600
F 0 "D1" H 5050 3700 50  0000 C CNN
F 1 "BR1111C" H 5050 3450 50  0000 C CNN
F 2 "~" H 5050 3600 60  0000 C CNN
F 3 "~" H 5050 3600 60  0000 C CNN
	1    5050 3600
	1    0    0    -1  
$EndComp
$Comp
L CONN_2 CN1
U 1 1 59891976
P 4250 3800
F 0 "CN1" H 4250 4000 50  0000 C CNN
F 1 "PSS-410153-02" H 4250 3600 50  0000 C CNN
F 2 "~" H 4250 3800 60  0000 C CNN
F 3 "~" H 4250 3800 60  0000 C CNN
	1    4250 3800
	-1   0    0    -1  
$EndComp
$Comp
L R R1
U 1 1 59891999
P 5050 4000
F 0 "R1" V 5130 4000 50  0000 C CNN
F 1 "330 5% 1608" V 4950 4000 50  0000 C CNN
F 2 "~" V 4980 4000 30  0000 C CNN
F 3 "~" H 5050 4000 30  0000 C CNN
	1    5050 4000
	0    -1   -1   0   
$EndComp
Wire Wire Line
	4600 3700 4700 3700
Wire Wire Line
	4700 3700 4700 3600
Wire Wire Line
	4700 3600 4850 3600
Wire Wire Line
	4600 3900 4700 3900
Wire Wire Line
	4700 3900 4700 4000
Wire Wire Line
	4700 4000 4800 4000
Wire Wire Line
	5250 3600 5350 3600
Wire Wire Line
	5350 3600 5350 4000
Wire Wire Line
	5350 4000 5300 4000
$EndSCHEMATC
