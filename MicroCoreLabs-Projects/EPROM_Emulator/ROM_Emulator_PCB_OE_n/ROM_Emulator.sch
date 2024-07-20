EESchema Schematic File Version 4
EELAYER 30 0
EELAYER END
$Descr A4 11693 8268
encoding utf-8
Sheet 1 1
Title ""
Date ""
Rev ""
Comp ""
Comment1 ""
Comment2 ""
Comment3 ""
Comment4 ""
$EndDescr
$Comp
L Memory_EPROM:27C512 U1
U 1 1 601A8B4C
P 1800 2700
F 0 "U1" H 1450 3900 50  0000 C CNN
F 1 "27C512" H 1450 3800 50  0000 C CNN
F 2 "Package_DIP:DIP-28_W15.24mm" H 1800 2700 50  0001 C CNN
F 3 "http://ww1.microchip.com/downloads/en/DeviceDoc/doc0015.pdf" H 1800 2700 50  0001 C CNN
	1    1800 2700
	1    0    0    -1  
$EndComp
Wire Wire Line
	2500 1800 2200 1800
Text GLabel 2500 1900 2    50   Input ~ 0
V5_D1
Wire Wire Line
	2500 1900 2200 1900
Text GLabel 2500 2000 2    50   Input ~ 0
V5_D2
Wire Wire Line
	2500 2000 2200 2000
Text GLabel 2500 2100 2    50   Input ~ 0
V5_D3
Wire Wire Line
	2500 2100 2200 2100
Text GLabel 2500 2200 2    50   Input ~ 0
V5_D4
Wire Wire Line
	2500 2200 2200 2200
Text GLabel 2500 2300 2    50   Input ~ 0
V5_D5
Wire Wire Line
	2500 2300 2200 2300
Text GLabel 2500 2400 2    50   Input ~ 0
V5_D6
Wire Wire Line
	2500 2400 2200 2400
Text GLabel 2500 2500 2    50   Input ~ 0
V5_D7
Wire Wire Line
	2500 2500 2200 2500
Text GLabel 5750 2100 2    50   Input ~ 0
V3_D0
Wire Wire Line
	5750 2100 5450 2100
Text GLabel 5750 2200 2    50   Input ~ 0
V3_D1
Wire Wire Line
	5750 2200 5450 2200
Text GLabel 5750 2300 2    50   Input ~ 0
V3_D2
Wire Wire Line
	5750 2300 5450 2300
Wire Wire Line
	5750 2400 5450 2400
Text GLabel 5750 2500 2    50   Input ~ 0
V3_D6
Wire Wire Line
	5750 2500 5450 2500
Text GLabel 5750 2600 2    50   Input ~ 0
V3_D5
Wire Wire Line
	5750 2600 5450 2600
Wire Wire Line
	5750 2700 5450 2700
Wire Wire Line
	5750 2800 5450 2800
Text GLabel 4150 2800 0    50   Input ~ 0
V5_D3
Text GLabel 4150 2700 0    50   Input ~ 0
V5_D4
Text GLabel 4150 2600 0    50   Input ~ 0
V5_D5
Text GLabel 4150 2500 0    50   Input ~ 0
V5_D6
Text GLabel 4150 2400 0    50   Input ~ 0
V5_D7
Text GLabel 4150 2300 0    50   Input ~ 0
V5_D2
Text GLabel 4150 2200 0    50   Input ~ 0
V5_D1
Text GLabel 4150 2100 0    50   Input ~ 0
V5_D0
Text GLabel 2500 1800 2    50   Input ~ 0
V5_D0
Wire Wire Line
	1100 2500 1400 2500
Text GLabel 1100 2500 0    50   Input ~ 0
V5_A7
Wire Wire Line
	1100 2400 1400 2400
Text GLabel 1100 2400 0    50   Input ~ 0
V5_A6
Wire Wire Line
	1100 2300 1400 2300
Text GLabel 1100 2300 0    50   Input ~ 0
V5_A5
Wire Wire Line
	1100 2200 1400 2200
Text GLabel 1100 2200 0    50   Input ~ 0
V5_A4
Wire Wire Line
	1100 2100 1400 2100
Text GLabel 1100 2100 0    50   Input ~ 0
V5_A3
Wire Wire Line
	1100 2000 1400 2000
Text GLabel 1100 2000 0    50   Input ~ 0
V5_A2
Wire Wire Line
	1100 1900 1400 1900
Text GLabel 1100 1900 0    50   Input ~ 0
V5_A1
Wire Wire Line
	1100 1800 1400 1800
Text GLabel 1100 1800 0    50   Input ~ 0
V5_A0
Wire Wire Line
	1100 3300 1400 3300
Text GLabel 1100 3300 0    50   Input ~ 0
V5_A15
Wire Wire Line
	1100 3200 1400 3200
Text GLabel 1100 3200 0    50   Input ~ 0
V5_A14
Wire Wire Line
	1100 3100 1400 3100
Text GLabel 1100 3100 0    50   Input ~ 0
V5_A13
Wire Wire Line
	1100 3000 1400 3000
Text GLabel 1100 3000 0    50   Input ~ 0
V5_A12
Wire Wire Line
	1100 2900 1400 2900
Text GLabel 1100 2900 0    50   Input ~ 0
V5_A11
Wire Wire Line
	1100 2800 1400 2800
Text GLabel 1100 2800 0    50   Input ~ 0
V5_A10
Wire Wire Line
	1100 2700 1400 2700
Text GLabel 1100 2700 0    50   Input ~ 0
V5_A9
Wire Wire Line
	1100 2600 1400 2600
Text GLabel 1100 2600 0    50   Input ~ 0
V5_A8
$Comp
L power:GND #PWR01
U 1 1 601AFAF8
P 1800 3950
F 0 "#PWR01" H 1800 3700 50  0001 C CNN
F 1 "GND" H 1805 3777 50  0000 C CNN
F 2 "" H 1800 3950 50  0001 C CNN
F 3 "" H 1800 3950 50  0001 C CNN
	1    1800 3950
	1    0    0    -1  
$EndComp
Wire Wire Line
	1800 3950 1800 3800
$Comp
L power:GND #PWR04
U 1 1 601B12E9
P 4950 3550
F 0 "#PWR04" H 4950 3300 50  0001 C CNN
F 1 "GND" H 4955 3377 50  0000 C CNN
F 2 "" H 4950 3550 50  0001 C CNN
F 3 "" H 4950 3550 50  0001 C CNN
	1    4950 3550
	1    0    0    -1  
$EndComp
Wire Wire Line
	4950 3550 4950 3400
$Comp
L power:+3.3V #PWR03
U 1 1 601B2BA8
P 4950 1650
F 0 "#PWR03" H 4950 1500 50  0001 C CNN
F 1 "+3.3V" H 4965 1823 50  0000 C CNN
F 2 "" H 4950 1650 50  0001 C CNN
F 3 "" H 4950 1650 50  0001 C CNN
	1    4950 1650
	1    0    0    -1  
$EndComp
Wire Wire Line
	4950 1800 4950 1650
Wire Wire Line
	1100 3600 1400 3600
Text GLabel 1100 3600 0    50   Input ~ 0
OE_n
Text GLabel 4150 3100 0    50   Input ~ 0
OE_n
Wire Wire Line
	4150 3100 4450 3100
Wire Wire Line
	4150 2800 4450 2800
Wire Wire Line
	4150 2700 4450 2700
Wire Wire Line
	4150 2600 4450 2600
Wire Wire Line
	4150 2500 4450 2500
Wire Wire Line
	4150 2400 4450 2400
Wire Wire Line
	4150 2300 4450 2300
Wire Wire Line
	4150 2200 4450 2200
Wire Wire Line
	4150 2100 4450 2100
$Comp
L 74xx:74LS245 U2
U 1 1 601A9F2F
P 4950 2600
F 0 "U2" H 4500 3450 50  0000 C CNN
F 1 "74LS245" H 4600 3350 50  0000 C CNN
F 2 "Package_SO:SSOP-20_5.3x7.2mm_P0.65mm" H 4950 2600 50  0001 C CNN
F 3 "http://www.ti.com/lit/gpn/sn74LS245" H 4950 2600 50  0001 C CNN
	1    4950 2600
	1    0    0    -1  
$EndComp
$Comp
L power:GND #PWR02
U 1 1 601BD23C
P 3750 3150
F 0 "#PWR02" H 3750 2900 50  0001 C CNN
F 1 "GND" H 3755 2977 50  0000 C CNN
F 2 "" H 3750 3150 50  0001 C CNN
F 3 "" H 3750 3150 50  0001 C CNN
	1    3750 3150
	1    0    0    -1  
$EndComp
Wire Wire Line
	3750 3150 3750 3000
Wire Wire Line
	4450 3000 3750 3000
$Comp
L power:GND #PWR0101
U 1 1 601CCD89
P 3150 7000
F 0 "#PWR0101" H 3150 6750 50  0001 C CNN
F 1 "GND" H 3155 6827 50  0000 C CNN
F 2 "" H 3150 7000 50  0001 C CNN
F 3 "" H 3150 7000 50  0001 C CNN
	1    3150 7000
	1    0    0    -1  
$EndComp
Wire Wire Line
	3150 7000 3150 6850
$Comp
L power:+3.3V #PWR0102
U 1 1 601CCD90
P 3150 5100
F 0 "#PWR0102" H 3150 4950 50  0001 C CNN
F 1 "+3.3V" H 3165 5273 50  0000 C CNN
F 2 "" H 3150 5100 50  0001 C CNN
F 3 "" H 3150 5100 50  0001 C CNN
	1    3150 5100
	1    0    0    -1  
$EndComp
Wire Wire Line
	3150 5250 3150 5100
$Comp
L 74xx:74LS245 U4
U 1 1 601CCDA1
P 3150 6050
F 0 "U4" H 2700 6900 50  0000 C CNN
F 1 "74LS245" H 2800 6800 50  0000 C CNN
F 2 "Package_SO:SSOP-20_5.3x7.2mm_P0.65mm" H 3150 6050 50  0001 C CNN
F 3 "http://www.ti.com/lit/gpn/sn74LS245" H 3150 6050 50  0001 C CNN
	1    3150 6050
	1    0    0    -1  
$EndComp
$Comp
L power:GND #PWR0103
U 1 1 601CCDA7
P 1950 6600
F 0 "#PWR0103" H 1950 6350 50  0001 C CNN
F 1 "GND" H 1955 6427 50  0000 C CNN
F 2 "" H 1950 6600 50  0001 C CNN
F 3 "" H 1950 6600 50  0001 C CNN
	1    1950 6600
	1    0    0    -1  
$EndComp
Wire Wire Line
	1950 6600 1950 6450
Wire Wire Line
	2650 6450 2450 6450
Text GLabel 3950 5550 2    50   Input ~ 0
V5_A5
Wire Wire Line
	3950 5550 3650 5550
Wire Wire Line
	2450 6550 2650 6550
Wire Wire Line
	2450 6550 2450 6450
Connection ~ 2450 6450
Wire Wire Line
	2450 6450 1950 6450
$Comp
L power:GND #PWR0104
U 1 1 601FC8DE
P 6400 7000
F 0 "#PWR0104" H 6400 6750 50  0001 C CNN
F 1 "GND" H 6405 6827 50  0000 C CNN
F 2 "" H 6400 7000 50  0001 C CNN
F 3 "" H 6400 7000 50  0001 C CNN
	1    6400 7000
	1    0    0    -1  
$EndComp
Wire Wire Line
	6400 7000 6400 6850
$Comp
L power:+3.3V #PWR0105
U 1 1 601FC8E5
P 6400 5100
F 0 "#PWR0105" H 6400 4950 50  0001 C CNN
F 1 "+3.3V" H 6415 5273 50  0000 C CNN
F 2 "" H 6400 5100 50  0001 C CNN
F 3 "" H 6400 5100 50  0001 C CNN
	1    6400 5100
	1    0    0    -1  
$EndComp
Wire Wire Line
	6400 5250 6400 5100
$Comp
L 74xx:74LS245 U5
U 1 1 601FC8F4
P 6400 6050
F 0 "U5" H 5950 6900 50  0000 C CNN
F 1 "74LS245" H 6050 6800 50  0000 C CNN
F 2 "Package_SO:SSOP-20_5.3x7.2mm_P0.65mm" H 6400 6050 50  0001 C CNN
F 3 "http://www.ti.com/lit/gpn/sn74LS245" H 6400 6050 50  0001 C CNN
	1    6400 6050
	1    0    0    -1  
$EndComp
$Comp
L power:GND #PWR0106
U 1 1 601FC8FA
P 5200 6600
F 0 "#PWR0106" H 5200 6350 50  0001 C CNN
F 1 "GND" H 5205 6427 50  0000 C CNN
F 2 "" H 5200 6600 50  0001 C CNN
F 3 "" H 5200 6600 50  0001 C CNN
	1    5200 6600
	1    0    0    -1  
$EndComp
Wire Wire Line
	5200 6600 5200 6450
Wire Wire Line
	5900 6450 5700 6450
Text GLabel 7200 5650 2    50   Input ~ 0
V5_A14
Text GLabel 7200 5850 2    50   Input ~ 0
V5_A13
Wire Wire Line
	7200 5850 6900 5850
Wire Wire Line
	5700 6550 5900 6550
Wire Wire Line
	5700 6550 5700 6450
Connection ~ 5700 6450
Wire Wire Line
	5700 6450 5200 6450
Text Notes 750  850  0    118  ~ 0
MicroCore Labs - ROM Emulator\n
Text GLabel 5750 2400 2    50   Input ~ 0
V3_D7
Text GLabel 7450 2700 0    50   Input ~ 0
V3_D0
Wire Wire Line
	7450 2700 7750 2700
Text GLabel 7450 2800 0    50   Input ~ 0
V3_D1
Wire Wire Line
	7450 2800 7750 2800
Text GLabel 7450 2900 0    50   Input ~ 0
V3_D2
Wire Wire Line
	7450 2900 7750 2900
Text GLabel 5750 2800 2    50   Input ~ 0
V3_D3
Text GLabel 5750 2700 2    50   Input ~ 0
V3_D4
Text GLabel 7450 3600 0    50   Input ~ 0
V3_D4
Text GLabel 7450 3500 0    50   Input ~ 0
V3_D3
Text GLabel 7450 3900 0    50   Input ~ 0
V3_D7
Wire Wire Line
	7450 3500 7750 3500
Wire Wire Line
	7450 3600 7750 3600
Wire Wire Line
	7450 3700 7750 3700
Text GLabel 7450 3700 0    50   Input ~ 0
V3_D5
Wire Wire Line
	7450 3800 7750 3800
Text GLabel 7450 3800 0    50   Input ~ 0
V3_D6
Wire Wire Line
	7450 3900 7750 3900
Text GLabel 7450 2100 0    50   Input ~ 0
V3_A7
Text GLabel 10250 4300 2    50   Input ~ 0
V3_A6
Text GLabel 7450 2400 0    50   Input ~ 0
V3_A4
Wire Wire Line
	7450 2100 7750 2100
Wire Wire Line
	10250 4300 9950 4300
Wire Wire Line
	7450 2400 7750 2400
Wire Wire Line
	7200 5550 6900 5550
Text GLabel 7200 5550 2    50   Input ~ 0
V5_A15
Text GLabel 7450 1900 0    50   Input ~ 0
V3_A12
Wire Wire Line
	7450 1900 7750 1900
Text GLabel 7450 1800 0    50   Input ~ 0
V3_A14
Wire Wire Line
	7450 1800 7750 1800
Text GLabel 7450 2300 0    50   Input ~ 0
V3_A11
Wire Wire Line
	7450 2300 7750 2300
$Comp
L power:GND #PWR0107
U 1 1 60270919
P 6850 1700
F 0 "#PWR0107" H 6850 1450 50  0001 C CNN
F 1 "GND" H 6855 1527 50  0000 C CNN
F 2 "" H 6850 1700 50  0001 C CNN
F 3 "" H 6850 1700 50  0001 C CNN
	1    6850 1700
	1    0    0    -1  
$EndComp
$Comp
L power:GND #PWR0108
U 1 1 6027B17F
P 10700 4150
F 0 "#PWR0108" H 10700 3900 50  0001 C CNN
F 1 "GND" H 10705 3977 50  0000 C CNN
F 2 "" H 10700 4150 50  0001 C CNN
F 3 "" H 10700 4150 50  0001 C CNN
	1    10700 4150
	1    0    0    -1  
$EndComp
Text GLabel 7450 2000 0    50   Input ~ 0
V3_A13
Wire Wire Line
	7450 2000 7750 2000
$Comp
L power:+3.3V #PWR0109
U 1 1 60290E82
P 10600 4300
F 0 "#PWR0109" H 10600 4150 50  0001 C CNN
F 1 "+3.3V" H 10615 4473 50  0000 C CNN
F 2 "" H 10600 4300 50  0001 C CNN
F 3 "" H 10600 4300 50  0001 C CNN
	1    10600 4300
	1    0    0    1   
$EndComp
Wire Wire Line
	9950 4100 10600 4100
Wire Wire Line
	10600 4100 10600 4300
NoConn ~ 9950 3900
Wire Wire Line
	6850 1600 6850 1700
Wire Wire Line
	6850 1600 7750 1600
Wire Wire Line
	7450 1700 7750 1700
Text GLabel 7450 1700 0    50   Input ~ 0
V3_A15
$Comp
L teensy:Teensy4.0 U3
U 1 1 601A6865
P 8850 2950
F 0 "U3" H 8850 4565 50  0000 C CNN
F 1 "Teensy4.0" H 8850 4474 50  0000 C CNN
F 2 "Teensy:Teensy40" H 8450 3150 50  0001 C CNN
F 3 "" H 8450 3150 50  0001 C CNN
	1    8850 2950
	1    0    0    -1  
$EndComp
Wire Wire Line
	7450 4300 7750 4300
Text GLabel 7450 4300 0    50   Input ~ 0
V3_A9
Wire Wire Line
	10250 4200 9950 4200
Text GLabel 10250 4200 2    50   Input ~ 0
V3_A8
Wire Wire Line
	7200 5750 6900 5750
Text GLabel 7200 5750 2    50   Input ~ 0
V5_A12
Wire Wire Line
	7200 5650 6900 5650
Text GLabel 7200 6050 2    50   Input ~ 0
V5_A8
Text GLabel 7200 6250 2    50   Input ~ 0
V5_A9
Wire Wire Line
	7200 6250 6900 6250
Wire Wire Line
	7200 5950 6900 5950
Text GLabel 7200 5950 2    50   Input ~ 0
V5_A7
Wire Wire Line
	7200 6150 6900 6150
Text GLabel 7200 6150 2    50   Input ~ 0
V5_A6
Wire Wire Line
	7200 6050 6900 6050
Text GLabel 5600 5650 0    50   Input ~ 0
V3_A14
Text GLabel 5600 5850 0    50   Input ~ 0
V3_A13
Wire Wire Line
	5600 5850 5900 5850
Wire Wire Line
	5600 5550 5900 5550
Text GLabel 5600 5550 0    50   Input ~ 0
V3_A15
Wire Wire Line
	5600 5750 5900 5750
Text GLabel 5600 5750 0    50   Input ~ 0
V3_A12
Wire Wire Line
	5600 5650 5900 5650
Text GLabel 5600 6050 0    50   Input ~ 0
V3_A8
Text GLabel 5600 6250 0    50   Input ~ 0
V3_A9
Wire Wire Line
	5600 6250 5900 6250
Wire Wire Line
	5600 5950 5900 5950
Text GLabel 5600 5950 0    50   Input ~ 0
V3_A7
Wire Wire Line
	5600 6150 5900 6150
Text GLabel 5600 6150 0    50   Input ~ 0
V3_A6
Wire Wire Line
	5600 6050 5900 6050
Text GLabel 3950 5650 2    50   Input ~ 0
V5_A11
Wire Wire Line
	3950 5650 3650 5650
Text GLabel 3950 5750 2    50   Input ~ 0
V5_A4
Wire Wire Line
	3950 5750 3650 5750
Text GLabel 3950 5850 2    50   Input ~ 0
V5_A10
Wire Wire Line
	3950 5850 3650 5850
Text GLabel 3950 5950 2    50   Input ~ 0
V5_A3
Wire Wire Line
	3950 5950 3650 5950
Text GLabel 3950 6050 2    50   Input ~ 0
V5_A2
Wire Wire Line
	3950 6050 3650 6050
Text GLabel 3950 6150 2    50   Input ~ 0
V5_A1
Wire Wire Line
	3950 6150 3650 6150
Text GLabel 3950 6250 2    50   Input ~ 0
V5_A0
Wire Wire Line
	3950 6250 3650 6250
Text GLabel 2350 5550 0    50   Input ~ 0
V3_A5
Wire Wire Line
	2350 5550 2650 5550
Text GLabel 2350 5650 0    50   Input ~ 0
V3_A11
Wire Wire Line
	2350 5650 2650 5650
Text GLabel 2350 5750 0    50   Input ~ 0
V3_A4
Wire Wire Line
	2350 5750 2650 5750
Text GLabel 2350 5850 0    50   Input ~ 0
V3_A10
Wire Wire Line
	2350 5850 2650 5850
Text GLabel 2350 5950 0    50   Input ~ 0
V3_A3
Wire Wire Line
	2350 5950 2650 5950
Text GLabel 2350 6050 0    50   Input ~ 0
V3_A2
Wire Wire Line
	2350 6050 2650 6050
Text GLabel 2350 6150 0    50   Input ~ 0
V3_A1
Wire Wire Line
	2350 6150 2650 6150
Text GLabel 2350 6250 0    50   Input ~ 0
V3_A0
Wire Wire Line
	2350 6250 2650 6250
Wire Wire Line
	7450 2200 7750 2200
Text GLabel 7450 2200 0    50   Input ~ 0
V3_A5
Wire Wire Line
	9950 4000 10700 4000
Wire Wire Line
	10700 4150 10700 4000
Text GLabel 7450 2500 0    50   Input ~ 0
V3_A10
Wire Wire Line
	7450 2500 7750 2500
Text GLabel 7450 2600 0    50   Input ~ 0
V3_A3
Wire Wire Line
	7450 2600 7750 2600
Text GLabel 7450 4000 0    50   Input ~ 0
V3_A0
Wire Wire Line
	7450 4000 7750 4000
Text GLabel 7450 4100 0    50   Input ~ 0
V3_A1
Wire Wire Line
	7450 4100 7750 4100
Text GLabel 7450 4200 0    50   Input ~ 0
V3_A2
Wire Wire Line
	7450 4200 7750 4200
$EndSCHEMATC
