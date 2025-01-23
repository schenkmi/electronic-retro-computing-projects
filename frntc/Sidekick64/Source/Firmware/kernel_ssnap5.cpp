﻿/*
  _________.__    .___      __   .__        __     ___________                                       _________ _________.________
 /   _____/|__| __| _/____ |  | _|__| ____ |  | __ \_   _____/______   ____   ____ ________ ____    /   _____//   _____/|   ____/
 \_____  \ |  |/ __ |/ __ \|  |/ /  |/ ___\|  |/ /  |    __) \_  __ \_/ __ \_/ __ \\___   // __ \   \_____  \ \_____  \ |____  \ 
 /        \|  / /_/ \  ___/|    <|  \  \___|    <   |     \   |  | \/\  ___/\  ___/ /    /\  ___/   /        \/        \/       \
/_______  /|__\____ |\___  >__|_ \__|\___  >__|_ \  \___  /   |__|    \___  >\___  >_____ \\___  > /_______  /_______  /______  /
        \/         \/    \/     \/       \/     \/      \/                \/     \/      \/    \/          \/        \/       \/ 

 kernel_ssnap5.cpp

 Sidekick64 - A framework for interfacing 8-Bit Commodore computers (C64/C128,C16/Plus4,VC20) and a Raspberry Pi Zero 2 or 3A+/3B+
            - Sidekick Freeze: example how to implement a Super Snapshot V5 Cartridge compatible 
 Copyright (c) 2019-2022 Carsten Dachsbacher <frenetic@dachsbacher.de>

 Logo created with http://patorjk.com/software/taag/
 
 This program is free software: you can redistribute it and/or modify
 it under the terms of the GNU General Public License as published by
 the Free Software Foundation, either version 3 of the License, or
 (at your option) any later version.

 This program is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 GNU General Public License for more details.

 You should have received a copy of the GNU General Public License
 along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/

#include "kernel_ssnap5.h"

// we will read this .CRT file 
static const char DRIVE[] = "SD:";
static const char FILENAME[] = "SD:Freezer/ss5.crt";

static const char FILENAME_SPLASH_RGB[] = "SD:SPLASH/sk64_ss5.tga";

#pragma pack(push)
#pragma pack(1)
typedef struct
{
	u8	reg;
	// extra RAM
	u8  ram[ 32768 ];

	u32 exportRAM, active, nROMBanks, curBank, curRAMBank, ultimax;

	// counts the #cycles when the C64-reset line is pulled down (to detect a reset), cycles since release and status
	u32 resetCounter, cyclesSinceReset,  
		resetPressed, resetReleased;
	u32 countWrites;

	// total count of C64 cycles (not set to zero upon reset)
	u64 c64CycleCount;

	// pointer to ROM data
	u8 *flash_cacheoptimized;

	// orchestration of freezing
	u32 freezeNMICycles, lastFreezeButton;

	u32 releaseDMA;

	u32 LONGBOARD;

	u32 hasKernal;
} __attribute__((packed)) SS5STATE;
#pragma pack(pop)

static volatile SS5STATE ss5 AAA;

static unsigned char kernalROM[ 8192 ] AAA;

// ... flash/ROM
extern u8 *flash_cacheoptimized_pool;

static __attribute__( ( always_inline ) ) inline void ss5Config( u8 c )
{
	if ( c < 4 )
		latchSetClear( LATCH_LED0, 0 ); else
		latchSetClear( 0, LATCH_LED0 ); 

	switch ( c & 3 )
	{
		case 0:	SETCLR_GPIO( bDMA | bNMI | bEXROM, bGAME );	break;
		default:
		case 1:	SET_GPIO( bGAME | bEXROM | bDMA | bNMI );	break;
		case 2:	SETCLR_GPIO( bDMA | bNMI, bGAME | bEXROM );	break;
		case 3:	SETCLR_GPIO( bGAME | bDMA | bNMI, bEXROM );	break;
	}
}

static void initSS5()
{
	ss5.reg = 0;
	ss5.curBank = 0;
	ss5.exportRAM = 1;
	ss5.curRAMBank = 0;
    ss5.active = 1;
	ss5.ultimax = 1;

	ss5.resetCounter = ss5.resetPressed = 
	ss5.resetReleased = ss5.cyclesSinceReset = 
	ss5.freezeNMICycles = ss5.lastFreezeButton = 	
	ss5.countWrites = ss5.c64CycleCount = 0;

	ss5.releaseDMA = 0;

	ss5Config( ss5.reg );
	SETCLR_GPIO( bDMA | bNMI, bCTRL257 );
}

__attribute__( ( always_inline ) ) inline void callbackReset()
{
	initSS5();
	latchSetClearImm( LATCH_LED0, 0 );
}

static u32 LED_CLEAR, LED_ALL_BUT_0;

static void initScreenAndLEDCodes()
{
	#ifndef COMPILE_MENU
	int screenType = 0;
	#endif
	if ( screenType == 0 ) // OLED with SCL and SDA (i.e. 2 Pins) -> 4 LEDs
	{
		LED_ALL_BUT_0	= LATCH_LED1to3;
		LED_CLEAR		= LATCH_LED_ALL;
	} else
	if ( screenType == 1 ) // RGB TFT with SCL, SDA, DC, RES -> 2 LEDs
	{
		LED_ALL_BUT_0	= LATCH_LED1;
		LED_CLEAR		= LATCH_LED0to1;
	}
}

#ifdef COMPILE_MENU
void KernelSS5Run( CGPIOPinFIQ m_InputPin, CKernelMenu *kernelMenu, char *FILENAME, const char *FILENAME_KERNAL = NULL )
#else
void CKernelSS5::Run( void )
#endif
{
	// initialize latch and software I2C buffer
	initLatch();
	initScreenAndLEDCodes();

	latchSetClearImm( 0, LATCH_RESET | LED_CLEAR | LATCH_ENABLE_KERNAL );

	SET_GPIO( bGAME | bEXROM | bNMI | bDMA );
	CLR_GPIO( bCTRL257 ); 

	//
	// load ROM and convert to cache-friendly format
	//
	#ifndef COMPILE_MENU
	m_EMMC.Initialize();
	#endif

	// load kernal is any
	u32 kernalSize = 0; // should always be 8192
	if ( FILENAME_KERNAL != NULL )
	{
		ss5.hasKernal = 1;
		readFile( logger, (char*)DRIVE, (char*)FILENAME_KERNAL, flash_cacheoptimized_pool, &kernalSize );
		memcpy( kernalROM, flash_cacheoptimized_pool, 8192 );
	} else
		ss5.hasKernal = 0;

	ss5.flash_cacheoptimized = (u8 *)( ( (u64)&flash_cacheoptimized_pool[0] + 128 ) & ~127 );

	CRT_HEADER header;
	u32 ROM_LH;
	u8 bankswitchType;
	u32 nBanks;
	readCRTFile( logger, &header, (char*)DRIVE, (char*)FILENAME, (u8*)ss5.flash_cacheoptimized, &bankswitchType, &ROM_LH, &nBanks, true );
	ss5.nROMBanks = nBanks;

	#ifdef COMPILE_MENU
	if ( screenType == 0 )
	{
		splashScreen( raspi_freeze_splash );
	} else
	if ( screenType == 1 )
	{
		extern bool loadCustomLogoIfAvailable( char *FILENAME );

		if ( !loadCustomLogoIfAvailable( FILENAME ) )
		{
			tftLoadBackgroundTGA( DRIVE, FILENAME_SPLASH_RGB, 8 );

			int w, h; 
			extern char FILENAME_LOGO_RGBA[128];
			extern unsigned char tempTGA[ 256 * 256 * 4 ];

			if ( tftLoadTGA( DRIVE, FILENAME_LOGO_RGBA, tempTGA, &w, &h, true ) )
			{
				tftBlendRGBA( tempTGA, tftBackground, 0 );
			}
		}

		tftCopyBackground2Framebuffer();
		tftInitImm();
		tftSendFramebuffer16BitImm( tftFrameBuffer );
	}
	#endif

	// setup FIQ
	DisableIRQs();
	m_InputPin.ConnectInterrupt( FIQ_HANDLER, FIQ_PARENT );

	// reset the SS5 and warm caches
	callbackReset();

	CleanDataCache();
	InvalidateDataCache();
	InvalidateInstructionCache();

	CACHE_PRELOAD_DATA_CACHE( ss5.flash_cacheoptimized, 8192 * 2 * ss5.nROMBanks, CACHE_PRELOADL2KEEP )
	FORCE_READ_LINEARa( ss5.flash_cacheoptimized, 8192 * 2 * ss5.nROMBanks, 8192 * 2 * ss5.nROMBanks * 1 )

	CACHE_PRELOAD_DATA_CACHE( (u8*)&ss5, ( sizeof( SS5STATE ) + 63 ) / 64, CACHE_PRELOADL1KEEP )
	CACHE_PRELOAD_INSTRUCTION_CACHE( (void*)&FIQ_HANDLER, 2048 )

	FORCE_READ_LINEAR32a( &ss5, sizeof( SS5STATE ), 65536 );
	FORCE_READ_LINEAR32a( &FIQ_HANDLER, 2048, 65536 );

	FORCE_READ_LINEARa( ss5.flash_cacheoptimized, 8192 * 2 * 4, 8192 * 2 * 4 * 32 )

	if ( ss5.hasKernal )
	{
		CACHE_PRELOAD_DATA_CACHE( kernalROM, 8192, CACHE_PRELOADL2KEEP );
		FORCE_READ_LINEAR32a( kernalROM, 8192, 65536 );
	}

	// different timing C64-longboards and C128 compared to 469-boards
	ss5.LONGBOARD = 0;
	if ( modeC128 || modeVIC == 0 )
		ss5.LONGBOARD = 1; 

	m_InputPin.EnableInterrupt ( GPIOInterruptOnRisingEdge );
	//m_InputPin.EnableInterrupt2( GPIOInterruptOnFallingEdge );

	// ready to go
	DELAY(10);
	
	if ( ss5.hasKernal )
		latchSetClearImm( LATCH_RESET | LATCH_ENABLE_KERNAL, LED_ALL_BUT_0 ); else
		latchSetClearImm( LATCH_RESET, LED_ALL_BUT_0 | LATCH_ENABLE_KERNAL );

	// main loop
	while ( true ) {
		#ifdef COMPILE_MENU
		TEST_FOR_JUMP_TO_MAINMENU( ss5.c64CycleCount, ss5.resetCounter )
		#endif

		if ( ss5.resetCounter > 30 && ss5.resetReleased )
			callbackReset();

		asm volatile ("wfi");
	}

	// and we'll never reach this...
	m_InputPin.DisableInterrupt2();
	m_InputPin.DisableInterrupt();
}


#ifdef COMPILE_MENU
void KernelSS5FIQHandler( void *pParam )
#else
void CKernelSS5::FIQHandler (void *pParam)
#endif
{
	register u8 *flashBank = &ss5.flash_cacheoptimized[ ss5.curBank * 8192 * 2 ];
	register u32 D;

	START_AND_READ_ADDR0to7_RW_RESET_CS

	UPDATE_COUNTERS( ss5.c64CycleCount, ss5.resetCounter, ss5.resetPressed, ss5.resetReleased, ss5.cyclesSinceReset )

	// starting from here we are in the CPU-half cycle
	WAIT_AND_READ_ADDR8to12_ROMLH_IO12_BA

	if ( ss5.hasKernal && ROMH_ACCESS && KERNAL_ACCESS && ( !ss5.ultimax || !ss5.active ) )
	{
		WRITE_D0to7_TO_BUS( kernalROM[ GET_ADDRESS ] );
		FINISH_BUS_HANDLING
		return;
	}

	if ( CPU_READS_FROM_BUS && ROML_ACCESS )
	{
		if ( ss5.exportRAM )
			D = ss5.ram[ ss5.curRAMBank * 8192 + GET_ADDRESS ]; else
		if ( ss5.active )
			D = *(u8*)&flashBank[ GET_ADDRESS * 2 ]; else
			D = 0;
		WRITE_D0to7_TO_BUS( D )
		FINISH_BUS_HANDLING
		return;
	}
	if ( CPU_READS_FROM_BUS && ROMH_ACCESS && ss5.active )
	{
		D = *(u16*)&flashBank[ GET_ADDRESS * 2 ];
		D >>= 8;
		WRITE_D0to7_TO_BUS( D )
		FINISH_BUS_HANDLING
		return;
	}

	// read from SS5's ROM visible in IO1
	if ( CPU_READS_FROM_BUS && IO1_ACCESS && ss5.active )
	{
		//register u32 bank = ss5.curBank >> 1;
		//register u32 shift = ss5.curBank & 1 ? 8 : 0;
		//D = ss5.flash_cacheoptimized[ bank * 8192 * 2 + ( (30 << 8) | GET_IO12_ADDRESS ) * 2 ];
		//D >>= shift;
		D = *(u8*)&flashBank[ ( (30 << 8) | GET_IO12_ADDRESS ) * 2 ];
		WRITE_D0to7_TO_BUS( D )
		FINISH_BUS_HANDLING
		return;
	}

	if ( CPU_WRITES_TO_BUS )
		ss5.countWrites ++; else
		ss5.countWrites = 0;

	if ( CPU_WRITES_TO_BUS && IO1_ACCESS && ss5.active )
	{
		READ_D0to7_FROM_BUS( D );

		ss5.reg = D;
		ss5.curBank = ( ( D >> 3 ) & 2 ) | ( ( D >> 2 ) & 1 );

		// or 0 is 8k RAM?
		ss5.curRAMBank = ss5.curBank;

		// 0: GAME = 0, EXROM = 1
		// 1: GAME = 1, EXROM = 1
		// 2: GAME = 0, EXROM = 0
		// 3: GAME = 1, EXROM = 0
		u32 m = ( D & 3 ) | ( ( D & 8 ) ? 4 : 0 );
		ss5Config( m );

		ss5.active = 1 - ( ( D >> 3 ) & 1 );
		if ( m == 0 ) // ultimax mode
			ss5.ultimax = 1; else
			ss5.ultimax = 0;
		if (!ss5.active)
			ss5.ultimax = 0;

		if ( ( ( D >> 1 ) & 1 ) == 0 )
        {
			ss5.exportRAM = 1;
        } else
        {
			ss5.exportRAM = 0;
        } 

		//if ( ( ( m & 3 ) == 1 ) || ( ( m & 3 ) == 3 ) )
		if ( ( D & 1 ) == 1 )
			SET_GPIO( bNMI ); 
	}

	if ( CPU_WRITES_TO_BUS && ROML_ACCESS && ss5.exportRAM && ss5.active )
	{
		READ_D0to7_FROM_BUS( D );
		ss5.ram[ ss5.curRAMBank * 8192 + GET_ADDRESS ] = D;
		return;
	}

	// freeze
	if ( BUTTON_PRESSED && ( ss5.lastFreezeButton == 0 ) )
	{
		ss5.lastFreezeButton = 500000; // debouncing
		ss5.freezeNMICycles  = 10;
		ss5.countWrites      = 0;

		ss5.active = 1;
		ss5.ultimax = 1;
		ss5.exportRAM = 1;
		ss5.curBank = 0;
		ss5.curRAMBank = 0;
		ss5Config( 0 );

		CLR_GPIO( bNMI | bCTRL257 ); 
		RESET_CPU_CYCLE_COUNTER
		return;
	}

	// debouncing
	if ( ss5.lastFreezeButton > 0 )
		ss5.lastFreezeButton --;

	// and go to ultimax mode once the CPU is ready
	if ( ss5.freezeNMICycles && ss5.countWrites == 3 )
	{
		SETCLR_GPIO( bEXROM, bGAME | bCTRL257 ); 
		ss5.ultimax = 1;
		ss5.freezeNMICycles = 0;
		RESET_CPU_CYCLE_COUNTER
		return;
	} 

	OUTPUT_LATCH_AND_FINISH_BUS_HANDLING
}

