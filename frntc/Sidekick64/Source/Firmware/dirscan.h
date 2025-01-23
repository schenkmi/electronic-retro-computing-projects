/*
  _________.__    .___      __   .__        __        _________   ________   _____  
 /   _____/|__| __| _/____ |  | _|__| ____ |  | __    \_   ___ \ /  _____/  /  |  | 
 \_____  \ |  |/ __ |/ __ \|  |/ /  |/ ___\|  |/ /    /    \  \//   __  \  /   |  |_
 /        \|  / /_/ \  ___/|    <|  \  \___|    <     \     \___\  |__\  \/    ^   /
/_______  /|__\____ |\___  >__|_ \__|\___  >__|_ \     \______  /\_____  /\____   | 
        \/         \/    \/     \/       \/     \/            \/       \/      |__| 
 
 dirscan.h

 Sidekick64 - A framework for interfacing 8-Bit Commodore computers (C64/C128,C16/Plus4,VC20) and a Raspberry Pi Zero 2 or 3A+/3B+
            - code for reading/parsing D64 files, reading directories etc.
 Copyright (c) 2019-2022 Carsten Dachsbacher <frenetic@dachsbacher.de>

 .d64 reader below adapted from d642prg V2.09, original source (C)Covert Bitops, (C)2003/2009 by iAN CooG/HokutoForce^TWT^HVSC

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
#ifndef _dirscan_h
#define _dirscan_h

#include <SDCard/emmc.h>
#include <fatfs/ff.h>
#include <circle/util.h>
#include "helpers.h"
#include "mempool.h"

#define D64_GET_HEADER	( 1 << 24 )
#define D64_GET_DIR		( 1 << 25 )
#define D64_GET_FILE	( 1 << 26 )
#define D64_COUNT_FILES ( 1 << 27 )

#define DISPLAY_LINES 19
#define DISPLAY_LINES_VIC20 16

typedef struct
{
	u8	name[ 256 ];
	u32 f, parent, next, level, size;

	u32 vc20; // stores: high-byte of start/end addr
	union {
		u8 vc20flags;
		u8 c64flags;
	};
} __attribute__((packed)) DIRENTRY;

// file type requires 3 bits
#define SHIFT_TYPE		16

#define DIR_MUSIC_FILE	(1<<20)
#define DIR_KERNAL_FILE	(1<<21)
#define DIR_SID_FILE	(1<<22)
#define DIR_LISTALL 	(1<<23)
#define DIR_SCANNED 	(1<<24)
#define DIR_UNROLLED	(1<<25)
#define DIR_D64_FILE	(1<<26)
#define DIR_CRT_FILE	(1<<27)
#define DIR_PRG_FILE	(1<<28)
#define DIR_DIRECTORY	(1<<29)
#define DIR_FILE_IN_D64	(1<<30)
#define DIR_BIN_FILE	(1<<31)

// vc20flags
#define CART20        1
#define ITEM_SELECTED 128

// c64flags
#define DONT_SHOW_ON_C64	1

#define MAX_DIR_ENTRIES		16384
extern DIRENTRY *dir;//[ MAX_DIR_ENTRIES ];
extern s32 nDirEntries;

extern void scanDirectoriesVIC20( char *DRIVE );
extern void printBrowserScreen();
extern int printFileTree( s32 cursorPos, s32 scrollPos );
extern int d64ParseExtract( u8 *d64buf, u32 d64size, u32 job, u8 *dst, s32 *s = 0, u32 parent = 0xffffffff, u32 *nFiles = 0, char *filenameInD64 = 0 );


#endif
