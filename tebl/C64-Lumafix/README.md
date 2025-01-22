# C64 Lumafix

Based on Lumafix64 by e5frog over at the lemon64.com forums, this is a somewhat simplified version that should be easier to solder together for beginners with access to a reasonable set of soldering tools. Gone are the slightly confusing surface mount options that's mostly only suitable for those with more advanced skills or aim to have them premade with components from a fabrication company.

For those not familiar with the Lumafix board, this is a board that attempts to minimize the amount of vertical banding that to some extent occurrs on most Commodore 64 computers when connected to a modern TV (C64C and other short boards seem to be affected the most). It doesn't remove the problem completely since it doesn't solve the root cause of them, but it cancels out most of the effect - granting you the best visual clarity you can get from the original hardware! The board plugs in between the VIC-II chip and your Commodore 64 motherboard.

![Lumafix installed](https://github.com/tebl/C64-Lumafix/raw/master/Gallery/2018-10-06%2023.15.37.jpg)

All of the components have, where applicable, been picked out so that it can be built as cheap as possible for people wanting to do this as a fun project in the home workshop (or kitchen table if that is what you have). For a complete list of components, see the BOM below. 

## Building the board
The recommended sequence of components to be soldered in place is as follows:

1. Check errata below for relevant changes
2. Read through the secion "Signal bypass" below if you have a revision B or later board, decide which functionality you want the board to have.
2. Solder in place the round pin headers into the position marked U2, the 40 pin strip should be cut in half so that you have two sections with 20 pins each. Take a look at the PCB, see two sections with 2x20 holes in them? These pins should be soldered on the top side of the PCB in the right set of 20 pin holes. The rest of the components from here on in are soldered on the backside as usual! You can use a spare 40 pin socket to ensure that the pins line up nicely before soldering them in place, if the board is going into a Commodore 64C you should shorten the pins with so that the board fits flush with the socket.
2. Solder in the three ceramic capacitors, note that C3 has a different value than the other two.
3. Solder in a 40 pin socket for U3, a 14 pin socket for U1. Note that sockets have an indent on one side indicating pin 1, this should be matched to a similar indent on the PCB silkscreen.
4. Solder the three potentiometers into place with the adjustment screw down towards the bottom. Temporarily hold them in place using tape to get the orientation just right if you like everything to be lined up perfectly, solder one pin in place and change orientation - heat up solder joint and reorient it until satisfied. Solder the rest of the pins now!

Inspect the board thoroughly until you are satisfied that none of the pins haven't been shorted together, to get a clean finish I recommend cleaning away any flux residue with some nail-polish remover without perfume (dilute it with a little IPA). Finally clean it again using IPA and some cotton buds. If you have access to the stuff, there are dedicated PCB-cleaning solutions that can be purchased though the first option can be easier to get depending on where you live.

![Step 1](https://github.com/tebl/C64-Lumafix/raw/master/Gallery/2018-10-07%2001.00.25.jpg)
![Step 2](https://github.com/tebl/C64-Lumafix/raw/master/Gallery/2018-10-07%2000.57.11.jpg)
![Step 3](https://github.com/tebl/C64-Lumafix/raw/master/Gallery/2018-10-06%2001.15.19.jpg)

### Signal bypass
When developing the [C64 Modulator Replacement](https://github.com/tebl/C64-Modulator-Replacement), there was an increase in the amount of visible jailbars on the screen (of the kind the Lumafix was made to smoothe over). This is due to crosstalk between video signals travelling across the motherboard in close proximity to other signals, the Lumafix attempts to remove these by adding the reverse of the same signals. I did however find that most of the issues disappear if the signals are run directly between the modulator board and the Lumafix with most of the circuitry no longer needed in this combination - video signals are wired directly to the modulator instad (bypassing the noise on the motherboard itself).

- If you want to build a standard revision B Lumafix board **without** replacing the modulator, then you should install two pieces of wire across the two jumpers marked CHR_SEP and LUM_SEP.
- If you want to build a standard revision B Lumafix board for use **with** the modulator replacement, install a piece of wire across pin 1-2 of the bottom potentiometer footprint. Run CHR/LUM wires from Lumafix board to the modulator replacement.

![Revision B passthrough](https://github.com/tebl/C64-Lumafix/raw/master/Gallery/rev_b_passthrough.png)

### Errata
- No issues noted so far, but thanks for checking!

## Installing the board
Did all the checking for shorts, board cleaning and ready to continue? If not, do so now (I'll wait)! Locate the VIC-II chip, it's a 40 pin IC like the board we just soldered together - mine was marked 8565 - to be sure you get the right one, check the [Wikipedia](https://en.wikipedia.org/wiki/MOS_Technology_VIC-II#List_of_VIC-II_versions) site for various IC version numbers to look for.

Now, this Commodore 64 you've opened up is probably over 30 years old so don't touch anything that you don't need to. When removing the IC it may appear stuck - don't yank on it, just carefully ease it out. If you don't have a dedicated IC-extractor, you _can_ use a flat screwdriver - just loosen it slightly from each side until it comes loose, take care not to scrape it against the motherboard under the socket! Insert the extracted IC into the board you just built, matching the indent to the socket while taking care that all pins goes into the socket and don't get stuck or bent underneath it. Insert the board into the now empty socket on the motherboard, taking care to match the orientation to the socket.

Turn all the potentiometers anti-clockwise all the way until you can hear you're at the beginning, with the Commodore 64 adjust the top one until you get the picture as good as it can get. The middle one will dial it in the rest of the way until you are completely satisfied with the startup screen on the Commodore 64. The bottom potentiometer adjusts phase-variance on the color signal, for this you may need to play around with it until you are satisfied with the results - mostly this will help alleviate slight ghosting effects on coloured objects on the screen, but for the most part just dial in the prettiest blue startup screen you can find and come back to it later if you find that you need to.

Note that the values dialed in will be very specific for the TV you'll be using, so test it out on the TV-setup you'll be using. Some TVs need more help than others, if you're lucky enough to still own a CRT you probably won't need this in the first place!

If you decided to build a revision B, or later version, of the Lumafix with signal bypass you need to run wires directly from the top of the board to the relevant points on the modulator replacement (these are labelled LUM and CHR on both boards, wire to the top pads on the modulator replacement board).

![Lumafix with modulator replacement](https://github.com/tebl/C64-Lumafix/raw/master/Gallery/2019-09-25%2023.01.22.jpg)

# Schematic
The supplied KiCad files should be sufficient as both a schematic and as a  starting point for ordering PCBs (basically you could just zip the contents of the export folder and upload that on a fabrication site), the schematic is also available in [PDF-format](https://github.com/tebl/C64-Lumafix/raw/master/export/Lumafix.pdf) and this is what you'll need to print and work your way through this things don't work as expected after assembly.

# BOM
Most parts should be easy to get a hold of from your favourite local electronic component shop, but given that I don't have access to such shops where I live so everything was based on whatever I could get cheapest from ebay/AliExpress (free shipping, plan on usually waiting 3-4 weeks though). You don't need the newest and fastest components, after all it's going into a 80s-era computer so don't go overboard unless you want to (you can replace 74LS ICs with 74HCT or 74ALS if you want something newer). Most components have been produced by various manufacturers, these mostly do the same thing without any relevant differences so just go for the cheapest.

Some vendors will have the same ICs in different form factors, the ones you want will often be specified as being in the form of a DIP/PDIP package. Usually you'll want sockets for each of the ICs as well, a bag of assorted sockets should be easily available without setting you back more than a couple bucks. With the sockets in place, you don't need to dread having to remove an IC later, this leads to easier fault finding and you can even "borrow" them for other projects later! For the pin headers, you should order the round pin ones since the standard ones will easily break the IC socket and you don't want to save pennies if it costs you a Commodore 64 mainboard!

| Reference    | Item                                  | Count |
| ------------ | ------------------------------------- | ----- |
| PCB          | Fabricate using Gerber files ([order](https://www.pcbway.com/project/shareproject/Commodore_64_Lumafix__revision_B_.html?inviteid=88707))  |     1 |
| C1-C2        | 47pf ceramic capacitor                |     2 |
| C3           | 100nF ceramic capacitor               |     1 |
| U1           | 74LS14 DIP-14                         |     1 |
| U2           | 40 pin round pin header (break in 2)  |     1 |
| U3           | DIP-40 IC Socket                      |     1 |
| RV1-RV3      | 3296W Potentiometer 2k Ohm            |     3 |

Please use the order link above when signing up to help me support this project, I'll get a small discount on future orders and hopefully that means I can afford to keep developing new and exciting modules in the future. By using the URL you won't have to deal with the rather daunting order forms for PCB fabrication, but if you want to go that route they'll probably want a zipped-up copy of the files in the export-directory!