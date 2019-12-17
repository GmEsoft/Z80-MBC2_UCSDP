Z80-MBC2_UCSDP
==============


64K UCSD p-System IV.0 C/SBIOS V1.3 for Z80-MBC2 - (C) 2019 by GmEsoft
----------------------------------------------------------------------


This  is a  custom bootstrap  loader to  load  the `Adaptable p-System IV`  for 8080/Z-80  on the  Z80-
MBC2 board.

The  loader was initially designed  to replace the `autoboot.bin` file on the SD card. It will mount one
or  more  UCSD Pascal  disk images, then  load and execute  the secondary  bootstrap from the first disk
image.

The  bootloader  can also be built  as a bootloader  for an additional disk  set. In this case, the disk
image  files will need to  be renamed from `DS2Nxx.DSK` to `DSmNxx.DSK`, with `m` = the disk set number. So,
for the 4th disk set, the DSK names will begin from `DS3N20.DSK`.

Two  disk images have been prepared for this system. They have been converted from the original 77-track
8 inch floppy disk images ; the link to the location from where the disk images is given below.

The  new  disk image format  for the Z80-MBC2 board  is of up  to 77 tracks  of 128 sectors, each of 128
bytes,  for  a maximum of 1.2  megabytes. The images are  stored in files `DSnNxx.DSK`, with `xx` starting
from  `20` for the first disk. The disk set 'n' must be chosen from the config screen, so for the disk set
number  `2`, we have  to first activate CP/M 3.0 using  the menu option `8: Change Disk set` ; then select
`4: Autoboot` to load UCSD Pascal.

In the provided set:
- `DS2N20.DSK` = disk 0 = `SYS1:` (loaded as unit `#4`);
- `DS2N21.DSK` = disk 1 = `SYS2:` (loaded as unit `#5`);



Quick startup procedure:
------------------------

1. Copy the provided `autoboot.bin` file and the 2 disk images `DS2N20.DSK` and 
`DS2N21.DSK` to the root folder of the SD card.

2. Start the Z80-MBC2 with the USER button pressed, to get the config menu.

3. Select Disk Set 2 (CP/M 3.0) using the menu item `8: Change Disk Set`.

4. Choose boot mode `4: Autoboot`.

5. Enjoy UCSD Pascal.



Regional keyboard mapping:
--------------------------

This  version  of  the  bootloader  contains  a  keyboard  translation map for the `BE` (Belgian) keyboard. By default the
original  `US` keyboard layout of the UTERM is used. To switch to the `BE` layout, if a `BE` keyboard is connected to the PS/2
port,  press  `Ctrl-Shift-)` then `B`. To switch back to the `US` layout, press `Ctrl-Shift-)` then `U`. On the `US`
keyboard, the key combination is `Ctrl-Shift-_` ; the `_` key is immediately at the right of the `0` key.

Because  the non-ASCII chars (accented letters, symbols) can't be handled by the terminal, their corresponding keys have
been redefined as follows:

    From:  é    §    è    ç    à    °    ^    ¨    ù    `    £    ²    ³   ^µ   ^$
    To:    [    #    ]    ^    @    \    {    }    `    <    >    |    ~    |    ~



With  the bootloader  version 1.3, it is also  possible to configure the initial keyboard mapping in the
boot setup manu. See below.



Bootloader setup menu:
----------------------

It  is  possible to configure some  settings of the UCSD  p-System before booting it. Those settings are
stored  in the last sector of the first disk's first track, track 0 512-byte sector 31, in the last 128-
byte subsector (usually unused). Please note that the UCSD p-System works with 128-byte logical sectors,
but the host Z80-MBC2 works using 512-byte physical sectors (FAT-32).

To  access the  setup menu, the USER  button needs to be  pressed while launching the p-System. Boot the
system  while holding the USER  button down, keep the button down while pressing the number `3` (to boot
the loader using the selected disk set) or `4` (to boot the custom AUTOBOOT.BIN file).

A screen like this one will appear:


    64K UCSD p-System IV.0 C/SBIOS V1.3 for Z80-MBC2, Copyright (C) 2019 by GmEsoft
    Build: Sep 05 2019 - 09:36:01

    Scanning Volumes: [++++++++++++++++++++++++++++++++++++++++++++++++++]

    Available volumes:
    20:SYS1C     21:SYS2      22:INTERP    23:ADVX      24:ADVSRC    25:SYSCPM1
    26:STARTUP   27:UTIL1     28:ASM1      29:STARTRK   30:GAMES     31:KBDB
    32:KBDBX     33:88SYS     34:88SYS     35:SYSCPM1   36:Z8SYS     37:INTCPM
    38:INTZ80    39:STARTUP   40:SYS1      41:STARTUP   42:SYS1      43:SYS3
    44:SYSCPM1   45:UTIL1     46:UTIL1     48:ASM1      50:SYS1S     51:PASCAL1
    80:88SYS     81:ASM1      82:ASM2      83:ASM3      84:INTCPM    85:STARTUP
    86:SYS1      87:SYS2      88:SYS3      89:SYSCPM1   90:UTIL1     91:UTIL2


    Select volume numbers (<T>ens, <U>nits, <ret> to accept):
    #04 20:SYS1C   ?
    #05 21:SYS2    ?
    #09 51:PASCAL1 ?
    #10 23:ADVX    ?
    #11 24:ADVSRC  ?
    #12 22:INTERP  ?

    Select BIOS Type (<S>BIOS, <C>BIOS, <ret> to accept):CBIOS?

    Select keyboard layout (<ret> to accept):U?


The configurable settings are:

-  the mapping  of 6 logical UCSD p-System 'block'  units (`#4`-`#5`, `#9`-`#12`) to 6 physical disk image files
`DSn0xx.DSK`: the value of `n` is fixed, it is the disk set number; the value of `xx` is in the range `00`-`99`;

-  the type  of BIOS: `CBIOS` and `SBIOS`:  `CBIOS` must be selected  if the boot disk image is `CP/M Adaptive
System`  and `SBIOS`  must be selected if the boot  disk image is `Full Adaptive System`. The jump vectors
and the registers used to exchange parameters between the system and the BIOS routines are different and
incompatible (more info later about this);

- the initial keyboard mapping: currently choice is given among `US` and `BE` mappings.



How to create the set of disks ?
--------------------------------

1: Download  the  original  disk  image  from the web. I chose to download them from the Z80-SIM page at `ClassicCmp.org`,
at this URL:
	http://www.classiccmp.org/cpmarchives/cpm/mirrors/www.unix4fun.org/z80pack/ftp/ucsd-iv.tgz

2: Extract the 2 disk images from the archive: `ucsd-iv-1.dsk` and `ucsd-iv-2.dsk`.

3: Convert the disk images to the Z80-MBC2 format using the provided tool `makedisk` (source code: `makedisk.c`):

    makedisk -M:DS2N20.DSK -I:ucsd-iv-1.dsk -L:128 -SI:26 -SO:128 -F:T0 -T:T1 -D:T0 -F:T1 -T:T77 -D:T1 -P:T77
    makedisk -M:DS2N21.DSK -I:ucsd-iv-2.dsk -L:128 -SI:26 -SO:128 -F:T0 -T:T1 -D:T0 -F:T1 -T:T77 -D:T1 -P:T77

This  will  extract  the  first  26-sector track of the source images and copy the sectors to the first 128-sector track
of  the  new disk images. Then data from the tracks 2 to 77 from the source images to the new images starting from track
2, adjusting for the new track size. Finally the images will be padded for the 1.2 megabytes capacity.

The syntax for makedisk is:

    Makedisk -M:outfile [-L:lrl] [-SI:input track size] [-SO:output track size]    
             [ [-I:infile] [-XI:input interleave] [-KI:input skew]
               -F:[T]from -T:[T]to -D:[T]dest ... ]
             [ -P:[T]padsize ]
                 T for whole tracks, else sectors
    Defaults:
             -L:128   bytes per sector
             -SI:26   sectors per side for input (8inch format)
             -SO:128  sectors per side for output (Z80-MBC2 image format)
             -XI:1    (no interleave)
             -KI:0    (no skew)

4: Start the UCSP p-System.

5: Set today's date using `F(iler - D(ate`.

Example: `24-Aug-19`

6: Extend the disk size of units `#4` and `#5` using `X(ecute - SYS2:DISKSIZE`:

    Change directory size of what unit? (4,5,9..12) --> 4, then 5

    What is the new directory size in 512 byte blocks ? --> 2432(=76*32)

7:  Configure  the  p-System  to the characteristics of the UTERM terminal (30 lines of 80 characters), so the scrolling
in the editor will perform correctly, using `X(ecute - SETUP`:

    SETUP: C(hange T(each H(elp Q(uit --> C
    CHANGE: S(ingle) P(rompted) ... --> P
    --> reply N until getting FIELD NAME = SCREEN HEIGHT, then reply Y
    NEW VALUE: --> 30
    Then reply N to the remaining prompts.
    Type L to check that the new value has been accepted.
    --> Q(uit) to exit change mode
    --> Q(uit) to save new settings --> D(isk), then E(xit)

The  new settings are now saved to `SYS1:NEW.MISCINFO`. Rename this file to `SYS1:SYSTEM.MISCINFO`, overwriting the existing
file, using `F(iler - C(hng`:

    Change what file ? --> NEW.MISCINFO
    Change to what ? --> SYSTEM.MISCINFO
    Remove old SYS1:SYSTEM.MISCINFO ? --> Y

8: Alter the `GOTOXY` procedure, to make it accept row numbers between `1` and `30`.

    F(iler - G(et the workfile --> gotoxy
    Q(uit the Filer
    E(dit
    Change line 13 to read:
        IF Y>29 THEN Y:=29;
    (go over the 3; D(let, press the spacebar once then Ctrl-Z; I(nsrt, hit 9 then Ctrl-Z)
    Q)uit the editor and U)pdate the workfile
    C)ompile the file
    F)iler - S)ave
    Save as SYS1:GOTOXY ? --> Y
    Q)uit the Filer

Then bind `GOTOXY` into the system library:

    X)exute --> SYS2:LIBRARY
    Output file? --> NEW.PASCAL
    Input file? --> GOTOXY
    --> T(ext (Interface sections will not be copied)
    --> 0
    From slot #? 0 --> spacebar
    To slot #? --> 16
    --> N(ew
    Input file? --> SYSTEM.PASCAL
    --> E(very (get all slots except GOTOXY)
    --> Q(uit LIBRARY

Now using  F)iler rename  `NEW.PASCAL` to `SYSTEM.PASCAL`:

    --> F)iler - C)hng
    Change what file ? --> NEW.PASCAL
    Change to what ? --> SYSTEM.PASCAL
    Remove old SYS1:SYSTEM.PASCAL ? --> Y
    --> K)rnch to compress the directory
    Crunch what vol ? --> :
    From end of disk, block 2432  ? (Y/N) --> Y

Reboot the system. The p-System should now use all 30 terminal's lines.


	
How to build the bootloader ?
-----------------------------

1: Download the ZMAC assembler from here (or use `getzmac.sh`):
	http://48k.ca/zmac.html

2: Execute `MAKEIT.BAT`. This will create the file `autoboot.bin` that will be copied to the SD card.


	
