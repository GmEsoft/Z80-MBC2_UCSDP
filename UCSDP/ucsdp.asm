;=============================================================================
;	UCSD p-System Bootstrap Loader and C/SBIOS V1.32 for Z80-MBC2
;=============================================================================
;
;	Build:
;		zmac UCSDP.asm --od UCSDP --oo cim,lst -c -s -g
;	ZMAC: 8080/Z-80 Cross-Assembler for Windows
;		http://48k.ca/zmac.html
;=============================================================================


;-----------------------------------------------------------------------------
; 	Settings
DISK0	EQU	20		; First disk number in set (boot disk)
DEBUG	EQU	0		; Debug mode

;-----------------------------------------------------------------------------
;	This auto-generated file defines $DATE and $TIME macros
	include	"datetime.asm"

CR	EQU	0DH
LF	EQU	0AH
ESC	EQU	1BH

;-----------------------------------------------------------------------------
;	Macro to break in emulator
$BREAK	MACRO
	IF	DEBUG
	DB	0EDH,0F5H	; emulator $BREAK extended opcode
	ENDIF
	ENDM
				;

;-----------------------------------------------------------------------------
;	IPL entry
	ORG	0
	JP	BOOTZ80		; bootstrap routine
	DC	38H,0		; fill RST vector area with NULL

;-----------------------------------------------------------------------------
;	Bootstrap routine, executed at first IPL
BOOTZ80:
	DI

	LD	HL,$BIOS_BEGIN	; Install CBIOS in high memory (at first IPL)
	LD	DE,BIOS		;
	LD	BC,$BIOS_END-$BIOS_BEGIN
	LDIR			;
	LD	SP,BIOS		; CBIOS GOES HERE, RESET THE STACK
	CALL	SETUP		;

;-----------------------------------------------------------------------------
;	Second IPL entry point
MAIN:				;LET'S BOOT UCSD PASCAL
;
;	  This program is a skeletal outline for a 128-byte primary
;	bootstrap for automatically booting to UCSD Pascal (tm).
;	  Set the correct origin for this program for your system, set
;	'MSIZE' for the appropriate number of kilobytes of RAM memory
;	for your system, set the appropriate parameters describing your
;	disk environment and finally write a very low level disk read
;	routine to allow reading in the secondary bootstrap and your
;	CBIOS off the disk and into RAM.
;	  The program 'CPMBOOT' on the UCSD Pascal distribution disk will
;	then use this program and your CBIOS to generate an automatically
;	booting UCSD Pascal system.
;
;	Adapted for Z80SIM, August 2008, Udo Munk
;	Adapted for Z80-MBC2, August 2019, Michel B.
;
BOOT	EQU	8200H		; SECONDARY BOOTSTRAP LOADED HERE
SECNUM	EQU	16		; SECONDARY BOOTSTRAP IS 16 SECTORS LONG
SECSEC	EQU	3		; SECONDARY BOOTSTRAP ON THIS SECTOR
;
PBOOT:	LD	SP,BIOS		; CBIOS GOES HERE, RESET THE STACK
	LD	HL,READSEC$	; Reading sec bootstrap
	CALL	PUTS		; Display message (at first IPL only)
	LD	HL,BOOT		; LOAD BOOT BASE ADDRESS
	LD	D,SECNUM	; D - # OF SECTORS TO READ
	LD	E,SECSEC	; E - STARTING SECTOR
	CALL	READIT		; READ IN SECONDARY BOOTSTRAP
	LD	HL,128		; MAXIMUM NUMBER OF BYTES PER SECTOR
	PUSH	HL
	LD	HL,128		; MAXIMUM NUMBER OF SECTORS IN TABLE
	PUSH	HL
	LD	HL,0		; TRACK-TO-TRACK SKEW
	PUSH	HL
	LD	HL,1		; FIRST INTERLEAVED TRACK
	PUSH	HL
	LD	HL,1		; 1:1 INTERLEAVING
	PUSH	HL
	LD	HL,128		; BYTES PER SECTOR
	PUSH	HL
	LD	HL,128		; SECTORS PER TRACK
	PUSH	HL
	LD	HL,77		; TRACKS PER DISK
	PUSH	HL
	LD	HL,BIOS-2	; TOP OF MEMORY (MUST BE WORD BOUNDARY)
	PUSH	HL
	LD	HL,0100H	; BOTTOM OF MEMORY
	PUSH	HL
	LD	DE,BIOS+3	; START OF THE SBIOS (JMP WBOOT)^
$BIOS	EQU	$-2
	PUSH	DE
	PUSH	HL		; STARTING ADDRESS OF INTERPRETER
;	LD	HL,0
;	PUSH	HL
	LD	HL,BOOTING$	; 'Booting to UCSD Pascal'
	CALL	PUTS1		; Display message (after reset)
	LD	A,0C3H		; For next reset actions:
	LD	(0),A		; - skip CBIOS move to high memory
	LD	HL,MAIN		;
	LD	(1),HL		;
	LD	A,0C9H		; - deactivate first IPL display routine
	LD	(PUTS),A	;
	LD	HL,BOOT
	LD	A,(HL)
	CP	0C3H
	JP	NZ,NOSECBT
	JP	(HL)		; ENTER SECONDARY BOOTSTRAP

;-----------------------------------------------------------------------------
;
;	  READIT must read the number of sectors specified in the D
;	reg, starting at the sector specified in the E reg, into the
;	memory location specified in the HL pair.
;
READIT:
	PUSH	HL		; Save loading pointer
	LD	C,0		; SELECT DRIVE 0
	CALL	SELDSK
	LD	C,0
	CALL	SETTRK		; SELECT TRACK 0
	POP	HL		; get back loading pointer
L1:
	LD	C,E		; SELECT SECTOR
	CALL	SETSEC
	LD	C,L		; SET DMA ADDRESS LOW
	LD	B,H		; SET DMA ADDRESS HIGH
	CALL	SETDMA
	CALL	READ		; READ SECTOR
	OR	A		; READ SUCCESSFULL?
	JP	Z,L2		; YES, CONTINUE
	DI			; FAILURE, HALT CPU
	HALT
L2:
	DEC	D		; SECTORS = SECTORS - 1
	RET	Z		; RETURN IF ALL SECTORS LOADED
	INC	E		; NEXT SECTOR TO READ
	LD	BC,80H		; 128 BYTES PER SECTOR
	ADD	HL,BC		; DMA ADDRESS + 128
	JP	L1		; GO READ NEXT

;-----------------------------------------------------------------------------
;	Message display routine for first IPL
PUTS:	NOP
;	Message display routine for subsequent IPL
PUTS1:	LD	A,(HL)		; Get char from message
	OR	A		; End of message (NUL) ?
	RET	Z		; done if yes
	LD	C,A		;
	CALL	CONOUT		; SEND CHAR TO TERMINAL
	INC	HL		; bump message ptr
	JR	PUTS1		; and loop


;-----------------------------------------------------------------------------
;	Booting UCSD Pascal message, must fit below 0100H
;
BOOTING$:
	DB	CR,LF,LF,'Booting UCSD p-System',0

;	END of area 0000h-0100h which will remain untouched by P-System
	ASSERT	$ < 0100H	; enforce that ...

;-----------------------------------------------------------------------------
;	Reading secondary bootstrap message
READSEC$:
	DB	CR,LF,'Reading Secondary Bootstrap',0

;-----------------------------------------------------------------------------
;
;	copyright text
;
;	UCSD p-System IV CBIOS for Z80-MBC2
;
;	Copyright (C) 2019 by GmEsoft
;
;
HELLO$:	DB	ESC,'H',ESC,'J'
	DB	'64K UCSD p-System IV.0 C/SBIOS V1.32 for Z80-MBC2, '
	DB	'Copyright (C) 2019-2020 by GmEsoft'
	DB	CR,LF
	DB	'Build: '
	$DATE			; Macro defining build date
	DB	' - '
	$TIME			; Macro defining build time
	DB	CR,LF,0


;-----------------------------------------------------------------------------
;	SETUP DISKS
SETUP:	LD	HL,HELLO$	; Display welcome message
	CALL	PUTS		;
	LD	C,0		; Select DISK0
	CALL	SELDSK		;
	LD	C,0		; Select track 0
	CALL	SETTRK		;
	LD	C,128		; Select last sector of track 0
	CALL	SETSEC		;
	LD	BC,CFGBUF	; Use Config buffer
	PUSH	BC		; Save buffer address
	CALL	SETDMA		;
	CALL	READ		; Read sector into config buffer
	POP	HL		; Restore buffer address
	PUSH	HL		; Copy to IX
	POP	IX		;
	LD	A,(IX+127)	; Check signature byte
	CP	0A5H		;   Last sector's byte == 0A5h
	JR	NZ,DOCFG	; If yes skip applying config
	LD	DE,DSKTBL	; Read disk map table
	LD	BC,6		; 6 bytes
	LDIR			;
	LD	A,(HL)		; Read BIOS type
	LD	($BSTYPE),A	; Save it
	LD	A,80H		; IOS:USER_KEY opcode
	OUT	(1),A		; Send opcode
	IN	A,(0)		; Read user key status
	AND	1		;
	JR	NZ,DOCFG	; Go to config if key pressed

	CALL	SETBIOS		; Install SBIOS if selected
	LD	HL,DOCFG$	; Display 'Press USER key ...'
	JP	PUTS		; And return

;	Do configuration interactively with user
DOCFG:
	LD	HL,SCNDSK$	; Display 'Scanning disk: ['
	CALL	PUTS		;
	LD	B,50		; Prepare progress bar
	LD	C,'-'		;
SCAN1:	CALL	CONOUT		; Display 50 dashes
	DJNZ	SCAN1		;
	LD	C,']'		; and closing bracket
	CALL	CONOUT		;
	LD	HL,SCNDSK$+1	; Re-display 'Scanning disk: ['
	CALL	PUTS		;

	LD	A,0		; Init A = Disk image counter
	LD	IY,DSKNAMES	; Pointer to disk names table

LOOPDSK	PUSH	AF		; Save counter
	LD	(IY),0		; Blank disk name
	LD	HL,DSKTMP	; Use temporary slot for disk access
	LD	(HL),A		; Map scanned image to slot 6
	LD	C,6		; Select disk slot 6
	CALL	SELDSK		;
	LD	C,1		; Select track 1 containing directory
	CALL	SETTRK		;
	LD	C,9		; Select sector 9 containing vol name
	CALL	SETSEC		;
	LD	BC,DIRBUF	; Use directory sector buffer
	PUSH	BC		; Save pointer
	CALL	SETDMA		; Set buffer address
	CALL	READ		; Read 1st directory sector
	POP	HL		; Restore directory buffer address
	OR	A		; Check for error
	JR	NZ,DSKNOK	; Go and Skip image in case of error

	LD	A,(HL)		; Check 1st 6 bytes of sector
	INC	HL		; Bytes 0,1,3,4,5 must equal 0
	OR	(HL)		;
	INC	HL		;
	LD	C,(HL)		; Get byte 2 in C
	INC	HL		;
	LD	B,(HL)		; Get byte 3 in B
	INC	HL		;
	OR	(HL)		;
	INC	HL		;
	OR	(HL)		;
	INC	HL		;
	JR	NZ,DSKNOK	; Go and skip image if the 5 bytes are not all 0

LDSK1	LD	A,C		; Check byte 2
	CP	6		; Must be equal to 6
	JR	Z,DSKOK		; Get vol name if yes

	CP	10		; ... or 10
	JR	Z,DSKOK

	OR	A
	JR	NZ,DSKNOK	; Skip if not

	LD	C,B
	LD	B,0FFH
	JR	LDSK1

;	Disk image OK => get vol name
DSKOK:	LD	C,(HL)		; Get name length in C
	INC	HL		; Bump ptr
	PUSH	IY		; Names table entry to DE
	POP	DE		;
	LD	B,0		; BC = name length
	LDIR			; Copy name
	EX	DE,HL		; End name address to HL
	LD	(HL),0		; Terminate name with 0

;	Point to next disk
DSKNOK:	LD	BC,16		; Bump names table ptr
	ADD	IY,BC		;
	POP	AF		; Restore disk images counter
	INC	A		; Next disk image
	BIT	0,A		; Is Counter even ?
	JR	NZ,DSKNO1	; Skip if not
	PUSH	AF		; Save counter
	LD	C,'+'		; Display a '+' in the progress bar
	CALL	CONOUT		;
	POP	AF		; Restore counter
DSKNO1:	CP	100		; Last disk image reached ?
	JR	C,LOOPDSK	; Loop again if not

;	Display disk names with numbers
	LD	HL,AVAIL$	; Display 'Available volumes:'
	CALL	PUTS		;
	LD	B,0		; init B = Image counter
	LD	C,B		; init C = columns counter
	LD	HL,DSKNAMES	; init HL = vol names table pointer
LISTDSK	LD	A,(HL)		; Is there a name ?
	OR	A		;
	JR	Z,NODSKNM	; Skip displaying vol name if not
	PUSH	BC		; Save BC
	LD	A,B		; Display image number
	CALL	OUTDECA2	;
	LD	C,':'		; Display ':'
	CALL	CONOUT		;
	CALL	PUTS		; Display name pointed by HL
	LD	C,' '		;
	LD	A,10		; Tabulate
	SUB	L		;
	AND	0FH		;
	LD	B,A		; by displaying ' 's
LISTTAB	CALL	CONOUT		;
	DJNZ	LISTTAB		;
	POP	BC		; Restore BC
	INC	C		; Next column
	LD	A,C		; Column counter to A
	CP	6		; 6 columns shown
	JR	C,NODSKNM	; go if not
	LD	C,CR		; Move to next screen line
	CALL	CONOUT		;
	LD	C,LF		;
	CALL	CONOUT		;
	LD	C,0		; Reset column counter
NODSKNM	LD	A,L		; Move vol names table ptr
	OR	0FH		;   to next entry
	LD	L,A		;
	INC	HL		;
	INC	B		; Increment vol number
	LD	A,B		;
	CP	100		; Last vol reached
	JR	C,LISTDSK	; Loop if not
	LD	HL,SELDSK$	; Display 'Select volume numbers ...'
	CALL	PUTS		;
	LD	HL,DSKTBL	; Init disk mapping table ptr
	LD	B,6		; Number of entries = 6
	LD	C,4		; first p-System Logical unit number

LOOPSEL	PUSH	BC		; Save counters
	LD	C,CR		; Cursor to beginning of line
	CALL	CONOUT		;
	LD	C,'#'		; Display '#'
	CALL	CONOUT		;
	POP	BC		; Restore counters
	PUSH	BC		; And save them again
	LD	A,C		; Display logical unit number
	CALL	OUTDECA2	;
	LD	C,' '		; Display ' '
	CALL	CONOUT		;
	LD	A,(HL)		; Display image number
	CALL	OUTDECA2	;
	LD	C,':'		; Display ':'
	CALL	CONOUT		;
	PUSH	HL		; Save disks mapping table pointer
	LD	A,(HL)		; Get image number
	LD	L,A		;   to HL
	LD	H,0		;
	ADD	HL,HL		; Multiply by 16
	ADD	HL,HL		;
	ADD	HL,HL		;
	ADD	HL,HL		;
	LD	DE,DSKNAMES	; Add vol names table origin
	ADD	HL,DE		;
	CALL	PUTS		; Display vol name
	LD	C,' '		; Pad to 8 chars
TABSL1	CALL	CONOUT		; Display ' '
	INC	L		; Increment counter
	BIT	3,L		; reached 8 ?
	JR	Z,TABSL1	; Loop if not
	POP	HL		; Restore disks mapping table pointer
	LD	C,'?'		; Display '?'
	CALL	CONOUT		;
DSELKEY	CALL	CONIN		; Get char from keyboard
	CP	CR		; <RET> pressed ?
	JR	Z,DSELNXT	; Go and accept if yes
	AND	0DFH		; Convert to upper case
	LD	C,10		; 10 to add
	CP	'T'		;   if 'T' pressed
	JR	Z,DSELADD	; go if yes
	LD	C,1		; 1 to add
	CP	'U'		;   if 'U' pressed
	JR	NZ,DSELKEY	; loop if not
DSELADD	LD	A,(HL)		; Add 1 or 10 to the vol number
	ADD	A,C		;
	LD	(HL),A		;
	SUB	100		; Greater than 99 ?
	JR	C,DSELNSB	; Subtract 100 if yes
	LD	(HL),A		;
DSELNSB	POP	BC		; Restore counters
	JR	LOOPSEL		; Loop again displaying new vol name
;	Accept vol number
DSELNXT	LD	C,CR		; Go to next screen line
	CALL	CONOUT		;
	LD	C,LF		;
	CALL	CONOUT		;
	POP	BC		; Restore counters
	INC	HL		; Increment disk mapping table ptr
	INC	C		; Increment logical unit number
	LD	A,C		;
	CP	6		; If logical unit number is 6
	JR	NZ,DSELNX1	;
	LD	C,9		;   change it to 9
DSELNX1	DJNZ	LOOPSEL		; and loop again

	LD	C,LF		; Move to next screen line
	CALL	CONOUT		;

;	Now ask BIOS type
LOOPBS:	LD	HL,SLBIOS$	; Display 'Select BIOS type ...'
	CALL	PUTS
	LD	A,($BSTYPE)	; Get BIOS Type
	LD	C,A
	CALL	CONOUT		; Display 'S' or 'C'
	LD	HL,BIOS$	; Displ 'BIOS'
	CALL	PUTS
LPBS1:	CALL	CONIN		; Get char from keyboard
	CP	CR		; <RET> ?
	JR	Z,OKBIOS	; If yes, accept
	AND	0DFH		; to upper case
	CP	'S'		; 'S' ?
	JR	Z,SAVEBS	; If yes, save it
	CP	'C'		; 'C' ?
	JR	NZ,LPBS1	; Loop if not
SAVEBS:	LD	($BSTYPE),A	; Save BIOS type
	JR	LOOPBS		; And loop

OKBIOS:	LD	C,LF		; Next screen line
	CALL	CONOUT		;
	CALL	CONOUT		;

;	Save new config to DISK0 track 0's last sector
CFGSAVE	LD	HL,DSKTBL	; Copy disk mapping table
	PUSH	IX		;
	POP	DE		;   to config buffer
	LD	BC,6		; 6 bytes to copy
	LDIR			;
	LD	A,($BSTYPE)	; Get BIOS type
	LD	(DE),A		; Save to config buffer
	LD	A,DISK0		; Map slot 4 to DISK0
	LD	(DSKTMP),A	;
	LD	C,6		; Select slot 4
	CALL	SELDSK		;
	LD	C,0		; Select track 0
	CALL	SETTRK		;
	LD	C,128		; Select track 0's last sector
	CALL	SETSEC		;
	LD	(IX+127),0A5H	; Store signature byte in last pos
	PUSH	IX		; Get config buffer's address
	POP	BC		;
	CALL	SETDMA		; Set buffer address
	CALL	WRITE		; Write sector
	CALL	SETBIOS		; Install SBIOS if selected

	RET			; Done, proceed loading secondary bootstrap.

;-----------------------------------------------------------------------------
;	SETUP BIOS
SETBIOS	LD	A,'C'		; BIOS Type: <C>BIOS or <S>BIOS
$BSTYPE	EQU	$-1
	CP	'S'		; SBIOS type
	RET	NZ		; Return if not
	LD	HL,SBIOS_BEG	; Move SBIOS vector
	LD	DE,BIOS		;   over CBIOS
	LD	($BIOS),DE	; Update vector address for sec bootstrap
	LD	BC,SBIOS_END-SBIOS_BEG
	LDIR			; Move it
	RET			; Done


;-----------------------------------------------------------------------------
;	OUT 'A' IN DECIMAL
OUTDECA2:
	PUSH	BC		; Save BC
	LD	B,10		; Display tens
	CALL	OUTDEC1		;
	ADD	A,'0'		; Display units
	LD	C,A		;
	CALL	CONOUT		;
	POP	BC		; Restore BC
	RET			; Done

OUTDEC1:
	LD	C,'0'-1		; Init tens
OUTDEC11:
	INC	C		; Increment tens
	SUB	B		; Try and Subtract 10
	JR	NC,OUTDEC11	; Loop while OK
	ADD	A,B		; add 10 if A<0
	LD	B,A		; Save to B
	CALL	CONOUT		; Display tens
	LD	A,B		; Restore from B
	RET			; Done

;-----------------------------------------------------------------------------
;	NO SECONDARY BOOTSTRAP
NOSECBT	LD	HL,NOSECB$
	CALL	PUTS1
	HALT
	JR	$

;-----------------------------------------------------------------------------
DOCFG$	DB	LF,'Press USER key while booting to change system setup.',CR,LF,0

SCNDSK$	DB	LF,CR,'Scanning Volumes: [',0

AVAIL$	DB	LF,CR,LF,'Available volumes:',CR,LF,0

SELDSK$	DB	CR,LF,LF,'Select volume numbers (<T>ens, <U>nits, <ret> to accept):',CR,LF,0

SLBIOS$	DB	CR,'Select BIOS Type (<S>BIOS, <C>BIOS, <ret> to accept):',0

BIOS$	DB	'BIOS?',0

NOSECB$	DB	CR,LF,'Secondary bootstrap not present !'
HALTED$	DB	CR,LF,LF,'** SYSTEM HALTED **',0

;-----------------------------------------------------------------------------
	DS	1 + low not $	; go to next 256 bytes boundary
CFGBUF:	DS	128		; Sector buffer for config
DIRBUF:	DS	128		; Sector buffer for directory
DSKNAMES:
	DS	100*16		; Disk names table


;-----------------------------------------------------------------------------
;	SBIOS jump vectors, to move over CBIOS vectors if SBIOS mode selected
;
;
SBIOS_BEG:
	JP	SYSINIT		; Initialize system
	JP	SYSHALT		; Halt system
	JP	CONINIT		; console initialize
	JP	CONSTAT		; console status
	JP	CONREAD		; console character in
	JP	CONOUT		; console character out
	JP	SELDSK		; select disk
	JP	SETTRK		; set track number
	JP	SETSEC		; set sector number
	JP	SETDMA		; set dma address
	JP	READ		; read disk
	JP	WRITE		; write disk
	JP	DSKINIT		; reset disk
	JP	DSKSTRT		; activate disk
	JP	DSKSTOP		; de-act disk
;--- Ext-SBIOS
	JP	PRNINIT		;
	JP	PRNSTAT		;
	JP	PRNREAD		;
	JP	PRNWRIT		;
	JP	REMINIT		;
	JP	REMSTAT		;
	JP	REMREAD		;
	JP	REMWRIT		;
	JP	USRINIT		;
	JP	USRSTAT		;
	JP	USRREAD		;
	JP	USRWRIT		;
	JP	CLKREAD		; system clock read
SBIOS_END:

;-----------------------------------------------------------------------------
;	CBIOS area begin, to move in high memory at first IPL
;
$BIOS_BEGIN
MSIZE	EQU	64		; memory size in kilobytes
BIAS	EQU	(MSIZE*1024)-01A00H
BIOS	EQU	1500H+BIAS	; base of bios
    	PHASE	BIOS		; where the CBIOS code will be moved


;-----------------------------------------------------------------------------
;
;	Jump vector for individual subroutines
;
	JP	CBOOT		; cold start / sys init
WBOOTE: JP	WBOOT		; warm start / sys halt
	JP	CONST		; console status
	JP	CONIN		; console character in
	JP	CONOUT		; console character out
	JP	LIST		; list character out
	JP	PUNCH		; punch character out
	JP	READER		; reader character out
	JP	HOME		; move head to home position
	JP	SELDSK		; select disk
	JP	SETTRK		; set track number
	JP	SETSEC		; set sector number
	JP	SETDMA		; set dma address
	JP	READ		; read disk
	JP	WRITE		; write disk
	JP	LISTST		; return list status
	JP	SECTRAN		; sector translate
	REPT	11
	DB	76H,00H,0C9H	; HALT,NOP,RET
	ENDM

;-----------------------------------------------------------------------------
;
;	individual subroutines to perform each function
;
;-----------------------------------------------------------------------------

;-----------------------------------------------------------------------------
;	Cold boot
CBOOT:
SYSINIT:
	$BREAK			; break into simulator's debugger
	RET			; done

;-----------------------------------------------------------------------------
;	Warm boot / halt
WBOOT:
SYSHALT:
	DI			; interrupts off
;	$BREAK
	HALT			; halt system
	RET			; ret if not halted
;
;
;-----------------------------------------------------------------------------
;	simple i/o handlers
;
;-----------------------------------------------------------------------------
;	console init, return 0ffh if terminal ready, 00h if not
;
CONINIT	XOR	A
	RET

;-----------------------------------------------------------------------------
;	console status, return 0ffh if character ready, 00h if not
;
CONST:
	PUSH	BC		; Save register
	LD	BC,$-$		; Get keyboard poll countdown timer
KBTIMER	EQU	$-2		;
	LD	A,B		; Done counting ?
	OR	C		;
	JR	Z,CONST1	; Go if yes
	DEC	BC		; Do the countdown at each routine call
	LD	(KBTIMER),BC	; Update counter
	LD	A,B		; Done counting ?
	OR	C		;
	JR	NZ,CONST1	; Go if not
	LD	BC,0101H	; for OUT (C),B, with C=1 and B=1
	LD	A,(DIRTY)	; Check if sector buffer is dirty
	OR	A		;
	CALL	NZ,WRITE512	; If yes, write back to disk
CONST1:	POP	BC		; Restore register
	LD	A,83H		; IOS:SYSFLAGS opcode
	OUT	(1),A		; send opcode
	IN	A,(0)		; get console status
	AND	4		; Is char ready in input buffer ?
	RET	Z		; done if not, returning A=0
	LD	A,0FFH		; else return A=0FFh
	RET			;

CONSTAT	CALL	CONST
RETATOC	LD	C,A
	XOR	A
	RET

;-----------------------------------------------------------------------------
;
;	console character into register a
;
CONREAD	CALL	CONIN
	OR	A
	JR	Z,CONREAD
	JR	RETATOC

;-----------------------------------------------------------------------------
;
;	read character into register a from reader device
;
READER: 			; re-route to console input

CONIN:	LD	A,$-$		; Get last key
LASTKEY	EQU	$-1		;
	CP	27		; last key is ESC ?
	JR	NZ,CONIN1	;
	IN	A,(1)		;
	CP	'['		; is it ANSI Esc ?
	JR	NZ,CONIN2	; if yes, swallow '['
CONIN1:	IN	A,(1)		; get character from console
CONIN2:	INC	A		; Is there a char ?
	RET	Z		; done if yes, returning A=0

	DEC	A		; recover char
	LD	(LASTKEY),A	; Save as last key
	RET			; Done


;-----------------------------------------------------------------------------
;
;	list character from register c
;
LIST:				; re-route to console output

;-----------------------------------------------------------------------------
;
;	punch character from register c
;
PUNCH:				; re-route to console output

;-----------------------------------------------------------------------------
;
;	console character output from register c
;
CONOUT:
	LD	A,C		; get char to send
	OR	A		; Check for NUL
	RET	Z		; Don't send if NUL
	LD	A,01H		; IOS:SERIAL_TX opcode
	OUT	(1),A		; Send IOS opcode
	LD	A,C		; Get to accumulator
	OUT	(0),A		; Send character to console
	CP	ESC		; ESC character?
	RET	NZ		; No, done
	LD	A,01H		; SERIAL_TX
	OUT	(1),A		; Send IOS opcode
	LD	A,'['		; Send second lead in for ANSI terminals
	OUT	(0),A		;
	XOR	A		; Clear for SBIOS
	RET			; done

;-----------------------------------------------------------------------------
;
;	return list status (00h if not ready, 0ffh if ready)
;
LISTST: LD	A,0FFH		; List device always ready :)
	RET			; done

;-----------------------------------------------------------------------------
;
;	select disk given by register C
;
SELDSK: LD	A,B		; save B
	LD	B,0		; clear it
	LD	HL,DSKTBL	; Get phys disk table pointer
	ADD	HL,BC		; Add logical disk number
	LD	B,A		; restore B
	LD	A,(HL)		; get physical disk # from table
	LD	(NEWDSK),A	; to be used later when actual I/O ops are performed
	XOR	A		; return A=0 for OK
	LD	H,A		; return code: HL = 0
	LD	L,A		;
	RET			; done

;-----------------------------------------------------------------------------
;
;
;	i/o drivers for the disk follow
;
;	move to the track 00 position of current drive
;	translate this call into a settrk call with parameter 00
;
HOME:	LD	C,0		; select track 0
	;continue to SETTRK

;-----------------------------------------------------------------------------
;
;	set track given by register c
;
SETTRK: LD	A,B		; Save original value of B
	LD	B,0		; (high byte is 0)
	LD	(NEWTRK),BC	; to be used later when actual I/O ops are performed
	LD	B,A		; restore value of B
	XOR	A		; return OK (A=0)
	RET			; done

;-----------------------------------------------------------------------------
;
;	set sector given by register c
;
SETSEC: DEC	C		; Sectors are numbered from O, so decrement
	LD	A,C		; get decremented sect #
	AND	3		; keep lower 2 bits
	LD	(SECOFF),A	; save as sector offset (block of 128 bytes inside the 512 bytes sector)
	LD	A,C		; get decremented sect #
	SRL	A		; move bits 2-7 to pos 0-5, as 512 bytes sector #
	SRL	A		;
	LD	(NEWSEC),A	; save
	INC	C		; restore initial sector 1
	XOR	A		; return OK (A=0)
	RET			;

;-----------------------------------------------------------------------------
;
;	translate the sector given by BC using the
;	translate table given by DE
;
SECTRAN:
	LD	L,C		; return untranslated
	LD	H,B		; in HL
	INC	HL		; sector no. start with 1
	RET			; done

;-----------------------------------------------------------------------------
;
;	set dma address given by registers b and c
;
SETDMA: LD	(DMA),BC	; Save transfer location
	XOR	A		; return 0K (A=0)
	RET			; done

;-----------------------------------------------------------------------------
;
;	perform read operation
;
READ:	XOR	A		; read command -> A
	JR	WAITIO		; to perform the actual i/o

;-----------------------------------------------------------------------------
;
;	perform a write operation
;
WRITE:
	LD	A,1		; write command -> A

;-----------------------------------------------------------------------------
;
;	enter here from read and write to perform the actual i/o
;	operation.  return a 00h in register a if the operation completes
;	properly, and 01h if an error occurs during the read or write
;
WAITIO:
	PUSH	HL		; Save all 16-bit regs
	PUSH	DE		;
	PUSH	BC		;
	LD	(IO_OP),A	; Save I/O direction flag
	LD	A,0		; Get new disk #
NEWDSK	EQU	$-1		;
	LD	HL,CURDSK	; Pointer to current disk #
	CP	(HL)		; Are they the same ?
	JR	NZ,FLUSH	; Go if not, to flush current sect if necessary
	LD	BC,0		; Get new track #
NEWTRK	EQU	$-2		;
	LD	HL,(CURTRK)	; Pointer to current track #
	LD	A,C		; Compare low bytes
	CP	L		; Are they the same ?
	JR	NZ,FLUSH	; Go if not ...
	LD	A,B		; Compare high bytes
	CP	H		; Are they the same ?
	JR	NZ,FLUSH	; Go if not
	LD	A,0		; Get new sector #
NEWSEC	EQU	$-1		;
	LD	HL,CURSEC	; Pointer to current sector #
	CP	(HL)		; Are they the same ?
	JR	Z,NOREAD	; Don't flush and read if yes

;	Dirty sector write is now done if needed
;	Flush the current sector if buffer is dirty
FLUSH:
	LD	A,(DIRTY)	; Get the dirty flag
	OR	A		; Is it dirty ?
	CALL	NZ,WRITE512	; Write sector if yes
	JR	NZ,WAITIOX	; Go if error on write
	LD	A,(NEWDSK)	; Get new disk #
	LD	HL,CURDSK	; Pointer to current disk #
	CP	(HL)		; Are they the same ?
	JR	Z,NOSELDSK	; Skip disk select if yes
	LD	(HL),A		; Save new disk # as current disk #
	LD	A,09H		; IOS:SELDISK opcode
	OUT	(1),A		; Send IOS opcode
	LD	A,(HL)		; Get disk #
	OUT	(0),A		; Send disk # with offset
	CALL	RETSTAT		; Check error code
	JR	NZ,WAITIOX	; Go if error on disk select

;	Disk selection now done if needed
NOSELDSK:
	CALL	READ512		; Read sector
	JR	NZ,WAITIOX	; Go if error on read

;	Sector now read if needed (currently no check for allocation)
NOREAD:
	LD	DE,BUFFER	; Get 512-byte buffer address in DE
	LD	A,0		; Get sector offset in A (0..3)
SECOFF	EQU	$-1		; Sector offset
	LD	H,A		; Move offset * 128 to HL
	LD	L,0		;
	SRL	H		;
	RR	L		;
	ADD	HL,DE		; Add buffer address to offset
	LD	DE,0		; Get "DMA" address to DE
DMA	EQU	$-2		; "DMA" (CBIOS I/O transfer location)
	LD	BC,80H		; get sector length of 128 bytes to BC
	LD	A,0		; Get I/O direction in A
IO_OP	EQU	$-1		; I/O direction
	OR	A		; Is it a write ?
	JR	NZ,WRITEIO	; go if yes
	LDIR			; transfer 128 bytes sector from buffer to DMA
	JR	WAITIOX		; return OK (shows Z)

;	Code segment to write a sector from DMA to the buffer
WRITEIO:
	LD	(DIRTY),A	; Set the buffer "dirty" (needs to be written to disk)
	EX	DE,HL		; Swap DMA and buffer address
	LDIR			; transfer 128 bytes sector from DMA to buffer
	LD	B,10H		; Initialize the keyboard timer to commit the sector write
	LD	(KBTIMER),BC	;
	XOR	A		; return OK (shows Z)

;	Restore registers and return A=0 if Z or A=1 if NZ
WAITIOX:
	POP	BC		; restore regs
	POP	DE		;
	POP	HL		;
	LD	A,0		; A=0
	RET	Z		;   if Z
	INC	A		; otherwise A=1
	RET			; done


;-----------------------------------------------------------------------------
;	Routine to read a physical sector of 512 bytes to the buffer
READ512:
	LD	A,0AH		; IOS:SELTRACK opcode
	OUT	(1),A		; send opcode
	LD	BC,(NEWTRK)	; load the new track #
	LD	(CURTRK),BC	; save as current track #
	LD	A,C		; send low byte
	OUT	(0),A		;
	LD	A,B		; send high byte
	OUT	(0),A		;
	LD	A,0BH		; IOS:SELSECT opcode
	OUT	(1),A		; send opcode
	LD	A,(NEWSEC)	; load new physical sector #
	LD	(CURSEC),A	; save as current sector #
	OUT	(0),A		; send sector #
	LD	HL,BUFFER	; load buffer address
	LD	BC,0		; B = byte count = 256 ; C = I/O port #
	LD	A,86H		; IOS:READSECT opcode
	OUT	(1),A		; send opcode
	INIR			; read first 256 bytes
	INIR			; read last 256 bytes
;	Check the error status of the I/O operation
RETSTAT:
	LD	A,85H		; IOS:ERRDISK opcode
	OUT	(1),A		; send opcode
	IN	A,(0)		; get the error status
	OR	A		; is it 0 ?
	RET	Z		; return A=0 if yes
	LD	A,1		; otherwise return A=1
	RET			; done ; return to p-System

;-----------------------------------------------------------------------------
;	Routine to write a physical sector of 512 bytes from the buffer
WRITE512:
	XOR	A		; IOS:USER_LED opcode
	OUT	(1),A		; send opcode
	INC	A		; set A bit 1 to set the LED on
	OUT	(0),A		; send A
	LD	A,0AH		; IOS:SELTRACK opcode
	OUT	(1),A		; send opcode
	LD	BC,(CURTRK)	; get the current track #
	LD	A,C		; send low byte
	OUT	(0),A		;
	LD	A,B		; send high byte
	OUT	(0),A		;
	LD	A,0BH		; IOS:SELSECT opcode
	OUT	(1),A		; send opcode
	LD	A,(CURSEC)	; get the current sector #
	OUT	(0),A		; send sector #
	LD	HL,BUFFER	; load buffer address
	LD	BC,0		; B = byte count = 256 ; C = I/O port #
	LD	A,0CH		; IOS:WRITESECT opcode
	OUT	(1),A		; send opcode
	OTIR			; write first 256 bytes
	OTIR			; write next 256 bytes
	XOR	A		; clear the DIRTY flag
	LD	(DIRTY),A	;
	OUT	(1),A		; send IOS:USER_LED opcode
	OUT	(0),A		; set the LED off
	JR	RETSTAT		; check the error status and exit


;-----------------------------------------------------------------------------
;
;	initialize disk, (de-)activate disk
;
DSKINIT:
DSKSTRT:
DSKSTOP:
;	Unused Extended SBIOS functions: Printer access,
;	Remote access, User devices access ...
PRNINIT:
PRNSTAT:
PRNREAD:
PRNWRIT:
REMINIT:
REMSTAT:
REMREAD:
REMWRIT:
USRINIT:
USRSTAT:
USRREAD:
USRWRIT:
	XOR	A
	RET

;-----------------------------------------------------------------------------
;	System clock read
CLKREAD:
	LD	A,84h		; IOS:DATETIME opcode
	OUT	(1),A		; Send opcode
	IN	A,(0)		;
	LD	E,A		; Secs
	IN	A,(0)		;
	LD	D,A		; Mins
	IN	A,(0)		;
	LD	L,A		; Hours
	LD	H,0		;
	EXX			;
	LD	HL,0		;
	EXX			;
	CALL	LMUL60		; Long mult by 60
	LD	C,D		; Get mins
	LD	B,0		;
	LD	D,B		; Clear D for later
	ADD	HL,BC		; Add mins
	CALL	LMUL60		; Long mult by 60
	ADD	HL,DE		; Add secs
	EXX			;
	ADC	HL,BC		; Report carry (BC' == 0)
	EXX			;
	CALL	LMUL60		; Long mult by 60
	EXX			;
	PUSH	HL		; Get MSW
	EXX			;
	POP	DE		; To DE
	EX	DE,HL		; MSB to HL, LSB to DE
	XOR	A		; Clear IORESULT
	RET

;	Long Multiply HL':HL by 60d
LMUL60:	CALL	LLDBCHL
	CALL	LADHLHL		; *2
	CALL	LADHLBC		; *3
	CALL	LLDBCHL		;
	CALL	LADHLHL		; *6
	CALL	LADHLHL		; *12
	CALL	LADHLBC		; *15
	CALL	LADHLHL		; *30
	JP	LADHLHL		; *60

;	Long Move HL':HL to BC':BC
LLDBCHL	LD	B,H		;
	LD	C,L		;
	EXX			;
	LD	B,H		;
	LD	C,L		;
	EXX			;
	RET			;

;	Long Shift HL':HL left by 1 bit
LADHLHL	ADD	HL,HL		;
	EXX			;
	ADC	HL,HL		;
	EXX			;
	RET			;

;	Long Add BC':BC to HL':HL
LADHLBC	ADD	HL,BC		;
	EXX			;
	ADC	HL,BC		;
	EXX			;
	RET			;

;-----------------------------------------------------------------------------
;	Variables for disk I/O

;	Disk mapping table for drives 0-3
DSKTBL	DB	DISK0, DISK0+1, DISK0+2, DISK0+3, DISK0+4, DISK0+5
DSKTMP	DB	0		; Temp disk number for drive 6
CURDSK	DB	0FFH		; current disk #
CURTRK	DW	0FFFFH		; current track #
CURSEC	DB	0FFH		; current sector #
DIRTY	DB	0		; buffer dirty flag (needs to be written)

	DS	1 + low not $	; go to next 256 bytes boundary

BUFFER	DS	200H		; 512 bytes sector buffer

ENDBUF	EQU	$		; end of buffer, should be 0000h
	ASSERT	ENDBUF == 0000H	; enforce that ...
;
	DEPHASE
$BIOS_END:

	END	BOOT
