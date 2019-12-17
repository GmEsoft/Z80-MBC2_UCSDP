;
DISK0	EQU	20		;First disk number in set
DEBUG	EQU	1		;Debug mode        ^                ^
TEST	EQU	0		;Load SBIOSTESTER /!\ ERASES DISKS /!\
				;                -----            -----
PRIMARY	EQU	1		;Load Primary bootstrap

$BREAK	MACRO
	IF	DEBUG
	DB	0EDH,0F5H
	ENDIF
	ENDM
					;

	ORG	0
	JP	BOOTZ80			;bootstrap routine
	DC	0DH,0			;fill

	ORG	80H
BOOTZ80:
;	HALT

	LD	SP,80H

	LD	HL,$BIOS_BEGIN
	LD	DE,BIOS
	LD	BC,$BIOS_END-$BIOS_BEGIN
	LDIR

	LD	A,0C3H
	LD	(0),A
	LD	HL,MAIN
	LD	(1),HL
	JP	MAIN

	ORG	100H			; 100H
					;
MAIN:					;LET'S BOOT UCSD PASCAL
;
;	  THIS PROGRAM IS A SKELETAL OUTLINE FOR A 128-BYTE PRIMARY
;	BOOTSTRAP FOR AUTOMATICALLY BOOTING TO UCSD PASCAL (TM).
;	  SET THE CORRECT ORIGIN FOR THIS PROGRAM FOR YOUR SYSTEM, SET
;	'MSIZE' FOR THE APPROPRIATE NUMBER OF KILOBYTES OF RAM MEMORY
;	FOR YOUR SYSTEM, SET THE APPROPRIATE PARAMETERS DESCRIBING YOUR
;	DISK ENVIRONMENT AND FINALLY WRITE A VERY LOW LEVEL DISK READ
;	ROUTINE TO ALLOW READING IN THE SECONDARY BOOTSTRAP AND YOUR
;	CBIOS OFF THE DISK AND INTO RAM.
;	  THE PROGRAM 'CPMBOOT' ON THE UCSD PASCAL DISTRIBUTION DISK WILL
;	THEN USE THIS PROGRAM AND YOUR CBIOS TO GENERATE AN AUTOMATICALLY
;	BOOTING UCSD PASCAL SYSTEM.
;
;	ADAPTED FOR Z80SIM, AUGUST 2008, UDO MUNK
;
BOOT	EQU	8200H		; SECONDARY BOOTSTRAP LOADED HERE
MSIZE	EQU	64		; MEMORY SIZE FOR ASSEMBLY
BIAS	EQU	(MSIZE*1024)-01900H
CBIOS	EQU	1500H+BIAS	; ORIGIN POINT
SECNUM	EQU	16		; SECONDARY BOOTSTRAP IS 16 SECTORS LONG
SECSEC	EQU	3		; SECONDARY BOOTSTRAP ON THIS SECTOR
;BIOSNUM	EQU	2		; CBIOS IS 8 SECTORS LONG
;BIOSSEC	EQU	1		; CBIOS IS ON THIS SECTOR
;
;	ORG	0		; WHATEVER IS RIGHT FOR YOUR SYSTEM
;
PBOOT:	LD	HL,CBIOS	; CBIOS GOES HERE
	LD	SP,HL		; RESET THE STACK
;	LD	D,BIOSNUM	; D - # OF SECTORS TO READ
;	LD	E,BIOSSEC	; E - STARTING SECTOR
;	CALL	READIT		; READ IN CBIOS
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
	LD	HL,CBIOS-2	; TOP OF MEMORY (MUST BE WORD BOUNDARY)
	PUSH	HL
	LD	HL,0100H	; BOTTOM OF MEMORY
	PUSH	HL
	LD	DE,CBIOS+3	; START OF THE SBIOS (JMP WBOOT)
	PUSH	DE
	PUSH	HL		; STARTING ADDRESS OF INTERPRETER
;	$BREAK
	JP	BOOT		; ENTER SECONDARY BOOTSTRAP
;
;	  READIT MUST READ THE NUMBER OF SECTORS SPECIFIED IN THE D
;	REG, STARTING AT THE SECTOR SPECIFIED IN THE E REG, INTO THE
;	MEMORY LOCATION SPECIFIED IN THE HL PAIR.
;
READIT:
;
;  PUT YOUR CODE IN HERE
;
	PUSH	HL
	LD	C,0		; SELECT DRIVE 0
	CALL	SELDSK
	LD	C,0
	CALL	SETTRK		; SELECT TRACK 0
	POP	HL
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
;

$BIOS_BEGIN
    	PHASE	BIOS
;	UCSD p-System IV CBIOS for Z80-Simulator
;
;	Copyright (C) 2008 by Udo Munk
;
MSIZE	EQU	64		;memory size in kilobytes
BIAS	EQU	(MSIZE*1024)-01900H
BIOS	EQU	1500H+BIAS	;base of bios


;
;	jump vector for individual subroutines
;
	JP	CBOOT		;cold start
WBOOTE: JP	WBOOT		;warm start
	JP	CONST		;console status
	JP	CONIN		;console character in
	JP	CONOUT		;console character out
	JP	LIST		;list character out
	JP	PUNCH		;punch character out
	JP	READER		;reader character out
	JP	HOME		;move head to home position
	JP	SELDSK		;select disk
	JP	SETTRK		;set track number
	JP	SETSEC		;set sector number
	JP	SETDMA		;set dma address
	JP	READ		;read disk
	JP	WRITE		;write disk
	JP	LISTST		;return list status
	JP	SECTRAN		;sector translate
;
;	copyright text
;
	DEFM	'64K UCSD p-System IV.0 CBIOS V1.1 for Z80SIM, '
	DEFM	'Copyright 2008 by Udo Munk'
	DEFB	13,10,0
;
;	individual subroutines to perform each function
;	simplest case is to just perform parameter initialization
;
CBOOT:
	$BREAK
	RET

WBOOT:
	DI
	HALT
	RET
;
;
;	simple i/o handlers
;
;	console status, return 0ffh if character ready, 00h if not
;
CONST:
	LD	A,83H		;SYSFLAGS
	OUT	(1),A
	IN	A,(0)		;get console status
	AND	4
	RET	Z
	LD	A,0FFH
	RET
;
;	console character into register a
;
CONIN:
	IN	A,(1)		;get character from console
	RET
;
;	console character output from register c
;
CONOUT:
	LD	A,C
	OR	A
	RET	Z		;don't send NUL
	LD	A,01H		;SERIAL_TX
	OUT	(1),A		;
	LD	A,C		;get to accumulator
	OUT	(0),A		;send character to console
	CP	27		;ESC character?
	RET	NZ		;no, done
	LD	A,01H		;SERIAL_TX
	OUT	(1),A		;
	LD	A,'['		;send second lead in for ANSI terminals
	OUT	(0),A
;	$BREAK
	RET
;
;	list character from register c
;
LIST:	JR	CONOUT
;
;	return list status (00h if not ready, 0ffh if ready)
;
LISTST: LD	A,0FFH
	RET
;
;	punch character from register c
;
PUNCH:	JR	CONOUT
;
;	read character into register a from reader device
;
READER: JR	CONIN
;
;
;	i/o drivers for the disk follow
;
;	move to the track 00 position of current drive
;	translate this call into a settrk call with parameter 00
;
HOME:	LD	C,0		;select track 0
	JP	SETTRK		;we will move to 00 on first read/write
;
;	select disk given by register C
;
SELDSK: LD	HL,0000H	;return code
	LD	A,C
	LD	(NEWDSK),A
	XOR	A
	RET
;
;	set track given by register c
;
SETTRK: LD	A,B
	LD	B,0
	LD	(NEWTRK),BC
	LD	B,A
	XOR	A
	RET
;
;	set sector given by register c
;
SETSEC: DEC	C
	LD	A,C
	AND	3
	LD	(SECOFF),A
	LD	A,C
	SRL	A
	SRL	A
	LD	(NEWSEC),A
	INC	C
	XOR	A
	RET
;
;	translate the sector given by BC using the
;	translate table given by DE
;
SECTRAN:
	LD	L,C		;return untranslated
	LD	H,B		;in HL
	INC	HL		;sector no. start with 1
	RET
;
;	set dma address given by registers b and c
;
SETDMA: LD	(DMA),BC
	XOR	A
	RET
;
;	perform read operation
;
READ:	XOR	A		;read command -> A
	JR	WAITIO		;to perform the actual i/o
;
;	perform a write operation
;
WRITE:
;	$BREAK
	LD	A,1		;write command -> A
;
;	enter here from read and write to perform the actual i/o
;	operation.  return a 00h in register a if the operation completes
;	properly, and 01h if an error occurs during the read or write
;
WAITIO:
	PUSH	HL
	PUSH	DE
	PUSH	BC
	LD	(IO_OP),A
	LD	A,0
NEWDSK	EQU	$-1
	LD	HL,CURDSK
	CP	(HL)
	JR	NZ,FLUSH
	LD	BC,0
NEWTRK	EQU	$-2
	LD	HL,(CURTRK)
	LD	A,C
	CP	L
	JR	NZ,FLUSH
	LD	A,B
	CP	H
	JR	NZ,FLUSH
	LD	A,0
NEWSEC	EQU	$-1
	LD	HL,CURSEC
	CP	(HL)
	JR	Z,NOREAD

FLUSH:
	LD	HL,DIRTY
	LD	A,(HL)
	OR	A
	JR	Z,NOWRITE
	PUSH	HL
	CALL	WRITE512
	POP	HL
	LD	(HL),A
	JR	NZ,WAITIOX
NOWRITE:
	LD	A,(NEWDSK)
	LD	HL,CURDSK
	CP	(HL)
	JR	Z,NOSELDSK
	LD	(HL),A
	LD	A,09H		;SELDISK
	OUT	(1),A
	LD	A,(HL)
	ADD	A,DISK0		;DISK 0 offset
	OUT	(0),A
NOSELDSK:
	CALL	READ512
;	$BREAK
	JR	NZ,WAITIOX
NOREAD:
	LD	DE,BUFFER
	LD	A,0
SECOFF	EQU	$-1		;sector offset
	LD	H,A
	LD	L,0
	SRL	H
	RR	L
	ADD	HL,DE
	LD	DE,0
DMA	EQU	$-2
	LD	BC,80H		;128 bytes sector
	LD	A,0
IO_OP	EQU	$-1
	OR	A
	JR	NZ,WRITEIO
	LDIR
	JR	WAITIOX

WRITEIO:
	LD	(DIRTY),A
	EX	DE,HL
	LDIR
	XOR	A

WAITIOX:
	POP	BC
	POP	DE
	POP	HL
	LD	A,0
	RET	Z
	INC	A
	RET


READ512:
	LD	A,0AH		;SELTRACK
	OUT	(1),A
	LD	BC,(NEWTRK)
	LD	(CURTRK),BC
	LD	A,C
	OUT	(0),A
	LD	A,B
	OUT	(0),A
	LD	A,0BH		;SELSECT
	OUT	(1),A
	LD	A,(NEWSEC)
	LD	(CURSEC),A
	OUT	(0),A
	LD	HL,BUFFER
	LD	BC,0
	LD	A,86H		;READSECT
	OUT	(1),A
	INIR
	INIR
RETSTAT:
	LD	A,85H		;ERRDISK
	OUT	(1),A
	IN	A,(0)
	OR	A
	RET	Z
	LD	A,1
	RET


WRITE512:
	LD	A,0AH		;SELTRACK
	OUT	(1),A
	LD	BC,(CURTRK)
	LD	A,C
	OUT	(0),A
	LD	A,B
	OUT	(0),A
	LD	A,0BH		;SELSECT
	OUT	(1),A
	LD	A,(CURSEC)
	OUT	(0),A
	LD	HL,BUFFER
	LD	BC,0
	LD	A,0CH		;WRITESECT
	OUT	(1),A
	OTIR
	OTIR
	JR	RETSTAT

CURDSK	DB	0FFH
CURTRK	DW	0FFFFH
CURSEC	DB	0FFH
DIRTY	DB	0

	DS	1 + low not $

BUFFER	DS	200H		;DISK BUFFER

ENDBUF	EQU	$
;
	DEPHASE
$BIOS_END:

	END	BOOT

