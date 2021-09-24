	TITLE	SYSINT

	LOCALS	@@

; Version equates

    IFDEF _DPMI_
DPMIVersion	EQU	1
    ELSE
DPMIVersion	EQU	0
    ENDIF

; Double byte record

b0		EQU	(BYTE PTR 0)
b1		EQU	(BYTE PTR 1)

; Double word record

w0		EQU	(WORD PTR 0)
w2		EQU	(WORD PTR 2)

; Keyboard scan codes

scSpaceKey	EQU	39H
scInsKey	EQU	52H
scDelKey	EQU	53H
scBackKey	EQU	0EH

; Keyboard shift flags

kbShiftKey	EQU	03H
kbCtrlKey	EQU	04H
kbAltKey	EQU	08H

; ROM BIOS workspace

KeyFlags	EQU	(BYTE PTR 17H)
KeyBufHead	EQU	(WORD PTR 1AH)
KeyBufTail	EQU	(WORD PTR 1CH)
KeyBufOrg	EQU	(WORD PTR 1EH)
KeyBufEnd	EQU	(WORD PTR 3EH)

; DOS function call classes

cNothing	EQU	0	;No check needed
cName		EQU	2	;Check name at DS:DX
cHandle		EQU	4	;Check handle in BX
cDrive		EQU	6	;Check drive in DL

    IF DPMIVersion

; DPMI interrupt vector number

DPMI		EQU	31H

; DPMI function codes

dpmiAllocDesc	EQU	0000H		;Allocate descriptor
dpmiFreeDesc	EQU	0001H		;Free descriptor
dpmiSetSegBase	EQU	0007H		;Set segment base address
dpmiSetSegSize	EQU	0008H		;Set segment size
dpmiGetRealInt	EQU	0200H		;Get real mode interrupt vector
dpmiSetRealInt	EQU	0201H		;Set real mode interrupt vector
dpmiGetProtInt	EQU	0204H		;Get protected mode interrupt vector
dpmiSetProtInt	EQU	0205H		;Set protected mode interrupt vector
dpmiAllocRMCB	EQU	0303H		;Allocate real mode call-back
dpmiFreeRMCB	EQU	0304H		;Free real mode call-back

; DPMI real mode call-back registers structure

realDI		EQU	(WORD PTR 00H)
realSI		EQU	(WORD PTR 04H)
realBP		EQU	(WORD PTR 08H)
realBX		EQU	(WORD PTR 10H)
realDX		EQU	(WORD PTR 14H)
realCX		EQU	(WORD PTR 18H)
realAX		EQU	(WORD PTR 1CH)
realFlags	EQU	(WORD PTR 20H)
realES		EQU	(WORD PTR 22H)
realDS		EQU	(WORD PTR 24H)
realFS		EQU	(WORD PTR 26H)
realGS		EQU	(WORD PTR 28H)
realIP		EQU	(WORD PTR 2AH)
realCS		EQU	(WORD PTR 2CH)
realSP		EQU	(WORD PTR 2EH)
realSS		EQU	(WORD PTR 30H)

    ENDIF

; Data segment

_DATA		SEGMENT WORD PUBLIC 'DATA'

; Externals

	EXTRN	SysErrorFunc:DWORD
	EXTRN	CtrlBreakHit:BYTE
	EXTRN	SaveCtrlBreak:BYTE
	EXTRN	SysErrActive:BYTE
	EXTRN	SaveInt09:DWORD

    IF DPMIVersion

	EXTRN	SelectorInc:WORD
	EXTRN	Seg0040:WORD
	EXTRN	RealModeRegs:BYTE

; Temporary selector for Int24Handler

TempSelector	DW	?

    ENDIF

_DATA		ENDS

; Data group

DGROUP		GROUP	_DATA

; Code segment

SYSINT_TEXT	SEGMENT	BYTE PUBLIC 'CODE'

	ASSUME	CS:SYSINT_TEXT,DS:DGROUP

	PUBLIC	InitSysError
	PUBLIC	DoneSysError

; CS-based variables

OldInt09	DD	0	;Saved INT 09H vector
OldInt1B	DD	0	;Saved INT 1BH vector
OldInt21	DD	0	;Saved INT 21H vector
OldInt23	DD	0	;Saved INT 23H vector
OldInt24	DD	0	;Saved INT 24H vector

; Keyboard conversion table

KeyConvertTab	LABEL	BYTE

	DB	scSpaceKey,kbAltKey
	DW	0200H
	DB	scInsKey,kbCtrlKey
	DW	0400H
	DB	scInsKey,kbShiftKey
	DW	0500H
	DB	scDelKey,kbCtrlKey
	DW	0600H
	DB	scDelKey,kbShiftKey
	DW	0700H
        DB	scBackKey,kbAltKey
        DW	0800H

KeyConvertCnt	EQU	($-KeyConvertTab)/4

; DOS function call class table

FuncClassTab	LABEL	BYTE

	DB	cDrive		;36H - Get disk free space
	DB	cNothing
	DB	cNothing
	DB	cName		;39H - Make directory
	DB	cName		;3AH - Remove directory
	DB	cName		;3BH - Change directory
	DB	cName		;3CH - Create file
	DB	cName		;3DH - Open file
	DB	cHandle		;3EH - Close file
	DB	cHandle		;3FH - Read file
	DB	cHandle		;40H - Write file
	DB	cName		;41H - Delete file
	DB	cHandle		;42H - Seek file
	DB	cName		;43H - Change file attributes
	DB	cNothing
	DB	cNothing
	DB	cNothing
	DB	cDrive		;47H - Get current directory
	DB	cNothing
	DB	cNothing
	DB	cNothing
	DB	cName		;4BH - Load or execute program
	DB	cNothing
	DB	cNothing
	DB	cName		;4EH - Find first
	DB	cNothing
	DB	cNothing
	DB	cNothing
	DB	cNothing
	DB	cNothing
	DB	cNothing
	DB	cNothing
	DB	cName		;56H - Rename file
	DB	cHandle		;57H - Get/Set file date and time

; Function check routines table

FuncCheckTab	LABEL	WORD

	DW	CheckNothing
	DW	CheckName
	DW	CheckHandle
	DW	CheckDrive

; Install system error handlers

InitSysError:

	MOV	AX,3300H
	INT	21H
	MOV	SaveCtrlBreak,DL
	MOV	AX,3301H
	MOV	DL,0
	INT	21H
    IF DPMIVersion
	MOV	AX,dpmiAllocDesc
	MOV	CX,1
	INT	DPMI
	MOV	TempSelector,AX
	MOV	BX,AX
	XOR	CX,CX
	MOV	DX,0FFFFH
	MOV	AX,dpmiSetSegSize
	INT	DPMI
	MOV	AX,CS
	ADD	AX,SelectorInc
	MOV	ES,AX
	MOV	DI,OFFSET OldInt09
	CLD
	MOV	BL,09H
	CALL	GetProtInt
	MOV	BL,1BH
	CALL	GetRealInt
	MOV	BL,21H
	CALL	GetProtInt
	MOV	BL,23H
	CALL	GetProtInt
	MOV	BL,24H
	CALL	GetRealInt
	MOV	BL,09H
	MOV	DX,OFFSET Int09Handler
	MOV	CX,CS
	CALL	SetProtInt
	MOV	BL,1BH
	MOV	DX,OFFSET Int1BHandler
	MOV	CX,CS
	CALL	SetRealInt
	MOV	ES,Seg0040
	MOV	AX,ES:[10H]
	AND	AX,0C1H
	DEC	AX
	JNE	@@1
	MOV	BL,21H
	MOV	DX,OFFSET Int21Handler
	MOV	CX,CS
	CALL	SetProtInt
@@1:	MOV	BL,23H
	MOV	DX,OFFSET Int23Handler
	MOV	CX,CS
	CALL	SetProtInt
	MOV	BL,24H
	MOV	DX,OFFSET Int24Handler
	MOV	CX,CS
	CALL	SetRealInt
	MOV	AX,dpmiGetProtInt
	MOV	BL,10H
	INT	DPMI
	PUSH	CX
	PUSH	DX
	MOV	AX,dpmiSetProtInt
	MOV	BL,10H
	MOV	DX,OFFSET Int10Handler
	MOV	CX,CS
	INT	DPMI
	MOV	AH,0BH
	INT	21H
	POP	DX
	POP	CX
	MOV	AX,dpmiSetProtInt
	MOV	BL,10H
	INT	DPMI
    ELSE
	PUSH	DS
	XOR	AX,AX
	MOV	DS,AX
	MOV	DI,OFFSET OldInt09
	PUSH	CS
	POP	ES
	CLD
	CLI
	MOV	SI,09H*4
	MOVSW
	MOVSW
	MOV	SI,1BH*4
	MOVSW
	MOVSW
	MOV	SI,21H*4
	MOVSW
	MOVSW
	MOV	SI,23H*4
	MOVSW
	MOVSW
	MOVSW
	MOVSW
	MOV	WORD PTR DS:[09H*4+0],OFFSET Int09Handler
	MOV	WORD PTR DS:[09H*4+2],CS
	MOV	WORD PTR DS:[1BH*4+0],OFFSET Int1BHandler
	MOV	WORD PTR DS:[1BH*4+2],CS
	MOV	AX,DS:[410H]
	AND	AX,0C1H
	DEC	AX
	JNE	@@1
	MOV	WORD PTR DS:[21H*4+0],OFFSET Int21Handler
	MOV	WORD PTR DS:[21H*4+2],CS
@@1:	MOV	WORD PTR DS:[23H*4+0],OFFSET Int23Handler
	MOV	WORD PTR DS:[23H*4+2],CS
	MOV	WORD PTR DS:[24H*4+0],OFFSET Int24Handler
	MOV	WORD PTR DS:[24H*4+2],CS
	STI
	MOV	AX,CS
	XCHG	AX,WORD PTR DS:[10H*4+2]
	PUSH	AX
	MOV	AX,OFFSET CS:Int10Handler
	XCHG	AX,WORD PTR DS:[10H*4+0]
	PUSH	AX
	MOV	AH,0BH
	INT	21H
	POP	DS:WORD PTR [10H*4+0]
	POP	DS:WORD PTR [10H*4+2]
	POP	DS
    ENDIF
	MOV	AX,OldInt09.w0
	MOV	SaveInt09.w0,AX
	MOV	AX,OldInt09.w2
	MOV	SaveInt09.w2,AX
	MOV	SysErrActive,1
	RETF

; Remove system error handlers

DoneSysError:

	CMP	SysErrActive,0
	JE	@@1
	MOV	SysErrActive,0
    IF DPMIVersion
	PUSH	DS
	MOV	SI,OFFSET OldInt09
	PUSH	CS
	POP	DS
	CLD
	MOV	BL,09H
	CALL	ResetProtInt
	MOV	BL,1BH
	CALL	ResetRealInt
	MOV	BL,21H
	CALL	ResetProtInt
	MOV	BL,23H
	CALL	ResetProtInt
	MOV	BL,24H
	CALL	ResetRealInt
	POP	DS
	MOV	AX,dpmiFreeDesc
	MOV	BX,TempSelector
	INT	DPMI
    ELSE
	PUSH	DS
	MOV	SI,OFFSET OldInt09
	PUSH	CS
	POP	DS
	XOR	AX,AX
	MOV	ES,AX
	CLD
	CLI
	MOV	DI,09H*4
	MOVSW
	MOVSW
	MOV	DI,1BH*4
	MOVSW
	MOVSW
	MOV	DI,21H*4
	MOVSW
	MOVSW
	MOV	DI,23H*4
	MOVSW
	MOVSW
	MOVSW
	MOVSW
	STI
	POP	DS
    ENDIF
	MOV	AX,3301H
	MOV	DL,SaveCtrlBreak
	INT	21H
@@1:	RETF

    IF DPMIVersion

; Get real mode interrupt vector

GetRealInt:

	MOV	AX,dpmiGetRealInt
	JMP	SHORT GetIntVector

; Get protected mode interrupt vector

GetProtInt:

	MOV	AX,dpmiGetProtInt

; Get interrupt vector

GetIntVector:

	INT	DPMI
	XCHG	AX,DX
	STOSW
	XCHG	AX,CX
	STOSW
	RET

; Reset real mode interrupt vector

ResetRealInt:

	MOV	AX,dpmiGetRealInt
	INT	DPMI
	PUSH	CX
	PUSH	DX
	LODSW
	XCHG	AX,DX
	LODSW
	XCHG	AX,CX
	MOV	AX,dpmiSetRealInt
	INT	DPMI
	POP	DX
	POP	CX
	MOV	AX,dpmiFreeRMCB
	INT	DPMI
	RET

; Set real mode interrupt vector

SetRealInt:

	MOV	AX,dpmiAllocRMCB
	MOV	DI,OFFSET RealModeRegs
	PUSH	DS
	POP	ES
	MOV	SI,DX
	MOV	DS,CX
	INT	DPMI
	PUSH	ES
	POP	DS
	MOV	AX,dpmiSetRealInt
	INT	DPMI
	RET

; Reset protected mode interrupt vector

ResetProtInt:

	LODSW
	XCHG	AX,DX
	LODSW
	XCHG	AX,CX

; Set protected mode interrupt vector

SetProtInt:

	MOV	AX,dpmiSetProtInt
	INT	DPMI
	RET

; Simulate an IRET in a real mode call-back

RealModeIRET:

	CLD
	LODSW
	MOV	ES:[DI].realIP,AX
	LODSW
	MOV	ES:[DI].realCS,AX
	LODSW
	MOV	ES:[DI].realFlags,AX
	ADD	ES:[DI].realSP,6
	RET

    ENDIF

; INT 09H handler signature

	DB	'TVI9'

; INT 09H handler

Int09Handler:

	PUSH	DS
	PUSH	DI
	PUSH	AX
    IF DPMIVersion
	MOV	AX,SEG DGROUP
	MOV	DS,AX
	MOV	DS,Seg0040
    ELSE
	MOV	AX,40H
	MOV	DS,AX
    ENDIF
	MOV	DI,DS:KeyBufTail
	IN	AL,60H
	MOV	AH,DS:KeyFlags
	PUSHF
	CALL	OldInt09
	TEST	AL,80H
	JNE	@@9
	PUSH	SI
	PUSH	CX
	MOV	SI,OFFSET CS:KeyConvertTab
	MOV	CX,KeyConvertCnt
@@1:	CMP	AL,CS:[SI]
	JNE	@@2
	TEST	AH,CS:[SI+1]
	JNE	@@3
@@2:	ADD	SI,4
	LOOP	@@1
	JMP	SHORT @@8
@@3:	CMP	DI,DS:KeyBufTail
	JNE	@@5
	MOV	AX,DI
	INC	AX
	INC	AX
	CMP	AX,OFFSET KeyBufEnd
	JNE	@@4
	MOV	AX,OFFSET KeyBufOrg
@@4:	CMP	AX,DS:KeyBufHead
	JE	@@8
	MOV	DS:KeyBufTail,AX
	MOV	DI,AX
@@5:	MOV	AX,CS:[SI+2]
	MOV	DS:[DI],AX
@@8:	POP	CX
	POP	SI
@@9:	POP	AX
	POP	DI
	POP	DS
	IRET

; INT 1BH handler

Int1BHandler:

    IF DPMIVersion
	CALL	RealModeIRET
	MOV	DS,ES:Seg0040
	AND	BYTE PTR DS:[71H],7FH
	MOV	ES:CtrlBreakHit,1
    ELSE
	PUSH	DS
	PUSH	AX
	XOR	AX,AX
	MOV	DS,AX
	AND	BYTE PTR DS:[471H],7FH
	MOV	AX,SEG DGROUP
	MOV	DS,AX
	MOV	CtrlBreakHit,1
	POP	AX
	POP	DS
    ENDIF
	IRET

; INT 21H handler

Int21Handler:

	PUSHF
	STI
	CMP	AH,36H
	JB	@@1
	CMP	AH,57H
	JA	@@1
	PUSH	DX
	PUSH	BX
	MOV	BL,AH
	XOR	BH,BH
	MOV	BL,CS:FuncClassTab[BX-36H]
	CALL	CS:FuncCheckTab[BX]
	POP	BX
	POP	DX
	JC	@@2
@@1:	POPF
	JMP	OldInt21
@@2:	POPF
	STI
	CMP	AH,36H
	MOV	AX,0FFFFH
	JE	@@3
	MOV	AX,5
@@3:	STC
	RETF	2

; Check filename

CheckName:

	MOV	BX,DX
	MOV	DX,[BX]
	AND	DL,1FH
	DEC	DL
	CMP	DH,':'
	JE	CheckAbsDrive
	JMP	SHORT CheckCurDrive

; Check handle

CheckHandle:

	MOV	BX,SP
	MOV	BX,SS:[BX+2]
	PUSH	AX
	MOV	AX,4400H
	PUSHF
	CALL	OldInt21
	POP	AX
	OR	DL,DL
	JNS	CheckAbsDrive
	JMP	SHORT CheckNothing

; Check drive

CheckDrive:

	DEC	DL
	JNS	CheckAbsDrive

; Check current drive

CheckCurDrive:

	PUSH	AX
	MOV	AH,19H
	PUSHF
	CALL	OldInt21
	MOV	DL,AL
	POP	AX

; Check absolute drive
; In	DL = Drive (0=A, 1=B, etc)
; Out	CF = 1 if drive swap failed

CheckAbsDrive:

	CMP	DL,2
	JAE	CheckNothing
	PUSH	DS
	PUSH	AX
    IF DPMIVersion
	MOV	AX,SEG DGROUP
	MOV	DS,AX
	MOV	DS,Seg0040
	MOV	AL,DS:[104H]
    ELSE
	XOR	AX,AX
	MOV	DS,AX
	MOV	AL,DS:[504H]
    ENDIF
	CMP	AL,0FFH
	JE	@@1
	CMP	DL,AL
	JE	@@1
	PUSH	ES
	PUSH	DS
	PUSH	DI
	PUSH	SI
	PUSH	DX
	PUSH	CX
	MOV	AX,SEG DGROUP
	MOV	DS,AX
	MOV	AX,15
	PUSH	AX
	PUSH	DX
	CALL	SysErrorFunc
	POP	CX
	POP	DX
	POP	SI
	POP	DI
	POP	DS
	POP	ES
	NEG	AX
	JC	@@1
    IF DPMIVersion
	MOV	DS:[104H],DL
    ELSE
	MOV	DS:[504H],DL
    ENDIF
@@1:	POP	AX
	POP	DS

; No check required

CheckNothing:

	RET

; INT 23H and temporary INT 10H handler

Int10Handler:
Int23Handler:

	IRET

; INT 24H handler

Int24Handler:

	STI				;Enable interrupts
    IF DPMIVersion
	CALL	RealModeIRET
	PUSH	DS			;Save real mode stack pointer
	PUSH	SI
	PUSH	DI			;Save RealModeRegs pointer
	PUSH	ES			;Point DS to data segment
	POP	DS
	MOV	AL,[DI].realAX.b0	;Get drive code
	MOV	DL,[DI].realDI.b0	;Get error code
	XOR	DH,DH
	CMP	DL,9			;Printer out of paper?
	JE	@@1			;Yes, @@1
	TEST	[DI].realAX.b1,80H	;Disk error?
	JE	@@2			;Yes, @@2
	MOV	DL,13			;Bad memory image of FAT
	PUSH	DX
	PUSH	AX
	MOV	AX,16			;Get linear address of segment
	MUL	[DI].realBP		;at real mode BP register
	MOV	CX,DX			;Move address to CX:DX
	MOV	DX,AX
	MOV	AX,dpmiSetSegBase	;Set base address of temporary
	MOV	BX,TempSelector		;selector
	INT	DPMI
	MOV	ES,BX			;ES:BX = device header pointer
	MOV	BX,[DI].realSI
	TEST	BYTE PTR ES:[BX+5],80H	;Block device?
	POP	AX
	POP	DX
	JE	@@1			;Yes, @@0
	INC	DX			;Device access error
@@1:	MOV	AL,0FFH			;No drive code
@@2:	PUSH	DX			;Push error code
	PUSH	AX			;Push drive code
	CALL	SysErrorFunc		;Call system error handler
	PUSH	DS
	POP	ES
	POP	DI
	POP	SI
	POP	DS
	OR	AX,AX			;Zero if retry
	MOV	DX,1			;Retry return code
	JE	@@4			;Jump if retry
	MOV	AH,54H			;Dummy function call to get
	INT	21H			;DOS into a stable state
	CLD
	LODSW				;Pop AX from real mode stack
	MOV	DL,ES:[DI].realDI.b0	;Get critical error code
	ADD	DL,19			;Return AX = 19..31
	MOV	DH,0
	CMP	AH,39H			;DOS 2.0 style function?
	JAE	@@3			;Yes, @@3
	MOV	DX,0FFFFH		;Return AX = 0FFFFH
@@3:	LODSW				;Restore other registers
	MOV	ES:[DI].realBX,AX
	LODSW
	MOV	ES:[DI].realCX,AX
	LODSW
	MOV	ES:[DI].realDX,AX
	LODSW
	MOV	ES:[DI].realSI,AX
	LODSW
	MOV	ES:[DI].realDI,AX
	LODSW
	MOV	ES:[DI].realBP,AX
	LODSW
	MOV	ES:[DI].realDS,AX
	LODSW
	MOV	ES:[DI].realES,AX
	ADD	ES:[DI].realSP,18
	CALL	RealModeIRET		;Simulate IRET
	OR	ES:[DI].realFlags,1	;Set CF in return flags
@@4:	MOV	ES:[DI].realAX,DX	;Store return code
    ELSE
	PUSH	ES			;Save registers
	PUSH	DS
	PUSH	BP
	PUSH	DI
	PUSH	SI
	PUSH	DX
	PUSH	CX
	PUSH	BX
	AND	DI,0FFH			;Error code in low byte
	PUSH	DI			;Save error code
	CMP	DI,9			;Printer out of paper
	JE	@@0			;Yes, @@0
	TEST	AH,80H			;Disk error?
	JE	@@1			;Yes, @@1
	MOV	DI,13			;Bad memory image of FAT
	MOV	DS,BP			;Point DS:SI to device header
	TEST	BYTE PTR DS:[SI+5],80H	;Block device?
	JE	@@1			;Yes, @@0
	INC	DI			;Device access error
@@0:	MOV	AL,0FFH			;No drive code
@@1:	MOV	DX,SEG DGROUP		;Setup DS
	MOV	DS,DX
	PUSH	DI			;Push error code
	PUSH	AX			;Push drive code
	CALL	SysErrorFunc		;Call system error handler
	POP	DI			;Restore error code
	OR	AX,AX			;Zero if retry
	MOV	AX,1			;Retry return code
	JE	@@3			;Jump if retry
	ADD	SP,(8+3)*2		;Remove saved regs and INT
	POP	AX			;Get INT 21H AX register
	ADD	DI,19			;Return AX = 19..31
	CMP	AH,39H			;DOS 2.0 style function?
	JAE	@@2			;Yes, @@1
	MOV	DI,0FFFFH		;Return AX = 0FFFFH
@@2:	MOV	AH,54H			;Dummy function call to get
	INT	21H			;DOS into a stable state
	MOV	AX,DI			;Get return code
	MOV	BP,SP			;Set CF in return flags
	OR	BYTE PTR [BP+20],1
@@3:	POP	BX			;Restore registers
	POP	CX
	POP	DX
	POP	SI
	POP	DI
	POP	BP
	POP	DS
	POP	ES
    ENDIF
	IRET

SYSINT_TEXT	ENDS

	END
