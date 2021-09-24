;******************************************************
;                 OPCOMMON.ASM 1.30
;           Widely used macros and equates
;     Copyright (c) TurboPower Software 1987, 1992.
;                All rights reserved.
;******************************************************

COMMENT |
  For maximum snow prevention on color graphics adapters, set SuppressAllSnow
  to 1 and reassemble the following files:

    OPCRT.ASM
    OPFAST.ASM

  You will also need to recompile the OPCRT unit, of course.
|

SuppressAllSnow = 0

;****************************************************** Macros

StackFrame      MACRO
                MOV     BX,SP           ;Set BX to point to stack
                ENDM

StackFrameBP    MACRO
                PUSH    BP
                MOV     BP,SP
                ENDM

Exit_Code       MACRO   PopCount        ;!!.13
                MOV     SP,BP
                POP     BP
                RET     PopCount
                ENDM

SetZero         MACRO Reg
                XOR     Reg,Reg         ;Reg = 0
                ENDM

SetPtr          MACRO   P, S, O
                MOV     P.Ofst, O       ;set offset
                MOV     P.Segm, S       ;set segment
                ENDM

SetPtrByOfst    MACRO   P, S, O
                MOV     AX,Offset O
                MOV     P.Ofst, AX      ;set offset
                MOV     P.Segm, S       ;set segment
                ENDM

GetPtr          MACRO   GPtr
                LES     DI, GPtr        ;Load ES:DI with GPtr
                ENDM

GetDSPtr        MACRO   GDPtr
                LDS     SI, GDPtr       ;Load DS:SI with GDPtr
                ENDM

NullJump        MACRO
                JMP SHORT $+2           ;jump to next instruction
                ENDM

GetVector       MACRO   VecNum, PtrVar
                MOV     AH,35h          ;Get vector
                MOV     AL,VecNum       ;AL = vector number
                INT     21h             ;call DOS
                SetPtr  PtrVar, ES, BX  ;save the vector in PtrVar
                ENDM

SetVector       MACRO   VecNum, PtrVar
                PUSH    DS
                MOV     AH,25h          ;Set vector
                MOV     AL,VecNum       ;AL = vector number
                LDS     DX,PtrVar       ;get the vector into DS:DX
                INT     21h             ;call DOS
                POP     DS
                ENDM

JmpFar          MACRO JAddr
                JMP     DWORD PTR JAddr ;Jump far to JAddr
                ENDM

CallFar         MACRO CAddr
                CALL    DWORD PTR CAddr ;Call far to CAddr
                ENDM

;Work around bug in the POPF instruction on certain 80286 chips
FakePOPF        MACRO
                LOCAL X1, X2
                JMP     SHORT X2        ;skip over IRET
        X1:     IRET                    ;POP flags and do a RETF
        X2:     PUSH    CS              ;fake a FAR CALL
                CALL    X1
                ENDM

WaitForRetrace  MACRO
                LOCAL WaitNoH, WaitH, Go
                ;Note DX must be initialized to 03DAh
                CLI                     ;Interrupts off
        WaitNoH:
                IN      AL,DX           ;Get 6845 status
                TEST    AL,8            ;Check for vertical retrace
                JNZ     Go              ;In progress? go
                SHR     AL,1            ;Wait for end of horizontal
                JC      WaitNoH         ; retrace
        WaitH:
                IN      AL,DX           ;Get 6845 status again
                SHR     AL,1            ;Wait for horizontal
                JNC     WaitH           ; retrace
        Go:
                ENDM

IF SuppressAllSnow

WordMoveNoSnow  MACRO
                LOCAL Wait1, Wait2, Go, Next
                ;Note DX must be initialized to 03DAh, CX to loop count
                SHL     CX,1            ;Words to bytes
        Next:   CLI                     ;Interrupts off
        Wait1:  IN      AL,DX           ;Get 6845 status
                TEST    AL,8            ;Check for vertical retrace
                JNZ     Go              ;In progress? go
                SHR     AL,1            ;Wait for end of horizontal
                JC      Wait1           ; retrace
        Wait2:  IN      AL,DX           ;Get 6845 status again
                SHR     AL,1            ;Wait for horizontal
                JNC     Wait2           ;  retrace
        Go:     MOVSB                   ;Move one byte
                STI                     ;Allow interrupts
                LOOP    Next            ;Move next byte
                ENDM

ELSE

WordMoveNoSnow  MACRO
                LOCAL Wait1, Wait2, Go, Next
                ;Note DX must be initialized to 03DAh, CX to loop count
        Next:   CLI                     ;Interrupts off
        Wait1:  IN      AL,DX           ;Get 6845 status
                SHR     AL,1            ;Wait for end of horizontal
                JC      Wait1           ; retrace
        Wait2:  IN      AL,DX           ;Get 6845 status again
                SHR     AL,1            ;Wait for horizontal
                JNC     Wait2           ;  retrace
        Go:     MOVSW                   ;Move one word
                STI                     ;Allow interrupts
                LOOP    Next            ;Move next byte
                ENDM

ENDIF

FastMoveNoSnow  MACRO
                LOCAL Wait1, Wait2, Go, Next
                ;Note DX must be initialized to 03DAh, CX to loop count
        Next:   CLI                     ;Interrupts off
        Wait1:  IN      AL,DX           ;Get 6845 status
                TEST    AL,8            ;Check for vertical retrace
                JNZ     Go              ;In progress? go
                SHR     AL,1            ;Wait for end of horizontal
                JC      Wait1           ; retrace
        Wait2:  IN      AL,DX           ;Get 6845 status again
                SHR     AL,1            ;Wait for horizontal
                JNC     Wait2           ;  retrace
        Go:     MOVSW                   ;Move one word
                STI                     ;Allow interrupts
                LOOP    Next            ;Move next byte
                ENDM

VideoPrim       MACRO
                PUSH    BP              ;save BP
                INT     10h             ;call BIOS
                POP     BP              ;restore BP
                ENDM

VideoCall       MACRO   VidService
                MOV     AH, VidService  ;service number
                VideoPrim               ;primitive form
                ENDM

VidCallAX       MACRO   VidServAX
                MOV     AX, VidServAX   ;service number
                VideoPrim               ;primitive form
                ENDM

KbdCall         MACRO   KbdService
                MOV     AH, KbdService  ;service number
                INT     16h             ;BIOS keyboard interrupt
                ENDM

DosCall         MACRO   DosFuncNum
                MOV     AH,DosFuncNum   ;AH = Function number
                INT     21h             ;Call DOS
                ENDM

DosCallAX       MACRO   AXval
                MOV     AX,AXval        ;AH = Function #, AL has modifier
                INT     21h             ;Call DOS
                ENDM

WordToCGA       MACRO   MVreg
                XCHG    AX,MVreg        ;Move video word into AX !!.01
                STOSW                   ; and then to screen
                STI                     ;Allow interrupts
                XCHG    AX,MVreg        ;Restore value in MVreg !!.01
                ENDM

;saves all basic registers in order of a Registers variable,
;but assumes that flags are already pushed
SaveAllNoFlags  MACRO
                PUSH    ES              ;save all basic registers
                PUSH    DS
                PUSH    DI
                PUSH    SI
                PUSH    BP
                PUSH    DX
                PUSH    CX
                PUSH    BX
                PUSH    AX
                ENDM

;Save all registers, including flags
SaveAllRegs     MACRO
                PUSHF                   ;save flags
                SaveAllNoFlags          ;save all basic registers
                ENDM

;Restore all registers saved in order of SaveAllRegs
RestoreAllRegs  MACRO
                POP     AX              ;restore all basic registers
                POP     BX
                POP     CX
                POP     DX
                POP     BP
                POP     SI
                POP     DI
                POP     DS
                POP     ES
                FakePOPF
                ENDM

;Reset the PIC with an end of interrupt command sent to the proper port.
ResetPIC        MACRO
                MOV     AL,20h          ;end of interrupt to PIC
                OUT     20h,AL          ;send it
                ENDM

;Reset the keyboard on an IBM PC

ResetKbd        MACRO
                IN      AL,61h          ;get keyboard control
                MOV     AH,AL           ;in AH and AL
                OR      AL,80h          ;reset keyboard
                OUT     61h,AL          ;back out
                NullJump                ;delay
                MOV     AL,AH           ;original value
                OUT     61h,AL          ;keyboard is reset
                ENDM

;****************************************************** Equates

True            =       1
False           =       0
WP              EQU     WORD PTR

;constants for referring to window coordinates

XLow            =       (BYTE PTR 0)
YLow            =       (BYTE PTR 1)
XHigh           =       (BYTE PTR 0)
YHigh           =       (BYTE PTR 1)

;display types

UnknownD        =       -1
Unenhanced      =       0
Mono            =       0
CGA             =       1
MCGA            =       2
EGA             =       3
VGA             =       4
PGC             =       5

;miscellaneous constants and equates

Font8x8         =       256
TurboBlink      =       16
Blink           =       10000000b

;****************************************************** Structures

;Structure of a pointer
Pointer STRUC
        Ofst    DW      0
        Segm    DW      0
Pointer ENDS

