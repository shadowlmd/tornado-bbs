;******************************************************
;                  OPCMISC.ASM 1.30
;              Miscellaneous CRT routines
;     Copyright (c) TurboPower Software 1987, 1992.
;                All rights reserved.
;******************************************************


        INCLUDE OPCOMMON.ASM

;****************************************************** Data

DATA    SEGMENT BYTE PUBLIC

        ;Pascal variables

        EXTRN   CheckBreak : BYTE               ;Check for ^Break?
        EXTRN   CheckEof : BYTE                 ;Check for ^Z on Read/Ln?
        EXTRN   DirectVideo : BYTE              ;If false, use BIOS
        EXTRN   WindMin : WORD                  ;Min. XY coordinates
        EXTRN   WindMax : WORD                  ;Max. XY coordinates
        EXTRN   LastMode : WORD                 ;Current video mode
        EXTRN   CurrentMode : BYTE              ;Current video mode
        EXTRN   CurrentPage : BYTE              ;Current video page
        EXTRN   CurrentDisplay : BYTE           ;Current display type
        EXTRN   EnhancedDisplay : BYTE          ;Type of enhanced display
                                                ;installed, if any
        EXTRN   InTextMode : BYTE               ;False if in graphics mode
        EXTRN   TextAttr : BYTE                 ;Current video attribute
        EXTRN   NormalAttr : BYTE;              ;Attribute for NormVideo
        EXTRN   CheckSnow : BYTE                ;If true, check for retrace
        EXTRN   CtrlBreakFlag : BYTE            ;True if ^Break pressed
        EXTRN   VideoSegment : WORD             ;Segment of Video Memory
        EXTRN   VirtualSegment : WORD           ;Segment of Video Memory--alt
        EXTRN   BufLen : WORD                   ;Max length of string for Read
        EXTRN   ExitProc : DWORD                ;Turbo's ExitProc pointer
        EXTRN   SaveInt1B : DWORD               ;Previous INT 1Bh handler
        EXTRN   DetectMultitasking : BYTE       ;If True, automatically checks
                                                ;  for multitasker
        EXTRN   MultitaskingOn : BYTE           ;True if running under
                                                ;  multitasker
        EXTRN   IsCompaq : BYTE                 ;True if system is a COMPAQ
        EXTRN   BiosScroll : BYTE               ;If False, use special scroll
                                                ;  routine
        EXTRN   UseEnhancedKbd : BYTE           ;True to use enhanced kbd calls

DATA    ENDS

;****************************************************** Code

CODE    SEGMENT BYTE PUBLIC

        ASSUME  CS:CODE, DS:DATA

        ;standard CRT unit routines

        PUBLIC  Sound, NoSound, ClrEol, InsLine, DelLine
        PUBLIC  ScrollWindowUp, ScrollWindowDown

        ;extensions

        PUBLIC  WhereXAbs, WhereYAbs, WhereXY, SetVisiblePage
        PUBLIC  ReadCharAtCursor, ReadAttrAtCursor
        PUBLIC  SetCursorSize, CursorTypeSL, CursorStartLine, CursorEndLine
        PUBLIC  KbdFlags, CheckKbd

        EXTRN   ReadCursorPrim : NEAR
        EXTRN   SetCursorPrim : NEAR
        EXTRN   GetCursorPrim : NEAR
        EXTRN   GetCrtModePrim : NEAR
        EXTRN   ScrollUpPrim : NEAR
        EXTRN   ScrollDownPrim : NEAR
        EXTRN   GetCharAttr : NEAR

;****************************************************** SetCursorSize

;procedure SetCursorSize(StartLine, EndLine : Byte);
;Sets the cursor's starting and ending scan lines.

SCStart EQU     BYTE PTR SS:[BX+6]
SCEnd   EQU     BYTE PTR SS:[BX+4]

SetCursorSize   PROC FAR

        StackFrame                      ;Set up stack frame
        MOV     CH,SCStart              ;CH = starting scan line
        MOV     CL,SCEnd                ;CL = ending scan line
        VideoCall       1               ;Set cursor size service
        RET     4                       ;Return

SetCursorSize   ENDP

;****************************************************** CursorTypeSL

;function CursorTypeSL : Word;
;Returns a word. High byte has starting scan line, low byte has ending.

CursorTypeSL    PROC FAR

        CALL    ReadCursorPrim          ;Call our primitive routine
        RET

CursorTypeSL    ENDP

;****************************************************** CursorStartLine

;function CursorStartLine : Byte;
;Returns the starting scan line of the cursor

CursorStartLine PROC FAR

        CALL    ReadCursorPrim          ;Call our primitive routine
        SetZero AL                      ;AL = 0
        XCHG    AH,AL                   ;AL has result, AH = 0
        RET

CursorStartLine ENDP

;****************************************************** CursorEndLine

;function CursorEndLine : Byte;
;Returns the ending scan line of the cursor.

CursorEndLine PROC FAR

        CALL    ReadCursorPrim          ;Call our primitive routine
        SetZero AH                      ;AH = 0, AL has result
        RET

CursorEndLine ENDP

;****************************************************** WhereXAbs

;function WhereXAbs : Byte;
;Return column coordinate of cursor.

WhereXAbs       PROC FAR

        CALL    GetCursorPrim           ;Get current column into DL
        INC     DL                      ;Adjust for 1..80 format
        SetZero AH                      ;Clear AH
        MOV     AL,DL                   ;Result into AL
        RET

WhereXAbs       ENDP

;****************************************************** WhereYAbs

;function WhereYAbs : Byte;
;Return row coordinate of cursor

WhereYAbs       PROC FAR

        CALL    GetCursorPrim           ;Get current row into DH
        INC     DH                      ;Adjust for 1..25 format
        SetZero AH                      ;Clear AH
        MOV     AL,DH                   ;Result into AL
        RET

WhereYAbs       ENDP

;****************************************************** WhereXY

;function WhereXY : Word;
;Return absolute row and column coordinates of cursor.
;High byte has current row (Y), low byte has current column (X).

WhereXY PROC FAR

        CALL    GetCursorPrim           ;Get current column into DL
        INC     DL                      ;Adjust for 1..80 format
        INC     DH                      ;Adjust for 1..25 format
        MOV     AX,DX                   ;Result into AX
        RET

WhereXY ENDP

;****************************************************** KbdFlags

;function KbdFlags : Byte;
;Returns keyboard status flags as a bit-coded byte

KbdFlags        PROC FAR

        CMP     UseEnhancedKbd,True     ;Use enhanced keyboard call?
        JNE     KFgo

        KbdCall 12h                     ;Get shift status service
        JMP     SHORT KFdone

KFgo:   KbdCall 2h                      ;Get shift status service
KFdone: SetZero AH                      ;Clear AH -- result in AL
        RET

KbdFlags        ENDP

;****************************************************** CheckKbd

;function CheckKbd(var KeyCode : Word) : Boolean;
;Returns True (and the key codes) if a keystroke is waiting

KeyCode EQU     DWORD PTR SS:[BX+4]
KeyPtr  EQU     WORD PTR ES:[DI]

CheckKbd        PROC FAR

        StackFrame
        SetZero DX                      ;Assume false (DX has boolean result)
        CMP     UseEnhancedKbd,True     ;Use enhanced keyboard call?
        JNE     CKgo

        KbdCall 11h                     ;Character ready service
        JZ      CkDone                  ;Done if zero flag set
        CMP     AL,0E0h                 ;Check for $E0 flag
        JNE     CKskip
        SetZero AL                      ;Change it to 0
        JMP     SHORT CKskip

CKgo:   KbdCall 1h                      ;Character ready service
        JZ      CkDone                  ;Done if zero flag set
CKskip: INC     DX                      ;DX = Ord(True)
        GetPtr  KeyCode                 ;ES:DI points to KeyCode
        MOV     KeyPtr,AX               ;Character and scan code into KeyCode
CKDone:
        MOV     AX,DX                   ;Boolean result into AX
        RET     4

CheckKbd        ENDP

;****************************************************** Sound

;procedure Sound(Hz: Word);
;Turn on the sound at the designated frequency

Hertz   EQU     WORD PTR SS:[BX+4]

Sound   PROC FAR

        StackFrame
        MOV     BX,Hertz                ;BX = Hz
        MOV     AX,34DCh
        MOV     DX,0012h                ;DX:AX = $1234DC = 1,193,180
        CMP     DX,BX                   ;Make sure the division won't
        JAE     SoundExit               ; produce a divide by zero error
        DIV     BX                      ;Count (AX) = $1234DC div Hz
        MOV     BX,AX                   ;Save Count in BX

        IN      AL,61h                  ;Check the value in port $61
        TEST    AL,00000011b            ;Bits 0 and 1 set if speaker is on
        JNZ     SetCount                ;If they're already on, continue

        ;turn on speaker
        OR      AL,00000011b            ;Set bits 0 and 1
        OUT     61h,AL                  ;Change the value
        MOV     AL,182                  ;Tell the timer that the count is coming
        OUT     43h,AL                  ;by sending 182 to port $43

SetCount:
        MOV     AL,BL                   ;Low byte into AL
        OUT     42h,AL                  ;Load low order byte into port $42
        MOV     AL,BH                   ;High byte into AL
        OUT     42h,AL                  ;Load high order byte into port $42

SoundExit:
        RET     2

Sound   ENDP

;****************************************************** NoSound

;procedure NoSound;
;Turn off the sound

NoSound PROC FAR

        IN      AL,61h                  ;Get current value of port $61
        AND     AL,11111100b            ;Turn off bits 0 and 1
        OUT     61h,AL                  ;Reset the port
        RET

NoSound ENDP

;****************************************************** SetVisiblePage

;procedure SetVisiblePage(PageNum : Byte);
;Set current video page

PageNum         EQU     BYTE PTR SS:[BX+4]

SetVisiblePage  PROC FAR

        StackFrame
        MOV     AL,PageNum              ;AL = PageNum
        VideoCall       5               ;Set active display page service
        CALL    GetCrtModePrim          ;make sure the change was accepted
        RET     2

SetVisiblePage  ENDP

;****************************************************** ClrEol

;procedure ClrEol;
;Clear the remainder of the current screen line

ClrEol  PROC FAR

        CALL    GetCursorPrim           ;Get current row,col into DH,DL
        MOV     CX,DX                   ;top row = bottom row
                                        ;left column = right column
        MOV     DL,WindMax.XHigh        ;right column = right edge of window
        MOV     BH,TextAttr             ;BH = filler attribute
        SetZero AL                      ;AL = 0 (lines to scroll)
        CALL    ScrollUpPrim            ;Call scroll window up primitive
        RET

ClrEol  ENDP

;****************************************************** InsLineSetup

;Setup for InsLine and DelLine

InsLineSetup    PROC NEAR

        CALL    GetCursorPrim           ;Get current row,col into DH,DL
        MOV     CH,DH                   ;top row of window = current row
        MOV     CL,WindMin.XLow         ;left column = XLow
        MOV     DX,WindMax              ;right column = XHigh
                                        ;bottom row = YHigh
        MOV     BH,TextAttr             ;BH = filler attribute
        MOV     AL,1                    ;Scroll one line
        CMP     CH,DH                   ;is it a one-line window?
        JNE     ILSdone                 ;if not, we're OK
        SetZero AL                      ;else we need to scroll 0 lines
ILSdone:
        RET

InsLineSetup    ENDP

;****************************************************** InsLine

;procedure InsLine;
;Insert a new line at the position of the cursor

InsLine PROC FAR

        CALL    InsLineSetup            ;Call setup routine
        CALL    ScrollDownPrim          ;Call primitive scroll routine
        RET

InsLine ENDP

;****************************************************** DelLine

;procedure DelLine;
;Delete current screen line

DelLine PROC FAR

        CALL    InsLineSetup            ;Call setup routine
        CALL    ScrollUpPrim            ;Call primitive scroll routine
        RET

DelLine ENDP

;****************************************************** ScrollSetup

SsXLo   EQU     BYTE PTR SS:[BX+14]
SsYLo   EQU     BYTE PTR SS:[BX+12]
SsXHi   EQU     BYTE PTR SS:[BX+10]
SsYHi   EQU     BYTE PTR SS:[BX+8]
SsLines EQU     BYTE PTR SS:[BX+6]

ScrollSetup     PROC NEAR

        StackFrame
        MOV     CL,SsXLo                ;left column = X1
        DEC     CL
        MOV     CH,SsYLo                ;top row = Y1
        DEC     CH
        MOV     DL,SsXHi                ;right column = X2
        DEC     DL
        MOV     DH,SsYHi                ;bottom row = Y2
        DEC     DH
        MOV     AL,SsLines              ;AL = lines to scroll
        MOV     BH,TextAttr             ;BH = filler attribute
        CMP     CH,DH                   ;is it a one-line window?
        JNE     SSdone                  ;if not, we're OK
        SetZero AL                      ;else we need to scroll 0 lines
SSDone: RET

ScrollSetup     ENDP

;****************************************************** ScrollWindowUp

;procedure ScrollWindowUp(X1, Y1, X2, Y2, Lines : Byte);
;Scrolls the designated window up the specified number of lines.

ScrollWindowUp  PROC FAR

        CALL    ScrollSetup             ;Get parameters, load registers
        CALL    ScrollUpPrim            ;Call primitive scroll routine
        RET     10

ScrollWindowUp  ENDP

;****************************************************** ScrollWindowDown

;procedure ScrollWindowDown(X1, Y1, X2, Y2, Lines : Byte);
;Scrolls the designated window down the specified number of lines.

ScrollWindowDown        PROC FAR

        CALL    ScrollSetup             ;Get parameters, load registers
        CALL    ScrollDownPrim          ;Call primitive scroll routine
        RET     10

ScrollWindowDown        ENDP

;****************************************************** ReadCharAtCursor

;function ReadCharAtCursor : Char;
;Returns character at the current cursor location on the selected page.

ReadCharAtCursor        PROC FAR

        CALL    GetCharAttr             ;Get character and attribute
        RET                             ;Character is in AL

ReadCharAtCursor        ENDP

;****************************************************** ReadAttrAtCursor

;function ReadAttrAtCursor : Byte;
;Returns attribute at the current cursor location on the selected page.

ReadAttrAtCursor        PROC FAR

        CALL    GetCharAttr             ;Get character and attribute
        MOV     AL,AH                   ;Attribute is in AH
        RET

ReadAttrAtCursor        ENDP

CODE    ENDS

        END
