;******************************************************
;                  OPFAST.ASM 1.30
;           Fast screen writing routines
;    Copyright (c) TurboPower Software 1987, 1992.
;                All rights reserved.
;******************************************************

        INCLUDE OPCOMMON.ASM

;****************************************************** Data

DATA    SEGMENT BYTE PUBLIC

        ;Pascal variables

        EXTRN   DirectVideo : BYTE              ;If false, use BIOS
        EXTRN   CurrentPage : BYTE              ;Current video page
        EXTRN   CurrentMode : BYTE              ;Current video mode
        EXTRN   InTextMode : BYTE               ;False if in graphics mode
        EXTRN   TextAttr : BYTE                 ;Current video attribute
        EXTRN   NormalAttr : BYTE               ;Attribute for NormVideo
        EXTRN   CheckSnow : BYTE                ;If true, check for retrace
        EXTRN   VideoSegment : WORD             ;Segment of Video Memory
        EXTRN   VirtualSegment : WORD           ;Segment of Video Memory--alt
        EXTRN   VirtualWidth : WORD             ;Current width of virtual display

DATA    ENDS

;****************************************************** Code

CODE    SEGMENT BYTE PUBLIC

        ASSUME  CS:CODE, DS:DATA

        PUBLIC  CalcOffset
        PUBLIC  FastWrite, FastFill, FastVert, FastFillVert
        PUBLIC  ChangeAttribute, MoveScreen

;****************************************************** CalcOffset

;calculate Offset in video memory.
;On entry, AX has Row, DI has Column
;On exit, CX and ES have VideoSegment, DI has offset,
; and DL = 1 if snow checking is needed

CalcOffset      PROC NEAR

        DEC     AX                      ;Row to 0..24 range
        MOV     CX, VirtualWidth        ;CX = Rows per column
        MUL     CX                      ;AX = Row * VirtualWidth
        DEC     DI                      ;Column to 0..79 range
        ADD     DI,AX                   ;DI = (Row * VirtualWidth) + Col
        SHL     DI,1                    ;Account for attribute bytes
        MOV     CX,VirtualSegment       ;CX = VirtualSegment
        MOV     ES,CX                   ;ES:DI points to VideoSegment:Row,Col
        CLD                             ;Set direction to forward
        MOV     DL,CheckSnow            ;Get snow check into DL
        CMP     DL,True                 ;Is it set?
        JNE     CalcExit                ;Exit if not
        CMP     CH,0B8h                 ;Writing to CGA memory?
        JE      CalcExit                ;Exit if so
        SetZero DL                      ;Otherwise turn snow checking off
CalcExit:
        RET                             ;Return

CalcOffset      ENDP

;****************************************************** FastWrite

;procedure FastWrite(St : String; Row, Col : Word; Attr : Byte);
;Write St at Row,Col in Attr (video attribute) without snow

FWAttr          EQU     BYTE PTR SS:[BX+4]
FWCol           EQU     WORD PTR SS:[BX+6]
FWRow           EQU     WORD PTR SS:[BX+8]
FWSt            EQU     DWORD PTR SS:[BX+10]

FastWrite     PROC FAR

        StackFrame
        PUSH    DS                      ;Save DS
        MOV     AX,FWRow                ;AX = Row
        MOV     DI,FWCol                ;DI = Column
        CALL    CalcOffset              ;Call routine to calculate offset
        GetDSPtr        FWSt            ;DS:SI points to St[0]
        SetZero CX                      ;CX = 0
        LODSB                           ;AL = Length(St); DS:SI -> St[1]
        MOV     CL,AL                   ;CX = Length
        JCXZ    FWExit                  ;If string empty, exit
        MOV     AH,FWAttr               ;AH = Attribute
        SHR     DL,1                    ;If snow checking is off...
        JNC     FWNoWait                ; use FWNoWait routine
        MOV     DX,03DAh                ;Point DX to CGA status port
FWGetNext:
        LODSB                           ;Load next character into AL
                                        ; AH already has Attr
        MOV     BX,AX                   ;Store video word in BX
        WaitForRetrace                  ;Wait for an opportunity to write
        WordToCGA       BX              ;Move the word
        LOOP    FWGetNext               ;Get next character
        JMP     SHORT FWExit            ;Done
FWNoWait:
        LODSB                           ;Load next character into AL
                                        ; AH already has Attr
        STOSW                           ;Move video word into place
        LOOP    FWNoWait                ;Get next character
FWExit:
        POP     DS                      ;Restore DS
        RET     10

FastWrite       ENDP

;****************************************************** FastFill

;procedure FastFill(Number : word; Ch : Char; Row, Col : Word; Attr : Byte);
;Write Num Chs at Row,Col in Attr (video attribute) without snow

FFAttr          EQU     BYTE PTR SS:[BX+4]
FFCol           EQU     WORD PTR SS:[BX+6]
FFRow           EQU     WORD PTR SS:[BX+8]
FFCh            EQU     BYTE PTR SS:[BX+10]
FFNumber        EQU     WORD PTR SS:[BX+12]

FastFill        PROC FAR

        StackFrame
        PUSH    DS                      ;Save DS
        MOV     AX,FFRow                ;AX = Row
        MOV     DI,FFCol                ;DI = Column
        CALL    CalcOffset              ;Call routine to calculate offset
        MOV     CX,FFNumber             ;CX = Number to write
        JCXZ    FFExit                  ;If zero, exit
        MOV     AH,FFAttr               ;AH = Attribute
        MOV     AL,FFCh                 ;AL = Char
        SHR     DL,1                    ;If snow checking is off...
        JNC     FFNoWait                ; use FFNoWait routine
        MOV     DX,03DAh                ;Point DX to CGA status port
        MOV     BX,AX                   ;Store video word in BX
FFGetNext:
        WaitForRetrace                  ;Wait for an opportunity to write
        WordToCGA       BX              ;Move the word
        LOOP    FFGetNext               ;Get next character
        JMP     SHORT FFExit            ;Done
FFNoWait:
        REP STOSW                       ;Move video word into place !!.03
;       LOOP    FFNoWait                ;Get next character         !!.03
FFExit:
        POP     DS                      ;Restore DS
        RET     10

FastFill        ENDP

;****************************************************** FastFillVert !!.03

;procedure FastFillVert(Number : word; Ch : Char; Row, Col : Word; Attr : Byte);
;Fill Number chs vertically at Row,Col in Attr (video attribute)

FastFillVert    PROC FAR

        StackFrame
        PUSH    DS                      ;Save DS
        PUSH    BP                      ;Save BP
        MOV     BP,VirtualWidth         ;BP = 2*(VirtualWidth-1)
        DEC     BP
        SHL     BP,1
        MOV     AX,FFRow                ;AX = Row
        MOV     DI,FFCol                ;DI = Column
        CALL    CalcOffset              ;Call routine to calculate offset
        MOV     CX,FFNumber             ;CX = Number to write
        JCXZ    FFVExit                 ;If zero, exit
        MOV     AH,FFAttr               ;AH = Attribute
        MOV     AL,FFCh                 ;AL = Char
        SHR     DL,1                    ;If snow checking is off...
        JNC     FFVNoWait               ; use FFVNoWait routine
        MOV     DX,03DAh                ;Point DX to CGA status port
        MOV     BX,AX                   ;Store video word in BX
FFVGetNext:
        WaitForRetrace                  ;Wait for an opportunity to write
        WordToCGA       BX              ;Move the word
        ADD     DI,BP                   ;Next row
        LOOP    FFVGetNext              ;Get next character
        JMP     SHORT FFVExit           ;Done
FFVNoWait:
        STOSW                           ;Move video word into place
        ADD     DI,BP                   ;Next row
        LOOP    FFVNoWait               ;Repeat
FFVExit:
        POP     BP                      ;Restore regs
        POP     DS
        RET     10

FastFillVert    ENDP

;****************************************************** FastVert

;procedure FastVert(St : String; Row, Col : Word; Attr : Byte);
;Write St vertically at Row,Col in Attr (video attribute)

FVAttr          EQU     BYTE PTR SS:[BX+4]
FVCol           EQU     WORD PTR SS:[BX+6]
FVRow           EQU     WORD PTR SS:[BX+8]
FVSt            EQU     DWORD PTR SS:[BX+10]

FastVert      PROC FAR

        StackFrame
        PUSH    DS                      ;Save DS
        PUSH    BP                      ;Save BP
        MOV     AX,FVRow                ;AX = Row
        MOV     DI,FVCol                ;DI = Column
        CALL    CalcOffset              ;Call routine to calculate offset
        MOV     BP,VirtualWidth         ;BP = 2*(VirtualWidth-1)
        DEC     BP
        SHL     BP,1
        GetDSPtr FVSt                   ;DS:SI points to St[0]
        LODSB                           ;AL = Length(St); DS:SI -> St[1]
        SetZero CH                      ;CH = 0
        MOV     CL,AL                   ;CX = Length
        JCXZ    FVExit                  ;If string empty, exit
        MOV     AH,FVAttr               ;AH = Attribute
        SHR     DL,1                    ;If snow checking is off...
        JNC     FVNoWait                ; use FVNoWait routine
        MOV     DX,03DAh                ;Point DX to CGA status port
FVGetNext:
        LODSB                           ;Load next character into AL
                                        ; AH already has Attr
        MOV     BX,AX                   ;Store video word in BX
        WaitForRetrace                  ;Wait for an opportunity to write
        WordToCGA       BX              ;Move the word
        ADD     DI,BP                   ;Next row
        LOOP    FVGetNext               ;Get next character
        JMP     SHORT FVExit            ;Done
FVNoWait:
        LODSB                           ;Load next character into AL
                                        ; AH already has Attr
        STOSW                           ;Move video word into place
        ADD     DI,BP                   ;Next row
        LOOP    FVNoWait                ;Get next character
FVExit:
        POP     BP                      ;Restore regs
        POP     DS
        RET     10

FastVert        ENDP

;****************************************************** ChangeAttribute

;procedure ChangeAttribute(Number : Word; Row, Col : Word; Attr : Byte);
;Change Number video attributes to Attr starting at Row,Col

CAAttr          EQU     BYTE PTR SS:[BX+4]
CACol           EQU     WORD PTR SS:[BX+6]
CARow           EQU     WORD PTR SS:[BX+8]
CANumber        EQU     WORD PTR SS:[BX+10]

ChangeAttribute       PROC FAR

        StackFrame
        MOV     AX,CARow                ;AX = Row
        MOV     DI,CACol                ;DI = Column
        CALL    CalcOffset              ;Call routine to calculate offset
        INC     DI                      ;Skip character
        MOV     AL,CAAttr               ;AL = Attribute
        MOV     CX,CANumber             ;CX = Number to change
        JCXZ    CAExit                  ;If zero, exit
        SHR     DL,1                    ;If snow checking is off...
        JNC     CANoWait                ; use CANoWait routine
        MOV     AH,AL                   ;Store attribute in AH
        MOV     DX,03DAh                ;Point DX to CGA status port
CAGetNext:
        WaitForRetrace                  ;Wait for an opportunity to write
        MOV     AL,AH                   ;Move Attr back to AL...
        STOSB                           ; and then to screen
        STI                             ;Allow interrupts
        INC     DI                      ;Skip characters
        LOOP    CAGetNext               ;Look for next opportunity
        JMP     SHORT CAExit            ;Done
CANoWait:
        STOSB                           ;Change the attribute
        INC     DI                      ;Skip characters
        LOOP    CANoWait                ;Get next character
CAExit:
        RET     8

ChangeAttribute       ENDP

;****************************************************** MoveScreen

;procedure MoveScreen(var Source, Dest; Length : Word);
;Move Length words from Source to Dest without snow

MLength         EQU     WORD PTR SS:[BX+4]
MDest           EQU     DWORD PTR SS:[BX+6]
MSource         EQU     DWORD PTR SS:[BX+10]

MoveScreen      PROC FAR

        StackFrame
        PUSH    DS                      ;Save DS
        SetZero AH                      ;AH = 0
        MOV     AL,CheckSnow            ;Grab before changing DS
        GetPtr          MDest           ;ES:DI points to Dest
        GetDSPtr        MSource         ;DS:SI points to Source
        MOV     CX,MLength              ;CX = Length
        JCXZ    MSExit                  ;Exit if CX = 0
        CLD                             ;Assume forward
        MOV     BX,DS                   ;BX = DS
        MOV     DX,ES                   ;DX = ES
        CMP     DX,BX                   ;Same segment?
        MOV     BL,0                    ;Clear same-segment flag
        JNE     MSForward               ;If not, go forward
        INC     BL                      ;Set same-segment flag
        CMP     SI,DI                   ;Check for potential overlap
        JAE     MSForward               ;Go forward if Source at higher offset

        STD                             ;Go backwards
        DEC     CX                      ;CX = Number of words to add to SI/DI
        ADD     DI,CX                   ;Point DI to end of Dest area
        ADD     DI,CX
        ADD     SI,CX                   ;Point SI to end of Source area
        ADD     SI,CX
        INC     CX                      ;Reset CX
        INC     AH                      ;Flag to indicate we're going backward

MSForward:
        SHR     AL,1                    ;Snow checking on?
        JNC     MSNoWait                ;Skip the rest of this if not
        CMP     BH,0B8h                 ;See if we're reading from CGA memory
        JE      MSGetNext               ;If so, wait for retrace
        CMP     DH,0B8h                 ;Check segment in ES, too
        JNE     MSNoWait                ;Not writing to CGA
MSGetNext:
        MOV     DX,03DAh                ;Point DX to CGA status port

IF SuppressAllSnow
        OR      AH,AH                   ;Going forward?
        JZ      MSgo                    ;If so, continue
        INC     SI                      ;Else, point SI/DI to last byte, rather
        INC     DI                      ; than the last word
ELSE
        ;see if we can use a faster algorithm
        OR      BL,BL                   ;is same-segment flag set?
        JNZ     MSgo                    ;if so, use WordMoveNoSnow algorithm
        FastMoveNoSnow                  ;else, use faster algorithm
        JMP     SHORT MSExit
ENDIF

MSgo:
        WordMoveNoSnow                  ;Move CX words, preventing snow
        JMP     SHORT MSExit            ;All done
MSNoWait:
        REP     MOVSW                   ;That's all!
MSExit:
        CLD                             ;Reset direction flag
        POP     DS                      ;Restore DS
        RET     10

MoveScreen      ENDP

CODE    ENDS

        END
