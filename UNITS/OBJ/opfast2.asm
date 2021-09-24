;******************************************************
;                  OPFAST2.ASM 1.30
;           More fast screen writing routines
;      Copyright (c) TurboPower Software 1987, 1992.
;                All rights reserved.
;******************************************************

        INCLUDE OPCOMMON.ASM

;****************************************************** Data

DATA    SEGMENT BYTE PUBLIC

        ;Pascal variables

        EXTRN   WindMin : WORD                  ;Min. XY coordinates
        EXTRN   WindMax : WORD                  ;Max. XY coordinates
        EXTRN   CurrentPage : BYTE              ;Current video page
        EXTRN   CurrentMode : BYTE              ;Current video mode
        EXTRN   InTextMode : BYTE               ;False if in graphics mode
        EXTRN   TextAttr : BYTE                 ;Current video attribute
        EXTRN   NormalAttr : BYTE               ;Attribute for NormVideo
        EXTRN   CheckSnow : BYTE                ;If true, check for retrace
        EXTRN   VirtualSegment : WORD           ;Segment of Video Memory--alt

DATA    ENDS


CODE    SEGMENT BYTE PUBLIC

        ASSUME  CS:CODE, DS:DATA

        PUBLIC  FastCenter, FastFlush
        PUBLIC  FastText, WriteAttribute
        PUBLIC  FastRead, ReadAttribute

        EXTRN   CalcOffset : NEAR               ;in TPFAST.OBJ
        EXTRN   FastWrite : FAR

;****************************************************** ReadAttribute

;procedure ReadAttribute(Number : Byte; Row, Col : Word; var St : string);
;Read Number attributes from the screen into St starting at Row,Col

ReadAttribute   PROC FAR

        MOV     SI,1                    ;Read attributes
        JMP     SHORT FastReadPrim

ReadAttribute   ENDP

;****************************************************** FastRead

;procedure FastRead(Number : Byte; Row, Col : Word; var St : string);
;Read Number characters from the screen into St starting at Row,Col

FRSt    EQU     DWORD PTR SS:[BX+4]
FRCol   EQU     WORD PTR SS:[BX+8]
FRRow   EQU     WORD PTR SS:[BX+10]
FRNum   EQU     BYTE PTR SS:[BX+12]

FastRead      PROC FAR

        SetZero SI                      ;Read characters

FastReadPrim:

        StackFrame
        PUSH    DS                      ;Save DS
        MOV     AX,FRRow                ;AX = Row
        MOV     DI,FRCol                ;DI = Column
        CALL    CalcOffset              ;Call routine to calculate offset
        ADD     DI,SI                   ;adjust for attributes if necessary
        MOV     DS,CX                   ;CX still has VideoSegment
        MOV     SI,DI                   ;DS:SI points to VideoSegment:Row,Col
        GetPtr  FRSt                    ;ES:DI points to St[0]
        SetZero AH                      ;AH = 0
        MOV     AL,FRNum                ;AX = number of bytes to read
        STOSB                           ;Set length byte
        MOV     CX,AX                   ;CX = Length
        JCXZ    FRExit                  ;If string empty, exit
        SHR     DL,1                    ;If snow checking is off...
        JNC     FRNoWait                ; use FWNoWait routine
        MOV     DX,03DAh                ;Point DX to CGA status port
FRGetNext:
        WaitForRetrace                  ;Wait for an opportunity
        LODSB                           ;Load next char into AX
        STI                             ;Allow interrupts
        STOSB                           ;Store the character in St
        INC     SI                      ;Skip attribute
        LOOP    FRGetNext               ;Get next character
        JMP     SHORT FRExit            ;Done
FRNoWait:
        LODSB                           ;Load next byte into AL  !!.22
        INC     SI                      ;Skip over unused byte   !!.22
;!!.22  LODSW                           ;Load next word into AX
        STOSB                           ;Move character into St
        LOOP    FRNoWait                ;Get next character
FRExit:
        POP     DS                      ;Restore DS
        RET     10

FastRead      ENDP

;****************************************************** WriteAttribute

;procedure WriteAttribute(St : String; Row, Col : Word);
;Write string of attributes St at Row,Col without changing characters

WriteAttribute  PROC FAR

        MOV     SI,1                    ;Write attributes
        JMP     SHORT FastTextPrim

WriteAttribute  ENDP

;****************************************************** FastText

;procedure FastText(St : string; Row, Col : Word);
;Write St at Row,Col without changing the underlying video attribute.

FTCol           EQU     WORD PTR SS:[BX+4]
FTRow           EQU     WORD PTR SS:[BX+6]
FTStr           EQU     DWORD PTR SS:[BX+8]

FastText        PROC FAR

        SetZero SI                      ;Write characters

FastTextPrim:

        StackFrame
        PUSH    DS                      ;Save DS
        MOV     AX,FTRow                ;AX = Row
        MOV     DI,FTCol                ;DI = Column
        CALL    CalcOffset              ;Call routine to calculate offset
        ADD     DI,SI                   ;adjust for attributes if necessary
        GetDSPtr        FTStr           ;DS:SI points to St[0]
        SetZero CX                      ;CX = 0
        LODSB                           ;AL = Length(St); DS:SI -> St[1]
        MOV     CL,AL                   ;CX = Length
        JCXZ    FTExit                  ;If string empty, exit
        SHR     DL,1                    ;If snow checking is off...
        JNC     FTNoWait                ; use FTNoWait routine
        MOV     DX,03DAh                ;Point DX to CGA status port
FTGetNext:
        LODSB                           ;Load next character into AL
                                        ; AH already has Attr
        MOV     AH,AL                   ;Store char in AH
        WaitForRetrace                  ;Wait for an opportunity to write
        MOV     AL,AH                   ;Move char back to AL
        STOSB                           ;And then to screen
        STI                             ;Allow interrupts
        INC     DI                      ;Skip attribute bytes
        LOOP    FTGetNext               ;Get next character
        JMP     SHORT FTExit            ;Done
FTNoWait:
        MOVSB                           ;Move character to screen
        INC DI                          ;Skip attribute bytes
        LOOP    FTNoWait                ;Get next character
FTExit:
        POP     DS                      ;Restore DS
        RET     8

FastText        ENDP

;****************************************************** FastCenter

;procedure FastCenter(St : String; Row, Attr : Byte);
;Write St centered on window Row in Attr (video attribute) without snow

FCAttr          EQU     BYTE PTR SS:[BX+4]
FCRow           EQU     BYTE PTR SS:[BX+6]
FCSt            EQU     DWORD PTR SS:[BX+8]

FastCenter      PROC FAR

        StackFrame
        GetPtr  FCSt                    ;ES:DI points to St
        SetZero DH                      ;DX = (Length / 2)
        MOV     DL,ES:[DI]
        SHR     DX,1
        SetZero AH                      ;AX = Succ(LeftEdge+RightEdge)/2
        MOV     AL,WindMin.XLow
        ADD     AL,WindMax.XHigh
        INC     AX
        SHR     AX,1
        SUB     AX,DX                   ;AX has column for St
        INC     AX                      ;convert to 1-base

        ;now set up the stack the way FastWrite wants it

        POP     CX                      ;get the return address into DX:CX
        POP     DX
        POP     DI                      ;get the attribute into DI
        POP     BX                      ;get the row into BX
        ADD     BL,WindMin.YLow         ;adjust the row and put it back
        SetZero BH
        PUSH    BX
        PUSH    AX                      ;push the column for St
        PUSH    DI                      ;put back the attribute and return addr
        PUSH    DX
        PUSH    CX
        JMP     FastWrite               ;let FastWrite do the rest

FastCenter      ENDP

;****************************************************** FastFlush

;procedure FastFlush(St : String; Row, Attr : Byte);
;Write St flush right on window Row in Attr (video attribute) without snow

FastFlush       PROC FAR

        StackFrame
        GetPtr  FCSt                    ;ES:DI points to St
        SetZero DH                      ;DX = Length(St)
        MOV     DL,ES:[DI]
        SetZero AH
        MOV     AL,WindMax.XHigh
        INC     AX                      ;convert to 1-base
        SUB     AX,DX                   ;AX will have column for St
        INC     AX

        ;now set up the stack the way FastWrite wants it

        POP     CX                      ;get the return address into DX:CX
        POP     DX
        POP     DI                      ;get the attribute into DI
        POP     BX                      ;get the row into BX
        ADD     BL,WindMin.YLow         ;adjust the row and put it back
        SetZero BH
        PUSH    BX
        PUSH    AX                      ;push the column for St
        PUSH    DI                      ;put back the attribute and return addr
        PUSH    DX
        PUSH    CX
        JMP     FastWrite               ;let FastWrite do the rest

FastFlush       ENDP

CODE    ENDS

        END
