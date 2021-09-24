;******************************************************
;                  OPFLEX.ASM 1.30
;             Flexible fast write routine
;     Copyright (c) TurboPower Software 1988, 1992.
;                All rights reserved.
;******************************************************

        INCLUDE OPCOMMON.ASM

;****************************************************** Data

DATA    SEGMENT BYTE PUBLIC

DATA    ENDS

;****************************************************** Code

CODE    SEGMENT BYTE PUBLIC

        ASSUME  CS:CODE, DS:DATA

        PUBLIC  FlexWrite, FlexLen, FastWriteCtrl, FastWriteAttr

        EXTRN   CalcOffset : NEAR               ;in OPFAST.OBJ

;****************************************************** FlexWrite

;procedure FlexWrite(St : String; Row, Col : Word; var FAttrs : FlexAttrs);
;Write St at Row,Col with flexible color handling

FWSt    EQU     DWORD PTR [BP+14]
FWRow   EQU     WORD PTR [BP+12]
FWCol   EQU     WORD PTR [BP+10]
FWAttrs EQU     DWORD PTR [BP+6]
;return address is at BP+2
;old BP is at BP+0
SaveDS  EQU     WORD PTR [BP-2]
Attrs   EQU     BYTE PTR [BP-6]

FlexWrite       PROC FAR

        StackFrameBP
        PUSH    DS                      ;Save DS
        SUB     SP,4                    ;make room for locals
        CLD                             ;go forward

        LDS     SI,FWAttrs              ;DS:SI => FAttrs
        PUSH    SS                      ;ES:DI => Attrs
        POP     ES
        LEA     DI,Attrs
        MOVSW                           ;make a local copy of the attributes
        MOVSW
        MOV     DS,SaveDS               ;restore DS

        MOV     AX,FWRow                ;AX = Row
        MOV     DI,FWCol                ;DI = Column
        CALL    CalcOffset              ;Call routine to calculate offset

        GetDSPtr FWSt                   ;DS:SI points to St[0]
        SetZero CH                      ;CH = 0
        LODSB                           ;AL = Length(St); DS:SI -> St[1]
        MOV     CL,AL                   ;CX = Length
        JCXZ    FWExit                  ;If string empty, exit

        MOV     AH,Attrs                ;get the default attribute into AH
        SetZero AL                      ;AL = 0
        PUSH    AX                      ;push the pair onto the stack

        MOV     DH,03h                  ;DH = 3
        CMP     DL,CH                   ;If snow checking is on...
        JE      FWGetNext
        MOV     DL,0DAh                 ;Point DX to CGA status port (03DAh)

FWGetNext:
        LODSB                           ;Load next character into AL
        CMP     AL,DH                   ;is it in range ^A..^C?
        JA      NotSpecial
        OR      AL,AL
        JZ      NotSpecial

        POP     BX                      ;get current char/attr pair off stack
        OR      BL,BL                   ;is the character 0?
        JZ      NewAttr                 ;if so, this is a new attribute
        CMP     BL,AL                   ;is it the same as the current one?
        JNE     NewAttr                 ;if not, this is a new attribute
        POP     AX                      ;else get previous char/attr into AX
;       OR      AL,AL                   ;is the character 0?    !!.11
;       JNZ     NotZero                                         !!.11
        PUSH    AX                      ;if so, push the pair back on the stack
;NotZero:                                                       !!.11
        LOOP    FWGetNext               ;and get next character
        JMP     SHORT FWExit

NewAttr:
        PUSH    BX                      ;put the current pair back on stack
        MOV     AH,AL                   ;save the special character in AH
        LEA     BX,Attrs                ;SS:BX points to Attrs
        XLAT    BYTE PTR SS:[0]         ;translate the attribute
        XCHG    AH,AL                   ;get the attr into AH, char into AL
        PUSH    AX                      ;push the char/attr onto the stack
        LOOP    FWGetNext               ;and get next character
        JMP     SHORT FWExit

NotSpecial:
        OR      DL,DL                   ;is DL set up for retrace checking?
        JNZ     FWWait                  ;if so, use special routine
        STOSW                           ;else, move video word into place
        LOOP    FWGetNext               ;and get next character
        JMP     SHORT FWExit

FWWait:
        MOV     BX,AX                   ;Store video word in BX
        WaitForRetrace                  ;Wait for an opportunity to write
        WordToCGA       BX              ;Move the word
        LOOP    FWGetNext               ;Get next character

FWExit:
        MOV     DS,SaveDS               ;Restore DS
        Exit_Code 12                    ;!!.13

FlexWrite       ENDP

;****************************************************** FlexLen

;function FlexLen(S : string) : Byte;
;Return actual (visible) length of a flex string

FLSt    EQU     DWORD PTR SS:[BX+4]

FlexLen PROC FAR

        StackFrame                      ;Set up stack frame
        MOV     DX,DS                   ;save DS
        LDS     SI,FLSt                 ;DS:SI -> string[0]
        CLD                             ;go forward
        SetZero AH                      ;AH will be actual length
        LODSB                           ;AL = length
        MOV     CL,AL
        SetZero CH                      ;CX = length
        JCXZ    FLDone                  ;get out for empty string
FLNext:
        LODSB                           ;AL = next char
        OR      AL,AL                   ;Is it #0?               !!.10
        JZ      FLInc                   ;If so, it counts        !!.10
        CMP     AL,03                   ;is it a flex char?
        JBE     FLCheck                 ;jump if so
FLInc:                                                          ;!!.10
        INC     AH                      ;else inc length
FLCheck:
        LOOP    FLNext                  ;do all the chars
FLDone:
        MOV     DS,DX                   ;restore DS
        MOV     AL,AH                   ;return length
        RET     4                       ;Remove parameters and return

FlexLen ENDP

;****************************************************** FastWriteCtrl

;procedure FastWriteCtrl(St : String; Row, Col : Word; Attr, Ctrl : Byte);
;Write St at Row,Col in Attr (video attribute) without snow. Ctrl characters
;displayed in Ctrl as upper-case letters

FCCtrl          EQU     BYTE PTR SS:[BX+4]
FCAttr          EQU     BYTE PTR SS:[BX+6]
FCCol           EQU     WORD PTR SS:[BX+8]
FCRow           EQU     WORD PTR SS:[BX+10]
FCSt            EQU     DWORD PTR SS:[BX+12]

FastWriteCtrl   PROC FAR

        StackFrame                      ;Set up stack frame
        PUSH    DS                      ;Save DS
        PUSH    BP                      ;Save BP
        MOV     AX,FCRow                ;AX = Row
        MOV     DI,FCCol                ;DI = Column
        CALL    CalcOffset              ;Call routine to calculate offset
        GetDSPtr        FCSt            ;DS:SI points to St[0]
        SetZero CX                      ;CX = 0
        LODSB                           ;AL = Length(St); DS:SI -> St[1]
        MOV     CL,AL                   ;CX = Length
        JCXZ    FCExit                  ;If string empty, exit
        MOV     AH,FCAttr               ;AH = Attr
        MOV     BL,FCCtrl               ;BL = Ctrl
        MOV     BH,AH                   ;BH = Attr
        SHR     DL,1                    ;If snow checking is off...
        JNC     FCNoWait                ; use FCNoWait routine
        MOV     DX,03DAh                ;Point DX to CGA status port
FCGetNext:
        MOV     AH,BH                   ;Assume regular attribute
        LODSB                           ;Load next character into AL
        CMP     AL,' '                  ;Is it a ctrl character?
        JAE     FCnotCtrl               ;If not, continue
        MOV     AH,BL                   ;Else use Ctrl attribute
        ADD     AL,64                   ;Convert ^A to A, etc.
FCnotCtrl:
        MOV     BP,AX                   ;Store video word in BP
        WaitForRetrace                  ;Wait for an opportunity to write
        WordToCGA       BP              ;Move the word
        LOOP    FCGetNext               ;Get next character
        JMP     SHORT FCExit            ;Done
FCNoWait:
        MOV     DX,4020h                ;DH = 64, DL = ' '
FCNoWaitAgain:
        MOV     AH,BH                   ;Assume regular attribute
        LODSB                           ;Load next character into AL
        CMP     AL,DL                   ;Is it a ctrl character?
        JAE     FCnotCtrl2              ;If not, continue
        MOV     AH,BL                   ;Else use Ctrl attribute
        ADD     AL,DH                   ;Convert ^A to A, etc.
FCnotCtrl2:
        STOSW                           ;Move video word into place
        LOOP    FCNoWaitAgain           ;Get next character
FCExit:
        POP     BP                      ;Restore BP
        POP     DS                      ;Restore DS
        RET     12                      ;Remove parameters and return

FastWriteCtrl   ENDP

;****************************************************** FastWriteAttr

;procedure FastWriteAttr(St : String; Row, Col : Word; AttrSt : String);
;Write St at Row,Col using attributes in AttrSt

FASt            EQU     DWORD PTR [BP+14]
FARow           EQU     WORD PTR [BP+12]
FACol           EQU     WORD PTR [BP+10]
FAAttrs         EQU     DWORD PTR [BP+6]
;return address is at BP+2
;old BP is at BP+0
FASaveDS        EQU     WORD PTR [BP-2]
FAAttrSeg       EQU     WORD PTR [BP-4]
FAAttrOfs       EQU     WORD PTR [BP-6]
FAAttrPtr       EQU     DWORD PTR [BP-6]
FAStSeg         EQU     WORD PTR [BP-8]
FAStOfs         EQU     WORD PTR [BP-10]
FAStPtr         EQU     DWORD PTR [BP-10]

FastWriteAttr   PROC FAR

        StackFrameBP                    ;Set up stack frame
        PUSH    DS                      ;Save DS
        CLD                             ;go forward

        MOV     AX,FARow                ;AX = Row
        MOV     DI,FACol                ;DI = Column
        CALL    CalcOffset              ;Call routine to calculate offset

        LDS     BX,FAAttrs              ;DS:BX => FAAttrs[0]
        INC     BX                      ;DS:BX => FAAttrs[1]
        PUSH    DS                      ;push pointer onto the stack (AttrPtr)
        PUSH    BX

        LDS     SI,FASt                 ;DS:SI points to St[0]
        LODSB                           ;AL = Length(St); DS:SI -> St[1]
        PUSH    DS                      ;push pointer onto the stack (StPtr)
        PUSH    SI
        SetZero CH                      ;CH = 0
        MOV     CL,AL                   ;CX = Length
        JCXZ    FAExit                  ;If string empty, exit
        SHR     DL,1                    ;If snow checking is off...
        JNC     FANoWait                ; use FANoWait routine
        MOV     DX,03DAh                ;Point DX to CGA status port

FAGetNext:
        LDS     SI,FAStPtr              ;Get pointer to the character string
        LODSB                           ;Load next character into AL
        MOV     FAStOfs,SI              ;Save the new offset
        MOV     BL,AL                   ;Character to write in BL

        LDS     SI,FAAttrPtr            ;Get pointer to the attribute string
        LODSB                           ;Load next attribute into AL
        MOV     FAAttrOfs,SI            ;Save the new offset
        MOV     BH,AL                   ;Attribute to use in BH

        WaitForRetrace                  ;Wait for an opportunity to write
        WordToCGA       BX              ;Move the word
        LOOP    FAGetNext               ;Get next character
        JMP     SHORT FAExit            ;Done

FANoWait:
        PUSH    BP                      ;Save BP
        MOV     DX,FAStSeg              ;Save segments in registers
        MOV     BP,FAAttrSeg
FANWstart:
        MOV     DS,DX                   ;Get pointer to the character string
        LODSB                           ;Load next character into AL
        MOV     DS,BP                   ;Get pointer to the attribute string
        MOV     AH,[BX]                 ;Load next attribute into AH
        INC     BX                      ;Point BX to next one
        STOSW                           ;Move video word into place
        LOOP    FANWstart               ;Get next character
        POP     BP                      ;Restore BP

FAExit:
        MOV     DS,FASaveDS             ;Restore DS
        Exit_Code 12                    ;!!.13

FastWriteAttr   ENDP

CODE    ENDS

        END
