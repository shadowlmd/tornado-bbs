                cCR             EQU     13
                ZDle            EQU     24

.386P
LOCALS

CODE32          SEGMENT DWORD USE32 PUBLIC 'CODE'
CODE32          ENDS

CONST32         SEGMENT DWORD USE32 PUBLIC 'CONST'
CONST32         ENDS

DATA32          SEGMENT DWORD USE32 PUBLIC 'DATA'
DATA32          ENDS

TLS             SEGMENT DWORD USE32 PUBLIC 'TLS'
TLS             ENDS

BSS32           SEGMENT DWORD USE32 PUBLIC 'BSS'
BSS32           ENDS

DGROUP          GROUP CONST32, DATA32, TLS, BSS32
                .MODEL FLAT

CODE32          SEGMENT
                ASSUME CS:FLAT, DS:FLAT, SS:FLAT, ES:FLAT

        PUBLIC  ApZmodem@AsmEscapeBlock

; Function AsmEscapeBlock (InBuf: Pointer; InBufSize: Word; OutBuf: Pointer;
;                          Escaping: CharSet; Var LastChar: Char;
;                          EscapeAll: Boolean): Word;
        EscB_InBuf      EQU     DWord Ptr [EBP + 28]
        EscB_InBufSize  EQU     DWord Ptr [EBP + 24]
        EscB_OutBuf     EQU     DWord Ptr [EBP + 20]
        EscB_Escaping   EQU     DWord Ptr [EBP + 16]
        EscB_LastChar   EQU     DWord Ptr [EBP + 12]
        EscB_EscapeAll  EQU     Byte Ptr [EBP + 8]
        Size_Params     =       24

        ALIGN   4
ApZmodem@AsmEscapeBlock PROC    NEAR
        Push    EBP
        Mov     EBP, ESP
        Push    EBX
        Mov     EAX, EscB_LastChar
        Push    ESI
        Mov     CL, EscB_EscapeAll
        Push    EDI
        Xor     EBX, EBX
        Mov     CH, [EAX]
        Mov     EDX, EscB_Escaping
        Mov     EDI, EscB_OutBuf
        Mov     ESI, EscB_InBuf
        Mov     EBP, EscB_InBufSize
        Push    EDI
        Push    EAX
        Test    EBP, EBP
        Jz      @@Done
        Test    CL, CL
        Jnz     @@Loop_
  @@Loop:
        Mov     AL, [ESI]
        Inc     ESI
        Mov     BL, AL
        Mov     CL, AL
        Shr     BL, 3
        Mov     AH, AL
        And     CL, 7
        Mov     BL, [EDX + EBX]
        Inc     CL
        And     AH, 7Fh
        Shr     BL, CL
        Jc      @@Escape
        Cmp     AH, cCR
        Jz      @@CheckCR
  @@NoEscape:
        Mov     [EDI], AL
        Inc     EDI
        Mov     CH, AL
        Dec     EBP
        Jnz     @@Loop
  @@Done:
        Mov     EAX, EDI
        Pop     EBX
        Pop     EDX
        Pop     EDI
        Mov     [EBX], CH
        Sub     EAX, EDX
        Pop     ESI
        Pop     EBX
        Pop     EBP
        Ret     Size_Params
  @@CheckCR:
        And     CH, 7Fh
        Cmp     CH, 40h
        Jnz     @@NoEscape
  @@Escape:
        Mov     AH, AL
        Mov     AL, ZDle
        Xor     AH, 40h
        Mov     [EDI], AX
        Add     EDI, 2
        Mov     CH, AH
        Dec     EBP
        Jnz     @@Loop
        Jmp     Short @@Done
  @@Loop_:
        Mov     AL, [ESI]
        Inc     ESI
        Mov     BL, AL
        Mov     CL, AL
        Shr     BL, 3
        Mov     AH, AL
        And     CL, 7
        Mov     BL, [EDX + EBX]
        Inc     CL
        And     AH, 7Fh
        Shr     BL, CL
        Jc      @@Escape_
        Cmp     AH, cCR
        Jz      @@CheckCR_
  @@NoCR_:
        Mov     AH, AL
        And     AH, 60h
        Jz      @@Escape_
        Mov     [EDI], AL
        Inc     EDI
        Mov     CH, AL
        Dec     EBP
        Jnz     @@Loop_
        Jmp     Short @@Done
  @@CheckCR_:
        And     CH, 7Fh
        Cmp     CH, 40h
        Jnz     @@NoCR_
  @@Escape_:
        Mov     AH, AL
        Mov     AL, ZDle
        Xor     AH, 40h
        Mov     [EDI], AX
        Add     EDI, 2
        Mov     CH, AH
        Dec     EBP
        Jnz     @@Loop_
        Jmp     Short @@Done
ApZmodem@AsmEscapeBlock EndP

CODE32  ENDS

END
