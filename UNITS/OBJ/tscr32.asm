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

        PUBLIC  TScript@GetParams

; Function GetParams (Const S: String): String
        GetP_Result     EQU     DWord Ptr [ESP + 20]
        GetP_Str        EQU     DWord Ptr [ESP + 16]
        Size_Params     =       4

        ALIGN   4
TScript@GetParams       PROC    NEAR
        Push    EBX
        Push    ESI
        Push    EDI
        Mov     ESI, GetP_Str
        Xor     ECX, ECX
        Mov     EDI, GetP_Result
        Mov     CL, [ESI]
        Inc     ESI
        Test    CL, CL
        Jz      @@EmptyStr
  @@FindFirstBracket:
        Mov     AL, [ESI]
        Inc     ESI
        Cmp     AL, '('
        Jz      @@GotFirstBracket
        Dec     ECX
        Jnz     @@FindFirstBracket
  @@EmptyStr:
        Mov     Byte Ptr [EDI], 0
        Jmp     Short @@Exit
  @@GotFirstBracket:
        Dec     ECX
        Jz      @@EmptyStr
  @@SkipLeadSpaces:
        Mov     AL, [ESI]
        Inc     ESI
        Cmp     AL, 20h
        Ja      @@GotFirstChar
        Dec     ECX
        Jnz     @@SkipLeadSpaces
        Jmp     Short @@EmptyStr
  @@GotFirstChar:
        Mov     EBX, ESI
        Xor     AH, AH
        Dec     EBX
        Mov     EDX, EBX
        Jmp     Short @@SkipLoading
  @@MainLoop:
        Mov     AL, [ESI]
        Inc     ESI
  @@SkipLoading:
        Cmp     AL, ')'
        Jz      @@ProcessCloseBracket
        Cmp     AL, '('
        Jz      @@ProcessOpenBracket
        Cmp     AL, '"'
        Jz      @@ProcessQuote
        Cmp     AL, 20h
        Jna     @@ContinueMainLoop
  @@SetEndPtr:
        Mov     EDX, ESI
  @@ContinueMainLoop:
        Dec     ECX
        Jnz     @@MainLoop
        Jmp     Short @@EndReached
  @@ProcessOpenBracket:
        Inc     AH
        Jmp     Short @@SetEndPtr
  @@ProcessQuote:
        Dec     ECX
        Jz      @@QuoteSkipped
  @@SkipQuoteLoop:
        Mov     AL, [ESI]
        Inc     ESI
        Cmp     AL, '"'
        Jz      @@SetEndPtr
        Dec     ECX
        Jnz     @@SkipQuoteLoop
  @@QuoteSkipped:
        Mov     EDX, ESI
        Jmp     Short @@EndReached
  @@ProcessCloseBracket:
        Dec     AH
        Jns     @@SetEndPtr
  @@EndReached:
        Sub     EDX, EBX
        Mov     ESI, EBX
        Mov     [EDI], DL
        Mov     ECX, EDX
        Inc     EDI
        Shr     EDX, 2
        And     ECX, 3
        CLD
        Rep     MovSB
        Mov     ECX, EDX
        Rep     MovSD
  @@Exit:
        Pop     EDI
        Pop     ESI
        Pop     EBX
        Ret     Size_Params
TScript@GetParams       EndP

CODE32  ENDS

END
