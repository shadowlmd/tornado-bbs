;                BM_TableSize            EQU     256
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
;        PosBM_Table     DB      BM_TableSize Dup (?)
BSS32           ENDS

DGROUP          GROUP CONST32, DATA32, TLS, BSS32
                .MODEL FLAT

CODE32          SEGMENT
                ASSUME CS:FLAT, DS:FLAT, SS:FLAT, ES:FLAT

        PUBLIC  TMisc@PadCh, TMisc@Pad, TMisc@LeftPadCh, TMisc@LeftPad
        PUBLIC  TMisc@TrimLead, TMisc@TrimTrail, TMisc@Trim, TMisc@CenterCh
        PUBLIC  TMisc@Replicate, TMisc@UpCase, TMisc@LoCase, TMisc@UpString
        PUBLIC  TMisc@LoString, TMisc@EngLoString, TMisc@WordCount
        PUBLIC  TMisc@WordPosition, TMisc@ExtractWord, TMisc@AsciiCount
        PUBLIC  TMisc@AsciiPosition, TMisc@ExtractAscii, TMisc@AsciiPosCh
        PUBLIC  TMisc@AsciiPos, TMisc@ConsistsOf, TMisc@StrContains
        PUBLIC  TMisc@DelChars, TMisc@Str2Set, TMisc@StrCompare
        PUBLIC  TMisc@SetString, TMisc@FlagsValid, TMisc@MaskMatch
        PUBLIC  TMisc@XlatStr, TMisc@HexB, TMisc@HexW, TMisc@HexL
        PUBLIC  TMisc@FillLong, TMisc@Hex2Byte, TMisc@Hex2Word, TMisc@Hex2Long
        PUBLIC  TMisc@NPos

UpTable DB 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15
        DB 16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31
        DB 32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47
        DB 48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63
        DB 64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79
        DB 80,81,82,83,84,85,86,87,88,89,90,91,92,93,94,95
        DB 96,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79
        DB 80,81,82,83,84,85,86,87,88,89,90,123,124,125,126,127
        DB 128,129,130,131,132,133,134,135,136,137,138,139,140,141,142,143
        DB 144,145,146,147,148,149,150,151,152,153,154,155,156,157,158,159
        DB 128,129,130,131,132,133,134,135,136,137,138,139,140,141,142,143
        DB 176,177,178,179,180,181,182,183,184,185,186,187,188,189,190,191
        DB 192,193,194,195,196,197,198,199,200,201,202,203,204,205,206,207
        DB 208,209,210,211,212,213,214,215,216,217,218,219,220,221,222,223
        DB 144,145,146,147,148,149,150,151,152,153,154,155,156,157,158,159
        DB 240,240,242,243,244,245,246,247,248,249,250,251,252,253,254,255

LoTable DB 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15
        DB 16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31
        DB 32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47
        DB 48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63
        DB 64,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111
        DB 112,113,114,115,116,117,118,119,120,121,122,91,92,93,94,95
        DB 96,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111
        DB 112,113,114,115,116,117,118,119,120,121,122,123,124,125,126,127
        DB 160,161,162,163,164,165,166,167,168,169,170,171,172,173,174,175
        DB 224,225,226,227,228,229,230,231,232,233,234,235,236,237,238,239
        DB 160,161,162,163,164,165,166,167,168,169,170,171,172,173,174,175
        DB 176,177,178,179,180,181,182,183,184,185,186,187,188,189,190,191
        DB 192,193,194,195,196,197,198,199,200,201,202,203,204,205,206,207
        DB 208,209,210,211,212,213,214,215,216,217,218,219,220,221,222,223
        DB 224,225,226,227,228,229,230,231,232,233,234,235,236,237,238,239
        DB 241,241,242,243,244,245,246,247,248,249,250,251,252,253,254,255

EngLoTable      DB 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15
        DB 16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31
        DB 32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47
        DB 48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63
        DB 64,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111
        DB 112,113,114,115,116,117,118,119,120,121,122,91,92,93,94,95
        DB 96,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111
        DB 112,113,114,115,116,117,118,119,120,121,122,123,124,125,126,127
        DB 128,129,130,131,132,133,134,135,136,137,138,139,140,141,142,143
        DB 144,145,146,147,148,149,150,151,152,153,154,155,156,157,158,159
        DB 160,161,162,163,164,165,166,167,168,169,170,171,172,173,174,175
        DB 176,177,178,179,180,181,182,183,184,185,186,187,188,189,190,191
        DB 192,193,194,195,196,197,198,199,200,201,202,203,204,205,206,207
        DB 208,209,210,211,212,213,214,215,216,217,218,219,220,221,222,223
        DB 224,225,226,227,228,229,230,231,232,233,234,235,236,237,238,239
        DB 240,241,242,243,244,245,246,247,248,249,250,251,252,253,254,255

Hex2LongTable   DB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
        DB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
        DB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
        DB 0,1,2,3,4,5,6,7,8,9,0,0,0,0,0,0
        DB 0,10,11,12,13,14,15,0,0,0,0,0,0,0,0,0
        DB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
        DB 0,10,11,12,13,14,15,0,0,0,0,0,0,0,0,0
        DB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
        DB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
        DB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
        DB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
        DB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
        DB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
        DB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
        DB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
        DB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0

; Function PadCh (S: String; Chr: Char; Len: Byte): String;
        PadCh_Result    EQU     DWord Ptr [ESP + 28]
        PadCh_Str       EQU     DWord Ptr [ESP + 24]
        PadCh_Chr       EQU     Byte Ptr  [ESP + 20]
        PadCh_Len       EQU     Byte Ptr  [ESP + 16]
        Size_Params     =       12

        ALIGN   4
TMisc@PadCh     PROC    NEAR
        Push    EBX
        Push    ESI
        Push    EDI
        Mov     ESI, PadCh_Str
        Xor     EAX, EAX
        Mov     EDI, PadCh_Result
        Xor     EBX, EBX
        Mov     DL, PadCh_Chr
        Mov     BL, PadCh_Len
        Mov     AL, [ESI]
        Mov     [EDI], BL
        Inc     ESI
        Inc     EDI
        Cmp     EAX, EBX
        Jna     @@StrLess
        Mov     EAX, EBX
  @@StrLess:
        Mov     ECX, EAX
        Mov     DH, DL
        And     ECX, 3
        CLD
        Rep     MovSB
        Mov     ECX, EAX
        Shr     ECX, 2
        Rep     MovSD
        Sub     EBX, EAX
        Jz      @@SkipFill
        Mov     EAX, EDX
        Mov     ECX, EBX
        Shl     EAX, 16
        And     EBX, 3
        Mov     AX, DX
        Shr     ECX, 2
        Rep     StoSD
        Mov     ECX, EBX
        Rep     StoSB
  @@SkipFill:
        Pop     EDI
        Pop     ESI
        Pop     EBX
        Ret     Size_Params
TMisc@PadCh     EndP

; Function Pad (S: String; Len: Byte): String;
        Pad_Result      EQU     DWord Ptr [ESP + 24]
        Pad_Str         EQU     DWord Ptr [ESP + 20]
        Pad_Len         EQU     Byte Ptr  [ESP + 16]
        Size_Params     =       8

        ALIGN   4
TMisc@Pad       PROC    NEAR
        Push    EBX
        Push    ESI
        Push    EDI
        Mov     ESI, Pad_Str
        Xor     EAX, EAX
        Mov     EDI, Pad_Result
        Xor     EBX, EBX
        Mov     AL, [ESI]
        Mov     BL, Pad_Len
        Inc     ESI
        Mov     [EDI], BL
        Inc     EDI
        Cmp     EAX, EBX
        Jna     @@StrLess
        Mov     EAX, EBX
  @@StrLess:
        Mov     ECX, EAX
        And     ECX, 3
        CLD
        Rep     MovSB
        Mov     ECX, EAX
        Shr     ECX, 2
        Rep     MovSD
        Sub     EBX, EAX
        Jz      @@SkipFill
        Mov     ECX, EBX
        Mov     EAX, 20202020h
        And     EBX, 3
        Shr     ECX, 2
        Rep     StoSD
        Mov     ECX, EBX
        Rep     StoSB
  @@SkipFill:
        Pop     EDI
        Pop     ESI
        Pop     EBX
        Ret     Size_Params
TMisc@Pad       EndP

; Function LeftPadCh (S: String; Chr: Char; Len: Byte): String;
        LPadCh_Result   EQU     DWord Ptr [ESP + 28]
        LPadCh_Str      EQU     DWord Ptr [ESP + 24]
        LPadCh_Chr      EQU     Byte Ptr  [ESP + 20]
        LPadCh_Len      EQU     Byte Ptr  [ESP + 16]
        Size_Params     =       12

        ALIGN   4
TMisc@LeftPadCh PROC    NEAR
        Push    EBX
        Push    ESI
        Push    EDI
        Mov     ESI, LPadCh_Str
        Xor     ECX, ECX
        Mov     EDI, LPadCh_Result
        Xor     EBX, EBX
        Mov     DL, LPadCh_Chr
        CLD
        Mov     CL, LPadCh_Len
        Mov     BL, [ESI]
        Mov     DH, DL
        Inc     ESI
        Mov     [EDI], CL
        Inc     EDI
        Cmp     EBX, ECX
        Jna     @@StrLess
        Mov     EBX, ECX
  @@StrLess:
        Sub     ECX, EBX
        Jz      @@SkipFill
        Mov     EAX, EDX
        Shl     EAX, 16
        Mov     AX, DX
        Mov     EDX, ECX
        And     ECX, 3
        Shr     EDX, 2
        Rep     StoSB
        Mov     ECX, EDX
        Rep     StoSD
  @@SkipFill:
        Mov     ECX, EBX
        And     EBX, 3
        Shr     ECX, 2
        Rep     MovSD
        Mov     ECX, EBX
        Rep     MovSB
        Pop     EDI
        Pop     ESI
        Pop     EBX
        Ret     Size_Params
TMisc@LeftPadCh EndP

; Function LeftPad (S: String; Len: Byte): String;
        LPad_Result     EQU     DWord Ptr [ESP + 24]
        LPad_Str        EQU     DWord Ptr [ESP + 20]
        LPad_Len        EQU     Byte Ptr [ESP + 16]
        Size_Params     =       8

        ALIGN   4
TMisc@LeftPad   PROC    NEAR
        Push    EBX
        Push    ESI
        Push    EDI
        Mov     ESI, LPad_Str
        Xor     ECX, ECX
        Mov     EDI, LPad_Result
        Xor     EBX, EBX
        CLD
        Mov     CL, LPad_Len
        Mov     BL, [ESI]
        Inc     ESI
        Mov     [EDI], CL
        Inc     EDI
        Cmp     EBX, ECX
        Jna     @@StrLess
        Mov     EBX, ECX
  @@StrLess:
        Sub     ECX, EBX
        Jz      @@SkipFill
        Mov     EDX, ECX
        Mov     EAX, 20202020h
        And     ECX, 3
        Shr     EDX, 2
        Rep     StoSB
        Mov     ECX, EDX
        Rep     StoSD
  @@SkipFill:
        Mov     ECX, EBX
        And     EBX, 3
        Shr     ECX, 2
        Rep     MovSD
        Mov     ECX, EBX
        Rep     MovSB
        Pop     EDI
        Pop     ESI
        Pop     EBX
        Ret     Size_Params
TMisc@LeftPad   EndP

; Function TrimLead (S: String): String;
        TrimL_Result    EQU     DWord Ptr [ESP + 16]
        TrimL_Str       EQU     DWord Ptr [ESP + 12]
        Size_Params     =       4

        ALIGN   4
TMisc@TrimLead  PROC    NEAR
        Push    ESI
        Push    EDI
        Mov     ESI, TrimL_Str
        Xor     ECX, ECX
        Mov     EDI, TrimL_Result
        Mov     CL, [ESI]
        Inc     ESI
        Test    CL, CL
        Jz      @@ZeroResult
  @@Loop:
        Mov     AL, [ESI]
        Inc     ESI
        Cmp     AL, 20h
        Ja      @@DoMove
        Dec     ECX
        Jnz     @@Loop
  @@ZeroResult:
        Mov     [EDI], CL
        Pop     EDI
        Pop     ESI
        Ret     Size_Params
  @@DoMove:
        Mov     AH, AL
        Mov     AL, CL
        Dec     ECX
        Mov     [EDI], AX
        Add     EDI, 2
        Mov     EAX, ECX
        And     ECX, 3
        Shr     EAX, 2
        CLD
        Rep     MovSB
        Mov     ECX, EAX
        Rep     MovSD
        Pop     EDI
        Pop     ESI
        Ret     Size_Params
TMisc@TrimLead  EndP

; Function TrimTrail (S: String): String;
        TrimR_Result    EQU     DWord Ptr [ESP + 16]
        TrimR_Str       EQU     DWord Ptr [ESP + 12]
        Size_Params     =       4

        ALIGN   4
TMisc@TrimTrail PROC    NEAR
        Push    ESI
        Push    EDI
        Mov     ESI, TrimR_Str
        Xor     ECX, ECX
        Mov     EDI, TrimR_Result
        Mov     CL, [ESI]
        Test    CL, CL
        Jz      @@ZeroResult
        Add     ESI, ECX
  @@Loop:
        Cmp     Byte Ptr [ESI], 20h
        Ja      @@DoMove
        Dec     ESI
        Dec     ECX
        Jnz     @@Loop
  @@ZeroResult:
        Mov     [EDI], CL
        Pop     EDI
        Pop     ESI
        Ret     Size_Params
  @@DoMove:
        Mov     [EDI], CL
        Mov     EAX, ECX
        Inc     EDI
        Sub     ESI, ECX
        Shr     EAX, 2
        Inc     ESI
        And     ECX, 3
        CLD
        Rep     MovSB
        Mov     ECX, EAX
        Rep     MovSD
        Pop     EDI
        Pop     ESI
        Ret     Size_Params
TMisc@TrimTrail EndP

; Function Trim (S: String): String;
        Trim_Result     EQU     DWord Ptr [ESP + 16]
        Trim_Str        EQU     DWord Ptr [ESP + 12]
        Size_Params     =       4

        ALIGN   4
TMisc@Trim      PROC    NEAR
        Push    ESI
        Push    EDI
        Mov     ESI, Trim_Str
        Xor     ECX, ECX
        Mov     EDI, Trim_Result
        Mov     CL, [ESI]
        Test    CL, CL
        Jz      @@ZeroResult
        Add     ESI, ECX
        Mov     AH, 20h
  @@LoopTrail:
        Cmp     [ESI], AH
        Jnz     @@CheckLead
        Dec     ESI
        Dec     ECX
        Jnz     @@LoopTrail
  @@ZeroResult:
        Mov     [EDI], CL
        Pop     EDI
        Pop     ESI
        Ret     Size_Params
  @@CheckLead:
        Sub     ESI, ECX
        Inc     ESI
  @@LoopLead:
        Mov     AL, [ESI]
        Inc     ESI
        Cmp     AL, AH
        Jnz     @@DoMove
        Dec     ECX
        Jnz     @@LoopLead
  @@DoMove:
        Mov     AH, AL
        Mov     AL, CL
        Dec     ECX
        Mov     [EDI], AX
        Add     EDI, 2
        Mov     EAX, ECX
        And     ECX, 3
        Shr     EAX, 2
        CLD
        Rep     MovSB
        Mov     ECX, EAX
        Rep     MovSD
        Pop     EDI
        Pop     ESI
        Ret     Size_Params
TMisc@Trim      EndP

; Function CenterCh (S: String; Chr: Char; Width: Byte): String;
        Center_Result   EQU     DWord Ptr [ESP + 28]
        Center_Str      EQU     DWord Ptr [ESP + 24]
        Center_Chr      EQU     Byte Ptr [ESP + 20]
        Center_Len      EQU     Byte Ptr [ESP + 16]
        Size_Params     =       12

        ALIGN   4
TMisc@CenterCh  PROC    NEAR
        Push    EBX
        Push    ESI
        Push    EDI
        Mov     ESI, Center_Str
        Xor     EDX, EDX
        Mov     EDI, Center_Result
        Xor     EBX, EBX
        Mov     DL, Center_Len
        Mov     BL, [ESI]
        CLD
        Cmp     EBX, EDX
        Jl      @@DoCenter
        Inc     EBX
        Mov     ECX, EBX
        And     EBX, 3
        Shr     ECX, 2
        Rep     MovSD
        Mov     ECX, EBX
        Rep     MovSB
        Jmp     Short @@Exit
  @@DoCenter:
        Mov     [EDI], DL
        Mov     AL, Center_Chr
        Inc     EDI
        Mov     AH, AL
        Mov     ECX, EAX
        Shl     EAX, 16
        Mov     AX, CX
        Mov     ECX, EDX
        And     ECX, 3
        Rep     StoSB
        Mov     ECX, EDX
        Shr     ECX, 2
        Rep     StoSD
        Sub     EDI, EDX
        Mov     ECX, EBX
        Sub     EDX, EBX
        Inc     ESI
        Shr     EDX, 1
        And     EBX, 3
        Shr     ECX, 2
        Add     EDI, EDX
        Rep     MovSD
        Mov     ECX, EBX
        Rep     MovSB
  @@Exit:
        Pop     EDI
        Pop     ESI
        Pop     EBX
        Ret     Size_Params
TMisc@CenterCh  EndP

; Function Replicate (Ch: Char; Len: Integer): String
        Repl_Result     EQU     DWord Ptr [ESP + 16]
        Repl_Ch         EQU     Byte Ptr  [ESP + 12]
        Repl_Len        EQU     DWord Ptr [ESP + 8]
        Size_Params     =       8

        ALIGN   4
TMisc@Replicate PROC    NEAR
        Push    EDI
        Mov     EAX, Repl_Len
        Mov     EDI, Repl_Result
        Test    EAX, EAX
        Jng     @@ZeroResult
        Cmp     EAX, 255
        Jb      @@ValidRange
        Mov     EAX, 255
  @@ValidRange:
        Mov     ECX, EAX
        Mov     AH, Repl_CH
        Mov     [EDI], AX
        Add     EDI, 2
        Mov     AL, AH
        Dec     ECX
        Mov     EDX, EAX
        Shl     EAX, 16
        Mov     AX, DX
        Mov     EDX, ECX
        And     ECX, 3
        Shr     EDX, 2
        CLD
        Rep     StoSB
        Mov     ECX, EDX
        Rep     StoSD
        Pop     EDI
        Ret     Size_Params
  @@ZeroResult:
        Mov     Byte Ptr [EDI], 0
        Pop     EDI
        Ret     Size_Params
TMisc@Replicate EndP

; Function UpCase (C: Char): Char
        UpCase_CH       EQU     Byte Ptr [ESP + 4]
        Size_Params     =       4

        ALIGN   4
TMisc@UpCase    PROC    NEAR
        MovZX   EAX, UpCase_CH
        Mov     AL, UpTable + EAX
        Ret     Size_Params
TMisc@UpCase    EndP

; Function LoCase (C: Char): Char
        LoCase_CH       EQU     Byte Ptr [ESP + 4]
        Size_Params     =       4

        ALIGN   4
TMisc@LoCase    PROC    NEAR
        MovZX   EAX, LoCase_CH
        Mov     AL, LoTable + EAX
        Ret     Size_Params
TMisc@LoCase    EndP

; Function UpString (S: String): String
        UpStr_Result    EQU     DWord Ptr [ESP + 16]
        UpStr_Str       EQU     DWord Ptr [ESP + 12]
        Size_Params     =       4

        ALIGN   4
TMisc@UpString  PROC    NEAR
        Push    ESI
        Push    EDI
        Mov     ESI, UpStr_Str
        Xor     EAX, EAX
        Mov     EDI, UpStr_Result
        Mov     AL, [ESI]
        Lea     EDX, UpTable
        Mov     [EDI], AL
        Test    AL, AL
        Jz      @@Exit
        Push    EBX
        Inc     ESI
        Mov     ECX, EAX
        Inc     EDI
        Xor     EBX, EBX
        And     EAX, 3
        Jz      @@CheckX4
  @@LoopX1:
        Mov     BL, [ESI]
        Inc     ESI
        Mov     BL, [EDX + EBX]
        Mov     [EDI], BL
        Inc     EDI
        Dec     EAX
        Jnz     @@LoopX1
  @@CheckX4:
        Shr     ECX, 2
        Jz      @@Done
  @@LoopX4:
        Mov     EAX, [ESI]
        Add     ESI, 4
        Mov     BL, AL
        Mov     AL, [EDX + EBX]
        Mov     BL, AH
        Mov     AH, [EDX + EBX]
        Rol     EAX, 16
        Mov     BL, AL
        Mov     AL, [EDX + EBX]
        Mov     BL, AH
        Mov     AH, [EDX + EBX]
        Rol     EAX, 16
        Mov     [EDI], EAX
        Add     EDI, 4
        Dec     ECX
        Jnz     @@LoopX4
  @@Done:
        Pop     EBX
  @@Exit:
        Pop     EDI
        Pop     ESI
        Ret     Size_Params
TMisc@UpString  EndP

; Function LoString (S: String): String
        LoStr_Result    EQU     DWord Ptr [ESP + 16]
        LoStr_Str       EQU     DWord Ptr [ESP + 12]
        Size_Params     =       4

        ALIGN   4
TMisc@LoString  PROC    NEAR
        Push    ESI
        Push    EDI
        Mov     ESI, LoStr_Str
        Xor     EAX, EAX
        Mov     EDI, LoStr_Result
        Mov     AL, [ESI]
        Lea     EDX, LoTable
        Mov     [EDI], AL
        Test    AL, AL
        Jz      @@Exit
        Push    EBX
        Inc     ESI
        Mov     ECX, EAX
        Inc     EDI
        Xor     EBX, EBX
        And     EAX, 3
        Jz      @@CheckX4
  @@LoopX1:
        Mov     BL, [ESI]
        Inc     ESI
        Mov     BL, [EDX + EBX]
        Mov     [EDI], BL
        Inc     EDI
        Dec     EAX
        Jnz     @@LoopX1
  @@CheckX4:
        Shr     ECX, 2
        Jz      @@Done
  @@LoopX4:
        Mov     EAX, [ESI]
        Add     ESI, 4
        Mov     BL, AL
        Mov     AL, [EDX + EBX]
        Mov     BL, AH
        Mov     AH, [EDX + EBX]
        Rol     EAX, 16
        Mov     BL, AL
        Mov     AL, [EDX + EBX]
        Mov     BL, AH
        Mov     AH, [EDX + EBX]
        Rol     EAX, 16
        Mov     [EDI], EAX
        Add     EDI, 4
        Dec     ECX
        Jnz     @@LoopX4
  @@Done:
        Pop     EBX
  @@Exit:
        Pop     EDI
        Pop     ESI
        Ret     Size_Params
TMisc@LoString  EndP

; Function EngLoString (S: String): String
        ELoStr_Result   EQU     DWord Ptr [ESP + 16]
        ELoStr_Str      EQU     DWord Ptr [ESP + 12]
        Size_Params     =       4

        ALIGN   4
TMisc@EngLoString       PROC    NEAR
        Push    ESI
        Push    EDI
        Mov     ESI, ELoStr_Str
        Xor     EAX, EAX
        Mov     EDI, ELoStr_Result
        Mov     AL, [ESI]
        Lea     EDX, EngLoTable
        Mov     [EDI], AL
        Test    AL, AL
        Jz      @@Exit
        Push    EBX
        Inc     ESI
        Mov     ECX, EAX
        Inc     EDI
        Xor     EBX, EBX
        And     EAX, 3
        Jz      @@CheckX4
  @@LoopX1:
        Mov     BL, [ESI]
        Inc     ESI
        Mov     BL, [EDX + EBX]
        Mov     [EDI], BL
        Inc     EDI
        Dec     EAX
        Jnz     @@LoopX1
  @@CheckX4:
        Shr     ECX, 2
        Jz      @@Done
  @@LoopX4:
        Mov     EAX, [ESI]
        Add     ESI, 4
        Mov     BL, AL
        Mov     AL, [EDX + EBX]
        Mov     BL, AH
        Mov     AH, [EDX + EBX]
        Rol     EAX, 16
        Mov     BL, AL
        Mov     AL, [EDX + EBX]
        Mov     BL, AH
        Mov     AH, [EDX + EBX]
        Rol     EAX, 16
        Mov     [EDI], EAX
        Add     EDI, 4
        Dec     ECX
        Jnz     @@LoopX4
  @@Done:
        Pop     EBX
  @@Exit:
        Pop     EDI
        Pop     ESI
        Ret     Size_Params
TMisc@EngLoString       EndP

; Function WordCount (S: String; WordDelims: CharSet): Byte
        WordC_Str       EQU     DWord Ptr [ESP + 16]
        WordC_Delims    EQU     DWord Ptr [ESP + 12]
        Size_Params     =       8

        ALIGN   4
TMisc@WordCount PROC    NEAR
        Push    ESI
        Push    EDI
        Mov     ESI, WordC_Str
        Xor     EAX, EAX
        Mov     EDI, WordC_Delims
        Xor     EDX, EDX
        Mov     AH, [ESI]
        Inc     ESI
        Test    AH, AH
        Jz      @@Exit
  @@SkipDelimsLoop:
        Mov     DL, [ESI]
        Inc     ESI
        Mov     CL, DL
        Shr     DL, 3
        And     CL, 7
        Mov     DL, [EDI + EDX]
        Inc     CL
        Shr     DL, CL
        Jnc     @@WordBegins
  @@SkipDelimAgain:
        Dec     AH
        Jnz     @@SkipDelimsLoop
        Pop     EDI
        Pop     ESI
        Ret     Size_Params
  @@WordBegins:
        Inc     AL
        Dec     AH
        Jz      @@Exit
  @@SkipWordLoop:
        Mov     DL, [ESI]
        Inc     ESI
        Mov     CL, DL
        Shr     DL, 3
        And     CL, 7
        Mov     DL, [EDI + EDX]
        Inc     CL
        Shr     DL, CL
        Jc      @@SkipDelimAgain
        Dec     AH
        Jnz     @@SkipWordLoop
  @@Exit:
        Pop     EDI
        Pop     ESI
        Ret     Size_Params
TMisc@WordCount EndP

; Function WordPosition (N: Byte; S: String; WordDelims: CharSet): Byte
        WordP_Num       EQU     Byte Ptr  [ESP + 24]
        WordP_Str       EQU     DWord Ptr [ESP + 20]
        WordP_Delims    EQU     DWord Ptr [ESP + 16]
        Size_Params     =       12

        ALIGN   4
TMisc@WordPosition      PROC    NEAR
        Push    EBX
        Push    ESI
        Push    EDI
        Mov     ESI, WordP_Str
        Xor     EAX, EAX
        Mov     DL, WordP_Num
        Mov     AL, [ESI]
        Inc     ESI
        Test    AL, AL
        Jz      @@Exit
        Test    DL, DL
        Jz      @@NotFound
        Xor     EBX, EBX
        Mov     EDI, WordP_Delims
        Mov     CH, AL
  @@SkipDelimsLoop:
        Mov     BL, [ESI]
        Inc     ESI
        Mov     CL, BL
        Shr     BL, 3
        And     CL, 7
        Mov     BL, [EDI + EBX]
        Inc     CL
        Shr     BL, CL
        Jnc     @@WordBegins
  @@SkipDelimAgain:
        Dec     CH
        Jnz     @@SkipDelimsLoop
        Pop     EDI
        Pop     ESI
        Pop     EBX
        Xor     EAX, EAX
        Ret     Size_Params
  @@WordBegins:
        Inc     AH
        Cmp     AH, DL
        Jnz     @@SkipWordAgain
        Pop     EDI
        Sub     AL, CH
        Pop     ESI
        And     EAX, 0FFh
        Pop     EBX
        Inc     EAX
        Ret     Size_Params
  @@SkipWordLoop:
        Mov     BL, [ESI]
        Inc     ESI
        Mov     CL, BL
        Shr     BL, 3
        And     CL, 7
        Mov     BL, [EDI + EBX]
        Inc     CL
        Shr     BL, CL
        Jc      @@SkipDelimAgain
  @@SkipWordAgain:
        Dec     CH
        Jnz     @@SkipWordLoop
  @@NotFound:
        Xor     EAX, EAX
  @@Exit:
        Pop     EDI
        Pop     ESI
        Pop     EBX
        Ret     Size_Params
TMisc@WordPosition      EndP

; Function ExtractWord (N: Byte; S: String; WordDelims: CharSet): String
        WordE_Result    EQU     DWord Ptr [ESP + 28]
        WordE_Num       EQU     Byte Ptr  [ESP + 24]
        WordE_Str       EQU     DWord Ptr [ESP + 20]
        WordE_Delims    EQU     DWord Ptr [ESP + 16]
        Size_Params     =       12

        ALIGN   4
TMisc@ExtractWord       PROC    NEAR
        Push    EBX
        Push    ESI
        Push    EDI
        Mov     ESI, WordE_Str
        Xor     EAX, EAX
        Mov     CH, WordE_Num
        Mov     AL, [ESI]
        Inc     ESI
        Test    AL, AL
        Jz      @@NotFound
        Test    CH, CH
        Jz      @@NotFound
        Xor     EBX, EBX
        Mov     EDI, WordE_Delims
  @@SkipDelimsLoop:
        Mov     BL, [ESI]
        Inc     ESI
        Mov     CL, BL
        Shr     BL, 3
        And     CL, 7
        Mov     BL, [EDI + EBX]
        Inc     CL
        Shr     BL, CL
        Jnc     @@WordBegins
  @@SkipDelimAgain:
        Dec     AL
        Jnz     @@SkipDelimsLoop
        Jmp     Short @@NotFound
  @@WordBegins:
        Inc     AH
        Cmp     AH, CH
        Jnz     @@SkipWordAgain
        Mov     EDX, ESI
        Mov     AH, AL
        Dec     EDX
        Dec     AH
        Jz      @@MoveFound
  @@ScanFoundLoop:
        Mov     BL, [ESI]
        Inc     ESI
        Mov     CL, BL
        Shr     BL, 3
        And     CL, 7
        Mov     BL, [EDI + EBX]
        Inc     CL
        Shr     BL, CL
        Jc      @@MoveFound
        Dec     AH
        Jnz     @@ScanFoundLoop
  @@MoveFound:
        Sub     AL, AH
        Mov     EDI, WordE_Result
        And     EAX, 0FFh
        Mov     ESI, EDX
        Mov     [EDI], AL
        Mov     ECX, EAX
        Inc     EDI
        Shr     EAX, 2
        And     ECX, 3
        CLD
        Rep     MovSB
        Mov     ECX, EAX
        Rep     MovSD
        Pop     EDI
        Pop     ESI
        Pop     EBX
        Ret     Size_Params
  @@SkipWordLoop:
        Mov     BL, [ESI]
        Inc     ESI
        Mov     CL, BL
        Shr     BL, 3
        And     CL, 7
        Mov     BL, [EDI + EBX]
        Inc     CL
        Shr     BL, CL
        Jc      @@SkipDelimAgain
  @@SkipWordAgain:
        Dec     AL
        Jnz     @@SkipWordLoop
  @@NotFound:
        Mov     EAX, WordE_Result
        Pop     EDI
        Pop     ESI
        Pop     EBX
        Mov     Byte Ptr [EAX], 0
        Ret     Size_Params
TMisc@ExtractWord       EndP

; Function AsciiCount (S: String; WordDelims: CharSet; Quote: Char): Byte
        AsciiC_Str      EQU     DWord Ptr [ESP + 20]
        AsciiC_Delims   EQU     DWord Ptr [ESP + 16]
        AsciiC_Quote    EQU     Byte Ptr  [ESP + 12]
        Size_Params     =       12

        ALIGN   4
TMisc@AsciiCount        PROC    NEAR
        Push    ESI
        Push    EDI
        Mov     ESI, AsciiC_Str
        Xor     EAX, EAX
        Mov     EDI, AsciiC_Delims
        Mov     AH, [ESI]
        Inc     ESI
        Test    AH, AH
        Jz      @@Exit
        Xor     EDX, EDX
        Mov     CH, AsciiC_Quote
        Mov     DL, [ESI]
        Mov     CL, DL
        Shr     DL, 3
        And     CL, 7
        Mov     DL, [EDI + EDX]
        Inc     CL
        Shr     DL, CL
        Jnc     @@WordBegins
        Inc     AL
        Inc     ESI
        Dec     AH
        Jz      @@Exit
  @@SkipDelimsLoop:
        Mov     DL, [ESI]
        Mov     CL, DL
        Shr     DL, 3
        And     CL, 7
        Mov     DL, [EDI + EDX]
        Inc     CL
        Shr     DL, CL
        Jnc     @@WordBegins
        Inc     ESI
  @@SkipDelimAgain:
        Dec     AH
        Jnz     @@SkipDelimsLoop
        Pop     EDI
        Pop     ESI
        Ret     Size_Params
  @@WordBegins:
        Inc     AL
  @@SkipWordLoop:
        Mov     DL, [ESI]
        Inc     ESI
        Cmp     DL, CH
        Jz      @@SkipQuote
        Mov     CL, DL
        Shr     DL, 3
        And     CL, 7
        Mov     DL, [EDI + EDX]
        Inc     CL
        Shr     DL, CL
        Jc      @@SkipDelimAgain
  @@SkipWordAgain:
        Dec     AH
        Jnz     @@SkipWordLoop
  @@Exit:
        Pop     EDI
        Pop     ESI
        Ret     Size_Params
  @@SkipQuote:
        Dec     AH
        Jz      @@Exit
  @@SkipQuoteLoop:
        Mov     DL, [ESI]
        Inc     ESI
        Cmp     DL, CH
        Jz      @@SkipWordAgain
        Dec     AH
        Jnz     @@SkipQuoteLoop
        Jmp     Short @@Exit
TMisc@AsciiCount        EndP

; Function AsciiPosition (N: Byte; S: String; WordDelims: CharSet; Quote: Char): Byte
        AsciiP_Num      EQU     Byte Ptr  [ESP + 28]
        AsciiP_Str      EQU     DWord Ptr [ESP + 24]
        AsciiP_Delims   EQU     DWord Ptr [ESP + 20]
        AsciiP_Quote    EQU     Byte Ptr  [ESP + 16]
        Size_Params     =       16

        ALIGN   4
TMisc@AsciiPosition     PROC    NEAR
        Push    EBX
        Push    ESI
        Push    EDI
        Mov     ESI, AsciiP_Str
        Xor     EAX, EAX
        Mov     DH, AsciiP_Num
        Xor     EBX, EBX
        Mov     AL, [ESI]
        Inc     ESI
        Test    AL, AL
        Jz      @@Exit
        Test    DH, DH
        Jz      @@Exit
        Mov     BL, [ESI]
        Mov     CH, AL
        Mov     EDI, AsciiP_Delims
        Mov     CL, BL
        Shr     BL, 3
        Mov     DL, AsciiP_Quote
        And     CL, 7
        Mov     BL, [EDI + EBX]
        Inc     CL
        Shr     BL, CL
        Jnc     @@WordBegins
        Inc     AH
        Inc     ESI
        Dec     AL
        Jz      @@Exit
  @@SkipDelimsLoop:
        Mov     BL, [ESI]
        Mov     CL, BL
        Shr     BL, 3
        And     CL, 7
        Mov     BL, [EDI + EBX]
        Inc     CL
        Shr     BL, CL
        Jnc     @@WordBegins
        Inc     ESI
  @@SkipDelimAgain:
        Dec     AL
        Jnz     @@SkipDelimsLoop
        Pop     EDI
        Pop     ESI
        Pop     EBX
        Xor     EAX, EAX
        Ret     Size_Params
  @@WordBegins:
        Inc     AH
        Cmp     AH, DH
        Jnz     @@SkipWordLoop
        Sub     CH, AL
        Pop     EDI
        Xor     EAX, EAX
        Pop     ESI
        Mov     AL, CH
        Pop     EBX
        Inc     EAX
        Ret     Size_Params
  @@SkipWordLoop:
        Mov     BL, [ESI]
        Inc     ESI
        Cmp     BL, DL
        Jz      @@SkipQuote
        Mov     CL, BL
        Shr     BL, 3
        And     CL, 7
        Mov     BL, [EDI + EBX]
        Inc     CL
        Shr     BL, CL
        Jc      @@SkipDelimAgain
  @@SkipWordAgain:
        Dec     AL
        Jnz     @@SkipWordLoop
  @@Exit:
        Pop     EDI
        Pop     ESI
        Pop     EBX
        Xor     EAX, EAX
        Ret     Size_Params
  @@SkipQuote:
        Dec     AL
        Jz      @@Exit
  @@SkipQuoteLoop:
        Mov     BL, [ESI]
        Inc     ESI
        Cmp     BL, DL
        Jz      @@SkipWordAgain
        Dec     AL
        Jnz     @@SkipQuoteLoop
        Jmp     Short @@Exit
TMisc@AsciiPosition     EndP

; Function ExtractAscii (N: Byte; S: String; WordDelims: CharSet; Quote: Char): String
        AsciiE_Result   EQU     DWord Ptr [EBP + 24]
        AsciiE_Num      EQU     Byte Ptr  [EBP + 20]
        AsciiE_Str      EQU     DWord Ptr [EBP + 16]
        AsciiE_Delims   EQU     DWord Ptr [EBP + 12]
        AsciiE_Quote    EQU     Byte Ptr  [EBP + 8]
        Size_Params     =       16

        ALIGN   4
TMisc@ExtractAscii      PROC    NEAR
        Push    EBP
        Mov     EBP, ESP
        Push    EBX
        Push    ESI
        Push    EDI
        Mov     ESI, AsciiE_Str
        Xor     EAX, EAX
        Mov     CH, AsciiE_Num
        Xor     EBX, EBX
        Mov     AL, [ESI]
        Inc     ESI
        Test    AL, AL
        Jz      @@Exit
        Test    CH, CH
        Jz      @@Exit
        Mov     BL, [ESI]
        Mov     EDI, AsciiE_Delims
        Mov     CL, BL
        Shr     BL, 3
        Mov     DL, AsciiE_Quote
        And     CL, 7
        Mov     BL, [EDI + EBX]
        Inc     CL
        Shr     BL, CL
        Jnc     @@WordBegins
        Inc     AH
        Inc     ESI
        Dec     AL
        Jz      @@Exit
  @@SkipDelimsLoop:
        Mov     BL, [ESI]
        Mov     CL, BL
        Shr     BL, 3
        And     CL, 7
        Mov     BL, [EDI + EBX]
        Inc     CL
        Shr     BL, CL
        Jnc     @@WordBegins
        Inc     ESI
  @@SkipDelimAgain:
        Dec     AL
        Jnz     @@SkipDelimsLoop
  @@Exit:
        Mov     EAX, AsciiE_Result
        Pop     EDI
        Pop     ESI
        Pop     EBX
        Pop     EBP
        Mov     Byte Ptr [EAX], 0
        Ret     Size_Params
  @@WordBegins:
        Inc     AH
        Cmp     AH, CH
        Jz      @@DoFound
  @@SkipWordLoop:
        Mov     BL, [ESI]
        Inc     ESI
        Cmp     BL, DL
        Jz      @@Word_SkipQuote
        Mov     CL, BL
        Shr     BL, 3
        And     CL, 7
        Mov     BL, [EDI + EBX]
        Inc     CL
        Shr     BL, CL
        Jc      @@SkipDelimAgain
  @@SkipWordAgain:
        Dec     AL
        Jnz     @@SkipWordLoop
        Jmp     Short @@Exit
  @@Word_SkipQuote:
        Dec     AL
        Jz      @@Exit
  @@Word_SkipQuoteLoop:
        Mov     BL, [ESI]
        Inc     ESI
        Cmp     BL, DL
        Jz      @@SkipWordAgain
        Dec     AL
        Jnz     @@Word_SkipQuoteLoop
        Jmp     Short @@Exit
  @@Scan_SkipQuote:
        Dec     AH
        Jz      @@MoveFound
  @@Scan_SkipQuoteLoop:
        Mov     BL, [ESI]
        Inc     ESI
        Cmp     BL, DL
        Jz      @@SkipScanAgain
        Dec     AH
        Jnz     @@Scan_SkipQuoteLoop
        Jmp     Short @@MoveFound
  @@DoFound:
        Mov     AH, AL
        Push    ESI
  @@ScanFoundLoop:
        Mov     BL, [ESI]
        Inc     ESI
        Cmp     BL, DL
        Jz      @@Scan_SkipQuote
        Mov     CL, BL
        Shr     BL, 3
        And     CL, 7
        Mov     BL, [EDI + EBX]
        Inc     CL
        Shr     BL, CL
        Jc      @@MoveFound
  @@SkipScanAgain:
        Dec     AH
        Jnz     @@ScanFoundLoop
  @@MoveFound:
        Sub     AL, AH
        Mov     EDI, AsciiE_Result
        And     EAX, 0FFh
        Pop     ESI
        Mov     [EDI], AL
        Inc     EDI
        Mov     ECX, EAX
        Shr     EAX, 2
        And     ECX, 3
        CLD
        Rep     MovSB
        Mov     ECX, EAX
        Rep     MovSD
        Pop     EDI
        Pop     ESI
        Pop     EBX
        Pop     EBP
        Ret     Size_Params
TMisc@ExtractAscii      EndP

; Function AsciiPosCh (S: String; Ch, Quote: Char): Byte
        AsciiPC_Str     EQU     DWord Ptr [ESP + 16]
        AsciiPC_Ch      EQU     Byte Ptr  [ESP + 12]
        AsciiPC_Quote   EQU     Byte Ptr  [ESP + 8]
        Size_Params     =       12

        ALIGN   4
TMisc@AsciiPosCh        PROC    NEAR
        Push    ESI
        Mov     ESI, AsciiPC_Str
        Xor     ECX, ECX
        Mov     AL, AsciiPC_Ch
        Mov     CL, [ESI]
        Inc     ESI
        Test    CL, CL
        Jz      @@Exit
        Mov     AH, AsciiPC_Quote
        Mov     EDX, EAX
        Mov     AH, CL
  @@Loop:
        Mov     AL, [ESI]
        Inc     ESI
        Cmp     AL, DL
        Jz      @@CharFound
        Cmp     AL, DH
        Jz      @@SkipQuote
  @@SkipChar:
        Dec     ECX
        Jnz     @@Loop
  @@Exit:
        Pop     ESI
        Xor     EAX, EAX
        Ret     Size_Params
  @@CharFound:
        Sub     AH, CL
        MovZX   EAX, AH
        Pop     ESI
        Inc     EAX
        Ret     Size_Params
  @@SkipQuote:
        Dec     ECX
        Jz      @@Exit
  @@QuoteLoop:
        Mov     AL, [ESI]
        Inc     ESI
        Cmp     AL, DH
        Jz      @@SkipChar
        Dec     ECX
        Jnz     @@QuoteLoop
        Jmp     Short @@Exit
TMisc@AsciiPosCh        EndP

; Function AsciiPos (S, SubStr: String; Quote: Char): Byte
        AsciiPS_Str     EQU     DWord Ptr [ESP + 28]
        AsciiPS_SubStr  EQU     DWord Ptr [ESP + 24]
        AsciiPS_Quote   EQU     Byte Ptr  [ESP + 20]
        Size_Params     =       12

        ALIGN   4
TMisc@AsciiPos  PROC    NEAR
        Push    EBP
        Push    EBX
        Push    ESI
        Push    EDI
        Mov     ESI, AsciiPS_Str
        Mov     EDI, AsciiPS_SubStr
        Mov     CL, [ESI]
        Mov     AL, [EDI]
        Inc     ESI
        Test    CL, CL
        Jz      @@Exit
        Test    AL, AL
        Jz      @@Exit
        Dec     AL
        Inc     EDI
        Mov     CH, AL
        Mov     BL, AsciiPS_Quote
        Mov     AH, [EDI]
        Mov     BH, CL
        Inc     EDI
        CLD
        Mov     EBP, EDI
  @@Loop:
        Mov     AL, [ESI]
        Inc     ESI
        Cmp     AL, BL
        Jz      @@SkipQuote
        Cmp     AL, AH
        Jz      @@CharFound
  @@SkipChar:
        Dec     CL
        Jnz     @@Loop
  @@Exit:
        Pop     EDI
        Pop     ESI
        Pop     EBX
        Pop     EBP
        Xor     EAX, EAX
        Ret     Size_Params
  @@SkipQuote:
        Dec     CL
        Jz      @@Exit
  @@QuoteLoop:
        Mov     AL, [ESI]
        Inc     ESI
        Cmp     AL, BL
        Jz      @@SkipChar
        Dec     CL
        Jnz     @@QuoteLoop
        Jmp     Short @@Exit
  @@CharFound:
        Or      CH, CH
        Jz      @@StrFound
        Cmp     CL, CH
        Jna     @@Exit
        Push    ECX
        Mov     EDX, ESI
        Mov     EDI, EBP
        MovZX   ECX, CH
        RepZ    CmpSB
        Pop     ECX
        Jz      @@StrFound
        Mov     ESI, EDX
        Jmp     Short @@SkipChar
  @@StrFound:
        Sub     BH, CL
        Xor     EAX, EAX
        Pop     EDI
        Mov     AL, BH
        Pop     ESI
        Inc     EAX
        Pop     EBX
        Pop     EBP
        Ret     Size_Params
TMisc@AsciiPos  EndP

; Function ConsistsOf (S: String; Symbols: CharSet): Boolean
        Consist_Str     EQU     DWord Ptr [ESP + 16]
        Consist_Chars   EQU     DWord Ptr [ESP + 12]
        Size_Params     =       8

        ALIGN   4
TMisc@ConsistsOf        PROC    NEAR
        Push    ESI
        Push    EDI
        Mov     ESI, Consist_Str
        Xor     EAX, EAX
        Mov     EDI, Consist_Chars
        Xor     EDX, EDX
        Mov     AL, [ESI]
        Inc     ESI
        Test    AL, AL
        Jz      @@Exit
  @@Loop:
        Mov     DL, [ESI]
        Inc     ESI
        Mov     CL, DL
        Shr     DL, 3
        And     CL, 7
        Mov     DL, [EDI + EDX]
        Inc     CL
        Shr     DL, CL
        Jnc     @@WrongChar
        Dec     EAX
        Jnz     @@Loop
        Inc     EAX
  @@Exit:
        Pop     EDI
        Pop     ESI
        Ret     Size_Params
  @@WrongChar:
        Pop     EDI
        Pop     ESI
        Xor     EAX, EAX
        Ret     Size_Params
TMisc@ConsistsOf        EndP

; Function StrContains (S: String; Symbols: CharSet): Boolean
        Contains_Str    EQU     DWord Ptr [ESP + 16]
        Contains_Chars  EQU     DWord Ptr [ESP + 12]
        Size_Params     =       8

        ALIGN   4
TMisc@StrContains       PROC    NEAR
        Push    ESI
        Push    EDI
        Mov     ESI, Contains_Str
        Xor     EAX, EAX
        Mov     EDI, Contains_Chars
        Xor     EDX, EDX
        Mov     AL, [ESI]
        Inc     ESI
        Test    AL, AL
        Jz      @@Exit
  @@Loop:
        Mov     DL, [ESI]
        Inc     ESI
        Mov     CL, DL
        Shr     DL, 3
        And     CL, 7
        Mov     DL, [EDI + EDX]
        Inc     CL
        Shr     DL, CL
        Jc      @@Contains
        Dec     EAX
        Jnz     @@Loop
  @@Exit:
        Pop     EDI
        Pop     ESI
        Ret     Size_Params
  @@Contains:
        Xor     EAX, EAX
        Pop     EDI
        Pop     ESI
        Inc     EAX
        Ret     Size_Params
TMisc@StrContains       EndP

; Function DelChars (Ch: CharSet; S: String): String
        DelCh_Result    EQU     DWord Ptr [ESP + 28]
        DelCh_Chars     EQU     DWord Ptr [ESP + 24]
        DelCh_Str       EQU     DWord Ptr [ESP + 20]
        Size_Params     =       8

        ALIGN   4
TMisc@DelChars  PROC    NEAR
        Push    EBP
        Push    EBX
        Push    ESI
        Push    EDI
        Mov     ESI, DelCh_Str
        Xor     EAX, EAX
        Xor     EBX, EBX
        Mov     AL, [ESI]
        Mov     EDX, DelCh_Result
        Inc     ESI
        Test    AL, AL
        Jz      @@ZeroStr
        Mov     EBP, EDX
        Mov     EDI, DelCh_Chars
        Inc     EDX
  @@Loop:
        Mov     BL, [ESI]
        Inc     ESI
        Mov     CL, BL
        Shr     BL, 3
        Mov     CH, CL
        And     CL, 7
        Mov     BL, [EDI + EBX]
        Inc     CL
        Shr     BL, CL
        Jc      @@SkipChar
        Mov     [EDX], CH
        Inc     EDX
  @@SkipChar:
        Dec     EAX
        Jnz     @@Loop
        Sub     EDX, EBP
        Pop     EDI
        Dec     EDX
        Pop     ESI
        Mov     [EBP], DL
        Pop     EBX
        Pop     EBP
        Ret     Size_Params
  @@ZeroStr:
        Mov     [EDX], AL
        Pop     EDI
        Pop     ESI
        Pop     EBX
        Pop     EBP
        Ret     Size_Params
TMisc@DelChars  EndP

; Procedure Str2Set (Const S: String; Var CS: CharSet)
        Str2S_Str       EQU     DWord Ptr [ESP + 16]
        Str2S_Set       EQU     DWord Ptr [ESP + 12]
        Size_Params     =       8

        ALIGN   4
TMisc@Str2Set   PROC    NEAR
        Push    ESI
        Push    EDI
        Xor     ECX, ECX
        Mov     EDI, Str2S_Set
        Xor     EAX, EAX
        Mov     ESI, Str2S_Str
        Mov     CL, 8
        Mov     EDX, EDI
        CLD
        Rep     StoSD
        Mov     CH, [ESI]
        Mov     EDI, EDX
        Inc     ESI
        Xor     EDX, EDX
        Test    CH, CH
        Jz      @@Exit
  @@Loop:
        Mov     DL, [ESI]
        Mov     AL, 1
        Mov     CL, DL
        Shr     DL, 3
        And     CL, 7
        Shl     AL, CL
        Inc     ESI
        Or      [EDI + EDX], AL
        Dec     CH
        Jnz     @@Loop
  @@Exit:
        Pop     EDI
        Pop     ESI
        Ret     Size_Params
TMisc@Str2Set   EndP

; Function StrCompare (S1, S2: String): Integer
        StrC_Str1       EQU     DWord Ptr [ESP + 16]
        StrC_Str2       EQU     DWord Ptr [ESP + 12]
        Size_Params     =       8

        ALIGN   4
TMisc@StrCompare        PROC    NEAR
        Push    ESI
        Push    EDI
        Mov     ESI, StrC_Str1
        Mov     EDI, StrC_Str2
        Xor     EAX, EAX
        Xor     EDX, EDX
        Mov     AL, [ESI]
        Mov     DL, [EDI]
        Inc     ESI
        Mov     ECX, EAX
        Inc     EDI
        Cmp     ECX, EDX
        Jna     @@LenOK
        Mov     ECX, EDX
  @@LenOK:
        CLD
        Rep     CmpSB
        Jz      @@CalcResult
        Dec     ESI
        Dec     EDI
        Mov     AL, [ESI]
        Mov     DL, [EDI]
  @@CalcResult:
        Pop     EDI
        Pop     ESI
        Sub     EAX, EDX
        Ret     Size_Params
TMisc@StrCompare        EndP

; Procedure SetString (Var Source, Dest; Count: Byte)
        SetS_Source     EQU     DWord Ptr [ESP + 20]
        SetS_Dest       EQU     DWord Ptr [ESP + 16]
        SetS_Count      EQU     Byte Ptr  [ESP + 12]
        Size_Params     =       12

        ALIGN   4
TMisc@SetString PROC    NEAR
        Xor     EAX, EAX
        Push    ESI
        Push    EDI
        Mov     AL, SetS_Count
        Mov     ESI, SetS_Source
        Mov     EDI, SetS_Dest
        Mov     ECX, EAX
        Jmp     Short @@FirstEntry
  @@AlignLoop:
        Dec     ECX
        Js      @@Exit
        Mov     AL, [ESI]
        Inc     ESI
  @@FirstEntry:
        Mov     [EDI], AL
        Inc     EDI
        Test    EDI, 3
        Jnz     @@AlignLoop
        Mov     EAX, ECX
        Shr     ECX, 2
        And     EAX, 3
        CLD
        Rep     MovSD
        Mov     ECX, EAX
        Rep     MovSB
  @@Exit:
        Pop     EDI
        Pop     ESI
        Ret     Size_Params
TMisc@SetString EndP

; Function FlagsValid (UserFlags, NeedFlags: String): Boolean
        FlagsV_UserStr  EQU     DWord Ptr [ESP + 20]
        FlagsV_NeedStr  EQU     DWord Ptr [ESP + 16]
        Size_Params     =       8

        ALIGN   4
TMisc@FlagsValid        PROC    NEAR
        Push    EBX
        Push    ESI
        Push    EDI
        Mov     ESI, FlagsV_NeedStr
        Xor     EDX, EDX
        Mov     EDI, FlagsV_UserStr
        Mov     DL, [ESI]
        Inc     ESI
        Test    DL, DL
        Jz      @@Valid
        CLD
        Mov     AH, [EDI]
        Inc     EDI
        Xor     ECX, ECX
        Mov     EBX, EDI
  @@Loop:
        Mov     AL, [ESI]
        Inc     ESI
        Cmp     AL, 'Z'
        Ja      @@CheckInverted
        Test    AH, AH
        Jz      @@NotValid
        Mov     CL, AH
        RepNZ   ScaSB
        Jnz     @@NotValid
  @@Again:
        Mov     EDI, EBX
  @@ContinueLoop:
        Dec     EDX
        Jnz     @@Loop
  @@Valid:
        Xor     EAX, EAX
        Pop     EDI
        Pop     ESI
        Pop     EBX
        Inc     EAX
        Ret     Size_Params
  @@CheckInverted:
        Test    AH, AH
        Jz      @@ContinueLoop
        Mov     CL, AH
        Sub     AL, 32
        RepNZ   ScaSB
        Jnz     @@Again
  @@NotValid:
        Pop     EDI
        Pop     ESI
        Pop     EBX
        Xor     EAX, EAX
        Ret     Size_Params
TMisc@FlagsValid        EndP

; Function MaskMatch (Var Str; Var Mask): Boolean
        MaskM_Str       EQU     DWord Ptr [ESP + 24]
        MaskM_Mask      EQU     DWord Ptr [ESP + 20]
        Size_Params     =       8

        ALIGN   4
TMisc@MaskMatch PROC   NEAR
        Push    EBP
        Push    EBX
        Push    ESI
        Push    EDI
        Xor     EBP, EBP
        Mov     EBX, MaskM_Str
        Xor     ECX, ECX
        Mov     EDX, MaskM_Mask
        Xor     EAX, EAX
        Lea     ESI, UpTable
        Xor     EDI, EDI
  @@MatchLoop:
        Mov     AL, [EDX]             ; char from mask
        Mov     CL, [EBX]             ; char from string
        Test    AL, AL
        Jz      @@CheckStr            ; end of mask
        Cmp     AL, '*'
        Jz      @@CheckAster
        Test    CL, CL
        Jz      @@NotMatch            ; mask not over, but string done
        Inc     EDX
        Inc     EBX
        Cmp     AL, '?'
        Jz      @@MatchLoop           ; ok, continue
        Mov     AL, [ESI + EAX]
        Cmp     AL, [ESI + ECX]
        Jz      @@MatchLoop
        Or      EDI, EDI
        Jnz     @@ContinueAster
  @@NotMatch:
        Pop     EDI
        Pop     ESI
        Pop     EBX
        Pop     EBP
        Xor     EAX, EAX
        Ret     Size_Params
  @@CheckStr:
        Test    CL, CL                 ; string also over?
        Jz      @@Match
        Test    EDI, EDI
        Jz      @@Exit
  @@ContinueAster:                     ; if checks after '*', try to continue
        Mov     EBX, EBP
        Mov     AL, [EDI]              ; char from mask
        Mov     EDX, EDI
        Mov     CL, [EBX]              ; char from string
        Jmp     Short @@AsterSkipPrepare
  @@Match:
        Mov     EAX, 1
  @@Exit:
        Pop     EDI
        Pop     ESI
        Pop     EBX
        Pop     EBP
        Ret     Size_Params
  @@CheckAster:
        Inc     EDX
        Mov     AL, [EDX]             ; char from mask
        Test    AL, AL
        Jz      @@Match
        Cmp     AL, '*'
        Jz      @@CheckAster
  @@AsterSkipPrepare:
        Test    CL, CL
        Jz      @@NotMatch            ; mask not over, but string done
        Inc     EBX
        Cmp     AL, '?'
        Jz      @@AsterMatch
        Mov     AL, [ESI + EAX]
  @@AsterSkipLoop:
        Cmp     AL, [ESI + ECX]
        Jz      @@AsterMatch
        Mov     CL, [EBX]             ; char from string
        Inc     EBX
        Test    CL, CL
        Jnz     @@AsterSkipLoop
        Jmp     Short @@NotMatch      ; mask not over, but string done
  @@AsterMatch:
        Mov     EDI, EDX
        Mov     EBP, EBX
        Inc     EDX
        Jmp     @@MatchLoop
TMisc@MaskMatch EndP

; Function XLatStr (S: String; Var Table): String
        XlatStr_Result  EQU     DWord Ptr [ESP + 24]
        XlatStr_Str     EQU     DWord Ptr [ESP + 20]
        XlatStr_Table   EQU     DWord Ptr [ESP + 16]
        Size_Params     =       8

        ALIGN   4
TMisc@XlatStr   PROC    NEAR
        Push    EBX
        Push    ESI
        Push    EDI
        Mov     ESI, XlatStr_Str
        Xor     EAX, EAX
        Mov     EDI, XlatStr_Result
        Xor     EBX, EBX
        Mov     AL, [ESI]
        Inc     ESI
        Mov     [EDI], AL
        Inc     EDI
        Test    AL, AL
        Jz      @@Exit
        Mov     ECX, EAX
        Mov     EDX, XlatStr_Table
        And     AL, 3
        Jz      @@StartLoopX4
  @@LoopX1:
        Mov     BL, [ESI]
        Inc     ESI
        Mov     BL, [EDX + EBX]
        Mov     [EDI], BL
        Inc     EDI
        Dec     EAX
        Jnz     @@LoopX1
  @@StartLoopX4:
        Shr     ECX, 2
        Jz      @@Exit
  @@LoopX4:
        Mov     EAX, [ESI]
        Add     ESI, 4
        Mov     BL, AL
        Mov     AL, [EDX + EBX]
        Mov     BL, AH
        Mov     AH, [EDX + EBX]
        Rol     EAX, 16
        Mov     BL, AL
        Mov     AL, [EDX + EBX]
        Mov     BL, AH
        Mov     AH, [EDX + EBX]
        Rol     EAX, 16
        Mov     [EDI], EAX
        Add     EDI, 4
        Dec     ECX
        Jnz     @@LoopX4
  @@Exit:
        Pop     EDI
        Pop     ESI
        Pop     EBX
        Ret     Size_Params
TMisc@XlatStr   EndP

; Function HexB (B: Byte): String
        HexB_Result     EQU     DWord Ptr [ESP + 8]
        HexB_Byte       EQU     Byte Ptr  [ESP + 4]
        Size_Params     =       4

        ALIGN   4
TMisc@HexB      PROC    NEAR
        Mov     AL, HexB_Byte
        Mov     AH, AL
        And     EAX, 0FF0Fh
        Add     AL, 90h
        DAA
        Adc     AL, 40h
        DAA
        Xchg    AL, AH
        Shr     AL, 4
        Add     AL, 90h
        DAA
        Adc     AL, 40h
        DAA
        Shl     EAX, 8
        Mov     AL, 2
        Mov     EDX, HexB_Result
        Mov     [EDX], EAX
        Ret     Size_Params
TMisc@HexB      EndP

; Function HexW (W: Word): String
        HexW_Result     EQU     DWord Ptr [ESP + 8]
        HexW_Word       EQU     DWord Ptr [ESP + 4]
        Size_Params     =       4

        ALIGN   4
TMisc@HexW      PROC    NEAR
        Mov     EDX, HexW_Word
        Mov     ECX, HexW_Result
        Mov     AL, DL
        Shr     AL, 4
        Add     AL, 90h
        DAA
        Adc     AL, 40h
        DAA
        Mov     AH, AL
        Mov     AL, DH
        And     AL, 0Fh
        Add     AL, 90h
        DAA
        Adc     AL, 40h
        DAA
        Shl     EAX, 16
        Mov     AL, DH
        Shr     AL, 4
        Add     AL, 90h
        DAA
        Adc     AL, 40h
        DAA
        Mov     AH, AL
        Mov     AL, 4
        Mov     [ECX], EAX
        Mov     AL, DL
        And     AL, 0Fh
        Add     AL, 90h
        DAA
        Adc     AL, 40h
        DAA
        Mov     [ECX + 4], AL
        Ret     Size_Params
TMisc@HexW      EndP

; Function HexL (L: LongInt): String
        HexL_Result     EQU     DWord Ptr [ESP + 8]
        HexL_Long       EQU     DWord Ptr [ESP + 4]
        Size_Params     =       4

        ALIGN   4
TMisc@HexL      PROC    NEAR
        Mov     EDX, HexL_Long
        Mov     ECX, HexL_Result
        Rol     EDX, 16
        Mov     AL, DL
        Shr     AL, 4
        Add     AL, 90h
        DAA
        Adc     AL, 40h
        DAA
        Mov     AH, AL
        Mov     AL, DH
        And     AL, 0Fh
        Add     AL, 90h
        DAA
        Adc     AL, 40h
        DAA
        Shl     EAX, 16
        Mov     AL, DH
        Shr     AL, 4
        Add     AL, 90h
        DAA
        Adc     AL, 40h
        DAA
        Mov     AH, AL
        Mov     AL, 8
        Mov     [ECX], EAX
        Mov     AL, DL
        And     AL, 0Fh
        Shr     EDX, 16
        Add     AL, 90h
        DAA
        Adc     AL, 40h
        DAA
        Mov     AH, AL
        Mov     AL, DL
        Shr     AL, 4
        Add     AL, 90h
        DAA
        Adc     AL, 40h
        DAA
        Shl     EAX, 16
        Mov     AL, DH
        And     AL, 0Fh
        Add     AL, 90h
        DAA
        Adc     AL, 40h
        DAA
        Mov     AH, AL
        Mov     AL, DH
        Shr     AL, 4
        Add     AL, 90h
        DAA
        Adc     AL, 40h
        DAA
        Rol     EAX, 8
        Mov     [ECX + 4], EAX
        Mov     AL, DL
        And     AL, 0Fh
        Add     AL, 90h
        DAA
        Adc     AL, 40h
        DAA
        Mov     [ECX + 8], AL
        Ret     Size_Params
TMisc@HexL      EndP

; Procedure FillLong (Var Dest; Count: Word; Filler: LongInt)
        FilL_Dest       EQU     DWord Ptr [ESP + 16]
        FilL_Count      EQU     DWord Ptr [ESP + 12]
        FilL_Filler     EQU     DWord Ptr [ESP + 8]
        Size_Params     =       12

        ALIGN   4
TMisc@FillLong  PROC    NEAR
        Push    EDI
        Mov     ECX, FilL_Count
        Mov     EDI, FilL_Dest
        Mov     EAX, FilL_Filler
        CLD
        Rep     StoSD
        Pop     EDI
        Ret     Size_Params
TMisc@FillLong  EndP

;               ESI     Hex string
; Returns       EAX     Number
;
        ALIGN   4
Hex2L   PROC    NEAR
        Mov     CL, [ESI]
        Xor     EDX, EDX
        Inc     ESI
        Xor     EAX, EAX
        And     ECX, 0FFh
        Jz      @@Exit
  @@Loop:
        Mov     DL, [ESI]
        Shl     EAX, 4
        Inc     ESI
        Or      AL, Hex2LongTable + EDX
        Dec     ECX
        Jnz     @@Loop
  @@Exit:
        Ret
Hex2L   EndP
; Function Hex2Byte (Const S: String): Byte
        Hex2B_Str       EQU     DWord Ptr [ESP + 8]
        Size_Params     =       4

        ALIGN   4
TMisc@Hex2Byte  PROC    NEAR
        Push    ESI
        Mov     ESI, Hex2B_Str
        Call    Hex2L
        Pop     ESI
        Ret     Size_Params
TMisc@Hex2Byte  EndP

; Function Hex2Word (Const S: String): Word
        Hex2W_Str       EQU     DWord Ptr [ESP + 8]
        Size_Params     =       4

        ALIGN   4
TMisc@Hex2Word  PROC    NEAR
        Push    ESI
        Mov     ESI, Hex2W_Str
        Call    Hex2L
        Pop     ESI
        Ret     Size_Params
TMisc@Hex2Word  EndP

; Function Hex2Long (Const S: String): LongInt
        Hex2L_Str       EQU     DWord Ptr [ESP + 8]
        Size_Params     =       4

        ALIGN   4
TMisc@Hex2Long  PROC    NEAR
        Push    ESI
        Mov     ESI, Hex2L_Str
        Call    Hex2L
        Pop     ESI
        Ret     Size_Params
TMisc@Hex2Long  EndP

; Function NPos (Const SubStr, S: String; FromPos: Byte): Byte
        NPos_Sub        EQU     DWord Ptr [ESP + 28]
        NPos_S          EQU     DWord Ptr [ESP + 24]
        NPos_From       EQU     Byte Ptr  [ESP + 20]
        Size_Params     =       12

        ALIGN   4
TMisc@NPos      PROC    NEAR
        Push    EBP
        Push    EBX
        Push    ESI
        Push    EDI
        Mov     ESI, NPos_Sub
        Xor     EDX, EDX
        Mov     EDI, NPos_S
        Xor     ECX, ECX
        Mov     DL, [ESI]
        Xor     EAX, EAX
        Mov     CL, [EDI]
        Xor     EBX, EBX
        Test    DL, DL
        Jz      @@NotFound
        Test    CL, CL
        Jz      @@NotFound
        Dec     EDX
        Mov     BL, NPos_From
        Mov     AL, DL
        Dec     EBX
        Inc     ESI
        Sub     ECX, EBX
        Jng     @@NotFound
        Add     EDI, EBX
        Sub     ECX, EAX
        Jng     @@NotFound
        Inc     EDI
        CLD
  @@FirstChar:
        Mov     AL, [ESI]
        RepNE   ScaSB
        Jz      @@CheckRest
  @@NotFound:
        Pop     EDI
        Pop     ESI
        Pop     EBX
        Pop     EBP
        Xor     EAX, EAX
        Ret     Size_Params
  @@CheckRest:
        Test    EDX, EDX
        Jz      @@CharFound
        Mov     EBX, EDI
        Mov     EAX, ECX
        Mov     EBP, ESI
        Mov     ECX, EDX
        Inc     ESI
        Shr     ECX, 2
        RepE    CmpSD
        Mov     ECX, EDX
        Jne     @@Again
        And     ECX, 3
        RepE    CmpSB
        Je      @@StrFound
  @@Again:
        Mov     EDI, EBX
        Mov     ESI, EBP
        Mov     ECX, EAX
        Jmp     Short @@FirstChar
  @@StrFound:
        Mov     EDI, EBX
  @@CharFound:
        Mov     EAX, EDI
        Sub     EAX, NPos_S
        Pop     EDI
        Pop     ESI
        Pop     EBX
        Pop     EBP
        Dec     EAX
        Ret     Size_Params
TMisc@NPos      EndP

; Function PosBM (Const S, Buf: String): Byte
;        PosBM_Str       EQU     DWord Ptr [EBP + 12]
;        PosBM_Buf       EQU     DWord Ptr [EBP + 8]
;        Size_Params     =       8
;
;        ALIGN   4
;TMisc@PosBM     PROC    NEAR
;        Push    EBP
;        Mov     EBP, ESP
;        Push    ESI
;        Push    EDI
;        Mov     ESI, PosBM_Str
;        Mov     EDI, PosBM_Buf
;        Xor     EDX, EDX
;        Mov     AL, [ESI]
;        Mov     DL, [EDI]
;        Inc     ESI
;        Inc     EDI
;        And     EAX, 0FFh
;        Jz      @@Exit
;        Cmp     EAX, EDX
;        Ja      @@Not_Match
;        Cmp     AL, 1
;        Ja      @@Do_Boyer_Moore
;        Inc     EDI
;        Mov     AL, [ESI]
;        Mov     ECX, EDX
;        Mov     ESI, EDI
;        CLD
;        RepNZ   ScaSB
;        Jnz     @@Not_Match
;        Mov     EAX, EDI
;        Sub     EAX, ESI
;        Pop     EDI
;        Pop     ESI
;        Pop     EBP
;        Ret     Size_Params
;  @@Not_Match:
;        Xor     EAX, EAX
;  @@Exit:
;        Pop     EDI
;        Pop     ESI
;        Pop     EBP
;        Ret     Size_Params
;  @@Do_Boyer_Moore:
;        Mov     AH, AL
;        Push    EBX
;        Lea     EDI, PosBM_Table
;        Mov     EBX, EAX
;        Shl     EAX, 16
;        Mov     ECX, BM_TableSize / 4
;        Mov     AX, BX
;        CLD
;        Rep     StoSD
;        Xor     EBX, EBX
;        Mov     CL, AL
;  @@Fill_Table:
;        Mov     BL, [ESI]
;        Inc     ESI
;        Dec     ECX
;        Mov     PosBM_Table + EBX, CL
;        Jnz     @@Fill_Table
;        STD
;        Mov     ESI, PosBM_Buf
;        And     EAX, 0FFh
;        Inc     ESI
;        Mov     EDI, PosBM_Str
;        Mov     EBX, EAX
;        Dec     EDX
;        Add     EDI, EAX
;        Dec     EBX
;        Mov     EBP, ESI
;  @@Search_Last:
;        Sub     EDX, EBX
;        Jc      @@Not_BM_Match
;        Add     ESI, EBX
;        Mov     BL, [ESI]
;        Mov     BL, PosBM_Table + EBX
;        Test    BL, BL
;        Jnz     @@Search_Last
;        Mov     ECX, EAX
;        Mov     EBX, ESI
;        RepZ    CmpSB
;        Jz      @@Exact_BM_Match
;        Add     EDI, EAX
;        Mov     ESI, EBX
;        Sub     EDI, ECX
;        Mov     EBX, 1
;        Jmp     Short @@Search_Last
;  @@Exact_BM_Match:
;        CLD
;        Pop     EBX
;        Mov     EAX, ESI
;        Pop     EDI
;        Sub     EAX, EBP
;        Pop     ESI
;        Inc     EAX
;        Pop     EBP
;        Inc     EAX
;        Ret     Size_Params
;  @@Not_BM_Match:
;        CLD
;        Pop     EBX
;        Pop     EDI
;        Pop     ESI
;        Pop     EBP
;        Xor     EAX, EAX
;        Ret     Size_Params
;TMisc@PosBM     EndP

CODE32  ENDS

END
