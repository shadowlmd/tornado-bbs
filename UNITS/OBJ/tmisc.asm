; * * * * * * * * * * * * * * * DATA * * * * * * * * * * * * * * * * * * * * *
;  DATA  SEGMENT WORD PUBLIC
;        ASSUME  DS:DATA
;  DATA  EndS
; * * * * * * * * * * * * * * * CODE * * * * * * * * * * * * * * * * * * * * *
  .286
  LOCALS
  CODE  SEGMENT PUBLIC
        ASSUME  CS:CODE

        PUBLIC  PadCh, Pad, LeftPadCh, LeftPad, TrimLead, TrimTrail, Trim
        PUBLIC  CenterCh, Replicate, UpCase, LoCase, UpString, LoString
        PUBLIC  EngLoString, WordCount, WordPosition, ExtractWord, AsciiCount
        PUBLIC  AsciiPosition, ExtractAscii, AsciiPosCh, AsciiPos, ConsistsOf
        PUBLIC  StrContains, DelChars, Str2Set, StrCompare, SetLength
        PUBLIC  SetString, FlagsValid, MaskMatch, XlatStr, HexB, HexW, HexL,
        PUBLIC  FillLong, Hex2Byte, Hex2Word, Hex2Long, NPos

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

; * * * Function PadCh (S : String; CH : Char; Len : Byte) : String * * * * * *
        PadCh_Result    EQU     DWord Ptr [BP + 14]
        PadCh_Str       EQU     DWord Ptr [BP + 10]
        PadCh_CH        EQU     Byte Ptr [BP + 8]
        PadCh_Len       EQU     Byte Ptr [BP + 6]
        Size_Params     =       8
  PadCh         PROC    FAR
        Push    BP
        Mov     BP, SP
        Mov     DX, DS
        CLD
        LDS     SI, PadCh_Str
        LES     DI, PadCh_Result
        Mov     AL, PadCh_Len
        Mov     BL, [SI]
        Xor     AH, AH
        Xor     BH, BH
        Inc     SI
        StoSB
        Cmp     BX, AX
        Jna     @@StrLess
        Mov     BX, AX
  @@StrLess:
        Mov     CX, BX
        Shr     CX, 1
        Jnc     @@EvenMove
        MovSB
  @@EvenMove:
        Rep     MovSW
        Sub     AX, BX
        Jz      @@SkipFill
        Mov     CX, AX
        Mov     AL, PadCh_CH
        Shr     CX, 1
        Mov     AH, AL
        Jnc     @@EvenFill
        StoSB
  @@EvenFill:
        Rep     StoSW
  @@SkipFill:
        Mov     DS, DX
        Pop     BP
        Ret     Size_Params
  PadCh         EndP
; * * * Function Pad (S : String; Len : Byte) : String * * * * * * * * * * * *
        Pad_Result      EQU     DWord Ptr SS:[BX + 10]
        Pad_Str         EQU     DWord Ptr SS:[BX + 6]
        Pad_Len         EQU     Byte Ptr SS:[BX + 4]
        Size_Params     =       6
  Pad           PROC    FAR
        Mov     BX, SP
        Mov     DX, DS
        CLD
        LDS     SI, Pad_Str
        LES     DI, Pad_Result
        Mov     AL, Pad_Len
        Mov     BL, [SI]
        Xor     AH, AH
        Xor     BH, BH
        Inc     SI
        StoSB
        Cmp     BX, AX
        Jna     @@StrLess
        Mov     BX, AX
  @@StrLess:
        Mov     CX, BX
        Shr     CX, 1
        Jnc     @@EvenMove
        MovSB
  @@EvenMove:
        Rep     MovSW
        Sub     AX, BX
        Jz      @@SkipFill
        Mov     CX, AX
        Mov     AX, 2020h
        Shr     CX, 1
        Jnc     @@EvenFill
        StoSB
  @@EvenFill:
        Rep     StoSW
  @@SkipFill:
        Mov     DS, DX
        Ret     Size_Params
  Pad           EndP
; * * * Function LeftPadCh (S : String; CH : Char; Len : Byte) : String * * * *
        LPadCh_Result   EQU     DWord Ptr [BP + 14]
        LPadCh_Str      EQU     DWord Ptr [BP + 10]
        LPadCh_CH       EQU     Byte Ptr [BP + 8]
        LPadCh_Len      EQU     Byte Ptr [BP + 6]
        Size_Params     =       8
  LeftPadCh     PROC    FAR
        Push    BP
        Mov     BP, SP
        Mov     DX, DS
        CLD
        LDS     SI, LPadCh_Str
        LES     DI, LPadCh_Result
        Mov     AL, LPadCh_Len
        Mov     BL, [SI]
        Xor     AH, AH
        Xor     BH, BH
        Inc     SI
        StoSB
        Cmp     BX, AX
        Jna     @@StrLess
        Mov     BX, AX
  @@StrLess:
        Sub     AX, BX
        Jz      @@SkipFill
        Mov     CX, AX
        Mov     AL, LPadCh_CH
        Shr     CX, 1
        Mov     AH, AL
        Jnc     @@EvenFill
        StoSB
  @@EvenFill:
        Rep     StoSW
  @@SkipFill:
        Mov     CX, BX
        Shr     CX, 1
        Jnc     @@EvenMove
        MovSB
  @@EvenMove:
        Rep     MovSW
        Mov     DS, DX
        Pop     BP
        Ret     Size_Params
  LeftPadCh     EndP
; * * * Function LeftPad (S : String; Len : Byte) : String * * * * * * * * * *
        LPad_Result     EQU     DWord Ptr SS:[BX + 10]
        LPad_Str        EQU     DWord Ptr SS:[BX + 6]
        LPad_Len        EQU     Byte Ptr SS:[BX + 4]
        Size_Params     =       6
  LeftPad       PROC    FAR
        Mov     BX, SP
        Mov     DX, DS
        CLD
        LDS     SI, LPad_Str
        LES     DI, LPad_Result
        Mov     AL, LPad_Len
        Mov     BL, [SI]
        Xor     AH, AH
        Xor     BH, BH
        Inc     SI
        StoSB
        Cmp     BX, AX
        Jna     @@StrLess
        Mov     BX, AX
  @@StrLess:
        Sub     AX, BX
        Jz      @@SkipFill
        Mov     CX, AX
        Mov     AX, 2020h
        Shr     CX, 1
        Jnc     @@EvenFill
        StoSB
  @@EvenFill:
        Rep     StoSW
  @@SkipFill:
        Mov     CX, BX
        Shr     CX, 1
        Jnc     @@EvenMove
        MovSB
  @@EvenMove:
        Rep     MovSW
        Mov     DS, DX
        Ret     Size_Params
  LeftPad       EndP
; * * * Function TrimLead (S : String) : String * * * * * * * * * * * * * * * *
        TLead_Result    EQU     DWord Ptr SS:[BX + 8]
        TLead_Str       EQU     DWord Ptr SS:[BX + 4]
        Size_Params     =       4
  TrimLead      PROC    FAR
        Mov     BX, SP
        Mov     DX, DS
        LDS     SI, TLead_Str
        LES     DI, TLead_Result
        Mov     CL, [SI]
        Inc     SI
        And     CX, 0FFh
        Jz      @@ZeroResult
  @@Loop:
        Mov     AL, [SI]
        Inc     SI
        Cmp     AL, 32
        Ja      @@DoMove
        Loop    @@Loop
  @@ZeroResult:
        Mov     ES:[DI], CL
        Mov     DS, DX
        Ret     Size_Params
  @@DoMove:
        Mov     AH, AL
        Mov     AL, CL
        Dec     CX
        Mov     ES:[DI], AX
        Add     DI, 2
        Shr     CX, 1
        CLD
        Rep     MovSW
        Jnc     @@MoveDone
        MovSB
  @@MoveDone:
        Mov     DS, DX
        Ret     Size_Params
  TrimLead      EndP
; * * * Function TrimTrail (S : String) : String * * * * * * * * * * * * * * *
        TTrail_Result   EQU     DWord Ptr SS:[BX + 8]
        TTrail_Str      EQU     DWord Ptr SS:[BX + 4]
        Size_Params     =       4
  TrimTrail     PROC    FAR
        Mov     BX, SP
        Mov     DX, DS
        LDS     SI, TTrail_Str
        LES     DI, TTrail_Result
        Mov     CL, [SI]
        And     CX, 0FFh
        Jz      @@ZeroResult
        Add     SI, CX
  @@Loop:
        Cmp     Byte Ptr [SI], 32
        Ja      @@DoMove
        Dec     SI
        Loop    @@Loop
  @@ZeroResult:
        Mov     ES:[DI], CL
        Mov     DS, DX
        Ret     Size_Params
  @@DoMove:
        Mov     ES:[DI], CL
        Sub     SI, CX
        Inc     DI
        Inc     SI
        CLD
        Shr     CX, 1
        Jnc     @@EvenMove
        MovSB
  @@EvenMove:
        Rep     MovSW
        Mov     DS, DX
        Ret     Size_Params
  TrimTrail     EndP
; * * * Function Trim (S : String) : String * * * * * * * * * * * * * * * * * *
        Trim_Result     EQU     DWord Ptr SS:[BX + 8]
        Trim_Str        EQU     DWord Ptr SS:[BX + 4]
        Size_Params     =       4
  Trim          PROC    FAR
        Mov     BX, SP
        Mov     DX, DS
        LDS     SI, Trim_Str
        LES     DI, Trim_Result
        Mov     CL, [SI]
        And     CX, 0FFh
        Jz      @@ZeroResult
        Add     SI, CX
        Mov     AH, 32
  @@LoopTrail:
        Cmp     [SI], AH
        Jnz     @@CheckLead
        Dec     SI
        Loop    @@LoopTrail
  @@ZeroResult:
        Mov     ES:[DI], CL
        Mov     DS, DX
        Ret     Size_Params
  @@CheckLead:
        Sub     SI, CX
        Inc     SI
  @@LoopLead:
        Mov     AL, [SI]
        Inc     SI
        Cmp     AL, AH
        Jnz     @@DoMove
        Loop    @@LoopLead
  @@DoMove:
        Mov     AH, AL
        Mov     AL, CL
        Dec     CX
        Mov     ES:[DI], AX
        Add     DI, 2
        Shr     CX, 1
        CLD
        Rep     MovSW
        Jnc     @@MoveDone
        MovSB
  @@MoveDone:
        Mov     DS, DX
        Ret     Size_Params
  Trim          EndP
; * * * Function CenterCh (S : String; CH : Char; Len : Byte) : String * * * *
        CenterCh_Result EQU     DWord Ptr [BP + 14]
        CenterCh_Str    EQU     DWord Ptr [BP + 10]
        CenterCh_CH     EQU     Byte Ptr [BP + 8]
        CenterCh_Len    EQU     Byte Ptr [BP + 6]
        Size_Params     =       8
  CenterCh      PROC    FAR
        Push    BP
        Mov     BP, SP
        Push    DS
        CLD
        LDS     SI, CenterCh_Str
        LES     DI, CenterCh_Result
        Mov     BL, [SI]
        Mov     DL, CenterCh_Len
        Xor     BH, BH
        Xor     DH, DH
        Cmp     BX, DX
        Jl      @@DoCenter
        Mov     CX, BX
        Inc     CX
        Shr     CX, 1
        Rep     MovSW
        Jnc     @@MoveDone
        MovSB
  @@MoveDone:
        Pop     DS
        Pop     BP
        Ret     Size_Params
  @@DoCenter:
        Mov     ES:[DI], DL
        Inc     DI
        Mov     AL, CenterCh_CH
        Mov     CX, DX
        Mov     AH, AL
        Shr     CX, 1
        Jnc     @@EvenFill
        StoSB
  @@EvenFill:
        Rep     StoSW
        Sub     DI, DX
        Sub     DX, BX
        Mov     CX, BX
        Shr     DX, 1
        Inc     SI
        Add     DI, DX
        Shr     CX, 1
        Jnc     @@EvenMove
        MovSB
  @@EvenMove:
        Rep     MovSW
        Pop     DS
        Pop     BP
        Ret     Size_Params
  CenterCh      EndP
; * * * Function Replicate (Ch: Char; Len: Integer): String * * * * * * * * * *
        Repl_Result     EQU     DWord Ptr SS:[BX + 8]
        Repl_CH         EQU     Byte Ptr SS:[BX + 6]
        Repl_Len        EQU     Word Ptr SS:[BX + 4]
        Size_Params     =       4
  Replicate     PROC    FAR
        Mov     BX, SP
        LES     DI, Repl_Result
        Mov     AX, Repl_Len
        Or      AX, AX
        Jng     @@ZeroResult
        Or      AH, AH
        Jz      @@ValidRange
        Mov     AX, 255
  @@ValidRange:
        Mov     CX, AX
        Mov     AH, Repl_CH
        Mov     ES:[DI], AX
        Add     DI, 2
        Dec     CX
        Mov     AL, AH
        Shr     CX, 1
        CLD
        Rep     StoSW
        Jnc     @@FillDone
        StoSB
  @@FillDone:
        Ret     Size_Params
  @@ZeroResult:
        Mov     Byte Ptr ES:[DI], 0
        Ret     Size_Params
  Replicate     EndP
; * * * Function UpCase (C: Char) : Char * * * * * * * * * * * * * * * * * * *
        UpCase_CH       EQU     Byte Ptr SS:[BX + 4]
        Size_Params     =       2
  UpCase        PROC    FAR
        Mov     BX, SP
        Mov     BL, UpCase_CH
        Xor     BH, BH
        Mov     AL, CS:UpTable + BX
        Ret     Size_Params
  UpCase        EndP
; * * * Function LoCase (C: Char) : Char * * * * * * * * * * * * * * * * * * *
        LoCase_CH       EQU     Byte Ptr SS:[BX + 4]
        Size_Params     =       2
  LoCase        PROC    FAR
        Mov     BX, SP
        Mov     BL, LoCase_CH
        Xor     BH, BH
        Mov     AL, CS:LoTable + BX
        Ret     Size_Params
  LoCase        EndP
; * * * Function UpString (S: String): String * * * * * * * * * * * * * * * * *
        UpStr_Result    EQU     DWord Ptr SS:[BX + 8]
        UpStr_Str       EQU     DWord Ptr SS:[BX + 4]
        Size_Params     =       4
  UpString      PROC    FAR
        Mov     BX, SP
        Mov     DX, DS
        LDS     SI, UpStr_Str
        LES     DI, UpStr_Result
        Mov     CL, [SI]
        Mov     ES:[DI], CL
        And     CX, 0FFh
        Jz      @@Exit
        Inc     SI
        Inc     DI
        Xor     BX, BX
        Shr     CX, 1
        Jnc     @@Loop
        Mov     BL, [SI]
        Mov     AL, CS:UpTable + BX
        Mov     ES:[DI], AL
        Or      CX, CX
        Jz      @@Exit
        Inc     SI
        Inc     DI
  @@Loop:
        Mov     AX, [SI]
        Mov     BL, AL
        Mov     AL, CS:UpTable + BX
        Mov     BL, AH
        Mov     AH, CS:UpTable + BX
        Add     SI, 2
        Mov     ES:[DI], AX
        Add     DI, 2
        Loop    @@Loop
  @@Exit:
        Mov     DS, DX
        Ret     Size_Params
  UpString      EndP
; * * * Function LoString (S: String): String * * * * * * * * * * * * * * * * *
        LoStr_Result    EQU     DWord Ptr SS:[BX + 8]
        LoStr_Str       EQU     DWord Ptr SS:[BX + 4]
        Size_Params     =       4
  LoString      PROC    FAR
        Mov     BX, SP
        Mov     DX, DS
        LDS     SI, LoStr_Str
        LES     DI, LoStr_Result
        Mov     CL, [SI]
        Mov     ES:[DI], CL
        And     CX, 0FFh
        Jz      @@Exit
        Inc     SI
        Inc     DI
        Xor     BX, BX
        Shr     CX, 1
        Jnc     @@Loop
        Mov     BL, [SI]
        Mov     AL, CS:LoTable + BX
        Mov     ES:[DI], AL
        Or      CX, CX
        Jz      @@Exit
        Inc     SI
        Inc     DI
  @@Loop:
        Mov     AX, [SI]
        Mov     BL, AL
        Mov     AL, CS:LoTable + BX
        Mov     BL, AH
        Mov     AH, CS:LoTable + BX
        Add     SI, 2
        Mov     ES:[DI], AX
        Add     DI, 2
        Loop    @@Loop
  @@Exit:
        Mov     DS, DX
        Ret     Size_Params
  LoString      EndP
; * * * Function EngLoString (S: String): String * * * * * * * * * * * * * * *
        ELoStr_Result   EQU     DWord Ptr SS:[BX + 8]
        ELoStr_Str      EQU     DWord Ptr SS:[BX + 4]
        Size_Params     =       4
  EngLoString   PROC    FAR
        Mov     BX, SP
        Mov     DX, DS
        LDS     SI, ELoStr_Str
        LES     DI, ELoStr_Result
        Mov     CL, [SI]
        Mov     ES:[DI], CL
        And     CX, 0FFh
        Jz      @@Exit
        Inc     SI
        Inc     DI
        Xor     BX, BX
        Shr     CX, 1
        Jnc     @@Loop
        Mov     BL, [SI]
        Mov     AL, CS:EngLoTable + BX
        Mov     ES:[DI], AL
        Or      CX, CX
        Jz      @@Exit
        Inc     SI
        Inc     DI
  @@Loop:
        Mov     AX, [SI]
        Mov     BL, AL
        Mov     AL, CS:EngLoTable + BX
        Mov     BL, AH
        Mov     AH, CS:EngLoTable + BX
        Add     SI, 2
        Mov     ES:[DI], AX
        Add     DI, 2
        Loop    @@Loop
  @@Exit:
        Mov     DS, DX
        Ret     Size_Params
  EngLoString   EndP
; * * * Function WordCount (S : String; WordDelims : CharSet) : Byte * * * * *
        WordC_Str       EQU     DWord Ptr SS:[BX + 8]
        WordC_Delims    EQU     DWord Ptr SS:[BX + 4]
        Size_Params     =       8
  WordCount     PROC    FAR
        Mov     BX, SP
        Mov     DX, DS
        LDS     SI, WordC_Str
        Mov     AH, [SI]
        Inc     SI
        And     AX, 0FF00h
        Jz      @@Exit
        LES     DI, WordC_Delims
        Xor     BH, BH
  @@SkipDelimsLoop:
        Mov     BL, [SI]
        Inc     SI
        Mov     CL, BL
        Shr     BL, 3
        And     CL, 7
        Mov     BL, ES:[BX + DI]
        Inc     CL
        Shr     BL, CL
        Jnc     @@WordBegins
  @@SkipDelimAgain:
        Dec     AH
        Jnz     @@SkipDelimsLoop
        Mov     DS, DX
        Ret     Size_Params
  @@WordBegins:
        Inc     AL
        Dec     AH
        Jz      @@Exit
  @@SkipWordLoop:
        Mov     BL, [SI]
        Inc     SI
        Mov     CL, BL
        Shr     BL, 3
        And     CL, 7
        Mov     BL, ES:[BX + DI]
        Inc     CL
        Shr     BL, CL
        Jc      @@SkipDelimAgain
        Dec     AH
        Jnz     @@SkipWordLoop
  @@Exit:
        Mov     DS, DX
        Ret     Size_Params
  WordCount     EndP
; * * * Function WordPosition (N : Byte; S : String; WordDelims : CharSet) : Byte
        WordP_Num       EQU     Byte Ptr SS:[BX + 12]
        WordP_Str       EQU     DWord Ptr SS:[BX + 8]
        WordP_Delims    EQU     DWord Ptr SS:[BX + 4]
        Size_Params     =       10
  WordPosition  PROC    FAR
        Mov     BX, SP
        Push    DS
        LDS     SI, WordP_Str
        Mov     AL, [SI]
        Inc     SI
        And     AX, 0FFh
        Jz      @@Exit
        Mov     DL, WordP_Num
        Or      DL, DL
        Jz      @@NotFound
        LES     DI, WordP_Delims
        Xor     BH, BH
        Mov     CH, AL
  @@SkipDelimsLoop:
        Mov     BL, [SI]
        Inc     SI
        Mov     CL, BL
        Shr     BL, 3
        And     CL, 7
        Mov     BL, ES:[BX + DI]
        Inc     CL
        Shr     BL, CL
        Jnc     @@WordBegins
  @@SkipDelimAgain:
        Dec     CH
        Jnz     @@SkipDelimsLoop
  @@NotFound:
        Xor     AL, AL
  @@Exit:
        Pop     DS
        Ret     Size_Params
  @@WordBegins:
        Inc     AH
        Cmp     AH, DL
        Jnz     @@SkipWordAgain
        Sub     AL, CH
        Pop     DS
        Inc     AL
        Ret     Size_Params
  @@SkipWordLoop:
        Mov     BL, [SI]
        Inc     SI
        Mov     CL, BL
        Shr     BL, 3
        And     CL, 7
        Mov     BL, ES:[BX + DI]
        Inc     CL
        Shr     BL, CL
        Jc      @@SkipDelimAgain
  @@SkipWordAgain:
        Dec     CH
        Jnz     @@SkipWordLoop
        Xor     AL, AL
        Pop     DS
        Ret     Size_Params
  WordPosition  EndP
; * * * Function ExtractWord (N : Byte; S : String; WordDelims : CharSet) : String
        WordE_Result    EQU     DWord Ptr [BP + 16]
        WordE_Num       EQU     Byte Ptr [BP + 14]
        WordE_Str       EQU     DWord Ptr [BP + 10]
        WordE_Delims    EQU     DWord Ptr [BP + 6]
        Size_Params     =       10
  ExtractWord   PROC    FAR
        Push    BP
        Mov     BP, SP
        Push    DS
        LDS     SI, WordE_Str
        Mov     AL, [SI]
        Inc     SI
        And     AX, 0FFh
        Jz      @@NotFound
        Mov     CH, WordE_Num
        Or      CH, CH
        Jz      @@NotFound
        Xor     BH, BH
        LES     DI, WordE_Delims
  @@SkipDelimsLoop:
        Mov     BL, [SI]
        Inc     SI
        Mov     CL, BL
        Shr     BL, 3
        And     CL, 7
        Mov     BL, ES:[BX + DI]
        Inc     CL
        Shr     BL, CL
        Jnc     @@WordBegins
  @@SkipDelimAgain:
        Dec     AL
        Jnz     @@SkipDelimsLoop
  @@NotFound:
        LES     DI, WordE_Result
        Pop     DS
        Pop     BP
        Mov     Byte Ptr ES:[DI], 0
        Ret     Size_Params
  @@WordBegins:
        Inc     AH
        Cmp     AH, CH
        Jnz     @@SkipWordAgain
        Mov     DX, SI
        Mov     AH, AL
        Dec     DX
        Dec     AH
        Jz      @@MoveFound
  @@ScanFoundLoop:
        Mov     BL, [SI]
        Inc     SI
        Mov     CL, BL
        Shr     BL, 3
        And     CL, 7
        Mov     BL, ES:[BX + DI]
        Inc     CL
        Shr     BL, CL
        Jc      @@MoveFound
        Dec     AH
        Jnz     @@ScanFoundLoop
  @@MoveFound:
        Sub     AL, AH
        LES     DI, WordE_Result
        Mov     SI, DX
        Mov     CL, AL
        Mov     ES:[DI], AL
        Xor     CH, CH
        Inc     DI
        CLD
        Shr     CX, 1
        Jnc     @@MoveDone
        MovSB
  @@MoveDone:
        Rep     MovSW
        Pop     DS
        Pop     BP
        Ret     Size_Params
  @@SkipWordLoop:
        Mov     BL, [SI]
        Inc     SI
        Mov     CL, BL
        Shr     BL, 3
        And     CL, 7
        Mov     BL, ES:[BX + DI]
        Inc     CL
        Shr     BL, CL
        Jc      @@SkipDelimAgain
  @@SkipWordAgain:
        Dec     AL
        Jnz     @@SkipWordLoop
        LES     DI, WordE_Result
        Pop     DS
        Pop     BP
        Mov     ES:[DI], AL
        Ret     Size_Params
  ExtractWord   EndP
; * * * Function AsciiCount (S : String; WordDelims : CharSet; Quote: Char) : Byte
        AsciiC_Str      EQU     DWord Ptr SS:[BX + 10]
        AsciiC_Delims   EQU     DWord Ptr SS:[BX + 6]
        AsciiC_Quote    EQU     Byte Ptr SS:[BX + 4]
        Size_Params     =       10
  AsciiCount    PROC    FAR
        Mov     BX, SP
        Mov     DX, DS
        LDS     SI, AsciiC_Str
        Mov     AH, [SI]
        Inc     SI
        And     AX, 0FF00h
        Jz      @@Exit
        LES     DI, AsciiC_Delims
        Mov     CH, AsciiC_Quote
        Mov     BL, [SI]
        Xor     BH, BH
        Mov     CL, BL
        Shr     BL, 3
        And     CL, 7
        Mov     BL, ES:[BX + DI]
        Inc     CL
        Shr     BL, CL
        Jnc     @@WordBegins
        Inc     SI
        Inc     AL
        Dec     AH
        Jz      @@Exit
  @@SkipDelimsLoop:
        Mov     BL, [SI]
        Mov     CL, BL
        Shr     BL, 3
        And     CL, 7
        Mov     BL, ES:[BX + DI]
        Inc     CL
        Shr     BL, CL
        Jnc     @@WordBegins
        Inc     SI
  @@SkipDelimAgain:
        Dec     AH
        Jnz     @@SkipDelimsLoop
  @@Exit:
        Mov     DS, DX
        Ret     Size_Params
  @@WordBegins:
        Inc     AL
  @@SkipWordLoop:
        Mov     BL, [SI]
        Inc     SI
        Cmp     BL, CH
        Jz      @@SkipQuote
        Mov     CL, BL
        Shr     BL, 3
        And     CL, 7
        Mov     BL, ES:[BX + DI]
        Inc     CL
        Shr     BL, CL
        Jc      @@SkipDelimAgain
  @@SkipWordAgain:
        Dec     AH
        Jnz     @@SkipWordLoop
        Mov     DS, DX
        Ret     Size_Params
  @@SkipQuote:
        Dec     AH
        Jz      @@Exit
  @@SkipQuoteLoop:
        Mov     BL, [SI]
        Inc     SI
        Cmp     BL, CH
        Jz      @@SkipWordAgain
        Dec     AH
        Jnz     @@SkipQuoteLoop
        Jmp     Short @@Exit
  AsciiCount    EndP
; * * * Function AsciiPosition (N : Byte; S : String; WordDelims : CharSet; Quote : Char) : Byte
        AsciiP_Num      EQU     Byte Ptr [BP + 16]
        AsciiP_Str      EQU     DWord Ptr [BP + 12]
        AsciiP_Delims   EQU     DWord Ptr [BP + 8]
        AsciiP_Quote    EQU     Byte Ptr [BP + 6]
        Size_Params     =       12
  AsciiPosition PROC    FAR
        Push    BP
        Mov     BP, SP
        Push    DS
        LDS     SI, AsciiP_Str
        Mov     AL, [SI]
        Inc     SI
        And     AX, 0FFh
        Jz      @@Exit
        Mov     DH, AsciiP_Num
        Or      DH, DH
        Jz      @@Exit
        LES     DI, AsciiP_Delims
        Xor     BH, BH
        Mov     CH, AL
        Mov     BL, [SI]
        Mov     DL, AsciiP_Quote
        Mov     CL, BL
        Shr     BL, 3
        And     CL, 7
        Mov     BL, ES:[BX + DI]
        Inc     CL
        Shr     BL, CL
        Jnc     @@WordBegins
        Inc     SI
        Inc     AH
        Dec     AL
        Jz      @@Exit2
  @@SkipDelimsLoop:
        Mov     BL, [SI]
        Mov     CL, BL
        Shr     BL, 3
        And     CL, 7
        Mov     BL, ES:[BX + DI]
        Inc     CL
        Shr     BL, CL
        Jnc     @@WordBegins
        Inc     SI
  @@SkipDelimAgain:
        Dec     AL
        Jnz     @@SkipDelimsLoop
  @@Exit:
        Pop     DS
        Pop     BP
        Xor     AL, AL
        Ret     Size_Params
  @@WordBegins:
        Inc     AH
        Cmp     AH, DH
        Jnz     @@SkipWordLoop
        Sub     CH, AL
        Pop     DS
        Mov     AL, CH
        Pop     BP
        Inc     AL
        Ret     Size_Params
  @@SkipWordLoop:
        Mov     BL, [SI]
        Inc     SI
        Cmp     BL, DL
        Jz      @@SkipQuote
        Mov     CL, BL
        Shr     BL, 3
        And     CL, 7
        Mov     BL, ES:[BX + DI]
        Inc     CL
        Shr     BL, CL
        Jc      @@SkipDelimAgain
  @@SkipWordAgain:
        Dec     AL
        Jnz     @@SkipWordLoop
  @@Exit2:
        Pop     DS
        Pop     BP
        Ret     Size_Params
  @@SkipQuote:
        Dec     AL
        Jz      @@Exit2
  @@SkipQuoteLoop:
        Mov     BL, [SI]
        Inc     SI
        Cmp     BL, DL
        Jz      @@SkipWordAgain
        Dec     AL
        Jnz     @@SkipQuoteLoop
        Jmp     Short @@Exit2
  AsciiPosition EndP
; * * * Function ExtractAscii (N : Byte; S : String; WordDelims : CharSet; Quote : Char) : String
        AsciiE_Result   EQU     DWord Ptr [BP + 18]
        AsciiE_Num      EQU     Byte Ptr [BP + 16]
        AsciiE_Str      EQU     DWord Ptr [BP + 12]
        AsciiE_Delims   EQU     DWord Ptr [BP + 8]
        AsciiE_Quote    EQU     Byte Ptr [BP + 6]
        Size_Params     =       12
  ExtractAscii  PROC    FAR
        Push    BP
        Mov     BP, SP
        Push    DS
        LDS     SI, AsciiE_Str
        Mov     AL, [SI]
        Inc     SI
        And     AX, 0FFh
        Jz      @@Exit
        Mov     CH, AsciiE_Num
        Or      CH, CH
        Jz      @@Exit
        Xor     BH, BH
        LES     DI, AsciiE_Delims
        Mov     BL, [SI]
        Mov     DL, AsciiE_Quote
        Mov     CL, BL
        Shr     BL, 3
        And     CL, 7
        Mov     BL, ES:[BX + DI]
        Inc     CL
        Shr     BL, CL
        Jnc     @@WordBegins
        Inc     SI
        Inc     AH
        Dec     AL
        Jz      @@Exit
  @@SkipDelimsLoop:
        Mov     BL, [SI]
        Mov     CL, BL
        Shr     BL, 3
        And     CL, 7
        Mov     BL, ES:[BX + DI]
        Inc     CL
        Shr     BL, CL
        Jnc     @@WordBegins
        Inc     SI
  @@SkipDelimAgain:
        Dec     AL
        Jnz     @@SkipDelimsLoop
  @@Exit:
        LES     DI, AsciiE_Result
        Pop     DS
        Pop     BP
        Mov     Byte Ptr ES:[DI], 0
        Ret     Size_Params
  @@WordBegins:
        Inc     AH
        Cmp     AH, CH
        Jz      @@DoFound
  @@SkipWordLoop:
        Mov     BL, [SI]
        Inc     SI
        Cmp     BL, DL
        Jz      @@Word_SkipQuote
        Mov     CL, BL
        Shr     BL, 3
        And     CL, 7
        Mov     BL, ES:[BX + DI]
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
        Mov     BL, [SI]
        Inc     SI
        Cmp     BL, DL
        Jz      @@SkipWordAgain
        Dec     AL
        Jnz     @@Word_SkipQuoteLoop
        Jmp     Short @@Exit
  @@Scan_SkipQuote:
        Dec     AH
        Jz      @@MoveFound
  @@Scan_SkipQuoteLoop:
        Mov     BL, [SI]
        Inc     SI
        Cmp     BL, DL
        Jz      @@SkipScanAgain
        Dec     AH
        Jnz     @@Scan_SkipQuoteLoop
        Jmp     Short @@MoveFound
  @@DoFound:
        Mov     AH, AL
        Push    SI
  @@ScanFoundLoop:
        Mov     BL, [SI]
        Inc     SI
        Cmp     BL, DL
        Jz      @@Scan_SkipQuote
        Mov     CL, BL
        Shr     BL, 3
        And     CL, 7
        Mov     BL, ES:[BX + DI]
        Inc     CL
        Shr     BL, CL
        Jc      @@MoveFound
  @@SkipScanAgain:
        Dec     AH
        Jnz     @@ScanFoundLoop
  @@MoveFound:
        LES     DI, AsciiE_Result
        Sub     AL, AH
        Pop     SI
        Xor     CH, CH
        Mov     ES:[DI], AL
        Inc     DI
        Mov     CL, AL
        CLD
        Shr     CX, 1
        Jnc     @@MoveDone
        MovSB
  @@MoveDone:
        Rep     MovSW
        Pop     DS
        Pop     BP
        Ret     Size_Params
  ExtractAscii  EndP
; * * * Function AsciiPosCh (S : String; Ch, Quote: Char) : Byte * * * * * * *
        AsciiPC_Str     EQU     DWord Ptr SS:[BX + 8]
        AsciiPC_Ch      EQU     Byte Ptr SS:[BX + 6]
        AsciiPC_Quote   EQU     Byte Ptr SS:[BX + 4]
        Size_Params     =       8
  AsciiPosCh    PROC    FAR
        Mov     BX, SP
        Mov     DX, DS
        LDS     SI, AsciiPC_Str
        Mov     CL, [SI]
        Inc     SI
        And     CX, 0FFh
        Jz      @@Exit
        Mov     AL, AsciiPC_Ch
        Mov     AH, AsciiPC_Quote
        Mov     BX, AX
        Mov     AH, CL
  @@Loop:
        Mov     AL, [SI]
        Inc     SI
        Cmp     AL, BL
        Jz      @@CharFound
        Cmp     AL, BH
        Jz      @@SkipQuote
  @@SkipChar:
        Loop    @@Loop
  @@Exit:
        Mov     DS, DX
        Xor     AL, AL
        Ret     Size_Params
  @@CharFound:
        Mov     AL, AH
        Mov     DS, DX
        Sub     AL, CL
        Inc     AL
        Ret     Size_Params
  @@SkipQuote:
        Dec     CX
        Jz      @@Exit
  @@QuoteLoop:
        Mov     AL, [SI]
        Inc     SI
        Cmp     AL, BH
        Jz      @@SkipChar
        Loop    @@QuoteLoop
        Jmp     Short @@Exit
  AsciiPosCh    EndP
; * * * Function AsciiPos (S, SubStr: String; Quote: Char) : Byte * * * * * * *
        AsciiPS_Str     EQU     DWord Ptr [BP + 12]
        AsciiPS_SubStr  EQU     DWord Ptr [BP + 8]
        AsciiPS_Quote   EQU     Byte Ptr [BP + 6]
        Size_Params     =       10
  AsciiPos      PROC    FAR
        Push    BP
        Mov     BP, SP
        Push    DS
        LDS     SI, AsciiPS_Str
        Mov     CL, [SI]
        Inc     SI
        Test    CL, CL
        Jz      @@Exit
        LES     DI, AsciiPS_SubStr
        Mov     AL, ES:[DI]
        Test    AL, AL
        Jz      @@Exit
        Dec     AL
        Inc     DI
        Mov     CH, AL
        Mov     BL, AsciiPS_Quote
        Mov     AH, ES:[DI]
        Mov     BH, CL
        Inc     DI
        CLD
        Mov     BP, DI
  @@Loop:
        Mov     AL, [SI]
        Inc     SI
        Cmp     AL, BL
        Jz      @@SkipQuote
        Cmp     AL, AH
        Jz      @@CharFound
  @@SkipChar:
        Dec     CL
        Jnz     @@Loop
  @@Exit:
        Pop     DS
        Pop     BP
        Xor     AL, AL
        Ret     Size_Params
  @@SkipQuote:
        Dec     CL
        Jz      @@Exit
  @@QuoteLoop:
        Mov     AL, [SI]
        Inc     SI
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
        Push    CX
        Mov     DX, SI
        Mov     CL, CH
        Mov     DI, BP
        Xor     CH, CH
        RepZ    CmpSB
        Pop     CX
        Jz      @@StrFound
        Mov     SI, DX
        Jmp     Short @@SkipChar
  @@StrFound:
        Mov     AL, BH
        Pop     DS
        Sub     AL, CL
        Pop     BP
        Inc     AL
        Ret     Size_Params
  AsciiPos      EndP
; * * * Function ConsistsOf (S: String; Symbols: CharSet): Boolean * * * * * *
        Consist_Str     EQU     DWord Ptr SS:[BX + 8]
        Consist_Chars   EQU     DWord Ptr SS:[BX + 4]
        Size_Params     =       8
  ConsistsOf    PROC    FAR
        Mov     BX, SP
        Mov     DX, DS
        LDS     SI, Consist_Str
        Mov     AL, [SI]
        Inc     SI
        Or      AL, AL
        Jz      @@Exit
        LES     DI, Consist_Chars
        Xor     BH, BH
  @@Loop:
        Mov     BL, [SI]
        Inc     SI
        Mov     CL, BL
        Shr     BL, 3
        And     CL, 7
        Mov     BL, ES:[BX + DI]
        Inc     CL
        Shr     BL, CL
        Jnc     @@WrongChar
        Dec     AL
        Jnz     @@Loop
        Mov     AL, 1
  @@Exit:
        Mov     DS, DX
        Ret     Size_Params
  @@WrongChar:
        Mov     DS, DX
        Xor     AL, AL
        Ret     Size_Params
  ConsistsOf    EndP
; * * * Function StrContains (S: String; Symbols: CharSet): Boolean * * * * * *
        Contains_Str    EQU     DWord Ptr SS:[BX + 8]
        Contains_Chars  EQU     DWord Ptr SS:[BX + 4]
        Size_Params     =       8
  StrContains   PROC    FAR
        Mov     BX, SP
        Mov     DX, DS
        LDS     SI, Contains_Str
        Mov     AL, [SI]
        Inc     SI
        Or      AL, AL
        Jz      @@Exit
        LES     DI, Contains_Chars
        Xor     BH, BH
  @@Loop:
        Mov     BL, [SI]
        Inc     SI
        Mov     CL, BL
        Shr     BL, 3
        And     CL, 7
        Mov     BL, ES:[BX + DI]
        Inc     CL
        Shr     BL, CL
        Jc      @@Contains
        Dec     AL
        Jnz     @@Loop
  @@Exit:
        Mov     DS, DX
        Ret     Size_Params
  @@Contains:
        Mov     DS, DX
        Mov     AL, 1
        Ret     Size_Params
  StrContains   EndP
; * * * Function DelChars (Ch: CharSet; S: String): String * * * * * * * * * *
        DelCh_Result    EQU     DWord Ptr [BP + 14]
        DelCh_Chars     EQU     DWord Ptr [BP + 10]
        DelCh_Str       EQU     DWord Ptr [BP + 6]
        Size_Params     =       8
  DelChars      PROC    FAR
        Push    BP
        Mov     BP, SP
        Push    DS
        LDS     SI, DelCh_Str
        LES     DI, DelCh_Result
        Mov     AL, [SI]
        Inc     SI
        Or      AL, AL
        Jz      @@ZeroStr
        Mov     DX, ES
        Mov     CX, DI
        LES     DI, DelCh_Chars
        Push    CX
        Inc     CX
        Xor     BH, BH
        Mov     BP, CX
  @@Loop:
        Mov     BL, [SI]
        Inc     SI
        Mov     CL, BL
        Mov     AH, BL
        Shr     BL, 3
        And     CL, 7
        Mov     BL, ES:[BX + DI]
        Inc     CL
        Shr     BL, CL
        Jc      @@SkipChar
        Mov     CX, ES
        Mov     ES, DX
        Mov     ES:[BP], AH
        Mov     ES, CX
        Inc     BP
  @@SkipChar:
        Dec     AL
        Jnz     @@Loop
        Mov     AX, BP
        Pop     BX
        Mov     ES, DX
        Sub     AX, BX
        Pop     DS
        Dec     AX
        Pop     BP
        Mov     ES:[BX], AL
        Ret     Size_Params
  @@ZeroStr:
        Pop     DS
        Pop     BP
        Mov     ES:[DI], AL
        Ret     Size_Params
  DelChars      EndP
; * * * Procedure Str2Set (Const S: String; Var CS: CharSet) * * * * * * * * *
        Str2S_Str       EQU     DWord Ptr SS:[BX + 8]
        Str2S_Set       EQU     DWord Ptr SS:[BX + 4]
        Size_Params     =       8
  Str2Set       PROC    FAR
        Mov     BX, SP
        Mov     DX, DS
        LES     DI, Str2S_Set
        LDS     SI, Str2S_Str
        Mov     BX, DI
        Mov     CX, 16
        Xor     AX, AX
        CLD
        Rep     StoSW
        Mov     CH, [SI]
        Mov     DI, BX
        Inc     SI
        Xor     BH, BH
        Or      CH, CH
        Jz      @@Exit
  @@Loop:
        Mov     BL, [SI]
        Inc     SI
        Mov     CL, BL
        Mov     AL, 1
        Shr     BL, 3
        And     CL, 7
        Shl     AL, CL
        Or      ES:[BX + DI], AL
        Dec     CH
        Jnz     @@Loop
  @@Exit:
        Mov     DS, DX
        Ret     Size_Params
  Str2Set       EndP
; * * * Function StrCompare (S1, S2: String): Integer * * * * * * * * * * * * *
        StrC_Str1       EQU     DWord Ptr SS:[BX + 8]
        StrC_Str2       EQU     DWord Ptr SS:[BX + 4]
        Size_Params     =       8
  StrCompare    PROC    FAR
        Mov     BX, SP
        Mov     DX, DS
        CLD
        LDS     SI, StrC_Str1
        LES     DI, StrC_Str2
        Mov     AL, [SI]
        Inc     SI
        Mov     AH, ES:[DI]
        Mov     CL, AL
        Inc     DI
        Cmp     CL, AH
        Jna     @@LenOK
        Mov     CL, AH
  @@LenOK:
        Xor     CH, CH
        Rep     CmpSB
        Jz      @@CalcResult
        Mov     AL, [SI - 1]
        Mov     AH, ES:[DI - 1]
  @@CalcResult:
        Sub     AL, AH
        Sbb     AH, AH
        Mov     DS, DX
        Ret     Size_Params
  StrCompare    EndP
; * * * Procedure SetLength (Var S: String; Len: Byte) * * * * * * * * * * * *
        SetL_Str        EQU     DWord Ptr SS:[BX + 6]
        SetL_Len        EQU     Byte Ptr SS:[BX + 4]
        Size_Params     =       6
  SetLength     PROC    FAR
        Mov     BX, SP
        LES     DI, SetL_Str
        Mov     AL, SetL_Len
        Mov     ES:[DI], AL
        Ret     Size_Params
  SetLength     EndP
; * * * Procedure SetString (Var Source, Dest; Count: Byte) * * * * * * * * * *
        SetS_Source     EQU     DWord Ptr SS:[BX + 10]
        SetS_Dest       EQU     DWord Ptr SS:[BX + 6]
        SetS_Count      EQU     Byte Ptr SS:[BX + 4]
        Size_Params     =       10
  SetString     PROC    FAR
        Mov     BX, SP
        Mov     AX, DS
        CLD
        LDS     SI, SetS_Source
        LES     DI, SetS_Dest
        Mov     CL, SetS_Count
        Xor     CH, CH
        Mov     ES:[DI], CL
        Inc     DI
        Shr     CX, 1
        Jnc     @@WordMove
        MovSB
  @@WordMove:
        Rep     MovSW
        Mov     DS, AX
        Ret     Size_Params
  SetString     EndP
; * * * Function FlagsValid (UserFlags, NeedFlags: String): Boolean * * * * * *
        FlagsV_UserStr  EQU     DWord Ptr SS:[BX + 8]
        FlagsV_NeedStr  EQU     DWord Ptr SS:[BX + 4]
        Size_Params     =       8
  FlagsValid    PROC    FAR
        Mov     BX, SP
        Push    DS
        LDS     SI, FlagsV_NeedStr
        Mov     DL, [SI]
        Inc     SI
        And     DX, 0FFh
        Jz      @@Valid
        CLD
        LES     DI, FlagsV_UserStr
        Mov     AH, ES:[DI]
        Inc     DI
        Xor     CH, CH
        Mov     BX, DI
  @@Loop:
        Mov     AL, [SI]
        Inc     SI
        Cmp     AL, 'Z'
        Ja      @@CheckInverted
        Or      AH, AH
        Jz      @@NotValid
        Mov     CL, AH
        RepNZ   ScaSB
        Jnz     @@NotValid
  @@Again:
        Mov     DI, BX
  @@ContinueLoop:
        Dec     DX
        Jnz     @@Loop
  @@Valid:
        Pop     DS
        Mov     AL, 1
        Ret     Size_Params
  @@CheckInverted:
        Or      AH, AH
        Jz      @@ContinueLoop
        Sub     AL, 32
        Mov     CL, AH
        RepNZ   ScaSB
        Jnz     @@Again
  @@NotValid:
        Pop     DS
        Xor     AL, AL
        Ret     Size_Params
  FlagsValid    EndP
; * * * Function MaskMatch (Var Str; Var Mask): Boolean * * * * * * * * * * * *
        MaskM_Str       EQU     DWord Ptr [BP + 10]
        MaskM_Mask      EQU     DWord Ptr [BP + 6]
        Size_Params     =       8
  MaskMatch     PROC    FAR
        Push    BP
        Mov     BP, SP
        Push    DS
        LDS     SI, MaskM_Str
        LES     DI, MaskM_Mask
        Xor     BX, BX
        Xor     CX, CX
  @@MatchLoop:
        Mov     AL, ES:[DI]           ; char from mask
        Mov     AH, [SI]              ; char from string
        Test    AL, AL
        Jz      @@CheckStr            ; end of mask
        Cmp     AL, '*'
        Jz      @@CheckAster
        Or      AH, AH
        Jz      @@NotMatch            ; mask not over, but string done
        Inc     SI
        Inc     DI
        Cmp     AL, '?'
        Jz      @@MatchLoop           ; ok, continue
        Mov     BL, AL
        Mov     AL, CS:UpTable + BX
        Mov     BL, AH
        Cmp     AL, CS:UpTable + BX
        Jz      @@MatchLoop
        Or      CX, CX
        Jnz     @@ContinueAster
  @@NotMatch:
        Pop     DS
        Pop     BP
        Xor     AL, AL
        Ret     Size_Params
  @@CheckStr:
        Or      AH, AH                ; string also over?
        Jz      @@Match
        Or      CX, CX
        Jz      @@Exit
  @@ContinueAster:                    ; if checks after '*', try to continue
        Mov     DI, DX
        Mov     SI, BP
        Mov     AL, ES:[DI]           ; char from mask
        Mov     AH, [SI]              ; char from string
        Jmp     Short @@AsterSkipPrepare
  @@Match:
        Mov     AL, 1
  @@Exit:
        Pop     DS
        Pop     BP
        Ret     Size_Params
  @@CheckAster:
        Inc     DI
        Mov     AL, ES:[DI]           ; char from mask
        Or      AL, AL
        Jz      @@Match
        Cmp     AL, '*'
        Jz      @@CheckAster
  @@AsterSkipPrepare:
        Or      AH, AH
        Jz      @@NotMatch            ; mask not over, but string done
        Inc     SI
        Cmp     AL, '?'
        Jz      @@AsterMatch
        Mov     BL, AL
        Mov     AL, CS:UpTable + BX
  @@AsterSkipLoop:
        Mov     BL, AH
        Cmp     AL, CS:UpTable + BX
        Jz      @@AsterMatch
        Mov     AH, [SI]              ; char from string
        Inc     SI
        Test    AH, AH
        Jnz     @@AsterSkipLoop
        Jmp     Short @@NotMatch      ; mask not over, but string done
  @@AsterMatch:
        Mov     DX, DI
        Mov     CX, 1
        Mov     BP, SI
        Inc     DI
        Jmp     @@MatchLoop
  MaskMatch     EndP
; * * * Function XLatStr (S: String; Var Table): String * * * * * * * * * * * *
        XlatStr_Result  EQU     DWord Ptr [BP + 14]
        XlatStr_Str     EQU     DWord Ptr [BP + 10]
        XlatStr_TablSeg EQU     Word Ptr [BP + 8]
        XlatStr_TablOfs EQU     Word Ptr [BP + 6]
        Size_Params     =       8
  XlatStr       PROC    FAR
        Push    BP
        Mov     BP, SP
        Push    DS
        LDS     SI, XlatStr_Str
        LES     DI, XlatStr_Result
        Mov     CL, [SI]
        Mov     ES:[DI], CL
        And     CX, 0FFh
        Jz      @@Exit
        Mov     DX, XlatStr_TablSeg
        Mov     AX, XlatStr_TablOfs
        Mov     BP, DI
        Xor     BX, BX
        Mov     DI, AX
        Mov     AX, ES
  @@Loop:
        Inc     SI
        Inc     BP
        Mov     BL, [SI]
        Mov     ES, DX
        Mov     BL, ES:[BX + DI]
        Mov     ES, AX
        Mov     ES:[BP], BL
        Loop    @@Loop
  @@Exit:
        Pop     DS
        Pop     BP
        Ret     Size_Params
  XlatStr       EndP
; * * * Function HexB (B: Byte): String * * * * * * * * * * * * * * * * * * * *
        HexB_Result     EQU     DWord Ptr SS:[BX + 6]
        HexB_Byte       EQU     Byte Ptr SS:[BX + 4]
        Size_Params     =       2
  HexB          PROC    FAR
        Mov     BX, SP
        Mov     AL, HexB_Byte
        Mov     AH, AL
        And     AL, 0Fh
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
        LES     DI, HexB_Result
        Mov     Byte Ptr ES:[DI], 2
        Mov     ES:[DI + 1], AX
        Ret     Size_Params
  HexB          EndP
; * * * Function HexW (W: Word): String * * * * * * * * * * * * * * * * * * * *
        HexW_Result     EQU     DWord Ptr SS:[BX + 6]
        HexW_Word       EQU     Word Ptr SS:[BX + 4]
        Size_Params     =       2
  HexW          PROC    FAR
        Mov     BX, SP
        LES     DI, HexW_Result
        Mov     BX, HexW_Word
        Mov     AL, BH
        Shr     AL, 4
        Add     AL, 90h
        DAA
        Adc     AL, 40h
        DAA
        Mov     AH, AL
        Mov     AL, 4
        Mov     ES:[DI], AX
        Mov     AL, BL
        Shr     AL, 4
        Add     AL, 90h
        DAA
        Adc     AL, 40h
        DAA
        Mov     AH, AL
        Mov     AL, BH
        And     AL, 0Fh
        Add     AL, 90h
        DAA
        Adc     AL, 40h
        DAA
        Mov     ES:[DI + 2], AX
        Mov     AL, BL
        And     AL, 0Fh
        Add     AL, 90h
        DAA
        Adc     AL, 40h
        DAA
        Mov     ES:[DI + 4], AL
        Ret     Size_Params
  HexW          EndP
; * * * Function HexL (L: LongInt): String * * * * * * * * * * * * * * * * * *
        HexL_Result     EQU     DWord Ptr SS:[BX + 8]
        HexL_HiWord     EQU     Word Ptr SS:[BX + 6]
        HexL_LoWord     EQU     Word Ptr SS:[BX + 4]
        Size_Params     =       4
  HexL          PROC    FAR
        Mov     BX, SP
        LES     DI, HexL_Result
        Mov     CX, HexL_LoWord
        Mov     BX, HexL_HiWord
        Mov     AL, BH
        Shr     AL, 4
        Add     AL, 90h
        DAA
        Adc     AL, 40h
        DAA
        Mov     AH, AL
        Mov     AL, 8
        Mov     ES:[DI], AX
        Mov     AL, BL
        Shr     AL, 4
        Add     AL, 90h
        DAA
        Adc     AL, 40h
        DAA
        Mov     AH, AL
        Mov     AL, BH
        And     AL, 0Fh
        Add     AL, 90h
        DAA
        Adc     AL, 40h
        DAA
        Mov     ES:[DI + 2], AX
        Mov     AL, CH
        Shr     AL, 4
        Add     AL, 90h
        DAA
        Adc     AL, 40h
        DAA
        Mov     AH, AL
        Mov     AL, BL
        And     AL, 0Fh
        Add     AL, 90h
        DAA
        Adc     AL, 40h
        DAA
        Mov     ES:[DI + 4], AX
        Mov     AL, CL
        Shr     AL, 4
        Add     AL, 90h
        DAA
        Adc     AL, 40h
        DAA
        Mov     AH, AL
        Mov     AL, CH
        And     AL, 0Fh
        Add     AL, 90h
        DAA
        Adc     AL, 40h
        DAA
        Mov     ES:[DI + 6], AX
        Mov     AL, CL
        And     AL, 0Fh
        Add     AL, 90h
        DAA
        Adc     AL, 40h
        DAA
        Mov     ES:[DI + 8], AL
        Ret     Size_Params
  HexL          EndP
; * * * Procedure FillLong (Var Dest; Count: Word; Filler: LongInt) * * * * * *
        FilL_Dest       EQU     DWord Ptr SS:[BX + 10]
        FilL_Count      EQU     Word Ptr  SS:[BX + 8]
        FilL_FillerHI   EQU     Word Ptr  SS:[BX + 6]
        FilL_FillerLO   EQU     Word Ptr  SS:[BX + 4]
        Size_Params     =       10
  FillLong      PROC    FAR
        Mov     BX, SP
        CLD
        LES     DI, FilL_Dest
        Mov     CX, FilL_Count
        Mov     AX, FilL_FillerLO
        Test    CX, CX
        Jz      @@Exit
        Mov     BX, FilL_FillerHI
  @@Loop:
        Mov     ES:[DI], AX
        Mov     ES:[DI + 2], BX
        Add     DI, 4
        Dec     CX
        Jnz     @@Loop
  @@Exit:
        Ret     Size_Params
  FillLong      EndP
; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;               DS:SI   Hex string
; Returns       DX:AX   LongInt
;
  Hex2L PROC    NEAR
        Mov     CL, [SI]
        Xor     BX, BX
        Inc     SI
        Xor     AX, AX
        Xor     DX, DX
        Test    CL, CL
        Jz      @@Exit
  @@Loop:
        Mov     CH, AH
        Mov     BL, [SI]
        Shr     CH, 4
        Inc     SI
        Shl     AX, 4
        Cmp     BL, '9'
        Ja      @@CheckUpcase
        Sub     BL, '0'
        Js      @@Zero
        Shl     DX, 4
        Or      AL, BL
        Or      DL, CH
        Dec     CL
        Jnz     @@Loop
  @@Exit:
        Ret
  @@CheckUpcase:
        Mov     BL, CS:UpTable + BX
        Sub     BL, 'A'
        Js      @@Zero
        Cmp     BL, 5
        Ja      @@Zero
        Add     BL, 10
  @@Done:
        Shl     DX, 4
        Or      AL, BL
        Or      DL, CH
        Dec     CL
        Jnz     @@Loop
        Ret
  @@Zero:
        Xor     BL, BL
        Jmp     Short @@Done
  Hex2L EndP
; * * * Function Hex2Byte (Const S: String): Byte * * * * * * * * * * * * * * *
        Hex2B_Str       EQU     DWord Ptr SS:[BX + 4]
        Size_Params     =       4
  Hex2Byte      PROC    FAR
        Mov     BX, SP
        Push    DS
        LDS     SI, Hex2B_Str
        Call    Hex2L
        Pop     DS
        Ret     Size_Params
  Hex2Byte      EndP
; * * * Function Hex2Word (Const S: String): Word * * * * * * * * * * * * * * *
        Hex2W_Str       EQU     DWord Ptr SS:[BX + 4]
        Size_Params     =       4
  Hex2Word      PROC    FAR
        Mov     BX, SP
        Push    DS
        LDS     SI, Hex2W_Str
        Call    Hex2L
        Pop     DS
        Ret     Size_Params
  Hex2Word      EndP
; * * * Function Hex2Long (Const S: String): LongInt * * * * * * * * * * * * *
        Hex2L_Str       EQU     DWord Ptr SS:[BX + 4]
        Size_Params     =       4
  Hex2Long      PROC    FAR
        Mov     BX, SP
        Push    DS
        LDS     SI, Hex2L_Str
        Call    Hex2L
        Pop     DS
        Ret     Size_Params
  Hex2Long      EndP
; * * * Function NPos (Const SubStr, S: String; FromPos: Byte): Byte * * * * *
        NPos_Sub        EQU     DWord Ptr [BP + 12]
        NPos_S          EQU     DWord Ptr [BP + 8]
        NPos_SOfs       EQU     Word Ptr  [BP + 8]
        NPos_From       EQU     Byte Ptr  [BP + 6]
        Size_Params     =       10
  NPos  PROC    FAR
        Push    BP
        Mov     BP, SP
        Push    DS
        Push    ES
        Push    BX
        Push    SI
        Push    DI
        LDS     SI, NPos_Sub
        Xor     DX, DX
        LES     DI, NPos_S
        Xor     CX, CX
        Mov     DL, [SI]
        Xor     AX, AX
        Mov     CL, ES:[DI]
        Xor     BX, BX
        Test    DL, DL
        Jz      @@NotFound
        Test    CL, CL
        Jz      @@NotFound
        Dec     DX
        Mov     BL, NPos_From
        Mov     AL, DL
        Dec     BX
        Inc     SI
        Sub     CX, BX
        Jng     @@NotFound
        Add     DI, BX
        Sub     CX, AX
        Jng     @@NotFound
        Inc     DI
        CLD
  @@FirstChar:
        Mov     AL, [SI]
        RepNE   ScaSB
        Jz      @@CheckRest
  @@NotFound:
        Pop     DI
        Pop     SI
        Pop     BX
        Pop     ES
        Pop     DS
        Pop     BP
        Xor     AX, AX
        Ret     Size_Params
  @@CheckRest:
        Test    DX, DX
        Jz      @@CharFound
        Mov     BX, DI
        Mov     AX, CX
        Push    SI
        Mov     CX, DX
        Inc     SI
        Shr     CX, 1
        RepE    CmpSW
        Mov     CX, DX
        Jne     @@Again
        And     CX, 1
        RepE    CmpSB
        Je      @@StrFound
  @@Again:
        Pop     SI
        Mov     DI, BX
        Mov     CX, AX
        Jmp     Short @@FirstChar
  @@StrFound:
        Pop     SI
        Mov     DI, BX
  @@CharFound:
        Mov     AX, DI
        Sub     AX, NPos_SOfs
        Pop     DI
        Pop     SI
        Pop     BX
        Pop     ES
        Pop     DS
        Pop     BP
        Dec     AX
        Ret     Size_Params
  NPos  EndP
; * * * Function PosBM (Const S, Buf: String): Byte * * * * * * * * * * * * * *
;        PosBM_Str       EQU     DWord Ptr [BP + 10]
;        PosBM_StrSeg    EQU     Word Ptr  [BP + 12]
;        PosBM_StrOfs    EQU     Word Ptr  [BP + 10]
;        PosBM_Buf       EQU     DWord Ptr [BP + 6]
;        Size_Params     =       8
;
;        BM_TableSize    EQU     256
;        PosBM_Table     EQU     [BP - 2 - BM_TableSize]
;  PosBM         PROC    FAR
;        Push    BP
;        Mov     BP, SP
;        Push    DS
;        LDS     SI, PosBM_Str
;        LES     DI, PosBM_Buf
;        Mov     AL, [SI]
;        Mov     DL, ES:[DI]
;        Inc     SI
;        Xor     DH, DH
;        And     AX, 0FFh
;        Jz      @@Exit
;        Cmp     AX, DX
;        Ja      @@Not_Match
;        Cmp     AL, 1
;        Ja      @@Do_Boyer_Moore
;        Inc     DI
;        Mov     AL, [SI]
;        Mov     CX, DX
;        Mov     SI, DI
;        CLD
;        RepNZ   ScaSB
;        Jnz     @@Not_Match
;        Pop     DS
;        Mov     AX, DI
;        Pop     BP
;        Sub     AX, SI
;        Ret     Size_Params
;  @@Not_Match:
;        Xor     AX, AX
;  @@Exit:
;        Pop     DS
;        Pop     BP
;        Ret     Size_Params
;  @@Do_Boyer_Moore:
;        Sub     SP, BM_TableSize
;        Mov     CX, SS
;        Mov     ES, CX
;        Lea     DI, PosBM_Table
;        Mov     AH, AL
;        Mov     CX, BM_TableSize / 2
;        CLD
;        Rep     StoSW
;        Xor     BX, BX
;        Lea     DI, PosBM_Table
;        Mov     CL, AL
;  @@Fill_Table:
;        Mov     BL, [SI]
;        Inc     SI
;        Dec     CX
;        Mov     ES:[BX + DI], CL
;        Jnz     @@Fill_Table
;        STD
;        LDS     SI, PosBM_Buf
;        Mov     ES, PosBM_StrSeg
;        Xor     AH, AH
;        Inc     SI
;        Mov     BX, AX
;        Dec     DX
;        Dec     BX
;        Push    SI
;  @@Search_Last:
;        Sub     DX, BX
;        Jc      @@Not_BM_Match
;        Add     SI, BX
;        Mov     BL, [SI]
;        Mov     BL, SS:[BX + DI]
;        Test    BL, BL
;        Jnz     @@Search_Last
;        Mov     DI, PosBM_StrOfs
;        Mov     CX, AX
;        Mov     BX, SI
;        Add     DI, AX
;        RepZ    CmpSB
;        Jz      @@Exact_BM_Match
;        Mov     SI, BX
;        Lea     DI, PosBM_Table
;        Mov     BX, 1
;        Jmp     Short @@Search_Last
;  @@Exact_BM_Match:
;        CLD
;        Pop     BX
;        Mov     AX, SI
;        Add     SP, BM_TableSize
;        Sub     AX, BX
;        Pop     DS
;        Add     AX, 2
;        Pop     BP
;        Ret     Size_Params
;  @@Not_BM_Match:
;        CLD
;        Add     SP, BM_TableSize + 2
;        Xor     AX, AX
;        Pop     DS
;        Pop     BP
;        Ret     Size_Params
;  PosBM         EndP
  CODE  EndS
  END
