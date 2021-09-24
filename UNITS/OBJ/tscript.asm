; * * * * * * * * * * * * * * * DATA * * * * * * * * * * * * * * * * * * * * *
;  DATA  SEGMENT WORD PUBLIC
;        ASSUME  DS:DATA
;  DATA  EndS
; * * * * * * * * * * * * * * * CODE * * * * * * * * * * * * * * * * * * * * *
  .286
  LOCALS
  CODE  SEGMENT PUBLIC
        ASSUME  CS:CODE

        PUBLIC  GetParams
; * * * Function GetParams (Const S: String): String * * * * * * * * * * * * *
        GetP_Result     EQU     DWord Ptr SS:[BX + 8]
        GetP_Str        EQU     DWord Ptr SS:[BX + 4]
        Size_Params     =       4
  GetParams     PROC    FAR
        Mov     BX, SP
        Push    DS
        LDS     SI, GetP_Str
        LES     DI, GetP_Result
        Mov     CL, [SI]
        Inc     SI
        And     CX, 0FFh
        Jz      @@EmptyStr
  @@FindFirstBracket:
        Mov     AL, [SI]
        Inc     SI
        Cmp     AL, '('
        Jz      @@GotFirstBracket
        Loop    @@FindFirstBracket
  @@EmptyStr:
        Pop     DS
        Mov     Byte Ptr ES:[DI], 0
        Ret     Size_Params
  @@GotFirstBracket:
        Dec     CX
        Jz      @@EmptyStr
  @@SkipLeadSpaces:
        Mov     AL, [SI]
        Inc     SI
        Cmp     AL, 32
        Ja      @@GotFirstChar
        Loop    @@SkipLeadSpaces
        Jmp     Short @@EmptyStr
  @@GotFirstChar:
        Mov     BX, SI            ; StartCount + 1
        Xor     AH, AH            ; BracketCounter
        Dec     BX
        Mov     DX, BX            ; EndCount + 1
        Jmp     Short @@SkipLoading
  @@MainLoop:
        Mov     AL, [SI]
        Inc     SI
  @@SkipLoading:
        Cmp     AL, ')'
        Jz      @@ProcessCloseBracket
        Cmp     AL, '('
        Jz      @@ProcessOpenBracket
        Cmp     AL, '"'
        Jz      @@ProcessQuote
        Cmp     AL, 32
        Jna     @@ContinueMainLoop
  @@SetEndPtr:
        Mov     DX, SI
  @@ContinueMainLoop:
        Loop    @@MainLoop
        Jmp     Short @@EndReached
  @@ProcessOpenBracket:
        Inc     AH
        Jmp     Short @@SetEndPtr
  @@ProcessQuote:
        Dec     CX
        Jz      @@QuoteSkipped
  @@SkipQuoteLoop:
        Mov     AL, [SI]
        Inc     SI
        Cmp     AL, '"'
        Jz      @@SetEndPtr
        Loop    @@SkipQuoteLoop
  @@QuoteSkipped:
        Mov     DX, SI
        Jmp     Short @@EndReached
  @@ProcessCloseBracket:
        Dec     AH
        Jns     @@SetEndPtr
  @@EndReached:
        Sub     DX, BX
        Mov     SI, BX
        Mov     ES:[DI], DL
        Mov     CX, DX
        Inc     DI
        Shr     CX, 1
        CLD
        Jnc     @@EvenMove
        MovSB
  @@EvenMove:
        Rep     MovSW
        Pop     DS
        Ret     Size_Params
  GetParams     EndP
  CODE  EndS
  END
