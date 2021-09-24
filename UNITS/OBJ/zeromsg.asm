; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
        TRUE                    EQU     1
        FALSE                   EQU     0
; * * * * * * * * * * * * * * * DATA * * * * * * * * * * * * * * * * * * * * *
;  DATA  SEGMENT WORD PUBLIC
;        ASSUME  DS:DATA
;  DATA  EndS
; * * * * * * * * * * * * * * * CODE * * * * * * * * * * * * * * * * * * * * *
  LOCALS
  CODE  SEGMENT BYTE PUBLIC
        ASSUME  CS:CODE
        PUBLIC  ZeroMsg, ZeroBackGround
; * * * Function ZeroMsg (S: String; CR: Boolean): String * * * * * * * * * * *
        ZMsg_Result     EQU     DWord Ptr SS:[BX + 10]
        ZMsg_Str        EQU     DWord Ptr SS:[BX + 6]
        ZMsg_CR         EQU     Byte Ptr SS:[BX + 4]
        Size_Params     =       6
  ZeroMsg       PROC    FAR
        Mov     BX, SP
        Push    DS
        LDS     SI, ZMsg_Str
        LES     DI, ZMsg_Result
        Mov     CL, [SI]
        Xor     AX, AX
        And     CX, 0FFh
        Jz      @@Skip
        Mov     DL, ZMsg_CR
        Inc     SI
        Push    DI
        Inc     DI
  @@Loop:
        Mov     AL, [SI]
        Inc     SI
        Cmp     AL, '\'
        Jz      @@CheckColor
        Cmp     AL, '%'
        Jz      @@CheckColor
        Cmp     AL, '|'
        Jz      @@CheckReturn
  @@StoreChar:
        Mov     ES:[DI], AL
        Inc     DI
        Inc     AH
        Loop    @@Loop
  @@Done:
        Pop     DI
  @@Skip:
        Mov     ES:[DI], AH
        Pop     DS
        Ret     Size_Params
  @@CheckColor:
        Cmp     CX, 3
        Jb      @@StoreChar
        Mov     BX, [SI]
        Cmp     BH, '0'
        Jb      @@StoreChar
        Cmp     BL, '0'
        Jb      @@StoreChar
        Ja      @@CheckMore
        Cmp     BH, '9'
        Ja      @@StoreChar
  @@RemoveColor:
        Add     SI, 2
        Sub     CX, 3
        Jnz     @@Loop
        Jmp     Short @@Done
  @@CheckMore:
        Cmp     BL, '1'
        Ja      @@StoreChar
        Cmp     BH, '5'
        Jna     @@RemoveColor
        Jmp     Short @@StoreChar
  @@CheckReturn:
        Cmp     DL, TRUE
        Jnz     @@StoreChar
        Loop    @@Loop
        Jmp     Short @@Done
  ZeroMsg       EndP
; * * * Function ZeroBackGround (S: String): String * * * * * * * * * * * * * *
        ZBck_Result     EQU     DWord Ptr SS:[BX + 8]
        ZBck_Str        EQU     DWord Ptr SS:[BX + 4]
        Size_Params     =       4
  ZeroBackGround       PROC    FAR
        Mov     BX, SP
        Push    DS
        LDS     SI, ZBck_Str
        LES     DI, ZBck_Result
        Mov     CL, [SI]
        Xor     AX, AX
        And     CX, 0FFh
        Jz      @@Skip
        Inc     SI
        Push    DI
        Inc     DI
  @@Loop:
        Mov     AL, [SI]
        Inc     SI
        Cmp     AL, '%'
        Jz      @@CheckColor
        Cmp     AL, '|'
        Jz      @@RemoveReturn
  @@StoreChar:
        Mov     ES:[DI], AL
        Inc     DI
        Inc     AH
  @@RemoveReturn:
        Loop    @@Loop
  @@Done:
        Pop     DI
  @@Skip:
        Mov     ES:[DI], AH
        Pop     DS
        Ret     Size_Params
  @@CheckColor:
        Cmp     CX, 3
        Jb      @@StoreChar
        Mov     BX, [SI]
        Cmp     BH, '0'
        Jb      @@StoreChar
        Cmp     BL, '0'
        Jb      @@StoreChar
        Ja      @@CheckMore
        Cmp     BH, '9'
        Ja      @@StoreChar
  @@RemoveColor:
        Add     SI, 2
        Sub     CX, 3
        Jnz     @@Loop
        Jmp     Short @@Done
  @@CheckMore:
        Cmp     BL, '1'
        Ja      @@StoreChar
        Cmp     BH, '5'
        Jna     @@RemoveColor
        Jmp     Short @@StoreChar
  ZeroBackGround       EndP
  CODE  EndS
  END
