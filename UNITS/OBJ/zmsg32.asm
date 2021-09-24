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

        PUBLIC  MainComm@ZeroMsg, MainComm@ZeroBackground

; Function ZeroMsg (Param: String; CR: Boolean): String
        ZMsg_Result     EQU     DWord Ptr [ESP + 24]
        ZMsg_Str        EQU     DWord Ptr [ESP + 20]
        ZMsg_CR         EQU     Byte Ptr [ESP + 16]
        Size_Params     =       8

        ALIGN   4
MainComm@ZeroMsg        PROC    NEAR
        Push    EBX
        Push    ESI
        Push    EDI
        Mov     ESI, ZMsg_Str
        Xor     ECX, ECX
        Mov     EDI, ZMsg_Result
        Mov     CL, [ESI]
        Xor     EAX, EAX
        Test    CL, CL
        Jz      @@Skip
        Inc     ESI
        Mov     DL, ZMsg_CR
        Push    EDI
        Inc     EDI
  @@Loop:
        Mov     AL, [ESI]
        Inc     ESI
        Cmp     AL, '\'
        Jz      @@CheckColor
        Cmp     AL, '%'
        Jz      @@CheckColor
        Cmp     AL, '|'
        Jz      @@CheckReturn
  @@StoreChar:
        Mov     [EDI], AL
        Inc     EDI
        Inc     AH
        Dec     ECX
        Jnz     @@Loop
        Jmp     Short @@Done
  @@CheckColor:
        Cmp     ECX, 3
        Jb      @@StoreChar
        Mov     BX, [ESI]
        Cmp     BH, '0'
        Jb      @@StoreChar
        Cmp     BL, '0'
        Jb      @@StoreChar
        Ja      @@CheckMore
        Cmp     BH, '9'
        Ja      @@StoreChar
  @@RemoveColor:
        Add     ESI, 2
        Sub     ECX, 3
        Jnz     @@Loop
        Jmp     Short @@Done
  @@CheckMore:
        Cmp     BL, '1'
        Ja      @@StoreChar
        Cmp     BH, '5'
        Jna     @@RemoveColor
        Jmp     Short @@StoreChar
  @@CheckReturn:
        Cmp     DL, 0
        Jz      @@StoreChar
        Dec     ECX
        Jnz     @@Loop
  @@Done:
        Pop     EDI
  @@Skip:
        Mov     [EDI], AH
        Pop     EDI
        Pop     ESI
        Pop     EBX
        Ret     Size_Params
MainComm@ZeroMsg        EndP

; Function ZeroBackGround (Param: String): String
        ZBck_Result     EQU     DWord Ptr [ESP + 20]
        ZBck_Str        EQU     DWord Ptr [ESP + 16]
        Size_Params     =       4

        ALIGN   4
MainComm@ZeroBackground PROC    NEAR
        Push    EBX
        Push    ESI
        Push    EDI
        Mov     ESI, ZBck_Str
        Xor     ECX, ECX
        Mov     EDI, ZBck_Result
        Mov     CL, [ESI]
        Xor     EAX, EAX
        Test    CL, CL
        Jz      @@Skip
        Inc     ESI
        Push    EDI
        Inc     EDI
  @@Loop:
        Mov     AL, [ESI]
        Inc     ESI
        Cmp     AL, '%'
        Jz      @@CheckColor
        Cmp     AL, '|'
        Jz      @@RemoveReturn
  @@StoreChar:
        Mov     [EDI], AL
        Inc     EDI
        Inc     AH
  @@RemoveReturn:
        Dec     ECX
        Jnz     @@Loop
        Jmp     Short @@Done
  @@CheckColor:
        Cmp     ECX, 3
        Jb      @@StoreChar
        Mov     BX, [ESI]
        Cmp     BH, '0'
        Jb      @@StoreChar
        Cmp     BL, '0'
        Jb      @@StoreChar
        Ja      @@CheckMore
        Cmp     BH, '9'
        Ja      @@StoreChar
  @@RemoveColor:
        Add     ESI, 2
        Sub     ECX, 3
        Jnz     @@Loop
        Jmp     Short @@Done
  @@CheckMore:
        Cmp     BL, '1'
        Ja      @@StoreChar
        Cmp     BH, '5'
        Jna     @@RemoveColor
        Jmp     Short @@StoreChar
  @@Done:
        Pop     EDI
  @@Skip:
        Mov     [EDI], AH
        Pop     EDI
        Pop     ESI
        Pop     EBX
        Ret     Size_Params
MainComm@ZeroBackground EndP

CODE32  ENDS

END
