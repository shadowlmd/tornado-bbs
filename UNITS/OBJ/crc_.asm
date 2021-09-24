; * * * * * * * * * * * * * * * DATA * * * * * * * * * * * * * * * * * * * * *
  DATA  SEGMENT WORD PUBLIC
        ASSUME  DS:DATA

        EXTRN   CrcTable   : Word;
        EXTRN   Crc32Table : DWord;
  DATA  EndS
; * * * * * * * * * * * * * * * CODE * * * * * * * * * * * * * * * * * * * * *
  .286
  LOCALS
  CODE  SEGMENT PUBLIC
        ASSUME  CS:CODE

        PUBLIC  UpdateCrc32, UpdateCrc, Crc32Str, UpdateCrc32Buf, UpdateCrcBuf
        PUBLIC  GetChecksum

; * * * Function UpdateCrc32 (CurByte: Byte; CurCrc: LongInt): LongInt * * * *
        Upd32_Byte      EQU     Byte Ptr SS:[BX + 8]
        Upd32_CrcH      EQU     Word Ptr SS:[BX + 6]
        Upd32_CrcL      EQU     Word Ptr SS:[BX + 4]
        Size_Params     =       6
  UpdateCrc32   PROC    FAR
        Mov     BX, SP
        Mov     AX, Upd32_CrcL
        Mov     DX, Upd32_CrcH
        Mov     BL, Upd32_Byte
        Lea     SI, Crc32Table
        Xor     BL, AL
        Mov     AL, AH
        Xor     BH, BH
        Mov     AH, DL
        Shl     BX, 2
        Shr     DX, 8
        Xor     AX, [BX + SI]
        Xor     DX, [BX + SI + 2]
        Ret     Size_Params
  UpdateCrc32   EndP
; * * * Function UpdateCrc (CurByte: Byte; CurCrc: Word): Word * * * * * * * *
        Upd16_Byte      EQU     Byte Ptr SS:[BX + 6]
        Upd16_Crc       EQU     Word Ptr SS:[BX + 4]
        Size_Params     =       4
  UpdateCrc     PROC    FAR
        Mov     BX, SP
        Mov     AL, Upd16_Byte
        Mov     BX, Upd16_Crc
        Mov     AH, BL
        Shr     BX, 8
        Shl     BX, 1
        Xor     AX, Word Ptr CrcTable + BX
        Ret     Size_Params
  UpdateCrc     EndP
; * * * Function Crc32Str (Const S: String): LongInt * * * * * * * * * * * * *
        Crc_Str         EQU     DWord Ptr SS:[BX + 4]
        Size_Params     =       4
  Crc32Str      PROC    FAR
        Mov     BX, SP
        LES     DI, Crc_Str
        Mov     AX, 0FFFFh
        Mov     CL, ES:[DI]
        Inc     DI
        Mov     DX, AX
        And     CX, 0FFh
        Jz      @@Exit
        Lea     SI, Crc32Table
  @@Loop:
        Mov     BL, ES:[DI] ; Crc := crc_32_tab[byte(crc xor longint(s[Cnt]))]
        Inc     DI          ; xor ((crc shr 8) and $00FFFFFF);
        Xor     BL, AL
        Mov     AL, AH
        Xor     BH, BH
        Mov     AH, DL
        Shl     BX, 2
        Shr     DX, 8
        Xor     AX, [BX + SI]
        Xor     DX, [BX + SI + 2]
        Dec     CX
        Jnz     @@Loop
  @@Exit:
        Ret     Size_Params
  Crc32Str      EndP
; * * * Function UpdateCrc32Buf (Var Buf; Len: Word; StartCrc: LongInt): LongInt *
        Crc32B_Buf      EQU     DWord Ptr SS:[BX + 10]
        Crc32B_Len      EQU     Word Ptr SS:[BX + 8]
        Crc32B_SCrcH    EQU     Word Ptr SS:[BX + 6]
        Crc32B_SCrcL    EQU     Word Ptr SS:[BX + 4]
        Size_Params     =       10
  UpdateCrc32Buf        PROC    FAR
        Mov     BX, SP
        Mov     CX, Crc32B_Len
        Mov     AX, Crc32B_SCrcL
        Mov     DX, Crc32B_SCrcH
        Or      CX, CX
        Jz      @@Exit
        LES     DI, Crc32B_Buf
        Lea     SI, Crc32Table
  @@Loop:
        Mov     BL, ES:[DI]
        Inc     DI
        Xor     BL, AL
        Mov     AL, AH
        Xor     BH, BH
        Mov     AH, DL
        Shl     BX, 2
        Shr     DX, 8
        Xor     AX, [BX + SI]
        Xor     DX, [BX + SI + 2]
        Dec     CX
        Jnz     @@Loop
  @@Exit:
        Ret     Size_Params
  UpdateCrc32Buf        EndP
; * * * Function UpdateCrcBuf (Var Buf; Len: Word; StartCrc: Word): Word * * *
        Crc16B_Buf      EQU     DWord Ptr SS:[BX + 8]
        Crc16B_Len      EQU     Word Ptr SS:[BX + 6]
        Crc16B_SCrc     EQU     Word Ptr SS:[BX + 4]
        Size_Params     =       8
  UpdateCrcBuf  PROC    FAR
        Mov     BX, SP
        Mov     CX, Crc16B_Len
        Mov     AX, Crc16B_SCrc
        Or      CX, CX
        Jz      @@Exit
        LES     DI, Crc16B_Buf
        Lea     SI, CrcTable
  @@Loop:
        Mov     BL, AH
        Mov     AH, AL
        Xor     BH, BH
        Mov     AL, ES:[DI]
        Shl     BX, 1
        Inc     DI
        Xor     AX, [BX + SI]
        Dec     CX
        Jnz     @@Loop
  @@Exit:
        Ret     Size_Params
  UpdateCrcBuf  EndP
; * * * Procedure GetChecksum (Const S: String; Var C: tCheckSum) * * * * * * *
        GetC_Str        EQU     DWord Ptr [BP + 10]
        GetC_Checksum   EQU     DWord Ptr [BP + 6]
        Size_Params     =       8
  GetChecksum   PROC    FAR
        Push    BP
        Mov     BP, SP
        LES     DI, GetC_Str
        Mov     AX, 0FFFFh
        Mov     CL, ES:[DI]
        Mov     DX, AX
        Inc     DI
        And     CX, 0FFh
        Jz      @@Exit
        Lea     SI, Crc32Table
        Mov     BP, CX
  @@Loop:
        Mov     BL, ES:[DI]
        Inc     DI
        Add     CH, BL
        Xor     BL, AL
        Mov     AL, AH
        Xor     BH, BH
        Mov     AH, DL
        Shl     BX, 2
        Shr     DX, 8
        Xor     AX, [BX + SI]
        Xor     DX, [BX + SI + 2]
        Dec     BP
        Jnz     @@Loop
  @@Exit:
        Mov     BP, SP
        LES     DI, GetC_Checksum
        Mov     ES:[DI], AX
        Mov     ES:[DI + 2], DX
        Pop     BP
        Mov     ES:[DI + 4], CX
        Ret     Size_Params
  GetChecksum   EndP
  CODE  EndS
  END
