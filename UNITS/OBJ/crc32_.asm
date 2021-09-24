.386P
LOCALS

CODE32          SEGMENT DWORD USE32 PUBLIC 'CODE'
CODE32          ENDS

CONST32         SEGMENT DWORD USE32 PUBLIC 'CONST'
CONST32         ENDS

DATA32          SEGMENT DWORD USE32 PUBLIC 'DATA'
        EXTRN   Crc@CrcTable    : Word;
        EXTRN   Crc@Crc32Table  : DWord;
DATA32          ENDS

TLS             SEGMENT DWORD USE32 PUBLIC 'TLS'
TLS             ENDS

BSS32           SEGMENT DWORD USE32 PUBLIC 'BSS'
BSS32           ENDS

DGROUP          GROUP CONST32, DATA32, TLS, BSS32
                .MODEL FLAT

CODE32          SEGMENT
                ASSUME CS:FLAT, DS:FLAT, SS:FLAT, ES:FLAT

        PUBLIC  Crc@UpdateCrc32, Crc@UpdateCrc, Crc@Crc32Str
        PUBLIC  Crc@UpdateCrc32Buf, Crc@UpdateCrcBuf, Crc@GetChecksum

; Function UpdateCrc32 (CurByte: Byte; CurCrc: LongInt): LongInt
        Upd32_Byte      EQU     Byte Ptr  [ESP + 8]
        Upd32_Crc       EQU     DWord Ptr [ESP + 4]
        Size_Params     =       8

        ALIGN   4
Crc@UpdateCrc32 PROC    NEAR
        MovZX   ECX, Upd32_Byte
        Mov     EAX, Upd32_Crc
        Xor     CL, AL
        Shr     EAX, 8
        Xor     EAX, DWord Ptr [Crc@Crc32Table + ECX*4]
        Ret     Size_Params
Crc@UpdateCrc32 EndP

; Function UpdateCrc (CurByte: Byte; CurCrc: System. Word): System. Word
        Upd16_Byte      EQU     Byte Ptr [ESP + 8]
        Upd16_Crc       EQU     Word Ptr [ESP + 4]
        Size_Params     =       8

        ALIGN   4
Crc@UpdateCrc   PROC    NEAR
        Mov     AL, Upd16_Byte
        Mov     CX, Upd16_Crc
        Mov     AH, CL
        Mov     CL, CH
        And     ECX, 0FFh
        Xor     AX, Word Ptr [Crc@CrcTable + ECX + ECX]
        Ret     Size_Params
Crc@UpdateCrc   EndP

; Function Crc32Str (Const S: String): LongInt
        CrcS_Str        EQU     DWord Ptr [ESP + 16]
        Size_Params     =       4

        ALIGN   4
Crc@Crc32Str    PROC    NEAR
        Push    EBX
        Push    ESI
        Push    EDI
        Mov     ESI, CrcS_Str
        Xor     EDX, EDX
        Xor     EAX, EAX
        Xor     EBX, EBX
        Mov     DL, [ESI]
        Dec     EAX
        Test    DL, DL
        Jz      @@Exit
        Inc     ESI
        Mov     ECX, EDX
        Lea     EDI, Crc@Crc32Table
        And     DL, 3
        Jz      @@StartLoopX4
  @@Loop:
        Mov     BL, [ESI]
        Inc     ESI
        Xor     BL, AL
        Shr     EAX, 8
        Xor     EAX, DWord Ptr [EDI + EBX*4]
        Dec     EDX
        Jnz     @@Loop
  @@StartLoopX4:
        Shr     ECX, 2
        Jz      @@Exit
  @@LoopX4:
        Mov     EDX, [ESI]
        Mov     BL, AL
        Add     ESI, 4
        Xor     BL, DL
        Shr     EAX, 8
        Xor     EAX, DWord Ptr [EDI + EBX*4]
        Mov     BL, DH
        Xor     BL, AL
        Shr     EAX, 8
        Shr     EDX, 16
        Xor     EAX, DWord Ptr [EDI + EBX*4]
        Mov     BL, DL
        Xor     BL, AL
        Shr     EAX, 8
        Xor     EAX, DWord Ptr [EDI + EBX*4]
        Mov     BL, DH
        Xor     BL, AL
        Shr     EAX, 8
        Xor     EAX, DWord Ptr [EDI + EBX*4]
        Dec     ECX
        Jnz     @@LoopX4
  @@Exit:
        Pop     EDI
        Pop     ESI
        Pop     EBX
        Ret     Size_Params
Crc@Crc32Str    EndP

; Function UpdateCrc32Buf (Var Buf; Len: Word; StartCrc: LongInt): LongInt
        Upd32B_Buf      EQU     DWord Ptr [ESP + 24]
        Upd32B_Len      EQU     DWord Ptr [ESP + 20]
        Upd32B_Crc      EQU     DWord Ptr [ESP + 16]
        Size_Params     =       12

        ALIGN   4
Crc@UpdateCrc32Buf      PROC    NEAR
        Push    EBX
        Push    ESI
        Push    EDI
        Xor     EBX, EBX
        Lea     EDI, Crc@Crc32Table
        Mov     ECX, Upd32B_Len
        Mov     EAX, Upd32B_Crc
        Mov     ESI, Upd32B_Buf
        Shr     ECX, 3
        Jz      @@StartLoop
  @@LoopX8:
        Mov     EDX, [ESI]
        Mov     BL, AL
        Add     ESI, 4
        Xor     BL, DL
        Shr     EAX, 8
        Xor     EAX, DWord Ptr [EDI + EBX*4]
        Mov     BL, DH
        Shr     EDX, 16
        Xor     BL, AL
        Shr     EAX, 8
        Xor     EAX, DWord Ptr [EDI + EBX*4]
        Mov     BL, DL
        Xor     BL, AL
        Shr     EAX, 8
        Xor     EAX, DWord Ptr [EDI + EBX*4]
        Mov     BL, DH
        Xor     BL, AL
        Shr     EAX, 8
        Mov     EDX, [ESI]
        Xor     EAX, DWord Ptr [EDI + EBX*4]
        Mov     BL, DL
        Add     ESI, 4
        Xor     BL, AL
        Shr     EAX, 8
        Xor     EAX, DWord Ptr [EDI + EBX*4]
        Mov     BL, DH
        Shr     EDX, 16
        Xor     BL, AL
        Shr     EAX, 8
        Xor     EAX, DWord Ptr [EDI + EBX*4]
        Mov     BL, DL
        Xor     BL, AL
        Shr     EAX, 8
        Xor     EAX, DWord Ptr [EDI + EBX*4]
        Mov     BL, DH
        Xor     BL, AL
        Shr     EAX, 8
        Xor     EAX, DWord Ptr [EDI + EBX*4]
        Dec     ECX
        Jnz     @@LoopX8
  @@StartLoop:
        Mov     EDX, Upd32B_Len
        And     EDX, 7
        Jz      @@Exit
  @@Loop:
        Mov     BL, [ESI]
        Inc     ESI
        Xor     BL, AL
        Shr     EAX, 8
        Xor     EAX, DWord Ptr [EDI + EBX*4]
        Dec     EDX
        Jnz     @@Loop
  @@Exit:
        Pop     EDI
        Pop     ESI
        Pop     EBX
        Ret     Size_Params
Crc@UpdateCrc32Buf      EndP

; Function UpdateCrcBuf (Var Buf; Len: Word; StartCrc: System. Word): System. Word
        Upd16B_Buf      EQU     DWord Ptr [ESP + 24]
        Upd16B_Len      EQU     DWord Ptr [ESP + 20]
        Upd16B_Crc      EQU     Word Ptr [ESP + 16]
        Size_Params     =       12

        ALIGN   4
Crc@UpdateCrcBuf        PROC    NEAR
        Push    EBX
        Push    ESI
        Push    EDI
        Mov     ECX, Upd16B_Len
        MovZX   EAX, Upd16B_Crc
        Xor     EBX, EBX
        Mov     ESI, Upd16B_Buf
        Push    ECX
        Lea     EDI, Crc@CrcTable
        Shr     ECX, 2
        Jz      @@StartLoop
  @@LoopX4:
        Mov     BL, AH
        Mov     EDX, [ESI]
        Mov     AH, AL
        Add     ESI, 4
        Mov     AL, DL
        Xor     AX, Word Ptr [EDI + EBX*2]
        Mov     BL, AH
        Mov     AH, AL
        Mov     AL, DH
        Xor     AX, Word Ptr [EDI + EBX*2]
        Shr     EDX, 16
        Mov     BL, AH
        Mov     AH, AL
        Mov     AL, DL
        Xor     AX, Word Ptr [EDI + EBX*2]
        Mov     BL, AH
        Mov     AH, AL
        Mov     AL, DH
        Xor     AX, Word Ptr [EDI + EBX*2]
        Dec     ECX
        Jnz     @@LoopX4
  @@StartLoop:
        Pop     EDX
        And     EDX, 3
        Jz      @@Exit
  @@Loop:
        Mov     BL, AH
        Mov     AH, AL
        Mov     AL, [ESI]
        Inc     ESI
        Xor     AX, Word Ptr [EDI + EBX*2]
        Dec     EDX
        Jnz     @@Loop
  @@Exit:
        Pop     EDI
        Pop     ESI
        Pop     EBX
        Ret     Size_Params
Crc@UpdateCrcBuf        EndP

; Procedure GetChecksum (Const S: String; Var C: tCheckSum)
        GetC_Str        EQU     DWord Ptr [ESP + 8]
        GetC_Checksum   EQU     DWord Ptr [ESP + 4]
        Size_Params     =       8

        ALIGN   4
Crc@GetChecksum PROC    NEAR
        Mov     EAX, GetC_Str
        Push    EBX
        Push    ESI
        Push    EDI
        Push    EBP
        Mov     ESI, EAX
        Mov     EDX, 0FFFFFFFFh
        Mov     CL, [ESI]
        Xor     EBX, EBX
        And     ECX, 0FFh
        Jz      @@Exit
        Mov     EAX, ECX
        Inc     ESI
        Mov     EBP, ECX
        Lea     EDI, Crc@Crc32Table
        And     EAX, 3
        Jz      @@StartLoopX4
  @@Loop:
        Mov     BL, [ESI]
        Inc     ESI
        Add     CH, BL
        Xor     BL, DL
        Shr     EDX, 8
        Xor     EDX, DWord Ptr [EDI + EBX*4]
        Dec     EAX
        Jnz     @@Loop
  @@StartLoopX4:
        Shr     EBP, 2
        Jz      @@Exit
  @@LoopX4:
        Mov     EAX, [ESI]
        Mov     BL, DL
        Add     ESI, 4
        Xor     BL, AL
        Shr     EDX, 8
        Add     CH, AL
        Xor     EDX, DWord Ptr [EDI + EBX*4]
        Mov     BL, AH
        Xor     BL, DL
        Add     CH, AH
        Shr     EDX, 8
        Shr     EAX, 16
        Xor     EDX, DWord Ptr [EDI + EBX*4]
        Mov     BL, AL
        Xor     BL, DL
        Shr     EDX, 8
        Add     CH, AL
        Xor     EDX, DWord Ptr [EDI + EBX*4]
        Mov     BL, AH
        Xor     BL, DL
        Shr     EDX, 8
        Add     CH, AH
        Xor     EDX, DWord Ptr [EDI + EBX*4]
        Dec     EBP
        Jnz     @@LoopX4
  @@Exit:
        Pop     EBP
        Pop     EDI
        Pop     ESI
        Pop     EBX
        Mov     EAX, GetC_Checksum
        Mov     [EAX], EDX
        Mov     [EAX + 4], CX
        Ret     Size_Params
Crc@GetChecksum EndP

CODE32  ENDS

END
