{$F+}
Unit
  TimeTask;

Interface

{
TaskRec.OS
   0 : No MultiTasking
   1 : MS Windows
   2 : OS/2
   3 : DESQview
   4 : TopView
   5 : PC-MOS/386
   6 : Linux DOSEMU
   7 : OS/2 Warp
   8 : European MS-DOS
   9 : DoubleDOS
  10 : Win95 // removed
  11 : NTVDM
}

Type
  TaskRec = Record
    OS      : System. Word;
    Version : System. Word;
  End;

Const
  Task : TaskRec = (OS: 0; Version: 0);

{$IFDEF MSDOS}
Type
  SliceProc = Procedure;

Var
  TimeSlice : SliceProc;
{$ELSE}
Procedure TimeSlice;
{$ENDIF}

Procedure InitMulti;

Implementation

{$IFDEF OS2}
Uses
  Os2Base,
  OpCrt,
  VPutils;
{$ENDIF}
{$IFDEF WIN32}
Uses
  vpSysLow;
{$ENDIF}
{$IFDEF MSDOS}
{$IFNDEF DPMI32}
Uses
  NTVDMSVC;
{$ENDIF}
{$ENDIF}

{$IFDEF MSDOS}
Procedure NoSlice; Assembler; {&USES None} {&FRAME-}
Asm
End;

Procedure WinSlice; Assembler; {&USES None} {&FRAME-}
Asm
  Mov  AX, 1680h
  Int  2Fh
End;

Procedure OS2Slice; Assembler; {&USES None} {&FRAME-}
Asm
  Mov  AX, 35
  Xor  DX, DX
  Hlt
  DB   35h, 0CAh
End;

Procedure DV_TVSlice; Assembler; {&USES None} {&FRAME-}
Asm
  Mov  AX, 1000h
  Int  15h
End;

Procedure PC_MOSSlice; Assembler; {&USES None} {&FRAME-}
Asm
  Int  28h
End;

Procedure LinuxSlice; Assembler; {&USES None} {&FRAME-}
Asm
  Int  28h
End;

Procedure EuropianSlice; Assembler; {&USES None} {&FRAME-}
Asm
  Mov  AH, 89h
  Mov  CX, 55     { default time quant (55mS) }
  Int  2Fh
End;

Procedure DoubleDOSSlice; Assembler; {&USES None} {&FRAME-}
Asm
  Mov  AX, $EE01
  Int  21h
End;

{$IFNDEF DPMI32}
Procedure NTVDMSlice;
Begin
  NTVDMSleep (10);
End;
{$ENDIF}

Procedure InitMulti;
Begin
  {$IFNDEF DPMI32}
  If NTVDMInitOk Then
  Begin
    Task. Os := 11;
    Task. Version := 0;
  End Else
  {$ENDIF}
  Asm
    mov  Task. OS, 0
    mov  Task. Version, 0

    (*
    {* Start of Win95 detection ***************** }
    mov  AX, 3001h
    int  21h
    jc   @NoWin95
    cmp  AL, 7
    jb   @NoWin95
    mov  AX, 4A33h
    int  2Fh
    cmp  AX, 0
    je   @Win95
  @NoWin95:
    {* End of Win95 detection part ************** }
    *)

    {* Starting MS Win 3.x system detection ***** }
    mov  AX, 160Ah       { function 160Ah of 2fh  }
    Int  2Fh
    cmp  AX, 0           { AX=0 if Win 3.1        }
    je   @Windows
    {* End of M$-SUX detection part ************* }

    { Start of OS/2 system detection              }
    mov  AX, 4010h       { int 2fh, AX=4010h      }
    int  2fh
    cmp  AX, 4010h       { AX was changed?        }
    jne  @OS2            { Yeah, it is!           }
    { End of OS/2 detection part                  }

    mov  AX, 1022h
    xor  BX, BX
    Int  15h
    cmp  BX, 0
    jne  @DESQview

    mov  AX, 2B01H
    mov  CX, 4445h
    mov  DX, 5351h
    Int  21h
    cmp  AL, $FF
    jne  @TopView

    mov  AX, 3000h
    mov  BX, 3000h
    mov  CX, 3000h
    mov  DX, 3000h
    int  21h
    cmp  AL, 3
    je   @PC_MOS

    {* Starting European MS-DOS 4.0 detection *** }
    mov  AH, 87h  { int 21h, AH=87h               }
    mov  AL, 0
    int  21h
    cmp  AL, 0    { if AL <> 0 then Eur. detected }
    jne  @European
    {* End of European MS-DOS detection part **** }

    {* DoubleDOS system detection *************** }
    mov  AX, $E400       { int 21h, AX=E400h      }
    int  21h
    cmp  AL, 0           { AL=0 if not installed  }
    jne  @DoubleDOS
    {* DoubleDOS system detection end *********** }

  {$IFNDEF DPMI32}
    {* Start Linux DOSEMU detection ************* }
    {* Check for int E6h vector address ********* }
    mov  AH, $35  { get int vector                }
    mov  AL, $E6  { of int E6h                    }
    int  21h

    cmp  AX, $F000 { int E6h points to F000:0E60? }
    jne  @Fin
    cmp  BX, $0E60
    jne  @Fin

    mov  AX, 0 { It's points to address F000:0E60 }
    int  $E6
    cmp  AH, $AA
    jne  @Fin
    cmp  AL, $55
    je   @Linux
    {* End of Linux DOSEMU detection ************ }
  {$ENDIF}

    jmp  @Fin

    (*
  @Win95:
    mov  AX, 160Ah       { function 160Ah of 2fh  }
    Int  2Fh
    mov  Task. OS, 10
    mov  Task. Version, BX
    jmp  @Fin
    *)

  @Windows:
    Mov  Task. OS, 1
    Mov  Task. Version, BX
    jmp  @Fin

  @OS2:
    Mov  Task. OS, 2
    cmp  AX, 0000h
    je   @Warp

    Mov  BH, AH
    XOr  AH, AH
    Mov  CL, 10
    Div  CL
    Mov  AH, BH
    Xchg AH, AL
    jmp  @OS2_done

  @Warp:
    mov  AH, 3
    mov  AL, 0
    mov  Task. OS, 7

  @OS2_done:
    Mov  Task. Version, AX
    jmp  @Fin

  @DESQview:
    mov  Task. OS, 3
    mov  Task. Version, BX
    jmp  @Fin

  @TopView:
    mov  Task. OS, 4
    mov  Task. Version, BX
    jmp  @Fin

  @PC_MOS:
    mov  Task. OS, 5
    mov  Task. Version, AX
    jmp  @Fin

  @Linux:
    {Setting HogTreshold to optimal value}
    mov  AH, 12h
    mov  BX, 20
    int  $E6

    {Reporting, what we are running under Linux DOSEMU}
    mov  Task. OS, 6
    mov  Task. Version, BX
    jmp  @Fin

  @European:
    mov  Task. OS, 8
    mov  Task. Version, 0400
    jmp  @Fin

  @DoubleDOS:
    mov  Task. OS, 9
    mov  Task. Version, 0100
    jmp  @Fin

  @Fin:
  End;

  Case Task. OS Of
       0 : TimeSlice := NoSlice;
       1 : TimeSlice := WinSlice;
    2, 7 : TimeSlice := OS2Slice;
    3, 4 : TimeSlice := DV_TVSlice;
       5 : TimeSlice := PC_MOSSlice;
       6 : TimeSlice := LinuxSlice;
       8 : TimeSlice := EuropianSlice;
       9 : TimeSlice := DoubleDOSSlice;
{$IFNDEF DPMI32}
      11 : TimeSlice := NTVDMSlice;
{$ENDIF}
  End;
End;
{$ENDIF}

{$IFDEF WIN32}
Procedure TimeSlice;
Begin
  SysCtrlSleep (55);
End;

Procedure InitMulti;
Begin
  Task. OS := 1;
  Task. Version := Swap (SysOsVersion);
End;
{$ENDIF}

{$IFDEF OS2}
Procedure TimeSlice;
Begin
  DosSleep (55);
End;

Procedure InitMulti;
Begin
  Task. OS := 2;
  Task. Version := SysOsVersion;
End;
{$ENDIF}

{$IFDEF MSDOS}
Begin
  TimeSlice := NoSlice;
{$ENDIF}
End.
