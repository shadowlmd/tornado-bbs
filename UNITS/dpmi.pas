{$S-,R-,V-,I-,B-,F+}

{$IFNDEF Ver40}
  {$R-,O-,A-}
{$ENDIF}

{$IFDEF DPMI}
  {$G+}
{$ENDIF}

{*********************************************************}
{*                     DPMI.PAS 1.00                     *}
{*        Copyright (c) TurboPower Software 1992.        *}
{*                 All rights reserved.                  *}
{*********************************************************}

Unit Dpmi;       {primitive routines for DPMI management}

Interface

{-The following consts are used throughout Object Professional.  Your code
  is free to reference them, but they must *not* be changed.}
Const
  DpmiInUse : Boolean = False;        {True if running in protected mode}
  ColorSele : Word = $B800;           {selector/segment for color video}
  MonoSele  : Word = $B000;           {selector/segment for mono video}
  BiosDataSele : Word = $0040;        {selector/segment for bios data area}
  BiosSele : Word = $F000;            {selector/segment for bios memory}


  {$IFDEF Dpmi}
Type
  {.Z+}
  DoubleWord = Record
                 LoWord  : Word;
                 HiWord  : Word;
               End;

  DPMIRegisters =                        {!!.31, added AL, AH, ..., DH}
  Record
    DI : LongInt;
    SI : LongInt;
    BP : LongInt;
    Reserved : LongInt;
    Case Integer Of
      1 : (BX : LongInt;
      DX : LongInt;
      CX : LongInt;
      AX : LongInt;
      Flags : Word;
      ES : Word;
      DS : Word;
      FS : Word;
      GS : Word;
      IP : Word;
      CS : Word;
      SP : Word;
      SS : Word);
      2 : (BL, BH : Byte; EBXH : Word;
      DL, DH : Byte; EDXH : Word;
      CL, CH : Byte; ECXH : Word;
      AL, AH : Byte; EAXH : Word);
    End;

    MemInfoRec =
    Record
      LargestFreeBlock   : LongInt;
      MaxUnlockedPages   : LongInt;
      MaxLockedPages     : LongInt;
      LinearAddrPages    : LongInt;
      TotalUnlockedPages : LongInt;
      TotalFreePages     : LongInt;
      TotalPhysicalPages : LongInt;
      FreeLinearPages    : LongInt;
      PageSize           : LongInt;
      Reserved           : Array [1..$C] Of Byte;
    End;

    DPMIInfoRec =
    Record {Information returned by GetDPMIInfo routine}
      MinorVersion     : Byte;
      MajorVersion     : Byte;
      Flags            : Word;
      SlavePICInt      : Byte;
      MasterPICInt     : Byte;
      Processor        : Byte;
    End;

    Type
      DescriptorTableEntry =
      Record
        LimitL : Word;
        BaseL  : Word;
        Words : Array [0..1] Of Word;
      End;
      {.Z-}

    Function Linear (P : Pointer) : LongInt;
    {-Converts a pointer to a linear address to allow differences in addresses
    to be calculated. The pointer must be in the range $0:$0 to $FFFF:$000F.}

    Function UnLinear (L : LongInt) : Pointer;
    {-Converts a linear address to a pointer to allow selector base addresses to
    be converted to pointers. The longInt must be in the range $0 to $000FFFFF.}

    Function ValidPointer (P : Pointer) : Boolean; {!!.22 new}
    {-verify a pmode pointer is valid, by verifying the access rights and limit
    on the selector and verifying the selector is write-able.  This only checks
    the specific pointer address; if you will be accessing a range from the
    pointer you should validate the maximum possible offset for the pointer
    as well.}

    Function AllocLDTDescriptors (NumOfDesc : Word; Var BaseSelector : Word) : Word;
    {-Allocates one or more descriptors in the task's Local Descriptor Table
    (LDT). The descriptor is not initialized; this must be done with calls to
    SetSegmentBaseAddr and SetSegmentLimit. The allocated descriptor will be
    set to "data" with a priviledge level equal to the application's code
    segment priviledge level. If requesting more than one descriptor, the
    BaseSelector will be set to the first of a contiguous array of
    descriptors. The Selector values for subsequent descriptors in the array
    must be calculated by adding the value returned by GetSelectorIncrement.}

    Function GetSelectorIncrement : Word;
    {-gets the selector increment value}

    Function SetSegmentBaseAddr (Selector : Word; BaseAddress : LongInt) : Word;
    {-Sets the base (starting) address for Selector}

    Function SetSegmentLimit (Selector : Word; Limit : LongInt) : Word;
    {-Sets the limit (length) for Selector}

    Function GetSegmentBaseAddr (Selector : Word; Var BaseAddress: LongInt) : Word;
    {-Gets the base (starting) address for Selector}

    Function GetSegmentLimit (Selector : Word; Var Limit : LongInt) : Word;
    {-Gets the limit (length) for Selector}

    Function FreeLDTDescriptor (Selector : Word) : Word;
    {-Deallocates Selector}

    Function GetSelectorForRealMem (RealPtr : Pointer; Limit : LongInt; Var Selector : Word) : Word;
    {-Allocates Selector of Size bytes in Real memory, starting at RealPtr}

    Function GetDescriptor (Selector : Word;
    Var Descriptor : DescriptorTableEntry) : Word;
    {-Gets the Descriptor Table information on Selector, returns 0 if successful}

    Function CallFarRealModeProc (StackWords : Word; StackData : Pointer;
    Var Regs : DPMIRegisters) : Word;
    {-Simulates a FAR CALL to a real mode procedure.}

    Function SimulateRealModeInt (IntNo : Byte;
    Var Regs : DPMIRegisters) : Word;
    {-Simulates an interrupt in real mode. Control is transferred to the
    address specified by the real mode interrupt vector.}

    Procedure GetRealModeIntVector (IntNo : Byte; Var Vector : Pointer);
    {-Returns the contents of the current virtual machine's real mode interrupt
    vector number for IntNo. Note, the returned address is a real mode
    segment:offset.}

    Procedure SetRealModeIntVector (IntNo : Byte; Vector : Pointer);
    {-Set the current virtual machine's real mode interrupt vector for
    vector IntNo. Vector must be a real mode segment:offset.}

    Function AllocRealModeCallbackAddr (CallbackProc : Pointer;
    Var Regs : DPMIRegisters;
    Var Callback : Pointer) : Word;
    {-Allocates a unique real mode segment:offset that will transfer control
    from real mode to a protected mode procedure.}

    Function FreeRealModeCallbackAddr (Callback : Pointer) : Word;
    {-Frees a real mode callback previously allocated with
    AllocateRealModeCallbackAddr.}

    Procedure GetProtectedModeInt (IntNo : Byte; Var Handler : Pointer);
    {-Returns the address of the current protected mode interrupt handler for
    IntNo.}

    Function SetProtectedModeInt (IntNo : Byte; Handler : Pointer) : Word;
    {-Sets the address of the protected mode handler for IntNo.}

    Procedure GetDPMIMemInfo (Var MemInfo : MemInfoRec);
    {-Returns information about the amount of available physical memory, linear
    address space, and disk space for page swapping. See the MemInfoRec
    declared above for information on the returned values. Only the first
    field of the MemInfoRec is guantanteed to be valid. All invalid fields
    will be set to -1.}

    Function AllocDosMem (SizeInParas : Word;
                         Var RealModeSeg : Word;
                         Var ProtModeSel : Word) : Word;

    Function FreeDosMem (ProtModeSel : Word) : Word;

    {$ENDIF}

    Implementation


    {$IFDEF Dpmi}
    Type
      OS =
      Record
        O, S : Word;
      End;

    Var
      DpmiPrimExitPtr : Pointer;

    Function Linear (P : Pointer) : LongInt;
    {-Converts a pointer to a linear address to allow differences in addresses
    to be calculated. The pointer must be in the range $0:$0 to $FFFF:$000F.}
  Begin
    With OS (P) Do
      Linear := (LongInt (S) ShL 4) + LongInt (O);
  End;

Function UnLinear (L : LongInt) : Pointer;
    {-Converts a linear address to a pointer allow selector base addresses to
      be converted to pointers. The longInt must be in the range $0 to $000FFFFF.}
  Begin
    UnLinear := Ptr (Word (L ShR 4), Word (L And $000F) );  {!!.21}
  End;

  Function ValidPointer (P : Pointer) : Boolean; Assembler;  {!!.22 new}
    {-verify a pmode pointer is valid}
  Asm
    push   DX;               {preserve dx}
    push   BX;               {preserve bx}
    XOr    AX, AX;            {assume failure}
    mov    DX, [BP + 8];        {get selector portion of pointer}
    lar    BX, DX;            {get Access Rights byte}
    jnz    @@Out;            {bad selector, get out}
    lsl    BX, DX;            {get Selector Limit}
    jnz    @@Out;            {bad selector, get out}
    cmp    BX, [BP + 6];        {compare selector limit to offset of pointer}
    jb     @@Out;            {limit is less, get out}
    verw   DX;               {is selector writable?}
    jnz    @@Out;            {nope, get out}
    mov    AL, 1;             {return True}
    @@Out:
    pop    BX;               {restore bx}
    pop    DX;               {restore dx}
  End;

  Function CallFarRealModeProc (StackWords : Word; StackData : Pointer;
                               Var Regs : DPMIRegisters) : Word; Assembler;
  Asm
    push    DS;
    mov     CX, StackWords;
    jcxz    @@NoParams;
    lds     SI, StackData;
    mov     AX, CX;
    Dec     AX;
    ShL     AX, 1;
    add     SI, AX;
    std;
    @@ParamLoop:
    lodsw;
    push    AX;
    loop    @@ParamLoop;
    @@NoParams:
    cld;
    XOr     BX, BX;
    mov     CX, StackWords;
    les     DI, Regs;
    mov     AX, 0301h;
    Int     31h;
    jc      @@ExitPoint;
    XOr     AX, AX;
    @@ExitPoint:
    mov     BX, StackWords;
    ShL     BX, 1;
    add     sp, BX;
    pop     DS;
  End;

  Function SimulateRealModeInt (IntNo : Byte;
                               Var Regs : DPMIRegisters) : Word; Assembler;
  Asm
    XOr     BX, BX;
    mov     BL, IntNo;
    XOr     CX, CX;       {StackWords = 0}
    les     DI, Regs;
    mov     AX, 0300h;
    Int     31h;
    jc      @@ExitPoint;
    XOr     AX, AX;
    @@ExitPoint:
  End;

  Procedure GetRealModeIntVector (IntNo : Byte; Var Vector : Pointer); Assembler;
  Asm
    mov     AX, 0200h;
    mov     BL, IntNo;
    Int     31h;
    les     DI, Vector;
    mov     Word Ptr ES: [DI], DX;
    mov     Word Ptr ES: [DI + 2], CX;
  End;

  Procedure SetRealModeIntVector (IntNo : Byte; Vector : Pointer); Assembler;
  Asm
    mov     AX, $0201;
    mov     BL, IntNo;
    mov     DX, Word Ptr Vector;
    mov     CX, Word Ptr Vector + 2;
    Int     $31;
  End;

  Function GetCPUFlags : Byte; Assembler;
  Asm
    lahf;
    mov     AL, AH;
  End;

  {Doesn't work under Windows 3.1. Don't use in Windows!}
  Function AllocDosMem (SizeInParas : Word;
                       Var RealModeSeg : Word;
                       Var ProtModeSel : Word) : Word; Assembler;
  Asm
    mov     BX, SizeInParas;
    mov     AX, 0100h;
    Int     31h;
    jc      @@ExitPoint;
    les     DI, RealModeSeg;
    mov     ES: [DI], AX;
    les     DI, ProtModeSel;
    mov     ES: [DI], DX;
    XOr     AX, AX;
    @@ExitPoint:
  End;

  {Doesn't work under Windows 3.1. Don't use in Windows!}
  Function FreeDosMem (ProtModeSel : Word) : Word; Assembler;
  Asm
    mov     AX, 0101h;
    mov     DX, ProtModeSel;
    Int     31h;
    jc      @@ExitPoint;
    XOr     AX, AX;
    @@ExitPoint:
  End;

  Function AllocLDTDescriptors (NumOfDesc : Word; Var BaseSelector : Word) : Word; Assembler;
  Asm
    mov     CX, NumOfDesc;
    XOr     AX, AX;
    Int     31h;
    jc      @@ErrorExitPoint;        {!!.31}
    les     DI, BaseSelector;
    mov     ES: [DI], AX;
    XOr     AX, AX;
    jmp     @@ExitPoint              {!!.31}
    @@ErrorExitPoint:                  {!!.31}
    mov     AX, 1                     {!!.31}
    @@ExitPoint:
  End;

  Function SetSegmentBaseAddr (Selector : Word; BaseAddress : LongInt) : Word; Assembler;
  Asm
    mov     BX, Selector;
    mov     DX, Word Ptr BaseAddress;
    mov     CX, Word Ptr BaseAddress + 2;
    mov     AX, 0007h;
    Int     31h;
    jc      @@ExitPoint;
    XOr     AX, AX;
    @@ExitPoint:
  End;

  Function GetSegmentAccessRights (Selector : Word; Var Rights : Word) : Word;
  Var
    Status : Word;
    Descriptor : DescriptorTableEntry;
  Begin
    Status := GetDescriptor (Selector, Descriptor);
    If Status = 0 Then
      With Descriptor Do
        Rights := (Words [0] ShR 8) Or ( (Words [1] And $00F0) ShL 8);
    GetSegmentAccessRights := Status;
  End;

  Function SetRightsPrim (Selector : Word; Rights : Word) : Word; Assembler;
    {-Primitive rights change}
  Asm
    mov     BX, Selector;
    mov     CX, Rights;
    mov     AX, 0009h;
    Int     31h;
    jc      @@ExitPoint;
    XOr     AX, AX;
    @@ExitPoint:
  End;

  Function SetSegmentAccessRights (Selector : Word;
                                  ReadWrite : WordBool; Code : WordBool) : Word;
  Var
    Rights : Word;
    Status : Word;
  Begin
    Status := GetSegmentAccessRights (Selector, Rights);
    If Status <> 0 Then Begin
      SetSegmentAccessRights := Status;
      Exit;
    End;

    {Modify the Rights mask according to parameters}
    If Code Then Begin
      ReadWrite := True; {For code, means segment can be read as well as executed}
      Rights := Rights And Not $0004; {Code is always expand-up}
      Rights := Rights Or $0008;      {Set Code bit}
    End Else
      Rights := Rights And Not $0008; {Clear Code bit}
    If ReadWrite Then
      Rights := Rights Or $0002       {Set ReadWrite bit}
    Else
      Rights := Rights And Not $0002; {Clear ReadWrite bit}

    {Change the rights}
    SetSegmentAccessRights := SetRightsPrim (Selector, Rights);
  End;

  Function GetSegmentLimit (Selector : Word; Var Limit : LongInt) : Word;
  Var
    Status : Word;
    Descriptor : DescriptorTableEntry;
  Begin
    Status := GetDescriptor (Selector, Descriptor);
    If Status = 0 Then
      With Descriptor Do Begin
        Limit := LongInt (LimitL) Or (LongInt (Words [1] And $0F) ShL 16);
        {Account for granularity}
        If Words [1] And $80 <> 0 Then
          Limit := Limit * 4096;
      End;
    GetSegmentLimit := Status;
  End;

  Function GetSegmentBaseAddr (Selector : Word; Var BaseAddress : LongInt) : Word; Assembler;
  Asm
    mov     BX, Selector;
    mov     AX, 0006h;
    Int     31h;
    jc      @@ExitPoint;
    XOr     AX, AX;
    les     DI, BaseAddress;
    mov     ES: [DI], DX;
    mov     ES: [DI + 2], CX;
    @@ExitPoint:
  End;

  Function SetLimitPrim (Selector : Word; Limit : LongInt) : Word; Assembler;
    {-Primitive limit change}
  Asm
    mov     BX, Selector;
    mov     DX, Word Ptr Limit;
    mov     CX, Word Ptr Limit + 2;
    mov     AX, 0008h;
    Int     31h;
    jc      @@ExitPoint;
    XOr     AX, AX;
    @@ExitPoint:
  End;

  Function SetSegmentLimit (Selector : Word; Limit : LongInt) : Word;
  Var
    Rights : Word;
    Status : Word;
  Begin
    {Handle limit granularity}
    Status := GetSegmentAccessRights (Selector, Rights);
    If Status <> 0 Then Begin
      SetSegmentLimit := Status;
      Exit;
    End;
    If Limit > $FFFFF Then Begin
      {Segment larger than 1MB}
      If Limit And $FFF <> $FFF Then Begin
        {Not page aligned}
        SetSegmentLimit := $8021;
        Exit;
      End;
      Rights := Rights Or $8000;       {Page-granular}
    End Else
      Rights := Rights And Not $8000;  {Byte-granular}

    {Assure no overflow when granularity changed}
    Status := SetLimitPrim (Selector, 0);
    If Status = 0 Then
      Status := SetRightsPrim (Selector, Rights);
    If Status = 0 Then
      SetSegmentLimit := SetLimitPrim (Selector, Limit);
    SetSegmentLimit := Status;
  End;

  Function FreeLDTDescriptor (Selector : Word) : Word; Assembler;
  Asm
    mov     BX, Selector;
    mov     AX, 0001h;
    Int     31h;
    jc      @@ExitPoint;
    XOr     AX, AX;
    @@ExitPoint:
  End;

  Function GetSelectorIncrement : Word; Assembler;
  Asm
    mov     AX, 0003h;
    Int     31h;
  End;

  Function GetSelectorForRealMem (RealPtr : Pointer; Limit : LongInt; Var Selector : Word) : Word;

    Procedure FreeSele;
    Begin
      FreeLDTDescriptor (Selector);
    End;

  Var
    ErrorCode : Word;

  Begin
    ErrorCode := AllocLDTDescriptors (1, Selector);
    If ErrorCode = 0 Then Begin
      ErrorCode := SetSegmentBaseAddr (Selector, Linear (RealPtr) );
      If ErrorCode = 0 Then Begin
        ErrorCode := SetSegmentLimit (Selector, Limit);
        If ErrorCode <> 0 Then
          FreeSele;
      End
      Else
        FreeSele;
    End;
    GetSelectorForRealMem := ErrorCode;
  End;

  Function AllocRealModeCallbackAddr (CallbackProc : Pointer;
                                     Var Regs : DPMIRegisters;
                                     Var Callback : Pointer) : Word; Assembler;
  Asm
    push    DS;
    lds     SI, CallbackProc;
    les     DI, Regs;
    mov     AX, 0303h;
    Int     31h;
    jnc     @@Exitpoint;
    XOr     CX, CX;
    XOr     DX, DX;
    jmp     @@ExitPoint2;
    @@ExitPoint:
    XOr     AX, AX;
    @@ExitPoint2:
    les     DI, Callback;
    mov     Word Ptr ES: [DI], DX;
    mov     Word Ptr ES: [DI + 2], CX;
    pop     DS;
  End;

  Function FreeRealModeCallbackAddr (Callback : Pointer) : Word; Assembler;
  Asm
    mov     CX, Word Ptr Callback + 2;
    mov     DX, Word Ptr Callback;
    mov     AX, 0304h;
    Int     31h;
    jc      @@ExitPoint;
    XOr     AX, AX;
    @@ExitPoint:
  End;

  Procedure GetProtectedModeInt (IntNo : Byte; Var Handler : Pointer); Assembler;
  Asm
    mov     AX, 0204h;
    mov     BL, IntNo;
    Int     31h;
    les     DI, Handler;
    mov     Word Ptr ES: [DI], DX;
    mov     Word Ptr ES: [DI + 2], CX;
  End;

  Function SetProtectedModeInt (IntNo : Byte; Handler : Pointer) : Word; Assembler;
  Asm
    mov     BL, IntNo;
    mov     DX, Word Ptr Handler;
    mov     CX, Word Ptr Handler + 2;
    mov     AX, 0205h;
    Int     31h;
    jc      @@ExitPoint;
    XOr     AX, AX;
    @@ExitPoint:
  End;

  Function GetExceptionHandler (ExceptionNum : Byte;
                               Var Handler : Pointer) : Word; Assembler;
  Asm
    mov     BL, ExceptionNum;
    mov     AX, 0202h;
    Int     31h;
    jc      @@ExitPoint;
    XOr     AX, AX;
    les     DI, Handler;
    mov     Word Ptr ES: [DI], DX;
    mov     Word Ptr ES: [DI + 2], CX;
    @@ExitPoint:
  End;

  Function SetExceptionHandler (ExceptionNum : Byte;
                               Handler : Pointer) : Word; Assembler;
  Asm
    mov     BL, ExceptionNum;
    mov     AX, 0203h;
    mov     DX, Word Ptr Handler;
    mov     CX, Word Ptr Handler + 2;
    Int     31h;
    jc      @@ExitPoint;
    XOr     AX, AX;
    @@ExitPoint:
  End;


  Procedure GetDPMIMemInfo (Var MemInfo : MemInfoRec); Assembler;
  Const
    SizeOfMemInfoRec = SizeOf (MemInfoRec);
    Asm
      les     DI, MemInfo;
      mov     SI, DI;
      mov     CX, SizeOfMemInfoRec;
      mov     AL, 0FFh;
      cld;
      rep     stosb;          {set record to -1 in case DPMI doesn't}
      mov     DI, SI;
      mov     AX, 0500h;       {get free memory info}
      Int     31h;            {this function doesn't fail}
      mov     AX, 0604h;       {get page size}
      Int     31h;
      jc      @@ExitPoint;    {not supported by 16-bit hosts}
      lea     DI, MemInfoRec (ES: [SI] ).PageSize;
      cld;
      mov     AX, CX;
      stosw;
      mov     AX, BX;
      stosw;
      @@ExitPoint:
    End;

  Procedure GetDPMIInfo (Var DPMIInfo : DPMIInfoRec); Assembler;
  Asm
    mov     AX, 0400h;
    Int     31h;            {this function doesn't fail}
    les     DI, DPMIInfo;
    cld;
    stosw;                  {store minor and major version numbers}
    mov     AX, BX;
    stosw;                  {store Flags}
    mov     AX, DX;
    stosw;                  {store PIC base interrupt numbers}
    mov     AL, CL;
    stosb;                  {store processor type}
  End;

  Function GetPageSize (Var PageSize : LongInt) : Word; Assembler;
  Asm
    mov     AX, 0604h;
    Int     31h;
    jc      @@ExitPoint;
    les     DI, PageSize;
    mov     ES: [DI], CX;
    mov     ES: [DI + 2], BX;
    XOr     AX, AX;
    @@ExitPoint:
  End;

  Function GetDescriptor (Selector : Word;
                         Var Descriptor : DescriptorTableEntry) : Word; Assembler;
  Asm
    mov     AX, 000BH;
    mov     BX, Selector;
    les     DI, Descriptor;
    Int     31h;
    jc      @@ExitPoint;
    XOr     AX, AX;
    @@ExitPoint:
  End;

  Procedure DpmiPrimExitProc;
    {-Our exit handler for this unit}
  Begin
    ExitProc := DpmiPrimExitPtr;

    {free our BiosSele selector}
    FreeLDTDescriptor (BiosSele);
  End;

Var
  W : Word;

Begin
  ColorSele    := SegB800;
  MonoSele     := SegB000;
  BiosDataSele := Seg0040;
  DpmiInUse    := True;

  {since the RTL doesn't provide an important predefined selector, we get one}
  W := GetSelectorForRealMem (Ptr ($F000, 0), $FFFF, BiosSele);
  If W <> 0 Then
    {failed; generate Runtime Error 203 (out of heap)}
    RunError (203)
  Else Begin
    {and set up an exit handler to release it}
    DpmiPrimExitPtr := ExitProc;
    ExitProc := @DpmiPrimExitProc;
  End;
  {$ENDIF}
End.

