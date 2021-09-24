{$F+}

{$I APDEFINE.INC}

{*********************************************************}
{*                    APFOSSIL.PAS 2.03                  *}
{*     Copyright (c) TurboPower Software 1991.           *}
{*                 All rights reserved.                  *}
{*********************************************************}

Unit ApFossil;

Interface

Uses
  DOS,
  ApMisc,
  ApSame,
  OpInline,
  ApPort;

Type
  PDriverInfo = ^DriverInfo;
  DriverInfo = Record
    diSize     : Word;
    diSpec     : Byte;
    diRev      : Byte;
    diID       : Pointer;
    diInSize   : Word;
    diInFree   : Word;
    diOutSize  : Word;
    diOutFree  : Word;
    diSWidth   : Byte;
    diSHeight  : Byte;
    diBaudMask : Byte;
    diJunk     : Word;
  End;

Const
  ptHandleFossilBug = $8000;
  ptTrueOutBuffFree = $4000;
  ptPutCharWait     = $2000;

  DefFossilOptions  = ptHandleFossilBug + ptPutCharWait;

  FossilSignature   = $1954;
  FossilInt         = $14;

  ValidLineStatus   = $FF;
  ValidModemStatus  = $FF;

  InBufferLen       = 1024;
  OutBufferLen      = {$IFDEF DPMI} 2048 {$ELSE} 1024 {$ENDIF};

Procedure fInitPort (Var P: PortRecPtr; ComName: ComNameType; Baud: LongInt;
                     Parity: ParityType; DataBits: DataBitType;
                     StopBits: StopBitType; InSize, OutSize: Word;
                     Options: Word);
Procedure fInitPortKeep (Var P: PortRecPtr; ComName: ComNameType;
                         InSize, OutSize: Word);
Procedure fDonePort (Var P: PortRecPtr);
Procedure fSetUart (ComName: ComNameType; NewBase: Word;
                    NewIrq, NewVector: Byte);
Procedure fSetLine (P: PortRecPtr; Baud: LongInt; Parity: ParityType;
                    DataBits: DataBitType; StopBits: StopBitType);
Procedure fGetLine (P: PortRecPtr; Var Baud: LongInt; Var Parity: ParityType;
                    Var DataBits: DataBitType; Var StopBits: StopBitType;
                    FromHardware: Boolean);
Procedure fSetModem (P: PortRecPtr; DTR, RTS: Boolean);
Procedure fGetModem (P: PortRecPtr; Var DTR, RTS: Boolean);
Procedure fGetChar (P: PortRecPtr; Var C: Char);
Procedure fPeekChar (P: PortRecPtr; Var C: Char; PeekAhead: Word);
Procedure fPutChar (P: PortRecPtr; C: Char);
Function fPutBlock (P: PortRecPtr; Var Block; Size: Word): Word;
Procedure fStartTransmitter (P: PortRecPtr);
Function fCharReady (P: PortRecPtr): Boolean;
Function fTransReady (P: PortRecPtr): Boolean;
Procedure fSendBreak (P: PortRecPtr);
Procedure fActivatePort (P: PortRecPtr; Restore: Boolean);
Procedure fDeactivatePort (P: PortRecPtr; Restore: Boolean);
Procedure fSavePort (P: PortRecPtr; Var PSR);
Procedure fRestorePort (P: PortRecPtr; Var PSR);
Function fUpdateLineStatus (P: PortRecPtr): Byte;
Function fUpdateModemStatus (P: PortRecPtr): Byte;
Procedure fBufferStatus (P: PortRecPtr; Var InFree, OutFree, InUsed,
                         OutUsed: Word);
Procedure fBufferFlush (P: PortRecPtr; FlushIn, FlushOut: Boolean);

{$IFDEF UseHWFlow}
Procedure fHWFlowSet (P: PortRecPtr; Enable: Boolean;
                      BufferFull, BufferResume: Word; Options: Word);
Function fHWFlowGet (P: PortRecPtr): FlowState;
{$ENDIF}

{$IFDEF UseSWFlow}
Procedure fSWFlowSet (P: PortRecPtr; Enable: Boolean;
                      BufferFull, BufferResume: Word; Options: Word);
Function fSWFlowGet (P: PortRecPtr): FlowState;
Procedure fSWFlowCtl (P: PortRecPtr; OnChar, OffChar: Char; Resume: Boolean);
{$ENDIF}

Procedure ActivateApFossil;

Implementation

Uses
{$IFDEF DPMI}
  Dpmi,
  WinAPI,
{$ENDIF}
  TimeTask;

Type
  OS = Record
    Ofs, Seg : Word;
  End;

{$IFDEF DPMI}
  DosMemRec = Record
    Sele, Segm : Word;
  End;
{$ENDIF}

Var
  Regs : {$IFDEF DPMI} DPMIRegisters {$ELSE} Registers {$ENDIF};

{$IFDEF DPMI}
Procedure FossilIntr (Var Regs: DPMIRegisters);
Var
  Result : Word;

Begin
  Result := SimulateRealModeInt (FossilInt, Regs);
End;
{$ENDIF}

Function fGetDriverInfo (P: PortRecPtr): PDriverInfo;
Begin
  With P^ Do
  Begin
    Regs. AH := $1B;
    Regs. CX := SizeOf (DriverInfo);
    Regs. DX := Ord (PortName);
  {$IFDEF DPMI}
    Regs. ES := DosMemRec (StatHead). Segm;
    Regs. DI := 0;
    FossilIntr (Regs);
    fGetDriverInfo := Ptr (DosMemRec (StatHead). Sele, 0);
  {$ELSE}
    Regs. ES := OS (StatHead). Seg;
    Regs. DI := OS (StatHead). Ofs;
    Intr (FossilInt, Regs);
    fGetDriverInfo := Pointer (StatHead);
  {$ENDIF}
  End;
End;

Procedure fClosePort (Var P: PortRecPtr);
Var
  Result : Word;

Begin
  If P <> Nil Then
  Begin
    Regs. AH := $05;
    Regs. DX := Ord (P^. PortName);

  {$IFDEF DPMI}
    FossilIntr (Regs);
    If P^. StatBuff <> Nil Then
      Result := GlobalDosFree (DosMemRec (P^. StatBuff). Sele);
    If P^. StatHead <> Nil Then
      Result := GlobalDosFree (DosMemRec (P^. StatHead). Sele);
    If P^. StatTail <> Nil Then
      Result := GlobalDosFree (DosMemRec (P^. StatTail). Sele);
  {$ELSE}
    Intr (FossilInt, Regs);
    FreeMemCheck (P^. StatBuff, InBufferLen);
    FreeMemCheck (P^. StatHead, SizeOf (DriverInfo));
  {$ENDIF}

    ActiveComPort [P^. CurrentPort] := Nil;

    FreeMemCheck (P, SizeOf (PortRec));
  End;
End;

Procedure fInitPortKeep (Var P: PortRecPtr; ComName: ComNameType;
                         InSize, OutSize: Word);
Var
  i     : Integer;
  Found : Boolean;

Begin
  Regs. AH := $04;
  Regs. BX := 0;
  Regs. DX := Ord (ComName);
{$IFDEF DPMI}
  FossilIntr (Regs);
{$ELSE}
  Intr (FossilInt, Regs);
{$ENDIF}
  If Regs. AX <> FossilSignature Then
  Begin
    AsyncStatus := ecNoFossil;
    Exit;
  End;

  if Not GetMemCheck (P, SizeOf (PortRec)) Then
  Begin
    AsyncStatus := ecOutOfMemory;
    Exit;
  End;

  AsyncStatus := ecOk;

  With P^ Do
  Begin
    Found := False;
    i := 9;

    While Not Found And (i <= MaxActivePort) Do
      If ActiveComPort [i] = Nil Then
      Begin
        CurrentPort := i;
        ActiveComPort [i] := P;
        Found := True;
      End
      Else
        Inc (i);

    If Not Found Then
    Begin
      FreeMemCheck (P, SizeOf (PortRec));
      AsyncStatus := ecNoMorePorts;
      Exit;
    End;

    PortName := ComName;
    ModemControl := 0;

    SWFState := 0;
    SWFGotXoff := False;
    SWFSentXoff := False;
    SWFOnChar := DefaultXonChar;
    SWFOffChar := DefaultXoffChar;
    HWFTransHonor := 0;
    HWFRecHonor := 0;
    HWFRemoteOff := False;
    LastXmitError := 0;

    Flags := DefPortOptions Or DefFossilOptions;
    Buffered := False;
    BreakReceived := False;
    TxReady := True;
    TxInts := False;
    LineStatus := 0;
    DoneProc := fDonePort;
    ErrorProc := NoErrorProc;
    ErrorData := Nil;
    UserAbort := NoAbortFunc;
    ProtocolActive := False;
    FaxActive := False;

    InBuffLen := 65535;
    InBuffCount := 0;
    OutBuff := Nil;
    OutHead := Nil;
    OutBuffEnd := Nil;
    OutBuffLen := 65535;
    OutBuffCount := 0;
    UseStatusBuffer := False;

  {$IFDEF DPMI}
    LongInt (StatBuff) := GlobalDosAlloc (InBufferLen);
    LongInt (StatHead) := GlobalDosAlloc (SizeOf (DriverInfo));
    LongInt (StatTail) := GlobalDosAlloc (OutBufferLen);

    If (StatBuff = Nil) Or (StatHead = Nil) Or (StatTail = Nil) Then
    Begin
      fClosePort (P);
      AsyncStatus := ecOutOfMemory;
      Exit;
    End;

    InBuff := Ptr (DosMemRec (StatBuff). Sele, 0);
  {$ELSE}
    StatBuff := Nil;
    StatHead := Nil;
    StatTail := Nil;

    If Not (GetMemCheck (StatBuff, InBufferLen) And
            GetMemCheck (StatHead, SizeOf (DriverInfo))) Then
    Begin
      AsyncStatus := ecOutOfMemory;
      fClosePort (P);
      Exit;
    End;

    InBuff := StatBuff;
  {$ENDIF}

    InHead := InBuff;
    InTail := InBuff;
    InBuffEnd := InBuff;
    Inc (OS (InBuffEnd). Ofs, InBufferLen);

    fGetLine (P, CurBaud, CurParity, CurDataBits, CurStopBits, True);
  End;
End;

Procedure fInitPort (Var P: PortRecPtr; ComName: ComNameType; Baud: LongInt;
                     Parity: ParityType; DataBits: DataBitType;
                     StopBits: StopBitType; InSize, OutSize: Word;
                     Options: Word);
Begin
  AsyncStatus := ecOk;

  fInitPortKeep (P, ComName, InSize, OutSize);
  If AsyncStatus = ecOk Then
    With P^ Do
    Begin
      fSetLine (P, Baud, Parity, DataBits, StopBits);
      If AsyncStatus <> ecOk Then
      Begin
        FreeMemCheck (P, SizeOf (PortRec));
        ActiveComPort [CurrentPort] := Nil;
      End
      Else
        Flags := Options Or DefFossilOptions;
    End;
End;

Procedure fDonePort (Var P: PortRecPtr);
Begin
  fClosePort (P);
  AsyncStatus := ecOk;
End;

Procedure fSetUart (ComName: ComNameType; NewBase: Word;
                    NewIrq, NewVector: Byte);
Begin
  AsyncStatus := epNonFatal + ecNotSupported;
End;

Function BaudMask (Baud: LongInt): Byte;
Begin
  Case (Baud Div 10) Of
    30   : BaudMask := $02;
    60   : BaudMask := $03;
    120  : BaudMask := $04;
    240  : BaudMask := $05;
    480  : BaudMask := $06;
    960  : BaudMask := $07;
    1920 : BaudMask := $00;
  Else
    BaudMask := $01;
  End;
End;

Procedure fSetLine (P: PortRecPtr; Baud: LongInt; Parity: ParityType;
                    DataBits: DataBitType; StopBits: StopBitType);
Var
  BaudCode, ParityCode, DataCode, StopCode : Byte;
  SaveAX                                   : Word;

Begin
  AsyncStatus := ecOk;

  If Baud = 0 Then BaudCode := BaudMask (P^. CurBaud)
              Else BaudCode := BaudMask (Baud);

  Case Parity Of
    NoParity   : ParityCode := 0;
    OddParity  : ParityCode := 1;
    EvenParity : ParityCode := 3;
  Else
    GotError(P, epFatal+ecInvalidParity);
    Exit;
  End;

  StopCode := StopBits - 1;
  DataCode := DataBits - 5;

  Regs. AH := $00;
  Regs. AL := (BaudCode Shl 5) + (ParityCode Shl 3) +
              (StopCode Shl 2) + DataCode;
  SaveAX := Regs. AX;
  Regs. DX := Ord (P^. PortName);
{$IFDEF DPMI}
  FossilIntr (Regs);
{$ELSE}
  Intr (FossilInt, Regs);
{$ENDIF}

  If SaveAX = Regs. AX Then
  Begin
    GotError (P, epFatal + ecBadPortNumber);
    Exit;
  End;

  With P^ Do
  Begin
    If Baud <> 0 Then
      CurBaud := Baud;
    CurParity := Parity;
    CurDataBits := DataBits;
    CurStopBits := StopBits;
  End;
End;

Procedure fGetLine(P: PortRecPtr; Var Baud: LongInt; Var Parity: ParityType;
                   Var DataBits: DataBitType; Var StopBits: StopBitType;
                   FromHardware: Boolean);
Begin
  If Not FromHardware Then
  Begin
    Baud := P^. CurBaud;
    Parity := P^. CurParity;
    DataBits := P^. CurDataBits;
    StopBits := P^. CurStopBits;
  End Else
  Begin
    Case (fGetDriverInfo (P)^. diBaudMask Shr 5) Of
      $00 : Baud := 19200;
      $01 : Baud := 38400;
      $02 : Baud := 300;
      $03 : Baud := 600;
      $04 : Baud := 1200;
      $05 : Baud := 2400;
      $06 : Baud := 4800;
      $07 : Baud := 9600;
    End;
    Parity := NoParity;
    DataBits := 8;
    StopBits := 1;
  End;
End;

Procedure fSetModem (P: PortRecPtr; DTR, RTS: Boolean);
Begin
  With P^ Do
  Begin
    Regs. AH := $06;
    Regs. AL := Ord (DTR);
    Regs. DX := Ord (PortName);
  {$IFDEF DPMI}
    FossilIntr (Regs);
  {$ELSE}
    Intr (FossilInt, Regs);
  {$ENDIF}

    ModemControl := ModemControl And Not (DTRMask Or RTSMask);
    If DTR Then
      ModemControl := ModemControl Or DTRMask;
    If RTS Then
      ModemControl := ModemControl Or RTSMask;
  End;
End;

Procedure fGetModem (P: PortRecPtr; Var DTR, RTS: Boolean);
Begin
  GotError (P, epNonFatal + ecNotSupported);
  DTR := True;
  RTS := True;
End;

Function fCharReadyPhys (P: PortRecPtr): Boolean;
Begin
  Regs. AH := $03;
  Regs. DX := Ord (P^. PortName);
{$IFDEF DPMI}
  FossilIntr (Regs);
{$ELSE}
  Intr (FossilInt, Regs);
{$ENDIF}
  fCharReadyPhys := Odd (Regs. AH);
End;

Procedure fFillBuffer (P: PortRecPtr);

  Procedure ReadData (Count: Word);
  Begin
    With P^, Regs Do
    Begin
      AH := $18;
      CX := Count;
      DX := Ord (PortName);
      DI := OS (InHead). Ofs;
    {$IFDEF DPMI}
      ES := DosMemRec (StatBuff). Segm;
      FossilIntr (Regs);
    {$ELSE}
      ES := OS (InHead). Seg;
      Intr (FossilInt, Regs);
    {$ENDIF}
      Inc (InBuffCount, AX);
      Inc (OS (InHead). Ofs, AX);
      If InHead = InBuffEnd Then
        InHead := InBuff;
    End;
  End;

Begin
  With P^ Do
    If OS (InHead). Ofs >= OS (InTail). Ofs Then
    Begin
      ReadData (OS (InBuffEnd). Ofs - OS (InHead). Ofs);
      If (InHead = InBuff) And (InTail <> InBuff) Then
        ReadData (OS (InTail). Ofs - OS (InBuff). Ofs);
    End
    Else
      ReadData (OS (InTail). Ofs - OS (InHead). Ofs);
End;

Procedure fGetChar (P: PortRecPtr; Var C: Char);
Begin
  AsyncStatus := ecOk;

  With P^ Do
  Begin
    If InBuffCount = 0 Then
    Begin
      fFillBuffer (P);
      If InBuffCount = 0 Then
      Begin
        GotError (P, epNonFatal + ecBufferIsEmpty);
        Exit;
      End;
    End;

    C := Char (InTail^);
    Inc (OS (InTail). Ofs);
    If InTail = InBuffEnd Then
      InTail := InBuff;
    Dec (InBuffCount);
  End;
End;

Procedure fPeekChar (P: PortRecPtr; Var C: Char; PeekAhead: Word);
Var
  TrcP  : BPtr;
  Count : Word;

Begin
  AsyncStatus := ecOk;

  With P^ Do
  Begin
    If PeekAhead > InBuffCount Then
    Begin
      fFillBuffer (P);

      If PeekAhead > InBuffCount Then
      Begin
        C := #$FF;
        GotError(P, epNonFatal + ecInvalidArgument);
        Exit;
      End;
    End;

    Count := OS (InTail). Ofs + PeekAhead - 1;
    If Count >= OS (InBuffEnd). Ofs Then
      Dec (Count, InBufferLen);
    TrcP := InBuff;
    Inc (OS (TrcP). Ofs, Count);
    C := Char (TrcP^);
  End;
End;

Procedure fPutChar (P: PortRecPtr; C: Char);
Begin
  AsyncStatus := ecOk;

  Regs. AL := Byte (C);
  Regs. DX := Ord (P^. PortName);

  If FlagIsSet (P^. Flags, ptPutCharWait) Then
  Begin
    Regs. AH := $01;
  {$IFDEF DPMI}
    FossilIntr (Regs);
  {$ELSE}
    Intr (FossilInt, Regs);
  {$ENDIF}
  End Else
  Begin
    Regs. AH := $0B;
  {$IFDEF DPMI}
    FossilIntr (Regs);
  {$ELSE}
    Intr (FossilInt, Regs);
  {$ENDIF}
    If Regs. AX = 0 Then
      GotError (P, epNonFatal + ecBufferIsFull);
  End;
End;

Procedure fStartTransmitter (P: PortRecPtr);
Begin
End;

Function fCharReady (P: PortRecPtr): Boolean;
Begin
  If P^. InBuffCount > 0 Then
    fCharReady := True
  Else
  Begin
    Regs. AH := $03;
    Regs. DX := Ord (P^. PortName);
  {$IFDEF DPMI}
    FossilIntr (Regs);
  {$ELSE}
    Intr (FossilInt, Regs);
  {$ENDIF}
    fCharReady := Odd (Regs. AH);
  End;
End;

Function fTransReady (P: PortRecPtr): Boolean;
Begin
  Regs. AH := $03;
  Regs. DX := Ord (P^. PortName);
{$IFDEF DPMI}
  FossilIntr (Regs);
{$ELSE}
  Intr (FossilInt, Regs);
{$ENDIF}

  fTransReady := (Regs. AH And $20) <> 0;
End;

Procedure fSendBreak (P: PortRecPtr);
Begin
(*  Regs. AX := $1A01;
  Regs. DX := Ord (P^. PortName);
{$IFDEF DPMI}
  FossilIntr (Regs);
{$ELSE}
  Intr (FossilInt, Regs);
{$ENDIF} *)
End;

Procedure fActivatePort (P: PortRecPtr; Restore: Boolean);
Begin
End;

Procedure fDeactivatePort (P: PortRecPtr; Restore: Boolean);
Begin
End;

Procedure fSavePort (P: PortRecPtr; Var PSR);
Begin
End;

Procedure fRestorePort (P: PortRecPtr; Var PSR);
Begin
End;

Function fUpdateLineStatus (P: PortRecPtr): Byte;
Begin
  With P^ Do
  Begin
    Regs. AH := $03;
    Regs. DX := Ord (PortName);
  {$IFDEF DPMI}
    FossilIntr (Regs);
  {$ELSE}
    Intr (FossilInt, Regs);
  {$ENDIF}
    ModemStatus := Regs. AL And ValidModemStatus;
    LineStatus := Regs. AH And ValidLineStatus;
    fUpdateLineStatus := LineStatus;
  End;
End;

Function fUpdateModemStatus (P: PortRecPtr): Byte;
Begin
  With P^ Do
  Begin
    Regs. AH := $03;
    Regs. DX := Ord (PortName);
  {$IFDEF DPMI}
    FossilIntr (Regs);
  {$ELSE}
    Intr (FossilInt, Regs);
  {$ENDIF}
    ModemStatus := Regs. AL And ValidModemStatus;
    LineStatus := Regs. AH And ValidLineStatus;
    fUpdateModemStatus := ModemStatus;
  End;
End;

{$IFDEF UseHWFlow}
Procedure fHWFlowSet (P: PortRecPtr; Enable: Boolean;
            BufferFull, BufferResume: Word; Options: Word);
Begin
  AsyncStatus := ecOk;

  With P^, Regs Do
  Begin
    AX := $0F00;

    If Enable Then
    Begin
      If (Options And (hfUseRTS + hfRequireCTS)) <> (hfUseRTS + hfRequireCTS) Then
      Begin
        GotError (P, ecInvalidArgument);
        Exit;
      End;

      AL := AL Or $02;
      HWFRecHonor := 1;
    End
    Else
      HWFRecHonor := 0;

    If FlagIsSet (SWFState, sfTransmitFlow) Then
      AL := AL Or $01;
    If FlagIsSet (SWFState, sfReceiveFlow) Then
      AL := AL Or $08;
    DX := Ord (PortName);
  {$IFDEF DPMI}
    FossilIntr (Regs);
  {$ELSE}
    Intr (FossilInt, Regs);
  {$ENDIF}
  End;
End;

Function fHWFlowGet (P: PortRecPtr): FlowState;
Begin
  If P^. HWFRecHonor = 0 Then fHWFlowGet := fsOff
                         Else fHWFlowGet := fsClear;
End;
{$ENDIF}

{$IFDEF UseSWFlow}
Procedure fSWFlowSet (P: PortRecPtr; Enable: Boolean;
                      BufferFull, BufferResume: Word; Options: Word);
Begin
  AsyncStatus := ecOk;

  With P^, Regs Do
  Begin
    AX := $0F00;

    If Enable Then
    Begin
      If FlagIsSet (Options, sfTransmitFlow) Then
        AL := AL Or $01;
      If FlagIsSet (Options, sfReceiveFlow) Then
        AL := AL Or $08;

      SWFState := Options; {Say it's on}
    End
    Else
      SWFState := 0; {Say it's off}

    If HWFRecHonor <> 0 Then
      AL := AL Or $02;
    DX := Ord (PortName);
  {$IFDEF DPMI}
    FossilIntr (Regs);
  {$ELSE}
    Intr (FossilInt, Regs);
  {$ENDIF}
  End;
End;

Function fSWFlowGet (P: PortRecPtr): FlowState;
Begin
  If P^. SWFState = 0 Then fSWFlowGet := fsOff
                      Else fSWFlowGet := fsClear;
End;

Procedure fSWFlowCtl (P: PortRecPtr; OnChar, OffChar: Char; Resume: Boolean);
Begin
  GotError (P, epNonFatal + ecNotSupported);
End;
{$ENDIF}

Procedure fBufferStatus (P: PortRecPtr; Var InFree, OutFree, InUsed,
                         OutUsed: Word);
Begin
  With P^, fGetDriverInfo (P)^ Do
  Begin
    InFree := diInFree;
    InUsed := diInSize - diInFree;
    OutUsed := diOutSize - diOutFree;
    If (OutUsed = 1) And FlagIsSet (Flags, ptHandleFossilBug) Then
      OutUsed := 0;

    If FlagIsSet (Flags, ptTrueOutBuffFree) Then
    Begin
      OutFree := diOutFree;
      If (OutFree = diOutSize - 1) And FlagIsSet (Flags, ptHandleFossilBug) Then
        OutFree := diOutSize;
    End
    Else
      If (LongInt (OutUsed) * 9) < diOutSize Then
        OutFree := 65535
      Else
        OutFree := 0;
  End;
End;

Procedure fBufferFlush (P: PortRecPtr; FlushIn, FlushOut: Boolean);
Begin
  With P^ Do
  Begin
    If FlushIn Then
    Begin
      Regs. AH := $0A;
      Regs. DX := Ord (PortName);
    {$IFDEF DPMI}
      FossilIntr (Regs);
    {$ELSE}
      Intr (FossilInt, Regs);
    {$ENDIF}

      InHead := InBuff;
      InTail := InBuff;
      InBuffCount := 0;
    End;

    If FlushOut Then
    Begin
      Regs. AH := $09;
      Regs. DX := Ord (PortName);
    {$IFDEF DPMI}
      FossilIntr (Regs);
    {$ELSE}
      Intr (FossilInt, Regs);
    {$ENDIF}
    End;
  End;
End;

Function fPutBlock (P: PortRecPtr; Var Block; Size: Word): Word;
Var
  Written : Word;
  InPtr   : BPtr;
{$IFDEF DPMI}
  Len     : Word;
  OutPtr  : BPtr;
{$ENDIF}

Begin
  Written := 0;

  If Size > 0 Then
  Begin
  {$IFDEF DPMI}
    InPtr := @Block;
    OutPtr := Ptr (DosMemRec (P^. StatTail). Sele, 0);
  {$ELSE}
    InPtr := Normalized (@Block);
  {$ENDIF}

    Repeat
      Regs. AH := $19;
      Regs. CX := Size - Written;
      Regs. DX := Ord (P^. PortName);

    {$IFDEF DPMI}
      If Regs. CX > OutBufferLen Then
        Regs. CX := OutBufferLen;
      Len := Regs. CX;
      Move (InPtr^, OutPtr^, Len);
      Regs. ES := DosMemRec (P^. StatTail). Segm;
      Regs. DI := 0;
      FossilIntr (Regs);
    {$ELSE}
      Regs. ES := OS (InPtr). Seg;
      Regs. DI := OS (InPtr). Ofs;
      Intr (FossilInt, Regs);
    {$ENDIF}

      Inc (Written, Regs. AX);
      If Written = Size Then
        Break;

      Inc (InPtr, Regs. AX);

    {$IFDEF DPMI}
      If Regs. AX < Len Then
    {$ENDIF}
        Repeat
          TimeSlice;

          Regs. AH := $03;
          Regs. DX := Ord (P^. PortName);
        {$IFDEF DPMI}
          FossilIntr (Regs);
        {$ELSE}
          Intr (FossilInt, Regs);
        {$ENDIF}

          If (Regs. AH And $20) <> 0 Then      { TX buffer has some room }
            Break;

          If (Regs. AL And $80) = 0 Then       { Carrier lost }
          Begin
            GotError (P, epNonFatal + ecBufferIsFull);
            fPutBlock := Written;
            Exit;
          End;
        Until False;
    Until False;
  End;

  AsyncStatus := ecOk;
  fPutBlock := Written;
End;

Procedure ActivateApFossil;
Begin
{$IFNDEF UseOOP}
  InitPort := fInitPort;
  InitPortKeep := fInitPortKeep;
  DonePort := fDonePort;
  SetLine := fSetLine;
  GetLine := fGetLine;
  SetModem := fSetModem;
  GetModem := fGetModem;
  GetChar := fGetChar;
  PeekChar := fPeekChar;
  PutChar := fPutChar;
  PutBlock := fPutBlock;
  CharReady := fCharReady;
  TransReady := fTransReady;
  SendBreak := fSendBreak;
  ActivatePort := fActivatePort;
  DeactivatePort := fDeactivatePort;
  SavePort := fSavePort;
  RestorePort := fRestorePort;

  UpdateLineStatus := fUpdateLineStatus;
  UpdateModemStatus := fUpdateModemStatus;

{$IFDEF UseHWFlow}
  HWFlowSet := fHWFlowSet;
  HWFlowGet := fHWFlowGet;
{$ENDIF}

{$IFDEF UseSWFlow}
  SWFlowSet := fSWFlowSet;
  SWFlowGet := fSWFlowGet;
  SWFlowCtl := fSWFlowCtl;
{$ENDIF}

  BufferStatus := fBufferStatus;
  BufferFlush := fBufferFlush;
{$ENDIF}

  SetUart := fSetUart;
End;

End.
