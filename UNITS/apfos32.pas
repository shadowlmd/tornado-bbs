{ Fossil interface for /386 (32bit flat model, VP only) }

{$I APDEFINE.INC}
{&Delphi+}

Unit ApFos32;

Interface

Uses
  Dpmi32,
  Dpmi32df,
  OpInline,
  ApSame;

Type
  PDriverInfo = ^DriverInfo;
  DriverInfo = Record
    diSize     : System. Word;
    diSpec     : Byte;
    diRev      : Byte;
    diID       : Pointer;
    diInSize   : System. Word;
    diInFree   : System. Word;
    diOutSize  : System. Word;
    diOutFree  : System. Word;
    diSWidth   : Byte;
    diSHeight  : Byte;
    diBaudMask : Byte;
    diJunk     : System. Word;
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
  OutBufferLen      = 2048;

Procedure fInitPort (Var P: PortRecPtr; ComName: ComNameType; Baud: LongInt;
            Parity: ParityType; DataBits: DataBitType; StopBits: StopBitType;
            InSize, OutSize: Word; Options: Word);
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
Function fCharReady (P: PortRecPtr): Boolean;
Function fTransReady (P: PortRecPtr): Boolean;
Procedure fStartTransmitter (P: PortRecPtr);
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
                     BufferFull, BufferResume: Word;
                     Options: Word);
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
  vpUtils,
  TimeTask;

Var
  Regs : real_mode_call_structure_typ;

Function fGetDriverInfo (P: PortRecPtr): PDriverInfo;
Begin
  Regs. AH_ := $1B;
  Regs. CX_ := SizeOf (DriverInfo);
  Regs. DX_ := Ord (P^. PortName);
  Regs. ES_ := SmallWord (P^. StatHead);
  Regs. DI_ := 0;
  intr_RealMode (Regs, FossilInt);

  fGetDriverInfo := Pointer (dosseg_linear (SmallWord (P^. StatHead)));
End;

Procedure fClosePort (Var P: PortRecPtr);
Begin
  If P <> Nil Then
  Begin
    Regs. AH_ := $05;
    Regs. DX_ := Ord (P^. PortName);
    intr_RealMode (Regs, FossilInt);

    If P^. StatBuff <> Nil Then
      FreeDosMem (SmallWord (P^. StatBuff));
    If P^. StatHead <> Nil Then
      FreeDosMem (SmallWord (P^. StatHead));
    If P^. StatTail <> Nil Then
      FreeDosMem (SmallWord (P^. StatTail));

    ActiveComPort [P^. CurrentPort] := Nil;

    FreeMemCheck (P, SizeOf (PortRec));
  End;
End;

Function AllocDosMem (Var P: BPtr; Len: Word): Boolean;
Var
  RMSeg : SmallWord;

Begin
  If GetDosMem (RMSeg, Len) <> 0 Then
  Begin
    P := Nil;
    AllocDosMem := False;
  End Else
  Begin
    LongInt (P) := RMSeg;
    AllocDosMem := True;
  End;
End;

Procedure fInitPortKeep (Var P: PortRecPtr; ComName: ComNameType;
                         InSize, OutSize: Word);
Var
  i     : Integer;
  Found : Boolean;

Begin
  Regs. AH_ := $04;
  Regs. BX_ := 0;
  Regs. DX_ := Ord (ComName);
  intr_RealMode (Regs, FossilInt);
  If Regs. AX_ <> FossilSignature Then
  Begin
    AsyncStatus := ecNoFossil;
    Exit;
  End;

  If Not GetMemCheck (P, SizeOf (PortRec)) Then
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

    OutBuff := Nil;
    OutHead := Nil;
    OutBuffEnd := Nil;
    OutBuffLen := 65535;
    OutBuffCount := 0;
    UseStatusBuffer := False;

    If Not (AllocDosMem (StatBuff, InBufferLen) And
            AllocDosMem (StatHead, SizeOf (DriverInfo)) And
            AllocDosMem (StatTail, OutBufferLen)) Then
    Begin
      fClosePort (P);
      AsyncStatus := ecOutOfMemory;
      Exit;
    End;

    InBuff := Pointer (dosseg_linear (SmallWord (StatBuff)));
    InHead := InBuff;
    InTail := InBuff;
    InBuffEnd := Pointer (LongInt (InBuff) + InBufferLen);
    InBuffLen := 65535;
    InBuffCount := 0;

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
  Begin
    fSetLine (P, Baud, Parity, DataBits, StopBits);
    If AsyncStatus = ecOk Then
      P^. Flags := Options Or DefFossilOptions
    Else
      fClosePort (P);
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
      30 : BaudMask := $02;
      60 : BaudMask := $03;
     120 : BaudMask := $04;
     240 : BaudMask := $05;
     480 : BaudMask := $06;
     960 : BaudMask := $07;
    1920 : BaudMask := $00;
  Else
    BaudMask := $01;
  End;
End;

Procedure fSetLine (P: PortRecPtr; Baud: LongInt; Parity: ParityType;
                    DataBits: DataBitType; StopBits: StopBitType);
Var
  BaudCode, ParityCode, DataCode, StopCode : Byte;
  SaveAX                                   : SmallWord;

Begin
  AsyncStatus := ecOk;

  If Baud <> 0 Then BaudCode := BaudMask (Baud)
               Else BaudCode := BaudMask (P^. CurBaud);

  Case Parity Of
      NoParity : ParityCode := 0;
     OddParity : ParityCode := 1;
    EvenParity : ParityCode := 3;
  Else
    GotError (P, epFatal + ecInvalidParity);
    Exit;
  End;

  StopCode := StopBits - 1;
  DataCode := DataBits - 5;

  Regs. AH_ := 0;
  Regs. AL_ := (BaudCode Shl 5) + (ParityCode Shl 3) +
               (StopCode Shl 2) + DataCode;
  SaveAX := Regs. AX_;
  Regs. DX_ := Ord (P^. PortName) And $07;
  intr_RealMode (Regs, FossilInt);

  If SaveAX = Regs. AX_ Then
    GotError (P, epFatal + ecBadPortNumber)
  Else
    With P^ Do
    Begin
      If Baud <> 0 Then
        CurBaud := Baud;
      CurParity := Parity;
      CurDataBits := DataBits;
      CurStopBits := StopBits;
    End;
End;

Procedure fGetLine (P: PortRecPtr; Var Baud: LongInt; Var Parity: ParityType;
                    Var DataBits: DataBitType; Var StopBits: StopBitType;
                    FromHardware: Boolean);
Var
  Info : DriverInfo;

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
    Regs. AH_ := $06;
    Regs. AL_ := Ord (DTR);
    Regs. DX_ := Ord (PortName);
    intr_RealMode (Regs, FossilInt);

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
  Regs. AH_ := $03;
  Regs. DX_ := Ord (P^. PortName);
  intr_RealMode (Regs, FossilInt);
  fCharReadyPhys := Odd (Regs. AH_);
End;

Procedure fFillBuffer (P: PortRecPtr);

  Procedure ReadData (Count: Word);
  Begin
    With P^, Regs Do
    Begin
      AH_ := $18;
      ES_ := SmallWord (StatBuff);
      DI_ := LongInt (InHead) - LongInt (InBuff);
      CX_ := Count;
      DX_ := Ord (PortName);
      intr_RealMode (Regs, FossilInt);
      Inc (InBuffCount, AX_);
      Inc (InHead, AX_);
      If InHead = InBuffEnd Then
        InHead := InBuff;
    End;
  End;

Begin
  With P^ Do
    If LongInt (InHead) >= LongInt (InTail) Then
    Begin
      ReadData (LongInt (InBuffEnd) - LongInt (InHead));
      If (InHead = InBuff) And (InTail <> InBuff) Then
        ReadData (LongInt (InTail) - LongInt (InBuff));
    End
    Else
      ReadData (LongInt (InTail) - LongInt (InHead));
End;

Procedure fPeekChar (P: PortRecPtr; Var C: Char; PeekAhead: Word);
Type
  ChPtr = ^Char;

Var
  Count : Word;

Begin
  AsyncStatus := ecOk;

  With P^ Do
  Begin
    If PeekAhead > InBuffCount Then
    Begin
      fFillBuffer (P); {Peeking too far, try to refill buffer}
      If PeekAhead > InBuffCount Then
      Begin
        {Still too far, give up}
        C := #$FF;
        GotError(P, epNonFatal + ecInvalidArgument);
        Exit;
      End;
    End;

    Count := LongInt (InTail) + PeekAhead - 1;
    If Count >= InBufferLen Then
      Dec (Count, InBufferLen);
    C := ChPtr (LongInt (InBuff) + Count)^;
  End;
End;

Procedure fPutChar (P: PortRecPtr; C: Char);
Begin
  AsyncStatus := ecOk;

  Regs. AL_ := Byte (C);
  Regs. DX_ := Ord (P^. PortName);

  If FlagIsSet (P^. Flags, ptPutCharWait) Then
  Begin
    Regs. AH_ := $01;
    intr_RealMode (Regs, FossilInt);
  End Else
  Begin
    Regs. AH_ := $0B;
    intr_RealMode (Regs, FossilInt);
    If Regs. AX_ = 0 Then
    Begin
      GotError (P, epNonFatal + ecBufferIsFull);
      Exit;
    End;
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
    Regs. AH_ := $03;
    Regs. DX_ := Ord (P^. PortName);
    intr_RealMode (Regs, FossilInt);
    fCharReady := Odd (Regs. AH_);
  End;
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
    Inc (InTail);
    If InTail = InBuffEnd Then
      InTail := InBuff;
    Dec (InBuffCount);
  End;
End;

Function fTransReady (P: PortRecPtr): Boolean;
Begin
  Regs. AH_ := $03;
  Regs. DX_ := Ord (P^. PortName);
  intr_RealMode (Regs, FossilInt);
  fTransReady := (Regs. AH_ And $20) <> 0;
End;

Procedure fSendBreak (P: PortRecPtr);
Begin
(*  Regs. AX_ := $1A01;
  Regs. DX_ := Ord (P^. PortName);
  intr_RealMode (Regs, FossilInt); *)
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
    Regs. AH_ := $03;
    Regs. DX_ := Ord (PortName);
    intr_RealMode (Regs, FossilInt);
    ModemStatus := Regs. AL_ And ValidModemStatus;
    LineStatus := Regs. AH_ And ValidLineStatus;
    fUpdateLineStatus := LineStatus;
  End;
End;

Function fUpdateModemStatus (P: PortRecPtr): Byte;
  {-Returns modem status register value}
Begin
  With P^ Do
  Begin
    Regs. AH_ := $03;
    Regs. DX_ := Ord (PortName);
    intr_RealMode (Regs, FossilInt);
    ModemStatus := Regs. AL_ And ValidModemStatus;
    LineStatus := Regs. AH_ And ValidLineStatus;
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
    AX_ := $0F00;

    If Enable Then
    Begin
      If (Options And (hfUseRTS + hfRequireCTS)) <> (hfUseRTS + hfRequireCTS) Then
      Begin
        GotError (P, ecInvalidArgument);
        Exit;
      End;

      AL_ := AL_ Or $02;
      HWFRecHonor := 1;
    End
    Else
      HWFRecHonor := 0;

    If FlagIsSet (SWFState, sfTransmitFlow) Then
      AL_ := AL_ Or $01;
    If FlagIsSet (SWFState, sfReceiveFlow) Then
      AL_ := AL_ Or $08;
    DX_ := Ord (PortName);
    intr_RealMode (Regs, FossilInt);
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
    AX_ := $0F00;

    If Enable Then
    Begin
      If FlagIsSet (Options, sfTransmitFlow) Then
        AL_ := AL_ Or $01;
      If FlagIsSet (Options, sfReceiveFlow) Then
        AL_ := AL_ Or $08;

      SWFState := Options; {Say it's on}
    End
    Else
      SWFState := 0; {Say it's off}

    If HWFRecHonor <> 0 Then
      AL_ := AL_ Or $02;
    DX_ := Ord (PortName);
    intr_RealMode (Regs, FossilInt);
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
      Regs. AH_ := $0A;
      Regs. DX_ := Ord (PortName);
      intr_RealMode (Regs, FossilInt);

      InHead := InBuff;
      InTail := InBuff;
      InBuffCount := 0;
    End;

    If FlushOut Then
    Begin
      Regs. AH_ := $09;
      Regs. DX_ := Ord (PortName);
      intr_RealMode (Regs, FossilInt);
    End;
  End;
End;

Function fPutBlock (P: PortRecPtr; Var Block; Size: Word): Word;
Var
  InPtr, OutPtr : BPtr;
  Len           : Word;

Begin
  Result := 0;

  If Size > 0 Then
  Begin
    InPtr := @Block;
    OutPtr := Pointer (dosseg_linear (SmallWord (P^. StatTail)));

    Repeat
      Len := Min (OutBufferLen, Size - Result);
      Move (InPtr^, OutPtr^, Len);

      Regs. AH_ := $19;
      Regs. CX_ := Len;
      Regs. DX_ := Ord (P^. PortName);
      Regs. ES_ := SmallWord (P^. StatTail);
      Regs. DI_ := 0;
      intr_RealMode (Regs, FossilInt);

      Inc (Result, Regs. AX_);
      If Result = Size Then
        Break;

      Inc (InPtr, Regs. AX_);

      If Regs. AX_ < Len Then
        Repeat
          TimeSlice;

          Regs. AH_ := $03;
          Regs. DX_ := Ord (P^. PortName);
          intr_RealMode (Regs, FossilInt);

          If (Regs. AH_ And $20) <> 0 Then     { TX buffer has some room }
            Break;

          If (Regs. AL_ And $80) = 0 Then      { Carrier lost }
          Begin
            GotError (P, epNonFatal + ecBufferIsFull);
            Exit;
          End;
        Until False;
    Until False;
  End;

  AsyncStatus := ecOk;
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
