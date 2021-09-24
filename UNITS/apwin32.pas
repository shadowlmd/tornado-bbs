{ Win32 com interface, wrappers to EleCOM }

{$I APDEFINE.INC}

Unit ApWin32;

Interface

Uses
  ApSame;

Const
  ComHandle: Longint = -1;

Procedure wInitPort (Var P: PortRecPtr; ComName: ComNameType;
                     Baud: LongInt; Parity: ParityType;
                     DataBits: DataBitType; StopBits: StopBitType;
                     InSize, OutSize: Word; Options: Word);
Procedure wDonePort (Var P: PortRecPtr);
Procedure wGetLine (P: PortRecPtr; Var Baud: LongInt; Var Parity: ParityType;
                     Var DataBits: DataBitType; Var StopBits: StopBitType;
                     FromHardware: Boolean);
Procedure wSetLine (P: PortRecPtr; Baud: LongInt; Parity: ParityType;
                    DataBits: DataBitType; StopBits: StopBitType);
Procedure wSetModem (P: PortRecPtr; DTR, RTS: Boolean);
Procedure wGetModem (P: PortRecPtr; Var DTR, RTS: Boolean);
Procedure wGetChar (P: PortRecPtr; Var C: Char);
Procedure wPutChar (P: PortRecPtr; C: Char);
Function wPutBlock (P: PortRecPtr; Var Block; Size: Word): Word;
Function wCharReady (P: PortRecPtr): Boolean;
Function wTransReady (P: PortRecPtr): Boolean;

Function wUpdateLineStatus (P: PortRecPtr): Byte;
Function wUpdateModemStatus (P: PortRecPtr): Byte;
Procedure wBufferFlush (P: PortRecPtr; FlushIn, FlushOut: Boolean);

{$IFDEF UseHWFlow}
Procedure wHWFlowSet (P: PortRecPtr; Enable: Boolean;
                      BufferFull, BufferResume: Word;
                      Options: Word);
Function wHWFlowGet (P: PortRecPtr): FlowState;
{$ENDIF}

{$IFDEF UseSWFlow}
Procedure wSWFlowSet (P: PortRecPtr; Enable: Boolean;
                      BufferFull, BufferResume: Word;
                      Options: Word);
Function wSWFlowGet (P: PortRecPtr): FlowState;
Procedure wSWFlowCtl (P: PortRecPtr; OnChar, OffChar: Char;
                      Resume: Boolean);
{$ENDIF}

Procedure ActivateApW32;

Implementation

Uses
  Combase,
  w32sngl,
  OpInline;

Const
  InBufferLen = 1024;

Var
  ComObj : PCommObj;

Procedure Int_ComReadProc (Var TempPtr: Pointer);
Begin
  PWin32Obj (ComObj)^. Com_DataProc (TempPtr);
End;

Procedure Int_ComWriteProc (Var TempPtr: Pointer);
Begin
  PWin32Obj (ComObj)^. Com_DataProc (TempPtr);
End;

Function Parity2Char (Parity: ParityType): Char;
Begin
  Case Parity Of
      OddParity : Parity2Char := 'O';
     EvenParity : Parity2Char := 'E';
     MarkParity : Parity2Char := 'M';
    SpaceParity : Parity2Char := 'S';
  Else
    Parity2Char := 'N';
  End;
End;

Function Char2Parity (Ch: Char): ParityType;
Begin
  Case Ch Of
    'O' : Char2Parity := OddParity;
    'E' : Char2Parity := EvenParity;
    'M' : Char2Parity := MarkParity;
    'S' : Char2Parity := SpaceParity;
  Else
    Char2Parity := NoParity;
  End;
End;

Procedure wnInitPortKeep (Var P: PortRecPtr; ComName: ComNameType;
                          InSize, OutSize: Word);
Begin
End;

Procedure wInitPort (Var P: PortRecPtr; ComName: ComNameType; Baud: LongInt;
                     Parity: ParityType; DataBits: DataBitType;
                     StopBits: StopBitType; InSize, OutSize: Word;
                     Options: Word);
Begin
  {Allocate Port record}
  If Not GetMemCheck (P, SizeOf (PortRec)) Then
  Begin
    AsyncStatus := ecOutOfMemory;
    Exit;
  End;

  AsyncStatus := ecOk;

  With P^ Do
  Begin
    {Store the port name}
    PortName := ComName;

    {No control over the modem, set to zero for now}
    ModemControl := 0;

    {No flow control}
    SWFState := 0;
    SWFGotXoff := False;
    SWFSentXoff := False;
    SWFOnChar := DefaultXonChar;
    SWFOffChar := DefaultXoffChar;
    HWFTransHonor := 0;
    HWFRecHonor := 0;
    HWFRemoteOff := False;
    LastXmitError := 0;

    {Misc other inits}
    Flags := Options;
    Buffered := False;
    BreakReceived := False;
    TxReady := True;
    TxInts := False;
    LineStatus := 0;
    DoneProc := wDonePort;
    ErrorProc := NoErrorProc;
    ErrorData := Nil;
    UserAbort := NoAbortFunc;
    ProtocolActive := False;
    FaxActive := False;

    If Not GetMemCheck (InBuff, InBufferLen) Then
    Begin
      FreeMemCheck (P, SizeOf (PortRec));
      AsyncStatus := ecOutOfMemory;
      Exit;
    End;
    InHead := InBuff;
    InTail := InBuff;
    InBuffEnd := Pointer (LongInt (InBuff) + InBufferLen);
    InBuffLen := 65535;
    InBuffCount := 0;

    {Zero out buffer stuff (prevents errors if buffer routines are called)}
    OutBuff := Nil;
    OutHead := Nil;
    OutBuffEnd := Nil;
    OutBuffLen := 65535;
    OutBuffCount := 0;
    UseStatusBuffer := False;
    StatBuff := Nil;
    StatHead := Nil;
    StatTail := Nil;
  End;

  ComObj := New (pWin32Obj, Init);
  ComObj^. Com_SetDataProc (@Int_ComReadProc, @Int_ComWriteProc);

  If ComHandle <> -1 Then
    ComObj^. Com_OpenQuick (ComHandle)
  Else
    If Not ComObj^. Com_Open (Ord (ComName) + 1, Baud, DataBits,
      Parity2Char (Parity), StopBits) Then
    Begin
      wDonePort (P);
      AsyncStatus := ecPortNotOpen;
      Exit;
    End;

  wGetLine (P, P^. CurBaud, P^. CurParity, P^. CurDataBits,
    P^. CurStopBits, True);
End;

Procedure wDonePort (Var P: PortRecPtr);
Begin
  AsyncStatus := ecOk;

  If ComObj <> Nil Then
  Begin
    ComObj^. Com_PauseCom(ComHandle = -1);
    Dispose (ComObj, Done);
  End;

  FreeMemCheck (P^. InBuff, InBufferLen);
  FreeMemCheck (P, SizeOf (PortRec));
End;

Procedure wGetLine (P: PortRecPtr; Var Baud: LongInt; Var Parity: ParityType;
                     Var DataBits: DataBitType; Var StopBits: StopBitType;
                     FromHardware: Boolean);
Var
  Ch : Char;

Begin
  With P^ Do
    If Not FromHardware Then
    Begin
      {Return current field values}
      Baud := CurBaud;
      Parity := CurParity;
      DataBits := CurDataBits;
      StopBits := CurStopBits;
    End Else
    Begin
      PWin32Obj (ComObj)^. Com_GetLine (Baud, Ch, Byte (DataBits),
        Byte (StopBits));
      Parity := Char2Parity (Ch);
    End;
End;

Procedure wSetLine (P: PortRecPtr; Baud: LongInt; Parity: ParityType;
                    DataBits: DataBitType; StopBits: StopBitType);
Begin
  AsyncStatus := ecOk;

  ComObj^. Com_SetLine (Baud, Parity2Char (Parity), DataBits, StopBits);

  With P^ Do
  Begin
    If Baud <> 0 Then
      CurBaud := Baud;
    CurParity := Parity;
    CurDataBits := DataBits;
    CurStopBits := StopBits;
  End;
End;

Procedure wGetModem (P: PortRecPtr; Var DTR, RTS: Boolean);
Begin
  GotError (P, epNonFatal + ecNotSupported);
  DTR := True;
  RTS := True;
End;

Procedure wSetModem (P: PortRecPtr; DTR, RTS: Boolean);
Begin
  PWin32Obj (ComObj)^. Com_SetDtr (DTR);
  PWin32Obj (ComObj)^. Com_SetRts (RTS);

  With P^ Do
  Begin
    ModemControl := ModemControl And Not (DTRMask Or RTSMask);
    If DTR Then
      ModemControl := ModemControl Or DTRMask;
    If RTS Then
      ModemControl := ModemControl Or RTSMask;
  End;
End;

Function wUpdateModemStatus (P: PortRecPtr): Byte;
Begin
  ComObj^. Com_GetModemStatus (P^. LineStatus, P^. ModemStatus);
  wUpdateModemStatus := P^. ModemStatus;
End;

Function wUpdateLineStatus (P: PortRecPtr): Byte;
Begin
  ComObj^. Com_GetModemStatus (P^. LineStatus, P^. ModemStatus);
  wUpdateLineStatus := P^. LineStatus;
End;

Procedure wFillBuffer (P: PortRecPtr);

  Procedure ReadData (Count: LongInt);
  Var
    Reads : LongInt;

  Begin
    With P^ Do
    Begin
      PWin32Obj (ComObj)^. Com_ReadMax (InHead^, Count, Reads);
      Inc (InBuffCount, Reads);
      Inc (InHead, Reads);
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

Function wCharReady (P: PortRecPtr): Boolean;
Begin
  wCharReady := (P^. InBuffCount > 0) Or ComObj^. Com_CharAvail;
End;

Procedure wGetChar (P: PortRecPtr; Var C: Char);
Begin
  AsyncStatus := ecOk;

  With P^ Do
  Begin
    If InBuffCount = 0 Then
    Begin
      wFillBuffer (P);
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

Function wTransReady (P: PortRecPtr): Boolean;
Var
  InFree, OutFree, InUsed, OutUsed : LongInt;

Begin
  ComObj^. Com_GetBufferStatus (InFree, OutFree, InUsed, OutUsed);
  wTransReady := OutFree > 0;
End;

Procedure wPutChar (P: PortRecPtr; C: Char);
Begin
  If ComObj^. Com_SendChar (C) Then AsyncStatus := ecOk
                               Else GotError (P, epNonFatal + ecBufferIsFull);
End;

Function wPutBlock (P: PortRecPtr; Var Block; Size: Word): Word;
Var
  Written : LongInt;

Begin
  ComObj^. Com_SendBlock (Block, Size, Written);

  If Written = Size Then AsyncStatus := ecOk
                    Else GotError (P, epNonFatal + ecBufferIsFull);
  wPutBlock := Written;
End;

Procedure wBufferStatus (P: PortRecPtr; Var InFree, OutFree, InUsed, OutUsed: Word);
Begin
  ComObj^. Com_GetBufferStatus (InFree, OutFree, InUsed, OutUsed);

  If LongInt (OutUsed) * 9 < OutFree Then
    OutFree := 65535
  Else
    OutFree := 0;
End;

Procedure wBufferFlush (P: PortRecPtr; FlushIn, FlushOut: Boolean);
Begin
  If FlushIn Then
  Begin
    ComObj^. Com_PurgeInBuffer;

    With P Do
    Begin
      InHead := InBuff;
      InTail := InBuff;
      InBuffCount := 0;
    End;
  End;

  If FlushOut Then
    ComObj^. Com_PurgeOutBuffer;
End;

Procedure SetupComFlow (P: PortRecPtr);
Begin
  ComObj^. Com_SetFlow (FlagIsSet (P^. SWFState, sfTransmitFlow),
    FlagIsSet (P^. SWFState, sfReceiveFlow), P^. HWFRecHonor = 1);
  AsyncStatus := ecOk;
End;

{$IFDEF UseHWFlow}
Procedure wHWFlowSet (P: PortRecPtr; Enable: Boolean;
                      BufferFull, BufferResume: Word; Options: Word);
                      {-Enables/disables hardware flow control}
Begin
  If Enable Then
  Begin
    If (Options And (hfUseRTS + hfRequireCTS)) <> (hfUseRTS + hfRequireCTS) Then
    Begin
      GotError (P, ecInvalidArgument);
      Exit;
    End;
    P^. HWFRecHonor := 1;
  End
  Else
    P^. HWFRecHonor := 0;

  SetupComFlow (P);
End;

Function wHWFlowGet (P: PortRecPtr): FlowState;
  {-Returns hardware flow control state, on or off only}
Begin
  If P^. HWFRecHonor = 1 Then wHWFlowGet := fsClear
                         Else wHWFlowGet := fsOff;
End;
{$ENDIF}

{$IFDEF UseSWFlow}
Procedure wSWFlowSet (P: PortRecPtr; Enable: Boolean;
                      BufferFull, BufferResume: Word; Options: Word);
  {-Enables/disables software flow control}
Begin
  If Enable Then P^. SWFState := Options
            Else P^. SWFState := 0;

  SetupComFlow (P);
End;

Function wSWFlowGet (P: PortRecPtr): FlowState;
  {-Returns software flow control state}
Begin
  If P^. SWFState <> 0 Then wSWFlowGet := fsClear
                       Else wSWFlowGet := fsOff;
End;

Procedure wSWFlowCtl (P: PortRecPtr; OnChar, OffChar: Char; Resume: Boolean);
  {-Sets software flow control characters and/or resumes transmits}
Begin
  GotError (P, epNonFatal + ecNotSupported);
End;
{$ENDIF}

Procedure wPeekChar (P: PortRecPtr; Var C: Char; PeekAhead: Word);
Begin
  AsyncStatus := epNonFatal + ecNotSupported;
End;

Procedure wNothing (P: PortRecPtr);
Begin
End;

Procedure wnSendBreak (P: PortRecPtr);
Begin
End;

Procedure wDeactivatePort (P: PortRecPtr; Restore: Boolean);
Begin
  If ComHandle <> -1 Then
    Exit;

  GetLine (P, P^. CurBaud, P^. CurParity, P^. CurDataBits, P^. CurStopBits, True);
  ComObj^. Com_Close;
  AsyncStatus := ecOk;
End;

Procedure wActivatePort (P: PortRecPtr; Restore: Boolean);
Begin
  If ComHandle <> -1 Then
    Exit;

  If Not ComObj^. Com_Open (Ord (P^. PortName) + 1, P^. CurBaud, P^. CurDataBits,
    Parity2Char (P^. CurParity), P^. CurStopBits) Then
  Begin
    DonePort (P);
    AsyncStatus := ecPortNotOpen;
  End
  Else
    SetupComFlow (P);
End;

Procedure wnSavePort (P: PortRecPtr; Var PSR);
Begin
End;

Procedure wnRestorePort (P: PortRecPtr; Var PSR);
Begin
End;

Procedure wnSetUart (ComName: ComNameType; NewBase: Word; NewIrq, NewVector: Byte);
Begin
End;

Procedure ActivateApW32;
Begin
  InitPort := wInitPort;
  InitPortKeep := wnInitPortKeep;
  DonePort := wDonePort;
  SetLine := wSetLine;
  GetLine := wGetLine;
  SetModem := wSetModem;
  GetModem := wGetModem;
  GetChar := wGetChar;
  PeekChar := wPeekChar;
  PutChar := wPutChar;
  PutBlock := wPutBlock;
  CharReady := wCharReady;
  TransReady := wTransReady;
  SendBreak := wnSendBreak;
  ActivatePort := wActivatePort;
  DeactivatePort := wDeactivatePort;
  SavePort := wnSavePort;
  RestorePort := wnRestorePort;
  UpdateLineStatus := wUpdateLineStatus;
  UpdateModemStatus := wUpdateModemStatus;

  {$IFDEF UseHWFlow}
  HWFlowSet := wHWFlowSet;
  HWFlowGet := wHWFlowGet;
  {$ENDIF}

  {$IFDEF UseSWFlow}
  SWFlowSet := wSWFlowSet;
  SWFlowGet := wSWFlowGet;
  SWFlowCtl := wSWFlowCtl;
  {$ENDIF}

  BufferStatus := wBufferStatus;
  BufferFlush := wBufferFlush;

  SetUart := wnSetUart;
End;

End.
