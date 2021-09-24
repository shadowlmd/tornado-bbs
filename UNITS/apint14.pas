{$I-,S-}
{$IFNDEF OS2}
{$R-,V-,B-,F+,A-} (* O+ *)
{$ENDIF}

{Conditional defines that may affect this unit}
{$I APDEFINE.INC}

{*********************************************************}
{*                    APINT14.PAS 2.03                   *}
{*     Copyright (c) TurboPower Software 1991.           *}
{*                 All rights reserved.                  *}
{*********************************************************}

unit ApInt14;
  {-Provides serial I/O using standard BIOS services (INT14)}

interface

uses
{$IFNDEF WIN32}
  DOS,
{$ENDIF}
  ApMisc,
  ApSame,
  ApPort;

procedure iInitPort(var P : PortRecPtr; ComName : ComNameType;
                    Baud : LongInt;
                    Parity : ParityType; DataBits : DataBitType;
                    StopBits : StopBitType;
                    InSize, OutSize : Word;
                    Options : Word);
procedure iInitPortKeep(var P : PortRecPtr; ComName : ComNameType;
                    InSize, OutSize : Word);
procedure iDonePort(var P : PortRecPtr);
procedure iSetUart(ComName : ComNameType; NewBase : Word;
                   NewIrq, NewVector : Byte);
procedure iSetLine(P : PortRecPtr; Baud : LongInt; Parity : ParityType;
                   DataBits : DataBitType; StopBits : StopBitType);
procedure iGetLine(P : PortRecPtr; var Baud : LongInt;
                   var Parity : ParityType;
                   var DataBits : DataBitType;
                   var StopBits : StopBitType;
                   FromHardware : Boolean);
procedure iSetModem(P : PortRecPtr; DTR, RTS : Boolean);
procedure iGetModem(P : PortRecPtr; var DTR, RTS : Boolean);
procedure iGetChar(P : PortRecPtr; var C : Char);
procedure iPeekChar(P : PortRecPtr; var C : Char; PeekAhead : Word);
procedure iPutChar(P : PortRecPtr; C : Char);
Function iPutBlock (P: PortRecPtr; Var Block; Size: Word): Word;
procedure iStartTransmitter(P : PortRecPtr);
function iCharReady(P : PortRecPtr) : Boolean;
function iTransReady(P : PortRecPtr) : Boolean;
procedure iSendBreak(P : PortRecPtr);
procedure iActivatePort(P : PortRecPtr; Restore : Boolean);
procedure iDeactivatePort(P : PortRecPtr; Restore : Boolean);
procedure iSavePort(P : PortRecPtr; var PSR);
procedure iRestorePort(P : PortRecPtr; var PSR);
function iUpdateLineStatus(P : PortRecPtr) : Byte;
function iUpdateModemStatus(P : PortRecPtr) : Byte;
{$IFDEF UseHWFlow}
procedure iHWFlowSet(P : PortRecPtr; Enable : Boolean;
                     BufferFull, BufferResume : Word;
                     Options : Word);
function iHWFlowGet(P : PortRecPtr) : FlowState;
{$ENDIF}
{$IFDEF UseSWFlow}
procedure iSWFlowSet(P : PortRecPtr; Enable : Boolean;
                     BufferFull, BufferResume : Word;
                     Options : Word);
function iSWFlowGet(P : PortRecPtr) : FlowState;
procedure iSWFlowCtl(P : PortRecPtr; OnChar, OffChar : Char;
                     Resume : Boolean);
{$ENDIF}
procedure iBufferStatus(P : PortRecPtr;
                        var InFree, OutFree, InUsed, OutUsed : Word);
procedure iBufferFlush(P : PortRecPtr; FlushIn, FlushOut: Boolean);
procedure ActivateApInt14;

  {=====================================================================}

implementation

var
  Regs : Registers;

procedure iUpdateLineAndModemStatus(P : PortRecPtr);
  {-Update line and modem status from BIOS}
begin
  with P^, Regs do begin
    AH := $03;
    DX := Ord(PortName);
    Intr($14, Regs);

    {Refresh status values}
    ModemStatus := AL;
    LineStatus := AH;
  end;
end;

procedure iInitPortKeep(var P : PortRecPtr; ComName : ComNameType;
                        InSize, OutSize : Word);
  {-Generic open port procedure}
var
  Found : Boolean;
  I : Byte;
begin
  AsyncStatus := ecOk;

  {For BIOS, ComName must be in Com1..Com4}
  if ComName > Com4 then begin
    AsyncStatus := ecBadPortNumber;
    Exit;
  end;

  {Allocate Port record}
  if not GetMemCheck(P, SizeOf(PortRec)) then begin
    AsyncStatus := ecOutOfMemory;
    Exit;
  end;

  with P^ do begin
    {Check for an available port slot}
    Found := False;
    I := 9;                                                     {!!.01}{!!.02}
    while not Found and (I <= MaxActivePort) do
      if ActiveComPort[I] = nil then begin
        CurrentPort := I;
        ActiveComPort[I] := P;
        Found := True;
      end else
        Inc(I);

    {Can't open port if no slots available}
    if not Found then begin
      FreeMemCheck(P, SizeOf(PortRec));
      AsyncStatus := ecNoMorePorts;
      Exit;
    end;

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
    Flags := DefPortOptions;
    Buffered := False;
    BreakReceived := False;
    TxReady := True;
    TxInts := False;
    LineStatus := 0;
    DoneProc := iDonePort;
    ErrorProc := NoErrorProc;
    ErrorData := nil;
    UserAbort := NoAbortFunc;
    ProtocolActive := False;
    FaxActive := False;

    {Use this flag for PeekChar buffering}                             {!!.02}
    ISRActive := False;                                                {!!.02}

    {Zero out buffer stuff (prevents errors if buffer routines are called)}
    InBuff := nil;
    InHead := nil;
    InBuffEnd := nil;
    InBuffLen := 65535;
    InBuffCount := 0;
    OutBuff := nil;
    OutHead := nil;
    OutBuffEnd := nil;
    OutBuffLen := 65535;
    OutBuffCount := 0;
    UseStatusBuffer := False;
    StatBuff := nil;
    StatHead := nil;
    StatTail := nil;
  end;
end;

procedure iInitPort(var P : PortRecPtr; ComName : ComNameType;
                    Baud : LongInt;
                    Parity : ParityType; DataBits : DataBitType;
                    StopBits : StopBitType;
                    InSize, OutSize : Word;
                    Options : Word);
  {-Generic open port procedure}
begin
  AsyncStatus := ecOk;

  {Allocate the port record and do inits}
  iInitPortKeep(P, ComName, InSize, OutSize);
  if AsyncStatus <> ecOk then
    Exit;

  with P^ do begin
    {Set the line parameters}
    iSetLine(P, Baud, Parity, DataBits, StopBits);
    if AsyncStatus <> ecOk then begin
      {Failed, release memory and free slot}
      FreeMemCheck(P, SizeOf(PortRec));
      ActiveComPort[CurrentPort] := nil;
    end;
  end;
end;

procedure iDonePort(var P : PortRecPtr);
  {-Closes ComName}
begin
  AsyncStatus := ecOk;

  {Show port slot as now available}
  ActiveComPort[P^.CurrentPort] := nil;

  {Release the heap space}
  FreeMemCheck(P, SizeOf(PortRec));
  P := Nil;
end;

procedure iSetUart(ComName : ComNameType; NewBase : Word;
                   NewIrq, NewVector : Byte);
  {-Dummy routine required by high-level routines}
begin
  {nothing to do}
end;

procedure iSetLine(P : PortRecPtr; Baud : LongInt; Parity : ParityType;
                  DataBits : DataBitType; StopBits : StopBitType);
  {-sets the port record with the new values}
var
  BaudCode,
  ParityCode,
  DataCode,
  StopCode : Byte;
  SaveAX : Word;
begin
  AsyncStatus := ecOk;

  with Regs do begin
    AH := $00;
    {Set Baud code}
    case Baud of
      110  : BaudCode := $00;
      150  : BaudCode := $01;
      300  : BaudCode := $02;
      600  : BaudCode := $03;
      1200 : BaudCode := $04;
      2400 : BaudCode := $05;
      4800 : BaudCode := $06;
      9600 : BaudCode := $07;
      else begin
        GotError(P, epFatal+ecInvalidBaudRate);
        Exit;
      end;
    end;

    {Set Parity code}
    case Parity of
      NoParity : ParityCode := 0;
      OddParity : ParityCode := 1;
      EvenParity : ParityCode := 3;
      else begin
        GotError(P, epFatal+ecInvalidParity);
        Exit;
      end;
    end;

    {Set databit and stopbit codes}
    StopCode := StopBits - 1;
    DataCode := DataBits - 5;

    {Assemble the option byte and try to set the options}
    AL := (BaudCode shl 5) + (ParityCode shl 3) +
          (StopCode shl 2) + DataCode;
    DX := Ord(P^.PortName) and $07;
    SaveAX := AX;
    Intr($14, Regs);

    with P^ do begin
    {Port is open, note first statuses, clear control regs}
      LineStatus := Regs.AH;
      ModemStatus := Regs.AL;
      LineControl := 0;
      ModemControl := 0;

    {Save line option values}
    CurBaud := Baud;
    CurParity := Parity;
    CurDataBits := DataBits;
    CurStopBits := StopBits;
    end;
  end;
end;

procedure iGetLine(P : PortRecPtr; var Baud : LongInt;
                   var Parity : ParityType;
                   var DataBits : DataBitType;
                   var StopBits : StopBitType;
                   FromHardware : Boolean);
  {-Does nothing (can't get line params from Int14)}
begin
  with P^ do
    if not FromHardware then begin
      {Return current field values}
      Baud := CurBaud;
      Parity := CurParity;
      DataBits := CurDataBits;
      StopBits := CurStopBits;
    end else begin
      Baud := 0;
      DataBits := 5;
      StopBits := 1;
      GotError(P, epNonFatal+ecNotSupported);
    end;
end;

procedure iSetModem(P : PortRecPtr; DTR, RTS : Boolean);
  {-Sets the port record with the new values}
begin
  GotError(P, epNonFatal+ecNotSupported);
end;

procedure iGetModem(P : PortRecPtr; var DTR, RTS : Boolean);
  {-Does nothing (can't get modem params from Int14)}
begin
  GotError(P, epNonFatal+ecNotSupported);
  DTR := False;
  RTS := False;
end;

procedure iGetChar(P : PortRecPtr; var C : Char);
  {-Calls int14 to check for and return C}
begin
  AsyncStatus := ecOk;

  with P^, Regs do begin

    {Return saved char if this is first GetChar after a PeekChar}      {!!.02}
    if ISRActive then begin                                            {!!.02}
      ISRActive := False;                                              {!!.02}
      C := SaveChar;                                                   {!!.02}
      Exit;                                                            {!!.02}
    end;                                                               {!!.02}

    AH := $02;
    DX := Ord(PortName);
    Intr($14, Regs);
    if (AH and $80) = $80 then begin                                   {!!.01}
      {Timeout waiting for char, report error}
      C := #$FF;
      GotError(P, epNonFatal+ecTimeout);
    end else begin
      {Get char and error bits}
      Byte(C) := AL;
      LineStatus := AH;
      if (LineStatus and $0E) <> 0 then
        GotError(P, epNonFatal+ecUartError);

      {$IFDEF Tracing}
      if TracingOn then
        AddTraceEntry('R', C);
      {$ENDIF}
    end;
  end;
end;

{!!.02 rewritten}
procedure iPeekChar(P : PortRecPtr; var C : Char; PeekAhead : Word);
  {-Note: ISRActive flag is used to flag that a character has been peeked}
begin
  with P^ do begin
    if (PeekAhead <> 1) then begin
      C := #$FF;
      GotError(P, epNonFatal+ecInvalidArgument);
    end else if ISRActive then begin
      {Consecutive call to PeekChar, return last saved character}
      C := SaveChar;
    end else if not iCharReady(P) then begin
      {No char to peek, return error}
      C := #$FF;
      GotError(P, epNonFatal+ecBufferIsEmpty);
    end else begin
      {Peek character by reading and saving it, next call to GetChar returns it}
      iGetChar(P, C);
      SaveChar := C;
      ISRActive := True;
    end;
  end;
end;

procedure iPutChar(P : PortRecPtr; C : Char);
  {-Puts a char via int14}
begin
  AsyncStatus := ecOk;

  {Call int14 to send a char}
  with P^, Regs do begin
    AL := Byte(C);
    AH := $01;
    DX := Ord(PortName);
    Intr($14, Regs);

    {Check for "timeout" bit}
    if (AH and $80) = $80 then                                         {!!.01}
      GotError(P, epNonFatal+ecTransmitFailed)
    else begin
      LineStatus := AH;

      {$IFDEF Tracing}
      if TracingOn then
        AddTraceEntry('T', C);
      {$ENDIF}
    end;
  end;
end;

procedure iStartTransmitter(P : PortRecPtr);
  {-Dummy procedure required by high-level routines}
begin
  {nothing to do}
end;

function iCharReady(P : PortRecPtr) : Boolean;
  {-Returns True if Int14 status call has DataReady set}
var
  Regs : Registers;
begin
  with P^, Regs do begin
    AH := $03;
    DX := Ord(PortName);
    Intr($14, Regs);

    {Refresh status values, set function result}
    ModemStatus := AL;
    LineStatus := AH;
    iCharReady := Odd(AH);
  end;
end;

function iTransReady(P : PortRecPtr) : Boolean;
  {-Returns True if Int14 status call has TransmitHoldingRegEmpty set}
begin
  with P^, Regs do begin
    AH := $03;
    DX := Ord(PortName);
    Intr($14, Regs);

    {Refresh status values, set function result}
    ModemStatus := AL;
    LineStatus := AH;
    iTransReady := (AH and $40) = $40
  end;
end;

procedure iSendBreak(P : PortRecPtr);
  {-Int14 can't send a break}
begin
  GotError(P, epNonFatal+ecNotSupported);
end;

procedure iActivatePort(P : PortRecPtr; Restore : Boolean);
  {-Does nothing -- Int14 uses polled I/O}
begin
  {nothing to do}
end;

procedure iDeactivatePort(P : PortRecPtr; Restore : Boolean);
  {-Does nothing -- Int14 uses polled I/O}
begin
  {nothing to do}
end;

procedure iSavePort(P : PortRecPtr; var PSR);
  {-Does nothing -- Int14 uses polled I/O}
begin
  {nothing to do}
end;

procedure iRestorePort(P : PortRecPtr; var PSR);
  {-Does nothing -- Int14 uses polled I/O}
begin
  {nothing to do}
end;

function iUpdateLineStatus(P : PortRecPtr) : Byte;
  {-Returns line status register value}
begin
  iUpdateLineAndModemStatus(P);
  iUpdateLineStatus := P^.LineStatus;
end;

function iUpdateModemStatus(P : PortRecPtr) : Byte;
  {-Returns modem status register value}
begin
  iUpdateLineAndModemStatus(P);
  iUpdateModemStatus := P^.ModemStatus;
end;

{$IFDEF UseHWFlow}
procedure iHWFlowSet(P : PortRecPtr; Enable : Boolean;
                     BufferFull, BufferResume : Word;
                     Options : Word);
  {-Enables/disables hardware flow control}
begin
  GotError(P, epNonFatal+ecNotSupported);
end;

function iHWFlowGet(P : PortRecPtr) : FlowState;
  {-Returns hardware flow control state}
begin
  GotError(P, epNonFatal+ecNotSupported);
  iHWFlowGet := fsOff;
end;
{$ENDIF}

{$IFDEF UseSWFlow}
procedure iSWFlowSet(P : PortRecPtr; Enable : Boolean;
                     BufferFull, BufferResume : Word;
                     Options : Word);
  {-Enables/disables software flow control}
begin
  GotError(P, epNonFatal+ecNotSupported);
end;

function iSWFlowGet(P : PortRecPtr) : FlowState;
  {-Returns software flow control state}
begin
  GotError(P, epNonFatal+ecNotSupported);
  iSWFlowGet := fsOff;
end;

procedure iSWFlowCtl(P : PortRecPtr; OnChar, OffChar : Char;
                     Resume : Boolean);
  {-Sets software flow control characters and/or resumes transmits}
begin
  GotError(P, epNonFatal+ecNotSupported);
end;
{$ENDIF}

procedure iBufferStatus(P : PortRecPtr;
                        var InFree, OutFree, InUsed, OutUsed : Word);
  {-Returns various buffer values}
begin
  InFree := 65535;
  OutFree := 65535;
  InUsed := 1;                                                         {!!.02}
  OutUsed := 0;
end;

procedure iBufferFlush(P : PortRecPtr; FlushIn, FlushOut: Boolean);
  {-Flushes input/output buffers}
begin
end;

Function iPutBlock (P: PortRecPtr; Var Block; Size: Word): Word;
Var
  i : Word;

Begin
  i := 0;

  While i < Size Do
  Begin
    iPutChar (P, CharArray (Block) [i]);
    If AsyncStatus <> ecOk Then
      Break;

    Inc (i);
  End;

  If i = Size Then AsyncStatus := ecOk
              Else GotError (P, epNonFatal + ecBufferIsFull);
  iPutBlock := i;
End;

procedure ActivateApInt14;
  {-Registers this unit as the active "device layer"}
begin
  {$IFNDEF UseOOP}
  InitPort := iInitPort;
  InitPortKeep := iInitPortKeep;
  DonePort := iDonePort;
  SetLine := iSetLine;
  GetLine := iGetLine;
  SetModem := iSetModem;
  GetModem := iGetModem;
  PeekChar := iPeekChar;
  GetChar := iGetChar;
  PutChar := iPutChar;
  PutBlock := iPutBlock;
  CharReady := iCharReady;
  TransReady := iTransReady;
  SendBreak := iSendBreak;
  ActivatePort := iActivatePort;
  DeactivatePort := iDeactivatePort;
  SavePort := iSavePort;
  RestorePort := iRestorePort;

  UpdateLineStatus := iUpdateLineStatus;
  UpdateModemStatus := iUpdateModemStatus;
  {$IFDEF UseHWFlow}
  HWFlowSet := iHWFlowSet;
  HWFlowGet := iHWFlowGet;
  {$ENDIF}
  {$IFDEF UseSWFlow}
  SWFlowSet := iSWFlowSet;
  SWFlowGet := iSWFlowGet;
  SWFlowCtl := iSWFlowCtl;
  {$ENDIF}
  BufferStatus := iBufferStatus;
  BufferFlush := iBufferFlush;

  {$ENDIF}
  SetUart := iSetUart;
end;

end.
