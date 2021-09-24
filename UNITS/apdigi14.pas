{$I-,S-}
{$IFNDEF OS2}
{$R-,V-,B-,F+,A-} (* O+ *)
{$ENDIF}

{Conditional defines that may affect this unit}
{$I APDEFINE.INC}

{*********************************************************}
{*                  APDIGI14.PAS 2.03                    *}
{*     Copyright (c) TurboPower Software 1991.           *}
{*                 All rights reserved.                  *}
{*********************************************************}

unit ApDigi14;
  {-A Digiboard device layer supporting the Digiboard Universal Driver}

interface

Uses
{$IFDEF DPMI}
  Dpmi,
  WinAPI,
{$ENDIF}
{$IFNDEF WIN32}
  DOS,
{$ENDIF}
  ApMisc,
  ApSame,
  OpInline,
  ApPort;

const
  {---- Digi14 options ----}
  ptReadWriteWait     = $8000; {Set to use "wait" PutChar/GetChar}
  ptClearFlow         = $4000; {Set to clear flow control on startup}
  {ptBufferGetChar     = $1000; {Set to use buffered reads}     {!!.02}{!!.03}

  {---- Default options ----}
  DefDigi14OptionsSimple  = ptBufferGetChar;
  DefDigi14Options : Word = DefDigi14OptionsSimple;

  DigBufferMax : Word = 1024;       {Max size of getchar buffer}  {!!.02}{!!.03}

procedure dInitPort(var P : PortRecPtr; ComName : ComNameType;
                    Baud : LongInt;
                    Parity : ParityType; DataBits : DataBitType;
                    StopBits : StopBitType;
                    InSize, OutSize : Word;
                    Options : Word);
procedure dInitPortKeep(var P : PortRecPtr; ComName : ComNameType;
                    InSize, OutSize : Word);
procedure dDonePort(var P : PortRecPtr);
procedure dSetUart(ComName : ComNameType; NewBase : Word;
                   NewIrq, NewVector : Byte);
procedure dSetLine(P : PortRecPtr; Baud : LongInt; Parity : ParityType;
                   DataBits : DataBitType; StopBits : StopBitType);
procedure dGetLine(P : PortRecPtr; var Baud : LongInt;
                   var Parity : ParityType;
                   var DataBits : DataBitType;
                   var StopBits : StopBitType;
                   FromHardware : Boolean);
procedure dSetModem(P : PortRecPtr; DTR, RTS : Boolean);
procedure dGetModem(P : PortRecPtr; var DTR, RTS : Boolean);
procedure dGetChar(P : PortRecPtr; var C : Char);
procedure dPeekChar(P : PortRecPtr; var C : Char; PeekAhead : Word);
procedure dPutChar(P : PortRecPtr; C : Char);
Function dPutBlock (P: PortRecPtr; Var Block; Size: Word): Word;
procedure dStartTransmitter(P : PortRecPtr);
function dCharReady(P : PortRecPtr) : Boolean;
function dTransReady(P : PortRecPtr) : Boolean;
procedure dSendBreak(P : PortRecPtr);
procedure dActivatePort(P : PortRecPtr; Restore : Boolean);
procedure dDeactivatePort(P : PortRecPtr; Restore : Boolean);
procedure dSavePort(P : PortRecPtr; var PSR);
procedure dRestorePort(P : PortRecPtr; var PSR);
function dUpdateLineStatus(P : PortRecPtr) : Byte;
function dUpdateModemStatus(P : PortRecPtr) : Byte;
{$IFDEF UseHWFlow}
procedure dHWFlowSet(P : PortRecPtr; Enable : Boolean;
                     BufferFull, BufferResume : Word;
                     Options : Word);
function dHWFlowGet(P : PortRecPtr) : FlowState;
{$ENDIF}
{$IFDEF UseSWFlow}
procedure dSWFlowSet(P : PortRecPtr; Enable : Boolean;
                     BufferFull, BufferResume : Word;
                     Options : Word);
function dSWFlowGet(P : PortRecPtr) : FlowState;
procedure dSWFlowCtl(P : PortRecPtr; OnChar, OffChar : Char;
                     Resume : Boolean);
{$ENDIF}
procedure dBufferStatus(P : PortRecPtr;
                        var InFree, OutFree, InUsed, OutUsed : Word);
procedure dBufferFlush(P : PortRecPtr; FlushIn, FlushOut: Boolean);
procedure ActivateApDigi14;

  {=====================================================================}

implementation

{$IFDEF DPMI}                                                          {!!.02}
type                                                                   {!!.02}
  DosMemRec = record                                                   {!!.02}
    Sele, Segm : Word;                                                 {!!.02}
  end;                                                                 {!!.02}
{$ENDIF}                                                               {!!.02}
type                                                                   {!!.02}
  OS = record                                                          {!!.02}
    Ofs,                                                               {!!.02}
    Seg : Word;                                                        {!!.02}
  end;                                                                 {!!.02}

const
  {For checking error bits}
  DigiOverrun   = $02;
  DigiParity    = $04;
  DigiFraming   = $08;
  DigiErrorBits = DigiOverrun + DigiParity + DigiFraming;
  DigiBreak     = $10;

var
  Regs     : Registers;            {Required by every routine}

procedure DigiIntr(var Regs : Registers);
{$IFDEF DPMI}
var
  DRegs : DPMIRegisters;
{$ENDIF}
begin
  {$IFDEF DPMI}
  FillChar(DRegs, SizeOf(DRegs), 0);
  with DRegs do begin
    AX := Regs.AX;
    BX := Regs.BX;
    CX := Regs.CX;
    DX := Regs.DX;
    ES := Regs.ES;
    if SimulateRealModeInt($14, DRegs) = 0 then ;

    Regs.Flags := Flags;
    Regs.AX := AX;
    Regs.BX := BX;
    Regs.CX := CX;
    Regs.DX := DX;
    Regs.ES := ES;
  end;
  {$ELSE}
  Intr($14, Regs);
  {$ENDIF}
end;

procedure dUpdateLineAndModemStatus(P : PortRecPtr);
  {-Update LineStatus and ModemStatus fields}
Begin
  with P^, Regs do begin
    AH := $03;
    DX := Ord(PortName);
    DigiIntr(Regs);

    {Refresh status values, set function result}
    ModemStatus := AL;
    LineStatus := AH;
  end;
end;

procedure dInitPortKeep(var P : PortRecPtr; ComName : ComNameType;
                        InSize, OutSize : Word);
  {-Open Digiboard port and set line parameters}
var
  Found : Boolean;
  I : Byte;
  LowCom : Byte;
  HighCom : Byte;
begin
  AsyncStatus := ecOk;

  {See if Digi14 driver is loaded}
  with Regs do begin
    AH := $06;
    AL := $01;
    DX := Ord(ComName);
    CX := $FF;
    DigiIntr(Regs);

    {Check for various error conditions}
    if CX = $FF then
      {If CX still $FF then Digi14 driver probably not loaded or bad port}
      AsyncStatus := ecBadPortNumber
    else if AH = $FF then
      AsyncStatus := ecDigiFailure;
    if AsyncStatus <> ecOk then
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
    Flags := DefPortOptions or DefDigi14Options;
    Buffered := False;
    BreakReceived := False;
    TxReady := True;
    TxInts := False;
    LineStatus := 0;
    DoneProc := dDonePort;
    ErrorProc := NoErrorProc;
    ErrorData := nil;
    UserAbort := NoAbortFunc;
    ProtocolActive := False;
    FaxActive := False;

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

    {Set buffer values we can find out about}
    with Regs do begin
      AH := $1B;
      DX := Ord(PortName);
      AL := $00;
      BX := 0;
      DigiIntr(Regs);
      if DH <> $FF then
        OutBuffLen := BX;

      AH := $1B;
      DX := Ord(PortName);
      AL := $01;
      DigiIntr(Regs);
      if DH <> $FF then
        InBuffLen := BX;
    end;

    {!!.02 new}
    {Use input buffer for GetChar buffering}
    if FlagIsSet(Flags, ptBufferGetChar) then begin
      {$IFDEF DPMI}
      {StatBuff holds selector/segment values}
      LongInt(StatBuff) := GlobalDosAlloc(DigBufferMax);
      if LongInt(StatBuff) = 0 then begin
        dDonePort(P);
        AsyncStatus := ecOutOfMemory;
        Exit;
      end;
      InBuff := Ptr(DosMemRec(StatBuff).Sele, 0);
      {$ELSE}
      if not GetMemCheck(InBuff, DigBufferMax) then begin
        AsyncStatus := ecOutOfMemory;
        dDonePort(P);
        Exit;
      end;
      {$ENDIF}
      InHead := InBuff;
      InTail := InBuff;
      InBuffEnd := InBuff;
      Inc(OS(InBuffEnd).Ofs, DigBufferMax);
    end;
    {!!.02 new end}

    {Set requested wait behavior}
    with Regs do begin
      AH := $20;
      DX := Ord(PortName);
      if Flags and ptReadWriteWait = ptReadWriteWait then
        AL := 1
      else
        AL := 0;
      DigiIntr(Regs);
    end;
  end;
end;

procedure dInitPort(var P : PortRecPtr; ComName : ComNameType;
                    Baud : LongInt;
                    Parity : ParityType; DataBits : DataBitType;
                    StopBits : StopBitType;
                    InSize, OutSize : Word;
                    Options : Word);
  {-Open Digiboard port without changing line parameters}
begin
  AsyncStatus := ecOk;

  {Allocate the port record and do inits}
  dInitPortKeep(P, ComName, InSize, OutSize);
  if AsyncStatus <> ecOk then
    Exit;

  with P^ do begin
    {Set the line parameters}
    dSetLine(P, Baud, Parity, DataBits, StopBits);
    if AsyncStatus <> ecOk then begin
      {Failed, release memory and free slot}
      FreeMemCheck(P, SizeOf(PortRec));
      ActiveComPort[CurrentPort] := nil;
    end;

    {Set requested options, but keep ptBufferGetChar set if necessary} {!!.02}
    if FlagIsSet(Flags, ptBufferGetChar) then                          {!!.02}
      Flags := Options or ptBufferGetChar                              {!!.02}
    else                                                               {!!.02}
      Flags := Options;                                                {!!.02}

    {Clear flow control if asked to do so}
    if Flags and ptClearFlow = ptClearFlow then
      with Regs do begin
        AH := $1E;
        DX := Ord(PortName);
        BX := 0;
        DigiIntr(Regs);
      end;
  end;
end;

procedure dDonePort(var P : PortRecPtr);
  {-Close Digiboard port}
var
  I : Word;
begin
  AsyncStatus := ecOk;

  I := P^.CurrentPort;

  {Release getchar buffer}                                             {!!.02}
  with P^ do                                                           {!!.02}
    if FlagIsSet(Flags, ptBufferGetChar) then                          {!!.02}
      {$IFDEF DPMI}                                                    {!!.02}
      if GlobalDosFree(LongInt(StatBuff)) = 0 then ;                   {!!.02}
      {$ELSE}                                                          {!!.02}
      FreeMemCheck(InBuff, DigBufferMax);                              {!!.02}
      {$ENDIF}

  {Release the heap space}
  FreeMemCheck(P, SizeOf(PortRec));
  P := Nil;

  {Show port slot as now available}
  ActiveComPort[I] := nil;
end;

procedure dSetUart(ComName : ComNameType; NewBase : Word;
                   NewIrq, NewVector : Byte);
  {-Dummy routine}
begin
  {nothing to do}
end;

function BaudMask(Baud : LongInt; var Mask : Byte) : Boolean;
  {-Convert Baud to Mask, return False if invalid Baud}
begin
  BaudMask := True;
  case Baud div 10 of
    11    : Mask := $00;
    15    : Mask := $01;
    30    : Mask := $02;
    60    : Mask := $03;
    120   : Mask := $04;
    240   : Mask := $05;
    480   : Mask := $06;
    960   : Mask := $07;
    1920  : Mask := $08;
    3840  : Mask := $09;
    5760  : Mask := $0A;
    7680  : Mask := $0B;
    11520 : Mask := $0C;
    5     : Mask := $0D;
    7     : Mask := $0E;
    13    : Mask := $0F;
    20    : Mask := $10;
    180   : Mask := $11;
    else begin
      Mask := 0;
      BaudMask := False;
    end;
  end;
end;

procedure dSetLine(P : PortRecPtr; Baud : LongInt; Parity : ParityType;
                  DataBits : DataBitType; StopBits : StopBitType);
  {-Sets the line parameters}
begin
  AsyncStatus := ecOk;

  with Regs do begin
    AH := $04;
    DX := Ord(P^.PortName);

    {No break}
    AL := 0;

    {Set parity}
    case Parity of
      NoParity   : BH := 0;
      OddParity  : BH := 1;
      EvenParity : BH := 2;
      else begin
        GotError(P, epFatal+ecInvalidArgument);
        Exit;
      end;
    end;

    {Set stop bits}
    BL := StopBits - 1;

    {Set data bits}
    CH := DataBits - 5;

    if Baud = 0 then
      {Set mask with known baud}
      if BaudMask(P^.CurBaud, CL) then else
    else
      if not BaudMask(Baud, CL) then begin
        GotError(P, epFatal+ecInvalidBaudRate);
        Exit;
      end;

    {Set parameters}
    DigiIntr(Regs);

    if AH = $FF then
      GotError(P, epNonFatal+ecDigiFailure)
    else with P^ do begin
      {Port open, note statuses}
      LineStatus := AH;
      ModemStatus := AL;

      {Save line option values}
      if Baud <> 0 then
        CurBaud := Baud;
      CurParity := Parity;
      CurDataBits := DataBits;
      CurStopBits := StopBits;
    end;
  end;
end;

procedure dGetLine(P : PortRecPtr; var Baud : LongInt;
                   var Parity : ParityType;
                   var DataBits : DataBitType;
                   var StopBits : StopBitType;
                   FromHardware : Boolean);
  {-Return the line parameters}
begin
  with P^ do begin
    if FromHardware then
      with Regs do begin
        AH := $0C;
        DX := Ord(PortName);
        DigiIntr(Regs);
        if AH = $FF then
          GotError(P, epNonFatal+ecDigiFailure)
        else begin
          case BH of
            0 : CurParity := NoParity;
            1 : CurParity := OddParity;
            2 : CurParity := EvenParity;
          end;
          CurStopBits := BL + 1;
          CurDataBits := CH + 5;
          case CL of
            $02  : CurBaud := 300;
            $03  : CurBaud := 600;
            $04  : CurBaud := 1200;
            $05  : CurBaud := 2400;
            $06  : CurBaud := 4800;
            $07  : CurBaud := 9600;
            $08  : CurBaud := 19200;
            $09  : CurBaud := 38400;
            $0A  : CurBaud := 57600;
            $0C  : CurBaud := 115200;
          end;
        end;
      end;

    {Return current field values}
    Baud := CurBaud;
    Parity := CurParity;
    DataBits := CurDataBits;
    StopBits := CurStopBits;
  end;
end;

procedure dSetModem(P : PortRecPtr; DTR, RTS : Boolean);
  {-Sets both modem control values}
begin
  with P^, Regs do begin
    AH := $05;
    DX := Ord(PortName);
    AL := $01;
    BL := 0;
    ModemControl := 0;
    if DTR then begin
      BL := $01;
      ModemControl := DTRMask;
    end;
    if RTS then begin
      BL := BL or $02;
      ModemControl := ModemControl or RTSMask;
    end;
    DigiIntr(Regs);
    LineStatus := AH;
    ModemStatus := AL;
  end;
end;

procedure dGetModem(P : PortRecPtr; var DTR, RTS : Boolean);
  {-Return modem control values}
begin
  with P^, Regs do begin
    AH := $05;
    DX := Ord(PortName);
    AL := $00;

    DigiIntr(Regs);
    LineStatus := AH;
    ModemStatus := AL;
    DTR := BL and $01 = $01;
    RTS := BL and $02 = $02;
  end;
end;

{!!.02 new}
function dCharReadyPhys(P : PortRecPtr) : Boolean;
  {-Returns True if DigiBoard has data available}
begin
  with P^, Regs do begin
    AH := $03;
    DX := Ord(PortName);
    DigiIntr(Regs);

    {Refresh status values, set function result}
    ModemStatus := AL;
    LineStatus := AH;
    dCharReadyPhys := Odd(AH);
  end;
end;

{!!.02 new}
procedure dFillBuffer(P : PortRecPtr);
  {-Try to refill buffer}
var
  Count : Integer;

  procedure ReadData;
  begin
    with P^, Regs do begin
      AH := $0F;
      {$IFDEF DPMI}
      ES := DosMemRec(StatBuff).Segm;
      {$ELSE}
      ES := OS(InHead).Seg;
      {$ENDIF}
      BX := OS(InHead).Ofs;
      CX := Count;
      DX := Ord(PortName);
      DigiIntr(Regs);
      Inc(InBuffCount, AX);
      Inc(OS(InHead).Ofs, AX);
      if InHead = InBuffEnd then
        InHead := InBuff;
    end;
  end;

begin
  with P^ do begin
    if OS(InHead).Ofs >= OS(InTail).Ofs then begin
      {Normal buffer, fill free space at end...}
      if dCharReadyPhys(P) then begin
        Count := (OS(InBuffEnd).Ofs - OS(InHead).Ofs);
        ReadData;
      end;

      {...and at beginning of buffer}
      if dCharReadyPhys(P) and (InBuff = InHead) then begin
        Count := OS(InTail).Ofs - OS(InBuff).Ofs;
        if Count > 0 then
          ReadData;
      end;
    end else begin
      {Wrapped buffer, fill free space in the middle}
      if dCharReadyPhys(P) then begin
        Count := OS(InTail).Ofs - OS(InHead).Ofs;
        if Count > 0 then
          ReadData;
      end;
    end;
  end;
end;

{!!.02 new}
procedure dGetCharPhys(P : PortRecPtr; var C : Char);
  {-Get the next received character}
begin
  with P^, Regs do begin
    AH := $02;
    DX := Ord(PortName);
    DigiIntr(Regs);

    if (AH and $80) = $80 then begin
      {Timeout waiting for char, report error}
      C := #$FF;
      GotError(P, epNonFatal+ecBufferIsEmpty);
    end else begin
      {Get char and error bits}
      C := Char(AL);
      LineStatus := AH;

      if LineStatus and DigiErrorBits <> 0 then begin
        {Got some type of line error}
        if LineStatus and DigiOverrun = DigiOverrun then
          AsyncStatus := ecOverrunError
        else if LineStatus and DigiParity = DigiParity then
          AsyncStatus := ecParityError
        else if LineStatus and DigiFraming = DigiFraming then
          AsyncStatus := ecFramingError;

        GotError(P, epNonFatal+AsyncStatus);
      end;

      if LineStatus and DigiBreak = DigiBreak then
        BreakReceived := True;

      {$IFDEF Tracing}
      if TracingOn then
        AddTraceEntry('R', C);
      {$ENDIF}
    end;
  end;
end;

{!!.02 new}
procedure dGetCharBuf(P : PortRecPtr; var C : Char);
  {-Get character from buffer}
var
  I : Word;
begin
  with P^ do begin
    if (InHead = InTail) and dCharReadyPhys(P) then
      {Buffer empty but data is ready, refill the buffer}
      dFillBuffer(P);

    if InBuffCount > 0 then begin
      {Get next char from buffer}
      C := Char(InTail^);
      Inc(OS(InTail).Ofs);
      if InTail = InBuffEnd then
        InTail := InBuff;
      Dec(InBuffCount);

      {$IFDEF Tracing}
      if TracingOn then
        AddTraceEntry('R', C);
      {$ENDIF}
    end else
      GotError(P, epNonFatal+ecBufferIsEmpty);
  end;
end;

{!!.02 rewritten}
procedure dGetChar(P : PortRecPtr; var C : Char);
  {-Calls DigiBoard driver to check for and return C}
begin
  AsyncStatus := ecOk;
  if FlagIsSet(P^.Flags, ptBufferGetChar) then
    dGetCharBuf(P, C)
  else
    dGetCharPhys(P, C);
end;

procedure dPeekCharPhys(P : PortRecPtr; var C : Char; PeekAhead : Word);
  {-Can only peek ahead one character}
begin
  if PeekAhead <> 1 then
    GotError(P, epNonFatal+ecInvalidArgument)
  else with P^, Regs do begin
    AH := $08;
    DX := Ord(PortName);
    DigiIntr(Regs);
    if AH = $FF then
      GotError(P, ecBufferIsEmpty)
    else begin
      AsyncStatus := ecOk;
      LineStatus := AH;
      C := Char(AL);
    end;
  end;
end;

{!!.02 new}
procedure dPeekCharBuf(P : PortRecPtr; var C : Char; PeekAhead : Word);
var
  TrcP : BPtr;
  Count : Word;
begin
  AsyncStatus := ecOk;
  with P^ do begin
    if PeekAhead > InBuffCount then begin
      {Peeking too far, try to refill buffer}
      dFillBuffer(P);
      if PeekAhead > InBuffCount then begin
        {Still too far, give up}
        C := #$FF;
        GotError(P, epNonFatal+ecBufferIsEmpty);                      {!!.03}
        Exit;
      end;
    end;

    {!!.03 rewritten}
    {Return the requested char}
    Count := (OS(InTail).Ofs + PeekAhead)-1;
    if Count >= OS(InBuffEnd).Ofs then
      Dec(Count, DigBufferMax);
    TrcP := InBuff;
    Inc(OS(TrcP).Ofs, Count);
    C := Char(TrcP^);
  end;
end;

{!!.02 rewritten}
procedure dPeekChar(P : PortRecPtr; var C : Char; PeekAhead : Word);
begin
  AsyncStatus := ecOk;
  if FlagIsSet(P^.Flags, ptBufferGetChar) then
    dPeekCharBuf(P, C, PeekAhead)
  else
    dPeekCharPhys(P, C, PeekAhead);
end;

procedure dPutChar(P : PortRecPtr; C : Char);
  {-Transmit C}
begin
  AsyncStatus := ecOk;

  with P^, Regs do begin
    AH := $01;
    DX := Ord(PortName);
    AL := Byte(C);
    DigiIntr(Regs);

    if AH = $FF then
      {Digiboard failure}
      GotError(P, epNonFatal+ecDigiFailure)
    else if (AH and $80) = $80 then
      {No room in buffer}
      GotError(P, epNonFatal+ecBufferIsFull)
    else begin
      {Transmit OK, note latest status values}
      LineStatus := AH;
      ModemStatus := AL;

      {$IFDEF Tracing}
      if TracingOn then
        AddTraceEntry('T', C);
      {$ENDIF}
    end;
  end;
end;

procedure dStartTransmitter(P : PortRecPtr);
  {-Does nothing}
begin
end;

function dCharReady(P : PortRecPtr) : Boolean;
  {-Returns True if data is available}
begin
  with P^ do begin
    {Always call CharReadyPhys to get updated line/modem values}
    dCharReady := dCharReadyPhys(P);

    {Force a true return if we've data buffered}
    if FlagIsSet(Flags, ptBufferGetChar) then
      if InHead <> InTail then
        dCharReady := True;
  end;
end;

function dTransReady(P : PortRecPtr) : Boolean;
  {-Returns True if Digiboard can accept another character to transmit}
begin
  with P^, Regs do begin
    AH := $03;
    DX := Ord(PortName);
    DigiIntr(Regs);

    {Refresh status values, set function result}
    ModemStatus := AL;
    LineStatus := AH;
    dTransReady := (AH and $40) = $40
  end;
end;

procedure dSendBreak(P : PortRecPtr);
  {-Send a break}
begin
  with P^, Regs do begin
    AH := $07;
    DX := Ord(PortName);
    AL := $00;
    DigiIntr(Regs);
  end;
end;

procedure dActivatePort(P : PortRecPtr; Restore : Boolean);
  {-Does nothing}
begin
end;

procedure dDeactivatePort(P : PortRecPtr; Restore : Boolean);
  {-Does nothing}
begin
end;

procedure dSavePort(P : PortRecPtr; var PSR);
  {-Does nothing}
begin
end;

procedure dRestorePort(P : PortRecPtr; var PSR);
  {-Does nothing}
begin
end;

function dUpdateLineStatus(P : PortRecPtr) : Byte;
  {-Returns line status register value}
begin
  dUpdateLineAndModemStatus(P);
  dUpdateLineStatus := P^.LineStatus;
end;

function dUpdateModemStatus(P : PortRecPtr) : Byte;
  {-Returns modem status register value}
begin
  dUpdateLineAndModemStatus(P);
  dUpdateModemStatus := P^.ModemStatus;
end;

{$IFDEF UseHWFlow}
procedure dHWFlowSet(P : PortRecPtr; Enable : Boolean;
                     BufferFull, BufferResume : Word;
                     Options : Word);
  {-Enables/disables hardware flow control}
const
  InvertedBits = hfDTRActiveLow +
                 hfRTSActiveLow +
                 hfDSRActiveLow +
                 hfCTSActiveLow;
var
  Bits : Byte;
begin
  AsyncStatus := ecOk;

  with P^, Regs do begin
    {Turn it on...}
    if Enable then begin
      {Don't allow inverted signals}
      if Options and InvertedBits <> 0 then begin
        GotError(P, epNonFatal+ecInvalidArgument);
        Exit;
      end;

      {Check buffer sizes}
      if (BufferResume >= BufferFull) or
         (BufferFull >= InBuffLen) then begin                          {!!.01}
         {(BufferFull >= OutBuffLen) then begin}                       {!!.01}
        GotError(P, epNonFatal+ecInvalidArgument);
        Exit;
      end;

      {Set buffer full point}
      AH := $1C;
      DX := Ord(PortName);
      AL := 2;
      BX := BufferFull;
      DigiIntr(Regs);

      {Set buffer resume point}
      AH := $1C;
      DX := Ord(PortName);
      AL := 1;
      BX := BufferResume;
      DigiIntr(Regs);

      {Turn on requested options}
      Bits := 0;
      if Options and hfUseDTR = hfUseDTR then
        Bits := Bits or $01;
      if Options and hfUseRTS = hfUseRTS then
        Bits := Bits or $02;
      if Options and hfRequireDSR = hfRequireDSR then
        Bits := Bits or $20;
      if Options and hfRequireCTS = hfRequireCTS then
        Bits := Bits or $10;

      AH := $1E;
      DX := Ord(PortName);
      BH := 0;
      BL := Bits;
      DigiIntr(Regs);

      if AH = $FF then begin
        {Report error}
        GotError(P, epNonFatal+ecDigiFailure);
        HWFRecHonor := 0;
      end else begin
        {Say it's on}
        HWFRecHonor := 1;

        {And software flow was forced off}
        SWFState := 0;
      end;
    end else begin
      {Turn if off}
      AH := $1E;
      DX := Ord(PortName);
      BH := 0;
      BL := 0;
      DigiIntr(Regs);

      if AH = $FF then
        {Report error}
        GotError(P, epNonFatal+ecDigiFailure);

      HWFRecHonor := 0;
    end;
  end;
end;

function dHWFlowGet(P : PortRecPtr) : FlowState;
  {-Returns hardware flow control state, on or off only}
begin
  with P^ do begin
    if HWFRecHonor = 1 then
      dHWFlowGet := fsClear
    else
      dHWFlowGet := fsOff;
  end;
end;
{$ENDIF}

{$IFDEF UseSWFlow}
procedure dSWFlowSet(P : PortRecPtr; Enable : Boolean;
                     BufferFull, BufferResume : Word;
                     Options : Word);
  {-Enables/disables software flow control}
begin
  AsyncStatus := ecOk;

  with P^, Regs do begin

    if Enable then begin
      {Check buffer sizes}
      if FlagIsSet(Options, sfReceiveFlow) then begin
        if (BufferResume >= BufferFull) or
           (BufferFull >= InBuffLen) or
           (BufferFull >= OutBuffLen) then begin
          GotError(P, epNonFatal+ecInvalidArgument);
          Exit;
        end;

        {Set buffer full point}
        AH := $1C;
        DX := Ord(PortName);
        AL := 2;
        BX := BufferFull;
        DigiIntr(Regs);

        {Set buffer resume point}
        AH := $1C;
        DX := Ord(PortName);
        AL := 1;
        BX := BufferResume;
        DigiIntr(Regs);
      end;

      {Turn on flow control}
      if FlagIsSet(Options, sfTransmitFlow) then
        BH := $01
      else
        BH := $00;
      if FlagIsSet(Options, sfReceiveFlow) then
        BH := BH or $02;
      AH := $1E;
      DX := Ord(PortName);
      BL := 0;
      DigiIntr(Regs);
      if AH = $FF then begin
        GotError(P, epNonFatal+ecDigiFailure);
        SWFState := 0;
      end else begin
        SWFState := Options;
        HWFRecHonor := 0;
      end;
    end else begin
      {Turn it off}
      AH := $1E;
      DX := Ord(PortName);
      BX := $00;
      DigiIntr(Regs);

      {Say it's off}
      SWFState := 0;
    end;
  end;
end;

function dSWFlowGet(P : PortRecPtr) : FlowState;
  {-Returns software flow control state}
begin
  with P^ do begin
    if SWFState <> 0 then
      dSWFlowGet := fsClear
    else
      dSWFlowGet := fsOff;
  end;
end;

procedure dSWFlowCtl(P : PortRecPtr; OnChar, OffChar : Char;
                     Resume : Boolean);
  {-Sets software flow control characters and/or resumes transmits}
begin
  AsyncStatus := ecOk;

  with P^, Regs do begin
    if Resume then begin
      AH := $17;
      DX := Ord(PortName);
      AL := 1;
      DigiIntr(Regs);
    end;

    if (OnChar <> OffChar) then begin
      AH := $1E;
      DX := Ord(PortName);
      BH := $02;
      CL := Byte(OnChar);
      CH := Byte(OffChar);
      DigiIntr(Regs);
      if AH = $FF then
        GotError(P, epNonFatal+ecDigiFailure);
    end;
  end;
end;
{$ENDIF}

procedure dBufferStatus(P : PortRecPtr;
                        var InFree, OutFree, InUsed, OutUsed : Word);
  {-Returns various buffer values}
begin
  with P^, Regs do begin

    {Get in buffer sizes}
    AH := $0A;
    DX := Ord(PortName);
    DigiIntr(Regs);
    if DH = $FF then begin
      GotError(P, epNonFatal+ecDigiFailure);
      InUsed := 0;
      InFree := 0;
    end else begin
      InUsed := AX;
      InFree := P^.InBuffLen - InUsed;
    end;

    {Get out buffer sizes}
    AH := $12;
    DX := Ord(P^.PortName);
    DigiIntr(Regs);
    if DH = $FF then begin
      GotError(P, epNonFatal+ecDigiFailure);
      OutUsed := 0;
      OutFree := 0;
    end else begin
      OutFree := AX;
      OutUsed := P^.OutBuffLen - OutFree;
    end;
  end;
end;

procedure dBufferFlush(P : PortRecPtr; FlushIn, FlushOut: Boolean);
  {-Flushes input/output buffers}
begin
  with P^, Regs do begin

    if FlushIn then begin
      {Flush the input buffer}
      AH := $10;
      DX := Ord(P^.PortName);
      DigiIntr(Regs);
      if AH = $FF then
        GotError(P, epNonFatal+ecDigiFailure);

      {If buffering input, get rid of any buffered data as well}       {!!.03}
      if FlagIsSet(P^.Flags, ptBufferGetChar) then begin               {!!.03}
        InHead := InBuff;                                              {!!.03}
        InTail := InBuff;                                              {!!.03}
      end;                                                             {!!.03}
    end;

    if FlushOut then begin
      {Flush the output buffer}
      AH := $11;
      DX := Ord(P^.PortName);
      DigiIntr(Regs);
      if AH = $FF then
        GotError(P, epNonFatal+ecDigiFailure);
    end;
  end;
end;

Function dPutBlock (P: PortRecPtr; Var Block; Size: Word): Word;
Var
  i : Word;

Begin
  i := 0;

  While i < Size Do
  Begin
    dPutChar (P, CharArray (Block) [i]);
    If AsyncStatus <> ecOk Then
      Break;

    Inc (i);
  End;

  If i = Size Then AsyncStatus := ecOk
              Else GotError (P, epNonFatal + ecBufferIsFull);
  dPutBlock := i;
End;

procedure ActivateApDigi14;
  {-Registers this unit as the active "device layer"}
begin
  {$IFNDEF UseOOP}
  InitPort := dInitPort;
  InitPortKeep := dInitPortKeep;
  DonePort := dDonePort;
  SetLine := dSetLine;
  GetLine := dGetLine;
  SetModem := dSetModem;
  GetModem := dGetModem;
  GetChar := dGetChar;
  PeekChar := dPeekChar;                                               {!!.03}
  PutChar := dPutChar;
  PutBlock := dPutBlock;
  CharReady := dCharReady;
  TransReady := dTransReady;
  SendBreak := dSendBreak;
  ActivatePort := dActivatePort;
  DeactivatePort := dDeactivatePort;
  SavePort := dSavePort;
  RestorePort := dRestorePort;

  UpdateLineStatus := dUpdateLineStatus;
  UpdateModemStatus := dUpdateModemStatus;
  {$IFDEF UseHWFlow}
  HWFlowSet := dHWFlowSet;
  HWFlowGet := dHWFlowGet;
  {$ENDIF}
  {$IFDEF UseSWFlow}
  SWFlowSet := dSWFlowSet;
  SWFlowGet := dSWFlowGet;
  SWFlowCtl := dSWFlowCtl;
  {$ENDIF}
  BufferStatus := dBufferStatus;
  BufferFlush := dBufferFlush;

  {$ENDIF}
  SetUart := dSetUart;
end;

end.
