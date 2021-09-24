{$S-,R-,V-,I-,B-,F-,O-,A+}

{Conditional defines that may affect this unit}
{$I APDEFINE.INC}

{*********************************************************}
{*                    APUART.PAS 2.00                    *}
{*     Copyright (c) TurboPower Software 1991.           *}
{* Portions copyright (c) Information Technology 1989,   *}
{*    and used under license to TurboPower Software      *}
{*                 All rights reserved.                  *}
{*********************************************************}

unit ApUart;
  {-Provides buffered, interrupt-driven serial I/O at the UART level}

interface

uses
  Dpmi,
{$IFDEF DPMI}
  WinAPI,
{$ENDIF}
  DOS,
  ApMisc,
  ApSame,
  OpInline,
  ApPort;

Type
  {Supported uart types}
  UartType = (NoUart, U8250B, U8250A, U16550, U16550A);

Const
  {For displaying uart types}
  UartTypeString : Array [UartType] Of String [11] =
  ('No UART', '8250B', '8250A/16450', '16550', '16550A');

  {Standard BaseAddr values}
  DefBaseAddr : Array [Com1..Com8] Of Word = (
  $03F8, $02F8, $03E8, $02E8, $4220, $4228, $5220, $5228);

  {Standard irq numbers}
  DefIrqNumber : Array [Com1..Com8] Of Byte = (
  4, 3, 4, 3, 3, 3, 3, 3);

  {Standard vectors}
  DefComVector : Array [Com1..Com8] Of Byte = (
  $0C, $0B, $0C, $0B, $0B, $0B, $0B, $0B);

  {Com3/Com4 specifics for PS2s}
  DefPS2Addr : Array [Com3..Com4] Of Word = ($3220, $3228);
  DefPS2Irq : Array [Com3..Com4] Of Byte = (3, 3);
  DefPS2Vector : Array [Com3..Com4] Of Byte = ($0B, $0B);

  {Com3/Com4 specifics for defacto standard}
  DefStdAddr : Array [Com3..Com4] Of Word = ($03E8, $02E8);
  DefStdIrq : Array [Com3..Com4] Of Byte = (4, 3);
  DefStdVector : Array [Com3..Com4] Of Byte = ($0C, $0B);

Procedure uInitPort (Var P : PortRecPtr; ComName : ComNameType; Baud : LongInt;
                    Parity : ParityType; DataBits : DataBitType;
                    StopBits : StopBitType; InSize, OutSize : Word;
                    Options : Word);
Procedure uInitPortKeep (Var P : PortRecPtr; ComName : ComNameType;
                        InSize, OutSize : Word);
Procedure uDonePort (Var P : PortRecPtr);
Procedure uSetUart (ComName : ComNameType; NewBase: Word;
                   NewIrq, NewVector : Byte);
Procedure uSetLine (P : PortRecPtr; Baud : LongInt;
                   Parity : ParityType; DataBits : DataBitType;
                   StopBits : StopBitType);
Procedure uGetLine (P : PortRecPtr; Var Baud : LongInt;
                   Var Parity : ParityType;
                   Var DataBits : DataBitType;
                   Var StopBits : StopBitType;
                   FromHardware : Boolean);
Procedure uSetModem (P : PortRecPtr; DTR, RTS : Boolean);
Procedure uGetModem (P : PortRecPtr; Var DTR, RTS : Boolean);
Procedure uGetChar (P : PortRecPtr; Var C : Char);
Procedure uPeekChar (P : PortRecPtr; Var C : Char; PeekAhead : Word);
Procedure uPutChar (P : PortRecPtr; C : Char);
Function uPutBlock (P: PortRecPtr; Var Block; Size: Word): Word;
Procedure uStartTransmitter (P : PortRecPtr);
Function uCharReady (P : PortRecPtr) : Boolean;
Function uTransReady (P : PortRecPtr) : Boolean;
Procedure uSendBreak (P : PortRecPtr);
Procedure uActivatePort (P : PortRecPtr; Restore : Boolean);
Procedure uDeactivatePort (P : PortRecPtr; Restore : Boolean);
Procedure uSavePort (P : PortRecPtr; Var PSR);
Procedure uRestorePort (P : PortRecPtr; Var PSR);
Function uUpdateLineStatus (P : PortRecPtr) : Byte;
Function uUpdateModemStatus (P : PortRecPtr) : Byte;
{$IFDEF UseHWFlow}
Procedure uHWFlowSet (P : PortRecPtr; Enable : Boolean;
                     BufferFull, BufferResume : Word;
                     Options : Word);
Function uHWFlowGet (P : PortRecPtr) : FlowState;
{$ENDIF}
{$IFDEF UseSWFlow}
Procedure uSWFlowSet (P : PortRecPtr; Enable : Boolean;
                     BufferFull, BufferResume : Word;
                     Options : Word);
Function uSWFlowGet (P : PortRecPtr) : FlowState;
Procedure uSWFlowCtl (P : PortRecPtr; OnChar, OffChar : Char;
                     Resume : Boolean);
{$ENDIF}
Procedure uBufferStatus (P : PortRecPtr;
                        Var InFree, OutFree, InUsed, OutUsed : Word);
Procedure uBufferFlush (P : PortRecPtr; FlushIn, FlushOut: Boolean);
Procedure ActivateApUart;
Function UartTest1 (BaseAddr : Word) : Boolean;
Function UartTest2 (BaseAddr : Word) : Boolean;
Function UartTest3 (BaseAddr : Word) : Boolean;
Function ClassifyUart (BaseAddr : Word; CheckUart : Boolean) : UartType;
Procedure SetFifoBuffering (BaseAddr : Word; Enable : Boolean; Level : Byte);
Function FifoStatus (BaseAddr : Word) : Boolean;
Procedure SaveUartState (BaseAddr : Word; Var PSR : PortSaveRec);
Procedure RestoreUartState (BaseAddr : Word; PSR : PortSaveRec);
{#F+}
Function GetLineStatusDirect (BaseAddr : Word) : Byte;
Function CheckTEDirect (BaseAddr : Word) : Boolean;
Function GetModemStatusDirect (BaseAddr : Word) : Byte;
{#F-}
Procedure SendLongBreak (BaseAddr : Word; Count : Byte);
{$IFDEF EventLogging}
Procedure InitEventLogging (Events : Word);
Procedure DumpEvents (FName : PathStr);
{$ENDIF}

  {======================================================================}

implementation

const
  {Max number of ISR entry points provided by APUART.ASM}
  MaxApuartPort = 4;

  {Parity masks}
  ParityMask : array[ParityType] of Byte = (
    $00,     {No parity}
    $08,     {Odd parity}
    $18,     {Even parity}
    $28,     {Mark parity}
    $38);    {Space parity}

  {DataBits masks}
  DataBitMask : array[DataBitType] of Byte = (
    $00,     {5 data bits}
    $01,     {6 data bits}
    $02,     {7 data bits}
    $03);    {8 data bits}

  {StopBits masks}
  StopBitMask : array[StopBitType] of Byte = (
    $00,     {1 stop bit}
    $04);    {2 stop bit}

  {Uart testing masks}
  FifoWriteMask : Byte = $01;    {Turns on FIFO register writing}
  FifoOnMask    : Byte = $C1;    {Turns FIFO buffering on (depth 14)}
  FifoOffMask   : Byte = $00;    {Turns FIFO buffering off}

  {8259 OCW1 addresses}
  Master8259 = $21;
  Slave8259  = $A1;

type
  {For buffer address calculations}
  OS = record
      Ofs,
      Seg : Word;
    end;

var
  {For restoring ExitProc after APUART's exit procedure}
  UartExitSave : Pointer;

  {Holds addresses of APUART ISRs}
  CommIntAddr : array[1..MaxApuartPort] of Pointer;

  {Used ISRs entry points in APUART.ASM}
  ISRInUse : array[1..MaxApuartPort] of Boolean;

{$IFDEF EventLogging}
type
  EventRec = record
    case Byte of
      {Timer record}
      0 : (IsrNumber : ShortInt; Signature : Byte; CurTime : LongInt);
      {Data record}
      1 : (EventType : Byte; Junk : Byte; DataWord : Word; Junk1 : Word);
    end;
const
  HighestEvent = (65520 div SizeOf(EventRec)) - 1;
  TimerResolution = 1193181.667;
  StartSignature = $A5;
  EventLogging : Boolean = False;
type
  EventQueueArray = array[0..HighestEvent] of EventRec;
var
  EventQueue : ^EventQueueArray;
  Delta : LongInt;
  EventIndex : Word;
  EventCount : Word;
  MaxEvent : Word;
  MaxEventIndex : Word;
  EventsWrapped : Boolean;
  EventStart : LongInt;
{$ENDIF}

{$IFDEF Standard}        const TooManyUARTConfigsSpecified = 1; {$ENDIF}
{$IFDEF StandardLogging} const TooManyUARTConfigsSpecified = 1; {$ENDIF}
{$IFDEF Status}          const TooManyUARTConfigsSpecified = 1; {$ENDIF}
{$IFDEF StatusLogging}   const TooManyUARTConfigsSpecified = 1; {$ENDIF}
{$IFDEF HWOnly}          const TooManyUARTConfigsSpecified = 1; {$ENDIF}
{$IFDEF SWOnly}          const TooManyUARTConfigsSpecified = 1; {$ENDIF}
{$IFDEF Basic}           const TooManyUARTConfigsSpecified = 1; {$ENDIF}
{$IFDEF UserDefined}     const TooManyUARTConfigsSpecified = 1; {$ENDIF}

{!!!!!!!!!!!! Note !!!!!!!!!!!!!!!!!}
{If you get an "Error 4: Duplicate Identifier" above then
 you have more than one UART configuration specified in
 APDEFINE.INC. Go back to  APDEFINE.INC and choose one
 and only one UART configuration.}

{$L APUART1.OBJ}

procedure CommInt1; external;
procedure CommInt2; external;
procedure CommInt3; external;
procedure CommInt4; external;

procedure CommTx(P : PortRecPtr); external;
procedure SendByte(P : PortRecPtr; C : Char); external;
{$IFDEF EventLogging}
{$F+}
function ReadTimer : LongInt; external;
{$F-}
{$ENDIF}

{$IFDEF DPMI}
procedure IntOff; inline($9C/$FA);      {PUSHF/CLI}
procedure IntOn; inline($58/            {POP  AX}
                        $A9/$00/$02/    {TEST AX,0200}
                        $74/$01/        {JE   @DONE}
                        $FB);           {STI}
{$ELSE}
procedure IntOff; inline($9C/$FA);      {PUSHF/CLI}
procedure IntOn; inline($9D);           {POPF}
{$ENDIF}

procedure uInitPortKeep(var P : PortRecPtr; ComName : ComNameType;
                        InSize, OutSize : Word);
  {-UART open port procedure (doesn't change line settings or DTR/RTS)}
var
  Found1 : Boolean;
  Found2 : Boolean;
  I : Byte;
  J : Byte;
  DTR, RTS : Boolean;
label
  ErrorExit;
begin
  AsyncStatus := ecOk;
  P := nil;

  {Validate the buffer sizes}
  if not CheckRange(InSize, MinInBuff, 65521) or
     not CheckRange(OutSize, MinOutBuff, 65521) then begin
    AsyncStatus := ecOutOfRange;
    Exit;
  end;

  {Allocate Port record}
  {$IFDEF DPMI}
  if not GlobalAllocCheck(P, gmem_Fixed, SizeOf(PortRec)) then begin
    AsyncStatus := ecOutOfMemory;
    Exit;
  end;
  {$ELSE}
  if not GetMemCheck(P, SizeOf(PortRec)) then begin
    AsyncStatus := ecOutOfMemory;
    Exit;
  end;
  {$ENDIF}

  with P^ do begin
    {Allocate I/O buffers}
    OutBuff := nil;
    {$IFDEF DPMI}
    if not GlobalAllocCheck(InBuff, gmem_Fixed, InSize) or
       not GlobalAllocCheck(OutBuff, gmem_Fixed, OutSize) then begin
      AsyncStatus := ecOutOfMemory;
      goto ErrorExit;
    end;
    {$ELSE}
    if not GetMemCheck(InBuff, InSize) or
       not GetMemCheck(OutBuff, OutSize) then begin
      AsyncStatus := ecOutOfMemory;
      goto ErrorExit;
    end;
    {$ENDIF}

    {$IFDEF LargeComNameSet}
    {Range check ComName}
    if ComName > Com8 then begin
      AsyncStatus := ecOutOfRange;
      goto ErrorExit;
    end;
    {$ENDIF}

    {For Com3/Com4, pick between PS2 defaults and defacto defaults}
    if (ComName = Com3) or (ComName = Com4) then
    begin
      DefBaseAddr[ComName] := DefStdAddr[ComName];
      DefIrqNumber[ComName] := DefStdIrq[ComName];
      DefComVector[ComName] := DefStdVector[ComName];
    end;

    {Setup UART hardware addressing}
    BaseAddr := DefBaseAddr[ComName];
    Vector := DefComVector[ComName];
    PortName := ComName;

    {Make sure there is a UART at this address}
    if not UartTest1(BaseAddr) or not UartTest2(BaseAddr) then begin
      AsyncStatus := ecNotaUart;
      goto ErrorExit;
    end;

    {Save original state of UART}
    SaveUartState(BaseAddr, OrigPortState);

    {Check for an available port slot}
    Found1 := False;
    I := 1;
    while not Found1 and (I <= MaxActivePort) do
      if ActiveComPort[I] = nil then begin
        {Found open ComPort slot, now look for open ISR entry point}
        Found2 := False;
        J := 1;
        while not Found2 and (J <= MaxApuartPort) do
          if not ISRInUse[J] then begin
            {Found open ISR entry point}
            Found2 := True;
            ISRInUse[J] := True;
            ISREntryPoint := J;
          end else
            Inc(J);
        if not Found2 then begin
          AsyncStatus := ecNoMorePorts;
          goto ErrorExit;
        end;
        {SetIntVec(DefComVector[ComName], CommIntAddr[J]);}
        CurrentPort := I;
        ActiveComPort[I] := P;
        Found1 := True;
      end else
        Inc(I);

    {Can't open port if no slots available}
    if not Found1 then begin
      AsyncStatus := ecNoMorePorts;
      goto ErrorExit;
    end;

    {Set initial software flow control conditions}
    SWFState := 0;
    SWFGotXoff := False;
    SWFSentXoff := False;
    SWFOnChar := DefaultXonChar;
    SWFOffChar := DefaultXoffChar;

    {Set initial hardware flow control conditions}
    HWFRecHonor := 0;
    HWFTransHonor := 0;
    HWFRemoteOff := False;
    LastXmitError := 0;

    {Buffer initialization}
    Buffered := True;
    InBuffLen := InSize;
    OutBuffLen := OutSize;
    InBuffCount := 0;
    InHead := InBuff;
    InTail := InBuff;
    InBuffEnd := InBuff;
    Inc(OS(InBuffEnd).Ofs, InBuffLen);
    OutBuffCount := 0;
    OutHead := OutBuff;
    OutTail := OutBuff;
    OutBuffEnd := OutBuff;
    Inc(OS(OutBuffEnd).Ofs, OutBuffLen);

    {Always init status buffer off}
    UseStatusBuffer := False;
    StatBuff := nil;
    StatHead := nil;
    StatTail := nil;

    {Misc other inits}
    Flags := DefPortOptions;
    BreakReceived := False;
    TxReady := True;
    TxInts := True;
    TxIntsActive := False;
    LostCharCount := 0;
    DoneProc := uDonePort;
    ErrorProc := NoErrorProc;
    ErrorData := nil;
    UserAbort := NoAbortFunc;
    ProtocolActive := False;
    FaxActive := False;
    ISRActive := False;

    {Set flag for old UARTs}
    if ClassifyUart(BaseAddr, False) = U8250B then
      OldUart := True
    else
      OldUart := False;

    {Init current line vars to original line values}
    uGetLine(P, CurBaud, CurParity, CurDataBits, CurStopBits, True);
    LineControl := 0;

    {Init ModemControl to current values}
    uGetModem(P, DTR, RTS);

    {Turn UART interrupts off at the UART and the 8259}
    IrqNumber := DefIrqNumber[PortName];
    uDeactivatePort(P, False);

    {Turn UART interrupts on at the UART and the 8259}
    uActivatePort(P, True);

    {Finished}
    Exit;

ErrorExit:
    {$IFDEF DPMI}
    GlobalFreeCheck(InBuff);
    GlobalFreeCheck(OutBuff);
    GlobalFreeCheck(P);
    {$ELSE}
    FreeMemCheck(InBuff, InSize);
    FreeMemCheck(OutBuff, OutSize);
    FreeMemCheck(P, SizeOf(PortRec));
    {$ENDIF}
  end;
end;

procedure uInitPort(var P : PortRecPtr; ComName : ComNameType;
                    Baud : LongInt;
                    Parity : ParityType; DataBits : DataBitType;
                    StopBits : StopBitType; InSize, OutSize : Word;
                    Options : Word);
  {-UART open port procedure}
var
  B : Boolean;
begin
  {Allocate record and buffers; init fields}
  uInitPortKeep(P, ComName, InSize, OutSize);
  if AsyncStatus <> ecOk then
    Exit;

  {Set the line parameters}
  with P^ do begin
    uSetLine(P, Baud, Parity, DataBits, StopBits);
    if AsyncStatus <> ecOk then begin
      {Port open failed, undo results of InitPortKeep}
      {SetIntVec(DefComVector[ComName], OrigPortState.Vector);}
      uDeactivatePort(P, True);
      ActiveComPort[CurrentPort] := nil;
      ISRInUse[ISREntryPoint] := False;
      {$IFDEF DPMI}
      GlobalFreeCheck(P);
      GlobalFreeCheck(InBuff);
      GlobalFreeCheck(OutBuff);
      {$ELSE}
      FreeMemCheck(P, SizeOf(PortRec));
      FreeMemCheck(InBuff, InSize);
      FreeMemCheck(OutBuff, OutSize);
      {$ENDIF}
      Exit;
    end;

    {Save the desired options}
    B := FlagIsSet(Flags, ptHiIrq);
    if B then
      Flags := Options or ptHiIrq
    else
      Flags := Options;

    {Set modem control state}
    B := FlagIsSet(Flags, ptRaiseModemOnOpen);
    if B then
      ModemControl := ModemControl or (DTRMask or RTSMask);
  end;

  {Set the modem parameters (no errors possible)}
  uSetModem(P, B, B);
end;

procedure uDonePort(var P : PortRecPtr);
  {-Closes a communications port}
var
  B : Byte;
  I : Word;
begin
  AsyncStatus := ecOk;

  {Exit immediately if port already closed}
  if P = nil then
    Exit;

  I := P^.CurrentPort;

  with P^ do begin
    {Turn off this interrupt at the 8259}
    uDeactivatePort(P, False);

    if FlagIsSet(Flags, ptRestoreOnClose) then
      {Restore the original UART configuration}
      RestoreUartState(BaseAddr, OrigPortState)
    else begin
      {Restore the old vector}
      SetIntVec(Vector, OrigPortState.Vector);

      {Drop Out2 only}
      ModemControl := ModemControl and not Out2Mask;
      Port[BaseAddr+MCreg] := ModemControl;
    end;

    {Test for DropModemOnClose no matter what RestoreOnClose says}
    if FlagIsSet(Flags, ptDropModemOnClose) then
      {Drop everything}
      Port[BaseAddr+MCreg] := 0;

    {Clear UART status and stray ints}
    inline($EB/$00);
    B := Port[BaseAddr+LSreg];            {Clear the line status reg}
    inline($EB/$00);
    B := Port[BaseAddr+MSreg];            {Clear the modem status}
    inline($EB/$00);
    B := Port[BaseAddr];                  {Clear stray chars}
    inline($EB/$00);
    B := Port[BaseAddr+IIDreg];           {Clear stray ints}

    {Free I/O buffers}
    {$IFDEF DPMI}
    GlobalFreeCheck(InBuff);
    GlobalFreeCheck(OutBuff);
    if UseStatusBuffer then
      GlobalFreeCheck(StatBuff);
    {$ELSE}
    FreeMemCheck(InBuff, InBuffLen);
    FreeMemCheck(OutBuff, OutBuffLen);
    if UseStatusBuffer then
      FreeMemCheck(StatBuff, InBuffLen);
    {$ENDIF}

    {Show ISR entry point as now available}
    ISRInUse[ISREntryPoint] := False;
  end;

  {Release heap space}
  {$IFDEF DPMI}
  GlobalFreeCheck(P);
  {$ELSE}
  FreeMemCheck(P, SizeOf(PortRec));
  {$ENDIF}
  P := Nil;

  {Show port slot as now available}
  ActiveComPort[I] := Nil;
end;

procedure uSetUart(ComName : ComNameType; NewBase : Word;
                   NewIrq, NewVector : Byte);
  {-Changes the standard base, irq and vector of (unopened) ComName}
begin
  AsyncStatus := ecOk;

  {$IFDEF LargeComNameSet}
  {Range check ComName}
  if ComName > Com8 then begin
    AsyncStatus := ecOutOfRange;
    Exit;
  end;
  {$ENDIF}

  {Change the default values}
  if NewBase <> 0 then
    DefBaseAddr[ComName] := NewBase;
  if NewIrq <> 0 then begin
    DefIrqNumber[ComName] := NewIrq;
    if NewVector = 0 then
      if NewIrq < 8 then
        DefComVector[ComName] := NewIrq+8
      else
        DefComVector[ComName] := NewIrq+$68
  end;
  if NewVector <> 0 then begin
    DefComVector[ComName] := NewVector;
    if NewIrq = 0 then
      if NewVector <= $F then
        DefIrqNumber[ComName] := NewVector-8
      else
        DefIrqNumber[ComName] := NewVector-$68;
  end;

  if (ComName = Com3) or (ComName = Com4) then
    PS2DetectMode := PS2Ignore;
end;

procedure uSetLine(P : PortRecPtr; Baud : LongInt;
                   Parity : ParityType;
                   DataBits : DataBitType;
                   StopBits : StopBitType);
  {-sets the UART and the port record with the new values}
var
  Divisor : Word;
  B : Byte;
begin
  AsyncStatus := ecOk;

  with P^ do begin
    {Do baud rate}
    if Baud <> 0 then begin
      {Calculate the divisor}
      if (Baud < 110) or (Baud > 115200) then begin
        GotError(P, epNonFatal+ecInvalidBaudRate);
        Exit;
      end;
      Divisor := Word(115200 div Baud);

      {Set baud}
      IntOff;                               {No ints while changing line}
      Port[BaseAddr+LCreg] := DLABMask;     {Enable baud rate setting}
      inline($EB/$00);
      Port[BaseAddr+BRLreg] := Lo(Divisor); {Store Divisor lo byte}
      inline($EB/$00);
      Port[BaseAddr+BRHreg] := Hi(Divisor); {Store Divisor hi byte}
      inline($EB/$00);
      Port[BaseAddr+LCreg] := LineControl;  {Turn off DLAB before IntOn}
      IntOn;
      CurBaud := Baud;
    end;

    {Set the LineReg mask (with parity, databits and stopbits)}
    LineControl := 0;
    LineControl := ParityMask[Parity] or
                   DataBitMask[DataBits] or
                   StopBitMask[StopBits];
    Port[BaseAddr+LCreg] := LineControl;

    {Store line parms in the ComPort record}
    CurParity := Parity;
    CurDataBits := DataBits;
    CurStopBits := StopBits;

    {Clear/save the UART if the baud changed}
    if Baud <> 0 then begin
      LineStatus := Port[BaseAddr+LSreg];     {Clear the line status reg}
      inline($EB/$00);
      ModemStatus := Port[BaseAddr+MSreg];    {Clear the modem status}
      inline($EB/$00);
      B := Port[BaseAddr];                    {Clear stray chars}
      inline($EB/$00);
      B := Port[BaseAddr+IIDreg];             {Clear stray ints}
    end;
  end;
end;

procedure uGetLine(P : PortRecPtr; var Baud : LongInt;
                   var Parity : ParityType;
                   var DataBits : DataBitType;
                   var StopBits : StopBitType;
                   FromHardware : Boolean);
  {-Gets the line params directly from the UART}
const
  MaxIter = 30000;
var
  Divisor : Word;
  SaveLC : Byte;
  Bits : Byte;
  LS : Byte;
  Finished : Boolean;
  DivL : Word;
  DivH : Word;
  I : Word;
begin
  AsyncStatus := ecOk;

  with P^ do begin
    if not FromHardware then begin
      {Return current field values}
      Baud := CurBaud;
      Parity := CurParity;
      DataBits := CurDataBits;
      StopBits := CurStopBits;
    end else begin
      {Get values from hardware}
      IntOff;

      {Wait until transmitter ready}
      I := 1;
      repeat
        LS := Port[BaseAddr+LSreg];
        Finished := (LS and TEMask) = TEMask;

        {Escape required for some TwinCom modems}
        if not Finished then begin
          {Just waste time...}
          I := I+2;
          I := I-1;
          Finished := I > MaxIter;
        end;
      until Finished;

      {Set the divisor latch to get the baud rate divisor}
      SaveLC := Port[BaseAddr+LCreg];
      inline($EB/$00);
      Port[BaseAddr+LCreg] := DLABMask;

      {Get the baud rate divisor bytes}
      DivL := Port[BaseAddr+BRLreg];
      inline($EB/$00);
      DivH := Port[BaseAddr+BRHreg];
      inline($EB/$00);

      {Restore the LC register}
      Port[BaseAddr+LCreg] := SaveLC;
      IntOn;

      {Calculate divisor and baud rate}
      Divisor := DivL + (DivH shl 8);
      if Divisor <> 0 then
        Baud := 115200 div Divisor
      else
        Baud := 115200;

      {Get the word length bits}
      DataBits := (SaveLC and $03) + 5;

      {Get the stop bits}
      if (SaveLC and StopBitsMask) = StopBitsMask then
        StopBits := 2
      else
        StopBits := 1;

      {Get the parity bits}
      Bits := (SaveLC shr 3) and $07;
      case Bits of
        0, 2, 4, 6 : Parity := NoParity;
        1 : Parity := OddParity;
        3 : Parity := EvenParity;
        5 : Parity := MarkParity;
        7 : Parity := SpaceParity;
      end;

      {Stuff the line values in the port record}
      CurBaud := Baud;
      CurParity := Parity;
      CurDataBits := DataBits;
      CurStopBits := StopBits;
    end;
  end;
end;

procedure uSetModem(P : PortRecPtr; DTR, RTS : Boolean);
  {-sets the port record with the new values}
begin
  AsyncStatus := ecOk;

  {Set the modem control behaviour}
  with P^ do begin
    {Set DTR and RTS states}
    ModemControl := Out2Mask;
    if DTR then
      ModemControl := ModemControl or DTRMask;
    if RTS then
      ModemControl := ModemControl or RTSMask;
    Port[BaseAddr+MCreg] := ModemControl;
  end;
end;

procedure uGetModem(P : PortRecPtr; var DTR, RTS : Boolean);
  {-Gets the DTR,RTS settings directly from the UART}
var
  Bits : Byte;
begin
  AsyncStatus := ecOk;

  {Get the modem control behaviour}
  with P^ do begin
    Bits := Port[BaseAddr+MCreg];

    {Check DTR}
    if (Bits and DTRMask) = DTRMask then
      DTR := True
    else
      DTR := False;

    {Check RTS}
    if (Bits and RTSMask) = RTSMask then
      RTS := True
    else
      RTS := False;

    {Stuff the modem control values in the port record}
    ModemControl := Bits;
  end;
end;

procedure uGetChar(P : PortRecPtr; var C : Char);
  {-Returns the next received character}
var
  Error : Byte;
begin
  AsyncStatus := ecOk;

  with P^ do begin
    if InBuffCount <= 0 then begin
      {Buffer is empty, return error}
      C := Char($FF);
      GotError(P, epNonFatal+ecBufferIsEmpty);
      AsyncStatus := ecBufferIsEmpty;

    end else begin
      {Get the next char}
      C := Char(InTail^);
      Inc(OS(InTail).Ofs);
      if InTail = InBuffEnd then
        InTail := InBuff;
      Dec(InBuffCount);

      {$IFDEF StatusBuffering}
      {Check status (and clear it for next time)}
      if UseStatusBuffer then begin
        if Byte(StatTail^) <> 0 then begin
          Error := Byte(StatTail^);
          Byte(StatTail^) := 0;
        end;
        {Increment buffer and check for wrap-around}
        Inc(OS(StatTail).Ofs);
        if StatTail = StatBuffEnd then
          StatTail := StatBuff;
      end else
      {$ENDIF}
        {No status buffering, just return ambiguous line error}
        Error := LineStatus;

      {Set AsyncStatus according to Error}
      if (Error and OverrunErrorMask) = OverrunErrorMask then
        AsyncStatus := ecOverrunError
      else if (Error and ParityErrorMask) = ParityErrorMask then
        AsyncStatus := ecParityError
      else if (Error and FramingErrorMask) = FramingErrorMask then
        AsyncStatus := ecFramingError
      else
        AsyncStatus := ecOk;

      {Clear the errors from LineStatus}
      if AsyncStatus <> ecOk then
        LineStatus := LineStatus and
                      not (OverrunErrorMask or
                           ParityErrorMask or
                           FramingErrorMask);

      {Check for buffer overflow (if no other errors occur)}
      if AsyncStatus = ecOk then
        if LostCharCount > 0 then begin
          AsyncStatus := ecBufferIsFull;
          LostCharCount := 0;
        end;

      {Report line errors to the error handler}
      if AsyncStatus <> ecOk then
        GotError(P, epNonFatal + AsyncStatus);

      {$IFDEF UseHWFlow}
      {Check for hardware flow control}
      if (HWFRecHonor <> 0) and HWFRemoteOff and
         (InBuffCount < HWFResume) then begin
        {Clear bits we are honoring, then set to "on" state}
        ModemControl := ModemControl xor HWFRecHonor;
        Port[BaseAddr+MCreg] := ModemControl;
        HWFRemoteOff := False;
      end;
      {$ENDIF}

      {$IFDEF UseSWFlow}
      {Check for software flow control}
      if (SWFState <> 0) and
         SWFSentXoff and
         (InBuffCount < SWFResume) then begin
        {Need to turn remote back on, send Xon}
        SWFSentXoff := False;
        SendByte(P, SWFOnChar);
      end;
      {$ENDIF}

      {$IFDEF Tracing}
      if TracingOn then
        AddTraceEntry('R', C);
      {$ENDIF}
    end;
  end;
end;

procedure uPeekChar(P : PortRecPtr; var C : Char; PeekAhead : Word);
  {-Looks ahead PeekAhead chars (with 1 being the next character)}
var
  PeekPtr : BPtr;
  LongOfs : LongInt;
begin
  AsyncStatus := ecOk;

  with P^ do begin
    if InBuffCount < PeekAhead then begin
      {Buffer is empty, return error}
      C := Char($FF);
      GotError(P, epNonFatal+ecBufferIsEmpty);
      AsyncStatus := ecBufferIsEmpty;
    end else begin
      {Make sure it's within the buffer}
      if PeekAhead > InBuffLen then begin
        GotError(P, epNonFatal+ecInvalidArgument);
        Exit;
      end else begin
        {Look ahead in the input buffer PeekAhead characters}
        Dec(PeekAhead);
        LongOfs := OS(InTail).Ofs;
        Inc(LongOfs, PeekAhead);

        {Init PeekPtr and adjust offset}
        if (LongOfs < 65521) and (LongOfs < OS(InBuffEnd).Ofs) then begin
          LongOfs := PeekAhead;
          PeekPtr := InTail
        end else begin
          Dec(LongOfs, (InBuffLen+OS(InBuff).Ofs));
          PeekPtr := InBuff;
        end;

        {Calculate offset and peek at the character}
        Inc(OS(PeekPtr).Ofs, LongOfs);
        C := Char(PeekPtr^);
      end;
    end;
  end;
end;

procedure uPutChar(P : PortRecPtr; C : Char);
  {-Adds char to xmit buffer}
begin
  AsyncStatus := ecOk;
  with P^ do begin
    if TxInts then begin
      {Attempt to stuff char in the output buffer}
      if  OutBuffCount < OutBuffLen then begin
        {It fits, insert the new character}
        IntOff;
        OutHead^ := Byte(C);
        Inc(OutBuffCount);
        Inc(OS(OutHead).Ofs);
        {Check for buffer wrap-around}
        if OutHead = OutBuffEnd then
          OutHead := OutBuff;
        {Start the transmitter if required}
        uStartTransmitter(P);
        IntOn;
      end else begin
        {Transmit buffer overflow}
        GotError(P, epNonFatal+ecBufferIsFull);
        Exit;
      end;
    end else
      {Output one char if the transmit register is empty}
      if (Port[BaseAddr+LSreg] and THREMask) = THREMask then
        Port[BaseAddr] := Byte(C)
      else begin
        GotError(P, epNonFatal+ecBufferIsFull);
        Exit;
      end;
  end;

  {$IFDEF Tracing}
  if TracingOn then
    AddTraceEntry('T', C);
  {$ENDIF}
end;

procedure uStartTransmitter(P : PortRecPtr);
begin
  {If the transmitter needs to be started...}
  with P^ do begin
    if TxReady then begin

      {$IFDEF EventLogging}
      if EventLogging then begin
        {Convert the word index to a record index}
        EventCount := EventIndex div SizeOf(EventRec);
        with EventQueue^[EventCount] do begin
          IsrNumber := -P^.CurrentPort;
          Signature := StartSignature;
          CurTime := ReadTimer;
          Inc(EventIndex, 6);
          if EventIndex >= (MaxEvent * SizeOf(EventRec)) then begin
            EventIndex := 0;
            EventsWrapped := True;
          end;
        end;
      end;
      {$ENDIF}

      {Go start the transmitter}
      CommTx(P);
    end;
  end;
end;

function uCharReady(P : PortRecPtr) : Boolean;
  {-Returns True if at least one char is in the input buffer}
begin
  with P^ do
    uCharReady := InBuffCount > 0;
end;

function uTransReady(P : PortRecPtr) : Boolean;
  {-Returns True if at least one space is avaialable in the output buffer}
begin
  with P^ do
    if TxInts then
      uTransReady := OutBuffLen > OutBuffCount
    else
      uTransReady := (Port[BaseAddr+LSreg] and THREMask) = THREMask;
end;

procedure uSendBreak(P : PortRecPtr);
  {-Sends a serial line break}
var
  LCR, LSR : Byte;
begin
  with P^ do begin
    AsyncStatus := ecOk;

    {Get current line control}
    LCR := Port[BaseAddr+LCreg];

    {Wait for THRE (transmit holding register empty)}
    IntOff;
    repeat
      LSR := Port[BaseAddr+LSreg];
    until (LSR and THREMask) = THREMask;

    {Send a null character}
    Port[BaseAddr] := $00;

    {Wait for null to be moved from hold register to shift register}
    repeat
      LSR := Port[BaseAddr+LSreg];
    until (LSR and THREMask) = THREMask;

    {Set break control bit}
    Port[BaseAddr+LCreg] := LCR or SetBreakMask;
    inline($EB/$00);

    {Wait for null to get shifted out}
    repeat
      LSR := Port[BaseAddr+LSreg];
    until (LSR and TEMask) = TEMask;

    {Reset Break bit}
    Port[BaseAddr+LCreg] := LCR;
    IntOn;
  end;
end;

procedure uActivatePort(P : PortRecPtr; Restore : Boolean);
  {-Turns on interrupts for this port}
var
  Irq : Byte;
  TempIrq : Byte;
  IrqMask : Byte;
  PicOcw1 : Word;
  Junk : Word;
begin
  with P^ do begin
    {Assure no COM interrupts}
    IntOff;

    {Set the UART interrupt mask (all types on except transmit)}
    IntMask := ReceiveIntMask + LineIntMask + ModemIntMask;

    {Turn interrupts on at the UART}
    Port[BaseAddr+IEreg] := $00;            {Avoid triggering UART ints}
    inline($EB/$00);                        {jmp short $+2 ;delay}
    Port[BaseAddr+IEreg] := IntMask;        {Set the interrupt mask}

    {Clear/save the UART}
    LineStatus := Port[BaseAddr+LSreg];     {Clear the line status reg}
    inline($EB/$00);                        {jmp short $+2 ;delay}
    ModemStatus := Port[BaseAddr+MSreg];    {Save the modem status}
    inline($EB/$00);                        {jmp short $+2 ;delay}
    Junk := Port[BaseAddr];                 {Clear stray chars}
    inline($EB/$00);                        {jmp short $+2 ;delay}
    Junk := Port[BaseAddr+IIDreg];          {Clear stray ints}

    {Calculate the IrqMask}
    IrqNumber := DefIrqNumber[PortName];
    case IrqNumber of
      0..7 : {Master 8259}
        begin
          IrqMask := 1 shl IrqNumber;
          PicOcw1 := Master8259;
          Flags := Flags and not ptHiIrq;
        end;

      8..15 : {Slave 8259}
        begin
          IrqMask := 1 shl (IrqNumber - 8);
          PicOcw1 := Slave8259;
          Flags := Flags or ptHiIrq;
        end;
    end;

    {Optionally (re)set this port's vector}
    if Restore then
      SetIntVec(Vector, CommIntAddr[ISREntryPoint]);

    {Turn on interrupts for this UART}
    Irq := Port[PicOcw1];                 {Get the current int mask}
    TempIrq := (not IrqMask);             {Clear UART int bit}
    Port[PicOcw1] := (Irq and TempIrq);   {Restores old ints with UART int}

    {Turn on int2 if IrqNumber > 7}
    if IrqNumber > 7 then begin
      TempIrq := Port[Master8259];
      inline($EB/$00);
      Port[Master8259] := TempIrq and $FB;
    end;

    {Out2 always required}
    ModemControl := Port[BaseAddr+MCreg];
    ModemControl := ModemControl or Out2Mask;
    Port[BaseAddr+MCreg] := ModemControl;

    {Reset the transmit completion chain}
    TxReady := True;
    TxIntsActive := False;

    {Allow interrupts again}
    IntOn;
  end;
end;

procedure uDeactivatePort(P : PortRecPtr; Restore : Boolean);
  {-Turns off interrupts for this port}
var
  IrqMask : Byte;
  Irq : Byte;
  PicOcw1 : Word;
  Junk : Byte;
begin
  with P^ do begin
    {Assure no COM interrupts}
    IntOff;

    {Drop Out2}
    ModemControl := ModemControl and not Out2Mask;
    Port[BaseAddr+MCreg] := ModemControl;
    inline($EB/$00);

    {Clear interrupt enable register}
    Port[BaseAddr+IEreg] := 0;
    inline($EB/$00);

    {Clear any pending conditions}
    Junk := Port[BaseAddr+LSreg];           {Clear the line status reg}
    inline($EB/$00);                        {jmp short $+2 ;delay}
    Junk := Port[BaseAddr+MSreg];           {Clear the modem status}
    inline($EB/$00);                        {jmp short $+2 ;delay}
    Junk := Port[BaseAddr];                 {Clear stray chars}
    inline($EB/$00);                        {jmp short $+2 ;delay}
    Junk := Port[BaseAddr+IIDreg];          {Clear stray ints}

    {Get the 8259 address and mask}
    case IrqNumber of
      0..7 : {Master 8259}
        begin
          IrqMask := 1 shl IrqNumber;
          PicOcw1 := Master8259;
        end;

      8..15 : {Slave 8259}
        begin
          IrqMask := 1 shl (IrqNumber - 8);
          PicOcw1 := Slave8259;
        end;
    end;

    {Force interrupts off at the UART}
    Irq := Port[PicOcw1];
    inline($EB/$00);
    Port[PicOcw1] := (Irq or IrqMask);

    {Optionally restore vector}
    if Restore then
      SetIntVec(Vector, OrigPortState.Vector);

    {Restore interrupts}
    IntOn;
  end;
end;

procedure uSavePort(P : PortRecPtr; var PSR);
  {-Saves the state of the UART into PSR}
begin
  SaveUartState(P^.BaseAddr, PortSaveRec(PSR));
end;

procedure uRestorePort(P : PortRecPtr; var PSR);
  {-Restores the state of the UART from PSR}
var
  Junk : Byte;
begin
  with P^ do begin
    RestoreUartState(BaseAddr, PortSaveRec(PSR));

    {Update internal variables with current UART state}
    LineStatus := Port[BaseAddr+LSreg];
    inline($EB/$00);
    ModemStatus := Port[BaseAddr+MSreg];

    {Assure all interrupt states are cleared}
    Junk := Port[BaseAddr];
    inline($EB/$00);                        {jmp short $+2 ;delay}
    Junk := Port[BaseAddr+IIDreg];
  end;
end;

function uUpdateLineStatus(P : PortRecPtr) : Byte;
  {-Returns line status register value}
begin
  {Reread line status register to get updated TE bit value}
  with P^ do begin
    LineStatus := Port[BaseAddr+LSreg];
    uUpdateLineStatus := LineStatus;
  end;
end;

function uUpdateModemStatus(P : PortRecPtr) : Byte;
  {-Returns modem status register value}
begin
  {Modem status register is always up to date....}
  uUpdateModemStatus := P^.ModemStatus;
end;

{$IFDEF UseHWFlow}
procedure uHWFlowSet(P : PortRecPtr; Enable : Boolean;
                     BufferFull, BufferResume : Word;
                     Options : Word);
  {-Enables/disables hardware flow control}
begin
  AsyncStatus := ecOk;

  with P^ do begin
    if Enable then begin
      {Turning flow control on....}
      if Options = 0 then begin
        GotError(P, epNonFatal+ecInvalidArgument);
        Exit;
      end;

      {Make sure BufferFull, BufferResume values are within buffer size}
      if Options and (hfUseDTR or hfUseRTS) <> 0 then
        if not CheckRange(BufferFull, MinInBuff, InBuffLen) or
           not CheckRange(BufferResume, MinInBuff, BufferFull) then begin
            GotError(P, epNonFatal+ecOutOfRange);
          Exit;
        end;

      {Make a transmit control mask out of the requested signals. Make}
      {the mask such that when xor'd with the actual modem status bits}
      {the result is zero if the status bit is in the "on" condition  }
      HWFTransMask := 0;
      HWFTransHonor := 0;
      if FlagIsSet(Options, hfRequireDSR) then begin
        HWFTransHonor := DSRMask;
        HWFTransMask := HWFTransMask or DSRMask;
        if FlagIsSet(Options, hfDSRActiveLow) then
          HWFTransMask := HWFTransMask and not DSRMask;
      end;
      if FlagIsSet(Options, hfRequireCTS) then begin
        HWFTransHonor := HWFTransHonor or CTSMask;
        HWFTransMask := HWFTransMask or CTSMask;
        if FlagIsSet(Options, hfCTSActiveLow) then
          HWFTransMask := HWFTransMask and not CTSMask;
      end;

      {Make a modem control "on" mask out of the requested signals}
      HWFRecMask := 0;
      HWFRecHonor := 0;
      if FlagIsSet(Options, hfUseDTR) then begin
        HWFRecHonor := DTRMask;
        HWFRecMask := DTRMask;
        if FlagIsSet(Options, hfDTRActiveLow) then
          HWFRecMask := HWFRecMask and not DTRMask;
      end;
      if FlagIsSet(Options, hfUseRTS) then begin
        HWFRecHonor := HWFRecHonor or RTSMask;
        HWFRecMask := HWFRecMask or RTSMask;
        if FlagIsSet(Options, hfRTSActiveLow) then
          HWFRecMask := HWFRecMask and not RTSMask;
      end;

      {Set the stop/resume limits}
      HWFFull := BufferFull;
      HWFResume := BufferResume;
    end else begin
      {Turning flow control off...}
      HWFRecHonor := 0;
      HWFTransHonor := 0;
    end;
  end;
end;

function uHWFlowGet(P : PortRecPtr) : FlowState;
  {-Returns hardware flow control state}
var
  FS : FlowState;
begin
  with P^ do begin
    if (HWFRecHonor = 0) and (HWFTransHonor = 0) then
      FS := fsOff
    else if HWFRemoteOff and (LastXmitError = 1) then
      FS := fsAllWait
    else if LastXmitError = 1 then
      FS := fsTransWait
    else if HWFRemoteOff then
      FS := fsRecWait
    else
      FS := fsClear;
  end;
  uHWFlowGet := FS;
end;
{$ENDIF}

{$IFDEF UseSWFlow}
procedure uSWFlowSet(P : PortRecPtr; Enable : Boolean;
                     BufferFull, BufferResume : Word;
                     Options : Word);
  {-Enables/disables software flow control}
begin
  AsyncStatus := ecOk;
  with P^ do begin
    if Enable then begin
      if FlagIsSet(Options, sfReceiveFlow) then
        {Make sure BufferFull, BufferResume values are within buffer size}
        if not CheckRange(BufferFull, MinInBuff, InBuffLen) or
           not CheckRange(BufferResume, MinInBuff, BufferFull) then begin
          GotError(P, epNonFatal+ecOutOfRange);
          Exit;
        end;

      {Turn it on}
      SWFState := Lo(Options);
      SWFFull := BufferFull;
      SWFResume := BufferResume;
      SWFGotXoff := False;
      SWFSentXoff := False;
    end else
      SWFState := 0;
  end;
end;

function uSWFlowGet(P : PortRecPtr) : FlowState;
  {-Returns software flow control state}
var
  FS : FlowState;
begin
  with P^ do begin
    if (SWFState  = 0) then
      FS := fsOff
    else if SWFSentXoff and SWFGotXoff then
      FS := fsAllWait
    else if SWFSentXoff then
      FS := fsRecWait
    else if SWFGotXoff then
      FS := fsTransWait
    else
      FS := fsClear;
  end;
  uSWFlowGet := FS;
end;

procedure uSWFlowCtl(P : PortRecPtr; OnChar, OffChar : Char;
                     Resume : Boolean);
  {-Sets software flow control characters and/or resumes transmits}
begin
  with P^ do begin
    {Handle resuming}
    if Resume and SWFGotXoff then begin
      SWFGotXoff := False;
      if OutBuffCount > 0 then
        uStartTransmitter(P);
    end;

    {Handle new on/off chars}
    if (OnChar <> OffChar) then begin
      SWFOnChar := OnChar;
      SWFOffChar := OffChar;
    end;
  end;
end;
{$ENDIF}

procedure uBufferStatus(P : PortRecPtr;
                        var InFree, OutFree, InUsed, OutUsed : Word);
  {-Returns various buffer values}
begin
  with P^ do begin
    InFree := InBuffLen - InBuffCount;
    OutFree := OutBuffLen - OutBuffCount;
    InUsed :=  InBuffCount;
    OutUsed := OutBuffCount;
   end;
end;

procedure uBufferFlush(P : PortRecPtr; FlushIn, FlushOut: Boolean);
  {-Flushes input/output buffers}
begin
  with P^ do begin
    if FlushIn then begin
      {Flush the input buffer}
      IntOff;
      InTail := InHead;
      InBuffCount := 0;
      IntOn;

      {$IFDEF UseHWFlow}
      if HWFRemoteOff then begin
        HWFRemoteOff := False;
        {Remote was paused, release our flow control}
        if FlagIsSet(HWFRecHonor, DTRMask) then
          uSetModem(P, FlagIsSet(HWFRecMask, DTRMask),
                       (ModemControl and RTSMask) = RTSMask);
        if FlagIsSet(HWFRecHonor, RTSMask) then
          uSetModem(P, (ModemControl and DTRMask) = DTRMask,
                        FlagIsSet(HWFRecMask, RTSMask));
      end;
      {$ENDIF}

      {$IFDEF UseSWFlow}
      if (SWFState <> 0) and SWFSentXoff then begin
        {Remote was paused, release our flow control}
        SWFSentXoff := False;
        uPutChar(P, SWFOnChar);
      end;
      {$ENDIF}
    end;

    if FlushOut then begin
      {Flush the output buffer}
      IntOff;
      OutTail := OutHead;
      OutBuffCount := 0;
      IntOn;
    end;
  end;
end;

function UartTest1(BaseAddr : Word) : Boolean;
  {-Passive read of IEreg, IIDreg and MCreg}
const
  BadIEMask  = $F0;     {1111xxxx}
  BadIIDMask = $30;     {xx11xxxx}
  BadMCMask  = $E0;     {111xxxxx}
var
  CheckIE  : Byte;
  CheckIID : Byte;
  CheckMC  : Byte;
begin
  {Get registers and AND with bits that should be zero}
  CheckIE := Port[BaseAddr+IEreg] and BadIEMask;
  inline($EB/$00);
  CheckIID := Port[BaseAddr+IIDreg] and BadIIDMask;
  inline($EB/$00);
  CheckMC := Port[BaseAddr+MCreg] and BadMCMask;

  {If any bits are left on this can't be a UART}
  if (CheckIE = 0) and (CheckIID = 0) and (CheckMC = 0) then
    UartTest1 := True
  else
    UartTest1 := False;
end;

function UartTest2(BaseAddr : Word) : Boolean;
  {-Can IE register be set to zero?}
var
  SaveIEreg : Byte;
begin
  SaveIEreg := Port[BaseAddr+IEreg];
  inline($EB/$00);
  Port[BaseAddr+IEreg] := 0;
  inline($EB/$00);
  if Port[BaseAddr+IEreg] = 0 then
    UartTest2 := True
  else
    UartTest2 := False;
  Port[BaseAddr+IEreg] := SaveIEreg;
end;

function UartTest3(BaseAddr : Word) : Boolean;
  {-Does the BIOS know about this port?}
const
  MaxPort = 4;            {Check for 4 ports}
type
  BiosDataType = Array[1..4] of Word;
  BiosDataPtr = ^BiosDataType;
var
  BiosData : BiosDataPtr; {array[1..4] of Word absolute $40:0;} {Com port base addresses}
  I : Byte;
begin
  UartTest3 := True;
  BiosData := Ptr(BiosDataSele, $0);

  {Search the BIOS data area for BaseAddr}
  for I := 1 to MaxPort do
    if BiosData^[I] = BaseAddr then
      Exit;

  {BaseAddr not found, BIOS doesn't know about this uart}
  UartTest3 := False;
end;

function ClassifyUart(BaseAddr : Word; CheckUart : Boolean) : UartType;
  {-Returns the UartType for the uart at BaseAddr}
const
  TestMask : Byte = $F0;         {Checks the existance of scratch register}
var
  IID : Byte;
  Test : Byte;
  SaveIID : Byte;
  SaveScratch : Byte;
begin
  {If requested, check to see if uart exists at BaseAddr}
  if CheckUart then
    if not UartTest1(BaseAddr) or not UartTest2(BaseAddr) then begin
      ClassifyUart := NoUart;
      Exit;
    end;

  {Check for the scratch register}
  SaveScratch := Port[BaseAddr+Sreg];
  inline($EB/$00);
  Port[BaseAddr+Sreg] := TestMask;
  inline($EB/$00);
  Test := Port[BaseAddr+Sreg];

  if Test <> TestMask then begin
    {Scratch register doesn't exist, must be an 8250B}
    ClassifyUart := U8250B;
    Exit;
  end else begin
    Port[BaseAddr+Sreg] := SaveScratch;
    inline($EB/$00);
  end;

  {Try to turn FIFO buffering on}
  SaveIID := Port[BaseAddr+IIDreg];
  Port[BaseAddr+IIDreg] := FifoOnMask;
  inline($EB/$00);

  {Read it back}
  IID := Port[BaseAddr+IIDreg];

  {If FIFO was previously off, turn it back off}
  if (SaveIID and $C0) <> $C0 then begin
    Port[BaseAddr+IIDreg] := FifoWriteMask;
    inline($EB/$00);
    Port[BaseAddr+IIDreg] := FifoOffMask;
  end;

  {Bits 7,6 hold result: 00=pre 16550, 10=16550, 11=16550AN}
  IID := IID shr 6;
  case IID of
    0 : ClassifyUart := U8250A;
    2 : ClassifyUart := U16550;
    3 : ClassifyUart := U16550A;
  end;
end;

procedure SetFifoBuffering(BaseAddr : Word; Enable : Boolean; Level : Byte);
  {-Turns fifo buffering on/off for uart at BaseAddr}
var
  LevelMask : Byte;
begin
  if Enable then begin
    {Turning FIFO on, build a mask from the requested level}
    if Level < 4 then
      LevelMask := $01
    else if Level < 8 then
      LevelMask := $41
    else if Level < 14 then
      LevelMask := $81
    else
      LevelMask :=$C1;

    {Write to the FIFO register}
    Port[BaseAddr+IIDreg] := FifoWriteMask;
    inline($EB/$00);
    Port[BaseAddr+IIDreg] := LevelMask;

  end else begin
    {Turning FIFO off, enable writes and write the off mask}
    Port[BaseAddr+IIDreg] := FifoWriteMask;
    inline($EB/$00);
    Port[BaseAddr+IIDreg] := FifoOffMask;
  end;
end;

function FifoStatus(BaseAddr : Word) : Boolean;
  {-Returns True if FIFO buffering is on for uart at BaseAddr}
var
  SaveIID : Byte;
begin
  SaveIID := Port[BaseAddr+IIDreg];
  if (SaveIID and $C0) = $C0 then
    FifoStatus := True
  else
    FifoStatus := False;
end;

procedure SaveUartState(BaseAddr : Word; var PSR : PortSaveRec);
  {-Save the state of the the uart at BaseAddr}
var
  I : ComNameType;
begin
  with PSR do begin
    {Save IER, MCR and LCR}
    IER := Port[BaseAddr+IEreg];
    inline($EB/$00);
    MCR := Port[BaseAddr+MCreg];
    inline($EB/$00);
    LCR := Port[BaseAddr+LCreg];
    inline($EB/$00);

    IntOff;
    {Enable reading of baud-rate divisors}
    Port[BaseAddr+LCreg] := LCR or $80;
    inline($EB/$00);
    BRLR := Port[BaseAddr+BRLreg];
    inline($EB/$00);
    BRHR := Port[BaseAddr+BRHreg];
    inline($EB/$00);

    {Turn off baud-rate divisor reading}
    Port[BaseAddr+LCreg] := LCR;
    IntOn;

    {Save FIFO status}
    if ClassifyUart(BaseAddr, False) = U16550A then
      FIFO := Port[BaseAddr+IIDreg];

    {Save the associated vector}
    for I := Com1 to Com8 do
      if DefBaseAddr[I] = BaseAddr then begin
        GetIntVec(DefComVector[I], Vector);
        Exit;
      end;
  end;
end;

procedure RestoreUartState(BaseAddr : Word; PSR : PortSaveRec);
  {-Restore the communications chips to their previous state}
var
  I : ComNameType;
begin
  with PSR do begin
    {Restore IER and MCR}
    Port[BaseAddr+IEreg] := IER;
    inline($EB/$00);
    Port[BaseAddr+MCreg] := MCR;
    inline($EB/$00);

    IntOff;
    {Enable setting of baud rate divisors}
    Port[BaseAddr+LCreg] := LCR or $80;
    inline($EB/$00);

    {Restore baud rate}
    Port[BaseAddr+BRLreg] := BRLR;
    inline($EB/$00);
    Port[BaseAddr+BRHreg] := BRHR;
    inline($EB/$00);

    {Restore LCR}
    Port[BaseAddr+LCreg] := LCR;
    inline($EB/$00);
    IntOn;

    {Restore original FIFO status}
    if ClassifyUart(BaseAddr, False) = U16550A then
      if (FIFO and $C0) <> $C0 then
        {FIFO was originally off, turn it back off}
        SetFifoBuffering(BaseAddr, False, 0)
      else
        {FIFO was originally on, turn in back on}
        SetFifoBuffering(BaseAddr, True, 8);

    {Restore the associated vector}
    for I := Com1 to Com8 do
      if DefBaseAddr[I] = BaseAddr then
        SetIntVec(DefComVector[I], Vector);
  end;
end;

function GetLineStatusDirect(BaseAddr : Word) : Byte;
  {-Return the line status byte from the UART register}
  {-Included for APRO 1.0 compatibility, no longer documented}
begin
  GetLineStatusDirect := Port[BaseAddr+LSReg];
end;

function CheckTEDirect(BaseAddr : Word) : Boolean;
  {-Return True if the transmitter is completely empty}
begin
  CheckTEDirect := (Port[BaseAddr+LSReg] and TEMask) = TEMask;
end;

function GetModemStatusDirect(BaseAddr : Word) : Byte;
  {-Return the modem status byte from UART register}
begin
  GetModemStatusDirect := Port[BaseAddr+MSReg];
end;

procedure SendLongBreak(BaseAddr : Word; Count : Byte);
  {-Sends a long serial line break}
var
  LCR, LSR : Byte;
  I : Byte;
begin
  AsyncStatus := ecOk;

  {Get current line control}
  LCR := Port[BaseAddr+LCreg];

  {Wait for THRE (transmit holding register empty)}
  IntOff;
  repeat
    LSR := Port[BaseAddr+LSreg];
  until (LSR and THREMask) = THREMask;

  {Set break control bit}
  Port[BaseAddr+LCreg] := LCR or SetBreakMask;

  for I := 1 to Count do begin
    {Send a null character}
    Port[BaseAddr] := $00;

    {Wait for null to be moved from hold register to shift register}
    repeat
      LSR := Port[BaseAddr+LSreg];
    until (LSR and THREMask) = THREMask;

    {Wait for null to get shifted out}
    repeat
      LSR := Port[BaseAddr+LSreg];
    until (LSR and TEMask) = TEMask;
  end;

  {Reset Break bit}
  Port[BaseAddr+LCreg] := LCR;
  IntOn;
end;

{$IFDEF EventLogging}
function Cardinal(L : LongInt) : Real;
  {-Return the unsigned equivalent of L as a real}
begin
  if L < 0 then
    Cardinal := 4294967296.0+L
  else
    Cardinal := L;
end;

function ElapsedTime(Start, Stop : LongInt) : Real;
  {-Calculate time elapsed (in milliseconds) between Start and Stop}
begin
  ElapsedTime := 1000.0*Cardinal(Stop-(Start+Delta))/TimerResolution;
end;

function ElapsedTimeString(Start, Stop : LongInt) : string;
  {-Return time elapsed (in milliseconds) between Start and Stop as a string}
var
  R : Real;
  S : string;
begin
  R := ElapsedTime(Start, Stop);
  Str(R:0:3, S);
  ElapsedTimeString := S;
end;

procedure InitializeTimer;
  {-Reprogram the timer chip to allow 1 microsecond resolution}
begin                      {InitializeTimer}
  {select timer mode 2, read/write channel 0}
  Port[$43] := $34;        {00110100b}
  inline($EB/$00);         {jmp short $+2 ;delay}
  Port[$40] := $00;        {LSB = 0}
  inline($EB/$00);         {jmp short $+2 ;delay}
  Port[$40] := $00;        {MSB = 0}
end;                       {InitializeTimer}

procedure RestoreTimer;
  {-Restore the timer chip to its normal state}
begin                      {RestoreTimer}
  {select timer mode 3, read/write channel 0}
  Port[$43] := $36;        {00110110b}
  inline($EB/$00);         {jmp short $+2 ;delay}
  Port[$40] := $00;        {LSB = 0}
  inline($EB/$00);         {jmp short $+2 ;delay}
  Port[$40] := $00;        {MSB = 0}
end;                       {RestoreTimer}

procedure Calibrate;
  {-Calibrate the timer}
const
  Reps = 1000;
var
  I : Word;
  L1, L2, Diff : LongInt;
begin
  Delta := MaxInt;
  for I := 1 to Reps do begin
    L1 := ReadTimer;
    L2 := ReadTimer;
    {use the minimum difference}
    Diff := L2-L1;
    if Diff < Delta then
      Delta := Diff;
  end;
end;

procedure InitEventLogging(Events : Word);
  {-Accelerates and calibrates the timer, allocates an event buffer}
begin
  AsyncStatus := ecOk;

  {Make sure the requested number of events will fit}
  if Events > HighestEvent then begin
    AsyncStatus := ecOutOfRange;
    Exit;
  end else begin
    MaxEventIndex := Events * SizeOf(EventRec);
    MaxEvent := Events;
  end;

  {Allocate an event buffer}
  {$IFDEF DPMI}
  if not GlobalAllocCheck(EventQueue, gmem_Fixed,
                        Events * SizeOf(EventRec)) then begin
    AsyncStatus := ecOutOfMemory;
    Exit;
  end;
  {$ELSE}
  if not GetMemCheck(EventQueue, Events * SizeOf(EventRec)) then begin
    AsyncStatus := ecOutOfMemory;
    Exit;
  end;
  {$ENDIF}

  {Other inits}
  EventIndex := 0;
  EventCount := 0;
  EventsWrapped := False;
  EventLogging := True;

  {Reprogram the timer chip}
  InitializeTimer;

  {Adjust for speed of machine}
  Calibrate;

  {Get the starting time}
  EventStart := ReadTimer;
end;

procedure DumpEvents(FName : PathStr);
  {-Write the EventQueue to EVENTS.TXT}
type
  String2 = String[2];
const
  {Report headings (first page only)}
  EventHdg1 = 'Event#        Time   ISR  Data  [Hex]    Event';
  EventHdg2 = '------        ----   ---  ----  -----    -----';

  {Printable codes for unprintable characters}
  PrintCode : array[0..33] of String[3] = (
    'NUL', 'SOH', 'STX', 'ETX', 'EOT', 'ENQ', 'ACK', 'BEL', 'BS ',
    'TAB', 'LF ', 'VT ', 'FF ', 'CR ', 'SO ', 'SI ', 'DLE', 'DC1',
    'DC2', 'DC3', 'DC4', 'NAK', 'SYN', 'ETB', 'CAN', 'EM ', 'SUB',
    'ESC', 'FS ', 'GS ', 'RS ', 'US ', 'SP ', 'HIH');

var
  Start, Len : Word;
  EventFile : Text;
  EventFileBuffer : array[1..4096] of Char;
  C : Char;
  B : String2;
  JustKicked : Boolean;
  EventNumber : Word;
  LastIsrNumber : Word;

  function HexB(C : Char) : String2;
    {-return hex representation of C}
  const
    hc : array[0..15] of Char = '0123456789ABCDEF';
  begin
    HexB := hc[Byte(C) shr 4]+hc[Byte(C) and $F];
  end;

  procedure WriteTime(CurTime : LongInt; IsrNumber : Word; EventStr : String);
    {-Write a time event}
  begin
    WriteLn(EventFile, EventNumber:5,
                       ElapsedTimeString(EventStart, CurTime):14,
                       IsrNumber:4, ' ':18, EventStr);
  end;

  procedure WriteEvent(IsrNumber : Word; Data : Word; EventStr : String);
    {-Write a followup event}
  var
    DataS : String;
    C : Char;
    CString : String[5];
  begin
    C := Char(Lo(Data));
    B := HexB(C);
    if (EventStr[1] = 't') or (EventStr[1] = 'r') then
      if C < #32 then
        CString := '<'+PrintCode[Byte(C)]+'>'
      else if C >= #128 then
        CString := '<'+PrintCode[33]+'>'
      else
        CString := C + '    '
    else
      CString := '';
    DataS := '   ' + CString + ' [' + B + ']     ';
    WriteLn(EventFile, EventNumber:5, ' ':14, IsrNumber:4, DataS:18, EventStr);
  end;

begin
  {Make sure we have something to do}
  if not EventLogging then
    Exit;

  {Restore timer and indicate event logging is off}
  RestoreTimer;
  EventLogging := False;

  {Convert the word index to a record index}
  EventCount := EventIndex div SizeOf(EventRec);

  {Set the Start and Len markers}
  Len := EventCount;
  if EventsWrapped then begin
    Start := EventCount;
    {Skip events until we find a valid starting signature}
    while (EventQueue^[Start].Signature <> StartSignature) do begin
      Inc(Start);
      if Start = MaxEvent then
        Start := 0;
      {Exit if we couldn't find a start signature anywhere}
      if Start = Len then
        Exit;
    end;
  end else if EventCount <> 0 then
    Start := 0
  else
    {No events, just exit}
    Exit;

  {Open the file (overwritting any existing trace file)}
  Assign(EventFile, FName);
  SetTextBuf(EventFile, EventFileBuffer, SizeOf(EventFileBuffer));
  ReWrite(EventFile);
  if IoResult <> 0 then begin
    AsyncStatus := ecEventFileError;
    {$IFDEF DPMI}
    GlobalFreeCheck(EventQueue);
    {$ELSE}
    FreeMemCheck(EventQueue, MaxEvent * SizeOf(EventRec));
    {$ENDIF}
    Exit;
  end;

  {Write out a one-time header}
  WriteLn(EventFile, EventHdg1);
  WriteLn(EventFile, EventHdg2);

  {Write the event buffer to a file}
  JustKicked := False;
  EventNumber := 1;
  repeat
    with EventQueue^[Start] do begin
      if Signature = StartSignature then begin
        {Process a starting event}
        if IsrNumber < 0 then begin
          WriteTime(CurTime, Abs(IsrNumber), 'transmitter call');
          JustKicked := True;
        end else begin
          WriteTime(CurTime, IsrNumber, 'ISR entered');
          JustKicked := False;
        end;
        LastIsrNumber := Abs(IsrNumber);
      end else begin
        {Process a continuing event}
        case EventType of
          0 : {End of Interrupt routine}
            begin
              WriteTime(CurTime, LastIsrNumber, 'ISR exited');
              WriteLn(EventFile);
            end;
          1 : {IID mask}
            WriteEvent(LastIsrNumber, DataWord, 'interrupt ID');
          2 : {Line status changed}
            WriteEvent(LastIsrNumber, DataWord, 'line status');
          3 : {Received character}
            WriteEvent(LastIsrNumber, DataWord, 'receive character');
          4 : {Transmitted character}
            begin
              WriteEvent(LastIsrNumber, DataWord, 'transmit character');
              if JustKicked then begin
                JustKicked := False;
                WriteLn(EventFile);
              end;
            end;
          5 : {Modem status changed}
            WriteEvent(LastIsrNumber, DataWord, 'modem status');
          else
            WriteLn(EventFile, 'unknown eventtype ', EventType);
        end;
      end;

      {Get the next event}
      Inc(Start);
      if Start = MaxEvent then
        Start := 0;
      Inc(EventNumber);
    end;
  until Start = Len;

  {Clear any I/O errors}
  AsyncStatus := IOResult;

  {Clean up}
  Close(EventFile);
  {$IFDEF DPMI}
  GlobalFreeCheck(EventQueue);
  {$ELSE}
  FreeMemCheck(EventQueue, MaxEvent * SizeOf(EventRec));
  {$ENDIF}
end;
{$ENDIF}

Function uPutBlock (P: PortRecPtr; Var Block; Size: Word): Word;
Var
  FreeSpace, InFree, OutFree, InUsed, OutUsed : Word;

Begin
  {Get static value for freespace}
  BufferStatus (P, InFree, OutFree, InUsed, OutUsed);

  {Make sure there is enough room}
  If Not FlagIsSet (P^. Flags, ptExecutePartialPuts) Then
    If OutFree < Size Then
    Begin
      GotError (P, epNonFatal + ecBufferIsFull);
      uPutBlock := 0;
      Exit;
    End;

  {Truncate if not enough room}
  If Size > OutFree Then
    Size := OutFree;

  With P^ Do
  Begin
    IntOff;
    If (OS (OutHead). Ofs >= OS (OutTail). Ofs) Then
    Begin
      FreeSpace := (OS (OutBuffEnd). Ofs - OS (OutHead). Ofs); {"Normal" buffer}
      If FreeSpace >= Size Then
      Begin
        Move (CharArray (Block) [0], OutHead^, Size);
        Inc (OS (OutHead). Ofs, Size);
      End Else
      Begin
        Move (CharArray (Block) [0], OutHead^, FreeSpace);
        Move (CharArray (Block) [FreeSpace], OutBuff^, Size - FreeSpace);
        OutHead := OutBuff;
        Inc (OS (OutHead). Ofs, Size - FreeSpace);
      End;
    End Else
    Begin
      Move (CharArray (Block) [0], OutHead^, Size); {"Wrapped" buffer}
      Inc (OS (OutHead). Ofs, Size);
    End;

    Inc (OutBuffCount, Size);     {Adjust OutHead and count}
    If OutHead = OutBuffEnd Then  {Check for buffer wrap-around}
      OutHead := OutBuff;
    StartTransmitter (P);         {(Re)start the transmit stream}
    IntOn;

    uPutBlock := Size;            {Return the bytes written}
  End;

  AsyncStatus := ecOk;
End;

{$F+}
procedure UartExitProc;
  {-Exit procedure to restore the timer}
begin
  ExitProc := UartExitSave;
  {$IFDEF EventLogging}
  if EventLogging then
    RestoreTimer;
  {$ENDIF}
end;
{$F-}

procedure ActivateApUart;
  {-Registers this unit as the active "device layer"}
begin
  {$IFNDEF UseOOP}
  {Assign uart device-level procedures}
  InitPort := uInitPort;
  InitPortKeep := uInitPortKeep;
  DonePort := uDonePort;
  SetLine := uSetLine;
  GetLine := uGetLine;
  SetModem := uSetModem;
  GetModem := uGetModem;
  GetChar := uGetChar;
  PeekChar := uPeekChar;
  PutChar := uPutChar;
  PutBlock := uPutBlock;
  StartTransmitter := uStartTransmitter;
  CharReady := uCharReady;
  TransReady := uTransReady;
  SendBreak := uSendBreak;
  ActivatePort := uActivatePort;
  DeactivatePort := uDeactivatePort;
  SavePort := uSavePort;
  RestorePort := uRestorePort;

  UpdateLineStatus := uUpdateLineStatus;
  UpdateModemStatus := uUpdateModemStatus;
  {$IFDEF UseHWFlow}
  HWFlowSet := uHWFlowSet;
  HWFlowGet := uHWFlowGet;
  {$ENDIF}
  {$IFDEF UseSWFlow}
  SWFlowSet := uSWFlowSet;
  SWFlowGet := uSWFlowGet;
  SWFlowCtl := uSWFlowCtl;
  {$ENDIF}
  BufferStatus := uBufferStatus;
  BufferFlush := uBufferFlush;

  {$ENDIF}
  SetUart := uSetUart;
end;

var
  I : Word;
begin
  {$IFDEF AutoDeviceInit}
  {Activate this unit's device-layer}
  ActivateApUart;
  {$ELSE}
  SetUart := uSetUart;
  {$ENDIF}

  {Setup exit procedure to close open com ports}
  UartExitSave := ExitProc;
  ExitProc := @UartExitProc;

  {Set the ISR addresses}
  CommIntAddr[1] := @CommInt1;
  CommIntAddr[2] := @CommInt2;
  CommIntAddr[3] := @CommInt3;
  CommIntAddr[4] := @CommInt4;

  {Mark all ISR entry points as available}
  for I := 1 to MaxApuartPort do
    ISRInUse[I] := False;
end.
