{$IFDEF MSDOS}
  {$IFNDEF DPMI32}
    {$F+}
  {$ENDIF}
{$ENDIF}
{$I-}

{Conditional defines that may affect this unit}
{$I APDEFINE.INC}

{*********************************************************}
{*                    APCOM.PAS 2.03                     *}
{*     Copyright (c) TurboPower Software 1991.           *}
{* Portions copyright (c) Information Technology 1989,   *}
{*    and used under license to TurboPower Software      *}
{*                 All rights reserved.                  *}
{*********************************************************}

Unit ApCom;
  {-Interface layer for serial I/O}

Interface

Uses
{$IFNDEF OS2}
  ApMisc,
  ApPort,
{$ELSE}
  ApOS2,
  OS2Base,
{$ENDIF}
  OpInline,
  ApSame,
  ApTimer;

{$IFNDEF VirtualPascal}
Procedure IntOff; Inline ($9C / $FA); {PUSHF/CLI}
Procedure IntOn; Inline ($9D); {POPF}
{$ELSE}
Procedure IntOff;
Procedure IntOn;
{$ENDIF}

Procedure GetCharTimeout (P: PortRecPtr; Var C: Char; Timeout: Word);
Procedure GetString (P: PortRecPtr; Var S: String; ExpectedLen: Byte;
                     Const DelimSet: CharSet);
Procedure GetStringTimeout (P: PortRecPtr; Var S: String; ExpectedLen: Byte;
                            Const DelimSet: CharSet; Timeout: Word);
Procedure PutCharTimeout (P: PortRecPtr; C: Char; Timeout: Word);
Procedure PutString (P: PortRecPtr; S: String);
Procedure PutStringTimeout (P: PortRecPtr; S: String; Timeout: Word);
Procedure FlushInBuffer (P: PortRecPtr);
Procedure FlushOutBuffer (P: PortRecPtr);
Function InBuffUsed (P: PortRecPtr): Word;
Function OutBuffUsed (P: PortRecPtr): Word;
Function InBuffFree (P: PortRecPtr): Word;
Function OutBuffFree (P: PortRecPtr): Word;
Procedure DrainOutBuffer (P: PortRecPtr; Timeout: Word);
{$IFNDEF OS2}
Function GetModemStatus (P: PortRecPtr): Byte;
{$ENDIF}
{$IFDEF UseSWFlow}
Procedure SWFlowEnable (P: PortRecPtr; BufferFull, BufferResume: Word);
{$ENDIF}
{$IFDEF UseHWFlow}
Procedure HWFlowEnable (P: PortRecPtr; BufferFull, BufferResume: Word;
                       Options: Word);
{$ENDIF}
Procedure SetDTR (P: PortRecPtr; State: Boolean);
Procedure SetRTS (P: PortRecPtr; State: Boolean);
Function CheckDCD (P: PortRecPtr): Boolean;
Function CheckCTS (P: PortRecPtr): Boolean;
Function CheckDSR (P: PortRecPtr): Boolean;
Function CheckRI (P: PortRecPtr): Boolean;
Function CheckDataReady (P: PortRecPtr): Boolean;
Function GetComName (P: PortRecPtr): ComNameType;
Procedure SetErrorProc (P: PortRecPtr; EP: AsyncErrorProc);
Procedure SetAbortFunc (P: PortRecPtr; AFunc: AbortFunc);
Procedure ptOptionsOn (P: PortRecPtr; OptionFlags: Word);
Procedure ptOptionsOff (P: PortRecPtr; OptionFlags: Word);
Function ProtocolInProgress (P: PortRecPtr): Boolean;
Function WaitComplete (P: PortRecPtr; Const ET: EventTimer): Boolean;

Implementation

Type
  OS = Record
         Ofs : Word;
         Seg : Word;
       End;

Const
  EmptySet : CharSet = [];

{$IFDEF VirtualPascal}
Procedure IntOff; Assembler; {&USES None} {&FRAME-}
Asm
End;

Procedure IntOn; Assembler; {&USES None} {&FRAME-}
Asm
End;
{$ENDIF}

Function WaitComplete (P: PortRecPtr; Const ET: EventTimer): Boolean;
  {-Returns True if ET expired or UserAbort returns True}
Begin
  WaitComplete := True;

  {Check for timer expired or user abort}
  If TimerExpired (ET) Then
    AsyncStatus := ecTimeout
  Else
    If P^. UserAbort Then
      AsyncStatus := ecUserAbort
    Else
      WaitComplete := False; {Still waiting...}
End;

Function BlockReady (P: PortRecPtr; ExpectedLen: Word;
                    Const DelimSet: CharSet): Boolean;
  {-Returns True if a block is ready of ExpectedLen or with DelimSet}
Var
  i, CharsInBuff : Word;
  C              : Char;

Begin
  BlockReady := True;
  CharsInBuff := InBuffUsed (P);

  {Check expected length first}
  If (ExpectedLen > 0) And (ExpectedLen <= CharsInBuff) Then
    Exit;

  {Check for delimiter}
  If DelimSet <> EmptySet Then
    {Look thru buffer for chars in DelimSet}
    For i := 1 To CharsInBuff Do
    Begin
      PeekChar (P, C, i);
      If C in DelimSet Then
        Exit;
    End;

  {If we get here, the string isn't ready}
  BlockReady := False;
End;

Procedure GetCharTimeout (P: PortRecPtr; Var C: Char; Timeout: Word);
  {-Waits for C or Timeout}
Var
  ET : EventTimer;

Begin
  AsyncStatus := ecOk;
  C := #255;

  If CharReady (P) Then {If a char is ready now just exit}
    GetChar (P, C)
  Else
  Begin {Char not ready, start waiting and checking}
    NewTimer (ET, Timeout);
    Repeat
    Until CharReady (P) Or WaitComplete (P, ET);

    {Report WaitComplete errors only}
    If (AsyncStatus = ecTimeout) Or (AsyncStatus = ecUserAbort) Then
      GotError (P, epNonFatal + AsyncStatus)
    Else
      GetChar (P, C);
  End;
End;

Procedure GetString (P: PortRecPtr; Var S: String; ExpectedLen: Byte;
                    Const DelimSet: CharSet);
  {-Returns string S with Len or ending with DelimSet}
Var
  C                             : Char;
  Cnt                           : Byte;
  Finished, GotDelim, Delimited : Boolean;

Begin
  AsyncStatus := ecOk;
  S := '';
  Delimited := DelimSet <> EmptySet;

  If (ExpectedLen = 0) And Not Delimited Then
  Begin
    GotError (P, epNonFatal + ecInvalidArgument);
    Exit;
  End;

  {If partial strings aren't allowed, make sure we can satisfy the request}
  If Not FlagIsSet (P^. Flags, ptReturnPartialGets) And P^. Buffered Then
    If Not BlockReady (P, ExpectedLen, DelimSet) Then
    Begin
      GotError (P, epNonFatal + ecBufferIsEmpty);
      Exit;
    End;

  {Ok to return partial strings}
  Finished := False;
  GotDelim := False;
  Cnt := 1;

  Repeat
    {Get a character (or error)}
    If Not CharReady (P) Then
    Begin
      {Expected char but didn't get one, return incomplete string}
      If Cnt = 1 Then
        GotError (P, epNonFatal + ecBufferIsEmpty)
      Else
        GotError (P, epNonFatal + ecStringIncomplete);
      S [0] := Char (Cnt - 1);
      Exit;
    End Else
    Begin
      {Get the char}
      GetChar (P, C);
      If AsyncStatus <> ecOk Then
      Begin
        {Got line error, just exit}
        S [0] := Char (Cnt - 1);
        Exit;
      End;

      {Add the character}
      S [Cnt] := C;

      {Check to see if we're finished}
      If Delimited And (C in DelimSet) Then
      Begin
        Finished := True;
        GotDelim := True;
      End;

      If (ExpectedLen <> 0) And (Cnt >= ExpectedLen) Then
        Finished := True;

      {Check for wrap}
      If Not Finished Then
      Begin
        Inc (Cnt);
        If Cnt = 0 Then
        Begin
          GotError (P, epNonFatal + ecStringOverrun);
          S [0] := #255;
          Exit;
        End;
      End;
    End;
  Until Finished;

  S [0] := Char (Cnt);

  {Optionally delete the delimiter character}
  If Delimited And GotDelim And Not FlagIsSet (P^. Flags, ptReturnDelimiter) Then
    S [0] := Char (Cnt - 1);
End;

Procedure GetStringTimeout (P: PortRecPtr; Var S: String; ExpectedLen: Byte;
                            Const DelimSet: CharSet; Timeout: Word);
  {-Waits for string S or Timeout}
Var
  ET                  : EventTimer;
  SaveStatus          : Word;
  Cnt                 : Byte;
  C                   : Char;
  Delimited, Finished : Boolean;

Begin
  AsyncStatus := ecOk;
  S := '';
  Delimited := DelimSet <> EmptySet;

  NewTimer (ET, Timeout);

  If P^. Buffered Then {Wait until string is ready or Timeout}
    While Not BlockReady (P, ExpectedLen, DelimSet) And
          Not WaitComplete (P, ET)
      Do ;

  SaveStatus := AsyncStatus;
  If (AsyncStatus = ecOk) Or (Not P^. Buffered) Or
     FlagIsSet (P^. Flags, ptReturnPartialGets)
  Then
    If P^. Buffered Then {Need to try to get a string}
    Begin
      GetString (P, S, ExpectedLen, DelimSet); {String is ready, go get it}

      If SaveStatus <> ecOk Then {Conditionally restore WaitComplete's status}
        If (AsyncStatus = ecOk) Or
           ((AsyncStatus Mod 10000) = ecStringIncomplete) Or
           ((AsyncStatus Mod 10000) = ecBufferIsEmpty)
        Then
          GotError (P, epNonFatal + SaveStatus);
    End Else
    Begin
      Finished := False; {Need to Timeout each character in a non-buffered device}
      Cnt := 1;

      Repeat
        GetCharTimeout (P, C, Timeout); {Get a character (or error)}

        If (AsyncStatus <> ecOk) Then
        Begin
          If (AsyncStatus Mod 10000) = ecBufferIsEmpty Then
            GotError (P, epNonFatal + ecStringIncomplete);
          S [0] := Char (Cnt - 1);
          Exit;
        End Else
        Begin
          {Add the character}
          S [Cnt] := C;

          If Delimited And (C in DelimSet) Then {Check to see if we're finished}
            Finished := True;
          If (ExpectedLen <> 0) And (Cnt >= ExpectedLen) Then
            Finished := True;

          {Check for wrap}
          If Not Finished Then
          Begin
            Inc (Cnt);
            If Cnt = 0 Then
            Begin
              GotError (P, epNonFatal + ecStringOverrun);
              S [0] := #255;
              Exit;
            End;
          End;
        End;
      Until Finished;

      S [0] := Char (Cnt);

      {Optionally delete the delimiter character}
      If Delimited And Not FlagIsSet (P^. Flags, ptReturnDelimiter) Then
        S [0] := Char (Cnt - 1);
    End
  Else
    GotError (P, epNonFatal + (AsyncStatus Mod 10000)); {No string to get}
End;

Procedure PutCharTimeout (P: PortRecPtr; C: Char; Timeout: Word);
  {-Puts char in output buffer or Timeout}
Var
  ET : EventTimer;

Begin
  AsyncStatus := ecOk;

  With P^ Do
  Begin
    If TransReady (P) Then {If room is available now, just do it}
      PutChar (P, C)
    Else
    Begin
      NewTimer (ET, Timeout); {No room, start waiting}

      Repeat
      Until TransReady (P) Or WaitComplete (P, ET);

      If AsyncStatus = ecOk Then
        PutChar (P, C); {Now there is room, send the character}
    End;

    {Report timeouts or user aborts only}
    If (AsyncStatus = ecTimeout) Or (AsyncStatus = ecUserAbort) Then
      GotError (P, epNonFatal + AsyncStatus);
  End;
End;

Procedure PutStringTimeout (P: PortRecPtr; S: String; Timeout: Word);
  {-Puts string in output buffer or Timeout}
Var
  ET         : EventTimer;
  SaveStatus : Word;
  i          : Byte;

Begin
  AsyncStatus := ecOk;
  NewTimer (ET, Timeout);

  If P^. Buffered Then {Wait until there is enough room or Timeout}
    While (OutBuffFree (P) < Length (S)) And Not WaitComplete (P, ET)
      Do ;

  {Save the results of ptWaitComplete}
  SaveStatus := AsyncStatus;

  If (AsyncStatus = ecOk) Or Not P^. Buffered Or
     FlagIsSet (P^. Flags, ptExecutePartialPuts)
  Then
    If P^. Buffered Then {Need to try to put a string}
    Begin
      PutString (P, S); {Buffer has room, send the string}

      If SaveStatus <> ecOk Then {Conditionally restore ptWaitComplete's status}
        If (AsyncStatus = ecOk) Or ((AsyncStatus Mod 10000) = ecBufferIsFull)
        Then
          GotError (P, epNonFatal + SaveStatus);
    End
    Else
      For i := 1 To Length (S) Do {Device isn't buffered, need to Timeout each char}
      Begin
        PutCharTimeout (P, S [i], Timeout);
        If AsyncStatus <> ecOk Then
          Exit;
      End
  Else
    GotError (P, epNonFatal + (AsyncStatus Mod 10000)); {No room for string, return error}
End;

Procedure PutString (P: PortRecPtr; S: String);
  {-Puts string S in output buffer}
Var
  BytesWritten : Word;

Begin
  AsyncStatus := ecOk;

  If Not FlagIsSet (P^. Flags, ptExecutePartialPuts) Then
    If OutBuffFree (P) < Length (S) Then
    Begin
      GotError (P, epNonFatal + ecBufferIsFull);
      Exit;
    End;

{$IFDEF OS2}
  DosWrite (ComHandle, S [1], Length (S), i);
{$ELSE}
  BytesWritten := PutBlock (P, S [1], Length (S));
{$ENDIF}
End;

Procedure FlushInBuffer (P: PortRecPtr); {-Flush the input buffer}
Begin
  BufferFlush (P, True, False);
End;

Procedure FlushOutBuffer (P: PortRecPtr); {-Flush the output buffer}
Begin
  BufferFlush (P, False, True);
End;

Function InBuffUsed (P: PortRecPtr): Word;
  {-Return number of chars in input buffer}
Var
  InFree, OutFree, InUsed, OutUsed : Word;

Begin
  BufferStatus (P, InFree, OutFree, InUsed, OutUsed);
  InBuffUsed := InUsed;
End;

Function OutBuffUsed (P: PortRecPtr): Word;
  {-Return number of chars in output buffer}
Var
  InFree, OutFree, InUsed, OutUsed : Word;

Begin
  BufferStatus (P, InFree, OutFree, InUsed, OutUsed);
  OutBuffUsed := OutUsed;
End;

Function InBuffFree (P: PortRecPtr): Word;
  {-Return number of chars in input buffer}
Var
  InFree, OutFree, InUsed, OutUsed : Word;

Begin
  BufferStatus (P, InFree, OutFree, InUsed, OutUsed);
  InBuffFree := InFree;
End;

Function OutBuffFree (P: PortRecPtr): Word;
  {-Return number of chars in output buffer}
Var
  InFree, OutFree, InUsed, OutUsed : Word;

Begin
  BufferStatus (P, InFree, OutFree, InUsed, OutUsed);
  OutBuffFree := OutFree;
End;

Procedure DrainOutBuffer (P: PortRecPtr; Timeout: Word);
  {-Delays until output buffer drained or Timeout}
Var
  ET : EventTimer;

Begin
  AsyncStatus := ecOk;
  NewTimer (ET, Timeout);

  {Drain the output buffer}
  While (AsyncStatus = ecOk) And (OutBuffUsed (P) <> 0) And
        Not WaitComplete (P, ET)
    Do ;

  {If timer expired, set Timeout status}
  If ((AsyncStatus Mod 10000) = ecTimeout) Or
     ((AsyncStatus Mod 10000) = ecUserAbort)
  Then
    GotError (P, epNonFatal + (AsyncStatus Mod 10000));
End;

{$IFDEF UseSWFlow}
Procedure SWFlowEnable (P: PortRecPtr; BufferFull, BufferResume: Word);
  {-Enables automatic xon/xoff flow control}
Begin
  SWFlowSet (P, True, BufferFull, BufferResume, DefSWFOpt);
End;
{$ENDIF}

{$IFDEF UseHWFlow}
Procedure HWFlowEnable (P: PortRecPtr;  BufferFull, BufferResume: Word;
                        Options: Word);
  {-Enable hardware flow control (DTR and/or RTS)}
Begin
  HWFlowSet (P, True, BufferFull, BufferResume, Options);
End;
{$ENDIF}

Procedure SetDTR (P: PortRecPtr; State: Boolean); {-Raise/lower DTR}
Begin
  SetModem (P, State, (P^. ModemControl And RTSMask) = RTSMask);
End;

Procedure SetRTS (P: PortRecPtr; State: Boolean); {-Raise/lower CTS}
Begin
  SetModem (P, (P^. ModemControl And DTRMask) = DTRMask, State);
End;

{$IFNDEF OS2}
Function GetModemStatus (P: PortRecPtr): Byte;
Begin
  P^. ModemStatus := UpdateModemStatus (P);
  GetModemStatus := P^. ModemStatus;
End;
{$ENDIF}

Function CheckCTS (P: PortRecPtr): Boolean;
  {-Returns True if CTS is high}
{$IFDEF OS2}
Var
  Error         : Byte;
  InLen, OutLen : LongInt;

Begin
  Res := DosDevIOCtl (ComHandle, 1, IO_Lamps, nil, 0, @OutLen, @Error, 1,
    @InLen);
  CheckCTS := Odd (Error Shr 4);
End;
{$ELSE}
Begin
  CheckCTS := GetModemStatus (P) And CTSMask = CTSMask;
End;
{$ENDIF}

Function CheckDSR (P: PortRecPtr): Boolean;
  {-Returns True if DSR is high}
{$IFDEF OS2}
Var
  Error         : Byte;
  InLen, OutLen : LongInt;

Begin
  Res := DosDevIOCtl (ComHandle, 1, IO_Lamps, nil, 0, @OutLen, @Error, 1, @InLen);
  CheckDSR := Odd (Error Shr 5);
End;
{$ELSE}
Begin
  CheckDSR := GetModemStatus (P) And DSRMask = DSRMask;
End;
{$ENDIF}

Function CheckRI (P: PortRecPtr): Boolean;
  {-Returns True if RI is high}
{$IFDEF OS2}
Var
  Error         : Byte;
  InLen, OutLen : LongInt;

Begin
  Res := DosDevIOCtl (ComHandle, 1, IO_Lamps, nil, 0, @OutLen, @Error, 1, @InLen);
  CheckRI := Odd (Error Shr 6);
End;
{$ELSE}
Begin
  CheckRI := GetModemStatus (P) And RIMask = RIMask;
End;
{$ENDIF}

Function CheckDCD (P: PortRecPtr): Boolean;
  {-Returns True if DCD is high}
{$IFDEF OS2}
Var
  Error         : Byte;
  InLen, OutLen : LongInt;

Begin
  Res := DosDevIOCtl (ComHandle, 1, IO_Lamps, nil, 0, @OutLen, @Error, 1, @InLen);
  CheckDCD := (Odd (Error Shr 7) Or VirtualDCD);
End;
{$ELSE}
Begin
  CheckDCD := GetModemStatus (P) And DCDMask = DCDMask;
End;
{$ENDIF}

Function CheckDataReady (P: PortRecPtr): Boolean;
  {-Returns True if DR (Data Ready) is high}
Begin
  {Update line status field}
  P^. LineStatus := UpdateLineStatus (P);
  CheckDataReady := (P^. LineStatus And DataReadyMask) = DataReadyMask;
End;

Function GetComName (P: PortRecPtr): ComNameType;
  {-Returns the ComName of this port}
Begin
  GetComName := P^. PortName;
End;

Procedure SetErrorProc (P: PortRecPtr; EP: AsyncErrorProc);
  {-Sets an error handler for ComPort P}
Begin
  P^. ErrorProc := EP;
  P^. ErrorData := Pointer (P);
End;

Procedure SetAbortFunc (P: PortRecPtr; AFunc: AbortFunc);
  {-Sets AFunc as the user abort function}
Begin
  P^. UserAbort := AFunc;
End;

Procedure ptOptionsOn (P: PortRecPtr; OptionFlags: Word);
  {-Activate multiple options}
Begin
  With P^ Do
    Flags := Flags Or (OptionFlags And Not BadPortOptions);
End;

Procedure ptOptionsOff (P: PortRecPtr; OptionFlags: Word);
  {-Deactivate multiple options}
Begin
  With P^ Do
    Flags := Flags And Not (OptionFlags And Not BadPortOptions);
End;

Function ProtocolInProgress (P: PortRecPtr): Boolean;
  {-Returns True if this port is currently processing a protocol}
Begin
  ProtocolInProgress := P^. ProtocolActive;
End;

End.
