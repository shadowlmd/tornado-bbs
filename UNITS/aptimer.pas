{$IFDEF MSDOS}
  {$IFNDEF DPMI32}
    {$F+}
  {$ENDIF}
{$ENDIF}

{Conditional defines that may affect this unit}
{$I APDEFINE.INC}

{*********************************************************}
{*                    APTIMER.PAS 2.03                   *}
{*     Copyright (c) TurboPower Software 1991.           *}
{*                 All rights reserved.                  *}
{*********************************************************}

Unit ApTimer;
  {-BIOS timing functions}

Interface

Uses
{$IFDEF VirtualPascal}
  vpUtils;
{$ELSE}
  Dpmi;
{$ENDIF}

Const
  TicsPerDay = 1573040;      {Assumes 18.20648 tics/sec}
  SecsPerDay = 86400;        {Number of seconds in one day}

  {Clock frequency of 1193180/65536 is reduced to 1675/92. This}
  {allows longint conversions of tics values upto TicsPerDay}
  TicsFreq = 1675;
  SecsFreq = 92;

Type
  {For calculating timeouts}
  EventTimer = Record
                 StartTics  : LongInt;
                 ExpireTics : LongInt;
               End;

Function Tics2Secs (Tics: LongInt): LongInt;
Function Secs2Tics (Secs: LongInt): LongInt;
Procedure NewTimer (Var ET: EventTimer; Tics: LongInt);
Procedure NewTimerSecs (Var ET: EventTimer; Secs: LongInt);
Function TimerExpired (Const ET: EventTimer): Boolean;
Function ElapsedTime (Const ET: EventTimer): LongInt;
Function ElapsedTimeInSecs (Const ET: EventTimer): LongInt;
Function ElapsedTimeInMSecs (Const ET: EventTimer): LongInt;
Function RemainingTime (Const ET: EventTimer): LongInt;
Function RemainingTimeInSecs (Const ET: EventTimer): LongInt;
Function RemainingTimeInMSecs (Const ET: EventTimer): LongInt;
Procedure DelayTics (Tics: LongInt);

Implementation

{$IFDEF MSDOS}
Var
  BiosTics : ^LongInt; {Data area for BIOS tics value}
{$ENDIF}

Function Tics2Secs (Tics: LongInt): LongInt;
  {-Returns seconds value for Tics tics}
Begin
  Tics2Secs := ((Tics + 9) * SecsFreq) Div TicsFreq;
End;

Function Secs2Tics (Secs: LongInt): LongInt;
  {-Returns tics value for Secs seconds}
Begin
  Secs2Tics := (Secs * TicsFreq) Div SecsFreq;
End;

Procedure NewTimer (Var ET: EventTimer; Tics: LongInt);
  {-Returns a set EventTimer that will expire in Tics}
Begin
  {Max acceptable value is 24 hours}
  If Tics > TicsPerDay Then
    Tics := TicsPerDay;

  With ET Do
  Begin
  {$IFDEF MSDOS}
    StartTics := BiosTics^;
  {$ELSE}
    StartTics := GetTimemSec Div 55;
  {$ENDIF}
    ExpireTics := StartTics + Tics;
  End;
End;

Procedure NewTimerSecs (Var ET: EventTimer; Secs: LongInt);
  {-Returns a set EventTimer}
Begin
  NewTimer (ET, Secs2Tics (Secs));
End;

Function TimerExpired (Const ET: EventTimer): Boolean;
  {-Returns True if ET has expired}
Var
  CurTics : LongInt;

Begin
  With ET Do
  Begin
    {Get current tics; assume timer has expired}
  {$IFDEF MSDOS}
    CurTics := BiosTics^;
  {$ELSE}
    CurTics := GetTimemSec Div 55;
  {$ENDIF}

    {Check normal expiration}
    {Check wrapped CurTics}
    TimerExpired := (CurTics > ExpireTics) Or
      ((CurTics < StartTics) And (CurTics + TicsPerDay > ExpireTics));
  End;
End;

Function ElapsedTime (Const ET: EventTimer): LongInt;
  {-Returns elapsed time, in tics, for this timer}
Var
  CurTics: LongInt;

Begin
  With ET Do
  Begin
  {$IFDEF MSDOS}
    CurTics := BiosTics^;
  {$ELSE}
    CurTics := GetTimemSec Div 55;
  {$ENDIF}

    If CurTics >= StartTics Then
      {No midnight wrap yet}
      ElapsedTime := CurTics - StartTics
    Else
      {Got a midnight wrap, account for it}
      ElapsedTime := (TicsPerDay - StartTics) + CurTics;
  End;
End;

Function ElapsedTimeInSecs (Const ET: EventTimer): LongInt;
  {-Returns elapsed time, in seconds, for this timer}
Begin
  ElapsedTimeInSecs := Tics2Secs (ElapsedTime (ET));
End;

Function ElapsedTimeInMSecs (Const ET: EventTimer): LongInt;
  {-Returns elapsed time, in milliseconds, for this timer}
Begin
  ElapsedTimeInMSecs := ElapsedTime (ET) * 55;
End;

Function RemainingTime (Const ET: EventTimer): LongInt;
  {-Returns remaining time, in tics, for this timer}
Var
  CurTics       : LongInt;
  RemainingTics : LongInt;

Begin
  With ET Do Begin
  {$IFDEF MSDOS}
    CurTics := BiosTics^;
  {$ELSE}
    CurTics := GetTimemSec Div 55;
  {$ENDIF}

    If CurTics >= StartTics Then
      {No midnight wrap yet}
      RemainingTics := ExpireTics - CurTics
    Else
      {Got a midnight wrap, account for it}
      RemainingTics := (ExpireTics - TicsPerDay) - CurTics;
  End;

  If RemainingTics < 0 Then RemainingTime := 0
                       Else RemainingTime := RemainingTics;
End;

Function RemainingTimeInSecs (Const ET: EventTimer): LongInt;
  {-Returns remaining time, in seconds, for this timer}
Begin
  RemainingTimeInSecs := Tics2Secs (RemainingTime (ET));
End;

Function RemainingTimeInMSecs (Const ET: EventTimer): LongInt;
  {-Returns remaining time (in milliseconds) for this timer}
Begin
  RemainingTimeInMSecs := RemainingTime (ET) * 55;
End;

Procedure DelayTics (Tics: LongInt);
  {-Delay for Tics tics}
Var
  ET : EventTimer;

Begin
  If Tics > 0 Then
  Begin
    If Tics > TicsPerDay Then
      Tics := TicsPerDay;

    NewTimer (ET, Tics);
    Repeat
    Until TimerExpired (ET);
  End;
End;

{$IFDEF MSDOS}
Begin
  {$IFNDEF DPMI32}
    BiosTics := Ptr (BiosDataSele, $6C);
  {$ELSE}
    BiosTics := Ptr (Seg0040 + $6C);
  {$ENDIF}
{$ENDIF}
End.
