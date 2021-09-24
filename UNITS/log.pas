{$IFDEF MSDOS}
  {$IFNDEF DPMI32}
    {$F+}
  {$ENDIF}
{$ENDIF}
{$I-}

Unit Log;

{*********************************************************}
{*                      LOG.PAS                          *}
{*                                                       *}
{*  Copyright (c) Konstantin Klyagin, 1995-98,           *}
{*                exspecially for Tornado BBS System     *}
{*                                                       *}
{*********************************************************}

Interface

Procedure LogOpen (Const Name, Events, NewStr: String);
Procedure LogWrite (Symb: Char; Const Event: String);
Procedure LogClose;

Implementation

Uses
  tGlob,
  tMisc;

Var
  LastCall  : LongInt;
  Logging   : Boolean;
  LogName   : String [80];
  LogEvents : String [20];
  LogNewStr : String;

Procedure LogOpen (Const Name, Events, NewStr: String);
Var
  IOR  : Integer;
  fLog : Text;

Begin
  Logging := False;
  If Name = '' Then
    Exit;

  Assign (fLog, Name);
  Append (fLog);
  IOR := IOResult;

  If IOR <> 0 Then
  Begin
    ReWrite (fLog);
    IOR := IOResult;

    If IOR = 0 Then
      WriteLn (fLog, '# ' + FormattedCurrDT ('DD-NNN-YYYY HH:II:SS') + NewStr);
  End;

  If IOR <> 0 Then
  Begin
    ExitProc := SavedExitProc;
    WriteLn ('! Log file open/create error!');
    WriteLn ('!          IO error code: ' + Long2Str (IOR));
    WriteLn;
    Halt (208);
  End;

  WriteLn (fLog);
  WriteLn (fLog, '컴컴컴컴컴컴컴컴� ' + FormattedCurrDT ('DD-NNN-YYYY, ') +
    NameVer);
  Close (fLog);

  LastCall := MidSec;
  LogName := Name;
  LogNewStr := NewStr;
  LogEvents := Events;
  Logging := True;
End;

Procedure LogWrite (Symb: Char; Const Event: String);
Var
  tMS  : LongInt;
  fLog : Text;

Begin
  If Logging Then
  Begin
    tMS := MidSec;
    If (LastCall > tMS) And (TimeDiff (LastCall, tMS) > 0) Then
    Begin
      LogOpen (LogName, LogEvents, LogNewStr);
      If Not Logging Then
        Exit;
    End;
    LastCall := tMS;

    If Pos (Symb, LogEvents) <> 0 Then
    Begin
      Assign (fLog, LogName);
      Append (fLog);

      If IOResult <> 0 Then
      Begin
        ReWrite (fLog);
        If IOResult <> 0 Then
          Exit;
      End;

      WriteLn (fLog, Symb + ' ' + StrTime + ' ' + DelChars ([#13, #10], Event));
      Close (fLog);
    End;
  End;
End;

Procedure LogClose;
Begin
  Logging := False;
End;

End.
