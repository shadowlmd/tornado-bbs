{$IFDEF MSDOS}
  {$IFNDEF DPMI32}
    {$F+}
  {$IFNDEF DPMI}
    {$DEFINE RealMode}
    {$O+}
  {$ENDIF}
  {$ENDIF}
{$ENDIF}
{$I-}
{&Delphi+}

Unit Shell;

{*********************************************************}
{*                       SHELL.PAS                       *}
{*                                                       *}
{*  Copyright (c) Alex Radzishevskiy, 1995-96,           *}
{*   Portions (c) Konstantin Klyagin, 1996-98,           *}
{*                exspecially for Tornado BBS System     *}
{*                                                       *}
{*********************************************************}

Interface

Procedure InitShellUnit;

Implementation

Uses
{$IFDEF RealMode}
  ExecSwap,
{$ENDIF}

{$IFDEF WIN32}
  ApWin32,
{$ENDIF}

{$IFDEF OS2}
  Os2PmApi,
  Os2Def,
  Os2Base,
  VPUtils,
{$ENDIF}

  ApSame,
  DOS,
  tWin,
  tMisc,
  Log,
  OpCrt,
  Protocol,
  SysMsgs,
  TGlob,
  Doors;

Function fDosShell (Prg: String; CommandCom, LogErrorLevel: Boolean): Word;
{$IFDEF RealMode}
Var
  SwapLoc : Array [Boolean] Of String [20];
{$ENDIF}

  Procedure ExecPrepare;
  Begin
    {$IFDEF RealMode}
    WriteLn ('þ ' + sm (smSwapReserved), BytesSwapped, sm (smBytes), ' ',
      SwapLoc [EmsAllocated]);
    {$ENDIF}

    WriteLn ('þ ' + sm (smGenInfo));
    WriteLn;

    If Not Registering Then
    Begin
      DorinfoGen;
      DoorSysGen;
      ExitInfoGen;
    End;
  End;

Var
  i                           : Integer;
  Status                      : Word;
  Covers                      : Pointer;
  oxx, oxy, Attr              : Byte;
  ExeExt                      : String [12];
  ExeName, Parameters, CurDir : String;
  CloseCom                    : Boolean;

Begin
  CloseCom := Prg[1] = '!';
  If CloseCom Then
    Delete(Prg, 1, 1);
  ExeName := ExtractAscii (1, Prg, SpaceOnly, '"');
  ExeExt := UpString (JustExtension (ExeName));
  CommandCom := CommandCom Or ((ExeExt <> 'EXE') And (ExeExt <> 'COM'));

  If CommandCom Then
  Begin
    If Prg <> '' Then
      Prg := '/c ' + Prg;
  End Else
  Begin
    i := AsciiPosition (2, Prg, SpaceOnly, '"');
    If i > 0 Then Parameters := Copy (Prg, i, 255)
             Else Parameters := '';
  End;

  Attr := TextAttr;
  TextAttr := $07;
  oxx := WhereX;
  oxy := WhereY;
  Status := 0;

  SaveWindow (1, 1, ScrX + 1, ScrY + 1, True, Covers);
  Window (1, 1, ScrX + 1, ScrY + 1);
  ClrScr;
  GetDir (0, CurDir);

  {$IFDEF RealMode}
  SwapLoc [True]  := sm (smSwapEMS);
  SwapLoc [False] := sm (smSwapDisk);
  UseEmsIfAvailable := False;

  If Not InitExecSwap (HeapPtr, Cnf. TempDir + DelChars ([':'], StrTime) +
     'sw' + LineExt) Then
  Begin
    WriteLn ('þ ' + sm (smSwapError));
    LogWrite ('!', sm (smSwapError));
    If Cnf. Sound Then
      SoundOf ('1 200 1 2 800 1000 -1 2 800');
  End Else
  {$ENDIF}
  Begin
    ExecPrepare;

    If Not Local And CloseCom Then
      DeactivatePort (Port, True);

    {$IFDEF MSDOS}
    SwapVectors;
    {$ENDIF}

    If CommandCom Then
      {$IFDEF RealMode}
      ExecWithSwap (GetEnv ('COMSPEC'), Prg)
      {$ELSE}
      Exec (GetEnv ('COMSPEC'), Prg)
      {$ENDIF}
    Else
      {$IFDEF RealMode}
      ExecWithSwap (ExeName, Parameters);
      {$ELSE}
      Exec (ExeName, Parameters);
      {$ENDIF}

    Status := DosExitCode;

    {$IFDEF MSDOS}
    SwapVectors;
    {$ENDIF}

    {$IFDEF RealMode}
    ShutdownExecSwap;
    {$ENDIF}

    If Not Local And CloseCom Then
    Begin
      ActivatePort (Port, True);
      If AsyncStatus <> ecOk Then
      Begin
        LogWrite ('!', sm (smlInitError) + ': ' + Status2Str (
          AsyncStatus mod 10000));
        NormExit;
      End;
    End;
  End;

  fDosShell := Status;

  SmartChDir (CurDir);

  If Not CommandCom And LogErrorLevel Then
    LogWrite (':', sm (smErrorLevel) + Long2Str (Status));

  ReInitCrt;
  TextAttr := 0;
  ClrScr;
  RestoreWindow (1, 1, ScrX + 1, ScrY + 1, True, Covers);
  TWinInit;
  If StatusBar Then
    Window (1, 1, ScrX + 1, ScrY - 1);
  SetBlink (Cnf. Blinking);
  GotoXY (oxx, oxy);
  TextAttr := Attr;
End;

Function fTranslateExecParams (Const S: String): String;
Var
  i      : Integer;
  S1     : String;
{$IFNDEF VirtualPascal}
  Result : String;
{$ENDIF}

Begin
  If Not Local Then
  Begin
  {$IFNDEF OS2}
    S1 := Long2Str (Cnf. ComPort);
  {$ELSE}
    If Copy (Cnf. ComPort, 1, 3) = 'COM' Then
      S1 := Trim (Copy (Cnf. ComPort, 4, 255))
    Else
      S1 := '0';
  {$ENDIF}
  End
  Else
    S1 := '0';

  Result := PlaceSubStr (S, '*P', S1);

{$IFDEF OS2}
  If Not Local Then
    S1 := Long2Str (ApOS2. ComHandle)
  Else
    S1 := '0';
  PlaceSubStrP (Result, '*H', S1);
{$ENDIF}

{$IFDEF WIN32}
  If Not Local Then
    S1 := Long2Str (ApWin32. ComHandle)
  Else
    S1 := '0';
  PlaceSubStrP (Result, '*H', S1);
{$ENDIF}

  PlaceSubStrP (Result, '*N', Long2Str (BbsLine));
  PlaceSubStrP (Result, '*B', Long2Str (GetConnectSpeed));
  PlaceSubStrP (Result, '*G', ZeroOne [R. Emu in [teAnsi, teAvatar]]);
  PlaceSubStrP (Result, '*F', ExtractWord (1, R. Name, SpaceOnly));

  i := WordCount (R. Name, SpaceOnly);
  If i > 1 Then
    S1 := ExtractWord (i, R. Name, SpaceOnly)
  Else
    S1 := '';

  PlaceSubStrP (Result, '*L', S1);
{$IFNDEF VirtualPascal}
  fTranslateExecParams := PlaceSubStr
{$ELSE}
  PlaceSubStrP
{$ENDIF}
  (Result, '*T', Long2Str (Round ((R. TotalTime - TimeDiff (EnterTime, MidSec))
    / 60)));
End;

Procedure InitShellUnit;
Begin
  DosShell := fDosShell;
  TranslateExecParams := fTranslateExecParams;
End;

End.
