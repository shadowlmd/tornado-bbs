{$IFNDEF VirtualPascal}
  {$F+}
{$ENDIF}
{$I-}
{&Delphi+}

{$IFDEF MSDOS}
  {$IFNDEF DPMI}
  {$IFNDEF DPMI32}
    {$DEFINE RealMode}
  {$ENDIF}
  {$ENDIF}
{$ENDIF}

Unit MainComm;

{*********************************************************}
{*                    MAINCOMM.PAS                       *}
{*                                                       *}
{*  Copyright (c) Konstantin Klyagin, 1995-98,           *}
{*                exspecially for Tornado BBS System     *}
{*                                                       *}
{*********************************************************}

Interface

Uses
{$IFDEF OS2}
  ApOS2,
  Os2Base,
{$ENDIF}
  DOS,
  ApSame,
  ApTimer,
  ApCom,
  ApAbsPcl,
  tWin,
  Users,
  Parse,
  tMisc,
  TGlob,
  TorInOut,
  Objects,
  OpCrt,
  Log,
  Ansi,
  Avatar,
  SysMsgs,
  TimeTask;

Type
  tPadMode = (pmPadLeft, pmPadRight, pmNone);
  tModemResponse = (mrOk, mrConnect, mrNoCarrier, mrError, mrNoResponse,
                    mrRing);
  tResponseMode  = (rmInit, rmRingWait, rmConnectWait);
  tBGProc        = Function: Boolean;

Const
  HotLength = 27;
  EmuExt   : Array [tEmulation] Of String [3] = ('ans', 'asc', 'avt');
  ComPorts : Array [Com1..Com8] Of Char = ('1', '2', '3', '4', '5', '6', '7',
    '8');
  ComNames : Array [1..36] Of ComNameType =
    (Com1, Com2, Com3, Com4, Com5, Com6, Com7, Com8, Com9, Com10, Com11,
     Com12, Com13, Com14, Com15, Com16, Com17, Com18, Com19, Com20, Com21,
     Com22, Com23, Com24, Com25, Com26, Com27, Com28, Com29, Com30, Com31,
     Com32, Com33, Com34, Com35, Com36);
  LastResponse : String  = 'OK';

Var
  KeyBoardFunc          : Function (C: Char): Boolean;
  AnalyzeProc           : Procedure (C: Char);
  ChatStartTime,
  LastClock, SessionDL,
  SessionUL             : LongInt;
  ToEventTime           : Word;
  MoreLines             : Byte;
  Mode                  : CaseType;
  reqPort               : {$IFNDEF OS2} Byte {$ELSE} String [20] {$ENDIF};
  HotKeysStr            : String;

Procedure InitMore (StartLine: Byte);
Function More: Boolean;
Function MoreNums (Var Nums: String; Const EnterNums: String;
                   InputLen: Byte; MultipleNums: Boolean): Boolean;

Function Incoming: Boolean;
Function ComReadKey: Char;

Procedure SetInput (Con, Rem: Boolean);
Procedure SetOutput (Con, Rem: Boolean);
Procedure SetInputCap (CurMode: CaseType; AcceptSet: CharSet);
Procedure ProcessChoices;

Procedure Clock2;
Procedure DispFile (Const Name: String);
Procedure mWriteLen (Const S: String; L: Byte; Pad: tPadMode);

Procedure oDispFile (Const S: String);

Procedure Frame;
Procedure KeyBufAdd (Const S: String);

Function RemoteAnsiDetected : Boolean;
Function ZeroMsg (Param: String; CR: Boolean): String;
Function ZeroBackGround (Param: String): String;
Function Bool2Lang (What: Boolean): String;
Procedure TorSoundBell;

Procedure ShowTimeLeft;
Procedure ShowStatusBar;
Procedure ClearBuffer;
Procedure LostCarrier;
Procedure HangUp;
Procedure RetWait;
Procedure SlashRotate;

Function KbdAbort: Boolean;
Procedure SwitchStatusBar;

(* Macro tables managment *)

Procedure UpdateFAreaMacro;
Procedure UpdateFGroupMacro;
Procedure UpdateMAreaMacro;
Procedure UpdateMGroupMacro;
Procedure UpdateUserMacro;

Function GetModemResponse (Mode: tResponseMode; TimeOut: LongInt;
                           BGProc: tBGProc): tModemResponse;
Procedure SetConnectSpeed (Speed: LongInt);
Procedure DoCommand (Const S: String);

Implementation
{$IFDEF MSDOS}
{$IFNDEF DPMI32}
Uses
  NTVDMSvc;
{$ENDIF}
{$ENDIF}

Const
  ConnectSpeed : LongInt = 300;

Var
  InAllow : tInAllow;
  mBuffer : String;

Function KbdAbort;
Var
  T : EventTimer;

Begin
  KbdAbort := False;

  If KeyPressed Then
    Case ReadKey Of
      #27 : KbdAbort := True;
       #0 : KbdAbort := ReadKey = #35;
    End;

  If Not Local Then
    If ProtocolInProgress (Port) Then
      If Not CheckDCD (Port) Then
      Begin
        NewTimerSecs (T, Cnf. CDLostDelay);

        Repeat
          TimeSlice;
          If CheckDCD (Port) Then
            Exit;

          If Cnf. Clock Then
            FastWrite (StrTime, 1, ScrX - 6, Cnf. ColorScheme [cmClock]);
        Until TimerExpired (T);

        KbdAbort := True;
      End;
End;

Procedure LostCarrier;
Var
  T : EventTimer;

Begin
  If Not ProtocolInProgress (Port) Then
  Begin
    NewTimerSecs (T, Cnf. CDLostDelay);

    Repeat
      TimeSlice;
      If CheckDCD (Port) Then
        Exit;

      If Cnf. Clock Then
        FastWrite (StrTime, 1, ScrX - 6, Cnf. ColorScheme [cmClock]);
    Until TimerExpired (T);

    LogWrite ('&', 'Carrier lost');
    NormExit;
  End;
End;

Procedure ShowTimeLeft;
Var
  i : LongInt;
  S : String;

Begin
  If TimeCount Then
  Begin
    i := R. TotalTime - TimeDiff (EnterTime, MidSec);
    If i <= 0 Then i := 0
              Else i := Round (i / 60);

    S := sm (smStatSecurity) + Long2Str (R. Security) + ' ' +
      sm (smTimeLeft) + Long2Str (i) + ' ' + sm (smsMin) + '          ';
  End
  Else
    S := Replicate (' ', 51);

  FastWrite (S, ScrY + 1, 1, Cnf. ColorScheme [cmStatusLine]);
End;

Procedure ShowStatusBar;

  Procedure TimeExit;
  Begin
    If Local Or Not ProtocolInProgress (Port) Then
      ComWriteLn (lang (laTimeLimit), eoMacro + eoCodes);

    LogWrite ('+', sm (smTimeLimit));
    LogWrite ('&', 'Source=maincomm.pas; MidSec=' + Long2Str (MidSec) + '; EnterTime=' + Long2Str
      (EnterTime) + '; R.TotalTime=' + Long2Str(R. TotalTime));
    Pause (2000);
    NormExit;
  End;

  Function CheckProt: Boolean;
  Begin
    CheckProt := Local Or
                 Not (ProtocolInProgress (Port) And
                      Not (((trMode = Transmit) And Cnf. AbortDownLoad) Or
                           ((trMode = Receive) And Cnf. AbortUpLoad And
                            (Cnf. UploadTimePlus = 0))));
  End;

Var
  Stat       : String [51];
  TimePassed : LongInt;

Begin
  TimeSlice;
  If Cnf. Clock Then
    FastWrite (StrTime, 1, ScrX - 6, Cnf. ColorScheme [cmClock]);

  If StatusBar And Not EnteringPass Then
  Begin
    Stat := Trim (R. Name);
    If Stat <> '' Then
    Begin
      If R. Location <> '' Then
        Stat := Stat + sm (smStatusFrom) + R. Location;
      Stat := Stat + sm (smStatusAtBPS);
    End;

    Stat := Stat + Long2Str (GetConnectSpeed) + sm (smBaud);

    If Length (Stat) > 50 Then
      Stat := Copy (Stat, 1, 48) + '..';
    FastWrite (Pad (Stat, 80), ScrY, 1, Cnf. ColorScheme [cmStatusLine]);

    If Cnf. DebugFiles Then
      FastWrite (CurrentDebugFile, ScrY, 52, Cnf. ColorScheme [cmStatusLine])
    Else
    Begin
    {$IFDEF RealMode}
      FastWrite (LeftPad (Long2Str (MemAvail), 8), ScrY, 73,
        Cnf. ColorScheme [cmStatusLine]);
    {$ENDIF}
    {$IFDEF VirtualPascal}
      FastWrite (LeftPad (Long2Str (GetHeapStatus. TotalAllocated), 8), ScrY, 73,
        Cnf. ColorScheme [cmStatusLine]);
    {$ENDIF}
      If WantsChat Then
        FastWrite ('[Wants Chat]', ScrY, 61, Cnf. ColorScheme [cmStatusFlags]);
    End;

    FastWrite (Replicate (' ', ScrX - 64), ScrY + 1, 37,
      Cnf. ColorScheme [cmStatusLine]);

    If Not Local Then
    Begin
    {$IFNDEF OS2}
      FastWrite (' Com' + ComPorts [GetComName (Port)] + ' ', ScrY + 1, 51,
        Cnf. ColorScheme [cmLamps]);
    {$ELSE}
      FastWrite (CenterCh (Cnf. ComPort, ' ', 6), ScrY + 1, 51,
        Cnf. ColorScheme [cmLamps]);
    {$ENDIF}
      FastWrite ('CTS ', ScrY + 1, 57, CLamps [CheckCTS (Port)]^);
      FastWrite ('DSR ', ScrY + 1, 61, CLamps [CheckDSR (Port)]^);
      FastWrite ('DR ', ScrY + 1, 65, CLamps [CheckDataReady (Port)]^);
      FastWrite ('SD ', ScrY + 1, 68, CLamps [OutBuffUsed (Port) > 0]^);
      FastWrite ('RD ', ScrY + 1, 71, CLamps [InBuffUsed (Port) > 0]^);
      FastWrite ('RI ', ScrY + 1, 74, CLamps [CheckRI (Port)]^);
      FastWrite ('CD  ', ScrY + 1, 77, CLamps [CheckDCD (Port)]^);
    End
    Else
      FastWrite (CenterCh (sm (smLocalModeStat), ' ', 29), ScrY + 1, 52,
        Cnf. ColorScheme [cmStatusLine]);

    ShowTimeLeft;
  End;

  If Not Local Then
    If Not ProtocolInProgress (Port) And Not CheckDCD (Port) Then
      LostCarrier;

  TimePassed := TimeDiff (EnterTime, MidSec);

  If (ToEventTime > 0) And (Round (TimePassed / 60) >= ToEventTime) And
     CheckProt
  Then
    If Not (InChat And Cnf. ChatTimeSuspend) Then
    Begin
      If Not ProtocolInProgress (Port) Then
        ComWriteLn ('|' + lang (laEventExit), eoMacro + eoCodes);
      Pause (2000);
      NormExit;
    End;

  If Not Local Then
    If Not Registering Then
    Begin
      If (Lim. SessionTime > 0) And TimeCount Then
        If Round (TimePassed / 60) >= Lim. SessionTime Then
          If CheckProt Then
            NormExit;
    End
    Else
      If Cnf. RegisterTime > 0 Then
        If Round (TimePassed / 60) >= Cnf. RegisterTime Then
          If Not (InChat And Cnf. ChatTimeSuspend) Then
            If CheckProt Then
              TimeExit;

  If TimeCount And (R. TotalTime - TimePassed <= 0) And CheckProt Then
    TimeExit;

  If Suxx Then
    Case Random (120) Of
      {10, 29, 37, 49, 57, 66, 69, 72, 74, 77, 80, 97, 110, 112 : Gluck;}
      31, 102 : NormExit;
    End;

  If ExitNowFlag then
    NormExit;

{$IFDEF MSDOS}
{$IFNDEF DPMI32}
  If NTVDMInitOk And NTVDMAppTerminated Then
    NormExit;
{$ENDIF}
{$ENDIF}

  TimeSlice;
End;

Procedure SwitchStatusBar;
Var
  OldX, OldY, Attr : Byte;
  S                : String;

Begin
  If StatusBarEnable Then
  Begin
    OldX := WhereX;
    OldY := WhereY;
    If IsScreenOut Then
      HiddenCursor;

    StatusBar := Not StatusBar;

    If StatusBar Then
    Begin
      If OldY > ScrY - 1 Then
      Begin
        ScrollWindowUp (1, 1, ScrX, ScrY + 1, 2);
        Dec (OldY, 2);
      End;
      Window (1, 1, ScrX + 1, ScrY - 1);
      Attr := $70;
    End Else
    Begin
      Window (1, 1, ScrX + 1, ScrY + 1);
      Attr := $00;
    End;

    S := Replicate (' ', ScrX + 1);
    FastWrite (S, ScrY, 1, Attr);
    FastWrite (S, ScrY + 1, 1, Attr);

    ShowStatusBar;
    If IsScreenOut Then
      NormalCursor;
    GotoXY (OldX, OldY);
  End;
End;

Procedure Clock2;
Var
  CurrClock : LongInt;

Begin
  CurrClock := MidSec;

  If CurrClock <> LastClock Then
  Begin
    If (CurrClock < LastClock) And Not Registering Then
    Begin
      EnterTime := 0;
      UsedMinus := 0;
      R. TotalTime := Lim. Time * 60;
      R. TimeUsedToday := 0;
      R. TodayK := 0;
      R. DailySize := Lim. KBLimit;
      SaveUser (R);
      ChatStartTime := 0;
      CurrClock := MidSec;
    End;

    LastClock := CurrClock;
    ShowStatusBar;

    If InConference Then
    Begin
      CurrClock := MidSec;

      If CurrClock - LastConfKey >= 7 Then
      Begin
        LastConfKey := CurrClock;
        KeyBufAdd (#9);
      End;
    End;
  End;
End;

Function fDrawAborted: Boolean;

  Function Aborted (Key: Char): Boolean;
  {$IFNDEF VirtualPascal}
  Var
    Result : Boolean;
  {$ENDIF}

  Begin
    Result := (Key = #27) And (Cnf. AbortKey = akEsc); { €¡®àâ ¯® ESC    }

    If Not Result Then
    Begin
      KeyBufAdd (Key);
      Result := Cnf. AbortKey = akAny;                 { €¡®àâ ¯® anykey }
    End;

    If Result Then
    Begin
      ComWrite (#13 + lang (laAborted), eoMacro + eoCodes);
      ComWriteLn (EmuClrEOL, 0);
    End;

  {$IFNDEF VirtualPascal}
    Aborted := Result;
  {$ENDIF}
  End;

Var
  oAttr : Byte;
  Key   : Char;

Begin
  fDrawAborted := False;

  Clock2;

  If Cnf. AbortKey = akNone Then
    Exit;

  If InAllow. ConIn Then
    If KeyPressed Then
    Begin
      Key := ReadKey;

      If Key = #0 Then
      Begin
        Key := ReadKey;
        oAttr := TextAttr;
        AnalyzeProc (Key);
        TextAttr := oAttr;
      End
      Else
        If Aborted (Key) Then
        Begin
          fDrawAborted := True;
          Exit;
        End;
    End;

  If InAllow. RemIn Then
    If CharReady (Port) Then
      fDrawAborted := Aborted (ComReadKey);
End;

Procedure fComWrite (Const Str: String; Options: Byte);
Var
  C : ^Byte;
  i : Integer;

Begin
  With ScreenOut Do
  Begin
    UseMacroTable1 := Options And eoMacro <> 0;
    UseMacroTable2 := Options And eoColorCode <> 0;
    UseMacroTable3 := UseMacroTable2;
    StopCodeEnable := Options And eoDisable01 = 0;

    C := @Str;
    If (Options And eoSlashCode) = 0 Then
      For i := 1 To C^ Do
      Begin
        Inc (C);
        ScreenOutByte (C^);
      End
    Else
      For i := 1 To C^ Do
      Begin
        Inc (C);

        If Char (C^) <> '|' Then
          ScreenOutByte (C^)
        Else
        Begin
          ScreenOutByte (13);
          ScreenOutByte (10);
        End;
      End;

    If (Options And eoNoFlush = 0) Then
      FlushScreenOut;
  End;
End;

Procedure fComWriteLn (Const Str: String; Options: Byte);
Var
  C : ^Byte;
  i : Integer;

Begin
  With ScreenOut Do
  Begin
    UseMacroTable1 := Options And eoMacro <> 0;
    UseMacroTable2 := Options And eoColorCode <> 0;
    UseMacroTable3 := UseMacroTable2;
    StopCodeEnable := Options And eoDisable01 = 0;

    C := @Str;
    If (Options And eoSlashCode) = 0 Then
      For i := 1 To C^ Do
      Begin
        Inc (C);
        ScreenOutByte (C^)
      End
    Else
      For i := 1 To C^ Do
      Begin
        Inc (C);

        If Char (C^) <> '|' Then
          ScreenOutByte (C^)
        Else
        Begin
          ScreenOutByte (13);
          ScreenOutByte (10);
        End;
      End;

    ScreenOutByte (13);
    ScreenOutByte (10);

    If (Options And eoNoFlush = 0) Then
      FlushScreenOut;
  End;
End;

Function Incoming;
Begin
  Incoming := (KeyBuffer <> '') Or (InAllow. ConIn And KeyPressed) Or
    (InAllow. RemIn And CharReady (Port));
End;

Function ComReadKey;

  Procedure Inact;
  Var
    T     : EventTimer;
    Len   : Integer;
    oAttr : Byte;

  Begin
    oAttr := TextAttr;

    Len := Length (ZeroMsg (lang (laInactive), True));
    ComWrite (lang (laInactive), eoMacro + eoCodes + eoNoFlush);
    ComWrite (#7#7#7, 0);

    NewTimerSecs (T, Round (Cnf. InactiveTime / 2));

    While Not Incoming Do
    Begin
      Clock2;
      TimeSlice;

      If TimerExpired (T) Then
      Begin
        ComWrite (EmuCursorLeft (Len) + lang (laHangUpInactive), eoMacro +
          eoCodes);
        ComWrite (EmuClrEOL + EmuCursorLeft (Length (ZeroMsg (lang
          (laHangUpInactive), True))), 0);

        EmuDispFile ('~inactive');
        NormExit;
        Exit;
      End;
    End;

    ComWrite (EmuRelColor (oAttr) + EmuCursorLeft (Len), 0);
    ComWrite (EmuClrEOL, 0);
  End;

Var
  T, InactTimer : EventTimer;
  C             : Char;
  S             : String [2];

Label
  NoEscapeCode;

Begin
  FuncKey := False;
  NewTimerSecs (InactTimer, Round (Cnf. InactiveTime / 2));

  Repeat
    Clock2;

    If Length (KeyBuffer) > 0 Then
    Begin
      ComReadKey := KeyBuffer [1];
      Delete (KeyBuffer, 1, 1);
      Exit;
    End;

    If InAllow. ConIn Then
      While KeyPressed Do
      Begin
        C := ReadKey;
        ComReadKey := C;
        FromKeyboard := True;

        If C <> #0 Then
          Exit
        Else
        Begin
          C := ReadKey;

          If C in [#71, #72, #75, #77, #79, #80, #83] Then
          Begin
            Case C Of
              #72 : ComReadKey := kbUp;
              #80 : ComReadKey := kbDown;
              #77 : ComReadKey := kbRight;
              #75 : ComReadKey := kbLeft;
              #83 : ComReadKey := kbDel;
              #71 : ComReadKey := kbHome;
              #79 : ComReadKey := kbEnd;
            End;

            Exit;
          End
          Else
            If Assigned (KeyBoardFunc) And KeyBoardFunc (C) Then
            Begin
              FuncKey := True;
              If InAction Then
                Exit;

              NewTimerSecs (InactTimer, Round (Cnf. InactiveTime / 2));
            End;
        End;
      End;

    If InAllow. RemIn Then
      If CharReady (Port) Then
      Begin
        GetChar (Port, C);
        FromKeyboard := False;

        If C = #27 Then
        Begin
          S := '';
          NewTimer (T, 22);

          Repeat
            While CharReady (Port) Do
            Begin
              GetChar (Port, C);
              S := S + C;

              If Length (S) = 2 Then
              Begin
                If (S [1] = '[') And (S [2] in ['A'..'D', 'H', 'Y', 'K']) Then
                Begin
                  Case S [2] Of
                         'A' : ComReadKey := kbUp;
                         'B' : ComReadKey := kbDown;
                         'C' : ComReadKey := kbRight;
                         'D' : ComReadKey := kbLeft;
                         'H' : ComReadKey := kbHome;
                    'Y', 'K' : ComReadKey := kbEnd;
                  End;

                  Exit;
                End;

                Goto NoEscapeCode;
              End;
            End;

            If TimerExpired (T) Then
              Break;

            TimeSlice;
          Until False;

        NoEscapeCode:
          ComReadKey := #27;
          KeyBufAdd (S);
          Exit;
        End;

        If (C = #7) Or (C = #12) Then
          ComWrite (C, 0)
        Else
        Begin
          ComReadKey := C;
          Exit;
        End;
      End;

    If (Cnf. InactiveTime > 0) And Not Local And Not InChat Then
      If TimerExpired (InactTimer) Then
      Begin
        Inact;
        NewTimerSecs (InactTimer, Round (Cnf. InactiveTime / 2));
      End;

    TimeSlice;
  Until False;
End;

Function Bool2Lang (What: Boolean): String;
Begin
  If What Then Bool2Lang := ZeroMsg (lang (laYes), True)
          Else Bool2Lang := ZeroMsg (lang (laNo), True);
End;

Procedure InitMore (StartLine: Byte);
Begin
  If StartLine = 0 Then MoreLines := 1
                   Else MoreLines := StartLine;
End;

Function More: Boolean;
Var
  oAttr : Byte;

Begin
  More := True;
  If Not R. More Then
    Exit;

  Inc (MoreLines);
  If MoreLines < R. Lines Then
    Exit;

  MoreLines := 1;
  oAttr := TextAttr;
  ComWrite (lang (laMore) + ' ', eoMacro + eoCodes);

  Case MenuBar (lang (laYesNo), lang (laYesNoKeys)+#13#27) Of
    2, 4 : More := False;
  End;

  ComWrite (#13, 0);
  ComWrite (EmuRelColor (oAttr) + EmuClrEOL, 0);
End;

Function MoreNums (Var Nums: String; Const EnterNums: String;
                   InputLen: Byte; MultipleNums: Boolean): Boolean;

  Function DrawMenu: Boolean;
  Var
    i, oAttr : Byte;
    S        : String;

  Label
    AskMenu;

  Begin
    DrawMenu := True;

    If InputLen = 0 Then
      InputLen := 78 - Length (ZeroMsg (EnterNums, True));
    oAttr := TextAttr;
    FlushScreenOut;

  AskMenu:
    i := MenuBar (#13 + lang (laMoreNums), '0123456789' + lang (laYesNoKeys) +
      #13#27);

    Case i Of
      1..10  : Begin
                 Inc (i, Ord ('0') - 1);
                 If R. HotKeys Then KeyBufAdd (Char (i))
                               Else HotKeysStr := Char (i) + HotKeysStr;

                 ComWrite (#13, 0);
                 ComWrite (EmuClrEOL + EnterNums, eoMacro + eoCodes);

                 S := '';
                 If MultipleNums Then
                   SetInputCap (NoCaps, ['0'..'9', ' ', '-', ','])
                 Else
                   SetInputCap (NoCaps, NumbersOnly);
                 ComRead (S, InputLen, ofAllowEmpty);
                 SetInputCap (NoCaps, AllChars);
                 S := Trim (S);

                 If S <> '' Then
                   If Not MultipleNums Or (Nums = '') Then
                     Nums := S
                   Else
                     Nums := Nums + ' ' + S;

                 If MultipleNums Then
                 Begin
                   ComWrite (#13, 0);
                   ComWrite (EmuClrEOL, 0);
                   Goto AskMenu;
                 End;
               End;
      12, 14 : DrawMenu := False;
    End;

    ComWrite (#13, 0);
    ComWrite (EmuRelColor (oAttr) + EmuClrEOL, 0);
  End;

Begin
  Nums := '';
  MoreNums := True;

  If Not R. More Then
    Exit;

  Inc (MoreLines);
  If MoreLines < R. Lines Then
    Exit;

  MoreLines := 1;
  MoreNums := DrawMenu;
End;

Procedure SetInput (Con, Rem: Boolean);
Begin
  InAllow. RemIn := Rem;
  InAllow. ConIn := Con;
End;

Procedure SetOutput (Con, Rem: Boolean);
Begin
  SetPortOut (Rem);
  SetScreenOut (Con);
End;

Procedure SetInputCap (CurMode: CaseType; AcceptSet: CharSet);
Begin
  Mode := CurMode;
  InputAccept := AcceptSet;
End;

Procedure DispFile (Const Name: String);
Const
  BufferLen = 1024;

Var
  i, rResult             : Integer;
  File2Disp              : File;
  rBuf                   : Array [1..BufferLen] Of Byte;
  OldMacro1, OldMacro2,
  OldMacro3, OldStopCode : Boolean;

Begin
  If Cnf. DebugFiles Then
    SetDebugFile (Name);

  Assign (File2Disp, Name);
  ReSet (File2Disp, 1);

  If IOResult <> 0 Then
  Begin
    LogWrite ('!', sm (smFileOpenErr) + Name);
    Exit;
  End;

  UpdateUserMacro;

  With ScreenOut Do
  Begin
    OldMacro1 := UseMacroTable1;
    OldMacro2 := UseMacroTable2;
    OldMacro3 := UseMacroTable3;
    OldStopCode := StopCodeEnable;

    UseMacroTable1 := True;
    UseMacroTable2 := False;
    UseMacroTable3 := False;
    StopCodeEnable := True;

    Repeat
      If DrawAborted Then
        Break;

      BlockRead (File2Disp, rBuf, BufferLen, rResult);

      For i := 1 To rResult Do
        ScreenOutByte (rBuf [i]);

      Clock2;
    Until rResult <> BufferLen;

    Close (File2Disp);

    FlushScreenOut;
    UseMacroTable1 := OldMacro1;
    UseMacroTable2 := OldMacro2;
    UseMacroTable3 := OldMacro3;
    StopCodeEnable := OldStopCode;
  End;
End;

Function fEmuDispFile (F: String): Boolean;
Var
  Names    : PStringCollection;
  i        : Integer;
  Name     : String [8];
  FullName : String;
  DirInfo  : SearchRec;

Begin
  fEmuDispFile := False;

  If F [1] = '~' Then
  Begin
    Delete (F, 1, 1);
    i := Length (F);

    If i < 8 Then
    Begin
      Names := New (PStringCollection, Init (10, 1));
      FullName := DefaultName (F + '*', EmuExt [R. Emu], lang (laTxtFiles));

      FindFirst (FullName, AnyFile-VolumeID-Directory-Hidden, DirInfo);

      While DOSError = 0 Do
      Begin
        Name := Copy (JustName (DirInfo. Name), i + 1, 255);
        If (Name = '') Or ((Length (Name) = 1) And (Name [1] in ['1'..'9'])) Then
        Begin
          Names^. Insert (NewStr (DirInfo. Name));
          fEmuDispFile := True;
        End;

        FindNext (DirInfo);
      End;

    {$IFNDEF MSDOS}
      FindClose (DirInfo);
    {$ENDIF}

      FullName := JustPathName (FullName) + '\';

      For i := 0 To Names^. Count-1 Do
        DispFile (FullName + PString (Names^. At (i))^);

      Dispose (Names, Done);
      Exit;
    End;
  End;

  FullName := DefaultName (F, EmuExt [R. Emu], lang (laTxtFiles));
  If FileExists (FullName) Then
  Begin
    DispFile (FullName);
    fEmuDispFile := True;
  End;
End;

Procedure oDispFile (Const S: String);
Begin
  EmuDispFile (S);
End;

Procedure Frame;
Var
  oAttr : Byte;

Begin
  If R. Frames And (R. Emu <> teTty) Then
  Begin
    oAttr := TextAttr;
    ComWriteLn (EmuRelColor (Cnf. ColorScheme [scFrames]) +
                EmuGotoXY (1, R. Lines - 4) +
                'Ö' + Replicate ('Ä', 76) + '·', 0);
    ComWriteLn ('º' + Replicate (' ', 76) + 'º', 0);
    ComWriteLn ('Ó' + Replicate ('Ä', 76) + '½' + EmuRelColor (oAttr), 0);
    ComWrite (EmuCursorUp (2) + EmuCursorRight (2), 0);
  End;
End;

Function fEmuColor (Attr: Byte): String;
Begin
  Case R. Emu Of
     teAnsi : fEmuColor := AnsiColor (Attr);
   teAvatar : fEmuColor := AvtColor (Attr);
      teTty : fEmuColor := '';
  End;
End;

Function fEmuRelColor (Attr: Byte): String;
Begin
  Case R. Emu Of
     teAnsi : fEmuRelColor := AnsiColorRelative (ioTextAttr, Attr);
   teAvatar : fEmuRelColor := AvtColorRelative (ioTextAttr, Attr);
      teTty : fEmuRelColor := '';
  End;
End;

Function fEmuGotoXY (X, Y: Integer): String;
Begin
  If X < 1 Then
    X := 1;
  If Y < 1 Then
    Y := 1;

  Case R. Emu Of
     teAnsi : fEmuGotoXY := ANSIGotoXY (X, Y);
   teAvatar : fEmuGotoXY := AvtGotoXY (X, Y);
      teTty : fEmuGotoXY := '';
  End;
End;

Function fEmuCursorLeft (LX: Integer): String;
Begin
  If LX > 0 Then
    Case R. Emu Of
       teAnsi : fEmuCursorLeft := ANSILeft (LX);
     teAvatar : fEmuCursorLeft := AvtLeft (LX);
        teTty : fEmuCursorLeft := Replicate (#8, LX);
    End
  Else
    fEmuCursorLeft := '';
End;

Function fEmuCursorRight (LX: Integer): String;
Begin
  If LX > 0 Then
    Case R. Emu Of
       teAnsi : fEmuCursorRight := ANSIRight (LX);
     teAvatar : fEmuCursorRight := AvtRight (LX);
        teTty : fEmuCursorRight := Replicate (' ', LX);
    End
  Else
    fEmuCursorRight := '';
End;

Function fEmuCursorUp (LY: Integer): String;
Begin
  If LY > 0 Then
    Case R. Emu Of
       teAnsi : fEmuCursorUp := ANSIUp (LY);
     teAvatar : fEmuCursorUp := AvtUp (LY);
        teTty : fEmuCursorUp := '';
    End
  Else
    fEmuCursorUp := '';
End;

Function fEmuCursorDown (LY: Integer): String;
Begin
  If LY > 0 Then
    Case R. Emu Of
       teAnsi : fEmuCursorDown := ANSIDown (LY);
     teAvatar : fEmuCursorDown := AvtDown (LY);
        teTty : fEmuCursorDown := '';
    End
  Else
    fEmuCursorDown := '';
End;

Function fEmuCls: String;
Begin
  Case R. Emu Of
     teAnsi : fEmuCls := #27'[2J';
   teAvatar : fEmuCls := ^L;
      teTty : fEmuCls := ^L;
  End;
End;

Procedure fCls;
Begin
  ComWrite (EmuCls, 0);
End;

Function fEmuClrEoL: String;
Begin
  Case R. Emu Of
     teAnsi : fEmuClrEoL := #27'[K';
   teAvatar : fEmuClrEoL := ^V^G;
      teTty : fEmuClrEoL := Replicate (' ', 79 - WhereX) +
                            Replicate (#8, 79 - WhereX);
  End;
End;

Procedure fSmartLine;
Begin
  If R. Frames And (R. Emu <> teTty) Then
    ComWriteLn ('', 0);
End;

Procedure fMessage (Const Mess: String);
Begin
  If Mess <> '' Then
    ComWriteLn (#13 + Mess + #10, eoMacro + eoCodes)
  Else
    ComWriteLn ('', 0);

  ComWrite (lang (laEnterForCont), eoMacro + eoCodes);

  If HotKeysStr = '' Then
    While (ComReadKey <> #13) Do
      TimeSlice;

  ComWrite (#13, 0);
  ComWrite (EmuClrEOL, 0);
End;

Procedure ProcessChoices;
Begin
  If Not R. HotKeys Then
  Begin
    SetInputCap (NoCaps, AllChars);

    If Length (HotKeysStr) = 0 Then
    Begin
      ComRead (HotKeysStr, HotLength, ofAllowEmpty);

      If HotKeysStr <> '' Then
        ComWrite (Replicate (#8, Length (HotKeysStr)), 0)
      Else
      Begin
        KeyBufAdd (#13);
        Exit;
      End;
    End;

    KeyBufAdd (HotKeysStr [1]);
    Delete (HotKeysStr, 1, 1);
  End;
End;

{$IFNDEF VirtualPascal}
  {$L ZeroMsg.Obj}
{$ELSE}
  {$L ZMsg32.Obj}
{$ENDIF}
  Function ZeroMsg (Param: String; CR: Boolean): String; External;
  Function ZeroBackGround (Param: String): String; External;

Function GetModemResponse (Mode: tResponseMode; TimeOut: LongInt;
                           BGProc: tBGProc): tModemResponse;
Var
  T : EventTimer;
  i : Integer;
  C : Char;
  S : String;

Begin
  NewTimerSecs (T, TimeOut);

  Repeat
    While CharReady (Port) Do
    Begin
      GetChar (Port, C);

      Case C Of
        #13 : If mBuffer <> '' Then
              Begin
                LastResponse := Trim (mBuffer);
                mBuffer := '';

                If ((Mode = rmRingWait) Or (Mode = rmInit)) And
                   (LastResponse = Cnf. RingStr) Then
                Begin
                  GetModemResponse := mrRing;
                  Exit;
                End;

                If ((Mode = rmRingWait) Or (Mode = rmConnectWait)) And
                    (Copy (LastResponse, 1, Length (Cnf. ConnectStr)) =
                     Cnf. ConnectStr) Then
                Begin
                  S := Trim (Copy (LastResponse, Length (Cnf. ConnectStr) + 1,
                    255));
                  i := 1;

                  While (i <= Length (S)) And (S [i] in NumbersOnly) Do
                    Inc (i);

                  ConnectSpeed := Str2Long (Copy (S, 1, i - 1));
                  If ConnectSpeed < 300 Then
                    ConnectSpeed := 300;

                  GetModemResponse := mrConnect;
                  Exit;
                End;

                If (Mode = rmConnectWait) And (LastResponse =
                   Cnf. NoCarrierStr) Then
                Begin
                  GetModemResponse := mrNoCarrier;
                  Exit;
                End;

                If (Mode = rmInit) And (LastResponse = Cnf. OkStr) Then
                Begin
                  GetModemResponse := mrOk;
                  Exit;
                End;
              End;
        #10 : ;
      Else
        mBuffer := mBuffer + C;
      End;
    End;
  Until TimerExpired (T) Or Not BGProc;

  LastResponse := Trim (mBuffer);
  GetModemResponse := mrNoResponse;
End;

Function fGetConnectSpeed: LongInt;
Begin
  If Not Local Then fGetConnectSpeed := ConnectSpeed
               Else fGetConnectSpeed := 0;
End;

Procedure SetConnectSpeed (Speed: LongInt);
Begin
  ConnectSpeed := Speed;
End;

Procedure DoCommand (Const S: String);
Var
  i : Integer;
  C : ^Char;

Begin
  C := @S;

  For i := 1 To Byte (C^) Do
  Begin
    Inc (C);

    Case C^ Of
      '|' : PutChar (Port, #13);
      '^' : SetDTR (Port, True);
      'v' : SetDTR (Port, False);
      '~' : {$IFDEF MSDOS} tmDelay {$ELSE} Pause {$ENDIF} (500);
      '''': {$IFDEF MSDOS} tmDelay {$ELSE} Pause {$ENDIF} (100);
    Else
      PutChar (Port, C^);
    End;
  End;
End;

Procedure HangUp;
Begin
  {SetModem (Port, True, True);} {!!}
  SetRTS (Port, True);           {!!}

  If CheckDCD (Port) Or LocalOffHook Then
    DoCommand (Cnf. HangUpString)
  Else
  Begin
    SetDTR (Port, False);
    Pause (1000);
    SetDTR (Port, True);
    Pause (100);
  End;

  {SetModem (Port, False, False);} {!!}
  SetRTS (Port, False);            {!!}
End;

Procedure ClearBuffer;
Begin
  If InAllow. ConIn Then
    While KeyPressed Do
      ReadKey;

  If InAllow. RemIn Then
    FlushInBuffer (Port);
End;

Function RemoteAnsiDetected: Boolean;
Var
  T   : EventTimer;
  C   : Char;
  Tmp : String [20];

Begin
  PutString (Port, #27'[6n'#13'    '#13);
  Tmp := '';
  NewTimer (T, 36);

  Repeat
    While CharReady (Port) Do
    Begin
      GetChar (Port, C);
      Tmp := Tmp + C;
      NewTimer (T, 36);
    End;

    TimeSlice;
  Until TimerExpired (T);

  RemoteAnsiDetected := Tmp [1] = #27;
End;

Procedure UpdateFAreaMacro;
Begin
  ScreenOut. MacroTable1^. ReplaceMacro ('FARE', ZeroMsg (FileArea. Name, True));
  ScreenOut. MacroTable1^. ReplaceMacro ('FNUM', Long2Str (R. FileArea));
End;

Procedure UpdateFGroupMacro;
Begin
  ScreenOut. MacroTable1^. ReplaceMacro ('FGRP', ZeroMsg (FileGroup. Name, True));
  ScreenOut. MacroTable1^. ReplaceMacro ('FGRN', Long2Str (R. FileGroup));
End;

Procedure UpdateMAreaMacro;
Begin
  ScreenOut. MacroTable1^. ReplaceMacro ('MARE', ZeroMsg (MsgArea. Name, True));
  ScreenOut. MacroTable1^. ReplaceMacro ('MNUM', Long2Str (R. MsgArea));
End;

Procedure UpdateMGroupMacro;
Begin
  ScreenOut. MacroTable1^. ReplaceMacro ('MGRP', ZeroMsg (MsgGroup. Name, True));
  ScreenOut. MacroTable1^. ReplaceMacro ('MGRN', Long2Str (R. MsgGroup));
End;

Procedure UpdateUserMacro;
Begin
  With ScreenOut. MacroTable1^ Do
  Begin
    ReplaceMacro ('NAME', R. Name);
    ReplaceMacro ('FNAM', ExtractWord (1, R. Name, SpaceOnly));
    ReplaceMacro ('LNAM', Copy (R. Name, Pos (' ', R. Name) + 1, 255));
    ReplaceMacro ('ALAS', R. Alias);
    ReplaceMacro ('PSWD', R. Password);
    ReplaceMacro ('DAOB', Long2Date (R. BirthDate, Cnf. DateMask));
    ReplaceMacro ('LOCA', R. Location);
    ReplaceMacro ('ORGZ', R. Organization);
    ReplaceMacro ('HPHN', R. HPhone);
    ReplaceMacro ('BPHN', R. BPhone);
    ReplaceMacro ('ADR1', R. Address1);
    ReplaceMacro ('ADR2', R. Address2);
    ReplaceMacro ('ADR3', R. Address3);
    ReplaceMacro ('CMNT', R. Comment);
    ReplaceMacro ('LINS', Long2Str (R. Lines));
    ReplaceMacro ('SECR', Long2Str (R. Security));
    ReplaceMacro ('ULDS', Long2Str (R. UpLoads));
    ReplaceMacro ('DNLS', Long2Str (R. DownLoads));
    ReplaceMacro ('TTME', Long2Str (Lim. Time));
    ReplaceMacro ('ETME', Long2Str (Round ((R. TotalTime - TimeDiff (EnterTime,
      MidSec)) / 60)));
    ReplaceMacro ('CALN', Long2Str (R. NoCalls));
    ReplaceMacro ('FDTE', Long2Date (R. FirstDate, Cnf. DateMask));
    ReplaceMacro ('LDTE', Long2Date (R. LastDate, Cnf. DateMask));
    ReplaceMacro ('LTME', Word2Time (R. LastTime));
    ReplaceMacro ('PROT', ProtocolDef. Name);
    ReplaceMacro ('EMUL', EmuName [R. Emu]);
    ReplaceMacro ('MORE', Bool2Lang (R. More));
    ReplaceMacro ('HKEY', Bool2Lang (R. HotKeys));
    ReplaceMacro ('FRAM', Bool2Lang (R. Frames));
    ReplaceMacro ('FSED', Bool2Lang (R. FSEditor));
    ReplaceMacro ('LANG', lang (laName));
    ReplaceMacro ('DATE', FormattedCurrDT (Cnf. DateMask));
    ReplaceMacro ('TIME', StrTime);
    ReplaceMacro ('SELK', Long2Str (Round (SizeOfAll / 1024)));
    ReplaceMacro ('SELN', Long2Str (F2Transfer^. Count));
    ReplaceMacro ('LIMK', Long2Str (R. DailySize));
    ReplaceMacro ('ETTM', Long2Str (Round (EstimatedTransferTime (SizeOfAll,
      R. AvgCPS, GetConnectSpeed) / 60)));
    ReplaceMacro ('DLKB', Long2Str (R. DownLoadsK));
    ReplaceMacro ('ULKB', Long2Str (R. UpLoadsK));
    ReplaceMacro ('LFKB', Long2Str (R. DailySize - (R. TodayK +
      Trunc (SizeOfAll / 1024))));
    ReplaceMacro ('FLGS', R. Flags);
    ReplaceMacro ('OTME', Long2Str (Round (TimeDiff (EnterTime, MidSec)
      / 60)));
    ReplaceMacro ('DLTK', Long2Str (R. TodayK));
    ReplaceMacro ('MSGP', Long2Str (R. MsgsPosted));

    ReplaceMacro ('FAGR', Long2Str (fAreasGroup));
    ReplaceMacro ('FAMT', Long2Str (fAreasAmount));
    ReplaceMacro ('FGAM', Long2Str (ffGroupsAmount));
    ReplaceMacro ('MAGR', Long2Str (mAreasGroup));
    ReplaceMacro ('MAMT', Long2Str (mAreasAmount));
    ReplaceMacro ('MGAM', Long2Str (mmGroupsAmount));
  End;

  UpdateFAreaMacro;
  UpdateFGroupMacro;
  UpdateMAreaMacro;
  UpdateMGroupMacro;
End;

Procedure TorSoundBell;
Begin
  If Cnf. Sound Then
    PlaySound (220, 200);
End;

Procedure KeyBufAdd (Const S: String);
Begin
  KeyBuffer := KeyBuffer + S;
End;

Procedure RetWait;
Begin
  While ComReadKey <> #13 Do;
End;

Procedure fSetSecurity;
Begin
  ReadLimit (Lim, R. Security);
  R. TotalTime := Lim. Time * 60 - (TimeDiff (EnterTime, MidSec) -
    UsedMinus + R. TimeUsedToday);
  R. DailySize := Lim. KBLimit;
  EnterTime := MidSec;
End;

Procedure mWriteLen (Const S: String; L: Byte; Pad: tPadMode);
Var
  C : ^Char;
  i : Integer;
  x : Byte;

Begin
  x := WhereX + L;

  If Pad = pmPadLeft Then
    ComWrite (Replicate (' ', L - Length (ZeroMsg (S, True))), 0);

  With ScreenOut Do
  Begin
    UseMacroTable1 := True;
    UseMacroTable2 := True;
    UseMacroTable3 := True;
    StopCodeEnable := True;

    i := 1;
    C := @S;

    While (i <= Length (S)) And (WhereX < x) Do
    Begin
      Inc (i);
      Inc (C);

      If C^ <> '|' Then
        ScreenOutByte (Byte (C^))
      Else
      Begin
        ScreenOutByte (13);
        ScreenOutByte (10);
      End;

      FlushScreenOut;
    End;
  End;

  If Pad = pmPadRight Then
  Begin
    L := WhereX;
    If L < x Then
      ComWrite (Replicate (' ', x - L), 0);
  End;
End;

Procedure SlashRotate;
Const
  j         : Byte = 1;
  LastGluck : LongInt = 0;

Var
  i, L      : LongInt;

Begin
  L := MidSec;
  i := L - LastGluck;

  If (i > 1) Or (i < 0) Then
  Begin
    Inc (j);
    If j > RotNum Then
      j := 1;

    ComWrite (RotBar [j] + EmuCursorLeft (1), 0);
    LastGluck := L;
  End;
End;

Begin
  ComWrite := fComWrite;
  ComWriteLn := fComWriteLn;
  EmuColor := fEmuColor;
  EmuRelColor := fEmuRelColor;
  EmuGotoXY := fEmuGotoXY;
  EmuCursorLeft := fEmuCursorLeft;
  EmuCursorRight := fEmuCursorRight;
  EmuCursorUp := fEmuCursorUp;
  EmuCursorDown := fEmuCursorDown;
  EmuCls := fEmuCls;
  Cls := fCls;
  EmuClrEOL := fEmuClrEOL;
  EmuDispFile := fEmuDispFile;
  SmartLine := fSmartLine;
  Message := fMessage;
  SetSecurity := fSetSecurity;
  DrawAborted := fDrawAborted;
  GetConnectSpeed := fGetConnectSpeed;
  mBuffer := '';
End.
