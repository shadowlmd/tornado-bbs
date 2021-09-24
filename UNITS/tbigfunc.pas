{$IFDEF MSDOS}
  {$IFNDEF DPMI32}
    {$F+}
  {$IFNDEF DPMI}
    {$DEFINE RealMode}
    {$O+}
  {$ENDIF}
  {$ENDIF}
{$ENDIF}
{$B-,I-}
{&Delphi+}

Unit TBigFunc;

Interface

Uses
{$IFDEF OS2}
  Os2Base,
{$ENDIF}
  DOS,
  tWin,
  ApTimer,
  ApCom,
  aDir,
  TorInOut,
  MainComm,
  Log,
  tGlob,
  tMisc,
  iEMSI,
  OpCrt,
  Parse,
  Parser,
  Objects,
  SysMsgs,
  TimeTask,
  Users,
  CompSys,
  BSC,
  Protocol,
  UserEd,
  IFace,
  iLine,
  tMsgLib;

Var
  ChatLog : Text;

Procedure InitTBigFunc;

Procedure Chat;
Procedure PageSysOp (Const Topic: String);

(* System status managment *)
Procedure ReadSystemStatus (Var System: SystemType; Const FN: String);
Procedure WriteSystemStatus (System: SystemType; Const FN: String);

(* LastCall.Tor *)
Function ReadLastCaller (Var LC: tLastCaller; Mode: tRLCmode): Boolean;
Procedure SaveLastCaller (Const Name: String; Line: Byte;
            Const LoginTime: String; Date, TimeOnLine, ULkb, DLkb: LongInt);

Procedure SaveTime;

Procedure OnlineArcView (Param: PathStr);
Procedure PageSwitch;
Procedure InsDisp (Const S: PathStr);
Function NoInfo: Boolean;
Procedure UserParams;

Procedure MessageToUser;
Procedure ManualSend;
Procedure ViewTagList;
Procedure UEdit;
Procedure Help;
Procedure AccessLevel;
Procedure InsertFile;

Procedure TodaysCallers;
Procedure Lockout;
Procedure IncTime;
Procedure DecTime;

Implementation

Uses
  Areas;

Type
  ScrChRec = Record
    Ch   : Char;
    Attr : Byte;
  End;

  ScrArr = Array [0..8192] Of ScrChRec;

  PScreenSave = ^TScreenSave;
  TScreenSave = Record
    SaveX, SaveY,
    Attribute     : Byte;
    svStatusBar   : Boolean;
    P             : ^ScrArr;
  End;

Const
  ArcV_FNameLen    = 42;
  ArcV_DateLen     = 10;
  ArcV_OrigSizeLen = 12;
  ArcV_PackSizeLen = 12;

Var
  TimeLeft : LongInt;
  lcFile   : File Of tLastCaller;

Procedure Chat;

  Procedure UsualChat;
  Var
    S : String;

    Procedure EndOfLine (Const S: String);
    Begin
      If Cnf. ChatLog <> '' Then
        WriteLn (ChatLog, S);
      ComWriteLn ('', 0);
    End;

    Procedure DoChar (C: Char; Var S: String; MaxLen: Integer);
    Var
      j        : Integer;
      LastWord : String;

    Begin
      Case C Of
       #13 : Begin
               EndOfLine (S);
               S := '';
             End;
        #8 : If Length (S) > 0 Then
             Begin
               SetLength (S, Length (S) - 1);
               ComWrite (#8#32#8, 0);
             End;
      Else
        S := S + C;

        If Length (S) <= MaxLen Then
          ComWrite (C, 0)
        Else
        Begin
          If C = ' ' Then
          Begin
            LastWord := '';
            SetLength (S, MaxLen);
          End Else
          Begin
            j := WordCount (S, SpaceOnly);
            If j > 1 Then LastWord := ExtractWord (j, S, SpaceOnly)
                     Else LastWord := C;
            SetLength (S, Length (S) - Length (LastWord));
          End;

          j := Length (LastWord) - 1;
          If j > 0 Then
            ComWrite (EmuCursorLeft (j) + Replicate (' ', j), 0);

          EndOfLine (S);
          S := LastWord;

          If j >= 0 Then
            ComWrite (S, 0);
        End;
      End;
    End;

  Const
    ChatColors : Array [Boolean] Of Byte = (14, 15);

  Var
    oLRColor : Boolean;
    C        : Char;

  Begin
    If Cnf. ChatLog <> '' Then
      WriteLn (ChatLog);

    S := '';
    oLRColor := False;
    ComWrite (EmuRelColor (ChatColors [oLRColor]), 0);

    Repeat
      C := ComReadKey;

      If C in [#8, #13, #32..#255] Then
      Begin
        If FromKeyBoard <> oLRColor Then
        Begin
          oLRColor := FromKeyBoard;
          ComWrite (EmuRelColor (ChatColors [oLRColor]), 0);
        End;

        DoChar (C, S, 78);
      End;
    Until (C = #27) And FromKeyboard;
  End;

  Procedure FramedChat;
  Const
    RingBufSize = 32;
    RingBufMask = RingBufSize - 1;

  Type
    RingBuffer  = Array [0..RingBufSize - 1] Of PString;
    tDoChar     = (dcSysOp, dcUser);

  Var
    SysBuf, UserBuf             : RingBuffer;
    SysBufHead, UserBufHead,
    sLineNum, uLineNum, SysOpX,
    SysOpY, UserX, UserY        : Integer;
    OnSysOpSide                 : Boolean;
    SysIni, UserIni             : String [MaxInitials + 3];
    SysS, UserS                 : String;

    Procedure EndOfLine (Const S: String; Mode: tDoChar);
    Var
      i, WinLen : Integer;

    Begin
      Case Mode Of
        dcSysOp : Begin
                    ChangePString (SysBuf [SysBufHead], S);
                    SysBufHead := (SysBufHead + 1) And RingBufMask;
                    If Cnf. ChatLog <> '' Then
                      WriteLn (ChatLog, SysIni + S);

                    If sLineNum = Cnf. ChatSysY2 - Cnf. ChatSysY1 Then
                    Begin
                      WinLen := Cnf. ChatSysX2 - Cnf. ChatSysX1;

                      For i := 0 To Cnf. ChatSysY2 - Cnf. ChatSysY1 - 1 Do
                        ComWrite (EmuGoToXY (Cnf. ChatSysX1, Cnf. ChatSysY1 + i)
                          + Pad (SysBuf [(SysBufHead - (Cnf. ChatSysY2 -
                          Cnf. ChatSysY1) + i) And RingBufMask]^, WinLen), 0);

                      ComWrite (EmuGoToXY (Cnf. ChatSysX1, Cnf. ChatSysY2) +
                        Replicate (' ', WinLen), 0);
                    End
                    Else
                      Inc (sLineNum);

                    ComWrite (EmuGoToXY (Cnf. ChatSysX1, Cnf. ChatSysY1 +
                      sLineNum), 0);
                  End;

         dcUser : Begin
                    ChangePString (UserBuf [UserBufHead], S);
                    UserBufHead := (UserBufHead + 1) And RingBufMask;
                    If Cnf. ChatLog <> '' Then
                      WriteLn (ChatLog, UserIni + S);

                    If uLineNum = Cnf. ChatUserY2 - Cnf. ChatUserY1 Then
                    Begin
                      WinLen := Cnf. ChatUserX2 - Cnf. ChatUserX1;

                      For i := 0 To Cnf. ChatUserY2 - Cnf. ChatUserY1 - 1 Do
                        ComWrite (EmuGoToXY (Cnf. ChatUserX1, Cnf. ChatUserY1 +
                          i) + Pad (UserBuf [(UserBufHead - (Cnf. ChatUserY2 -
                          Cnf. ChatUserY1) + i) And RingBufMask]^, WinLen), 0);

                      ComWrite (EmuGoToXY (Cnf. ChatUserX1, Cnf. ChatUserY2) +
                        Replicate (' ', WinLen), 0);
                    End
                    Else
                      Inc (uLineNum);

                    ComWrite (EmuGoToXY (Cnf. ChatUserX1, Cnf. ChatUserY1 +
                      uLineNum), 0);
                  End;
      End;
    End;

    Procedure DoChar (C: Char; Var S: String; MaxLen: Integer; Mode: tDoChar);
    Var
      i, j     : Integer;
      LastWord : String;

    Begin
      Case C Of
        #13 : Begin
                EndOfLine (S, Mode);
                S := '';
              End;
         #8 : If Length (S) > 0 Then
              Begin
                SetLength (S, Length (S) - 1);
                ComWrite (#8#32#8, 0);
              End;
        #24 : Begin                                          { Ctrl-X }
                j := Length (S);
                If j > 0 Then
                Begin
                  ComWrite (EmuCursorLeft (j), 0);
                  ComWrite (Replicate (' ', j), 0);
                  ComWrite (EmuCursorLeft (j), 0);
                  S := '';
                End;
              End;
        #18 : Begin                                          { Ctrl-R }
                If OnSysOpSide Then
                Begin
                  SysOpX := WhereX;
                  SysOpY := WhereY;
                End Else
                Begin
                  UserX := WhereX;
                  UserY := WhereY;
                End;

                EmuDispFile ('chtframe');
                ComWrite (EmuRelColor (Cnf. ColorScheme [chtText]), 0);

                For i := sLineNum DownTo 1 Do
                  ComWrite (EmuGoToXY (Cnf. ChatSysX1, Cnf. ChatSysY1 + i - 1)
                    + Pad (SysBuf [(SysBufHead - 1 - (sLineNum - i)) And
                    RingBufMask]^, Cnf. ChatSysX2 - Cnf. ChatSysX1), 0);

                ComWrite (EmuGoToXY (Cnf. ChatSysX1, SysOpY) + SysS, 0);

                For i := uLineNum DownTo 1 Do
                  ComWrite (EmuGoToXY (Cnf. ChatUserX1, Cnf. ChatUserY1 + i - 1)
                    + Pad (UserBuf [(UserBufHead - 1 - (uLineNum - i)) And
                    RingBufMask]^, Cnf. ChatUserX2 - Cnf. ChatUserX1), 0);

                ComWrite (EmuGoToXY (Cnf. ChatUserX1, UserY) + UserS, 0);
                OnSysOpSide := False;
              End;
         #7 : ComWrite (#7, 0);
      Else
        If Ord (C) >= 32 Then
        Begin
          S := S + C;

          If Length (S) <= MaxLen Then
            ComWrite (C, 0)
          Else
          Begin
            If C = ' ' Then
            Begin
              LastWord := '';
              SetLength (S, MaxLen);
            End Else
            Begin
              j := WordCount (S, SpaceOnly);
              If j > 1 Then LastWord := ExtractWord (j, S, SpaceOnly)
                       Else LastWord := C;
              SetLength (S, Length (S) - Length (LastWord));
            End;

            j := Length (LastWord) - 1;
            If j > 0 Then
              ComWrite (EmuCursorLeft (j) + Replicate (' ', j), 0);

            EndOfLine (S, Mode);
            S := LastWord;

            If j >= 0 Then
              ComWrite (S, 0);
          End;
        End;
      End;
    End;

  Var
    i : Integer;
    C : Char;

  Begin
    FillChar (SysBuf, SizeOf (SysBuf), 0);
    FillChar (UserBuf, SizeOf (UserBuf), 0);
    SysBufHead := 0;
    UserBufHead := 0;
    sLineNum := 0;
    uLineNum := 0;
    SysOpX := Cnf. ChatSysX1;
    SysOpY := Cnf. ChatSysY1;
    UserX := Cnf. ChatUserX1;
    UserY := Cnf. ChatUserY1;
    OnSysOpSide := True;
    SysS := '';
    UserS := '';

    If Cnf. ChatLog <> '' Then
    Begin
      SysIni := GetInitials (Cnf. SysOp, MaxInitials) + '> ';
      UserIni := GetInitials (R. Name, MaxInitials) + '>> ';
      WriteLn (ChatLog);
      WriteLn (ChatLog, '    ' + Pad (SysIni, 7) + '- ' + Cnf. SysOp);
      WriteLn (ChatLog, '    ' + Pad (UserIni, 7) + '- ' + R. Name);
      WriteLn (ChatLog);
    End;

    EmuDispFile ('chtframe');
    ComWrite (EmuGoToXY (SysOpX, SysOpY) + EmuRelColor (Cnf. ColorScheme
      [chtText]), 0);

    Repeat
      C := ComReadKey;

      If FromKeyBoard Then
      Begin
        If C = #27 Then
          Break;

        If Not OnSysOpSide Then
        Begin
          UserX := WhereX;
          UserY := WhereY;
          ComWrite (EmuGoToXY (SysOpX, SysOpY), 0);
          OnSysOpSide := True;
        End;

        DoChar (C, SysS, Cnf. ChatSysX2 - Cnf. ChatSysX1, dcSysOp);
      End Else
      Begin
        If OnSysOpSide Then
        Begin
          SysOpX := WhereX;
          SysOpY := WhereY;
          ComWrite (EmuGoToXY (UserX, UserY), 0);
          OnSysOpSide := False;
        End;

        DoChar (C, UserS, Cnf. ChatUserX2 - Cnf. ChatUserX1, dcUser);
      End;
    Until False;

    If Cnf. ChatLog <> '' Then
    Begin
      If SysS <> '' Then
        WriteLn (ChatLog, SysIni + SysS);
      If UserS <> '' Then
        WriteLn (ChatLog, UserIni + UserS);
    End;

    For i := 0 To RingBufSize-1 Do
    Begin
      DisposeStr (SysBuf [i]);
      DisposeStr (UserBuf [i]);
    End;
  End;

Begin
  If ManualChat Then
    ComSaveScreen;
  ComWrite (EmuRelColor ($07), 0);
  If InLightBarMenu And ManualChat Then Cls
                                   Else SmartLine;
  If Cnf. ChatTimeSuspend Then
    SuspendTime;

  ChatStartTime := MidSec;

  If Cnf. ExternalChat = '' Then
  Begin
    InChat := True;

    If Cnf. ChatLog <> '' Then
    Begin
      Assign (ChatLog, Cnf. ChatLog);
      Append (ChatLog);
      If IOResult = 0 Then WriteLn (ChatLog)
                      Else ReWrite (ChatLog);

      If IOResult = 0 Then
        WriteLn (ChatLog, PlaceSubStr (PlaceSubStr (PlaceSubStr (sm
          (smChatLogOpened), '%ver%', NameVer), '%username%', R. Name),
          '%timestamp%', FormattedCurrDT (Cnf. DateMask + ' HH:II:SS')))
      Else
      Begin
        HiddenCursor;
        CenterTempBox (Cnf. ColorScheme [cdFrame], Cnf. ColorScheme [cdButton],
          1, sm (smInvalidFN), ZoomSpeed, sm (smWarningWinTitle));
        NormalCursor;
        Cnf. ChatLog := '';
      End;
    End;

    ComWrite (lang (laBeginChat), eoMacro + eoCodes);

    If Not Cnf. ChatStyle Or (R. Emu = teTTY) Then UsualChat
                                              Else FramedChat;

    ComWrite (lang (laEndChat), eoMacro + eoCodes);

    InChat := False;

    If Cnf. ChatLog <> '' Then
    Begin
      WriteLn (ChatLog);
      WriteLn (ChatLog, sm (smChatLogClosed));
      Close (ChatLog);
    End;
  End
  Else
    DosShell (TranslateExecParams (Cnf. ExternalChat), exCommand, False);

  If Cnf. ChatTimeSuspend Then
    UnSuspendTime;
  If ManualChat Then
    ComRestoreScreen (False);
  WantsChat := False;
End;

Procedure PageSysOp (Const Topic: String);
Type
  tSysOpState = (sOK, sNoReaction, sBusy);

Const
  PageSubjLen = 40;
  PageTimes : Byte = 0;

Var
  Inp : String [PageSubjLen];

  Function DoPage: tSysOpState;
  Var
    PageBox  : pBoxRec;
    PTimer   : EventTimer;
    PageFile : Text;
    PageOK   : Boolean;
    oStat    : Boolean;

    Procedure PlayNextString;
    Var
      F, D : LongInt;
      i    : Integer;
      S1   : String [5];
      S    : String;

    Begin
      While Not EoF (PageFile) Do
      Begin
        ReadLn (PageFile, S);
        i := Pos (';', S);
        If i > 0 Then
          SetLength (S, i - 1);
        S := UpString (Trim (S));
        If S = '' Then
          Continue;

        S1 := ExtractWord (1, S, SpaceAndComma);

        If S1 = 'TONE' Then
        Begin
          F := Str2Long (ExtractWord (2, S, SpaceAndComma));
          D := Str2Long (ExtractWord (3, S, SpaceAndComma));

          If (F > 0) And (D > 0) Then
            PlaySound (F, D * 10);
        End
        Else
          If S1 = 'WAIT' Then
            Pause (Str2Long (ExtractWord (2, S, SpaceAndComma)) * 10);

        Exit;
      End;

      Close (PageFile);
      Reset (PageFile);
      PageOK := IOResult = 0;
    End;

  Begin
  {$IFDEF OS2}
    If ThreadLocked Then
    Begin
      DoPage := sBusy;
      Exit;
    End;
  {$ENDIF}

    DoPage := sOK;

    oStat := StatusBar;
    StatusBar := False;
    StatusBarEnable := False;

    FastWrite (Copy (CenterCh ('(' + R. Name + '): ' + Inp, ' ', ScrX + 1), 1,
      ScrX + 1), ScrY, 1, Cnf. ColorScheme [cmStatusLine]);
    FastWrite (Replicate (' ', ScrX + 1), ScrY + 1, 1,
      Cnf. ColorScheme [cmStatusLine]);

    InitWindow (PageBox, 25, 8, 55, 14, 2, Cnf. ColorScheme [cdFrame], '',
      $00, ZoomSpeed, True);
    DrawWindow (PageBox);

    FastWrite (sm (smwPaging), PageBox^. Y1 + 1, 28, Cnf. ColorScheme [cdInput]);
    FastWrite (sm (smwAnswer), PageBox^. Y1 + 3, 28, Cnf. ColorScheme [cdText]);
    FastWrite (sm (smwPageCanc), PageBox^. Y1 + 4, 28, Cnf. ColorScheme [cdText]);

    If Cnf. Sound And Not keyScrollLock Then
    Begin
      Assign (PageFile, Cnf. Path + 'page.tor');
      Reset (PageFile);
      PageOK := IOResult = 0;
    End
    Else
      PageOK := False;

    NewTimerSecs (PTimer, Cnf. PageDuration);

    Repeat
      If PageOK Then
        PlayNextString;

      SlashRotate;
      Clock2;

      If KeyPressed Then
      Begin
        If Not (UpCase (ReadKey) in ['C', '‘']) Then
          DoPage := sBusy;
        Break;
      End;

      If TimerExpired (PTimer) Then
      Begin
        DoPage := sNoReaction;
        Break;
      End;
    Until False;

    If PageOK Then
    Begin
      Close (PageFile);
      If IOResult <> 0 Then;
    End;

    If IsScreenOut Then
      NormalCursor;
    CloseWindow (PageBox);
    StatusBarEnable := True;
    StatusBar := oStat;
    If Not StatusBar Then
      ClearWindow (1, ScrY - 1, ScrX + 1, ScrY + 1, ' ', $00);
  End;

Begin
  If Not MatchTimeArray (Cnf. PageTime) Or ChkFlag (PageFlag) Then
  Begin
    WantsChat := True;
    EmuDispFile ('~notaval');
    Message ('|' + lang (laOutPageTime));
    Exit;
  End;

  If Cnf. AskChatTopic Or (Topic <> '') Then
  Begin
    If Topic = '' Then
    Begin
      If R. Frames And (R. Emu <> teTty) Then
        ComWrite (EmuGotoXY (1, 2), 0);
      Inp := Trim (GetAnswer (lang (laTopicForChat) + ' ', PageSubjLen,
        ofAllowEmpty, ''));
      ComWriteLn ('', 0);

      If Inp = '' Then
        Exit;
    End
    Else
      Inp := Topic;

    LogWrite ('+', sm (smlPagingSysOpTopic) + Inp);
  End Else
  Begin
    Inp := '* No Topic *';
    LogWrite ('+', sm (smlPagingSysOp));
  End;

  Inc (PageTimes);
  If (Cnf. PageLimit > 0) And (PageTimes > Cnf. PageLimit) Then
    Message (lang (laPageLimit))
  Else
  Begin
    SetTitle ('paging sysop for a chat');
    ComWrite (lang (laPagingForChat), eoMacro + eoCodes);
    If IsScreenOut Then
      HiddenCursor;
    WantsChat := True;

    Case DoPage Of
            sOK : Begin
                    LogWrite ('+', sm (smSysOpPaged));
                    SetTitle ('chatting with sysop');
                    PageTimes := 0;
                    Chat;
                  End;

    sNoReaction : Begin
                    LogWrite ('+', sm (smNoSysOp));
                    If Query (lang (laNoSysOpForChat), True, ofFramed) Then
                      Msg2SysOp (Inp, 'wantchat');
                  End;

          sBusy : Begin
                    LogWrite ('+', sm (smSysOpBusy));
                    If Query (lang (laSysOpBusy), True, ofFramed) Then
                      Msg2SysOp (Inp, 'wantchat');
                  End;
    End;

    SetTitle ('');
  End;

  If R. Emu = teTty Then
    TextAttr := 3;
End;

Procedure ReadSystemStatus;
Var
  F  : File Of SystemType;
  S1 : SystemType;

Begin
  WaitAndSetFlag ('sysfile.tbf');

  Assign (F, FN);
  ReSet (F);

  Read (F, S1);

  If IOResult = 100 Then
  Begin
    Close (F);
    Erase (F);
  End Else
  Begin
    System := S1;
    Close (F);
  End;

  DelFlag ('sysfile.tbf');
End;

Procedure WriteSystemStatus;
Var
  F : File Of SystemType;

Begin
  WaitAndSetFlag ('sysfile.tbf');

  Assign (F, FN);
  ReWrite (F);
  Write (F, System);
  Close (F);

  DelFlag ('sysfile.tbf');
End;

Function ReadLastCaller (Var LC: tLastCaller; Mode: tRLCmode): Boolean;
Var
  FTime, Poz       : LongInt;
  Year, Month, Day,
  DayOfWeek        : SysInt;
  FDate            : DateTime;

Begin
  ReadLastCaller := True;

  Case Mode Of
    trlcRead    : Begin
                    Read (lcFile, LC);
                    If IOResult <> 0 Then
                      ReadLastCaller := False;
                  End;

    trlcOpen4Stat, trlcOpen4All :
                  Begin
                    WaitAndSetFlag ('lcfile.tbf');
                    Assign (lcFile, Cnf. Path + 'lastcall.tor');
                    ReSet (lcFile);

                    If IOResult <> 0 Then
                    Begin
                      DelFlag ('lcfile.tbf');
                      ReadLastCaller := False;
                      Exit;
                    End;

                    If Mode = trlcOpen4Stat Then
                    Begin
                      Poz := FileSize (lcFile);
                      If Poz <= 8 Then Poz := 0
                                  Else Dec (Poz, 8);
                      Seek (lcFile, Poz);

                      If IOResult <> 0 Then
                      Begin
                        DelFlag ('lcfile.tbf');
                        ReadLastCaller := False;
                        Exit;
                      End;
                    End;

                    GetFTime (lcFile, FTime);
                    GetDate (Year, Month, Day, DayOfWeek);

                    UnPackTime (FTime, FDate);
                    If FDate. Day <> Day Then
                      ReWrite (lcFile);
                  End;

    trlcClose   : Begin
                    Close (lcFile);
                    DelFlag ('lcfile.tbf');
                  End;
  End;
End;

Procedure SaveLastCaller (Const Name: String; Line: Byte;
            Const LoginTime: String; Date, TimeOnLine, ULkb, DLkb: LongInt);
Var
  Year, Month, Day, DayOfWeek : SysInt;
  LastCallFile               : File Of tLastCaller;
  LC                         : tLastCaller;
  FTime                      : LongInt;
  FDate                      : DateTime;
  LCName                     : String;

Begin
  WaitAndSetFlag ('lcfile.tbf');

  LCName := Cnf. Path + 'lastcall.tor';

  Assign (LastCallFile, LCName);
  System. FileMode := Open_Access_ReadWrite Or Open_Share_DenyWrite;
  ReSet (LastCallFile);
  System. FileMode := Open_Access_ReadOnly Or Open_Share_DenyNone;
  If IOResult <> 0 Then
    ReWrite (LastCallFile);

  GetFTime (LastCallFile, FTime);
  GetDate (Year, Month, Day, DayOfWeek);

  UnPackTime (FTime, FDate);

  If (FDate. Day <> Day) Or
     (FDate. Month <> Month) Or
     (FDate. Year <> Year)
  Then
    ReWrite (LastCallFile);

  LC. Name := Name;
  LC. Line := Line;
  LC. LoginTime := LoginTime;
  LC. Date := Date;
  LC. TimeOnLine := TimeOnLine;
  LC. ULkb := ULkb;
  LC. DLkb := DLkb;

  Seek (LastCallFile, FileSize (LastCallFile));
  Write (LastCallFile, LC);
  Close (LastCallFile);
  If IOResult <> 0 Then;

  DelFlag ('lcfile.tbf');
End;

Procedure SaveTime;
Var
  SessionDuration : LongInt;

Begin
  If (R. Name = '') Or Registering Then
    Exit;

  SessionDuration := TimeDiff (enTime, MidSec);
  If TimeSusp Then
    UnSuspendTime;

  If TimeCount Then
  Begin
    Dec (R. TotalTime, SessionDuration);
    Inc (R. TimeUsedToday, SessionDuration - UsedMinus);

    If R. TotalTime < 0 Then
      R. TotalTime := 0;
    If R. TimeUsedToday < 0 Then
      R. TimeUsedToday := 0;
  End;

  SaveLastCaller (R. Name, BbsLine, Copy (HowTime (enTime), 1, 5), Ent,
    Round (SessionDuration / 60), SessionUL, SessionDL);

  R. LastDate := Ent;
  R. LastTime := CurrTime2Word;
  If (Cnf. OptimiseUserBase > 0) And (Sys. TotalCalls Mod
     Cnf. OptimiseUserBase = 0)
  Then
    SaveAndOptimise (R)
  Else
    SaveUser (R);
End;

Function FormArcString (FName, FileDate: String; OrigSize, PackSize: LongInt): String; Far;
Begin
  FormArcString := EmuColor (Cnf. ColorScheme [avFileName]) + Pad (NiceFileName
    (PlaceSubStr (FName, '/', '\'), ArcV_FNameLen), ArcV_FNameLen) + ' ' +
    EmuColor (Cnf. ColorScheme [avDate]) + Pad (ReFormatDate (FileDate,
    'MM-DD-YY', Cnf. DateMask), ArcV_DateLen) + ' ' + EmuColor
    (Cnf. ColorScheme [avSize]) + LeftPad (Long2Str (OrigSize),
    ArcV_OrigSizeLen) + ' ' + EmuColor (Cnf. ColorScheme [avCompressed]) +
    LeftPad (Long2Str (PackSize), ArcV_PackSizeLen);
End;

Procedure OnlineArcView;

  Procedure NoArchive;
  Begin
   If InShell Then ComWrite ('|' + lang (laNotArchive), eoMacro + eoCodes)
              Else Message ('|' + lang (laNotArchive));
  End;

Var
  i      : LongInt;
  CO     : CompressorType;
  IBMRec : IBM;
  MACRec : MAC;

Label
  EndOfProc;

Begin
  Param := UpString (Trim (Param));
  If (Param = '') Or (FileArea. DLPath = '') Or
     (Param = UpString (FileArea. DLPath))
  Then Exit;

  ArcFiles := New (PNotSortedCollection, Init (8, 8));
  BSC. FormArcStringFunc := FormArcString;

  If DetectCompressor (Param, CO) Then
  Begin
    CO^. CheckProtection;

    If CO^. Broken Then
    Begin
      NoArchive;
      Goto EndOfProc;
    End;

    CO^. WriteHeader;
    CO^. FindFirstEntry;

    While Not CO^. LastEntry Do
    Begin
      CO^. PrintEntry;

      Case PlatformID (CO^. WhichPlatform) Of
        ID_IBM : CO^. ReturnEntry (IBMRec);
        ID_MAC : CO^. ReturnEntry (MACRec);
       End;

      CO^. FindNextEntry;
    End;

    ComWriteLn ('', 0);
    ComWrite (lang (laArcName), eoMacro + eoCodes + eoNoFlush);
    ComWrite (JustFileName (Param), eoNoFlush);
    ComWriteLn (' (' + CO^. CompressorName + ')', 0);
    ComWriteLn (lang (laArcHead), eoMacro + eoCodes);
    ComWriteLn (EmuRelColor (Cnf. ColorScheme [umSeparator]) +
      'ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ ÄÄÄÄÄÄÄÄÄÄ ÄÄÄÄÄÄÄÄÄÄÄÄ ÄÄÄÄÄÄÄÄÄÄÄÄ',
      0);

    InitMore (WhereY);

    For i := 0 To ArcFiles^. Count - 1 Do
    Begin
      ComWriteLn (GetStr (ArcFiles^. At (i)), 0);
      If Not More Then
        Goto EndOfProc;
    End;

    If Not InShell Then
      Message ('');
  End
  Else
    NoArchive;

EndOfProc:
  Dispose (ArcFiles, Done);
End;

Procedure TodaysCallers;
Var
  LC : tLastCaller;

Begin
  Cls;
  ComWriteLn (lang (laTodaysHead), eoMacro + eoCodes);
  ComWriteLn (EmuRelColor (Cnf. ColorScheme [umSeparator]) +
    'ÄÄÄÄÄ ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ ÄÄÄÄÄ ÄÄÄÄÄ ÄÄÄÄÄÄÄÄÄ',
    eoMacro + eoCodes);
  InitMore (WhereY);

  If Not ReadLastCaller (LC, trlcOpen4All) Then
  Begin
    Message ('');
    Exit;
  End;

  While ReadLastCaller (LC, trlcRead) Do
  Begin
    If (LC. Name = '') Or (Cnf. HideSysOp And (LC. Name = Cnf. SysOp) And
       (R. Name <> Cnf. SysOp))
    Then
      Continue;

    If DrawAborted Then
      Break;

    ComWrite (EmuRelColor (Cnf. ColorScheme [tdLine]) +
      Pad (Long2Str (LC. Line), 6), eoNoFlush);
    ComWrite (EmuRelColor (Cnf. ColorScheme [tdName]) + Pad (LC. Name, 37),
      eoNoFlush);
    ComWrite (EmuRelColor (Cnf. ColorScheme [tdEnterTime]) + LC. LoginTime +
      ' ', eoNoFlush);
    ComWrite (EmuRelColor (Cnf. ColorScheme [tdOnLine]) + LeftPad (Long2Str
      (LC. TimeOnLine), 5) + ' ', eoNoFlush);
    ComWriteLn (EmuColor (Cnf. ColorScheme [tdTransfers]) + LeftPad (Long2Str
      (LC. DLkb), 4) + ':' + Pad (Long2Str (LC. ULkb), 4), 0);

    If Not More Then
      Break;
  End;

  ReadLastCaller (LC, trlcClose);
  Message ('');
End;

Procedure fGoTelnet (Host: String); Far;
Begin
End;

Procedure fUserInfo (Name: String); Far;
Var
  i, j, Num, SysOpNum : LongInt;
  SysOpSkip           : Byte;
  TmpUser             : tUser;
  S                   : String;

Label
  Loop,
  GetNumAgain,
  NotFound,
  EndOfProc;

Begin
  Name := Trim (Name);
  SetInputCap (NoCaps, NumbersOnly);

Loop:
  S := '';

  If Name = '' Then
  Begin
    Cls;
    ComWriteLn (lang (laUsersHead), eoMacro + eoCodes);
    ComWriteLn (EmuRelColor (Cnf. ColorScheme [umSeparator]) +
      'ÄÄÄÄ ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ ÄÄÄÄÄÄÄÄÄÄ ÄÄÄÄÄÄÄÄÄÄ',
      0);
    InitMore (WhereY);

    Num := UsersNum;
    SysOpNum := 0;
    SysOpSkip := 0;

    For j := 1 To Num Do
    Begin
      If DrawAborted Then
        Goto EndOfProc;

      GetUserByNum (j, TmpUser);

      If ((TmpUser. Name = Cnf. SysOp) And (R. Name <> Cnf. SysOp)) And
         Cnf. HideSysOp Then
      Begin
        SysOpNum := j;
        SysOpSkip := 1;
        Continue;
      End;

      ComWrite (EmuRelColor (Cnf. ColorScheme [ulNumbers]) + LeftPad (Long2Str
        (j - SysOpSkip), 4) + ' ', eoNoFlush);
      ComWrite (EmuRelColor (Cnf. ColorScheme [ulName]) + Pad (TmpUser. Name,
        36) + ' ', eoNoFlush);
      ComWrite (EmuRelColor (Cnf. ColorScheme [ulLocation]) + Pad
        (TmpUser. Location, 15) + ' ', eoNoFlush);
      ComWrite (EmuRelColor (Cnf. ColorScheme [ulBirthDate]) +
        Pad (Long2Date (TmpUser. BirthDate, Cnf. DateMask), 10) + ' ',
        eoNoFlush);
      ComWriteLn (EmuRelColor (Cnf. ColorScheme [ulLastDate]) +
        Long2Date (TmpUser. LastDate, Cnf. DateMask), 0);

      If (R. Security < Cnf. UserInfoMinSec) Or
         Not FlagsValid (R. Flags, Cnf. UserInfoMinFlags) Then
      Begin
        If Not More Then
          Goto EndOfProc;
      End Else
      Begin

      GetNumAgain:
        If Not MoreNums (S, lang (laEnterUsrNums), 4, False) Then
          Goto EndOfProc;

        If S <> '' Then
        Begin
          i := Str2Long (S);
          If i >= SysOpNum Then
            Inc (i, SysOpSkip);
          If (i < 1) Or (i > j) Then
          Begin
            MoreLines := R. Lines;
            S := '';
            Goto GetNumAgain;
          End;

          Name := GetUserName (i);
          Break;
        End;
      End;
    End;
  End;

  If Trim (Name) = '' Then
    If (R. Security >= Cnf. UserInfoMinSec) And
       FlagsValid (R. Flags, Cnf. UserInfoMinFlags) Then
    Begin
      ComWriteLn ('', 0);
      ComWrite (lang (laEnterUsrNums), eoMacro + eoCodes + eoNoFlush);
      S := '';
      ComReadLn (S, 4, ofAllowEmpty);

      i := Str2Long (S);
      If (i >= SysOpNum) Then
        Inc (i, SysOpSkip);
      If (i < 1) Or (i > j) Then
        Goto EndOfProc;

      Name := GetUserName (i);
      ComWriteLn ('', 0);
    End Else
    Begin
      Message ('');
      Goto EndOfProc;
    End
  Else
    ComWriteLn ('', 0);

  If Not Is_User (Name, Cnf. Aliases) Then
  Begin

  NotFound:
    Message (lang (laUser) + Name + lang (laUserNotFound));
    Goto EndOfProc;
  End;

  If Name <> R. Name Then GetUser (Name, TmpUser, Cnf. Aliases)
                     Else TmpUser := R;

  If (TmpUser. Name = Cnf. SysOp) And (R. Name <> Cnf. SysOp) And
     Cnf. HideSysop
  Then
    Goto NotFound;

  SmartLine;

  With TmpUser Do
  Begin
    Cls;
    ComWrite ('|' + TGlob. lang (laInfoName) + Name, eoMacro + eoCodes +
      eoNoFlush);
    If Alias <> '' Then
      ComWrite (' (' + Alias + ')', eoNoFlush);
    ComWriteLn ('', 0);

    ComWrite (TGlob. lang (laInfoLoc), eoMacro + eoCodes + eoNoFlush);
    ComWriteLn (Location, 0);
    ComWrite (TGlob. lang (laInfoOrg), eoMacro + eoCodes + eoNoFlush);
    ComWriteLn (Organization, 0);
    ComWrite (TGlob. lang (laInfoAddress1), eoMacro + eoCodes + eoNoFlush);
    ComWriteLn (Address1, 0);
    ComWrite (TGlob. lang (laInfoAddress2), eoMacro + eoCodes + eoNoFlush);
    ComWriteLn (Address2, 0);
    ComWrite (TGlob. lang (laInfoAddress3), eoMacro + eoCodes + eoNoFlush);
    ComWriteLn (Address3, 0);
    ComWrite (TGlob. lang (laInfoHPhone), eoMacro + eoCodes + eoNoFlush);
    ComWriteLn (HPhone, 0);
    ComWrite (TGlob. lang (laInfoBPhone), eoMacro + eoCodes + eoNoFlush);
    ComWriteLn (BPhone, 0);

    ComWrite (TGlob. lang (laInfoFirstCall), eoMacro + eoCodes + eoNoFlush);
    ComWriteLn (Long2Date (FirstDate, Cnf. DateMask), 0);

    ComWrite (TGlob. lang (laInfoLastCall), eoMacro + eoCodes + eoNoFlush);
    ComWriteLn (Long2Date (LastDate, Cnf. DateMask), 0);

    ComWrite (TGlob. lang (laInfoBirthDate), eoMacro + eoCodes + eoNoFlush);
    ComWriteLn (Long2Date (BirthDate, Cnf. DateMask), 0);

    ComWrite (TGlob. lang (laInfoSecurity), eoMacro + eoCodes + eoNoFlush);
    ComWriteLn (Long2Str (Security), 0);
    ComWrite (TGlob. lang (laInfoCallNum), eoMacro + eoCodes + eoNoFlush);
    ComWriteLn (Long2Str (NoCalls), 0);
  End;

  Message ('');

  If S <> '' Then
  Begin
    Name := '';
    Goto Loop;
  End;

EndOfProc:
  SetInputCap (NoCaps, AllChars);
End;

Procedure ManualSend;
Const
  Path : PathStr = '';

Var
  oSizeOfAll      : LongInt;
  i               : Integer;
  Stat            : pnState;
  oF2Transfer     : PTagFilesCollection;
  MarkedFiles     : PNotSortedCollection;
  FileRec         : TTagFileRec;
  CurDir, CurDir1 : PathStr;

Begin
{$IFDEF OS2}
  ThreadLocked := False;
{$ENDIF}
  GetDir (0, CurDir);
  HiddenCursor;

  MarkedFiles := New (PNotSortedCollection, Init (8, 4));

  Stat := Pane_Process (20, Round (tWin. ScrY / 2) - 6, pmMultiHalf,
    ivOnlyName, Path, 'DD-MM-YY', Cnf. Blinking, MarkedFiles);

  GetDir (0, CurDir1);
  If CurDir <> CurDir1 Then
    SmartChDir (CurDir);
  NormalCursor;

  If (Stat <> pnOk) Or (MarkedFiles^. Count = 0) Then
  Begin
    Dispose (MarkedFiles, Done);
    Exit;
  End;

  oF2Transfer := F2Transfer;
  F2Transfer := New (PTagFilesCollection, Init (MarkedFiles^. Count, 1));
  oSizeOfAll := SizeOfAll;
  SizeOfAll := 0;

  FileRec. GroupNum := 0;
  FileRec. AreaNum := 0;
  FileRec. FromName := Nil;
  FileRec. Free := True;

  For i := 0 To MarkedFiles^. Count-1 Do
  Begin
    FileRec. PathName := PString (MarkedFiles^. At (i));
    FileRec. Size := gFileSize (FileRec. PathName^);
    InsertInTagList (FileRec);
  End;

  MarkedFiles^. DeleteAll;
  Dispose (MarkedFiles, Done);

  AutoDL := True;
  Transfer ('', Transmit, tsNormal);
  AutoDL := False;

  Dispose (F2Transfer, Done);
  F2Transfer := oF2Transfer;
  SizeOfAll := oSizeOfAll;
End;

Procedure ViewTagList;
Var
  i, Size       : LongInt;
  oX, oY, oAttr : Byte;
  ScrollList    : PNotSortedCollection;
  PF            : PTagFileRec;
  TagBox        : PBoxRec;
  S             : String;

Begin
  HiddenCursor;

  If F2Transfer^. Count <= 0 Then
  Begin
    CenterTempBox (Cnf. ColorScheme [cdFrame], Cnf. ColorScheme [cdButton],
      1, sm (smNoTagFiles), ZoomSpeed, sm (smWarningWinTitle));
    NormalCursor;
    Exit;
  End;

  ScrollList := New (PNotSortedCollection, Init (F2Transfer^. Count, 1));
  Size := 0;

  For i := 0 To F2Transfer^. Count-1 Do
  Begin
    PF := PTagFileRec (F2Transfer^. At (i));
    ScrollList^. Insert (NewStr (' ' + Pad (NiceFileName (PF^. PathName^, 47),
      47) + ' ' + LeftPad (NiceFileSize (PF^. Size), 5)));
    Inc (Size, PF^. Size);
  End;

  oX := WhereX;
  oY := WhereY;
  oAttr := TextAttr;

  InitWindow (TagBox, 1, 1, 57, 15, 4, Cnf. ColorScheme [cdFrame],
    ' ' + sm (smTagListTitle) + ' ', Cnf. ColorScheme [cdTitle], ZoomSpeed,
    True);
  DrawWindow (TagBox);

  With TagBox^ Do
  Begin
    FastWrite ('Æ' + Replicate ('Í', X2 - X1 - 1) + 'µ', Y1 + 11, X1,
      Cnf. ColorScheme [cdFrame]);
    S := sm (smTotalFiles);
    FastWrite (S, Y1 + 12, X1 + 2, Cnf. ColorScheme [cdText]);
    FastWrite (Long2Str (F2Transfer^. Count), Y1 + 12, X1 + 3 + Length (S),
      Cnf. ColorScheme [cdInput]);
    S := sm (smTotalSize);
    FastWrite (S, Y1 + 13, X1 + 2, Cnf. ColorScheme [cdText]);
    FastWrite (NiceFileSize (Size), Y1 + 13, X1 + 3 + Length (S),
      Cnf. ColorScheme [cdInput]);
    S := sm (smibSpeed);
    FastWrite (S, Y1 + 12, X1 + 26, Cnf. ColorScheme [cdText]);
    FastWrite (Long2Str (GetConnectSpeed) + ' bps', Y1 + 12, X1 + 27 +
      Length (S), Cnf. ColorScheme [cdInput]);
    S := sm (smEstTime);
    FastWrite (S, Y1 + 13, X1 + 26, Cnf. ColorScheme [cdText]);
    FastWrite (Long2Str (Round (EstimatedTransferTime (SizeOfAll, R. AvgCPS,
      GetConnectSpeed) / 60)) + ' min.', Y1 + 13, X1 + 27 + Length (S),
      Cnf. ColorScheme [cdInput]);

    ScrollTextWindow (X1 + 1, Y1 + 1, X2, Y1 + 11,
      Cnf. ColorScheme [cdFrame] And $0F, Cnf. ColorScheme [cdFrame] Shr 4,
      Cnf. ColorScheme [cdScroller] And $0F, Cnf. ColorScheme [cdScroller] Shr 4,
      Cnf. ColorScheme [cdScroller] Shr 4, Cnf. ColorScheme [cdScroller] And $0F,
      ScrollList);
  End;

  CloseWindow (TagBox);
  Dispose (ScrollList, Done);

  GotoXY (oX, oY);
  TextAttr := oAttr;
  NormalCursor;
End;

Procedure UEdit;
Var
  TmpUser : tUser;

Begin
  If NoInfo Then
    Exit;

  TmpUser := R;
  If EditUser (TmpUser) Then
  Begin
    R := TmpUser;
    SaveUser (R);
    UpdateUserMacro;
  End;
End;

Procedure Help;
Var
  Help          : PNotSortedCollection;
  HelpBox       : PBoxRec;
  oX, oY, oAttr : Byte;

Begin
  Help := New (PNotSortedCollection, Init (8, 8));

  If Not ReadCollection (MsgFileName, Help^, 'Help', NameVer, True) Then
  Begin
    Dispose (Help, Done);
    Exit;
  End;

  HiddenCursor;
  oX := WhereX;
  oY := WhereY;
  oAttr := TextAttr;

  InitWindow (HelpBox, 1, 1, 55, 14, 4, Cnf. ColorScheme [cdFrame],
    ' ' + sm (smHelpWinTitle) + ' ', Cnf. ColorScheme [cdTitle], ZoomSpeed,
    True);
  DrawWindow (HelpBox);
  ScrollTextWindow (HelpBox^. X1 + 1, HelpBox^. Y1 + 1, HelpBox^. X2,
    HelpBox^. Y2, Cnf. ColorScheme [cdFrame] And $0F,
    Cnf. ColorScheme [cdFrame] Shr 4, Cnf. ColorScheme [cdScroller] And $0F,
    Cnf. ColorScheme [cdScroller] Shr 4, Cnf. ColorScheme [cdScroller] Shr 4,
    Cnf. ColorScheme [cdScroller] And $0F, Help);

  Dispose (Help, Done);
  CloseWindow (HelpBox);

  GotoXY (oX, oY);
  TextAttr := oAttr;
  NormalCursor;
End;

Procedure AccessLevel;
Var
  TmpSec         : LongInt;
  oX, oY, iState : Byte;
  Changed        : Boolean;
  SecBox         : PBoxRec;

Label
  Loop,
  ProcExit;

Begin
  If NoInfo Then
    Exit;

  oX := WhereX;
  oY := WhereY;
  InitWindow (SecBox, 24, 9, 52, 11, 4, Cnf. ColorScheme [cdFrame],
    sm (smSecLevel), Cnf. ColorScheme [cdTitle], ZoomSpeed, True);
  DrawWindow (SecBox);

  TmpSec := R. Security;

Loop:
  TmpSec := Str2Long (Input (37, SecBox^. Y1 + 1, Long2Str (TmpSec), ' ', '',
    6, 0, [#32..#175], True, iState, Cnf. ColorScheme [cdInput], Changed));

  If iState = 27 Then
    Goto ProcExit;

  If iState In [72, 80, 73, 81] Then
    Goto Loop;

  If (TmpSec > 65535) Or (TmpSec < 1) Then
  Begin
    HiddenCursor;
    CenterTempBox (Cnf. ColorScheme [cdFrame], Cnf. ColorScheme [cdButton], 1,
      sm (smSecOutRange), ZoomSpeed, sm (smWarningWinTitle));
    NormalCursor;
    GoTo Loop;
  End;

  R. Security := TmpSec;
  SetSecurity;

ProcExit:
  CloseWindow (SecBox);
  GotoXY (oX, oY);
End;

Procedure MessageToUser;
Var
  DispBox        : PBoxRec;
  iState, oX, oY : Byte;
  Changed        : Boolean;
  Msg2Disp       : String;

Label
  Loop;

Begin
  oX := WhereX;
  oY := WhereY;
  HiddenCursor;
  InitWindow (DispBox, 5, 9, 73, 11, 4, Cnf. ColorScheme [cdFrame],
    sm (smSendMsgText), Cnf. ColorScheme [cdTitle], ZoomSpeed, True);
  DrawWindow (DispBox);
  NormalCursor;

Loop:
  Msg2Disp := Input (9, DispBox^. Y1 + 1, '', ' ', '', 61, 0, [#32..#255],
    True, iState, Cnf. ColorScheme [cdInput], Changed);

  If iState In [72, 80, 73, 81] Then
    Goto Loop;

  CloseWindow (DispBox);
  GoToXY (oX, oY);

  If (iState <> 27) And (Trim (Msg2Disp) <> '') Then
  Begin
    ComWriteLn (#13#10, 0);
    ComWriteLn (lang (laMsgFromSysOp) + Msg2Disp, eoMacro + eoCodes);
  End;
End;

Procedure InsertFile;
Const
  Path      : PathStr = '';
  File2Disp : PathStr = '';

Var
  MarkedFiles     : PNotSortedCollection;
  DispBox         : PBoxRec;
  iState, oX, oY  : Byte;
  Stat            : pnState;
  Changed         : Boolean;
  CurDir, CurDir1 : PathStr;

Label
  Loop;

Begin
  oX := WhereX;
  oY := WhereY;

  ComSaveScreen;

  HiddenCursor;
  InitWindow (DispBox, 5, 9, 73, 11, 4, Cnf. ColorScheme [cdFrame],
    sm (smFile2Disp), Cnf. ColorScheme [cdTitle], ZoomSpeed, True);
  DrawWindow (DispBox);
  NormalCursor;

Loop:
  File2Disp := Trim (Input (9, DispBox^. Y1 + 1, File2Disp, ' ', '', 61, 0,
    [#32..#175], True, iState, Cnf. ColorScheme [cdInput], Changed));

  If iState In [72, 80, 73, 81] Then
    Goto Loop;

  CloseWindow (DispBox);
  GoToXY (oX, oY);

  If iState = 27 Then
    Exit;

  If File2Disp = '' Then
  Begin
    MarkedFiles := New (PNotSortedCollection, Init (8, 4));

    GetDir (0, CurDir);

    HiddenCursor;

    Stat := Pane_Process (20, Round (tWin. ScrY/2)-6, pmMultiHalf, ivOnlyName,
      Path, 'DD-MM-YY', Cnf. Blinking, MarkedFiles);

    GetDir (0, CurDir1);
    If CurDir <> CurDir1 Then
      SmartChDir (CurDir);
    NormalCursor;

    If (Stat <> pnOk) Or (MarkedFiles^. Count = 0) Then
    Begin
      Dispose (MarkedFiles, Done);
      Exit;
    End;

    While MarkedFiles^. Count > 0 Do
    Begin
      InsDisp (GetStr (MarkedFiles^. At (0)));
      MarkedFiles^. AtFree (0);
    End;

    Dispose (MarkedFiles, Done);
  End
  Else
    InsDisp (File2Disp);

  ComRestoreScreen (InLightBarMenu);
End;

Procedure PageSwitch;
Var
  Res        : Byte;
  PageStatus : Boolean;
  S          : String [5];
  OnOff      : Array [Boolean] Of String [5];

Begin
  PageStatus := Not ChkFlag (PageFlag);
  OnOff [True] := sm (smPageOn);
  OnOff [False] := sm (smPageOff);
  S := OnOff [False];
  If Not PageStatus Then
    S := '~' + S;
  HiddenCursor;

  Res := SelectBox (OnOff [True] + '|' + S, Cnf. ColorScheme [cdFrame],
    Cnf. ColorScheme [cdButton], Cnf. ColorScheme [cdTitle], ZoomSpeed,
    sm (smPageIs) + OnOff [PageStatus]);

  Case Res Of
    1 : EraseFlag (PageFlag);
    2 : MakeFlag (PageFlag);
  End;

  NormalCursor;
End;

Procedure fComSaveScreen; Far;
Var
  PScrSave : PScreenSave;

Begin
  GetMem (PScrSave, SizeOf (tScreenSave));
  With PScrSave^ Do
  Begin
    SaveWindow (1, 1, ScrX + 1, ScrY + 1, True, Pointer (P));
    SaveX := WhereX;
    SaveY := WhereY;
    Attribute := TextAttr;
    svStatusBar := StatusBar;
  End;
  Screens^. Insert (PScrSave);
End;

Procedure fComRestoreScreen (WaitForKey: Boolean); Far;
Var
  X, MaxX, Y, MaxY, LineOfs : Word;
  PScrSave                  : PScreenSave;
  C                         : ScrChRec;
  PrevAttr                  : Byte;
  S                         : String;

Begin
  If WaitForKey Then
    KeyBufAdd (ComReadKey);

  Cls;

  PScrSave := PScreenSave (Screens^. At (Screens^. Count-1));
  With PScrSave^ Do
  Begin
    LineOfs := 0;
    PrevAttr := P^ [0]. Attr;
    S := '';

    If svStatusBar Then MaxY := ScrY - 3
                   Else MaxY := ScrY - 1;
    If (R. Emu = teTTY) And (SaveY <= MaxY) Then
      MaxY := SaveY - 1;

    For Y := 0 To MaxY Do
    Begin
      If (R. Emu = teTTY) And (Y = MaxY) Then
        MaxX := SaveX - 1
      Else
        If (Y = 0) And Cnf. Clock Then MaxX := ScrX - 8
                                  Else MaxX := ScrX - 1;

      For X := 0 To MaxX Do
      Begin
        C := P^ [LineOfs + X];

        If C. Attr = PrevAttr Then
          S := S + C. Ch
        Else
        Begin
          ComWrite (EmuRelColor (PrevAttr) + S, 0);
          PrevAttr := C. Attr;
          S := C. Ch;
        End;
      End;

      S := TrimTrail (S);
      If S <> '' Then
      Begin
        ComWriteLn (EmuRelColor (PrevAttr) + S, 0);
        S := '';
      End
      Else
        ComWriteLn ('', 0);

      Inc (LineOfs, ScrX + 1);
    End;

    ComWrite (EmuGoToXY (SaveX, SaveY) + EmuRelColor (Attribute), 0);
  End;

  FreeMem (PScrSave^. P, (Succ (ScrX) * Succ (ScrY)) Shl 1);
  FreeMem (PScrSave, SizeOf (tScreenSave));
  Screens^. AtDelete (Screens^. Count-1);
End;

Procedure fSuspendTime; Far;
Begin
  If Not TimeSusp Then
  Begin
    TimeSusp := True;
    TimeCount := False;
    TimeLeft := R. TotalTime - TimeDiff (EnterTime, MidSec);
  End;
End;

Procedure fUnSuspendTime; Far;
Begin
  If TimeSusp Then
  Begin
    TimeSusp := False;

    If Not Registering Then
    Begin
      R. TotalTime := TimeLeft + TimeDiff (EnterTime, MidSec);
      UsedMinus := UsedMinus + TimeDiff (ChatStartTime, MidSec);
      TimeCount := True;
    End
    Else
      EnterTime := MidSec;
  End;
End;

Procedure InsDisp (Const S: PathStr);
Begin
  LogWrite ('+', sm (smlShowFile) + NiceFileName (S, 50));
  EmuDispFile (S);
End;

Procedure Lockout;
Begin
  If (R. Name <> '') And Not Registering Then
  Begin
    LogWrite ('!', sm (smLockOut));
    EmuDispFile ('~lockout');
    R. Security := 0;
    NormExit;
  End;
End;

Procedure IncTime;
Begin
  If TimeCount Then
  Begin
    Inc (R. TotalTime, 60);
    If StatusBar And Not EnteringPass Then
      ShowTimeLeft;
  End;
End;

Procedure DecTime;
Begin
  If TimeCount Then
  Begin
    If R. TotalTime >= 60 Then Dec (R. TotalTime, 60)
                          Else R. TotalTime := 0;
    If StatusBar And Not EnteringPass Then
      ShowTimeLeft;
  End;
End;

Function NoInfo;
Begin
  If ((R. Name = '') Or Registering) Then
  Begin
    HiddenCursor;
    CenterTempBox (Cnf. ColorScheme [cdFrame], Cnf. ColorScheme [cdButton],
      1, sm (smiNotLogined), ZoomSpeed, sm (smWarningWinTitle));
    NormalCursor;
    NoInfo := True;
  End
  Else
    NoInfo := False;
End;

Procedure UserParams;
Var
  k       : LongInt;
  InfoBox : pBoxRec;
  Age     : Byte;
  C       : Char;
  S1, S2  : String [4];

Begin
  If NoInfo Then
    Exit;

  HiddenCursor;
  InitWindow (InfoBox, 0, 0, 61, 15, 4, Cnf. ColorScheme [cdFrame],
    ' ' + sm (smibTitle) + ' ', Cnf. ColorScheme [cdTitle], ZoomSpeed, True);
  DrawWindow (InfoBox);

  With InfoBox^ Do
  Begin
    FastWrite ('Æ' + Replicate ('Í', X2 - X1 - 1) + 'µ', Y1 + 9, X1,
      Cnf. ColorScheme [cdFrame]);
    FastWrite ('Æ' + Replicate ('Í', X2 - X1 - 1) + 'µ', Y1 + 12, X1,
      Cnf. ColorScheme [cdFrame]);
    FastWrite (sm (smibSpeed), Y1 + 1, X1 + 2, Cnf. ColorScheme [cdText]);
    FastWrite (sm (smibAvgCPS), Y1 + 2, X1 + 2, Cnf. ColorScheme [cdText]);
    FastWrite (sm (smibName), Y1 + 3, X1 + 2, Cnf. ColorScheme [cdText]);
    FastWrite (sm (smibBirthdate), Y1 + 4, X1 + 2, Cnf. ColorScheme [cdText]);
    FastWrite (sm (smibAge), Y1 + 5, X1 + 2, Cnf. ColorScheme [cdText]);
    FastWrite (sm (smibLocation), Y1 + 6, X1 + 2, Cnf. ColorScheme [cdText]);
    FastWrite (sm (smibPassword), Y1 + 7, X1 + 2, Cnf. ColorScheme [cdText]);
    FastWrite (sm (smibSecurity), Y1 + 8, X1 + 2, Cnf. ColorScheme [cdText]);
    FastWrite (sm (smibLastDate), Y1 + 10, X1 + 2, Cnf. ColorScheme [cdText]);
    FastWrite (sm (smibFirstDate), Y1 + 11, X1 + 2, Cnf. ColorScheme [cdText]);
    FastWrite (sm (smibDailyTime), Y1 + 10, X1 + 32, Cnf. ColorScheme [cdText]);
    FastWrite (sm (smibTimeLeft), Y1 + 11, X1 + 32, Cnf. ColorScheme [cdText]);
    If EMSI. Session Then
      FastWrite (sm (smibIEMSI_yes), Y1 + 13, X1 + 2,
        Cnf. ColorScheme [cdText])
    Else
      FastWrite (sm (smibIEMSI_no), Y1 + 13, X1 + 2,
        Cnf. ColorScheme [cdText]);

    Age := Str2Long (FormattedCurrDT ('YYYY')) - Str2Long (Long2Date
      (R. BirthDate, 'YYYY'));
    S1 := Long2Date (R. BirthDate, 'MMDD');
    S2 := FormattedCurrDT ('MMDD');
    k := Str2Long (Copy (S1, 1, 2)) - Str2Long (Copy (S2, 1, 2));
    If k = 0 Then
      k := Str2Long (Copy (S1, 3, 2)) - Str2Long (Copy (S2, 3, 2));
    If k > 0 Then
      Dec (Age);

    FastWrite (Long2Str (GetConnectSpeed), Y1 + 1, X1 + 21,
      Cnf. ColorScheme [cdInput]);
    FastWrite (Long2Str (R. AvgCPS), Y1 + 2, X1 + 21,
      Cnf. ColorScheme [cdInput]);
    FastWrite (R. Name, Y1 + 3, X1 + 21, Cnf. ColorScheme [cdInput]);
    FastWrite (Long2Date (R. BirthDate, Cnf. DateMask), Y1 + 4, X1 + 21,
      Cnf. ColorScheme [cdInput]);
    FastWrite (Long2Str (Age) + ' ' + sm (smibYears), Y1 + 5, X1 + 21,
      Cnf. ColorScheme [cdInput]);
    FastWrite (Pad (R. Location, 38), Y1 + 6, X1 + 21,
      Cnf. ColorScheme [cdInput]);
    FastWrite (R. Password, Y1 + 7, X1 + 21, Cnf. ColorScheme [cdInput]);
    FastWrite (Long2Str (R. Security), Y1 + 8, X1 + 21,
      Cnf. ColorScheme [cdInput]);
    FastWrite (Long2Date (R. LastDate, Cnf. DateMask), Y1 + 10, X1 + 21,
      Cnf. ColorScheme [cdInput]);
    FastWrite (Long2Date (R. FirstDate, Cnf. DateMask), Y1 + 11, X1 + 21,
      Cnf. ColorScheme [cdInput]);
    FastWrite (Long2Str (Lim. Time) + ' ' + sm (smsMin), Y1 + 10, X1 + 51,
      Cnf. ColorScheme [cdInput]);

    If InChat Then k := ChatStartTime
              Else k := MidSec;
    FastWrite (Long2Str (Round ((R. TotalTime - TimeDiff (EnterTime, k)) / 60)) + ' ' +
      sm (smsMin), Y1 + 11, X1 + 51, Cnf. ColorScheme [cdInput]);

    If EMSI. Session Then
    Begin
      FastWrite (sm (smibSoftWare), Y1 + 14, X1 + 2,
        Cnf. ColorScheme [cdText]);
      FastWrite (Pad (EMSI. SoftWare, 38), Y1 + 14, X1 + 21,
        Cnf. ColorScheme [cdInput]);
    End;
  End;

  WaitForKey (C);
  If C = #0 Then
    WaitForKey (C);

  CloseWindow (InfoBox);
  NormalCursor;
End;

Procedure InitTBigFunc;
Begin
  UserInfo := fUserInfo;
  ComSaveScreen := fComSaveScreen;
  ComRestoreScreen := fComRestoreScreen;
  SuspendTime := fSuspendTime;
  UnSuspendTime := fUnSuspendTime;
  GoTelnet := fGoTelnet;
End;

End.
