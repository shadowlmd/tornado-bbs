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

Unit tMainOvr;

{*********************************************************}
{*                    TMAINOVR.PAS                       *}
{*                                                       *}
{*  Copyright (c) Konstantin Klyagin, 1995-98,           *}
{*                exspecially for Tornado BBS System     *}
{*                                                       *}
{*********************************************************}

Interface

Uses
{$IFDEF RealMode}
  Streams,
{$ENDIF}
{$IFDEF MSDOS}
{$IFNDEF DPMI32}
  Vector,
{$ENDIF}
  tDebug,
{$ENDIF}
{$IFDEF OS2}
  Os2Base,
  VPUtils,
  tRexx,
{$ENDIF}
  DOS,
  ApSame,
  ApCom,
  ApTimer,
  Iface,
  tWin,
  OpCrt,
  MainComm,
  tGlob,
  tMisc,
  Crc,
  Log,
  SysMsgs,
  Areas,
  Parser,
  Parse,
  BinCfg,
  Users,
  FilesBBS,
  tMenus,
  Protocol,
  Shell,
  SaveTag,
  Upgrader,
  torMacro,
  torInOut,
  TimeTask,
  DoReg,
  Objects,
  mFind,
  MainCOVr,
  tQWK,
  IEMSI,
  tScript,
  tBigFunc,
  DoorWay,
  FCache,
  RCache;

Var
  ReqName       : String [36];

Procedure InitTMainOvr;
Procedure Login;
Procedure DoBBS;
Procedure DoMenu (Var FileName: Str8);
Procedure ChangeParam (ParNum: Byte);
Procedure ErrorExit;
{$IFNDEF OS2}
Function tExecRexx (S: PathStr): Boolean;
{$ENDIF}

Implementation

Type
  PLangDesc = ^TLangDesc;
  TLangDesc = Record
    Name     : String [33];
    FileName : String [80];
  End;

  PLangDescCollection = ^TLangDescCollection;
  TLangDescCollection = Object (TSortedCollection)
    Function Compare (Key1, Key2: Pointer): Integer; Virtual;
    Procedure FreeItem (Item: Pointer); Virtual;
  End;

Var
  KolPass               : Byte;
  VIPinformed, VIPcheck : Boolean;
  MenuFile              : Str8;

{$IFDEF OS2}
Function PreThread (P : Pointer): LongInt;
Begin
  PreThread := 0;
  ThreadLocked := True;

  SetInput (False, Not Local);
  SetOutput (False, Not Local);

  PT (P);

  If Not Local Then
    ClrScr;

  ThreadLocked := False;

  SetInput (True, Not Local);
  SetOutput (True, Not Local);
End;
{$ENDIF}

{$IFNDEF OS2}
Function tExecRexx (S: PathStr): Boolean;
Begin
  tExecRexx := False;
End;
{$ENDIF}

Procedure ClearReps;
Var
  PS : PString;
  i  : Integer;

Begin
  For i := 0 To Reps^. Count-1 Do
  Begin
    PS := Reps^. At (i);
    tDeleteFile (Copy (PS^, Pos (#255, PS^) + 1, 255));
  End;

  Reps^. FreeAll;
End;

Procedure VeryImportant;
Var
  i         : Byte;
  BoxDrawed : Boolean;

Begin
  If Not VIPinformed Then
  Begin
    BoxDrawed := False;

    If Not Local Then
      If R. Name = 'Konstantin Klyagin' Then
      Begin
        LogWrite ('!', sm (smAuthor));
        BoxDrawed := True;
        CenterDelayTempBox (Cnf. ColorScheme [caFrame],
          Cnf. ColorScheme [caText], 1, CenterCh (sm (smAuthor), ' ', 30),
          ZoomSpeed, '');
      End;

    If Not BoxDrawed And InList (Cnf. VIPList, R. Name) Then
    Begin
      LogWrite ('!', sm (smVIP));
      BoxDrawed := True;
      CenterDelayTempBox (Cnf. ColorScheme [caFrame], Cnf. ColorScheme [caText],
        1, CenterCh (sm (smVIP), ' ', 30), ZoomSpeed, '');
    End;

    If BoxDrawed Then
    Begin
      If Cnf. Sound Then
      Begin
        SoundOf ('4 3500 0 50 1 3000 0 50 1 4000 0 5 1');

        For i := 1 To 6 Do
        Begin
          If KeyPressed Then
            Break;

          Pause (500);
          SoundOf ('1 500 0 2 1');
        End;
      End;

      DoneTempBox;
    End;

    VIPinformed := True;
  End;
End;

Procedure DoBBS;
Begin
  LastClock := MidSec;
  HiddenCursor;
  SetTitle ('logging in');

  R. Emu := teTty;
  R. More := False;
  R. LastDate := 0;
  R. LastTime := 0;
  R. FirstDate := 0;
  R. BirthDate := 0;
  HotKeysStr := '';

  InitScreenOut (oDispFile, oExecScript, 4096, Nil);
  With ScreenOut. MacroTable1^ Do
  Begin
    ReplaceMacro ('SYSO', Cnf. SysOp);
    ReplaceMacro ('PROD', NameVer);
    ReplaceMacro ('BBSN', Cnf. BBSname);
    ReplaceMacro ('NODE', Long2Str (BbsLine));
    ReplaceMacro ('DATE', FormattedCurrDT (Cnf. DateMask));
    ReplaceMacro ('TIME', StrTime);
    ReplaceMacro ('BAUD', Long2Str (GetConnectSpeed));
    ReplaceMacro ('USRS', Long2Str (uNum));
  End;

  ReadLanguage (Language, Cnf. DefLangFile);
  R. Lang := UpString (JustName (Cnf. DefLangFile));
  R. XLAT := '';

  SetInput (True, Not Local);
  SetOutput (True, Not Local);

  If Not Local Then
  Begin
    SetAbortFunc (Port, KbdAbort);

    If Cnf. DetectEmu = ynAuto Then
    Begin
      AddMsg (ShortStrTime + sm (smTestEmu), False,
        Cnf. ColorScheme [cmUsers]);
      If RemoteAnsiDetected Then
        R. Emu := teAnsi;
    End
    Else
      If Cnf. DetectEmu = ynYes Then
        R. Emu := teAnsi;

    AddMsg (' [' + EmuName [R. Emu] + ']', False, Cnf. ColorScheme [cmUsers]);
  End
  Else
    R. Emu := teAnsi;

  StatusBar := Cnf. StatusBar;
  StatusBarEnable := True;
  EnterTime := MidSec;
  enTime := EnterTime;

  TextAttr := $07;
  ClrScr;

  If Cnf. StatusBar Then
    Window (1, 1, 80, ScrY - 1);

  ShowStatusBar;
  NormalCursor;

  If Not Local And (GetConnectSpeed < Cnf. MinSpeed) Then
  Begin
    EmuDispFile ('~tooslow');
    LogWrite ('!', sm (smlTooSlow));
    Pause (2000);
    NormExit;
  End;

  Login;

  If (RunScript <> '') Or (RunRexx <> '') Then
  Begin
    If RunScript <> '' Then
      ExecScript (RunScript);
    If RunRexx <> '' Then
      ExecRexx (RunRexx);
    NormExit;
  End;

  Repeat
    ComWrite (EmuRelColor (3), 0);
    DoMenu (MenuFile);
  Until False;
End;

Function EnterPass (Const Msg: String; NewUser: Boolean): String;
Var
  C     : Char;
  PwPos : Byte;
  S     : String;

Label
  Loop;

Begin
  Inc (KolPass);
  If KolPass = Cnf. PassTryes + 1 Then
  Begin
    If Not (R. Frames And (R. Emu <> teTty)) Then
      ComWriteLn ('', 0);
    If Query (lang (laOutOfTryes), True, ofFramed) Then
      Msg2SysOp ('Password error.', 'pswd_err');
    NormExit;
  End;

  StatusBarEnable := False;
  EnteringPass := True;

Loop:
  If StatusBar Then
  Begin
    FastWrite (Pad (sm (smEntering), 80), ScrY, 1,
      Cnf. ColorScheme [cmStatusLine]);
    PwPos := Length (sm (smEntering)) + 1;

    If Trim (R. Password) <> '' Then
      FastWrite (Pad (sm (smPassMustBe) + R. Password, 80), ScrY + 1, 1,
        Cnf. ColorScheme [cmStatusLine])
    Else
      FastWrite (Replicate (' ', ScrX + 1), ScrY + 1, 1,
        Cnf. ColorScheme [cmStatusLine]);
  End;

  Frame;
  ComWrite (Msg, eoMacro + eoCodes);

  If VIPcheck Then
    VeryImportant;

  S := '';

  Repeat
    C := LoCase (ComReadKey);

    Case C Of

      #31..#255: If Length (S) < 15 Then
                 Begin
                   S := S + C;
                   ComWrite ('þ', 0);
                   If StatusBar Then
                     FastWrite (Pad (S, 40), ScrY, PwPos,
                       Cnf. ColorScheme [cmStatusLine]);
                 End;

             #8: If Length (S) > 0 Then
                 Begin
                   SetLength (S, Length (S) - 1);
                   ComWrite (#8#32#8, 0);
                   If StatusBar Then
                     FastWrite (Pad (S, 40), ScrY, PwPos,
                       Cnf. ColorScheme [cmStatusLine]);
                 End;

            #24: If Length (S) > 0 Then
                 Begin
                   ComWrite (EmuCursorLeft (Length (S)), 0);
                   ComWrite (EmuClrEOL, 0);
                   S := '';
                   If StatusBar Then
                     FastWrite (Replicate (' ', 40), ScrY, PwPos,
                       Cnf. ColorScheme [cmStatusLine]);
                 End;
    End;
  Until C = #13;

  If NewUser And (Length (S) < Cnf. PassLength) Then
  Begin
    ComWriteLn (#13#10 + PlaceSubStrNoCase (lang (laPassTooShort), '@MinLen',
      Long2Str (Cnf. PassLength)), eoMacro + eoCodes);
    Goto Loop;
  End;

  EnterPass := S;

  If StatusBar Then
  Begin
    FastWrite (Replicate (' ', ScrX + 1), ScrY, 1, $07);
    FastWrite (Replicate (' ', ScrX + 1), ScrY + 1, 1, $07);
  End;

  StatusBarEnable := True;
  EnteringPass := False;

  ShowStatusBar;
  ComWriteLn ('', 0);
End;

Function TLangDescCollection. Compare (Key1, Key2: Pointer): Integer;
Begin
  Compare := StrCompare (PLangDesc (Key1)^. Name, PLangDesc (Key2)^. Name);
End;

Procedure TLangDescCollection. FreeItem (Item: Pointer);
Begin
  If Item <> Nil Then
    Dispose (PLangDesc (Item));
End;

Procedure SetXLAT (Const FName: String);
Var
  i : Integer;
  S : String;

Begin
  If FName <> '' Then
  Begin
    S := UpString (FName);

    For i := 0 To XLATs^. Count-1 Do
      With PXLATrec (XLATs^. At (i))^ Do
        If UpString (FileName) = S Then
        Begin
          ScreenOut. XLAT := Table;
          ScreenOut. XLATenabled := True;
          Exit;
        End;
  End;

  ScreenOut. XLATenabled := False;
End;

Procedure ChooseLang;
Var
  LangColl : PLangDescCollection;
  MenuLang : PNotSortedCollection;
  PLD      : PLangDesc;
  i        : Integer;
  DirInfo  : SearchRec;
  CC       : tConfigParser;
  S1, S2   : String;

Label
  ProcExit;

Begin
  LangColl := New (PLangDescCollection, Init (2, 2));
  MenuLang := New (PNotSortedCollection, Init (4, 4));

  FindFirst (Cnf. LngPath + '*.lng', AnyFile, DirInfo);

  While DosError = 0 Do
  Begin
    If ParserOpen (CC, Cnf. LngPath + DirInfo. Name, tpoWriteLog) Then
    Begin
      While Not ParserEnd (CC) Do
      Begin
        S1 := ParserRead (CC, S2);
        If (S2 = 'MAIN') And (S1 = 'LANGUAGE') Then
        Begin
          ParserGetParam (CC, tptString, '', S1);
          New (PLD);
          PLD^. Name := S1;
          PLD^. FileName := DirInfo. Name;
          LangColl^. Insert (PLD);

          Break;
        End;
      End;

      ParserClose (CC);
    End;

    FindNext (DirInfo);
  End;

{$IFNDEF MSDOS}
  FindClose (DirInfo);
{$ENDIF}

  If LangColl^. Count < 2 Then
  Begin
    If LangColl^. Count < 1 Then
    Begin
      LogWrite ('!', sm (smNoOneLngFile));
      Halt (202);
    End;

    i := 1;
  End Else
  Begin
    For i := 0 To LangColl^. Count-1 Do
      MenuLang^. Insert (NewStr (PLangDesc (LangColl^. At (i))^. Name));

    i := ComMenu (Cnf. ChoiceLanguageStr1, Cnf. ChoiceLanguageStr2, MenuLang);
    If i < 1 Then
      Goto ProcExit;
  End;

  S1 := UpString (JustName (PLangDesc (LangColl^. At (i - 1))^. FileName));
  If (UpString (R. Lang) = S1) And (lang (laAskXLAT) <> 'YES') Then
    Goto ProcExit;

  R. Lang := S1;
  ReadLanguage (Language, Cnf. LngPath + R. Lang + '.lng');

  R. XLAT := '';
  If lang (laAskXLAT) = 'YES' Then
  Begin
    MenuLang^. FreeAll;
    MenuLang^. Insert (NewStr (lang (laRawXLAT)));

    For i := 0 To XLATs^. Count-1 Do
      MenuLang^. Insert (NewStr (PXLATrec (XLATs^. At (i))^. Name));

    If MenuLang^. Count > 1 Then
    Begin
      i := ComMenu ('|' + lang (laXLATs), lang (laChooseXLAT), MenuLang);
      If i > 1 Then
        R. XLAT := PXLATrec (XLATs^. At (i - 2))^. FileName;
    End;
  End;

  SetXLAT (R. XLAT);

ProcExit:
  Dispose (MenuLang, Done);
  Dispose (LangColl, Done);
End;

Procedure SetLanguage;
Var
  L : String;

Begin
  L := Cnf. LngPath + R. Lang + '.lng';
  If FileExists (L) Then
  Begin
    ReadLanguage (Language, L);
    If R. XLAT <> '' Then
      SetXLAT (R. XLAT);
  End
  Else
    ChooseLang;
End;

Procedure ChooseEmu;
Var
  MenuEmu    : PNotSortedCollection;
  i          : Integer;
  EmuChoices : Array [1..3] Of tEmulation;

  Procedure AddItem (Item: tEmulation);
  Begin
    Inc (i);
    EmuChoices [i] := Item;
    MenuEmu^. Insert (NewStr (EmuName [Item]));
  End;

Begin
  MenuEmu := New (PNotSortedCollection, Init (3, 0));

  If Not (Cnf. eaAnsi Or Cnf. eaAvatar Or Cnf. eaTty) Then
    Cnf. eaTty := True;

  i := 0;
  If Cnf. eaAnsi Then
    AddItem (teAnsi);
  If Cnf. eaAvatar Then
    AddItem (teAvatar);
  If Cnf. eaTty Then
    AddItem (teTty);

  If i = 1 Then
    R. Emu := EmuChoices [1]
  Else
  Begin
    i := ComMenu ('', lang (laChooseEmulation), MenuEmu);
    If i > 0 Then
      R. Emu := EmuChoices [i];
  End;

  Dispose (MenuEmu, Done);
End;

Procedure ChangeParam (ParNum: Byte);
Var
  Was, Now : String;

  Procedure GetPassword;
  Var
    Password, Pas1 : String [15];

  Label
    GetPwdAgain;

  Begin
    Was := R. Password;

    If (R. Password <> '') And
       Not InList (Cnf. BadPasswordsList, R. Password) Then
    Begin
      KolPass := 0;
      If EnterPass (lang (laPassword), False) <> R. Password Then
      Begin
        Message (lang (laPasswordNotValid));
        KolPass := 0;
        Now := Was;
        Exit;
      End;
    End;

  GetPwdAgain:
    KolPass := 0;
    Password := EnterPass (lang (laChoosePassword), Registering);

    If InList (Cnf. BadPasswordsList, Password) Then
    Begin
      ComWriteLn ('|' + lang (laForbiddenPassword) + '|', eoMacro + eoCodes);
      Goto GetPwdAgain;
    End;

    KolPass := 0;
    Pas1 := EnterPass (lang (laRepeatPassword), Registering);

    If Password <> Pas1 Then
    Begin
      Message ('|' + lang (laPasswordsNotSame));
      Goto GetPwdAgain;
    End;

    KolPass := 0;
    R. Password := Password;
    Now := R. Password;
  End;

  Procedure GetLines;
  Var
    Err : SysInt;
    S   : String;

  Begin
    Was := Long2Str (R. Lines);
    S := Trim (GetAnswer (lang (laDispLines) + ' ', 3, ofAllowEmpty, Was));

    If S = '' Then
      R. Lines := Cnf. DisplayLines
    Else
    Begin
      Val (S, R. Lines, Err);
      If Err <> 0 Then
        R. Lines := Cnf. DisplayLines;
    End;

    Now := Long2Str (R. Lines);
  End;

  Procedure GetLocation;
  Var
    S : String;

  Begin
    Was := R. Location;

    S := PlaceSubStr (Trim (GetAnswer (lang (laLocation), 50, 0, R. Location)),
      '$', '');
    If S <> '' Then
      R. Location := S;

    Now := R. Location;
  End;

  Procedure GetOrganization;
  Var
    S : String;

  Begin
    Was := R. Organization;

    S := PlaceSubStr (Trim (GetAnswer (lang (laOrganization), 50, 0,
      R. Organization)), '$', '');
    If S <> '' Then
      R. Organization := S;

    Now := R. Organization;
  End;

  Procedure GetAddresses;

    Function GetAdr (Const Query: String; Default: String): String;
    Begin
      ComWrite (Query, eoMacro + eoCodes + eoNoFlush);
      ComReadLn (Default, 50, ofAllowEmpty);
      GetAdr := PlaceSubStr (Trim (Default), '$', '');
    End;

  Var
    S : String;

  Begin
    Was := R. Address1 + ', ' + R. Address2 + ', ' + R. Address3;

    SmartLine;
    ComWrite (lang (laFIDOAddress), eoMacro + eoCodes);

    S := GetAdr (lang (laAddress1), R. Address1);

    If Not (Registering And (S = '')) Then
    Begin
      R. Address1 := S;

      S := GetAdr (lang (laAddress2), R. Address2);

      If Not (Registering And (S = '')) Then
      Begin
        R. Address2 := S;

        S := GetAdr (lang (laAddress3), R. Address3);

        If Not (Registering And (S = '')) Then
          R. Address3 := S;
      End;
    End;

    Now := R. Address1 + ', ' + R. Address2 + ', ' + R. Address3;
  End;

  Procedure GetBPhone;
  Var
    S : String;

  Begin
    Was := R. BPhone;

    Repeat
      S := Trim (GetAnswer (lang (laDataPhone), 15, 0, R. BPhone));
    Until PhoneValid (S);

    R. BPhone := S;
    Now := R. BPhone;
  End;

  Procedure GetHPhone;
  Var
    S : String;

  Begin
    Was := R. HPhone;

    Repeat
      S := Trim (GetAnswer (lang (laHomePhone), 15, 0, R. HPhone));
    Until PhoneValid (S);

    R. HPhone := S;
    Now := R. HPhone;
  End;

  Procedure GetBirthDate;
  Var
    S, Dt : String;

  Begin
    If R. BirthDate = 0 Then
      Dt := ''
    Else
      Dt := Long2Date (R. BirthDate, Cnf. DateMask);

    Was := Dt;

    Repeat
      S := Trim (GetAnswer (PlaceSubStrNoCase (lang (laBirthDate), '@DateMask',
        Cnf. DateMask), Length (Cnf. DateMask), 0, Dt));
    Until DateMaskMatch (S, Cnf. DateMask);

    R. BirthDate := Date2Long (ReFormatDate (S, Cnf. DateMask,
      DefaultDateMask));
    Now := S;
  End;

  Procedure GetAlias;
  Var
    S : String;

  Label
    ReAlias;

  Begin
    Was := R. Alias;

  ReAlias:
    S := PlaceSubStr (GetAnswer (lang (laAlias), 15, ofAllowEmpty, R. Alias),
      '$', '');
    If UpString (S) = UpString (R. Alias) Then
    Begin
      Now := Was;
      Exit;
    End;

    If (S <> '') And Is_User (S, True) Then
    Begin
      ComWriteLn (lang (laAliasExist), eoMacro + eoCodes);
      Goto ReAlias;
    End;

    R. Alias := S;
    Now := R. Alias;
    If Not Registering Then
      SaveUser (R);
  End;

Label
  EoP,
  Done;

Begin
  Was := '';
  Now := '';

  Case ParNum Of

    pProtocol    : Begin
                     Was := ProtocolDef. Name;
                     SetProtocol (#0);
                     Now := ProtocolDef. Name;
                   End;

    pLang        : Begin
                     Was := R. Lang;
                     ChooseLang;
                     Now := R. Lang;
                   End;

    pEmu         : Begin
                     If Registering Then
                     Begin
                       If EMSI. Session Then
                         Exit;

                       If Cnf. ANSI = atYes Then
                       Begin
                         R. Emu := teAnsi;
                         Goto Done;
                       End
                       Else
                         If Cnf. ANSI = atNo Then
                         Begin
                           R. Emu := teTty;
                           Goto Done;
                         End;
                     End;

                     Was := EmuName [R. Emu];
                     ChooseEmu;
                     Now := EmuName [R. Emu];

                     If R. Emu = teTty Then
                     Begin
                       TextAttr := 3;

                       If R. FSEditor Then
                       Begin
                         ComWriteLn ('|' + lang (lafsANSIreq), eoMacro +
                           eoCodes);
                         R. FSEditor := Query (lang (laUseFSEditor), False,
                           ofFramed);
                       End;
                     End;
                   End;

    pPassword    : Begin
                     If Registering And (EMSI. Session Or (R. Password <> ''))
                     Then
                       Exit;

                     GetPassword;
                   End;

    pLines       : Begin
                     If Registering Then
                     Begin
                       If EMSI. Session Then
                         Exit;

                       If Not Cnf. DispAsk Then
                       Begin
                         R. Lines := Cnf. DisplayLines;
                         Goto Done;
                       End;
                     End;

                     GetLines;
                   End;

    pMore        : Begin
                     If Registering Then
                     Begin
                       If EMSI. Session Then
                         Exit;

                       If Cnf. More in [atYes, atNo] Then
                       Begin
                         R. More := YNA2Bool (Cnf. More);
                         Goto Done;
                       End;
                     End;

                     Was := BoolMsg [R. More];
                     R. More := Query (lang (laPausing), True, ofFramed);
                     Now := BoolMsg [R. More];
                   End;

    pLocation    : Begin
                     If Registering And (Not Cnf. AskLocation Or
                        (EMSI. Session And (R. Location <> '')))
                     Then
                       Exit;

                     GetLocation;
                   End;

    pOrganization: Begin
                     If Registering And Not Cnf. Organization Then
                       Exit;

                     GetOrganization;
                   End;

    pAddress     : Begin
                     If Registering And Not Cnf. Address Then
                       Exit;

                     GetAddresses;
                   End;

    pBPhone      : Begin
                     If Registering And (Not Cnf. DataPhone Or
                        (EMSI. Session And (R. BPhone <> '')))
                     Then
                       Exit;

                     GetBPhone;
                   End;

    pHPhone      : Begin
                     If Registering And (Not Cnf. VoicePhone Or
                        (EMSI. Session And (R. HPhone <> '')))
                     Then
                       Exit;

                     GetHPhone;
                   End;

    pHotKeys     : Begin
                     If Registering Then
                     Begin
                       If EMSI. Session Then
                         Exit;

                       If Cnf. HotKeys in [atYes, atNo] Then
                       Begin
                         R. HotKeys := YNA2Bool (Cnf. HotKeys);
                         Goto Done;
                       End;
                     End;

                     Was := BoolMsg [R. HotKeys];
                     R. HotKeys := Query (lang (laUseHotKeys), True, ofFramed);
                     If R. HotKeys Then
                     Begin
                       KeyBuffer := '';
                       HotKeysStr := '';
                     End;
                     Now := BoolMsg [R. HotKeys];
                   End;

    pBirthDate   : Begin
                     If Registering And (Not Cnf. BirthDate Or
                        (EMSI. Session And (R. BirthDate <> 0)))
                     Then
                       Exit;

                     GetBirthDate;
                   End;

    pFrames    : Begin
                   If Registering And (Cnf. Frames in [atYes, atNo]) Then
                   Begin
                     R. Frames := YNA2Bool (Cnf. Frames);
                     Goto Done;
                   End;

                   Was := BoolMsg [R. Frames];
                   R. Frames := Query (lang (laUseFrames), False, ofFramed);
                   Now := BoolMsg [R. Frames];
                 End;

    pAlias     : Begin
                   If Registering And (Not Cnf. AskAlias) Then
                     Exit;

                   GetAlias;
                 End;

    pFSEditor  : Begin
                   If Registering And (Cnf. FSEditor in [atYes, atNo]) Then
                   Begin
                     R. FSEditor := YNA2Bool (Cnf. FSEditor);
                     Goto Done;
                   End;

                   Was := BoolMsg [R. FSEditor];
                   R. FSEditor := Query (lang (laUseFSEditor), True, ofFramed);
                   Now := BoolMsg [R. FSEditor];
                 End;
  End;

EoP:
  If Registering Then
  Begin
    If Not (R. Frames Or (ParNum in [pMore, pHotKeys, pFrames, pFSEditor])) Then
      ComWriteLn ('', 0);
  End
  Else
    If Was <> Now Then
    Begin
      LogWrite ('+', sm (smParam) + GetStr (ParamNames^. At (ParNum-1)) +
        sm (smParamChanged));
      LogWrite ('@', PlaceSubStr (PlaceSubStr (sm (smParamChangeInfo), '%1',
        Was), '%2', Now));
    End;

Done:
  UpdateUserMacro;
End;

Procedure Register;
Const
  Registration : String [14] = 'KPMHONLABCEFGD';

Var
  Param : Byte;

Label
  Reg;

Begin
  If Cnf. Private Then
  Begin
    If Query (lang (laPrivate), True, ofFramed) Then
    Begin
      LogWrite ('!', sm (smRejectNew));
      Msg2SysOp ('New user.', 'priv_bbs');
    End;

    NormExit;
  End;

  R. FileGroup := 0;
  R. FileArea := 1;

  R. MsgGroup := 0;
  R. MsgArea := 1;

  R. Protocol := #0;

  EmuDispFile ('~newuser');
  ExecScript ('newuser');
  ExecRexx ('newuser');

  If GetDoReg (R) Then
  Begin
    SetLanguage;
    ComWriteLn ('|' + lang (laRegResume) + '|', eoMacro + eoCodes);
  End Else
  Begin
    LogWrite ('~', sm (smNewReg));
    SetTitle ('registering as a new user');
    Registering := True;
    RegLet := 1;
  End;

Reg:
  While RegLet <= Length (Registration) Do
  Begin
    Param := Letters [Registration [RegLet]];

    LogWrite ('&', 'Asking registration question about ' + GetStr
      (ParamNames^. At (Param - 1)));

    If (Registration [RegLet] = 'D') And EMSI. Session Then
      R. Password := EMSI. Password
    Else
      ChangeParam (Param);

    Inc (RegLet);
  End;

  RegLet := 255;

  If Cnf. AcceptReg Then
    If Not EMSI. Session And
       Not Query (lang (laIsRegCorrect), True, ofFramed) Then
    Begin
      RegLet := 1;
      R. Password := '';
      Goto Reg;
    End;

  If InList (Cnf. GoodUsersList, R. Name) Then
  Begin
    R. Security := Cnf. GoodSecurity;
    R. Flags := Cnf. GoodFlags;
  End Else
  Begin
    R. Security := Cnf. Security;
    R. Flags := Cnf. Flags;
  End;

  R. TimeUsedToday := 0;
  R. LastDate := DateL;
  R. LastTime := CurrTime2Word;
  R. FirstDate := 0;
  R. LastRead := NextLastRead;

  If Not ReadLimit (Lim, R. Security) Then
  Begin
    LogWrite ('!', PlaceSubStr (PlaceSubStr (sm (smNoSec), '%level%',
      Long2Str (R. Security)), '%limfile%', Cnf. LimitsFile));
    Halt (203);
  End;

  R. TotalTime := Lim. Time * 60;
  R. DailySize := Lim. KBLimit;
  R. ReReadLimit := False;

  EnterTime := MidSec;
  TimeCount := True;

  SaveUser (R);

  LogWrite ('@', 'ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ');
  If R. Alias <> '' Then
    LogWrite ('@', Pad (Sm (smueAlias), 16) + ': ' + R. Alias);
  If R. Birthdate <> 0 Then
    LogWrite ('@', Pad (Sm (smueBirthdate), 16) + ': ' +
      Long2Date (R. BirthDate, Cnf. DateMask));
  If R. Location <> '' Then
    LogWrite ('@', Pad (Sm (smueLocation), 16) + ': ' + R. Location);
  If R. Organization <> '' Then
    LogWrite ('@', Pad (Sm (smueOrganization), 16) + ': ' +
      R. Organization);
  If R. Address1 <> '' Then
    LogWrite ('@', Pad (Sm (smue1stAddress), 16) + ': ' + R. Address1);
  If R. Address2 <> '' Then
    LogWrite ('@', Pad (Sm (smue2ndAddress), 16) + ': ' + R. Address2);
  If R. Address3 <> '' Then
    LogWrite ('@', Pad (Sm (smue3rdAddress), 16) + ': ' + R. Address3);
  If R. HPhone <> '' Then
    LogWrite ('@', Pad (Sm (smueHomePhone), 16) + ': ' + R. HPhone);
  If R. BPhone <> '' Then
    LogWrite ('@', Pad (Sm (smueDataPhone), 16) + ': ' + R. BPhone);
  LogWrite ('@', Pad (Sm (smueLines), 16) + ': ' + Long2Str (R. Lines));
  LogWrite ('@', Pad (Sm (smueLanguage), 16) + ': ' + R. Lang);
  LogWrite ('@', Pad (Sm (smueEmulation), 16) + ': ' + EmuName [R. Emu]);
  LogWrite ('@', Pad (Sm (smuePausing), 16) + ': ' + BoolMsg [R. More]);
  LogWrite ('@', Pad (Sm (smueHotKeys), 16) + ': ' + BoolMsg [R. HotKeys]);
  LogWrite ('@', Pad (Sm (smueFSeditor), 16) + ': ' + BoolMsg [R. FSeditor]);
  LogWrite ('@', 'ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ');

  Registering := False;
End;

Procedure Login;
Var
  PrevLang : String [8];
  S        : String;

Label
  ReLogOn,
  ReGetPass,
  PassEnd;

Begin
{$IFDEF OS2}
  InitRexx;
{$ENDIF}

  If Cnf. eaAnsi Then
    R. Emu := teAnsi
  Else
    If Cnf. eaAvatar Then
      R. Emu := teAvatar
    Else
      If Cnf. eaTty Then
        R. Emu := teTty;

  R. HotKeys := Cnf. HotKeys in [atYes, atAsk];
  SetInputCap (NoCaps, AllChars);

  ComWrite (EmuColor ($07), 0);
  Cls;

  ExecScript ('logo');
  ExecRexx ('logo');
  EmuDispFile (Cnf. Logo);

  ComWriteLn (EmuRelColor ($07), 0);

  InConference := False;
  VIPcheck := False;

  If EMSI. Allowed And Not Local Then
  Begin
    ComWrite ('**EMSI_IRQ8E08'#13, 0);
    If R. Emu = teTty Then ComWrite ('              '#13, 0)
                      Else ComWrite (EmuClrEOL, 0);
  End;

  If Cnf. ShowVer Then
    ComWriteLn (NameVer, 0);

  ReqName := DelSpaces (ReqName);

ReLogOn:
  Registering := True;
  TimeCount := False;

  R. Name := '';
  R. Lines := Cnf. DisplayLines;
  R. Frames := Cnf. Frames = atYes;

  ComWrite (Cnf. LoginString, eoMacro + eoCodes);

  If ReqName = '' Then
  Begin
    If Cnf. CapitalizeNames Then SetInputCap (Proper, LettersOnly)
                            Else SetInputCap (NoCaps, LettersOnly);
    S := '';
    EMSI. Allowed := True;
    ComReadLn (S, 36, 0);
    EMSI. Allowed := False;
    SetInputCap (NoCaps, AllChars);

    R. Name := DelSpaces (S);
    If R. Name = '' Then
      Goto ReLogOn;

    If Cnf. CapitalizeNames Then
      R. Name := PrString (R. Name);
  End Else
  Begin
    ComWriteLn (Pad (ReqName, 36), 0);
    R. Name := ReqName;
    ReqName := '';
  End;

  LogWrite ('~', R. Name + sm (smOnLine));

  If InList (Cnf. BadUsersList, R. Name) Then
  Begin
    EmuDispFile ('~baduser');
    ExecScript ('baduser');
    ExecRexx ('baduser');
    LogWrite ('!', sm (smlBadUser));
    Pause (2000);
    NormExit;
  End;

  If Not Cnf. OneWordNames And (WordCount (R. Name, SpaceOnly) < 2) And
     Not (Cnf. Aliases And Is_User (R. Name, Cnf. Aliases)) Then
  Begin
    ComWriteLn (Cnf. NoOneWordNames, eoMacro + eoCodes);
    Goto ReLogOn;
  End;

  PrevLang := R. Lang;

  If Not Is_User (R. Name, Cnf. Aliases) Then
    If Not EMSI. Session Then
    Begin
      If DoRegExist Then
      Begin
        Register;
        Goto PassEnd;
      End;

      ChooseLang;

      ComWriteLn ('|' + lang (laYourNameNotFound), eoMacro + eoCodes);
      ComWriteLn (lang (laNameEntered) + R. Name, eoMacro + eoCodes);
      If R. Frames And (R. Emu <> teTty) Then
        ComWriteLn (#10#10, 0);
      If Query (lang (laWantToReg), True, ofFramed) Then
      Begin
        Register;
        mL_Init (BbsLine);
        Goto PassEnd;
      End Else
      Begin
        SmartLine;
        Goto ReLogOn;
      End;
    End
    Else
      Register;

  GetUser (R. Name, R, Cnf. Aliases);

  If R. Emu = teTTY Then
    TextAttr := 3;

  If InList (Cnf. GoodUsersList, R. Name) Then
  Begin
    EmuDispFile ('~gooduser');
    ExecScript ('gooduser');
    ExecRexx ('gooduser');
    LogWrite ('!', sm (smlGoodUser));
  End;

  Suxx := InList (Cnf. SuxxUsersList, R. Name);
  If Suxx Then
    LogWrite ('!', sm (smlSuxxUser));

  If BbsLine <> 0 Then
    If Not mL_Init (BbsLine) Then
    Begin
      LogWrite ('!', sm (smOnAnotherLine));
      ComWriteLn ('|' + lang (laOnAnotherLine), eoMacro + eoCodes);
      Pause (2000);
      NormExit;
    End;

  If R. Password = '' Then
  Begin
    If Not R. Guest Then
      ChangeParam (pPassword);
    Goto PassEnd;
  End;

  KolPass := 0;
  VIPinformed := False;

  If Not (Local And Cnf. FastLogon) Then
  Begin

  ReGetPass:
    VIPcheck := True;
    If Not (EMSI. Session And (KolPass = 0)) Then
      EMSI. Password := EnterPass (lang (laPassword), False)
    Else
      VeryImportant;
    VIPcheck := False;

    If UpString (EMSI. Password) <> UpString (R. Password) Then
    Begin
      SmartLine;
      ComWriteLn (lang (laPasswordNotValid), eoMacro + eoCodes);
      LogWrite ('!', PlaceSubStr (PlaceSubStr (sm (smInvalidPass), '%pass1%',
        EMSI. Password), '%pass2%', R. Password));
      If KolPass = 0 Then
        Inc (KolPass);
      Goto ReGetPass;
    End;
  End;

PassEnd:
  If InList (Cnf. BadPasswordsList, R. Password) Then
  Begin
    ComWriteLn ('|' + lang (laForbiddenPassword), eoMacro + eoCodes);
    ChangeParam (pPassword);
  End;

  Registering := False;
  KolPass := 0;

  If (UpString (R. Lang) <> UpString (PrevLang)) Or (XLATs^. Count <> 0) Then
    SetLanguage;

  If Not (R. Emu in [teAnsi..teAvatar]) Or
     (Not Cnf. eaAnsi And (R. Emu = teAnsi)) Or
     (Not Cnf. eaAvatar And (R. Emu = teAvatar)) Or
     (Not Cnf. eaTty And (R. Emu = teTty))
  Then
    ChangeParam (pEmu);

  If R. Security <= 0 Then
  Begin
    LogWrite ('!', sm (smLockOut));
    EmuDispFile ('~lockout');
    Pause (2000);
    NormExit;
  End;

  If Not ReadLimit (Lim, R. Security) Then
  Begin
    LogWrite ('!', PlaceSubStr (PlaceSubStr (sm (smSecNotFound), '%level%',
      Long2Str (R. Security)), '%limfile%', NiceFileName (Cnf. LimitsFile, 30)));
    ErrorExit;
  End;

  If R. ReReadLimit Then
  Begin
    SetSecurity;
    R. ReReadLimit := False;
  End;

  TimeCount := True;
  EnterTime := MidSec;
  Ent := DateL;

  If (R. LastDate <> Ent) Or R. Guest Or (R. FirstDate = 0) Then
  Begin
    If R. FirstDate = 0 Then
      R. FirstDate := Ent;

    R. TotalTime := Lim. Time * 60;
    R. DailySize := Lim. KBLimit;
    R. TimeUsedToday := 0;
    R. TodayK := 0;
  End;

  {!!BUG ‚ íâ®¬ ¬¥áâ¥ ¢ë«¥â ¥¬ ¯à¨ à¥«®£¨­¥ (­® ¬®¦¥â ¡ëâì ¡ £ ¨ ­¥ §¤¥áì) BUG!!}
  If R. TotalTime <= 0 Then
  Begin
    ComWriteLn (lang (laTimeLimit), eoMacro + eoCodes);
    LogWrite ('+', sm (smTimeLimit));
    LogWrite ('&', 'Source=tmainovr.pas; MidSec=' + Long2Str (MidSec) + '; EnterTime=' + Long2Str
      (EnterTime) + '; R.TotalTime=' + Long2Str(R. TotalTime));
    R. TotalTime := 0;
    Pause (2000);
    NormExit;
  End;

  If Not Local And (GetConnectSpeed < Lim. MinSpeed) Then
  Begin
    EmuDispFile ('~tooslow');
    LogWrite ('!', sm (smlTooSlow));
    NormExit;
  End;

  If Not MatchTimeArray (Lim. LoginTime) Then
  Begin
    EmuDispFile ('~nlogtime');
    ComWriteLn (#13#10#10 + lang (laLoginNAllow), eoMacro + eoCodes);
    LogWrite ('+', sm (smLoginNAllow));
    NormExit;
  End;

  SaveUser (R);

  SetDefaultAreas;

  FillChar (ProtocolDef, SizeOf (ProtocolDef), #0);
  If R. Protocol <> #0 Then SetProtocol (R. Protocol)
                       Else ProtocolDef. Name := 'None';

  If (R. NoCalls = 0) And Not R. Guest Then
  Begin
    ExecScript ('regend');
    ExecRexx ('regend');
    EmuDispFile ('~regend');
  End;

  Inc (R. NoCalls);
  mL_EnableMsg;

  Inc (Sys. TotalCalls);
  qwkReadList;

  If Not R. Guest Then
    Upgrade;

  UpdateUserMacro;

  If RestoreTagList Then
  Begin
    UpdateUserMacro;
    If Cnf. DlOnStart Then
      DownLoad;
  End;

  MenuFile := LoString (Cnf. MainMenu);

  If Not (Local And Cnf. FastLogon) Then
  Begin
    If ToEventTime <> 0 Then
    Begin
      SmartLine;
      Message (PlaceSubStrNoCase (lang (laToEventLeft), '@ToEvent',
        Long2Str (ToEventTime)));
    End;

    EmuDispFile ('~welcome');
    ComWriteLn ('', 0);
    ExecScript ('welcome');
    ExecRexx ('welcome');

    If EMSI. Session Then
    Begin
      If EMSI. CheckMail Then
        SearchPrivate;
      If EMSI. CheckNewFiles Then
        GlobalSearch (AllFilesMask, fsDate, atYes, '');
    End Else
    Begin
      If (Cnf. ScanPrivMail = atYes) Or ((Cnf. ScanPrivMail = atAsk) And
         Query (lang (laSearchPrivateMsg), True, ofFramed))
      Then
        SearchPrivate;

      If R. NoCalls > 1 Then
        If Cnf. ScanNewFiles <> atNo Then
          GlobalSearch (AllFilesMask, fsDate, Cnf. ScanNewFiles, '1');
    End;

    If Cnf. ShowNews <> tnNo Then
      News (R. NoCalls <> 1);
  End;

  EmuDispFile ('~start');
  ExecScript ('start');
  ExecRexx ('start');

  mL_SendMsg (0, trcSysMsgPrefix + '** ' + R. Name + lang (laTRCline) +
    Long2Str (BbsLine) + lang (laTRCentered), mtConference);
End;

Procedure fNews (Logon: Boolean); Far;
Var
  DateMaskLen         : Byte;
  New, Cont, Skipped,
  SomeShowed          : Boolean;
  F                   : Text;
  S, T, Last          : String;

Begin
  If Trim (lang (laNews)) = '' Then
    Exit;

  Assign (F, lang (laNews));
  ReSet (F);
  DOSerror := IOResult;

  If DOSerror <> 0 Then
  Begin
    LogWrite ('!', sm (smFile) + lang (laNews) + sm (smNotFound));
    LogWrite ('&', 'IOResult: ' + Long2Str (DOSerror));
    Exit;
  End;

  SetTitle ('reading news');

  Cls;
  New := True;
  Skipped := False;
  SomeShowed := False;
  Cont := True;
  DateMaskLen := Length (Cnf. DateMask);
  Last := Long2Date (R. LastDate, 'DD-MM-YYYY') + ' ' + Word2Time (R. LastTime);

  If Cnf. DebugFiles Then
    SetDebugFile (lang (laNews));

  While Not EoF (F) Do
  Begin
    ReadLn (F, S);
    PlaceSubStrP (S, #9, ReplaceTabSpaces);
    T := Trim (S);

    If T <> '' Then
    Begin
      If T [1] = ';' Then
        Continue;

      If T = '/***/' Then
      Begin
        New := True;
        Skipped := False;
        Continue;
      End;
    End;

    If DrawAborted Then
      Break;

    If New Then
    Begin
      If Logon And ((Cnf. ShowNews = tnLast) And (CompareDates (Last,
         ReFormatDate (ExtractWord (1, T, SpaceOnly), Cnf. DateMask,
           'DD-MM-YYYY') + ' ' + ExtractWord (2, T, SpaceOnly)) <> 2)) Then
      Begin
        New := False;
        Skipped := True;
        Continue;
      End;

      If Not Skipped Then
        If SomeShowed Then
        Begin
          ComWriteLn (EmuRelColor (Cnf. ColorScheme [nwFrames]) +
            Replicate ('Ä', 79), 0);

          If Cnf. NewsPauseAsk Then
            Cont := fQuery (lang (laNextBulletin), True, 0)
          Else
          Begin
            Cont := True;
            ComWrite (lang (laEnterForCont), eoMacro + eoCodes);
            While (ComReadKey <> #13) Do;
          End;
        End;

      If Not Cont Then
      Begin
        Close (F);
        Exit;
      End;

      Cls;
      SomeShowed := True;

      ComWriteLn (EmuRelColor (Cnf. ColorScheme [nwFrames]) + '     Ú' +
        Replicate ('Ä', Length (Cnf. DateMask) + 2) + '¿', 0);
      ComWrite ('ÄÄÄÄÄ´ ' + EmuRelColor (Cnf. ColorScheme [nwDate]) +
        Pad (Copy (T, 1, DateMaskLen), DateMaskLen), eoNoFlush);
      ComWriteLn (EmuRelColor (Cnf. ColorScheme [nwFrames]) + ' Ã' +
        Replicate ('Ä', 70 - Length (Cnf. DateMask)), 0);
      ComWriteLn ('     À' + Replicate ('Ä', Length (Cnf. DateMask) + 2) + 'Ù'
        + EmuRelColor ($03), 0);

      InitMore (WhereY - 1);
      New := False;
    End
    Else
      If Not Skipped Then
        ComWriteLn (S, eoMacro + eoCodes);

    If Not Skipped Then
      If Not More Then
      Begin
        Skipped := True;
      End;
  End;

  If SomeShowed Then
  Begin
    ComWrite (EmuRelColor (Cnf. ColorScheme [nwFrames]) + Replicate ('Ä', 79),
      0);
    Message ('');
  End;

  Close (F);
End;

Procedure Information;
Begin
  Cls;
  ComWriteLn ('|\15$BBSN running on ' + NameVer, eoMacro + eoCodes);
  If BbsLine <> 0 Then
    ComWriteLn ('Line Number ' + Long2Str (BbsLine), eoMacro + eoCodes);
  If Task. OS <> 0 Then
  Begin
    ComWrite ('|\10Operating system: ' + MTaskers [Task. OS],
      eoMacro + eoCodes);
    If Task. Version <> 0 Then
      ComWriteLn (' ' + Long2Str (Hi (Task. Version)) + '.'
        + Long2Str (Lo (Task. Version)), eoMacro + eoCodes)
    Else
      ComWriteLn ('', eoMacro + eoCodes);
  End;
  Message ('');
End;

Procedure DoFinger (Q: String);
Begin
  If Q = '' Then
  Begin
    ComWrite ('\11Finger query: ', eoMacro + eoCodes + eoNoFlush);
    ComReadLn (Q, 50, ofAllowEmpty);
    Q := Trim (Q);
  End;
End;

Procedure DoMenu;
Const
  LightedArrItem : Integer = 0;

Var
  wFin : Boolean;

  Procedure PopMenu;
  Var
    P : PString;
    i : Byte;

  Begin
    If Menus^. Count > 0 Then
    Begin
      P := Menus^. At (Menus^. Count-1);
      i := Pos (' ', P^);
      FileName := Copy (P^, 1, i - 1);
      LightedArrItem := Str2Long (Copy (P^, i + 1, 255));
      Menus^. AtFree (Menus^. Count-1);
    End
    Else
      NormExit;
  End;

  Function ExecMenuAction (Action: Byte; Param: String): Boolean;

    Procedure DL;
    Var
      AllSize : LongInt;
      DirInfo : SearchRec;
      FileRec : TTagFileRec;

    Begin
      FindFirst (Param, AnyFile-VolumeID-Directory, DirInfo);

      While DOSerror = 0 Do
      Begin
        AllSize := R. TodayK + (SizeOfAll + DirInfo. Size) Shr 10;

        If AllSize < R. DailySize Then
        Begin
          If Local Or Not (Cnf. CheckDlTime And (AllSize > Round (
            (GetConnectSpeed / 10 * (R. TotalTime - TimeDiff (EnterTime,
               MidSec))) / 1024))) Then
          Begin
            FileRec. GroupNum := 0;
            FileRec. AreaNum := 0;
            FileRec. Size := DirInfo. Size;
            FileRec. PathName := NewStr (AddBackSlash (JustPathName (Param)) +
              DirInfo. Name);
            FileRec. FromName := Nil;
            FileRec. Free := False;
            InsertInTagList (FileRec);
          End
          Else
            Message (lang (laDLTimeLimit));
        End
        Else
          Message (lang (laDLLimitExceed));

        FindNext (DirInfo);
      End;

    {$IFNDEF MSDOS}
      FindClose (DirInfo);
    {$ENDIF}
      DownLoad;
    End;

    Procedure UL;
    Var
      SavePath : String;

    Begin
      UpLoadToUser := '';
      If Param = '' Then
        Transfer (Param, Receive, tsNormal)
      Else
      Begin
        SavePath := FileArea. ULPath;
        FileArea. ULPath := AddBackSlash (Param);
        Transfer (Param, Receive, tsNormal);
        FileArea. ULPath := SavePath;
      End;
    End;

    Procedure LogOff;
    Var
      T : EventTimer;

    Begin
      If Not R. HotKeys Then
        ComWriteLn ('', 0);

      If Not (Cnf. LogoffAsk And Not Query (lang (laLogOff), True,
         ofFramed)) Then
      Begin
        SetTitle ('logging out');
        If Cnf. LogoffMail And Query (lang (laLogoffMessage), False,
           ofFramed)
        Then
          Msg2SysOp ('Feedback', 'Feedback');

      {$IFDEF OS2}
        NeedThreadClose := True;
        Repeat
        Until Not ThreadLocked;
      {$ENDIF}

        LogWrite ('+', sm (smlHangUp));

        EmuDispFile ('~logoff');
        ExecScript ('logoff');
        ExecRexx ('logoff');

        If Not Local Then
        Begin
          NewTimerSecs (T, 10);
          While (OutBuffUsed (Port) > 0) And Not TimerExpired (T) Do;
        End;

        {$IFDEF MSDOS} tmDelay {$ELSE} Pause {$ENDIF} (2000);
        NormExit;
      End;
    End;

    Procedure ChFGroup;
    Var
      LastGroup, LastArea: LongInt;

    Begin
      If Param = '' Then
        SelectFGroup
      Else
      Begin
        LastGroup := R. FileGroup;
        LastArea := R. FileArea;
        SmartChangeFArea (Str2Long (Param), 1, LastGroup, LastArea);
      End;
    End;

    Procedure ChFArea;
    Begin
      If Param = '' Then SelectFArea
                    Else SetFileArea (Str2Long (Param));
    End;

    Procedure ChMGroup;
    Var
      LastGroup, LastArea: LongInt;

    Begin
      If Param = '' Then
        SelectMGroup
      Else
      Begin
        LastGroup := R. MsgGroup;
        LastArea := R. MsgArea;
        SmartChangeMArea (Str2Long (Param), 1, LastGroup, LastArea);
      End;
    End;

    Procedure ChMArea;
    Begin
      If Param = '' Then SelectMArea
                    Else SetMsgArea (Str2Long (Param));
    End;

    Procedure FList (Raw: Boolean);
    Var
      tB          : Boolean;
      OldFileArea : tFileArea;

    Begin
      If Param <> '' Then
      Begin
        OldFileArea := FileArea;
        OpenFileAreas;
        ReadFileArea (FileArea, Str2Long (Param));
        CloseFileAreas;
      End;

      If (FileArea. List_Security > R. Security) Or
         Not FlagsValid (R. Flags, FileArea. List_Flags) Then
      Begin
        If R. HotKeys Then
          ComWriteLn ('', 0);
        Message (lang (laSecurityLow));
        wFin := True;
      End Else
      Begin
        SetTitle ('browsing file area contents');
        LogWrite ('+', sm (smlBrowsingFArea) + ZeroMsg (FileArea. Name, True));
        InitDispFiles;
        DispFilesBBS (AllFilesMask, '', False, Raw, tB);
        DoneDispFiles;
      End;

      If Param <> '' Then
        FileArea := OldFileArea;
    End;

    Procedure DispNotFound;
    Begin
      ComWrite ('|\12' + lang (laFileName), eoMacro + eoCodes + eoNoFlush);
      ComWrite (Param, eoNoFlush);
      ComWriteLn (lang (laNotFound), eoMacro + eoCodes);
      Message ('');
    End;

    Procedure ArcView;
    Var
      F : String;

    Begin
      If Trim (Param) = '' Then
        Param := JustFileName (GetAnswer (lang (laQueryArcName), 12,
          ofAllowEmpty, ''));

      If Trim (Param) <> '' Then
      Begin
        If (Not IsDevice (Param)) And InFilesBBS (Param) Then
        Begin
          F := mFileExist (FileArea. DLPath, Param);
          If F <> '' Then
          Begin
            SetTitle ('viewing archive ' + LoString (F));
            Cls;
            OnlineArcView (F);
            Exit;
          End;
        End;

        DispNotFound;
      End;
    End;

    Procedure FType;
    Var
      F : String;

    Begin
      If Trim (Param) = '' Then
        Param := JustFileName (GetAnswer (lang (laAskFileName), 12,
          ofAllowEmpty, ''));

      If Trim (Param) <> '' Then
      Begin
        If (Not IsDevice (Param)) And InFilesBBS (Param) Then
        Begin
          F := mFileExist (FileArea. DLPath, Param);
          If F <> '' Then
          Begin
            ComWriteLn ('', 0);
            TypeFile (F);
            Message ('');
            Exit;
          End;
        End;

        DispNotFound;
      End;
    End;

    Procedure RunScript;
    Var
      SName : String;

    Begin
      If Param <> '' Then
      Begin
        SName := DefaultName (ExtractWord (1, Param, SpaceOnly), 'trs',
          lang (laTxtFiles));
        If FileExists (SName) Then
        Begin
          SetTitle ('running ' + LoString (JustFilename (SName)));
          LogWrite ('%', sm (smlExecScript) + NiceFileName (SName, 40));
          ExecScript (Param);
        End
        Else
          LogWrite ('!', sm (smFile) + NiceFileName (SName, 50) +
            sm (smNotFound));
      End;
    End;

    Procedure SendMsg;
    Var
      cLine : Byte;

    Begin
      cLine := mL_ChooseLine;
      If cLine = BbsLine Then
        wFin := True;
      If cLine = 0 Then
        wFin := True
      Else
      Begin
        If Not mL_GetMesgStat (cLine) Then
        Begin
          Message ('|' + lang (laLineDisabled));
          wFin := True;
          Exit;
        End;
        mL_SendMsg (cLine, GetAnswer (lang (laSendMessage), 55, ofAllowEmpty,
          ''), mtUserMsg);
        LogWrite ('+', sm (smlSendingMsg) + Long2Str (cLine));
      End;
    End;

    Procedure MsgPost;
    Var
      S1, S2 : String [80];

    Begin
      S1 := ExtractWord (1, Param, SpaceAndComma);
      S2 := ExtractWord (2, Param, SpaceAndComma);
      If (S2 = '') And Not ConsistsOf (S1, NumbersOnly) Then
      Begin
        S2 := S1;
        S1 := '';
      End;

      PrePostMsg (S1, S2);
    End;

    Procedure GlobSearch (Mode: tFSearchMode; Const Msg: String);
    Begin
      If Param = '' Then
        Param := GetMaxStr (Msg);
      If Trim (Param) <> '' Then
        GlobalSearch (Param, Mode, atNo, '');
    End;

    Procedure Shell;
    Var
      CurDir : String [100];

    Begin
      LogWrite ('+', sm (smlEnterDW));
      GetDir (0, CurDir);
      CommandProcessor;
      LogWrite ('+', sm (smExitDW));
      SmartChDir (CurDir);
    End;

  Begin
    CurrentDebugFile := '';
    SetTitle ('');
    ExecMenuAction := True;
    InLightBarMenu := True;

    If Param = '' Then LogWrite ('&', Actions [Action])
                  Else LogWrite ('&', Actions [Action] + ' (' + Param + ')');

    If (BbsLine = 0) And (Action in [cSend_Msg, cSet_Msg_Off, cSet_Msg_On,
       cLine_List, cConference]) Then
    Begin
      wFin := True;
      Exit;
    End;

    If Not R. Frames Then
      ComWriteLn ('', 0);

    Case Action Of
          cGosub : Begin
                     Menus^. Insert (NewStr (UpString (FileName) + ' ' +
                       Long2Str (LightedArrItem)));
                     FileName := ExtractWord (1, Param, SpaceAndComma);
                     LightedArrItem := Str2Long (ExtractWord (2, Param,
                       SpaceAndComma)) - 1;
                   End;

           cGoTo : Begin
                     Menus^. FreeAll;
                     FileName := ExtractWord (1, Param, SpaceAndComma);
                     LightedArrItem := Str2Long (ExtractWord (2, Param,
                       SpaceAndComma)) - 1;
                   End;

         cReturn : PopMenu;

     cExecScript : RunScript;

           cExec : Begin
                     Param := TranslateExecParams (Param);
                     SetTitle ('executing external program ');
                     LogWrite (':', sm (smlExec) + Param);
                     DosShell (Param, exCommand, True);
                   End;

 cChangeFileGroup : ChFGroup;

  cChangeFileArea : ChFArea;

  cChangeMsgGroup : ChMGroup;

   cChangeMsgArea : ChMArea;

    cNextFileArea : SetFileArea (R. FileArea + 1);

    cPrevFileArea : SetFileArea (R. FileArea - 1);

     cNextMsgArea : SetMsgArea (R. MsgArea + 1);

     cPrevMsgArea : SetMsgArea (R. MsgArea - 1);

   cNextFileGroup : SetFileGroup (R. FileGroup + 1);

   cPrevFileGroup : SetFileGroup (R. FileGroup - 1);

    cNextMsgGroup : SetMsgGroup (R. MsgGroup + 1);

    cPrevMsgGroup : SetMsgGroup (R. MsgGroup - 1);

        cFileList : FList (False);

        cReadMsgs : Begin
                      LogWrite ('+', sm (smlReadingMsg) +
                        ZeroMsg (MsgArea. Name, True));
                      ReadMsgs;
                    End;

        cDownLoad : If Param = '' Then
                      DownLoad
                    Else
                      If FileExists (Param) Then
                        DL
                      Else
                        Message (lang (laFileName) + Param + lang (laNotFound));

          cUpLoad : UL;

     cDisplayFile : Begin
                      LogWrite ('+', sm (smlShowFile) + NiceFileName
                        (DefaultName (Param, EmuExt [R. Emu], lang
                        (laTxtFiles)), 50));
                      EmuDispFile (Param);
                    End;

   cScan_NewFiles : GlobalSearch (AllFilesMask, fsDate, atAsk, '');

   cScan_PrivMail : SearchPrivate;

    cGlobalSearch : GlobSearch (fsName, lang (laSearchMask));

    cSearchByDesc : GlobSearch (fsDesc, lang (laSearchDesc));

       cPageSysOp : PageSysOp (Param);

      cUpLoadPriv : Begin
                      UpLoadToUser := Param;
                      Transfer (Param, Receive, tsPrivate);
                    End;

         cArcView : ArcView;

         cPostMsg : MsgPost;

        cListMsgs : Begin
                      LogWrite ('+', sm (smlListingMsg) +
                        ZeroMsg (MsgArea. Name, True));
                      ListMsgs;
                    End;

      cSearchMsgs : Begin
                      If Param = '' Then
                        Param := GetMaxStr (lang (laMsgMaskGet));
                      If Trim (Param) <> '' Then
                        SearchMessages (Param);
                    End;

     cChangeParam : ChangeParam (Letters [Param [1]]);

            cNews : fNews (False);

   cTodaysCallers : TodaysCallers;

         cVerInfo : Information;

        cTypeFile : FType;

        cUserInfo : If (Param = '') And (R. Security >= Cnf. UserInfoMinSec) And
                       FlagsValid (R. Flags, Cnf. UserInfoMinFlags) Then
                    Begin
                      If Cnf. CapitalizeNames Then
                        SetInputCap (Proper, LettersOnly)
                      Else
                        SetInputCap (NoCaps, LettersOnly);

                      UserInfo (GetAnswer (lang (laUserName), 36, ofAllowEmpty,
                        ''));
                      SetInputCap (NoCaps, AllChars);
                    End
                    Else
                      UserInfo (Param);

      cShowRawDir : FList (True);

           cShell : Shell;

        cExecRexx : Begin
                    {$IFDEF OS2}
                      LogWrite ('+', 'Executing REXX program ' + NiceFileName
                        (Param, 50));
                      If Not ExecRexx (Param) Then
                        LogWrite ('!', 'Unable to execute REXX program');
                    {$ENDIF}
                    End;

     cDownLoadQWK : qwkDownLoad;
       cUpLoadQWK : qwkUpLoad;
       cSelectQWK : qwkSelect;

      cSet_Msg_On : Begin
                      LogWrite ('+', sm (smlEnabelingFrom));
                      mL_EnableMsg;
                      Message ('|' + lang (laMsgsEnabled));
                    End;

     cSet_Msg_Off : Begin
                      LogWrite ('+', sm (smlDisabelingFrom));
                      mL_DisableMsg;
                      Message ('|' + lang (laMsgsDisabled));
                    End;

        cSend_Msg : SendMsg;

       cLine_List : Begin
                      LogWrite ('+', sm (smlOnLineList));
                      mL_WhoDo (lmAll);
                      Message ('');
                    End;

          cLogOff : LogOff;

         cReLogin : Begin
                      SaveTime;
                      SaveTagList;
                      ClearTagList;
                      ClearReps;
                      mL_Done;

                      TimeCount := False;
                      EMSI. Session := False;
                      FillChar (R, SizeOf (R), #0);
                      ReadLanguage (Language, Cnf. DefLangFile);
                      R. Lang := UpString (JustName (Cnf. DefLangFile));
                      R. XLAT := '';
                      EnterTime := MidSec;
                      enTime := EnterTime;
                      UsedMinus := 0;

                      Login;
                    End;

      cConference : mL_RealTimeConference;

          cTelnet : GoTelnet (Param);
          cFinger : DoFinger (Param);
    End;
  End;

  Function Secured (M: PMenuItem): Boolean;
  Begin
    Secured := (R. Security < M^. Security) Or
               Not FlagsValid (R. Flags, M^. Flags^);
  End;

  Procedure ArrowedMenu;
  Var
    M, M1                                 : PMenuItem;
    i, CurItem, oItem, Amount, TailItems,
    MaxLength, mL, Hor, Ver               : Integer;
    C                                     : Char;
    nL                                    : Boolean;
    S                                     : String;

    Function GetXCoord (Item: Integer): Integer;
    Begin
      GetXCoord := (Item Div mHeader. ArrowVer) * (MaxLength + 1) +
        mHeader. StartX;
    End;

    Function GetYCoord (Item: Integer): Integer;
    Begin
      GetYCoord := (Item Mod mHeader. ArrowVer) + mHeader. StartY;
    End;

    Procedure FoundNext (Direction: Char);
    Var
      M : PMenuItem;

    Begin
      While (CurItem < Amount) And (CurItem >= 0) Do
      Begin
        Case Direction Of
          kbUp    : Dec (CurItem);
          kbDown  : Inc (CurItem);
          kbLeft  : Dec (CurItem, mHeader. ArrowVer);
          kbRight : Inc (CurItem, mHeader. ArrowVer);
          kbHome  : Begin
                      CurItem := -1;
                      Direction := kbLeft;
                    End;
          kbEnd   : Begin
                      CurItem := Amount;
                      Direction := kbRight;
                    End;
        End;

        If (CurItem < 0) Or (CurItem >= Amount) Then
        Begin
          Case Direction Of
            kbLeft  : CurItem := 0;
            kbRight : CurItem := Amount - 1;
          Else
            Break;
          End;

          M := PMenuItem (MenuItems^. At (CurItem));

          While Secured (M) Or (M^. Action = cDisplayOnly) Do
          Begin
            Case Direction Of
              kbLeft  : Inc (CurItem);
              kbRight : Dec (CurItem);
            End;

            If CurItem < 0 Then
            Begin
              CurItem := 0;
              Break;
            End;

            If CurItem >= Amount Then
            Begin
              CurItem := Amount - 1;
              Break;
            End;

            M := PMenuItem (MenuItems^. At (CurItem));
          End;

          Exit;
        End;

        M := PMenuItem (MenuItems^. At (CurItem));

        If (Not Secured (M) Or (Cnf. ShowSecured <> ssHidden)) And
           (M^. Action <> cDisplayOnly)
        Then
          Exit;
      End;

      CurItem := oItem;
    End;

    Function ExecAction (M: PMenuItem): Boolean;
    Begin
      If Not Secured (M) Then
      Begin
        ComWrite (EmuRelColor ($07), 0);
        If Not (M^. Action in [cExecScript, cReturn, cGosub, cGoTo]) Then
          Cls;
        LightedArrItem := CurItem;
        ExecAction := ExecMenuAction (M^. Action, M^. OptData^);
      End Else
      Begin
        ComWrite (#7, 0);
        ExecAction := False;
      End;
    End;

    Procedure NextLine (Ver: Integer);
    Begin
      If R. Emu = teTTY Then
        ComWriteLn ('', 0)
      Else
      Begin
        ComWrite (EmuGoToXY (GetXCoord (Ver), GetYCoord (Ver)), 0);
        CurItem := Ver + 1;
      End;
    End;

    Function FindStartItem (TestItem: Integer): Boolean;
    Begin
      CurItem := TestItem;

      While (CurItem < Amount) And (CurItem >= 0) Do
      Begin
        M := PMenuItem (MenuItems^. At (CurItem));
        If Not Secured (M) And (M^. Action <> cDisplayOnly) Then
        Begin
          FindStartItem := True;
          Exit;
        End;

        Inc (CurItem);
      End;

      FindStartItem := False;
    End;

  Begin
    TailItems := 0;
    i := 0;

    While i < MenuItems^. Count - TailItems Do
    Begin
      M := PMenuItem (MenuItems^. At (i));

      If M^. AutoExec Then
      Begin
        If Not Secured (M) Then
          ExecMenuAction (M^. Action, M^. OptData^);

        MenuItems^. AtFree (i);
        Continue;
      End;

      If Secured (M) And (Cnf. ShowSecured = ssHide) Then
      Begin
        MenuItems^. AtFree (i);
        Continue;
      End;

      If (M^. Display^ = '') And (M^. Action <> cDisplayOnly) Then
      Begin
        MenuItems^. AtDelete (i);
        MenuItems^. Insert (M);
        Inc (TailItems);
        Continue;
      End;

      Inc (i);
    End;

    mL_LineMsg;

    If (mHeader. DisplayFile <> '') And (R. Emu <> teTTY) Then
      DispFile (DefaultName (mHeader. DisplayFile, EmuExt [R. Emu],
        lang (laTxtFiles)))
    Else
    Begin
      Cls;
      UpdateUserMacro;
    End;

    Amount := MenuItems^. Count - TailItems;
    i := mHeader. ArrowVer * mHeader. ArrowHor;
    If Amount > i Then
      Amount := i;

    If Amount = 0 Then
      While True Do
        ComReadKey;

    MaxLength := 0;

    For i := 0 To Amount - 1 Do
    Begin
      mL := Length (ZeroMsg (PMenuItem (MenuItems^. At (i))^. Display^, True));
      If mL > MaxLength Then
        MaxLength := mL;
    End;

    mL := (80 - mHeader. ArrowHor - mHeader. StartX) Div mHeader. ArrowHor;
    If MaxLength > mL Then
      MaxLength := mL;

    If R. Emu <> teTTY Then
      ComWrite (EmuGoToXY (GetXCoord (0), GetYCoord (0)), 0);

    nL := False;

    For Ver := 1 To mHeader. ArrowVer Do
      For Hor := 1 To mHeader. ArrowHor Do
      Begin
        CurItem := (Hor - 1) * mHeader. ArrowVer + Ver;

        If CurItem <= Amount Then
        Begin
          If nL Then
          Begin
            NextLine (Ver - 1);
            nL := False;
          End;

          M := PMenuItem (MenuItems^. At (CurItem-1));

          If Secured (M) And (Cnf. ShowSecured = ssHidden) Then
          Begin
            ComWrite (EmuRelColor (Cnf. ColorScheme [mnHidden]), 0);
            ChangePString (M^. Display, ZeroMsg (M^. Display^, True));
          End
          Else
            ChangePString (M^. Display, PlaceSubStr (M^. Display^, '|', ''));

          mWriteLen (M^. Display^, MaxLength, pmPadRight);

          If Hor = mHeader. ArrowHor Then NextLine (Ver)
                                     Else ComWrite (' ', 0);
        End
        Else
          nL := True;
      End;

    If Not FindStartItem (LightedArrItem) Then
      If Not FindStartItem (mHeader. StartItem) Then
        If Not FindStartItem (0) Then
          While True Do
            ComReadKey;

    InLightBarMenu := True;

    If R. Emu = teTTY Then
      Repeat
        C := UpCase (ComReadKey);

        For i := 0 To MenuItems^. Count-1 Do
        Begin
          M := PMenuItem (MenuItems^. At (i));
          If M^. HotKey = C Then
            If ExecAction (M) Then Exit
                              Else Break;
        End;
      Until False
    Else
      Repeat
        M := PMenuItem (MenuItems^. At (CurItem));
        S := EmuGoToXY (GetXCoord (CurItem), GetYCoord (CurItem));
        ComWrite (S + EmuRelColor (mHeader. SelectedColor), 0);
        mWriteLen (ZeroBackGround (M^. Display^), MaxLength, pmNone);
        ComWrite (S + EmuRelColor ($07), 0);

        Repeat
          C := UpCase (ComReadKey);
          oItem := CurItem;

          Case C Of
            kbUp..kbEnd : FoundNext (C);
                    #13 : If ExecAction (M) Then
                            Exit;
          Else
            For i := 0 To MenuItems^. Count-1 Do
            Begin
              M1 := PMenuItem (MenuItems^. At (i));

              If M1^. HotKey = C Then
              Begin
                If Secured (M1) And (Cnf. ShowSecured = ssHidden) Then
                  ComWrite (#7, 0)
                Else
                Begin
                  If i < Amount Then
                    CurItem := i;

                  If R. HotKeys Or (M1^. Display^ = '') Then
                    If ExecAction (M1) Then
                      Exit;
                End;

                Break;
              End;
            End;
          End;
        Until oItem <> CurItem;

        mWriteLen (M^. Display^, MaxLength, pmNone);
      Until False;
  End;

  Procedure OrdinalMenu;
  Var
    i, LastDraw     : LongInt;
    mc, ic          : Integer;
    MenuKey         : Char;
    Breaked, Denied : Boolean;
    M               : PMenuItem;
    dFile, oFN      : PathStr;
    HotKeys         : String [100];

  Label
    Loop2,
    Loop3;

  Begin
    dFile := Trim (mHeader. DisplayFile);
    LastDraw := MidSec;

  Loop2:
    mL_LineMsg;

    { Ž¡à ¡®âª  HEADER'     }

    mHeader. DisplayFile := dFile;
    If mHeader. DisplayFile = #255 Then
      mHeader. DisplayFile := '';

    { ‚ë¢®¤ ANSI-ä ©«  ¬¥­î }

    If mHeader. DisplayFile <> '' Then
      DispFile (DefaultName (mHeader. DisplayFile, EmuExt [R. Emu],
        lang (laTxtFiles)))
    Else
    Begin
      Cls;
      UpdateUserMacro;
    End;

    Breaked := (HotKeysStr <> '') Or (KeyBuffer <> '');
    If Breaked Then
      mHeader. DisplayFile := #255;

    HotKeys := '';
    ic := 0;

    Repeat
      If R. HotKeys Then
        If Incoming Then
        Begin
          InAction := True;
          MenuKey := UpCase (ComReadKey);
          InAction := False;

          If Not FuncKey Then
          Begin
            HotKeys := '';

            For mc := 0 To MenuItems^. Count-1 Do
            Begin
              M := PMenuItem (MenuItems^. At (mc));
              If M^. Action = 0 Then
                Break;

              If M^. Action <> cDisplayOnly Then
                HotKeys := HotKeys + M^. HotKey;
            End;

            KeyBufAdd (MenuKey);
            ClearBuffer;

            mHeader. DisplayFile := #255;
            Breaked := True;
          End;
        End;

      Inc (ic);
      If ic > MenuItems^. Count Then
        Break;

      M := PMenuItem (MenuItems^. At (ic-1));

      If M^. Action > ActionsNum Then
        Continue;

      Denied := Secured (M);

      If M^. AutoExec And Not Denied Then
      Begin
        oFN := FileName;
        ExecMenuAction (M^. Action, M^. OptData^);
        If (oFN <> FileName) Or (M^. Action = cReturn) Then
          Exit;
      End;

      If Not (Denied And (Cnf. ShowSecured = ssHidden)) Then
        If M^. Action <> cDisplayOnly Then
          HotKeys := HotKeys + M^. HotKey;

      If mHeader. DisplayFile = '' Then
        If Not Denied Then
          ComWrite (M^. Display^, eoMacro + eoCodes)
        Else
          If Cnf. ShowSecured = ssShow Then
            ComWrite (M^. Display^, eoMacro + eoCodes)
          Else
            If Cnf. ShowSecured = ssHidden Then
              ComWrite (EmuRelColor (Cnf. ColorScheme [mnHidden]) +
                ZeroMsg (M^. Display^, False), eoMacro + eoCodes)
            Else
              ComWrite (Replicate ('|', SymbolCount (M^. Display^, '|')),
                eoCodes);
    Until False;

    If Not Breaked Then
    Begin
      If mHeader. WriteHotKeys Then
      Begin
        ComWriteLn ('', 0);
        ComWriteLn ('[' + HotKeys + ']', 0);
      End;

      Frame;
      If (Not R. Frames) Or (R. Emu = teTty) Then
        ComWriteLn ('', 0);
      ComWrite (mHeader. Prompt, eoMacro + eoCodes);
    End;

  Loop3:
    ProcessChoices;
    MenuKey := UpCase (ComReadKey);

    If Pos (MenuKey, HotKeys) = 0 Then
    Begin
      If Cnf. CRinMenu Then
      Begin
        i := TimeDiff (LastDraw, MidSec);
        Inc (LastDraw, i);

        If (i > 1) Or (i < 0) Then
          Goto Loop2;
      End;

      Goto Loop3;
    End;

    If R. HotKeys Then
    Begin
      If Not (MenuKey in [#1..#31]) Then
        ComWrite (MenuKey, 0);
    End
    Else
      If HotKeysStr = '' Then
        ComWriteLn ('', 0);

    If ((KeyBuffer = '') And (HotKeysStr = '')) Or
       (R. HotKeys And (R. Emu = teTty))
    Then
      ComWriteLn ('', 0);

    mL_LineMsg;

    {€­ «¨§ ­ ¦ â®© ª« ¢¨è¨}

    wFin := False;
    mc := 0;

    While Not wFin Do
    Begin
      Inc (mc);
      If mc > MenuItems^. Count Then
        Break;

      M := PMenuItem (MenuItems^. At (mc-1));

      If MenuKey = M^. HotKey Then
      Begin
        If Secured (M) Then
        Begin
          If R. HotKeys Then
            ComWriteLn ('', 0);
          Message (lang (laSecurityLow));
          Break;
        End;

        SmartLine;

        If ExecMenuAction (M^. Action, M^. OptData^) Then
          Exit;
      End;
    End;

    Goto Loop2;
  End;

Begin
  If Not UseMenu (FileName) Then
  Begin
    PopMenu;
    Exit;
  End;

  If Cnf. DebugFiles Then
    SetDebugFile (lang (laMenus) + FileName + '.mnu');
  SetTitle ('browsing ' + LoString (FileName) + '.mnu');

  If (mHeader. ArrowHor > 0) And (mHeader. ArrowVer > 0) Then ArrowedMenu
                                                         Else OrdinalMenu;
End;

Procedure fNormExit; Far;
Const
  Gone : Boolean = False;

Var
  AddTime : LongInt;

Begin
  If Gone Then
    Exit;

  Gone := True;
  ExitProc := SavedExitProc;

{$IFDEF OS2}
  NeedThreadClose := True;

  Repeat
  Until Not ThreadLocked;
{$ENDIF}

  If Language <> Nil Then
  Begin
    ExecScript ('normexit ' + Long2Str (ExitCode));
    If Not Registering Then
      mL_SendMsg (0, trcSysMsgPrefix + '** ' + R. Name + lang (laTRCline) +
        Long2Str (BbsLine) + lang (laTRCleftBBS), mtConference);
  End;

  If TransferTime > 0 Then
  Begin
    AddTime := Round (TransferTime / 60) * Cnf. UploadTimePlus;
    If AddTime > 0 Then
      Inc (R. TotalTime, AddTime * 60);
  End;

  ClearCL;

  HiddenCursor;
  DoneTornadoScreen;
  CenterDelayTempBox (Cnf. ColorScheme [cdFrame], Cnf. ColorScheme [cdText],
    1, CenterCh (sm (smNormalExit) + '..', ' ', 30), ZoomSpeed, '');

  qwkWriteList;
  If Kill_Line_Flag Then
    EraseFlag (RunFlag);
  SaveTime;
  SaveTagList;
  SaveDoReg;
  ClearReps;
  mL_Done;

  If Is_User (R. Name, Cnf. Aliases) Then
    LogWrite ('~', R. Name + sm (smUsrFinished) +
      Long2Str (Round (TimeDiff (enTime, MidSec) / 60)) + sm (smMin) +
      Long2Str (SessionDL) + '/' + Long2Str (SessionUL));

  If InChat And (Cnf. ChatLog <> '') Then
  Begin
    WriteLn (ChatLog);
    WriteLn (ChatLog, sm (smChatLogClosed));
    Close (ChatLog);
  End;

  If (Not Local And Not InitError) Or LocalOffHook Then
  Begin
    HangUp;
  {$IFDEF MSDOS}
    ptOptionsOn (Port, ptRestoreOnClose);
    ptOptionsOn (Port, ptDropModemOnClose);
  {$ENDIF}
    DonePort (Port);
  End;

  If Not Exeption Then
    LogWrite (':', sm (smEnd));

  WriteSystemStatus (Sys, Cnf. Path + 'system.tor');
  CloseResourceCache;
  DisposeAllCaches;
  DoneAreasGroups;
  {SetTitle (SaveTitle);}
  FreeTitleMem;

  If Language <> Nil Then
    Dispose (Language, Done);
  If ReadHistory <> Nil Then
    Dispose (ReadHistory, Done);
  If Reps <> Nil Then
    Dispose (Reps, Done);
  If Menus <> Nil Then
    Dispose (Menus, Done);
  If UpFiles <> Nil Then
    Dispose (UpFiles, Done);
  If ParamNames <> Nil Then
    Dispose (ParamNames, Done);
  If MsgText <> Nil Then
    Dispose (MsgText, Done);

  DoneTempBox;

{$IFDEF RealMode}
  OvrDisposeStreams;
{$ENDIF}

  LogClose;

  NormalCursor;
  Window (1, 1, 80, ScrY + 1);
  GotoXY (WXG, WYG);
  TextAttr := 3;

  WriteLn ('þ ' + sm (smNormalExit) + '..');
  If SysMsg <> Nil Then
    Dispose (SysMsg, Done);
  TextAttr := 7;
  WriteLn;
  {RestBlink;}

{$IFDEF MSDOS}
{$IFNDEF DPMI32}
  SetCBreak (CtrlCState);
  RestoreVecs;
{$ENDIF}
{$ENDIF}

  SmartChDir (oDir);
  Halt (ExitCode);
End;

Procedure ErrorExit;
{$IFDEF VirtualPascal}
{$IFOPT D+,L+}
Var
  FName  : String;
  LineNo : LongInt;
{$ENDIF}
{$ENDIF}

Begin
{$IFDEF OS2}
  NeedThreadClose := True;

  Repeat
  Until Not ThreadLocked;
{$ENDIF}

  ExitProc := SavedExitProc;

{$IFDEF RealMode}
  If ExitCode in [208, 209] Then
  Begin
    DoneTornadoScreen;
    Window (1, 1, ScrX + 1, ScrY + 1);
    GotoXY (WXG, WYG);
    WriteLn ('! ' + sm (smOvrError));
    Halt (209);
  End;
{$ENDIF}

  HiddenCursor;
  DoneTornadoScreen;
  CenterDelayTempBox (Cnf. ColorScheme [caFrame], Cnf. ColorScheme [caText], 1,
    CenterCh (sm (smErrorExit) + '..', ' ', 30), ZoomSpeed, '');

  If Cnf. Sound Then
    SoundOf ('1 200 1 2 800 1000 -1 2 800');
  EraseFlag (RunFlag);

  If ErrorAddr <> Nil Then
  Begin
    LogWrite ('!', sm (smRunTime) + Long2Str (ExitCode) + ' at ' +
      HexPtr (ErrorAddr));
  {$IFDEF VirtualPascal}
  {$IFOPT D+,L+}
    If GetLocationInfo (ErrorAddr, FName, LineNo) <> Nil Then
      LogWrite ('!', '(' + FName + ', line ' + Long2Str (LineNo) + ')');
  {$ENDIF}
  {$ENDIF}
    ErrorAddr := Nil;
    ExitCode := 211;
  End
  Else
    LogWrite ('!', sm (smErrorExit));

  LogClose;

  SaveTime;
  mL_Done;

  If (Not Local And Not InitError) Or LocalOffHook Then
  Begin
    HangUp;
  {$IFDEF MSDOS}
    ptOptionsOn (Port, ptRestoreOnClose);
    ptOptionsOn (Port, ptDropModemOnClose);
  {$ENDIF}
    DonePort (Port);
  End;

  DoneTempBox;

  CloseResourceCache;
  DisposeAllCaches;
{$IFDEF RealMode}
  OvrDisposeStreams;
{$ENDIF}

  NormalCursor;
  Window (1, 1, ScrX + 1, ScrY + 1);
  GotoXY (WXG, WYG);
  TextAttr := 12;
  WriteLn ('! ' + sm (smErrorExit) + '..'#13#10);
  TextAttr := 7;
  WriteLn;
  {RestBlink;}

{$IFDEF MSDOS}
{$IFNDEF DPMI32}
  SetCBreak (CtrlCState);
  RestoreVecs;
{$ENDIF}
{$ENDIF}

  {SetTitle (SaveTitle);}
  FreeTitleMem;

  SmartChDir (oDir);
  Halt (ExitCode);
End;

(*
Procedure fGluck; Far;
Var
  i, j : Integer;
  S    : Char;

Begin
  Randomize;
  j := Random (100);
  For i := 1 To j Do
  Begin
    S := Chr (Random (254));
    If (S = #1) Or (S = #12) Then
      Continue;
    ComWrite (S, 0);
  End;
End;
*)

Procedure InitTMainOvr;
Begin
  NormExit := fNormExit;
  News := fNews;
  {Gluck := fGluck;}
End;

End.
