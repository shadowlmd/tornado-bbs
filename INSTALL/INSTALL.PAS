{$IFDEF MSDOS}
  {$IFNDEF DPMI}
    {$M 32768,0,655360}
    {$F+}
  {$ENDIF}
{$ENDIF}

(*
{$IFNDEF RealMode}
  {$M 75520}
{$ENDIF}
*)

{&Use32-}
Program Tornado_Installer;

Uses
(*
{$IFDEF VirtualPascal}
  Os2Def,
  Os2Base,
{$ENDIF}
*)
  DOS,
  Objects,
  TimeTask,
  OpCrt,
  ApZip,
  ApArchiv,
  tMisc,
  tGlob,
  tWin,
  iLine,
  {ConfUpd,}
  InstGlob;

Const
  Mono           : Boolean          = False;
  CheckFreeSpace : Boolean          = True;
  Actions        : Array [Boolean] Of String [11] = ('', '');
  InstTo         : Array [Boolean] Of String [23] = ('', '');
  LangName       : Array [tInstLanguage] Of String [7] = ('Russian', 'English', 'German');

Var
  Box1, Box2                                    : pBoxRec;
  Changes                                       : Boolean;
  C, Ch                                         : Char;
  FML                                           : FileMaskList;
  i, WX, WY, Keyb, Choice                       : Byte;
  F, F1                                         : Text;
  St                                            : String;
  SaveScr                                       : Pointer;
  Items                                         : PNotSortedCollection;
  MenPos, PrevMenPos                            : Byte;
  Free                                          : LongInt;

{$IFDEF VirtualPascal}
  PrevXcptProc                                  : Pointer;
  Times                                         : ULong;
{$ENDIF}

{$I ver.inc}

Const
  Rates : Array [1..8] Of LongInt =
          (1200, 2400, 4800, 9600, 19200, 38400, 57600, 115200);

Procedure ExitInst;
Begin
  NormalCursor; TextAttr := $07; WriteLn;
  RestoreWindow (1, 1, Lo (WindMax)+1, Hi (WindMax)+1, True, SaveScr);
  GoToXY (WX, WY);
  Halt;
End;

Function OKF (Var CDH: CentralDirHead; Var FName: PathStr) : Boolean;
Begin
  OKF := OkFile (FName);
End;

Function ExtractSuccessFuncZip (Var CDH: CentralDirHead; FName: PathStr;
         ErrCode: {$IFDEF VirtualPascal} LongInt {$ELSE} Word {$ENDIF}) : Boolean;
Begin
End;

Procedure ShowMethodProcZip (Method : Byte; FName : PathStr);
Begin
  ScrollWindowUp (6, 4, 74, 15, 1);
  FastWrite (PadCh (FName, ' ', 12), 19, 34, ColorScheme [Fields]);
  FastWrite ('  ' + UpString (FName), 15, 6, ColorScheme [BoxText]);
End;

Procedure AddInstallMsg (S: String);
Begin
  ScrollWindowUp (6, 4, 74, 15, 1);
  FastWrite (S, 15, 6, ColorScheme [BoxText]);
End;

Procedure ShowCommentsProcZip (FName: PathStr; CP: CommentPtr; CLen : {$IFDEF VirtualPascal} LongInt {$ELSE} Word {$ENDIF});
Var
  S     : String;

Begin
  FastWrite (PadCh (JustFileName (FName), ' ', 12), 19, 16, ColorScheme [Fields]);
  GetDir (0, S);
  AddInstallMsg (S);
End;

Function ShowProgressF (BytesWritten: LongInt; TotalBytes: LongInt): Boolean;
Var
  Tmp   : Byte;

Begin
  If TotalBytes > 0
  Then
    Tmp := Round (BytesWritten / (TotalBytes / 100))
  Else
    Tmp := 0;

  If Tmp > 100 Then Tmp := 100;
  FastWrite (LeftPadCh (Long2Str (Tmp) + '%', ' ', 4), 21, 70, ColorScheme [Headers]);

  Tmp := Round (Tmp / 1.612);
  FastWrite (Replicate (' ', Tmp), 21, 7, ColorScheme [EmptyLine]);
  FastWrite (Replicate (' ', 62 - Tmp), 21, 7 + Tmp, ColorScheme [Progress]);

  ShowProgressF := True;
End;

Label
  _SysOp,
  _ComPort,
  _BaudRate,
  _Location,
  _BBSName,
  _InitString,
  _AnswerString,
  StValEnd;

{$IFNDEF VirtualPascal}
Procedure CBreakProc; Interrupt;
Begin
End;

{$ELSE}
Function CtrlBreakHandler (Report:       PExceptionReportRecord;
                           Registration: PExceptionRegistrationRecord;
                           Context:      PContextRecord;
                           P:            Pointer): ULong; CDecl;
Begin
  CtrlBreakHandler := Xcpt_Continue_Search;

  If (Report^.ExceptionNum = Xcpt_Signal) Then
  Case Report^. ExceptionInfo [0] of
    Xcpt_Signal_Intr,
    Xcpt_Signal_Break    : CtrlBreakHandler := XCpt_Continue_Execution;
    xcpt_Signal_KillProc : ;
  End;

  XcptProc := PrevXcptProc;
End;

{$ENDIF}

Procedure ItemShow (PrevMenPos: Byte);
Var
  i     : Byte;
  bGray : Boolean;

Begin
  bGray := False;

  Case PrevMenPos Of
    1 : i := 2;
    2 : i := 4;
    3 : i := 5;
    4 : Begin bGray := Upgrade; i := 6; End;
    5 : i := 8;
    6 : i := 9;
    7 : i := 11;
    8 : i := 12;
  Else
    Exit;
  End;

  If PrevMenPos in [7, 8] Then
    ChangeAttribute (56, Box1^. Y1 + i, 13, ColorScheme [Frames])
  Else If bGray Then
  Begin
    ChangeAttribute (56, Box1^. Y1 + i, 13, ColorScheme [Gray]);
  End Else
  Begin
    ChangeAttribute (26, Box1^. Y1 + i, 13, ColorScheme [Frames]);
    ChangeAttribute (30, Box1^. Y1 + i, 39, ColorScheme [HighLight]);
  End;
End;

Function FormLangs : String;
Var
  S     : String;

Begin
  S := '';
  If lngEnglish Then S := LangName [lEnglish];
  If (S <> '') And lngRussian Then S := S + ' + ';
  If lngRussian Then S := S + LangName [lRussian];
  If (S <> '') And lngGerman Then S := S + ' + ';
  If lngGerman Then S := S + LangName [lGerman];
  FormLangs := PadCh (S, ' ', 29);
End;

Procedure SetLanguage (L: tInstLanguage);
Begin
  If L = lRussian
    Then Language := RussianLang
    Else Language := EnglishLang;

  Actions [False] := Language [InstallNew];
  Actions [True]  := Language [Upgr];

  InstTo [False]  := Language [InstallTo];
  InstTo [True]   := Language [Previous];

  FastWrite (PadCh (Language [Action], ' ', 24) + ': ' + Actions [Upgrade], Box1^. Y1 + 2, 14, ColorScheme [Frames]);
  FastWrite (PadCh (Language [Documentation], ' ', 24) + ': '+ LangName [DocLang], Box1^. Y1 + 4, 14, ColorScheme [Frames]);
  FastWrite (PadCh (Language [Interfce], ' ', 24) + ': ' + LangName [IntfLang], Box1^. Y1 + 5, 14, ColorScheme [Frames]);
  FastWrite (PadCh (Language [LNGfiles], ' ', 24) + ': ' + FormLangs, Box1^. Y1 + 6, 14, ColorScheme [Frames]);
  FastWrite (PadCh (Language [InstallFrom], ' ', 24) + ': ' + InstDir, Box1^. Y1 + 8, 14, ColorScheme [Frames]);
  FastWrite (PadCh (InstTo [Upgrade], ' ', 24) + ': ' + iPath, Box1^. Y1 + 9, 14, ColorScheme [Frames]);
  FastWrite (CenterCh (Language [StartInst], ' ', 55), Box1^. Y1 + 11, 14, ColorScheme [Frames]);
  FastWrite (CenterCh (Language [Quit], ' ', 55), Box1^. Y1 + 12, 14, ColorScheme [Frames]);

  For i := 1 To 8 Do ItemShow (i);
End;

Procedure Extract (FName: PathStr);
Begin
  InitFileMaskList (FML);
  InitZipFile (AddBackSlash (InstDir) + FName);
  ExtractFileMaskListZip (FML);
  DoneZipFile;
  DoneFileMaskList(FML);

  FastWrite (Replicate (' ', 62), 21, 7, ColorScheme [Progress]);
  FastWrite ('  0%', 21, 70, ColorScheme [Headers]);
End;

Label
  Loop,
  Completed,
  Start;

Begin
  If ParamCount > 0 Then
  For i := 1 To ParamCount Do
  If UpString (ParamStr (i)) = '-M' Then Mono := True Else
  If UpString (ParamStr (i)) = '-C' Then CheckFreeSpace := False Else
  Begin
    WriteLn;
    WriteLn ('Tornado install program command line parameters:');
    WriteLn;
    WriteLn ('/? = see this help screen');
    WriteLn ('-c = don''t check free space before installation');
    WriteLn ('-m = force mono color scheme');
    WriteLn;
    Halt;
  End;

  Local := True;
  InitMulti;

{$IFNDEF VirtualPascal}

  SetIntVec ($23, Addr (CBreakProc));
  SetCBreak (False);

{$ELSE}

  PrevXcptProc := XcptProc;
  XcptProc := @CtrlBreakHandler;
  // DosSetSignalExceptionFocus (True, Times);

{$ENDIF}

  tWinInit;
  CheckBreak := False;
  GetDir (0, InstDir);

  LastAnswer := 1;
  SetBlink (False);
  Upgrade := False;

  If {$IFNDEF VirtualPascal} (CurrentDisplay in [EGA, VGA]) And {$ENDIF} (Not Mono)
  Then ColorScheme := ColorScheme1
  Else ColorScheme := ColorScheme2;

  iPath := 'C:\TORNADO';
  Items := New (PNotSortedCollection, Init (5, 5));

  WX := WhereX;
  WY := WhereY;
  MenPos := 1;
  PrevMenPos := 0;
  DocLang := lEnglish; IntfLang := lEnglish;
  SaveWindow (1, 1, Lo (WindMax)+1, Hi (WindMax)+1, True, SaveScr);
  HiddenCursor;

  FastWrite (Replicate (' ', 80), 1, 1, ColorScheme [StatusLine]);
  FastWrite (Replicate (' ', 80), Hi (WindMax) + 1, 1, ColorScheme [StatusLine]);
  FastWrite (CenterCh (NameVer + ' install program. (c) Konstantin Klyagin, 1997-98',
                                 ' ', 80), 1, 1, ColorScheme [StatusLine]);

  InitWindow (Box1, 11, 5, 68, 19, 4, ColorScheme [Frames], ' Installation options ', ColorScheme [Headers], 5, True);
  DrawWindow (Box1);

  SetLanguage (lEnglish);

  Loop:

  Case MenPos Of
    1 : ChangeAttribute (56, Box1^. Y1 + 2, 13, ColorScheme [MenuItems]);
    2 : ChangeAttribute (56, Box1^. Y1 + 4, 13, ColorScheme [MenuItems]);
    3 : ChangeAttribute (56, Box1^. Y1 + 5, 13, ColorScheme [MenuItems]);
    4 : ChangeAttribute (56, Box1^. Y1 + 6, 13, ColorScheme [MenuItems]);
    5 : ChangeAttribute (56, Box1^. Y1 + 8, 13, ColorScheme [MenuItems]);
    6 : ChangeAttribute (56, Box1^. Y1 + 9, 13, ColorScheme [MenuItems]);
    7 : ChangeAttribute (56, Box1^. Y1 + 11, 13, ColorScheme [MenuItems]);
    8 : ChangeAttribute (56, Box1^. Y1 + 12, 13, ColorScheme [MenuItems]);
  End;

  If MenPos <> PrevMenPos Then ItemShow (PrevMenPos);

  Ch := ReadKey;
  If Ch = #0 Then Ch := ReadKey;
  PrevMenPos := MenPos;

  Case Ch Of
    #75, #72 : {Up  } If MenPos > 1 Then Dec (MenPos);
    #77, #80 : {Down} If MenPos < 8 Then Inc (MenPos);
    #71, #73 : {Home} MenPos := 1;
    #79, #81 : {End } MenPos := 8;
    #27      : ExitInst;
    #13, #32 : Case MenPos Of
                 1 : Begin
                       Upgrade := Not Upgrade;
                       If Upgrade Then
                       Begin
                         FastWrite (PadCh (Language [Upgr], ' ', Length
                                   (Language [Upgr])), Box1^. Y1 + 2,
                                   40, ColorScheme [MenuItems]);
                         FastWrite (Language [Previous], Box1^. Y1 + 9, 14,
                                   ColorScheme [Frames]);
                       End Else
                       Begin
                         FastWrite (PadCh (Language [InstallNew], ' ', Length
                                   (Language [Upgr])), Box1^. Y1 + 2,
                                   40, ColorScheme [MenuItems]);
                         FastWrite (PadCh (Language [InstallTo], ' ', Length
                                   (Language [Previous])), Box1^. Y1 + 9, 14,
                                   ColorScheme [Frames]);
                       End;
                       ItemShow (4);
                     End;
                 2 : Begin
                       If DocLang = lEnglish
                         Then DocLang := lRussian
                         Else DocLang := lEnglish;
                       FastWrite (LangName [DocLang], Box1^. Y1 + 4, 40, ColorScheme [MenuItems])
                     End;
                 3 : Begin
                       If IntfLang = lEnglish Then IntfLang := lRussian Else
                       If IntfLang = lRussian Then IntfLang := lGerman
                       Else IntfLang := lEnglish;

                       FastWrite (PadCh (LangName [IntfLang], ' ', 8), Box1^. Y1 + 5, 40, ColorScheme [MenuItems]);
                       SetLanguage (IntfLang);
                     End;
                 4 : Begin
                       If lngEnglish and lngRussian and lngGerman Then lngGerman := False Else
                       If lngEnglish and lngRussian and not lngGerman Then
                       Begin
                         lngRussian := False;
                         lngGerman := True;
                       End Else
                       If lngEnglish and lngGerman and not lngRussian Then
                       Begin
                         lngEnglish := False;
                         lngRussian := True;
                       End Else
                       If lngRussian and lngGerman and not lngEnglish Then
                       Begin
                         lngRussian := False;
                         lngGerman := False;
                         lngEnglish := True;
                       End Else
                       If lngEnglish and not lngRussian and not lngGerman Then
                       Begin
                         lngEnglish := False;
                         lngRussian := True;
                       End Else
                       If not lngEnglish and lngRussian and not lngGerman Then
                       Begin
                         lngGerman := True;
                         lngRussian := False;
                       End Else
                       If lngGerman and not lngRussian and not lngEnglish Then
                       Begin
                         lngRussian := True;
                         lngEnglish := True;
                       End;

                       FastWrite (FormLangs, Box1^. Y1 + 6, 40, ColorScheme [MenuItems]);
                     End;
                 5 : Begin
                       i := 0;
                       While Not (i in [27, 13]) Do
                         St := Input (40, Box1^. Y1 + 8, InstDir, ' ', '', 29, 0, [#32..#255],
                               True, i, ColorScheme [Inp], Changes);
                       If i <> 27 Then InstDir := St;
                       HiddenCursor;
                       FastWrite (PadCh (InstDir, ' ', 29), Box1^. Y1 + 8, 40, ColorScheme [MenuItems]);
                     End;
                 6 : Begin
                       i := 0;
                       While Not (i in [27, 13]) Do
                         St := Input (40, Box1^. Y1 + 9, iPath, ' ', '', 29, 0, [#32..#255],
                               True, i, ColorScheme [Inp], Changes);
                       If i <> 27 Then iPath := St;
                       HiddenCursor;
                       FastWrite (PadCh (iPath, ' ', 29), Box1^. Y1 + 9, 40, ColorScheme [MenuItems]);
                     End;
                 7 : GoTo Start;
                 8 : ExitInst;
               End;
  End;

  If (MenPos = 4) And Upgrade Then
  If PrevMenPos > MenPos
  Then Dec (MenPos)
  Else Inc (MenPos);

  GoTo Loop;
  Start:

  If (iPath [Length (iPath)] = '\') And (Length (iPath) > 3) Then iPath [0] := Chr (Length (iPath)-1);
  If iPath = '' Then GoTo Loop;

  {If Upgrade Then VerInstalled := DetectVer (iPath);}

  If (Not FileExists (AddBackSlash (InstDir) + 'tbase.zip')) Or
     (Not FileExists (AddBackSlash (InstDir) + 'tctl.zip')) Or
     (Not FileExists (AddBackSlash (InstDir) + 'tdoc.zip')) Or
     (Not FileExists (AddBackSlash (InstDir) + 'tmisc.zip')) Then
  Begin
    CenterTempBox (ColorScheme [Frames], ColorScheme [Buttons], 1,
                   Language [InstNotFound], 5, 'Warning');
    GoTo Loop;
  End;

  If Not DirExists (iPath) Then
  Begin
    HiddenCursor;
    {$I-} MkDir (iPath);
    {$I+} If IOResult <> 0 Then
          Begin
            CenterTempBox (ColorScheme [Frames], ColorScheme [Buttons], 1,
                           Language [InvalidPath], 5, 'Warning');
            GoTo Loop;
          End;
    NormalCursor;
  End;

  ChDir (iPath);

  If CheckFreeSpace Then
  Begin
    Free := DiskFree (0);
    If Free < 0 Then Free := 2147483647;
    If Free < SpaceNeed Then
    Begin
      HiddenCursor;
      CenterTempBox (ColorScheme [Frames], ColorScheme [Buttons], 1,
                     Language [DiskSpace] + Long2Str (SpaceNeed) +
                     Language [BytesNeed], 5, 'Warning');
      NormalCursor;
      GoTo Loop;
    End;
  End;

  CloseWindow (Box1);

  TextAttr := ColorScheme [BoxText];
  HiddenCursor;
  InitWindow (Box2, 5, 18, 75, 23, 4, ColorScheme [Frames], '', ColorScheme [Headers], 5, False);
  InitWindow (Box1, 5, 3, 75, 16, 4, ColorScheme [Frames], Language [TorInst], ColorScheme [Headers], 5, False);

  DrawWindow (Box1);
  DrawWindow (Box2);

  FastWrite (Language [Archive], 19, 7, ColorScheme [BoxText]);
  FastWrite (Language [mFile], 19, 28, ColorScheme [BoxText]);

  FastWrite (Replicate (' ', 62), 21, 7, ColorScheme [Progress]);
  FastWrite ('  0%', 21, 70, ColorScheme [Headers]);

  SetShowCommentsProcZip (ShowCommentsProcZip);
  SetShowMethodProcZip (ShowMethodProcZip);
  SetExtractSuccessFuncZip (ExtractSuccessFuncZip);
  SetShowProgressFuncZip (ShowProgressF);
  SetOkToWriteFuncZip (OKF);

  UnpackFiles (Extract);

  CloseWindow (Box1);
  CloseWindow (Box2);

  If Upgrade Then
  Begin
    CenterDelayTempBox (ColorScheme [Frames], ColorScheme [HighLight], 1,
                        CenterCh (Language [UpdCTL], ' ', 30),
                        ZoomSpeed, '');

    {$I-} ChDir (iPath); {$I+}
    {UpdateConfigFiles ('tornado.upd', VerInstalled);}

    {$I-}
    ChDir (InstDir);

    {$I+}

    DoneTempBox;

    CenterTempBox (ColorScheme [Frames], ColorScheme [Buttons], 1,
                   Language [UpgrCompl1] + NameVer +
                   Language [UpgrCompl2], 5, 'Completed');
    ExitInst;
  End;

  i := 1;
  InitWindow (Box1, 10, 5, 70, 20, 2, ColorScheme [Frames], Language [TorInst], ColorScheme [Headers], 5, True);
  DrawWindow (Box1);

  With StVal Do
  Begin
    SysOp := 'Konstantin Klyagin';
    BBSName := 'Pulse BBS';
    Location := 'Paradise';
    ComPort := '2';
    BaudRate := 19200;
    InitString := 'ATZ|';
    AnswerString := 'ATA|';

    FastWrite (Language [StartupVal1], Box1^. Y1+2, 13, ColorScheme [Welcome]);
    FastWrite (Language [StartupVal2], Box1^. Y1+3, 13, ColorScheme [Welcome]);

    FastWrite ('Ç' + Replicate ('Ä', 59) + '¶', Box1^. Y1+5, 10, ColorScheme [Frames]);

    FastWrite (Language [SysOpName], Box1^. Y1+6, 12, ColorScheme [Headers]);
    FastWrite (PadCh (SysOp, '°', 36), Box1^. Y1+6, 25, ColorScheme [InputLines]);

    FastWrite (Language [BBS], Box1^. Y1+7, 12, ColorScheme [Headers]);
    FastWrite (PadCh (BBSName, '°', 36), Box1^. Y1+7, 25, ColorScheme [InputLines]);

    FastWrite (Language [Locat], Box1^. Y1+8, 12, ColorScheme [Headers]);
    FastWrite (PadCh (Location, '°', 36), Box1^. Y1+8, 25, ColorScheme [InputLines]);

    FastWrite (Language [Port], Box1^. Y1+10, 12, ColorScheme [Headers]);
    FastWrite (ComPort, Box1^. Y1+10, 25, ColorScheme [InputLines]);

    FastWrite (Language [Baud], Box1^. Y1+11, 12, ColorScheme [Headers]);
    FastWrite (Long2Str (BaudRate), Box1^. Y1+11, 25, ColorScheme [InputLines]);

    FastWrite (Language [InitStr], Box1^. Y1+10, 37, ColorScheme [Headers]);
    FastWrite (PadCh (InitString, '°', 11), Box1^. Y1+10, 50, ColorScheme [InputLines]);

    FastWrite (Language [AnswerStr], Box1^. Y1+11, 37, ColorScheme [Headers]);
    FastWrite (PadCh (AnswerString, '°', 11), Box1^. Y1+11, 50, ColorScheme [InputLines]);

    FastWrite ('Ç' + Replicate ('Ä', 59) + '¶', Box1^. Y1+13, 10, ColorScheme [Frames]);

    _SysOp:
    SysOp := Trim (Input (25, Box1^. Y1+6, SysOp, '°', '', 36, 0, [#32..#255], True,
             Keyb, ColorScheme [InputLines], Changes));

    Case Keyb Of
      13, 80 : ;
      72     : GoTo _AnswerString;
      27     : GoTo StValEnd;
    Else
      GoTo _SysOp;
    End;

    _BBSName:
    BBSName := Trim (Input (25, Box1^. Y1+7, BBSName, '°', '', 36, 0, [#32..#255], True,
               Keyb, ColorScheme [InputLines], Changes));

    Case Keyb Of
      13, 80 : ;
      72     : GoTo _SysOp;
      27     : GoTo StValEnd;
    Else
      GoTo _BBSName;
    End;

    _Location:
    Location := Trim (Input (25, Box1^. Y1+8, Location, '°', '', 36, 0, [#32..#255], True,
                Keyb, ColorScheme [InputLines], Changes));

    Case Keyb Of
      13, 80 : ;
      72     : GoTo _BBSName;
      27     : GoTo StValEnd;
    Else
      GoTo _Location;
    End;

    _ComPort:
    ComPort := Trim (Input (25, Box1^. Y1+10, ComPort, '°', '', 1, 0, ['1'..'9'], False,
               Keyb, ColorScheme [InputLines], Changes));

    Case Keyb Of
      13, 80 : ;
      72     : GoTo _Location;
      27     : GoTo StValEnd;
    Else
      GoTo _ComPort;
    End;

    _BaudRate:
    NormalCursor;
    GoToXY (25, Box1^. Y1+11);
    While (Rates [i] <> BaudRate) and (i < 9) Do Inc (i);
    FastWrite (PadCh (Long2Str (Rates [i]), ' ', 6), Box1^. Y1+11, 25, ColorScheme [InputLines]);

    Case ReadKey Of
      #13, #32 : Begin
                   Inc (i);
                   If i >= 9 Then i := 1;
                   BaudRate := Rates [i];
                   GoTo _BaudRate;
                 End;

      #72      : GoTo _ComPort;
      #80      : GoTo _InitString;
      #27      : GoTo StValEnd;
    Else
      GoTo _BaudRate;
    End;

    _InitString:
    InitString := Trim (Input (50, Box1^. Y1+10, InitString, '°', '', 11, 0, [#32..#255], True,
                Keyb, ColorScheme [InputLines], Changes));

    Case Keyb Of
      13, 80 : ;
      72     : GoTo _BaudRate;
      27     : GoTo StValEnd;
    Else
      GoTo _InitString;
    End;

    _AnswerString:
    AnswerString := Trim (Input (50, Box1^. Y1+11, AnswerString, '°', '', 11, 0,
                    [#32..#255], True, Keyb, ColorScheme [InputLines], Changes));

    Case Keyb Of
      13, 80 : GoTo _SysOp;
      72     : GoTo _InitString;
      27     : GoTo StValEnd;
    Else
      GoTo _AnswerString;
    End;

    StValEnd:

    FastWrite (Language [UpdCTL], Box1^. Y1+14, 14, ColorScheme [HighLight]);
    UpdConfigs (nil);
  End;

  {$I+}

  Box1^. DelayTime := 0;
  CloseWindow (Box1);

  Completed:
  HiddenCursor;
  CenterTempBox (ColorScheme [Frames], ColorScheme [Buttons], 1,
                 NameVer + Language [Complete], 5, 'Completed');

  ExitInst;
End.
