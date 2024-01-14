{$IFDEF MSDOS}
  {$IFNDEF DPMI32}
    {$F+}
  {$IFNDEF DPMI}
    {$DEFINE RealMode}
    {$M 55520,0,655360}
  {$ENDIF}
  {$ENDIF}
{$ENDIF}

{$IFNDEF RealMode}
  {$M 65520}
{$ENDIF}

{&Delphi+}

Program Tornado (Input, Output);

Uses
{$IFDEF WIN32}
  Windows,
  ApWin32,
{$ENDIF}

{$IFDEF VirtualPascal}
  VpSysLow,
{$ENDIF}

{$IFDEF MSDOS}
{$IFDEF RealMode}
  Overlay,
  ExecSwap,
  Streams,
{$ENDIF}
{$IFNDEF DPMI32}
  NTVDMSVC,
  Vector,
  ApUart,
  ApDigi14,
  ApInt14,
  ApFossil,
{$ELSE}
  ApFos32,
{$ENDIF}
  ApMisc,
  tDebug,
  Memory,
{$ENDIF}

{$IFDEF OS2}
  OS2Base,
  VPutils,
  ApOS2,
  OS2Def,
  Strings,
  tRexx,
{$ENDIF}

  ApPort,
  ApTimer,
  ApSame,
  DOS,
  ApAbsPcl,
  ApXModem,
  ApYModem,
  ApZModem,
  ApCom,
  Objects,
  OpCrt,
  tWin,
  ILine,
  Log,
  Users,
  skCommon,
  skMHL,
  skMHLjam,
  skMHLmsg,
  skMHLsq,
  skOpen,
  Parse,
  BinCfg,
  tMisc,
  tMenus,
  Iface,
  MainComm,
  FilesBBS,
  UserEd,
  IBM_PKZ,
  IBM_LHA,
  IBM_ARJ,
  IBM_SQZ,
  IBM_ARC,
  IBM_HYP,
  IBM_ZOO,
  IBM_RAR,
  CompSys,
  ADir,
  Shell,
  SysMsgs,
  Protocol,
  TimeTask,
  Parser,
  tMainOvr,
  Areas,
  SaveTag,
  Upgrader,
  Tree,
  DoorWay,
  FCache,
  RCache,
  TGlob,
  DoReg,
  TorMacro,
  TorInOut,
  MainCTL,
  tScript,
  Ansi,
  MainCOvr,
  iEMSI,
  tQWK,
  tFSed,
  Multi,
  tBigFunc;

{$IFDEF RealMode}
  {$I overlay.inc}
{$ENDIF}

Type
  PT = Procedure;

Var
{$IFDEF MSDOS}
{$IFNDEF DPMI32}
  NewBase, NewIrq, NewVector : Word;
{$ENDIF}
{$IFDEF RealMode}
  OvrStream                  : PStream;
  OvrBufInc, OvrRetrySize    : Integer;
{$ENDIF}
{$ENDIF}

{$IFDEF OS2}
  PrevXcptProc               : Pointer;
  Times                      : ULong;
  TempChar                   : Array [0..19] Of Char;
{$ENDIF}

  DBits                      : DataBitType;
  SBits                      : StopBitType;
  Parity                     : ParityType;
  ModemResult                : tModemResponse;
  sLayer                     : String [30];
  Timer                      : EventTimer;
  InitOk, ExitRequest        : Boolean;
  InitTry, Rings             : Byte;
  C                          : Char;

{$IFDEF MSDOS}
  {$IFNDEF DPMI32}
    Procedure CBreakProc; Interrupt;
    Begin
    End;
  {$ELSE}
    Function CBreakProc: Boolean;
    Begin
      CBreakProc := True;
    End;
  {$ENDIF}
{$ENDIF}

{$IFDEF OS2}
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
    xcpt_Signal_KillProc : Begin
                             Exeption := True;
                             LogWrite (':', sm (smOsReqTerm));
                             NormExit;
                           End;
  End;

  XcptProc := PrevXcptProc;
End;
{$ENDIF}

{$IFDEF WIN32}
Function CtrlHandler (fdwCtrlType: DWord): Bool; stdcall;
Begin
  Result := True;
  Case fdwCtrlType Of
    Ctrl_Close_Event    : ExitNowFlag := True;
    Ctrl_LogOff_Event,
    Ctrl_ShutDown_Event : Begin
                            LogWrite (':', sm (smOsReqTerm));
                            Exeption := True;
                            ExitNowFlag := True;
                          End;
  End;
End;
{$ENDIF}

Procedure ThreadStart (P: PT);
{$IFNDEF OS2}
Begin
  P;
End;
{$ELSE}
Var
  TID : LongInt;

Begin
  BeginThread (Nil, 25600, PreThread, @P, 0, TID);
End;
{$ENDIF}

Procedure AnalyzeKey (C: Char); Far;
Begin
  Case C Of
    #35: NormExit;                   { .Alt-H. }
    #46: If Not InChat Then          { .Alt-C. }
         Begin
           ManualChat := True;
           Chat;
           ManualChat := False;
         End;
    #68: SwitchStatusBar;            {. F10 .}
    #61: IncTime;                    {. F3 .}
    #62: DecTime;                    {. F4 .}
    #59: ThreadStart (Help);         {. F1 .}
    #60: ThreadStart (UserParams);   {. F2 .}
    #31: ThreadStart (AccessLevel);  { .Alt-S. }
    #18: ThreadStart (UEdit);        { .Alt-E. }
    #50: MessageToUser;              { .Alt-M. }
    #25: PageSwitch;                 { .Alt-P. }
    #20: ThreadStart (ViewTagList);  { .Alt-T. }
    #38: LockOut;                    { .Alt-L. }
    #22: ManualSend;                 { .Alt-U. }
    #23: InsertFile;                 { .Alt-I. }
    #36: Begin                       { .Alt-J. }
           LogWrite (':', sm (smDOSShell));
           DosShell ('', exCommand, False);
         End;
  {$IFDEF MSDOS}
    #32: DebugInfo;                  { .Alt-D. }
  {$ENDIF}
  End;
End;

Function KeyBoard (C: Char): Boolean; Far;
Begin
  AnalyzeKey (C);
  KeyBoard := True;
End;

Procedure ParseCommandLine;
Var
  i           : Integer;
  Err         : SysInt;
  Key         : String [2];
  pStr, Value : String;

Begin
  For i := 1 To ParamCount Do
  Begin
    pStr := PlaceSubStr (ParamStr (i), '##', '&');
    Key := UpString (Copy (pStr, 1, 2));
    Value := Copy (pStr, 3, 255);

    If ((Key = '-?') Or (Key = '/?') Or (Key [1] = '?')) Then
    Begin
      WriteLn ('  Command line parameters:');
      WriteLn;
      WriteLn ('-K<filename>    - Set config file name');
      WriteLn ('-C<num>         - Set comm. port');
      WriteLn ('-B<baud>        - Set baud rate');
      WriteLn ('-E<min>         - Time to next event');
      WriteLn ('-M<command>     - Command to put into modem after start');
    {$IFDEF RealMode}
      WriteLn ('-O<filename>    - Set overlay file name');
      WriteLn ('-V<size,[size]> - Expand overlay buffer [and set retry] to <size> Kb');
    {$ENDIF}
    {$IFDEF WIN32}
      WriteLn ('-H<handle>      - ComPort handle');
    {$ENDIF}
    {$IFDEF OS2}
      WriteLn ('-H<handle>      - ComPort handle');
      WriteLn ('-T<filename>    - Run Tornado REXX file');
    {$ENDIF}
      WriteLn ('-U<user_name>   - Set users''s name');
      WriteLn ('-N<node_num>    - Run Tornado node <node_num> (multiline mode)');
      WriteLn ('-L              - Execute in local mode');
    {$IFDEF MSDOS}
      WriteLn ('-R              - Do not release timeslices');
    {$ENDIF}
      WriteLn ('-S<filename>    - Run TRS script file');
      WriteLn;
      Halt (0);
    End;

    If Key = '-K' Then
    Begin
      If Trim (Value) <> '' Then
        ConfigName := Value;
    End Else
    If Key = '-O' Then
  {$IFDEF RealMode}
      OvrName := Value
  {$ENDIF}
    Else
    If Key = '-V' Then
    Begin
  {$IFDEF RealMode}
      OvrBufInc := Str2Long (ExtractWord (1, Value, MinusOnly));
      OvrRetrySize := Str2Long (ExtractWord (2, Value, MinusOnly));
  {$ENDIF}
    End
    Else
    If Key = '-U' Then
      ReqName := PrString (PlaceSubStr (Value, '_', ' '))
    Else
    If Key = '-S' Then
      RunScript := Value
    Else
    If Key = '-M' Then
      mCommand := Value
    Else
    If Key = '-E' Then
      ToEventTime := Str2Long (Value)
    Else
    If (Key = '-L') And (Value = '') Then
    Begin
      Local := True;
      reqBaud := 0;
      Cnf. ComPort := {$IFNDEF OS2} 0 {$ELSE} 'COM1' {$ENDIF};
    End Else
    If (Key = '-R') And (Value = '') Then
    Begin
  {$IFDEF MSDOS}
      Task. OS := 255;
      WriteLn ('þ TimeSlices will not be released');
  {$ENDIF}
    End Else
    If Key = '-C' Then
    Begin
  {$IFNDEF OS2}
      Val (Value, reqPort, Err);
  {$ELSE}
      reqPort := Value;
      If (Length (reqPort) <= 2) And (reqPort [1] in ['0'..'9']) Then
        reqPort := 'COM' + reqPort;
  {$ENDIF}
      AutoMode := True;
      Local := False;
    End Else
    If Key = '-B' Then
    Begin
      Val (Value, reqBaud, Err);
      AutoMode := True;
      Local := False;
    End Else
    If Key = '-N' Then
      Val (Value, BbsLine, Err)
    Else
  {$IFDEF WIN32}
    If Key = '-H' Then
    Begin
      Val (Value, ApWin32. ComHandle, Err);
      AutoMode := True;
      Local := False;
    End Else
  {$ENDIF}
  {$IFDEF OS2}
    If Key = '-H' Then
    Begin
      Val (Value, ApOS2. ComHandle, Err);
      ApOS2. HandleExist := True;
      AutoMode := True;
      Local := False;
    End Else
    If Key = '-T' Then
      RunRexx := Value
    Else
  {$ENDIF}
    Begin
      TextAttr := 12;
      WriteLn ('! Invalid command line parameter: "' + pStr + '"');
      TextAttr := 7;
      WriteLn;
      Halt (0);
    End;
  End;
End;

{$IFDEF RealMode}
Procedure OverlayLoad;

  Procedure OvrErr (Const Msg: String);
  Begin
    TextAttr := 12;
    WriteLn ('! ' + Msg);
    If Cnf. Sound Then
      SoundOf ('1 200 1 2 800 1000 -1 2 800');
    TextAttr := 7;
    Write;
    NormalCursor;
    Halt (209);
  End;

Var
  OvrSize  : LongInt;
  XMS, EMS : Boolean;

Begin
  Case OvrResult Of
      OvrOk : ;
      OvrNotFound : OvrErr (sm (smOvrOpenErr));
  Else
    OvrErr (sm (smOvrLoadErr));
  End;

{$IFDEF DEBUG}
  WriteLn ('þ Overlay buffer size: ', OvrGetBuf);
{$ENDIF}

  XMS := False;
  EMS := False;
  OvrSize := OvrSizeNeeded;

  If Cnf. Overlay_EMS And (OvrSize < EMS_MemAvail) Then
  Begin
  { OvrInitEMS;
    If OvrResult = 0 Then
      EMS := True; }
    OvrStream := New (PEMSStream2, Init (OvrSize, OvrSize));

    If OvrStream <> Nil Then
      If OvrStream^. Status <> stOk Then
        Dispose (OvrStream, Done)
      Else
      Begin
        OvrInitStream (OvrStream);
        EMS := True;
        WriteLn ('þ ' + sm (smOvrLoading) + ' EMS..');
      End;
  End;

  If Not EMS And (Cnf. Overlay_XMS And (OvrSize < XMS_MemAvail)) Then
  Begin
    OvrStream := New (PXMSStream, Init (OvrSize, OvrSize));

    If OvrStream <> Nil Then
      If OvrStream^. Status <> stOk Then
        Dispose (OvrStream, Done)
      Else
      Begin
        OvrInitStream (OvrStream);
        XMS := True;
        WriteLn ('þ ' + sm (smOvrLoading) + ' XMS..');
      End;
  End;

  If Not (EMS Or XMS) Then
  Begin
    WriteLn ('þ ' + sm (smOvrDisk));
    OvrDiskReads := 0;
    OvrMemReads := 0;
  End;
End;
{$ENDIF}

Procedure CheckFiles;
Var
  LogString : String [100];
  Fatal     : Boolean;

  Procedure LogNotFound (Const Name: String);
  Begin
    LogString := sm (smFile) + NiceFileName (Name, 50) + sm (smNotFound);
  End;

Begin
  AddMsg (ShortStrTime + sm (smTestFiles), False, Cnf. ColorScheme [cmUsers]);
  Fatal := False;
  LogString := '';

  If Not FileExists (Cnf. FileAreasFile) Then
    LogNotFound (Cnf. FileAreasFile);

  If Not FileExists (Cnf. MsgAreasFile) Then
    LogNotFound (Cnf. MsgAreasFile);

  If Not FileExists (Cnf. LimitsFile) Then
  Begin
    LogNotFound (Cnf. LimitsFile);
    Fatal := True;
  End;

  If Not FileExists (Cnf. DefLangFile) Then
  Begin
    LogNotFound (Cnf. DefLangFile);
    Fatal := True;
  End;

  If LogString <> '' Then
  Begin
    AddMsg ('', True, $00);
    AddMsg (ShortStrTime + ' ' + LogString, False, Cnf. ColorScheme [cmUsers]);
    LogWrite ('!', LogString);
    Pause (2000);
    If Fatal Then
      ErrorExit;
    AddMsg ('', True, $00);
  End
  Else
    AddMsg (' Ok', True, Cnf. ColorScheme [cmUsers]);
End;

Function InitWaiter: Boolean; Far;
Begin
  If KeyPressed And (ReadKey = #27) Then
    InitWaiter := False
  Else
  Begin
    TimeSlice;
    Clock;
    TimeSlice;
    InitWaiter := Not (KeyPressed And (ReadKey = #27));
  End;
End;

Function ConnectWaiter: Boolean; Far;
Begin
  If KeyPressed And (ReadKey = #27) Then
    ConnectWaiter := False
  Else
  Begin
    TimeSlice;
    TickTimer;
    TimeSlice;
    Clock;
    TimeSlice;
    ConnectWaiter := Not (KeyPressed And (ReadKey = #27));
  End;
End;

Function CallWaiter: Boolean; Far;
Begin
  CallWaiter := True;

  If KeyPressed Then
  Begin
    Case ReadKey Of
      #27 : Begin
              ExitRequest := True;
              CallWaiter := False;
              Exit;
            End;
       #0 : If KeyPressed Then
              Case ReadKey Of
                #38 : Begin
                        Local := True;
                        iFace. oX := 2;
                        SetTitle ('entering local mode');
                        AddMsg (ShortStrTime + ' ' + sm (smLocalLogOn), False,
                          Cnf. ColorScheme [cmUsers]);
                        LogWrite (':', sm (smLocalLogOn));
                        If Cnf. LocalOffHook Then
                        Begin
                          LocalOffHook := True;
                          DoCommand (Cnf. OffHookString);
                        End;
                        DoBBS;
                        Exit;
                      End;
                #30 : Begin
                        Answer := True;
                        Manual := True;
                        CallWaiter := False;
                        Exit;
                      End;
              End;
    End;
  End;

  TimeSlice;
  Clock;
  TimeSlice;
End;

Procedure InitErr (Const Msg: String);
Begin
  WriteLn ('! ' + Msg);
  If Cnf. Sound Then
    SoundOf ('1 200 1 2 800 1000 -1 2 800');
  ExitProc := SavedExitProc;
  NormalCursor;
  Halt (205);
End;

Procedure Launch;
Begin
  If Cnf. Sound Then
    SoundOf ('1 100 10 100 4 100 -3 100 4');

  AddMsg ('', True, $00);
  AddMsg (ShortStrTime + ' ' + sm (smConnectSpeed) + Long2Str (GetConnectSpeed)
    + sm (smBaud), True, Cnf. ColorScheme [cmUsers]);
  LogWrite (':', sm (smIncomingCall) + LastResponse);
  LogWrite (':', sm (smConnectSpeed) + Long2Str (GetConnectSpeed) +
    sm (smBaud));
  DoBBS;
End;

Label
  RingRecv;

Begin
  SavedExitProc := ExitProc;

  System. FileMode := Open_Access_ReadOnly Or Open_Share_DenyNone;
  OpCrt. DirectVideo := True;
  CheckBreak := False;

{$IFDEF MSDOS}
{$IFNDEF DPMI32}
  If NTVDMInitOk Then
    NTVDMInstallCtrlHandler;
  SetIntVec ($23, Addr (CBreakProc));
  GetCBreak (CtrlCState);
  SetCBreak (CheckBreak);
{$ELSE}
  CtrlBreakHandler := CBreakProc;
{$ENDIF}
{$ENDIF}

{$IFDEF OS2}
  PrevXcptProc := XcptProc;
  XcptProc := @CtrlBreakHandler;
  DosSetSignalExceptionFocus (True, Times);
  Os2Base. DosError (0);
  SetANSI (False);

  ThreadLocked := False;
  NeedThreadClose := False;
{$ENDIF}

{$IFDEF WIN32}
  SetConsoleMode (SysFileStdIn, 0);
  SetConsoleCtrlHandler (@CtrlHandler, True);
{$ENDIF}

  {SaveTitle := GetTitle;}
  SetTitle ('starting');
  Exeption := False;
  Local := False;
  ToEventTime := 0;

  TorDir := AddBackSlash (UpString (JustPathName (ParamStr (0))));
{$IFDEF RealMode}
  OvrName := TorDir + JustName (ParamStr (0)) + '.OVR';
  OvrBufInc := 0;
  OvrRetrySize := 0;
{$ENDIF}

  GetDir (0, oDir);
  SmartChDir (TorDir);

  ConfigName := TorDir + 'tornado.ctl';
  ReqName := '';
  Registering := False;
  AutoMode := False;
  reqBaud := 0;

  TextAttr := $0A;
  WriteLn;
  WriteLn ('* ' + NameVer + ' started at ' +
    FormattedCurrDT ('HH:II DD-MM-YYYY'));

  TextAttr := 3;
  ParseCommandLine;

  If Task. OS <> 255 Then
  Begin
    InitMulti;

    If Task. OS <> 0 Then
    Begin
      Write ('þ Running under ' + MTaskers [Task. OS]);
      If Task. Version <> 0 Then
        WriteLn (' ' + Long2Str (Hi (Task. Version)) + '.' + Long2Str (Lo (Task. Version)))
      Else
        WriteLn;
    End
    Else
      WriteLn ('þ No multitasking environment detected');
  End
  Else
    Task. OS := 0;

{$IFDEF RealMode}
  Overlay. OvrFileMode := 0;
  OvrInit (OvrName);
  If (OvrBufInc > 0) And (OvrResult = 0) Then
    OvrSetBuf (OvrGetBuf + LongInt (OvrBufInc) * 1024);
  If (OvrRetrySize > 0) And (OvrResult = 0) Then
    OvrSetRetry (LongInt (OvrRetrySize) * 1024);

  If MemAvail < 100 * 1024 Then
  Begin
    TextAttr := 12;
    WriteLn ('þ Not enough memory to run the program.');
    TextAttr := 7;
    WriteLn ('');
    Halt (212);
  End;

{$IFDEF DEBUG}
  WriteLn ('þ Conventional memory free: ' + Long2Str (MemAvail));
{$ENDIF}
{$ENDIF}

  ResRead (CompletePath (JustPathName (ParamStr (0))) + 'tornado.msg');

  WriteLn (sm (smCtlRead) + NiceFileName (CompletePath (JustPathName
    (ConfigName)) + JustFileName (ConfigName), 50));

  TextAttr := 12;
  ReadMainConfig (ConfigName, Cnf);

  If ((RunScript <> '') Or (RunRexx <> '')) And (ReqName = '') Then
    ReqName := PrString (Cnf. SysOp);

  If Trim (Cnf. LogFile) <> '' Then
    LogOpen (Cnf. LogFile, Cnf. LogEvents, sm (smLogCreated));

  If AutoMode Then
  Begin
    If reqBaud = 0 Then
      reqBaud := Cnf. BaudRate;
  {$IFNDEF OS2}
    If reqPort <> 0 Then Cnf. ComPort := reqPort
                    Else reqPort := Cnf. ComPort;
  {$ELSE}
    If reqPort <> '' Then Cnf. ComPort := reqPort
                     Else reqPort := Cnf. ComPort;
  {$ENDIF}
  End;

  ZoomSpeed := 0;
  WantsChat := False;

  HotKeysStr := '';
  SetBlink (Cnf. Blinking);

  HiddenCursor;
  TextAttr := 3;

{$IFDEF RealMode}
  OverlayLoad;
{$ENDIF}

  OpenResourceCache;

  OpenFileGroups;
  CloseFileGroups;
  OpenFileAreas;
  CloseFileAreas;
  OpenMsgGroups;
  CloseMsgGroups;
  OpenMsgAreas;
  CloseMsgAreas;

  TWinInit;
  InitMainCOvr;
  InitMultiUnit;
  InitShellUnit;
  InitTBigFunc;
  InitTMainOvr;
  InitDeviceTable;
  PrepareScriptTables;

  If Cnf. Effects Then ZoomSpeed := 5
                  Else ZoomSpeed := 0;
  TextAttr := 12;

  ExitProc := @ErrorExit;

  If DirExists (Cnf. Path) Then
    SmartChDir (Cnf. Path)
  Else
    InitErr (sm (smWorkDirNotFound));

  If Not DirExists (Cnf. TempDir) Or Not DirExists (Cnf. DoorInfoDir) Then
    InitErr (sm (smTempDirNotFound));

  If (UpString (Cnf. TempDir) = UpString (Cnf. Path)) Or
     (UpString (Cnf. DoorInfoDir) = UpString (Cnf. Path))
  Then
    InitErr (sm (smTempDoorNPath));

  WXG := WhereX;
  WYG := WhereY;
  StatusBar := False;
  StatusBarEnable := False;
  EnterTime := MidSec;
  InChat := False;
  InShell := False;
  InAction := False;
  EnteringPass := False;
  SizeOfAll := 0;
  SessionDL := 0;
  SessionUL := 0;
  AutoDL := False;
  NetMailEntered := False;
  EchoMailEntered := False;
  WaitReturn := MainComm. RetWait;
  ExecScript := tExecScript;
  ExecRexx := tExecRexx;
  Randomize;

  Menus := New (PNotSortedCollection, Init (4, 4));
  MenuItems := New (PMenuItemsCollection, Init (16, 4));
  MsgText := New (PBigCollection, Init (0, 16));
  Reps := New (PBigCollection, Init (0, 4));
  UpFiles := New (PBigCollection, Init (0, 4));
  XLATs := New (PNotSortedCollection, Init (2, 2));
  qwkAreas := New (PLongIntCollection, Init (0, 4));
  Screens := New (PNotSortedCollection, Init (1, 1));
  F2Transfer := New (PTagFilesCollection, Init (0, 8));
  Language := New (PBigCollection, Init (laLastLanguageNum + 1, 16));
  ReadHistory := New (PBigCollection, Init (0, 4));

  InitCompSys;

  If Not FileExists (Cnf. Path + 'system.tor') Then
    WriteSystemStatus (Sys, Cnf. Path + 'system.tor');
  ReadSystemStatus (Sys, Cnf. Path + 'system.tor');

  TextAttr := 7;
  SetBaseName (Cnf. Path + 'users.tor');
  uNum := UsersNum;
  DrawTornadoScreen (Sys. TotalCalls, Sys. UpLoads, Sys. DownLoads, uNum,
    Sys. MsgsPosted);

  AddMsg (NameVer + ', ' + Copyright, True, Cnf. ColorScheme [cmUsers]);

  If BbsLine <> 0 Then
    AddMsg (ShortStrTime + sm (smLineNum) + Long2Str (BbsLine), True,
      Cnf. ColorScheme [cmUsers]);

  Kill_Line_Flag := True;

  If LineUsed Then
  Begin
    If BbsLine <> 0 Then
    Begin
      AddMsg (ShortStrTime + ' ' + sm (smLineUsed), False,
        Cnf. ColorScheme [cmUsers]);
      LogWrite ('!', sm (smLineUsed));
    End Else
    Begin
      AddMsg (ShortStrTime + ' ' + sm (smAlreadyRun), False,
        Cnf. ColorScheme [cmUsers]);
      LogWrite ('!', sm (smAlreadyRun));
    End;

    Pause (2000);
    Kill_Line_Flag := False;
    NormExit;
  End;

  MakeFlag (RunFlag);

  EraseFlag (FileFlag);
  KeyBoardFunc := KeyBoard;
  AnalyzeProc := AnalyzeKey;
  KeyBuffer := '';
  EMSI. Allowed := Cnf. IEMSI_Enable;
  EMSI. Session := False;
  RegLet := 0;

  If ReadLastCaller (LC, trlcOpen4Stat) Then
  Begin
    While ReadLastCaller (LC, trlcRead) Do
      If LC. Name <> '' Then
        AddUsr (LC. Name, LC. LoginTime, LC. TimeOnLine, LC. DLkb, LC. ULkb);

    ReadLastCaller (LC, trlcClose);
  End;

  If BbsLine <> 0 Then
    LogWrite (':', sm (smlMultiLine) + Long2Str (BbsLine));
  CheckFiles;

  If (Cnf. BaudRate = 0) Or (Cnf. ComPort = {$IFNDEF OS2} 0 {$ELSE} '' {$ENDIF}) Then
  Begin
    AutoMode := False;
    Local := True;
  End;

  If Cnf. Clock Then
    FastWrite (StrTime, 1, ScrX-6, Cnf. ColorScheme [cmClock]);

  If Local Then
  Begin
    AddMsg (ShortStrTime + ' ' + sm (smLocalMode), False,
      Cnf. ColorScheme [cmUsers]);
    LogWrite (':', sm (smLocalMode));
    DoBBS;
  End Else
  Begin
    DBits := Cnf. DataBits;
    SBits := Cnf. StopBits;

    Case Cnf. Parity [1] Of
      'N': Parity := NoParity;
      'O': Parity := OddParity;
      'E': Parity := EvenParity;
      'S': Parity := SpaceParity;
      'M': Parity := MarkParity;
    End;

  {$IFDEF MSDOS}
  {$IFNDEF DPMI32}
    Case Cnf. Layer Of
      Fossil : Begin
                 SLayer := 'FOSSIL driver';
                 ActivateApFossil;
               End;
      Uart   : Begin
                 SLayer := 'UART chip';
                 ActivateApUart;
                 If Cnf. PortAddress = 0 Then NewBase := DefBaseAddr [ComNames [Cnf. ComPort]]
                                         Else NewBase := Cnf. PortAddress;
                 If Cnf. IRQ = 0 Then NewIrq := DefIrqNumber [ComNames [Cnf. ComPort]]
                                 Else NewIrq := Cnf. IRQ;
                 If Cnf. ComVector = 0 Then NewVector := DefComVector [ComNames [Cnf. ComPort]]
                                       Else NewVector := Cnf. ComVector;
                 SetUart (ComNames [Cnf. ComPort], NewBase, NewIrq, NewVector);
               End;
      Int14  : Begin
                 SLayer := 'PC BIOS Interrupt 14h';
                 ActivateApInt14;
               End;
      Digi14 : Begin
                 SLayer := 'DigiBoard';
                 ActivateApDigi14;
               End;
    End;
  {$ELSE}
    SLayer := 'FOSSIL driver';
    ActivateApFossil;
  {$ENDIF}
  {$ENDIF}

  {$IFDEF OS2}
    StrPCopy (TempChar, Cnf. ComPort);
    ApOS2. ComPortName := @TempChar;
    ActivateApOS2;
    sLayer := 'OS/2 FOSSIL driver';
  {$ENDIF}

  {$IFDEF WIN32}
    sLayer := 'Win32 com driver';
    ActivateApW32;
  {$ENDIF}

    AddMsg (sm (smLayer) + SLayer, True, Cnf. ColorScheme [cmUsers]);
    InitPort (Port, {$IFNDEF OS2} ComNames [Cnf. ComPort] {$ELSE} Com1 {$ENDIF},
      Cnf. BaudRate, Parity, DBits, SBits, 4096, 4096, DefPortOptions);

    If AsyncStatus <> ecOk Then
    Begin
      AddMsg (ShortStrTime + ' ' + Status2Str (AsyncStatus mod 10000), False,
        Cnf. ColorScheme [cmUsers]);
      LogWrite ('!', sm (smlInitError) + ': ' + Status2Str (AsyncStatus mod
        10000));

      Pause (1500);
      ExitProc := SavedExitProc;
      DoneTornadoScreen;
      TextAttr := 12;
      GotoXY (WXG, WYG);
      WriteLn ('! ' + sm (smErrorExit) + '..');
      TextAttr := 7;
      WriteLn;
      If Cnf. Sound Then
        SoundOf ('1 200 1 2 800 1000 -1 2 800');
      NormalCursor;
      LogClose;
      EraseFlag (RunFlag);
      Halt (206);
    End;

    InitError := False;
    TimeSlice;

  {$IFNDEF OS2}
    If Cnf. HWFlow Then
      HWFlowEnable (Port, 2000, 200, hfUseRTS + hfRequireCTS);
    If Cnf. SWFlow Then
      SWFlowEnable (Port, 2000, 200);
  {$ENDIF}

    SetModem (Port, True, True);

    If AutoMode Then
    Begin
      SetConnectSpeed (reqBaud);

      LogWrite ('~', sm (smLoadBBS) + ' ' +
      {$IFNDEF OS2}
        'Com' + Long2Str (Cnf. ComPort)
      {$ELSE}
        Cnf. ComPort
      {$ENDIF}
        + ', Baud: ' + Long2Str (GetConnectSpeed) + '...');

      If Not CheckDCD (Port) Then
      Begin
        AddMsg (ShortStrTime + ' ' + sm (smNoCD), False,
          Cnf. ColorScheme [cmUsers]);
        LogWrite ('!', sm (smNoCD));
        Pause (1500);
        If Cnf. Sound Then
          SoundOf ('1 200 1 2 800 1000 -1 2 800');
        NormExit;
      End;

      If mCommand <> '' Then
      Begin
        PutString (Port, mCommand + #13);
        {$IFDEF MSDOS} tmDelay {$ELSE} Pause {$ENDIF} (1000);
        While CharReady (Port) Do
          GetChar (Port, C);
      End;

      AddMsg (ShortStrTime + ' ' + sm (smLoadBBS), True,
        Cnf. ColorScheme [cmUsers]);
      DoBBS;
    End;

    InitOk := False;
    Answer := False;
    Manual := False;
    ExitRequest := False;
    Rings := 0;

    For InitTry := 1 To Cnf. InitTryes Do
    Begin
      AddMsg (ShortStrTime + sm (smInitModem) + ', ' + Long2Str (Cnf. BaudRate)
        + ' bps..', False, Cnf. ColorScheme [cmUsers]);

      DoCommand (Cnf. InitString);

      ModemResult := GetModemResponse (rmInit, 10, InitWaiter);
      AddResponse (LastResponse, Cnf. ColorScheme [cmUsers]);

      If ModemResult = mrOk Then
      Begin
        InitOk := True;
        Break;
      End
      Else
        If ModemResult = mrRing Then
          Goto RingRecv;

      AddMsg ('', True, 0);
    End;

    If Not InitOk Then
    Begin
      AddMsg (ShortStrTime + ' ' + sm (smlInitError), False,
        Cnf. ColorScheme [cmUsers]);
      LogWrite ('!', sm (smlInitError));
      Pause (1500);
      ErrorExit;
    End;

    AddMsg ('', True, 0);
  End;

  AddMsg (ShortStrTime + ' ' + sm (smCallWait) + '..', False,
    Cnf. ColorScheme [cmUsers]);
  LogWrite (':', sm (smCallWait));
  SetTitle ('waiting for call');

  Repeat
    ModemResult := GetModemResponse (rmRingWait, 60, CallWaiter);
    AddResponse (LastResponse, Cnf. ColorScheme [cmUsers]);

    Case ModemResult Of
         mrRing : Begin

                  RingRecv:
                    If Cnf. Sound Then
                      SoundOf ('4 3500 0 50 1 3000 0 50 1 4000 0 5 1');
                    iFace. oX := 2;
                    AddMsg (ShortStrTime + sm (smRingNo) + Long2Str (Rings) +
                      '..', False, Cnf. ColorScheme [cmUsers]);

                    If (Rings > 0) And TimerExpired (Timer) Then
                      Rings := 0;
                    Inc (Rings);
                    If Rings < Cnf. Rings Then
                      NewTimerSecs (Timer, 7)
                    Else
                      Answer := True;
                  End;
      mrConnect : Launch;
    Else
      If ExitRequest Then
        Break;
    End;

    If Answer Then
    Begin
      Answer := False;

      If Manual Or MatchTimeArray (Cnf. Answer) Then
      Begin
        Manual := False;
        iFace. oX := 2;
        AddMsg (ShortStrTime + sm (smCarrierWait), False,
          Cnf. ColorScheme [cmUsers]);

        DoCommand (Cnf. AnswerString);

        InitTimer (8 + Length (sm (smCarrierWait)), ScrY - 12, Cnf. CDTime,
          Cnf. ColorScheme [cmFrame]);
        ModemResult := GetModemResponse (rmConnectWait, Cnf. CDTime,
          ConnectWaiter);
        AddResponse (LastResponse, Cnf. ColorScheme [cmUsers]);
        DoneTimer;

        If ModemResult = mrConnect Then
          Launch
        Else
        Begin
          AddMsg ('No carrier', True, Cnf. ColorScheme [cmUsers]);
          AddMsg (ShortStrTime + ' ' + sm (smCallWait) + '..', False,
            Cnf. ColorScheme [cmUsers]);
          LogWrite (':', 'no carrier');
          DoCommand ('^~~v|~~^');
        End;
      End Else
      Begin
        If Length (sm (smCallIgnored) + sm (smRingNo)) > 50 Then
        Begin
          AddMsg ('', True, $00);
          AddMsg (ShortStrTime, False, Cnf. ColorScheme [cmUsers]);
        End;

        AddMsg (sm (smCallIgnored), True, Cnf. ColorScheme [cmUsers]);
        AddMsg (ShortStrTime + ' ' + sm (smCallWait) + '..', False,
          Cnf. ColorScheme [cmUsers]);
      End;
    End;
  Until False;

  iFace. oX := 2;
  AddMsg (ShortStrTime + ' ' + Pad (Sm (smEnding), 45), False,
    Cnf. ColorScheme [cmUsers]);

  Pause (400);
  ExitProc := @NormExit;
  Halt (10);
End.
