{$F+,I-}

Unit MainCTL;

Interface

Uses
  DOS,
  OpCrt,
  tGlob,
  tMisc,
  SysMsgs,
  Parser;

{$IFDEF MSDOS}
  {$IFNDEF DPMI}
  {$IFNDEF DPMI32}
    {$DEFINE RealMode}
  {$ENDIF}
  {$ENDIF}
{$ENDIF}

Procedure ReadMainConfig (Const FileName: PathStr; Var Cnf: ConfigRecord);

Implementation

Procedure ReadMainConfig (Const FileName: PathStr; Var Cnf: ConfigRecord);
Var
  bCount, i, Bt : Byte;
  cfg           : tConfigParser;
  sWord         : String [70];
  S1, S2        : String;

{$IFDEF OS2}
  PortNo        : Byte;
  Result        : LongInt;
{$ENDIF}

Begin
  If Not ParserOpen (cfg, FileName, tpoWriteScreen) Then
  Begin
    TextAttr := $0C;
    WriteLn ('! ', sm (smFile) + FileName + sm (smNotFound));
    TextAttr := $07;
    WriteLn;
    ExitProc := SavedExitProc;
    Halt (201);
  End;

  FillChar (Cnf, SizeOf (Cnf), 0);

  Repeat
    S1 := ParserRead (cfg, S2);
    If ParserEnd (cfg) Then
      Break;

    If S2 = 'SYSTEM' Then
    Begin
      If S1 = 'SYSOP' Then
      Begin
        ParserGetParam (cfg, tptString, '', S1);
        Cnf. SysOp := S1;
      End Else
      If S1 = 'BBSNAME' Then
      Begin
        ParserGetParam (cfg, tptString, '', S1);
        Cnf. BBSname := S1;
      End Else
      If S1 = 'LOCATION' Then
      Begin
        ParserGetParam (cfg, tptString, '', S1);
        Cnf. Location := S1;
      End Else
      If S1 = 'PRIVATE' Then ParserGetParam (cfg, tptBoolean, '', Cnf. Private) Else
      If S1 = 'DATE_MASK' Then
      Begin
        ParserGetParam (cfg, tptString, '', S1);
        Cnf. DateMask := S1;
      End Else
      If S1 = 'OVERLAY_XMS' Then {$IFDEF RealMode} ParserGetParam (cfg, tptBoolean, '', Cnf. Overlay_XMS) {$ENDIF} Else
      If S1 = 'OVERLAY_EMS' Then {$IFDEF RealMode} ParserGetParam (cfg, tptBoolean, '', Cnf. Overlay_EMS) {$ENDIF} Else
      If S1 = 'ANSWER' Then
      Begin
        ParserGetParam (cfg, tptTime, '', S1);
        Str2TimeArray (S1, Cnf. Answer);
      End Else
      If S1 = 'PATH' Then
      Begin
        ParserGetParam (cfg, tptFilePath, '', S1);
        Cnf. Path := CompletePath (S1);
        SmartChDir (Cnf. Path);
        If IOResult <> 0 Then;
      End
      Else
        ParserUnknown (cfg);
    End Else
    If S2 = 'FILES&DIRS' Then
    Begin
      If S1 = 'LOGO' Then
      Begin
        ParserGetParam (cfg, tptFilePath, '', S1);
        Cnf. Logo := S1;
      End Else
      If S1 = 'LOGFILE' Then
      Begin
        ParserGetParam (cfg, tptFilePath, '', S1);
        Cnf. LogFile := DefaultName (S1, 'log', Cnf. Path);
      End Else
      If S1 = 'CHAT_LOG' Then
      Begin
        ParserGetParam (cfg, tptFilePath, '', S1);
        Cnf. ChatLog := DefaultName (S1, 'log', Cnf. Path);
      End Else
      If S1 = 'TRC_LOG' Then
      Begin
        ParserGetParam (cfg, tptFilePath, '', S1);
        Cnf. TRCLog := DefaultName (S1, 'log', Cnf. Path);
      End Else
      If S1 = 'TEMPDIR' Then
      Begin
        ParserGetParam (cfg, tptFilePath, '', S1);
        Cnf. TempDir := CompletePath (S1);
      End Else
      If S1 = 'FLAGSDIR' Then
      Begin
        ParserGetParam (cfg, tptFilePath, '', S1);
        Cnf. FlagsDir := CompletePath (S1);
      End Else
      If S1 = 'DOORINFODIR' Then
      Begin
        ParserGetParam (cfg, tptFilePath, '', S1);
        Cnf. DoorInfoDir := CompletePath (S1);
      End Else
      If S1 = 'PRIV_UPLOADS_DIR' Then
      Begin
        ParserGetParam (cfg, tptFilePath, '', S1);
        Cnf. PrivUploadsDir := CompletePath (S1);
      End Else
      If S1 = 'FILEAREAS_FILE' Then
      Begin
        ParserGetParam (cfg, tptFilePath, '', S1);
        Cnf. FileAreasFile := DefaultName (S1, 'ctl', Cnf. Path);
      End Else
      If S1 = 'MSGAREAS_FILE' Then
      Begin
        ParserGetParam (cfg, tptFilePath, '', S1);
        Cnf. MsgAreasFile := DefaultName (S1, 'ctl', Cnf. Path);
      End Else
      If S1 = 'FILEGROUPS_FILE' Then
      Begin
        ParserGetParam (cfg, tptFilePath, '', S1);
        Cnf. FileGroupsFile := DefaultName (S1, 'ctl', Cnf. Path);
      End Else
      If S1 = 'MSGGROUPS_FILE' Then
      Begin
        ParserGetParam (cfg, tptFilePath, '', S1);
        Cnf. MsgGroupsFile := DefaultName (S1, 'ctl', Cnf. Path);
      End Else
      If S1 = 'LIMITS_FILE' Then
      Begin
        ParserGetParam (cfg, tptFilePath, '', S1);
        Cnf. LimitsFile := DefaultName (S1, 'ctl', Cnf. Path);
      End Else
      If S1 = 'LNG_PATH' Then
      Begin
        ParserGetParam (cfg, tptFilePath, '', S1);
        Cnf. LngPath := CompletePath (S1);
      End Else
      If S1 = 'DEFAULT_LANGFILE' Then
      Begin
        ParserGetParam (cfg, tptFilePath, '', S1);
        Cnf. DefLangFile := DefaultName (S1, 'lng', Cnf. LNGPath);
      End Else
      If S1 = 'VIP_LIST' Then
      Begin
        ParserGetParam (cfg, tptFilePath, '', S1);
        Cnf. VIPlist := DefaultName (S1, 'ctl', Cnf. Path);
      End Else
      If S1 = 'GOODUSERS_LIST' Then
      Begin
        ParserGetParam (cfg, tptFilePath, '', S1);
        Cnf. GoodUsersList := DefaultName (S1, 'ctl', Cnf. Path);
      End Else
      If S1 = 'BADUSERS_LIST' Then
      Begin
        ParserGetParam (cfg, tptFilePath, '', S1);
        Cnf. BadUsersList := DefaultName (S1, 'ctl', Cnf. Path);
      End Else
      If S1 = 'BADPASSWORDS_LIST' Then
      Begin
        ParserGetParam (cfg, tptFilePath, '', S1);
        Cnf. BadPasswordsList := DefaultName (S1, 'ctl', Cnf. Path);
      End Else
      If S1 = 'SUXXUSERS_LIST' Then
      Begin
        ParserGetParam (cfg, tptFilePath, '', S1);
        Cnf. SuxxUsersList := DefaultName (S1, 'ctl', Cnf. Path);
      End Else
      If S1 = 'DOORWAY_CTL' Then
      Begin
        ParserGetParam (cfg, tptFilePath, '', S1);
        Cnf. DoorWayCTL := DefaultName (S1, 'ctl', Cnf. Path);
      End Else
      If S1 = 'UPGRADER_CTL' Then
      Begin
        ParserGetParam (cfg, tptFilePath, '', S1);
        Cnf. UpgraderCTL := DefaultName (S1, 'ctl', Cnf. Path);
      End Else
      If S1 = 'PROTOCOL_CTL' Then
      Begin
        ParserGetParam (cfg, tptFilePath, '', S1);
        Cnf. ProtocolCTL := DefaultName (S1, 'ctl', Cnf. Path);
      End Else
      If S1 = 'SAVE_TAG_PATH' Then
      Begin
        ParserGetParam (cfg, tptFilePath, '', S1);
        Cnf. SaveTagPath := CompletePath (S1);
      End
      Else
        ParserUnknown (cfg);
    End Else
    If S2 = 'MISCELLANEOUS' Then
    Begin
      If S1 = 'SHOWVERSION' Then ParserGetParam (cfg, tptBoolean, '', Cnf. ShowVer) Else
      If S1 = 'CDTIME' Then ParserGetParam (cfg, tptLongInt, '', Cnf. CDTime) Else
      If S1 = 'PASSTRYES' Then ParserGetParam (cfg, tptByte, '', Cnf. PassTryes) Else
      If S1 = 'INACTIVE_TIME' Then ParserGetParam (cfg, tptWord, '', Cnf. InactiveTime) Else
      If S1 = 'CRINMENU' Then ParserGetParam (cfg, tptBoolean, '', Cnf. CRinMenu) Else
      If S1 = 'MAINMENU' Then
      Begin
        ParserGetParam (cfg, tptString, '', S1);
        Cnf. MainMenu := S1;
      End Else
      If S1 = 'LOGEVENTS' Then
      Begin
        ParserGetParam (cfg, tptString, '', S1);
        Cnf. LogEvents := S1;
      End Else
      If S1 = 'EXTERNAL_CHAT' Then
      Begin
        ParserGetParam (cfg, tptString, '', S1);
        Cnf. ExternalChat := S1;
      End Else
      If S1 = 'ASKCHATTOPIC' Then ParserGetParam (cfg, tptBoolean, '', Cnf. AskChatTopic) Else
      If S1 = 'EFFECTS' Then ParserGetParam (cfg, tptBoolean, '', Cnf. Effects) Else
      If S1 = 'CHAT_TIME_SUSPEND' Then ParserGetParam (cfg, tptBoolean, '', Cnf. ChatTimeSuspend) Else
      If S1 = 'SAVE_TAG_LIST' Then ParserGetParam (cfg, tptBoolean, '', Cnf. SaveTagList) Else
      If S1 = 'DL_ON_START' Then ParserGetParam (cfg, tptBoolean, '', Cnf. DlOnStart) Else
      If S1 = 'CHECK_DL_TIME' Then ParserGetParam (cfg, tptBoolean, '', Cnf. CheckDlTime) Else
      If S1 = 'TXT_MORE_DISABLE' Then ParserGetParam (cfg, tptBoolean, '', Cnf. TxtMoreDisable) Else
      If S1 = 'LOCAL_OFFHOOK' Then ParserGetParam (cfg, tptBoolean, '', Cnf. LocalOffHook) Else
      If S1 = 'HIDE_SYSOP' Then ParserGetParam (cfg, tptBoolean, '', Cnf. HideSysOp) Else
      If S1 = 'NEWS_PAUSE_ASK' Then ParserGetParam (cfg, tptBoolean, '', Cnf. NewsPauseAsk) Else
      If S1 = 'CLOCK' Then ParserGetParam (cfg, tptBoolean, '', Cnf. Clock) Else
      If S1 = 'ONE_COLUMN_MENUS' Then ParserGetParam (cfg, tptBoolean, '', Cnf. OneColMenus) Else
      If S1 = 'SOUND' Then ParserGetParam (cfg, tptBoolean, '', Cnf. Sound) Else
      If S1 = 'STATUS_BAR' Then ParserGetParam (cfg, tptBoolean, '', Cnf. StatusBar) Else
      If S1 = 'LOGOFF_ASK' Then ParserGetParam (cfg, tptBoolean, '', Cnf. LogoffAsk) Else
      If S1 = 'LOGOFF_MAIL' Then ParserGetParam (cfg, tptBoolean, '', Cnf. LogoffMail) Else
      If S1 = 'FILENAMES_DEBUG' Then ParserGetParam (cfg, tptBoolean, '', Cnf. DebugFiles) Else
      If S1 = 'RESREG_EXPIRE' Then ParserGetParam (cfg, tptWord, '', Cnf. DoRegExpire) Else
      If S1 = 'USERINFO_MINSEC' Then
      Begin
        ParserGetParam (cfg, tptString, '', S1);
        Str2Security (S1, Cnf. UserInfoMinSec, S1);
        Cnf. UserInfoMinFlags := S1;
      End Else
      If S1 = 'IGNORE_OLD_FLAGS' Then ParserGetParam (cfg, tptWord, '', Cnf. IgnoreOldFlags) Else
      If S1 = 'CHAT_STYLE' Then
      Begin
        ParserGetParam (cfg, tptFixedList, 'FULLSCREEN USUAL', Bt);
        Cnf. ChatStyle := Bt = 0;
      End Else
      If S1 = 'YESNO_STYLE' Then
      Begin
        ParserGetParam (cfg, tptFixedList, 'MENU STRING', Bt);
        Cnf. YesNoStyle := Bt = 0;
      End Else
      If S1 = 'ABORT_KEY' Then ParserGetParam (cfg, tptFixedList, 'ANY ESC NONE', Cnf. AbortKey) Else
      If S1 = 'PAGE_DURATION' Then ParserGetParam (cfg, tptByte, '', Cnf. PageDuration) Else
      If S1 = 'PAGE_TIMES_LIMIT' Then ParserGetParam (cfg, tptByte, '', Cnf. PageLimit) Else
      If S1 = 'PASSWORD_LENGTH' Then ParserGetParam (cfg, tptByte, '', Cnf. PassLength) Else
      If S1 = 'SHOW_SECURED' Then ParserGetParam (cfg, tptFixedList, 'YES HIDDEN NO', Cnf. ShowSecured) Else
      If S1 = 'PAGE_TIME' Then
      Begin
        ParserGetParam (cfg, tptString, '', S1);
        Str2TimeArray (S1, Cnf. PageTime);
      End Else
      If S1 = 'AUTOANSI' Then ParserGetParam (cfg, tptFixedList, 'YES NO DETECT', Cnf. DetectEmu) Else
      If S1 = 'CHAT_SYSOP_WINDOW' Then
      Begin
        ParserGetParam (cfg, tptString, '', S1);
        Cnf. ChatSysY1 := Str2Long (ExtractWord (1, S1, SpaceAndComma));
        Cnf. ChatSysX1 := Str2Long (ExtractWord (2, S1, SpaceAndComma));
        Cnf. ChatSysY2 := Str2Long (ExtractWord (3, S1, SpaceAndComma));
        Cnf. ChatSysX2 := Str2Long (ExtractWord (4, S1, SpaceAndComma));
      End Else
      If S1 = 'CHAT_USER_WINDOW' Then
      Begin
        ParserGetParam (cfg, tptString, '', S1);
        Cnf. ChatUserY1 := Str2Long (ExtractWord (1, S1, SpaceAndComma));
        Cnf. ChatUserX1 := Str2Long (ExtractWord (2, S1, SpaceAndComma));
        Cnf. ChatUserY2 := Str2Long (ExtractWord (3, S1, SpaceAndComma));
        Cnf. ChatUserX2 := Str2Long (ExtractWord (4, S1, SpaceAndComma));
      End Else
      If S1 = 'EMU_ALLOW' Then
      Begin
        ParserGetParam (cfg, tptUpString, '', S1);
        bCount := WordCount (S1, SpaceAndComma);

        Cnf. eaAnsi := False;
        Cnf. eaAvatar := False;
        Cnf. eaTty := False;

        For i := 1 To bCount Do
        Begin
          sWord := ExtractWord (i, S1, SpaceAndComma);
          If sWord = 'ANSI' Then Cnf. eaAnsi := True Else
          If sWord = 'AVATAR' Then Cnf. eaAvatar := True Else
          If sWord = 'TTY' Then Cnf. eaTTY := True;
        End;
      End Else
      If S1 = 'OPTIMISEUSERBASE' Then ParserGetParam (cfg, tptWord, '', Cnf. OptimiseUserBase)
      Else
        ParserUnknown (cfg);
    End Else
    If S2 = 'NEWUSERS' Then
    Begin
      If S1 = 'IEMSI_ENABLE' Then ParserGetParam (cfg, tptBoolean, '', Cnf. IEMSI_Enable) Else
      If S1 = 'DATA_PHONE' Then ParserGetParam (cfg, tptBoolean, '', Cnf. DataPhone) Else
      If S1 = 'VOICE_PHONE' Then ParserGetParam (cfg, tptBoolean, '', Cnf. VoicePhone) Else
      If S1 = 'ONE_WORD_NAMES' Then ParserGetParam (cfg, tptBoolean, '', Cnf. OneWordNames) Else
      If S1 = 'BIRTHDATE' Then ParserGetParam (cfg, tptBoolean, '', Cnf. BirthDate) Else
      If S1 = 'LOCATION' Then ParserGetParam (cfg, tptBoolean, '', Cnf. AskLocation) Else
      If S1 = 'DISPLAY_LINES_ASK' Then ParserGetParam (cfg, tptBoolean, '', Cnf. DispAsk) Else
      If S1 = 'ADDRESS' Then ParserGetParam (cfg, tptBoolean, '', Cnf. Address) Else
      If S1 = 'ORGANIZATION' Then ParserGetParam (cfg, tptBoolean, '', Cnf. Organization) Else
      If S1 = 'ALIASES' Then ParserGetParam (cfg, tptBoolean, '', Cnf. Aliases) Else
      If S1 = 'ASK_ALIAS' Then ParserGetParam (cfg, tptBoolean, '', Cnf. AskAlias) Else
      If S1 = 'ACCEPT_REGISTRATION' Then ParserGetParam (cfg, tptBoolean, '', Cnf. AcceptReg) Else
      If S1 = 'REG_RESUME' Then ParserGetParam (cfg, tptBoolean, '', Cnf. RegResume) Else
      If S1 = 'CAPITALIZENAMES' Then ParserGetParam (cfg, tptBoolean, '', Cnf. CapitalizeNames) Else
      If S1 = 'FAST_LOGON' Then ParserGetParam (cfg, tptBoolean, '', Cnf. FastLogon) Else
      If S1 = 'SECURITY' Then
      Begin
        ParserGetParam (cfg, tptString, '', S1);
        Str2Security (S1, Cnf. Security, S1);
        Cnf. Flags := S1;
      End Else
      If S1 = 'GOOD_SECURITY' Then
      Begin
        ParserGetParam (cfg, tptString, '', S1);
        Str2Security (S1, Cnf. GoodSecurity, S1);
        Cnf. GoodFlags := S1;
      End Else
      If S1 = 'REGISTER_TIME' Then ParserGetParam (cfg, tptWord, '', Cnf. RegisterTime) Else
      If S1 = 'ANSI' Then ParserGetParam (cfg, tptFixedList, 'ASK YES NO', Cnf. ANSI) Else
      If S1 = 'MORE' Then ParserGetParam (cfg, tptFixedList, 'ASK YES NO', Cnf. More) Else
      If S1 = 'HOT_KEYS' Then ParserGetParam (cfg, tptFixedList, 'ASK YES NO', Cnf. HotKeys) Else
      If S1 = 'FRAMES' Then ParserGetParam (cfg, tptFixedList, 'ASK YES NO', Cnf. Frames) Else
      If S1 = 'SCAN_PRIVMAIL' Then ParserGetParam (cfg, tptFixedList, 'ASK YES NO', Cnf. ScanPrivMail) Else
      If S1 = 'SCAN_NEWFILES' Then ParserGetParam (cfg, tptFixedList, 'ASK YES NO', Cnf. ScanNewFiles) Else
      If S1 = 'FS_EDITOR' Then ParserGetParam (cfg, tptFixedList, 'ASK YES NO', Cnf. FSEditor) Else
      If S1 = 'DISPLAY_LINES' Then ParserGetParam (cfg, tptByte, '', Cnf. DisplayLines) Else
      If S1 = 'SHOW_NEWS' Then
      Begin
        ParserGetParam (cfg, tptString, '', S1);

        Case UpCase (S1 [1]) Of
          'Y' : Cnf. ShowNews := tnYes;
          'L' : Cnf. ShowNews := tnLast;
          'N' : Cnf. ShowNews := tnNo;
        End;
      End
      Else
        ParserUnknown (cfg);
    End Else
    If S2 = 'MODEM' Then
    Begin
      If S1 = 'INITSTRING' Then
      Begin
        ParserGetParam (cfg, tptString, '', S1);
        Cnf. InitString := S1;
      End Else
      If S1 = 'ANSWERSTRING' Then
      Begin
        ParserGetParam (cfg, tptString, '', S1);
        Cnf. AnswerString := S1;
      End Else
      If S1 = 'OFFHOOKSTRING' Then
      Begin
        ParserGetParam (cfg, tptString, '', S1);
        Cnf. OffHookString := S1;
      End Else
      If S1 = 'MODEM_OK' Then
      Begin
        ParserGetParam (cfg, tptString, '', S1);
        Cnf. OkStr := S1;
      End Else
      If S1 = 'MODEM_CONNECT' Then
      Begin
        ParserGetParam (cfg, tptString, '', S1);
        Cnf. ConnectStr := S1;
      End Else
      If S1 = 'MODEM_RING' Then
      Begin
        ParserGetParam (cfg, tptString, '', S1);
        Cnf. RingStr := S1;
      End Else
      If S1 = 'MODEM_NOCARRIER' Then
      Begin
        ParserGetParam (cfg, tptString, '', S1);
        Cnf. NoCarrierStr := S1;
      End Else
      If S1 = 'HANGUPSTRING' Then
      Begin
        ParserGetParam (cfg, tptString, '', S1);
        Cnf. HangUpString := S1;
      End Else
      If S1 = 'PORT' Then
      Begin
      {$IFNDEF OS2}
        ParserGetParam (cfg, tptByte, '', Cnf. ComPort);
      {$ELSE}
        ParserGetParam (cfg, tptString, '', S1);
        Val (S1, PortNo, Result);
        If (Result <> 0) Or Not (PortNo in [1..36]) Then
          Cnf. ComPort := S1
        Else
          Cnf. ComPort := 'COM' + Long2Str (PortNo);
      {$ENDIF}
      End Else
      If S1 = 'INIT_TRYES' Then ParserGetParam (cfg, tptByte, '', Cnf. InitTryes) Else
      If S1 = 'BAUDRATE' Then ParserGetParam (cfg, tptLongInt, '', Cnf. BaudRate) Else
      If S1 = 'RINGS' Then ParserGetParam (cfg, tptByte, '', Cnf. Rings) Else
      If S1 = 'LAYER' Then {$IFDEF MSDOS}
        ParserGetParam (cfg, tptFixedList, 'FOSSIL UART DIGI14 INT14', Cnf. Layer) {$ENDIF} Else
      If S1 = 'PORT_ADDRESS' Then
      Begin
      {$IFDEF MSDOS}
        ParserGetParam (cfg, tptUpString, '', S1);
        Cnf. PortAddress := Hex2Word (S1);
      {$ENDIF}
      End Else
      If S1 = 'COM_VECTOR' Then {$IFDEF MSDOS} ParserGetParam (cfg, tptByte, '', Cnf. ComVector) {$ENDIF} Else
      If S1 = 'IRQ' Then {$IFDEF MSDOS} ParserGetParam (cfg, tptByte, '', Cnf. IRQ) {$ENDIF} Else
      If S1 = 'HARDWARE_FLOW' Then {$IFNDEF OS2} ParserGetParam (cfg, tptBoolean, '', Cnf. HWFlow) {$ENDIF} Else
      If S1 = 'SOFTWARE_FLOW' Then {$IFNDEF OS2} ParserGetParam (cfg, tptBoolean, '', Cnf. SWFlow) {$ENDIF} Else
      If S1 = 'DATABITS' Then {$IFNDEF OS2} ParserGetParam (cfg, tptByte, '5-8', Cnf. DataBits) {$ENDIF} Else
      If S1 = 'STOPBITS' Then {$IFNDEF OS2} ParserGetParam (cfg, tptByte, '1-2', Cnf. StopBits) {$ENDIF} Else
      If S1 = 'PARITY' Then
      Begin
      {$IFNDEF OS2}
        ParserGetParam (cfg, tptUpString, '', S1);
        Cnf. Parity := S1;
      {$ENDIF}
      End Else
      If S1 = 'CARRIER_LOST_DELAY' Then ParserGetParam (cfg, tptByte, '', Cnf. CDLostDelay) Else
      If S1 = 'MIN_SPEED' Then ParserGetParam (cfg, tptLongInt, '', Cnf. MinSpeed) Else
        ParserUnknown (cfg);
    End Else
    If S2 = 'MSGAREAS' Then
    Begin
      If S1 = 'QWK_PACK' Then
      Begin
        ParserGetParam (cfg, tptString, '', S1);
        Cnf. QWKpack := S1;
      End Else
      If S1 = 'QWK_UNPACK' Then
      Begin
        ParserGetParam (cfg, tptString, '', S1);
        Cnf. QWKunpack := S1;
      End Else
      If S1 = 'QWK_ADD' Then
      Begin
        ParserGetParam (cfg, tptString, '', S1);
        Cnf. QWKadd := S1;
      End Else
      If S1 = 'EXTERNAL_EDITOR' Then
      Begin
        ParserGetParam (cfg, tptString, '', S1);
        Cnf. ExtMailEd := S1;
      End Else
      If S1 = 'TO_SYSOP_AREA' Then
      Begin
        ParserGetParam (cfg, tptString, '', S1);
        Cnf. ToSysOpArea := S1;
      End Else
      If S1 = 'MSGQUOTE_PREFIX' Then ParserGetParam (cfg, tptBoolean, '', Cnf. QuotePrefix) Else
      If S1 = 'POST_QUOTE' Then ParserGetParam (cfg, tptFixedList, 'ASK YES NO', Cnf. PostQuote) Else
      If S1 = 'POST_UPLOAD' Then ParserGetParam (cfg, tptFixedList, 'ASK YES NO', Cnf. PostUpload) Else
      If S1 = 'ECHO_LOG' Then
      Begin
        ParserGetParam (cfg, tptFilePath, '', S1);
        Cnf. EchoLog := CompletePath (S1);
      End
      Else
        ParserUnknown (cfg);
    End Else
    If S2 = 'MESSAGES' Then
    Begin
      If S1 = 'LOGINSTRING' Then
      Begin
        ParserGetParam (cfg, tptQuote, '', S1);
        Cnf. LoginString := S1;
      End Else
      If S1 = 'CHOOSELANG1' Then
      Begin
        ParserGetParam (cfg, tptQuote, '', S1);
        Cnf. ChoiceLanguageStr1 := S1;
      End Else
      If S1 = 'CHOOSELANG2' Then
      Begin
        ParserGetParam (cfg, tptQuote, '', S1);
        Cnf. ChoiceLanguageStr2 := S1;
      End Else
      If S1 = 'NO_ONE_WORD_NAMES' Then
      Begin
        ParserGetParam (cfg, tptQuote, '', S1);
        Cnf. NoOneWordNames := S1;
      End
      Else
        ParserUnknown (cfg);
    End Else
    If S2 = 'FILEAREAS' Then
    Begin
      If S1 = 'DLCOUNT' Then ParserGetParam (cfg, tptBoolean, '', Cnf. DLCount) Else
      If S1 = 'CUT_LONGDESCS' Then ParserGetParam (cfg, tptBoolean, '', Cnf. CutLongDescs) Else
      If S1 = 'CUT_DESCS' Then ParserGetParam (cfg, tptBoolean, '', Cnf. CutDescs) Else
      If S1 = 'DESC_UNDER_DLC' Then ParserGetParam (cfg, tptBoolean, '', Cnf. DescUnderDLC) Else
      If S1 = 'UPLOAD_OVERWRITE' Then ParserGetParam (cfg, tptBoolean, '', Cnf. ULOverWrite) Else
      If S1 = 'UPLOAD_AJUSTDATE' Then ParserGetParam (cfg, tptBoolean, '', Cnf. AjustULdate) Else
      If S1 = 'LOGICALSIZE' Then ParserGetParam (cfg, tptBoolean, '', Cnf. LogicalSize) Else
      If S1 = 'DLCOUNTMASK' Then
      Begin
        ParserGetParam (cfg, tptQuote, '', S1);
        Cnf. DLCountMask := S1;
      End Else
      If S1 = 'DLFREE_KEY' Then
      Begin
        ParserGetParam (cfg, tptQuote, '', S1);
        Cnf. DLFreeKey := S1;
      End Else
      If S1 = 'LONG_DESC_CHAR' Then
      Begin
        ParserGetParam (cfg, tptString, '', S1);
        Cnf. LongDescChar := S1 [1];
      End Else
      If S1 = 'LONG_DESC_POS' Then
        ParserGetParam (cfg, tptByte, '', Cnf. LongDescPos)
      Else
      If S1 = 'DLDESCRIPTIONS' Then ParserGetParam (cfg, tptFixedList, 'ASK YES NO', Cnf. DLDescs) Else
      If S1 = 'UPLOAD_TIME_PLUS' Then ParserGetParam (cfg, tptByte, '', Cnf. UploadTimePlus) Else
      If S1 = 'UPLOAD_SPACE' Then ParserGetParam (cfg, tptWord, '', Cnf. UploadSpace) Else
      If S1 = 'ABORT_PROTOCOL' Then
      Begin
        ParserGetParam (cfg, tptUpString, '', S1);
        bCount := WordCount (S1, SpaceAndComma);

        Cnf. AbortUpLoad := False;
        Cnf. AbortDownLoad := False;

        For i := 1 To bCount Do
        Begin
          sWord := ExtractWord (i, S1, SpaceAndComma);
          If sWord = 'DOWNLOAD' Then Cnf. AbortDownLoad := True Else
          If sWord = 'UPLOAD' Then Cnf. AbortUpLoad := True;
        End;
      End Else
      If S1 = 'LIST_OPTIONS' Then
      Begin
        ParserGetParam (cfg, tptUpString, '', S1);
        bCount := WordCount (S1, SpaceAndComma);

        Cnf. FAShowDate := False;
        Cnf. FAShowSize := False;
        Cnf. FAShowMissing := False;
        Cnf. FAShowDLC := False;

        For i := 1 To bCount Do
        Begin
          sWord := ExtractWord (i, S1, SpaceAndComma);
          If sWord = 'DATE' Then Cnf. FAShowDate := True Else
          If sWord = 'SIZE' Then Cnf. FAShowSize := True Else
          If sWord = 'MISSING' Then Cnf. FAShowMissing := True Else
          If sWord = 'DLC' Then Cnf. FAShowDLC := True;
        End;

        Cnf. FAShowDLC := Cnf. DLCount And Cnf. FAShowDLC;
      End
      Else
        ParserUnknown (cfg);
    End Else
    If S2 = 'COLORS' Then
    With Cnf Do
    Begin
      If S1 = 'BLINKING' Then ParserGetParam (cfg, tptBoolean, '', Blinking) Else
      If S1 = 'MS_FRAME' Then ParserGetParam (cfg, tptColor, '', ColorScheme [cmFrame]) Else
      If S1 = 'MS_HEADERS' Then ParserGetParam (cfg, tptColor, '', ColorScheme [cmMsgs]) Else
      If S1 = 'MS_STATUS' Then ParserGetParam (cfg, tptColor, '', ColorScheme [cmStatusLine]) Else
      If S1 = 'MS_STATUSFLAGS' Then ParserGetParam (cfg, tptColor, '', ColorScheme [cmStatusFlags]) Else
      If S1 = 'MS_LAMPS' Then ParserGetParam (cfg, tptColor, '', ColorScheme [cmLamps]) Else
      If S1 = 'MS_HIGHLAMPS' Then ParserGetParam (cfg, tptColor, '', ColorScheme [cmHighlight]) Else
      If S1 = 'MS_STATISTICS' Then ParserGetParam (cfg, tptColor, '', ColorScheme [cmOther]) Else
      If S1 = 'MS_LOG' Then ParserGetParam (cfg, tptColor, '', ColorScheme [cmUsers]) Else
      If S1 = 'MS_CLOCK' Then ParserGetParam (cfg, tptColor, '', ColorScheme [cmClock]) Else
      If S1 = 'PROT_FRAME' Then ParserGetParam (cfg, tptColor, '', ColorScheme [cpFrame]) Else
      If S1 = 'PROT_HEADER' Then ParserGetParam (cfg, tptColor, '', ColorScheme [cpHeader]) Else
      If S1 = 'PROT_TEXT' Then ParserGetParam (cfg, tptColor, '', ColorScheme [cpFieldNames]) Else
      If S1 = 'PROT_NOTICE' Then ParserGetParam (cfg, tptColor, '', ColorScheme [cpHeadText]) Else
      If S1 = 'PROT_VALUES' Then ParserGetParam (cfg, tptColor, '', ColorScheme [cpFieldValues]) Else
      If S1 = 'PROT_SCROLLBAR' Then ParserGetParam (cfg, tptColor, '', ColorScheme [cpScrollBar]) Else
      If S1 = 'PROT_MESSAGES' Then ParserGetParam (cfg, tptColor, '', ColorScheme [cpMessages]) Else
      If S1 = 'DLG_FRAME' Then ParserGetParam (cfg, tptColor, '', ColorScheme [cdFrame]) Else
      If S1 = 'DLG_TITLE' Then ParserGetParam (cfg, tptColor, '', ColorScheme [cdTitle]) Else
      If S1 = 'DLG_INPUT' Then ParserGetParam (cfg, tptColor, '', ColorScheme [cdInput]) Else
      If S1 = 'DLG_SCROLLER' Then ParserGetParam (cfg, tptColor, '', ColorScheme [cdScroller]) Else
      If S1 = 'DLG_BUTTON' Then ParserGetParam (cfg, tptColor, '', ColorScheme [cdButton]) Else
      If S1 = 'DLG_TEXT' Then ParserGetParam (cfg, tptColor, '', ColorScheme [cdText]) Else
      If S1 = 'ALERT_FRAME' Then ParserGetParam (cfg, tptColor, '', ColorScheme [caFrame]) Else
      If S1 = 'ALERT_TEXT' Then ParserGetParam (cfg, tptColor, '', ColorScheme [caText]) Else
      If S1 = 'MENUS_HIDDEN' Then ParserGetParam (cfg, tptColor, '', ColorScheme [mnHidden]) Else
      If S1 = 'LIST_NUMBER' Then ParserGetParam (cfg, tptColor, '', ColorScheme [umNumber]) Else
      If S1 = 'LIST_DOT' Then ParserGetParam (cfg, tptColor, '', ColorScheme [umDot]) Else
      If S1 = 'LIST_ITEM' Then ParserGetParam (cfg, tptColor, '', ColorScheme [umItem]) Else
      If S1 = 'LIST_SEPARATOR' Then ParserGetParam (cfg, tptColor, '', ColorScheme [umSeparator]) Else
      If S1 = 'MSGLIST_NUMBER' Then ParserGetParam (cfg, tptColor, '', ColorScheme [mlNumber]) Else
      If S1 = 'MSGLIST_FROM' Then ParserGetParam (cfg, tptColor, '', ColorScheme [mlFrom]) Else
      If S1 = 'MSGLIST_TO' Then ParserGetParam (cfg, tptColor, '', ColorScheme [mlTo]) Else
      If S1 = 'MSGLIST_LIGHTFROM' Then ParserGetParam (cfg, tptColor, '', ColorScheme [mlLightFrom]) Else
      If S1 = 'MSGLIST_LIGHTTO' Then ParserGetParam (cfg, tptColor, '', ColorScheme [mlLightTo]) Else
      If S1 = 'MSGLIST_SUBJ' Then ParserGetParam (cfg, tptColor, '', ColorScheme [mlSubj]) Else
      If S1 = 'USERLIST_NUMBERS' Then ParserGetParam (cfg, tptColor, '', ColorScheme [ulNumbers]) Else
      If S1 = 'USERLIST_NAME' Then ParserGetParam (cfg, tptColor, '', ColorScheme [ulName]) Else
      If S1 = 'USERLIST_LOCATION' Then ParserGetParam (cfg, tptColor, '', ColorScheme [ulLocation]) Else
      If S1 = 'USERLIST_BIRTHDATE' Then ParserGetParam (cfg, tptColor, '', ColorScheme [ulBirthDate]) Else
      If S1 = 'USERLIST_LASTDATE' Then ParserGetParam (cfg, tptColor, '', ColorScheme [ulLastDate]) Else
      If S1 = 'TODAYS_LINE' Then ParserGetParam (cfg, tptColor, '', ColorScheme [tdLine]) Else
      If S1 = 'TODAYS_NAME' Then ParserGetParam (cfg, tptColor, '', ColorScheme [tdName]) Else
      If S1 = 'TODAYS_ENTERTIME' Then ParserGetParam (cfg, tptColor, '', ColorScheme [tdEnterTime]) Else
      If S1 = 'TODAYS_ONLINE' Then ParserGetParam (cfg, tptColor, '', ColorScheme [tdOnLine]) Else
      If S1 = 'TODAYS_TRANSFERS' Then ParserGetParam (cfg, tptColor, '', ColorScheme [tdTransfers]) Else
      If S1 = 'ARCVIEW_FILENAME' Then ParserGetParam (cfg, tptColor, '', ColorScheme [avFileName]) Else
      If S1 = 'ARCVIEW_DATE' Then ParserGetParam (cfg, tptColor, '', ColorScheme [avDate]) Else
      If S1 = 'ARCVIEW_SIZE' Then ParserGetParam (cfg, tptColor, '', ColorScheme [avSize]) Else
      If S1 = 'ARCVIEW_COMPRESSED' Then ParserGetParam (cfg, tptColor, '', ColorScheme [avCompressed]) Else
      If S1 = 'FLIST_FILENAME' Then ParserGetParam (cfg, tptColor, '', ColorScheme [flFileName]) Else
      If S1 = 'FLIST_TAGNUM' Then ParserGetParam (cfg, tptColor, '', ColorScheme [flTagNum]) Else
      If S1 = 'FLIST_SIZE' Then ParserGetParam (cfg, tptColor, '', ColorScheme [flSize]) Else
      If S1 = 'FLIST_DLC' Then ParserGetParam (cfg, tptColor, '', ColorScheme [flDLC]) Else
      If S1 = 'FLIST_DATE' Then ParserGetParam (cfg, tptColor, '', ColorScheme [flDate]) Else
      If S1 = 'FLIST_DESCRIPTION' Then ParserGetParam (cfg, tptColor, '', ColorScheme [flDesc]) Else
      If S1 = 'FLIST_COMMENTS' Then ParserGetParam (cfg, tptColor, '', ColorScheme [flComments]) Else
      If S1 = 'MREAD_NORMAL' Then ParserGetParam (cfg, tptColor, '', ColorScheme [mrNormal]) Else
      If S1 = 'MREAD_QUOTE' Then ParserGetParam (cfg, tptColor, '', ColorScheme [mrQuote]) Else
      If S1 = 'MREAD_QUOTE2' Then ParserGetParam (cfg, tptColor, '', ColorScheme [mrQuote2]) Else
      If S1 = 'MREAD_ORIGIN' Then ParserGetParam (cfg, tptColor, '', ColorScheme [mrOrigin]) Else
      If S1 = 'FRAMES_COLOR' Then ParserGetParam (cfg, tptColor, '', ColorScheme [scFrames]) Else
      If S1 = 'NEWS_FRAMES' Then ParserGetParam (cfg, tptColor, '', ColorScheme [nwFrames]) Else
      If S1 = 'NEWS_DATE' Then ParserGetParam (cfg, tptColor, '', ColorScheme [nwDate]) Else
      If S1 = 'EDITOR_TEXT' Then ParserGetParam (cfg, tptColor, '', ColorScheme [edText]) Else
      If S1 = 'EDITOR_QUOTES' Then ParserGetParam (cfg, tptColor, '', ColorScheme [edQuote]) Else
      If S1 = 'CHAT_TEXT' Then ParserGetParam (cfg, tptColor, '', ColorScheme [chtText]) Else
        ParserUnknown (cfg);
    End;
  Until False;

  ParserClose (cfg);
End;

End.
