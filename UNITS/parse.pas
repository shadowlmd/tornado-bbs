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

Unit Parse;

{*********************************************************}
{*                      PARSE.PAS                        *}
{*                                                       *}
{*  Copyright (c) Konstantin Klyagin, 1996-98,           *}
{*                exspecially for Tornado BBS System     *}
{*                                                       *}
{*********************************************************}

Interface

Uses
  SysMsgs,
  DOS,
  TGlob,
  Parser,
  tMisc;

Function ReadLanguage (Var L: PBigCollection; Const FileName: String): Boolean;
Function ReadDoorWay (Const FileName: String; Var R: DoorWayControlRec): Boolean;
Function ReadLimit (Var Limit: LimitRec; L: System. Word): Boolean;

Function opUpgrades (Const FileName: String): Boolean;
Function rUpgrade (CallNum: LongInt; Var UR: UpgradeRec): Boolean;
Procedure pUpgradeDone;

Function opMsgAreas (Const FileName: String): Boolean;
Function rMsgArea (Var MsgArea: tMsgArea): Boolean;
Procedure pMsgDone;

Function opFileAreas (Const FileName: String): Boolean;
Function rFileArea (Var FileArea: tFileArea): Boolean;
Procedure pFileDone;

Function opFileGroups (Const FileName: String): Boolean;
Function rFileGroup (Var FileGroup: tFileGroup): Boolean;
Procedure pFileGroupDone;

Function opMsgGroups (Const FileName: String): Boolean;
Function rMsgGroup (Var MsgGroup: tMsgGroup): Boolean;
Procedure pMsgGroupDone;

Function MtoAbs (Group, Area: Word): Word;

Function InList (Const FileName, Name: String): Boolean;

Implementation

Uses
  skCommon,
  Log,
  BinCfg,
  Objects,
  RCache;

Type
  PLimitRec = ^LimitRec;
  PLimitCollection = ^TLimitCollection;
  TLimitCollection = Object (TSortedCollection)
    Function Compare (Key1, Key2: Pointer): Integer; Virtual;
    Procedure FreeItem (Item: Pointer); Virtual;
  End;

  PLangTokensTable = ^TLangTokensTable;
  TLangTokensTable = Object (THashTable)
    Procedure Insert (Const Token: String; Number: Word);
  End;

Const
  msa : PConfigParser = Nil;
  fla : PConfigParser = Nil;
  fgr : PConfigParser = Nil;
  mgr : PConfigParser = Nil;
  upg : PConfigParser = Nil;

  LangTokensBits = {$IFDEF RealMode} 7 {$ELSE} 8 {$ENDIF};

Function NewPLimitRec (Const S: LimitRec): PLimitRec;
Var
  P : PLimitRec;

Begin
  New (P);
  P^ := S;
  NewPLimitRec := P;
End;

Function TLimitCollection. Compare (Key1, Key2: Pointer): Integer;
Begin
  If PLimitRec (Key1)^. Level < PLimitRec (Key2)^. Level Then
    Compare := -1
  Else
    If PLimitRec (Key1)^. Level > PLimitRec (Key2)^. Level Then
      Compare := 1
    Else
      Compare := 0;
End;

Procedure TLimitCollection. FreeItem (Item: Pointer);
Begin
  If Item <> Nil Then
    Dispose (PLimitRec (Item));
End;

Procedure TLangTokensTable. Insert (Const Token: String; Number: Word);
Begin
  Inherited Insert (Token, Pointer (LongInt (Number + 1)));
End;

Function ReadLanguage (Var L: PBigCollection; Const FileName: String): Boolean;
Var
  X : PXLATrec;

  Procedure DisposeXLATs;
  Begin
    While XLATs^. Count > 0 Do
    Begin
      FreeMem (XLATs^. At (0), SizeOf (tXLATrec));
      XLATs^. AtDelete (0);
    End;
  End;

  Function RawReadLanguage: Boolean;
  Type
    PLangArray = ^TLangArray;
    tLangArray = Array [0..laLastLanguageNum] Of PString;

  Var
    LT         : LongInt;
    PS         : PString;
    L1         : PLangArray;
    LangTokens : PLangTokensTable;
    RS         : PMemoryStream;
    i          : Integer;
    F1         : File;
    P          : tConfigParser;
    S, S1, S2  : String;

  Begin
    If Not ParserOpen (P, FileName, tpoWriteLog) Then
    Begin
      LogWrite ('!', sm (smFile) + NiceFileName (FileName, 30) +
        sm (smNotFound));
      RawReadLanguage := False;
      Exit;
    End;

    L^. FreeAll;
    DisposeXLATs;

    LangTokens := New (PLangTokensTable, Init (LangTokensBits));
    With LangTokens^ Do
    Begin
      Insert ('ENTER_FOR_CONTINUE', laEnterForCont);
      Insert ('INACTIVE', laInactive);
      Insert ('MORE', laMore);
      Insert ('SELECTFILE', laSelectFile);
      Insert ('SECURITYLOW', laSecurityLow);
      Insert ('TOPIC_FOR_CHAT', laTopicForChat);
      Insert ('PAGING_FOR_CHAT', laPagingForChat);
      Insert ('SYSOP_ABSENT', laNoSysOpForChat);
      Insert ('MSGNUM', laMsgNum);
      Insert ('SELECTPROTOCOL', laSelectProtocol);
      Insert ('PROTOCOLSTR', laProtocolStr);
      Insert ('SYSOP_BUSY', laSysOpBusy);
      Insert ('SYSOP_ANSWERED', laSysOpAnswered);
      Insert ('ACCESS_DENIED', laAccessDenied);
      Insert ('ORGANIZATION', laOrganization);
      Insert ('CHOOSE_PASSWORD', laChoosePassword);
      Insert ('EMPTY_FAREA', laEmptyFArea);
      Insert ('REPEAT_PASSWORD', laRepeatPassword);
      Insert ('PASSWORDS_NOT_SAME', laPasswordsNotSame);
      Insert ('BIRTHDATE', laBirthDate);
      Insert ('FAREALINE', laFAreaLine);
      Insert ('ALIAS_EXIST', laAliasExist);
      Insert ('TIME_EXEEDED', laTimeLimit);
      Insert ('MSGHEAD', laMsgHead);
      Insert ('FIDO_ADDRESS', laFidoAddress);
      Insert ('PAUSING', laPausing);
      Insert ('HOME_PHONE', laHomePhone);
      Insert ('DATA_PHONE', laDataPhone);
      Insert ('DISPLAY_LINES', laDispLines);
      Insert ('MSGFOOTER', laMsgFooter);
      Insert ('LOCATION', laLocation);
      Insert ('PASSWORD_INCORRECT', laPasswordNotValid);
      Insert ('YOUR_CHOICE', laYourChoice);
      Insert ('PASSWORD', laPassword);
      Insert ('LOGOFF', laLogOff);
      Insert ('QUIT', laQuit);
      Insert ('HANGUPINACTIVE', laHangUpInactive);
      Insert ('PAGE_LIMIT', laPageLimit);
      Insert ('LIST_HEADER', laListHeader);
      Insert ('ENTER_MSG_LINE', laEnterMsgLine);
      Insert ('POST_MENU', laPostMenu);
      Insert ('NOMOREMESSAGES', laNoMoreMessages);
      Insert ('ERRORMBCLOSE', laErrorMBClose);
      Insert ('MSGFROM', laMsgFrom);
      Insert ('MSGTO', laMsgTo);
      Insert ('MSGSUBJ', laMsgSubj);
      Insert ('NEXT_MSG', laNextMsg);
      Insert ('REPLY_NEXT_MSG', laReplyNextMsg);
      Insert ('ERRORSAVEMSG', laErrorSaveMsg);
      Insert ('MSGSAVED', laMsgSaved);
      Insert ('SELECTMAREA', laChooseMsgArea);
      Insert ('MSGSTRING', laMsgString);
      Insert ('MSGOPENERROR', laMsgOpenError);
      Insert ('NOMSGSINAREA', laNoMsgsInArea);
      Insert ('YES', laYes);
      Insert ('NO', laNo);
      Insert ('INFOLOC', laInfoLoc);
      Insert ('INFONAME', laInfoName);
      Insert ('INFOORG', laInfoOrg);
      Insert ('INFOADDRESS1', laInfoAddress1);
      Insert ('INFOADDRESS2', laInfoAddress2);
      Insert ('INFOADDRESS3', laInfoAddress3);
      Insert ('INFOHPHONE', laInfoHPhone);
      Insert ('INFOBPHONE', laInfoBPhone);
      Insert ('INFOFIRSTCALL', laInfoFirstCall);
      Insert ('INFOLASTCALL', laInfoLastCall);
      Insert ('INFOBIRTHDATE', laInfoBirthDate);
      Insert ('INFOSECURITY', laInfoSecurity);
      Insert ('INFOCALLNUM', laInfoCallNum);
      Insert ('WANTTOREG', laWantToReg);
      Insert ('YOURNAMENOTFOUND', laYourNameNotFound);
      Insert ('SCANMSGAREAS', laScanMsgAreas);
      Insert ('SCANFILEAREAS', laScanFileAreas);
      Insert ('SEND_FILE', laSendFile);
      Insert ('RECV_FILE', laRecvFile);
      Insert ('LINE_STATUS', laLineStatus);
      Insert ('FILE_NAME', laFileName);
      Insert ('NOT_FOUND', laNotFound);
      Insert ('MSG_WRITE_TEXT1', laMsgWriteText1);
      Insert ('TOO_SLOW', laTooSlow);
      Insert ('MSG_WRITE_TEXT2', laMsgWriteText2);
      Insert ('MSG_WRITE_TEXT3', laMsgWriteText3);
      Insert ('MSG_WRITE_FOOTER', laWriteMsgFooter);
      Insert ('MSG_NO_PREVIOUS', laNoPrevMsg);
      Insert ('ALIAS', laAlias);
      Insert ('YES_NO_QUIT', laYesNoQuit);
      Insert ('NO_YES_QUIT', laNoYesQuit);
      Insert ('PRIVATE_MSG', laFoundPrivMsg);
      Insert ('PRIVATE_WANT_READ', laWantToRead);
      Insert ('MSG_UNKNOWN_FORMAT', laUnknownMsgFormat);
      Insert ('PRIVATE_WANT_ANSWER', laWantToAnswer);
      Insert ('MSG_DELETE', laDeleteMsg);
      Insert ('USER_NAME', laUserName);
      Insert ('MULTI_MSGS_ENABLE', laMsgsEnabled);
      Insert ('MULTI_MSGS_DISABLE', laMsgsDisabled);
      Insert ('YES_NO', laYesNo);
      Insert ('NO_YES', laNoYes);
      Insert ('MULTI_LINE_DISABLE', laLineDisabled);
      Insert ('MULTI_SEND_MSG', laSendMessage);
      Insert ('NAME_ENTERED', laNameEntered);
      Insert ('BEGIN_CHAT', laBeginChat);
      Insert ('END_CHAT', laEndChat);
      Insert ('ADDRESS1', laAddress1);
      Insert ('ADDRESS2', laAddress2);
      Insert ('ADDRESS3', laAddress3);
      Insert ('USER', laUser);
      Insert ('USER_NOT_FOUND', laUserNotFound);
      Insert ('MORE_NUMS', laMoreNums);
      Insert ('UL_DESCRIPTION', laULDescription);
      Insert ('NO_FILES_SELECTED', laNoFilesSelected);
      Insert ('DEL_FILE_NO', laDelFileNo);
      Insert ('FORBIDDEN_PASSWORD', laForbiddenPassword);
      Insert ('ADD_FILE', laAddFile);
      Insert ('ADD_FILE_NOT_FOUND', laAddFileNotFound);
      Insert ('DL_LIMIT_EXCEED', laDLLimitExceed);
      Insert ('CLEAR_LIST', laClearList);
      Insert ('CHOOSE_FILEAREA', laChooseFileArea);
      Insert ('USE_FRAMES', laUseFrames);
      Insert ('SEARCH_PRIV_MAIL', laSearchPrivateMsg);
      Insert ('FIND_NEW_FILES', laFindNewFiles);
      Insert ('IS_REG_CORRECT', laIsRegCorrect);
      Insert ('FILES_MORE', laFilesMore);
      Insert ('TAG_FILES_NUM', laTagFilesNum);
      Insert ('ARCNAME', laArcName);
      Insert ('ARC_HEAD', laArcHead);
      Insert ('QUERY_ARCNAME', laQueryArcName);
      Insert ('NEW_SINCE_DATE', laNewSinceDate);
      Insert ('PASSWORD_TOO_SHORT', laPassTooShort);
      Insert ('MISSING', laMissing);
      Insert ('USERS_HEAD', laUsersHead);
      Insert ('NO_FILES_FOUND', laNoFilesFound);
      Insert ('DL_TIME_LIMIT', laDLTimeLimit);
      Insert ('TODAYS_HEAD', laTodaysHead);
      Insert ('PRIVATE', laPrivate);
      Insert ('OUT_PAGE_TIME', laOutPageTime);
      Insert ('QUOTE_MSG', laQuoteMsg);
      Insert ('OUT_OF_TRYES', laOutOfTryes);
      Insert ('EVENT_TIME_LEFT', laToEventLeft);
      Insert ('EVENT_EXIT', laEventExit);
      Insert ('USE_HOT_KEYS', laUseHotKeys);
      Insert ('PAUSE_AFTER_EACH', laPauseAfterEach);
      Insert ('CHOOSE_EMULATION', laChooseEmulation);
      Insert ('UPLOAD_PLUS', laUpLoadPlus);
      Insert ('LOCAL_DOWNLOAD', laLocalDownLoad);
      Insert ('LOCAL_UPLOAD', laLocalUpLoad);
      Insert ('LOCAL_UL_FILE_NAME', laLocalULFileName);
      Insert ('LOCAL_DL_DIR', laLocalDLDir);
      Insert ('LD_DIR_NOT_FOUND', laLdDirNotFound);
      Insert ('LU_FILE_NOT_FOUND', laLuFileNotFound);
      Insert ('COPY_FILE', laCopyFile);
      Insert ('COPY_TO', laCopyTo);
      Insert ('COPY_OK', laCopyOk);
      Insert ('COPY_ERROR', laCopyError);
      Insert ('COPY_DONE', laCopyDone);
      Insert ('UPLOAD_SPACE', laUpLoadSpace);
      Insert ('ABORTED', laAborted);
      Insert ('FREE_DL', laFreeDL);
      Insert ('ASK_FILE_NAME', laAskFileName);
      Insert ('DOWNLOAD_DESCS', laDownLoadDescs);
      Insert ('NO_PRIV_FOUND', laNoPrivFound);
      Insert ('MSG_MASK_GET', laMsgMaskGet);
      Insert ('MSG_MASK_SEARCH', laMsgMaskSearch);
      Insert ('MSG_MASK_NOTFOUND', laMsgMaskNotFound);
      Insert ('PREV_TAG_AREA', laPrevTagArea);
      Insert ('NEXT_BULLETIN', laNextBulletin);
      Insert ('SEARCH_MASK', laSearchMask);
      Insert ('SEARCH_DESC', laSearchDesc);
      Insert ('ML_MESSAGE_FROM', lamlMessageFrom);
      Insert ('ML_LINE', lamlLine);
      Insert ('LINE_NUMBER', laLineNumber);
      Insert ('UPLOAD_MSG', laUpLoadMsg);
      Insert ('LOGIN_NALLOW', laLoginNAllow);
      Insert ('DOWNLOAD_HEAD1', laDownLoadHead1);
      Insert ('DOWNLOAD_HEAD2', laDownLoadHead2);
      Insert ('DOWNLOAD_HEAD3', laDownLoadHead3);
      Insert ('DOWNLOAD_HEAD4', laDownLoadHead4);
      Insert ('PRIV_MSG', laPrivMsg);
      Insert ('QUERY_PRIVATE', laQueryPrivate);
      Insert ('ENTER_MSG_NUMS', laEnterMsgNums);
      Insert ('REG_RESUME', laRegResume);
      Insert ('ON_ANOTHER_LINE', laOnAnotherLine);
      Insert ('CHOOSE_FILEGROUP', laChooseFileGroup);
      Insert ('NOT_ARCHIVE', laNotArchive);
      Insert ('CHOOSE_MSGGROUP', laChooseMsgGroup);
      Insert ('FROMUSERS_AREA', laFromUsersArea);
      Insert ('FILE_FROM_USER', laFileFromUser);
      Insert ('UL_FOR_USER', laULforuser);
      Insert ('MSG_KEYS', laMsgKeys);
      Insert ('FAREA_KEYS', laFAreaKeys);
      Insert ('POST_KEYS', laPostKeys);
      Insert ('YESNO_KEYS', laYesNoKeys);
      Insert ('YESNOQUIT_KEYS', laYNQKeys);
      Insert ('LOGOFF_MESSAGE', laLogoffMessage);
      Insert ('USE_FS_EDITOR', laUseFSeditor);
      Insert ('FS_ANSI_REQUIRED', laFsAnsiReq);
      Insert ('UL_ALREADY_EXISTS', laAlreadyExists);
      Insert ('ARE_YOU_SURE', laSure);
      Insert ('MSG_FROM_SYSOP', laMsgFromSysOp);
      Insert ('CANNOT_CONNECT', laCannotConnect);
      Insert ('TELNET_TRYING', laTelnTrying);
      Insert ('ENTER_USER_NUMS', laEnterUsrNums);
      Insert ('QWK_SCAN', laQWKscan);
      Insert ('QWK_AJUST', laQWKajust);
      Insert ('QWK_NOT_FOUND', laQWKnotFound);
      Insert ('REP_OK', laREPok);
      Insert ('XLATS', laXLATs);
      Insert ('CHOOSE_XLAT', laChooseXLAT);
      Insert ('QWK_LIST', laQWKlist);
      Insert ('QWK_ENTERNUM', laQWKEnterNum);
      Insert ('QWK_CONTROLS', laQWKcontrols);
      Insert ('NO_QWK_AREAS', laNoQWKareas);
      Insert ('QWK_ADDED', laQWKadded);
      Insert ('QWK_DELETED', laQWKdeleted);
      Insert ('TELNET_CONNECTED', laTelnConnected);
      Insert ('TELNET_CONNECTION', laTelnConnection);
      Insert ('TELNET_CLOSED', laTelnClosed);
      Insert ('UNABLE_TO_RESOLVE', laUnableToResolve);
      Insert ('DEL_LINES', laDelLines);
      Insert ('FSEDIT_STATUSLINE', laEditStatusLine);
      Insert ('TRC_LINE', laTRCline);
      Insert ('TRC_JOINED', laTRCjoined);
      Insert ('TRC_LEFT', laTRCleft);
      Insert ('TRC_ENTERED', laTRCentered);
      Insert ('TRC_LEFT_BBS', laTRCleftBBS);
    End;

    New (L1);
    FillChar (L1^, SizeOf (L1^), 0);

    S1 := ParserRead (P, S2);

    While Not ParserEnd (P) Do
      If S2 = 'MAIN' Then
        Repeat
          ParserGetParam (P, tptString, '', S);
          If S1 = 'LANGUAGE' Then ChangePString (L1^ [laName], S) Else
          If S1 = 'RAW_XLAT_NAME' Then ChangePString (L1^ [laRawXLAT], S) Else
          If S1 = 'ASK_XLAT' Then ChangePString (L1^ [laAskXLAT], UpString (S));
          S1 := ParserRead (P, S2);
        Until ParserEndSection (P)
      Else
      If S2 = 'FILES' Then
        Repeat
          ParserGetParam (P, tptFilePath, '', S);
          If S1 = 'TXTFILES' Then ChangePString (L1^ [laTxtFiles], S) Else
          If S1 = 'MENUS' Then ChangePString (L1^ [laMenus], S) Else
          If S1 = 'NEWS' Then ChangePString (L1^ [laNews], S);
          S1 := ParserRead (P, S2);
        Until ParserEndSection (P)
      Else
      If S2 = 'LANGUAGE_SECTION' Then
        Repeat
          LT := LongInt (LangTokens^. Search (S1));
          If LT > 0 Then
          Begin
            ParserGetParam (P, tptQuote, '', S);
            ChangePString (L1^ [LT - 1], S);
          End;
          S1 := ParserRead (P, S2);
        Until ParserEndSection (P)
      Else
      If S2 = 'XLAT' Then
        Repeat
          If S1 <> '' Then
          Begin
            Assign (F1, S1);
            Reset (F1, 1);

            If IOResult = 0 Then
            Begin
              GetMem (X, SizeOf (tXLATrec));
              BlockRead (F1, X^. Table, SizeOf (X^. Table));
              Close (F1);
              If IOResult <> 0 Then;
              ParserGetParam (P, tptString, '', S);
              X^. Name := XlatStr (S, X^. Table);
              X^. FileName := S1;
              XLATs^. Insert (X);
            End;
          End;
          S1 := ParserRead (P, S2);
        Until ParserEndSection (P)
      Else
        S1 := ParserRead (P, S2);

    Dispose (LangTokens, Done);
    ParserClose (P);

    RS := New (PMemoryStream, Init (0, 0));
    LT := laLastLanguageNum;
    RS^. Write (LT, SizeOf (LT));
    S := '';

    For i := 0 To laLastLanguageNum Do
    Begin
      PS := L1^ [i];
      If PS <> Nil Then
      Begin
        L^. InsLine (PS^);
        RS^. Write (PS^, Length (PS^) + 1);
        FreeMem (PS, Length (PS^) + 1);
      End Else
      Begin
        L^. InsLine (S);
        RS^. Write (S, Length (S) + 1);
      End;
    End;

    Dispose (L1);

    LT := XLATs^. Count;
    RS^. Write (LT, SizeOf (LT));
    For i := 0 To XLATs^. Count-1 Do
      RS^. Write (XLATs^. At (i)^, SizeOf (tXLATrec));

    If RS^. Status = stOk Then
      PutStreamResource (FileName, RS);
    Dispose (RS, Done);

    RawReadLanguage := True;
  End;

Var
  Block  : Pointer;
  P      : ^Byte;
  i, Len : LongInt;

Begin
  Block := GetBlockResource (FileName, Len);

  If Block <> Nil Then
  Begin
    L^. FreeAll;
    DisposeXLATs;

    P := Block;
    i := PLongInt (P)^;
    Inc (P, SizeOf (i));

    While i >= 0 Do
    Begin
      L^. InsLine (PString (P)^);
      Inc (P, P^ + 1);
      Dec (i);
    End;

    i := PLongInt (P)^;
    Inc (P, SizeOf (i));

    While i > 0 Do
    Begin
      GetMem (X, SizeOf (X^));
      Move (P^, X^, SizeOf (X^));
      Inc (P, SizeOf (X^));
      XLATs^. Insert (X);
      Dec (i);
    End;

    FreeMem (Block, Len);
    ReadLanguage := True;
  End
  Else
    ReadLanguage := RawReadLanguage;
End;

Function opMsgAreas (Const FileName: String): Boolean;
Begin
  If msa = Nil Then
  Begin
    New (msa);

    If Not ParserOpen (msa^, FileName, tpoWriteLog) Then
    Begin
      Dispose (msa);
      msa := Nil;
      opMsgAreas := False;
      Exit;
    End;
  End;

  opMsgAreas := True;
End;

Function rMsgArea (Var MsgArea: tMsgArea): Boolean;
Var
  S1, S2 : String;

Begin
  FillChar (MsgArea, SizeOf (MsgArea), 0);

  If ParserEnd (msa^) Then
    rMsgArea := False
  Else
    With MsgArea Do
    Begin
      S1 := ParserRead (msa^, S2);

      While Not ParserEnd (msa^) Do
      Begin
        If S1 = 'NAME' Then
        Begin
          ParserGetParam (msa^, tptQuote, '', S1);
          Name := S1;
        End Else
        If S1 = 'ORIGIN' Then
        Begin
          ParserGetParam (msa^, tptQuote, '', S1);
          Origin := S1;
        End Else
        If S1 = 'BASETYPE' Then ParserGetParam (msa^, tptFixedList, 'JAM FIDO SQUISH', BaseType) Else
        If S1 = 'BASEPATH' Then
        Begin
          ParserGetParam (msa^, tptFilePath, '', S1);
          BasePath := S1;
        End Else
        If S1 = 'USEADDRESS' Then
        Begin
          ParserGetParam (msa^, tptString, '', S1);
          ParseStrAddr (S1, Address);
        End Else
        If S1 = 'READ_SECURITY' Then
        Begin
          ParserGetParam (msa^, tptString, '', S1);
          Str2Security (S1, ReadSec, S1);
          ReadFlags := S1;
        End Else
        If S1 = 'WRITE_SECURITY' Then
        Begin
          ParserGetParam (msa^, tptString, '', S1);
          Str2Security (S1, WriteSec, S1);
          WriteFlags := S1;
        End Else
        If S1 = 'SHOW_SECURITY' Then
        Begin
          ParserGetParam (msa^, tptString, '', S1);
          Str2Security (S1, ShowSec, S1);
          ShowFlags := S1;
        End Else
        If S1 = 'SYSOP_SECURITY' Then
        Begin
          ParserGetParam (msa^, tptString, '', S1);
          Str2Security (S1, SysOpSec, S1);
          SysOpFlags := S1;
        End Else
        If S1 = 'TYPE' Then ParserGetParam (msa^, tptFixedList, 'NETMAIL ECHOMAIL LOCAL', AreaType) Else
        If S1 = 'GROUP' Then
        Begin
          ParserGetParam (msa^, tptUpString, '', S1);
          Group := S1;
        End Else
        If S1 = 'PRIVATE' Then ParserGetParam (msa^, tptExtBoolean, '', Private) Else
        If S1 = 'SCAN_PRIVMAIL' Then ParserGetParam (msa^, tptBoolean, '', ScanPrivMail) Else
        If S1 = 'GATEWAY' Then
        Begin
          ParserGetParam (msa^, tptString, '', S1);
          GateWay := S1;
        End;

        S1 := ParserRead (msa^, S2);
        If msa^. SectionChanged Then
        Begin
          ParserGetBack (msa^);
          Break;
        End;
      End;

      If BaseType = mbfMSG Then
        BasePath := AddBackSlash (BasePath)
      Else
        BasePath := StripTrailBackSlashes (BasePath);

      rMsgArea := True;
    End;
End;

Procedure pMsgDone;
Begin
  If msa <> Nil Then
  Begin
    ParserClose (msa^);
    Dispose (msa);
    msa := Nil;
  End;
End;

Function opFileAreas (Const FileName: String): Boolean;
Begin
  If fla = Nil Then
  Begin
    New (fla);

    If Not ParserOpen (fla^, FileName, tpoWriteLog) Then
    Begin
      Dispose (fla);
      fla := Nil;
      opFileAreas := False;
      Exit;
    End;
  End;

  opFileAreas := True;
End;

Function rFileArea (Var FileArea: tFileArea): Boolean;
Var
  S1, S2 : String;

Begin
  FillChar (FileArea, SizeOf (FileArea), 0);
  FileArea. FileList := 'files.bbs';

  If ParserEnd (fla^) Then
    rFileArea := False
  Else
  Begin
    S1 := ParserRead (fla^, S2);

    With FileArea Do
    Begin
      While Not ParserEnd (fla^) Do
      Begin
        If S1 = 'NAME' Then
        Begin
          ParserGetParam (fla^, tptQuote, '', S1);
          Name := S1;
        End Else
        If S1 = 'DLPATH' Then
        Begin
          ParserGetParam (fla^, tptFilePath, '', S1);
          S1 := AddBackSlash (S1);
          If DLPath = '' Then DLPath := S1
                         Else DLPath := DLPath + #255 + S1;
        End Else
        If S1 = 'ULPATH' Then
        Begin
          ParserGetParam (fla^, tptFilePath, '', S1);
          ULPath := AddBackSlash (S1);
        End Else
        If S1 = 'FILELIST' Then
        Begin
          ParserGetParam (fla^, tptFilePath, '', S1);
          FileList := S1;
        End Else
        If S1 = 'DL_SECURITY' Then
        Begin
          ParserGetParam (fla^, tptString, '', S1);
          Str2Security (S1, DL_Security, S1);
          DL_Flags := S1;
        End Else
        If S1 = 'UL_SECURITY' Then
        Begin
          ParserGetParam (fla^, tptString, '', S1);
          Str2Security (S1, UL_Security, S1);
          UL_Flags := S1;
        End Else
        If S1 = 'LIST_SECURITY' Then
        Begin
          ParserGetParam (fla^, tptString, '', S1);
          Str2Security (S1, List_Security, S1);
          List_Flags := S1;
        End Else
        If S1 = 'SHOW_SECURITY' Then
        Begin
          ParserGetParam (fla^, tptString, '', S1);
          Str2Security (S1, ShowSec, S1);
          ShowFlags := S1;
        End Else
        If S1 = 'GROUP' Then
        Begin
          ParserGetParam (fla^, tptUpString, '', S1);
          Group := S1;
        End Else
        If S1 = 'SCAN_NEWFILES' Then ParserGetParam (fla^, tptBoolean, '', ScanNewFiles) Else
        If S1 = 'MIN_SPEED' Then ParserGetParam (fla^, tptWord, '', MinSpeed) Else
        If S1 = 'COPY_LOCAL' Then ParserGetParam (fla^, tptBoolean, '', CopyLocal) Else
        If S1 = 'FLIST_FORMAT' Then ParserGetParam (fla^, tptFixedList, 'FILESBBS CD-LIST', FListFormat);

        S1 := ParserRead (fla^, S2);
        If fla^. SectionChanged Then
        Begin
          ParserGetBack (fla^);
          Break;
        End;
      End;

      If FListFormat = fCDList Then
        DLPath := ExtractWord (1, DLPath, [#255]);
      If JustPathname (FileList) = '' Then
        FileList := ExtractWord (1, DLPath, [#255]) + FileList;
      If ULPath = '' Then
        ULPath := ExtractWord (1, DLPath, [#255]);
    End;

    rFileArea := True;
  End;
End;

Procedure pFileDone;
Begin
  If fla <> Nil Then
  Begin
    ParserClose (fla^);
    Dispose (fla);
    fla := Nil;
  End;
End;

Function opMsgGroups (Const FileName: String): Boolean;
Begin
  If mgr = Nil Then
  Begin
    New (mgr);

    If Not ParserOpen (mgr^, FileName, tpoWriteLog) Then
    Begin
      Dispose (mgr);
      mgr := Nil;
      opMsgGroups := False;
      Exit;
    End;
  End;

  opMsgGroups := True;
End;

Function rMsgGroup (Var MsgGroup: tMsgGroup): Boolean;
Var
  S1, S2 : String;

Begin
  FillChar (MsgGroup, SizeOf (MsgGroup), 0);

  If ParserEnd (mgr^) Then
    rMsgGroup := False
  Else
  Begin
    S1 := ParserRead (mgr^, S2);

    While Not ParserEnd (mgr^) Do
    Begin
      With MsgGroup Do
        If S1 = 'NAME' Then
        Begin
          ParserGetParam (mgr^, tptQuote, '', S1);
          Name := S1;
        End Else
        If S1 = 'GROUP_TAG' Then
        Begin
          ParserGetParam (mgr^, tptUpString, '', S1);
          Tag := DelSpaces (S1);
        End Else
        If S1 = 'SHOW_SECURITY' Then
        Begin
          ParserGetParam (mgr^, tptString, '', S1);
          Str2Security (S1, ShowSec, S1);
          ShowFlags := S1;
        End;

      S1 := ParserRead (mgr^, S2);
      If mgr^. SectionChanged Then
      Begin
        ParserGetBack (mgr^);
        Break;
      End;
    End;

    rMsgGroup := True;
  End;
End;

Procedure pMsgGroupDone;
Begin
  If Mgr <> Nil Then
  Begin
    ParserClose (mgr^);
    Dispose (mgr);
    mgr := Nil;
  End;
End;

Function opFileGroups (Const FileName: String): Boolean;
Begin
  If fgr = Nil Then
  Begin
    New (fgr);

    If Not ParserOpen (fgr^, FileName, tpoWriteLog) Then
    Begin
      Dispose (fgr);
      fgr := Nil;
      opFileGroups := False;
      Exit;
    End;
  End;

  opFileGroups := True;
End;

Function rFileGroup (Var FileGroup: tFileGroup): Boolean;
Var
  S1, S2 : String;

Begin
  FillChar (FileGroup, SizeOf (FileGroup), 0);

  If ParserEnd (fgr^) Then
    rFileGroup := False
  Else
  Begin
    S1 := ParserRead (fgr^, S2);

    While Not ParserEnd (fgr^) Do
    Begin
      With FileGroup Do
        If S1 = 'NAME' Then
        Begin
          ParserGetParam (fgr^, tptQuote, '', S1);
          Name := S1;
        End Else
        If S1 = 'GROUP_TAG' Then
        Begin
          ParserGetParam (fgr^, tptUpString, '', S1);
          Tag := DelSpaces (S1);
        End Else
        If S1 = 'SHOW_SECURITY' Then
        Begin
          ParserGetParam (fgr^, tptString, '', S1);
          Str2Security (S1, ShowSec, S1);
          ShowFlags := S1;
        End;

      S1 := ParserRead (fgr^, S2);
      If fgr^. SectionChanged Then
      Begin
        ParserGetBack (fgr^);
        Break;
      End;
    End;

    rFileGroup := True;
  End;
End;

Procedure pFileGroupDone;
Begin
  If fgr <> Nil Then
  Begin
    ParserClose (fgr^);
    Dispose (fgr);
    fgr := Nil;
  End;
End;

Function opUpgrades (Const FileName: String): Boolean;
Begin
  If upg = Nil Then
  Begin
    New (upg);

    If Not ParserOpen (upg^, FileName, tpoWriteLog) Then
    Begin
      Dispose (upg);
      upg := Nil;
      opUpgrades := False;
      Exit;
    End;
  End;

  opUpgrades := True;
End;

Function rUpgrade (CallNum: LongInt; Var UR: UpgradeRec): Boolean;
Var
  UR1            : UpgradeRec;
  Reason, S1, S2 : String;

  Function CheckBirthDay: Boolean;
  Begin
    CheckBirthDay := ((Reason = 'BIRTHDAY') And (CallNum = -1)) And
                     ((R. LastDate <> DateL) Or (R. NoCalls = 1));
  End;

  Function CheckDate: Boolean;
  Var
    Date, Mask : String [12];

  Begin
    CheckDate := False;

    If Trim (ExtractWord (1, Reason, [' ', '('])) = 'DATE' Then
    Begin
      Date := FormattedCurrDT (Cnf. DateMask);
      Mask := Trim (ExtractWord (2, Reason, BracketsOnly));

      If DateMatch (Date, Mask) Then
        If (R. LastDate <> DateL) Or (R. NoCalls = 1) Then
          CheckDate := True;
    End;
  End;

  Function CheckCall: Boolean;
  Begin
    CheckCall := False;

    If Trim (ExtractWord (1, Reason, [' ', '('])) = 'CALL' Then
      If Str2Long (ExtractWord (2, Reason, BracketsOnly)) = CallNum Then
        CheckCall := True;
  End;

Begin
  rUpgrade := False;
  If ParserEnd (upg^) Then
    Exit;

  FillChar (UR1, SizeOf (UR1), 0);
  Reason := '';
  S1 := ParserRead (upg^, S2);
  S2 := '';

  While Not ParserEnd (upg^) Do
  Begin
    If S2 = '' Then
    Begin
      With UR1 Do
        If S1 = 'REASON' Then ParserGetParam (upg^, tptUpString, '', Reason) Else
        If S1 = 'FILENAME' Then
        Begin
          ParserGetParam (upg^, tptFilePath, '', S1);
          FileName := S1;
        End Else
        If S1 = 'CHECK' Then
        Begin
          ParserGetParam (upg^, tptString, '', S1);
          Check := S1;
        End Else
        If S1 = 'SECURITY' Then
        Begin
          ParserGetParam (upg^, tptString, '', S1);
          Security := S1;
        End Else
        If S1 = 'MODE' Then ParserGetParam (upg^, tptFixedList, 'FOREVER TODAY', Mode) Else
        If S1 = 'INFORM_VIA' Then
        Begin
          ParserGetParam (upg^, tptUpString, '', S1);
          InformVia := S1;
        End;

      S1 := ParserRead (upg^, S2);
    End
    Else
      If CheckBirthDay Or CheckDate Or CheckCall Then
      Begin
        UR := UR1;
        rUpgrade := True;
        Exit;
      End Else
      Begin
        S2 := '';
        Reason := '';
      End;
  End;

  If CheckBirthDay Or CheckDate Or CheckCall Then
  Begin
    UR := UR1;
    rUpgrade := True;
  End;
End;

Procedure pUpgradeDone;
Begin
  If upg <> Nil Then
  Begin
    ParserClose (upg^);
    Dispose (upg);
    upg := Nil;
  End;
End;

Function MtoAbs (Group, Area: Word): Word;
Var
  OldGroup, OldArea, LastGroup, LastArea : LongInt;

Begin
  OldGroup := R. MsgGroup;
  OldArea := R. MsgArea;
  LastGroup := OldGroup;
  LastArea := OldArea;
  SmartChangeMArea (Group, Area, LastGroup, LastArea);
  MtoAbs := PhysMsgArea;
  SmartChangeMArea (OldGroup, OldArea, LastGroup, LastArea);
End;

Function ReadDoorWay (Const FileName: String; Var R: DoorWayControlRec): Boolean;
Var
  P          : Pointer;
  Len        : LongInt;
  dwc        : tConfigParser;
  M          : tDoorWayCommands;
  C          : Char;
  S1, S2, S3 : String;

Begin
  ReadDoorWay := True;

  P := GetBlockResource (FileName, Len);
  If P <> Nil Then
  Begin
    Move (P^, R, Len);
    FreeMem (P, Len);
    Exit;
  End;

  If Not ParserOpen (dwc, FileName, tpoWriteLog) Then
    ReadDoorWay := False
  Else
    With R Do
    Begin
      FillChar (R, SizeOf (R), 0);
      For C := 'A' To 'Z' Do
        Drives [C] := 65535;
      For M := hcCls To hcKill Do
        SecLevel [M] := 65535;

      S1 := ParserRead (dwc, S2);

      While Not ParserEnd (dwc) Do
        If S2 = 'DRIVES' Then
          Repeat
            If Copy (S1, 1, 6) = 'DRIVE_'
            Then
              ParserGetParam (dwc, tptWord, '', Drives [S1 [7]])
            Else
            If S1 = 'START_DIR' Then
            Begin
              ParserGetParam (dwc, tptFilePath, '', S1);
              StartDir := JustPathname (AddBackSlash (S1));
            End;

            S1 := ParserRead (dwc, S2);
          Until ParserEndSection (dwc)
        Else
        If S2 = 'RESTRICTIONS' Then
          Repeat
            ParserGetParam (dwc, tptString, '', S3);

            If S1 = 'ENTER_SECURITY' Then
            Begin
              Str2Security (S3, Enter, S3);
              flgEnter := S3;
            End Else
            If S1 = 'CHDIR_SECURITY' Then
            Begin
              Str2Security (S3, SecLevel [hcCD], S3);
              SecFlags [hcCD] := S3;
            End Else
            If S1 = 'TREE_SECURITY' Then
            Begin
              Str2Security (S3, SecLevel [hcTree], S3);
              SecFlags [hcTree] := S3;
            End Else
            If S1 = 'MKDIR_SECURITY' Then
            Begin
              Str2Security (S3, SecLevel [hcMkDir], S3);
              SecFlags [hcMkDir] := S3;
            End Else
            If S1 = 'RMDIR_SECURITY' Then
            Begin
              Str2Security (S3, SecLevel [hcRmDir], S3);
              SecFlags [hcRmDir] := S3;
            End Else
            If S1 = 'DIRSIZE_SECURITY' Then
            Begin
              Str2Security (S3, SecLevel [hcDirSize], S3);
              SecFlags [hcDirSize] := S3;
            End Else
            If S1 = 'DIR_SECURITY' Then
            Begin
              Str2Security (S3, SecLevel [hcDir], S3);
              SecFlags [hcDir] := S3;
            End Else
            If S1 = 'DEL_SECURITY' Then
            Begin
              Str2Security (S3, SecLevel [hcDel], S3);
              SecFlags [hcDel] := S3;
            End Else
            If S1 = 'REN_SECURITY' Then
            Begin
              Str2Security (S3, SecLevel [hcRename], S3);
              SecFlags [hcRename] := S3;
            End Else
            If S1 = 'COPY_SECURITY' Then
            Begin
              Str2Security (S3, SecLevel [hcCopy], S3);
              SecFlags [hcCopy] := S3;
            End Else
            If S1 = 'BOOT_SECURITY' Then
            Begin
              Str2Security (S3, SecLevel [hcBoot], S3);
              SecFlags [hcBoot] := S3;
            End Else
            If S1 = 'SHELL_SECURITY' Then
            Begin
              Str2Security (S3, SecLevel [hcShell], S3);
              SecFlags [hcShell] := S3;
            End Else
            If S1 = 'XDEL_SECURITY' Then
            Begin
              Str2Security (S3, SecLevel [hcxDel], S3);
              SecFlags [hcxDel] := S3;
            End Else
            If S1 = 'UL_SECURITY' Then
            Begin
              Str2Security (S3, SecLevel [hcUL], S3);
              SecFlags [hcUL] := S3;
            End Else
            If S1 = 'DL_SECURITY' Then
            Begin
              Str2Security (S3, SecLevel [hcDL], S3);
              SecFlags [hcDL] := S3;
            End Else
            If S1 = 'DATE_SECURITY' Then
            Begin
              Str2Security (S3, SecLevel [hcDate], S3);
              SecFlags [hcDate] := S3;
            End Else
            If S1 = 'TIME_SECURITY' Then
            Begin
              Str2Security (S3, SecLevel [hcTime], S3);
              SecFlags [hcTime] := S3;
            End Else
            If S1 = 'ARCVIEW_SECURITY' Then
            Begin
              Str2Security (S3, SecLevel [hcArcView], S3);
              SecFlags [hcArcView] := S3;
            End Else
            If S1 = 'TYPE_SECURITY' Then
            Begin
              Str2Security (S3, SecLevel [hcType], S3);
              SecFlags [hcType] := S3;
            End Else
            If S1 = 'LOCATE_SECURITY' Then
            Begin
              Str2Security (S3, SecLevel [hcLocate], S3);
              SecFlags [hcLocate] := S3;
            End Else
            If S1 = 'TASKLIST_SECURITY' Then
            Begin
              Str2Security (S3, SecLevel [hcTaskList], S3);
              SecFlags [hcTaskList] := S3;
            End Else
            If S1 = 'KILL_SECURITY' Then
            Begin
              Str2Security (S3, SecLevel [hcKill], S3);
              SecFlags [hcKill] := S3;
            End Else
            If S1 = 'EDIT_SECURITY' Then
            Begin
              Str2Security (S3, SecLevel [hcEdit], S3);
              SecFlags [hcEdit] := S3;
            End;

            S1 := ParserRead (dwc, S2);
          Until ParserEndSection (dwc)
        Else
          S1 := ParserRead (dwc, S2);

      ParserClose (dwc);

      PutBlockResource (FileName, @R, SizeOf (R));
    End;
End;

Function ReadLimit (Var Limit: LimitRec; L: System. Word): Boolean;

  Function RawReadLimit: Boolean;
  Var
    LimColl : PLimitCollection;
    RS      : PMemoryStream;
    PL      : PLimitRec;
    i       : Integer;
    j       : LongInt;
    P       : tConfigParser;
    S1, S2  : String;

  Begin
    RawReadLimit := False;

    If Not ParserOpen (P, Cnf. LimitsFile, tpoWriteLog) Then
      Exit;

    RS := New (PMemoryStream, Init (0, 0));
    LimColl := New (PLimitCollection, Init (16, 8));
    S1 := ParserRead (P, S2);

    While Not ParserEnd (P) Do
    Begin
      FillChar (Limit, SizeOf (Limit), 0);

      Repeat
        If S1 = 'LEVEL' Then ParserGetParam (P, tptWord, '', Limit. Level) Else
        If S1 = 'TIMELIMIT' Then ParserGetParam (P, tptLongInt, '', Limit. Time) Else
        If S1 = 'KBLIMIT' Then ParserGetParam (P, tptLongInt, '', Limit. KbLimit) Else
        If S1 = 'MIN_SPEED' Then ParserGetParam (P, tptLongInt, '', Limit. MinSpeed) Else
        If S1 = 'MAX_SESSION' Then ParserGetParam (P, tptWord, '', Limit. SessionTime) Else
        If S1 = 'LOGIN_TIME' Then
        Begin
          ParserGetParam (P, tptTime, '', S1);
          Str2TimeArray (S1, Limit. LoginTime);
        End;

        S1 := ParserRead (P, S2);
      Until ParserEndSection (P);

      If Limit. Level > 0 Then
        LimColl^. Insert (NewPLimitRec (Limit));
    End;

    ParserClose (P);

    i := LimColl^. Count;
    j := i;
    RS^. Write (j, SizeOf (j));

    If i > 0 Then
    Begin
      Repeat
        Dec (i);
        PL := LimColl^. At (i);
      Until (PL^. Level <= L) Or (i = 0);

      Limit := PL^;

      For i := 0 To j - 1 Do
        RS^. Write (PLimitRec (LimColl^. At (i))^, SizeOf (LimitRec));

      RawReadLimit := True;
    End
    Else
      FillChar (Limit, SizeOf (Limit), 0);

    Dispose (LimColl, Done);
    If RS^. Status = stOk Then
      PutStreamResource (Cnf. LimitsFile, RS);
    Dispose (RS, Done);
  End;

Var
  Block  : Pointer;
  P      : ^Byte;
  PL     : PLimitRec;
  i, Len : LongInt;

Begin
  ReadLimit := False;

  Block := GetBlockResource (Cnf. LimitsFile, Len);
  If Block <> Nil Then
  Begin
    P := Block;
    i := PLongInt (P)^;

    If i = 0 Then
      FillChar (Limit, SizeOf (Limit), 0)
    Else
    Begin
      Inc (P, SizeOf (i));

      Repeat
        PL := Pointer (P);
        Inc (P, SizeOf (LimitRec));
        Dec (i);
      Until (i = 0) Or (PLimitRec (P)^. Level > L);

      Limit := PL^;
    End;

    FreeMem (Block, Len);
    ReadLimit := True;
  End
  Else
    ReadLimit := RawReadLimit;

  Limit. Level := L;
End;

Function InList (Const FileName, Name: String): Boolean;
Var
  AName : String;

  Function InRawList: Boolean;
  Var
    RS      : PMemoryStream;
    i       : LongInt;
    P       : Integer;
    FileBuf : FileBufArr;
    F       : Text;
    S       : String;
    AMask   : Array [0..256] Of Byte;
  {$IFNDEF VirtualPascal}
    Result  : Boolean;
  {$ENDIF}

  Begin
    Result := False;

    Assign (F, FileName);
    SetTextBuf (F, FileBuf, FileBufSize);
    ReSet (F);

    If IOResult = 0 Then
    Begin
      RS := New (PMemoryStream, Init (0, 0));
      i := 0;
      RS^. Write (i, SizeOf (i));

      While Not EOF (F) Do
      Begin
        ReadLn (F, S);
        P := Pos (';', S);
        If P > 0 Then
          SetLength (S, P - 1);
        S := Trim (S);

        If S <> '' Then
        Begin
          P := Length (S) + 1;
          Move (S, AMask, P);
          AMask [P] := 0;
          If Not Result And MaskMatch (AName, AMask [1]) Then
            Result := True;

          RS^. Write (AMask, P + 1);
          Inc (i);
        End;
      End;

      If i > 0 Then
      Begin
        RS^. Seek (0);
        RS^. Write (i, SizeOf (i));
      End;

      If RS^. Status = stOk Then
        PutStreamResource (FileName, RS);
      Dispose (RS, Done);
      Close (F);
    End;

  {$IFNDEF VirtualPascal}
    InRawList := Result;
  {$ENDIF}
  End;

Var
  Block  : Pointer;
  P      : ^Byte;
  i, Len : LongInt;

Begin
  InList := False;
  If Trim (FileName) = '' Then
    Exit;

  Move (Name [1], AName, Length (Name));
  AName [Length (Name)] := #0;

  Block := GetBlockResource (FileName, Len);
  If Block <> Nil Then
  Begin
    P := Block;
    i := PLongInt (P)^;
    Inc (P, SizeOf (i));

    While i > 0 Do
    Begin
      If MaskMatch (AName, PString (P)^ [1]) Then
      Begin
        InList := True;
        Break;
      End;

      Inc (P, P^ + 2);
      Dec (i);
    End;

    FreeMem (Block, Len);
  End
  Else
    InList := InRawList;
End;

End.
