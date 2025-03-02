{$IFDEF MSDOS}
  {$IFNDEF DPMI32}
    {$F+}
  {$IFNDEF DPMI}
    {$O+}
  {$ENDIF}
  {$ENDIF}
{$ENDIF}
{$I-}

Unit Areas;

{*********************************************************}
{*                       AREAS.PAS                       *}
{*                                                       *}
{*  Copyright (c) Konstantin Klyagin, 1995-98,           *}
{*                exspecially for Tornado BBS System     *}
{*                                                       *}
{*********************************************************}

Interface

Uses
{$IFDEF OS2}
  Os2Base,
{$ENDIF}
  ApAbsPcl,
  DOS,
  ApTimer,
  ApCom,
  tWin,
  Objects,
  skCommon,
  skMHL,
  MainComm,
  MainCOvr,
  tMisc,
  Crc,
  SysMsgs,
  BinCfg,
  Parse,
  FilesBBS,
  OpCrt,
  Log,
  Protocol,
  TGlob,
  Users,
  TimeTask,
  tFSed;

Type
  tPostMode        = (pmNew, pmReply, pmEdit);
  tFSearchMode     = (fsName, fsDate, fsDesc);

Const
  pfAutoOpen       = $01;
  pfPrivate        = $02;
  pfUpgrader       = $04;
  pfUseDefaultAddr = $08;

  MaxInitials      = 4;

Var
  Msg        : PMessageBase;
  MsgText    : PBigCollection;

Procedure SelectFGroup;
Procedure SelectFArea;
Procedure SelectMGroup;
Procedure SelectMArea;

Function  OpenMessageArea (ErrorReport, Make: Boolean): Boolean;

Function  PostMsg (PostMode: tPostMode; ToUser, Subj: String; Const AReply, eMail: String;
          ReplyToNumRelative: LongInt; Const Tpl: String): Boolean;
Procedure PrePostMsg (Const Param, Tpl: String);
Procedure Msg2SysOp (Const Subj, Tpl: String);

Procedure PostFile (PostMode: tPostMode; Const FileName: PathStr; AbsoluteNum: Word;
          Const FromName, ToName, mSubj, AReply, eMail: String; OrigAddr, DestAddr: TAddress;
          ReplyToNumRelative: LongInt; Options: Byte);
Procedure TypeFile (Const FileName: PathStr);

Procedure ReadMsgs;
Procedure ListMsgs;
Procedure SearchPrivate;
Procedure SearchMessages (Const Keywords: String);

Procedure GlobalSearch (Const WildCard: String; Mode: tFSearchMode;
          SinceLast: AskType; Const dFrom: String);

Implementation

Uses
{$IFDEF MSDOS}
{$IFNDEF DPMI32}
  NTVDMSvc,
{$ENDIF}
{$ENDIF}
{$IFDEF WIN32}
  Windows,
{$ENDIF}
  Strings,
  skOpen;

Const
  UserNameLen      = 45;
  AddrLen          = 23;

  ml_MaxNumbers    = 99;
  ml_NumLen        = 2;
  ml_FromLen       = 24;
  ml_ToLen         = 24;
  ml_SubjLen       = 25;

  EditorBottomLine = 23;

Var
  FinishReading : Boolean;
  LR            : Longint;
  H             : PMsgHead;
  LineBuf       : PChar;

{$IFDEF WIN32}
{$I INC\win.inc}
{$ENDIF}

Procedure SelectFGroup;
Var
  M                   : PNotSortedCollection;
  LastGroup, LastArea : LongInt;
  i                   : Integer;
  tFG                 : tFileGroup;

Begin
  If OpenFileGroups Then
  Begin
    New (M, Init (16, 8));
    StartFGroupReadSequence (0);

    While ReadNextFileGroup (tFG) Do
      If (tFG. Name <> '') And (tFG. ShowSec <= R. Security) And
         FlagsValid (R. Flags, tFG. ShowFlags)
      Then
        M^. Insert (NewStr (tFG. Name));

    Cls;
    i := ComMenu (lang (laChooseFileGroup), lang (laYourChoice), M);

    If i > 0 Then
    Begin
      LastGroup := R. FileGroup;
      LastArea := R. FileArea;
      SmartChangeFArea (i, 1, LastGroup, LastArea);
      UpdateFGroupMacro;
      UpdateFAreaMacro;
    End;

    Dispose (M, Done);
    CloseFileGroups;
  End;
End;

Procedure SelectFArea;
Var
  M   : PNotSortedCollection;
  i   : Integer;
  tFA : tFileArea;

Begin
  If OpenFileAreas Then
  Begin
    New (M, Init (16, 8));
    StartFAreaReadSequence (0);

    While ReadNextFileArea (tFA) Do
      If (tFA. Name <> '') And (WordInString (FileGroup. Tag, tFA. Group) Or
         (tFA. Group = '') Or (FileGroup. Tag = '')) And
         (tFA. ShowSec <= R. Security) And FlagsValid (R. Flags, tFA. ShowFlags)
      Then
        M^. Insert (NewStr (tFA. Name));

    Cls;
    i := ComMenu (lang (laChooseFileArea), lang (laYourChoice), M);

    If (i > 0) And (i <> R. FileArea) Then
    Begin
      SetFileArea (i);
      UpdateFAreaMacro;
    End;

    Dispose (M, Done);
    CloseFileAreas;
  End;
End;

Procedure SelectMGroup;
Var
  M                   : PNotSortedCollection;
  LastGroup, LastArea : LongInt;
  i                   : Integer;
  tMG                 : tMsgGroup;

Begin
  If OpenMsgGroups Then
  Begin
    New (M, Init (16, 8));
    StartMGroupReadSequence (0);

    While ReadNextMsgGroup (tMG) Do
      If (tMG. Name <> '') And (tMG. ShowSec <= R. Security) And
         FlagsValid (R. Flags, tMG. ShowFlags)
      Then
        M^. Insert (NewStr (tMG. Name));

    Cls;
    i := ComMenu (lang (laChooseMsgGroup), lang (laYourChoice), M);

    If i > 0 Then
    Begin
      LastGroup := R. MsgGroup;
      LastArea := R. MsgArea;
      SmartChangeMArea (i, 1, LastGroup, LastArea);
      UpdateMGroupMacro;
      UpdateMAreaMacro;
    End;

    Dispose (M, Done);
    CloseMsgGroups;
  End;
End;

Procedure SelectMArea;
Var
  M   : PNotSortedCollection;
  i   : Integer;
  tMA : tMsgArea;

Begin
  If OpenMsgAreas Then
  Begin
    New (M, Init (16, 8));
    StartMAreaReadSequence (0);

    While ReadNextMsgArea (tMA) Do
      If (tMA. Name <> '') And (WordInString (MsgGroup. Tag, tMA. Group) Or
         (MsgGroup. Tag = '') Or (tMA. Group = '')) And
         (tMA. ShowSec <= R. Security) And FlagsValid (R. Flags, tMA. ShowFlags)
      Then
        M^. Insert (NewStr (tMA. Name));

    Cls;
    i := ComMenu (lang (laChooseMsgArea), lang (laYourChoice), M);

    If (i > 0) And (i <> R. MsgArea) Then
    Begin
      SetMsgArea (i);
      UpdateMAreaMacro;
    End;

    Dispose (M, Done);
    CloseMsgAreas;
  End;
End;

Function OpenMessageArea;
Var
  B      : Boolean;
  AreaID : String;

Begin
  AreaID := MsgBaseLetter [MsgArea. BaseType] + MsgArea. BasePath;

  If Make Then B := OpenOrCreateMessageBase (Msg, AreaID)
          Else B := OpenMessageBase (Msg, AreaID);

  If B Then
    Msg^. SetBaseType (MsgArea. AreaType)
  Else
    If ErrorReport Then
    Begin
      If R. HotKeys Then
        ComWriteLn ('', 0);
      Message (lang (laMsgOpenError));
      LogWrite ('!', sm (smMAreaOpenErr) + ZeroMsg (MsgArea. Name, True));
    End;

  OpenMessageArea := B;
End;

Function PostMenu (Var S: String): tLineEditResult; Far;
Var
  b : Boolean;

Begin
  Repeat
    b := True;

    Case MenuBar ('|' + lang (laPostMenu), lang (laPostKeys)) Of
      1 : Begin
            Message ('|' + lang (laMsgSaved));
            PostMenu := mpSave;
          End;
      2 : Begin
            ComWriteLn ('', 0);
            b := Query (lang (laSure), False, 0);
            PostMenu := mpAbort;
          End;
      3 : Begin
            ComWriteLn ('', 0);
            PostMenu := mpContinue;
          End;
      4 : Begin
            S := Trim (GetAnswer ('|' + lang (laEnterMsgLine), 3, ofAllowEmpty,
              ''));
            If S = '' Then
              b := False;
            PostMenu := mpEditLine;
          End;
      5 : Begin
            ComWriteLn ('||' + lang (laMsgWriteText3), eoMacro + eoCodes);
            PostMenu := mpShow;
          End;
      6 : Begin
            S := Trim (GetAnswer ('|' + lang (laDelLines), 30, ofAllowEmpty,
              ''));
            If S = '' Then
              b := False;
            PostMenu := mpDeleteLine;
          End;
    End;
  Until b;
End;

Procedure ReadMessageText (SkipTearlineAndOrigin: Boolean);
Var
  S: String [79];
  i: Byte;

Begin
  Msg^. OpenMessage;
  Msg^. SetTextPos (Msg^. AfterLastKludge);

  While Not Msg^. EndOfMessage Do
  Begin
    Msg^. GetStringPChar (LineBuf, MaxLineSize);

    If LineBuf [0] = #1 Then
      Continue;

    If StrLen (LineBuf) = 0 Then
      MsgText^. InsLine ('')
    Else
    Begin
      S := TrimTrail (SplitStringPChar (LineBuf, 79, True, True));

      If Copy (S, 1, 8) = 'SEEN-BY:' Then
        Continue;

      If SkipTearlineAndOrigin And
         ((S = '---') Or (Copy (S, 1, 4) = '--- ') Or
          (Copy (S, 1, 10) = ' * Origin:'))
      Then
        Continue;

      MsgText^. InsLine (S);

      If StrLen (LineBuf) <> 0 Then
      Begin
        S := GetQuote (S);
        i := 79 - Length (S);

        While StrLen (LineBuf) <> 0 Do
          MsgText^. InsLine (S + Trim (SplitStringPChar (LineBuf, i, True, True)));
      End;
    End;
  End;

  Msg^. CloseMessage;
End;

Procedure EditLine (Var S: String); Far;
Begin
  S := GetAnswer ('', 73, 0, S);
End;

Function PostMsg (PostMode: tPostMode; ToUser, Subj: String; Const AReply, eMail: String;
                  ReplyToNumRelative: LongInt; Const Tpl: String): Boolean;
Var
  i, j, EditStartLine     : Integer;
  Options, EditY          : Byte;
  C                       : Char;
  B, Priv, NeedQuote      : Boolean;
  F                       : Text;
  DestAddr                : TAddress;
  UR                      : tUser;
  WToAddr, S, TmpTextName : String;
  Initials                : String [MaxInitials];
  FileBuf                 : FileBufArr;

  Function ProcessTemplate: Boolean;
  Var
    Line, Len                     : Integer;
    T                             : Text;
    Time, OTime                   : String [5];
    Date, ODate                   : String [10];
    DFName, CFName, OFName, Alias : String [20];
    BBS, OName                    : String [40];

    Function OpenTpl (Const Name: String): Boolean;
    Begin
      Assign (T, DefaultName (Name, 'tpl', Cnf. Path));
      ReSet (T);
      OpenTpl := IOResult = 0;
    End;

  Begin
    ProcessTemplate := False;

    If Not R. FSEditor Then
      Exit;

    If Not ((Trim (Tpl) <> '') And OpenTpl (Tpl)) Then
      If Not OpenTpl ('default') Then
        Exit;

    Assign (F, TmpTextName);
    SetTextBuf (F, FileBuf, FileBufSize);
    ReWrite (F);

    If IOResult = 0 Then
    Begin
      DFName := ExtractWord (1, ToUser, SpaceOnly);
      CFName := ExtractWord (1, R. Name, SpaceOnly);
      If R. Alias <> '' Then Alias := R. Alias
                        Else Alias := CFName;
      Date := FormattedCurrDT (Cnf. DateMask);
      Time := ShortStrTime;
      BBS := Cnf. BBSName;

      If PostMode = pmReply Then
      Begin
        ODate := FormattedDate (H^. MsgDate, Cnf. DateMask);
        OTime := FormattedDate (H^. MsgDate, 'HH:II');
        OName := H^. MsgTo;
        OFName := ExtractWord (1, OName, SpaceOnly);
      End Else
      Begin
        ODate := Date;
        OTime := Time;
        OName := '';
        OFName := '';
      End;

      Line := 1;

      While Not EoF (T) Do
      Begin
        ReadLn (T, S);
        S := TrimTrail (PlaceSubStr (S, #9, ReplaceTabSpaces));

        Len := Length (S);
        PlaceSubStrNoCaseP (S, '@IfReply', '');
        If (Length (S) <> Len) And (PostMode <> pmReply) Then
          Continue;

        Len := Length (S);
        PlaceSubStrNoCaseP (S, '@IfNew', '');
        If (Length (S) <> Len) And (PostMode <> pmNew) Then
          Continue;

        PlaceSubStrNoCaseP (S, '@Time', Time);
        PlaceSubStrNoCaseP (S, '@OTime', OTime);
        PlaceSubStrNoCaseP (S, '@Date', Date);
        PlaceSubStrNoCaseP (S, '@ODate', ODate);
        PlaceSubStrNoCaseP (S, '@BBS', BBS);
        PlaceSubStrNoCaseP (S, '@DName', ToUser);
        PlaceSubStrNoCaseP (S, '@DFName', DFName);
        PlaceSubStrNoCaseP (S, '@CName', R. Name);
        PlaceSubStrNoCaseP (S, '@CFName', CFName);
        PlaceSubStrNoCaseP (S, '@Alias', Alias);
        PlaceSubStrNoCaseP (S, '@OName', OName);
        PlaceSubStrNoCaseP (S, '@OFName', OFName);

        Len := Length (S);
        PlaceSubStrNoCaseP (S, '@Cursor', '');
        If Length (S) <> Len Then
          EditStartLine := Line;

        Len := Length (S);
        PlaceSubStrNoCaseP (S, '@Quote', '');
        If (Length (S) <> Len) And NeedQuote Then
        Begin
          Inc (Line, QuoteMsg (Msg, F, Initials));
          If Trim (S) = '' Then
            Continue;
        End;

        WriteLn (F, S);
        Inc (Line);
      End;

      Close (F);
    End;

    Close (T);
    ProcessTemplate := True;
  End;

Begin
  PostMsg := False;

  If (MsgArea. WriteSec > R. Security) Or
     Not FlagsValid (R. Flags, MsgArea. WriteFlags) Then
  Begin
    If R. HotKeys Then
      ComWriteLn ('', 0);
    Message (lang (laSecurityLow));
    Exit;
  End;

  SetTitle ('posting message');
  SetInputCap (NoCaps, AllChars);
  Cls;
  ComWriteLn (lang (laMsgHead), eoMacro + eoCodes);
  ComWrite (lang (laMsgFrom), eoMacro + eoCodes + eoNoFlush);
  ComWriteLn (Pad (R. Name, UserNameLen) +
    Pad (AddressToStrEx (MsgArea. Address), AddrLen), 0);
  ComWrite (lang (laMsgTo), eoMacro + eoCodes + eoNoFlush);

  ToUser := Trim (ToUser);
  If ToUser <> '' Then
    ComWrite (Pad (ToUser, UserNameLen), eoNoFlush)
  Else
    Repeat
      ToUser := '';
      ComRead (ToUser, UserNameLen, ofAllowEmpty + ofSpaceAdd);
      ToUser := Trim (ToUser);
      If ToUser = '' Then
        Exit;

      If (Pos ('$EXEC', UpString (ToUser)) <> 0) Or
         (Pos ('$FILE', UpString (ToUser)) <> 0) Then
      Begin
        ComWrite (EmuCursorLeft (UserNameLen), eoNoFlush);
        ComWrite (Replicate (' ', UserNameLen), eoNoFlush);
        ComWrite (EmuCursorLeft (UserNameLen), eoNoFlush);
      End
      Else
        Break;
    Until False;

  If UpString (ToUser) = 'SYSOP' Then
  Begin
    ToUser := Cnf. SysOp;
    ComWrite (EmuCursorLeft (UserNameLen), eoNoFlush);
    ComWrite (Pad (ToUser, UserNameLen), eoNoFlush);
  End
  Else
    If Cnf. Aliases And (MsgArea. AreaType = btLocal) Then
      If Is_User (ToUser, True) Then
      Begin
        GetUser (ToUser, UR, True);
        If ToUser <> UR. Name Then
        Begin
          ToUser := UR. Name;
          ComWrite (EmuCursorLeft (UserNameLen), eoNoFlush);
          ComWrite (Pad (ToUser, UserNameLen), eoNoFlush);
        End;
      End;

  WToAddr := '';
  If MsgArea. AreaType = btNetmail Then
  Begin
    If (Pos ('@', ToUser) <> 0) And (MsgArea. GateWay <> '') Then
    Begin
      WToAddr := RelativeAddr (MsgArea. GateWay, MsgArea. Address);
      ComWriteLn (WToAddr, 0);
    End Else
    If PostMode = pmReply Then
    Begin
      WToAddr := RelativeAddr (AddressToStrEx (H^. FromAddr), MsgArea. Address);
      ComWriteLn (WToAddr, 0);
    End Else
    If PostMode = pmEdit Then
    Begin
      WToAddr := RelativeAddr (AddressToStrEx (H^. ToAddr), MsgArea. Address);
      ComWriteLn (WToAddr, 0);
    End Else
    Begin
      ComRead (WToAddr, AddrLen, ofAllowEmpty + ofSpaceAdd);
      ComWriteLn (EmuCursorLeft (AddrLen) +
        Pad (RelativeAddr (WToAddr, MsgArea. Address), AddrLen), 0);
    End;
  End Else
    ComWriteLn ('', 0);

  ComWrite (lang (laMsgSubj), eoMacro + eoCodes);
  i := 79 - WhereX;

  If (PostMode = pmReply) Or (PostMode = pmEdit) Or (Subj <> '') Then
    ComWriteLn (Copy (Subj, 1, i), 0)
  Else
    ComReadLn (Subj, i, ofAllowEmpty);

  ComWriteLn (lang (laMsgFooter), eoMacro + eoCodes);
  EditY := WhereY;

  TmpTextName := Cnf. DoorInfoDir + 'msgtmp.';
  tDeleteFile (TmpTextName);

  EditStartLine := 1;
  NeedQuote := False;

  If PostMode = pmReply Then
  Begin
    If Cnf. PostQuote = atAsk Then
      NeedQuote := Query (lang (laQuoteMsg), True, 0)
    Else
      NeedQuote := Cnf. PostQuote = atYes;

    If NeedQuote Then
      If Cnf. QuotePrefix Then Initials := GetInitials (ToUser, MaxInitials)
                          Else Initials := '';

    If Not ProcessTemplate And NeedQuote Then
    Begin
      Assign (F, TmpTextName);
      SetTextBuf (F, FileBuf, FileBufSize);
      ReWrite (F);

      If IOResult = 0 Then
      Begin
        QuoteMsg (Msg, F, Initials);
        Close (F);
      End;
    End;
  End Else
  If PostMode = pmEdit Then
  Begin
    Assign (F, TmpTextName);
    SetTextBuf (F, FileBuf, FileBufSize);
    ReWrite (F);

    If IOResult <> 0 Then
      Exit;

    MsgText^. FreeAll;

    ReadMessageText (True);

    For i := 0 To MsgText^. Count-1 Do
      WriteLn(F, PString (MsgText^. At (i))^);

    MsgText^. FreeAll;

    Close (F);
  End Else
    ProcessTemplate;

  If MsgArea. AreaType = btNetmail Then
    Priv := True
  Else
    If MsgArea. Private = atAsk Then
      Priv := Query (lang (laQueryPrivate),
        (PostMode = pmReply) And H^. IsPriv, 0)
    Else
      Priv := MsgArea. Private = atYes;

  If Cnf. PostUpload = atAsk Then B := Query (lang (laUpLoadMsg), False, 0)
                             Else B := Cnf. PostUpload = atYes;
  If B Then
  Begin
    AutoDL := True;
    SmartChDir (Cnf. DoorInfoDir);
    Transfer ('', Receive, tsUploadMsg);
    SmartChDir (Cnf. Path);
    AutoDL := False;
  End
  Else
    If R. FSEditor Then
    Begin
      If Cnf. ExtMailEd <> '' Then
      Begin
        Assign (F, Cnf. DoorInfoDir + 'msginf.');
        SetTextBuf (F, FileBuf, FileBufSize);
        ReWrite (F);

        If IOResult = 0 Then
        Begin
          WriteLn (F, R. Name);
          WriteLn (F, ToUser);
          WriteLn (F, Subj);
          WriteLn (F, Long2Str (BbsLine));
          WriteLn (F, ZeroMsg (MsgArea. Name, True));
          If Priv Then WriteLn (F, 'YES')
                  Else WriteLn (F, 'NO');
          Close (F);
        End;

        DosShell (TranslateExecParams (Cnf. ExtMailEd), exCommand, False);
      End Else
      Begin
        ComWrite (EmuGoToXY (1, EditorBottomLine), eoNoFlush);
        ComWrite (lang (laEditStatusLine), eoMacro + eoCodes);

        Repeat
          C := fsEditFile (TmpTextName, [#27, #26], EditStartLine, ofsQuoting,
            Cnf. ColorScheme [edQuote], Cnf. ColorScheme [edText],
            1, EditY, 79, EditorBottomLine - 1);

          Case C Of
            #26 : Break;
            #27 : Begin
                    ComWrite (EmuGoToXY (1, EditorBottomLine - 1) + EmuClrEOL,
                      0);
                    If Query (lang (laSure), False, ofNoCR) Then
                    Begin
                      tDeleteFile (TmpTextName);
                      Break;
                    End;
                  End;
          End;
        Until False;
      End;
    End Else
    Begin
      Cls;
      ComWriteLn (lang (laMsgWriteText1), eoMacro + eoCodes);
      ComWriteLn (lang (laMsgWriteText2), eoMacro + eoCodes);
      ComWriteLn (lang (laMsgWriteText3), eoMacro + eoCodes);

      If lnEditFile (TmpTextName, PostMenu, EditLine,
         Cnf. ColorScheme [edQuote], Cnf. ColorScheme [edText]) = mpAbort
      Then
        tDeleteFile (TmpTextName);
    End;

  If FileExists (TmpTextName) Then
  Begin
    If Priv Then Options := pfPrivate
            Else Options := 0;
    ParseStrAddr (RelativeAddr (WToAddr, MsgArea. Address), DestAddr);

    PostFile (PostMode, TmpTextName, MtoAbs (R. MsgGroup, R. MsgArea), R. Name,
      ToUser, Subj, AReply, eMail, MsgArea. Address, DestAddr, ReplyToNumRelative, Options);

    tDeleteFile (TmpTextName);

    If PostMode <> pmEdit Then
    Begin
      Inc (Sys. MsgsPosted);
      Inc (R. MsgsPosted);
      LogWrite ('@', 'To: ' + ToUser + ', Subj: ' + Subj);
    End;

    PostMsg := True;
  End;
End;

Function DontShowMsg (Const mFrom, mTo: String; Const mFromAddr, mToAddr: TAddress): Boolean;
Var
  UserName: String;

Begin
  DontShowMsg := False;

  UserName := LoString (R. Name);

  If (R. Security >= MsgArea. SysOpSec) And
    FlagsValid (R. Flags, MsgArea. SysOpFlags)
  Then
    Exit;

  If (UserName = LoString (mTo)) And
     ((MsgArea. AreaType <> btNetmail) Or
      (AddressCompare (mToAddr, MsgArea. Address) = 0))
  Then
    Exit;

  If (UserName = LoString (mFrom)) And
     (AddressCompare (mFromAddr, MsgArea. Address) = 0)
  Then
    Exit;

  DontShowMsg := True;
End;

Function CannotModifyMsg: Boolean;
Begin
  If (R. Security >= MsgArea. SysOpSec) And
    FlagsValid (R. Flags, MsgArea. SysOpFlags) Then
  Begin
    CannotModifyMsg := False;
    Exit;
  End;

  CannotModifyMsg := True;

  If (H^. IsSent) Or (H^. ReplyNum <> 0) Then
    Exit;

  If (H^. MsgFrom <> R. Name) Or
     (AddressCompare (H^. FromAddr, MsgArea. Address) <> 0)
  Then
    Exit;

  If H^. IsRcvd And
     ((H^. MsgTo <> R. Name) Or
      ((MsgArea. AreaType = btNetmail) And
       (AddressCompare (H^. ToAddr, MsgArea. Address) <> 0)))
  Then
    Exit;

  CannotModifyMsg := False;
End;

Function ShowCurrentMsg (Pause, SkipSecured: Boolean): Boolean;

Var
  PS      : PString;
  i       : LongInt;
  Color   : Byte;
  Secured : Boolean;
  S       : String;

Begin
  ShowCurrentMsg := True;

  If HotKeysStr <> '' Then
    Exit;

  If Not Msg^. OpenMessage Then
    Exit;

  FillChar (H^, SizeOf (H^), #0);
  Msg^. GetFromAndToAddress (H^. FromAddr, H^. ToAddr);
  Msg^. GetWrittenDateTime (H^. MsgDateMBDT);
  MBDateTime2DosDateTime (H^. MsgDateMBDT, H^. MsgDate);
  H^. MsgFrom := Trim (Msg^. GetFrom);
  H^. MsgTo := Trim (Msg^. GetTo);
  H^. MsgSubj := Trim (PlaceSubStr (PlaceSubStr (Msg^. GetSubject, #10, ' '), #13, ''));
  H^. IsPriv := Msg^. GetAttribute (maPrivate);
  H^. IsRcvd := Msg^. GetAttribute (maReceived);
  H^. IsSent := Msg^. GetAttribute (maSent);
  H^. MsgNum := Msg^. Current;

  Secured := H^. IsPriv And DontShowMsg (H^. MsgFrom, H^. MsgTo, H^. FromAddr, H^. ToAddr);
  If Secured And SkipSecured And (H^. MsgNum > 1) And (H^. MsgNum < Msg^. GetCount) Then
  Begin
    ShowCurrentMsg := False;
    Exit;
  End;

  H^. ReplyNum := Msg^. GetFirstReply;
  If H^. ReplyNum <> 0 Then
    H^. ReplyNum := Msg^. AbsoluteToRelative (H^. ReplyNum);

  H^. ReplyTo := Msg^. GetReplyTo;
  If H^. ReplyTo <> 0 Then
    H^. ReplyTo := Msg^. AbsoluteToRelative (H^. ReplyTo);

  If Msg^. GetKludge (#1'MSGID', S) Then
    H^. MSGID := Copy (S, 9, 255);

  If (MsgArea. AreaType = btNetmail) And
    Msg^. GetKludge (#1'REPLYADDR', S) Then
    If Pos('<', S) <> 0 Then
      H^. eMail := Trim (ExtractWord (2, S, ['<', '>']))
    Else
      H^. eMail := Trim (S);

  If Not H^. IsRcvd Then
  Begin
    S := UpString (H^. MsgTo);
    if ((S = UpString (R. Name)) Or (S = UpString (R. Alias))) And
       ((MsgArea. AreaType <> btNetmail) Or
        (AddressCompare (H^. ToAddr, MsgArea. Address) = 0)) Then
    Begin
      H^. IsRcvd := True;
      Msg^. SetAttribute (maReceived, True);
      Msg^. WriteMessage;
    End;
  End;

  Msg^. CloseMessage;

  If Pause Then
    Cls;

  ComWriteLn (lang (laMsgHead), eoMacro + eoCodes);

  If Not (Pause Or More) Then
  Begin
    FinishReading := True;
    Exit;
  End;

  S := Long2Str (H^. MsgNum) + '/' + Long2Str (Msg^. GetCount);

  If H^. ReplyTo <> 0 Then
    S := S + ' -' + Long2Str (H^. ReplyTo);
  If H^. ReplyNum <> 0 Then
    S := S + ' +' + Long2Str (H^. ReplyNum);
  If H^. IsRcvd Then
    S := S + ' Rcv';
  If H^. IsSent Then
    S := S + ' Snt';
  If H^. IsPriv Then
    S := S + ' Pvt';

  If Length (S) >= UserNameLen Then
  Begin
    S [UserNameLen] := '>';
    SetLength (S, UserNameLen);
  End;

  ComWriteLn (lang (laMsgNum) + Pad (S, UserNameLen) + FormattedDate (
    H^. MsgDate, 'DD NNN YYYY HH:II'), eoMacro + eoCodes);

  If Not (Pause Or More) Then
  Begin
    FinishReading := True;
    Exit;
  End;

  ComWrite (lang (laMsgFrom), eoMacro + eoCodes + eoNoFlush);
  ComWrite (Pad (H^. MsgFrom, UserNameLen), eoNoFlush);
  ComWriteLn (AddressToStrEx (H^. FromAddr), eoDisable01);

  If Not (Pause Or More) Then
  Begin
    FinishReading := True;
    Exit;
  End;

  ComWrite (lang (laMsgTo), eoMacro + eoCodes + eoNoFlush);

  If MsgArea. AreaType <> btNetmail Then
    ComWriteLn (Copy (H^. MsgTo, 1, UserNameLen), eoDisable01)
  Else
    ComWriteLn (Pad (H^. MsgTo, UserNameLen) + AddressToStrEx (H^. ToAddr),
      eoDisable01);

  If Not (Pause Or More) Then
  Begin
    FinishReading := True;
    Exit;
  End;

  ComWrite (lang (laMsgSubj), eoMacro + eoCodes + eoNoFlush);

  If Secured Then
    ComWriteLn ('* Private *', 0)
  Else
    ComWriteLn (Copy (H^. MsgSubj, 1, 79 - Length (ZeroMsg (lang (laMsgSubj),
      True))), eoDisable01);

  If Not (Pause Or More) Then
  Begin
    FinishReading := True;
    Exit;
  End;

  ComWriteLn (lang (laMsgFooter), eoMacro + eoCodes);

  If Not (Pause Or More) Then
  Begin
    FinishReading := True;
    Exit;
  End;

  If Pause Then
    InitMore (WhereY);

  If Secured Then
  Begin
    ComWriteLn (#13#10 + lang (laPrivMsg), eoMacro + eoCodes);
    Exit;
  End;

  MsgText^. FreeAll;

  ReadMessageText (False);

  For i := 0 To MsgText^. Count-1 Do
  Begin
    PS := MsgText^. At (i);

    If Length (PS^) > 0 Then
    Begin
      If ((i >= MsgText^. Count - 3) And (Copy (PS^, 1, 4) = '... ')) Or
         ((i >= MsgText^. Count - 2) And
          ((PS^ = '---') Or (Copy (PS^, 1, 4) = '--- '))) Or
         ((i = MsgText^. Count - 1) And (Copy (PS^, 1, 10) = ' * Origin:'))
      Then
        Color := Cnf. ColorScheme [mrOrigin]
      Else
      Begin
        S := GetQuote (PS^);

        If Length (S) = 0 Then
          Color := Cnf. ColorScheme [mrNormal]
        Else
        If Odd (SymbolCount (S, QuoteChar)) Then
          Color := Cnf. ColorScheme [mrQuote]
        Else
          Color := Cnf. ColorScheme [mrQuote2];
      End;

      ComWriteLn (EmuRelColor (Color) + PS^, eoDisable01);
    End
    Else
      ComWriteLn ('', 0);

    If Not More Then
    Begin
      If Not Pause Then
        FinishReading := True;
      Break;
    End;

    If DrawAborted Then
      Break;
  End;

  MsgText^. FreeAll;
End;

Function CheckAndOpenMsgArea: Boolean;
Begin
  If (MsgArea. ReadSec > R. Security) Or
     Not FlagsValid (R. Flags, MsgArea. ReadFlags) Then
  Begin
    If R. HotKeys Then
      ComWriteLn ('', 0);
    Message (lang (laSecurityLow));
    CheckAndOpenMsgArea := False;
  End
  Else
    CheckAndOpenMsgArea := OpenMessageArea (True, True);
End;

Function ReplyToCurrMsg (Const Tpl: String): Boolean;
Begin
  LogWrite ('+', sm (smAnswerInArea) + ZeroMsg (MsgArea. Name, True));
  If PostMsg (pmReply, H^. MsgFrom, H^. MsgSubj,
              H^. MSGID, H^. eMail, H^. MsgNum, Tpl) Then
  Begin
    ReplyToCurrMsg := True;
  End Else
    ReplyToCurrMsg := False;
End;

Procedure ReadMsgs;
Var
  i              : Integer;
  MsgNum         : LongInt;
  PauseAfterEach : Boolean;

Label
  ReSelect;

Begin
  If Not CheckAndOpenMsgArea Then
    Exit;

  If Msg^. GetCount = 0 Then
    Message (#13#10 + lang (laNoMsgsInArea))
  Else
  Begin
    SetTitle ('reading messages');

    MsgNum := 1;
    If R. LastRead >= 0 Then
    Begin
      If MsgArea. BaseType = mbfJam Then
        LR := Crc32Str (LoString (R. Name))
      Else
        LR := R. LastRead;
      MsgNum := Msg^. GetLastRead (LR);
      If MsgNum < 1 Then
        MsgNum := 1;
    End;

    Msg^. Seek (MsgNum);
    If Not Msg^. SeekFound Then
      Msg^. Seek (1);

    New (H);
    GetMem (LineBuf, MaxLineSize);

    PauseAfterEach := Query (lang (laPauseAfterEach), True, 0);
    If Not PauseAfterEach Then
      Cls;
    InitMore (0);
    FinishReading := False;
    i := 0;

    While Not FinishReading Do
    Begin
      While Not ShowCurrentMsg (PauseAfterEach, (i <> 12) And (i <> 13)) Do
        If i = 5 Then Msg^. SeekPrev
                 Else Msg^. SeekNext;

      ComWriteLn ('', 0);

      If Not PauseAfterEach Then
      Begin
        If Not More Or DrawAborted Then
          Break;
      End Else
      Begin
        If R. LastRead >= 0 Then
          Msg^. SetLastRead (LR, H^. MsgNum);

      ReSelect:
        i := MenuBar (lang (laMsgString), Copy (lang (laMsgKeys), 1, 3) +
          #13 + Copy (lang (laMsgKeys), 4, 255));

        If (Msg^. GetCount < 1) And (i in [1..6, 8, 9, 11]) Then
        Begin
          ComWriteLn (#13#10 + lang (laNoMsgsInArea), eoMacro + eoCodes);
          Goto ReSelect;
        End;

        Case i Of
           1 : Continue;                                            {Again}

           2 : Begin                                               {Delete}
                 If CannotModifyMsg Then
                 Begin
                   If R. HotKeys Then
                     ComWriteLn ('', 0);
                   Message (lang (laSecurityLow));
                   Continue;
                 End;

                 If Not R. Frames Or (R. Emu = teTty) Then
                   ComWriteLn ('', 0);

                 If Query (lang (laDeleteMsg), False, ofFramed) Then
                 Begin
                   LogWrite ('+', sm (smlDeletingMsg) +
                     Long2Str (H^. MsgNum) + sm (smFromArea) +
                     ZeroMsg (MsgArea. Name, True));

                   Msg^. KillMessage;
                   Msg^. SeekNext;

                   If Not Msg^. SeekFound Then
                     Msg^. Seek (Msg^. GetCount);

                   If Msg^. SeekFound Then
                     Continue;
                 End
                 Else
                   Continue;
               End;

        3, 4 : ;                                                     {Next}

           5 : Begin                                             {Previous}
                 Msg^. SeekPrev;

                 If Msg^. SeekFound Then
                   Continue
                 Else
                 Begin
                   Msg^. Seek (1);
                   ComWrite (#13, 0);
                   ComWriteLn (EmuClrEOL, eoNoFlush);
                   ComWriteLn (lang (laNoPrevMsg), eoMacro + eoCodes);
                   Goto ReSelect;
                 End;
               End;

           6 : If Not (H^. IsPriv And DontShowMsg (H^. MsgFrom,     {Reply}
                  H^. MsgTo, H^. FromAddr, H^.ToAddr)) Then
               Begin
                 If ReplyToCurrMsg ('menu') Then
                 Begin
                   Cls;
                   Message (lang (laMsgSaved));
                   Msg^. Seek (H^. MsgNum);
                   If Msg^. SeekFound Then
                   Begin
                     If (Msg^. Current = H^.MsgNum) And
                        (H^. ReplyNum = 0) Then
                     Begin
                       Msg^. OpenMessageHeader;
                       Msg^. SetFirstReply (Msg^. GetHighest);
                       Msg^. WriteMessageHeader;
                       Msg^. CloseMessage;
                     End;
                     Continue;
                   End;
                 End Else
                   Continue;
               End Else
                 Continue;

           7 : If PostMsg (pmNew, '', '', '', '', 0, 'menu') Then    {Post}
               Begin
                 LogWrite ('+', sm (smlPosting) + ZeroMsg (MsgArea. Name,
                   True));
                 Cls;
                 Message (lang (laMsgSaved));
                 Msg^. Seek (H^. MsgNum);
                 If Msg^. SeekFound Then
                   Continue;
               End Else
                 Continue;

           8 : Begin                                             {To begin}
                 Msg^. Seek (1);
                 If Not Msg^. SeekFound Then
                 Begin
                   ComWrite (#13, 0);
                   ComWriteLn (EmuClrEOL, eoNoFlush);
                   ComWriteLn (lang (laNoMoreMessages), eoMacro + eoCodes);
                   Goto ReSelect;
                 End;
                 Continue;
               End;

           9 : Begin                                               {To end}
                 Msg^. Seek (Msg^. GetCount);
                 If Not Msg^. SeekFound Then
                 Begin
                   ComWrite (#13, 0);
                   ComWriteLn (EmuClrEOL, eoNoFlush);
                   ComWriteLn (lang (laNoMoreMessages), eoMacro + eoCodes);
                   Goto ReSelect;
                 End;
                 Continue;
               End;

          10 : Break;                                                {Stop}
          11 : Begin                                               {Change}
                 If CannotModifyMsg Then
                 Begin
                   If R. HotKeys Then
                     ComWriteLn ('', 0);
                   Message (lang (laSecurityLow));
                   Continue;
                 End;

                 If Not R. Frames Or (R. Emu = teTty) Then
                   ComWriteLn ('', 0);

                 LogWrite ('+', sm (smlEditingMsg) +
                   Long2Str (H^. MsgNum) + sm (smInArea) +
                   ZeroMsg (MsgArea. Name, True));

                 PostMsg (pmEdit, H^. MsgTo, H^. MsgSubj, '', '', 0, 'menu');

                 Continue;
               End;
          12 : Begin                                       {GoToThreadPrev}
                 If H^. ReplyTo <> 0 Then
                   Msg^. Seek (H^. ReplyTo);
                 If Not Msg^. SeekFound Then
                   Msg^. Seek (H^. MsgNum);
                 Continue;
               End;
          13 : Begin                                       {GoToThreadNext}
                 If H^. ReplyNum <> 0 Then
                   Msg^. Seek (H^. ReplyNum);
                 If Not Msg^. SeekFound Then
                   Msg^. Seek (H^. MsgNum);
                 Continue;
               End;
        End;
      End;

      Msg^. SeekNext;
      If Not Msg^. SeekFound Then
      Begin
        ComWrite (#13, 0);
        ComWriteLn (EmuClrEOL, eoNoFlush);
        ComWriteLn (lang (laNoMoreMessages), eoMacro + eoCodes);
        If PauseAfterEach Then
        Begin
          Msg^. Seek (Msg^. GetCount);
          Goto ReSelect;
        End;

        Message ('');
        Break;
      End;
    End;

    If Not PauseAfterEach And (R. LastRead >= 0) Then
      Msg^. SetLastRead (LR, H^. MsgNum);

    Dispose (H);
    FreeMem (LineBuf, MaxLineSize);
    MsgText^. SetLimit (0);
  End;

  If Not CloseMessageBase (Msg) Then
    Message (lang (laErrorMBClose));
End;

Procedure ListMsgs;
Type
  NumsArray = Array [1..ml_MaxNumbers] of LongInt;

Var
  Numbers               : ^NumsArray;
  ShowNumsColl          : PLongIntCollection;
  i, CurrNum, CountNums : Integer;
  mFrom, mTo            : String [UserNameLen];
  mFromAddr, mToAddr    : TAddress;
  S, EnterStr           : String;
  MoreRes               : Boolean;
  FromColors, ToColors  : Array [Boolean] Of Byte;

  Procedure ParseNumbers;

    Procedure InsNum (Num: LongInt);
    Begin
      Num := Numbers^ [Num];
      If Not ShowNumsColl^. Contains (Num) Then
        ShowNumsColl^. Insert (Pointer (Num));
    End;

  Var
    j, sItem, eItem : LongInt;
    i, Words        : Integer;
    Item            : String;

  Begin
    Words := WordCount (S, SpaceAndComma);

    For i := 1 To Words Do
    Begin
      Item := ExtractWord (i, S, SpaceAndComma);

      If Pos ('-', Item) = 0 Then
      Begin
        j := Str2Long (Item);
        If (j > 0) And (j <= CountNums) Then
          InsNum (j);
      End Else
      Begin
        sItem := Str2Long (ExtractWord (1, Item, MinusOnly));
        eItem := Str2Long (ExtractWord (2, Item, MinusOnly));
        If sItem < 1 Then
          sItem := 1;
        If eItem > CountNums Then
          eItem := CountNums;

        For j := sItem To eItem Do
          InsNum (j);
      End;
    End;

    S := '';
  End;

Label
  EndOfProc,
  ShowMsgs;

Begin
  If Not CheckAndOpenMsgArea Then
    Exit;

  Msg^. Seek (1);
  If Not Msg^. SeekFound Then
  Begin
    Message (#10 + lang (laNoMsgsInArea));
    Goto EndOfProc;
  End;

  If MsgArea. BaseType = mbfJam Then
    LR := Crc32Str (LoString (R. Name))
  Else
    LR := R. LastRead;

  SetTitle ('browsing messages list');
  New (Numbers);
  New (ShowNumsColl, Init (0, 8));
  CurrNum := 0;
  CountNums := 0;
  EnterStr := lang (laEnterMsgNums);
  FromColors [False] := Cnf. ColorScheme [mlFrom];
  FromColors [True] := Cnf. ColorScheme [mlLightFrom];
  ToColors [False] := Cnf. ColorScheme [mlTo];
  ToColors [True] := Cnf. ColorScheme [mlLightTo];

  Cls;
  ComWriteLn (lang (laListHeader), eoMacro + eoCodes);
  ComWriteLn (EmuRelColor (Cnf. ColorScheme [umSeparator]) +
    '컴 컴컴컴컴컴컴컴컴컴컴컴컴 컴컴컴컴컴컴컴컴컴컴컴컴 컴컴컴컴컴컴컴컴컴컴컴컴�',
    0);
  InitMore (WhereY);

  Repeat
    Inc (CurrNum);
    If CurrNum > ml_MaxNumbers Then
      CurrNum := 1;
    If CurrNum > CountNums Then
      CountNums := CurrNum;
    Numbers^ [CurrNum] := Msg^. Current;

    Msg^. OpenMessageHeader;
    Msg^. GetFromAndToAddress(mFromAddr, mToAddr);
    mFrom := Trim (Msg^. GetFrom);
    mTo := Trim (Msg^. GetTo);

    ComWrite (EmuRelColor (Cnf. ColorScheme [mlNumber]) +
      LeftPadCh (Long2Str (CurrNum), '0', ml_NumLen) + ' ', eoNoFlush);
    ComWrite (EmuRelColor (FromColors [mFrom = R. Name]) + Pad (mFrom,
      ml_FromLen) + ' ', eoNoFlush + eoDisable01);
    ComWrite (EmuRelColor (ToColors [mTo = R. Name]) + Pad (mTo, ml_ToLen) +
      ' ', eoNoFlush + eoDisable01);
    ComWrite (EmuRelColor (Cnf. ColorScheme [mlSubj]), eoNoFlush);

    If Msg^. GetAttribute (maPrivate) And
      DontShowMsg (mFrom, mTo, mFromAddr, mToAddr)
    Then
      ComWriteLn ('* Private *', 0)
    Else
    Begin
      S := Trim (PlaceSubStr (PlaceSubStr (Msg^. GetSubject, #10, ' '), #13, ''));
      ComWriteLn (Copy (S, 1, ml_SubjLen), eoDisable01);
    End;

    Msg^. CloseMessage;

    MoreRes := MoreNums (S, EnterStr, 0, True);
    If Length (S) > 0 Then
      ParseNumbers;
    If Not MoreRes Then
      Goto ShowMsgs;

    If DrawAborted Then
      Break;

    Msg^. SeekNext;
  Until Not Msg^. SeekFound;

  ComWrite (#13#10 + EnterStr, eoMacro + eoCodes);
  ComReadLn (S, 78 - WhereX, ofAllowEmpty);
  S := Trim (S);
  If Length (S) > 0 Then
    ParseNumbers;

ShowMsgs:
  If ShowNumsColl^. Count > 0 Then
  Begin
    New (H);
    GetMem (LineBuf, MaxLineSize);

    For i := 0 To ShowNumsColl^. Count-1 Do
    Begin
      Msg^. Seek (LongInt (ShowNumsColl^. At (i)));

      If Msg^. SeekFound Then
      Begin
        ShowCurrentMsg (True, False);

        If (H^. IsPriv And
            DontShowMsg (H^. MsgFrom, H^. MsgTo, H^. FromAddr, H^. ToAddr)) Or
           (MsgArea. WriteSec > R. Security) Or
           Not FlagsValid (R. Flags, MsgArea. WriteFlags) Then
        Begin
          If i = ShowNumsColl^. Count-1 Then
            Message ('')
          Else
            If Not Query ('|' + lang (laNextMsg), True, 0) Then
              Break;
        End
        Else
          Case Query_YNQ ('|' + lang (laReplyNextMsg), False) Of
            'y' : ReplyToCurrMsg ('list');
            'q' : Break;
          End;
      End;
    End;

    Dispose (H);
    FreeMem (LineBuf, MaxLineSize);
    MsgText^. SetLimit (0);
  End;

  Dispose (Numbers);
  Dispose (ShowNumsColl, Done);

EndOfProc:
  If Not CloseMessageBase (Msg) Then
    Message (lang (laErrorMBClose));
End;

Procedure SearchPrivate;
Var
  PassedAreasColl   : PSortedLongIntCollection;
  PrivNumsColl      : PLongIntCollection;
  i, j, a, Groups   : Integer;
  OldGroup, OldArea : Word;
  Found, Finish     : Boolean;
  UpName, UpAlias   : PString;

  Procedure ReadFound;
  Var
    P : Integer;
    S : String;

  Begin
    Msg^. OpenMessageHeader;
    H^. MsgFrom := Msg^. GetFrom;
    Msg^. CloseMessage;

    Found := True;

    If R. Frames And (R. Emu <> teTty) Then
      Cls;

    S := '|' + PlaceSubStrNoCase (lang (laFoundPrivMsg), '@MsgArea',
      MsgArea. Name);
    P := Pos ('@FROMNAME', UpString (S));
    If P <> 0 Then
    Begin
      ComWrite (Copy (S, 1, P - 1), eoMacro + eoCodes + eoNoFlush);
      ComWrite (H^. MsgFrom, eoDisable01 + eoNoFlush);
      ComWriteLn (Copy (S, P + 9, 255), eoMacro + eoCodes);
    End
    Else
      ComWriteLn (S, eoMacro + eoCodes);

    Case Query_YNQ (lang (laWantToRead), True) Of
      'n' : Exit;
      'q' : Begin
              Finish := True;
              Exit;
            End;
    End;

    ShowCurrentMsg (True, False);
    ComWriteLn ('', 0);

    If Query (lang (laWantToAnswer), True, 0) Then
    Begin
      ReplyToCurrMsg ('search');
      If R. FSEditor Then
        Cls;
    End;

    ComWriteLn ('', 0);
  End;

Begin
  Found := False;
  Finish := False;
  SetTitle ('searching private mail');

  New (PassedAreasColl, Init (16, 16));
  New (PrivNumsColl, Init (0, 8));
  New (H);
  GetMem (LineBuf, MaxLineSize);

  LogWrite ('+', sm (smlSearchPrivate));
  If R. Frames And (R. Emu <> teTty) Then
    Cls;
  ComWriteLn ('', 0);

  OldGroup := R. MsgGroup;
  OldArea := R. MsgArea;

  UpName := NewStr (UpString (Trim (R. Name)));
  UpAlias := NewStr (UpString (Trim (R. Alias)));

  OpenMsgGroups;
  OpenMsgAreas;
  SetMsgGroup (0);
  Groups := mmGroupsAmount;
  If Groups = 0 Then
    Groups := 1;

  For i := 1 To Groups Do
  Begin
    Finish := Finish Or DrawAborted;
    If Finish Then
      Break;

    SetMsgGroup (i);
    UpdateMGroupMacro;

    For j := 1 To mAreasGroup Do
    Begin
      Finish := Finish Or DrawAborted;
      If Finish Then
        Break;

      Clock2;
      SetMsgArea (j);

      If Not MsgArea. ScanPrivMail Or
         PassedAreasColl^. Search (Pointer (PhysMsgArea), a)
      Then
        Continue;

      PassedAreasColl^. AtInsert (a, Pointer (PhysMsgArea));

      UpdateMAreaMacro;
      ComWrite (EmuRelColor (9 + Random (6)) + lang (laScanMsgAreas), eoMacro +
        eoCodes);
      ComWrite (EmuClrEOL + #13, 0);

      If Not OpenMessageArea (False, False) Then
        Continue;

      If R. LastRead >= 0 Then
      Begin
        If MsgArea. BaseType = mbfJam Then
          LR := Crc32Str (LoString (R. Name))
        Else
          LR := R. LastRead;

        Msg^. Seek (Msg^. GetLastRead (LR));

        If Msg^. SeekFound Then
          Msg^. SeekNext;
      End Else
        Msg^. Seek (1);

      While Msg^. SeekFound Do
      Begin
        Msg^. OpenMessageHeader;
        Msg^. GetToAddress (H^. ToAddr);
        H^. MsgTo := UpString (Trim (Msg^. GetTo));
        H^. IsRcvd := Msg^. GetAttribute (maReceived);
        Msg^. CloseMessage;

        If Not H^. IsRcvd And
          (((UpName <> Nil) And (H^. MsgTo = UpName^)) Or
           ((UpAlias <> Nil) And (H^. MsgTo = UpAlias^))) And
          ((MsgArea. AreaType <> btNetmail) Or
           (AddressCompare (H^. ToAddr, MsgArea. Address) = 0))
        Then
          PrivNumsColl^. Insert (Pointer (Msg^. Current));

        Msg^. SeekNext;
      End;

      For a := 0 To PrivNumsColl^. Count-1 Do
      Begin
        Msg^. Seek (LongInt (PrivNumsColl^. At (a)));
        If Msg^. SeekFound Then
        Begin
          ReadFound;

          If Finish Then
            Break;
        End;
      End;

      PrivNumsColl^. FreeAll;
      CloseMessageBase (Msg);
    End;
  End;

  DisposeStr (UpName);
  DisposeStr (UpAlias);

  Dispose (H);
  FreeMem (LineBuf, MaxLineSize);
  Dispose (PrivNumsColl, Done);
  Dispose (PassedAreasColl, Done);
  MsgText^. SetLimit (0);

  SetMsgGroup (OldGroup);
  SetMsgArea (OldArea);
  UpdateMGroupMacro;
  UpdateMAreaMacro;

  CloseMsgGroups;
  CloseMsgAreas;

  If Not Found Then
  Begin
    ComWrite (#13, 0);
    ComWrite (EmuClrEOL, 0);
    Message (lang (laNoPrivFound));
  End;
End;

Procedure SeparateMasks (Const S: String; Var Masks: PAsciizCollection);
Var
  i, j : Integer;

Begin
  j := WordCount (S, SpaceOnly);
  Masks := New (PAsciizCollection, Init (j, 0));

  For i := 1 To j Do
    Masks^. InsItem ('*' + ExtractWord (i, S, SpaceOnly) + '*');
End;

Procedure SearchMessages (Const Keywords: String);
Var
  Masks         : PAsciizCollection;
  MatchNumsColl,
  MatchedMasks  : PLongIntCollection;
  i             : Integer;
  Found, Match  : Boolean;
  mFrom, mTo    : String [80];
  mFromAddr,
  mToAddr       : TAddress;
  S             : String;

Label
  Show,
  Done,
  EndOfProc;

Begin
  If Not CheckAndOpenMsgArea Then
    Exit;

  Msg^. Seek (1);
  If Not Msg^. SeekFound Then
  Begin
    Message (#10 + lang (laNoMsgsInArea));
    Goto EndOfProc;
  End;

  SetTitle ('searching messages by keywords');
  Found := False;

  If MsgArea. BaseType = mbfJam Then
    LR := Crc32Str (LoString (R. Name))
  Else
    LR := R. LastRead;

  SeparateMasks (Keywords, Masks);
  New (MatchNumsColl, Init (0, 8));
  New (MatchedMasks, Init (Masks^. Count, 0));
  New (H);
  GetMem (LineBuf, MaxLineSize);

  LogWrite ('+', PlaceSubStr (sm (smlSearchKeyword), '%masks%', Keywords));
  ComWrite ('|' + lang (laMsgMaskSearch), eoMacro + eoCodes);

  Repeat
    If DrawAborted Then
      Goto Done;

    Clock2;
    SlashRotate;

    For i := 1 To 16 Do
    Begin
      Msg^. OpenMessageHeader;
      Msg^. GetFromAndToAddress(mFromAddr, mToAddr);
      mFrom := Trim (Msg^. GetFrom);
      mTo := Trim (Msg^. GetTo);
      H^. IsPriv := Msg^. GetAttribute (maPrivate);
      H^. MsgSubj := Trim (PlaceSubStr (PlaceSubStr (Msg^. GetSubject, #10, ' '), #13, ''));
      Msg^. CloseMessage;

      If Not (H^. IsPriv And DontShowMsg (mFrom, mTo, mFromAddr, mToAddr)) Then
      Begin
        Match := TestMasks (Masks, MatchedMasks, H^. MsgSubj) Or TestMasks (Masks,
          MatchedMasks, mFrom) Or TestMasks (Masks, MatchedMasks, mTo);

        If Not Match Then
        Begin
          Msg^. OpenMessage;
          Msg^. SetTextPos (Msg^. AfterLastKludge);
          While Not Msg^. EndOfMessage Do
          Begin
            Msg^. GetStringPChar (LineBuf, MaxLineSize);
            If (LineBuf [0] = #1) Or (StrLen (LineBuf) = 0) Then
              Continue;

            While StrLen (LineBuf) <> 0 Do
              If TestMasks (Masks, MatchedMasks,
                SplitStringPChar (LineBuf, 255, False, False)) Then
              Begin
                Match := True;
                Break;
              End;
            If Match THen
              Break;
          End;
          Msg^. CloseMessage;
        End;

        If Match Then
          MatchNumsColl^. Insert (Pointer (Msg^. Current));
      End;

      MatchedMasks^. DeleteAll;
      Msg^. SeekNext;
      If Not Msg^. SeekFound Then
        Goto Show;
    End;
  Until False;

Show:
  For i := 0 To MatchNumsColl^. Count-1 Do
  Begin
    Msg^. Seek (LongInt (MatchNumsColl^. At (i)));

    If Msg^. SeekFound Then
    Begin
      Found := True;
      ShowCurrentMsg (True, False);

      If (MsgArea. WriteSec > R. Security) Or
         Not FlagsValid (R. Flags, MsgArea. WriteFlags) Then
      Begin
        If i = MatchNumsColl^. Count-1 Then
          Message ('')
        Else
          If Not Query ('|' + lang (laNextMsg), True, 0) Then
            Break;
      End
      Else
        Case Query_YNQ ('|' + lang (laReplyNextMsg), False) Of
          'q' : Break;
          'y' : ReplyToCurrMsg ('mask');
        End;
    End;
  End;

Done:
  FreeMem (LineBuf, MaxLineSize);
  Dispose (H);
  Dispose (MatchedMasks, Done);
  Dispose (MatchNumsColl, Done);
  Dispose (Masks, Done);
  MsgText^. SetLimit (0);

  If Not Found Then
  Begin
    ComWrite (#13, 0);
    ComWrite (EmuClrEOL, 0);
    Message (lang (laMsgMaskNotFound));
  End;

EndOfProc:
  If Not CloseMessageBase (Msg) Then
    Message (lang (laErrorMBClose));
End;

Procedure GlobalSearch (Const WildCard: String; Mode: tFSearchMode;
                        SinceLast: AskType; Const dFrom: String);
Var
  PassedAreasColl           : PSortedLongIntCollection;
  Masks                     : PAsciizCollection;
  i, j, idx                 : Integer;
  Groups, OldGroup, OldArea : Word;
  Found, tNf, Aborted       : Boolean;
  DateFrom                  : String [18];

  Procedure CheckSince;
  Var
    DT : DateTime;

  Begin
    DateFrom := Long2Date (R. LastDate, 'DD-MM-YYYY') + ' ' +
      Word2Time (R. LastTime);
    Aborted := False;

    If SinceLast <> atYes Then
    Begin
      If SinceLast = atNo Then
      Begin
        If dFrom <> '' Then
        Begin
          DateFrom := ReFormatDate (dFrom, Cnf. DateMask, 'DD-MM-YYYY');
          Exit;
        End;
      End
      Else
        If Query (lang (laFindNewFiles), True, ofFramed) Then
          Exit
        Else
          If dFrom <> '' Then
          Begin
            Aborted := True;
            Exit;
          End;

      DateFrom := Trim (GetAnswer (PlaceSubStrNoCase (lang (laNewSinceDate),
        '@DateMask', Cnf. DateMask), Length (Cnf. DateMask), ofAllowEmpty, ''));

      If ExtractDT (DateFrom, Cnf. DateMask, DT) Then
        DateFrom := FormattedDate (DT, 'DD-MM-YYYY')
      Else
        Aborted := True;
    End;
  End;

Begin
  OldGroup := R. FileGroup;
  OldArea := R. FileArea;

  If R. Frames And (R. Emu <> teTty) Then Cls
                                     Else ComWriteLn ('', 0);
  Case Mode Of
    fsDate : Begin
               CheckSince;
               If Aborted Then
                 Exit;

               LogWrite ('+', sm (smlNewSearch));
               ComWriteLn ('', 0);
             End;
    fsName : LogWrite ('+', PlaceSubStr (sm (smlFSearchName), '%masks%',
               WildCard));
    fsDesc : Begin
               LogWrite ('+', PlaceSubStr (sm (smlFSearchDesc), '%masks%',
                 WildCard));
               SeparateMasks (WildCard, Masks);
             End;
  End;

  SetTitle ('searching files');

  New (PassedAreasColl, Init (64, 16));
  Aborted := False;
  Found := False;
  InitDispFiles;

  OpenFileGroups;
  OpenFileAreas;
  SetFileGroup (0);
  Groups := ffGroupsAmount;
  If Groups = 0 Then
    Groups := 1;

  For i := 1 To Groups Do
  Begin
    Aborted := Aborted Or DrawAborted;
    If Aborted Then
      Break;

    SetFileGroup (i);
    UpdateFGroupMacro;

    For j := 1 To fAreasGroup Do
    Begin
      Aborted := Aborted Or DrawAborted;
      If Aborted Then
        Break;

      SetFileArea (j);

      If (FileArea. List_Security > R. Security) Or
         Not FlagsValid (R. Flags, FileArea. List_Flags) Or
         PassedAreasColl^. Search (Pointer (PhysFileArea), idx)
      Then
        Continue;

      PassedAreasColl^. AtInsert (idx, Pointer (PhysFileArea));
      If Not FileArea. ScanNewFiles And (Mode = fsDate) Then
        Continue;

      UpdateFAreaMacro;
      ComWrite (EmuRelColor (9 + Random (6)) + lang (laScanFileAreas),
        eoMacro + eoCodes);
      ComWrite (EmuClrEOL + #13, 0);
      Clock2;

      Case Mode Of
        fsDate : Aborted := Not DispFilesBBS (AllFilesMask, DateFrom, True, False, tNf);
        fsName : Aborted := Not DispFilesBBS (WildCard, '', True, False, tNf);
        fsDesc : Aborted := Not DispMatchedDesc (Masks, tNf);
      End;

      If tNf Then
      Begin
        Found := True;
        Inc (MoreLines);
      End;
    End;
  End;

  ComWrite (#13, 0);
  ComWrite (EmuClrEOL, 0);

  If Not Found Then
    Message (lang (laNoFilesFound))
  Else
    If Not Aborted Then
      While Tag Do;

  DoneDispFiles;
  Dispose (PassedAreasColl, Done);
  If Mode = fsDesc Then
    Dispose (Masks, Done);

  SetFileGroup (OldGroup);
  SetFileArea (OldArea);
  UpdateFGroupMacro;
  UpdateFAreaMacro;
  CloseFileGroups;
  CloseFileAreas;
End;

Procedure Msg2SysOp (Const Subj, Tpl: String);
Var
  TmpMA : tMsgArea;
  B     : Boolean;

Begin
  TmpMA := MsgArea;
  OpenMsgAreas;
  B := ReadMsgArea (MsgArea, Str2Long (Cnf. ToSysOpArea));
  CloseMsgAreas;

  If B Then
  Begin
    UpdateUserMacro;

    If OpenMessageArea (True, True) Then
    Begin
      If PostMsg (pmNew, Cnf. SysOp, Subj, '', '', 0, Tpl) Then
        LogWrite ('+', sm (smlPosting) + ZeroMsg (MsgArea. Name, True));

      CloseMessageBase (Msg);
    End;
  End;

  MsgArea := TmpMA;
End;

Procedure TypeFile;
Var
  F       : Text;
  S       : String;
  FileBuf : FileBufArr;

Begin
  If (FileArea. DL_Security > R. Security) Or
     (Not FlagsValid (R. Flags, FileArea. DL_Flags)) Then
  Begin
    ComWriteLn ('|' + lang (laSecurityLow), eoCodes + eoMacro);
    Exit;
  End;

  Assign (F, FileName);
  SetTextBuf (F, FileBuf, FileBufSize);
  ReSet (F);

  If IOResult = 0 Then
  Begin
    InitMore (0);
    ComWrite (EmuRelColor ($07), 0);

    While Not EoF (F) Do
    Begin
      ReadLn (F, S);
      S := TrimTrail (PlaceSubStr (S, #9, ReplaceTabSpaces));
      ComWriteLn (S, 0);
      If Not More Then
        Break;
    End;

    Close (F);
  End;
End;

Procedure PostFile;

  Procedure Add2EchoMail (Path: PathStr);
  Var
    F     : Text;
    fName : PathStr;

  Begin
    Case MsgArea. BaseType Of
         mbfJam : Case MsgArea. AreaType Of
                    btNetmail  : fName := 'netmail.jam';
                    btEchomail : fName := 'echomail.jam';
                  End;
      mbfSquish : fName := 'echotoss.log';
    Else
      Exit;
    End;

    If Cnf. EchoLog <> '' Then
      Assign (F, Cnf. EchoLog + fName)
    Else
      Assign (F, AddBackSlash (JustPathName (Path)) + fName);
    Append (F);
    If IOResult <> 0 Then
      ReWrite (F);

    If MsgArea. BaseType = mbfSquish Then WriteLn (F, MsgArea. Name)
                                     Else WriteLn (F, Path);
    Close (F);
  End;

  Function GetSysTZUTC (Var TZUTC: String): Boolean;
  Begin
  {$IFDEF DPMI32}
    GetSysTZUTC := False;
  {$ELSE}
  {$IFDEF MSDOS}
    If NTVDMInitOk Then
    Begin
      NTVDMGetSystemTZUTC (@TZUTC, 6);
      GetSysTZUTC := True;
    End Else
      GetSysTZUTC := False;
  {$ELSE}
  {$IFDEF WIN32}
    TZUTC := GetSystemTZUTC;
    GetSysTZUTC := True;
  {$ELSE}
    GetSysTZUTC := False;
  {$ENDIF}
  {$ENDIF}
  {$ENDIF}
  End;

Var
  F       : Text;
  oArea   : tMsgArea;
  FileBuf : FileBufArr;
  S       : String;

Label
  EndOfProc;

Begin
  If Options And pfAutoOpen <> 0 Then
  Begin
    oArea := MsgArea;
    OpenMsgAreas;
    ReadMsgArea (MsgArea, AbsoluteNum);
    CloseMsgAreas;
  End;

  If (MsgArea. WriteSec > R. Security) Or
     Not FlagsValid (R. Flags, MsgArea. WriteFlags)
  Then
    Goto EndOfProc;

  Assign (F, FileName);
  SetTextBuf (F, FileBuf, FileBufSize);
  ReSet (F);

  If IOResult <> 0 Then
    Goto EndOfProc;

  If Options And pfAutoOpen <> 0 Then
    If Not OpenMessageArea (False, True) Then
      Goto EndOfProc;

  If PostMode = pmEdit Then
  Begin
    Msg^. OpenMessage;
    Msg^. GetFromAddress (OrigAddr);
    Msg^. SetFromAddress (OrigAddr, True);
    Msg^. ResetDateTime;
    Msg^. SetTextPos (Msg^. AfterLastKludge);
    Msg^. TruncateText;
  End Else
  Begin
    Msg^. CreateNewMessage;

    If PostMode = pmReply Then
    Begin
      Msg^. SetReplyTo (Msg^. RelativeToAbsolute (ReplyToNumRelative));
      If AReply <> '' Then
        Msg^. SetKludge (#1'REPLY:', #1'REPLY: ' + AReply);
    End;

    { This is merely a workaround, but proper solution requires
      changes to LNG files, which would make sense only if
      Tornado would be used by non-Russian systems. }
    If R. Lang = 'RUSSIAN' Then
      Msg^. SetKludge (#1'CHRS:', #1'CHRS: CP866 2');

    If GetSysTZUTC (S) Then
      Msg^. SetKludge (#1'TZUTC:', #1'TZUTC: ' + S);

    If Options And pfUpgrader <> 0 Then
      Msg^. SetKludge (#1'PID:', #1'PID: ' + NameVer + ' upgrade manager');

    Msg^. SetFrom (FromName);
    Msg^. SetTo (ToName);
    Msg^. SetSubject (mSubj);
    Msg^. SetAttribute (maLocal, True);
    Msg^. SetAttribute (maPrivate, (MsgArea. AreaType = btNetmail)
      Or (Options And pfPrivate <> 0));

    If Options And pfUseDefaultAddr <> 0 Then
    Begin
      OrigAddr := MsgArea. Address;
      DestAddr := MsgArea. Address;
    End;

    Msg^. SetFromAndToAddress (OrigAddr, DestAddr, True);
    Msg^. SetTextPos (Msg^. GetTextSize);

    If (MsgArea. AreaType = btNetmail) And
      (PostMode = pmReply) And
      (MsgArea. GateWay <> '') And
      (eMail <> '') And
      (AddressToStrEx (DestAddr) = MsgArea. GateWay) Then
    Begin
      S := PlaceSubStr (ToName, '"', '''');
      Msg^. PutString ('To: "' + S + '" <' + eMail + '>');
      Msg^. PutString ('');
    End;
  End;

  While Not EoF (F) Do
  Begin
    ReadLn (F, S);
    Msg^. PutString (S);
  End;

  Close (F);

  Msg^. PutString ('--- ' + NameVer);
  Msg^. PutOrigin (OrigAddr, MsgArea. Origin);

  Msg^. WriteMessage;
  Msg^. CloseMessage;

  If Options And pfAutoOpen <> 0 Then
    CloseMessageBase (Msg);

  If MsgArea. AreaType <> btLocal Then
    Add2EchoMail (MsgArea. BasePath);

  MakeFlag (MailFlag);

EndOfProc:
  If Options And pfAutoOpen <> 0 Then
    MsgArea := oArea;
End;

Procedure PrePostMsg (Const Param, Tpl: String);
Var
  MA : tMsgArea;

Begin
  MA := MsgArea;

  If Param <> '' Then
  Begin
    If Not OpenMsgAreas Then
      Exit;

    ReadMsgArea (MsgArea, Str2Long (Param));
    CloseMsgAreas;
  End;

  If MsgArea. Name <> '' Then
  Begin
    If OpenMessageArea (True, True) Then
      If PostMsg (pmNew, '', '', '', '', 0, Tpl) Then
        LogWrite ('+', sm (smlPosting) + ZeroMsg (MsgArea. Name, True));
    CloseMessageBase (Msg);
  End
  Else
    LogWrite ('!', sm (smErrorMsgAreaDef) + Param);

  MsgArea := MA;
End;

End.
