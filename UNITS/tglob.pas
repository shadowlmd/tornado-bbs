{$IFDEF MSDOS}
  {$IFNDEF DPMI32}
    {$F+}
  {$IFNDEF DPMI}
    {$DEFINE RealMode}
  {$ENDIF}
  {$ENDIF}
{$ENDIF}
{$X+,I-}
{&Delphi+}

Unit
  tGlob;

{*********************************************************}
{*                      TGLOB.PAS                        *}
{*                                                       *}
{*  Copyright (c) Konstantin Klyagin, 1998,              *}
{*                exspecially for Tornado BBS System     *}
{*                                                       *}
{*********************************************************}

Interface

Uses
{$IFDEF OS2}
  Os2Base,
  Os2Def,
  Os2PmApi,
  VPutils,
{$ENDIF}

{$IFDEF MSDOS}
{$IFDEF DPMI}
  Dpmi,
{$ELSE}
{$IFNDEF DPMI32}
  Streams,
{$ELSE}
  Dpmi32,
  Dpmi32df,
{$ENDIF}
{$ENDIF}
{$ENDIF}

{$IFDEF WIN32}
  Windows,
{$ENDIF}

  DOS,
  OpInline,
  ApSame,
  ApTimer,
  ApAbsPcl,
  skCommon,
  tMisc,
  Crc,
  Objects,
  TimeTask;

{$I INC\misc.inc}
{$I INC\users.inc}

Const
  ReplaceTabSpaces: String [4] = '    ';

  MTaskers: Array [1..11] Of String [17] =
    ('Microsoft Windows', 'IBM OS/2', 'DESQview', 'TopView', 'PC-MOS/386',
    'Linux DOSEMU', 'IBM OS/2 Warp', 'European MS-DOS', 'DoubleDOS', 'Win95', 'NTVDM');

  ULorDL : Array [TransferModeType] Of String [8] = ('DownLoad', 'UpLoad');
  uNum : LongInt = 0;

  RotNum = 4;
  RotBar : Array [1..RotNum] Of Char = ('|', '/', 'Ä', '\');

  kbUp    = #1;
  kbDown  = #2;
  kbRight = #3;
  kbLeft  = #4;
  kbDel   = #127;
  kbHome  = #5;
  kbEnd   = #6;
  kbPgUp  = #8;
  kbPgDn  = #9;

  trcSysMsgPrefix = #11;

  Open_Access_ReadOnly          = $0000; { ---- ---- ---- -000 }
  Open_Access_WriteOnly         = $0001; { ---- ---- ---- -001 }
  Open_Access_ReadWrite         = $0002; { ---- ---- ---- -010 }
  Open_Share_DenyReadWrite      = $0010; { ---- ---- -001 ---- }
  Open_Share_DenyWrite          = $0020; { ---- ---- -010 ---- }
  Open_Share_DenyRead           = $0030; { ---- ---- -011 ---- }
  Open_Share_DenyNone           = $0040; { ---- ---- -100 ---- }

  Xmodem      = 0;
  XmodemCRC   = 1;
  Xmodem1K    = 2;
  Xmodem1KG   = 3;
  Ymodem      = 4;
  YmodemG     = 5;
  Zmodem      = 6;
  Zmodem8K    = 7;

  {-Options}
  ofSpaceAdd   = $01;
  ofAllowEmpty = $02;
  ofFramed     = $04;
  ofHistory    = $08;
  ofNoCR       = $10;

  {-emu Options}
  eoMacro      = $01;
  eoSlashCode  = $02;
  eoColorCode  = $04;
  eoDisable01  = $08;
  eoNoFlush    = $10;
  eoCodes      = eoSlashCode + eoColorCode;

  {Convenient protocol string constants}
  ProtocolTypeString : Array [XModem..ZModem8K] Of String [10] = (
    'Xmodem', 'XmodemCRC', 'Xmodem1K', 'Xmodem1KG',
    'Ymodem', 'YmodemG', 'Zmodem', 'Zmodem8k');

  mCommand     : String [128] = '';
  LocalOffHook : Boolean = False;
  ManualChat   : Boolean = False;
  ExitNowFlag  : Boolean = False;
  AllChars     : Set Of Char = [#32..#255];
  LettersOnly  : Set Of Char =
    ['A'..'Z', 'a'..'z', '€'..'¯', 'à'..'ï',
     ' ', '.', ',', '*', '_', '-', '''', #240, #241, #247];
  NumbersOnly  : Set Of Char = ['0'..'9'];
  InputAccept  : Set Of Char = [];

{$IFNDEF MSDOS}
  AllFilesMask = '*';
{$ELSE}
  AllFilesMask = '*.*';
{$ENDIF}

{$I INC\ver.inc}

Type
  PMenuItem = ^tMenuItem;
  tMenuItem = Record
    Action   : System. Integer;
    Security : System. Word;
    OptData  : PString;
    Flags    : PString;
    Display  : PString;
    HotKey   : Char;
    AutoExec : Boolean;
  End;

  tMenuHeader = Record
    DisplayFile, Prompt            : String [80];
    WriteHotKeys                   : Boolean;
    StartItem, ArrowHor, ArrowVer,
    StartY, StartX, SelectedColor  : Byte;
  End;

  tFlagType = (ftUserBase, ftSystem, ftLines, ftConference, ftConfList);
  tArr255 = Array [0..255] Of Char;

  PXLATrec = ^TXLATrec;
  TXLATrec = Record
    FileName : String [80];
    Name     : String [100];
    Table    : tArr255;
  End;

{$IFDEF RealMode}
  PEMSStream3 = ^TEMSStream3;
  TEMSStream3 = Object (TEMSStream2)
    Procedure Truncate; Virtual;
  End;

  TBigColl_BufState = (Reading, Writing);

  TBigCollection = Object (TCollection)
    DataStream       : PStream;
    DataBuf          : PString;
    BufStart, BufLen : LongInt;
    BufState         : TBigColl_BufState;
    Usual            : Boolean;

    Constructor Init (AMax, AStep: Integer);
    Procedure InsLine (Const S: String);
    Procedure AtPutLine (Index: Integer; Const S: String);
    Procedure AtInsLine (Index: Integer; Const S: String);
    Procedure FreeItem (Item: Pointer); Virtual;
    Procedure Insert (Item: Pointer); Virtual;
    Procedure AtPut (Index: Integer; Item: Pointer); Virtual;
    Procedure AtInsert (Index: Integer; Item: Pointer); Virtual;
    Function At (Index: Integer): Pointer;
    Procedure FreeAll; Virtual;
    Destructor Done; Virtual;
  End;
{$ELSE}
  TBigCollection = Object (TCollection)
    Procedure InsLine (Const S: String);
    Procedure AtInsLine (Index: Integer; Const S: String);
    Procedure AtPutLine (Index: Integer; Const S: String);
    Procedure FreeItem (Item: Pointer); Virtual;
    Procedure Insert (Item: Pointer); Virtual;
    Procedure AtPut (Index: Integer; Item: Pointer); Virtual;
    Procedure AtInsert (Index: Integer; Item: Pointer); Virtual;
  End;
{$ENDIF}
  PBigCollection = ^TBigCollection;

  PLongIntCollection = ^TLongIntCollection;
  TLongIntCollection = Object (TCollection)
    Procedure FreeItem (Item: Pointer); Virtual;
    Procedure FreeAll; Virtual;
    Function Contains (Item: LongInt): Boolean;
  End;

  PSortedLongIntCollection = ^TSortedLongIntCollection;
  TSortedLongIntCollection = Object (TSortedCollection)
    Function Compare (Key1, Key2: Pointer): Integer; Virtual;
    Procedure FreeItem (Item: Pointer); Virtual;
    Procedure FreeAll; Virtual;
    Function Contains (Item: LongInt): Boolean;
  End;

Const
  HashTable_MaxSize = 16380;
{$IFDEF RealMode}
  HashItemsInBlock = 4;  { 64b }
{$ELSE}
  HashItemsInBlock = 12; { 176b }
{$ENDIF}

Type
  PHashTableItem = ^HashTableItem;
  HashTableItem = Record
    Next     : PHashTableItem;
    Data     : Pointer;
    NameCSum : tChecksum;
  End;

  PHashTableArr = ^HashTableArr;
  HashTableArr = Array [0..HashTable_MaxSize-1] Of PHashTableItem;

  PHashTableBlock = ^HashTableBlock;
  HashTableBlock = Record
    Next  : PHashTableBlock;
    Used  : LongInt;
    Items : Array [0..HashItemsInBlock-1] Of HashTableItem;
  End;

  PHashTable = ^THashTable;
  THashTable = Object
    Table                : PHashTableArr;
    RootBlock, LastBlock : PHashTableBlock;
    TableSize            : Word;

    Constructor Init (TableBits: Word);
    Destructor Done;

    Procedure Clear;

    Procedure Insert (Const Name: String; Data: Pointer);
    Procedure InsertByChecksum (Const CSum: tChecksum; Data: Pointer);
    Function Search (Const Name: String): Pointer;
    Function SearchByChecksum (Const CSum: tChecksum): Pointer;

    Function NewData (P: Pointer): Pointer; Virtual;
    Procedure ReplaceData (Var Old: Pointer; New: Pointer); Virtual;
    Procedure DisposeData (P: Pointer); Virtual;

  Private
    Procedure InitData;
    Procedure DisposeAllData;
  End;

  PNotSortedCollection = ^TNotSortedCollection;
  TNotSortedCollection = Object (TCollection)
    Function Contains (Const S: String): Boolean;
    Procedure FreeItem (Item: Pointer); Virtual;
  End;

  PAsciizCollection = ^TAsciizCollection;
  TAsciizCollection = Object (TCollection)
    Procedure InsItem (Const S: String);
    Procedure FreeItem (Item: Pointer); Virtual;
  End;

  PTagFilesCollection = ^TTagFilesCollection;
  TTagFilesCollection = Object (TSortedCollection)
    Function Compare (Key1, Key2: Pointer): Integer; Virtual;
    Procedure FreeItem (Item: Pointer); Virtual;
  End;

  PMenuItemsCollection = ^TMenuItemsCollection;
  TMenuItemsCollection = Object (TCollection)
    Procedure FreeItem (Item: Pointer); Virtual;
  End;

Const
  ComRead       : Procedure (Var S: String; Max: Integer; Options: Byte) = nil;
  ComReadLn     : Procedure (Var S: String; Max: Integer; Options: Byte) = nil;
  ComMenu       : Function (Const Header, ChoiceStr: String; Items: PNotSortedCollection): Integer = nil;
  SetProtocol   : Procedure (C: Char) = nil;
  GetAnswer     : Function (Const Quest: String; Len, Options: Byte; Default: String): String = nil;
  Query         : Function (Const What: String; IsYes: Boolean; Flags: Byte): Boolean = nil;
  Query_YNQ     : Function (Const What: String; IsYes: Boolean): Char = nil;
  MenuBar       : Function (Const Text: String; Hot: String): Byte = nil;
  SmartLine     : Procedure = nil;
  ComSaveScreen : Procedure = nil;
  ComRestoreScreen : Procedure (WaitForKey: Boolean) = nil;
  ComWrite      : Procedure (Const Str: String; Options: Byte) = nil;
  ComWriteLn    : Procedure (Const Str: String; Options: Byte) = nil;
  Message       : Procedure (Const Mess: String) = nil;
  Cls           : Procedure = nil;
  EmuColor      : Function (Attr: Byte): String = nil;
  EmuRelColor   : Function (Attr: Byte): String = nil;
  EmuGotoXY     : Function (X, Y: Integer): String = nil;
  EmuCursorLeft : Function (LX: Integer): String = nil;
  EmuCursorRight: Function (LX: Integer): String = nil;
  EmuCursorUp   : Function (LY: Integer): String = nil;
  EmuCursorDown : Function (LY: Integer): String = nil;
  EmuCls        : Function: String = nil;
  EmuClrEoL     : Function: String = nil;
  EmuDispFile   : Function (F: String): Boolean = nil;
  mL_Init       : Function (LineNo: Byte): Boolean = nil;
  mL_MsgWaiting : Function: Boolean = nil;
  mL_GetMsg     : Function (Var Msg: tMsg; MsgType: tMsgType): Boolean = nil;
  mL_GetMesgStat: Function (LineNo: Byte): Boolean = nil;
  mL_SendMsg    : Procedure  (LineNo: Byte; MsgText: String; MsgType: tMsgType) = nil;
  mL_WhoDo      : Procedure (ListMode: tListMode) = nil;
  mL_ChooseLine : Function: Byte = nil;
  mL_RealTimeConference : Procedure = nil;
  mL_DisableMsg : Procedure = nil;
  mL_EnableMsg  : Procedure = nil;
  mL_LineMsg    : Procedure = nil;
  mL_Done       : Procedure = nil;
  NormExit      : Procedure = nil;
  SetSecurity   : Procedure = nil;
  {Gluck         : Procedure = nil;}
  SuspendTime   : Procedure = nil;
  UnSuspendTime : Procedure = nil;
  GoTelnet      : Procedure (Host: String) = nil;
  GetConnectSpeed : Function: LongInt = nil;
  DosShell      : Function (Prg: String; CommandCom, LogErrorLevel: Boolean): Word = nil;
  TranslateExecParams : Function (Const S: String): String = nil;
  UserInfo      : Procedure (Name: String) = nil;
  ExecScript    : Function (fName: String): Boolean = nil;
  ExecRexx      : Function (fName: PathStr): Boolean = nil;
  News          : Procedure (Logon: Boolean) = nil;
  DrawAborted   : Function: Boolean = nil;

  MsgBaseLetter : Array [mbfJam..mbfSquish] Of Char = ('J', 'F', 'S');

  qwkListChanged   : Boolean = False;
  InLightBarMenu   : Boolean = False;
  UsedMinus        : LongInt = 0;
  CurrentDebugFile : String [28] = '';

  SavedExitProc : Pointer = nil;

Type
  tCoord = Record
    X1, X2, Y1, Y2 : Byte;
  End;

  tBinaryHdr = Record
    Version : String [30];
    ExeName : String [80];
  End;

  TrueInt   = System. Integer;
  TrueWord  = System. Word;
  PTrueInt  = ^TrueInt;
  PTrueWord = ^TrueWord;
  PLongInt  = ^LongInt;
  PPointer  = ^Pointer;

Var
  WaitReturn       : Procedure;

  WXG, WYG, RegLet : Byte;
  Cnf              : ConfigRecord;

  PhysFileArea,
  PhysMsgArea,
  fAreasGroup,    { File Areas in Group           }
  mAreasGroup,    { Msg Areas in Group            }
  mAreasAmount,   { Msg Areas Total Amount        }
  fAreasAmount,   { File Areas Total Amount       }
  fGroupsAmount,  { File Groups Total Amount      }
  ffGroupsAmount, { Groups Amount (security used) }
  mGroupsAmount,  { Msg Groups Total Amount       }
  mmGroupsAmount  { Groups Count (security used)  } : Word;

  Local, NetMailEntered, EchoMailEntered,
  WantsChat, InChat, FromKeyBoard,
  StatusBar, StatusBarEnable, InConference,
  TimeCount, InShell, AutoDL,
  Registering, InAction, EnteringPass,
{$IFDEF OS2}
  NeedThreadClose, ThreadLocked,
{$ENDIF}
  StopCodeEnable, FuncKey                       : Boolean;

  Port                                          : PortRecPtr;
  Prot                                          : ProtocolRecPtr;
  R                                             : tUser;
  LC                                            : tLastCaller;
  Sys                                           : SystemType;
  EnterTime, enTime, Ent, reqBaud, LastConfKey,
  SizeOfAll                                     : LongInt;
  Lim                                           : LimitRec;
  trMode                                        : TransferModeType;
  AutoMode, Answer, CtrlCState, Exeption,
  DnsFinished, Manual                           : Boolean;
{$IFDEF RealMode}
  OvrName                                       : PathStr;
{$ENDIF}
  oDir, ConfigName, TorDir                      : PathStr;
  FileGroup                                     : tFileGroup;
  FileArea                                      : tFileArea;
  MsgGroup                                      : tMsgGroup;
  MsgArea                                       : tMsgArea;
  KeyBuffer                                     : String;
  EMSI                                          : tEMSI;
  ProtocolDef                                   : tProtocolConfig;
  UpFiles, Reps, ReadHistory, Language,
  ParamNames                                    : PBigCollection;
  F2Transfer                                    : PTagFilesCollection;
  Menus, Screens, XLATs                         : PNotSortedCollection;
  CLamps                                        : Array [Boolean] Of ^Byte;

Const
  InitError      : Boolean = True;
  Suxx           : Boolean = False;
  {SaveTitle      : String [80] = '';}
  exCommand      : Boolean = True;
  exDirect       : Boolean = False;
  Kill_Line_Flag : Boolean = True;
  RunScript      : PathStr = '';
  RunRexx        : PathStr = '';
  TimeSusp       : Boolean = False;

Const
  FileBufSize = 1024;

Type
  FileBufArr = Array [0..FileBufSize-1] Of Char;
  PFileBufArr = ^FileBufArr;

Function NewFileItem (Const S: tFileItem): PFileItem;
Procedure DisposeFileItem (P: PFileItem);

Function NewTagFile (Const S: TTagFileRec): PTagFileRec;
Procedure DisposeTagFile (P: PTagFileRec);
Procedure InsertInTagList (Var FileRec: TTagFileRec);
Procedure ClearTagList;

Function NewMenuItem (Const M: tMenuItem): PMenuItem;
Procedure DisposeMenuItem (P: PMenuItem);

Function GetStr (P: PString): String;
Procedure ChangePString (Var P: PString; Const S: String);

Function TestMasks (Masks: PAsciizCollection; MatchedList: PLongIntCollection; Const S: String): Boolean;

Function lang (Index: Integer): String;

{* Flags manipulating functions *}

Function FullFlagName (Const Name: String): String;
Procedure MakeFlag (Const Name: PathStr);
Function ChkFlag (Const Name: PathStr): Boolean;
Procedure EraseFlag (Const Name: PathStr);
Procedure SetFlag (Const Name: PathStr);
Procedure DelFlag (Const Name: PathStr);
Function CheckFlag (Const Name: PathStr): Boolean;
Procedure Wait4Flag (Const Name: PathStr);
Procedure WaitAndSetFlag (Const Name: PathStr);
Function LineUsed: Boolean;
Function LineExt: String;
Procedure ClearInputHistory;

Function BinaryHdrValid (Const Hdr: tBinaryHdr): Boolean;

Procedure SetDebugFile (Const S: String);
Procedure SetTitle (Title: String);
Function GetTitle: String;
Procedure FreeTitleMem;
Function keyScrollLock: Boolean;

{$IFDEF OS2}
Function GetFileDateCreation (FN: PathStr): String;
{$ENDIF}

Procedure InitDeviceTable;
Function IsDevice (Const DevName: String): Boolean;

Const
  RunFlag   = 'tor_run';
  PageFlag  = 'tor_ncht';
  MailFlag  = 'tor_mail';
  FileFlag  = 'tor_file';
  OrigHdr   : tBinaryHdr = (Version: ''; ExeName: '');
  EmuName   : Array [tEmulation] Of String [6] = ('ANSI', 'TTY', 'AVATAR');
  BbsLine   : Byte = 0;
  Copyright = '(c) Konstantin Klyagin, 1995-99';

Implementation

Uses
{$IFDEF MSDOS}
{$IFNDEF DPMI32}
  NTVDMSvc,
{$ENDIF}
{$ENDIF}
  Strings;

Const
  NullStr : String [1] = '';

{$IFDEF MSDOS}
Var
  ChangeTitle : Boolean;
{$IFDEF DPMI}
  TitlePMSel  : Word;
  TitleRMSeg  : Word;
{$ENDIF}
{$IFDEF DPMI32}
  TitleSeg    : SmallWord;
{$ENDIF}
{$ENDIF}
{$IFDEF WIN32}
Var
  PTitle : PChar;
{$ENDIF}

{$IFDEF RealMode}

Procedure TEMSStream3. Truncate;
Begin
  Inherited Truncate;
  EMSCurPage := $FFFF;
End;

Constructor TBigCollection. Init (AMax, AStep: Integer);
Begin
  Inherited Init (AMax, AStep);
  Usual := True;

  DataStream := New (PXMSStream, Init (1024, 1024));
  If DataStream <> Nil Then
    If DataStream^. Status = stOk Then
      Usual := False
    Else
      Dispose (DataStream, Done);

  If Usual Then
  Begin
    DataStream := New (PEMSStream3, Init (1024, 1024));
    If DataStream <> Nil Then
      If DataStream^. Status = stOk Then
        Usual := False
      Else
        Dispose (DataStream, Done);
  End;

  If Not Usual Then
  Begin
    New (DataBuf);
    BufStart := 0;
    BufLen := 0;
    BufState := Writing;
  End;
End;

Procedure TBigCollection. FreeItem (Item: Pointer);
Begin
  If Usual Then
    DisposeStr (Item);
End;

Procedure TBigCollection. FreeAll;
Begin
  If Not Usual Then
  Begin
    DataStream^. Seek (0);
    DataStream^. Truncate;
    BufStart := 0;
    BufLen := 0;
    BufState := Writing;
    DeleteAll;
  End
  Else
    Inherited FreeAll;
End;

Destructor TBigCollection. Done;
Begin
  If Not Usual Then
  Begin
    Dispose (DataStream, Done);
    Dispose (DataBuf);
    DeleteAll;
  End
  Else
    Inherited FreeAll;

  Inherited Done;
End;

Procedure TBigCollection. InsLine (Const S: String);
Var
  L   : LongInt;
  Len : Integer;

Begin
  If Not Usual Then
  Begin
    If BufState <> Writing Then
    Begin
      BufState := Writing;
      BufStart := DataStream^. GetSize;
      BufLen := 0;
    End;

    L := BufStart + BufLen;
    Len := Length (S) + 1;

    If BufLen + Len > SizeOf (DataBuf^) Then
    Begin
      DataStream^. Seek (BufStart);
      DataStream^. Write (DataBuf^, BufLen);
      BufStart := DataStream^. GetSize;
      BufLen := 0;
    End;

    Move (S, DataBuf^ [BufLen], Len);
    Inc (BufLen, Len);
    Inherited Insert (Pointer (L));
  End
  Else
    Inherited Insert (NewStr (S));
End;

Procedure TBigCollection. AtInsLine (Index: Integer; Const S: String);
Var
  L   : LongInt;
  Len : Integer;

Begin
  If Not Usual Then
  Begin
    If BufState <> Writing Then
    Begin
      BufState := Writing;
      BufStart := DataStream^. GetSize;
      BufLen := 0;
    End;

    L := BufStart + BufLen;
    Len := Length (S) + 1;

    If BufLen + Len > SizeOf (DataBuf^) Then
    Begin
      DataStream^. Seek (BufStart);
      DataStream^. Write (DataBuf^, BufLen);
      BufStart := DataStream^. GetSize;
      BufLen := 0;
    End;

    Move (S, DataBuf^ [BufLen], Len);
    Inc (BufLen, Len);
    Inherited AtInsert (Index, Pointer (L));
  End
  Else
    Inherited AtInsert (Index, NewStr (S));
End;

Function TBigCollection. At (Index: Integer): Pointer;
Var
  L, BufPos : LongInt;

Begin
  If Not Usual Then
  Begin
    If BufState <> Reading Then
    Begin
      BufState := Reading;
      DataStream^. Seek (BufStart);
      DataStream^. Write (DataBuf^, BufLen);
    End;

    L := LongInt (Inherited At (Index));
    BufPos := L - BufStart;
    If (BufPos >= 0) And (BufPos < BufLen) Then
      If BufPos + Ord (DataBuf^ [BufPos]) < BufLen Then
      Begin
        At := @DataBuf^ [BufPos];
        Exit;
      End;

    BufStart := L;
    BufLen := DataStream^. GetSize - BufStart;
    If BufLen > SizeOf (DataBuf^) Then
      BufLen := SizeOf (DataBuf^);

    DataStream^. Seek (BufStart);
    DataStream^. Read (DataBuf^, BufLen);
    At := DataBuf;
  End Else
  Begin
    L := LongInt (Inherited At (Index));
    If L <> 0 Then At := Pointer (L)
              Else At := @NullStr;
  End;
End;

{$ELSE}

Procedure TBigCollection. InsLine (Const S: String);
Begin
  Inherited Insert (NewStr (S));
End;

Procedure TBigCollection. AtInsLine (Index: Integer; Const S: String);
Begin
  Inherited AtInsert (Index, NewStr (S));
End;

Procedure TBigCollection. FreeItem (Item: Pointer);
Begin
  DisposeStr (Item);
End;

{$ENDIF}

Procedure TBigCollection. AtPutLine (Index: Integer; Const S: String);
Begin
  AtFree (Index);
  AtInsLine (Index, S);
End;

Procedure TBigCollection. Insert (Item: Pointer);
Begin
  RunError (200);
End;

Procedure TBigCollection. AtInsert (Index: Integer; Item: Pointer);
Begin
  RunError (200);
End;

Procedure TBigCollection. AtPut (Index: Integer; Item: Pointer);
Begin
  RunError (200);
End;

Procedure TNotSortedCollection. FreeItem (Item: Pointer);
Begin
  DisposeStr (Item);
End;

Function TNotSortedCollection. Contains (Const S: String): Boolean;
Const
  NPtr : PString = Nil;

  Function Matches (P: PString): Boolean;
  Begin
    Matches := P^ = NPtr^;
  End;

Begin
  NPtr := @S;
  Contains := FirstThat (@Matches) <> Nil;
End;

{ --- TAsciizCollection --- }

Type
  PAsciizArr = ^TAsciizArr;
  TAsciizArr = Array [0..255] Of Byte;

Procedure TAsciizCollection. InsItem (Const S: String);
Var
  P   : PAsciizArr;
  Len : Integer;

Begin
  Len := Length (S);
  GetMem (P, Len + 1);
  Move (S [1], P^, Len);
  P^ [Len] := 0;
  Insert (P);
End;

Procedure TAsciizCollection. FreeItem (Item: Pointer);
Var
  P   : ^Byte;
  Len : Integer;

Begin
  P := Item;
  Len := 1;

  While P^ <> 0 Do
  Begin
    Inc (Len);
    Inc (P);
  End;

  FreeMem (Item, Len);
End;

Function TTagFilesCollection. Compare (Key1, Key2: Pointer): Integer;
Var
  K1     : PTagFileRec Absolute Key1;
  K2     : PTagFileRec Absolute Key2;
{$IFNDEF VirtualPascal}
  Result : Integer;
{$ENDIF}

Begin
  Result := K1^. GroupNum - K2^. GroupNum;
  If Result = 0 Then
  Begin
    Result := K1^. AreaNum - K2^. AreaNum;
    If Result = 0 Then
      Result := StrCompare (UpString (Trim (K1^. PathName^)),
                            UpString (Trim (K2^. PathName^)));
  End;
{$IFNDEF VirtualPascal}
  Compare := Result;
{$ENDIF}
End;

Procedure TTagFilesCollection. FreeItem (Item: Pointer);
Begin
  DisposeTagFile (Item);
End;

Procedure TMenuItemsCollection. FreeItem (Item: Pointer);
Begin
  DisposeMenuItem (Item);
End;

{ --- TLongIntCollection --- }

Procedure TLongIntCollection. FreeItem;
Begin
End;

Procedure TLongIntCollection. FreeAll;
Begin
  Count := 0;
End;

Function TLongIntCollection. Contains;
Var
  Number : LongInt;

  Function Matches (N: LongInt): Boolean;
  Begin
    Matches := N = Number;
  End;

Begin
  Number := Item;
  Contains := FirstThat (@Matches) <> Nil;
End;

{ --- TSortedLongIntCollection --- }

Function TSortedLongIntCollection. Compare (Key1, Key2: Pointer): Integer;
{$IFNDEF VirtualPascal}
Var
  Result : LongInt;
{$ENDIF}

Begin
  Result := LongInt (Key1) - LongInt (Key2);

{$IFNDEF VirtualPascal}
  If Result < 0 Then
    Compare := -1
  Else
    If Result > 0 Then
      Compare := 1
    Else
      Compare := 0;
{$ENDIF}
End;

Function TSortedLongIntCollection. Contains (Item: LongInt): Boolean;
Var
  i : Integer;

Begin
  Contains := Search (Pointer (Item), i);
End;

Procedure TSortedLongIntCollection. FreeItem (Item: Pointer);
Begin
End;

Procedure TSortedLongIntCollection. FreeAll;
Begin
  Count := 0;
End;

{ --- THashTable --- }

Constructor THashTable. Init (TableBits: Word);
Begin
  TableSize := 1 Shl TableBits;
  GetMem (Table, TableSize * SizeOf (PHashTableItem));
  InitData;
End;

Destructor THashTable. Done;
Begin
  DisposeAllData;
  FreeMem (Table, TableSize * SizeOf (PHashTableItem));
End;

Procedure THashTable. InitData;
Begin
  FillChar (Table^, TableSize * SizeOf (PHashTableItem), 0);
  New (RootBlock);
  LastBlock := RootBlock;
  LastBlock^. Next := Nil;
  LastBlock^. Used := 0;
End;

Procedure THashTable. DisposeAllData;
Var
  Block : PHashTableBlock;
  i     : Integer;

Begin
  Block := RootBlock;

  While Block <> Nil Do
  Begin
    For i := 0 To Block^. Used-1 Do
      DisposeData (Block^. Items [i]. Data);

    RootBlock := Block;
    Block := Block^. Next;
    Dispose (RootBlock);
  End;
End;

Procedure THashTable. Clear;
Begin
  DisposeAllData;
  InitData;
End;

Procedure THashTable. Insert (Const Name: String; Data: Pointer);
Var
  CSum : tChecksum;

Begin
  GetChecksum (Name, CSum);
  InsertByChecksum (CSum, Data);
End;

Procedure THashTable. InsertByChecksum (Const CSum: tChecksum; Data: Pointer);
Var
  PH  : PHashTableItem;
  PPH : ^PHashTableItem;

Begin
  PPH := @Table^ [CSum. CRC And (TableSize - 1)];
  PH := PPH^;

  While PH <> Nil Do
  Begin
    If (CSum. CRC = PH^. NameCSum. CRC) And
       (CSum. Sum = PH^. NameCSum. Sum) Then
    Begin
      ReplaceData (PH^. Data, Data);
      Exit;
    End;

    PH := PH^. Next;
  End;

  If LastBlock^. Used = HashItemsInBlock Then
  Begin
    New (LastBlock^. Next);
    LastBlock := LastBlock^. Next;
    LastBlock^. Next := Nil;
    LastBlock^. Used := 0;
  End;

  PH := @LastBlock^. Items [LastBlock^. Used];
  Inc (LastBlock^. Used);

  PH^. Next := PPH^;
  PH^. Data := NewData (Data);
  PH^. NameCSum := CSum;
  PPH^ := PH;
End;

Function THashTable. Search (Const Name: String): Pointer;
Var
  PH   : PHashTableItem;
  CSum : tChecksum;

Begin
  GetChecksum (Name, CSum);
  PH := Table^ [CSum. CRC And (TableSize - 1)];

  While PH <> Nil Do
  Begin
    If (CSum. CRC = PH^. NameCSum. CRC) And
       (CSum. Sum = PH^. NameCSum. Sum) Then
    Begin
      Search := PH^. Data;
      Exit;
    End;

    PH := PH^. Next;
  End;

  Search := Nil;
End;

Function THashTable. SearchByChecksum (Const CSum: tChecksum): Pointer;
Var
  PH : PHashTableItem;

Begin
  PH := Table^ [CSum. CRC And (TableSize - 1)];

  While PH <> Nil Do
  Begin
    If (CSum. CRC = PH^. NameCSum. CRC) And
       (CSum. Sum = PH^. NameCSum. Sum) Then
    Begin
      SearchByChecksum := PH^. Data;
      Exit;
    End;

    PH := PH^. Next;
  End;

  SearchByChecksum := Nil;
End;

Function THashTable. NewData (P: Pointer): Pointer;
Begin
  NewData := P;
End;

Procedure THashTable. ReplaceData (Var Old: Pointer; New: Pointer);
Begin
End;

Procedure THashTable. DisposeData (P: Pointer);
Begin
End;

Function NewFileItem (Const S: tFileItem): PFileItem;
Var
  F : PFileItem;

Begin
  New (F);
  F^ := S;
  NewFileItem := F;
End;

Procedure DisposeFileItem (P: PFileItem);
Begin
  If P <> Nil Then
  Begin
    DisposeStr (P^. FileName);
    Dispose (P);
  End;
End;

Function NewMenuItem (Const M: tMenuItem): PMenuItem;
Var
  P : PMenuItem;

Begin
  New (P);
  P^ := M;
  NewMenuItem := P;
End;

Procedure DisposeMenuItem (P: PMenuItem);
Begin
  If P <> Nil Then
  Begin
    DisposeStr (P^. OptData);
    DisposeStr (P^. Flags);
    DisposeStr (P^. Display);
    Dispose (P);
  End;
End;

Function NewTagFile (Const S: TTagFileRec): PTagFileRec;
Var
  P : PTagFileRec;

Begin
  New (P);
  P^ := S;
  NewTagFile := P;
End;

Procedure DisposeTagFile (P: PTagFileRec);
Begin
  If P <> Nil Then
  Begin
    DisposeStr (P^. PathName);
    DisposeStr (P^. FromName);
    Dispose (P);
  End;
End;

Procedure InsertInTagList (Var FileRec: TTagFileRec);
Var
  i : Integer;

Begin
  If Not F2Transfer^. Search (@FileRec, i) Then
  Begin
    F2Transfer^. AtInsert (i, NewTagFile (FileRec));
    Inc (SizeOfAll, FileRec. Size);
  End Else
  Begin
    DisposeStr (FileRec. PathName);
    DisposeStr (FileRec. FromName);
    FileRec. PathName := Nil;
    FileRec. FromName := Nil;
  End;
End;

Procedure ClearTagList;
Begin
  F2Transfer^. FreeAll;
  SizeOfAll := 0;
End;

Function GetStr (P: PString): String;
Begin
  If P <> Nil Then GetStr := P^
              Else GetStr := '';
End;

Procedure ChangePString (Var P: PString; Const S: String);
Begin
  If P <> Nil Then
  Begin
    If Length (P^) = Length (S) Then
    Begin
      P^ := S;
      Exit;
    End;

    FreeMem (P, Length (P^) + 1);
  End;

  GetMem (P, Length (S) + 1);
  P^ := S;
End;

Function TestMasks (Masks: PAsciizCollection; MatchedList: PLongIntCollection; Const S: String): Boolean;
Var
  P : PAsciizArr;
  i : LongInt;
  A : TAsciizArr;

Begin
  i := Length (S);
  Move (S [1], A, i);
  A [i] := 0;

  For i := 0 To Masks^. Count-1 Do
    If MatchedList^. IndexOf (Pointer (I)) = -1 Then
    Begin
      P := Masks^. At (i);
      If MaskMatch (A, P^) Then
        MatchedList^. Insert (Pointer (I));
    End;

  TestMasks := Masks^. Count = MatchedList^. Count;
End;

Function lang (Index: Integer): String;
Begin
  If Index < Language^. Count Then
    lang := PString (Language^. At (Index))^
  Else
    lang := '';
End;

Function keyScrollLock: Boolean;
{$IFDEF DPMI}
Type
  PWord = ^Word;
{$ENDIF}

{$IFDEF OS2}
Var
  Key : KbdInfo;
{$ENDIF}

Begin
{$IFDEF RealMode}
  keyScrollLock := FlagIsSet (Mem [0000:$0417], 16);
{$ENDIF}

{$IFDEF DPMI}
  keyScrollLock := FlagIsSet (PWord (Ptr (Seg0040, $17))^, 16);
{$ENDIF}

{$IFDEF DPMI32}
  keyScrollLock := FlagIsSet (Mem [Seg0040 + $17], 16);
{$ENDIF}

{$IFDEF OS2}
  Key. CB := SizeOf (KbdInfo);
  Key. fsMask := keyboard_Shift_report;
  KbdGetStatus (Key, 0);
  keyScrollLock :=  FlagIsSet (Key. fsState, 16);
{$ENDIF}

{$IFDEF WIN32}
  keyScrollLock := False;
{$ENDIF}
End;

Function LineExt: String;
Var
  S : String [3];

Begin
  Str (BbsLine, S);
  LineExt := '.' + Copy ('bin', 1, 3 - Length (S)) + S;
End;

Function FullFlagName (Const Name: String): String;
Begin
  FullFlagName := Cnf. FlagsDir + Name + '.' + LeftPadCh (Long2Str (BbsLine),
    '0', 3);
End;

Procedure MakeFlag;
Var
  Flag : Text;

Begin
  Assign (Flag, FullFlagName (Name));
  ReWrite (Flag);
  Close (Flag);
  If IOResult <> 0 Then;
End;

Function ChkFlag;
Var
  Flag : Text;

Begin
  Assign (Flag, FullFlagName (Name));
  ReSet (Flag);
  If IOResult = 0 Then
  Begin
    Close (Flag);
    ChkFlag := True;
  End
  Else
    ChkFlag := False;
End;

Procedure EraseFlag;
Begin
  tDeleteFile (FullFlagName (Name));
End;

Procedure SetFlag;
Var
  Flag : Text;

Begin
  If BbsLine > 0 Then
  Begin
    Assign (Flag, Cnf. FlagsDir + Name);
    ReWrite (Flag);
    Close (Flag);
  End;
End;

Function CheckFlag;
Begin
  CheckFlag := (BbsLine > 0) And FileExists (Cnf. FlagsDir + Name);
End;

Procedure DelFlag;
Begin
  If BbsLine > 0 Then
    tDeleteFile (Cnf. FlagsDir + Name);
End;

Procedure Wait4Flag;
Var
  i  : Integer;
  ET : EventTimer;

Begin
  If Not CheckFlag (Name) Then
    Exit;

  For i := 1 To 8 Do
  Begin
    NewTimerSecs (ET, 2);

    Repeat
      TimeSlice;
    Until TimerExpired (ET);

    If Not CheckFlag (Name) Then
      Exit;
  End;

  DelFlag (Name);
End;

Procedure WaitAndSetFlag (Const Name: PathStr);
Begin
  If BbsLine > 0 Then
  Begin
    Wait4Flag (Name);
    SetFlag (Name);
  End;
End;

Procedure ClearInputHistory;
Begin
  ReadHistory^. FreeAll;
End;

Function LineUsed: Boolean;
Var
  Time     : LongInt;
  Cargo    : SysInt;
  DT, cDT  : DateTime;
  F        : File;
  FlagName : PathStr;

Begin
  LineUsed := False;

  FlagName := FullFlagName (RunFlag);
  If FileExists (FlagName) Then
  Begin
    Assign (F, FlagName);
    ReSet (F);
    If IOResult <> 0 Then
    Begin
      LineUsed := True;
      Exit;
    End;

    GetFTime (F, Time);
    Close (F);
    DOS. UnpackTime (Time, DT);
    GetDate (cDT. Year, cDT. Month, cDT. Day, Cargo);
    GetTime (cDT. Hour, cDT. Min, cDT. Sec, Cargo);

    If HoursDiff (DT, cDT) >= Cnf. IgnoreOldFlags Then
      Erase (F)
    Else
      LineUsed := True;
  End;
End;

Function BinaryHdrValid (Const Hdr: tBinaryHdr): Boolean;
Begin
  BinaryHdrValid := (Hdr. Version = OrigHdr. Version) And
                    (Hdr. ExeName = OrigHdr. ExeName);
End;

Procedure SetDebugFile (Const S: String);
Begin
  CurrentDebugFile := LeftPad (NiceFileName (AddBackSlash (CompletePath
    (JustPathName (S))) + JustFileName (S), 28), 28);
End;

Procedure SetTitle (Title: String);
{$IFDEF OS2}
Var
  SwHandle : hSwitch;
  T        : PTib;
  P        : PPib;
  Data     : SwCntrl;

Begin
  If Title = '' Then Title := 'Tornado'
                Else Title := 'Tornado: ' + Title;

  DosGetInfoBlocks (T, P);
  SwHandle := WinQuerySwitchHandle (NULLHANDLE, P^. Pib_ulPid);

  If SwHandle <> NULLHANDLE Then
    If WinQuerySwitchEntry (SwHandle, @Data) = 0 Then
    Begin
      WinQuerySwitchEntry (SwHandle, @Data);
      FillChar (Data. szSwtitle, SizeOf (Data. szSwtitle), #0);

      Move (Title [1], Data. szSwtitle, Length (Title));
      WinChangeSwitchEntry (SwHandle, @Data);
    End;
End;
{$ENDIF}

{$IFDEF WIN32}
Begin
  If Title = '' Then Title := 'Tornado'
                Else Title := 'Tornado: ' + Title;
  SetConsoleTitle (StrPCopy (PTitle, Title));
End;
{$ENDIF}

{$IFDEF MSDOS}
Var
{$IFDEF DPMI}
  R : DPMIRegisters;
{$ELSE}
{$IFDEF DPMI32}
  R : real_mode_call_structure_typ;
{$ELSE}
  R : Registers;
{$ENDIF}
{$ENDIF}

Begin
  If ChangeTitle Then
  Begin
    If Title = '' Then Title := 'Tornado'
                  Else Title := 'Tornado: ' + Title;

  {$IFDEF DPMI}
    StrPCopy (PChar (Ptr (TitlePMSel, 0)), Title);
    If NTVDMInitOk Then
      NTVDMSetSessionTitle (Ptr (TitlePMSel, 0), 255)
    Else
    Begin
      FillChar (R, SizeOf (R), 0);
      R. AX := $168E;
      R. CX := 80;
      R. ES := TitleRMSeg;
      SimulateRealModeInt ($2F, R);
    End;
  {$ELSE}
  {$IFDEF DPMI32}
    StrPCopy (PChar (dosseg_linear (TitleSeg)), Title);
    FillChar (R, SizeOf (R), 0);
    R. AX_ := $168E;
    R. CX_ := 80;
    R. ES_ := TitleSeg;
    intr_RealMode (R, $2F);
  {$ELSE}
    Str2Az (Title, 255, Title);
    If NTVDMInitOk Then
      NTVDMSetSessionTitle (@Title, 255)
    Else
    Begin
      FillChar (R, SizeOf (R), 0);
      R. AX := $168E;
      R. CX := 80;
      R. ES := Seg (Title);
      R. DI := Ofs (Title);
      Intr ($2F, R);
    End;
  {$ENDIF}
  {$ENDIF}
  End;
End;
{$ENDIF}

Function GetTitle: String;
{$IFDEF OS2}
Var
  SwHandle : hSwitch;
  T        : PTib;
  P        : PPib;
  Data     : SwCntrl;
  Title    : String;
  i        : ULong;

Begin
  Title := '';
  DosGetInfoBlocks (T, P);
  SwHandle := WinQuerySwitchHandle (NULLHANDLE, P^. Pib_ulPid);

  If SwHandle <> NULLHANDLE Then
  If WinQuerySwitchEntry (SwHandle, @Data) = 0 Then
  Begin
    WinQuerySwitchEntry (SwHandle, @Data);
    For i := 1 To MaxNameL+3 Do If Data. szSwtitle [i] = #0 Then Break;
    Move (Data. szSwtitle, Title [1], i);
    Title [0] := Chr (i);
  End;

  GetTitle := Title;
End;
{$ENDIF}

{$IFDEF MSDOS}
Var
{$IFDEF DPMI}
  R : DPMIRegisters;
{$ELSE}
{$IFDEF DPMI32}
  R : real_mode_call_structure_typ;
{$ELSE}
  R : Registers;
  S : String;
{$ENDIF}
{$ENDIF}

Begin
  If ChangeTitle Then
  Begin
  {$IFDEF DPMI}
    If NTVDMInitOk Then
      NTVDMGetSessionTitle (Ptr (TitlePMSel, 0), 255)
    Else
    Begin
      FillChar (R, SizeOf (R), 0);
      R. AX := $168E;
      R. CX := 80;
      R. DX := 2;
      R. ES := TitleRMSeg;
      SimulateRealModeInt ($2F, R);
    End;
    GetTitle := Az2Str (PString (Ptr (TitlePMSel, 0))^, 255);
  {$ELSE}
  {$IFDEF DPMI32}
    FillChar (R, SizeOf (R), 0);
    R. AX_ := $168E;
    R. CX_ := 80;
    R. DX_ := 2;
    R. ES_ := TitleSeg;
    intr_RealMode (R, $2F);
    GetTitle := Az2Str (PString (dosseg_linear (TitleSeg))^, 255);
  {$ELSE}
    If NTVDMInitOk Then
      NTVDMGetSessionTitle (@S, 255)
    Else
    Begin
      FillChar (R, SizeOf (R), 0);
      R. AX := $168E;
      R. CX := 80;
      R. DX := 2;
      R. ES := Seg (S);
      R. DI := Ofs (S);
      Intr ($2F, R);
    End;
    GetTitle := Az2Str (S, 255);
  {$ENDIF}
  {$ENDIF}
  End
  Else
    GetTitle := '';
End;
{$ENDIF}

{$IFDEF WIN32}
Begin
  GetConsoleTitle (PTitle, 255);
  GetTitle := StrPas (PTitle);
End;
{$ENDIF}

{$IFDEF OS2}
Function GetFileDateCreation (FN: PathStr): String;
Var
  S     : String;
  rc    : ApiRet;
  Info  : FileStatus3;
  FName : PChar;

Begin
  GetMem (FName, Length (FN) + 1);
  StrPCopy (FName, FN);

  rc := DosQueryPathInfo (FName, 1, Info, SizeOf (Info));
  With Info Do
  S := LeftPadCh (Long2Str ((fdateCreation and mfdDay) shr sfdDay), '0', 2) + '-' +
       LeftPadCh (Long2Str ((fdateCreation and mfdMonth) shr sfdMonth), '0', 2) + '-' +
       Long2Str ((fdateCreation and mfdYear) shr sfdYear+1980) + ' ' +
       LeftPadCh (Long2Str ((fdateCreation and mftHours) shr sfdDay), '0', 2) + ':' +
       LeftPadCh (Long2Str ((fdateCreation and mftMinutes) shr sfdDay), '0', 2);

  GetFileDateCreation := S;

  FreeMem (FName, Length (FN)+1);
End;
{$ENDIF}

{$IFDEF MSDOS}
Function IsWin95DosBox: Boolean;
{$IFNDEF DPMI32}
Var
  R : Registers;

Begin
  FillChar (R, SizeOf (R), 0);
  R. AX := $3001;
  MsDOS (R);
  If (R. Flags And fCarry) = 0 Then
    If R. AL >= 7 Then
    Begin
      R. AX := $4A33;
      Intr ($2F, R);
      IsWin95DosBox := R. AX = 0;
      Exit;
    End;

  IsWin95DosBox := False;
End;
{$ELSE}
Var
  R : real_mode_call_structure_typ;

Begin
  FillChar (R, SizeOf (R), 0);
  R. AX_ := $3001;
  intr_RealMode (R, $21);
  If (R. Flags_ And fCarry) = 0 Then
    If R. AL_ >= 7 Then
    Begin
      R. AX_ := $4A33;
      intr_RealMode (R, $2F);
      IsWin95DosBox := R. AX_ = 0;
      Exit;
    End;

  IsWin95DosBox := False;
End;
{$ENDIF}
{$ENDIF}

Procedure FreeTitleMem;
Begin
{$IFDEF MSDOS}
{$IFDEF DPMI}
  If ChangeTitle Then
    FreeDosMem (TitlePMSel);
{$ENDIF}
{$IFDEF DPMI32}
  If ChangeTitle Then
    FreeDosMem (TitleSeg);
{$ENDIF}
{$ENDIF}
{$IFDEF WIN32}
  FreeMem (PTitle, 256);
{$ENDIF}
End;

Type
  PDeviceTable = ^TDeviceTable;
  TDeviceTable = Object (THashTable)
    Procedure Insert (Const DevName: String);
  End;

Var
  Devices : PDeviceTable;

Procedure TDeviceTable. Insert (Const DevName: String);
Begin
  Inherited Insert (DevName, Pointer (LongInt (1)));
End;

Procedure InitDeviceTable;
Begin
  Devices := New (PDeviceTable, Init (3));
  With Devices^ Do
  Begin
    Insert ('COM1');
    Insert ('COM2');
    Insert ('COM3');
    Insert ('COM4');
    Insert ('COM5');
    Insert ('COM6');
    Insert ('COM7');
    Insert ('COM8');
    Insert ('LPT1');
    Insert ('LPT2');
    Insert ('LPT3');
    Insert ('LPT4');
    Insert ('MSCD001');
    Insert ('CLOCK$');
    Insert ('CON');
    Insert ('PRN');
    Insert ('NUL');
    Insert ('AUX');
  End;
End;

Function IsDevice (Const DevName: String): Boolean;
Begin
  IsDevice := Devices^. Search (UpString (DevName)) <> Nil;
End;

Begin
  OrigHdr. Version := ExtractWord (2, NameVer, SpaceOnly);
  OrigHdr. ExeName := UpString (ParamStr (0));
{$IFDEF MSDOS}
  ChangeTitle := {$IFNDEF DPMI32} NTVDMInitOk Or {$ENDIF} IsWin95DosBox;
{$IFDEF DPMI}
  If ChangeTitle Then
    AllocDosMem (16, TitleRMSeg, TitlePMSel);
{$ENDIF}
{$IFDEF DPMI32}
  If ChangeTitle Then
    GetDosMem (TitleSeg, 256);
{$ENDIF}
{$ENDIF}
{$IFDEF WIN32}
  GetMem (PTitle, 256);
{$ENDIF}
End.
