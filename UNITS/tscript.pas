{$IFDEF MSDOS}
  {$IFNDEF DPMI32}
    {$F+}
  {$IFNDEF DPMI}
    {$DEFINE RealMode}
    {$O+}
  {$ENDIF}
  {$ENDIF}
{$ENDIF}
{$I-,B-,X+}
{&Delphi+}

Unit
  tScript;

{*********************************************************}
{*                     TSCRIPT.PAS                       *}
{*                                                       *}
{*  Copyright (c) Konstantin Klyagin, 1995-98,           *}
{*                exspecially for Tornado BBS System     *}
{*                                                       *}
{*********************************************************}

Interface

Function tExecScript (fName: String): Boolean;
Procedure oExecScript (Const S: String);
Procedure PrepareScriptTables;

Implementation

Uses
{$IFDEF RealMode}
  Streams,
{$ENDIF}
{$IFDEF OS2}
  OS2Base,
{$ENDIF}
  DOS,
  Objects,
  tGlob,
  tMisc,
  Crc,
  OpCrt,
  Log,
  Parse,
  BinCfg,
  Shell,
  Areas,
  skCommon,
  Protocol,
  TorInOut,
  MainComm,
  MainCOvr,
  SysMsgs,
  Users,
  FilesBBS,
  tBigFunc,
  DoorWay,
  tQWK,
  RCache,
  mFind;

Const
  vReadOnly         = 1;
  vArray            = 2;
  vMacro            = 4;
  vBackgroundChange = 8;

  aFixIndexes   = 1;
  aContentValid = 2;

  TLongStream_BufSize = 64;

{$IFDEF RealMode}
  VariableBlockSize   = 8;
  VariablesTableBits  = 6;
  ProceduresTableBits = 4;
  LabelsTableBits     = 3;
  CommandsTableBits   = 5;
  VarMacrosTableBits  = 3;
  ResVarsTableBits    = 4;
  ProcTextDelta       = 8;
  ProcStackDelta      = 4;
  FilesDelta          = 2;
  FileFindDelta       = 1;
{$ELSE}
  VariableBlockSize   = 16;
  VariablesTableBits  = 8;
  ProceduresTableBits = 5;
  LabelsTableBits     = 4;
  CommandsTableBits   = 6;
  VarMacrosTableBits  = 4;
  ResVarsTableBits    = 5;
  ProcTextDelta       = 16;
  ProcStackDelta      = 16;
  FilesDelta          = 4;
  FileFindDelta       = 2;
{$ENDIF}
  VariableBlockMask   = $FFFF - (VariableBlockSize - 1);

  CommandGroupShift   = 16;
  CommandTypeMask     = $FFFF;

Type
  CommandGroup = (_Lang, _String, _Console, _File, _Misc);
  CommandType = (_End,_If,_Else,_Goto,_Exit,_Halt,                 { _Lang }
                 _GetStringLength,_StrPos,_SubString,              { _String }
                 _StrDelete,_StrReplace,_Val,_Ord,_TrimStr,
                 _PadStr,_WordCount,_ExtractWord,_StrConsists,
                 _StrMaskMatch,_StrStripColors,_UpCase,_LoCase,
                 _PrCase,_GetArg,
                 _KeyPressed,_ReadKey,_Write,_WriteLn,_SetColor,   { _Console }
                 _GetColor,_Clear,_ClrEOL,_SetCursorCoord,
                 _GetCursorCoord,_CursorLeft,_CursorRight,
                 _CursorUp,_CursorDown,_KeyMacro,_SetWriteMode,
                 _Message,_ReadVar,_YesNo,_NoYes,_FileDisplay,
                 _FileReadString,_FileGetEOF,_FileWriteString,     { _File }
                 _FileGetToString,_FileClose,_FileOpen,
                 _FileCreate,_FileAppend,_FileExists,_FileSize,
                 _FileDelete,_FindFirst,_FindNext,
                 _Random,_MidSec,_FlagsValid,_AddToDownloadList,   { _Misc }
                 _AddToDLListRelative,_GetFirstTagged,
                 _GetNextTagged,_IsUser,_GetUserNames,_Log,
                 _Download,_Upload,_UploadPriv,_GlobalSearch,
                 _SearchByDesc,_NewFilesSince,_NewFilesSinceLast,
                 _ChangeFileArea,_ChangeMsgArea,_ChangeFileGroup,
                 _ChangeMsgGroup,_FileList,_WriteMsg,_FilePost,
                 _MsgRead,_MsgList,_MsgSearch,_ScanPrivMail,
                 _DownloadQWK,_UploadQWK,_SelectQWK,_ShowRawDir,
                 _PageSysop,_DoorWay,_News,_Telnet,_UserInfo,
                 _HangUp,_Call,_Exec,_Delay);
  VarMacroType = (vmNONE, vmFNUM, vmMNUM, vmFGRN, vmMGRN, vmSECR, vmFLGS,
                  vmLINS, vmLANG, vmEMUL, vmLIMK, vmLFKB, vmULDS, vmDNLS,
                  vmULKB, vmDLKB, vmETME, vmPSWD, vmLOCA, vmORGZ, vmALAS,
                  vmHPHN, vmBPHN, vmADR1, vmADR2, vmADR3, vmCMNT, vmCALN,
                  vmDAOB, vmFDTE, vmLDTE, vmLTME, vmPROT, vmFSED, vmFRAM,
                  vmHKEY, vmMORE);
  ReservedVarType = (rvNone, rvNAME, rvSECR, rvBAUD, rvLANG, rvEMUL, rvALAS,
                     rvFLGS, rvLINS, rvPSWD, rvSYSO, rvBBSN, rvDATE, rvTIME,
                     rvFNUM, rvFGRN, rvMNUM, rvMGRN, rvFARE, rvFGRP, rvMARE,
                     rvMGRP, rvFAGR, rvFAMT, rvFGAM, rvMAGR, rvMAMT, rvMGAM,
                     rvKeybUp, rvKeybDown, rvKeybRight, rvKeybLeft, rvKeybHome,
                     rvKeybEnd, rvFADlPath, rvFAUlPath, rvFAFileList,
                     rvMAAddress, rvMAOrigin, rvMABase, rvMAPath, rvMABoard,
                     rvSELN, rvArgCount, rvCfgPath, rvCfgTempDir,
                     rvCfgFlagsDir, rvCfgDoorDir, rvCfgLangDir,
                     rvCfgLangTxtPath, rvCfgLangMnuPath, rvCfgLangNews,
                     rvCfgSaveTag, rvCfgPrivUL, rvCfgPort, rvCfgLog,
                     rvCfgChatLog, rvCfgTrcLog, rvCfgLogo, rvEOMacro,
                     rvEOSlashCode, rvEOColorCode, rvEODisable01,
                     rvPadStrRight, rvPadStrLeft, rvPadStrCenter,
                     rvReadOnly, rvHidden, rvSysFile, rvVolumeID, rvDirectory,
                     rvArchive, rvAnyFile, rvFNAM,
                     rvLNAM, rvUSRS, rvPROT, rvLOCA, rvORGZ, rvDAOB, rvHPHN,
                     rvBPHN, rvADR1, rvADR2, rvADR3, rvCMNT, rvCALN, rvLDTE,
                     rvLTME, rvFDTE, rvLFKB, rvOTME, rvDLTK, rvMSGP, rvSELK,
                     rvULDS, rvDNLS, rvULKB, rvDLKB, rvETTM, rvLIMK, rvETME,
                     rvTTME, rvPROD, rvFSED, rvNODE, rvFRAM, rvHKEY, rvMORE);
  tFileOpenMode = (foOpen, foCreate, foAppend);
  tActionType = (aUnknown, aCommand, aExpression, aProcedure);
  PadStrMode = (PadStrRight, PadStrLeft, PadStrCenter);
  ResourceBlockType = (rbVar, rbProc);
  ResourceVarType = (rvSingle, rvArray);

Const
  tFOpenModes : Array [_FileOpen.._FileAppend] Of tFileOpenMode =
                  (foOpen, foCreate, foAppend);

Type
  TLongStream_BufMode = (Reading, Writing);
  TLongStream_Buf = Array [0..TLongStream_BufSize-1] Of LongInt;

  PLongStream = ^TLongStream;
  TLongStream = Object
    DataStream       : PStream;
    BufStart, BufLen : LongInt;
    DataBuf          : TLongStream_Buf;
    BufState         : TLongStream_BufMode;

    Constructor Init (StartSize: LongInt);
    Function Count: LongInt;
    Procedure Add (Item: LongInt);
    Function Get (Num: LongInt): LongInt;
    Destructor Done;
  End;

  PProcTextCollection = ^TProcTextCollection;
  TProcTextCollection = Object (TCollection)
    Procedure FreeItem (Item: Pointer); Virtual;
  End;

  PProcStackCollection = ^TProcStackCollection;
  TProcStackCollection = Object (TCollection)
    Procedure FreeItem (Item: Pointer); Virtual;
  End;

  PFileCollection = ^TFileCollection;
  TFileCollection = Object (TCollection)
    Procedure FreeItem (Item: Pointer); Virtual;
  End;

  PFileFindCollection = ^TFileFindCollection;
  TFileFindCollection = Object (TCollection)
    Procedure FreeItem (Item: Pointer); Virtual;
  End;

  PCommandsTable = ^TCommandsTable;
  TCommandsTable = Object (THashTable)
    Procedure Insert (Const S: String; Cmd: CommandType; Grp: CommandGroup);
  End;

  PVarMacrosTable = ^TVarMacrosTable;
  TVarMacrosTable = Object (THashTable)
    Procedure Insert (Const Name: String; vmNum: VarMacroType);
  End;

  PReservedVarsTable = ^TReservedVarsTable;
  TReservedVarsTable = Object (THashTable)
    Procedure Insert (Const Name: String; rvNum: ReservedVarType);
  End;

  PVariablesTable = ^TVariablesTable;
  TVariablesTable = Object (THashTable)
    Function NewData (P: Pointer): Pointer; Virtual;
    Procedure DisposeData (P: Pointer); Virtual;
  End;

  PLabelsTable = ^TLabelsTable;
  TLabelsTable = Object (THashTable)
    Function NewData (P: Pointer): Pointer; Virtual;
    Procedure DisposeData (P: Pointer); Virtual;
  End;

  PProceduresTable = ^TProceduresTable;
  TProceduresTable = Object (THashTable)
    Function NewData (P: Pointer): Pointer; Virtual;
    Procedure DisposeData (P: Pointer); Virtual;
  End;

  tScriptState  = (psProcedure, psVariables, psNone);
  tVariableType = (tvString, tvLogical, tvNumber);

  PScriptString = ^tScriptString;
  tScriptString = Record
    ActionData  : Pointer;
    ContentData : Pointer;
    Line        : System. Word;
    Action      : tActionType;
    Flags       : Byte;
    Str         : String;
  End;

  PVarArrayRec = ^VarArrayRec;
  VarArrayRec = Record
    Size : Integer;
    Data : Array [1..16380] Of PString;
  End;

  PVariable = ^tVariable;
  tVariable = Record
    NameCSum : tChecksum;
    VarType  : tVariableType;
    Flags    : Byte;
    Case Byte Of
      0: (Value : PString);
      1: (Items : PVarArrayRec);
  End;

  GetVarRec = Record
    Variable : PVariable;
    DataPtr  : ^PString;
  End;

  PScriptLabel = ^tScriptLabel;
  tScriptLabel = Record
    Line     : LongInt;
    EndCount : Byte;
  End;

  PProcedure = ^tProcedure;
  tProcedure = Record
    Text   : PProcTextCollection;
    Labels : PLabelsTable;
  End;

  PFile = ^tFile;
  tFile = Record
    Number    : LongInt;
    Lines     : PLongStream;
    Handle    : Text;
    EndOfFile : Boolean;
  End;

  PFileFind = ^tFileFind;
  tFileFind = Record
    Number : LongInt;
    Info   : SearchRec;
  End;

  PProcStackElement = ^tProcStackElement;
  tProcStackElement = Record
    ProcText   : PProcTextCollection;
    ProcLabels : PLabelsTable;
    ProcLine   : LongInt;
    EndCount   : Byte;
  End;

Const
  Commands      : PCommandsTable = Nil;
  VarMacros     : PVarMacrosTable = Nil;
  ReservedVars  : PReservedVarsTable = Nil;

  ExprChars     = ['A'..'Z', 'a'..'z', '0'..'9', '$', '#', '_'];
  MainTag       = ' MAIN';

  CompareDelims : Set Of Char = ['<', '>', '='];

  StrBase       : Array [mbfJam..mbfSquish] Of String [6] =
                    ('JAM', 'Fido', 'Squish');

Var
  Parameter, Par1, Par2 : String;
  WorkSet               : CharSet;

Function NewScriptString (Const S: String; Line: Word; Action: tActionType;
                          Flags: Byte; ActionData: Pointer): PScriptString;
Var
  P : PScriptString;

Begin
  GetMem (P, SizeOf (P^) - SizeOf (P^. Str) + Length (S) + 1);
  P^. Line := Line;
  P^. Action := Action;
  P^. Flags := Flags;
  P^. ActionData := ActionData;
  P^. Str := S;
  NewScriptString := P;
End;

Procedure TProcTextCollection. FreeItem (Item: Pointer);
Var
  P : PScriptString Absolute Item;

Begin
  FreeMem (P, SizeOf (P^) - SizeOf (P^. Str) + Length (P^. Str) + 1);
End;

Procedure TProcStackCollection. FreeItem (Item: Pointer);
Begin
  Dispose (PProcStackElement (Item));
End;

Procedure TFileCollection. FreeItem (Item: Pointer);
Var
  P : PFile Absolute Item;

Begin
  If P <> Nil Then
  Begin
    Close (P^. Handle);
    If IOResult <> 0 Then;

    If P^. Lines <> Nil Then
      Dispose (P^. Lines, Done);
    Dispose (P);
  End;
End;

Procedure TFileFindCollection. FreeItem (Item: Pointer);
Var
  P : PFileFind Absolute Item;

Begin
  If P <> Nil Then
  Begin
  {$IFNDEF MSDOS}
    FindClose (P^. Info);
  {$ENDIF}
    Dispose (P);
  End;
End;

Function CommandTypeGroup (Cmd: CommandType; Grp: CommandGroup): LongInt;
Begin
  CommandTypeGroup := LongInt (Grp) Shl CommandGroupShift + LongInt (Cmd);
End;

Procedure TCommandsTable. Insert (Const S: String; Cmd: CommandType;
                                  Grp: CommandGroup);
Begin
  Inherited Insert (S, Pointer (CommandTypeGroup (Cmd, Grp) + 1));
End;

Procedure TVarMacrosTable. Insert (Const Name: String; vmNum: VarMacroType);
Begin
  Inherited Insert (Name, Pointer (LongInt (vmNum)));
End;

Procedure TReservedVarsTable. Insert (Const Name: String;
                                      rvNum: ReservedVarType);
Begin
  Inherited Insert (Name, Pointer (LongInt (rvNum)));
End;

Function TVariablesTable. NewData (P: Pointer): Pointer;
Var
  V : PVariable;

Begin
  New (V);
  V^ := PVariable (P)^;
  NewData := V;
End;

Function NewVarData (Const S: String): PString;
Var
  P : PString;

Begin
  GetMem (P, (Length (S) + VariableBlockSize) And VariableBlockMask);
  P^ := S;
  NewVarData := P;
End;

Procedure DisposeVarData (P: PString);
Begin
  If P <> Nil Then
    FreeMem (P, (Length (P^) + VariableBlockSize) And VariableBlockMask);
End;

Procedure TVariablesTable. DisposeData (P: Pointer);
Var
  V : PVariable Absolute P;
  i : Integer;

Begin
  If V <> Nil Then
  Begin
    If V^. Flags And vArray = 0 Then
      DisposeVarData (V^. Value)
    Else
      With V^. Items^ Do
      Begin
        For i := 1 To Size Do
          DisposeVarData (Data [i]);

        FreeMem (V^. Items, SizeOf (Size) + Size * SizeOf (PString));
      End;

    Dispose (V);
  End;
End;

Function TLabelsTable. NewData (P: Pointer): Pointer;
Var
  L : PScriptLabel;

Begin
  New (L);
  L^ := PScriptLabel (P)^;
  NewData := L;
End;

Procedure TLabelsTable. DisposeData (P: Pointer);
Begin
  If P <> Nil Then
    Dispose (PScriptLabel (P));
End;

Function TProceduresTable. NewData (P: Pointer): Pointer;
Var
  PP : PProcedure;

Begin
  New (PP);
  PP^ := PProcedure (P)^;
  NewData := PP;
End;

Procedure TProceduresTable. DisposeData (P: Pointer);
Var
  PP : PProcedure Absolute P;

Begin
  If PP <> Nil Then
  Begin
    If PP^. Text <> Nil Then
      Dispose (PP^. Text, Done);

    If PP^. Labels <> Nil Then
      Dispose (PP^. Labels, Done);

    Dispose (PP);
  End;
End;

Constructor TLongStream. Init (StartSize: LongInt);
{$IFDEF RealMode}
Var
  StreamOK : Boolean;
{$ENDIF}

Begin
{$IFDEF RealMode}
  StreamOK := False;

  DataStream := New (PXMSStream, Init (StartSize, StartSize));
  If DataStream <> Nil Then
    If DataStream^. Status <> stOk Then
      Dispose (DataStream, Done)
    Else
      StreamOK := True;

  If Not StreamOK Then
  Begin
    DataStream := New (PEMSStream3, Init (StartSize, StartSize));
    If DataStream <> Nil Then
      If DataStream^. Status <> stOk Then
        Dispose (DataStream, Done)
      Else
        StreamOK := True;
  End;

  If Not StreamOK Then
{$ENDIF}
    DataStream := New (PMemoryStream, Init (0, 0));

  BufStart := 0;
  BufLen := 0;
  BufState := Writing;
End;

Destructor TLongStream. Done;
Begin
  Dispose (DataStream, Done);
End;

Function TLongStream. Count: LongInt;
Begin
  If BufState = Writing Then
    Count := BufStart + BufLen
  Else
    Count := DataStream^. GetSize Div SizeOf (LongInt);
End;

Procedure TLongStream. Add (Item: LongInt);
Begin
  If BufState <> Writing Then
  Begin
    BufState := Writing;
    BufStart := DataStream^. GetSize Div SizeOf (Item);
    BufLen := 0;
  End
  Else
    If BufLen = TLongStream_BufSize Then
    Begin
      DataStream^. Seek (BufStart * SizeOf (Item));
      DataStream^. Write (DataBuf, BufLen * SizeOf (Item));
      Inc (BufStart, BufLen);
      BufLen := 0;
    End;

  DataBuf [BufLen] := Item;
  Inc (BufLen);
End;

Function TLongStream. Get (Num: LongInt): LongInt;
Var
  i : LongInt;

Begin
  If BufState <> Reading Then
  Begin
    BufState := Reading;
    DataStream^. Seek (BufStart * SizeOf (LongInt));
    DataStream^. Write (DataBuf, BufLen * SizeOf (LongInt));
  End;

  i := Num - BufStart;
  If (i >= 0) And (i < BufLen) Then
  Begin
    Get := DataBuf [i];
    Exit;
  End;

  BufStart := Num;
  BufLen := (DataStream^. GetSize Div SizeOf (LongInt)) - BufStart;
  If BufLen > TLongStream_BufSize Then
    BufLen := TLongStream_BufSize;

  DataStream^. Seek (BufStart * SizeOf (LongInt));
  DataStream^. Read (DataBuf, BufLen * SizeOf (LongInt));

  Get := DataBuf [0];
End;

Procedure AddToDLList (Const FileName: String; NewGroup, NewArea: LongInt);
Var
  DirInfo : SearchRec;
  FileRec : TTagFileRec;

Begin
  FileRec. PathName := NewStr (Trim (FileName));
  FindFirst (FileRec. PathName^, AnyFile-VolumeID-Directory-Hidden, DirInfo);

  If DosError = 0 Then
  Begin
    FileRec. GroupNum := NewGroup;
    FileRec. AreaNum := NewArea;
    FileRec. Size := DirInfo. Size;
    FileRec. FromName := Nil;
    FileRec. Free := False;
    InsertInTagList (FileRec);
  End
  Else
    DisposeStr (FileRec. PathName);

{$IFNDEF MSDOS}
  FindClose (DirInfo);
{$ENDIF}
End;

{$IFNDEF VirtualPascal}
  {$L TScript.Obj}
  Function GetParams (Const S: String): String; Far; External;
{$ELSE}
  {$L TScr32.Obj}
  Function GetParams (Const S: String): String; External;
{$ENDIF}

Function tExecScript (fName: String): Boolean;
Var
  Variables              : PVariablesTable;
  Procedures             : PProceduresTable;
  ProcText               : PProcTextCollection;
  ProcLabels             : PLabelsTable;
  ProcStack              : PProcStackCollection;
  Files                  : PFileCollection;
  FileFinders            : PFileFindCollection;
  ScriptParams           : PNotSortedCollection;
  CurrScriptString       : PScriptString;
  ProcLine               : LongInt;
  EndCount1              : Integer;
  LineNum                : Word;
  Finished, SystemChange : Boolean;
  S                      : String;

Procedure ChangeVarDirect (Const V: GetVarRec; Const NewVal: String); Forward;
Procedure ChangeVar (Const VarName, NewVal: String); Forward;
Function MathExpression (Const ScanStr: String; Var Out: LongInt): Boolean; Forward;
Function StrExpression (Const S: String; Monolite: Boolean): String; Forward;
Function LogicExpression (SStr: String): Boolean; Forward;
Procedure UpdateMacro (Const CSum: tChecksum; Value: String); Forward;
Procedure AddReservedVar (Const CSum: tChecksum); Forward;

Procedure Error (Code: Integer);
Var
  S : String;

Begin
  S := 'Script error in ' + NiceFileName (fName, 30) + ' (' + Long2Str
    (LineNum) + '): ' + sm (Code);
  LogWrite ('!', S);
  S [Pos ('): ', S) + 2] := '|';
  ComWrite (#13 + EmuRelColor (7), 0);
  ComWrite (EmuClrEOL, 0);
  Message (S);
  Finished := True;
End;

Procedure GetVar (Const N: String; Var V: GetVarRec);
Var
  Current, P, i : Integer;
  CSum          : tChecksum;

Begin
  P := Pos ('#', N);
  If P > 0 Then
  Begin
    Val (Copy (N, P + 1, 255), Current, i);
    If i <> 0 Then
      Current := 0;

    GetChecksum (UpString (Copy (N, 1, P - 1)), CSum);
  End Else
  Begin
    Current := 0;
    GetChecksum (UpString (N), CSum);
  End;

  V. Variable := Variables^. SearchByChecksum (CSum);

  If V. Variable = Nil Then
  Begin
    AddReservedVar (CSum);
    V. Variable := Variables^. SearchByChecksum (CSum);

    If V. Variable = Nil Then
      Exit;
  End;

  With V. Variable^ Do
    If (Flags And vArray) = 0 Then
      V. DataPtr := @Value
    Else
      If (Current < 1) Or (Current > Items^. Size) Then
      Begin
        Error (seInvalidIndex);
        V. Variable := Nil;
      End
      Else
        V. DataPtr := @Items^. Data [Current];
End;

Function GetValueDirect (Const V: GetVarRec): String;
Begin
  If V. Variable^. Flags And vBackgroundChange <> 0 Then
  Begin
    SystemChange := True;
    ChangeVar ('$TIME', StrTime);
    ChangeVar ('$ETME', Long2Str (Round ((R. TotalTime - TimeDiff (EnterTime,
      MidSec)) / 60)));
    ChangeVar ('$TTME', Long2Str (Lim. Time));
    ChangeVar ('$OTME', Long2Str (Round (TimeDiff (EnterTime, MidSec) / 60)));
    SystemChange := False;
    UpdateUserMacro;
  End;

  If V. DataPtr^ <> Nil Then
    GetValueDirect := V. DataPtr^^
  Else
    If V. Variable^. VarType = tvString Then
      GetValueDirect := ''
    Else
      GetValueDirect := '0';
End;

Function AddSingleVarByChecksum (Const CSum: tChecksum; vType: tVariableType;
                                 Const Value: String; Flags: Byte): Boolean;
Var
  V : TVariable;

Begin
  If Variables^. SearchByChecksum (CSum) = Nil Then
  Begin
    V. NameCSum := CSum;
    V. VarType := vType;
    V. Flags := Flags And Not vArray;
    If Value = '' Then V. Value := Nil
                  Else V. Value := NewVarData (Value);
    Variables^. InsertByChecksum (CSum, @V);
    AddSingleVarByChecksum := True;
  End
  Else
    AddSingleVarByChecksum := False;
End;

Function AddSingleVar (Const Name: String; vType: tVariableType;
                       Const Value: String; Flags: Byte): Boolean;
Var
  CSum : tChecksum;

Begin
  GetChecksum (Name, CSum);
  AddSingleVar := AddSingleVarByChecksum (CSum, vType, Value, Flags);
End;

Function AddArrayVarByChecksum (Const CSum: tChecksum; vType: tVariableType;
                                Flags: Byte; Size: Integer): Boolean;
Var
  V : TVariable;

Begin
  If Variables^. SearchByChecksum (CSum) = Nil Then
  Begin
    V. NameCSum := CSum;
    V. VarType := vType;
    V. Flags := Flags Or vArray;
    GetMem (V. Items, SizeOf (V. Items^. Size) + Size * SizeOf (PString));
    V. Items^. Size := Size;
    FillChar (V. Items^. Data, Size * SizeOf (PString), 0);
    Variables^. InsertByChecksum (CSum, @V);
    AddArrayVarByChecksum := True;
  End
  Else
    AddArrayVarByChecksum := False;
End;

Function AddArrayVar (Const Name: String; vType: tVariableType; Flags: Byte;
                      Size: Integer): Boolean;
Var
  CSum : tChecksum;

Begin
  GetChecksum (Name, CSum);
  AddArrayVar := AddArrayVarByChecksum (CSum, vType, Flags, Size);
End;

Procedure ChangeVarDirect (Const V: GetVarRec; Const NewVal: String);
Var
  NewLen, OldLen : Integer;

Label
  SetVal;

Begin
  With V. Variable^ Do
    If (Flags And vReadOnly = 0) Or SystemChange Then
    Begin
      NewLen := (Length (NewVal) + VariableBlockSize) And VariableBlockMask;

      If V. DataPtr^ <> Nil Then
      Begin
        OldLen := (Length (V. DataPtr^^) + VariableBlockSize) And
          VariableBlockMask;

        If OldLen = NewLen Then
          Goto SetVal;

        FreeMem (V. DataPtr^, OldLen);
      End;

      GetMem (V. DataPtr^, NewLen);

    SetVal:
      V. DataPtr^^ := NewVal;

      If (Flags And vMacro <> 0) And Not SystemChange Then
        UpdateMacro (NameCSum, V. DataPtr^^);
    End
    Else
      Error (seReadOnly);
End;

Procedure ChangeVar (Const VarName, NewVal: String);
Var
  V : GetVarRec;

Begin
  GetVar (VarName, V);
  If V. Variable <> Nil Then
    ChangeVarDirect (V, NewVal);
End;

Function GetPFileNum (N: LongInt): PFile;
Var
  i : Integer;
  F : PFile;

Begin
  For i := 0 To Files^. Count-1 Do
  Begin
    F := Files^. At (i);

    If F^. Number = N Then
    Begin
      GetPFileNum := F;
      Exit;
    End;
  End;

  GetPFileNum := Nil;
End;

Procedure ClosePFile (N: LongInt);
Var
  i : Integer;
  F : PFile;

Begin
  For i := 0 To Files^. Count-1 Do
  Begin
    F := Files^. At (i);

    If F^. Number = N Then
    Begin
      Files^. AtFree (i);
      Exit;
    End;
  End;
End;

Function fFileOpen (L: LongInt; Const T: String; Mode: tFileOpenMode): Boolean;
Var
  FileRec : PFile;
{$IFNDEF VirtualPascal}
  Result  : Boolean;
{$ENDIF}

Begin
  New (FileRec);

  With FileRec^ Do
  Begin
    Number := L;
    Lines := Nil;
    EndOfFile := False;
    Assign (Handle, T);

    Case Mode Of
        foOpen : ReSet (Handle);
      foCreate : ReWrite (Handle);
      foAppend : Begin
                   Append (Handle);
                   If IOResult <> 0 Then
                     ReWrite (Handle);
                 End;
    End;
  End;

  Result := IOResult = 0;
  If Result Then Files^. Insert (FileRec)
            Else Dispose (FileRec);

{$IFNDEF VirtualPascal}
  fFileOpen := Result;
{$ENDIF}
End;

Function GetEOF (N: LongInt): Boolean;
Var
  F : PFile;

Begin
  F := GetPFileNum (N);
  GetEOF := (F = Nil) Or F^. EndOfFile Or EoF (F^. Handle);
End;

Function GetPFileFindNum (N: LongInt): PFileFind;
Var
  i : Integer;
  F : PFileFind;

Begin
  For i := 0 To FileFinders^. Count-1 Do
  Begin
    F := FileFinders^. At (i);

    If F^. Number = N Then
    Begin
      GetPFileFindNum := F;
      Exit;
    End;
  End;

  GetPFileFindNum := Nil;
End;

Procedure ClosePFileFinder (N: LongInt);
Var
  i : Integer;
  F : PFileFind;

Begin
  For i := 0 To FileFinders^. Count-1 Do
  Begin
    F := FileFinders^. At (i);

    If F^. Number = N Then
    Begin
      FileFinders^. AtFree (i);
      Exit;
    End;
  End;
End;

Function MathExpression (Const ScanStr: String; Var Out: LongInt): Boolean;
Type
  Token = (nothing, number, plus, minus, ast, dvt, md, step, _not, _and, _or,
           _xor, lbracket, rbracket);

Var
  Lexeme                : LongInt;
  ScanLength, ScanCount : Word;
  BracketCounter        : Integer;
  sym                   : Token;
  Success               : Boolean;

  Procedure Scan;
  Var
    i   : Word;
    Err : Integer;
    C   : Char;
    V   : GetVarRec;

  Label
    SkipLoop;

  Begin

  SkipLoop:
    If ScanCount > ScanLength Then
    Begin
      sym := nothing;
      Exit;
    End;

    C := ScanStr [ScanCount];
    If C = ' ' Then
    Begin
      Inc (ScanCount);
      Goto SkipLoop;
    End;

    Success := True;

    If C in NumbersOnly Then
    Begin
      sym := number;
      i := ScanCount;

      Repeat
        Inc (ScanCount);
      Until (ScanCount > ScanLength) Or Not (ScanStr [ScanCount] in NumbersOnly);

      Val (Copy (ScanStr, i, ScanCount - i), Lexeme, Err);
      If Err <> 0 Then
        Lexeme := 0;
    End
    Else
      If C in ExprChars Then
      Begin
        i := ScanCount;

        Repeat
          Inc (ScanCount);
        Until (ScanCount > ScanLength) Or Not (ScanStr [ScanCount] in ExprChars);

        GetVar (Copy (ScanStr, i, ScanCount - i), V);

        If V. Variable <> Nil Then
        Begin
          If V. Variable^. VarType = tvNumber Then
          Begin
            sym := number;
            Val (GetValueDirect (V), Lexeme, Err);
            If Err <> 0 Then
              Lexeme := 0;

            Exit;
          End
          Else
            Error (seInvalidType);
        End
        Else
          Error (seUnrecSymb);

        Success := False;
      End Else
      Begin
        Case C Of
          '+' : sym := plus;
          '-' : sym := minus;
          '(' : Begin
                  sym := lbracket;
                  Inc (BracketCounter);
                End;
          ')' : Begin
                  sym := rbracket;
                  Dec (BracketCounter);
                End;
          '*' : sym := ast;
          '/',
          ':' : sym := dvt;
          '%' : sym := md;
          '^' : sym := step;
          '!' : sym := _not;
          '&' : sym := _and;
          '|' : sym := _or;
          '@' : sym := _xor;
          #0,
          #13 : Exit;
        Else
          Error (seUnrecSymb);
          Success := False;

          Exit;
        End;

        Inc (ScanCount);
      End;
  End;

  Function Power (a, b: Real): LongInt;
  Var
    P : LongInt;

  Begin
    P := Trunc (Exp (b * Ln (Abs (a))));
    If (a < 0) And (Odd (Round (b)) Or (Frac (b) <> 0)) Then
      P := - P;

    Power := P;
  End;

  Procedure Level3 (Var Out: LongInt); Forward;
  Procedure Level4 (Var Out: LongInt); Forward;
  Procedure Level5 (Var Out: LongInt); Forward;
  Procedure Level6 (Var Out: LongInt); Forward;

  Procedure Level2 (Var Out: LongInt);
  Var
    op2 : LongInt;
    op  : Token;

  Begin
    Level3 (Out);

    If Success Then
      While (sym = plus) Or (sym = minus) Do
      Begin
        op := sym;

        Scan;
        If Not Success Then
          Exit;

        Level3 (op2);
        If Not Success Then
          Exit;

        If op = plus Then Out := Out + op2
                     Else Out := Out - op2;
      End;
  End;

  Procedure Level3 (Var Out: LongInt);
  Var
    op2 : LongInt;
    op  : Token;

  Begin
    Level4 (Out);

    While (sym in [ast, dvt, md, _and, _or, _xor]) And Success Do
    Begin
      op := sym;

      Scan;
      If Not Success Then
        Exit;

      Level4 (op2);
      If Not Success Then
        Exit;

      Case op Of
        ast : Out := Out * op2;
        dvt : If op2 <> 0 Then
                Out := Out Div op2
              Else
              Begin
                Error (seZeroDivide);
                Success := False;
              End;
         md : If op2 <> 0 Then
                Out := Out Mod op2
              Else
              Begin
                Error (seZeroDivide);
                Success := False;
              End;
       _and : Out := Out And op2;
        _or : Out := Out Or op2;
       _xor : Out := Out Xor op2;
      End;
    End;
  End;

  Procedure Level4 (Var Out: LongInt);
  Var
    op2 : LongInt;

  Begin
    Level5 (Out);

    If Success Then
      While sym = step Do
      Begin
        Scan;
        If Not Success Then
          Exit;

        Level5 (op2);
        If Not Success Then
          Exit;

        Out := Power (Out, op2);
      End;
  End;

  Procedure Level5 (Var Out: LongInt);
  Var
    op : Token;

  Begin
    If (sym = minus) Or (sym = plus) Or (sym = _not) Then
    Begin
      op := sym;

      Scan;
      If Not Success Then
        Exit;

      Level6 (Out);

      If Success Then
        Case op Of
          minus : Out := -Out;
           _not : Out := Not Out;
        End;
    End
    Else
      Level6 (Out);
  End;

  Procedure Level6 (Var Out: LongInt);
  Begin
    If sym = number Then
    Begin
      Out := Lexeme;
      If Success Then
        Scan;
    End
    Else
      If sym = lbracket Then
      Begin
        Scan;
        If Not Success Then
          Exit;

        Level2 (Out);
        If Success Then
          If sym = rbracket Then
            Scan
          Else
          Begin
            Error (seWrongBrack);
            Success := False;
          End;
      End;
  End;

Begin
  MathExpression := False;

  ScanLength := Length (ScanStr);
  If ScanLength = 0 Then
  Begin
    Error (seExprError);
    Exit;
  End;

  ScanCount := 1;
  BracketCounter := 0;

  Scan;
  If Success Then
  Begin
    Level2 (Out);
    If Success Then
      If BracketCounter = 0 Then
        MathExpression := True
      Else
        Error (seWrongBrack);
  End;
End;

Function StringCompare (Const S: String; Const V: GetVarRec): Boolean;
Begin
  If AsciiCount (S, ['='], '"') = 2 Then
    StringCompare := GetValueDirect (V) = StrExpression (Trim (ExtractAscii
      (2, S, ['='], '"')), True)
  Else
    If (AsciiCount (S, ['<'], '"') = 2) And (AsciiCount (S, ['>'], '"') = 2)
    Then
      StringCompare := GetValueDirect (V) <> StrExpression (Trim (ExtractAscii
        (2, S, ['>'], '"')), True)
    Else
      Error (seExprError);
End;

Function NumberCompare (Const S: String; Const V: GetVarRec): Boolean;
Var
  Left, Right                 : LongInt;
  EqCount, MinCount, MaxCount : Integer;
{$IFNDEF VirtualPascal}
  Result                      : Boolean;
{$ENDIF}

Begin
  Result := False;

  If MathExpression (Trim (ExtractWord (2, S, CompareDelims)), Right) Then
  Begin
    Left := Str2Long (GetValueDirect (V));

    EqCount := WordCount (S, ['=']);
    MinCount := WordCount (S, ['<']);
    MaxCount := WordCount (S, ['>']);

    If EqCount = 2 Then
      Result := Left = Right;

    If (MinCount = 2) And (MaxCount < 2) Then
      Result := Result Or (Left < Right)
    Else
      If (MaxCount = 2) And (MinCount < 2) Then
        Result := Result Or (Left > Right)
      Else
        If (MaxCount = 2) And (MinCount = 2) Then
          Result := Left <> Right
        Else
          If EqCount < 2 Then
            Error (seExprError);
  End;

{$IFNDEF VirtualPascal}
  NumberCompare := Result;
{$ENDIF}
End;

Function BoolExpression (Const S: String): Char;
Var
  UpS : String;

Begin
  UpS := UpString (S);

  If UpS = 'TRUE' Then
    BoolExpression := '1'
  Else
    If UpS = 'FALSE' Then
      BoolExpression := '0'
    Else
      BoolExpression := ZeroOne [LogicExpression (S)];
End;

Function BoolCompare (Const S: String; Const V: GetVarRec): Boolean;
Begin
  If AsciiCount (S, ['='], '"') = 2
  Then
    BoolCompare := GetValueDirect (V) = BoolExpression (Trim (Copy (S,
      AsciiPosition (2, S, ['='], '"'), 255)))
  Else
    If (AsciiCount (S, ['<'], '"') = 2) And
       (AsciiCount (S, ['>'], '"') = 2)
    Then
      BoolCompare := GetValueDirect (V) <> BoolExpression (Trim (Copy (S,
        AsciiPosition (2, S, ['>'], '"'), 255)))
    Else
      Error (seExprError);
End;

Function LogicExpression (SStr: String): Boolean;

  Procedure EvaluateBoolean (Var S: String);
  Type
    Operation = (_or, _and, _xor);

  Var
    i, WC     : Integer;
    V         : GetVarRec;
    Res, ExpB : Boolean;
    Op        : Operation;
    Exp       : String;

  Begin
    S := UpString (S);
    WC := WordCount (S, SpaceOnly);
    i := 1;
    Res := False;
    Op := _or;

    While i <= WC Do
    Begin
      Exp := ExtractWord (i, S, SpaceOnly);
      Inc (i);

      ExpB := Exp = 'NOT';
      If ExpB Then
        If i <= WC Then
        Begin
          Exp := ExtractWord (i, S, SpaceOnly);
          Inc (i);
        End Else
        Begin
          Error (seIncorrectParams);
          Break;
        End;

      If (Exp <> '0') And (Exp <> '1') Then
      Begin
        GetVar (Exp, V);
        If V. Variable = Nil Then
        Begin
          Error (seUnknown);
          Break;
        End;

        If V. Variable^. VarType <> tvLogical Then
        Begin
          Error (seIncorrectParams);
          Break;
        End;

        Exp := GetValueDirect (V);
      End;

      ExpB := ExpB Xor (Exp <> '0');
      Case Op Of
         _or : Res := Res Or ExpB;
        _and : Res := Res And ExpB;
        _xor : Res := Res Xor ExpB;
      End;

      If i <= WC Then
      Begin
        Exp := ExtractWord (i, S, SpaceOnly);
        Inc (i);

        If Exp = 'OR' Then
          Op := _or
        Else
          If Exp = 'AND' Then
            Op := _and
          Else
            If Exp = 'XOR' Then
              Op := _xor
            Else
            Begin
              Error (seIncorrectParams);
              Break;
            End;
      End;
    End;

    S := ZeroOne [Res];
  End;

  Procedure CompileStr (Var S: String);
  Var
    V   : GetVarRec;
    Res : Boolean;

  Begin
    GetVar (Trim (ExtractWord (1, S, CompareDelims)), V);

    If V. Variable <> Nil Then
    Begin
      Case V. Variable^. VarType Of
         tvNumber : Res := NumberCompare (S, V);
        tvLogical : Res := BoolCompare (S, V);
         tvString : Res := StringCompare (S, V);
      End;
    End Else
    Begin
      Error (seUnknown);
      Res := False;
    End;

    S := ZeroOne [Res];
  End;

  Procedure EvaluateStr (Var S: String);
  Begin
    If StrContains (S, CompareDelims) Then CompileStr (S)
                                      Else EvaluateBoolean (S);
  End;

  Procedure BrackMax (Var Str: String);
  Const
    IdChars : Set Of Char = ['Ä','_','.','$','0'..'9','A'..'Z','a'..'z'];

  Var
    OBr, Cbr, Len, Index : Integer;
    SubStr               : String;

  Label
    Again;

  Begin

  Again:
    OBr := 0;
    CBr := 0;
    Len := Length (Str);
    Index := 1;

    While Index <= Len Do
    Begin
      Case Str [Index] Of
        '(' : OBr := Index;
        ')' : Begin
                CBr := Index;
                Break;
              End;
        '"' : Repeat
                Inc (Index);
              Until (Index > Len) Or (Str [Index] = '"');
      End;

      Inc (Index);
    End;

    If (OBr > 0) And (CBr > OBr) Then
    Begin
      If ((CBr < Len) And (Str [Cbr + 1] in IdChars)) Or
         ((Obr > 1) And (Str [Obr - 1] in IdChars)) Then
      Begin
        Error (seExprError);
        Exit;
      End;

      SubStr := Copy (Str, OBr + 1, CBr - OBr - 1);

      EvaluateStr (SubStr);
      If Not Finished Then
      Begin
        Delete (Str, OBr, (CBr - OBr) + 1);
        Insert (SubStr, Str, OBr);
        Goto Again;
      End;
    End
    Else
      If (CBr = 0) And (OBr = 0) Then EvaluateStr (Str)
                                 Else Error (seExprError);
  End;

Begin
  If AsciiPosCh (SStr, '(', '"') > 0 Then BrackMax (SStr)
                                     Else EvaluateStr (SStr);
  LogicExpression := SStr <> '0';
End;

Function StrExpression (Const S: String; Monolite: Boolean): String;
Var
  L         : LongInt;
  i, j, Len : Integer;
  V         : GetVarRec;
  Expr      : String;
{$IFNDEF VirtualPascal}
  Result    : String;
{$ENDIF}

Begin
  Result := '';
  i := 1;
  Len := Length (S);

  While i <= Len Do
    Case S [i] Of
      '"'      : Begin
                   Expr := ExtractWord (1, Copy (S, i + 1, 255), ['"']);
                   Result := Result + Expr;
                   Inc (i, Length (Expr) + 2);
                 End;

      ' ', '+' : Inc (i);

      ','      : If Monolite Then
                 Begin
                   Error (seIncorrectParams);
                   Break;
                 End
                 Else
                   Inc (i);
    Else
      j := i;
      While (i <= Len) And (S [i] in ExprChars) Do
        Inc (i);

      Expr := Copy (S, j, i - j);
      If Expr [1] = '#' Then
      Begin
        If Not MathExpression (Copy (Expr, 2, 255), L) Then
          Break;

        Result := Result + Char (L);
      End Else
      Begin
        GetVar (Expr, V);

        If V. Variable <> Nil Then
          Result := Result + GetValueDirect (V)
        Else
        Begin
          Error (seUnknown);
          Break;
        End;
      End;
    End;

{$IFNDEF VirtualPascal}
  StrExpression := Result;
{$ENDIF}
End;

Procedure ParseArray;
Const
  Delims: Set Of Char = ['(', ')', ' ', ',', '='];

Var
  L             : LongInt;
  i, j, Len, WC : Integer;
  S1            : String;

Begin
  WC := AsciiCount (S, Delims, '"');

  For i := 1 To WC Do
  Begin
    S1 := ExtractAscii (i, S, Delims, '"');
    j := Pos ('#', S1);

    If j > 1 Then
    Begin
      Delete (S1, 1, j);
      Len := Length (S1);

      If (Len > 0) And (S1 [1] in NumbersOnly) Then
        Continue;

      If Not MathExpression (S1, L) Then
      Begin
        Error (seInvalidIndex);
        Exit;
      End;

      Inc (j, AsciiPosition (i, S, Delims, '"'));
      Delete (S, j, Len);
      Insert (Long2Str (L), S, j);
    End;
  End;
End;

Procedure UpdateTransferVars;
Begin
  SystemChange := True;
  ChangeVar ('$SELK', Long2Str (Round (SizeOfAll / 1024)));
  ChangeVar ('$SELN', Long2Str (F2Transfer^. Count));
  ChangeVar ('$ULKB', Long2Str (R. UpLoadsK));
  ChangeVar ('$DLKB', Long2Str (R. DownLoadsK));
  ChangeVar ('$DLTK', Long2Str (R. TodayK));
  ChangeVar ('$ULDS', Long2Str (R. UpLoads));
  ChangeVar ('$DNLS', Long2Str (R. DownLoads));
  ChangeVar ('$ETTM', Long2Str (Round (EstimatedTransferTime (SizeOfAll,
    R. AvgCPS, GetConnectSpeed) / 60)));
  SystemChange := False;
  UpdateUserMacro;
End;

Procedure AddReservedVar (Const CSum: tChecksum);
Var
  i : LongInt;

Begin
  i := LongInt (ReservedVars^. SearchByChecksum (CSum));

  Case ReservedVarType (i) Of
    rvNAME : AddSingleVarByChecksum (CSum, tvString, R. Name, vReadOnly);
    rvSECR : AddSingleVarByChecksum (CSum, tvNumber, Long2Str (R. Security),
               vMacro);
    rvBAUD : AddSingleVarByChecksum (CSum, tvNumber, Long2Str (GetConnectSpeed),
               vReadOnly);
    rvLANG : AddSingleVarByChecksum (CSum, tvString, lang (laName), vMacro);
    rvEMUL : AddSingleVarByChecksum (CSum, tvString, EmuName [R. Emu], vMacro);
    rvALAS : AddSingleVarByChecksum (CSum, tvString, R. Alias, vMacro);
    rvFLGS : AddSingleVarByChecksum (CSum, tvString, R. Flags, vMacro);
    rvLINS : AddSingleVarByChecksum (CSum, tvNumber, Long2Str (R. Lines),
               vMacro);
    rvPSWD : AddSingleVarByChecksum (CSum, tvString, R. Password, vMacro);
    rvSYSO : AddSingleVarByChecksum (CSum, tvString, Cnf. SysOp, vReadOnly);
    rvBBSN : AddSingleVarByChecksum (CSum, tvString, Cnf. BbsName, vReadOnly);
    rvDATE : AddSingleVarByChecksum (CSum, tvString, FormattedCurrDT
               (Cnf. DateMask), 0);
    rvTIME : AddSingleVarByChecksum (CSum, tvString, StrTime,
               vBackgroundChange);
    rvFNUM : AddSingleVarByChecksum (CSum, tvNumber, Long2Str (R. FileArea),
               vMacro);
    rvFGRN : AddSingleVarByChecksum (CSum, tvNumber, Long2Str (R. FileGroup),
               vMacro);
    rvMNUM : AddSingleVarByChecksum (CSum, tvNumber, Long2Str (R. MsgArea),
               vMacro);
    rvMGRN : AddSingleVarByChecksum (CSum, tvNumber, Long2Str (R. MsgGroup),
               vMacro);
    rvFARE : AddSingleVarByChecksum (CSum, tvString, FileArea. Name, vReadOnly);
    rvFGRP : AddSingleVarByChecksum (CSum, tvString, FileGroup. Name,
               vReadOnly);
    rvMARE : AddSingleVarByChecksum (CSum, tvString, MsgArea. Name, vReadOnly);
    rvMGRP : AddSingleVarByChecksum (CSum, tvString, MsgGroup. Name, vReadOnly);
    rvFAGR : AddSingleVarByChecksum (CSum, tvNumber, Long2Str (fAreasGroup),
               vReadOnly);
    rvFAMT : AddSingleVarByChecksum (CSum, tvNumber, Long2Str (fAreasAmount),
               vReadOnly);
    rvFGAM : AddSingleVarByChecksum (CSum, tvNumber, Long2Str (ffGroupsAmount),
               vReadOnly);
    rvMAGR : AddSingleVarByChecksum (CSum, tvNumber, Long2Str (mAreasGroup),
               vReadOnly);
    rvMAMT : AddSingleVarByChecksum (CSum, tvNumber, Long2Str (mAreasAmount),
               vReadOnly);
    rvMGAM : AddSingleVarByChecksum (CSum, tvNumber, Long2Str (mmGroupsAmount),
               vReadOnly);
    rvKeybUp : AddSingleVarByChecksum (CSum, tvString, kbUp, vReadOnly);
    rvKeybDown : AddSingleVarByChecksum (CSum, tvString, kbDown, vReadOnly);
    rvKeybRight : AddSingleVarByChecksum (CSum, tvString, kbRight, vReadOnly);
    rvKeybLeft : AddSingleVarByChecksum (CSum, tvString, kbLeft, vReadOnly);
    rvKeybHome : AddSingleVarByChecksum (CSum, tvString, kbHome, vReadOnly);
    rvKeybEnd : AddSingleVarByChecksum (CSum, tvString, kbEnd, vReadOnly);
    rvFADlPath : AddSingleVarByChecksum (CSum, tvString, FileArea. DLPath,
                   vReadOnly);
    rvFAUlPath : AddSingleVarByChecksum (CSum, tvString, FileArea. ULPath,
                   vReadOnly);
    rvFAFileList : AddSingleVarByChecksum (CSum, tvString, FileArea. FileList,
                     vReadOnly);
    rvMAAddress : AddSingleVarByChecksum (CSum, tvString, Addr2Str
                     (MsgArea. Address), vReadOnly);
    rvMAOrigin : AddSingleVarByChecksum (CSum, tvString, MsgArea. Origin,
                   vReadOnly);
    rvMABase : AddSingleVarByChecksum (CSum, tvString, StrBase
                 [MsgArea. BaseType], vReadOnly);
    rvMAPath : AddSingleVarByChecksum (CSum, tvString, MsgArea. BasePath,
                 vReadOnly);
    rvSELN : AddSingleVarByChecksum (CSum, tvNumber, Long2Str
               (F2Transfer^. Count), vReadOnly);
    rvArgCount : AddSingleVarByChecksum (CSum, tvNumber, Long2Str
                   (ScriptParams^. Count), vReadOnly);
    rvCfgPath : AddSingleVarByChecksum (CSum, tvString, Cnf. Path, vReadOnly);
    rvCfgTempDir : AddSingleVarByChecksum (CSum, tvString, Cnf. TempDir,
                     vReadOnly);
    rvCfgFlagsDir : AddSingleVarByChecksum (CSum, tvString, Cnf. FlagsDir,
                      vReadOnly);
    rvCfgDoorDir : AddSingleVarByChecksum (CSum, tvString, Cnf. DoorInfoDir,
                     vReadOnly);
    rvCfgLangDir : AddSingleVarByChecksum (CSum, tvString, Cnf. LngPath,
                     vReadOnly);
    rvCfgLangTxtPath : AddSingleVarByChecksum (CSum, tvString, lang
                         (laTxtFiles), vReadOnly);
    rvCfgLangMnuPath : AddSingleVarByChecksum (CSum, tvString, lang (laMenus),
                         vReadOnly);
    rvCfgLangNews : AddSingleVarByChecksum (CSum, tvString, lang (laNews),
                      vReadOnly);
    rvCfgSaveTag : AddSingleVarByChecksum (CSum, tvString, Cnf. SaveTagPath,
                     vReadOnly);
    rvCfgPrivUL : AddSingleVarByChecksum (CSum, tvString, Cnf. PrivUploadsDir,
                     vReadOnly);
    rvCfgPort : AddSingleVarByChecksum (CSum, tvString, {$IFNDEF OS2} 'COM' +
                  Long2Str ({$ENDIF} Cnf. ComPort {$IFNDEF OS2}){$ENDIF},
                  vReadOnly);
    rvCfgLog : AddSingleVarByChecksum (CSum, tvString, Cnf. LogFile, vReadOnly);
    rvCfgChatLog : AddSingleVarByChecksum (CSum, tvString, Cnf. ChatLog,
                     vReadOnly);
    rvCfgTrcLog : AddSingleVarByChecksum (CSum, tvString, Cnf. TRCLog,
                    vReadOnly);
    rvCfgLogo : AddSingleVarByChecksum (CSum, tvString, Cnf. Logo, vReadOnly);
    rvEOMacro : AddSingleVarByChecksum (CSum, tvNumber, Long2Str (eoMacro),
                  vReadOnly);
    rvEOSlashCode : AddSingleVarByChecksum (CSum, tvNumber, Long2Str
                      (eoSlashCode), vReadOnly);
    rvEOColorCode : AddSingleVarByChecksum (CSum, tvNumber, Long2Str
                      (eoColorCode), vReadOnly);
    rvEODisable01 : AddSingleVarByChecksum (CSum, tvNumber, Long2Str
                      (eoDisable01), vReadOnly);
    rvPadStrRight : AddSingleVarByChecksum (CSum, tvNumber, Long2Str (LongInt
                      (PadStrRight)), vReadOnly);
    rvPadStrLeft : AddSingleVarByChecksum (CSum, tvNumber, Long2Str (LongInt
                     (PadStrLeft)), vReadOnly);
    rvPadStrCenter : AddSingleVarByChecksum (CSum, tvNumber, Long2Str (LongInt
                       (PadStrCenter)), vReadOnly);
    rvReadOnly : AddSingleVarByChecksum (CSum, tvNumber, Long2Str (LongInt
                   (ReadOnly)), vReadOnly);
    rvHidden : AddSingleVarByChecksum (CSum, tvNumber, Long2Str (LongInt
                 (Hidden)), vReadOnly);
    rvSysFile : AddSingleVarByChecksum (CSum, tvNumber, Long2Str (LongInt
                  (SysFile)), vReadOnly);
    rvVolumeID : AddSingleVarByChecksum (CSum, tvNumber, Long2Str (LongInt
                   (VolumeID)), vReadOnly);
    rvDirectory : AddSingleVarByChecksum (CSum, tvNumber, Long2Str (LongInt
                    (Directory)), vReadOnly);
    rvArchive : AddSingleVarByChecksum (CSum, tvNumber, Long2Str (LongInt
                  (Archive)), vReadOnly);
    rvAnyFile : AddSingleVarByChecksum (CSum, tvNumber, Long2Str (LongInt
                  (AnyFile)), vReadOnly);
    rvFNAM : AddSingleVarByChecksum (CSum, tvString, ExtractWord (1, R. Name,
               SpaceOnly), vReadOnly);
    rvLNAM : AddSingleVarByChecksum (CSum, tvString, Copy (R. Name, Pos (' ',
               R. Name) + 1, 255), vReadOnly);
    rvUSRS : AddSingleVarByChecksum (CSum, tvNumber, Long2Str (UsersNum),
               vReadOnly);
    rvPROT : AddSingleVarByChecksum (CSum, tvString, ProtocolDef. Name, vMacro);
    rvLOCA : AddSingleVarByChecksum (CSum, tvString, R. Location, vMacro);
    rvORGZ : AddSingleVarByChecksum (CSum, tvString, R. Organization, vMacro);
    rvDAOB : AddSingleVarByChecksum (CSum, tvString, Long2Date (R. BirthDate,
               Cnf. DateMask), vMacro);
    rvHPHN : AddSingleVarByChecksum (CSum, tvString, R. HPhone, vMacro);
    rvBPHN : AddSingleVarByChecksum (CSum, tvString, R. BPhone, vMacro);
    rvADR1 : AddSingleVarByChecksum (CSum, tvString, R. Address1, vMacro);
    rvADR2 : AddSingleVarByChecksum (CSum, tvString, R. Address2, vMacro);
    rvADR3 : AddSingleVarByChecksum (CSum, tvString, R. Address3, vMacro);
    rvCMNT : AddSingleVarByChecksum (CSum, tvString, R. Comment, vMacro);
    rvCALN : AddSingleVarByChecksum (CSum, tvNumber, Long2Str (R. NoCalls),
               vMacro);
    rvLDTE : AddSingleVarByChecksum (CSum, tvString, Long2Date (R. LastDate,
               Cnf. DateMask), vMacro);
    rvLTME : AddSingleVarByChecksum (CSum, tvString, Word2Time (R. LastTime),
               vMacro);
    rvFDTE : AddSingleVarByChecksum (CSum, tvString, Long2Date (R. FirstDate,
               Cnf. DateMask), vMacro);
    rvLFKB : AddSingleVarByChecksum (CSum, tvNumber, Long2Str (R. DailySize -
               R. TodayK - Trunc (SizeOfAll / 1024)), vMacro);
    rvOTME : AddSingleVarByChecksum (CSum, tvNumber, Long2Str (Round (TimeDiff (
               EnterTime, MidSec) / 60)), vReadOnly + vBackgroundChange);
    rvDLTK : AddSingleVarByChecksum (CSum, tvNumber, Long2Str (R. TodayK),
               vReadOnly);
    rvMSGP : AddSingleVarByChecksum (CSum, tvNumber, Long2Str (R. MsgsPosted),
               vReadOnly);
    rvSELK : AddSingleVarByChecksum (CSum, tvNumber, Long2Str (Round
               (SizeOfAll / 1024)), vReadOnly);
    rvULDS : AddSingleVarByChecksum (CSum, tvNumber, Long2Str (R. UpLoads),
               vMacro);
    rvDNLS : AddSingleVarByChecksum (CSum, tvNumber, Long2Str (R. DownLoads),
               vMacro);
    rvULKB : AddSingleVarByChecksum (CSum, tvNumber, Long2Str (R. UpLoadsK),
               vMacro);
    rvDLKB : AddSingleVarByChecksum (CSum, tvNumber, Long2Str (R. DownLoadsK),
               vMacro);
    rvETTM : AddSingleVarByChecksum (CSum, tvNumber, Long2Str (Round
               (EstimatedTransferTime (SizeOfAll, R. AvgCPS, GetConnectSpeed) /
               60)), vReadOnly);
    rvLIMK : AddSingleVarByChecksum (CSum, tvNumber, Long2Str (R. DailySize),
               vMacro);
    rvETME : AddSingleVarByChecksum (CSum, tvNumber, Long2Str (Round
               ((R. TotalTime - TimeDiff (EnterTime, MidSec)) / 60)), vMacro +
               vBackgroundChange);
    rvTTME : AddSingleVarByChecksum (CSum, tvNumber, Long2Str (Lim. Time),
               vReadOnly + vBackgroundChange);
    rvPROD : AddSingleVarByChecksum (CSum, tvString, NameVer, vReadOnly);
    rvFSED : AddSingleVarByChecksum (CSum, tvLogical, ZeroOne [R. FSEditor],
               vMacro);
    rvNODE : AddSingleVarByChecksum (CSum, tvNumber, Long2Str (BbsLine),
               vReadOnly);
    rvFRAM : AddSingleVarByChecksum (CSum, tvLogical, ZeroOne [R. Frames],
               vMacro);
    rvHKEY : AddSingleVarByChecksum (CSum, tvLogical, ZeroOne [R. HotKeys],
               vMacro);
    rvMORE : AddSingleVarByChecksum (CSum, tvLogical, ZeroOne [R. More],
               vMacro);
  End;
End;

Procedure UpdateMacro (Const CSum: tChecksum; Value: String);
Var
  i : LongInt;

Begin
  SystemChange := True;
  i := LongInt (VarMacros^. SearchByChecksum (CSum));

  Case VarMacroType (i) Of
    vmFNUM : Begin
               SetFileArea (Str2Long (Value));
               ChangeVar ('$FARE', FileArea. Name);
               ChangeVar ('FADLPATH', FileArea. DLPath);
               ChangeVar ('FAULPATH', FileArea. ULPath);
               ChangeVar ('FAFILELIST', FileArea. FileList);
             End;
    vmMNUM : Begin
               SetMsgArea (Str2Long (Value));
               ChangeVar ('$MARE', MsgArea. Name);
               ChangeVar ('MAADDRESS', Addr2Str (MsgArea. Address));
               ChangeVar ('MAORIGIN', MsgArea. Origin);
               ChangeVar ('MABASE', StrBase [MsgArea. BaseType]);
               ChangeVar ('MAPATH', MsgArea. BasePath);
             End;
    vmFGRN : Begin
               SetFileGroup (Str2Long (Value));
               ChangeVar ('$FGRP', FileGroup. Name);
               ChangeVar ('$FAGR', Long2Str (fAreasGroup));
             End;
    vmMGRN : Begin
               SetMsgGroup (Str2Long (Value));
               ChangeVar ('$MGRP', MsgGroup. Name);
               ChangeVar ('$MAGR', Long2Str (mAreasGroup));
             End;
    vmSECR : Begin
               i := Str2Long (Value);
               If i <> R. Security Then
               Begin
                 R. Security := i;
                 SetSecurity;
                 ChangeVar ('$LFKB', Long2Str (R. DailySize - (R. TodayK +
                   Trunc (SizeOfAll / 1024))));
                 ChangeVar ('$LIMK', Long2Str (R. DailySize));
               End;
             End;
    vmFLGS : R. Flags := Value;
    vmLINS : R. Lines := Str2Long (Value);
    vmLANG : If ReadLanguage (Language, DefaultName (Value, 'LNG',
                Cnf. LngPath))
             Then
               R. Lang := Value;
    vmEMUL : Begin
               Value := UpString (Value);
               If Value = 'ANSI' Then R. Emu := teAnsi Else
               If Value = 'AVATAR' Then R. Emu := teAvatar Else
               If Value = 'TTY' Then R. Emu := teTty;
             End;
    vmLIMK : R. DailySize := Str2Long (Value);
    vmLFKB : R. DailySize := Str2Long (Value) + R. TodayK +
               Trunc (SizeOfAll / 1024);
    vmULDS : R. UpLoads := Str2Long (Value);
    vmDNLS : R. DownLoads := Str2Long (Value);
    vmULKB : R. UpLoadsK := Str2Long (Value);
    vmDLKB : R. DownLoadsK := Str2Long (Value);
    vmETME : R. TotalTime := Str2Long (Value) * 60 + TimeDiff (EnterTime, MidSec);
    vmPSWD : R. Password := Value;
    vmLOCA : R. Location := Value;
    vmORGZ : R. Organization := Value;
    vmALAS : R. Alias := Value;
    vmHPHN : R. HPhone := Value;
    vmBPHN : R. BPhone := Value;
    vmADR1 : R. Address1 := Value;
    vmADR2 : R. Address2 := Value;
    vmADR3 : R. Address3 := Value;
    vmCMNT : R. Comment := Value;
    vmCALN : R. NoCalls := Str2Long (Value);
    vmDAOB : R. BirthDate := Date2Long (ReformatDate (Value, Cnf. DateMask,
               DefaultDateMask));
    vmFDTE : R. FirstDate := Date2Long (ReFormatDate (Value, Cnf. DateMask,
               DefaultDateMask));
    vmLDTE : R. LastDate := Date2Long (ReFormatDate (Value, Cnf. DateMask,
               DefaultDateMask));
    vmLTME : R. LastTime := Time2Word (Value);
    vmPROT : If Length (Value) > 0 Then
             Begin
               SetProtocol (Value [1]);
               ChangeVar ('$PROT', ProtocolDef. Name);
             End;
    vmFSED : R. FSEditor := Value <> '0';
    vmFRAM : R. Frames := Value <> '0';
    vmHKEY : R. HotKeys := Value <> '0';
    vmMORE : R. More := Value <> '0';
  End;

  SystemChange := False;
  UpdateUserMacro;
End;

Function LoadScript: Boolean;

  Procedure StartupScript;
  Var
    i, j : Integer;

  Begin
    j := WordCount (S, SpaceOnly);
    ScriptParams := New (PNotSortedCollection, Init (j, 1));

    For i := 1 To j Do
      ScriptParams^. Insert (NewStr (ExtractWord (i, S, SpaceOnly)));

    Variables := New (PVariablesTable, Init (VariablesTableBits));
    Procedures := New (PProceduresTable, Init (ProceduresTableBits));
    Files := New (PFileCollection, Init (0, FilesDelta));
    FileFinders := New (PFileFindCollection, Init (0, FileFindDelta));
    ProcStack := New (PProcStackCollection, Init (0, ProcStackDelta));

    If Cnf. DebugFiles Then
      SetDebugFile (fName);

    Finished := False;
    SystemChange := False;
  End;

Type
  PLabelRec = ^tLabelRec;
  tLabelRec = Record
    LSum : tChecksum;
    TL   : tScriptLabel;
  End;

  PVarRec = ^tVarRec;
  tVarRec = Record
    rvType : ResourceVarType;
    VType  : tVariableType;
    VSum   : tCheckSum;
    VCount : System. Word;
  End;

  Function LoadRawScript: Boolean;
  Type
    PDefineRec = ^DefineRec;
    DefineRec = Record
      WhatStr, ToStr : PString;
    End;

  Var
    Defines : PCollection;
    PD      : PDefineRec;
    F       : Text;
    BufStr  : String;
    FileBuf : FileBufArr;

    Function ReadScriptString (Var S, S1: String): Boolean;
    Var
      i, P : Integer;

    Begin
      ReadScriptString := False;

      Repeat
        If BufStr = '' Then
        Begin
          If EoF (F) Then
            Exit;

          ReadLn (F, S);
          Inc (LineNum);

          P := AsciiPos (S, '//', '"');
          If P > 0 Then
            SetLength (S, P - 1);
          S := Trim (PlaceSubStr (S, #9, ReplaceTabSpaces));

          If UpString (Copy (S, 1, 8)) = '#DEFINE ' Then
          Begin
            P := WordCount (S, SpaceOnly);
            If P >= 3 Then
            Begin
              S1 := ExtractWord (2, S, SpaceOnly);
              S := Copy (S, WordPosition (3, S, SpaceOnly), 255);

              For i := 0 To Defines^. Count-1 Do
                With PDefineRec (Defines^. At (i))^ Do
                Begin
                  PlaceSubStrP (S1, WhatStr^, ToStr^);
                  PlaceSubStrP (S, WhatStr^, ToStr^);
                End;

              New (PD);
              PD^. WhatStr := NewStr (S1);
              PD^. ToStr := NewStr (S);
              Defines^. Insert (PD);
            End;

            Continue;
          End;

          If AsciiPosCh (S, ';', '"') > 0 Then
          Begin
            P := Length (S);
            While (P > 0) And (S [P] in [';', ' ']) Do
              Dec (P);
            SetLength (S, P);

            P := 1;
            While (P <= Length (S)) And (S [P] in [';', ' ']) Do
              Inc (P);
            Delete (S, 1, P - 1);

            If AsciiCount (S, [';'], '"') > 1 Then
            Begin
              BufStr := Trim (Copy (S, AsciiPosition (2, S, [';'], '"'), 255));
              S := Trim (ExtractAscii (1, S, [';'], '"'));
            End;
          End;
        End
        Else
          If AsciiCount (BufStr, [';'], '"') > 1 Then
          Begin
            S := Trim (ExtractAscii (1, BufStr, [';'], '"'));
            Delete (BufStr, 1, AsciiPosition (2, BufStr, [';'], '"') - 1);
          End Else
          Begin
            S := Trim (BufStr);
            BufStr := '';
          End;

        If S <> '' Then
          Break;
      Until False;

      For i := 0 To Defines^. Count-1 Do
        With PDefineRec (Defines^. At (i))^ Do
          PlaceSubStrP (S, WhatStr^, ToStr^);

      S1 := UpString (S);
      ReadScriptString := True;
    End;

  Type
    ProcHeader = Record
      rbType : ResourceBlockType;
      CSum   : tChecksum;
      Count  : LongInt;
    End;

    tStringHdr = Record
      aData  : Pointer;
      Line   : System. Word;
      Action : tActionType;
      Flags  : Byte;
    End;

  Var
    L, ICount, ICPos : LongInt;
    RS               : PMemoryStream;
    ActionData       : Pointer;
    HashBlock        : PHashTableBlock;
    a, i             : Integer;
    w                : System. Word;
    CSum             : tChecksum;
    TP               : TProcedure;
    TL               : TScriptLabel;
    VarType          : tVariableType;
    Action           : tActionType;
    State            : tScriptState;
    rbType           : ResourceBlockType;
    PHdr             : ProcHeader;
    PStrHdr          : tStringHdr;
    PLabel           : tLabelRec;
    VR               : tVarRec;
    WC, aFlags       : Byte;
    ArrayVar         : Boolean;
    S1, T, ProcName  : String;

  Begin
    Assign (F, fName);
    SetTextBuf (F, FileBuf, FileBufSize);
    ReSet (F);

    If IOResult <> 0 Then
    Begin
      LoadRawScript := False;
      Exit;
    End;

    StartupScript;
    Defines := New (PCollection, Init (0, 8));
    RS := New (PMemoryStream, Init (0, 0));
    LineNum := 0;
    EndCount1 := 0;
    State := psNone;
    BufStr := '';

    While Not Finished And ReadScriptString (S, S1) Do
    Begin
      If S1 = 'END' Then
        Case State Of

        psProcedure : Begin
                        ProcText^. Insert (NewScriptString ('', LineNum,
                          aCommand, 0, Pointer (CommandTypeGroup (_End,
                          _Lang))));
                        If EndCount1 > 0 Then
                          Dec (EndCount1)
                        Else
                        Begin
                          GetChecksum (ProcName, PHdr. CSum);

                          If Procedures^. SearchByChecksum (PHdr. CSum) = Nil
                          Then
                          Begin
                            TP. Text := ProcText;
                            TP. Labels := ProcLabels;
                            Procedures^. InsertByChecksum (PHdr. CSum, @TP);

                            L := ProcText^. Count;
                            PHdr. rbType := rbProc;
                            PHdr. Count := L;
                            RS^. Write (PHdr, SizeOf (PHdr));

                            For a := 0 To L-1 Do
                              With PScriptString (ProcText^. At (a))^ Do
                              Begin
                                PStrHdr. aData := ActionData;
                                PStrHdr. Line := Line;
                                PStrHdr. Action := Action;
                                PStrHdr. Flags := Flags;
                                RS^. Write (PStrHdr, SizeOf (PStrHdr));
                                RS^. Write (Str, Length (Str) + 1);
                              End;

                            ICPos := RS^. GetPos;
                            L := 0;
                            RS^. Write (L, SizeOf (L));

                            HashBlock := ProcLabels^. RootBlock;

                            While HashBlock <> Nil Do
                            Begin
                              With HashBlock^ Do
                              Begin
                                For a := 0 To Used-1 Do
                                  With Items [a] Do
                                  Begin
                                    PLabel. LSum := NameCSum;
                                    PLabel. TL := PScriptLabel (Data)^;
                                    RS^. Write (PLabel, SizeOf (PLabel));
                                  End;

                                Inc (L, Used);
                              End;

                              HashBlock := HashBlock^. Next;
                            End;

                            If L > 0 Then
                            Begin
                              RS^. Seek (ICPos);
                              RS^. Write (L, SizeOf (L));
                              RS^. Seek (RS^. GetSize);
                            End;

                            State := psNone;
                          End
                          Else
                            Error (seDuplicate);
                        End;

                        Continue;
                      End;

        psVariables : Begin
                        State := psNone;
                        RS^. Seek (ICPos);
                        RS^. Write (ICount, SizeOf (ICount));
                        RS^. Seek (RS^. GetSize);
                        Continue;
                      End;

             psNone : Begin
                        Error (seUnexpEnd);
                        Break;
                      End;
        End;

      Case State Of

        psProcedure : Begin
                        L := LongInt (Commands^. Search (ExtractWord (1, S1,
                          [' ', '(', ')', '='])));
                        If L > 0 Then
                        Begin
                          Dec (L);
                          Action := aCommand;
                          ActionData := Pointer (L);
                          Case CommandType (L And CommandTypeMask) Of
                              _If : Begin
                                      Inc (EndCount1);
                                      S := Copy (S, WordPosition (2, S,
                                        SpaceOnly), 255);
                                    End;
                            _Goto : S := ExtractWord (2, S1, SpaceOnly);
                          Else
                            S := GetParams (S);
                          End;
                        End Else
                        Begin
                          T := ExtractAscii (1, S1, SpaceOnly, '"');
                          If T [Length (T)] = ':' Then
                          Begin
                            SetLength (T, Length (T) - 1);
                            GetChecksum (T, CSum);

                            If ProcLabels^. SearchByChecksum (CSum) = Nil Then
                            Begin
                              TL. Line := ProcText^. Count;
                              TL. EndCount := EndCount1;
                              ProcLabels^. InsertByChecksum (CSum, @TL);
                            End
                            Else
                              Error (seDuplicate);

                            Continue;
                          End;

                          If AsciiPosCh (S, '=', '"') > 0 Then
                            Action := aExpression
                          Else
                            Action := aUnknown;

                          ActionData := Nil;
                        End;

                        If AsciiPosCh (S, '#', '"') > 0 Then
                          aFlags := aFixIndexes
                        Else
                          aFlags := 0;

                        ProcText^. Insert (NewScriptString (S, LineNum, Action,
                          aFlags, ActionData));
                      End;

        psVariables : Begin
                        If Pos (':', S1) = 0 Then
                        Begin
                          Error (seColonMissing);
                          Break;
                        End;

                        T := Trim (ExtractWord (2, S1, [':']));
                        S1 := Trim (ExtractWord (1, S1, [':']));
                        WC := WordCount (S1, CommaOnly);
                        ArrayVar := False;

                        If WordCount (T, SpaceOnly) > 1 Then
                          If ExtractWord (2, T, SpaceOnly) = 'ARRAY' Then
                          Begin
                            w := Str2Long (ExtractWord (2, T, ['[', ']']));
                            T := ExtractWord (1, T, SpaceOnly);
                            ArrayVar := True;
                          End;

                        If T = 'NUMBER' Then VarType := tvNumber Else
                        If T = 'STRING' Then VarType := tvString Else
                        If T = 'LOGICAL' Then VarType := tvLogical Else
                        Begin
                          Error (seUnknown);
                          Break;
                        End;

                        VR. VType := VarType;

                        If Not ArrayVar Then
                        Begin
                          VR. rvType := rvSingle;

                          For a := 1 To WC Do
                          Begin
                            GetChecksum (Trim (ExtractWord (a, S1, CommaOnly)),
                              VR. VSum);
                            If AddSingleVarByChecksum (VR. VSum, VarType, '', 0)
                            Then
                              RS^. Write (VR, SizeOf (VR) - SizeOf (VR. VCount))
                            Else
                            Begin
                              Error (seDuplicate);
                              Break;
                            End;
                          End;
                        End Else
                        Begin
                          VR. rvType := rvArray;
                          VR. VCount := w;

                          For a := 1 To WC Do
                          Begin
                            GetChecksum (Trim (ExtractWord (a, S1, CommaOnly)),
                              VR. VSum);
                            If AddArrayVarByChecksum (VR. VSum, VarType, 0, w)
                            Then
                              RS^. Write (VR, SizeOf (VR))
                            Else
                            Begin
                              Error (seDuplicate);
                              Break;
                            End;
                          End;
                        End;

                        Inc (ICount, WC);
                      End;

        psNone      : Begin
                        If S1 = 'PROGRAM' Then
                          ProcName := MainTag
                        Else
                          If Copy (S1, 1, 10) = 'PROCEDURE ' Then
                            ProcName := Trim (Copy (S1, 11, 255))
                          Else
                          Begin
                            If S1 = 'VARIABLES' Then
                            Begin
                              State := psVariables;
                              rbType := rbVar;
                              ICount := 0;
                              RS^. Write (rbType, SizeOf (rbType));
                              ICPos := RS^. GetPos;
                              RS^. Write (ICount, SizeOf (ICount));
                            End
                            Else
                              Error (seUnknown);

                            Continue;
                          End;

                        ProcText := New (PProcTextCollection,
                          Init (ProcTextDelta, ProcTextDelta));
                        ProcLabels := New (PLabelsTable, Init (LabelsTableBits));
                        State := psProcedure;
                      End;
      End;
    End;

    Close (F);

    If Not Finished And (RS^. Status = stOk) Then
      PutStreamResource (fName, RS);
    Dispose (RS, Done);

    For i := 0 To Defines^. Count-1 Do
    Begin
      PD := Defines^. At (i);
      If PD <> Nil Then
      Begin
        DisposeStr (PD^. WhatStr);
        DisposeStr (PD^. ToStr);
        Dispose (PD);
      End;
    End;

    Defines^. DeleteAll;
    Dispose (Defines, Done);

    LoadRawScript := True;
  End;

  Procedure LoadCachedScript (Block: Pointer; Len: LongInt);
  Type
    PStringRec = ^tStringRec;
    tStringRec = Record
      aData  : Pointer;
      Line   : System. Word;
      Action : tActionType;
      Flags  : Byte;
      Str    : String;
    End;

  Var
    P, EndBlock : ^Byte;
    PSum        : PChecksum;
    i           : LongInt;
    TP          : TProcedure;
    rbType      : ResourceBlockType;

  Begin
    StartupScript;
    P := Block;
    EndBlock := P;
    Inc (EndBlock, Len);

    While P <> EndBlock Do
    Begin
      rbType := ResourceBlockType (P^);
      Inc (P, SizeOf (rbType));

      Case rbType Of

        rbProc : Begin
                   PSum := PCheckSum (P);
                   Inc (P, SizeOf (tChecksum));
                   i := PLongInt (P)^;
                   Inc (P, SizeOf (i));
                   TP. Text := New (PProcTextCollection, Init (i, 0));

                   While i > 0 Do
                   Begin
                     With PStringRec (P)^ Do
                     Begin
                       TP. Text^. Insert (NewScriptString (Str, Line, Action,
                         Flags, aData));
                       Inc (P, SizeOf (tStringRec) - SizeOf (Str) +
                         Length (Str) + 1);
                     End;

                     Dec (i);
                   End;

                   i := PLongInt (P)^;
                   Inc (P, SizeOf (i));
                   TP. Labels := New (PLabelsTable, Init (LabelsTableBits));

                   While i > 0 Do
                   Begin
                     With PLabelRec (P)^ Do
                       TP. Labels^. InsertByChecksum (LSum, @TL);

                     Inc (P, SizeOf (tLabelRec));
                     Dec (i);
                   End;

                   Procedures^. InsertByChecksum (PSum^, @TP);
                 End;

         rbVar : Begin
                   i := PLongInt (P)^;
                   Inc (P, SizeOf (i));

                   While i > 0 Do
                   Begin
                     With PVarRec (P)^ Do
                       Case rvType Of
                         rvSingle : Begin
                                      AddSingleVarByChecksum (VSum, VType, '',
                                        0);
                                      Inc (P, SizeOf (tVarRec) - SizeOf
                                        (VCount));
                                    End;
                          rvArray : Begin
                                      AddArrayVarByChecksum (VSum, VType, 0,
                                        VCount);
                                      Inc (P, SizeOf (tVarRec));
                                    End;
                       End;

                     Dec (i);
                   End;
                 End;
      End;
    End;

    FreeMem (Block, Len);
  End;

Var
  Block : Pointer;
  Len   : LongInt;
  i     : Integer;

Begin
  i := WordPosition (2, fName, SpaceOnly);
  If i > 0 Then S := Copy (fName, i, 255)
           Else S := '';
  fName := DefaultName (ExtractWord (1, fName, SpaceOnly), 'trs',
    lang (laTxtFiles));

  Block := GetBlockResource (fName, Len);
  If Block <> Nil Then
  Begin
    LoadCachedScript (Block, Len);
    LoadScript := True;
  End
  Else
    LoadScript := LoadRawScript;
End;

Procedure SkipLogic;
Var
  StartScriptString : PScriptString;
  i                 : LongInt;
  EndCount          : Integer;

Begin
  StartScriptString := CurrScriptString;
  StartScriptString^. Flags := StartScriptString^. Flags Or aContentValid;
  EndCount := 0;

  While ProcLine < ProcText^. Count Do
  Begin
    CurrScriptString := ProcText^. At (ProcLine);
    Inc (ProcLine);

    If (Length (CurrScriptString^. Str) > 0) And
       (CurrScriptString^. Str [1] = '&') Then
    Begin
      LineNum := CurrScriptString^. Line;
      S := Trim (StrExpression (Copy (CurrScriptString^. Str, 2, 255), False));
      i := LongInt (Commands^. Search (UpString (ExtractWord (1, S,
        [' ', '(', ')', '=']))));
      If i > 0 Then
      Begin
        CurrScriptString^. Action := aCommand;
        CurrScriptString^. ActionData := Pointer (i - 1);
      End;
      StartScriptString^. Flags := StartScriptString^. Flags And Not
        aContentValid;
    End;

    If CurrScriptString^. Action = aCommand Then
      Case CommandType (LongInt (CurrScriptString^. ActionData) And
                        CommandTypeMask) Of
         _End : If EndCount > 0 Then
                  Dec (EndCount)
                Else
                  Break;
        _Else : If EndCount = 0 Then
                  Break;
          _If : Inc (EndCount);
      End;
  End;

  StartScriptString^. ContentData := Pointer (ProcLine);
End;

Procedure SetConvertedData (Var V: GetVarRec; Const S: String);
Begin
  Case V. Variable^. VarType Of
     tvString : ChangeVarDirect (V, S);
     tvNumber : ChangeVarDirect (V, Long2Str (Str2Long (S)));
    tvLogical : ChangeVarDirect (V, ZeroOne [Str2Long (S) <> 0]);
  End;
End;

Procedure GetUserNames (Const NameOrAlias: String; Var Name, Alias: GetVarRec);
Var
  TmpR : tUser;

Begin
  FillChar (TmpR, SizeOf (TmpR), 0);
  GetUser (NameOrAlias, TmpR, True);
  ChangeVarDirect (Name, TmpR. Name);
  ChangeVarDirect (Alias, TmpR. Alias);
End;

Var
  PP               : PProcedure;
  PS               : PProcStackElement;
  PF               : PFile;
  PFF              : PFileFind;
  i, L, TagFileNum : LongInt;
  a, b             : Integer;
  V, V1            : GetVarRec;
  tB               : Boolean;
  WriteMode        : Byte;
  CT               : CommandType;

Label
  ProcessScriptString;

Begin
  If Not LoadScript Then
  Begin
    tExecScript := False;
    Exit;
  End;

  PP := Procedures^. Search (MainTag);
  If PP = Nil Then
    Error (seWrongEndCount)
  Else
  Begin
    WriteMode := eoMacro + eoCodes;

    ProcLine := 0;
    EndCount1 := 0;
    ProcText := PP^. Text;
    ProcLabels := PP^. Labels;

    While Not Finished And (ProcLine < ProcText^. Count) Do
    Begin
      CurrScriptString := ProcText^. At (ProcLine);
      Inc (ProcLine);
      LineNum := CurrScriptString^. Line;
      S := CurrScriptString^. Str;

      If (Length (S) > 0) And (S [1] = '&') Then
      Begin
        S := Trim (StrExpression (Copy (S, 2, 255), False));
        i := LongInt (Commands^. Search (UpString (ExtractWord (1, S,
          [' ', '(', ')', '=']))));

        If i > 0 Then
        Begin
          Dec (i);
          CurrScriptString^. Action := aCommand;
          CurrScriptString^. ActionData := Pointer (i);
          Case CommandType (i And CommandTypeMask) Of
              _If : S := Copy (S, WordPosition (2, S, SpaceOnly), 255);
            _Goto : S := UpString (ExtractWord (2, S, SpaceOnly));
          Else
            S := GetParams (S);
          End;
        End
        Else
          If AsciiPosCh (S, '=', '"') > 0 Then
            CurrScriptString^. Action := aExpression
          Else
            CurrScriptString^. Action := aUnknown;

        If AsciiPosCh (S, '#', '"') > 0 Then
          CurrScriptString^. Flags := aFixIndexes
        Else
          CurrScriptString^. Flags := 0;
      End;

      If (CurrScriptString^. Flags And aFixIndexes) <> 0 Then
        ParseArray;

    ProcessScriptString:
      Case CurrScriptString^. Action Of
      aCommand:
        Begin
          i := LongInt (CurrScriptString^. ActionData);
          CT := CommandType (i And CommandTypeMask);

          Case CommandGroup (i Shr CommandGroupShift) Of
            _Lang:
              Case CT Of

                _End:
                If EndCount1 > 0 Then
                  Dec (EndCount1)
                Else
                  If ProcStack^. Count > 0 Then
                  Begin
                    PS := ProcStack^. At (ProcStack^. Count-1);
                    ProcStack^. AtDelete (ProcStack^. Count-1);
                    ProcText := PS^. ProcText;
                    ProcLabels := PS^. ProcLabels;
                    ProcLine := PS^. ProcLine;
                    EndCount1 := PS^. EndCount;
                    Dispose (PS);
                  End
                  Else
                    Break;

                _If:
                If LogicExpression (S) Then
                  Inc (EndCount1)
                Else
                  If Not Finished Then
                  Begin
                    If (CurrScriptString^. Flags And aContentValid) <> 0 Then
                    Begin
                      ProcLine := LongInt (CurrScriptString^. ContentData);
                      CurrScriptString := ProcText^. At (ProcLine - 1);
                    End
                    Else
                      SkipLogic;

                    If CommandType (CurrScriptString^. ActionData) = _Else Then
                      Inc (EndCount1);
                  End;

                _Else:
                  If EndCount1 > 0 Then
                  Begin
                    Dec (EndCount1);
                    If (CurrScriptString^. Flags And aContentValid) <> 0 Then
                      ProcLine := LongInt (CurrScriptString^. ContentData)
                    Else
                      SkipLogic;
                  End
                  Else
                    Error (seUnknown);

                _Goto:
                Begin
                  If (CurrScriptString^. Flags And aContentValid) = 0 Then
                  Begin
                    CurrScriptString^. ContentData := ProcLabels^. Search (S);
                    If CurrScriptString^. ContentData = Nil Then
                    Begin
                      Error (seLabelNotFound);
                      Break;
                    End;
                    CurrScriptString^. Flags := CurrScriptString^. Flags Or
                      aContentValid;
                  End;

                  With PScriptLabel (CurrScriptString^. ContentData)^ Do
                  Begin
                    ProcLine := Line;
                    EndCount1 := EndCount;
                  End;
                End;

                _Exit:
                Begin
                  ProcLine := ProcText^. Count - 1;
                  EndCount1 := 0;
                End;

                _Halt: Break;
              End;

            _String:
              Case CT Of
                _GetStringLength:
                Begin
                  GetVar (Trim (ExtractWord (1, S, CommaOnly)), V);
                  GetVar (Trim (ExtractWord (2, S, CommaOnly)), V1);

                  If (V. Variable = Nil) Or (V1. Variable = Nil) Or
                     (V1. Variable^. VarType <> tvNumber)
                  Then
                    Error (seIncorrectParams)
                  Else
                    ChangeVarDirect (V1, Long2Str (Length (GetValueDirect (V))));
                End;

                _StrPos:
                Begin
                  Par1 := StrExpression (Trim (ExtractAscii (1, S, CommaOnly,
                    '"')), True);
                  Par2 := StrExpression (Trim (ExtractAscii (2, S, CommaOnly,
                    '"')), True);
                  GetVar (Trim (ExtractAscii (3, S, CommaOnly, '"')), V);

                  If (V. Variable = Nil) Or (V. Variable^. VarType <> tvNumber)
                  Then
                    Error (seIncorrectParams)
                  Else
                    ChangeVarDirect (V, Long2Str (Pos (Par1, Par2)));
                End;

                _SubString:
                Begin
                  Parameter := StrExpression (Trim (ExtractAscii (1, S,
                    CommaOnly, '"')), True);
                  GetVar (Trim (ExtractAscii (4, S, CommaOnly, '"')), V);

                  If V. Variable = Nil Then
                    Error (seUnknown)
                  Else
                    If (V. Variable^. VarType <> tvString) Or
                       Not MathExpression (Trim (ExtractAscii (2, S, CommaOnly,
                           '"')), i) Or
                       Not MathExpression (Trim (ExtractAscii (3, S, CommaOnly,
                           '"')), L)
                    Then
                      Error (seIncorrectParams)
                    Else
                      ChangeVarDirect (V, Copy (Parameter, i, L));
                End;

                _StrDelete:
                Begin
                  Parameter := StrExpression (Trim (ExtractAscii (1, S,
                    CommaOnly, '"')), True);
                  GetVar (Trim (ExtractAscii (4, S, CommaOnly, '"')), V);

                  If V. Variable = Nil Then
                    Error (seUnknown)
                  Else
                    If (V. Variable^. VarType <> tvString) Or
                       Not MathExpression (Trim (ExtractAscii (2, S, CommaOnly,
                           '"')), i) Or
                       Not MathExpression (Trim (ExtractAscii (3, S, CommaOnly,
                           '"')), L)
                    Then
                      Error (seIncorrectParams)
                    Else
                    Begin
                      Delete (Parameter, i, L);
                      ChangeVarDirect (V, Parameter);
                    End;
                End;

                _StrReplace:
                Begin
                  Parameter := StrExpression (Trim (ExtractAscii (1, S,
                    CommaOnly, '"')), True);
                  Par1 := StrExpression (Trim (ExtractAscii (2, S, CommaOnly,
                    '"')), True);
                  Par2 := StrExpression (Trim (ExtractAscii (3, S, CommaOnly,
                    '"')), True);
                  GetVar (Trim (ExtractAscii (4, S, CommaOnly, '"')), V);

                  If V. Variable = Nil Then
                    Error (seUnknown)
                  Else
                    If (V. Variable^. VarType <> tvString) Then
                      Error (seIncorrectParams)
                    Else
                      ChangeVarDirect (V, PlaceSubStr (Parameter, Par1, Par2));
                End;

                _Val:
                Begin
                  Par1 := StrExpression (Trim (ExtractAscii (1, S, CommaOnly,
                    '"')), True);
                  GetVar (Trim (ExtractAscii (2, S, CommaOnly, '"')), V);

                  If (V. Variable = Nil) Or (V. Variable^. VarType <> tvNumber)
                  Then
                    Error (seUnknown)
                  Else
                    ChangeVarDirect (V, Long2Str (Str2Long (Par1)));
                End;

                _Ord:
                Begin
                  Par1 := StrExpression (Trim (ExtractAscii (1, S, CommaOnly,
                    '"')), True);
                  GetVar (Trim (ExtractAscii (2, S, CommaOnly, '"')), V);

                  If (V. Variable = Nil) Or (V. Variable^. VarType <> tvNumber)
                  Then
                    Error (seUnknown)
                  Else
                    ChangeVarDirect (V, Long2Str (Ord (Par1 [1])));
                End;

                _TrimStr:
                Begin
                  GetVar (Trim (ExtractAscii (2, S, CommaOnly, '"')), V);

                  If V. Variable = Nil Then
                    Error (seUnknown)
                  Else
                    If (V. Variable^. VarType <> tvString) Then
                      Error (seIncorrectParams)
                    Else
                      ChangeVarDirect (V, Trim (StrExpression (Trim
                        (ExtractAscii (1, S, CommaOnly, '"')), True)));
                End;

                _PadStr:
                Begin
                  GetVar (Trim (ExtractAscii (4, S, CommaOnly, '"')), V);

                  If V. Variable = Nil Then
                    Error (seUnknown)
                  Else
                    If (V. Variable^. VarType <> tvString) Or
                       Not MathExpression (Trim (ExtractAscii (2, S, CommaOnly,
                           '"')), i) Or
                       Not MathExpression (Trim (ExtractAscii (3, S, CommaOnly,
                           '"')), L)
                    Then
                      Error (seIncorrectParams)
                    Else
                    Begin
                      Parameter := StrExpression (Trim (ExtractAscii (1, S,
                        CommaOnly, '"')), True);
                      S := StrExpression (Trim (ExtractAscii (5, S, CommaOnly,
                        '"')), True);
                      If S = '' Then
                        S := ' ';

                      Case PadStrMode (L) Of
                         PadStrRight : Parameter := PadCh (Parameter, S [1], i);
                          PadStrLeft : Parameter := LeftPadCh (Parameter, S [1],
                                         i);
                        PadStrCenter : Parameter := CenterCh (Parameter, S [1],
                                         i);
                      End;

                      ChangeVarDirect (V, Parameter);
                    End;
                End;

                _WordCount:
                Begin
                  GetVar (Trim (ExtractAscii (3, S, CommaOnly, '"')), V);

                  If V. Variable = Nil Then
                    Error (seUnknown)
                  Else
                    If V. Variable^. VarType <> tvNumber Then
                      Error (seIncorrectParams)
                    Else
                    Begin
                      Str2Set (StrExpression (Trim (ExtractAscii (2, S,
                        CommaOnly, '"')), True), WorkSet);
                      ChangeVarDirect (V, Long2Str (WordCount (StrExpression
                        (Trim (ExtractAscii (1, S, CommaOnly, '"')), True),
                        WorkSet)));
                    End;
                End;

                _ExtractWord:
                Begin
                  GetVar (Trim (ExtractAscii (4, S, CommaOnly, '"')), V);

                  If V. Variable = Nil Then
                    Error (seUnknown)
                  Else
                    If (V. Variable^. VarType <> tvString) Or
                       Not MathExpression (Trim (ExtractAscii (1, S, CommaOnly,
                           '"')), i)
                    Then
                      Error (seIncorrectParams)
                    Else
                    Begin
                      Str2Set (StrExpression (Trim (ExtractAscii (3, S,
                        CommaOnly, '"')), True), WorkSet);
                      ChangeVarDirect (V, ExtractWord (i, StrExpression (Trim
                        (ExtractAscii (2, S, CommaOnly, '"')), True), WorkSet));
                    End;
                End;

                _StrConsists:
                Begin
                  GetVar (Trim (ExtractAscii (3, S, CommaOnly, '"')), V);

                  If V. Variable = Nil Then
                    Error (seUnknown)
                  Else
                    If V. Variable^. VarType <> tvLogical Then
                      Error (seIncorrectParams)
                    Else
                    Begin
                      Str2Set (StrExpression (Trim (ExtractAscii (2, S,
                        CommaOnly, '"')), True), WorkSet);
                      ChangeVarDirect (V, ZeroOne [ConsistsOf (StrExpression
                        (Trim (ExtractAscii (1, S, CommaOnly, '"')), True),
                        WorkSet)]);
                    End;
                End;

                _StrMaskMatch:
                Begin
                  GetVar (Trim (ExtractAscii (3, S, CommaOnly, '"')), V);

                  If V. Variable = Nil Then
                    Error (seUnknown)
                  Else
                    If V. Variable^. VarType <> tvLogical Then
                      Error (seIncorrectParams)
                    Else
                      ChangeVarDirect (V, ZeroOne [StrMaskMatch (StrExpression
                        (Trim (ExtractAscii (1, S, CommaOnly, '"')), True),
                        StrExpression (Trim (ExtractAscii (2, S, CommaOnly,
                        '"')), True))]);
                End;

                _StrStripColors:
                Begin
                  GetVar (Trim (ExtractAscii (3, S, CommaOnly, '"')), V);

                  If V. Variable = Nil Then
                    Error (seUnknown)
                  Else
                    If V. Variable^. VarType <> tvString Then
                      Error (seIncorrectParams)
                    Else
                      ChangeVarDirect (V, ZeroMsg (StrExpression (Trim
                        (ExtractAscii (1, S, CommaOnly, '"')), True),
                        BoolExpression (Trim (ExtractAscii (2, S, CommaOnly,
                        '"'))) = '1'));
                End;

                _UpCase:
                Begin
                  GetVar (S, V);
                  If V. Variable <> Nil Then
                    ChangeVarDirect (V, UpString (GetValueDirect (V)))
                  Else
                    Error (seUnknown);
                End;

                _LoCase:
                Begin
                  GetVar (S, V);
                  If V. Variable <> Nil Then
                    ChangeVarDirect (V, LoString (GetValueDirect (V)))
                  Else
                    Error (seUnknown);
                End;

                _PrCase:
                Begin
                  GetVar (S, V);
                  If V. Variable <> Nil Then
                    ChangeVarDirect (V, PrString (GetValueDirect (V)))
                  Else
                    Error (seUnknown);
                End;

                _GetArg:
                Begin
                  GetVar (Trim (ExtractWord (2, S, CommaOnly)), V);

                  If (V. Variable = Nil) Or
                     Not MathExpression (Trim (ExtractWord (1, S, CommaOnly)),
                         i)
                  Then
                    Error (seIncorrectParams)
                  Else
                  Begin
                    i := i - 1;
                    If (i >= 0) And (i < ScriptParams^. Count) Then
                      SetConvertedData (V, PString (ScriptParams^. At (i))^)
                    Else
                      SetConvertedData (V, '');
                  End;
                End;
              End;

            _Console:
              Case CT Of
                _KeyPressed:
                Begin
                  GetVar (S, V);

                  If (V. Variable = Nil) Or (V. Variable^. VarType <> tvLogical)
                  Then
                    Error (seIncorrectParams)
                  Else
                  Begin
                    ChangeVarDirect (V, ZeroOne [Incoming]);
                    Clock2;
                  End;
                End;

                _ReadKey:
                Begin
                  GetVar (Trim (ExtractWord (1, S, CommaOnly)), V);

                  If V. Variable <> Nil Then
                  Begin
                    ChangeVarDirect (V, ComReadKey);

                    Par1 := Trim (ExtractWord (2, S, CommaOnly));

                    If Length (Par1) > 0 Then
                    Begin
                      GetVar (Par1, V);

                      If (V. Variable = Nil) Or (V. Variable^. VarType <>
                         tvLogical)
                      Then
                        Error (seIncorrectParams)
                      Else
                        ChangeVarDirect (V, ZeroOne [Not FromKeyboard]);
                    End;
                  End
                  Else
                    Error (seIncorrectParams);
                End;

                _Write:
                  ComWrite (StrExpression (S, False), WriteMode);

                _WriteLn:
                  ComWriteLn (StrExpression (S, False), WriteMode);

                _SetColor:
                  If Not MathExpression (Trim (ExtractWord (1, S, CommaOnly)),
                         i) Or
                     Not MathExpression (Trim (ExtractWord (2, S, CommaOnly)),
                         L)
                  Then
                    Error (seIncorrectParams)
                  Else
                    ComWrite (EmuRelColor ((i Shl 4) + L), 0);

                _GetColor:
                Begin
                  GetVar (Trim (ExtractWord (1, S, CommaOnly)), V);
                  GetVar (Trim (ExtractWord (2, S, CommaOnly)), V1);

                  If ((V. Variable = Nil) Or (V. Variable^. VarType <> tvNumber))
                     Or ((V1. Variable = Nil) Or (V1. Variable^. VarType <>
                     tvNumber))
                  Then
                    Error (seUnknown)
                  Else
                  Begin
                    ChangeVarDirect (V, Long2Str (ioTextAttr Shr 4));
                    ChangeVarDirect (V1, Long2Str (ioTextAttr And $0F));
                  End;
                End;

                _Clear: Cls;

                _ClrEOL: ComWrite (EmuClrEOL, 0);

                _SetCursorCoord:
                  If Not MathExpression (Trim (ExtractWord (1, S, CommaOnly)),
                         i) Or
                     Not MathExpression (Trim (ExtractWord (2, S, CommaOnly)),
                         L)
                  Then
                    Error (seIncorrectParams)
                  Else
                    ComWrite (EmuGoToXY (L, i), 0);

                _GetCursorCoord:
                Begin
                  GetVar (Trim (ExtractWord (1, S, CommaOnly)), V);
                  GetVar (Trim (ExtractWord (2, S, CommaOnly)), V1);

                  If ((V. Variable = Nil) Or (V. Variable^. VarType <> tvNumber))
                     Or ((V1. Variable = Nil) Or (V1. Variable^. VarType <>
                     tvNumber))
                  Then
                    Error (seUnknown)
                  Else
                  Begin
                    ChangeVarDirect (V, Long2Str (WhereX));
                    ChangeVarDirect (V1, Long2Str (WhereY));
                  End;
                End;

                _CursorLeft:
                  If MathExpression (S, i) Then
                    ComWrite (EmuCursorLeft (i), 0)
                  Else
                    Error (seIncorrectParams);

                _CursorRight:
                  If MathExpression (S, i) Then
                    ComWrite (EmuCursorRight (i), 0)
                  Else
                    Error (seIncorrectParams);

                _CursorUp:
                  If MathExpression (S, i) Then
                    ComWrite (EmuCursorUp (i), 0)
                  Else
                    Error (seIncorrectParams);

                _CursorDown:
                  If MathExpression (S, i) Then
                    ComWrite (EmuCursorDown (i), 0)
                  Else
                    Error (seIncorrectParams);

                _KeyMacro:
                  KeyBufAdd (StrExpression (S, False));

                _SetWriteMode:
                  If MathExpression (S, i) Then WriteMode := i
                                           Else Error (seIncorrectParams);

                _Message:
                  Message (StrExpression (S, False));

                _ReadVar:
                Begin
                  GetVar (Trim (ExtractAscii (1, S, CommaOnly, '"')), V);

                  If (V. Variable = Nil) Or Not MathExpression (Trim
                     (ExtractAscii (2, S, CommaOnly, '"')), i)
                  Then
                    Error (seIncorrectParams)
                  Else
                  Begin
                    Parameter := GetValueDirect (V);
                    S := Trim (ExtractAscii (3, S, CommaOnly, '"'));

                    If S <> '' Then
                    Begin
                      Str2Set (StrExpression (S, True), WorkSet);
                      SetInputCap (NoCaps, WorkSet);
                      ComReadLn (Parameter, i, ofAllowEmpty);
                      SetInputCap (NoCaps, AllChars);
                    End
                    Else
                      ComReadLn (Parameter, i, ofAllowEmpty);

                    SetConvertedData (V, Parameter);
                  End;
                End;

                _YesNo, _NoYes:
                Begin
                  GetVar (S, V);

                  If (V. Variable = Nil) Or (V. Variable^. VarType <> tvLogical)
                  Then
                    Error (seIncorrectParams)
                  Else
                    ChangeVarDirect (V, ZeroOne [Query ('', CT = _YesNo, 0)]);
                End;

                _FileDisplay:
                  EmuDispFile (StrExpression (S, True));
              End;

            _File:
              Case CT Of
                _FileReadString:
                Begin
                  GetVar (Trim (ExtractWord (2, S, CommaOnly)), V);

                  If (V. Variable = Nil) Or
                     Not MathExpression (Trim (ExtractWord (1, S, CommaOnly)),
                     i)
                  Then
                    Error (seIncorrectParams)
                  Else
                  Begin
                    PF := GetPFileNum (i);
                    If (PF <> Nil) And Not PF^. EndOfFile Then
                    Begin
                      ReadLn (PF^. Handle, Parameter);
                      PF^. EndOfFile := IOResult <> 0;
                    End
                    Else
                      Parameter := '';

                    SetConvertedData (V, Parameter);
                  End;
                End;

                _FileGetEOF:
                Begin
                  GetVar (Trim (ExtractWord (2, S, CommaOnly)), V);

                  If (V. Variable = Nil) Or (V. Variable^. VarType <> tvLogical)
                     Or Not MathExpression (Trim (ExtractWord (1, S,
                     CommaOnly)), i)
                  Then
                    Error (seIncorrectParams)
                  Else
                    ChangeVarDirect (V, ZeroOne [GetEoF (i)]);
                End;

                _FileWriteString:
                  If Not MathExpression (Trim (ExtractAscii (1, S, CommaOnly,
                     '"')), i)
                  Then
                    Error (seIncorrectParams)
                  Else
                  Begin
                    PF := GetPFileNum (i);
                    If PF <> Nil Then
                      WriteLn (PF^. Handle, StrExpression (Trim (ExtractAscii
                        (2, S, CommaOnly, '"')), True));
                  End;

                _FileGetToString:
                  If Not MathExpression (Trim (ExtractWord (1, S, CommaOnly)),
                         i) Or
                     Not MathExpression (Trim (ExtractWord (2, S, CommaOnly)),
                         L)
                  Then
                    Error (seIncorrectParams)
                  Else
                  Begin
                    PF := GetPFileNum (i);
                    If PF <> Nil Then
                      With PF^ Do
                      Begin
                        If Lines = Nil Then
                        Begin
                          Lines := New (PLongStream, Init (4));
                          Lines^. Add (0);
                        End;

                        L := L - 1;
                        If L < 0 Then
                          L := 0;
                        i := Lines^. Count - 1;

                        If L <= i Then
                          TextSeek (Handle, Lines^. Get (L))
                        Else
                        Begin
                          TextSeek (Handle, Lines^. Get (i));
                          Dec (L, i);

                          While (L > 0) And Not EOF (Handle) Do
                          Begin
                            ReadLn (Handle, S);
                            Lines^. Add (TextPos (Handle));
                            Dec (L);
                          End;

                          If IOResult <> 0 Then;
                        End;
                      End;
                  End;

                _FileOpen, _FileCreate, _FileAppend:
                  If Not MathExpression (Trim (ExtractAscii (1, S, CommaOnly,
                     '"')), i)
                  Then
                    Error (seIncorrectParams)
                  Else
                    If Not fFileOpen (i, StrExpression (Trim (ExtractAscii (2,
                       S, CommaOnly, '"')), True), tFOpenModes [CT])
                    Then
                      Error (seFileOpenErr);

                _FileClose:
                  If MathExpression (S, i) Then ClosePFile (i)
                                           Else Error (seIncorrectParams);

                _FileExists:
                Begin
                  GetVar (Trim (ExtractAscii (2, S, CommaOnly, '"')), V);

                  If (V. Variable = Nil) Or (V. Variable^. VarType <> tvLogical)
                  Then
                    Error (seIncorrectParams)
                  Else
                    ChangeVarDirect (V, ZeroOne [FileExists (StrExpression
                      (Trim (ExtractAscii (1, S, CommaOnly, '"')), False))]);
                End;

                _FileSize:
                Begin
                  GetVar (Trim (ExtractAscii (2, S, CommaOnly, '"')), V);

                  If (V. Variable = Nil) Or (V. Variable^. VarType <> tvNumber)
                  Then
                    Error (seIncorrectParams)
                  Else
                    ChangeVarDirect (V, Long2Str (gFileSize (StrExpression
                      (Trim (ExtractAscii (1, S, CommaOnly, '"')), True))));
                End;

                _FileDelete:
                  tDeleteFile (StrExpression (S, False));

                _FindFirst:
                  If Not MathExpression (Trim (ExtractAscii (1, S, CommaOnly,
                         '"')), i) Or
                     Not MathExpression (Trim (ExtractAscii (4, S, CommaOnly,
                         '"')), L)
                  Then
                    Error (seIncorrectParams)
                  Else
                  Begin
                    GetVar (Trim (ExtractAscii (5, S, CommaOnly, '"')), V);
                    GetVar (Trim (ExtractAscii (6, S, CommaOnly, '"')), V1);

                    If (V. Variable = Nil) Or (V1. Variable = Nil) Or
                       (V. Variable^. VarType <> tvString) Or
                       (V1. Variable^. VarType <> tvNumber)
                    Then
                      Error (seIncorrectParams)
                    Else
                    Begin
                      ClosePFileFinder (i);
                      New (PFF);

                      With PFF^ Do
                      Begin
                        FindFirst (AddBackSlash (StrExpression (Trim
                          (ExtractAscii (2, S, CommaOnly, '"')), True)) +
                          StrExpression (Trim (ExtractAscii (3, S, CommaOnly,
                          '"')), True), L, Info);

                        If DOSError = 0 Then
                        Begin
                          Number := i;
                          FileFinders^. Insert (PFF);
                          ChangeVarDirect (V, Info. Name);
                          ChangeVarDirect (V1, Long2Str (Info. Attr));
                        End Else
                        Begin
                        {$IFNDEF MSDOS}
                          FindClose (Info);
                        {$ENDIF}
                          Dispose (PFF);
                          ChangeVarDirect (V, '');
                          ChangeVarDirect (V1, '0');
                        End;
                      End;
                    End;
                  End;

                _FindNext:
                  If Not MathExpression (Trim (ExtractWord (1, S, CommaOnly)),
                         i)
                  Then
                    Error (seIncorrectParams)
                  Else
                  Begin
                    GetVar (Trim (ExtractWord (2, S, CommaOnly)), V);
                    GetVar (Trim (ExtractWord (3, S, CommaOnly)), V1);

                    If (V. Variable = Nil) Or (V1. Variable = Nil) Or
                       (V. Variable^. VarType <> tvString) Or
                       (V1. Variable^. VarType <> tvNumber)
                    Then
                      Error (seIncorrectParams)
                    Else
                    Begin
                      PFF := GetPFileFindNum (i);

                      If PFF = Nil Then
                        Error (seUnknown)
                      Else
                        With PFF^ Do
                        Begin
                          FindNext (Info);

                          If DOSError = 0 Then
                          Begin
                            ChangeVarDirect (V, Info. Name);
                            ChangeVarDirect (V1, Long2Str (Info. Attr));
                          End Else
                          Begin
                            ClosePFileFinder (i);
                            ChangeVarDirect (V, '');
                            ChangeVarDirect (V1, '0');
                          End;
                        End;
                    End;
                  End;

              End;

            _Misc:
              Case CT Of
                _Random:
                Begin
                  GetVar (Trim (ExtractWord (2, S, CommaOnly)), V);

                  If Not MathExpression (Trim (ExtractWord (1, S, CommaOnly)),
                     i) Or (V. Variable = Nil) Or (V. Variable^. VarType <>
                     tvNumber)
                  Then
                    Error (seIncorrectParams)
                  Else
                    ChangeVarDirect (V, Long2Str (Random (i) + 1));
                End;

                _MidSec:
                Begin
                  GetVar (S, V);
                  If (V. Variable = Nil) Or (V. Variable^. VarType <> tvNumber)
                  Then
                    Error (seIncorrectParams)
                  Else
                    ChangeVarDirect (V, Long2Str (MidSec));
                End;

                _FlagsValid:
                Begin
                  GetVar (Trim (ExtractAscii (2, S, CommaOnly, '"')), V);

                  If (V. Variable = Nil) Or (V. Variable^. VarType <> tvLogical)
                  Then
                    Error (seIncorrectParams)
                  Else
                    ChangeVarDirect (V, ZeroOne [FlagsValid (R. Flags,
                      StrExpression (Trim (ExtractAscii (1, S, CommaOnly, '"')),
                      True))]);
                End;

                _AddToDownLoadList:
                Begin
                  AddToDLList (StrExpression (S, True), 0, 0);
                  UpdateTransferVars;
                End;

                _AddToDLListRelative:
                Begin
                  S := mFileExist (FileArea. DLPath, JustFileName (Trim
                    (StrExpression (S, True))));
                  If S <> '' Then
                  Begin
                    AddToDLList (S, R. FileGroup, R. FileArea);
                    UpdateTransferVars;
                  End;
                End;

                _GetFirstTagged:
                Begin
                  GetVar (S, V);
                  If (V. Variable = Nil) Or (V. Variable^. VarType <> tvString)
                  Then
                    Error (seIncorrectParams)
                  Else
                  Begin
                    TagFileNum := 0;
                    If F2Transfer^. Count = 0 Then
                      Parameter := ''
                    Else
                      Parameter := PTagFileRec (F2Transfer^. At (TagFileNum))^.
                        PathName^;
                    ChangeVarDirect (V, Parameter);
                  End;
                End;

                _GetNextTagged:
                Begin
                  GetVar (S, V);
                  If (V. Variable = Nil) Or (V. Variable^. VarType <> tvString)
                  Then
                    Error (seIncorrectParams)
                  Else
                  Begin
                    Inc (TagFileNum);

                    If TagFileNum >= F2Transfer^. Count Then
                      Parameter := ''
                    Else
                      Parameter := PTagFileRec (F2Transfer^. At (TagFileNum))^.
                        PathName^;
                    ChangeVarDirect (V, Parameter);
                  End;
                End;

                _IsUser:
                Begin
                  GetVar (Trim (ExtractAscii (2, S, CommaOnly, '"')), V);

                  If (V. Variable = Nil) Or (V. Variable^. VarType <> tvLogical)
                  Then
                    Error (seIncorrectParams)
                  Else
                    ChangeVarDirect (V, ZeroOne [Is_User (StrExpression (Trim
                      (ExtractAscii (1, S, CommaOnly, '"')), True), True)]);
                End;

                _GetUserNames:
                Begin
                  GetVar (Trim (ExtractAscii (2, S, CommaOnly, '"')), V);
                  GetVar (Trim (ExtractAscii (3, S, CommaOnly, '"')), V1);

                  If (V. Variable = Nil) Or (V1. Variable = Nil) Then
                    Error (seUnknown)
                  Else
                    If (V. Variable^. VarType <> tvString) Or
                       (V1. Variable^. VarType <> tvString)
                    Then
                      Error (seIncorrectParams)
                    Else
                      GetUserNames (StrExpression (Trim (ExtractAscii (1, S,
                        CommaOnly, '"')), True), V, V1);
                End;

                _Log:
                  LogWrite ('%', StrExpression (S, False));

                _Download:
                Begin
                  Parameter := StrExpression (S, True);
                  If Parameter <> '' Then
                    AddToDLList (Parameter, 0, 0);

                  UpdateUserMacro;
                  DownLoad;
                  UpdateTransferVars;
                End;

                _Upload:
                Begin
                  Parameter := StrExpression (S, True);
                  UpLoadToUser := '';
                  UpdateUserMacro;

                  If Parameter <> '' Then
                  Begin
                    Par1 := FileArea. ULPath;
                    FileArea. ULPath := AddBackSlash (Parameter);
                    Transfer (Parameter, Receive, tsNormal);
                    FileArea. ULPath := Par1;
                  End
                  Else
                    Transfer (Parameter, Receive, tsNormal);

                  UpdateTransferVars;
                End;

                _UploadPriv:
                Begin
                  UpLoadToUser := StrExpression (S, True);
                  Transfer (UpLoadToUser, Receive, tsPrivate);
                End;

                _GlobalSearch:
                Begin
                  Parameter := StrExpression (S, True);
                  If Parameter = '' Then
                    Parameter := GetMaxStr (lang (laSearchMask));
                  If Parameter <> '' Then
                  Begin
                    GlobalSearch (Parameter, fsName, atNo, '');
                    UpdateTransferVars;
                  End;
                End;

                _SearchByDesc:
                Begin
                  Parameter := StrExpression (S, True);
                  If Parameter = '' Then
                    Parameter := GetMaxStr (lang (laSearchDesc));
                  If Parameter <> '' Then
                  Begin
                    GlobalSearch (Parameter, fsDesc, atNo, '');
                    UpdateTransferVars;
                  End;
                End;

                _NewFilesSince:
                Begin
                  GlobalSearch (AllFilesMask, fsDate, atNo, StrExpression (S, True));
                  UpdateTransferVars;
                End;

                _NewFilesSinceLast:
                Begin
                  GlobalSearch (AllFilesMask, fsDate, atYes, '');
                  UpdateTransferVars;
                End;

                _ChangeFileArea: SelectFArea;
                _ChangeMsgArea: SelectMArea;
                _ChangeFileGroup: SelectFGroup;
                _ChangeMsgGroup: SelectMGroup;

                _FileList:
                Begin
                  LogWrite ('+', sm (smlBrowsingFArea) + ZeroMsg (FileArea.
                    Name, True));
                  InitDispFiles;
                  DispFilesBBS (AllFilesMask, '', False, False, tB);
                  DoneDispFiles;
                  UpdateTransferVars;
                End;

                _WriteMsg:
                Begin
                  Parameter := Trim (ExtractAscii (1, S, CommaOnly, '"'));
                  If Parameter <> '' Then
                    If MathExpression (Parameter, i) Then
                      Str (i, Parameter)
                    Else
                      Error (seIncorrectParams);

                  PrePostMsg (Parameter, StrExpression (Trim (ExtractAscii (2,
                    S, CommaOnly, '"')), True));
                End;

                _FilePost:
                Begin
                  Parameter := StrExpression (Trim (ExtractAscii (1, S,
                    CommaOnly, '"')), True);
                  Par1 := StrExpression (Trim (ExtractAscii (2, S, CommaOnly,
                    '"')), True);
                  Par2 := StrExpression (Trim (ExtractAscii (3, S, CommaOnly,
                    '"')), True);
                  S := StrExpression (Trim (ExtractAscii (4, S, CommaOnly,
                    '"')), True);

                  PostFile (pmNew, Parameter, MtoAbs (R. MsgGroup, R. MsgArea), Par1,
                    Par2, S, '', MsgArea. Address, MsgArea. Address, pfAutoOpen +
                    pfUseDefaultAddr);
                End;

                _MsgRead:
                Begin
                  LogWrite ('+', sm (smlReadingMsg) + ZeroMsg (MsgArea. Name,
                    True));
                  ReadMsgs;
                End;

                _MsgList:
                Begin
                  LogWrite ('+', sm (smlListingMsg) + ZeroMsg (MsgArea. Name,
                    True));
                  ListMsgs;
                End;

                _MsgSearch:
                Begin
                  Parameter := StrExpression (S, True);
                  If Parameter = '' Then
                    Parameter := GetMaxStr (lang (laMsgMaskGet));
                  If Parameter <> '' Then
                    SearchMessages (Parameter);
                End;

                _ScanPrivMail: SearchPrivate;
                _DownloadQWK: qwkDownLoad;
                _UploadQWK: qwkUpLoad;
                _SelectQWK: qwkSelect;

                _ShowRawDir:
                Begin
                  If (FileArea. List_Security > R. Security) Or
                     (Not FlagsValid (R. Flags, FileArea. List_Flags)) Then
                  Begin
                    If R. HotKeys Then
                      ComWriteLn ('', 0);
                    Message (lang (laSecurityLow));
                  End Else
                  Begin
                    LogWrite ('+', sm (smlBrowsingFArea) + ZeroMsg (FileArea.
                      Name, True));
                    InitDispFiles;
                    DispFilesBBS (AllFilesMask, '', False, True, tB);
                    DoneDispFiles;
                    UpdateTransferVars;
                  End;
                End;

                _PageSysop:
                  PageSysOp (StrExpression (S, False));

                _DoorWay:
                Begin
                  LogWrite ('+', sm (smlEnterDW));
                  GetDir (0, S);
                  CommandProcessor;
                  LogWrite ('+', sm (smExitDW));
                  SmartChDir (S);
                End;

                _News: News (False);

                _Telnet:
                Begin
                {$IFNDEF MSDOS}
                  GoTelnet (StrExpression (S, False));
                {$ENDIF}
                End;

                _UserInfo:
                Begin
                  Parameter := StrExpression (S, True);

                  If (Parameter = '') And (R. Security >= Cnf. UserInfoMinSec)
                     And FlagsValid (R. Flags, Cnf. UserInfoMinFlags) Then
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
                    UserInfo (Parameter);
                End;

                _HangUp:
                Begin
                  NormExit;
                  Break;
                End;

                _Call:
                  ExecScript (StrExpression (S, False));

                _Exec:
                Begin
                  Parameter := TranslateExecParams (StrExpression (Trim
                    (ExtractAscii (1, S, CommaOnly, '"')), True));
                  Par1 := Trim (ExtractAscii (2, S, CommaOnly, '"'));

                  If Par1 <> '' Then
                  Begin
                    L := DosShell (Parameter, exDirect, False);
                    GetVar (Par1, V);
                    If (V. Variable <> Nil) And (V. Variable^. VarType =
                       tvNumber)
                    Then
                      ChangeVarDirect (V, Long2Str (L));
                  End
                  Else
                    DosShell (Parameter, exCommand, False);
                End;

                _Delay:
                  If MathExpression (S, i) Then Pause (Abs (i))
                                           Else Error (seIncorrectParams);
              End;
          End;
        End;

      aExpression:
        Begin
          a := Pos ('=', S);
          GetVar (Trim (Copy (S, 1, a - 1)), V);

          If V. Variable <> Nil Then
          Begin
            Parameter := Trim (Copy (S, a + 1, 255));

            Case V. Variable^. VarType Of
               tvNumber : If MathExpression (Parameter, i) Then
                            ChangeVarDirect (V, Long2Str (i));
               tvString : ChangeVarDirect (V, StrExpression (Parameter, True));
              tvLogical : ChangeVarDirect (V, BoolExpression (Parameter));
            End;
          End
          Else
            Error (seUnknown);
        End;

      aProcedure:
        Begin
          New (PS);
          PS^. ProcText := ProcText;
          PS^. ProcLabels := ProcLabels;
          PS^. ProcLine := ProcLine;
          PS^. EndCount := EndCount1;
          ProcStack^. Insert (PS);

          With PProcedure (CurrScriptString^. ActionData)^ Do
          Begin
            ProcText := Text;
            ProcLabels := Labels;
          End;

          ProcLine := 0;
          EndCount1 := 0;
        End;

      aUnknown:
        Begin
          PP := Procedures^. Search (UpString (ExtractWord (1, S,
            [' ', '(', ')', '='])));
          If PP <> Nil Then
          Begin
            CurrScriptString^. Action := aProcedure;
            CurrScriptString^. ActionData := PP;
            Goto ProcessScriptString;
          End
          Else
            Error (seUnknown);
        End;
      End;
    End;
  End;

  Dispose (ScriptParams, Done);
  Dispose (Variables, Done);
  Dispose (Procedures, Done);
  Dispose (Files, Done);
  Dispose (FileFinders, Done);
  Dispose (ProcStack, Done);

  tExecScript := True;
End;

Procedure oExecScript (Const S: String);
Var
  OldMacro1, OldMacro2, OldMacro3 : Boolean;

Begin
  OldMacro1 := ScreenOut. UseMacroTable1;
  OldMacro2 := ScreenOut. UseMacroTable2;
  OldMacro3 := ScreenOut. UseMacroTable3;
  ExecScript (S);
  ScreenOut. UseMacroTable1 := OldMacro1;
  ScreenOut. UseMacroTable2 := OldMacro2;
  ScreenOut. UseMacroTable3 := OldMacro3;
End;

Procedure PrepareScriptTables;
Begin
  If Commands = Nil Then
  Begin
    Commands := New (PCommandsTable, Init (CommandsTableBits));

    With Commands^ Do
    Begin
      Insert ('END', _End, _Lang);
      Insert ('DELAY', _Delay, _Misc);
      Insert ('EXEC', _Exec, _Misc);
      Insert ('CALL', _Call, _Misc);
      Insert ('HANGUP', _HangUp, _Misc);
      Insert ('HALT', _Halt, _Lang);
      Insert ('EXIT', _Exit, _Lang);
      Insert ('USERINFO', _UserInfo, _Misc);
      Insert ('TELNET', _Telnet, _Misc);
      Insert ('NEWS', _News, _Misc);
      Insert ('DOORWAY', _DoorWay, _Misc);
      Insert ('PAGESYSOP', _PageSysop, _Misc);
      Insert ('SHOWRAWDIR', _ShowRawDir, _Misc);
      Insert ('SELECTQWK', _SelectQWK, _Misc);
      Insert ('UPLOADQWK', _UploadQWK, _Misc);
      Insert ('DOWNLOADQWK', _DownloadQWK, _Misc);
      Insert ('SCANPRIVMAIL', _ScanPrivMail, _Misc);
      Insert ('MSGSEARCH', _MsgSearch, _Misc);
      Insert ('MSGLIST', _MsgList, _Misc);
      Insert ('MSGREAD', _MsgRead, _Misc);
      Insert ('FILEPOST', _FilePost, _Misc);
      Insert ('WRITEMSG', _WriteMsg, _Misc);
      Insert ('FILELIST', _FileList, _Misc);
      Insert ('CHANGEMSGGROUP', _ChangeMsgGroup, _Misc);
      Insert ('CHANGEFILEGROUP', _ChangeFileGroup, _Misc);
      Insert ('CHANGEMSGAREA', _ChangeMsgArea, _Misc);
      Insert ('CHANGEFILEAREA', _ChangeFileArea, _Misc);
      Insert ('NEWFILESSINCELAST', _NewFilesSinceLast, _Misc);
      Insert ('NEWFILESSINCE', _NewFilesSince, _Misc);
      Insert ('SEARCHBYDESC', _SearchByDesc, _Misc);
      Insert ('GLOBALSEARCH', _GlobalSearch, _Misc);
      Insert ('UPLOADPRIV', _UploadPriv, _Misc);
      Insert ('UPLOAD', _Upload, _Misc);
      Insert ('DOWNLOAD', _Download, _Misc);
      Insert ('FILEDISPLAY', _FileDisplay, _Console);
      Insert ('NOYES', _NoYes, _Console);
      Insert ('YESNO', _YesNo, _Console);
      Insert ('MESSAGE', _Message, _Console);
      Insert ('READVAR', _ReadVar, _Console);
      Insert ('LOG', _Log, _Misc);
      Insert ('GETUSERNAMES', _GetUserNames, _Misc);
      Insert ('ISUSER', _IsUser, _Misc);
      Insert ('SETWRITEMODE', _SetWriteMode, _Console);
      Insert ('FLAGSVALID', _FlagsValid, _Misc);
      Insert ('MIDSEC', _MidSec, _Misc);
      Insert ('GETFIRSTTAGGED', _GetFirstTagged, _Misc);
      Insert ('GETNEXTTAGGED', _GetNextTagged, _Misc);
      Insert ('ADDTODLLISTRELATIVE', _AddToDLListRelative, _Misc);
      Insert ('ADDTODOWNLOADLIST', _AddToDownloadList, _Misc);
      Insert ('FINDFIRST', _FindFirst, _File);
      Insert ('FINDNEXT', _FindNext, _File);
      Insert ('FILEDELETE', _FileDelete, _File);
      Insert ('FILESIZE', _FileSize, _File);
      Insert ('FILEEXISTS', _FileExists, _File);
      Insert ('FILEAPPEND', _FileAppend, _File);
      Insert ('FILECREATE', _FileCreate, _File);
      Insert ('FILEOPEN', _FileOpen, _File);
      Insert ('FILECLOSE', _FileClose, _File);
      Insert ('FILEGETTOSTRING', _FileGetToString, _File);
      Insert ('FILEWRITESTRING', _FileWriteString, _File);
      Insert ('FILEGETEOF', _FileGetEOF, _File);
      Insert ('FILEREADSTRING', _FileReadString, _File);
      Insert ('RANDOM', _Random, _Misc);
      Insert ('GETARG', _GetArg, _String);
      Insert ('PRCASE', _PrCase, _String);
      Insert ('LOCASE', _LoCase, _String);
      Insert ('UPCASE', _UpCase, _String);
      Insert ('ORD', _Ord, _String);
      Insert ('VAL', _Val, _String);
      Insert ('STRSTRIPCOLORS', _StrStripColors, _String);
      Insert ('STRMASKMATCH', _StrMaskMatch, _String);
      Insert ('STRCONSISTS', _StrConsists, _String);
      Insert ('EXTRACTWORD', _ExtractWord, _String);
      Insert ('WORDCOUNT', _WordCount, _String);
      Insert ('PADSTR', _PadStr, _String);
      Insert ('TRIMSTR', _TrimStr, _String);
      Insert ('STRREPLACE', _StrReplace, _String);
      Insert ('STRDELETE', _StrDelete, _String);
      Insert ('SUBSTRING', _SubString, _String);
      Insert ('STRPOS', _StrPos, _String);
      Insert ('GETSTRINGLENGTH', _GetStringLength, _String);
      Insert ('KEYMACRO', _KeyMacro, _Console);
      Insert ('CURSORDOWN',_CursorDown, _Console);
      Insert ('CURSORUP',_CursorUp, _Console);
      Insert ('CURSORRIGHT',_CursorRight, _Console);
      Insert ('CURSORLEFT',_CursorLeft, _Console);
      Insert ('GETCURSORCOORD', _GetCursorCoord, _Console);
      Insert ('SETCURSORCOORD', _SetCursorCoord, _Console);
      Insert ('CLREOL', _ClrEOL, _Console);
      Insert ('CLEAR', _Clear, _Console);
      Insert ('GETCOLOR', _GetColor, _Console);
      Insert ('SETCOLOR', _SetColor, _Console);
      Insert ('WRITELN', _WriteLn, _Console);
      Insert ('WRITE', _Write, _Console);
      Insert ('READKEY', _ReadKey, _Console);
      Insert ('KEYPRESSED', _KeyPressed, _Console);
      Insert ('GOTO', _Goto, _Lang);
      Insert ('ELSE', _Else, _Lang);
      Insert ('IF', _If, _Lang);
    End;
  End;

  If VarMacros = Nil Then
  Begin
    VarMacros := New (PVarMacrosTable, Init (VarMacrosTableBits));

    With VarMacros^ Do
    Begin
      Insert ('$FRAM', vmFRAM);
      Insert ('$HKEY', vmHKEY);
      Insert ('$MORE', vmMORE);
      Insert ('$FSED', vmFSED);
      Insert ('$PROT', vmPROT);
      Insert ('$LTME', vmLTME);
      Insert ('$FDTE', vmFDTE);
      Insert ('$LDTE', vmLDTE);
      Insert ('$DAOB', vmDAOB);
      Insert ('$CALN', vmCALN);
      Insert ('$CMNT', vmCMNT);
      Insert ('$ADR3', vmADR3);
      Insert ('$ADR2', vmADR2);
      Insert ('$ADR1', vmADR1);
      Insert ('$BPHN', vmBPHN);
      Insert ('$HPHN', vmHPHN);
      Insert ('$ALAS', vmALAS);
      Insert ('$ORGZ', vmORGZ);
      Insert ('$LOCA', vmLOCA);
      Insert ('$PSWD', vmPSWD);
      Insert ('$ETME', vmETME);
      Insert ('$DLKB', vmDLKB);
      Insert ('$ULKB', vmULKB);
      Insert ('$DNLS', vmDNLS);
      Insert ('$ULDS', vmULDS);
      Insert ('$LFKB', vmLFKB);
      Insert ('$LIMK', vmLIMK);
      Insert ('$EMUL', vmEMUL);
      Insert ('$LANG', vmLANG);
      Insert ('$LINS', vmLINS);
      Insert ('$FLGS', vmFLGS);
      Insert ('$SECR', vmSECR);
      Insert ('$MGRN', vmMGRN);
      Insert ('$FGRN', vmFGRN);
      Insert ('$MNUM', vmMNUM);
      Insert ('$FNUM', vmFNUM);
    End;
  End;

  If ReservedVars = Nil Then
  Begin
    ReservedVars := New (PReservedVarsTable, Init (ResVarsTableBits));

    With ReservedVars^ Do
    Begin
      Insert ('$FRAM', rvFRAM);
      Insert ('$HKEY', rvHKEY);
      Insert ('$MORE', rvMORE);
      Insert ('$NODE', rvNODE);
      Insert ('$FSED', rvFSED);
      Insert ('$PROD', rvPROD);
      Insert ('$TTME', rvTTME);
      Insert ('$ETME', rvETME);
      Insert ('$LIMK', rvLIMK);
      Insert ('$ETTM', rvETTM);
      Insert ('$DLKB', rvDLKB);
      Insert ('$ULKB', rvULKB);
      Insert ('$DNLS', rvDNLS);
      Insert ('$ULDS', rvULDS);
      Insert ('$SELK', rvSELK);
      Insert ('$MSGP', rvMSGP);
      Insert ('$DLTK', rvDLTK);
      Insert ('$OTME', rvOTME);
      Insert ('$LFKB', rvLFKB);
      Insert ('$FDTE', rvFDTE);
      Insert ('$LTME', rvLTME);
      Insert ('$LDTE', rvLDTE);
      Insert ('$CALN', rvCALN);
      Insert ('$CMNT', rvCMNT);
      Insert ('$ADR3', rvADR3);
      Insert ('$ADR2', rvADR2);
      Insert ('$ADR1', rvADR1);
      Insert ('$BPHN', rvBPHN);
      Insert ('$HPHN', rvHPHN);
      Insert ('$DAOB', rvDAOB);
      Insert ('$ORGZ', rvORGZ);
      Insert ('$LOCA', rvLOCA);
      Insert ('$PROT', rvPROT);
      Insert ('$USRS', rvUSRS);
      Insert ('$LNAM', rvLNAM);
      Insert ('$FNAM', rvFNAM);
      Insert ('FFREADONLY', rvReadOnly);
      Insert ('FFHIDDEN', rvHidden);
      Insert ('FFSYSTEM', rvSysFile);
      Insert ('FFVOLUMEID', rvVolumeID);
      Insert ('FFDIRECTORY', rvDirectory);
      Insert ('FFARCHIVE', rvArchive);
      Insert ('FFANYFILE', rvAnyFile);
      Insert ('PADSTRCENTER', rvPadStrCenter);
      Insert ('PADSTRLEFT', rvPadStrLeft);
      Insert ('PADSTRRIGHT', rvPadStrRight);
      Insert ('EODISABLE01', rvEODisable01);
      Insert ('EOCOLORCODE', rvEOColorCode);
      Insert ('EOSLASHCODE', rvEOSlashCode);
      Insert ('EOMACRO', rvEOMacro);
      Insert ('CFGLOGO', rvCfgLogo);
      Insert ('CFGTRCLOG', rvCfgTrcLog);
      Insert ('CFGCHATLOG', rvCfgChatLog);
      Insert ('CFGLOG', rvCfgLog);
      Insert ('CFGPORT', rvCfgPort);
      Insert ('CFGPRIVUL', rvCfgPrivUL);
      Insert ('CFGSAVETAG', rvCfgSaveTag);
      Insert ('CFGLANGNEWS', rvCfgLangNews);
      Insert ('CFGLANGMNUPATH', rvCfgLangMnuPath);
      Insert ('CFGLANGTXTPATH', rvCfgLangTxtPath);
      Insert ('CFGLANGDIR', rvCfgLangDir);
      Insert ('CFGDOORDIR', rvCfgDoorDir);
      Insert ('CFGFLAGSDIR', rvCfgFlagsDir);
      Insert ('CFGTEMPDIR', rvCfgTempDir);
      Insert ('CFGPATH', rvCfgPath);
      Insert ('ARGCOUNT', rvArgCount);
      Insert ('$SELN', rvSELN);
      Insert ('MABOARD', rvMABoard);
      Insert ('MAPATH', rvMAPath);
      Insert ('MABASE', rvMABase);
      Insert ('MAORIGIN', rvMAOrigin);
      Insert ('MAADDRESS', rvMAAddress);
      Insert ('FAFILELIST', rvFAFileList);
      Insert ('FAULPATH', rvFAUlPath);
      Insert ('FADLPATH', rvFADlPath);
      Insert ('KEYB_END', rvKeybEnd);
      Insert ('KEYB_HOME', rvKeybHome);
      Insert ('KEYB_LEFT', rvKeybLeft);
      Insert ('KEYB_RIGHT', rvKeybRight);
      Insert ('KEYB_DOWN', rvKeybDown);
      Insert ('KEYB_UP', rvKeybUp);
      Insert ('$MGAM', rvMGAM);
      Insert ('$MAMT', rvMAMT);
      Insert ('$MAGR', rvMAGR);
      Insert ('$FGAM', rvFGAM);
      Insert ('$FAMT', rvFAMT);
      Insert ('$FAGR', rvFAGR);
      Insert ('$MGRP', rvMGRP);
      Insert ('$MARE', rvMARE);
      Insert ('$FGRP', rvFGRP);
      Insert ('$FARE', rvFARE);
      Insert ('$MGRN', rvMGRN);
      Insert ('$MNUM', rvMNUM);
      Insert ('$FGRN', rvFGRN);
      Insert ('$FNUM', rvFNUM);
      Insert ('$TIME', rvTIME);
      Insert ('$DATE', rvDATE);
      Insert ('$BBSN', rvBBSN);
      Insert ('$SYSO', rvSYSO);
      Insert ('$PSWD', rvPSWD);
      Insert ('$LINS', rvLINS);
      Insert ('$FLGS', rvFLGS);
      Insert ('$ALAS', rvALAS);
      Insert ('$EMUL', rvEMUL);
      Insert ('$LANG', rvLANG);
      Insert ('$BAUD', rvBAUD);
      Insert ('$SECR', rvSECR);
      Insert ('$NAME', rvNAME);
    End;
  End;
End;

End.
