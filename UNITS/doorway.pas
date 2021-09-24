{$IFDEF MSDOS}
  {$IFNDEF DPMI}
  {$IFNDEF DPMI32}
    {$DEFINE RealMode}
    {$O+}
  {$ENDIF}
  {$ENDIF}
{$ENDIF}
{$I-,F+}
{&Delphi+}

Unit DoorWay;

Interface

Procedure CommandProcessor;

Implementation

Uses
{$IFDEF RealMode}
  ExecSwap,
{$ENDIF}

{$IFDEF OS2}
  Os2PmApi,
  Os2Def,
  Os2Base,
  VPUtils,
  VPSysLow,
{$ENDIF}

  ApSame,
  DOS,
  tWin,
  tMisc,
  MainComm,
  Parse,
  Log,
  OpCrt,
  Tree,
  Protocol,
  SysMsgs,
  TGlob,
  Areas,
  Users,
  TimeTask,
  Doors,
  tFSed,
  Objects,
  FilesBBS,
  tBigFunc,
  Shell;

Type
  tCommandProc = Procedure (Command: String);

  tcInfoRec = Record
    C         : String [15];
    Desc, Use : String [78];
    Proc      : tCommandProc;
    ArgCount  : Byte;
  End;

  CommandArr = Array [hcCls..hcKill] Of tcInfoRec;

  DirCollType = (dcVolume, dcDirectory, dcFile);

  PDirCollEntry = ^TDirCollEntry;
  TDirCollEntry = Record
    Size : LongInt;
    Time : LongInt;
    Attr : DirCollType;
    Name : String;
  End;

  PDirSortedCollection = ^TDirSortedCollection;
  TDirSortedCollection = Object (TSortedCollection)
    Procedure FreeItem (Item: Pointer); Virtual;
    Function Compare (Key1, Key2: Pointer): Integer; Virtual;
  End;

Const
  LongNames : Boolean = {$IFDEF MSDOS} False {$ELSE} True {$ENDIF};

Var
  DW           : ^DoorWayControlRec;
  CommandInfo  : ^CommandArr;
  CurDirectory : PathStr;

Function LnEdMenu (Var S: String): tLineEditResult;
Var
  i : Integer;

  Function iAnalyze: Boolean;
  Begin
    iAnalyze := True;

    Case i Of
      1 : Begin
            ComWriteLn ('|\15File saved.', 0);
            LnEdMenu := mpSave;
          End;
      2 : Begin
            ComWriteLn ('', 0);
            If Not Query (lang (laSure), False, 0) Then
              iAnalyze := False;
            LnEdMenu := mpAbort;
          End;
      3 : Begin
            ComWriteLn ('', 0);
            LnEdMenu := mpContinue;
          End;
      4 : Begin
            S := Trim (GetAnswer ('|' + lang (laEnterMsgLine), 3, ofAllowEmpty,
              ''));
            If S = '' Then
              iAnalyze := False;
            LnEdMenu := mpEditLine;
          End;
      5 : Begin
            ComWriteLn ('||' + lang (laMsgWriteText3), eoMacro + eoCodes);
            LnEdMenu := mpShow;
          End;
      6 : Begin
            S := Trim (GetAnswer ('||' + lang (laDelLines), 30, ofAllowEmpty,
              ''));
            If S = '' Then
              iAnalyze := False;
            LnEdMenu := mpDeleteLine;
          End;
    End;
  End;

Begin
  Repeat
    i := MenuBar ('|' + lang (laPostMenu), lang (laPostKeys));
  Until iAnalyze;
End;

Procedure EditLine (Var S: String);
Begin
  S := GetAnswer ('', 73, 0, S);
End;

Procedure ColdBoot;
{$IFNDEF RealMode}
Assembler;
Asm
  XOr AX, AX
  mov DS, AX
  mov AH, $40
  mov ES, AX
  mov Word Ptr ES:$72, 0
  mov AX, $FFFF
  mov ES, AX
  XOr SI, SI
  push AX
  push SI
  retf
{$ELSE}
Begin
{$ENDIF}
End;

Function CheckDrive (Const Command: String): Boolean;
Begin
  If (Command [2] = ':') And (Command [1] in ['A'..'Z', 'a'..'z']) And
     (DW^. Drives [UpCase (Command [1])] > R. Security) Then
  Begin
    ComWrite (#10 + lang (laSecurityLow) + #10, eoMacro + eoCodes);
    CheckDrive := False;
  End
  Else
    CheckDrive := True;
End;

Procedure CdCommand (Command: String);
Var
  CurDir : PathStr;
  ResStr : String [100];

Begin
  Command := Trim (Command);
  If Not CheckDrive (Command) Then
    Exit;

  GetDir (0, CurDir);
  SmartChDir (Command);
  If IOResult <> 0 Then
  Begin
    ComWriteLn (EmuRelColor ($0C) + #10 + Command + ' directory is invalid.',
      0);
    SmartChDir (CurDir);
  End Else
  Begin
    GetDir (0, CurDirectory);
    ResStr := 'Directory changed to ' + UpString (CurDirectory);
    ComWriteLn (EmuRelColor ($03) + #10 + ResStr, 0);
    LogWrite ('+', 'DoorWay: ' + ResStr);
  End;
End;

Procedure ClsCommand (Command: String);
Begin
  ComWrite (EmuRelColor ($07) + EmuCls, 0);
End;

Procedure TREECommand (Command: String);
Var
  ProcExit : Boolean;

  Procedure SearchDir (Dir: PathStr; Const GrDir: PathStr);
  Var
    SRec : SearchRec;

  Begin
    If ProcExit Then
      Exit;

    If Dir [Length (Dir)] <> '\' Then
      Dir := Dir + '\';

    FindFirst (Dir + AllFilesMask, Directory shl 8 or AnyFile, SRec);

    While DosError = 0 Do
    Begin
      With SRec Do
        If (Attr And Directory <> 0) And (Name [1] <> '.') Then
        Begin
          ComWrite (EmuRelColor ($0E) + GrDir + ' ù ', eoNoFlush);
          ComWriteLn (EmuRelColor ($07) + Name, 0);

          If Not More Then
          Begin
            ProcExit := True;
            Exit;
          End;

          SearchDir (Dir + Name, GrDir + '   ');
          If ProcExit Then
            Exit;
        End;

      FindNext (SRec);
    End;

  {$IFNDEF MSDOS}
    FindClose (SRec);
  {$ENDIF}
  End;

Var
  Dir  : DirStr;
  Name : NameStr;
  Ext  : ExtStr;

Begin
  InitMore (0);
  SmartChDir (Copy (CurDirectory, 1, 3));
  FSplit (FExpand (''), Dir, Name, Ext);
  ProcExit := False;
  ComWriteLn (EmuRelColor ($07) + Dir, 0);
  SearchDir (Dir, ' ');
  SmartChDir (CurDirectory);
  InitMore (0);
  LogWrite ('+', 'DoorWay: Showing directory tree');
End;

Procedure MdCommand (Command: String);
Var
  ResStr : String [100];

Begin
  Command := Trim (Command);
  If Not CheckDrive (Command) Then
    Exit;

  MkDir (Command);

  If IOResult <> 0 Then
    ComWriteLn (EmuRelColor ($0C) + #10'Can''t create directory ' + Trim
      (Command), 0)
  Else
  Begin
    ResStr := 'Directory ' + Command + ' has been created.';
    ComWriteLn (EmuRelColor ($03) + #10 + ResStr, 0);
    LogWrite ('+', 'DoorWay: ' + ResStr);
  End;
End;

Procedure RdCommand (Command: String);
Var
  ResStr : String [100];

Begin
  Command := Trim (Command);
  If Not CheckDrive (Command) Then
    Exit;

  RmDir (Command);

  If IOResult <> 0 Then
    ComWriteLn (EmuRelColor ($0C) + #10'Can''t delete directory ' + Command, 0)
  Else
  Begin
    ResStr := 'Directory ' + Trim (Copy (Command, Pos (' ', Command), 255)) +
      ' deleted.';
    ComWriteLn (EmuRelColor ($03) + #10 + ResStr, 0);
    LogWrite ('+', 'DoorWay: ' + ResStr);
  End;
End;

Procedure BootCommand (Command: String);
Begin
{$IFDEF MSDOS}
  If TimeTask. Task. OS = 0 Then
  Begin
    ComWriteLn ('\15Shuting down system ...', eoCodes);
    LogClose;
    mL_Done;
    EraseFlag (RunFlag);
    ColdBoot;
  End
  Else
{$ENDIF}
    ComWriteLn ('\12Cannot reboot system under multitasking evironment!',
      eoCodes);
End;

Procedure TDirSortedCollection. FreeItem (Item: Pointer);
Var
  P : PDirCollEntry Absolute Item;

Begin
  If P <> Nil Then
    FreeMem (P, SizeOf (P^) - SizeOf (P^. Name) + Length (P^. Name) + 1);
End;

Function TDirSortedCollection. Compare (Key1, Key2: Pointer): Integer;
Var
  D1     : PDirCollEntry Absolute Key1;
  D2     : PDirCollEntry Absolute Key2;
{$IFNDEF VirtualPascal}
  Result : Integer;
{$ENDIF}

Begin
  Result := Ord (D1^. Attr) - Ord (D2^. Attr);
  If Result = 0 Then
    Result := StrCompare (D1^. Name, D2^. Name);
{$IFNDEF VirtualPascal}
  Compare := Result;
{$ENDIF}
End;

Procedure DirCommand (Command: String);

  Function SplitSize (S: String): String;
  Var
    i : Integer;

  Begin
    i := Length (S) - 2;

    While i > 1 Do
    Begin
      Insert ('\15,\10', S, i);
      Dec (i, 3);
    End;

    SplitSize := S;
  End;

Const
  HeadColor : Array [DirCollType] Of Byte = ($0B, $0B, $0A);
  ItemColor : Array [DirCollType] Of Byte = ($0F, $0F, $03);

Var
  i, TotalSize,
  FilesCount,
  DirsCount     : LongInt;
  P             : PDirCollEntry;
  C             : PDirSortedCollection;
  Sr            : SearchRec;
  DT            : DateTime;
  SDate         : String [8];
  STime         : String [6];
  S             : String;

Label
  EndOfProc;

Begin
  Command := Trim (Command);
  If Command = '' Then
    Command := AllFilesMask;
  Command := CompletePath (JustPathName (Command)) + JustFileName (Command);
  If Not CheckDrive (Command) Then
    Exit;

  ComWrite (#10 + EmuRelColor ($0A) + 'Directory of ' + UpString (Command) +
    #13#10#10, 0);

  C := New (PDirSortedCollection, Init (16, 16));
  TotalSize := 0;
  FilesCount := 0;
  DirsCount := 0;

  FindFirst (Command, AnyFile, Sr);

  While DosError = 0 Do
  Begin
    GetMem (P, SizeOf (P^) - SizeOf (P^. Name) + Length (Sr. Name) + 1);

    With P^ Do
    Begin
      Size := Sr. Size;
      Time := Sr. Time;
      Name := Sr. Name;

      If (Sr. Attr And VolumeID) <> 0 Then
        Attr := dcVolume
      Else
        If (Sr. Attr And Directory) <> 0 Then
        Begin
          Attr := dcDirectory;
          Inc (DirsCount);
        End Else
        Begin
          Attr := dcFile;
          Inc (FilesCount);
          Inc (TotalSize, Size);
        End;
    End;

    C^. Insert (P);

    FindNext (Sr);
  End;

{$IFNDEF MSDOS}
  FindClose (Sr);
{$ENDIF}

  If C^. Count = 0 Then
    ComWriteLn (EmuRelColor ($0C) + 'No file(s) found', 0)
  Else
  Begin
    InitMore (3);

    For i := 0 To C^. Count-1 Do
    Begin
      With PDirCollEntry (C^. At (i))^ Do
      Begin
        UnpackTime (Time, DT);
        SDate := FormattedDate (DT, 'DD-MM-YY');
        STime := FormattedDate (DT, ' HH:II');

        Case Attr Of
               dcFile : S := LeftPad (Long2Str (Size), 10);
          dcDirectory : S := '     <DIR>';
             dcVolume : S := '<VolLabel>';
        End;

        If LongNames Then
        Begin
          ComWrite (EmuRelColor ($09) + SDate, eoNoFlush);
          ComWrite (EmuRelColor ($0C) + STime + ' ', eoNoFlush);
          ComWrite (EmuRelColor (HeadColor [Attr]) + S + ' ', eoNoFlush);
          ComWriteLn (EmuRelColor (ItemColor [Attr]) + Name, 0);
        End Else
        Begin
          ComWrite (EmuRelColor (ItemColor [Attr]) + Pad (Name, 17), eoNoFlush);
          ComWrite (EmuRelColor (HeadColor [Attr]) + S + ' ', eoNoFlush);
          ComWrite (EmuRelColor ($09) + SDate, eoNoFlush);
          ComWriteLn (EmuRelColor ($0C) + STime, 0);
        End;
      End;

      If Not More Then
        Goto EndOfProc;
    End;

    ComWrite (#13#10 + '\10' + LeftPad (Long2Str (FilesCount), 12) +
      ' \15file(s)\10', eoCodes);
    mWriteLen (SplitSize (Long2Str (TotalSize)), 16, pmPadLeft);
    ComWriteLn (' \15bytes', eoCodes);

    ComWrite ('\10' + LeftPad (Long2Str (DirsCount), 13) + ' \15dir(s)\10',
      eoCodes);
    mWriteLen (SplitSize (Long2Str (DiskFree (0))), 16, pmPadLeft);
    ComWriteLn (' \15bytes free', eoCodes);
  End;

EndOfProc:
  Dispose (C, Done);
  LogWrite ('+', 'DoorWay: Showing directory of ' + NiceFileName (Command, 30));
End;

Procedure DelCommand (Command: String);
Var
  Sr               : SearchRec;
  F                : File;
  Found            : Boolean;
  i                : Byte;
  S, Pth, NiceName : PathStr;
  ResStr           : String [100];

Begin
  If Not CheckDrive (Command) Then
    Exit;

  ComWriteLn ('', 0);
  Found := False;

  For i := 1 To WordCount (Command, SpaceOnly) Do
  Begin
    S := ExtractWord (i, Command, SpaceOnly);
    Pth := CompletePath (JustPathName (S));

    FindFirst (S, AnyFile-Directory-VolumeID, Sr);

    If DosError <> 0 Then
    Begin
    {$IFNDEF MSDOS}
      FindClose (SR);
    {$ENDIF}
      Continue;
    End;

    Found := True;

    While DosError = 0 Do
    Begin
      S := Pth + Sr. Name;
      NiceName := NiceFileName (S, 50);

      Assign (F, S);
      SetFAttr (F, 0);
      Erase (F);

      If IOResult <> 0 Then
        ComWriteLn (EmuRelColor ($0C) + 'Can''t delete ' + NiceName, 0)
      Else
      Begin
        ResStr := NiceName + ' deleted.';
        ComWriteLn (EmuRelColor ($03) + ResStr, 0);
        LogWrite ('+', 'DoorWay: ' + ResStr);
      End;

      FindNext (Sr);
    End;

  {$IFNDEF MSDOS}
    FindClose (Sr);
  {$ENDIF}
  End;

  If Not Found Then
    ComWriteLn (EmuRelColor ($0C) + 'No file(s) found.', 0);
End;

Procedure RenCommand (Command: String);
Var
  F1, F2 : PathStr;
  N1, N2 : String [30];
  ResStr : String [100];

Begin
  F1 := Trim (ExtractWord (1, Command, SpaceOnly));
  F2 := Trim (ExtractWord (2, Command, SpaceOnly));
  If Not CheckDrive (F1) Or Not CheckDrive (F2) Then
    Exit;

  If FileExists (F1) Then
  Begin
    N1 := NiceFileName (F1, 30);
    N2 := NiceFileName (F2, 30);

    ComWrite (EmuRelColor ($0B) + #10 + N1, eoNoFlush);
    ComWrite (EmuRelColor ($0C) + ' -> ', eoNoFlush);
    ComWriteLn (EmuRelColor ($0B) + N2, 0);

    If Not tRenameFile (F1, F2) Then
      ResStr := 'Can''t rename ' + N1 + ' to ' + N2
    Else
      ResStr := 'File ' + N1 + ' renamed to ' + N2;

    ComWriteLn (EmuRelColor ($0B) + ResStr, 0);
    LogWrite ('+', 'DoorWay: ' + ResStr);
  End
  Else
    ComWriteLn (EmuRelColor ($0C) + #10 + sm (smFile) + F1 + sm (smNotFound),
      0);
End;

Procedure CopyCommand (Command: String);
Var
  i                  : Integer;
  Sr                 : SearchRec;
  Fin, Fout, FTemp   : File;
  TempStr, S         : String [100];
  TempCurDir, F1, F2 : PathStr;
  Always             : Boolean;

Label
  2,
  4;

Begin;
  F1 := ExtractWord (1, Command, SpaceOnly);
  F2 := ExtractWord (2, Command, SpaceOnly);
  If Not CheckDrive (F1) Or Not CheckDrive (F2) Then
    Exit;

  Always := False;
  ComWriteLn ('', 0);

  FindFirst (F1, AnyFile-Directory-VolumeID, Sr);

  If DosError = 18 Then
  Begin
    ComWriteLn (EmuRelColor ($0C) + 'No file(s) found.', 0);
  {$IFNDEF MSDOS}
    FindClose (Sr);
  {$ENDIF}
    Exit;
  End;

  While DosError = 0 Do
  Begin
    TempStr := F2;

    If TempStr [Length (TempStr) ] <> '\' Then
    Begin
      Assign (FTemp, TempStr + '\copy.$$$');
      ReWrite (FTemp, 1);
      If IOResult = 0 Then
      Begin
        TempStr := TempStr + '\' + Sr. Name;
        Close (FTemp);
        Erase (FTemp);
      End;
    End Else
    Begin
      GetDir (0, TempCurDir);
      TempStr := Copy (TempStr, 1, Length (TempStr) - 1);
      SmartChDir (TempStr);
      If IOResult <> 0 Then
        MkDir (TempStr);
      TempStr := TempStr + '\' + Sr. Name;
      SmartChDir (TempCurDir);
    End;

    Assign (Fin, AddBackSlash (JustPathName (F1)) + Sr. Name);
    Assign (Fout, TempStr);
    Reset (Fin, 1);

    If IOResult <> 0 Then
    Begin
      ComWriteLn (EmuRelColor ($0C) + 'Can''t open source file ' + Sr. Name, 0);
      Goto 2;
    End;

    Reset (Fout, 1);

    If IOResult = 0 Then
    Begin
      If Always = True Then
      Begin
        Close (FOut);
        Goto 4;
      End;

      i := MenuBar (EmuColor ($0B) + 'File ' + TempStr +
        ' already exists. Overwrite it? (Yes/No/All/Cancel): ', 'YNCA');
      ComWriteLn (EmuRelColor ($0B), 0);

      Case i Of
        1: Close (Fout);

        2: Begin
             Close (FOut);
             Goto 2;
           End;

        3: Begin
             Close (FOut);
             Close (FIn);
             If IOResult <> 0 Then;
             Exit;
           End;

        4: Begin
             Close (FOut);
             Always := True;
           End;
      End;
    End;

  4:
    ReWrite (Fout, 1);

    If IOResult <> 0 Then
    Begin
      ComWrite (EmuRelColor ($0C) + 'Can''t open target file ' + TempStr, 0);
      Close (Fin);
      Goto 2;
    End;

    Close (Fin);
    Close (Fout);

    S := AddBackSlash (JustPathName (F1)) + Sr. Name;

    ComWrite (lang (laCopyFile), eoMacro + eoCodes);
    ComWrite (NiceFileName (S, 45), 0);
    ComWrite (lang (laCopyTo), eoMacro + eoCodes);
    ComWriteLn (NiceFileName (TempStr, 45) + '..', 0);

    tCopyFile (S, TempStr, True);

    If DrawAborted Then
      Break;

  2:
    FindNext (Sr);
  End;

  If DOSError <> 0 Then
    ComWriteLn (lang (laCopyDone), eoMacro + eoCodes);

{$IFNDEF MSDOS}
  FindClose (Sr);
{$ENDIF}

  LogWrite ('+', 'DoorWay: file(s) ' + F1 + ' copied to ' + F2);
End;

Procedure ShellCommand (Command: String);
Var
  F        : Text;
  TempStr  : String;
  Status   : Word;
  CloseCom : Boolean;

Begin
  ComWrite (EmuRelColor ($03) + #10, eoNoFlush);

  CloseCom := Command[1] = '!';
  If CloseCom Then
    Delete(Command, 1, 1);

{$IFDEF RealMode}
  ComWrite ('Swapping ... ', 0);
  If Not InitExecSwap (HeapPtr, Cnf. TempDir + DelChars ([':'], StrTime) +
     'sh.swp') Then
  Begin
    ComWriteLn (EmuRelColor ($0C) + 'Unable to allocate swap space!', 0);
    Exit;
  End
  Else
    ComWrite (#13, 0);
{$ENDIF}

  ComWrite ('Executing. Wait, please ..'#13, 0);
  If Not Local And CloseCom Then
    DeactivatePort (Port, True);

{$IFDEF MSDOS}
  SwapVectors;
{$ENDIF}

{$IFDEF RealMode}
  Status := ExecWithSwap (GetEnv ('COMSPEC'), ' /c ' + Command + ' >' +
    Cnf. TempDir + 'torshell.$$$');
  ShutDownExecSwap;
{$ELSE}
  Exec (GetEnv ('COMSPEC'), ' /c ' + Command + ' >' + Cnf. TempDir +
    'torshell.$$$');
  Status := DosError;
{$ENDIF}

{$IFDEF MSDOS}
  SwapVectors;
{$ENDIF}

  If Not Local And CloseCom Then
  Begin
    ActivatePort (Port, True);
    If AsyncStatus <> ecOk Then
    Begin
      LogWrite ('!', sm (smlInitError) + ': ' + Status2Str (
        AsyncStatus mod 10000));
      NormExit;
    End;
  End;

  SetBlink (Cnf. Blinking);

  If StatusBar And (WhereY >= ScrY) Then
  Begin
    GotoXY (1, ScrY - 1);
    ComWrite (EmuClrEOL, 0);
  End;

  If Status <> 0 Then
  Begin
    ComWriteLn (EmuRelColor ($0C) + 'Unable to execute.', 0);
    Exit;
  End;

  Assign (F, Cnf. TempDir + 'torshell.$$$');
  ReSet (F);
  InitMore (0);
  ComWrite (EmuRelColor (3), 0);

  While Not SeekEof (F) Do
  Begin
    ReadLn (F, TempStr);
    ComWriteLn (TempStr, 0);
    If Not More Then
      Break;
  End;

  Close (F);
  Erase (F);

  If IOResult <> 0 Then
    ComWriteLn (EmuRelColor ($03) + 'No information received from the program.',
      0);
  LogWrite ('+', 'DoorWay: exec shell "' + Trim (Command) + '"');
End;

Procedure DateCommand (Command: String);
Var
  DT      : DateTime;
  NewDate : String;

Begin
  ComWriteLn (EmuRelColor ($03) + 'Current date is: ' +
    FormattedCurrDT (Cnf. DateMask), 0);

  Repeat
    ComWrite ('Enter new date in format (' + Cnf. DateMask + '): ', 0);
    NewDate := '';
    ComReadLn (NewDate, Length (Cnf. DateMask), ofAllowEmpty);
    NewDate := Trim (NewDate);
    If NewDate = '' Then
      Exit;
  Until DateMaskMatch (NewDate, Cnf. DateMask);

  ExtractDT (NewDate, Cnf. DateMask, DT);
  SetDate (DT. Year, DT. Month, DT. Day);

  NewDate := FormattedCurrDT (Cnf. DateMask);
  ComWriteLn (EmuRelColor ($0F) + 'Current date is: ' + NewDate, 0);
  LogWrite ('+', 'DoorWay: date changed, new date is ' + NewDate);
End;

Procedure TimeCommand (Command: String);
Var
  Hour, Min, Sec          : Word;
  Point                   : SysInt;
  StrHour, StrMin, StrSec : String [2];
  S                       : String;

Begin
  ComWriteLn (EmuRelColor ($03) + #10'Current time is ' + StrTime, 0);
  ComWrite ('Enter new time: ' + EmuRelColor ($0F), 0);
  S := '';
  ComReadLn (S, 8, ofAllowEmpty);
  S := Trim (S);
  If S = '' Then
    Exit;

  StrHour := ExtractWord (1, S, [':']);
  StrMin := ExtractWord (2, S, [':']);
  StrSec := ExtractWord (3, S, [':']);

  If ConsistsOf (StrHour, NumbersOnly) And
     ConsistsOf (StrMin, NumbersOnly) And
     (ConsistsOf (StrSec, NumbersOnly) Or (StrSec = '')) Then
  Begin
    Dec (R. TotalTime, TimeDiff (EnterTime, MidSec));
    SetTime (Str2Long (StrHour), Str2Long (StrMin), Str2Long (StrSec), 0);
    EnterTime := MidSec;
  End
  Else
    ComWriteLn (EmuRelColor ($0C) + 'Entered time is invalid.', 0);

  LogWrite ('+', 'DoorWay: system time set to ' + StrTime);
End;

Procedure DirSizeCommand (Command: String);
Var
  Temp    : LongInt;
  TempStr : PathStr;

Begin
  If Not CheckDrive (Command) Then
    Exit;

  GetDir (0, TempStr);
  SmartChDir (Command);

  If IOResult <> 0 Then
  Begin
    SmartChDir (TempStr);
    ComWriteLn (EmuRelColor ($0E) + Command + ' directory is invalid.', 0);
    Exit;
  End;

  ComWrite (EmuRelColor ($03) + #10'Calculating. Please, wait ...', 0);
  SmartChDir (TempStr);
  Temp := DirSizeWSb (Command);
  SmartChDir (TempStr);

  ComWriteLn (EmuRelColor ($0A) + #13'Total size of ' + CompletePath (Command)
    + ' contents is ' + Long2Str (Temp) + ' bytes.', 0);
End;

Procedure xDelCommand (Command: String);
Var
  TempStr : PathStr;

Begin
  If Not CheckDrive (Command) Then
    Exit;

  GetDir (0, TempStr);
  SmartChDir (Command);

  If IOResult <> 0 Then
  Begin
    ComWriteLn (EmuRelColor ($0E) + Command + ' directory is invalid.', 0);
    Exit;
  End;

  SmartChDir (TempStr);

  If Query ('\12Are you sure you want to do recursive deleting?', False,
     ofFramed) Then
  Begin
    ComWriteLn (EmuRelColor ($0E) + 'Deleting. Wait ...', 0);
    DelWithSubDirs (Command);
    SmartChDir (TempStr);
    LogWrite ('+', 'DoorWay: directory ' + Command + ' deleted with subdirs');
  End;
End;

Procedure DlCommand (Command: String);
Var
  i           : Integer;
  oSizeOfAll  : LongInt;
  oF2Transfer : PTagFilesCollection;
  DirInfo     : SearchRec;
  FileRec     : TTagFileRec;
  Dir         : PathStr;
  S           : String;
  Found       : Boolean;

Begin
  GetDir (0, Dir);
  Dir := AddBackSlash (Dir);

  oF2Transfer := F2Transfer;
  F2Transfer := New (PTagFilesCollection, Init (8, 4));
  oSizeOfAll := SizeOfAll;
  SizeOfAll := 0;
  Found := False;

  For i := 1 To WordCount (Command, SpaceAndComma) Do
  Begin
    S := ExtractWord (i, Command, SpaceAndComma);
    If Not CheckDrive (S) Then
      Continue;

    FindFirst (S, AnyFile-Directory-VolumeID, DirInfo);

    While DOSError = 0 Do
    Begin
      Found := True;
      FileRec. GroupNum := 0;
      FileRec. AreaNum := 0;
      FileRec. Size := DirInfo. Size;
      FileRec. PathName := NewStr (Dir + DirInfo. Name);
      FileRec. FromName := Nil;
      InsertInTagList (FileRec);

      FindNext (DirInfo);
    End;

  {$IFNDEF MSDOS}
    FindClose (DirInfo);
  {$ENDIF}
  End;

  If Not Found Then
    ComWriteLn (EmuRelColor ($0C) + 'No file(s) found', 0)
  Else
  Begin
    UpdateUserMacro;
    AutoDL := True;
    Transfer ('', Transmit, tsNormal);
    AutoDL := False;
  End;

  Dispose (F2Transfer, Done);
  F2Transfer := oF2Transfer;
  SizeOfAll := oSizeOfAll;
  UpdateUserMacro;
End;

Procedure UlCommand (Command: String);
Var
  oProtocol : Char;

Begin
  oProtocol := R. Protocol;
  If (R. Protocol <> '7') And (R. Protocol <> '8') Then
    R. Protocol := '7';

  AutoDL := True;
  Transfer ('', Receive, tsNormal);
  AutoDL := False;

  R. Protocol := oProtocol;
End;

Procedure ArcViewCommand (Command: String);
Begin
  Command := Trim (Command);
  If Not CheckDrive (Command) Then
    Exit;

  If IsDevice (JustFileName (Command)) Or Not FileExists (Command) Then
  Begin
    ComWrite ('|\12' + lang (laFileName), eoMacro + eoCodes);
    ComWrite (Command, 0);
    ComWriteLn (lang (laNotFound), eoMacro + eoCodes);
  End
  Else
    OnLineArcView (Command);
End;

Procedure EditCommand (Command: String);
Var
  C        : Char;
  SaveFile : Boolean;
  TmpName  : String;

Begin
  Command := Trim (Command);
  If Not CheckDrive (Command) Then
    Exit;

  If IsDevice (JustFileName (Command)) Or Not FileExists (Command) Then
  Begin
    ComWrite ('|\12' + lang (laFileName), eoMacro + eoCodes);
    ComWrite (Command, 0);
    ComWriteLn (lang (laNotFound), eoMacro + eoCodes);
  End Else
  Begin
    SaveFile := False;
    SmartChDir (Cnf. Path);
    TmpName := CompletePath (Cnf. DoorInfoDir) + 'msgtmp.';
    SmartChDir (CurDirectory);
    tCopyFile (Command, TmpName, True);

    If R. Emu = teTTY Then
      SaveFile := lnEditFile (TmpName, LnEdMenu, EditLine, $03, $03) = mpSave
    Else
    Begin
      ComWrite (EmuGotoXY (1, 23), 0);
      ComWrite ('\09ÄÄ\15[ \10' + Pad (NiceFileName (CompletePath (JustPathName
        (Command)) + JustFileName (Command), 42), 42) +
        ' \15]\09ÄÄ\15[\14^\11Z \15Ä \11save\15]\09ÄÄ\15[\14E\11sc \15Ä \11abort\15]\09ÄÄ', eoCodes);

      Repeat
        C := fsEditFile (TmpName, [#27, #26], 1, 0, $03, $03, 1, 1, 79, 22);

        Case C Of
          #27 : Begin
                  ComWrite (EmuGotoXY (1, 22), 0);
                  ComWrite (EmuClrEOL, 0);
                  If Query (lang (laSure), False, ofNoCR) Then
                    Break;
                End;
          #26 : Begin
                  SaveFile := True;
                  Break;
                End;
        End;
      Until False;

      Cls;
    End;

    If SaveFile Then
      tRenameFile (TmpName, Command);
  End;
End;

Procedure TypeCommand (Command: String);
Begin
  Command := Trim (Command);
  If CheckDrive (Command) Then
  Begin
    ComWriteLn ('', 0);
    TypeFile (Command);
  End;
End;

Function WFileName (Const FName: PathStr): Boolean;
{$IFNDEF VirtualPascal}
Var
  Result : Boolean;
{$ENDIF}

Begin
  Result := More And Not DrawAborted;
  If Result Then
    ComWriteLn (NiceFileName (FName, 78), 0);
{$IFNDEF VirtualPascal}
  WFileName := Result;
{$ENDIF}
End;

Procedure LocateCommand (Command: String);
Begin
  Command := Trim (Command);
  If Not CheckDrive (Command) Then
    Exit;

  InitMore (1);
  ComWriteLn (EmuRelColor ($0A), 0);
  fLocate (Command, WFileName);
End;

Procedure TaskListCommand (Command: String);
{$IFDEF OS2}
Type
  Asshole = ^Byte;

Var
   aHab                 : Hab;
   NumItems, Status     : ULong;
   TskLst, TaskList     : PswBlock;
   TaskListSize         : Integer;
{$ENDIF}

Begin
{$IFDEF OS2}
  aHab:= NULLHANDLE;

  { ¯®èãª ¥¬ áª®«ìª® ¯ ¬ïâ¨ ­ ¤® ¯®¤ TaskList }

  NumItems := WinQuerySwitchList (aHab, nil, 0);

  If NumItems < 1 Then
  Begin
    ComWriteLn (EmuRelColor ($0C) + 'Can''t get task list without PMSHELL running.',
      0);
    Exit;
  End;

  ComWriteLn (#10 + EmuRelColor ($09) + Long2Str (NumItems) +
    ' items in the Task List.'#10, 0);
  TaskListSize := (NumItems * SizeOf (SWENTRY)) + SizeOf (HSWITCH);
  GetMem (TaskList, TaskListSize);

  {­ ¤® ¤®¡ ¢¨âì ¯p®¢¥p®çªã, ¬®¦¥â ¯ ¬ïâ¨ ¡®«ìè¥ ­¥âã ;) }

  If TaskList = Nil Then
  Begin
    ComWriteln (EmuRelColor ($0C) + 'Unable to allocate memory for tasklist!',
      0);
    Exit;
  End;

  {â¥¯¥pì ¬®¦­® § ¯®«ãç¨âì á ¬ TaskList}

  Status := WinQuerySwitchList (aHab, TaskList, TaskListSize);

  If Status = 0 Then
  Begin
    ComWriteLn ('\12Could not query Task List', eoCodes);
    Exit;
  End;

  TskLst := TaskList;

  { á®åp ­ï¥¬ áâ pë© ãª § â¥«ì }

  ComWriteLn (EmuRelColor ($0E) + '  PID   Process title', 0);
  ComWriteLn (EmuRelColor ($0F) + 'ÄÄÄÄÄÄÄ ÄÄÄÄÄÄÄÄÄÄÄÄÄ', 0);

  For Status := 1 To NumItems Do
  Begin
    ComWriteLn (EmuRelColor ($0C) + '[' + EmuColor ($03) + LeftPadCh (Long2Str
               (TaskList^. aSwentry. SwCtl. idProcess), ' ', 5) + EmuColor
               ($0C) + '] ' + EmuColor ($0B) + PlaceSubStr (Copy
               (TaskList^. aSwentry. SwCtl. szSwtitle, 1, Pos
               (#0, TaskList^. aSwentry. SwCtl. szSwtitle)), #10, ' '), 0);

    Inc (Asshole (TaskList), SizeOf (SWENTRY));
  End;

  FreeMem (TskLst, TaskListSize);
{$ENDIF}
End;

Procedure KillCommand (Command: String);
{$IFDEF OS2}
Var
  Status        : ApiRet;
{$ENDIF}

Begin
{$IFDEF OS2}
  Command := Trim (Command);
  Status := DosKillProcess (dkp_Process, Str2Long (Command));

  If Status = 0 Then
    ComWriteLn (EmuRelColor ($03) + #10'The process killed successfully.', 0)
  Else
    ComWriteLn (EmuRelColor ($0C) + #10'Error killing the process specified.',
    0);
{$ENDIF}
End;

Procedure HelpCommand (Command: String);
Var
  h : tDoorWayCommands;
  i : Byte;

Begin
  Command := UpString (Trim (Command));

  If Command = '' Then
  Begin
    ComWriteLn ('', 0);
    ComWriteLn ('\15Tornado DoorWay supported commands', eoCodes);
    ComWriteLn ('\08ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ', eoCodes);
    ComWriteLn (EmuRelColor ($07), 0);

    i := 0;
    For h := hcCls To hcKill Do
    Begin
      ComWrite (Pad (CommandInfo^ [h]. C, 10), 0);
      Inc (i);
      If i = 6 Then
      Begin
        ComWriteLn ('', 0);
        i := 0;
      End;
    End;

    ComWriteLn ('||\12Type HELP <Command> for help on a specified command.',
      eoCodes);
  End
  Else
    For h := hcCls To hcKill Do
      With CommandInfo^ [h] Do
        If C = Command Then
        Begin
          ComWriteLn ('\03|' + Desc, eoCodes);
          If Use <> '' Then
            ComWriteLn ('|' + Use, eoCodes);
          Break;
        End;
End;

Procedure FillCommandInfo;

  Procedure FillItem (Item: tDoorwayCommands; Const NewC,NewDesc,NewUse: String;
                      NewProc: tCommandProc; NewArgCount: Byte);
  Begin
    With CommandInfo^ [Item] Do
    Begin
      C := NewC;
      Desc := NewDesc;
      Use := NewUse;
      Proc := NewProc;
      ArgCount := NewArgCount;
    End;
  End;

Begin
  FillItem (hcCls, 'CLS', 'Clears the screen.', '', CLScommand, 0);
  FillItem (hcCD, 'CD', 'Changes current directory.', 'CD [drive][path]',
    CDcommand, 1);
  FillItem (hcDir, 'DIR', 'Displays directory contents.',
    'DIR [drive][path][wildcard]', DirCommand, 0);
  FillItem (hcTree, 'TREE', 'Displays the directory tree of current disk.',
    '', TreeCommand, 0);
  FillItem (hcMkDir, 'MKDIR', 'Makes directory with the name specified.',
    'MKDIR [drive][dirname]', MDCommand, 1);
  FillItem (hcRmDir, 'RMDIR', 'Removes directory specified.',
    'RMDIR [drive][path]', RDCommand, 1);
  FillItem (hcDirSize, 'DIRSIZE', 'Calculates size of the directory contents.',
    'DIRSIZE [drive][path]', DirSizeCommand, 1);
  FillItem (hcDel, 'DEL', 'Deletes file(s) specified.',
    'DEL [wildcard1] [wildcard2] ...', DelCommand, 1);
  FillItem (hcRename, 'REN', 'Renames or moves fromfile to tofile.',
    'REN [fromfile] [tofile]', RenCommand, 2);
  FillItem (hcCopy, 'COPY', 'Copies file(s).',
    'COPY [fromfilewildcard] [topath]', CopyCommand, 2);
  FillItem (hcArcView, 'ARCVIEW', 'Displays contents of the arcived file specified.',
    'ARCVIEW [drive][path][archived file name]', ArcViewCommand, 1);
  FillItem (hcType, 'TYPE', 'Displays text file.',
    'TYPE [drive][path][filename]', TypeCommand, 1);
  FillItem (hcLocate, 'LOCATE', 'Searches for files matching wildcard specified.',
    'LOCATE [wildcard]', LocateCommand, 1);
  FillItem (hcShell, 'SHELL', 'Runs the program specified.',
    'SHELL [command line]', ShellCommand, 1);
  FillItem (hcBoot, 'BOOT', 'Reboots the system.', '', BootCommand, 0);
  FillItem (hcDate, 'DATE', 'Changes system date.', 'DATE [new date]',
    DateCommand, 0);
  FillItem (hcTime, 'TIME', 'Changes system time.', 'TIME [new time]',
    TimeCommand, 0);
  FillItem (hcxDel, 'XDEL', 'Deletes directory with all the subdirs.',
    'XDEL [drive][path]', xDelCommand, 1);
  FillItem (hcUL, 'UL', 'Uploads file(s) into current directory.', '',
    ULCommand, 0);
  FillItem (hcDL, 'DL', 'Downloads file(s) specified.',
    'DL [file1] [file2] ...', DLCommand, 1);
  FillItem (hcEdit, 'EDIT', 'Invokes internal editor to edit the text file specified.',
    'EDIT [filename]', EditCommand, 1);
  FillItem (hcExit, 'EXIT', 'Exits internal Tornado DoorWay.', '', nil, 0);
  FillItem (hcTaskList, 'TASKLIST',
    'Displays list of the programs currently running.', '', TaskListCommand, 0);
  FillItem (hcKill, 'KILL', 'Kills the task specified.', 'KILL [pid]',
    KillCommand, 1);
End;

Procedure CommandProcessor;

  Procedure ParseCommandLine;
  Var
    Command, Cmd, Parameter : String;
    h                       : tDoorWayCommands;
    P                       : Byte;
    Ch                      : Char;
    Found                   : Boolean;

  Begin
    SetTitle ('doorway');
    ClearInputHistory;

    Repeat
      ComWriteLn ('', 0);

      Repeat
        GetDir (0, CurDirectory);
      {$IFDEF OS2}
        LongNames := GetDriveType (CurDirectory [1]) = dtHDHPFS;
      {$ENDIF}
        mL_LineMsg;
        ComWrite (EmuRelColor ($0F) + UpString (NiceFileName (CurDirectory, 55))
          + '>', eoNoFlush);
        ComWrite (EmuRelColor ($07), 0);
        Command := '';
        ComReadLn (Command, 78 - WhereX, ofAllowEmpty + ofHistory);
        Clock2;
        Command := Trim (Command);
      Until Command <> '';

      Cmd := UpString (ExtractWord (1, Command, SpaceOnly));
      P := Pos (' ', Command);
      If P = 0 Then
        Parameter := ''
      Else
        Parameter := Trim (Copy (Command, P + 1, 255));

      If (Length (Trim (Command)) <= 3) And (Command [2] = ':') Then {Change drive}
      Begin
        Ch := UpCase (Command [1]);

        If (Parameter <> '') Or Not (Ch in ['A'..'Z']) Then
        Begin
          ComWriteLn (EmuRelColor ($0C) + #10'Incorrect usage.', 0);
          Continue;
        End;

        If DW^. Drives [Ch] > R. Security Then
        Begin
          ComWrite (#10 + lang (laSecurityLow) + #10, eoMacro + eoCodes);
          Continue;
        End;

        SmartChDir (Ch + ':\');
        If IOResult <> 0 Then
        Begin
          ComWriteLn (EmuRelColor ($0C) + #10'Invalid drive specified or the drive isn''t ready.',
            0);
          SmartChDir (CurDirectory);
          Continue;
        End;

        ComWriteLn (EmuRelColor ($03) + #10'Drive changed to ' + Ch + ':', 0);
        LogWrite ('+', 'DoorWay: drive changed to ' + Ch + ':');
      End
      Else
      If Cmd = 'EXIT' Then
        Break
      Else
      If Cmd = 'HELP' Then
        HelpCommand (Parameter)
      Else
      Begin
        Found := False;

        For h := hcCls To hcKill Do
          With CommandInfo^ [h] Do
            If C = Cmd Then
            Begin
              If (DW^. SecLevel [h] > R. Security) Or
                 Not FlagsValid (R. Flags, DW^. SecFlags [h])
              Then
                ComWriteLn ('|' + lang (laSecurityLow), eoMacro + eoCodes)
              Else
                If ArgCount > WordCount (Parameter, SpaceOnly) Then
                  ComWriteLn ('\12|Wrong number of parameters for command \15' +
                    Cmd + '\12.|Type \15HELP ' + Cmd +
                    ' \12to see more info about its usage.', eoCodes)
                Else
                  Proc (Parameter);

              Found := True;
              Break;
            End;

        If Not Found Then
          ComWriteLn (EmuRelColor ($0C) + #10'Unknown command entered.', 0);
      End;
    Until False;
  End;

Begin
  If InShell Then
    Exit;

  InShell := True;
  New (DW);
  ReadDoorWay (Cnf. DoorWayCTL, DW^);

  If (DW^. Enter <= R. Security) And FlagsValid (R. Flags, DW^. flgEnter) Then
  Begin
    New (CommandInfo);
    FillCommandInfo;
    Cls;
    ComWriteLn (EmuRelColor ($07) + #13 + Replicate ('Ä', 79), 0);
    ComWrite (EmuRelColor ($0B) + Replicate (' ', 13) +
      'Command processor for ', eoNoFlush);
    ComWrite (EmuRelColor ($0E) + 'TORNADO ', eoNoFlush);
    ComWriteLn (EmuRelColor ($0B) + 'communications soft', 0);
    ComWriteLn (EmuRelColor ($0E) + Replicate (' ', 13) +
      'Written by Alex Radzishevskiy & Konstantin Klyagin', 0);
    ComWriteLn (EmuRelColor ($07) + Replicate ('Ä', 79), 0);
    ComWriteLn (EmuRelColor ($0F) + FormattedCurrDT (Cnf. DateMask +
      ' HH:II:SS'), 0);
    ComWriteLn (EmuRelColor ($07) +
      #10'If you need help, type "HELP" or "HELP <command>"', 0);

    SmartChDir (DW^. StartDir);
    If IOResult <> 0 Then
      Message (EmuColor ($0E) + DW^. StartDir + ' directory is invalid.')
    Else
      ParseCommandLine;

    Dispose (CommandInfo);
  End
  Else
    Message (lang (laSecurityLow));

  Dispose (DW);
  InShell := False;
End;

End.
