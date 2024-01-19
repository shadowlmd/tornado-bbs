{$IFDEF MSDOS}
  {$IFNDEF DPMI32}
    {$F+}
  {$IFNDEF DPMI}
    {$O+}
  {$ENDIF}
  {$ENDIF}
{$ENDIF}
{$I-}

Unit Protocol;

{*********************************************************}
{*                    PROTOCOL.PAS                       *}
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
  DOS,
  ApAbsPcl,
  ApSame,
  ApXModem,
  ApYModem,
  ApZModem,
  ApCom,
  Iface,
  SysMsgs,
  TimeTask,
  Log,
  Users,
  BinCfg,
  mFind,
  FilesBBS,
  tMisc,
  Crc,
  MainComm,
  OpCrt,
  TGlob,
  Objects;

Type
  tTransferState = (tsUploadMsg, tsNormal, tsPrivate);

Const
  TransferTime : LongInt = 0;

Var
  UpLoadToUser : String [36];

Procedure ClearCL;
Procedure Transfer (Const FileName: String; TransferMode: TransferModeType;
            State: tTransferState);
Procedure DownLoad;
Function Status2Str (Code: Word): String;

Implementation

Const
  CLDirName : PathStr = '';

Var
  LastGroup, LastArea              : LongInt;
  Index2Transfer, CurrentFileIndex : Word;
  trState                          : tTransferState;
  xFile                            : PathStr;

Function Status2Str (Code: Word): String;
Begin
  Case Code Mod 10000 Of
    ecOk: Status2Str := 'Ok';

    {DOS errors}
    ecFileNotFound: Status2Str := sm (sstatFileNotFound);
    ecPathNotFound: Status2Str := sm (sstatPathNotFound);
    ecTooManyFiles: Status2Str := sm (sstatTooManyFiles);
    ecAccessDenied: Status2Str := sm (sstatAccessDenied);
    ecInvalidHandle: Status2Str := sm (sstatInvalidHandle);
    ecOutOfMemory: Status2Str := sm (sstatOutOfMemory);
    ecInvalidDrive: Status2Str := sm (sstatInvalidDrive);
    ecNoMoreFiles: Status2Str := sm (sstatNoMoreFiles);

    {Turbo Pascal I/O errors}
    ecDiskRead: Status2Str := sm (sstatDiskRead);
    ecDiskFull: Status2Str := sm (sstatDiskFull);
    ecNotAssigned: Status2Str := sm (sstatNotAssigned);
    ecNotOpen: Status2Str := sm (sstatNotOpen);
    ecNotOpenInput: Status2Str := sm (sstatNotOpenInput);
    ecNotOpenOutput: Status2Str := sm (sstatNotOpenOutput);
    ecInvalidFormat: Status2Str := sm (sstatInvalidFormat);

    {DOS critical errors}
    ecWriteProtected: Status2Str := sm (sstatWriteProtected);
    ecUnknownUnit: Status2Str := sm (sstatUnknownUnit);
    ecDriveNotReady: Status2Str := sm (sstatDriveNotReady);
    ecUnknownCommand: Status2Str := sm (sstatUnknownCommand);
    ecCrcError: Status2Str := sm (sstatCrcError);
    ecBadStructLen: Status2Str := sm (sstatBadStructLen);
    ecSeekError: Status2Str := sm (sstatSeekError);
    ecUnknownMedia: Status2Str := sm (sstatUnknownMedia);
    ecSectorNotFound: Status2Str := sm (sstatSectorNotFound);
    ecOutOfPaper: Status2Str := sm (sstatOutOfPaper);
    ecDeviceWrite: Status2Str := sm (sstatDeviceWrite);
    ecDeviceRead: Status2Str := sm (sstatDeviceRead);
    ecHardwareFailure: Status2Str := sm (sstatHardwareFailure);

    {APUART port errors}
    ecNoMorePorts: Status2Str := sm (sstatNoMorePorts);
    ecOverrunError: Status2Str := sm (sstatOverrunError);
    ecParityError: Status2Str := sm (sstatParityError);
    ecFramingError: Status2Str := sm (sstatFramingError);

    {APINT14 port errors}
    ecTransmitFailed: Status2Str := sm (sstatTransmitFailed);
    ecUartError: Status2Str := sm (sstatUartError);

    {APCOM/OOCOM errors/status}
    ecBlockIncomplete: Status2Str := sm (sstatBlockIncomplete);
    ecBufferIsFull: Status2Str := sm (sstatBufferIsFull);
    ecBufferIsEmpty: Status2Str := sm (sstatBufferIsEmpty);
    ecTimeout: Status2Str := sm (sstatTimeout);
    ecStringIncomplete: Status2Str := sm (sstatStringIncomplete);
    ecStringOverrun: Status2Str := sm (sstatStringOverrun);
    ecUserAbort: Status2Str := sm (sstatUserAbort);

    {APMODEM/OOMODEM errors}
    ecTableFull: Status2Str := sm (sstatTableFull);
    ecNullCommand: Status2Str := sm (sstatNullCommand);

    {Tracing/EventFile file errors}
    ecEventFileError: Status2Str := sm (sstatEventFileError);
    ecTraceFileError: Status2Str := sm (sstatTraceFileError);

    {APCOM/OOCOM port errors}
    ecBadPortNumber: Status2Str := sm (sstatBadPortNumber);
    ecOutofRange: Status2Str := sm (sstatOutofRange);
    ecPortNotOpen: Status2Str := sm (sstatPortNotOpen);
    ecInvalidBaudRate: Status2Str := sm (sstatInvalidBaudRate);
    ecInvalidArgument: Status2Str := sm (sstatInvalidArgument);
    ecNoDevice: Status2Str := sm (sstatNoDevice);
    ecNotaUart: Status2Str := sm (sstatNotaUart);
    ecInvalidParity: Status2Str := sm (sstatInvalidParity);
    ecBadFileList: Status2Str := sm (sstatBadFileList);
    ecNotBuffered: Status2Str := sm (sstatNotBuffered);
    ecNotSupported: Status2Str := sm (sstatNotSupported);

    {Device layers error codes}
    ecNoFossil: Status2Str := sm (sstatNoFossil);
    ecDigiFailure: Status2Str := sm (sstatDigiFailure);

    {APXMODEM/OOABSPCL status codes}
    ecInitFail: Status2Str := sm (sstatInitFail);
    ecInitCancel: Status2Str := sm (sstatInitCancel);
    ecCancelRequested: Status2Str := sm (sstatCancelRequested);
    ecError: Status2Str := sm (sstatError);
    ecDuplicateBlock: Status2Str := sm (sstatDuplicateBlock);
    ecSequenceError: Status2Str := sm (sstatSequenceError);
    ecDirNotFound: Status2Str := sm (sstatDirNotFound);
    ecNoMatchingFiles: Status2Str := sm (sstatNoMatchingFiles);
    ecLongPacket: Status2Str := sm (sstatLongPacket);
    ecEndFile: Status2Str := sm (sstatEndFile);
    ecHandshakeInProgress: Status2Str := sm (sstatHandshakeInProgress);
    ecFileRenamed: Status2Str := sm (sstatFileRenamed);
    ecFileAlreadyExists: Status2Str := sm (sstatFileAlreadyExists);
    ecInvalidFilesize: Status2Str := sm (sstatInvalidFilesize);
    ecInvalidDateTime: Status2Str := sm (sstatInvalidDateTime);
    ecUnexpectedChar: Status2Str := sm (sstatUnexpectedChar);
    ecBlockCheckError: Status2Str := sm (sstatBlockCheckError);
    ecNoSearchMask: Status2Str := sm (sstatNoSearchMask);
    ecNoFilename: Status2Str := sm (sstatNoFilename);
    ecAsciiReceiveInProgress: Status2Str := sm (sstatAsciiReceiveInProgress);
    ecFileRejected: Status2Str := sm (sstatFileRejected);
    ecTooManyErrors: Status2Str := sm (sstatTooManyErrors);

    {APZMODEM/OOZMODEM status codes}
    ecGotCrcE: Status2Str := sm (sstatGotCrcE);
    ecGotCrcW: Status2Str := sm (sstatGotCrcW);
    ecGotCrcQ: Status2Str := sm (sstatGotCrcQ);
    ecGotCrcG: Status2Str := sm (sstatGotCrcG);
    ecGarbage: Status2Str := sm (sstatGarbage);
    ecSkipFile: Status2Str := sm (sstatSkipFile);
    ecBadPosition: Status2Str := sm (sstatBadPosition);
    ecFileDoesntExist: Status2Str := sm (sstatFileDoesntExist);
    ecCantWriteFile: Status2Str := sm (sstatCantWriteFile);
    ecFailedToHandshake: Status2Str := sm (sstatFailedToHandshake);
    ecNoFilesToReceive: Status2Str := sm (sstatNoFilesToReceive);

    {APMODEM/OOMODEM status codes}
    ecUnknownModemResult: Status2Str :=  sm (sstatUnknownModemResult);
    ecConnect: Status2Str := 'CONNECT';
    ecRing: Status2Str := 'RING';
    ecNoCarrier: Status2Str := 'NO CARRIER';
    ecNoDialTone: Status2Str := 'NO DIALTONE';
    ecBusy: Status2Str := 'BUSY';
    ecNoAnswer: Status2Str := 'NO ANSWER';
    ecRinging: Status2Str := 'RINGING';
    ecVoice: Status2Str := 'VOICE';

  Else
    Status2Str := sm (sstatUnknownErrorCode) + Long2Str (Code);
  End;
End;

Procedure BackgroundProc (Prot: ProtocolRecPtr); Far;
Begin
  TimeSlice;
  Clock2;
  TimeSlice;
End;

Procedure StatusP (Prot: ProtocolRecPtr; Starting, Ending: Boolean); Far;
Begin
  If Starting Or Ending Then
  Begin
    If Starting Then
      ProtocolMsg (sm (smtStart));

    AsyncStatus := AsyncStatus Mod 10000;
    If AsyncStatus <> ecOk Then
      ProtocolMsg (Status2Str (AsyncStatus));
  End
  Else
    UpdateProtBox (Prot);
End;

(*
Procedure Errorz (P: Pointer; Var StatusCode: Word); Far;
Begin
  If (P <> Nil) And ProtocolInProgress (P) Then
    If StatusCode Mod 10000 <> ecOk Then
      ProtocolMsg (Status2Str (StatusCode));
End;
*)

Function AcceptFile (P: ProtocolRecPtr): Boolean; Far;
Var
  fName   : String;
  DirInfo : SearchRec;

Begin
  fName := GetPathName (P);
  FindFirst (fName, AnyFile-VolumeID-Directory, DirInfo);

  AcceptFile :=
    (Cnf. ULOverWrite Or
     Not ((DosError = 0) And
          ((DirInfo. Attr And Hidden) = 0))) And
    Not IsDevice (JustFileName (fName)) And
    Not ((UpString (JustExtension (fName)) = 'LST') And
         (AddBackSlash (JustPathName (fName)) = Cnf. SaveTagPath));

{$IFNDEF MSDOS}
  FindClose (DirInfo);
{$ENDIF}
End;

Procedure ClearCL;
Var
  i, P : Integer;
  PS   : PString;

Begin
  If DirExists (CLDirName) Then
  Begin
    For i := 0 To Reps^. Count-1 Do
    Begin
      PS := Reps^. At (i);

      If R. Protocol in ['1'..'8'] Then
      Begin
        P := Pos (#255, PS^);
        tDeleteFile (Copy (PS^, P + 1, 255));
        tDeleteFile (Cnf. DoorInfoDir + JustFileName (Copy (PS^, 1, P - 1)));
      End
      Else
        tDeleteFile (PS^);
    End;

    Reps^. FreeAll;
    RmDir (CLDirName);
    If IOResult <> 0 Then;
  End;
End;

Procedure ClearSent;
Var
  i : Integer;

Begin
  i := 0;

  While i < F2Transfer^. Count Do
    If PTagFileRec (F2Transfer^. At (i))^. PathName^ = '!' Then
      F2Transfer^. AtFree (i)
    Else
      Inc (i);

  ClearCL;
  UpdateUserMacro;
End;

Procedure DownLoadOk (FSize: LongInt; i: Integer);
Var
  j      : Integer;
  FSizeK : LongInt;
  PT     : PTagFileRec;

Begin
  FSizeK := Round (FSize / 1024);
  Inc (SessionDL, FSizeK);
  Inc (TotalBytesTrans, FSize);
  Dec (SizeOfAll, FSize);

  If (i >= 0) And (i < F2Transfer^. Count) Then
  Begin
    PT := F2Transfer^. At (i);

    If PT^. FromName <> Nil Then
      If AddBackSlash (UpString (JustPathName (PT^. PathName^))) =
         UpString (Cnf. PrivUploadsDir)
      Then
        tDeleteFile (PT^. PathName^);

    If PT^. AreaNum > 0 Then
    Begin
      SmartChangeFArea (PT^. GroupNum, PT^. AreaNum, LastGroup, LastArea);

      If Cnf. DLCount Then
        IncFilesBBSCounter (JustFileName (PT^. PathName^), FileArea. FileList,
          Cnf. DLCountMask, FileArea. FListFormat = fCDList);

      If FileArea. CopyLocal And Not Local Then
        For j := 0 To Reps^. Count-1 Do
          If PT^. PathName^ = ExtractWord (1, PString (Reps^. At (j))^,
             [#255]) Then
          Begin
            tDeleteFile (Cnf. DoorInfoDir + JustFileName (PT^. PathName^));
            Break;
          End;
    End;

    ChangePString (PT^. PathName, '!');

    If Not PT^. Free Then
    Begin
      Inc (R. DownLoads);
      Inc (Sys. DownLoads);
      Inc (R. DownLoadsK, FSizeK);
      Inc (R. TodayK, FSizeK);
    End;
  End;
End;

Procedure UpLoadOk (Const FName: PathStr; FSize, Elapsed: LongInt);
Var
  FSizeK : LongInt;

Begin
  If (trState <> tsUploadMsg) And Not InShell Then
  Begin
    If trState = tsNormal Then
    Begin
      Inc (R. UpLoads);
      Inc (Sys. UpLoads);
      FSizeK := Round (FSize / 1024);
      Inc (R. UpLoadsK, FSizeK);
      Inc (SessionUL, FSizeK);
      Inc (TransferTime, Round (Elapsed / 18));
    End;

    UpFiles^. InsLine (FName);
  End;
End;

Function Next2Transfer (P: ProtocolRecPtr; Var FName: PathStr): Boolean; Far;
Var
  i, j : Integer;
  PS   : PString;

Label
  Loop;

Begin
  FName := '';

Loop:
  If Index2Transfer <= F2Transfer^. Count Then
  Begin
    CurrentFileIndex := Index2Transfer - 1;
    FName := PTagFileRec (F2Transfer^. At (CurrentFileIndex))^. PathName^;
    Inc (Index2Transfer);

    If FName = '!' Then
      Goto Loop;

    For i := 0 To Reps^. Count-1 Do
    Begin
      PS := Reps^. At (i);
      j := Pos (#255, PS^);
      If Copy (PS^, 1, j - 1) = FName Then
      Begin
        FName := Cnf. DoorInfoDir + JustFileName (FName);
        tRenameFile (Copy (PS^, j + 1, 255), FName);
        Break;
      End;
    End;

    SetTitle ('sending ' + NiceFileName (FName, 35));
    Next2Transfer := True;
  End
  Else
    Next2Transfer := False;
End;

Procedure LogFile (Prot: ProtocolRecPtr; LogFileStatus: LogFileType); Far;
Const
  LogLX = 67;
  ScrLX = 57;

Var
  IPos, FSize, ETics   : LongInt;
  PType                : Char;
  LogString, ScrString : String [127];
  PName                : String;

  Procedure AddCPS;
  Var
    CPS : LongInt;

  Begin
    CPS := FSize - IPos;
    If (ETics > 0) And (CPS > 0) Then
    Begin
      CPS := Round (CPS / (ETics / 18.2));
      If R. AvgCPS = 0 Then R. AvgCPS := CPS
                       Else R. AvgCPS := Round ((R. AvgCPS + CPS) / 2);
    End
    Else
      CPS := Round (GetConnectSpeed / 10);

    LogString := LogString + ' ' + Long2Str (CPS) + ' CPS Ok';
  End;

  Procedure InsName;
  Var
    i, L, S, M : Integer;

  Begin
    i := Length (LogString);
    M := Length (JustFilename (PName)) + 5;
    L := LogLX - i;
    If L < M Then
      L := M;
    S := ScrLX - i;
    If S < M Then
      S := M;
    ScrString := LogString;
    Insert (NiceFileName (PName, L), LogString, 5);
    Insert (NiceFileName (PName, S), ScrString, 5);
  End;

  Procedure SetHidden;
  Var
    F : File;

  Begin
    Assign (F, PName);
    SetFAttr (F, Hidden);
  End;

Begin
  If Local Or (Prot = Nil) Then
  Begin
    ETics := 0;
    FSize := gFileSize (xFile);
    IPos := 0;
    PName := xFile;
    PType := 'L';
  End Else
  Begin
    ETics := GetElapsedTics (Prot);
    FSize := GetFileSize (Prot);
    IPos := GetInitialFilePos (Prot);
    PName := GetPathName (Prot);
    PType := ProtocolTypeString [GetProtocol (Prot)] [1];
  End;

  If trState = tsUpLoadMsg Then
  Begin
    tRenameFile (PName, Cnf. DoorInfoDir + 'msgtmp.');
    Exit;
  End;

  If LogFileStatus in [lfReceiveOk..lfReceiveSkip,
                       lfTransmitOk..lfTransmitSkip] Then
  Begin
    If (IPos <> 0) And (LogFileStatus in [lfTransmitOk, lfReceiveOk,
       lfTransmitFail, lfReceiveFail])
    Then
      LogString := ' (' + sm (smpFrom) + ' ' + Long2Str (IPos) + ')'
    Else
      LogString := '';

    Case LogFileStatus Of

      lfTransmitOk :
      Begin
        LogString := PType + 'S: ' + LogString + ' ' + NiceFileSize (FSize);
        AddCPS;
        InsName;
        DownLoadOk (FSize, CurrentFileIndex);
      End;

      lfReceiveOk :
      Begin
        LogString := PType + 'R: ' + LogString + ' ' + NiceFileSize (FSize);
        AddCPS;
        InsName;
        UpLoadOk (PName, FSize, ETics);
      End;

      lfTransmitFail :
        If Not (Local Or (Prot = Nil)) Then
        Begin
          LogString := PType + 'S: ' + LogString + sm (smTransmitFail) + ' ' +
            sm (smpAtPos) + ' ' + Long2Str (GetBytesTransferred (Prot));
          InsName;
        End;

      lfReceiveFail :
        If Not (Local Or (Prot = Nil)) Then
        Begin
          LogString := PType + 'R: ' + LogString + sm (smReceiveFail) + ' ' +
            sm (smpAtPos) + ' ' + Long2Str (GetBytesTransferred (Prot));
          InsName;
          SetHidden;
        End;

      lfTransmitSkip :
      Begin
        LogString := PType + 'S: ' + LogString + sm (smTransmitSkip);
        InsName;
      End;

      lfReceiveSkip :
      Begin
        LogString := PType + 'R: ' + LogString + sm (smReceiveSkip);
        InsName;
      End;
    End;

    If Prot <> Nil Then
      ProtocolMsg (ScrString);
    LogWrite ('*', LogString);
  End;

  If Prot <> Nil Then
  Begin
    ApAbsPcl. AvgCPS := R. AvgCPS;
    UpdateProtBox (Prot);
    Prot^. PData^. InitFilePos := 0;
    Prot^. PData^. BytesTransferred := 0;
  End;
End;

Procedure Transfer (Const FileName: String; TransferMode: TransferModeType;
                    State: tTransferState);
Var
  oSizeOfAll, OldGroup, OldArea : LongInt;
  PT                            : PTagFileRec;
  i                             : Integer;
  List                          : Text;
  F                             : File;
  SRec                          : mSearchRec;
  tR                            : tUser;
  KWord                         : String [40];
  S, CurDir, ULDir, FName       : String;
  DirCreated                    : Boolean;

  Function CheckFreeSpace : Boolean;
  Var
    dFree : LongInt;
    oDir  : PathStr;

  Begin
    CheckFreeSpace := True;
    If Cnf. UploadSpace = 0 Then
      Exit;

    GetDir (0, oDir);
    SmartChDir (FileArea. ULPath);
    If IOResult <> 0 Then;

    dFree := DiskFree (0);
    If dFree < 0 Then
      dFree := 2147483647;
    If Round (dFree / 1024) < Cnf. UpLoadSpace Then
    Begin
      Message (lang (laUpLoadSpace));
      CheckFreeSpace := False;
    End;

    SmartChDir (oDir);
  End;

  Procedure ShutDown;
  Begin
    ClearCL;
    SmartChangeFArea (OldGroup, OldArea, LastGroup, LastArea);
    If (TransferMode = Transmit) And (FileName <> '') Then
    Begin
      SizeOfAll := oSizeOfAll;
      UpdateUserMacro;
    End;
  End;

  Function LocalDownLoad: Boolean;

    Procedure TransferFile;
    Begin
      ComWrite (lang (laCopyFile), eoMacro + eoCodes + eoNoFlush);
      ComWrite (NiceFileName (xFile, 30), eoNoFlush);
      ComWrite (lang (laCopyTo), eoMacro + eoCodes + eoNoFlush);
      ComWrite (S + '...', eoNoFlush);

      If Not tCopyFile (xFile, AddBackSlash (S) + JustFileName (xFile), True) Then
      Begin
        ComWriteLn (lang (laCopyError), eoMacro + eoCodes);
        LogFile (Nil, lfTransmitFail);
      End Else
      Begin
        ComWriteLn (lang (laCopyOk), eoMacro + eoCodes);
        LogFile (Nil, lfTransmitOk);
      End;
    End;

  Begin
    SetTitle ('local mode download');
    LocalDownLoad := False;

    S := '';
    ComWriteLn ('|' + lang (laLocalDownLoad), eoMacro + eoCodes);
    ComWrite (lang (laLocalDlDir), eoMacro + eoCodes + eoNoFlush);
    ComReadLn (S, 80, ofAllowEmpty);

    S := UpString (Trim (S));
    If S = '' Then
    Begin
      ShutDown;
      Exit;
    End;

    If Not DirExists (S) Then
    Begin
      Message (lang (laLdDirNotFound));
      ShutDown;
      Exit;
    End;

    If Not AutoDL Then
      ExportDescs;

    If FileName <> '' Then
    Begin
      xFile := FileName;
      TransferFile;
    End;

    While Next2Transfer (Nil, xFile) Do
      TransferFile;

    LocalDownLoad := True;
  End;

  Procedure LocalUpLoad;
  Var
    DirInfo : SearchRec;

  Begin
    SetTitle ('local mode upload');

    S := '';
    ComWriteLn ('|' + lang (laLocalUpLoad), eoMacro + eoCodes);
    ComWrite (lang (laLocalUlFileName), eoMacro + eoCodes + eoNoFlush);
    ComReadLn (S, 80, ofAllowEmpty);

    S := Trim (S);
    If S = '' Then
      Exit;

    FindFirst (S, AnyFile-VolumeID-Directory-Hidden, DirInfo);
    If DosError <> 0 Then
    Begin
      Message (lang (laLuFileNotFound));
      Exit;
    End;

    S := AddBackSlash (JustPathName (S));

    Repeat
      xFile := AddBackSlash (ULDir) + DirInfo. Name;

      If Not Cnf. ULOverWrite And FileExists (xFile) Then
        Message (lang (laFileName) + UpString (DirInfo. Name) +
          lang (laAlreadyExists))
      Else
      Begin
        ComWrite (lang (laCopyFile), eoMacro + eoCodes + eoNoFlush);
        ComWrite (NiceFileName (S + DirInfo. Name, 30) + '...', eoNoFlush);

        If Not tCopyFile (S + DirInfo. Name, xFile, True) Then
        Begin
          ComWriteLn (lang (laCopyError), eoMacro + eoCodes);
          LogFile (Nil, lfReceiveFail);
        End Else
        Begin
          ComWriteLn (lang (laCopyOk), eoMacro + eoCodes);
          LogFile (Nil, lfReceiveOk);
        End;
      End;

      FindNext (DirInfo);
    Until DosError <> 0;

  {$IFNDEF MSDOS}
    FindClose (DirInfo);
  {$ENDIF}
  End;

  Procedure UploadDone;
  Var
    i, P     : Integer;
    AddTime  : LongInt;
    Ft       : Text;
    F        : File;
    S, FName : String;

  Begin
    If InShell Then
    Begin
      UpFiles^. FreeAll;
      TransferTime := 0;
      Exit;
    End;

    AddTime := Round (TransferTime / 60) * Cnf. UploadTimePlus;
    If (AddTime > 0) And (trState = tsNormal) Then
    Begin
      Inc (R. TotalTime, AddTime * 60);
      Message (PlaceSubStrNoCase (lang (laUploadPlus), '@AddTime',
        Long2Str (AddTime)));
    End;

    If UpFiles^. Count > 0 Then
    Begin
      If FileName = '' Then
        If trState <> tsPrivate Then
        Begin
          For i := 0 To UpFiles^. Count-1 Do
          Begin
            FName := PString (UpFiles^. At (i))^;

            Assign (F, FName);
            SetFAttr (F, Archive);

            If Cnf. AjustULDate Then
            Begin
              System. FileMode := Open_Access_ReadWrite Or Open_Share_DenyReadWrite;
              ReSet (F);
              System. FileMode := Open_Access_ReadOnly Or Open_Share_DenyNone;
              SetFTime (F, GetDosDate);
              Close (F);
            End;

            FName := JustFileName (FName);
            S := lang (laULDescription);
            P := Pos ('@FILENAME', UpString (S));
            If P <> 0 Then
            Begin
              ComWrite (Copy (S, 1, P - 1), eoMacro + eoCodes + eoNoFlush);
              ComWrite (FName, eoNoFlush);
              ComWrite (Copy (S, P + 9, 255), eoMacro + eoCodes + eoNoFlush);
            End Else
            Begin
              ComWrite (S, eoMacro + eoCodes + eoNoFlush);
              ComWrite (' ' + FName + ': ', eoNoFlush);
            End;

            S := '';
            ComReadLn (S, 60, ofAllowEmpty);

            If Cnf. DLCount And (Trim (Cnf. DLCountMask) <> '') Then
              S := ZeroMask (Cnf. DLCountMask) + ' ' + S;

            AddDescription (FileArea. ULPath + 'files.bbs', UpString (FName),
              S);
            ExecScript ('afterupl ' + FName);
          End
        End Else
        Begin
          FName := Cnf. SaveTagPath + HexL (Crc32Str (UpLoadToUser)) + '.lst';
          Assign (Ft, FName);
          If FileExists (FName) Then
            Append (Ft)
          Else
          Begin
            ReWrite (Ft);
            WriteLn (Ft, '; ' + UpLoadToUser);
          End;

          If IOResult <> 0 Then
          Begin
            UpFiles^. FreeAll;
            TransferTime := 0;
            Exit;
          End;

          For i := 0 To UpFiles^. Count-1 Do
            WriteLn (Ft, '*' + R. Name + '|' + PString (UpFiles^. At (i))^);

          Close (Ft);
        End;

      If trState = tsNormal Then
      Begin
        FName := FullFlagName (FileFlag);
        Assign (Ft, FName);
        If FileExists (FName) Then Append (Ft)
                              Else ReWrite (Ft);

        For i := 0 To UpFiles^. Count-1 Do
          WriteLn (Ft, PString (UpFiles^. At (i))^);

        Close (Ft);
      End;

      UpFiles^. FreeAll;
    End;

    TransferTime := 0;
  End;

Begin
  TransferTime := 0;
  Index2Transfer := 1;
  trMode := TransferMode;
  trState := State;
  OldGroup := R. FileGroup;
  OldArea := R. FileArea;
  LastGroup := OldGroup;
  LastArea := OldArea;

  If (UpFiles <> Nil) And (UpFiles^. Count > 0) Then
    UpFiles^. FreeAll;
  If Not Local Then
    SetProtocol (R. Protocol);

  If State <> tsUploadMsg Then
    If State <> tsPrivate Then
    Begin
      If TransferMode = Receive Then
        If (FileArea. UL_Security > R. Security) Or
           Not FlagsValid (R. Flags, FileArea. UL_Flags) Then
        Begin
          Message ('|' + lang (laSecurityLow));
          Exit;
        End;
    End Else
    Begin
      UpLoadToUser := FileName;
      If UpLoadToUser = '' Then
      Begin
        ClearInputHistory;
        If Cnf. CapitalizeNames Then SetInputCap (Proper, LettersOnly)
                                Else SetInputCap (NoCaps, LettersOnly);

        Repeat
          UpLoadToUser := Trim (GetAnswer (lang (laULforUser), 36,
            ofAllowEmpty + ofFramed + ofHistory, ''));

          If UpLoadToUser = '' Then
          Begin
            SetInputCap (NoCaps, AllChars);
            Exit;
          End;

          If Not Is_User (UploadToUser, Cnf. Aliases) Then
          Begin
            ComWrite ('|' + lang (laUser), eoMacro + eoCodes + eoNoFlush);
            ComWrite (UpLoadToUser, eoNoFlush);
            ComWriteLn (lang (laUserNotFound) + '|', eoMacro + eoCodes);
          End Else
          Begin
            GetUser (UpLoadToUser, tR, Cnf. Aliases);
            UpLoadToUser := tR. Name;
            Break;
          End;
        Until False;

        SetInputCap (NoCaps, AllChars);
      End;
    End;

  If TransferMode = Transmit Then
  Begin
    If FileName <> '' Then
    Begin
      oSizeOfAll := SizeOfAll;
      SizeOfAll := gFileSize (FileName);
    End;

    If Not AutoDL Then
    Begin
      UpdateUserMacro;

      Cls;
      ComWriteLn (lang (laDownLoadHead1), eoMacro + eoCodes);
      ComWriteLn (lang (laDownLoadHead2), eoMacro + eoCodes);

      If Not Local Then
      Begin
        ComWriteLn (lang (laDownLoadHead3), eoMacro + eoCodes);
        ComWriteLn (lang (laDownLoadHead4), eoMacro + eoCodes);

        { CopyLocal processing ... }
        DirCreated := False;

        For i := 0 To F2Transfer^. Count-1 Do
        Begin
          PT := F2Transfer^. At (i);

          If PT^. AreaNum <= 0 Then
            Continue;

          SmartChangeFArea (PT^. GroupNum, PT^. AreaNum, LastGroup, LastArea);
          If Not FileArea. CopyLocal Then
            Continue;

          If R. Protocol in ['1'..'8'] Then
          Begin
            Repeat
              S := Cnf. DoorInfoDir + TempFName + 'cl.tor'
            Until Not FileExists (S);

            Reps^. InsLine (PT^. PathName^ + #255 + S);
          End Else
          Begin
            If Not DirCreated Then
            Begin
              Repeat
                CLDirName := Cnf. DoorInfoDir + TempFName;
              Until Not DirExists (CLDirName);

              MkDir (CLDirName);
              DirCreated := IOResult = 0;
            End;

            S := AddBackSlash (CLDirName) + JustFileName (PT^. PathName^);
            Reps^. InsLine (S);
          End;

          If Not tCopyFile (PT^. PathName^, S, True) Then
            LogWrite ('!', 'CopyLocal error happened (' + PT^. PathName^ +
              ' -> ' + S + ')')
          Else
          Begin
            Assign (F, S);
            SetFAttr (F, Archive);
          End;
        End;
      End;
    End;
  End Else
  Begin
    If State = tsPrivate Then
      ULDir := Cnf. PrivUploadsDir
    Else
      If FileName = '' Then ULDir := FileArea. ULPath
                       Else ULDir := FileName;
    If AutoDL Then
      GetDir (0, ULDir)
    Else
      If Not CheckFreeSpace Then
        Exit;
  End;

  If Local Then
    Case TransferMode Of
      Transmit : If Not LocalDownLoad Then
                   Exit;
       Receive : LocalUpLoad;
    End
  Else
  Begin
    ComWriteLn (#10, 0);
    SmartLine;
    SmartLine;

    If TransferMode = Transmit Then
    Begin
      LogWrite ('&', 'DownLoad. Using ' + ProtocolDef. Name + ' protocol');
      LogWrite ('&', Long2Str (F2Transfer^. Count) +
        ' file(s) selected, total size: ' + NiceFileSize (SizeOfAll));
      LogWrite ('&', 'Estimated transfer time : ' +
        Long2Str (Round (EstimatedTransferTime (SizeOfAll, R. AvgCPS,
        GetConnectSpeed) / 60)) + ' min. at ' + Long2Str (GetConnectSpeed) +
        ' bps, average cps: ' + Long2Str (R. AvgCPS));
      ExportDescs;
      ComWriteLn (lang (laSendFile), eoMacro + eoCodes);
    End
    Else
      ComWriteLn (lang (laRecvFile), eoMacro + eoCodes);

  {$IFDEF OS2}
    NeedThreadClose := True;
    Repeat
    Until Not ThreadLocked;
  {$ENDIF}

    xFile := '';

    If (TransferMode = Receive) And
       (((Ord (R. Protocol) - Ord ('1')) in [Xmodem, XmodemCRC, Xmodem1K,
         Xmodem1KG]) Or
        (Not ProtocolDef. Batch And Not (R. Protocol in ['1'..'8']))) Then
    Begin
      Repeat
        xFile := JustFileName (Trim (GetAnswer (lang (laAskFileName), 13, 0,
          '')));
      Until Not isDevice (xFile);

      If xFile = '' Then
        Exit;

      xFile := FileArea. ULPath + xFile;
    End;

    If Not (R. Protocol in ['1'..'8']) Then {external protocol}
    Begin
      tDeleteFile (ProtocolDef. Log);
      GetDir (0, S);

      If TransferMode = Transmit Then
      Begin
        KWord := ProtocolDef. DLKeyWord;

        {generating list file}

        Assign (List, ProtocolDef. List);
        ReWrite (List);

        For i := 0 To F2Transfer^. Count-1 Do
        Begin
          PT := F2Transfer^. At (i);

          If PT^. AreaNum > 0 Then
          Begin
            SmartChangeFArea (PT^. GroupNum, PT^. AreaNum, LastGroup, LastArea);

            If FileArea. CopyLocal Then
            Begin
              WriteLn (List, AddBackSlash (CLDirName) +
                JustFileName (PT^. PathName^));
              Continue;
            End;
          End;

          WriteLn (List, PT^. PathName^);
        End;

        Close (List);

        SmartChDir (Cnf. DoorInfoDir);
        DosShell (TranslateExecParams (ProtocolDef. DLCommand), exCommand,
          False);
        tDeleteFile (ProtocolDef. List);
      End Else
      Begin
        KWord := ProtocolDef. ULKeyWord;
        If xFile = '' Then
          xFile := ULDir;
        SmartChDir (Cnf. DoorInfoDir);
        DosShell (TranslateExecParams (PlaceSubStr (ProtocolDef. ULCommand,
          '*U', xFile)), exCommand, False);
      End;

      SmartChDir (S);
      If IOResult <> 0 Then;

      Assign (List, ProtocolDef. Log);
      ReSet (List);

      If IOResult = 0 Then
      Begin
        LogWrite ('&', 'Reading protocol log file ' + ProtocolDef. Log);

        While Not EoF (List) Do
        Begin
          ReadLn (List, S);
          FName := '';

          i := Pos (KWord, S);
          If i <> 0 Then
          Begin
            FName := UpString (ExtractWord (ProtocolDef. WordOffs + 1,
              Copy (S, i + Length (KWord), 255), SpaceAndComma));

            LogWrite ('&', 'File name is: ' + FName);

            If TransferMode = Receive Then
            Begin
              LogWrite ('*', '!R: ' + sm (smFile) + NiceFileName (FName, 40));
              If State = tsUpLoadMsg Then
                tRenameFile (FName, Cnf. DoorInfoDir + 'msgtmp.')
              Else
                UpLoadOk (FName, gFileSize (FName), 0);
            End Else
            Begin
              If FName = JustFileName (FName) Then
              Begin
                mFindFirst (FileArea. DLPath, FName, AnyFile-Directory, SRec);
                If DOSerror = 0 Then
                  FName := UpString (SRec. Name);
                mFindDone (SRec);
              End;

              For i := 0 To F2Transfer^. Count-1 Do
                If UpString (PTagFileRec (F2Transfer^. At (i))^. PathName^) =
                   FName Then
                Begin
                  LogWrite ('*', '!S: ' + sm (smFile) + NiceFileName (FName, 40));
                  DownLoadOk (gFileSize (FName), i);
                End;
            End;
          End
          Else
            LogWrite ('&', 'Line "' + S + '" ignored');
        End;

        Close (List);
      End
      Else
        LogWrite ('!', sm (smFile) + ProtocolDef. Log + sm (smNotFound));
    End Else
    Begin {internal protocols}
      Case Ord (R. Protocol) - Ord ('1') Of
        Zmodem    : InitZmodem (Prot, Port);
        Zmodem8K  : InitCustomZmodem (Prot, Port, DefProtocolOptions Or
                      apZmodem8K);
        Ymodem    : InitYmodem (Prot, Port, True, False);
        YmodemG   : InitYmodem (Prot, Port, True, True);
        Xmodem    : InitXmodem (Prot, Port, False, False);
        XmodemCRC : InitCustomXmodem (Prot, Port, True, True, 0);
        Xmodem1K  : InitXmodem (Prot, Port, True, False);
        Xmodem1KG : InitXmodem (Prot, Port, True, True);
      End;

      If AsyncStatus <> ecOk Then
      Begin
        LogWrite ('!', sm (smErrorProtInit));
        Exit;
      End;

      GetDir (0, CurDir);

      If TransferMode = Receive Then
        If AutoDL Then
          SetDestinationDirectory (Prot, CurDir)
        Else
          If State = tsPrivate Then
            SetDestinationDirectory (Prot, Cnf. PrivUploadsDir)
          Else
            If FileName = '' Then
              SetDestinationDirectory (Prot, FileArea. ULPath)
            Else
              SetDestinationDirectory (Prot, FileName);

      InitProtBox (Prot, TransferMode);
      HiddenCursor;

      {SetErrorProc (Port, Errorz);}
      SetShowStatusProc (Prot, StatusP);
      SetBackgroundProc (Prot, BackgroundProc);
      SetLogFileProc (Prot, LogFile);
      SetOverwriteOption (Prot, WriteRename);
      SetAcceptFileFunc (Prot, AcceptFile);
      SetBlockWait (Prot, RelaxedBlockWait);

      If FileName <> '' Then
        SetFileMask (Prot, FileName)
      Else
        If Prot^. PData^. BatchProtocol Then
          SetNextFileFunc (Prot, Next2Transfer)
        Else
          If F2Transfer^. Count > 0 Then
            SetFileMask (Prot, PTagFileRec (F2Transfer^. At (0))^. PathName^);

      If TransferMode = Transmit Then
        Case Ord (R. Protocol) - Ord ('1') Of
          Zmodem, Zmodem8K:
            Begin
              SetRecoverOption (Prot, True);
              ProtocolTransmitZM (Prot);
              DoneZmodem (Prot);
            End;
          Ymodem, YmodemG:
            Begin
              ProtocolTransmitYM (Prot);
              DoneYmodem (Prot);
            End;
          Xmodem, XmodemCRC, Xmodem1K, Xmodem1KG:
            Begin
              ProtocolTransmitXM (Prot);
              DoneXmodem (Prot);
            End;
        End
      Else
      Begin
        SetTitle ('receiving file(s)');

        Case Ord (R. Protocol) - Ord ('1') Of
          Zmodem, Zmodem8K:
            Begin
              SetRecoverOption (Prot, True);
              ProtocolReceiveZM (Prot);
              DoneZmodem (Prot);
            End;

          Ymodem, YmodemG:
            Begin
              ProtocolReceiveYM (Prot);
              DoneYmodem (Prot);
            End;

          Xmodem, XmodemCRC, Xmodem1K, Xmodem1KG:
            Begin
              SetReceiveFileName (Prot, xFile);
              ProtocolReceiveXM (Prot);
              DoneXmodem (Prot);
            End;
        End;
      End;

      {SetErrorProc (Port, NoErrorProc);}
      If AsyncStatus <> ecOk Then
        ProtocolMsg (sm (smTransFail));
      DoneProtBox;
    End;
  End;

{$IFDEF OS2}
  NeedThreadClose := False;
{$ENDIF}

  NormalCursor;
  ClearSent;

  If (TransferMode = Receive) And (trState in [tsNormal, tsPrivate]) And
     Not AutoDL
  Then
    UploadDone;

  ShowStatusBar;

  If TransferMode = Receive Then
  Begin
    ExecScript ('upload');
    ExecRexx ('upload');
  End Else
  Begin
    tDeleteFile (Cnf. DoorInfoDir + 'file_id.1');
    ExecScript ('download');
    ExecRexx ('download');
  End;

  ShutDown;
End;

Procedure DownLoad;

  Procedure ShowList;
  Var
    LastGroup, LastArea,
    OldGroup, OldArea,
    CurrGroup, CurrArea  : LongInt;
    PT                   : PTagFileRec;
    i                    : Integer;

  Begin
    Cls;
    If F2Transfer^. Count = 0 Then
    Begin
      ComWriteLn (lang (laNoFilesSelected), eoMacro + eoCodes);
      Exit;
    End;

    OldGroup := R. FileGroup;
    OldArea := R. FileArea;
    LastGroup := OldGroup;
    LastArea := OldArea;
    CurrGroup := 0;
    CurrArea := 0;

    InitMore (0);

    For i := 0 To F2Transfer^. Count-1 Do
    Begin
      PT := F2Transfer^. At (i);
      If (PT^. AreaNum <> CurrArea) Or (PT^. GroupNum <> CurrGroup) Then
      Begin
        Case PT^. AreaNum Of
          -10 : ComWriteLn ('|    ' + EmuRelColor (Cnf. ColorScheme [umItem]) +
                  lang (laPrevTagArea) + '|', eoMacro + eoCodes);
          -20 : ComWriteLn ('|    ' + EmuRelColor (Cnf. ColorScheme [umItem]) +
                  lang (laFromUsersArea) + '|', eoMacro + eoCodes);
        Else
          SmartChangeFArea (PT^. GroupNum, PT^. AreaNum, LastGroup, LastArea);
          UpdateFAreaMacro;
          ComWriteLn ('|' + EmuRelColor (Cnf. ColorScheme [umItem]) +
            lang (laScanFileAreas) + '|', eoMacro + eoCodes);
        End;

        CurrGroup := PT^. GroupNum;
        CurrArea := PT^. AreaNum;
        Inc (MoreLines, 3);
      End;

      ComWrite (EmuRelColor (Cnf. ColorScheme [flTagNum]) +
        LeftPad (Long2Str (i + 1), 3) + ' ', eoNoFlush);
      ComWrite (EmuRelColor (Cnf. ColorScheme [flFileName]) +
        Pad (UpString (JustFileName (PT^. PathName^)), 12) + ' ', eoNoFlush);
      ComWrite (EmuRelColor (Cnf. ColorScheme [flSize]) +
        LeftPad (NiceFileSize (PT^. Size), 6), eoNoFlush);

      If PT^. Free Then
        ComWrite (' ' + lang (laFreeDL), eoMacro + eoCodes + eoNoFlush);

      If (CurrArea = -20) And (PT^. FromName <> Nil) Then
        ComWrite (' ' + lang (laFileFromUser) + PT^. FromName^, eoMacro +
          eoCodes + eoNoFlush);

      ComWriteLn ('', 0);

      If Not More Then
        Break;
    End;

    SmartChangeFArea (OldGroup, OldArea, LastGroup, LastArea);
  End;

  Function DeleteFromList: Boolean;

    Function Del (FNum: Integer): Boolean;
    Begin
      If (FNum <= F2Transfer^. Count) And (FNum > 0) Then
      Begin
        With PTagFileRec (F2Transfer^. At (FNum - 1))^ Do
        Begin
          Dec (SizeOfAll, Size);
          If (FromName <> Nil) And
             (AddBackSlash (UpString (JustPathName (PathName^))) =
              UpString (Cnf. PrivUploadsDir))
          Then
            tDeleteFile (PathName^);

          ChangePString (PathName, '!');
        End;
        Del := True;
      End
      Else
        Del := False;
    End;

  Var
    i, j, sItem, eItem : Integer;
    Token              : String [8];
    Tmp                : String;

  Begin
    DeleteFromList := False;
    ComWrite (lang (laDelFileNo), eoMacro + eoCodes + eoNoFlush);
    Tmp := '';
    ComReadLn (Tmp, 50, ofAllowEmpty);
    If Trim (Tmp) = '' Then
      Exit;

    For i := 1 To WordCount (Tmp, SpaceAndComma) Do
    Begin
      Token := ExtractWord (i, Tmp, SpaceAndComma);
      If ConsistsOf (Token, ['0'..'9', '-']) Then
        If Pos ('-', Token) <> 0 Then
        Begin
          sItem := Str2Long (ExtractWord (1, Token, MinusOnly));
          eItem := Str2Long (ExtractWord (2, Token, MinusOnly));
          If sItem < 1 Then
            sItem := 1;
          If eItem > F2Transfer^. Count Then
            eItem := F2Transfer^. Count;

          For j := sItem To eItem Do
            If Del (j) Then
              DeleteFromList := True;
        End
        Else
          DeleteFromList := Del (Str2Long (Token));
    End;

    ClearSent;
  End;

Var
  AllSize         : LongInt;
  i, j            : Integer;
  TmpC            : Char;
  Found, FreeFile : Boolean;
  DirInfo         : mSearchRec;
  FileRec         : TTagFileRec;
  Tmp, AddStr     : PathStr;

Label
  AddFiles,
  Menu;

Begin
  ClearInputHistory;
  ShowList;
  If F2Transfer^. Count <> 0 Then
    Goto Menu;

AddFiles:
  If (FileArea. DL_Security > R. Security) Or
     (Not FlagsValid (R. Flags, FileArea. DL_Flags)) Then
  Begin
    ComWriteLn (lang (laSecurityLow), eoMacro + eoCodes);
    Goto Menu;
  End;

  If (FileArea. MinSpeed > GetConnectSpeed) And Not Local Then
  Begin
    Message (lang (laTooSlow));
    Goto Menu;
  End;

  AddStr := UpString (Trim (GetAnswer (lang (laAddFile), 80, ofAllowEmpty +
    ofHistory, '')));

  If AddStr = '' Then
    Goto Menu;

  Found := False;

  For j := 1 To WordCount (AddStr, SpaceAndComma) Do
  Begin
    Tmp := ExtractWord (j, AddStr, SpaceAndComma);
    mFindFirst (FileArea. DLPath, Tmp, AnyFile-VolumeID-Directory, DirInfo);

    While DosError = 0 Do
    Begin
      If InFilesBBS (DirInfo. Info. Name) Then
      Begin
        Found := True;
        OutFilesBBSString (DirInfo. Info. Name);
        TmpC := Query_YNQ (lang (laSelectFile), True);
        FreeFile := IsFree (DirInfo. Info. Name);

        If TmpC = 'y' Then
        Begin
          AllSize := R. TodayK + Trunc ((DirInfo. Info. Size + SizeOfAll) /
            1024);
          If (AllSize < R. DailySize) Or FreeFile Then
          Begin
            If Local Or FreeFile Or Not (Cnf. CheckDlTime And (AllSize >
               Round ((GetConnectSpeed / 10 * (R. TotalTime - TimeDiff (
               EnterTime, MidSec))) / 1024))) Then
            Begin
              FileRec. GroupNum := R. FileGroup;
              FileRec. AreaNum := R. FileArea;
              FileRec. Size := DirInfo. Info. Size;
              FileRec. PathName := NewStr (UpString (DirInfo. Name));
              FileRec. FromName := Nil;
              FileRec. Free := FreeFile;
              InsertInTagList (FileRec);
            End
            Else
              Message (lang (laDLTimeLimit));
          End
          Else
            Message (lang (laDLLimitExceed));
        End
        Else
          If TmpC = 'q' Then
            Break;
      End;

      mFindNext (DirInfo);
    End;

    mFindDone (DirInfo);
  End;

  If Not Found Then
  Begin
    ComWriteLn (lang (laAddFileNotFound), eoMacro + eoCodes);
    Goto AddFiles;
  End;

Menu:
  i := MenuBar (#13#10 + lang (laFAreaLine), lang (laFAreaKeys) + #13);

  Case i Of

    1 : Begin
          ComWriteLn ('', 0);
          Goto AddFiles;
        End;

    2 : Begin
          ShowList;
          ComWriteLn ('', 0);
          If DeleteFromList Then
            ShowList;
          Goto Menu;
        End;

    3 : Exit;

    4 : Begin
          ShowList;
          Goto Menu;
        End;

    5 : Begin
          If (Not R. Frames) Or (R. Emu = teTty) Then
            ComWriteLn ('', 0);

          If (Query (lang (laClearList), False, ofFramed)) And
             (F2Transfer^. Count > 0) Then
          Begin
            For i := 0 To F2Transfer^. Count-1 Do
              With PTagFileRec (F2Transfer^. At (i))^ Do
                If (FromName <> Nil) And (AddBackSlash (UpString (JustPathName
                   (PathName^))) = UpString (Cnf. PrivUploadsDir))
                Then
                  tDeleteFile (PathName^);

            F2Transfer^. FreeAll;
            SizeOfAll := 0;
          End;

          ShowList;
          Goto Menu;
        End;

    6 : Begin
          If Local Then
          Begin
            LogWrite ('&', Long2Str (F2Transfer^. Count) +
              ' files selected, total size: ' + NiceFileSize (SizeOfAll));
            LogWrite ('&', 'Download mode: LOCAL');
          End;

          If F2Transfer^. Count > 0 Then
            Transfer ('', Transmit, tsNormal)
          Else
            Message (#13#10 + lang (laNoFilesSelected));
        End;
  End;
End;

End.
