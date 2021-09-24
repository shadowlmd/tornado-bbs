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

{Conditional defines that may affect this unit}
{$I APDEFINE.INC}

{*********************************************************}
{*                    APABSPCL.PAS 2.03                  *}
{*     Copyright (c) TurboPower Software 1991.           *}
{* Portions copyright (c) Information Technology 1989,   *}
{*    and used under license to TurboPower Software      *}
{*                 All rights reserved.                  *}
{*********************************************************}

{$IFDEF UseOOP}
  !! STOP COMPILE - this unit requires UseOOP to be undefined
{$ENDIF}

Unit ApAbsPcl;
  {-Provides "abstract" protocol data definitions and general procedures}

Interface

Uses
{$IFNDEF OS2}
  ApMisc,
  ApPort,
{$ELSE}
  ApOS2,
{$ENDIF}
  DOS,
  ApTimer,
  ApCom,
  ApSame,
  OpInline,
  Crc,
  tMisc;

Const
  AvgCPS : LongInt = 0;

Const
  indexAPABSPCL = 100;
  indexAPXMODEM = 200;
  indexAPYMODEM = 300;
  indexAPZMODEM = 400;

  {Compile-time options}
{$IFDEF RealMode}
  FileBufferSize    = 8192; {Size of working buffer for receive/xmit files}
{$ELSE}
  FileBufferSize    = 16384;
{$ENDIF}
  DefHandshakeWait  = 182;  {Wait time for resp during handshake (10 sec)}
  DefHandshakeRetry = 10;   {Number of times to retry handshake}
  DefTransTimeout   = 1092; {Tics to wait for receiver flow control release}
  DefStatusInterval = 91;   {5 seconds between status updates}

  {Run-time options}
  BlockFillChar : Char = ^Z;   {Fill character for partial protocol blocks}
  GmtHourOffset : Integer = 0; {Default no GMT adjustments}
  TelixDelay    : Byte = 9;    {Delay handshake for 9 tics (for Telix)}

  {---- Option codes for protocols ----}
  apIncludeDirectory  = $0001;  {True to include directory in file names}
  apHonorDirectory    = $0002;  {True to honor directory in file names}
  apRTSLowForWrite    = $0004;  {True to lower RTS during disk writes}
  apKermitNoStripName = $0008;  {True to *not* strip Kermit filenames}
  apBP2KTransmit      = $0100;  {True if 2K B+ transmit desired}
  apKermitDisplay     = $1000;  {True to honor KDisplay packets}
  apKermitLongPackets = $2000;  {True to support long packets}
  apKermitSWC         = $4000;  {True to support SWC}
  apZmodem8K          = $8000;  {True to support 8K blocks}

  {---- Default options for AbstractProtocol ----}
  DefProtocolOptions : Word = 0;
  BadProtocolOptions : Word = apKermitLongPackets + apKermitSWC + apZmodem8K;

  {Block check codes}
  bcNone      = 0; {No block checking}
  bcChecksum1 = 1; {Basic checksum}
  bcChecksum2 = 2; {Two byte checksum}
  bcCrc16     = 3; {16 bit Crc}
  bcCrc32     = 4; {32 bit Crc}
  bcCrcK      = 5; {Kermit style Crc}

Const
  {Constants for supported protocol types}
  Xmodem    = 0;
  XmodemCRC = 1;
  Xmodem1K  = 2;
  Xmodem1KG = 3;
  Ymodem    = 4;
  YmodemG   = 5;
  Zmodem    = 6;
  Kermit    = 7;
  Ascii     = 8;
  BPlus     = 9;
  NoneProt  = 10;

Type
  {General protocol states}
  ProtocolStateType = (
    psReady,           {Ok to call again immediately}
    psWaiting,         {Protocol is waiting, ok to do something else}
    psFinished);       {Protocol is finished}

  {Generic protocol record pointer}
  ProtocolRecPtr = ^ProtocolRec;

  {Action to take if not allowed to write a file}
  WriteFailOptions = (WriteFail, WriteRename, WriteAnyway, WriteResume);

  {For storing received and transmitted blocks}
  DataBlockType = Array [1..1024] Of Char;

  {For buffering received and transmitted files}
  FileBufferArray = Array [0..FileBufferSize - 1] Of Byte;

  {For specifying log file calls}
  LogFileType = (lfReceiveStart,
    lfReceiveOk,
    lfReceiveFail,
    lfReceiveSkip,
    lfTransmitStart,
    lfTransmitOk,
    lfTransmitFail,
    lfTransmitSkip);

  {User procedure types}
  ShowStatusProc = Procedure (P: ProtocolRecPtr; Starting, Ending: Boolean);
  NextFileFunc = Function (P: ProtocolRecPtr; Var FName: PathStr): Boolean;
  LogFileProc = Procedure (P: ProtocolRecPtr; LogFileStatus: LogFileType);
  AcceptFileFunc = Function (P: ProtocolRecPtr): Boolean;
  UserBackProc = Procedure (P: ProtocolRecPtr);

  {Procedure types for reading/writing protocol blocks}
  PrepFinishProc = Procedure (P: ProtocolRecPtr);
  ReadProtProc = Function (P: ProtocolRecPtr; Var Block: DataBlockType;
                           Var BlockSize: Word): Boolean;
  WriteProtProc = Function (P: ProtocolRecPtr; Var Block: DataBlockType;
                            BlockSize: Word): Boolean;

  {Standard protocol data block}
  ProtocolDataPtr = ^ProtocolData;
  ProtocolData = Record
                   PR               : PortRecPtr;        {Associated port}
                   FileBuffer       : ^FileBufferArray;  {For reading/writing files}
                   BlockCheck       : LongInt;           {Block check value}
                   BytesRemaining   : LongInt;           {Bytes not yet transferred}
                   BytesTransferred : LongInt;           {Bytes already transferred}
                   ElapsedTics      : LongInt;           {Elapseds tics as of last block}
                   FileOfs          : LongInt;           {Current file offset}
                   StartOfs         : LongInt;           {Holds starting offset of file}
                   BufLen           : LongInt;           {Holds data length in buffer}
                   BufValid         : LongInt;           {Holds valid data length in buffer}
                   RealFileOfs      : LongInt;           {Real file offset}
                   SrcFileLen       : LongInt;           {Size of file (in bytes)}
                   SrcFileDate      : LongInt;           {Timestamp of source file}
                   InitFilePos      : LongInt;           {Initial file pos during resumes}
                   UserStatus       : ShowStatusProc;    {Hook for user display}
                   UserBack         : UserBackProc;      {Main background hook}
                   NextFile         : NextFileFunc;      {NextFile function}
                   LogFile          : LogFileProc;       {User proc to call when file received}
                   AcceptFile       : AcceptFileFunc;    {User hook to accept rcvd filename}
                   PrepareReading   : PrepFinishProc;    {Proc for preparing file read}
                   ReadProtocolBlock : ReadProtProc;     {Proc for reading blocks}
                   FinishReading    : PrepFinishProc;    {Proc for closing read file}
                   PrepareWriting   : PrepFinishProc;    {Proc for preparing file writes}
                   WriteProtocolBlock : WriteProtProc;   {Proc for writing blocks}
                   FinishWriting    : PrepFinishProc;    {Proc for closing write file}
                   Timer            : EventTimer;        {Used to time a transfer}
                   StatusTimer      : EventTimer;        {How often to show status}
                   BlockLen         : Word;              {Either 128 or 1024}
                   BlockNum         : Word;              {Current block number}
                   BlockErrors      : Word;              {Number of tries for block}
                   TotalErrors      : Word;              {Number of total tries}
                   StatusInterval   : Word;              {Tics between status updates}
                   HandshakeWait    : Word;              {Wait seconds during handshaking}
                   HandshakeRetry   : Word;              {Number of retries during handshaking}
                   HandshakeAttempt : Word;              {Current handshake attempt}
                   apFlags          : Word;              {AbstractProtocol options}
                   TransTimeout     : Word;              {Tics to wait for trans freespace}
                   ActCPS           : Word;              {Port or modem CPS}
                   OverHead         : Word;              {Overhead bytes per block}
                   TurnDelay        : Word;              {MSec turnaround delay}
                   ProtType         : Byte;              {Protocol type}
                   CheckType        : Byte;              {Code for block check type}
                   InProgress       : Byte;              {Non-zero if protocol in progress}
                   EotCheckCount    : Byte;              {Number of Eot retries required}
                   SaveMode         : Byte;              {Save FileMode}
                   ForceStatus      : Boolean;           {Force status update}
                   FileOpen         : Boolean;           {True if file open in protocol}
                   BatchProtocol    : Boolean;           {True if protocol supports batch}
                   GotOneFile       : Boolean;           {True if we've received one file}
                   FindingFirst     : Boolean;           {NextFileMask flag}
                   WriteFailOpt     : WriteFailOptions;  {Rules for overwriting files}
                   WorkFile         : File;              {Temp file for Get/PutProtocolBlock}
                   CurRec           : SearchRec;         {NextFileMask search record}
                   PathName         : PathStr;           {Complete path name of current file}
                   SearchMask       : PathStr;           {NextFileMask search mask}
                   DestDir          : DirStr;            {Destination directory}
                 End;

  {Generic protocol record}
  ProtocolRec = Record
                  PData: ProtocolDataPtr;
                End;

Procedure InitProtocolData (Var P: ProtocolDataPtr; PortPtr: PortRecPtr;
                            Options: Word);
Procedure DoneProtocolData (Var P: ProtocolDataPtr);
Procedure SetShowStatusProc (P: ProtocolRecPtr; SProc: ShowStatusProc);
Procedure SetNextFileFunc (P: ProtocolRecPtr; NFFunc: NextFileFunc);
Procedure SetFileMask (P: ProtocolRecPtr; NewMask: PathStr);
Procedure SetDestinationDirectory (P: ProtocolRecPtr; Dir: DirStr);
Procedure SetReceiveFilename (P: ProtocolRecPtr; Const Fname: PathStr);
Procedure SetLogFileProc (P: ProtocolRecPtr; LFP: LogFileProc);
Procedure SetAcceptFileFunc (P: ProtocolRecPtr; AFP: AcceptFileFunc);
Procedure SetBackgroundProc (P: ProtocolRecPtr; BP: UserBackProc);
Procedure SetOverwriteOption (P: ProtocolRecPtr; Opt: WriteFailOptions);
Function GetFilename (P: ProtocolRecPtr): PathStr;
Function GetPathname (P: ProtocolRecPtr): PathStr;
Function GetFileSize (P: ProtocolRecPtr): LongInt;
Function GetBytesRemaining (P: ProtocolRecPtr): LongInt;
Function GetBytesTransferred (P: ProtocolRecPtr): LongInt;
Function GetElapsedTics (P: ProtocolRecPtr): LongInt;
Function GetBlockErrors (P: ProtocolRecPtr): Word;
Function GetTotalErrors (P: ProtocolRecPtr): Word;
Function GetProtocol (P: ProtocolRecPtr): Byte;
Function GetBlockSize (P: ProtocolRecPtr): Word;
Function GetCheckType (P: ProtocolRecPtr): Byte;
Function GetInitialFilePos (P: ProtocolRecPtr): LongInt;
Procedure ShowFirstStatus (P: ProtocolRecPtr);
Procedure ShowLastStatus (P: ProtocolRecPtr);
Procedure ResetStatus (P: ProtocolRecPtr);
Function Crc32ofFile (P: ProtocolRecPtr; Len: LongInt): LongInt;
Procedure NoStatus (P: ProtocolRecPtr; Starting, Ending: Boolean);
Function NoNextFile (P: ProtocolRecPtr): Boolean;
Procedure NoLogFile (P: ProtocolRecPtr; LogFileStatus: LogFileType);
Function NoAcceptFile (P: ProtocolRecPtr): Boolean;
Procedure NoUserBack (P: ProtocolRecPtr);
Function NextFileMask (P: ProtocolRecPtr; Var FName: PathStr): Boolean;
Function AcceptOneFile (P: ProtocolRecPtr): Boolean;
Procedure apPrepareReading (P: ProtocolRecPtr);
Procedure apFinishReading (P: ProtocolRecPtr);
Function apReadProtocolBlock (P: ProtocolRecPtr; Var Block: DataBlockType;
                              Var BlockSize: Word): Boolean;
Procedure apPrepareWriting (P: ProtocolRecPtr);
Procedure apFinishWriting (P: ProtocolRecPtr);
Function apWriteProtocolBlock (P: ProtocolRecPtr; Var Block: DataBlockType;
                               BlockSize: Word): Boolean;
Procedure apUserStatus (P: ProtocolRecPtr; Starting, Ending: Boolean);
Function TrimZeros (S: String): String;
Function OctalStr (L: LongInt): String;
Function OctalStr2Long (S: String): LongInt;
Function PackToYMTimeStamp (RawTime: LongInt): LongInt;
Function YMTimeStampToPack (YMTime: LongInt): LongInt;
Function CurrentTimeStamp: LongInt;
Function ValidatePathname (Const Pathname: String): String;

Implementation

Const
  StartDate: DateTimeRec = (D: 135140; T: 0); {Set to 1/1/1970 00:00 GMT}

Procedure InitProtocolData (Var P: ProtocolDataPtr; PortPtr: PortRecPtr;
                            Options: Word);
    {-Allocates and initializes a protocol data block}
  Begin
    AsyncStatus := ecOk;

    {Allocate a protocol data block}
    If Not GetMemCheck (P, SizeOf (ProtocolData)) Then
    Begin
      GotError (PortPtr, ecOutOfMemory);
      Exit;
    End;

    With P^ Do
    Begin
      If Not GetMemCheck (FileBuffer, FileBufferSize) Then
      Begin
        FreeMemCheck (P, SizeOf (ProtocolData));
        GotError (PortPtr, epFatal + ecOutOfMemory);
        Exit;
      End;

      {Save the port record}
      PR := PortPtr;

      {Initialize the protocol fields}
      UserStatus := NoStatus;
      HandshakeWait := DefHandshakeWait;
      HandshakeRetry := DefHandshakeRetry;
      BlockLen := 128;
      PathName := '';
      SrcFileLen := 0;
      SrcFileDate := 0;
      ElapsedTics := 0;
      BytesRemaining := 0;
      BytesTransferred := 0;
      InProgress := 0;
      EotCheckCount := 1;
      BatchProtocol := False;
      WriteFailOpt := WriteFail;
      FileOpen := False;
      NextFile := NextFileMask;
      SearchMask := '';
      apFlags := Options;
      LogFile := NoLogFile;
      AcceptFile := NoAcceptFile;
      UserBack := NoUserBack;
      DestDir := '';
      CheckType := bcNone;
      ActCPS := PR^. CurBaud Div 10;
      StatusInterval := DefStatusInterval;
      OverHead := 0;
      TurnDelay := 0;
      InitFilePos := 0;
      TransTimeout := DefTransTimeout;
      PrepareReading := apPrepareReading;
      ReadProtocolBlock := apReadProtocolBlock;
      FinishReading := apFinishReading;
      PrepareWriting := apPrepareWriting;
      WriteProtocolBlock := apWriteProtocolBlock;
      FinishWriting := apFinishWriting;
    End;
  End;

  Procedure DoneProtocolData (Var P: ProtocolDataPtr);
    {-Disposes of a protocol data block}
  Begin
    FreeMemCheck (P^. FileBuffer, FileBufferSize);
    FreeMemCheck (P, SizeOf (ProtocolData));
  End;

  Procedure SetShowStatusProc (P: ProtocolRecPtr; SProc: ShowStatusProc);
    {-Sets a user status function}
  Begin
    P^. PData^. UserStatus := SProc;
  End;

  Procedure SetNextFileFunc (P: ProtocolRecPtr; NFFunc: NextFileFunc);
    {-Sets function for batch protocols to call to get file to transmit}
  Begin
    P^. PData^. NextFile := NFFunc;
  End;

  Procedure SetFileMask (P: ProtocolRecPtr; NewMask: PathStr);
    {-Sets dir/file mask for built-in NextFileMask function}
  Begin
    P^. PData^. SearchMask := NewMask;
  End;

  Procedure SetDestinationDirectory (P: ProtocolRecPtr; Dir: DirStr);
    {-Set the destination directory for received files}
  Begin
    P^. PData^. DestDir := UpString (Dir);
  End;

  Procedure SetReceiveFilename (P: ProtocolRecPtr; Const Fname: PathStr);
    {-Give a name to the file to be received}
  Begin
    With P^. PData^ Do
      If (DestDir <> '') And (JustPathName (Fname) = '') Then
        Pathname := AddBackSlash (DestDir) + Fname
      Else
        Pathname := Fname;
  End;

  Procedure SetLogFileProc (P: ProtocolRecPtr; LFP: LogFileProc);
    {-Sets a procedure to be called when a file is received}
  Begin
    P^. PData^. LogFile := LFP;
  End;

  Procedure SetAcceptFileFunc (P: ProtocolRecPtr; AFP: AcceptFileFunc);
    {-Sets a procedure to be called when a file is received}
  Begin
    P^. PData^. AcceptFile := AFP;
  End;

  Procedure SetBackgroundProc (P: ProtocolRecPtr; BP: UserBackProc);
    {-Sets a background procedure to be called while a file is transferred}
  Begin
    P^. PData^. UserBack := BP;
  End;

  Procedure SetOverwriteOption (P: ProtocolRecPtr; Opt: WriteFailOptions);
    {-Set option for what to do when the destination file already exists}
  Begin
    P^. PData^. WriteFailOpt := Opt;
  End;

  Function GetFilename (P: ProtocolRecPtr): PathStr;
    {-Returns the name of the current file}
  Begin
    GetFileName := JustFilename (P^. PData^. Pathname);
  End;

  Function GetPathname (P: ProtocolRecPtr): PathStr;
    {-Returns the complete pathname of the current file (if known)}
  Begin
    GetPathname := P^. PData^. Pathname;
  End;

  Function GetFileSize (P: ProtocolRecPtr): LongInt;
    {-Returns current file size (0 if no file active)}
  Begin
    GetFileSize := P^. PData^. SrcFileLen;
  End;

  Function GetBytesRemaining (P: ProtocolRecPtr): LongInt;
    {-Return bytes not yet transferred}
  Var
    BR : LongInt;

  Begin
    BR := P^. PData^. SrcFileLen - GetBytesTransferred (P);
    If BR > 0 Then GetBytesRemaining := BR
              Else GetBytesRemaining := 0;
  End;

  Function GetBytesTransferred (P: ProtocolRecPtr): LongInt;
    {-Returns bytes already transferred}
  Var
    TotalOverhead, OutBuff : Word;
    BT                     : LongInt;

  Begin
    With P^. PData^ Do
    Begin
    {$IFNDEF OS2}
      OutBuff := OutBuffUsed (PR);
      If OutBuff >= BlockLen Then
      Begin
        If BlockLen <> 0 Then
          TotalOverhead := Overhead * (OutBuff Div BlockLen)
        Else
          TotalOverhead := Overhead;
        BT := BytesTransferred - (OutBuff - TotalOverhead);
        If BT > 0 Then GetBytesTransferred := BT
                  Else GetBytesTransferred := 0;
      End
      Else
    {$ENDIF}
        GetBytesTransferred := BytesTransferred;
    End;
  End;

  Function GetElapsedTics (P: ProtocolRecPtr): LongInt;
    {-Returns tics since first block was sent (or received)}
  Begin
    GetElapsedTics := P^. PData^. ElapsedTics;
  End;

  Function GetBlockErrors (P: ProtocolRecPtr): Word;
    {-Returns the number of errors received this block}
  Begin
    GetBlockErrors := P^. PData^. BlockErrors;
  End;

  Function GetTotalErrors (P: ProtocolRecPtr): Word;
    {-Returns the number of errors recieved this transfer}
  Begin
    GetTotalErrors := P^. PData^. TotalErrors;
  End;

  Function GetProtocol (P: ProtocolRecPtr): Byte;
    {-Returns the current protocol type}
  Begin
    GetProtocol := P^. PData^. ProtType;
  End;

  Function GetBlockSize (P: ProtocolRecPtr): Word;
    {-Returns the current block size}
  Begin
    GetBlockSize := P^. PData^. BlockLen;
  End;

  Function GetCheckType (P: ProtocolRecPtr): Byte;
    {-Returns the bcXxx code for the block check type}
  Begin
    GetCheckType := P^. PData^. CheckType;
  End;

  Function GetInitialFilePos (P: ProtocolRecPtr): LongInt;
    {-Returns the file position at the start of resumed file transfer}
  Begin
    GetInitialFilePos := P^. PData^. InitFilePos;
  End;

  Procedure ShowFirstStatus (P: ProtocolRecPtr);
    {-Show (possible) first status}
  Begin
    With P^. PData^ Do
    Begin
      apUserStatus (P, InProgress = 0, False);
      Inc (InProgress);
    End;
  End;

  Procedure ShowLastStatus (P: ProtocolRecPtr);
    {-Reset field and show last status}
  Begin
    With P^. PData^ Do
      If InProgress <> 0 Then
      Begin
        Dec (InProgress);
        apUserStatus (P, False, InProgress = 0);
      End;
  End;

  Procedure ResetStatus (P: ProtocolRecPtr);
    {-Conditionally reset all status vars}
  Begin
    With P^. PData^ Do
    Begin
      If InProgress = 0 Then
      Begin
        {New protocol, reset status vars}
        SrcFileLen := 0;
        BytesRemaining := 0;
      End;
      BytesTransferred := 0;
      ElapsedTics := 0;
      BlockErrors := 0;
      TotalErrors := 0;
      BlockNum := 0;
    End;
  End;

  Function Crc32ofFile (P: ProtocolRecPtr; Len: LongInt): LongInt;
  Const
    BufSize = 4096;

  Type
    BufArray = Array [0..BufSize-1] Of Byte;

  Var
    Buffer      : ^BufArray;
    i           : Word;
    Crc, OldPos : LongInt;

  Begin
    AsyncStatus := ecOk;
    Crc := -1;

    With P^. PData^ Do
      If GetMemCheck (Buffer, BufSize) Then
      Begin
        OldPos := FilePos (WorkFile);
        Seek (WorkFile, 0);
        If Len <= 0 Then
          Len := $7FFFFFFF;

        Repeat
          i := BufSize;
          If Len < i Then
            i := Len;
          BlockRead (WorkFile, Buffer^, i, i);
          Crc := UpdateCrc32Buf (Buffer^, i, Crc);
          Dec (Len, i);
        Until i <> BufSize;

        Seek (WorkFile, OldPos);
        FreeMemCheck (Buffer, BufSize);
      End
      Else
        GotError (PR, epNonFatal + ecOutOfMemory);

    Crc32ofFile := Not Crc;
  End;

  {$F+}
  Procedure NoStatus (P: ProtocolRecPtr; Starting, Ending: Boolean);
    {-Empty show status procedure}
  Begin
  End;

  Function NoNextFile (P: ProtocolRecPtr): Boolean;
    {-Empty next file function -- always returns False}
  Begin
    NoNextFile := False;
  End;

  Procedure NoLogFile (P: ProtocolRecPtr; LogFileStatus: LogFileType);
    {-Empty LogFile procedure}
  Begin
  End;

  Function NoAcceptFile (P: ProtocolRecPtr): Boolean;
    {-Empty AcceptFile function}
  Begin
    NoAcceptFile := True;
  End;

  Procedure NoUserBack (P: ProtocolRecPtr);
    {-Empty UserBackProc procedure }
  Begin
  End;

  Function AcceptOneFile (P: ProtocolRecPtr): Boolean;
    {-Built-in function that accepts one file only}
  Begin
    With P^. PData^ Do
    Begin
      AcceptOneFile := Not GotOneFile;
      GotOneFile := True;
    End;
  End;

  Function NextFileMask (P: ProtocolRecPtr; Var FName: PathStr): Boolean;
    {-Built-in function that works with file mask fields}
  Const
    AnyFileButDir = AnyFile And Not (Directory Or VolumeID);

  Begin
    AsyncStatus := ecOk;

    With P^. PData^ Do
    Begin
      {Check for uninitialized search mask}
      If SearchMask = '' Then
      Begin
        GotError (PR, epFatal + ecNoSearchMask);
        AsyncStatus := ecNoSearchMask;
        NextFileMask := False;
        Exit;
      End;

      {Search for a matching file}
      If FindingFirst Then
      Begin
        FindFirst (SearchMask, AnyFileButDir, CurRec);

        If DosError = 18 Then
        Begin
          GotError (PR, epFatal + ecNoMatchingFiles);
          FName := '';
          NextFileMask := False;
        {$IFNDEF MSDOS}
          FindClose (CurRec);
        {$ENDIF}
          Exit;
        End
        Else
          FindingFirst := False;
      End
      Else
        FindNext (CurRec);

      {Check for errors}
      If DosError <> 0 Then
      Begin
        {Failed to find file, return error status}
        If DosError = 3 Then
          GotError (PR, epFatal + ecDirNotFound);
        FName := '';
        NextFileMask := False;
      {$IFNDEF MSDOS}
        FindClose (CurRec);
      {$ENDIF}
      End Else
      Begin
        {Found a file, return fully qualified file name}
        FName := AddBackSlash (JustPathName (SearchMask)) + CurRec. Name;
        NextFileMask := True;
      End;
    End;
  End;

  Procedure apPrepareReading (P: ProtocolRecPtr);
    {-Prepare to send protocol blocks (usually opens a file)}
  Var
    Result : Word;

  Begin
    With P^. PData^ Do
    Begin
      AsyncStatus := ecOk;

      {If file is already open then leave without doing anything}
      If FileOpen Then
        Exit;

      {Report notfound error for empty filename}
      If PathName = '' Then
      Begin
        GotError (PR, epFatal + ecFileNotFound);
        Exit;
      End;

      {Open up the previously specified file}
      SaveMode := FileMode;
      FileMode := AproFileMode;
      Assign (WorkFile, PathName);
      Reset (WorkFile, 1);
      FileMode := SaveMode;

      Result := IOResult;
      If Result <> 0 Then
      Begin
        GotError (PR, epFatal + Result);
        Exit;
      End;

      {Show file name and size}
      SrcFileLen := FileSize (WorkFile);
      BytesRemaining := SrcFileLen;
      apUserStatus (P, False, False);

      {Note file date/time stamp (for those protocols that care)}
      GetFTime (WorkFile, SrcFileDate);

      {Initialize the buffering variables}
      StartOfs := 0;
      BufLen := 0;
      BufValid := 0;
      RealFileOfs := 0;
      FileOpen := True;
    End;
  End;

  Procedure apFinishReading (P: ProtocolRecPtr);
    {-Clean up after reading protocol blocks (usually closes a file)}
  Begin
    With P^. PData^ Do
      If FileOpen Then
      Begin
        Close (WorkFile); {Error or end-of-protocol, clean up}
        If IOResult <> 0 Then;
        FileOpen := False;
      End;
  End;

  Function apReadProtocolBlock (P: ProtocolRecPtr; Var Block: DataBlockType;
                                Var BlockSize: Word): Boolean;
    {-Return with a block to transmit (True to quit)}
  Var
    i, j           : LongInt;
    Readed, Result : Word;

  Begin
    AsyncStatus := ecOk;

    With P^. PData^ Do
    Begin
      If FileOfs >= StartOfs Then
      Begin
        i := BufLen - (FileOfs - StartOfs);

        If i > 0 Then
        Begin
          If i > BlockSize Then
            i := BlockSize;
          Move (FileBuffer^ [FileOfs - StartOfs], Block, i);
        End
        Else
          i := 0;

        If i < BlockSize Then
        Begin
          j := FileOfs + i;
          If j <> RealFileOfs Then
            Seek (WorkFile, j);
          BlockRead (WorkFile, FileBuffer^, FileBufferSize, Readed);
          Result := IOResult;
          If Result <> 0 Then
          Begin
            GotError (PR, epFatal + Result);
            apReadProtocolBlock := True;
            BlockSize := 0;
            Exit;
          End;

          StartOfs := j;
          RealFileOfs := j + Readed;
          BufLen := FileBufferSize;
          BufValid := Readed;
          If Readed < FileBufferSize Then
            FillChar (FileBuffer^ [Readed], FileBufferSize - Readed,
              BlockFillChar);

          Move (FileBuffer^, Block [i + 1], BlockSize - i);
        End;
      End Else
      Begin
        Seek (WorkFile, FileOfs);
        BlockRead (WorkFile, FileBuffer^, FileBufferSize, Readed);
        Result := IOResult;
        If Result <> 0 Then
        Begin
          GotError (PR, epFatal + Result);
          apReadProtocolBlock := True;
          BlockSize := 0;
          Exit;
        End;

        StartOfs := FileOfs;
        RealFileOfs := FileOfs + Readed;
        BufLen := FileBufferSize;
        BufValid := Readed;
        If Readed < FileBufferSize Then
          FillChar (FileBuffer^ [Readed], FileBufferSize - Readed,
            BlockFillChar);

        Move (FileBuffer^, Block, BlockSize);
      End;

      If (BufValid < BufLen) And (FileOfs + BlockSize > StartOfs + BufValid) Then
      Begin
        BlockSize := StartOfs + BufValid - FileOfs;
        apReadProtocolBlock := True;
      End
      Else
        apReadProtocolBlock := False;
    End;
  End;

  Procedure apPrepareWriting (P: ProtocolRecPtr);
    {-Prepare to save protocol blocks (usually opens a file)}
  Var
    Result : Word;
    Dir    : DirStr;
    Name   : NameStr;
    Ext    : ExtStr;

  Label
    ExitPoint;

  Begin
    With P^. PData^ Do
    Begin
      {Does the file exist already?}
      SaveMode := FileMode;
      FileMode := AproFileMode;
      Assign (WorkFile, PathName);
      Reset (WorkFile, 1);
      FileMode := SaveMode;
      Result := IOResult;

      {Exit on errors other than FileNotFound}
      If (Result <> 0) And (Result <> 2) Then
      Begin
        GotError (PR, epFatal + Result);
        Goto ExitPoint;
      End;

      {Exit if file exists and option is WriteFail}
      If (Result = 0) And (WriteFailOpt = WriteFail) Then
      Begin
        GotError (PR, epNonFatal + ecFileAlreadyExists);
        Goto ExitPoint;
      End;

      Close (WorkFile);
      If IOResult = 0 Then;

      {Change the file name if it already exists the option is WriteRename}
      If (Result = 0) And (WriteFailOpt = WriteRename) Then
      Begin
        FSplit (Pathname, Dir, Name, Ext);
        Name [1] := '$';
        Pathname := Dir + Name + Ext;
        GotError (PR, epNonFatal + ecFileRenamed);
      End;

      {Give status a chance to show that the file was renamed}
      apUserStatus (P, False, False);

      {Ok to rewrite file now}
      Assign (WorkFile, Pathname);
      Rewrite (WorkFile, 1);
      Result := IOResult;
      If Result <> 0 Then
      Begin
        GotError (PR, epFatal + Result);
        Goto ExitPoint;
      End;

      {Initialized the buffer management vars}
      StartOfs := 0;
      BufLen := 0;
      RealFileOfs := 0;
      FileOpen := True;
      AsyncStatus := ecOk;
      Exit;

    ExitPoint:
      Close (WorkFile);
      If IOResult <> 0 Then;
    End;
  End;

  Procedure apFinishWriting (P: ProtocolRecPtr);
    {-Cleans up after saving all protocol blocks}
  Var
    Written, Result : Word;

  Begin
    With P^. PData^ Do
      If FileOpen Then
      Begin
        If RealFileOfs <> StartOfs Then
          Seek (WorkFile, StartOfs);
        BlockWrite (WorkFile, FileBuffer^, BufLen, Written);
        Result := IOResult;
        If (Result <> 0) Then
          GotError (PR, epFatal + Result)
        Else
          If (Written <> BufLen) Then
            GotError (PR, epFatal + ecDiskFull);
        Close (WorkFile);
        If IOResult <> 0 Then;
        FileOpen := False;
      End;
  End;

  Function apWriteProtocolBlock (P: ProtocolRecPtr; Var Block: DataBlockType;
                                 BlockSize: Word): Boolean;
    {-Write a protocol block (return True to quit)}
  Var
    Written, Result                 : Word;
    OffState, DTR, RTS, CurrentlyOn : Boolean;

  Begin
    AsyncStatus := ecOk;
    apWriteProtocolBlock := True;

    With P^. PData^ Do
    Begin
      If FileOfs < StartOfs Then
      Begin
        StartOfs := FileOfs;
        BufLen := 0;
      End
      Else
        If FileOfs < StartOfs + BufLen Then
          BufLen := FileOfs - StartOfs;

      If BufLen + BlockSize > FileBufferSize Then
      Begin
        If RealFileOfs <> StartOfs Then
          Seek (WorkFile, StartOfs);
      {$IFNDEF OS2}
        If FlagIsSet (apFlags, apRTSLowForWrite) Then
        Begin
          IntOff;
          GetModem (PR, DTR, RTS);
          If FlagIsSet (PR^. HWFRecHonor, hfUseRTS) And
             FlagIsSet (PR^. HWFRecMask, hfRTSActiveLow) Then
          Begin
            OffState := True;
            CurrentlyOn := Not RTS;
          End Else
          Begin
            OffState := False;
            CurrentlyOn := RTS;
          End;

          If CurrentlyOn Then
            SetRTS (PR, OffState);
          IntOn;

          BlockWrite (WorkFile, FileBuffer^, BufLen, Written);

          If CurrentlyOn Then
            SetRTS (PR, Not OffState);
        End
        Else
      {$ENDIF}
          BlockWrite (WorkFile, FileBuffer^, BufLen, Written);

        Result := IOResult;
        If (Result <> 0) Then
        Begin
          GotError (PR, epFatal + Result);
          Exit;
        End;
        If (Written <> BufLen) Then
        Begin
          GotError (PR, epFatal + ecDiskFull);
          Exit;
        End;

        Inc (StartOfs, BufLen);
        RealFileOfs := StartOfs;
        BufLen := 0;
      End;

      Move (Block, FileBuffer^ [FileOfs - StartOfs], BlockSize);
      Inc (BufLen, BlockSize);
    End;

    apWriteProtocolBlock := False;
  End;
  {$F-}

  Procedure apUserStatus (P: ProtocolRecPtr; Starting, Ending: Boolean);
    {-Calls user status routine while preserving AsyncStatus}
  Var
    SaveStatus : Word;

  Begin
    With P^. PData^ Do
    Begin
      SaveStatus := AsyncStatus;
      If (AsyncStatus = ecNoHeader) Or (AsyncStatus = ecGotHeader) Then
        AsyncStatus := ecOk;
      UserStatus (P, Starting, Ending);
      AsyncStatus := SaveStatus;
    End;
  End;

  Function TrimZeros (S: String): String;
    {-Return a string with leading and trailing white space removed}
  Var
    SLen : Byte Absolute S;
    i    : Integer;

  Begin
    While (SLen > 0) And (S [SLen] <= ' ') Do
      Dec (SLen);

    i := 1;
    While (i <= SLen) And ((S [i] <= ' ') Or (S [i] = '0')) Do
      Inc (i);

    Dec (i);
    If i > 0 Then
      Delete (S, 1, i);

    TrimZeros := S;
  End;

  Function OctalStr (L: LongInt): String;
    {-Convert L to octal base string}
  Var
    i : Integer;

  Begin
    OctalStr [0] := #12;

    For i := 0 To 11 Do
    Begin
      OctalStr [12 - i] := Chr ((L And 7) + Ord ('0'));
      L := L Shr 3;
    End;
  End;

  Function OctalStr2Long (S: String): LongInt;
    {-Convert S from an octal string to a longint}
  Var
    j, Result : LongInt;
    i, Step   : Integer;

  Begin
    S := TrimZeros (S);
    Result := 0;
    Step := 0;

    For i := Length (S) DownTo 1 Do
    Begin
      j := Ord (S [i]) - Ord ('0');

      If (j < 0) Or (j > 7) Then
      Begin
        OctalStr2Long := 0;
        Exit;
      End;

      Inc (Result, j Shl Step);
      Inc (Step, 3);
    End;

    OctalStr2Long := Result;
  End;

  Function PackToYMTimeStamp (RawTime: LongInt): LongInt;
    {-Return date/time stamp as octal seconds since 1/1/1970 00:00 GMT}
  Var
    DT       : DateTime;
    DTR      : DateTimeRec;
    DiffDays : Word;
    DiffSecs : LongInt;

  Begin
    {Convert to julian date}
    UnpackTime (RawTime, DT);
    With DT Do
    Begin
      DTR. D := DMYtoDate (Day, Month, Year);
      DTR. T := HMStoTime (Hour, Min, Sec);
    End;

    {Subtract GMT hour offset}
    IncDateTime (DTR, DTR, 0, - (3600 * GmtHourOffset));

    {Diff between date/time stamp and 1/1/1970 (in seconds)}
    DateTimeDiff (DTR, StartDate, DiffDays, DiffSecs);
    PackToYMTimeStamp := DiffSecs + (DiffDays * SecondsInDay);
  End;

  Function YMTimeStampToPack (YMTime: LongInt): LongInt;
    {-Return a file time stamp in packed format from a Ymodem time stamp}
  Var
    DT      : DateTime;
    DTR     : DateTimeRec;
    Ptime   : LongInt;
    H, M, S : Byte;

  Begin
    {Add the time stamp to StartDate}
    IncDateTime (StartDate, DTR, 0, YMTime);

    {Add the GMT hour offset}
    IncDateTime (DTR, DTR, 0, 3600 * GmtHourOffset);

    {Convert to DT format}
    With DT Do
    Begin
      DateToDMY (DTR. D, Integer (Day), Integer (Month), Integer (Year));
      TimeToHMS (DTR. T, H, M, S);
      Hour := H;
      Min := M;
      Sec := S;
    End;

    {Convert to packed format}
    PackTime (DT, Ptime);
    YMTimeStampToPack := Ptime;
  End;

  Function CurrentTimeStamp: LongInt;
    {-Return a Ymodem format file time stamp of the current date/time}
  Begin
    CurrentTimeStamp := PackToYMTimeStamp (GetDosDate);
  End;

  Function ValidatePathname (Const Pathname: String): String;
  Var
    DotPos         : Integer;
    Dir, Name, Ext : String;

  Begin
    Dir := AddBackSlash (JustPathname (Pathname));
    Name := DelChars ([#0..#32], JustFilename (Pathname));

    If HasExtension (Name, DotPos) Then
    Begin
      Ext := Copy (Name, DotPos, 255);
      If StrContains (Ext, [#127..#255]) Then
        Ext := '.' + Copy (HexL (Crc32Str (Ext)), 6, 3);
    End
    Else
      Ext := '';

    Name := JustName (Name);
    If StrContains (Name, [#127..#255]) Then
      Name := HexL (Crc32Str (Name));

    ValidatePathname := Dir + Copy (Name, 1, 8) + Copy (Ext, 1, 4);
  End;

End.
