{$IFDEF MSDOS}
  {$IFNDEF DPMI32}
    {$F+}
  {$IFNDEF DPMI}
    {$O+}
  {$ENDIF}
  {$ENDIF}
{$ENDIF}
{$I-,X+}

{Conditional defines that may affect this unit}
{$I APDEFINE.INC}

{*********************************************************}
{*                  APZMODEM.PAS 2.03                    *}
{*     Copyright (c) TurboPower Software 1991.           *}
{*                 All rights reserved.                  *}
{*********************************************************}

Unit ApZmodem;
  {-Provides Zmodem receive and transmit functions}

Interface

Uses
{$IFNDEF OS2}
  ApMisc,
  ApPort,
{$ELSE}
  ApOS2,
{$ENDIF}
  DOS,
  ApCom,
  ApTimer,
  ApSame,
  tMisc,
  OpInline,
  Crc,
  ApAbsPcl;

Procedure InitZmodem (Var P: ProtocolRecPtr; PortPtr: PortRecPtr);
Procedure InitCustomZmodem (Var P: ProtocolRecPtr; PortPtr: PortRecPtr;
                            Options: Word);
Procedure DoneZmodem (Var P: ProtocolRecPtr);
Procedure SetFileMgmtOptions (P: ProtocolRecPtr; Override, SkipNoFile: Boolean;
                              FOpt: Byte);
Procedure SetRecoverOption (P: ProtocolRecPtr; OnOff: Boolean);
Procedure SetBigSubpacketOption (P: ProtocolRecPtr; UseBig: Boolean);
Procedure SetFinishWaitZM (P: ProtocolRecPtr; NewWait: Word; NewRetry: Byte);
Procedure ProtocolTransmitZM (P: ProtocolRecPtr);
Procedure ProtocolReceiveZM (P: ProtocolRecPtr);
Procedure Cancel (P: ProtocolRecPtr);
Procedure AbortProtocol (P: ProtocolRecPtr);
Procedure zpPrepareWriting (P: ProtocolRecPtr);
Procedure zpFinishWriting (P: ProtocolRecPtr);

Implementation

Const
  {Compile-time constants}
  MaxAttentionLen = 32;              {Maximum length of attention string}
  MaxBlockSize    = 8192;            {Maximum data bytes on one zmodem frame}
  WorkBlockTail   = 32;
  HandshakeWaitZM = 1092;            {Tics to wait for first hdr (60 secs)}

Type
  CharArr = Array [0..65520] Of Char;

  {Main Zmodem state table}
  ZmodemStateType = (
    {Transmit states}
    tzHandshake,     {Wait for hdr (zrinit), rsend zrqinit on timout}
    tzGetFile,       {Call NextFile, build ZFile packet}
    tzCheckFile,     {Wait for hdr (zrpos), set next state to tzData}
    tzStartData,     {Send ZData and next data subpacket}
    tzEscapeData,    {Check for header, escape next block}
    tzSendData,      {Wait for free space in buffer, send escaped block}
    tzWaitAck,       {Wait for Ack on ZCRCW packets}
    tzSendEof,       {Send eof}
    tzDrainEof,      {Wait for output buffer to drain}
    tzCheckEof,      {Wait for hdr (zrinit)}
    tzCheckFinish,   {Wait for hdr (zfin)}
    tzError,         {Cleanup after errors}
    tzCleanup,       {Release buffers and other cleanup}
    tzDone,          {Signal end of protocol}

    {Receive states}
    rzRqstFile,      {Send zrinit}
    rzDelay,         {Delay handshake for Telix}
    rzWaitFile,      {Waits for hdr (zrqinit, zrfile, zsinit, etc)}
    rzCollectFile,   {Collect file info into work block}
    rzSendBlockPrep, {Collect post-hexhdr chars}
    rzSendBlock,     {Collect sendinit block}
    rzSync,          {Send ZrPos with current file position}
    rzStartData,     {Wait for hdr (zrdata)}
    rzCollectData,   {Collect data subpacket}
    rzWaitEof,       {Wait for hdr (zreof)}
    rzSendFinish,    {Send ZFin, goto rzWaitOO}
    rzCollectFinish, {Check for OO, goto rzFinish}
    rzError,         {Handle errors while file was open}
    rzWaitCancel,    {Wait for the cancel to leave the outbuffer}
    rzCleanup,       {Clean up buffers, etc.}
    rzDone);         {Signal end of protocol}

  {General header collection states}
  HeaderStateType = (
    hsNone,          {Not currently checking for a header}
    hsGotZPad,       {Got initial or second asterisk}
    hsGotZDle,       {Got ZDle}
    hsGotZBin,       {Got start of binary header}
    hsGotZBin32,     {Got start of binary 32 header}
    hsGotZHex);      {Got start of hex header}

  {Hex header collection states}
  HexHeaderStates = (
    hhFrame,         {Processing frame type char}
    hhPos1,          {Processing 1st position info byte}
    hhPos2,          {Processing 2nd position info byte}
    hhPos3,          {Processing 3rd position info byte}
    hhPos4,          {Processing 4th position info byte}
    hhCrc1,          {Processing 1st CRC byte}
    hhCrc2);         {Processing 2nd CRC byte}

  {Binary header collection states}
  BinaryHeaderStates = (
    bhFrame,         {Processing frame type char}
    bhPos1,          {Processing 1st position info byte}
    bhPos2,          {Processing 2nd position info byte}
    bhPos3,          {Processing 3rd position info byte}
    bhPos4,          {Processing 4th position info byte}
    bhCrc1,          {Processing 1st CRC byte}
    bhCrc2,          {Processing 2nd CRC byte}
    bhCrc3,          {Processing 3rd CRC byte}
    bhCrc4);         {Processing 4th CRC byte}

  {Only two states possible when receiving blocks}
  ReceiveBlockStates = (
    rbData,          {Receiving data bytes}
    rbCrc);          {Receiving block check bytes}

  {Describes working buffer for expanding a standard buffer with escapes}
  WorkBlockType = Array [0..2*MaxBlockSize + WorkBlockTail - 1] Of Char;

  {Describes data area of headers}
  PosFlagsType = Array [0..3] Of Byte;

  {Pointer to a protocol record}
  ZmodemPtr = ^ZmodemProtocol;
  ZmodemProtocol = Record
    {General...}
    PData            : ProtocolDataPtr; {General protocol data}
    UseCrc32         : Boolean;         {True when using 32bit CRCs}
    CanCrc32         : Boolean;         {True when Crc32 capable}
    LastFileOfs      : LongInt;         {File position reported by remote}
    AttentionStr     : Array [1..MaxAttentionLen] Of Byte; {Attn string value}
    ConvertOpts      : Byte;            {File conversion opts rqst by sender}
    FileMgmtOpts     : Byte;            {File mgmt opts rqst by sender}
    TransportOpts    : Byte;            {File transport opts rqst by sender}
    FileMgmtOverride : Boolean;         {True to override senders file mg opts}
    ReceiverRecover  : Boolean;         {True to force file recovery}
    FinishWait       : Word;            {Wait time for ZFin response}
    FinishRetry      : Byte;            {Times to resend ZFin}
    LastFrame        : Char;            {Holds last frame type for status}
    EscapeAll        : Boolean;         {True when escaping all ctl chrs}
    ZmodemState      : ZmodemStateType; {Current Zmodem state}
    HeaderState      : HeaderStateType; {Current Header state}
    ReplyTimer       : EventTimer;      {Used to timeout replies}
    WorkSize         : Word;            {Index into working buffer}
    LastBlock        : Boolean;         {True if no more blocks}
    Terminator       : Char;            {Current block type}
    HexPending       : Boolean;         {True for next char in hex pair}
    HexByte          : Byte;            {Used to build hex byte}
    EscapePending    : Boolean;         {True for next char in esc pair}
    ControlCharSkip  : Boolean;         {True to skip ctl chars}
    HeaderType       : Char;            {Current header type}
    HexHdrState      : HexHeaderStates; {Current hex header state}
    BinHdrState      : BinaryHeaderStates; {Current binary header state}
    RcvBlockState    : ReceiveBlockStates; {Current receive block state}
    FilesSent        : Boolean;         {True if at least one file sent}
    CanCount         : Byte;            {Track contiguous <cancels>}
    SaveStatus       : Word;            {Maintain status across parts}
    CrcCnt           : Byte;            {Number of CRC bytes expected}
    LastStatus       : Word;            {Status to set in ReceiveBlock}
    OCnt             : Byte;            {'O' byte received from 'OO'}
    TimerPending     : Boolean;         {True if waiting to start timer}
    DataInTransit    : Word;            {Count of bytes in window}
    TimerStarted     : Boolean;         {True if timer started}
    WasHex           : Boolean;         {True if last header was hex}
    DiscardCnt       : Word;            {Count chars before sendblock}

    {For controlling autoadjustment of block size}
    Use8KBlocks      : Boolean;         {True when using 8K blocks}
    TookHit          : Boolean;         {True if we got ZrPos packet}
    GoodAfterBad     : Word;            {Holds count of good blocks}

    {Working buffers}
    DataBlock        : ^DataBlockType;  {Standard data block}
    DataBlockLen     : Word;            {Count of valid data in DataBlock}
    WorkBlock        : ^WorkBlockType;  {Holds fully escaped data block}

    {Receiving...}
    RcvHeader        : PosFlagsType;    {Received header}
    RcvFrame         : Char;            {Type of last received frame}

    {Transmitting...}
    TransHeader      : PosFlagsType;    {Header to transmit}
    RcvBuffLen       : Word;            {Size of receiver's buffer}
    LastChar         : Char;
    RemoteCanOvIO    : Boolean;
  End;

Const
  {For manipulating file management masks}
  FileMgmtMask = 7;                {Isolate file mgmnt values}
  FileSkipMask = $80;              {Skip file if dest doesn't exist}

  {Only supported conversion option}
  FileRecover = $03;               {Resume interrupted file transfer}

  FreeMargin  = 30;

  {Data subpacket terminators}
  ZCrcE      = 'h';                {End  - last data subpacket of file}
  ZCrcG      = 'i';                {Go   - no response necessary}
  ZCrcQ      = 'j';                {Ack  - requests ZACK or ZRPOS}
  ZCrcW      = 'k';                {Wait - sender waits for answer}

  {Translate these escaped sequences}
  ZRub0      = 'l';                {Translate to $7F}
  ZRub1      = 'm';                {Translate to $FF}

  {Byte offsets for pos/flag bytes}
  ZF0 = 3;                         {Flag byte 0}
  ZF1 = 2;                         {Flag byte 1}
  ZF2 = 1;                         {Flag byte 2}
  ZF3 = 0;                         {Flag byte 3}
  ZP0 = 0;                         {Position byte 0}
  ZP1 = 1;                         {Position byte 1}
  ZP2 = 2;                         {Position byte 2}
  ZP3 = 3;                         {Position byte 3}

  {Bit masks for ZrInit}
  CanFdx  = $0001;                 {Can handle full-duplex}
  CanOvIO = $0002;                 {Can do disk and serial I/O overlaps}
  CanBrk  = $0004;                 {Can send a break}
  CanCry  = $0008;                 {Can encrypt/decrypt, not supported}
  CanLzw  = $0010;                 {Can LZ compress, not supported}
  CanFc32 = $0020;                 {Can use 32 bit CRC}
  EscAll  = $0040;                 {Escapes all control chars}
  Esc8    = $0080;                 {Escapes the 8th bit, not supported}

  {Bit masks for ZsInit}
  TEscAll  = $0040;                {Sender needs escaped ctl chars}
  TEsc8    = $0080;                {Sender needs escaped 8th bit, not supported}

  {Character constants}
  Hibit   = $80;
  cDleHi  = Char (Ord (cDle) + HiBit);
  cXonHi  = Char (Ord (cXon) + HiBit);
  cXoffHi = Char (Ord (cXoff) + HiBit);
  CanChar = #24;

  {Run-time constants}
  DrainingStatusInterval = 18;     {Default status interval for draining eof}
  DefFinishWaitZM        = 364;    {Wait time for ZFins, 30 secs}
  DefFinishRetryZM       = 3;      {Retry ZFin 3 times}
  MaxBadBlocks           = 20;     {Quit if this many bad blocks}

  {For estimating protocol transfer times}
  ZmodemTurnDelay = 0;             {Millisecond turnaround delay}
  ZmodemOverHead  = 20;            {Default overhead for each data subpacket}

  {Zmodem constants}
  ZPad       = '*';                {Pad}
  ZDle       = #24;                {Data link escape}
  ZDleE      = 'X';                {An escaped data link escape character}
  ZBin       = 'A';                {Binary header using Crc16}
  ZHex       = 'B';                {Hex header using Crc16}
  ZBin32     = 'C';                {Binary header using Crc32}

  {Zmodem frame types}
  ZrQinit    = #0;                 {Request init (to receiver)}
  ZrInit     = #1;                 {Init (to sender)}
  ZsInit     = #2;                 {Init (to receiver) (optional)}
  ZAck       = #3;                 {Acknowledge last frame}
  ZFile      = #4;                 {File info frame (to receiver)}
  ZSkip      = #5;                 {Skip to next file (to receiver)}
  ZNak       = #6;                 {Error receiving last data subpacket}
  ZAbort     = #7;                 {Abort protocol}
  ZFin       = #8;                 {Finished protocol}
  ZRpos      = #9;                 {Resume from this file position}
  ZData      = #10;                {Data subpacket(s) follows}
  ZEof       = #11;                {End of current file}
  ZFerr      = #12;                {Error reading or writing file}
  ZCrc       = #13;                {Request for file CRC (to receiver)}
  ZChallenge = #14;                {Challenge the sender}
  ZCompl     = #15;                {Complete}
  ZCan       = #16;                {Cancel requested (to either)}
  ZFreeCnt   = #17;                {Request diskfree}
  ZCommand   = #18;                {Execute this command (to receiver)}

  {File management options (how and when to accept a file)}
  WriteNewerLonger = 1;            {Transfer if new, newer or longer}
  WriteCrc         = 2;            {Not supported, same as WriteNewer}
  WriteAppend      = 3;            {Transfer if new, append if exists}
  WriteClobber     = 4;            {Transfer regardless}
  WriteNewer       = 5;            {Transfer if new or newer}
  WriteDifferent   = 6;            {Transfer if new or diff dates/lens}
  WriteProtect     = 7;            {Transfer only if new}

  EscapingChars : Set Of Char =
    [ZDle, cDle, cDleHi, cXon, cXonHi, cXoff, cXoffHi];

  {For checking max block sizes}
  ZMaxBlk : Array [Boolean] Of Word = (1024, 8192);

  StartBCheck   : Array [Boolean] Of LongInt = (0, -1);
  BlockCheckLen : Array [Boolean] Of Byte = (2, 4);

{$IFDEF VirtualPascal}
  {$L ZModem32.Obj}
  Function AsmEscapeBlock (Var InBuf; InBufSize: Word; Var OutBuf;
                           Escaping: CharSet; Var LastChar: Char;
                           EscapeAll: Boolean): Word; External;
{$ENDIF}

Procedure InitZmodem (Var P: ProtocolRecPtr; PortPtr: PortRecPtr);
  {-Allocates and initializes a protocol control block}
Begin
  InitCustomZmodem (P, PortPtr, DefProtocolOptions);
End;

Procedure InitCustomZmodem (Var P: ProtocolRecPtr; PortPtr: PortRecPtr;
                            Options: Word);
  {-Allocates and initializes a protocol control block with options}
Var
  ZM   : ZmodemPtr Absolute P;
  Temp : ProtocolDataPtr;

Begin
  AsyncStatus := ecOk;

  {Allocate the protocol data record}
  InitProtocolData (Temp, PortPtr, Options);
  If AsyncStatus <> ecOk Then
    Exit;

  {Allocate the Zmodem record}
  If Not GetMemCheck (ZM, SizeOf (ZModemProtocol)) Then
  Begin
    GotError (PortPtr, epFatal+ecOutOfMemory);
    DoneProtocolData (Temp);
    Exit;
  End;

  ZM^. PData := Temp;

  With ZM^, PData^ Do
  Begin
    {Allocate data blocks}
    Use8KBlocks := FlagIsSet (Options, apZmodem8K);

    If PR^. Buffered And (PR^. OutBuffLen < (ZMaxBlk [Use8KBlocks] * 2) +
       FreeMargin) Then
    Begin
      GotError (PortPtr, epFatal+ecBuffersTooSmall);
      DoneProtocolData (Temp);
      FreeMemCheck (ZM, SizeOf (ZmodemProtocol));
      Exit;
    End;

    If Not (GetMemCheck (DataBlock, ZMaxBlk [Use8KBlocks]) And
            GetMemCheck (WorkBlock, ZMaxBlk [Use8KBlocks] * 2 + WorkBlockTail))
    Then
    Begin
      GotError (PortPtr, epFatal+ecOutOfMemory);
      DoneZmodem (P);
      Exit;
    End;

    {Init this object's fields}
    ProtType := Zmodem;
    BatchProtocol := True;
    FileOfs := 0;
    LastFileOfs := 0;
    CheckType := bcCrc32;
    UseCrc32 := True;
    CanCrc32 := True;
    SrcFileDate := 0;
    ReceiverRecover := False;
    BlockLen := ZMaxBlk [Use8KBlocks];
    FillChar (AttentionStr, SizeOf (AttentionStr), 0);
    FileMgmtOpts := WriteNewer;
    FileMgmtOverride := False;
    FileOpen := False;
    Overhead := ZmodemOverhead;
    TurnDelay := ZmodemTurnDelay;
    HandshakeWait := HandshakeWaitZM;
    PrepareWriting := zpPrepareWriting;
    FinishWriting := zpFinishWriting;
    TookHit := False;
    GoodAfterBad := 0;
    EscapePending := False;
    HexPending := False;
    FinishWait := DefFinishWaitZM;
    FinishRetry := DefFinishRetryZM;
    EscapeAll := False;
  End;
End;

Procedure DoneZmodem (Var P: ProtocolRecPtr);
  {-Dispose of Zmodem}
Var
  ZM     : ZmodemPtr Absolute P;
  BigBlk : Boolean;

Begin
  With ZM^ Do
  Begin
    BigBlk := FlagIsSet (PData^. apFlags, apZmodem8K);
    FreeMemCheck (DataBlock, ZMaxBlk [BigBlk]);
    FreeMemCheck (WorkBlock, ZMaxBlk [BigBlk] * 2 + WorkBlockTail);
    DoneProtocolData (PData);
  End;
  FreeMemCheck (ZM, SizeOf (ZmodemProtocol));
End;

Procedure SetFileMgmtOptions (P: ProtocolRecPtr; Override, SkipNoFile: Boolean;
                              FOpt: Byte);
  {-Set file mgmt options to use when sender doesn't specify}
Begin
  With ZmodemPtr (P)^ Do
  Begin
    FileMgmtOverride := Override;
    FileMgmtOpts := FOpt And FileMgmtMask;
    If SkipNoFile Then
      FileMgmtOpts := FileMgmtOpts Or $80;
  End;
End;

Procedure SetRecoverOption (P: ProtocolRecPtr; OnOff: Boolean);
  {-Turn file recovery on (will be ignored If dest file doesn't exist)}
Begin
  ZmodemPtr (P)^. ReceiverRecover := OnOff;
End;

Procedure SetBigSubpacketOption (P: ProtocolRecPtr; UseBig: Boolean);
  {-Turn on/off 8K subpacket support}
Begin
  With ZmodemPtr (P)^, PData^ Do
    If FlagIsSet (apFlags, apZmodem8K) Then
    Begin
      Use8KBlocks := UseBig;
      BlockLen := ZMaxBlk [UseBig];
    End;
End;

Procedure SetFinishWaitZM (P: ProtocolRecPtr; NewWait: Word; NewRetry: Byte);
  {-Set new finish wait and retry values}
Begin
  With ZmodemPtr (P)^ Do
  Begin
    If NewWait <> 0 Then
      FinishWait := NewWait;

    FinishRetry := NewRetry;
  End;
End;

{$IFNDEF VirtualPascal}
Procedure EscapeChars (P: ProtocolRecPtr; Var InBuf; Const Len: Word;
                      Var OutBuf; Var OutBufPtr: Word);
Var
  IBuf : CharArr Absolute InBuf;
  OBuf : CharArr Absolute OutBuf;
  i    : Word;
  C    : Char;

Begin
  With ZmodemPtr (P)^ Do
    For i := 0 To Len-1 Do
    Begin
      C := IBuf [i];
      If (C in EscapingChars) Or
         ((Char (Byte (C) And $7F) = cCR) And (Byte (LastChar) And $7F = $40))
         Or (EscapeAll And (Byte (C) And $60 = 0)) Then
      Begin
        OBuf [OutBufPtr] := ZDle;
        Inc (OutBufPtr);
        C := Char (Byte (C) Xor $40);
      End;

      OBuf [OutBufPtr] := C;
      Inc (OutBufPtr);
      LastChar := C;
    End;
End;
{$ENDIF}

Procedure BufBinaryHeader (P: ProtocolRecPtr; FrameType: Char; Var Buf;
                           Var BufPtr: Word);
Type
  TempRec = Packed Record
    FType   : Char;
    THeader : LongInt;
    BCheck  : LongInt;
  End;

Const
  TRecHeadSize = 5;

  BinaryChar : Array [Boolean] Of Char = (ZBin, ZBin32);

Var
  B    : CharArr Absolute Buf;
  TRec : TempRec;

Begin
  With ZModemPtr (P)^ Do
  Begin
    LastFrame := FrameType;
    UseCrc32 := CanCrc32;

    B [BufPtr] := ZPad;
    B [BufPtr + 1] := ZDle;
    B [BufPtr + 2] := BinaryChar [UseCrc32];
    Inc (BufPtr, 3);

    TRec. FType := FrameType;
    TRec. THeader := LongInt (TransHeader);
    If UseCrc32 Then
      TRec. BCheck := Not UpdateCrc32Buf (TRec, TRecHeadSize, -1)
    Else
      TRec. BCheck := UpdateCrc (0, UpdateCrc (0, UpdateCrcBuf (TRec,
        TRecHeadSize, 0)));

  {$IFDEF VirtualPascal}
    Inc (BufPtr, AsmEscapeBlock (TRec, TRecHeadSize + BlockCheckLen [UseCrc32],
      B [BufPtr], EscapingChars, LastChar, EscapeAll));
  {$ELSE}
    EscapeChars (P, TRec, TRecHeadSize + BlockCheckLen [UseCrc32], B, BufPtr);
  {$ENDIF}
  End;
End;

Procedure PutBinaryHeader (P: ProtocolRecPtr; FrameType: Char);
  {-Sends a binary header (Crc16 or Crc32)}
Var
  i   : Word;
  Buf : Array [0..20] Of Char;

Begin
  i := 0;
  BufBinaryHeader (P, FrameType, Buf, i);
  i := PutBlock (ZModemPtr (P)^. PData^. PR, Buf, i);
End;

Procedure PutHexHeader (P: ProtocolRecPtr; FrameType: Char);
  {-Sends a hex header}
Type
  TempRec = Packed Record
    FType   : Char;
    THeader : LongInt;
    Cargo   : System. Word;
  End;

Const
  HexDigits : Array [0..15] Of Char = '0123456789abcdef';

Var
  i    : Word;
  TRec : TempRec;
  Buf  : Array [0..20] Of Char;

Begin
  With ZmodemPtr (P)^ Do
  Begin
    LastFrame := FrameType;

    TRec. FType := FrameType;
    TRec. THeader := LongInt (TransHeader);
    TRec. Cargo := 0;
    i := UpdateCrcBuf (TRec, SizeOf (TRec), 0);

    Buf [0] := ZPad;
    Buf [1] := ZPad;
    Buf [2] := ZDle;
    Buf [3] := ZHex;
    Buf [4] := HexDigits [Byte (FrameType) Shr 4];
    Buf [5] := HexDigits [Byte (FrameType) And $0F];
    Buf [6] := HexDigits [TransHeader [0] Shr 4];
    Buf [7] := HexDigits [TransHeader [0] And $0F];
    Buf [8] := HexDigits [TransHeader [1] Shr 4];
    Buf [9] := HexDigits [TransHeader [1] And $0F];
    Buf [10] := HexDigits [TransHeader [2] Shr 4];
    Buf [11] := HexDigits [TransHeader [2] And $0F];
    Buf [12] := HexDigits [TransHeader [3] Shr 4];
    Buf [13] := HexDigits [TransHeader [3] And $0F];
    Buf [14] := HexDigits [Hi (i) Shr 4];
    Buf [15] := HexDigits [Hi (i) And $0F];
    Buf [16] := HexDigits [Lo (i) Shr 4];
    Buf [17] := HexDigits [Lo (i) And $0F];
    Buf [18] := cCR;
    Buf [19] := cLF;

    i := 19;
    If (FrameType <> ZAck) And (FrameType <> ZFin) Then
    Begin
      Buf [20] := cXon;
      Inc (i);
    End;

    i := PutBlock (PData^. PR, Buf, i);
  End;
End;

Function VerifyBlockCheck (P: ProtocolRecPtr): Boolean;
  {-checks the block check value}
Begin
  With ZmodemPtr (P)^, PData^ Do
    If UseCrc32 Then
      VerifyBlockCheck := BlockCheck = $DEBB20E3
    Else
    Begin
      BlockCheck := UpdateCrc (0, UpdateCrc (0, BlockCheck));
      VerifyBlockCheck := BlockCheck = 0;
    End;
End;

Procedure Cancel (P: ProtocolRecPtr);
  {-Sends the cancel string}
Const
  {Cancel string is 8 CANs followed by 8 Backspaces}
  CancelStr = #24#24#24#24#24#24#24#24#8#8#8#8#8#8#8#8#8#8;

{$IFNDEF OS2}
Var
  TotalOverhead, OutBuff : Word;
{$ENDIF}

Begin
  With ZmodemPtr (P)^, PData^ Do
  Begin
    {Flush anything that might be left in the output buffer}
  {$IFNDEF OS2}
    OutBuff := OutBuffUsed (PR);
    If (OutBuff > BlockLen) And (BlockLen > 0) Then
    Begin
      TotalOverhead := Overhead * (OutBuff Div BlockLen);
      Dec (BytesTransferred, OutBuff - TotalOverhead);
    End;
    FlushOutBuffer (PR);
  {$ENDIF}

    {Send the cancel string}
    PutString (PR, CancelStr);
    DrainOutBuffer (PR, 18);

    AsyncStatus := ecCancelRequested;
  End;
End;

Procedure AbortProtocol (P: ProtocolRecPtr);
  {-Aborts the protocol}
Begin
  Cancel (P);
  GotError (ZmodemPtr (P)^. PData^. PR, ecCancelRequested);
End;

Procedure PutAttentionString (P: ProtocolRecPtr);
  {-Puts a string (#221 = Break, #222 = Delay)}
Var
  i  : Word;
  ET : EventTimer;

Begin
  With ZmodemPtr (P)^, PData^ Do
    For i := 1 To MaxAttentionLen Do
      Case AttentionStr [i] Of
          0 : Break;
        $DD : SendBreak (PR); {Remote wants Break as his attention signal}
        $DE : Begin {Remote wants us to pause for one second}
                NewTimer (ET, 18);
                Repeat
                Until TimerExpired (ET);
              End;
      Else {Remote wants us to send a normal char}
        PutChar (PR, Char (AttentionStr [i]));
        If AsyncStatus <> ecOk Then
          Break;
      End;
End;

Procedure GetCharEscaped (P: ProtocolRecPtr; Var C: Char);
  {-Get a character (handle data link escaping)}
Label
  GetFirstChar,
  GetSecondChar;

Begin
  With ZmodemPtr (P)^, PData^ Do
  Begin
    ControlCharSkip := False;

    If Not EscapePending Then
    Begin

    GetFirstChar:
      GetChar (PR, C);

      Case C Of
        ZDle    : Begin
                    Inc (CanCount);
                    If CanCount >= 5 Then
                      GotError (PR, ecCancelRequested)
                    Else
                    Begin
                      If CharReady (PR) Then
                        Goto GetSecondChar;

                      EscapePending := True;
                    End;
                  End;
        cXon,
        cXoff,
        cXonHi,
        cXoffHi : Begin
                    If CharReady (PR) Then
                      Goto GetFirstChar;

                    ControlCharSkip := True;
                  End;
      Else
        CanCount := 0;
      End;

      Exit;
    End;

    EscapePending := False;

  GetSecondChar:
    GetChar (PR, C);

    Case C Of
      CanChar : Begin
                  Inc (CanCount);
                  If CanCount >= 5 Then
                    GotError (PR, ecCancelRequested);

                  Exit;
                End;
        ZCrcE : AsyncStatus := ecGotCrcE;
        ZCrcG : AsyncStatus := ecGotCrcG;
        ZCrcQ : AsyncStatus := ecGotCrcQ;
        ZCrcW : AsyncStatus := ecGotCrcW;
        ZRub0 : C := Chr ($7F);
        ZRub1 : C := Chr ($FF);
    Else
      C := Char (Byte (C) Xor $40);
    End;

    CanCount := 0;
  End;
End;

Procedure CheckForHeader (P: ProtocolRecPtr);
  {-Samples input stream for start of header}

  Procedure GotHeader;
  Begin
    With ZmodemPtr (P)^ Do
    Begin
      AsyncStatus := ecGotHeader;

      If LastFrame in [ZrPos, ZAck, ZData, ZEof] Then
        {Header contained a reported file position}
        LastFileOfs := LongInt (RcvHeader);

      {Note frame type for status}
      LastFrame := RcvFrame;
    End;
  End;

  Procedure CollectHexHeader;
    {-Gets the data and trailing portions of a hex header}
  Var
    C : Char;
    B : Byte;

  Begin
    With ZmodemPtr (P)^, PData^ Do
    Begin
      UseCrc32 := False;

      Repeat
        {Get the waiting character}
        If Not HexPending Then
        Begin
          GetChar (PR, C);

          {Handle cancels}
          If C = cCan Then
          Begin
            Inc (CanCount);
            If CanCount >= 5 Then
            Begin
              GotError (PR, ecCancelRequested);
              Exit;
            End;
          End
          Else
            CanCount := 0;

          B := 0;

          If C <= '9' Then
          Begin
            If C > '0' Then
              B := Ord (C) - Ord ('0');
          End
          Else
            If C <= 'f' Then
              If C >= 'a' Then
                B := Ord (C) - 87;

          HexByte := B Shl 4;

          If Not CharReady (PR) Then
          Begin
            HexPending := True;
            Break;
          End;
        End
        Else
          HexPending := False;

        GetChar (PR, C);

        {Handle cancels}
        If C = cCan Then
        Begin
          Inc (CanCount);
          If CanCount >= 5 Then
          Begin
            GotError (PR, ecCancelRequested);
            Exit;
          End;
        End
        Else
          CanCount := 0;

        B := HexByte;

        If C <= '9' Then
        Begin
          If C > '0' Then
            Inc (B, Ord (C) - Ord ('0'));
        End
        Else
          If C <= 'f' Then
            If C >= 'a' Then
              Inc (B, Ord (C) - 87);

        {Always update the block check}
        BlockCheck := UpdateCrc (B, BlockCheck);

        {Process this character}
        Case HexHdrState Of
          hhFrame :
            RcvFrame := Char (B);
          hhPos1..hhPos4 :
            RcvHeader [Ord (HexHdrState) - Ord (hhPos1)] := B;
          hhCrc2 :
            Begin
              If VerifyBlockCheck (P) Then
                {Say we got a good header}
                GotHeader
              Else
              Begin
                GotError (PR, ecBlockCheckError);
                Inc (TotalErrors);
              End;

              HeaderState := hsNone;
              Exit;
            End;
        End;

        {Goto next state}
        Inc (HexHdrState);
      Until Not CharReady (PR);

      AsyncStatus := ecNoHeader;
    End;
  End;

  Procedure CollectBinaryHeader (Crc32: Boolean);
    {-Collects a binary header, returns True when ready}
  Var
    C : Char;

  Begin
    With ZmodemPtr (P)^, PData^ Do
    Begin
      UseCrc32 := Crc32;

      Repeat
        {Get the waiting character}
        GetCharEscaped (P, C);
        If EscapePending Or ControlCharSkip Then
          Break;

        If AsyncStatus = ecCancelRequested Then
          Exit;

        {Always update the block check}
        If UseCrc32 Then BlockCheck := UpdateCrc32 (Ord (C), BlockCheck)
                    Else BlockCheck := UpdateCrc (Ord (C), BlockCheck);

        {Process this character}
        Case BinHdrState Of
          bhFrame :
            RcvFrame := C;
          bhPos1..bhPos4 :
            RcvHeader [Ord (BinHdrState) - Ord (bhPos1)] := Ord (C);
          bhCrc2, bhCrc4 :
            If Not ((BinHdrState = bhCrc2) And UseCrc32) Then
            Begin
              If VerifyBlockCheck (P) Then
                {Say we got a good header}
                GotHeader
              Else
              Begin
                GotError (PR, ecBlockCheckError);
                Inc (TotalErrors);
              End;

              HeaderState := hsNone;
              Exit;
            End;
        End;

        {Go to next state}
        Inc (BinHdrState);
      Until Not CharReady (PR);

      AsyncStatus := ecNoHeader;
    End;
  End;

Var
  C : Char;

Label
  ValidChar;

Begin
  With ZmodemPtr (P)^, PData^ Do
    If HeaderState in [hsNone, hsGotZPad, hsGotZDle] Then
    Begin
      Repeat
        {Get a character, discard Xon and Xoff}
        Repeat
          GetChar (PR, C);
          If (C <> cXon) And (C <> cXoff) Then
            Goto ValidChar;
        Until Not CharReady (PR);

        AsyncStatus := ecNoHeader;
        Exit;

      ValidChar:
        {Handle cancels}
        If C = cCan Then
        Begin
          Inc (CanCount);
          If CanCount >= 5 Then
          Begin
            GotError (PR, ecCancelRequested);
            Exit;
          End;
        End
        Else
          CanCount := 0;

        Case HeaderState Of
          hsNone      : If C = ZPad Then
                          HeaderState := hsGotZPad;
          hsGotZPad   : Case C Of
                          ZDle : HeaderState := hsGotZDle;
                          ZPad : ;
                        Else
                          HeaderState := hsNone;
                        End;
          hsGotZDle   : Case C Of
                          ZHex :
                            Begin
                              HeaderState := hsGotZHex;
                              HexHdrState := hhFrame;
                              WasHex := True;
                              HexPending := False;
                              BlockCheck := 0;
                              If CharReady (PR) Then
                              Begin
                                CollectHexHeader;
                                Exit;
                              End;

                              Break;
                            End;
                          ZBin32 :
                            Begin
                              HeaderState := hsGotZBin32;
                              BinHdrState := bhFrame;
                              WasHex := False;
                              EscapePending := False;
                              BlockCheck := -1;
                              If CharReady (PR) Then
                              Begin
                                CollectBinaryHeader (True);
                                Exit;
                              End;

                              Break;
                            End;
                          ZBin :
                            Begin
                              HeaderState := hsGotZBin;
                              BinHdrState := bhFrame;
                              WasHex := False;
                              EscapePending := False;
                              BlockCheck := 0;
                              If CharReady (PR) Then
                              Begin
                                CollectBinaryHeader (False);
                                Exit;
                              End;

                              Break;
                            End;
                        Else
                          HeaderState := hsNone;
                        End;
        End;
      Until Not CharReady (PR);

      AsyncStatus := ecNoHeader;
    End
    Else
      Case HeaderState Of
          hsGotZHex : CollectHexHeader;
        hsGotZBin32 : CollectBinaryHeader (True);
          hsGotZBin : CollectBinaryHeader (False);
      End;
End;

Function ReceiveBlock (P: ProtocolRecPtr): Boolean;
  {-Get a binary data subpacket, return True when block complete (or error)}
Var
  C : Char;

Begin
  {Assume the block isn't ready}
  ReceiveBlock := False;

  With ZmodemPtr (P)^, PData^ Do
    Repeat
      {Get the waiting character}
      GetCharEscaped (P, C);
      If EscapePending Or ControlCharSkip Or (AsyncStatus = ecCancelRequested)
      Then
        Exit;

      {Always update the block check}
      If UseCrc32 Then BlockCheck := UpdateCrc32 (Ord (C), BlockCheck)
                  Else BlockCheck := UpdateCrc (Ord (C), BlockCheck);

      Case RcvBlockState Of
        rbData : Case AsyncStatus Of
                   ecGotCrcE,
                   ecGotCrcG,
                   ecGotCrcQ,
                   ecGotCrcW :
                     Begin        {End of DataSubpacket - get/check CRC}
                       LastStatus := AsyncStatus;
                       RcvBlockState := rbCrc;
                       CrcCnt := 0;
                     End;
                 Else
                   {ecOk or line error receiving char, count it anyway}
                   Inc (DataBlockLen);
                   If DataBlockLen > BlockLen Then
                   Begin
                     GotError (PR, ecLongPacket);
                     Inc (TotalErrors);
                     Inc (BlockErrors);
                     ReceiveBlock := True;
                     Exit;
                   End;

                   DataBlock^ [DataBlockLen] := C;
                 End;
        rbCrc :
          Begin
            Inc (CrcCnt);
            If ((CrcCnt = 2) And Not UseCrc32) Or
               ((CrcCnt = 4) And UseCrc32) Then
            Begin
              If Not VerifyBlockCheck (P) Then
              Begin
                Inc (BlockErrors);
                Inc (TotalErrors);
                GotError (PR, ecBlockCheckError);
              End
              Else {Show proper status}
                AsyncStatus := LastStatus;

              {Say block is ready for processing}
              ReceiveBlock := True;
              Exit;
            End;
          End;
      End;
    Until Not CharReady (PR);
End;

Procedure ExtractFileInfo (P: ProtocolRecPtr);
Var
  i, Code  : Integer;
  BlockPos : Word;
  S        : String;
  Dir      : DirStr;
  Name     : NameStr;
  Ext      : ExtStr;
  Finish   : Boolean;

Begin
  With ZmodemPtr (P)^, PData^ Do
  Begin
    BlockPos := 1;

    While (BlockPos < 255) And (DataBlock^ [BlockPos] <> #0) Do
      Inc (BlockPos);

    SetString (DataBlock^ [1], S, BlockPos - 1);
    Pathname := ValidatePathname (UpString (PlaceSubStr (S, '/', '\')));
    If Not FlagIsSet (apFlags, apHonorDirectory) Then
    Begin
      FSplit (Pathname, Dir, Name, Ext);
      Pathname := AddBackSlash (DestDir) + Name + Ext;
    End;

    Finish := True;
    i := 1;

    While i <= 255 Do
      Case DataBlock^ [BlockPos + i] Of
        ' ' : Begin
                Finish := False;
                Break;
              End;
        #0  : Break;
      Else
        Inc (i);
      End;

    SetString (DataBlock^ [BlockPos + 1], S, i - 1);
    Val (S, SrcFileLen, Code);
    If Code <> 0 Then
      SrcFileLen := 0;
    BytesRemaining := SrcFileLen;
    BytesTransferred := 0;
    SrcFileDate := CurrentTimeStamp;

    If Not Finish Then
    Begin
      Inc (BlockPos, i);
      i := 1;

      While i <= 255 Do
        Case DataBlock^ [BlockPos + i] Of
          ' ', #0 : Break;
        Else
          Inc (i);
        End;

      SetString (DataBlock^ [BlockPos + 1], S, i - 1);
      S := TrimZeros (S);
      If S <> '' Then
        SrcFileDate := OctalStr2Long (S);
    End;
  End;
End;

{$F+}
Procedure zpPrepareWriting (P: ProtocolRecPtr);
  {-Prepare to save protocol blocks (usually opens a file)}
Var
  FileLen, FileCrc, FileDate,
  SeekPoint, FileStartOfs     : LongInt;
  Result                      : Word;
  FileOpt                     : Byte;
  FileExists                  : Boolean;
  FileSkip                    : Boolean;

Label
  ExitPoint;

Begin
  With ZmodemPtr (P)^, PData^ Do
  Begin
    AsyncStatus := ecOk;

    {Set file mgmt options}
    FileSkip := (FileMgmtOpts And FileSkipMask) = FileSkipMask;
    FileOpt := FileMgmtOpts And FileMgmtMask;

    {Check for a local request for file recovery}
    If ReceiverRecover Then
      ConvertOpts := ConvertOpts Or FileRecover;

    {Does the file exist already?}
    SaveMode := FileMode;
    FileMode := AproFileMode;
    Assign (WorkFile, Pathname);
    Reset (WorkFile, 1);
    FileMode := SaveMode;
    Result := IOResult;

    {Exit on errors other than FileNotFound}
    If (Result <> 0) And (Result <> 2) Then
    Begin
      GotError (PR, epFatal+Result);
      Goto ExitPoint;
    End;

    {Note if file exists, its size and timestamp}
    FileExists := Result = 0;

    If FileExists Then
    Begin
      FileLen := FileSize (WorkFile);
      GetFTime (WorkFile, FileDate);
      FileDate := PackToYMTimeStamp (FileDate);
    End;

    Close (WorkFile);
    If IOResult = 0 Then;

    {If recovering, skip all file managment checks and go append file}
    If FileExists And (SrcFileLen > FileLen) And
       ((ConvertOpts And FileRecover) = FileRecover) Then
    Begin
      SeekPoint := FileLen;
      FileStartOfs := FileLen;
      InitFilePos := FileLen;
    End Else
    Begin
      {Tell status we're not recovering}
      InitFilePos := 0;

      {Check for skip condition}
      If FileSkip And Not FileExists Then
      Begin
        AsyncStatus := ecFileDoesntExist;
        Goto ExitPoint;
      End;

      {Process the file management options}
      SeekPoint := 0;
      FileStartOfs := 0;

      Case FileOpt Of
        WriteNewerLonger : {Transfer only if new, newer or longer}
          If FileExists Then
            If (SrcFileDate <= FileDate) And
               (SrcFileLen <= FileLen) Then
            Begin
              AsyncStatus := ecCantWriteFile;
              Goto ExitPoint;
            End;

        WriteAppend :      {Transfer regardless, append if exists}
          If FileExists Then
            SeekPoint := FileLen;

        WriteClobber :     {Transfer regardless, overwrite} ;
          {Nothing to do, this is the normal behavior}

        WriteDifferent :   {Transfer only if new, size diff, or dates diff}
          If FileExists Then
            If (SrcFileDate = FileDate) And
               (SrcFileLen = FileLen) Then
            Begin
              AsyncStatus := ecCantWriteFile;
              Goto ExitPoint;
            End;

        WriteProtect :     {Transfer only if dest file doesn't exist}
          If FileExists Then
          Begin
            AsyncStatus := ecCantWriteFile;
            Goto ExitPoint;
          End;

        WriteCrc,          {Not supported, treat as WriteNewer}
        WriteNewer :       {Transfer only if new or newer}
          If FileExists Then
            If SrcFileDate <= FileDate Then
            Begin
              AsyncStatus := ecCantWriteFile;
              Goto ExitPoint;
            End;
      End;
    End;

    {Rewrite or append to file}
    Assign (WorkFile, Pathname);
    If SeekPoint = 0 Then
      ReWrite (WorkFile, 1) {New or overwriting destination file}
    Else
    Begin
      SaveMode := FileMode;
      FileMode := $12; {ReadWrite + DenyReadWrite}
      ReSet (WorkFile, 1); {Appending to file}
      FileMode := SaveMode;
      Seek (WorkFile, SeekPoint);
    End;

    Result := IOResult;
    If Result <> 0 Then
    Begin
      GotError (PR, epFatal+Result);
      Goto ExitPoint;
    End;

    {Initialized the buffer management vars}
    StartOfs := FileStartOfs;
    BufLen := 0;
    FileOfs := FileStartOfs;
    RealFileOfs := SeekPoint;
    FileOpen := True;
    Exit;

  ExitPoint:
    Close (WorkFile);
    If IOResult <> 0 Then;
  End;
End;

Procedure zpFinishWriting (P: ProtocolRecPtr);
  {-Cleans up after saving all protocol blocks}
Var
  Written, Result : Word;

Begin
  With ZmodemPtr (P)^. PData^ Do
    If FileOpen Then
    Begin
      {Error or end-of-file, commit buffer}
      If RealFileOfs <> StartOfs Then
        Seek (WorkFile, StartOfs);
      BlockWrite (WorkFile, FileBuffer^, BufLen, Written);
      Result := IOResult;
      If (Result <> 0) Then
        GotError (PR, epFatal + Result)
      Else
        If (Written <> BufLen) Then
          GotError (PR, epFatal + ecDiskFull);

      {Set the timestamp to that of the source file}
      SetFTime (WorkFile, FromUnixDate (SrcFileDate));

      {Clean up}
      Close (WorkFile);
      If IOResult <> 0 Then;
      FileOpen := False;
    End;
End;
{$F-}

Procedure WriteDataBlock (P: ProtocolRecPtr);
  {-Call WriteProtocolBlock for the last received DataBlock}
Var
  SaveStatus : Word;

Begin
  With ZmodemPtr (P)^, PData^ Do
    {Call the method to write this block}
    If WriteProtocolBlock (P, DataBlock^, DataBlockLen) Then
    Begin
      SaveStatus := AsyncStatus;
      Cancel (P);
      AsyncStatus := SaveStatus;
    End Else
    Begin
      Inc (FileOfs, DataBlockLen);
      Dec (BytesRemaining, DataBlockLen);
      Inc (BytesTransferred, DataBlockLen);
      ElapsedTics := ElapsedTime (Timer);
    End;
End;

Procedure ProtocolReceiveZM (P: ProtocolRecPtr);
  {-Receive multiple Zmodem files}
Var
  C : Char;

Label
  ExitPoint;

Begin
  With ZmodemPtr (P)^, PData^ Do
  Begin
    {Do parent inits}
    PR^. ProtocolActive := True;
    GotOneFile := False;

    {Init the status stuff}
    ResetStatus (P);
    ShowFirstStatus (P);
    NewTimer (StatusTimer, StatusInterval);

    {Flush input buffer}
    FlushInBuffer (PR);

    HeaderType := ZrInit;
    ZmodemState := rzRqstFile;
    HeaderState := hsNone;
    AsyncStatus := ecOk;
    SaveStatus := ecOk;

    Repeat
      {Handle status updates}
      {Show status at requested intervals and after significant events}
      If ForceStatus Or TimerExpired (StatusTimer) Then
      Begin
        ForceStatus := False;
        apUserStatus (P, False, False);
        NewTimer (StatusTimer, StatusInterval);
      End;

      If ZmodemState in [rzDelay, rzWaitFile, rzSendBlockPrep, rzSendBlock,
         rzCollectFile, rzStartData, rzCollectData, rzWaitEof, rzCollectFinish,
         rzWaitCancel] Then
      Begin
        UserBack (P);

        {Check for user abort}
        If (SaveStatus <> ecCancelRequested) And PR^. UserAbort Then
        Begin
          AbortProtocol (P);
          AsyncStatus := ecCancelRequested;
          ZmodemState := rzError;
        End
        Else
          {Preprocess header requirements}
          If ZmodemState in [rzWaitFile, rzStartData, rzWaitEof] Then
            If CharReady (PR) Then {Header might be present, try to get one}
            Begin
              CheckForHeader (P);
              If AsyncStatus = ecCancelRequested Then
                ZmodemState := rzError;
            End
            Else
              If TimerExpired (ReplyTimer) Then
                AsyncStatus := ecTimeout
              Else
                AsyncStatus := ecNoHeader;
      End;

      {Main state processor}
      Case ZmodemState Of
        rzRqstFile :
          Begin
            CanCount := 0;

            {Init pos/flag bytes to zero}
            LongInt (TransHeader) := 0;
            {Set our receive options}
            If HeaderType = ZrInit Then
              TransHeader [ZF0] := CanFdx Or     {Full duplex}
                                   CanOvIO Or    {Overlap I/O}
                                   CanFc32 Or    {Use Crc32 on frames}
                                   CanBrk;       {Can send break}

            {Testing shows that Telix needs a delay here}
            NewTimer (ReplyTimer, TelixDelay);
            ZmodemState := rzDelay;
          End;

        rzDelay :
          If TimerExpired (ReplyTimer) Then
          Begin
            {Send the header}
            PutHexHeader (P, HeaderType);
            HeaderType := ZrInit;
            ZmodemState := rzWaitFile;
            HeaderState := hsNone;
            NewTimer (ReplyTimer, HandshakeWait);
          End;

        rzWaitFile :
          Case AsyncStatus Of
            ecGotHeader :
              Case RcvFrame Of
                ZFile : {Beginning of file transfer attempt}
                  Begin
                    {Save conversion and transport options}
                    ConvertOpts := RcvHeader [ZF0];
                    TransportOpts := RcvHeader [ZF2];

                    {Save file mgmt options (if not overridden)}
                    If Not FileMgmtOverride Then
                      FileMgmtOpts := RcvHeader [ZF1];

                    {Set file mgmt default if none specified}
                    If FileMgmtOpts = 0 Then
                      FileMgmtOpts := WriteNewer;

                    {Start collecting the ZFile's data subpacket}
                    ZmodemState := rzCollectFile;
                    BlockErrors := 0;
                    DataBlockLen := 0;
                    BlockCheck := StartBCheck [UseCrc32];
                    RcvBlockState := rbData;
                    NewTimer (ReplyTimer, HandshakeWait);
                  End;
                ZSInit :  {Sender's transmission options}
                  Begin
                    {Start collecting ZSInit's data subpacket}
                    BlockErrors := 0;
                    DataBlockLen := 0;
                    BlockCheck := StartBCheck [UseCrc32];
                    RcvBlockState := rbData;
                    NewTimer (ReplyTimer, HandshakeWait);
                    If WasHex Then
                    Begin
                      ZmodemState := rzSendBlockPrep;
                      DiscardCnt := 0;
                    End
                    Else
                      ZmodemState := rzSendBlock;
                  End;
                ZrQInit : {Go send ZrInit again}
                  ZmodemState := rzRqstFile;
                ZCompl,
                ZFin :     {Finished}
                  Begin
                    ZmodemState := rzSendFinish;
                    BlockErrors := 0;
                  End;
                ZFreeCnt : {Sender is requesting a count of our freespace}
                  Begin
                    LongInt (TransHeader) := DiskFree (0);
                    If LongInt (TransHeader) < 0 Then
                      LongInt (TransHeader) := 2147483647;
                    PutHexHeader (P, ZAck);
                    NewTimer (ReplyTimer, HandshakeWait);
                  End;
                ZCommand : {Commands not implemented}
                  Begin
                    PutHexHeader (P, ZNak);
                    NewTimer (ReplyTimer, HandshakeWait);
                  End;
              End;
            ecBlockCheckError,
            ecTimeout :
              Begin
                Inc (BlockErrors);
                If BlockErrors < HandshakeRetry Then
                  ZmodemState := rzRqstFile
                Else
                Begin
                  {Failed to handsake}
                  GotError (PR, ecFailedToHandshake);
                  ZmodemState := rzCleanup;
                End;
              End;
          End;

        rzSendBlockPrep :
          If CharReady (PR) Then
          Begin
            {Discard the first two chars}
            GetChar (PR, C);
            Inc (DiscardCnt);
            If DiscardCnt = 2 Then
              ZmodemState := rzSendBlock;
          End
          Else
            If TimerExpired (ReplyTimer) Then
            Begin
              Inc (BlockErrors);
              If BlockErrors < HandshakeRetry Then
              Begin
                PutHexHeader (P, ZNak);
                NewTimer (ReplyTimer, HandshakeWait);
                ZmodemState := rzWaitFile;
                HeaderState := hsNone;
              End
              Else
                ZmodemState := rzCleanup;
            End;

        rzSendBlock :
          If CharReady (PR) Then
          Begin
            {Collect the data subpacket}
            If ReceiveBlock (P) Then
              If AsyncStatus <> ecBlockCheckError Then
              Begin
                {Save attention string}
                Move (DataBlock^, AttentionStr, SizeOf (AttentionStr));

                {Turn on escaping if transmitter requests it}
                EscapeAll := (RcvHeader [ZF0] And TEscAll) = TEscAll;

                {Needs an acknowledge}
                LongInt (TransHeader) := 0;
                PutHexHeader (P, ZAck);
                {Go wait for ZFile packet}
                ZmodemState := rzWaitFile;
                HeaderState := hsNone;
                NewTimer (ReplyTimer, HandshakeWait);
              End
              Else
                {Error receiving block, go try again}
                ZmodemState := rzRqstFile
            Else
              If AsyncStatus = ecCancelRequested Then
                ZmodemState := rzError;
          End
          Else
            If TimerExpired (ReplyTimer) Then
            Begin
              Inc (BlockErrors);
              If BlockErrors < HandshakeRetry Then
              Begin
                PutHexHeader (P, ZNak);
                NewTimer (ReplyTimer, HandshakeWait);
                ZmodemState := rzWaitFile;
                HeaderState := hsNone;
              End
              Else
                ZmodemState := rzCleanup;
            End;

        rzCollectFile :
          If CharReady (PR) Then
          Begin
            {Collect the data subpacket}
            If ReceiveBlock (P) Then
              If AsyncStatus <> ecBlockCheckError Then
              Begin
                {Got the data subpacket to the ZFile, extract the file information}
                ExtractFileInfo (P);

                {Call user's LogFile Function}
                LogFile (P, lfReceiveStart);

                {Accept this file}
                If Not AcceptFile (P) Then
                Begin
                  LogFile (P, lfReceiveSkip);
                  ZmodemState := rzRqstFile;
                  HeaderType := ZSkip;
                  AsyncStatus := ecCantWriteFile;
                  ForceStatus := True;
                  Goto ExitPoint;
                End;

                {Prepare to write this file}
                PrepareWriting (P);
                Case AsyncStatus Mod 10000 Of
                  0 :                 {Fall thru} ;
                  ecCantWriteFile,
                  ecFileDoesntExist : {Skip this file}
                    Begin
                      LogFile (P, lfReceiveSkip);
                      ZmodemState := rzRqstFile;
                      HeaderType := ZSkip;
                      ForceStatus := True;
                      Goto ExitPoint;
                    End;
                Else
                  SaveStatus := AsyncStatus; {Fatal error opening file}
                  Cancel (P);
                  AsyncStatus := SaveStatus;
                  ZModemState := rzError;
                  Goto ExitPoint;
                End;

                {Go send the initial ZrPos}
                ZmodemState := rzSync;
                ForceStatus := True;
                TimerPending := True;
              End
              Else
                {Error getting block, go try again}
                ZmodemState := rzRqstFile
            Else
              If AsyncStatus = ecCancelRequested Then
                ZmodemState := rzError;
          End
          Else
            If TimerExpired (ReplyTimer) Then
            Begin
              Inc(BlockErrors);
              If BlockErrors < HandshakeRetry Then
              Begin
                PutHexHeader (P, ZNak);
                NewTimer (ReplyTimer, HandshakeWait);
              End
              Else
                ZmodemState := rzCleanup;
            End;

        rzSync :
          Begin
            {Don't care what's in the buffer now so get rid of it}
            FlushInBuffer (PR);

            {Insert file pos into header and send to remote}
            LongInt (TransHeader) := FileOfs;
            PutHexHeader (P, ZrPos);
            {Set status info}
            BytesRemaining := SrcFileLen - FileOfs;
            BytesTransferred := FileOfs;

            {Start protocol timer now}
            If TimerPending Then
            Begin
              TimerPending := False;
              NewTimer (Timer, 1);
            End;

            ZmodemState := rzStartData;
            HeaderState := hsNone;
            NewTimer (ReplyTimer, HandshakeWait);
          End;

        rzStartData :
          Case AsyncStatus Of
            ecGotHeader :
              Case RcvFrame Of
                ZData :  {One or more data subpackets follow}
                  If FileOfs = LastFileOfs Then
                  Begin
                    BlockErrors := 0;
                    DataBlockLen := 0;
                    BlockCheck := StartBCheck [UseCrc32];
                    RcvBlockState := rbData;
                    ZmodemState := rzCollectData;
                    NewTimer (ReplyTimer, HandshakeWait);
                  End Else
                  Begin
                    Inc (TotalErrors);
                    Inc (BlockErrors);
                    If BlockErrors <= MaxBadBlocks Then
                    Begin
                      PutAttentionString (P);
                      ZmodemState := rzSync;
                    End Else
                    Begin
                      Cancel (P);
                      GotError (PR, ecTooManyErrors);
                      ZmodemState := rzError;
                    End;
                  End;
                ZNak : {Nak received}
                  Begin
                    Inc (TotalErrors);
                    Inc (BlockErrors);
                    If BlockErrors <= MaxBadBlocks Then
                      {Resend ZrPos}
                      ZmodemState := rzSync
                    Else
                    Begin
                      ElapsedTics := ElapsedTime (Timer);
                      Cancel (P);
                      GotError (PR, ecTooManyErrors);
                      ZmodemState := rzError;
                    End;
                  End;
                ZEof : {End of current file}
                  If FileOfs = LastFileOfs Then
                  Begin
                    GotOneFile := True;
                    ElapsedTics := ElapsedTime (Timer);
                    AsyncStatus := ecEndFile;
                    FinishWriting (P);
                    {Send proper status to user logging routine}
                    If AsyncStatus = ecEndFile Then
                      LogFile (P, lfReceiveOk)
                    Else
                      LogFile (P, lfReceiveFail);

                    ZmodemState := rzRqstFile;
                  End
                  Else
                    ZmodemState := rzSync;
                ZFile : {File frame}
                  {Already got a ZFile frame, just go send ZrPos again}
                  ZmodemState := rzSync;
              Else
                {Error during GetHeader}
                Inc (TotalErrors);
                Inc (BlockErrors);
                If BlockErrors <= MaxBadBlocks Then
                Begin
                  PutAttentionString (P);
                  ZmodemState := rzSync;
                End Else
                Begin
                  Cancel (P);
                  GotError (PR, ecTooManyErrors);
                  ZmodemState := rzError;
                End;
              End;
            ecBlockCheckError,
            ecTimeout :
              Begin
                Inc (TotalErrors);
                Inc (BlockErrors);
                If BlockErrors > HandshakeRetry Then
                Begin
                  {Never got ZData header}
                  GotError (PR, ecFailedToHandshake);
                  ZmodemState := rzError;
                End
                Else
                  {Timeout out waiting for ZData, go send ZrPos}
                  ZmodemState := rzSync;
              End;
          End;

        rzCollectData :
          If CharReady (PR) Then
          Begin
            NewTimer (ReplyTimer, HandshakeWait);

            {Collect the data subpacket}
            If ReceiveBlock (P) Then
            Begin
              {Got a block or an error -- process it}
              SaveStatus := AsyncStatus;

              Case AsyncStatus Of
                ecGotCrcG : {Normal subpacket - no response necessary}
                  Begin
                    {Write this block}
                    WriteDataBlock (P);
                    If AsyncStatus = ecOk Then
                    Begin
                      {Prepare to collect next block}
                      DataBlockLen := 0;
                      BlockCheck := StartBCheck [UseCrc32];
                      RcvBlockState := rbData;
                      NewTimer (ReplyTimer, HandshakeWait);
                    End
                    Else
                      ZmodemState := rzError;
                  End;
                ecGotCrcQ : {Zack requested}
                  Begin
                    {Write this block}
                    WriteDataBlock (P);
                    If AsyncStatus = ecOk Then
                    Begin
                      LongInt (TransHeader) := FileOfs;
                      PutHexHeader (P, ZAck);
                      {Don't change state - will get next data subpacket}
                      {Prepare to collect next block}
                      DataBlockLen := 0;
                      BlockCheck := StartBCheck [UseCrc32];
                      RcvBlockState := rbData;
                      NewTimer (ReplyTimer, HandshakeWait);
                    End
                    Else
                      ZmodemState := rzError;
                  End;
                ecGotCrcW : {Send requests a wait}
                  Begin
                    {Write this block}
                    WriteDataBlock (P);
                    If AsyncStatus = ecOk Then
                    Begin
                      {Acknowledge with the current file position}
                      LongInt (TransHeader) := FileOfs;
                      PutHexHeader (P, ZAck);
                      ZmodemState := rzStartData;
                      HeaderState := hsNone;
                      NewTimer (ReplyTimer, HandshakeWait);
                    End
                    Else
                      ZmodemState := rzError;
                  End;
                ecGotCrcE : {Last data subpacket}
                  Begin
                    {Write this block}
                    WriteDataBlock (P);
                    If AsyncStatus = ecOk Then
                    Begin
                      BlockErrors := 0;
                      ZmodemState := rzWaitEof;
                      HeaderState := hsNone;
                      NewTimer (ReplyTimer, HandshakeWait);
                    End
                    Else
                      ZmodemState := rzError;
                  End;
              Else {Error during ReceiveBlock}
                If BlockErrors < MaxBadBlocks Then
                Begin
                  PutAttentionString (P);
                  ZmodemState := rzSync;
                End Else
                Begin
                  GotError (PR, ecGarbage);
                  ZmodemState := rzError;
                End;
              End;

              {Restore AsyncStatus so user status routine can see it}
              If AsyncStatus <> ecOk Then
                AsyncStatus := SaveStatus;
              ForceStatus := True;
            End
            Else
              If AsyncStatus = ecCancelRequested Then
                ZmodemState := rzError
          End
          Else
            If TimerExpired (ReplyTimer) Then
            Begin
              Inc (TotalErrors);
              Inc (BlockErrors);
              If BlockErrors < MaxBadBlocks Then
              Begin
                PutAttentionString (P);
                ZmodemState := rzSync;
              End
              Else
                ZmodemState := rzError;
            End;

        rzWaitEof :
          Case AsyncStatus Of
            ecGotHeader :
              If (RcvFrame = ZEof) And (FileOfs = LastFileOfs) Then
              Begin {End of current file}
                GotOneFile := True;
                ElapsedTics := ElapsedTime (Timer);
                AsyncStatus := ecEndFile;
                SaveStatus := AsyncStatus;
                apUserStatus (P, False, False);
                AsyncStatus := SaveStatus;
                FinishWriting (P);
                If AsyncStatus = ecEndFile Then
                  LogFile (P, lfReceiveOk)
                Else
                  LogFile (P, lfReceiveFail);

                {Go get the next file}
                ZmodemState := rzRqstFile;
              End Else
              Begin {Error during GetHeader}
                Inc (TotalErrors);
                Inc (BlockErrors);
                If BlockErrors > MaxBadBlocks Then
                Begin
                  Cancel (P);
                  GotError (PR, ecTooManyErrors);
                  ZmodemState := rzError;
                End Else
                Begin
                  PutAttentionString (P);
                  ZmodemState := rzSync;
                End;
              End;
            ecBlockCheckError,
            ecTimeout :
              Begin
                Inc (BlockErrors);
                Inc (TotalErrors);
                If BlockErrors > HandshakeRetry Then
                Begin
                  {Never got ZEof header}
                  GotError (PR, ecFailedToHandshake);
                  ZmodemState := rzError;
                End
                Else
                  {Timeout out waiting for ZEof, go send ZrPos}
                  ZmodemState := rzSync;
              End;
          End;

        rzSendFinish :
          Begin
            {Insert file position into header}
            LongInt (TransHeader) := FileOfs;
            PutHexHeader (P, ZFin);
            ZmodemState := rzCollectFinish;
            NewTimer (ReplyTimer, FinishWait);
            BlockErrors := 0;
            OCnt := 0;
          End;

        rzCollectFinish :
          If CharReady (PR) Then
          Begin
            GetChar (PR, C);
            If C = 'O' Then
            Begin
              Inc (OCnt);
              If OCnt = 2 Then
                ZmodemState := rzCleanup;
            End;
          End
          Else
            If TimerExpired (ReplyTimer) Then
            Begin
              {Retry 3 times only (same as DSZ)}
              Inc (BlockErrors);
              If BlockErrors < FinishRetry Then
                {Go send ZFin again}
                ZmodemState := rzSendFinish
              Else
                {Cleanup anyway}
                ZmodemState := rzCleanup;
            End;

        rzError :
          Begin
            If FileOpen Then
            Begin
              SaveStatus := AsyncStatus;
              FinishWriting (P);
              AsyncStatus := SaveStatus;
              LogFile (P, lfReceiveFail);
            End;

            {Wait for cancel to go out}
            If OutBuffUsed (PR) > 0 Then
            Begin
              NewTimer (ReplyTimer, TransTimeout);
              ZModemState := rzWaitCancel;
            End
            Else
              ZModemState := rzCleanup;
          End;

        rzWaitCancel :
          If (OutBuffUsed (PR) = 0) Or TimerExpired (ReplyTimer) Then
            ZmodemState := rzCleanup;

        rzCleanup :
          Begin
            FlushInBuffer (PR);
            ZmodemState := rzDone;
          End;
      End;

    ExitPoint:
      {Clear header state if we just processed a header}
      If (AsyncStatus = ecGotHeader) Or (AsyncStatus = ecNoHeader) Then
        AsyncStatus := ecOk;

      {Store AsyncStatus}
      SaveStatus := AsyncStatus;
    Until ZmodemState = rzDone;

    ShowLastStatus (P);
    PR^. ProtocolActive := False;
  End;
End;

Procedure EscapeBlock (P: ProtocolRecPtr);
{$IFNDEF VirtualPascal}
Type
  OS = Record
         Ofs : Word;
         Seg : Word;
       End;

Var
  i     : Word;
  Start : Pointer;
  Out   : ^Char;
  C     : Char;
{$ENDIF}

Begin
  With ZmodemPtr (P)^ Do
  Begin
    UseCrc32 := CanCrc32;
    If UseCrc32 Then
      PData^. BlockCheck := UpdateCrc32Buf (DataBlock^, DataBlockLen, -1)
    Else
      PData^. BlockCheck := UpdateCrcBuf (DataBlock^, DataBlockLen, 0);

  {$IFDEF VirtualPascal}
    Inc (WorkSize, AsmEscapeBlock (DataBlock^, DataBlockLen,
      WorkBlock^ [WorkSize], EscapingChars, LastChar, EscapeAll));
  {$ELSE}
    Start := @(WorkBlock^ [WorkSize]);
    Out := Start;

    For i := 1 To DataBlockLen Do
    Begin
      C := DataBlock^ [i];

      If (C in EscapingChars) Or
         ((Char (Byte (C) And $7F) = cCR) And (Byte (LastChar) And $7F = $40)) Or
         (EscapeAll And (Byte (C) And $60 = 0)) Then
      Begin
        Out^ := ZDle;
        Inc (Out);
        C := Char (Byte (C) Xor $40);
      End;

      Out^ := C;
      Inc (Out);
      LastChar := C;
    End;

    Inc (WorkSize, OS (Out). Ofs - OS (Start). Ofs);
  {$ENDIF}

    WorkBlock^ [WorkSize] := ZDle;
    WorkBlock^ [WorkSize + 1] := Terminator;
    Inc (WorkSize, 2);

    With PData^ Do
    Begin
      If UseCrc32 Then
        BlockCheck := Not UpdateCrc32 (Byte (Terminator), BlockCheck)
      Else
        BlockCheck := UpdateCrc (0, UpdateCrc (0, UpdateCrc (Byte (Terminator),
          BlockCheck)));

    {$IFDEF VirtualPascal}
      Inc (WorkSize, AsmEscapeBlock (BlockCheck, BlockCheckLen [UseCrc32],
        WorkBlock^ [WorkSize], EscapingChars, LastChar, EscapeAll));
    {$ELSE}
      EscapeChars (P, BlockCheck, BlockCheckLen [UseCrc32], WorkBlock^,
        WorkSize);
    {$ENDIF}
    End;

    If Terminator = ZCrcW Then
    Begin
      WorkBlock^ [WorkSize] := cXon;
      Inc (WorkSize);
    End;
  End;
End;

Procedure TransmitBlock (P: ProtocolRecPtr);
Var
  i : Word;

Begin
  With ZmodemPtr (P)^ Do
  {$IFNDEF OS2}
    i := PutBlock (PData^. PR, WorkBlock^, WorkSize);
  {$ELSE}
    os2PutBlockDirect (PChar (WorkBlock), WorkSize);
  {$ENDIF}
End;

Procedure InsertFileInfo (P: ProtocolRecPtr);
Var
  Dir  : DirStr;
  Name : NameStr;
  Ext  : ExtStr;
  S    : String;

Begin
  With ZmodemPtr (P)^, PData^ Do
  Begin
    If FlagIsSet (apFlags, apIncludeDirectory) Then
      S := Pathname
    Else
    Begin
      FSplit (Pathname, Dir, Name, Ext);
      S := Name + Ext;
    End;

    S := LoString (PlaceSubStr (S, '\', '/')) + #0;
    DataBlockLen := Length (S);
    Move (S [1], DataBlock^, DataBlockLen);

    Str (SrcFileLen, S);
    If SrcFileDate <> 0 Then
      S := S + ' ' + OctalStr (PackToYMTimeStamp (SrcFileDate));
    S := S + #0;

    Move (S [1], DataBlock^ [DataBlockLen + 1], Length (S));
    Inc (DataBlockLen, Length (S));

    BytesTransferred := 0;
    BytesRemaining := SrcFileLen;
  End;
End;

Procedure ProtocolTransmitZM (P: ProtocolRecPtr);
  {-Transmit all files that fit the Mask}
Const
  RZcommand   : Array [1..3] Of Char = 'rz'+cCr;
  RZcommand8K : Array [1..7] Of Char = 'ZAP'+cCr+'rz'+cCr;

Label
  ExitPoint;

Begin
  With ZModemPtr (P)^, PData^ Do
  Begin
    {Do parent inits}
    PR^. ProtocolActive := True;
    FindingFirst := True;

    {Reset status vars}
    ResetStatus (P);
    ShowFirstStatus (P);
    NewTimer (StatusTimer, StatusInterval);
    ForceStatus := False;
    TimerStarted := False;
    FilesSent := False;
    CanCount := 0;
    AsyncStatus := ecOk;

    {Send RZ command}
    If Use8kBlocks Then
      BlockErrors := PutBlock (PR, RZcommand8K, SizeOf (RZcommand8K))
    Else
      BlockErrors := PutBlock (PR, RZcommand, SizeOf (RZcommand));

    {Send ZrQInit header (requests receiver's ZrInit)}
    LongInt (TransHeader) := 0;
    PutHexHeader (P, ZrQInit);

    BlockErrors := 0;
    ZmodemState := tzHandshake;
    HeaderState := hsNone;
    NewTimer (ReplyTimer, HandshakeWait);
    SaveStatus := AsyncStatus;

    Repeat
      {Show status at requested intervals and after significant events}
      If ForceStatus Or TimerExpired (StatusTimer) Then
      Begin
        ForceStatus := False;

        If TimerStarted Then
          ElapsedTics := ElapsedTime (Timer);
        apUserStatus (P, False, False);

        {Use user-specified status interval unless draining eof}
        If ZmodemState = tzDrainEof Then
          NewTimer (StatusTimer, DrainingStatusInterval)
        Else
          NewTimer (StatusTimer, StatusInterval);
      End;

      If ZmodemState in [tzHandshake, tzCheckFile, tzSendData, tzWaitAck,
         tzDrainEof, tzCheckEof, tzCheckFinish] Then
      Begin
        UserBack (P);

        {Check for user abort (but not twice)}
        If (AsyncStatus <> ecCancelRequested) And PR^. UserAbort Then
        Begin
          AbortProtocol (P);
          AsyncStatus := ecCancelRequested;
          ZmodemState := tzError;
        End
        Else
          {Preprocess header requirements}
          If ZmodemState <> tzDrainEof Then
            If CharReady (PR) Then {Header might be present, try to get one}
            Begin
              CheckForHeader (P);
              If AsyncStatus = ecCancelRequested Then
                ZmodemState := tzError;
            End
            Else
              If TimerExpired (ReplyTimer) Then
                AsyncStatus := ecTimeout
              Else
                AsyncStatus := ecNoHeader;
      End;

      {Process the current state}
      Case ZmodemState Of
        tzHandshake :
          Case AsyncStatus Of
            ecGotHeader :
              Case RcvFrame Of
                ZrInit :     {Got ZrInit, extract info}
                  Begin
                    RcvBuffLen := RcvHeader [ZP0] + (RcvHeader [ZP1] Shl 8);
                    CanCrc32 := (RcvHeader [ZF0] And CanFc32) <> 0;
                    If CanCrc32 Then CheckType := bcCrc32
                                Else CheckType := bcCrc16;
                    EscapeAll := (RcvHeader [ZF0] And EscAll) <> 0;
                    RemoteCanOvIO := ((RcvHeader [ZF0] And CanOvIO) <> 0) Or
                      (RcvBuffLen > 0);
                    ZmodemState := tzGetFile;
                  End;
                ZChallenge : {Receiver is challenging, respond with same number}
                  Begin
                    TransHeader := RcvHeader;
                    PutHexHeader (P, ZAck);
                  End;
                ZrQInit :    {Remote is trying to transmit also, do nothing}
                  ;
              Else           {Unexpected reply, nak it}
                PutHexHeader (P, ZNak);
              End;
            ecBlockCheckError,
            ecTimeout  : {Send another ZrQinit}
              Begin
                Inc (TotalErrors);
                Inc (BlockErrors);
                If BlockErrors > HandshakeRetry Then
                Begin
                  {Never got ZrInit}
                  GotError (PR, ecFailedToHandshake);
                  ZmodemState := tzError;
                End Else
                Begin
                  LongInt (TransHeader) := 0;
                  PutHexHeader (P, ZrQInit);
                  NewTimer (ReplyTimer, HandshakeWait);
                End;
              End;
            End;

        tzGetFile :
          Begin
            {Get the next file to send}
            If Not NextFile (P, Pathname) Then
            Begin
              LongInt (TransHeader) := 0;
              PutHexHeader (P, ZFin);
              BlockErrors := 0;
              ZmodemState := tzCheckFinish;
              HeaderState := hsNone;
              NewTimer (ReplyTimer, FinishWait);
              Goto ExitPoint;
            End;

            FilesSent := True;
            {Let all hooks see an upper case pathname}
            Pathname := UpString (Pathname);

            {Show file name to user logging routine}
            LogFile (P, lfTransmitStart);

            {Prepare to read file blocks}
            PrepareReading (P);
            If AsyncStatus <> ecOk Then
            Begin
              Cancel (P);
              ZmodemState := tzError;
              Goto ExitPoint;
            End;

            {Start protocol timer now}
            NewTimer (Timer, 1);
            TimerStarted := True;
            ForceStatus := True;

            {Build the header data area}
            LongInt (TransHeader) := 0;
            TransHeader [ZF1] := FileMgmtOpts;
            If ReceiverRecover Then
              TransHeader [ZF0] := FileRecover;

            {Insert file information into datablock}
            InsertFileInfo (P);

            {Send the ZFile header and data subpacket with file info}
            WorkSize := 0;
            Terminator := ZCrcW;
            BufBinaryHeader (P, ZFile, WorkBlock^, WorkSize);
            EscapeBlock (P);
            TransmitBlock (P);

            FileOfs := 0;
            ForceStatus := True;

            {Go wait for response}
            BlockErrors := 0;
            ZmodemState := tzCheckFile;
            HeaderState := hsNone;
            NewTimer (ReplyTimer, HandshakeWait);
          End;

        tzCheckFile :
          Case AsyncStatus Of
            ecGotHeader :
              Case RcvFrame Of
                ZrPos :  {Receiver tells us where to seek in our file}
                  Begin
                    {Get file offset}
                    FileOfs := LongInt (RcvHeader);
                    InitFilePos := FileOfs;
                    BytesTransferred := FileOfs;
                    BytesRemaining := SrcFileLen - BytesTransferred;

                    {Go send the data subpackets}
                    ZModemState := tzStartData;
                  End;
                ZSkip :  {Receiver wants to skip this file}
                  Begin
                    AsyncStatus := ecSkipFile;
                    apUserStatus (P, False, False);
                    AsyncStatus := ecOk;

                    {Close file and log skip}
                    apFinishReading (P);
                    LogFile (P, lfTransmitSkip);

                    {Go look for another file}
                    ZmodemState := tzGetFile;
                  End;
                ZCrc :   {Receiver is asking for Crc32 of the file, send it}
                  Begin
                    LongInt (TransHeader) := Crc32OfFile (P, LongInt (RcvHeader));
                    If AsyncStatus = ecOk Then
                      PutHexHeader (P, ZCrc)
                    Else
                      ZModemState := tzError;
                  End;
              End;
            ecBlockCheckError,
            ecTimeout :  {Timeout waiting for response to ZFile}
              Begin
                Inc (BlockErrors);
                Inc (TotalErrors);
                If BlockErrors > HandshakeRetry Then
                Begin
                  {Never got response to ZFile}
                  GotError (PR, ecTimeout);
                  ZmodemState := tzError;
                End Else
                Begin
                  {Resend ZFile}
                  TransmitBlock (P);

                  HeaderState := hsNone;
                  NewTimer (ReplyTimer, HandshakeWait);
                End;
              End;
          End;

        tzStartData :
          Begin
            {Get ready}
            DataInTransit := 0;
            BlockErrors := 0;

            {Send ZData header}
            LongInt (TransHeader) := FileOfs;
            WorkSize := 0;
            BufBinaryHeader (P, ZData, WorkBlock^, WorkSize);

            ZmodemState := tzEscapeData;
          End;

        tzEscapeData :
          Begin
            {Get a block to send}
            If TookHit Then
            Begin
              Inc (GoodAfterBad);
              If GoodAfterBad > 4 Then
              Begin
                GoodAfterBad := 0;
                BlockLen := BlockLen Shl 1;
                If BlockLen = ZMaxBlk [Use8KBlocks] Then
                  TookHit := False;
              End;
            End;

            DataBlockLen := BlockLen;
            LastBlock := ReadProtocolBlock (P, DataBlock^, DataBlockLen);
            If AsyncStatus <> ecOk Then
            Begin
              SaveStatus := AsyncStatus;
              Cancel (P);
              AsyncStatus := SaveStatus;
              ZmodemState := tzError;
              Goto ExitPoint;
            End;

            {Show the new data on the way}
            If RcvBuffLen > 0 Then
              Inc (DataInTransit, DataBlockLen);

            {Set the terminator}
            If LastBlock Then
              {Tell receiver its the last subpacket}
              Terminator := ZCrcE
            Else
              If Not RemoteCanOvIO Or TookHit Or ((RcvBuffLen > 0) And
                 (DataInTransit + ZMaxBlk [Use8KBlocks] > RcvBuffLen))
              Then
                {Wait for acknowledge}
                Terminator := ZCrcW
              Else
                {Normal data subpacket, no special action}
                Terminator := ZCrcG;

            {Escape this data into WorkBlock}
            EscapeBlock (P);

            BlockErrors := 0;
            ZmodemState := tzSendData;
            NewTimer (ReplyTimer, TransTimeout);
          End;

        tzSendData :
          Case AsyncStatus Of
            ecNoHeader : {Nothing from receiver, keep going}
              If OutBuffFree (PR) > WorkSize + FreeMargin Then
              Begin {Wait for buffer free space}
                TransmitBlock (P);
                Inc (FileOfs, DataBlockLen);
                Inc (BytesTransferred, DataBlockLen);
                Dec (BytesRemaining, DataBlockLen);
                ForceStatus := True;

                If LastBlock Then
                Begin
                  BlockErrors := 0;
                  ZmodemState := tzSendEof;
                End
                Else
                  If Terminator = ZCrcW Then
                  Begin
                    ZmodemState := tzWaitAck;
                    NewTimer (ReplyTimer, TransTimeout);
                  End Else
                  Begin
                    WorkSize := 0;
                    ZmodemState := tzEscapeData;
                  End;
              End; {Timeout handled at top of state machine}

            ecGotHeader : {Got a header from the receiver, process it}
              Case RcvFrame Of
                ZAck :         {Response to last CrcW data subpacket}
                  ;
                ZrPos :        {Receiver is sending its desired file position}
                  Begin
                    FlushOutBuffer (PR);
                    Inc (TotalErrors);
                    FileOfs := LongInt (RcvHeader);
                    BytesTransferred := FileOfs;
                    BytesRemaining := SrcFileLen - BytesTransferred;
                    If BlockLen > 64 Then
                      BlockLen := BlockLen Shr 1;
                    TookHit := True;
                    GoodAfterBad := 0;
                    ZModemState := tzStartData;
                  End;
                ZCan, ZAbort, ZFerr : {Receiver says quit}
                  Begin
                    GotError (PR, ecCancelRequested);
                    ZmodemState := tzError;
                  End;
                ZSkip, ZrInit : {Finished with this file}
                  Begin
                    FlushOutBuffer (PR);
                    apFinishReading (P);
                    AsyncStatus := ecSkipFile;
                    LogFile (P, lfTransmitSkip);
                    ZmodemState := tzGetFile;
                  End;
              Else
                {Garbage, send Nak}
                PutBinaryHeader (P, ZNak);
              End;

            ecBlockCheckError :
              PutBinaryHeader (P, ZNak);

            ecTimeout :
              Begin
                GotError (PR, ecBufferIsFull);
                ZmodemState := tzError;
              End;
          End;

        tzWaitAck :
          Case AsyncStatus Of
            ecGotHeader :
              Case RcvFrame Of
                ZAck :
                  If LongInt (RcvHeader) = FileOfs Then
                    ZmodemState := tzStartData
                  Else
                    NewTimer (ReplyTimer, TransTimeout);
                ZrPos :        {Receiver is sending its desired file position}
                  Begin
                    FlushOutBuffer (PR);
                    Inc (TotalErrors);
                    FileOfs := LongInt (RcvHeader);
                    BytesTransferred := FileOfs;
                    BytesRemaining := SrcFileLen - BytesTransferred;
                    If BlockLen > 64 Then
                      BlockLen := BlockLen Shr 1;
                    TookHit := True;
                    GoodAfterBad := 0;
                    ZmodemState := tzStartData;
                  End;
                ZCan, ZAbort, ZFerr : {Receiver says quit}
                  Begin
                    GotError (PR, ecCancelRequested);
                    ZmodemState := tzError;
                  End;
                ZSkip, ZrInit : {Finished with this file}
                  Begin
                    FlushOutBuffer (PR);
                    apFinishReading (P);
                    AsyncStatus := ecSkipFile;
                    LogFile (P, lfTransmitSkip);
                    ZmodemState := tzGetFile;
                  End;
              Else
                {Garbage, send Nak}
                PutHexHeader (P, ZNak);
              End;
            ecBlockCheckError,
            ecTimeout :
              Begin
                Inc (TotalErrors);
                Inc (BlockErrors);
                If BlockErrors > MaxBadBlocks Then
                  ZmodemState := tzError
                Else
                  ZmodemState := tzStartData;
              End;
          End;

        tzSendEof :
          Begin
            {Send the eof}
            LongInt (TransHeader) := FileOfs;
            PutBinaryHeader (P, ZEof);
            ZModemState := tzDrainEof;
            NewTimer (StatusTimer, DrainingStatusInterval);
            NewTimer (ReplyTimer, TransTimeout);
          End;

        tzDrainEof :
          {Dawdle until output buffer is empty}
          If OutBuffUsed (PR) = 0 Then
          Begin
            ZmodemState := tzCheckEof;
            HeaderState := hsNone;
            NewTimer (ReplyTimer, FinishWait);
          End
          Else
            If TimerExpired (ReplyTimer) Then
            Begin
              AsyncStatus := ecTimeout;
              ZmodemState := tzError;
            End;

        tzCheckEof :
          Case AsyncStatus Of
            ecGotHeader :
              Case RcvFrame Of
                ZrInit, ZSkip : {Finished with this file}
                  Begin
                    {Close file and log success}
                    apFinishReading (P);
                    AsyncStatus := ecOk;
                    LogFile (P, lfTransmitOk);

                    {Go look for another file}
                    ZmodemState := tzGetFile;
                  End;
                ZrPos :        {Receiver is sending its desired file position}
                  Begin
                    FlushOutBuffer (PR);
                    FileOfs := LongInt (RcvHeader);
                    BytesTransferred := FileOfs;
                    BytesRemaining := SrcFileLen - BytesTransferred;
                    If BlockLen > 64 Then
                      BlockLen := BlockLen Shr 1;
                    TookHit := True;
                    GoodAfterBad := 0;
                    ZModemState := tzStartData;
                  End;
                ZAck :         {Response to last CrcW data subpacket}
                  ;
                ZCan, ZAbort, ZFerr : {Receiver says quit}
                  Begin
                    GotError (PR, ecCancelRequested);
                    ZmodemState := tzError;
                  End;
              Else
                {Garbage, send Nak}
                PutBinaryHeader (P, ZNak);
              End;
            ecBlockCheckError,
            ecTimeout :
              Begin
                Inc (BlockErrors);
                Inc (TotalErrors);
                If BlockErrors > MaxBadBlocks Then
                  ZmodemState := tzError
                Else
                  ZmodemState := tzSendEof;
              End;
          End;

        tzCheckFinish :
          Case AsyncStatus Of
            ecGotHeader :
              Begin
                If RcvFrame = ZFin Then
                Begin
                  PutChar (PR, 'O');
                  PutChar (PR, 'O');
                End
                Else
                  AsyncStatus := ecOk;

                ZmodemState := tzCleanup;
              End;
            ecBlockCheckError,
            ecTimeout :
              Begin
                {Just give up}
                AsyncStatus := ecOk;
                ZmodemState := tzCleanup;
              End;
          End;

        tzError :
          Begin
            {Cleanup on aborted or canceled protocol}
            FlushOutBuffer (PR);
            If FilesSent Then
            Begin
              apFinishReading (P);
              LogFile (P, lfTransmitFail);
            End;
            ZmodemState := tzCleanup;
          End;

        tzCleanup:
          Begin
            {Flush last few chars from last received header}
            FlushInBuffer (PR);

            If (AsyncStatus = ecOk) And Not FilesSent Then
              AsyncStatus := ecNoMatchingFiles;

            ZmodemState := tzDone;
          End;
      End;

    ExitPoint:
      {Clear header state if we just processed a header}
      If (AsyncStatus = ecGotHeader) Or (AsyncStatus = ecNoHeader) Then
        AsyncStatus := ecOk;

      {Store AsyncStatus}
      SaveStatus := AsyncStatus;
    Until ZmodemState = tzDone;

    ShowLastStatus (P);
    PR^. ProtocolActive := False;
  End;
End;

End.
