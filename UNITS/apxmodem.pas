{$I-,S-}
{$IFNDEF OS2}
{$R-,V-,B-,F+,O+,A-}
{$ENDIF}

{Conditional defines that may affect this unit}
{$I APDEFINE.INC}

{*********************************************************}
{*                    APXMODEM.PAS 2.03                  *}
{*     Copyright (c) TurboPower Software 1991.           *}
{* Portions copyright (c) Information Technology 1989,   *}
{*    and used under license to TurboPower Software      *}
{*                 All rights reserved.                  *}
{*********************************************************}

unit ApXmodem;
  {-Provides Xmodem/CRC/1K recieve and transmit functions}

interface

Uses
{$IFNDEF OS2}
  ApMisc,
  ApPort,
{$ELSE}
  ApOS2,
{$ENDIF}

  ApTimer,
  ApCom,
  ApSame,
  tMisc,
  Crc,
  ApAbsPcl;

const
  RelaxedHandShakeWait = 364;    {Relaxed wait time during handshaking (20 sec)}
  DefBlockWait = 91;             {Normal between-block wait time (5 sec)}
  RelaxedBlockWait = 182;        {Relaxed between-block wait time (10 sec)}
  CancelTimeout = 3;             {Drain timeout for cancel sequence}
  MaxSkipChars = 1000;           {Maximum chars skipped while seeking Soh}
  DrainWait = 1092;              {OutBuf drain time before error (60 sec)}
  MaxCrcTry = 3;                 {Max tries for Crc before trying checksum}
  CrcReq = 'C';                  {Crc mode request}

  {Run-time constants}
  DefMaxBlockErrors : Byte = 5;  {Default maximum acceptable errors per block}
  DefFinishWait     : Word = DefBlockWait*2; {Wait GMode EOT response}

  {For estimating protocol transfer times}
  XmodemOverhead : Word = 5;     {Overhead bytes for each block}
  XmodemTurnDelay : Word = 1000; {MSec turnaround delay for each block}

type
  {Xmodem protocol transmit states}
  XmodemStateType = (
    {Transmit states}
    txInitial,              {0  Open file, log it, etc.}
    txHandshake,            {1  Waiting for handshake}
    txGetBlock,             {2  Get the next block to transmit}
    txWaitFreeSpace,        {3  Wait until outbuffer has enough freespace}
    txSendBlock,            {4  Send next protocol block}
    txDraining,             {5  Waiting for protocol block to drain}
    txReplyPending,         {6  Waiting for reply to last block}
    txEndDrain,             {7  Wait for outbuf to drain before EOT}
    txFirstEndOfTransmit,   {8  Send first EOT}
    txRestEndOfTransmit,    {9  Send subseqent EOTs}
    txEotReply,             {10  Waiting for EOT reply}
    txFinished,             {11 Close file, log it, etc.}
    txDone,                 {12 Signal end of protocol}

    {Receive states}
    rxInitial,              {13 Initialize vars, get buffers, etc.}
    rxWaitForHSReply,       {14 Waiting for 1st reply to handshake}
    rxWaitForBlockStart,    {15 Wait for block start}
    rxCollectBlock,         {16 Collect data}
    rxProcessBlock,         {17 Process block}
    rxFinishedSkip,         {17 Close file, log as skip}
    rxFinished,             {18 Close file, log as good/bad}
    rxDone);                {19 Signal end of protocol}

  {Xmodem protocol record}
  XmodemPtr = ^XmodemProtocol;
  XmodemProtocol = record
    {General protocol data}
    PData          : ProtocolDataPtr;   {General protocol data}
    StartChar      : Char;              {Block start character}
    OneKMode       : Boolean;           {True for XModem1K}
    BlockWait      : Byte;              {Wait seconds between blocks}
    MaxBlockErrors : Byte;              {Max number of allowed block errors}
    FirstBlockNum  : Byte;              {First block number to use}
    GMode          : Boolean;           {True for YmodemG}
    TimerPending   : Boolean;           {True if waiting to start rcv timer}
    FinishWait     : Word;              {Extra wait GMode EOT response}

    {Temp vars that state machine requires to be static}
    HandshakeChar  : Char;              {Last handshake char used}
    NaksReceived   : Byte;              {Count naks received}
    EotCounter     : Byte;              {Counts received EOTs}
    CanCounter     : Byte;              {Counts received cCans}
    ReplyTimer     : EventTimer;        {Track timeouts waiting for replies}
    NoMoreData     : Boolean;           {Flag for tracking end-of-file}
    DataBlock      : ^DataBlockType;    {Data block}
    CharsLeft      : Word;              {Characters not yet xmitted}
    OutBufPos      : Word;              {Output buffer pos for reduced blocks}
    BlkIndex       : Word;              {Index into received chars in DataBlock}
    OverheadLen    : Word;              {Number of overhead bytes per block}
    LastBlockSize  : Word;              {Number of bytes last read}

    {State information}
    XmodemState    : XmodemStateType;   {Current state of Xmodem}
  end;

{$IFDEF UsePModeDLL}

procedure InitXmodem(var P : ProtocolRecPtr;
                     PortPtr : PortRecPtr;
                     Use1K, UseGMode : Boolean); Export;
procedure InitCustomXmodem(var P : ProtocolRecPtr;
                           PortPtr : PortRecPtr;
                           Use1K, UseGMode : Boolean;
                           Options : Word); Export;
procedure DoneXmodem(var P : ProtocolRecPtr); Export;
procedure Set1KMode(P : ProtocolRecPtr; Enable : Boolean); Export;
procedure SetGMode(P : ProtocolRecPtr; Enable : Boolean); Export;
procedure SetBlockWait(P : ProtocolRecPtr; NewBlockWait : Byte); Export;
procedure SetFinishWaitXM(P : ProtocolRecPtr; NewFinishWait : Word); Export;
{#F+}
procedure SendHandshakeChar(P : ProtocolRecPtr; Handshake : Char); Export;
{#F-}
procedure PrepareTransmitPartXM(P : ProtocolRecPtr); Export;
function ProtocolTransmitPartXM(P : ProtocolRecPtr) : ProtocolStateType; Export;
procedure ProtocolTransmitXM(P : ProtocolRecPtr); Export;
procedure PrepareReceivePartXM(P : ProtocolRecPtr); Export;
function ProtocolReceivePartXM(P : ProtocolRecPtr) : ProtocolStateType; Export;
procedure ProtocolReceiveXM(P : ProtocolRecPtr); Export;

{$ELSE}

procedure InitXmodem(var P : ProtocolRecPtr;
                     PortPtr : PortRecPtr;
                     Use1K, UseGMode : Boolean);
procedure InitCustomXmodem(var P : ProtocolRecPtr;
                           PortPtr : PortRecPtr;
                           Use1K, UseGMode : Boolean;
                           Options : Word);
procedure DoneXmodem(var P : ProtocolRecPtr);
procedure Set1KMode(P : ProtocolRecPtr; Enable : Boolean);
procedure SetGMode(P : ProtocolRecPtr; Enable : Boolean);
procedure SetBlockWait(P : ProtocolRecPtr; NewBlockWait : Byte);
procedure SetFinishWaitXM(P : ProtocolRecPtr; NewFinishWait : Word);
{#F+}
procedure SendHandshakeChar(P : ProtocolRecPtr; Handshake : Char);
{#F-}
procedure PrepareTransmitPartXM(P : ProtocolRecPtr);
function ProtocolTransmitPartXM(P : ProtocolRecPtr) : ProtocolStateType;
procedure ProtocolTransmitXM(P : ProtocolRecPtr);
procedure PrepareReceivePartXM(P : ProtocolRecPtr);
function ProtocolReceivePartXM(P : ProtocolRecPtr) : ProtocolStateType;
procedure ProtocolReceiveXM(P : ProtocolRecPtr);

{$ENDIF}

procedure TransmitBlock(P : ProtocolRecPtr;
                        var Block : DataBlockType;
                        BLen : Word; BType : Char);
function GetHandshakeChar(P : ProtocolRecPtr) : Char;
procedure ReceiveBlock(P : ProtocolRecPtr;
                       var Block : DataBlockType;
                       var BlockSize : Word;
                       var HandShake : Char);
procedure Cancel(P : ProtocolRecPtr);
function PrepHandshake(P : ProtocolRecPtr) : Boolean;
function ProcessHandshake(P : ProtocolRecPtr) : Boolean;
function ProcessBlockReply(P : ProtocolRecPtr) : Boolean;
function HandleAbort(P : ProtocolRecPtr) : Boolean;
function CheckForBlockStart(P : ProtocolRecPtr; var C : Char) : Boolean;

implementation

const
  GReq = 'G';                          {G mode request}

type
  Bytes = array[1..1024] of Byte;      {Maximum buffer supported}

  procedure InitXmodem(var P : ProtocolRecPtr;
                       PortPtr : PortRecPtr;
                       Use1K, UseGMode : Boolean);
    {-Allocates and initializes and Xmodem control block}
  begin
    InitCustomXmodem(P, PortPtr, Use1K, UseGMode, DefProtocolOptions);
  end;

  procedure InitCustomXmodem(var P : ProtocolRecPtr;
                             PortPtr : PortRecPtr;
                             Use1K, UseGMode : Boolean;
                             Options : Word);
    {-Allocates and initializes a protocol control block}
  var
    XM : XmodemPtr absolute P;
    Temp : ProtocolDataPtr;
  begin
    AsyncStatus := ecOk;
    P := nil;

    {Allocate the protocol data record}
    InitProtocolData(Temp, PortPtr, Options);
    if AsyncStatus <> ecOk then
      Exit;

    {Allocate the xmodem record}
    if not GetMemCheck(XM, SizeOf(XModemProtocol)) then begin
      DoneProtocolData(Temp);
      GotError(PortPtr, epFatal+ecOutOfMemory);
      Exit;
    end;

    {!!.01 moved from state machine}
    {Get a protocol DataBlock}
    if not GetMemCheck(XM^.DataBlock, SizeOf(DataBlockType)+5) then begin
      DoneProtocolData(Temp);
      FreeMemCheck(XM, SizeOf(XmodemProtocol));
      Exit;
    end;

    XM^.PData := Temp;

    with XM^, PData^ do begin
      {Set block mode}
      OneKMode := Use1K;

      {Miscellaneous inits}
      BlockWait := DefBlockWait;
      MaxBlockErrors := DefMaxBlockErrors;
      FirstBlockNum := 1;
      Overhead := XmodemOverhead;
      TurnDelay := XmodemTurnDelay;
      FinishWait := DefFinishWait;

      {Set streaming mode}
      GMode := UseGMode;
      if GMode then begin
        EotCheckCount := 0;
        OneKMode := True;
        TurnDelay := 0;
      end;

      {Assume crc mode}
      CheckType := bcCrc16;

      {Set the initial block size (and protocol type)}
      ProtType := XmodemCRC;
      Set1KMode(P, OneKMode);
    end;
  end;

  procedure DoneXmodem(var P : ProtocolRecPtr);
    {-Disposes of P}
  begin
    DoneProtocolData(P^.PData);
    FreeMemCheck(XModemPtr(P)^.DataBlock, SizeOf(DataBlockType)+5);
    FreeMemCheck(P, SizeOf(XmodemProtocol));
  end;

  procedure Set1KMode(P : ProtocolRecPtr; Enable : Boolean);
    {-Enable/disable Xmodem1K}
  const
    GType : array[Boolean] of Byte = (YmodemG, Xmodem1KG);
    KType : array[Boolean] of Byte = (Ymodem, Xmodem1K);
    XType : array[Boolean] of Byte = (Ymodem, Xmodem);
  var
    XM : XmodemPtr absolute P;
    WasXmodem : Boolean;
  begin
    with XM^, PData^ do begin
      {Note the original protocol type}
      case ProtType of
        Xmodem,Xmodem1K,Xmodem1KG,XmodemCRC : WasXmodem := True;
        else WasXmodem := False;
      end;

      OneKMode := Enable;
      if OneKMode then begin
        BlockLen := 1024;
        StartChar := cStx;
        if GMode then
          ProtType := GType[WasXmodem]
        else
          ProtType := KType[WasXmodem];
      end else begin
        BlockLen := 128;
        StartChar := cSoh;
        ProtType := XType[WasXmodem];
      end;
    end;
  end;

  procedure SetGMode(P : ProtocolRecPtr; Enable : Boolean);
    {-Enable/disable streaming}
  var
    XM : XmodemPtr absolute P;
  begin
    with XM^, PData^ do begin
      GMode := Enable;
      if GMode then begin
        Set1KMode(P, True);
        TurnDelay := 0;
        EotCheckCount := 0;
      end else begin
        MaxBlockErrors := DefMaxBlockErrors;
        TurnDelay := XmodemTurnDelay;
        EotCheckCount := 1;
      end;
    end;
  end;

  procedure SetBlockWait(P : ProtocolRecPtr; NewBlockWait : Byte);
    {-Set inter-block wait time}
  var
    XM : XmodemPtr absolute P;
  begin
    XM^.BlockWait := NewBlockWait;
  end;

  procedure SetFinishWaitXM(P : ProtocolRecPtr; NewFinishWait : Word);
    {-Set additional finish wait (time to wait for EOT response)}
  var
    XM : XmodemPtr absolute P;
  begin
    XM^.FinishWait := NewFinishWait;
  end;

  function GetHandshakeChar(P : ProtocolRecPtr) : Char;
    {-Returns proper handshake character}
  var
    XM : XmodemPtr absolute P;
  begin
    with XM^, PData^ do
      if Gmode then
        GetHandshakeChar := GReq
      else if CheckType = bcCrc16 then
        GetHandshakeChar := CrcReq
      else
        GetHandshakeChar := cNak;
  end;

  procedure Cancel(P : ProtocolRecPtr);
    {-Sends cancel request to remote}
  var
    ET : EventTimer;
    Tics : Word;
    C : Char;
    SaveStatus : Word;
  begin
    with P^.PData^ do begin
      SaveStatus := AsyncStatus;

      {Flush anything that might be left in the output buffer}
      FlushOutBuffer(PR);

      {Cancel with three cancel (^X) chars}
      PutChar(PR, cCan);
      PutChar(PR, cCan);
      PutChar(PR, cCan);

      {Wait until all three cCan's have been transmitted}
      NewTimer(ET, CancelTimeout);
      while (AsyncStatus = ecOk) and
            (OutBuffUsed(PR) <> 0) and
            (not WaitComplete(PR, ET)) do ;

      {Discard remaining characters}
      If ActCPS > 0 Then Tics := (BlockLen div ActCPS) * 18 Else Tics := 0;
      if Tics = 0 then Tics := 9;
      NewTimer(ET, Tics);

      repeat
        if CharReady(PR) then
          GetChar(PR, C);
      until TimerExpired(ET);
      if SaveStatus = ecOk then
        AsyncStatus := ecCancelRequested
      else
        AsyncStatus := SaveStatus;
    end;
  end;

  procedure AbortProtocol(P : ProtocolRecPtr);
    {-Calls Cancel, calls GotError}
  var
    SaveStatus : Word;
  begin
    SaveStatus := AsyncStatus;
    Cancel(P);
    GotError(P^.PData^.PR, epFatal+ecCancelRequested);
    ShowLastStatus(P);
    if SaveStatus <> ecUserAbort then
      AsyncStatus := SaveStatus;
  end;

  function HandleAbort(P : ProtocolRecPtr) : Boolean;
    {-Calls user abort function, aborts protocol and returns True if aborting}
  begin
    with P^. PData^ do begin
      if PR^.UserAbort then begin
        AsyncStatus := ecUserAbort;
        AbortProtocol(P);
        HandleAbort := True;
      end else
        HandleAbort := False;
    end;
  end;

  function ProcessHandshake(P : ProtocolRecPtr) : Boolean;
    {-Process initial handshake, return true if OK}
  var
    XM : XmodemPtr absolute P;
    C : Char;
  begin
    with XM^, PData^ do begin
      {If we get here we know a character is waiting}
      GetChar(PR, C);
      if AsyncStatus <> ecOk then begin
        {Line error or timeout}
        Inc(BlockErrors);
        Inc(TotalErrors);
      end else
        case C of
          cCan : {Remote requested a cancel}
            GotError(PR, epFatal+ecInitCancel);
          cNak : {Set checksum mode}
            CheckType := bcChecksum1;
          CrcReq : {Set CRC mode (and possibly reset the protocol type)}
            begin
              CheckType := bcCrc16;
              if ProtType = Xmodem then
                ProtType := XmodemCRC;
            end;
          GReq : {Set G mode (streaming mode)}
            begin
              GMode := True;
              CheckType := bcCrc16;
            end;
          else {Unexpected character}
            Inc(BlockErrors);
            Inc(TotalErrors);
            AsyncStatus := ecUnexpectedChar;
        end;

      ProcessHandshake := AsyncStatus = ecOk;
    end;
  end;

  function ProcessBlockReply(P : ProtocolRecPtr) : Boolean;
    {-Process reply to last block; return True if OK}
  var
    XM : XmodemPtr absolute P;
    C : Char;
  begin
    with XM^, PData^ do begin
      {Handle GMode (all blocks are assumed to succeed)}
      if GMode then begin
        AsyncStatus := ecOk;
        Inc(BytesTransferred, BlockLen);
        Dec(BytesRemaining, BlockLen);
        if BytesRemaining < 0 then
          BytesRemaining := 0;
        ElapsedTics := ElapsedTime(Timer);
        Inc(BlockNum);
        Inc(FileOfs, BlockLen);

        {Check for cancel from remote}
        if CharReady(PR) then begin
          GetChar(PR, C);
          if C = cCan then
            GotError(PR, epFatal+ecCancelRequested);
        end;
        ProcessBlockReply := AsyncStatus = ecOk;
        Exit;
      end;

      {We know a character is ready}
      GetChar(PR, C);
      if C <> cCan then
        CanCounter := 0;

      {Process the reply}
      if AsyncStatus = ecOk then
        case C of
          cAck : {Adjust buffer pos (in case we switched block sizes)}
            begin
              Dec(CharsLeft, BlockLen);
              Inc(OutBufPos, BlockLen);
              Inc(BytesTransferred, BlockLen);
              Dec(BytesRemaining, BlockLen);
              if BytesRemaining < 0 then
                BytesRemaining := 0;
              ElapsedTics := ElapsedTime(Timer);
              Inc(BlockNum);
              Inc(FileOfs, BlockLen);
            end;
          cCan : {Cancel requested by remote}
            begin
              Inc(CanCounter);
              if CanCounter >= 2 then
                GotError(PR, epFatal+ecCancelRequested);
            end;
          else {Unexpected character or Nak}
            Inc(BlockErrors);
            Inc(TotalErrors);
            GotError(PR, epNonFatal+ecUnexpectedChar);
        end
      else begin
        {Reply timeout}
        Inc(BlockErrors);
        Inc(TotalErrors);
      end;
      ProcessBlockReply := AsyncStatus = ecOk;
    end;
  end;

  procedure TransmitBlock(P : ProtocolRecPtr;
                          var Block : DataBlockType;
                          BLen : Word; BType : Char);
    {-Transmits one data block}
  var
    XM           : XmodemPtr absolute P;
    I            : Integer;
    INum         : Byte;
    BytesWritten : Word;
    B            : ^Bytes;

  begin
    with XM^, PData^ do
    begin
      if BlockErrors > MaxBlockErrors then
        {Too many errors}
        if OneKMode and (BlockLen = 1024) then begin
          {1KMode - reduce the block size and try again}
          BlockLen := 128;
          StartChar := cSoh;
          BlockErrors := 0;
        end else begin
          {Std Xmodem - have to cancel}
          Cancel(P);
          GotError(PR, epFatal+ecTooManyErrors);
          Exit;
        end;

      {Send the StartBlock char, the block sequence and its compliment}
      PutChar(PR, StartChar);
      INum := Lo(BlockNum);
      PutChar(PR, Char(INum));
      PutChar(PR, Char(not INum));

      {Init the BlockCheck value}
      BlockCheck := 0;

      {Send the data on its way}

      {$IFNDEF OS2}
      BytesWritten := PutBlock (PR, Bytes (Block) [OutBufPos], BlockLen);
      {$ELSE}
      GetMem (B, 1024);
      Move (Bytes (Block) [OutBufPos], B^, BlockLen);
      os2PutBlockDirect (PChar (B), BlockLen);
      FreeMem (B, 1024);
      BytesWritten := BlockLen;
      {$ENDIF}

      {Calculate the check character}
      if CheckType = bcCrc16 then
        for I := 1 to BlockLen do
          BlockCheck := UpdateCrc(Bytes(Block)[OutBufPos + (I-1)], BlockCheck)
      else
        for I := 1 to BlockLen do
          BlockCheck := UpdateCheckSum(Bytes(Block)[OutBufPos + (I-1)], BlockCheck);

      {Send the check character}
      if CheckType = bcCrc16 then begin
        BlockCheck := UpdateCrc(0, BlockCheck);
        BlockCheck := UpdateCrc(0, BlockCheck);
        PutChar(PR, Char(Hi(BlockCheck)));
        PutChar(PR, Char(Lo(BlockCheck)));
      end else
        PutChar(PR, Char(BlockCheck));
    end;
  end;

  procedure TransmitEot(P : ProtocolRecPtr; First : Boolean);
    {-Transmit an Xmodem EOT (end of transfer)}
  var
    XM : XmodemPtr absolute P;
  begin
    AsyncStatus := ecOk;

    with XM^, PData^ do begin
      if First then begin
        BlockErrors := 0;
        NaksReceived := 0;
        Dec(BlockNum);
      end;

      {Send the Eot char}
      PutChar(PR, cEot);
    end;
  end;

  function ProcessEotReply(P : ProtocolRecPtr) : Boolean;
    {-Get a response to an EOT}
  var
    XM : XmodemPtr absolute P;
    C  : Char;
  begin
    with XM^, PData^ do begin
      {Get the response}
      GetChar(PR, C);
      if AsyncStatus = ecOk then
        case C of
          cAck : {Receiver acknowledged Eot, this transfer is over}
            begin
              ProcessEotReply := True;
              AsyncStatus := ecEndFile;
            end;
          cCan : {Receiver asked to cancel, this transfer is over}
            begin
              ProcessEotReply := True;
              AsyncStatus := ecCancelRequested;
            end;
          cNak : {Some Xmodems always NAK the first 1 or 2 EOTs}
                 {So, don't count them as errors till we get 3 }
            begin
              ProcessEotReply := False;
              Inc(NaksReceived);
              If NaksReceived >= 3 then begin
                Inc(BlockErrors);
                Inc(TotalErrors);
              end;
            end;
          else {Unexpected character received}
            ProcessEotReply := False;
            Inc(BlockErrors);
            Inc(TotalErrors);
        end
      else begin
        {Line error}
        ProcessEotReply := False;
        Inc(BlockErrors);
        Inc(TotalErrors)
      end;
    end;
  end;

  procedure SendHandshakeChar(P : ProtocolRecPtr; Handshake : Char);
    {-Send the current handshake char}
  var
    XM : XmodemPtr absolute P;
  begin
    with XM^, PData^ do
      if not GMode or (Handshake <> cAck) then
        PutChar(PR, Handshake);
  end;

  function CheckForBlockStart(P : ProtocolRecPtr; var C : Char) : Boolean;
    {-Scan input buffer for start char, return True if found}
  var
    XM : XmodemPtr absolute P;
  begin
    AsyncStatus := ecOk;
    CheckForBlockStart := False;

    with XM^, PData^ do begin
      {Ready to scan...}
      BlockErrors := 0;

      {Start scanning}
      while CharReady(PR) do begin

        {Check the next character}
        GetChar(PR, C);
        if AsyncStatus = ecOk then begin
          case C of
            cSoh,
            cStx,
            cEot,
            cCan : begin
                     CheckForBlockStart := True;
                     Exit;
                   end;
            else begin
              GotError(PR, epNonFatal+ecUnexpectedChar);
              EotCounter := 0;
              CanCounter := 0;
            end;
          end;
        end;
      end;
    end;
  end;

  procedure ReceiveBlock(P : ProtocolRecPtr;
                         var Block : DataBlockType;
                         var BlockSize : Word;
                         var HandShake : Char);
    {-Receive on Xmodem record into Buffer}
  var
    XM : XmodemPtr absolute P;
    R1, R2 : Byte;
    GotLen : Word;
    I : Word;
    CrcHi : Byte;
    CrcLo : Byte;
  begin
    AsyncStatus := ecOk;

    with XM^, PData^ do begin
      {Assume an error}
      Handshake := cNak;

      {Get and compare block sequence numbers}
      R1 := Byte(Block[1]);
      R2 := Byte(Block[2]);
      if (not R1) <> R2 then begin
        Inc(BlockErrors);
        Inc(TotalErrors);
        FlushInBuffer(PR);
        Cancel(P);
        GotError(PR, epFatal+ecSequenceError);
        Exit;
      end;

      {Init checkchar accumulators}
      BlockCheck := 0;
      for I := 3 to BlockLen+2 do
        if CheckType = bcCrc16 then
          BlockCheck := UpdateCrc(Byte(Block[I]), BlockCheck)
        else
          BlockCheck := UpdateCheckSum(Byte(Block[I]), BlockCheck);

      {Check the block-check character}
      if CheckType = bcCrc16 then begin
        BlockCheck := UpdateCrc($00, BlockCheck);
        BlockCheck := UpdateCrc($00, BlockCheck);
        CrcHi := Byte(Block[BlockLen+3]);
        CrcLo := Byte(Block[BlockLen+4]);
      end else
        CrcLo := Byte(Block[BlockLen+3]);
      if CheckType = bcCrc16 then
        if (CrcHi <> Hi(BlockCheck)) or (CrcLo <> Lo(BlockCheck)) then begin
          {Block check error}
          Inc(BlockErrors);
          Inc(TotalErrors);
          FlushInBuffer(PR);
          GotError(PR, epNonFatal+ecBlockCheckError);
          Exit;
        end else
      else
        if CrcLo <> Lo(BlockCheck) then begin
          {Block check error}
          Inc(BlockErrors);
          Inc(TotalErrors);
          FlushInBuffer(PR);
          GotError(PR, epNonFatal+ecBlockCheckError);
          Exit;
        end;

      {Check the block sequence for missing or duplicate blocks}
      if R1 = Lo(BlockNum-1) then begin
        {This is a duplicate block}
        HandShake := cAck;
        BlockErrors := 0;
        GotError(PR, epNonFatal+ecDuplicateBlock);
        Exit;
      end;
      if R1 <> Lo(BlockNum) then begin
        {Its a sequence error}
        Cancel(P);
        GotError(PR, epFatal+ecSequenceError);
        Exit;
      end;

      {Block is ok, do ACK on next call to ReceiveBlock}
      Handshake := cAck;
      AsyncStatus := ecOk;
      Move(Block[3], Block[1], BlockLen);

      {Update status fields for the next call to the user status routine}
      Inc(BlockNum);
      Inc(BytesTransferred, BlockLen);
      Dec(BytesRemaining, BlockLen);
      if BytesRemaining < 0 then
        BytesRemaining := 0;
      ElapsedTics := ElapsedTime(Timer);
      BlockErrors := 0;
      BlockSize := BlockLen;
    end;
  end;

  function PrepHandshake(P : ProtocolRecPtr) : Boolean;
    {-Set up to wait for a handshake char}
  var
    XM : XmodemPtr absolute P;
  begin
    with XM^, PData^ do begin
      NewTimer(ReplyTimer, HandshakeWait);
      Inc(HandshakeAttempt);
      if HandshakeAttempt > HandshakeRetry then begin
        PrepHandshake := False;
        GotError(PR, ecTimeout);
      end else
        PrepHandshake := True;
    end;
  end;

  procedure PrepareTransmitPartXM(P : ProtocolRecPtr);
    {-Prepare for calling ProtocolTransmitXMPart}
  var
    XM : XmodemPtr absolute P;
  begin
    with XM^, PData^ do begin
      FindingFirst := True;
      PR^.ProtocolActive := True;

      ResetStatus(P);
      CheckType := bcChecksum1;

      ShowFirstStatus(P);
      if not NextFile(P, Pathname) then begin
        {AsyncStatus already set}
        ShowLastStatus(P);
        PR^.ProtocolActive := False;
        Exit;
      end;

      ForceStatus := True;
      XmodemState := txInitial;
      {DataBlock := nil;}                                              {!!.01}
      AsyncStatus := ecOk;
    end;
  end;

  function ProtocolTransmitPartXM(P : ProtocolRecPtr) : ProtocolStateType;
    {-Starts Xmodem protocol transmit}
  label
    ExitPoint;
  var
    XM : XmodemPtr absolute P;
    C : Char;
  begin
    with XM^, PData^ do begin

      {Check for user abort}
      if HandleAbort(P) then
        XmodemState := txFinished
      else if TimerExpired(StatusTimer) or ForceStatus then begin
        {Show status periodically}
        ForceStatus := False;
        NewTimer(StatusTimer, StatusInterval);
        apUserStatus(P, False, False);
      end;

      {Process current state}
      case XmodemState of
        txInitial :
          begin
            {Reset status vars}
            ResetStatus(P);
            NoMoreData := False;

            {!!.01 moved to init}
            {Get a protocol DataBlock}
            {if not GetMemCheck(DataBlock, SizeOf(DataBlockType)) then begin
              GotError(PR, epFatal+ecOutOfMemory);
              ShowLastStatus(P);
              ProtocolTransmitPartXM := psFinished;
              PR^.ProtocolActive := False;
              Exit;
            end;}

            {Pathname must already be set before we get here}
            Pathname := UpString(Pathname);

            {Show file name to user logging routine}
            LogFile(P, lfTransmitStart);

            {Go prepare for reading protocol blocks}
            PrepareReading(P);
            if AsyncStatus <> ecOk then begin
              XmodemState := txFinished;
              goto ExitPoint;
            end;

            {Set the first block number}
            BlockNum := FirstBlockNum;

            {Clear possible garbage (but allow case for already receieved handshake)}
            if InBuffUsed(PR) > 1 then
              FlushInBuffer(PR);

            {Check for handshake character}
            XmodemState := txHandshake;
            HandshakeAttempt := 0;
            if not PrepHandshake(P) then
              XmodemState := txFinished;
          end;

        txHandshake :
          begin
            if CharReady(PR) then
              if ProcessHandshake(P) then begin
                {Start protocol timer now}
                NewTimer(Timer, 1);
                {Use reply timer to wait for buffer freespace}
                NewTimer(ReplyTimer, TransTimeout);
                XmodemState := txGetBlock;
                FileOfs := 0;
                if GMode then
                  MaxBlockErrors := 0;
              end else begin
                if AsyncStatus = ecInitCancel then
                  XmodemState := txFinished
                else if not PrepHandshake(P) then
                  XmodemState := txFinished
               end
            else
              if TimerExpired(ReplyTimer) then
                if not PrepHandshake(P) then
                  XmodemState := txFinished
          end;

        txGetBlock :
          begin
            LastBlockSize := BlockLen;
            CharsLeft := BlockLen;
            OutBufPos := 1;
            BlockErrors := 0;
            NoMoreData := ReadProtocolBlock(P, DataBlock^, LastBlockSize);
            if AsyncStatus = ecOk then begin
              XmodemState := txWaitFreespace;
              NewTimer(ReplyTimer, TransTimeout);
            end else
              XmodemState := txFinished;
          end;

        txWaitFreeSpace :
          begin
            if OutBuffFree(PR) > BlockLen+5 then
              XmodemState := txSendBlock
            else if TimerExpired(ReplyTimer) then begin
              {Must be buffer full error}
              GotError(PR, epFatal+ecBufferIsFull);
              XmodemState := txFinished;
            end;
          end;

        txSendBlock :
          begin
            {Don't send empty blocks (will only happen with empty files)}
            if LastBlockSize <= 0 then
              XmodemState := txFirstEndOfTransmit
            else begin
              {If no errors, then send this block to the remote}
              if AsyncStatus = ecOk then begin
                TransmitBlock(P, DataBlock^, BlockLen, ' ');

                {If TransmitBlock failed, go clean up}
                if AsyncStatus <> ecOk then begin
                  FlushOutBuffer(PR);
                  XmodemState := txFinished;
                end;

                {Prepare to handle reply}
                if GMode then
                  {Go process psuedo reply}
                  XmodemState := txReplyPending
                else begin
                  {Wait for output buffer to drain}
                  XmodemState := txDraining;
                  NewTimer(ReplyTimer, DrainWait);
                end;

                {Force a status update}
                ForceStatus := True;
              end;
            end;
          end;

        txDraining :
          begin
            if (OutBuffUsed(PR) <= 1) or
               TimerExpired(ReplyTimer) then begin
              NewTimer(ReplyTimer, BlockWait);
              XmodemState := txReplyPending;
            end;
          end;

        txReplyPending :
          begin
            if CharReady(PR) or GMode then
            begin
              if ProcessBlockReply(P) then
                {Got reply, go send next block}
                if NoMoreData then begin
                  XmodemState := txEndDrain;
                  If ActCPS > 0
                  Then NewTimer (ReplyTimer, Secs2Tics ((PR^.OutBuffLen div ActCPS)) * 2)
                  Else NewTimer (ReplyTimer, 9);
                end else
                  XmodemState := txGetBlock
              else
                if AsyncStatus = ecCancelRequested then begin
                  {Got two cancels, we're finished}
                  FlushOutBuffer(PR);
                  XmodemState := txFinished;
                end else
                  if CanCounter > 0 then
                    {Just got a cancel, look for another}
                    XmodemState := txReplyPending
                  else begin
                    {Got junk for a response, go send block again}
                    XmodemState := txWaitFreespace;
                    AsyncStatus := ecOk;
                  end;
            end else
              if TimerExpired(ReplyTimer) then begin
                XmodemState := txSendBlock;
                Inc(BlockErrors);
                Inc(TotalErrors);
              end;
          end;

        txEndDrain:
          begin
            if (OutBuffUsed(PR) <= 1) or
                TimerExpired(ReplyTimer) then
              XmodemState := txFirstEndOfTransmit;
          end;

        txFirstEndOfTransmit :
          begin
            TransmitEot(P, True);
            if GMode then
              NewTimer(ReplyTimer, BlockWait+FinishWait)
            else
              NewTimer(ReplyTimer, BlockWait);
            XmodemState := txEotReply;
          end;

        txRestEndOfTransmit :
          begin
            TransmitEot(P, False);
            NewTimer(ReplyTimer, BlockWait);
            if BlockErrors <= MaxBlockErrors then
              XmodemState := txEotReply
            else begin
              XmodemState := txFinished;
              GotError(PR, epFatal+ecTooManyErrors);
            end;
          end;

        txEotReply :
          begin
            if CharReady(PR) then
              if ProcessEotReply(P) then
                XmodemState := txFinished
              else
                XmodemState := txRestEndOfTransmit
            else
              if TimerExpired(ReplyTimer) then
                XmodemState := txRestEndOfTransmit;
          end;

        txFinished :
          begin
            if AsyncStatus = ecCancelRequested then
              FlushInBuffer(PR);

            {Close the file (or whatever was giving us blocks)}
            FinishReading(P);

            {Show status, user logging, and clean up}
            if AsyncStatus = ecEndFile then begin
              AsyncStatus := ecOk;
              LogFile(P, lfTransmitOk)
            end else
              LogFile(P, lfTransmitFail);
            ShowLastStatus(P);
            {FreeMemCheck(DataBlock, SizeOf(DataBlockType));}          {!!.01}
            XmodemState := txDone;
            PR^.ProtocolActive := False;
          end;
      end;

ExitPoint:
      {Set function result}
      case XmodemState of
        txInitial,
        txGetBlock,
        txSendBlock,
        txFirstEndOfTransmit,
        txRestEndOfTransmit,
        txFinished             : ProtocolTransmitPartXM := psReady;

        txEndDrain,
        txHandshake,
        txWaitFreeSpace,
        txDraining,
        txReplyPending,
        txEotReply             : ProtocolTransmitPartXM := psWaiting;

        txDone                 : ProtocolTransmitPartXM := psFinished;
      end;
    end;
  end;

  procedure ProtocolTransmitXM(P : ProtocolRecPtr);
    {-Starts Xmodem protocol transmit}
  var
    XM : XmodemPtr absolute P;
    State : ProtocolStateType;
  begin
    with XM^, PData^ do begin
      PrepareTransmitPartXM(P);
      if AsyncStatus <> ecOk then
        Exit;

      XmodemState := txInitial;
      repeat
        State := ProtocolTransmitPartXM(P);
        if State = psWaiting then
          UserBack(P);
      until State = psFinished;
    end;
  end;

  procedure PrepareReceivePartXM(P : ProtocolRecPtr);
    {-Prepare for calling ProtocolReceiveXMPart}
  var
    XM : XmodemPtr absolute P;
  begin
    with XM^, PData^ do begin
      {Prepare}
      PR^.ProtocolActive := True;
      GotOneFile := False;
      XmodemState := rxInitial;
      ForceStatus := True;
      {DataBlock := nil;}                                              {!!.01}
      AsyncStatus := ecOk;
      ResetStatus(P);
    end;
  end;

  function ProtocolReceivePartXM(P : ProtocolRecPtr) : ProtocolStateType;
    {-Starts Xmodem protocol receive}
  label
    ExitPoint;
  var
    XM : XmodemPtr absolute P;
    C : Char;
    BlockSize : Word;
    Error : Boolean;
    RcvStatus : Word;
    {Cnt : Word;}                                                      {!!.03}

    procedure Cleanup(DisposeBuffers : Boolean);
      {-Handle error reporting and other cleanup}
    begin
      with XM^, PData^ do begin
        {if DisposeBuffers then
          FreeMemCheck(DataBlock, SizeOf(DataBlockType)+5);}           {!!.01}

        ShowLastStatus(P);
        XmodemState := rxDone;
        PR^.ProtocolActive := False;
      end;
    end;

    function CheckErrors : Boolean;
      {-Increment block errors, return True if too many}
    begin
      with XM^, PData^ do begin
        Inc(BlockErrors);
        Inc(TotalErrors);
        {CheckErrors := BlockErrors > MaxBlockErrors;}
        if BlockErrors > MaxBlockErrors then begin
          CheckErrors := True;
          GotError(XM^.PData^.PR, ecTimeout);
        end else
          CheckErrors := False;
      end;
    end;

  begin

    with XM^, PData^ do begin
      {Check for user abort}
      if HandleAbort(P) then
        XmodemState := rxFinished;

      {Process current state}
      case XmodemState of
        rxInitial :
          begin
            {Show (possible) first status}
            AsyncStatus := ecHandshakeInProgress;
            ShowFirstStatus(P);

            {!!.01 moved to init}
            {Get a protocol DataBlock}
            {if not GetMemCheck(DataBlock, SizeOf(DataBlockType)+5) then begin
              GotError(PR, epFatal+ecOutOfMemory);
              Cleanup(False);
              goto ExitPoint;
            end;}

            {Pathname should already have name of file to receive}
            if Pathname = '' then begin
              GotError(PR, epFatal+ecNoFilename);
              Cleanup(True);
              goto ExitPoint;
            end else begin
              {Merge in destdir if not already done}
              if (JustPathname(Pathname) = '') and (DestDir <> '') then
                Pathname := AddBackslash(DestDir)+Pathname;
              PathName := ValidatePathname (UpString (PathName));
            end;

            {Send file name to user's LogFile procedure}
            LogFile(P, lfReceiveStart);

            {Accept this file}
            if not AcceptFile(P) then begin
              Cancel(P);
              GotError(PR, epNonFatal+ecFileRejected);
              XmodemState := rxFinishedSkip;
              goto ExitPoint;
            end;

            {Prepare file for writing protocol blocks}
            PrepareWriting(P);
            if AsyncStatus <> ecOk then begin
              Cancel(P);
              XmodemState := rxFinished;
              goto ExitPoint;
            end;

            {Start sending handshakes}
            HandshakeChar := GetHandshakeChar(P);
            FileOfs := 0;
            XmodemState := rxWaitForHSReply;
            SendHandshakeChar(P, HandshakeChar);
            BlockNum := 1;
            EotCounter := 0;
            CanCounter := 0;

            NewTimer(ReplyTimer, HandshakeWait);
            TimerPending := True;

            {Set overhead length based on check type}
            if CheckType = bcCrc16 then
              OverheadLen := 4
            else
              OverheadLen := 3;
          end;

        rxWaitForHSReply :
          begin
            if CharReady(PR) then
              XmodemState := rxWaitForBlockStart
            else if TimerExpired(ReplyTimer) then
              if CheckErrors then
                XmodemState := rxFinished
              else begin
                if (HandshakeChar = CrcReq) and (BlockErrors > MaxCrcTry) then begin
                  {Step down to Xmodem checksum}
                  HandshakeChar := cNak;
                  BlockErrors := 0;
                  Dec(OverheadLen);
                  CheckType := bcChecksum1;
                end;
                PutChar(PR, HandshakeChar);
                NewTimer(ReplyTimer, HandshakeWait);
              end;
          end;

        rxWaitForBlockStart :
          begin
            if CharReady(PR) then begin
              if TimerPending then begin
                NewTimer(Timer, 0);
                TimerPending := False;
                if GMode then
                  MaxBlockErrors := 0;
              end;
              if CheckForBlockStart(P, C) then begin
                case C of
                  cSoh,
                  cStx :
                    begin
                      if C = cSoh then
                        BlockLen := 128
                      else
                        BlockLen := 1024;
                      XmodemState := rxCollectBlock;
                      NewTimer(ReplyTimer, BlockWait);
                      BlkIndex := 0;
                    end;
                  cCan :
                    begin
                      EotCounter := 0;
                      Inc(CanCounter);
                      if CanCounter > 2 then begin
                        Cancel(P);
                        XmodemState := rxFinished;
                      end;
                    end;
                  cEot :
                    begin
                      CanCounter := 0;
                      Inc(EotCounter);
                      if EotCounter > EotCheckCount then begin
                        PutChar(PR, cAck);
                        XmodemState := rxFinished;
                        AsyncStatus := ecEndFile;
                      end else begin
                        PutChar(PR, cNak);
                        XmodemState := rxWaitForBlockStart;
                        NewTimer(ReplyTimer, BlockWait);
                      end;
                    end;
                end;
              end else begin
                {Line error or junk received, discard and send Nak}
                if not CheckErrors then begin                          {!!.02}
                  FlushInBuffer(PR);
                  PutChar(PR, cNak);
                end else                                               {!!.02}
                  XmodemState := rxFinished;                           {!!.02}
              end;
            end else begin
              {No chars yet, check timeout}
              if TimerExpired(ReplyTimer) then begin
                {Finished if too many timeouts or timeout during eot or can}
                if EotCounter <> 0 then begin
                  {Timed out waiting for second Eot, end normally}
                  PutChar(PR, cAck);
                  XmodemState := rxFinished;
                  AsyncStatus := ecEndFile;
                end else if CheckErrors or (CanCounter <> 0) then
                  XmodemState := rxFinished
                else begin
                  {Simple timeout, resend handshake}
                  XmodemState := rxWaitForHSReply;
                  SendHandshakeChar(P, HandshakeChar);
                  NewTimer(ReplyTimer, BlockWait);
                end;
              end;
            end;
          end;

        rxCollectBlock :
          begin
            {Cnt := 1;}                                                {!!.03}
            while CharReady(PR) and
                  {(Cnt < 10) and}                                     {!!.03}
                  (BlkIndex < BlockLen + OverheadLen) do begin
              GetChar(PR, C);
              Inc(BlkIndex);
              {Inc(Cnt);}                                              {!!.03}
              DataBlock^[BlkIndex] := C;
              NewTimer(ReplyTimer, BlockWait);                         {!!.02}
            end;

            if BlkIndex >= BlockLen + OverheadLen then
              {Got a complete block, go process it}
              XmodemState := rxProcessBlock
            else if TimerExpired(ReplyTimer) then begin
              {Timeout out waiting for complete block, resend handshake}
              GotError(PR, epNonFatal+ecTimeout);
              PutChar(PR, cNak);
              XmodemState := rxWaitForBlockStart;
              NewTimer(ReplyTimer, BlockWait);
            end;
          end;

        rxProcessBlock :
          begin
            {Go process what's in DataBlock}
            ReceiveBlock(P, DataBlock^, BlockSize, HandshakeChar);
            RcvStatus := AsyncStatus;
            SendHandshakeChar(P, HandshakeChar);

            if RcvStatus = ecOk then begin
              {Got block ok, go write it out}
              Error := WriteProtocolBlock(P, DataBlock^, BlockSize);
              if AsyncStatus = ecOk then
                Inc(FileOfs, BlockSize)
              else begin
                {Failed to write the block, clean up and exit}
                Cancel(P);
                XmodemState := rxFinished;
                goto ExitPoint;
              end;

              {Normal received block -- keep going}
              XmodemState := rxWaitForBlockStart;
              NewTimer(ReplyTimer, BlockWait);
              ForceStatus := True;
            end else begin
              AsyncStatus := RcvStatus;
              if (AsyncStatus = ecSequenceError) or GMode then begin
                {Fatal error - just exit}
                if GMode then
                  Cancel(P);
                FlushOutBuffer(PR);
                XmodemState := rxFinished;
                goto ExitPoint;
              end else begin
                {Failed to get block, go try again (already sent Nak)}
                XmodemState := rxWaitForHSReply;
                ForceStatus := True;
                NewTimer(ReplyTimer, HandshakeWait);
                goto ExitPoint;
              end;
            end;
          end;

        rxFinishedSkip :
          begin
            FinishWriting(P);
            LogFile(P, lfReceiveSkip);
            Cleanup(True);
            goto ExitPoint;
          end;

        rxFinished :
          begin
            FinishWriting(P);
            if AsyncStatus = ecEndFile then begin
              AsyncStatus := ecOk;
              LogFile(P, lfReceiveOk);
            end else
              LogFile(P, lfReceiveFail);
            Cleanup(True);
            goto ExitPoint;
          end;
      end;

      {Show status periodically}
      if TimerExpired(StatusTimer) or ForceStatus then begin
        ForceStatus := False;
        NewTimer(StatusTimer, StatusInterval);
        apUserStatus(P, False, False);
      end;

ExitPoint:
      {Set function result}
      case XmodemState of
        rxInitial,
        rxFinishedSkip,
        rxProcessBlock,
        rxFinished           : ProtocolReceivePartXM := psReady;

        rxWaitForHSReply,
        rxWaitForBlockStart,
        rxCollectBlock       : ProtocolReceivePartXM := psWaiting;

        rxDone               : ProtocolReceivePartXM := psFinished;
      end;
    end;
  end;

  procedure ProtocolReceiveXM(P : ProtocolRecPtr);
    {-Starts Xmodem protocol receive}
  var
    XM : XmodemPtr absolute P;
    State : ProtocolStateType;
  begin
    with XM^, PData^ do begin
      {Prepare}
      PrepareReceivePartXM(P);
      if AsyncStatus <> ecOk then
        Exit;

      {Get the file, calling the user background procedure periodically}
      repeat
        State := ProtocolReceivePartXM(P);
        if State = psWaiting then
          UserBack(P);
      until State = psFinished;
    end;
  end;

end.
