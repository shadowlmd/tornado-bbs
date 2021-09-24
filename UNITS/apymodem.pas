{$I-,S-}
{$IFNDEF OS2}
{$R-,V-,B-,F+,O+,A-}
{$ENDIF}

{Conditional defines that may affect this unit}
{$I APDEFINE.INC}

{*********************************************************}
{*                 APYMODEM.PAS 2.03                     *}
{*     Copyright (c) TurboPower Software 1991.           *}
{* Portions copyright (c) Information Technology 1989,   *}
{*    and used under license to TurboPower Software      *}
{*                 All rights reserved.                  *}
{*********************************************************}

unit ApYmodem;
  {-Provides (true) Ymodem/G batch recieve and transmit}

interface

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
  ApAbsPcl,
  tMisc,
  OpInline,
  ApXmodem;

type
  {Ymodem protocol transmit states}
  YmodemStateType = (
    {Transmit states}
    tyInitial,              {0  Get next file}
    tyHandshake,            {1  Waiting for handshake}
    tyGetFileName,          {2  Get the next file to transmit}
    tySendFileName,         {3  Format and send file name block}
    tyDraining,             {4  Waiting for protocol block to drain}
    tyReplyPending,         {5  Waiting for reply to name block}
    tyPrepXmodem,           {6  Prepare to enter Xmodem state table}
    tySendXmodem,           {7  Calling Xmodem state table}
    tyFinished,             {8  Send EOT}
    tyFinishDrain,          {9  Wait for last block to go out}
    tyFinishAck,            {10 Wait for ACK to last block}
    tyDone,                 {11 Signal end of protocol}

    {Receive states}
    ryInitial,              {12 Initialize vars, get buffers, etc.}
    ryDelay,                {13 Delay the handshake for Telix}
    ryWaitForHSReply,       {14 Waiting for 1st reply to handshake}
    ryWaitForBlockStart,    {15 Wait for block start}
    ryCollectBlock,         {16 Collect received chars into DataBlock}
    ryProcessBlock,         {17 Process complete DataBlock}
    ryOpenFile,             {18 Extract file info}
    ryPrepXmodem,           {19 Prepare to enter Xmodem state}
    ryReceiveXmodem,        {20 Calling Xmodem state table}
    ryFinished,             {21 Clean up}
    ryDone);                {22 Signal end of protocol}

  {Define a Ymodem alias over Xmodem}
  YmodemPtr = ^YmodemProtocol;
  YmodemProtocol = record
    {vvvvv must be same as XmodemProtocolRec vvvvv}
    PData          : ProtocolDataPtr;   {General protocol data}
    StartChar      : Char;              {Block start character}
    OneKMode       : Boolean;           {True for XModem1K}
    BlockWait      : Byte;              {Wait seconds between blocks}
    MaxBlockErrors : Byte;              {Max number of allowed block errors}
    FirstBlockNum  : Byte;              {First block number to use}
    GMode          : Boolean;           {True for YmodemG}
    TimerPending   : Boolean;           {True if waiting to start timer}
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
    {^^^^^ must be same as XmodemProtocolRec ^^^^^}

    {Unique Ymodem fields}
    YmodemState    : YmodemStateType;   {Current Ymodem state}
    SaveName       : PathStr;           {Saved file name}
    SaveLen        : LongInt;           {Saved file length}
    FilesSent      : Boolean;           {True if we actually sent a file}
    FileHeader     : ^DataBlockType;    {Needed for file name block}
    NewDT          : Longint;           {New date/time stamp}
  end;

{$IFDEF UsePModeDLL}

procedure InitYmodem(var P : ProtocolRecPtr;
                     PortPtr : PortRecPtr;
                     Use1K, UseGMode : Boolean); Export;
procedure InitCustomYmodem(var P : ProtocolRecPtr;
                           PortPtr : PortRecPtr;
                           Use1K, UseGMode : Boolean;
                           Options : Word); Export;
procedure DoneYmodem(var P : ProtocolRecPtr); Export;
procedure ProtocolTransmitYM(P : ProtocolRecPtr); Export;
procedure PrepareTransmitPartYM(P : ProtocolRecPtr); Export;
function ProtocolTransmitPartYM(P : ProtocolRecPtr) : ProtocolStateType; Export;
procedure ProtocolReceiveYM(P : ProtocolRecPtr); Export;
procedure PrepareReceivePartYM(P : ProtocolRecPtr); Export;
function ProtocolReceivePartYM(P : ProtocolRecPtr) : ProtocolStateType; Export;

{$ELSE}

procedure InitYmodem(var P : ProtocolRecPtr;
                     PortPtr : PortRecPtr;
                     Use1K, UseGMode : Boolean);
procedure InitCustomYmodem(var P : ProtocolRecPtr;
                           PortPtr : PortRecPtr;
                           Use1K, UseGMode : Boolean;
                           Options : Word);
procedure DoneYmodem(var P : ProtocolRecPtr);
procedure ProtocolTransmitYM(P : ProtocolRecPtr);
procedure PrepareTransmitPartYM(P : ProtocolRecPtr);
function ProtocolTransmitPartYM(P : ProtocolRecPtr) : ProtocolStateType;
procedure ProtocolReceiveYM(P : ProtocolRecPtr);
procedure PrepareReceivePartYM(P : ProtocolRecPtr);
function ProtocolReceivePartYM(P : ProtocolRecPtr) : ProtocolStateType;

{$ENDIF}

implementation

  procedure InitYmodem(var P : ProtocolRecPtr;
                       PortPtr : PortRecPtr;
                       Use1K, UseGMode : Boolean);
  {-Allocates and initializes and Ymodem control block}
  begin
    InitCustomYmodem(P, PortPtr, Use1K, UseGMode, DefProtocolOptions);
  end;

  procedure InitCustomYmodem(var P : ProtocolRecPtr;
                             PortPtr : PortRecPtr;
                             Use1K, UseGMode : Boolean;
                             Options : Word);
    {-Allocates and initializes and Ymodem control block with options}
  var
    YM : YmodemPtr absolute P;
    Temp : ProtocolDataPtr;
  begin
    AsyncStatus := ecOk;
    P := nil;

    {Allocate the protocol data record}
    InitProtocolData(Temp, PortPtr, Options);
    if AsyncStatus <> ecOk then
      Exit;

    {Allocate the Ymodem record}
    if not GetMemCheck(YM, SizeOf(YModemProtocol)) then begin
      DoneProtocolData(Temp);
      GotError(PortPtr, epFatal+ecOutOfMemory);
      Exit;
    end;

    {Allocate the name block buffer}
    if not GetMemCheck(YM^.FileHeader, SizeOf(YM^.FileHeader^)) then begin
      DoneProtocolData(Temp);
      FreeMemCheck(YM, SizeOf(YModemProtocol));
      GotError(PortPtr, epFatal+ecOutOfMemory);
      Exit;
    end;

    {!!.01 copied from xmodem state machine}
    {Get a protocol DataBlock}
    if not GetMemCheck(YM^.DataBlock, SizeOf(DataBlockType)+5) then begin
      DoneProtocolData(Temp);
      FreeMemCheck(YM^.FileHeader, SizeOf(YM^.FileHeader^));
      FreeMemCheck(YM, SizeOf(YmodemProtocol));
      Exit;
    end;

    YM^.PData := Temp;

    with YM^, PData^ do begin
      {Set block mode}
      OneKMode := Use1K;
      Set1KMode(P, Use1K);

      {Miscellaneous inits}
      BlockWait := DefBlockWait;
      MaxBlockErrors := DefMaxBlockErrors;
      FirstBlockNum := 1;
      Overhead := XmodemOverhead;
      TurnDelay := XmodemTurnDelay;

      {Assume crc mode}
      CheckType := bcCrc16;

      {Ymodem specific stuff}
      BatchProtocol := True;
      GMode := UseGMode;
      if GMode then begin
        ProtType := YmodemG;
        TurnDelay := 0;
      end else
        ProtType := Ymodem;

      {Don't ask for any EOT retries}
      EotCheckCount := 0;

      {Set write fail option to rename}
      WriteFailOpt := WriteRename;
    end;
  end;

  procedure DoneYmodem(var P : ProtocolRecPtr);
    {-Disposes of P}
  var
    YM : YmodemPtr absolute P;
  begin
    with YM^ do begin
      DoneProtocolData(P^.PData);
      FreeMemCheck(FileHeader, SizeOf(FileHeader^));
      FreeMemCheck(YM^.DataBlock, SizeOf(DataBlockType)+5);
    end;
    FreeMemCheck(P, SizeOf(YmodemProtocol));
  end;

  procedure PrepareTransmitPartYM(P : ProtocolRecPtr);
    {-Prepare to transmit a Ymodem batch}
  var
    YM : YmodemPtr absolute P;

  begin
    with YM^, PData^ do begin
      FindingFirst := True;
      FilesSent := False;
      PR^.ProtocolActive := True;

      {Reset status vars}
      ResetStatus(P);

      {Show first status}
      AsyncStatus := ecHandshakeInProgress;
      ShowFirstStatus(P);
      AsyncStatus := ecOk;

      {Set first state}
      YmodemState := tyInitial;
    end;
  end;

  function ProtocolTransmitPartYM(P : ProtocolRecPtr) : ProtocolStateType;
    {-Transmit a batch of files}
  var
    YM : YmodemPtr absolute P;
    S : String;
    Len : Byte;
    I : Integer;
    Dir : DirStr;
    Name : NameStr;
    Ext : ExtStr;
    C : Char;
    SaveState : ProtocolStateType;

    function CheckErrors : Boolean;
      {-Increment block errors, return True if too many}
    begin
      with YM^, PData^ do begin
        Inc(BlockErrors);
        Inc(TotalErrors);
        CheckErrors := BlockErrors > MaxBlockErrors;
      end;
    end;

  begin
    with YM^, PData^ do begin
      {General stuff - only do if not dropping through to Xmodem state machine}
      if YmodemState <> tySendXmodem then begin

        {Check for user abort}
        if (YmodemState <> tyFinishDrain) and                          {!!.01}
           (YmodemState <> tyFinished) and                             {!!.01}
           HandleAbort(P) then begin                                   {!!.01}
          YmodemState := tyFinished;
          {Need to log cancellation here since APXMODEM won't see it}
          LogFile(P, lfTransmitFail);
        end;

        {Show status periodically}
        if TimerExpired(StatusTimer) or ForceStatus then begin
          ForceStatus := False;
          NewTimer(StatusTimer, StatusInterval);
          apUserStatus(P, False, False);
        end;
      end;

      {Process current state}
      case YmodemState of
        tyInitial :
          begin
            {Check for handshake character}
            YmodemState := tyHandshake;
            HandshakeAttempt := 0;
            if not PrepHandshake(P) then
              YmodemState := tyFinished;
          end;

        tyHandshake :
          begin
            if CharReady(PR) then
              if ProcessHandshake(P) then begin
                {Start protocol timer now}
                NewTimer(Timer, 1);
                {Use reply timer to wait for buffer freespace}
                NewTimer(ReplyTimer, TransTimeout);
                YmodemState := tyGetFileName;
                {If GMode don't allow any more errors}
                if GMode then
                  MaxBlockErrors := 0;
              end else begin
                if AsyncStatus = ecInitCancel then
                  YmodemState := tyFinished
                else if not PrepHandshake(P) then
                  YmodemState := tyFinished
              end
            else
              if TimerExpired(ReplyTimer) then
                if not PrepHandshake(P) then
                  YmodemState := tyFinished;
          end;

        tyGetFileName :
          begin
            if NextFile(P, SaveName) then begin
              {Format a file name block}
              Pathname := UpString(SaveName);

              {Make a Ymodem file header record}
              FillChar(FileHeader^, SizeOf(FileHeader^), 0);

              {Fill in the file name}
              FSplit(SaveName, Dir, Name, Ext);
              if FlagIsSet(apFlags, apIncludeDirectory) then
                S := SaveName
              else
                S := Name + Ext;

              {Change name to lower case, change '\' to '/'}
              Len := Length(S);
              for I := 1 to Len do begin
                S[I] := LoCaseMac(S[I]);
                if S[I] = '\' then
                  S[I] := '/';
              end;
              Move(S[1], FileHeader^, Len);

              {Open file now to get size and date stamp}
              PrepareReading(P);

              {Continue only if the file was opened ok}
              if AsyncStatus = ecOk then begin
                {Save the file length}
                SaveLen := SrcFileLen;

                {Fill in file size}
                Str(SrcFileLen, S);
                Move(S[1], FileHeader^[Len+2], Length(S));
                Inc(Len, Length(S));

                {Convert time stamp to Ymodem format and stuff in FileHeader}
                if SrcFileDate <> 0 then begin
                  S := ' ' + OctalStr(PackToYMTimeStamp(SrcFileDate));
                  Move(S[1], FileHeader^[Len+2], Length(S));
                  Len := Len + 2 + Length(S);
                end;

                {Determine block size from the used part of the FileHeader}
                if Len <= 128 then begin
                  BlockLen := 128;
                  OneKMode := False;
                  StartChar := cSoh;
                end else begin
                  BlockLen := 1024;
                  OneKMode := True;
                  StartChar := cStx;
                end;

                {Init status vars for the header transfer}
                SrcFileLen := BlockLen;
                BytesRemaining := BlockLen;
                BytesTransferred := 0;
                ElapsedTics := 0;
                Pathname := '';

                {Go send the file header}
                YmodemState := tySendFileName;
                CharsLeft := 0;
                OutBufPos := 1;
              end else begin
                GotError(PR, AsyncStatus);
                ShowLastStatus(P);
                YmodemState := tyDone;
                PR^.ProtocolActive := False;
              end;
            end else
              YmodemState := tyFinished;
          end;

        tySendFileName :
          begin
            {Send the file header}
            BlockNum := 0;
            TransmitBlock(P, FileHeader^, BlockLen, ' ');
            if AsyncStatus <> ecOk then begin
              YmodemState := tyFinished;
              ProtocolTransmitPartYM := psReady;
              Exit;
            end;

            {If we get this far we will eventually need a cleanup block}
            FilesSent := True;

            {Wait for the buffer to drain}
            YmodemState := tyDraining;
            NewTimer(ReplyTimer, DrainWait);
          end;

        tyDraining :
          begin
            if (OutBuffUsed(PR) <= 1) or TimerExpired(ReplyTimer) then begin
              NewTimer(ReplyTimer, BlockWait);
              YmodemState := tyReplyPending;
            end;
          end;

        tyReplyPending :
          begin
            if CharReady(PR) then begin
              if GMode then
                YmodemState := tyPrepXmodem
              else if ProcessBlockReply(P) then
                YmodemState := tyPrepXmodem
              else if CheckErrors then
                YmodemState := tyFinished
              else
                YmodemState := tySendFilename;
            end else
              if TimerExpired(ReplyTimer) then
                if CheckErrors then
                  YmodemState := tyFinished
                else
                  YmodemState := tySendFilename;
          end;

        tyPrepXmodem :
          begin
            {Reset some status vars}
            BytesTransferred := 0;
            ElapsedTics := 0;
            Inc(InProgress);

            {Restore the pathname and file size}
            Pathname := UpString(SaveName);
            SrcFileLen := SaveLen;
            BytesRemaining := SaveLen;

            {Start transmitting the file with 1K blocks}
            OneKMode := True;
            BlockLen := 1024;
            StartChar := cStx;
            FirstBlockNum := 1;

            CheckType := bcChecksum1;
            ForceStatus := True;
            XmodemState := txInitial;
            YmodemState := tySendXmodem;
            SaveState := psReady;
            {DataBlock := nil;}                                        {!!.01}
          end;

        tySendXmodem :
          begin
            {Let the Xmodem state machine handle it}
            SaveState := ProtocolTransmitPartXM(P);
            if SaveState = psFinished then
              if AsyncStatus = ecOk then
                YmodemState := tyInitial
              else begin
                FilesSent := False;
                YmodemState := tyFinished;
              end;

            {Say we're still in the protocol}
            PR^.ProtocolActive := True;
          end;

        tyFinished :
          begin
            YmodemState := tyFinishDrain;
            FinishReading(P);
            if (AsyncStatus = ecUserAbort) or
               (AsyncStatus = ecCancelRequested) then begin
              ShowLastStatus(P);
              YmodemState := tyDone;
              PR^.ProtocolActive := False;
            end;

            if FilesSent and (YmodemState <> tyDone) then begin        {!!.01}
              {Send an empty header block to indicate end of Batch}
              FillChar(FileHeader^, 128, 0);
              BlockNum := 0;
              OneKMode := False;
              BlockLen := 128;
              StartChar := cSoh;
              CharsLeft := 0;
              OutBufPos := 1;
              TransmitBlock(P, FileHeader^, BlockLen, ' ');
            end;
            NewTimer(ReplyTimer, FinishWait);
          end;

        tyFinishDrain :
          if (OutBuffUsed(PR) <= 1) or TimerExpired(ReplyTimer) then begin
            {Wait for ACK}
            YmodemState := tyFinishAck;
            NewTimer(ReplyTimer, BlockWait);
          end;

        tyFinishAck :
          if CharReady(PR) or TimerExpired(ReplyTimer) then begin
            {Get and discard Ack, or whatever}
            if CharReady(PR) then
              GetChar(PR, C);
            YmodemState := tyDone;
            ShowLastStatus(P);
            PR^.ProtocolActive := False;
          end;
      end;

      {Set function result}
      case YmodemState of
        tySendXmodem       : ProtocolTransmitPartYM := SaveState;

        tyInitial,
        tyGetFileName,
        tySendFileName,
        tyPrepXmodem,
        tyFinished         : ProtocolTransmitPartYM := psReady;

        tyFinishAck,
        tyFinishDrain,
        tyHandshake,
        tyDraining,
        tyReplyPending     : ProtocolTransmitPartYM := psWaiting;

        tyDone             : ProtocolTransmitPartYM := psFinished;
      end;
    end;
  end;

  procedure ProtocolTransmitYM(P : ProtocolRecPtr);
    {-Transmit a batch of files}
  var
    YM : YmodemPtr absolute P;
    State : ProtocolStateType;
  begin
    with YM^, PData^ do begin
      PrepareTransmitPartYM(P);
      if AsyncStatus <> ecOk then
        Exit;
      repeat
        State := ProtocolTransmitPartYM(P);
        if State = psWaiting then
          UserBack(P);
      until State = psFinished;
    end;
  end;

  procedure PrepareReceivePartYM(P : ProtocolRecPtr);
    {-Prepare to enter the Ymodem receive state machine}
  var
    YM : YmodemPtr absolute P;
  begin
    with YM^, PData^ do begin
      PR^.ProtocolActive := True;
      GotOneFile := False;

      {Reset status vars}
      ResetStatus(P);
      ShowFirstStatus(P);

      ForceStatus := True;
      YmodemState := ryInitial;
    end;
  end;

  function ProtocolReceivePartYM(P : ProtocolRecPtr) : ProtocolStateType;
    {-Ymodem receive state machine}
  label
    ExitPoint;
  var
    YM : YmodemPtr absolute P;
    Dir : DirStr;
    Name : NameStr;
    Ext : ExtStr;
    C : Char;
    F : File;
    S : String;
    SLen : Byte absolute S;
    CurSize : LongInt;
    BlockSize : Word;
    BlockPos, I : Integer;
    Finished : Boolean;
    Code : {$IFNDEF OS2} Word; {$ELSE} LongInt; {$ENDIF}
    Result : Word;
    SaveState : ProtocolStateType;
    RcvStatus : Word;
    {Cnt : Word;}                                                      {!!.03}

    function CheckErrors : Boolean;
      {-Increment block errors, return True if too many}
    begin
      with YM^, PData^ do begin
        Inc(BlockErrors);
        Inc(TotalErrors);
        CheckErrors := BlockErrors > MaxBlockErrors;
      end;
    end;

  begin
    with YM^, PData^ do begin
      {General stuff - only do if not dropping through to Xmodem state machine}
      if YmodemState <> ryReceiveXmodem then begin

        {Check for user abort}
        if HandleAbort(P) then
          YmodemState := ryFinished;

        {Show status periodically}
        if TimerExpired(StatusTimer) or ForceStatus then begin
          ForceStatus := False;
          NewTimer(StatusTimer, StatusInterval);
          if InProgress <> 0 then
            apUserStatus(P, False, False);
        end;
      end;

      {Process current state}
      case YmodemState of
        ryInitial :
          begin
            {Manually reset status vars before getting a file header}
            SrcFileLen := 0;
            BytesRemaining := 0;
            BytesTransferred := 0;
            ElapsedTics := 0;
            BlockNum := 0;
            Pathname := '';

            {Get a ymodem header block (really a 1 block xmodem transfer)}
            FillChar(FileHeader^[1], SizeOf(FileHeader^), 0);
            CheckType := bcCrc16;
            OverheadLen := 4;
            OneKMode := False;
            BlockSize := 128;
            BlockNum := 0;

            {Testing shows a short delay is required here for Telix}
            NewTimer(ReplyTimer, TelixDelay);
            YmodemState := ryDelay;
          end;

        ryDelay :
          if TimerExpired(ReplyTimer) then begin
            {Send handshake}
            HandshakeChar := GetHandshakeChar(P);
            PutChar(PR, HandshakeChar);
            EotCounter := 0;
            CanCounter := 0;

            {Start waiting for handshake reply}
            YmodemState := ryWaitForHSReply;
            NewTimer(ReplyTimer, HandshakeWait);
            TimerPending := True;
          end;

        ryWaitForHSReply :
          begin
            if CharReady(PR) then begin
              YmodemState := ryWaitForBlockStart;
            end else
              if TimerExpired(ReplyTimer) then begin
                if CheckErrors then
                  YmodemState := ryFinished
                else begin
                  if BlockErrors > 3 then begin
                    CheckType := bcChecksum1;
                    HandshakeChar := cNak;
                    OverheadLen := 3;
                  end;
                  PutChar(PR, HandshakeChar);
                  NewTimer(ReplyTimer, HandshakeWait);
                end;
              end;
            end;

        ryWaitForBlockStart :
          begin
            if CharReady(PR) then begin
              if CheckForBlockStart(P, C) then begin
                case C of
                  cSoh,
                  cStx :
                    begin
                      if C = cSoh then
                        BlockLen := 128
                      else
                        BlockLen := 1024;
                      if TimerPending then
                        NewTimer(Timer, 1);
                      YmodemState := ryCollectBlock;
                      BlkIndex := 0;
                      if GMode then
                        MaxBlockErrors := 0;
                    end;
                  cCan :
                    begin
                      EotCounter := 0;
                      Inc(CanCounter);
                      if CanCounter > 2 then begin
                        Cancel(P);
                        YmodemState := ryFinished;
                      end;
                    end;
                  cEot :
                    begin
                      CanCounter := 0;
                      Inc(EotCounter);
                      if EotCounter = 1 then
                        PutChar(PR, cNak)
                      else begin
                        PutChar(PR, cAck);
                        YmodemState := ryFinished;
                      end;
                    end;
                end;
              end;
            end else begin
              {No chars yet, check timeout}
              if TimerExpired(ReplyTimer) then
                if CheckErrors then
                  YmodemState := ryFinished
                else begin
                  FlushInBuffer(PR);
                  YmodemState := ryDelay;
                end;
            end;
          end;

        ryCollectBlock :
          if CharReady(PR) then begin
            {Cnt := 1;}                                                {!!.03}
            while CharReady(PR) and
                  {(Cnt < 10) and}                                     {!!.03}
                  (BlkIndex < BlockLen + OverheadLen) do begin
              GetChar(PR, C);
              Inc(BlkIndex);
              {Inc(Cnt);}                                              {!!.03}
              FileHeader^[BlkIndex] := C;
            end;

            if BlkIndex >= BlockLen + OverheadLen then
              {Got a complete block, go process it}
              YmodemState := ryProcessBlock
            else if TimerExpired(ReplyTimer) then
              if CheckErrors then
                YmodemState := ryFinished
              else
                {Timeout out waiting for initial block, resend handshake}
                YmodemState := ryInitial;
          end;

        ryProcessBlock :
          begin
            {Go process data already in DataBlock}
            ReceiveBlock(P, FileHeader^, BlockSize, HandshakeChar);
            RcvStatus := AsyncStatus;
            SendHandshakeChar(P, HandshakeChar);

            {Extract file info if we got block ok}
            if RcvStatus = ecOk then begin
              {Finished if entire block is null}
              Finished := True;
              I := 1;
              while (I < 120) and Finished do begin                    {!!.03}
                if FileHeader^[I] <> #0 then
                  Finished := False;
                Inc(I);
              end;

              {If finished, send last ack and exit}
              if Finished then begin
                YmodemState := ryFinished;
                goto ExitPoint;
              end;

              {Extract the file name from the header}
              BlockPos := 1;
              I := 0;
              while (FileHeader^[BlockPos] <> #0) and (BlockPos < 255) do begin
                Inc(I);
                S[I] := FileHeader^[BlockPos];
                Inc(BlockPos);
              end;
              SLen := I;

              {Change all '/' to '\'. Change name to all upper case}
              for I := 1 to SLen do begin
                if S[I] = '/' then
                  S[I] := '\';
                S[I] := Upcase(S[I]);
              end;
              Pathname := ValidatePathname (S);

              {Check the existance of the directory and file name}
              FSplit(Pathname, Dir, Name, Ext);

              {Should we use its directory or ours?}
              if not FlagIsSet(apFlags, apHonorDirectory) then
                Pathname := AddBackSlash (DestDir) + Name + Ext;

              {Extract the file size}
              I := 1;
              Inc(BlockPos);
              while (FileHeader^[BlockPos] <> #0) and
                    (FileHeader^[BlockPos] <> ' ') and
                    (I <= 255) do begin
                S[I] := FileHeader^[BlockPos];
                Inc(I);
                Inc(BlockPos);
              end;
              Dec(I);
              SLen := I;
              if SLen = 0 then
                SrcFileLen := 0
              else begin
                Val(S, SrcFileLen, Code);
                if Code <> 0 then
                  SrcFileLen := 0;
              end;
              BytesRemaining := SrcFileLen;

              {Extract the file date/time stamp}
              I := 1;
              Inc(BlockPos);
              while (FileHeader^[BlockPos] <> #0) and
                    (FileHeader^[BlockPos] <> ' ') and
                    (I <= 255) do begin
                S[I] := FileHeader^[BlockPos];
                Inc(I);
                Inc(BlockPos);
              end;
              Dec(I);
              SLen := I;
              if SLen = 0 then
                NewDT := 0
              else begin
                NewDT := OctalStr2Long(S);
                if NewDT = 0 then begin
                  {Invalid char in date/time stampt, show the error and continue}
                  GotError(PR, epNonFatal+ecInvalidDateTime);
                  NewDT := 0;
                  UserStatus(P, False, False);
                  AsyncStatus := ecOk;
                end;
              end;

              {Manually reset status vars before getting file}
              BytesTransferred := 0;
              ElapsedTics := 0;

              {Receive the file using CRC and 1K blocks}
              CheckType := bcCrc16;
              OneKMode := True;
              BlockLen := 1024;
              FirstBlockNum := 1;
              SaveLen := SrcFileLen;

              {Go prep Xmodem}
              YmodemState := ryPrepXmodem;
            end else
              {Error getting name block...}
              if GMode then
                {Can't recover when in GMode, go quit}
                YmodemState := ryFinished
              else begin
                {Nak already sent, go get block again}
                YmodemState := ryWaitForHSReply;
                NewTimer(ReplyTimer, HandshakeWait);
              end;
          end;

        ryPrepXmodem :
          begin
            PrepareReceivePartXM(P);
            YmodemState := ryReceiveXmodem;
            SaveState := psReady;
          end;

        ryReceiveXmodem :
          begin
            SaveState := ProtocolReceivePartXM(P);
            if SaveState = psFinished then begin
              if AsyncStatus = ecOk then begin
                {If this is a file, check for truncation and file date}
                SaveMode := FileMode;
                FileMode := $12; {ReadWrite + DenyReadWrite}
                Assign(F, Pathname);
                Reset(F, 1);
                FileMode := SaveMode;
                if IOResult = 0 then begin
                  {If a new file size was supplied, truncate to that length}
                  if SaveLen <> 0 then begin

                    {Get the file size of the file (as received)}
                    CurSize := FileSize(F);

                    {If the requested file size is within one block, truncate the file}
                    if (CurSize - SaveLen) < 1024 then begin
                      Seek(F, SaveLen);
                      Truncate(F);
                      Result := IOResult;
                      if Result <> 0 then begin
                        GotError(PR, epNonFatal+Result);
                        AsyncStatus := ecOk;
                      end;
                    end;
                  end;

                  {If a new date/time stamp was specified, update the file time}
                  if NewDT <> 0 then begin
                    NewDT := YMTimeStampToPack(NewDT);
                    SetFTime(F, NewDT);
                    Result := IOResult;
                    if Result <> 0 then begin
                      GotError(PR, epNonFatal+Result);
                      AsyncStatus := ecOk;
                    end;
                  end;
                end;
                Close(F);
                if IOResult <> 0 then ;

                {Go look for another file}
                YmodemState := ryInitial;
                NewTimer(ReplyTimer, HandshakeWait);
                ForceStatus := True;
              end else
                YmodemState := ryFinished;
            end;
            PR^.ProtocolActive := True;
          end;

        ryFinished :
          begin
            ShowLastStatus(P);
            YmodemState := ryDone;
            PR^.ProtocolActive := False;
          end;
      end;

ExitPoint:
      {Set function result}
      case YmodemState of
        ryReceiveXmodem     : ProtocolReceivePartYM := SaveState;

        ryInitial,
        ryOpenFile,
        ryProcessBlock,
        ryFinished,
        ryPrepXmodem        : ProtocolReceivePartYM := psReady;

        ryDelay,
        ryWaitForHSReply,
        ryWaitForBlockStart,
        ryCollectBlock      : ProtocolReceivePartYM := psWaiting;

        ryDone              : ProtocolReceivePartYM := psFinished;
      end;
    end;
  end;

  procedure ProtocolReceiveYM(P : ProtocolRecPtr);
    {-Receive a batch of Ymodem files}
  var
    YM : YmodemPtr absolute P;
    State : ProtocolStateType;
  begin
    with YM^, PData^ do begin
      PrepareReceivePartYM(P);
      if AsyncStatus <> ecOk then
        Exit;
      repeat
        State := ProtocolReceivePartYM(P);
        if State = psWaiting then
          UserBack(P);
      until State = psFinished;
    end;
  end;

end.
