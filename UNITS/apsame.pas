{$IFDEF MSDOS}
  {$IFNDEF DPMI32}
    {$F+}
  {$ENDIF}
{$ENDIF}
Unit ApSame;

{*********************************************************}
{*                                                       *}
{*                    APSAME.PAS 1.0                     *}
{*                                                       *}
{*     Copyright (c) Konstantin Klyagin, 1996.           *}
{*                   expecialy for Tornado BBS system    *}
{*                                                       *}
{*********************************************************}

Interface

Uses
  Crc;

Var
  AsyncStatus : Word;

Type
  CharArray = Array [0..MaxInt] Of Char;

  {For internal date/time manipulations}
  Date = LongInt;
  Time = LongInt;
  DateTimeRec = Record
    D : Date;
    T : Time;
  End;
  DayType = (Sunday, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday);

  PS2Mode = (PS2On, PS2Off, PS2Auto, PS2Ignore);

  StringPtr = ^String;
  CharSet = Set Of Char;

  ComNameType = (Com1, Com2, Com3, Com4, Com5, Com6, Com7, Com8, Com9, Com10,
    Com11, Com12, Com13, Com14, Com15, Com16, Com17, Com18, Com19, Com20,
    Com21, Com22, Com23, Com24, Com25, Com26, Com27, Com28, Com29, Com30,
    Com31, Com32, Com33, Com34, Com35, Com36);

  ParityType = (NoParity, OddParity, EvenParity, MarkParity, SpaceParity);
  DataBitType = 5..8;
  StopBitType = 1..2;
  FlowState = (fsOff, fsClear, fsTransWait, fsRecWait, fsAllWait);

  PortRecPtr = ^PortRec;

  {Line options record}
  LineOptionRecord = Record
                       Parity   : ParityType;
                       DataBits : DataBitType;
                       StopBits : StopBitType;
                       Options  : Word;
                       InSize   : Word;
                       OutSize  : Word;
                     End;

  InitPortProc = Procedure (Var P: PortRecPtr; ComName: ComNameType;
                           Baud: LongInt; Parity: ParityType;
                           DataBits: DataBitType; StopBit: StopBitType;
                           InSize, OutSize: Word; Options: Word);
  InitPortKeepProc = Procedure (Var P: PortRecPtr; ComName: ComNameType;
                               InSize, OutSize: Word);
  DonePortProc = Procedure (Var P: PortRecPtr);
  SetUartProc = Procedure (ComName: ComNameType; NewBase: Word;
                          NewIrq, NewVector: Byte);
  SetLineProc = Procedure (P: PortRecPtr; Baud: LongInt; Parity: ParityType;
                          DataBits: DataBitType; StopBits: StopBitType);
  GetLineProc = Procedure (P: PortRecPtr; Var Baud: LongInt;
                          Var Parity: ParityType; Var DataBits: DataBitType;
                          Var StopBits: StopBitType; FromHardware: Boolean);
  SetModemProc = Procedure (P: PortRecPtr; SetDTR, SetRTS: Boolean);
  GetModemProc = Procedure (P: PortRecPtr; Var DTR, RTS: Boolean);
  GetCharProc = Procedure (P: PortRecPtr; Var C: Char);
  PeekCharProc = Procedure (P: PortRecPtr; Var C: Char; PeekAhead: Word);
  PutCharProc = Procedure (P: PortRecPtr; C: Char);
  PutBlockFunc = Function (P: PortRecPtr; Var Block; Size: Word): Word;
  StartTransmitterProc = Procedure (P: PortRecPtr);
  CharReadyFunc = Function (P: PortRecPtr): Boolean;
  TransReadyFunc = Function (P: PortRecPtr): Boolean;
  SendBreakProc = Procedure (P: PortRecPtr);
  ActivatePortProc = Procedure (P: PortRecPtr; Restore: Boolean);
  SavePortProc = Procedure (P: PortRecPtr; Var PSR);

  {Core types added for alternate device layers}
  UpdateLineStatusFunc = Function (P: PortRecPtr): Byte;
  UpdateModemStatusFunc = Function (P: PortRecPtr): Byte;
  FlowSetProc = Procedure (P: PortRecPtr; Enable: Boolean;
                          BufferFull, BufferResume: Word; Options: Word);
  FlowGetFunc = Function (P: PortRecPtr): FlowState;
  FlowCtlProc = Procedure (P: PortRecPtr; OnChar, OffChar: Char;
                          Resume: Boolean);
  BufferStatusProc = Procedure (P: PortRecPtr;
                               Var InFree, OutFree, InUsed, OutUsed: Word);
  BufferFlushProc = Procedure (P: PortRecPtr; FlushIn, FlushOut: Boolean);

  {Procedure type for error handler}
  AsyncErrorProc = Procedure (P: Pointer; Var StatusCode: Word);

  BPtr = ^Byte;
  AbortFunc = Function: Boolean;

  PortSaveRec = Record
                  PicMask : Byte;
                  IER     : Byte;
                  MCR     : Byte;
                  LCR     : Byte;
                  BRLR    : Byte;
                  BRHR    : Byte;
                  FIFO    : Byte;
                  Trigger : Byte;
                  Vector  : Pointer;
                End;

  PortRec = Record
              BaseAddr      : Word;             {Base IO addr of UART}
              Flags         : Word;             {Option flags for port options}
              InBuffLen     : Word;             {Length of input buffer}
              InBuffCount   : Word;             {Current # of chars in buffer}
              OutBuffLen    : Word;             {Length of output buffer}
              OutBuffCount  : Word;             {Current # of chars in buffer}
              LostCharCount : Word;             {Number of lost characters}
              SWFFull       : Word;             {Hi-water mark for xoff}
              SWFResume     : Word;             {Lo-water mark for xon}
              HWFFull       : Word;             {Hi-water mark for auto-handshaking off}
              HWFResume     : Word;             {Lo-water mark for auto-handshaking on}
              CurBaud       : LongInt;          {Baud rate}
              InBuff        : BPtr;             {Addr of input buffer}
              InHead        : BPtr;             {Addr of current head}
              InTail        : BPtr;             {Addr of current tail}
              InBuffEnd     : BPtr;             {Addr of end of buffer}
              OutBuff       : BPtr;             {Addr of output buffer}
              OutHead       : BPtr;             {Addr of current head}
              OutTail       : BPtr;             {Addr of current tail}
              OutBuffEnd    : BPtr;             {Addr of end-of-buffer}
              StatBuff      : BPtr;             {Addr of status buffer}
              StatHead      : BPtr;             {Addr of current status head}
              StatTail      : BPtr;             {Addr of current status tail}
              StatBuffEnd   : BPtr;             {Addr of end of status buffer}
              PortName      : ComNameType;      {"Standard" name (COM1,COM2...)}
              Vector        : Byte;             {Vector number of UART interrupt}
              IrqNumber     : Byte;             {IRQ number for this port}
              IntMask       : Byte;             {Current UART interrupt enable}
              CurrentPort   : Byte;             {Current active port number}
              ISREntryPoint : Byte;             {Entry point number into APUART.ASM}
              ModemStatus   : Byte;             {Current modem status}
              ModemControl  : Byte;             {Current modem control value}
              LineStatus    : Byte;             {Current line status}
              LineControl   : Byte;             {Current line control value}
              SWFState      : Byte;             {Sofware flow control options}
              SWFGotXoff    : Boolean;          {True if Xoff char received}
              SWFSentXoff   : Boolean;          {True if Xoff char sent}
              SWFOnChar     : Char;             {SW flow on character (def = $11, Xon)}
              SWFOffChar    : Char;             {SW flow off character (def = $13, Xoff)}
              BreakReceived : Boolean;          {True in break received}
              TxReady       : Boolean;          {True if transmitter is available}
              TxInts        : Boolean;          {True if using transmit interrupts}
              TxIntsActive  : Boolean;          {True if transmit ints are active}
              Buffered      : Boolean;          {True if using buffer serial I/O}
              UseStatusBuffer : Boolean;        {True if using status buffer}
              OldUart       : Boolean;          {True if UART is 8250 or 8250B}
              CurParity     : ParityType;       {Parity}
              CurDataBits   : DataBitType;      {Data bits}
              CurStopBits   : StopBitType;      {Stop bits}
              SaveChar      : Char;             {Last known char (used internally only)}
              LastXmitError : Byte;             {Reason for last failed xmit}
              HWFTransMask  : Byte;             {Mask to XOR modem status bits to zero}
              HWFTransHonor : Byte;             {Mask of required modem status bits}
              HWFRecMask    : Byte;             {Mask of "on" modem status bits}
              HWFRecHonor   : Byte;             {Mask of modem status bits we care about}
              HWFRemoteOff  : Boolean;          {True if we have turned off the remote}
              ISRActive     : Boolean;          {True if in debugging mode}
              ProtocolActive : Boolean;         {True if this port is doing a protocol}
              FaxActive     : Boolean;          {True if this port is doing a fax}
              DoneProc      : DonePortProc;     {DonePort proc for this port}
              ErrorProc     : AsyncErrorProc;   {Pointer to error procedure}
              ErrorData     : Pointer;          {Pointer passed to error routine}
              UserAbort     : AbortFunc;        {Hook for user (keyboard) abort}
              OrigPortState : PortSaveRec;      {Record for saving init port config}
            End;

Const
  {8250 register designations}
  THreg  = 0;                        {Transmit hold}
  RDreg  = 0;                        {Read data}
  BRLreg = 0;                        {Baud rate least sig}
  BRHreg = 1;                        {Baud rate most sig}
  IEreg  = 1;                        {Int enable reg}
  IIDreg = 2;                        {Int ident reg}
  LCreg  = 3;                        {Line control}
  MCreg  = 4;                        {Modem control}
  LSreg  = 5;                        {Line status}
  MSreg  = 6;                        {Modem status}
  Sreg   = 7;                        {Scratch register}

  {Line control bit masks}
  WordLen0Mask     = $01;        {Word length select 0}
  WordLen1Mask     = $02;        {Word length select 1}
  StopBitsMask     = $04;        {Number of stop bits}
  ParityEnableMask = $08;        {Parity enable}
  EvenParityMask   = $10;        {Even parity select}
  StickParityMask  = $20;        {Stick parity select}
  SetBreakMask     = $40;        {Set break}
  DLABMask         = $80;        {Set divisor latch access}

  {Line status bit masks}
  DataReadyMask    = $01;        {Receive char is ready}
  OverrunErrorMask = $02;        {Overrun error received}
  ParityErrorMask  = $04;        {Parity error received}
  FramingErrorMask = $08;        {Framing error received}
  BreakReceivedMask = $10;       {Break received}
  THREMask         = $20;        {Transmitter holding register is empty}
  TEMask           = $40;        {Transmitter is empty}
  FIFOErrorMask    = $80;        {FIFO error received}

  {Modem status bit masks}
  DeltaCTSMask     = $01;        {CTS changed since last read}
  DeltaDSRMask     = $02;        {DSR changed since last read}
  DeltaRIMask      = $04;        {RI changed since last read}
  DeltaDCDMask     = $08;        {DCD changed since last read}
  CTSMask          = $10;        {Clear to send}
  DSRMask          = $20;        {Data set ready}
  RIMask           = $40;        {Ring indicator}
  DCDMask          = $80;        {Data carrier detect}

  {Interrupt enable bit masks}
  ReceiveIntMask   = $01;        {Interrupt on received data}
  TransmitIntMask  = $02;        {Interrupt on THR empty}
  LineIntMask      = $04;        {Interrupt on line status change}
  ModemIntMask     = $08;        {Interrupt on modem status change}

  {Hardware flow control options}
  hfUseDTR         = $01;   {Use DTR for receive flow control}
  hfUseRTS         = $02;   {Use RTS for receive flow control}
  hfRequireDSR     = $04;   {Require DSR before transmittting}
  hfRequireCTS     = $08;   {Require CTS before transmittting}
  hfDTRActiveLow   = $10;   {Make DTR active low}
  hfRTSActiveLow   = $20;   {Make RTS active low}
  hfDSRActiveLow   = $40;   {Make DSR active low}
  hfCTSActiveLow   = $80;   {Make CTS active low}

  sfReceiveFlow    = $01;   {Use receiver flow control}
  sfTransmitFlow   = $02;   {User transmitter flow control}
  DefSWFOpt        = sfReceiveFlow + sfTransmitFlow;

  AproFileMode  : Byte = $40;       {readonly filemode}
  PS2DetectMode : PS2Mode = PS2Auto;

  {Convenient character constants (and aliases)}
  cNul = #0;
  cSoh = #1;
  cStx = #2;
  cEtx = #3;
  cEot = #4;
  cEnq = #5;
  cAck = #6;
  cBel = #7;
  cBS  = #8;
  cTab = #9;
  cLF  = #10;
  cVT  = #11;
  cFF  = #12;
  cCR  = #13;
  cSO  = #14;
  cSI  = #15;
  cDle = #16;
  cDC1 = #17;       cXon  = #17;
  cDC2 = #18;
  cDC3 = #19;       cXoff = #19;
  cDC4 = #20;
  cNak = #21;
  cSyn = #22;
  cEtb = #23;
  cCan = #24;
  cEM  = #25;
  cSub = #26;
  cEsc = #27;
  cFS  = #28;
  cGS  = #29;
  cRS  = #30;
  cUS  = #31;

  DefaultXonChar  : Char = cXon;   {Standard Xon char (DC1)}
  DefaultXoffChar : Char = cXoff;  {Standard Xoff char (DC3)}

  {---- Option codes for ports ----}
  ptReturnPartialGets  = $0001;   {True to return partial strings}
  ptReturnDelimiter    = $0002;   {True to return delim char}
  ptExecutePartialPuts = $0004;   {True to send partial blocks}
  ptRestoreOnClose     = $0010;   {True to restore UART on close}
  ptDropModemOnClose   = $0020;   {True to drop modem signals on close}
  ptRaiseModemOnOpen   = $0040;   {True to raise modem signals on open}
  ptBufferGetChar      = $1000;   {Set to use buffered reads}

  {---- Internal option codes for ports ----}
  ptHiIrq              = $1000;   {True if IRQ > 7}

  DefPortOptionsSimple = ptReturnPartialGets +
                         ptReturnDelimiter +
                         ptExecutePartialPuts +
                         ptDropModemOnClose +
                         ptRaiseModemOnOpen +
                         ptRestoreOnClose;
  DefPortOptions : Word = DefPortOptionsSimple;
  BadPortOptions : Word = ptHiIrq;

  MinInBuff  = 10;           {Min allowable input buffer size}
  MinOutBuff = 10;           {Min allowable output buffer size}

  DefaultLineOptions : LineOptionRecord =
                         (Parity : NoParity;
                         DataBits : 8;
                         StopBits : 1;
                         Options : DefPortOptionsSimple;
                         InSize : 2048;
                         OutSize : 2048 + 30);

  {Modem control bit masks}
  DTRMask          = $01;        {Data terminal ready}
  RTSMask          = $02;        {Request to send}
  Out1Mask         = $04;        {Output bit 1}
  Out2Mask         = $08;        {Output bit 2}
  LoopbackMask     = $10;        {Loopback testing}

  {Error/status code constants}
  ecOk             = 0;          {Reset value for AsyncStatus}

  BadDate  = $FFFF;
  MinYear  = 1900;
  MaxYear  = 2078;
  First2Months = 58;         {1900 was not a leap year}
  FirstDayOfWeek = Monday;   {01/01/1900 was a Monday}
  BadTime = $FFFFFFFF;
  SecondsInHour = 3600;      {number of seconds in an hour}
  SecondsInMinute = 60;      {number of seconds in a minute}
  HoursInDay = 24;           {number of hours in a day}
  SecondsInDay = 86400;      {number of seconds in a day}

  {+++General error codes (0-999)+++}

  {DOS errors}
  ecFileNotFound           = 0002;  {File not found}
  ecPathNotFound           = 0003;  {Path not found}
  ecTooManyFiles           = 0004;  {Too many open files}
  ecAccessDenied           = 0005;  {File access denied}
  ecInvalidHandle          = 0006;  {Invalid file handle}
  ecOutOfMemory            = 0008;  {Insufficient memory}
  ecInvalidDrive           = 0015;  {Invalid drive}
  ecNoMoreFiles            = 0018;  {No more files}

  {Turbo Pascal I/O errors}
  ecDiskRead               = 0100;  {Attempt to read beyond end of file}
  ecDiskFull               = 0101;  {Disk is full}
  ecNotAssigned            = 0102;  {File not Assign-ed}
  ecNotOpen                = 0103;  {File not open}
  ecNotOpenInput           = 0104;  {File not open for input}
  ecNotOpenOutput          = 0105;  {File not open for output}
  ecInvalidFormat          = 0106;  {Invalid format for packed window}

  {DOS critical errors}
  ecWriteProtected         = 0150;  {Disk is write-protected}
  ecUnknownUnit            = 0151;  {Unknown disk unit}
  ecDriveNotReady          = 0152;  {Drive is not ready}
  ecUnknownCommand         = 0153;  {Unknown command}
  ecCrcError               = 0154;  {Data error}
  ecBadStructLen           = 0155;  {Bad request structure length}
  ecSeekError              = 0156;  {Seek error}
  ecUnknownMedia           = 0157;  {Unknown media type}
  ecSectorNotFound         = 0158;  {Disk sector not found}
  ecOutOfPaper             = 0159;  {Printer is out of paper}
  ecDeviceWrite            = 0160;  {Device write error}
  ecDeviceRead             = 0161;  {Device read error}
  ecHardwareFailure        = 0162;  {General failure}

  {+++Capacity or environmental errors (2900-2999)+++}

  {APUART port errors}
  ecNoMorePorts            = 2900;  {Can't open port, no slots available}
  ecOverrunError           = 2901;  {UART receiver overrun}
  ecParityError            = 2902;  {UART receiver parity error}
  ecFramingError           = 2903;  {UART receiver framing error}

  {APINT14 port errors}
  ecTransmitFailed         = 2910;  {Int14 transmit failed}
  ecUartError              = 2911;  {Int14 receive failed}

  {APCOM/OOCOM errors/status}
  ecBlockIncomplete        = 2920;  {Block shorter than requested}
  ecBufferIsFull           = 2921;  {No room for new char in buffer}
  ecBufferIsEmpty          = 2922;  {No characters to get}
  ecTimeout                = 2923;  {Timed out waiting for something}
  ecStringIncomplete       = 2924;  {String shorter than requested}
  ecStringOverrun          = 2925;  {String longer than 255}
  ecUserAbort              = 2926;  {User aborted during "wait"}

  {APMODEM/OOMODEM errors}
  ecTableFull              = 2930;  {No room in table to add new entry}
  ecNullCommand            = 2931;  {Modem - no command registered}

  {Tracing/EventFile file errors}
  ecEventFileError         = 2940;  {Failed to open or write to the event file}
  ecTraceFileError         = 2941;  {Failed to open or write to the trace file}

  {Constants for supported device types}
  NoDevice = 0;
  UartDevice = 1;
  Int14Device = 2;
  FossilDevice = 3;
  Digi14Device = 4;

  {Other device layer errors}
  ecNoFossil               = 2950;  {No fossil installed}
  ecDigiFailure            = 2960;  {Generic Digiboard failure code}

  {+++Warnings or user errors (7900-7999)+++}
  {This category not currently used by Async Professional}

  {+++Programmer errors (8900-8999)+++}

  {APCOM/OOCOM port errors}
  ecBadPortNumber          = 8900;  {Out-of-range port number}
  ecOutofRange             = 8901;  {General out-of-range error}
  ecPortNotOpen            = 8902;  {Port not open}
  ecInvalidBaudRate        = 8903;  {Bad baud rate for this device}
  ecInvalidArgument        = 8904;  {General programming error}
  ecNoDevice               = 8905;  {No device layer installed}
  ecNotaUart               = 8906;  {Couldn't find a uart at this address}
  ecInvalidParity          = 8907;  {Bad parity option for this device}
  ecNotBuffered            = 8910;  {Operation only allowed on buffered ports}
  ecNotSupported           = 8911;  {Function not supported by device-layer}

  {+++Status codes (9700-9999)+++}

  {APABSPCL/OOABSPCL status codes}
  ecInitFail               = 9900;  {Xmodem init failed}
  ecInitCancel             = 9901;  {Xmodem init was canceled on request}
  ecCancelRequested        = 9902;  {Cancel requested}
  ecDuplicateBlock         = 9903;  {Duplicate block received}
  ecSequenceError          = 9904;  {Wrong block number received}
  ecDirNotFound            = 9905;  {Directory not found in protocol transmit}
  ecNoMatchingFiles        = 9906;  {No matching files in protocol transmit}
  ecLongPacket             = 9907;  {Long packet received during protocol}
  ecEndFile                = 9908;  {End of transmitted file}
  ecHandshakeInProgress    = 9909;  {Initial protocol handshake in progress}
  ecFileRenamed            = 9910;  {Incoming file was renamed}
  ecFileAlreadyExists      = 9911;  {Incoming file already exists}
  ecInvalidFilesize        = 9912;  {Ymodem header has bad file size}
  ecInvalidDateTime        = 9913;  {Ymodem header has bad date/time}
  ecUnexpectedChar         = 9914;  {Unexpected char during protocol}
  ecBlockCheckError        = 9915;  {Incorrect CRC or checksum received}
  ecNoSearchMask           = 9916;  {No search mask specified for transmit}
  ecNoFilename             = 9917;  {No filename specifed in xmodem download}
  ecAsciiReceiveInProgress = 9918;  {Ascii receive in progress}
  ecFileRejected           = 9919;  {Receiver rejected file}
  ecTooManyErrors          = 9920;  {Too many errors received during protocol}
  ecBadFileList            = 9921;  {No end of list marker found in file list}

  {APZMODEM/OOZMODEM status codes}
  ecGotCrcE                = 9925;  {Zmodem - got CrcE DataSubpacket}
  ecGotCrcW                = 9926;  {Zmodem - got CrcW DataSubpacket}
  ecGotCrcQ                = 9927;  {Zmodem - got CrcQ DataSubpacket}
  ecGotCrcG                = 9928;  {Zmodem - got CrcG DataSubpacket}
  ecGarbage                = 9929;  {Zmodem - got garbage from remote}
  ecSkipFile               = 9930;  {Zmodem - skip file}
  ecBadPosition            = 9931;  {Zmodem - bad file position}
  ecFileDoesntExist        = 9932;  {Zmodem - specified file doesn't exist}
  ecCantWriteFile          = 9933;  {Zmodem - not allowed to overwrite file}
  ecFailedToHandshake      = 9934;  {Zmodem - never got proper handshake}
  ecNoFilesToReceive       = 9935;  {Zmodem - no files to receive}
  ecBuffersTooSmall        = 9936;  {ZModem - port buffers too small}
  ecGotHeader              = 9937;  {Zmodem - got a complete header}
  ecNoHeader               = 9938;  {Zmodem - (internal) no header yet}

  {APMODEM/OOMODEM status codes}
  ecUnknownModemResult     = 9940;  {Unexpected char in modem result string}
  ecConnect                = 9941;  {Modem response - CONNECT}
  ecRing                   = 9942;  {Modem response - RING}
  ecNoCarrier              = 9943;  {Modem response - NO CARRIER}
  ecNoDialTone             = 9944;  {Modem response - NO DIALTONE}
  ecBusy                   = 9945;  {Modem response - BUSY}
  ecNoAnswer               = 9947;  {Modem response - NO ANSWER}
  ecRinging                = 9948;  {Modem response - RINGING}
  ecVoice                  = 9949;  {Modem response - VOICE}
  ecError                  = 9950;  {Modem response - ERROR}

  {APMODEM2/OOMODEM2 status codes}
  ecTimeUpd                = 9951;  {Message reporting time remaining}
  ecGotBaud                = 9952;  {Received the baud rate}
  ecGotErrorCorrection     = 9953;  {Received an error correction tag}
  ecGotDataCompression     = 9954;  {Received a data compression tag}
  ecModemBusy              = 9955;  {Modem processing command}
  ecModemNotBusy           = 9956;  {Modem not doing anything}

  {APKERMIT/OOKERMIT status codes}
  ecGotData                = 9954;  {Kermit - got packet}
  ecNoData                 = 9955;  {Kermit - no data yet}

  {Archive status messages}
  ecUnknownMethod          = 9960;  {Unknown compression method}
  ecFileEncrypted          = 9961;  {Cannot extract--file is encrypted}
  ecBadFileCRC             = 9962;  {Bad CRC--file is probably corrupted}
  ecCannotCreate           = 9963;  {Unable to create output file}
  ecBadFileFormat          = 9964;  {Bad archive file format}
  ecNotAnLzhFile           = 9965;  {Not an LZH file}
  ecNotAZipFile            = 9966;  {Not a ZIP file}
  ecEmptyFileMaskList      = 9967;  {File mask list is empty}

  {CaptureTerminalWindow error codes}
  ecScrollBackTooBig       = 9980;  {Scroll back buffer > 64k}

  {APBPLUS/OOBPLUS status codes}
  ecResync                 = 9985;  {Resyncing with host}
  ecWaitACK                = 9986;  {Waiting for ACK}
  ecDropout                = 9987;  {Dropout}
  ecHostCan                = 9988;  {Host cancel}
  ecTryResume              = 9989;  {Attempting resume}
  ecHostResume             = 9990;  {Host resuming}
  ecResumeOK               = 9991;  {Resumed OK}
  ecResumeBad              = 9992;  {Failed to resume}
  ecUnPacket               = 9993;  {Invalid packet type}

  {Error types}
  etFatal          = 0;          {Fatal errors}
  etNonFatal       = 1;          {Non-fatal I/O errors}
  etWarning        = 2;          {Warning messages (currently not used)}
  etMessage        = 3;          {Status information (generally should
                                  not be acted on by an error handler)}

  {Error prefixes}
  epFatal          = etFatal * 10000;
  epNonFatal       = etNonFatal * 10000;
  epWarning        = etWarning * 10000;
  epMessage        = etMessage * 10000;

  MaxActivePort    = 36;

Var
  ActiveComPort    : Array [1..MaxActivePort] Of PortRecPtr;
  InitPort         : InitPortProc;
  InitPortKeep     : InitPortKeepProc;
  DonePort         : DonePortProc;
  SetLine          : SetLineProc;
  GetLine          : GetLineProc;
  SetModem         : SetModemProc;
  GetModem         : GetModemProc;
  GetChar          : GetCharProc;
  PeekChar         : PeekCharProc;
  PutChar          : PutCharProc;
  PutBlock         : PutBlockFunc;
  StartTransmitter : StartTransmitterProc;
  CharReady        : CharReadyFunc;
  TransReady       : TransReadyFunc;
  SendBreak        : SendBreakProc;
  ActivatePort     : ActivatePortProc;
  DeactivatePort   : ActivatePortProc;
  SavePort         : SavePortProc;
  RestorePort      : SavePortProc;

  {Procedure pointers needed by alternate device layers}
  UpdateLineStatus  : UpdateLineStatusFunc;
  UpdateModemStatus : UpdateModemStatusFunc;
  HWFlowSet         : FlowSetProc;
  HWFlowGet         : FlowGetFunc;
  SWFlowSet         : FlowSetProc;
  SWFlowGet         : FlowGetFunc;
  SWFlowCtl         : FlowCtlProc;
  BufferStatus      : BufferStatusProc;
  BufferFlush       : BufferFlushProc;
  SetUart           : SetUartProc;

Function GetMemCheck (Var P; Bytes: Word): Boolean;
Procedure FreeMemCheck (Var P; Bytes: Word);

Function UpdateChecksum (CurByte: Byte; CheckSum: System. Word): System. Word;
Function CheckRange (Value, Low, High: Word): Boolean;

Procedure DateToDMY (Julian: Date; Var Day, Month, Year: Integer);
Procedure TimeToHMS (T: Time; Var Hours, Minutes, Seconds: Byte);
Procedure IncDateTime (Var DT1, DT2: DateTimeRec; Days: Integer; Secs: LongInt);
Procedure DateTimeDiff (DT1, DT2: DateTimeRec; Var Days: Word; Var Secs: LongInt);
Function NoAbortFunc: Boolean;
Procedure NoErrorProc (P: Pointer; Var StatusCode: Word);
Procedure GotError (P: PortRecPtr; StatusCode: Word);

Implementation

Uses
  TMisc;

Function UpdateChecksum (CurByte: Byte; CheckSum: System. Word): System. Word;
  {-Returns an updated checksum}
Begin
  UpdateCheckSum := CheckSum + CurByte;
End;

Function CheckRange (Value, Low, High: Word): Boolean;
  {-Range check value}
Begin
  CheckRange := (Value >= Low) And (Value <= High);
End;

Procedure NoErrorProc (P: Pointer; Var StatusCode: Word);
  {-Dummy error procedure}
Begin
End;

Function NoAbortFunc: Boolean;
  {-Empty abort function}
Begin
  NoAbortFunc := False;
End;

Procedure DateTimeDiff (DT1, DT2: DateTimeRec; Var Days: Word; Var Secs: LongInt);
  {-Return the difference in days and seconds between two points in time}
Var
  DTTemp : DateTimeRec;

Begin
  {swap if DT1 later than DT2}
  If (DT1.D > DT2.D) Or ((DT1.D = DT2.D) And (DT1.T > DT2.T)) Then
  Begin
    DTTemp := DT1;
    DT1 := DT2;
    DT2 := DTTemp;
  End;

  {the difference in days is easy}
  Days := DT2.D - DT1.D;

  {difference in seconds}
  If DT2.T < DT1.T Then
  Begin
    {subtract one day, add 24 hours}
    Dec (Days);
    Inc (DT2.T, SecondsInDay);
  End;

  Secs := DT2.T - DT1.T;
End;

Procedure IncDateTime (Var DT1, DT2: DateTimeRec; Days: Integer; Secs: LongInt);
  {-Increment (or decrement) DT1 by the specified number of days and seconds
    and put the result in DT2}
Begin
  DT2 := DT1;

  {date first}
  Inc (Integer (DT2.D), Days);

  If Secs < 0 Then
  Begin
    {change the sign}
    Secs := -Secs;

    {adjust the date}
    Dec (DT2.D, Secs Div SecondsInDay);
    Secs := Secs Mod SecondsInDay;

    If Secs > DT2.T Then
    Begin
      {subtract a day from DT2.D and add a day's worth of seconds to DT2.T}
      Dec (DT2.D);
      Inc (DT2.T, SecondsInDay);
    End;

    {now subtract the seconds}
    Dec (DT2.T, Secs);
  End
  Else
  Begin
    {increment the seconds}
    Inc (DT2.T, Secs);

    {adjust date if necessary}
    Inc (DT2.D, DT2.T Div SecondsInDay);

    {force time to 0..SecondsInDay-1 range}
    DT2.T := DT2.T Mod SecondsInDay;
  End;
End;

Function GetMemCheck (Var P; Bytes: Word): Boolean;
Var
  Pt : Pointer Absolute P;

Begin
  Pt := Nil;
  GetMem (Pt, Bytes);
  GetMemCheck := Pt <> Nil;
End;

Procedure FreeMemCheck (Var P; Bytes: Word);
  {-Deallocate heap space}
Var
  Pt : Pointer Absolute P;

Begin
  If Pt <> Nil Then
  Begin
    FreeMem (Pt, Bytes);
    Pt := Nil;
  End;
End;

Procedure DateToDMY (Julian: Date; Var Day, Month, Year: Integer);
  {-Convert from a julian date to month, day, year}
Var
  I : LongInt;

Begin
  If Julian = BadDate Then Begin
    Day := 0;
    Month := 0;
    Year := 0;
  End
  Else If Julian <= First2Months Then Begin
    Year := MinYear;
    If Julian <= 30 Then Begin
      Month := 1;
      Day := Succ (Julian);
    End
    Else Begin
      Month := 2;
      Day := Julian - 30;
    End;
  End
  Else Begin
    I := (4 * LongInt (Julian - First2Months)) - 1;
    Year := I Div 1461;
    I := (5 * ((I Mod 1461) Div 4)) + 2;
    Month := I Div 153;
    Day := ((I Mod 153) + 5) Div 5;
    If Month < 10 Then
      Inc (Month, 3)
    Else Begin
      Dec (Month, 9);
      Inc (Year);
    End;
    Inc (Year, MinYear);
  End;
End;

Procedure TimeToHMS (T: Time; Var Hours, Minutes, Seconds: Byte);
  {-Convert a Time variable to Hours, Minutes, Seconds}
Begin
  If T = BadTime Then Begin
    Hours := 0;
    Minutes := 0;
    Seconds := 0;
  End
  Else Begin
    Hours := T Div SecondsInHour;
    Dec (T, LongInt (Hours) * SecondsInHour);
    Minutes := T Div SecondsInMinute;
    Dec (T, LongInt (Minutes) * SecondsInMinute);
    Seconds := T;
  End;
End;

Procedure GotError (P: PortRecPtr; StatusCode: Word);
  {-Called when an error occurs (GotError calls the optional ErrorHandler)}
Begin
  With P^ Do
  Begin
    If ProtocolActive Then
      AsyncStatus := StatusCode Mod 10000
    Else
      AsyncStatus := StatusCode;
    ErrorProc (ErrorData, StatusCode);
  End;
End;

End.
