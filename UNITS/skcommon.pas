{$Q-,I-,F+,X+,R-}
{$IFDEF MSDOS}
  {$IFNDEF DPMI}
  {$IFNDEF DPMI32}
    {$DEFINE RealMode}
    {$O+}
  {$ENDIF}
  {$ENDIF}
{$ENDIF}
{&AlignRec-,Use32-}
unit skCommon;

interface

{$IFDEF DELPHI}
 {$WARNINGS OFF}
 {$HINTS OFF}
{$ENDIF}

{$IFDEF DELPHI}
 {$IFOPT H+}
  {$DEFINE HUGEISON}
 {$ENDIF}
{$ENDIF}

{$IFDEF VIRTUALPASCAL}
 {$IFOPT H+}
  {$DEFINE HUGEISON}
 {$ENDIF}
{$ENDIF}

{$IFNDEF HUGEISON}
 {$DEFINE ASCIIZFIX}
{$ENDIF}

uses
{$IFDEF DELPHI}
     Windows,
     SysUtils;
{$ELSE}
{$IFDEF VIRTUALPASCAL}
     Objects,
     Dos,
     Strings,
     vpSysLow;
{$ELSE}
     Objects,
     Dos,
     Strings;
{$ENDIF}
{$ENDIF}

type
 PBaseType = ^TBaseType;
 TBaseType = (btNetmail, btEchomail, btLocal);

 TMessageBaseFormat = (mbfJam, mbfMSG, mbfSquish, mbfUnknown);

 TSquishAddress = packed record
  Zone, Net, Node, Point: System.Word;
 end;

 TSquishMessageReplies = array[1..9] of LongInt;

 TSquishBaseHeader = packed record
  Len: System.Word;
  Rsvd1: System.Word;
  NumMsg: Longint;
  HighMsg: Longint;
  SkipMsg: Longint;
  HighWater: Longint;
  UID: Longint;
  Base: array[1..80] of Char;
  FirstFrame: Longint;
  LastFrame: Longint;
  FirstFree: Longint;
  LastFree: Longint;
  EndFrame: Longint;
  MaxMsg: Longint;
  KeepDays: System.Word;
  SqHdrSize: System.Word;
  Rsvd2: array[1..124] of Byte;
 end;

 TSquishMessageHeader = packed record
  Attr: LongInt;
  MsgFrom: array[1..36] of Char;
  MsgTo: array[1..36] of Char;
  Subj: array[1..72] of Char;
  Orig: TSquishAddress;
  Dest: TSquishAddress;
  DateWritten: Longint;
  DateArrived: Longint;
  UTCoffset: System.Word;
  ReplyTo: Longint;
  Replies: TSquishMessageReplies;
  UID: Longint;
  AzDate: array[1..20] of Char;
 end;

 TSquishIndex = packed record
  Offset: Longint;
  Number: Longint;
  Hash: Longint;
 end;

 TSquishFrame = packed record
  Id: Longint;
  NextFrame: Longint;
  PrevFrame: Longint;
  FrameLength: Longint;
  MsgLength: Longint;
  ControlLength: Longint;
  FrameType: System.Word;
  Rsvd: System.Word;
 end;

 TJamMessageHeaderFirst = packed record
  Signature: array[1..4] of Char;
  Rev: System.Word;
  Resvd: System.Word;
  SubFieldLen: Longint;
  TimesRead: Longint;
  MsgIdCrc: Longint;
  ReplyCrc: Longint;
  ReplyTo: Longint;
  ReplyFirst: Longint;
  ReplyNext: Longint;
  DateWritten: Longint;
  DateRcvd: Longint;
  DateArrived: Longint;
  MsgNum: Longint;
  Attr1: Longint;
  Attr2: Longint;
  TextOfs: Longint;
  TextLen: Longint;
  PwdCrc: Longint;
  Cost: Longint;
 end;

 TJamIndex = packed record
  MsgToCrc: Longint;
  HdrLoc: Longint;
 end;

 TJamBaseHeader = packed record
  Signature: array[1..4] of Char;
  Created: Longint;
  ModCounter: Longint;
  ActiveMsgs: Longint;
  PwdCRC: Longint;
  BaseMsgNum: Longint;
  Extra: array[1..1000] of Char;
 end;

 PFidoHeader     = ^TFidoHeader;
 TFidoHeader     = packed record
  FromUser   : array[1..36] of Char;
  ToUser     : array[1..36] of Char;
  Subject    : array[1..72] of Char;
  DateTime   : array[1..20] of Char;
  TimesRead  : System.Word;
  DestNode   : System.Word;
  OrigNode   : System.Word;
  Cost       : System.Word;
  OrigNet    : System.Word;
  DestNet    : System.Word;
  DateWritten: Longint;
  DateArrived: Longint;
  ReplyTo    : System.Word;
  Attr       : System.Word;
  ReplyNext  : System.Word;
 end;

const
{ The internal constants for work with attributes: }
 maPrivate      = $00000001;
 maCrash        = $00000002;   { !!! after changing of   !!! }
 maReceived     = $00000004;   { !!! these constants     !!! }
 maSent         = $00000008;   { !!! sk must immediately !!! }
 maAttach       = $00000010;   { !!! update fp_post2     !!! }
 maTransit      = $00000020;   { !!! and fp_post3!!!!    !!! }
 maOrphan       = $00000040;
 maKill         = $00000080;
 maLocal        = $00000100;
 maHold         = $00000200;
 maFRq          = $00000400;
 maRRq          = $00000800;
 maRRc          = $00001000;
 maARq          = $00002000;
 maURq          = $00004000;
 maScanned      = $00008000; { squish only }

{ Flags of a message base are defined by means of the following constants: }
 afNetmail      = $00000001;
 afEchomail     = $00000002;
 afLocal        = $00000004;

{ The following constants are defining the size of a temporary buffers which
  will be created while working. }
{$IFDEF RealMode}
 MaxLineSize    : Longint = $1000;
{$ELSE}
 MaxLineSize    : Longint = $4000;
{$ENDIF}
 MaxMessageSize : Longint = $10000;

{ errors }

const
 fmbWrongPath           = $1000;
 fmbMessageNotFound     = $1001;
 fmbCannotCreateStream  = $1002;
 fmbAlreadyClosed       = $1003;
 fmbBufferOverflow      = $1004;

 jmbAbsentJHR           = $2000;
 jmbAbsentJDX           = $2001;
 jmbAbsentJDT           = $2002;
 jmbCantOpenJHR         = $2003;
 jmbCantOpenJDX         = $2004;
 jmbCantOpenJDT         = $2005;
 jmbStreamProblems      = $2006;
 jmbWrongHeaderLocation = $2007;
 jmbWrongDataLocation   = $2008;
 jmbUnknownSubField     = $2009;
 jmbCantCreateJHR       = $200A;
 jmbCantCreateJDX       = $200B;
 jmbCantCreateJDT       = $200C;
 jmbAbsentJLR           = $200D;
 jmbCantOpenJLR         = $200E;
 jmbCantCreateJLR       = $200F;

 smbAbsentSQD           = $3000; { obsolete }
 smbAbsentSQI           = $3001; { obsolete }
 smbCantOpenSQD         = $3002;
 smbCantOpenSQI         = $3003;
 smbIndexProblems       = $3004;
 smbUnsupported         = $3005;
 smbMessageIsNotExists  = $3006;
 smbGrungedFrame        = $3007;
 smbDataProblems        = $3008;
 smbCantCreateSQD       = $3009;
 smbCantCreateSQI       = $300A;
 smbBaseHeaderProblems  = $300B;

 ombUnknownIDorUnsupported      = $0100;
 ombLocked                      = $0101;
 ombLockedAttemptsExpired       = $0102;

const
 smCreate       = $0001;
 smOpen         = $0002;
 smOpenRead     = $0004;
 smOpenWrite    = $0008;

 smDenyAll      = $1000; { 16 }
 smDenyWrite    = $2000; { 32 }
 smDenyRead     = $3000; { 16 + 32 }
 smDenyNone     = $4000; { 64 }

 smOk           = $00;

 faReadOnly     = $01;
 faHidden       = $02;
 faSysFile      = $04;
 faVolumeID     = $08;
 faDirectory    = $10;
 faArchive      = $20;
 faAnyFile      = $3F;

 DefaultZone    : System.Integer = 2;

type
 TCharSet = set of char;

 TDosDateTime = packed record
  Time, Date: System.Word;
 end;

 PAddress = ^TAddress;
 TAddress = packed record
  Zone, Net, Node, Point: System.Integer;
 end;

 PMessageBaseDateTime = ^TMessageBaseDateTime;
 TMessageBaseDateTime = packed record
  Hour, Min, Sec, Sec100, Day, Month, Year: Word;
 end;

 PMessageBaseStream = ^TMessageBaseStream;
 TMessageBaseStream = object
  Status, Mode: Longint;
  FileName: String;

  constructor Init(const AFileName: String; const AMode: Longint);
  destructor Done; virtual;

  procedure CopyFrom(var S: TMessageBaseStream; Count: Longint); virtual;

  procedure Read(var Buf; Count: Word); virtual;
  procedure Write(var Buf; Count: Word); virtual;
  procedure Seek(Position: Longint); virtual;
  function GetPos: Longint; virtual;
  function GetSize: Longint; virtual;
  procedure Flush; virtual;
  procedure Truncate; virtual;
  procedure Reset; virtual;
 end;

 PMessageBaseFind = ^TMessageBaseFind;
 TMessageBaseFind = object
  iName: String;
  iAttributes: Longint;
  iTime: Longint;
  iSize: Longint;
  constructor Init;
  function StartSearch(const Mask: String; Attributes: Longint): Boolean; virtual;
  function NextSearch: Boolean; virtual;
  procedure StopSearch; virtual;
  destructor Done; virtual;
 end;

 TMessageBaseRegisterFileOpen = procedure(const FileName: String; Mode: Longint; const Stream: PMessageBaseStream);
 TMessageBaseRegisterFileClose = procedure(const Stream: PMessageBaseStream);

procedure Abstract;
procedure DefaultRegisterFileOpen(const FileName: String; Mode: Longint; const Stream: PMessageBaseStream);
procedure DefaultRegisterFileClose(const Stream: PMessageBaseStream);

{$IFDEF DELPHI}
 {$I mhlDi.inc}                 { Delphi objects interface }
{$ELSE}
 {$I mhlTVi.inc}                { TV objects interface (VP/BP) }
{$ENDIF}

const
 RegisterFileOpen: TMessageBaseRegisterFileOpen = DefaultRegisterFileOpen;
 RegisterFileClose: TMessageBaseRegisterFileClose = DefaultRegisterFileClose;

 AddressCharSet: TCharSet = [':', '/', '.'];

 { PatchAddress flags }
 paZone         = $01;
 paNet          = $02;
 paNode         = $04;
 paPoint        = $08;

 paZoneNetNode  = paZone or paNet or paNode;
 paAll          = paZone or paNet or paNode or paPoint;

const
 NullAddress: TAddress = (Zone: 0; Net: 0; Node: 0; Point: 0);

var
 StrToAddressRPool: array[1..16] of TAddress;

const
 StrToAddressRPos: Byte = 0;

{$IFDEF DELPHI}
function FExpand(const Path: String): String;
{$ENDIF}

function DirectoryExists(const Pathname: String): Boolean;
function FileExists(const Filename: String): Boolean;
function KillFile(const Filename: String): Boolean;
function RenameFile(const OldFilename, NewFilename: String): Boolean;
function LongToStr(const Number: Longint): String;
function StrToInteger(const S: String; var I: System.Integer): Boolean;
procedure StrToWord(const S: String; var I: Word);
function StrToNumber(const S: String): Boolean;
function ExtractWord(N: Byte; const S: String; WordDelims: TCharSet): String;
function CharRPos(C: Char; const S: String): Byte;

procedure ToASCIIZ(const Source: String; const Destination: Pointer);
function FromASCIIZ(const Source: Pointer): String;
procedure ConcatASCIIZ(const Destination, Source: Pointer);
function LenASCIIZ(const Source: Pointer): Word;
function PosASCIIZ(const SubString, Source: Pointer): Pointer;
function CompLASCIIZ(const Str1, Str2: Pointer; MaxLen: Word): Integer;

function AddressToStr(const Address: TAddress): String;
function AddressToStrPointless(const Address: TAddress): String;
function AddressToStrEx(const Address: TAddress): String;
function StrToAddress(Source: String; var Address: TAddress): Boolean;
function StrToAddressR(const Source: String): PAddress;
procedure ClearAddress(var Address: TAddress);
function IsCleanAddress(var Address: TAddress): Boolean;
function AddressCompare(const First, Second: TAddress): Longint;
function AddressEqu(const First, Second: TAddress): Boolean;
procedure PatchAddress(const Flags: Longint; const Valid: TAddress; var Partial: TAddress);

procedure ClearMessageBaseDateTime(var DateTime: TMessageBaseDateTime);
function IsRealMessageBaseDateTime(var DateTime: TMessageBaseDateTime): Boolean;
function IsValidMessageBaseDateTime(var DateTime: TMessageBaseDateTime): Boolean;
procedure GetCurrentMessageBaseDateTime(var DateTime: TMessageBaseDateTime);
procedure MessageBaseDateTimeToDosDateTime(var DateTime: TMessageBaseDateTime; var DosDateTime: Longint);
procedure DosDateTimeToMessageBaseDateTime(var DosDateTime: Longint; var DateTime: TMessageBaseDateTime);
function GregorianToJulian(DateTime: TMessageBaseDateTime): LongInt;
procedure JulianToGregorian(JulianDN: LongInt; var Year, Month, Day: Word);
procedure UnixDateTimeToMessageBaseDateTime(SecsPast: LongInt; var DateTime: TMessageBaseDateTime);
procedure MessageBaseDateTimeToUnixDateTime(const DateTime: TMessageBaseDateTime; var SecsPast: Longint);
procedure MessageBaseDateTimeToMSGDateTime(const DT: TMessageBaseDateTime; var L: Longint);
procedure MSGDateTimeToMessageBaseDateTime(const A: Longint; var DT: TMessageBaseDateTime);
function MessageBaseDateTimeCompare(const First, Second: TMessageBaseDateTime): Integer;

procedure MonthStringToMonthNumber(S: String; var Month: Word);
function MonthNumberToMonthString(const Month: Word): String;

function GenerateMSGID: String;

function UpdateCRC32(Octet: Byte; Crc: Longint): Longint;
function StringCRC32(const S: String; Crc: Longint): Longint;
function PCharCRC32(S: PChar; Crc: Longint): Longint;
function LoCase(Ch: Char): Char;

function CreateDirectory(Directory: String): Boolean;
function ExtractPathname(S: String): String;

function Y2ToDouble(const Year: Longint): Longint;
function DoubleToY2(const Year: Longint): Longint;

function ExplainStatus(const Status: Longint): String;

{ wrapped from mhl*c.inc }

function CreateMessageBaseFileStream(const FileName: String; Mode: Longint): PMessageBaseStream;
function CreateMessageBaseMemoryStream(Maximum: Longint): PMessageBaseStream;
function CreateMessageBaseFind: PMessageBaseFind;

implementation

{$IFDEF DELPHI}
 {$I mhlDc.inc}                 { Delphi objects code }
{$ELSE}
 {$I mhlTVc.inc}                { TV objects code (VP/BP) }
{$ENDIF}

{* Internal constants *}

const
 mfdDay        = $001F;
 mfdMonth      = $01E0;
 mfdYear       = $FE00;
 mftTwoSecs    = $001F;
 mftMinutes    = $07E0;
 mftHours      = $F800;
 sfdDay        = 0;
 sfdMonth      = 5;
 sfdYear       = 9;
 sftTwoSecs    = 0;
 sftMinutes    = 5;
 sftHours      = 11;

 C1970         = 2440588;
 D0            = 1461;
 D1            = 146097;
 D2            = 1721119;
 SecsDelta     = 2145916800;

{* Common stuff *}

procedure Abstract;
 begin
  RunError(217);
 end;

procedure DefaultRegisterFileOpen(const FileName: String; Mode: Longint; const Stream: PMessageBaseStream);
 begin
 end;

procedure DefaultRegisterFileClose(const Stream: PMessageBaseStream);
 begin
 end;

{* TMessageBaseStream *}

constructor TMessageBaseStream.Init(const AFileName: String; const AMode: Longint);
 begin
  Status:=0;

  FileName:=AFileName;

  Mode:=AMode;

  RegisterFileOpen(AFileName, AMode, @Self);
 end;

destructor TMessageBaseStream.Done;
 begin
  RegisterFileClose(@Self);
 end;

procedure TMessageBaseStream.CopyFrom(var S: TMessageBaseStream; Count: Longint);
 var
  N: Word;
  Buffer: Array[0..1023] Of Byte;
 begin
  while Count > 0 do
   begin
    if Count > SizeOf(Buffer) then
     N:=SizeOf(Buffer)
    else
     N:=Count;

    S.Read(Buffer, N);

    Write(Buffer, N);

    Dec(Count, N);
   end;
 end;

procedure TMessageBaseStream.Read(var Buf; Count: Word);
 begin
  Abstract;
 end;

procedure TMessageBaseStream.Write(var Buf; Count: Word);
 begin
  Abstract;
 end;

procedure TMessageBaseStream.Seek(Position: Longint);
 begin
  Abstract;
 end;

function TMessageBaseStream.GetPos: Longint;
 begin
  Abstract;
 end;

function TMessageBaseStream.GetSize: Longint;
 begin
  Abstract;
 end;

procedure TMessageBaseStream.Flush;
 begin
  Abstract;
 end;

procedure TMessageBaseStream.Truncate;
 begin
  Abstract;
 end;

procedure TMessageBaseStream.Reset;
 begin
  Abstract;
 end;

{* TMessageBaseFind *}

constructor TMessageBaseFind.Init;
 begin
 end;

function TMessageBaseFind.StartSearch(const Mask: String; Attributes: Longint): Boolean;
 begin
  Abstract;
 end;

function TMessageBaseFind.NextSearch: Boolean;
 begin
  Abstract;
 end;

procedure TMessageBaseFind.StopSearch;
 begin
  Abstract;
 end;

destructor TMessageBaseFind.Done;
 begin
 end;

{* Delphi compatibility *}

{$IFDEF DELPHI}
function FExpand(const Path: String): String;
 begin
  FExpand:=ExpandFileName(Path);
 end;
{$ENDIF}

{* Another common stuff *}

function DirectoryExists(const Pathname: String): Boolean;
 var
  Find: PMessageBaseFind;
 begin
  Find:=CreateMessageBaseFind;

  if Pathname[Length(PathName)] <> '\' then
   DirectoryExists:=Find^.StartSearch(Pathname + '\*.*', faAnyFile)
  else
   DirectoryExists:=Find^.StartSearch(Pathname + '*.*', faAnyFile);

  Find^.StopSearch;

  Dispose(Find, Done);
 end;

function FileExists(const Filename: String): Boolean;
{$IFDEF DELPHI} // strange D3 bug workaround (??? why not TMBF ???)
 begin
  FileExists:=SysUtils.FileExists(Filename);
 end;
{$ELSE}
 var
  Find: PMessageBaseFind;
 begin
  Find:=CreateMessageBaseFind;

  FileExists:=Find^.StartSearch(Filename, faAnyFile);

  Find^.StopSearch;

  Dispose(Find, Done);
 end;
{$ENDIF}

function KillFile(const Filename: String): Boolean;
 var
  F: System.Text;
 begin
  if IOResult <> 0 then;

  Assign(F, Filename);
  Erase(F);

  KillFile:=IOResult = 0;
 end;

function RenameFile(const OldFilename, NewFilename: String): Boolean;
 var
  F: Text;
 begin
  if IOResult <> 0 then;

  Assign(F, OldFilename);

  Rename(F, NewFilename);

  RenameFile:=IOResult = 0;
 end;

function LongToStr(const Number: Longint): String;
 var
  S: String;
 begin
  Str(Number, S);

  LongToStr:=S;
 end;

function StrToNumber(const S: String): Boolean;
 var
  I: Longint;
{$IFDEF VIRTUALPASCAL}
  C: Longint;
{$ELSE}
  C: Integer;
{$ENDIF}
 begin
  Val(S, I, C);

  StrToNumber:=C = 0;
 end;

function StrToInteger(const S: String; var I: System.Integer): Boolean;
 var
  V: System.Integer;
{$IFDEF VIRTUALPASCAL}
  C: Longint;
{$ELSE}
  C: Integer;
{$ENDIF}
 begin
  if S = '' then
   begin
    I:=0;

    StrToInteger:=True;
   end
  else
   begin
    Val(S, V, C);

    if C = 0 then
     begin
      I:=V;

      StrToInteger:=True;
     end
    else
     StrToInteger:=False;
   end;
 end;

procedure StrToWord(const S: String; var I: Word);
 var
{$IFDEF VIRTUALPASCAL}
  C: Longint;
{$ELSE}
  C: Integer;
{$ENDIF}
 begin
  Val(S, I, C);
 end;

function ExtractWord(N: Byte; const S: String; WordDelims: TCharSet): String;
 var
  I: Word;
  Count, Len: Byte;
  SLen: Byte absolute S;
 begin
  Count:=0;
  I:=1;
  Len:=0;
  {$IFDEF DELPHI}
  SetLength(Result, 0);
  {$ELSE}
  ExtractWord[0]:=#0;
  {$ENDIF}

  while (I <= SLen) and (Count <> N) do
   begin
    while (I <= SLen) and (S[I] in WordDelims) do
     Inc(I);

    if I <= SLen then
     Inc(Count);

    while (I <= SLen) and not(S[I] in WordDelims) do
     begin
      if Count = N then
       begin
        Inc(Len);
        {$IFDEF DELPHI}
        SetLength(Result, Len);
        {$ELSE}
        ExtractWord[0]:=Char(Len);
        {$ENDIF}
        ExtractWord[Len]:=S[I];
       end;
      Inc(I);
     end;
   end;
 end;

function CharRPos(C: Char; const S: String): Byte;
 var
  I: Byte;
 begin
  for I:=Length(S) downto 1 do
   if S[I] = C then
    begin
     CharRPos:=I;
     Exit;
    end;
  CharRPos:=0;
 end;

procedure ToASCIIZ(const Source: String; const Destination: Pointer);
 begin
  StrPCopy(Destination, Source);
 end;

function FromASCIIZ(const Source: Pointer): String;
 {$IFDEF ASCIIZFIX}
 var
  S: String;
 {$ENDIF}
 begin
  {$IFDEF ASCIIZFIX}
  if LenASCIIZ(Source) > SizeOf(String) - 1 then
   begin
    Move(Source^, S[1], SizeOf(String) - 1);

    {$IFDEF DELPHI}
    SetLength(S, 255);
    {$ELSE}
    S[0]:=#255;
    {$ENDIF}

    FromASCIIZ:=S;
   end
  else
   FromASCIIZ:=StrPas(Source);
  {$ELSE}
  FromASCIIZ:=StrPas(Source);
  {$ENDIF}
 end;

procedure ConcatASCIIZ(const Destination, Source: Pointer);
 begin
  StrCat(Destination, Source);
 end;

function LenASCIIZ(const Source: Pointer): Word;
 begin
  LenASCIIZ:=StrLen(Source);
 end;

function PosASCIIZ(const SubString, Source: Pointer): Pointer;
 begin
  PosASCIIZ:=StrPos(Source, SubString);
 end;

function CompLASCIIZ(const Str1, Str2: Pointer; MaxLen: Word): Integer;
 begin
  CompLASCIIZ:=StrLComp(Str1, Str2, MaxLen);
 end;

function AddressToStr(const Address: TAddress): String;
 begin
  AddressToStr:=
    LongToStr(Address.Zone) + ':' +
    LongToStr(Address.Net) + '/' +
    LongToStr(Address.Node) + '.' +
    LongToStr(Address.Point);
 end;

function AddressToStrPointless(const Address: TAddress): String;
 begin
  AddressToStrPointless:=
    LongToStr(Address.Zone) + ':' +
    LongToStr(Address.Net) + '/' +
    LongToStr(Address.Node);
 end;

function AddressToStrEx(const Address: TAddress): String;
 begin
  if Address.Point = 0 then
   AddressToStrEx:=AddressToStrPointless(Address)
  else
   AddressToStrEx:=AddressToStr(Address);
 end;

function StrToAddress(Source: String; var Address: TAddress): Boolean;
 begin
  StrToAddress:=False;

  if Source = '' then
   Exit;

  if Source[1] in ['/', ':'] then
   Delete(Source, 1, 1);

  if Pos('@', Source) <> 0 then
   Source:=Copy(Source, 1, Pos('@', Source) - 1);

  if Source = '' then
   Exit;

  if Pos(':', Source) = 0 then
   if Pos('/', Source) = 0 then
    if Pos('.', Source) = 1 then
     begin
      Delete(Source, 1, 1);

      StrToAddress:=StrToInteger(Source, Address.Point);
     end
    else
     begin
      StrToAddress:=StrToInteger(ExtractWord(1, Source, ['.']), Address.Node) and
                    StrToInteger(ExtractWord(2, Source, ['.']), Address.Point);
     end
   else
    begin
     StrToAddress:=StrToInteger(ExtractWord(1, Source, ['/']), Address.Net) and
                   StrToInteger(ExtractWord(2, Source, ['/', '.']), Address.Node) and
                   StrToInteger(ExtractWord(3, Source, ['/', '.']), Address.Point);
    end
   else
  if Pos('/', Source) <> 0 then
   begin
    StrToAddress:=StrToInteger(ExtractWord(1, Source, AddressCharSet), Address.Zone) and
                  StrToInteger(ExtractWord(2, Source, AddressCharSet), Address.Net) and
                  StrToInteger(ExtractWord(3, Source, AddressCharSet), Address.Node) and
                  StrToInteger(ExtractWord(4, Source, AddressCharSet), Address.Point);
   end
  else
   StrToAddress:=StrToInteger(ExtractWord(1, Source, AddressCharSet), Address.Zone) and
                 StrToInteger(ExtractWord(2, Source, AddressCharSet), Address.Net);
 end;

function StrToAddressR(const Source: String): PAddress;
 begin
  Inc(StrToAddressRPos);

  if StrToAddressRPos > High(StrToAddressRPool) then
   StrToAddressRPos:=Low(StrToAddressRPool);

  ClearAddress(StrToAddressRPool[StrToAddressRPos]);

  StrToAddress(Source, StrToAddressRPool[StrToAddressRPos]);

  StrToAddressR:=@StrToAddressRPool[StrToAddressRPos];
 end;

procedure ClearAddress(var Address: TAddress);
 begin
  FillChar(Address, SizeOf(Address), 0);
 end;

function IsCleanAddress(var Address: TAddress): Boolean;
 begin
  IsCleanAddress:=(Address.Zone = 0) and (Address.Net = 0) and (Address.Node = 0) and (Address.Point = 0);
 end;

function AddressCompare(const First, Second: TAddress): Longint;
 begin
  if First.Zone < Second.Zone then AddressCompare:=-1 else
  if First.Zone > Second.Zone then AddressCompare:=1 else
   if First.Net < Second.Net then AddressCompare:=-1 else
   if First.Net > Second.Net then AddressCompare:=1 else
    if First.Node < Second.Node then AddressCompare:=-1 else
    if First.Node > Second.Node then AddressCompare:=1 else
     if First.Point < Second.Point then AddressCompare:=-1 else
     if First.Point > Second.Point then AddressCompare:=1 else
      AddressCompare:=0;
 end;

function AddressEqu(const First, Second: TAddress): Boolean;
 begin
  AddressEqu:=(First.Zone = Second.Zone) and
              (First.Net = Second.Net) and
              (First.Node = Second.Node) and
              (First.Point = Second.Point);
 end;

procedure PatchAddress(const Flags: Longint; const Valid: TAddress; var Partial: TAddress);
 begin
  if (Flags and paZone = paZone) and (Partial.Zone = 0) then Partial.Zone:=Valid.Zone;
  if (Flags and paNet = paNet) and (Partial.Net = 0) then Partial.Net:=Valid.Net;
  if (Flags and paNode = paNode) and (Partial.Node = 0) then Partial.Node:=Valid.Node;
  if (Flags and paPoint = paPoint) and (Partial.Point = 0) then Partial.Point:=Valid.Point;
 end;

procedure ClearMessageBaseDateTime(var DateTime: TMessageBaseDateTime);
 begin
  FillChar(DateTime, SizeOf(DateTime), 0);
 end;

function IsRealMessageBaseDateTime(var DateTime: TMessageBaseDateTime): Boolean;
 begin
  IsRealMessageBaseDateTime:=DateTime.Year >= 1980;
 end;

function IsValidMessageBaseDateTime(var DateTime: TMessageBaseDateTime): Boolean;
 begin
  with DateTime do
   IsValidMessageBaseDateTime:=(Year >= 1980) and
                               (Month >= 1) and (Month <= 12) and
                               (Day >= 1) and (Day <= 31) and
                               (Hour >= 0) and (Hour <= 23) and
                               (Min >= 0) and (Min <= 59) and
                               (Sec >= 0) and (Sec <= 59);
 end;

procedure GetCurrentMessageBaseDateTime(var DateTime: TMessageBaseDateTime);
 var
{$IFDEF VIRTUALPASCAL}
  Year, Month, Day, Dow, Hour, Min, Sec, Sec100: Longint;
{$ELSE}
  Year, Month, Day, {$IFNDEF DELPHI}Dow,{$ENDIF} Hour, Min, Sec, Sec100: Word;
{$ENDIF}
 begin
  {$IFDEF DELPHI}
   DecodeDate(Date, Year, Month, Day);
   DecodeTime(Time, Hour, Min, Sec, Sec100);
  {$ELSE}
   Dos.GetDate(Year, Month, Day, Dow);
   Dos.GetTime(Hour, Min, Sec, Sec100);
  {$ENDIF}
  DateTime.Year:=Year;
  DateTime.Month:=Month;
  DateTime.Day:=Day;
  DateTime.Hour:=Hour;
  DateTime.Min:=Min;
  DateTime.Sec:=Sec;
  DateTime.Sec100:=Sec100;
 end;

procedure MessageBaseDateTimeToDosDateTime(var DateTime: TMessageBaseDateTime; var DosDateTime: Longint);
 var
  DT: TDosDateTime absolute DosDateTime;
 begin
  with DateTime, DT do
   begin
    Date:=(Year - 1980) shl sfdYear + Month shl sfdMonth + Day shl sfdDay;
    Time:=Hour shl sftHours + Min shl sftMinutes + (Sec div 2) shl sftTwoSecs;
   end;
 end;

procedure DosDateTimeToMessageBaseDateTime(var DosDateTime: Longint; var DateTime: TMessageBaseDateTime);
 var
  DT: TDosDateTime absolute DosDateTime;
 begin
  with DateTime, Dt do
   begin
    Year:=(Date and mfdYear) shr sfdYear + 1980;
    Month:=(Date and mfdMonth) shr sfdMonth;
    Day:=(Date and mfdDay) shr sfdDay;
    Hour:=(Time and mftHours) shr sftHours;
    Min:=(Time and mftMinutes) shr sftMinutes;
    Sec:=((Time and mftTwoSecs) shr sftTwoSecs) * 2;
   end;
 end;

function GregorianToJulian(DateTime: TMessageBaseDateTime): LongInt;
 var
  Century: LongInt;
  XYear: LongInt;
  Month: LongInt;
 begin
  Month:=DateTime.Month;
  if Month <= 2 then
   begin
    Dec(DateTime.Year);
    Inc(Month, 12);
   end;
  Dec(Month, 3);
  Century:=DateTime.Year Div 100;
  XYear:=DateTime.Year Mod 100;
  Century:=(Century * D1) shr 2;
  XYear:=(XYear * D0) shr 2;
  GregorianToJulian:=((((Month * 153) + 2) div 5) + DateTime.Day) + D2 + XYear + Century;
 end;

procedure JulianToGregorian(JulianDN: LongInt; var Year, Month, Day: Word);
 var
  Temp, XYear: LongInt;
  YYear, YMonth, YDay: Integer;
 begin
  Temp:=(((JulianDN - D2) shl 2) - 1);
  XYear:=(Temp mod D1) or 3;
  JulianDN:=Temp div D1;
  YYear:=(XYear div D0);
  Temp:=((((XYear mod D0) + 4) shr 2) * 5) - 3;
  YMonth:=Temp div 153;

  if YMonth >= 10 then
   begin
    YYear:=YYear + 1;
    YMonth:=YMonth - 12;
   end;

  YMonth:=YMonth + 3;
  YDay:=Temp mod 153;
  YDay:=(YDay + 5) div 5;
  Year:=YYear + (JulianDN * 100);
  Month:=YMonth;
  Day:=YDay;
 end;

procedure UnixDateTimeToMessageBaseDateTime(SecsPast: LongInt; var DateTime: TMessageBaseDateTime);
 var
  DateNum: LongInt;
  Year, Month, Day: Word;
 begin
  if SecsPast < 0 then
  begin
   Dec(SecsPast, SecsDelta);
   Datenum:=(SecsPast div 86400) + c1970 + (SecsDelta div 86400);
  end else
   Datenum:=(SecsPast div 86400) + c1970;

  JulianToGregorian(DateNum, Year, Month, Day);

  DateTime.Year:=Year;
  DateTime.Month:=Month;
  DateTime.Day:=Day;

  SecsPast:=SecsPast mod 86400;
  DateTime.Hour:=SecsPast div 3600;
  SecsPast:=SecsPast mod 3600;
  DateTime.Min:=SecsPast div 60;
  DateTime.Sec:=SecsPast mod 60;
 end;

procedure MessageBaseDateTimeToUnixDateTime(const DateTime: TMessageBaseDateTime; var SecsPast: Longint);
 var
  DaysPast: LongInt;
 begin
  DaysPast:=GregorianToJulian(DateTime) - c1970;
  SecsPast:=DaysPast * 86400;
  SecsPast:=SecsPast + (LongInt(DateTime.Hour) * 3600) + (DateTime.Min * 60) + (DateTime.Sec);
 end;

procedure MonthStringToMonthNumber(S: String; var Month: Word);
 var
  K: Byte;
 const
  MonthStrings: Array[1..12] Of String[3] = ('JAN', 'FEB', 'MAR', 'APR',
   'MAY', 'JUN', 'JUL', 'AUG', 'SEP', 'OCT', 'NOV', 'DEC');
 begin
  for K:=1 to Length(S) do
   S[K]:=UpCase(S[K]);

  for K:=1 to 12 do
   if MonthStrings[K] = S then
    begin
     Month:=K;
     Exit;
    end;

  Month:=0;
 end;

function MonthNumberToMonthString(const Month: Word): String;
 const
  MonthStrings: Array[1..12] Of String[3] = ('Jan', 'Feb', 'Mar', 'Apr',
   'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec');
 begin
  if (Month > 0) and (Month < 13) then
   MonthNumberToMonthString:=MonthStrings[Month]
  else
   MonthNumberToMonthString:='???';
 end;

function LongintToHex(const Stamp: Longint): String;
 const
  Digits: array[$0..$F] of Char = '0123456789abcdef';
 var
  StampW: packed record L, H: System.Word end absolute Stamp;
 begin
  LongintToHex:=Digits[Hi(StampW.H) shr 4] +
                Digits[Hi(StampW.H) and $F] +
                Digits[Lo(StampW.H) shr 4] +
                Digits[Lo(StampW.H) and $F] +
                Digits[Hi(StampW.L) shr 4] +
                Digits[Hi(StampW.L) and $F] +
                Digits[Lo(StampW.L) shr 4] +
                Digits[Lo(StampW.L) and $F];
 end;

function GenerateMSGID: String;
 const
  OldMSGID: Longint = 0;
 var
  DateTime: TMessageBaseDateTime;
  Stamp: Longint;
 begin
  repeat
   GetCurrentMessageBaseDateTime(DateTime);
   MessageBaseDateTimeToDosDateTime(DateTime, Stamp);
  until OldMSGID <> Stamp;

  OldMSGID:=Stamp;

  GenerateMSGID:=LongintToHex(Stamp);
 end;

const
{$IFDEF DELPHI}
 CRC32_Table: array[0..255] of ULong = (
{$ELSE}
 CRC32_Table: array[0..255] of Longint = (
{$ENDIF}
   $00000000, $77073096, $ee0e612c, $990951ba, $076dc419, $706af48f, $e963a535, $9e6495a3,
   $0edb8832, $79dcb8a4, $e0d5e91e, $97d2d988, $09b64c2b, $7eb17cbd, $e7b82d07, $90bf1d91,
   $1db71064, $6ab020f2, $f3b97148, $84be41de, $1adad47d, $6ddde4eb, $f4d4b551, $83d385c7,
   $136c9856, $646ba8c0, $fd62f97a, $8a65c9ec, $14015c4f, $63066cd9, $fa0f3d63, $8d080df5,
   $3b6e20c8, $4c69105e, $d56041e4, $a2677172, $3c03e4d1, $4b04d447, $d20d85fd, $a50ab56b,
   $35b5a8fa, $42b2986c, $dbbbc9d6, $acbcf940, $32d86ce3, $45df5c75, $dcd60dcf, $abd13d59,
   $26d930ac, $51de003a, $c8d75180, $bfd06116, $21b4f4b5, $56b3c423, $cfba9599, $b8bda50f,
   $2802b89e, $5f058808, $c60cd9b2, $b10be924, $2f6f7c87, $58684c11, $c1611dab, $b6662d3d,
   $76dc4190, $01db7106, $98d220bc, $efd5102a, $71b18589, $06b6b51f, $9fbfe4a5, $e8b8d433,
   $7807c9a2, $0f00f934, $9609a88e, $e10e9818, $7f6a0dbb, $086d3d2d, $91646c97, $e6635c01,
   $6b6b51f4, $1c6c6162, $856530d8, $f262004e, $6c0695ed, $1b01a57b, $8208f4c1, $f50fc457,
   $65b0d9c6, $12b7e950, $8bbeb8ea, $fcb9887c, $62dd1ddf, $15da2d49, $8cd37cf3, $fbd44c65,
   $4db26158, $3ab551ce, $a3bc0074, $d4bb30e2, $4adfa541, $3dd895d7, $a4d1c46d, $d3d6f4fb,
   $4369e96a, $346ed9fc, $ad678846, $da60b8d0, $44042d73, $33031de5, $aa0a4c5f, $dd0d7cc9,
   $5005713c, $270241aa, $be0b1010, $c90c2086, $5768b525, $206f85b3, $b966d409, $ce61e49f,
   $5edef90e, $29d9c998, $b0d09822, $c7d7a8b4, $59b33d17, $2eb40d81, $b7bd5c3b, $c0ba6cad,
   $edb88320, $9abfb3b6, $03b6e20c, $74b1d29a, $ead54739, $9dd277af, $04db2615, $73dc1683,
   $e3630b12, $94643b84, $0d6d6a3e, $7a6a5aa8, $e40ecf0b, $9309ff9d, $0a00ae27, $7d079eb1,
   $f00f9344, $8708a3d2, $1e01f268, $6906c2fe, $f762575d, $806567cb, $196c3671, $6e6b06e7,
   $fed41b76, $89d32be0, $10da7a5a, $67dd4acc, $f9b9df6f, $8ebeeff9, $17b7be43, $60b08ed5,
   $d6d6a3e8, $a1d1937e, $38d8c2c4, $4fdff252, $d1bb67f1, $a6bc5767, $3fb506dd, $48b2364b,
   $d80d2bda, $af0a1b4c, $36034af6, $41047a60, $df60efc3, $a867df55, $316e8eef, $4669be79,
   $cb61b38c, $bc66831a, $256fd2a0, $5268e236, $cc0c7795, $bb0b4703, $220216b9, $5505262f,
   $c5ba3bbe, $b2bd0b28, $2bb45a92, $5cb36a04, $c2d7ffa7, $b5d0cf31, $2cd99e8b, $5bdeae1d,
   $9b64c2b0, $ec63f226, $756aa39c, $026d930a, $9c0906a9, $eb0e363f, $72076785, $05005713,
   $95bf4a82, $e2b87a14, $7bb12bae, $0cb61b38, $92d28e9b, $e5d5be0d, $7cdcefb7, $0bdbdf21,
   $86d3d2d4, $f1d4e242, $68ddb3f8, $1fda836e, $81be16cd, $f6b9265b, $6fb077e1, $18b74777,
   $88085ae6, $ff0f6a70, $66063bca, $11010b5c, $8f659eff, $f862ae69, $616bffd3, $166ccf45,
   $a00ae278, $d70dd2ee, $4e048354, $3903b3c2, $a7672661, $d06016f7, $4969474d, $3e6e77db,
   $aed16a4a, $d9d65adc, $40df0b66, $37d83bf0, $a9bcae53, $debb9ec5, $47b2cf7f, $30b5ffe9,
   $bdbdf21c, $cabac28a, $53b39330, $24b4a3a6, $bad03605, $cdd70693, $54de5729, $23d967bf,
   $b3667a2e, $c4614ab8, $5d681b02, $2a6f2b94, $b40bbe37, $c30c8ea1, $5a05df1b, $2d02ef8d
  );

function UpdateCRC32(Octet: Byte; Crc: Longint): Longint;
 begin
  UpdateCRC32:=CRC32_Table[Byte(Crc xor Longint(Octet))] xor ((Crc shr 8) and $00FFFFFF)
 end;

function StringCRC32(const S: String; Crc: Longint): Longint;
 var
  K: Longint;
 begin
  for K:=1 to Length(S) do
   Crc:=CRC32_Table[Byte(Crc xor Longint(S[K]))] xor ((Crc shr 8) and $00FFFFFF);

  StringCRC32:=Crc;
 end;

function PCharCRC32(S: PChar; Crc: Longint): Longint;
 begin
  while S^ <> #0 do
   begin
    Crc:=CRC32_Table[Byte(Crc xor Longint(S^))] xor ((Crc shr 8) and $00FFFFFF);

    Inc(S);
   end;

  PCharCRC32:=Crc;
 end;

function LoCase(Ch: Char): Char;
 begin
  if Ch in ['A'..'Z'] then
   LoCase:=Chr(Ord(Ch) + 32)
  else
   LoCase:=Ch;
 end;

function CreateDirectoryA(Directory: String): Boolean;
 begin
  if IOResult <> 0 then;

  if (Length(Directory) <> 3) and (Directory[Length(Directory)] = '\') then
   Delete(Directory, Length(Directory), 1);

  MkDir(Directory);

  if IOResult <> 0 then;

  CreateDirectoryA:=True;
 end;

function CreateDirectory(Directory: String): Boolean;
 var
  K: Integer;
  S: String;
 begin
  CreateDirectory:=True;

  Directory:=FExpand(Directory);

  if Length(Directory) = 3 then
   Exit;

  if Directory[Length(Directory)] = '\' then
   Delete(Directory, Length(Directory), 1);

  S:='';

  for K:=1 to Length(Directory) do
   if Directory[K] = '\' then
    begin
     CreateDirectoryA(S);

     S:=Concat(S, '\');
    end
   else
    S:=Concat(S, Directory[K]);

  CreateDirectoryA(S);
 end;

function ExtractPathname(S: String): String;
 var
  K: Byte;
 begin
  S:=FExpand(S);

  for K:=Length(S) downto 1 do
   if S[K] = '\' then
    begin
     ExtractPathname:=Copy(S, 1, K - 1);

     Exit;
    end;

  ExtractPathname:='';
 end;

function Y2ToDouble(const Year: Longint): Longint;
 begin
  if Year >= 2000 then
   Y2ToDouble:=Year - 2000
  else
   Y2ToDouble:=Year - 1900;
 end;

function DoubleToY2(const Year: Longint): Longint;
 begin
  if Year <= 70 then
   DoubleToY2:=Year + 2000
  else
   DoubleToY2:=Year + 1900;
 end;

{ MSGDateTimeToMessageBaseDateTime & MessageDateTimeToMSGDateTime was written
  by Andrey Novikoff, 345:817/10. thanx. :) }

{ MSGDateTimeToMessageBaseDateTime }

procedure MSGDateTimeToMessageBaseDateTime(const A: Longint; var DT: TMessageBaseDateTime);
 var
  AX: System.Word;
 begin
  AX:=A SHR 16;
  AX:=Lo(AX);
  AX:=AX AND $1F;
  AX:=AX SHL 1;

  DT.Sec:=AX;

  AX:=A SHR 16;
  AX:=AX SHR 5;
  AX:=AX AND $3F;

  DT.Min:=AX;

  AX:=A SHR 16;
  AX:=Hi(AX);
  AX:=AX SHR 3;
  AX:=AX AND $1F;

  DT.Hour:=AX;

  AX:=A;
  AX:=Lo(AX);
  AX:=AX AND $1F;

  DT.Day:=AX;

  AX:=A;
  AX:=AX SHR 5;
  AX:=AX AND $0F;

  DT.Month:=AX;

  AX:=A;
  AX:=Hi(AX);
  AX:=AX SHR 1;
  AX:=AX AND $7F;

  INC(AX, $7BC);

  DT.Year:=AX;
 end;

{ MessageBaseDateTimeToMSGDateTime }

procedure MessageBaseDateTimeToMSGDateTime(const DT: TMessageBaseDateTime; var L: Longint);
 var
  AX, BX, DX: System.Word;
  AL: Byte;
 begin
  DX:=DT.Sec;
  DX:=DX SHR 1;
  DX:=DX AND $1F;
  BX:=DT.Min;
  BX:=BX AND $3F;
  BX:=BX SHL 5;

  INC(DX, BX);

  BX:=DT.Hour;
  BX:=BX AND $1F;
  BX:=BX SHL 11;

  INC(DX, BX);

  L:=DX;
  AL:=DT.Day;
  AL:=AL AND $1F;
  BX:=DT.Month;
  BX:=BX SHL 5;
  AX:=AL;

  INC(AX, BX);

  BX:=DT.Year;

  DEC(BX, $7BC);

  BX:=BX AND $7F;
  BX:=BX SHL 9;

  INC(AX, BX);

  L:=L SHL 16;

  INC(L, AX);
 end;

{ MessageBaseDateTimeCompare }

function MessageBaseDateTimeCompare(const First, Second: TMessageBaseDateTime): Integer;
 begin
  if First.Year < Second.Year then MessageBaseDateTimeCompare:=-1 else
  if First.Year > Second.Year then MessageBaseDateTimeCompare:=1 else
   if First.Month < Second.Month then MessageBaseDateTimeCompare:=-1 else
   if First.Month > Second.Month then MessageBaseDateTimeCompare:=1 else
    if First.Day < Second.Day then MessageBaseDateTimeCompare:=-1 else
    if First.Day > Second.Day then MessageBaseDateTimeCompare:=1 else
     if First.Hour < Second.Hour then MessageBaseDateTimeCompare:=-1 else
     if First.Hour > Second.Hour then MessageBaseDateTimeCompare:=1 else
      if First.Min < Second.Min then MessageBaseDateTimeCompare:=-1 else
      if First.Min > Second.Min then MessageBaseDateTimeCompare:=1 else
       if First.Sec < Second.Sec then MessageBaseDateTimeCompare:=-1 else
       if First.Sec > Second.Sec then MessageBaseDateTimeCompare:=1 else
        MessageBaseDateTimeCompare:=0;
 end;

{ ExplainStatus }

function ExplainStatus(const Status: Longint): String;
 begin
  case Status of
   0                            : ExplainStatus:='no error';

   fmbWrongPath                 : ExplainStatus:='(msg) wrong path';
   fmbMessageNotFound           : ExplainStatus:='(msg) msg not found';
   fmbCannotCreateStream        : ExplainStatus:='(msg) unable to create stream';
   fmbAlreadyClosed             : ExplainStatus:='(msg) already closed';
   fmbBufferOverflow            : ExplainStatus:='(msg) buffer overflow';

   jmbAbsentJHR                 : ExplainStatus:='(jam) absent JHR';
   jmbAbsentJDX                 : ExplainStatus:='(jam) absent JDX';
   jmbAbsentJDT                 : ExplainStatus:='(jam) absent JDT';
   jmbCantOpenJHR               : ExplainStatus:='(jam) can''t open JHR';
   jmbCantOpenJDX               : ExplainStatus:='(jam) can''t open JDX';
   jmbCantOpenJDT               : ExplainStatus:='(jam) can''t open JDT';
   jmbStreamProblems            : ExplainStatus:='(jam) stream problems';
   jmbWrongHeaderLocation       : ExplainStatus:='(jam) wrong header location';
   jmbWrongDataLocation         : ExplainStatus:='(jam) wrong data location';
   jmbUnknownSubField           : ExplainStatus:='(jam) unknown subfield';
   jmbCantCreateJHR             : ExplainStatus:='(jam) can''t create JHR';
   jmbCantCreateJDX             : ExplainStatus:='(jam) can''t create JDX';
   jmbCantCreateJDT             : ExplainStatus:='(jam) can''t create JDT';
   jmbAbsentJLR                 : ExplainStatus:='(jam) absent JLR';
   jmbCantOpenJLR               : ExplainStatus:='(jam) can''t open JLR';
   jmbCantCreateJLR             : ExplainStatus:='(jam) can''t create JLR';

   smbAbsentSQD                 : ExplainStatus:='(sq) absent SQD';
   smbAbsentSQI                 : ExplainStatus:='(sq) absent SQI';
   smbCantOpenSQD               : ExplainStatus:='(sq) can''t open SQD';
   smbCantOpenSQI               : ExplainStatus:='(sq) can''t open SQI';
   smbIndexProblems             : ExplainStatus:='(sq) index problems';
   smbUnsupported               : ExplainStatus:='(sq) unsupported';
   smbMessageIsNotExists        : ExplainStatus:='(sq) msg is not exists';
   smbGrungedFrame              : ExplainStatus:='(sq) grunged frame';
   smbDataProblems              : ExplainStatus:='(sq) data problems';
   smbCantCreateSQD             : ExplainStatus:='(sq) can''t create SQD';
   smbCantCreateSQI             : ExplainStatus:='(sq) can''t create SQI';
   smbBaseHeaderProblems        : ExplainStatus:='(sq) base header problems';

   ombUnknownIDorUnsupported    : ExplainStatus:='(open) unrecognized ID';
   ombLocked                    : ExplainStatus:='(open) sharing violation';
   ombLockedAttemptsExpired     : ExplainStatus:='(open) sharing violation, attempts expired';
  else
   ExplainStatus:='error #' + LongToStr(Status);
  end;
 end;

end.
