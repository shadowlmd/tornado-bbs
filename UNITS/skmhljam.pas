{$R-,B-}
{$IFDEF MSDOS}
  {$IFNDEF DPMI32}
    {$F+}
  {$IFNDEF DPMI}
    {$O+}
  {$ENDIF}
  {$ENDIF}
{$ENDIF}
{&Use32-}
unit skMHLjam;

interface
uses
     skMHL,

     skCommon;

const
 jamSource              = $0000;
 jamDestination         = $0001;
 jamFrom                = $0002;
 jamTo                  = $0003;
 jamMSGID               = $0004;
 jamREPLY               = $0005;
 jamSubject             = $0006;
 jamPID                 = $0007;
 jamVia                 = $0008;
 jamAttaches            = $0009;
 jamFreqs               = $000B;
 jamUnknown             = $07D0;
 jamSEENBY              = $07D1;
 jamPATH                = $07D2;
 jamFLAGS               = $07D3;

 jamaPrivate            = $00000004;
 jamaCrash              = $00000100;
 jamaReceived           = $00000008;
 jamaSent               = $00000010;
 jamaAttach             = $00002000;
 jamaTransit            = $00000002;
 jamaOrphan             = $00040000;
 jamaKill               = $00000020;
 jamaLocal              = $00000001;
 jamaHold               = $00000080;
 jamaFRq                = $00001000;
 jamaRRq                = $00010000;
 jamaARq                = $00020000;

 jamaTypeLocal          = $00800000;
 jamaTypeEcho           = $01000000;
 jamaTypeNet            = $02000000;

 jamaKilled             = $80000000;

 JamSubBufferSize       = $4000;

type
 TJamLastRead = packed record
  NameCrc: Longint;
  UserNumber: Longint;
  LastRead: Longint;
  HighRead: Longint;
 end;

 TJamSubBuffer = array[1..JamSubBufferSize] of char;

 PSubField = ^TSubField;
 TSubField = record
  LoID, HiID: Word;
  Length: Longint;
  Data: array[1..1000] of char;
 end;

 TJamMessageHeader = record
  JamHeader: TJamMessageHeaderFirst;
  SubBuffer: TJamSubBuffer;
 end;

 TJamInfo = record
  SourceAddress: TAddress;
  DestinationAddress: TAddress;
  SourceName: String;
  DestinationName: String;
  Subject: String;
 end;

 PJamMessageBase = ^TJamMessageBase;
 TJamMessageBase = object(TMessageBase)
  constructor Init;
  destructor Done; virtual;
  function Open(const Path: String): Boolean; virtual;
  function Create(const Path: String): Boolean; virtual;
  function Exist(const Path: String): Boolean; virtual;
  procedure Close; virtual;
  function Exists(Message: Longint): Boolean; virtual;
  function GetLocation: Longint; virtual;
  procedure SetLocation(Location: Longint); virtual;
  function OpenMessage: Boolean; virtual;
  function OpenMessageHeader: Boolean; virtual;
  function CloseMessage: Boolean; virtual;
  function GetHighest: Longint; virtual;
  function GetCount: Longint; virtual;
  function GetFrom: String; virtual;
  function GetTo: String; virtual;
  function GetSubject: String; virtual;
  procedure SetFrom(const S: String); virtual;
  procedure SetTo(const S: String); virtual;
  procedure SetSubject(const S: String); virtual;
  procedure GetFromAddress(var Address: TAddress); virtual;
  procedure GetToAddress(var Address: TAddress); virtual;
  procedure GetFromAndToAddress(var FromAddress, ToAddress: TAddress); virtual;
  function GetAttribute(Attribute: Longint): Boolean; virtual;
  procedure SetAttribute(Attribute: Longint; Enable: Boolean); virtual;
  procedure GetWrittenDateTime(var DateTime: TMessageBaseDateTime); virtual;
  procedure GetArrivedDateTime(var DateTime: TMessageBaseDateTime); virtual;
  procedure SetWrittenDateTime(var DateTime: TMessageBaseDateTime); virtual;
  procedure SetArrivedDateTime(var DateTime: TMessageBaseDateTime); virtual;
  function WriteMessage: Boolean; virtual;
  function WriteMessageHeader: Boolean; virtual;
  function CreateNewMessage: Boolean; virtual;
  function KillMessage: Boolean; virtual;
  function GetLastRead(const UserNumber: Longint): Longint; virtual;
  procedure SetLastRead(const UserNumber: Longint; const Value: Longint); virtual;
  function GetReplyTo: LongInt;
  function GetReplyFirst: LongInt;
  function GetReplyNext: LongInt;
  procedure SetReplyTo(const AReplyTo: LongInt);
  procedure SetReplyFirst(const AReplyFirst: LongInt);
  procedure SetReplyNext(const AReplyNext: LongInt);
  procedure GetMessageHeader(var AHeader: TJamMessageHeaderFirst);
  procedure GetStreams(var AHeaderLink, AIndexLink, ADataLink: PMessageBaseStream);
  function GetRead: Boolean; virtual;
  procedure SetRead(const Value: Boolean); virtual;
 private
  LastReadLink: PMessageBaseStream;
  JamDataPosition: Longint;
  JamDataSize: Longint;
  JamBaseHeader: TJamBaseHeader;
  JamMessageHeaderPosition: Longint;
{
  JamMessageHeaderSize: Longint;
}
  JamIndex: TJamIndex;
  JamIndexPosition: Longint;
  JamInfo: TJamInfo;
  JamMessageHeader: TJamMessageHeader;
  HeaderLink: PMessageBaseStream;
  IndexLink: PMessageBaseStream;
  DataLink: PMessageBaseStream;
  function GetSubField(const Number: Longint; var ID: Longint; var S: String): Boolean;
  procedure AddSubField(const ID: Longint; const S: String);
  function MapAttribute(var Attribute: Longint): Boolean;
  function StrCrc32(const S: String): Longint;
  procedure ResetAll;
  procedure RepackIndex;
 end;

implementation

constructor TJamMessageBase.Init;
 begin
  inherited Init;

  SetBasePath('');

  HeaderLink:=nil;
  IndexLink:=nil;
  DataLink:=nil;
  LastReadLink:=nil;

  SetCurrent(0);
 end;

destructor TJamMessageBase.Done;
 begin
  CloseMessage;

  Close;

  inherited Done;
 end;

function TJamMessageBase.Open(const Path: String): Boolean;
 var
  S: String;
 begin
  if HeaderLink <> nil then
   Close;

  Open:=False;

  S:=Path;

  if S[Length(S)] <> '.' then
   S:=Concat(S, '.');

  SetBasePath(S);

  if not FileExists(GetBasePath + 'JHR') then
   begin
    SetStatus(jmbAbsentJHR);

    Exit;
   end;

  if not FileExists(GetBasePath + 'JDX') then
   begin
    SetStatus(jmbAbsentJDX);

    Exit;
   end;

  if not FileExists(GetBasePath + 'JDT') then
   begin
    SetStatus(jmbAbsentJDT);

    Exit;
   end;

  HeaderLink:=nil;
  IndexLink:=nil;
  DataLink:=nil;
  LastReadLink:=nil;

  repeat
   HeaderLink:=CreateMessageBaseFileStream(GetBasePath + 'JHR', smOpen or smDenyWrite);

   if HeaderLink^.Status <> smOk then
    begin
     SetStatus(jmbCantOpenJHR);

     Break;
    end;

   IndexLink:=CreateMessageBaseFileStream(GetBasePath + 'JDX', smOpen or smDenyWrite);

   if IndexLink^.Status <> smOk then
    begin
     SetStatus(jmbCantOpenJDX);

     Break;
    end;

   DataLink:=CreateMessageBaseFileStream(GetBasePath + 'JDT', smOpen or smDenyWrite);

   if DataLink^.Status <> smOk then
    begin
     SetStatus(jmbCantOpenJDT);

     Break;
    end;

   LastReadLink:=CreateMessageBaseFileStream(GetBasePath + 'JLR', smOpen);

   if LastReadLink^.Status <> smOk then
    begin
     Dispose(LastReadLink, Done);

     LastReadLink:=CreateMessageBaseFileStream(GetBasePath + 'JLR', smCreate);

     if LastReadLink^.Status <> smOk then
      begin
       SetStatus(jmbCantCreateJLR);

       Break;
      end;
    end;

   HeaderLink^.Seek(0);

   HeaderLink^.Read(JamBaseHeader, SizeOf(JamBaseHeader));

   if IndexLink^.GetSize div SizeOf(TJamIndex) <> JamBaseHeader.ActiveMsgs then
    RepackIndex;

   Open:=True;

   SetOpened(True);

   Exit;
  until True;

  if HeaderLink <> nil then
   Dispose(HeaderLink, Done);

  if IndexLink <> nil then
   Dispose(IndexLink, Done);

  if DataLink <> nil then
   Dispose(DataLink, Done);

  if LastReadLink <> nil then
   Dispose(LastReadLink, Done);

  HeaderLink:=nil;
  IndexLink:=nil;
  DataLink:=nil;
  LastReadLink:=nil;

  SetBasePath('');
 end;

function TJamMessageBase.Create(const Path: String): Boolean;
 var
  DateTime: TMessageBaseDateTime;
  S: String;
 begin
  Create:=False;

  S:=Path;

  if S[Length(S)] <> '.' then
   S:=Concat(S, '.');

  SetBasePath(S);

  CreateDirectory(ExtractPathName(Path));

  HeaderLink:=nil;
  IndexLink:=nil;
  DataLink:=nil;
  LastReadLink:=nil;

  repeat
   HeaderLink:=CreateMessageBaseFileStream(GetBasePath + 'JHR', smCreate or smDenyWrite);

   if HeaderLink^.Status <> smOk then
    begin
     SetStatus(jmbCantCreateJHR);

     Break;
    end;

   IndexLink:=CreateMessageBaseFileStream(GetBasePath + 'JDX', smCreate or smDenyWrite);

   if IndexLink^.Status <> smOk then
    begin
     SetStatus(jmbCantCreateJDX);

     Break;
    end;

   DataLink:=CreateMessageBaseFileStream(GetBasePath + 'JDT', smCreate or smDenyWrite);

   if DataLink^.Status <> smOk then
    begin
     SetStatus(jmbCantCreateJDT);

     Break;
    end;

   LastReadLink:=CreateMessageBaseFileStream(GetBasePath + 'JLR', smCreate or smDenyWrite);

   if LastReadLink^.Status <> smOk then
    begin
     SetStatus(jmbCantCreateJLR);

     Break;
    end;

   ResetAll;

   FillChar(JamBaseHeader, SizeOf(JamBaseHeader), 0);

   JamBaseHeader.Signature[1]:='J';
   JamBaseHeader.Signature[2]:='A';
   JamBaseHeader.Signature[3]:='M';
   JamBaseHeader.Signature[4]:=#0;

   GetCurrentMessageBaseDateTime(DateTime);

   MessageBaseDateTimeToUnixDateTime(DateTime, JamBaseHeader.Created);

   JamBaseHeader.ModCounter:=0;
   JamBaseHeader.ActiveMsgs:=0;
   JamBaseHeader.PwdCRC:=-1;
   JamBaseHeader.BaseMsgNum:=1;

   HeaderLink^.Seek(0);
   HeaderLink^.Write(JamBaseHeader, SizeOf(JamBaseHeader));

   ResetAll;

   Create:=True;

   SetOpened(True);

   Exit;
  until True;

  if HeaderLink <> nil then
   Dispose(HeaderLink, Done);

  if IndexLink <> nil then
   Dispose(IndexLink, Done);

  if DataLink <> nil then
   Dispose(DataLink, Done);

  if LastReadLink <> nil then
   Dispose(LastReadLink, Done);

  HeaderLink:=nil;
  IndexLink:=nil;
  DataLink:=nil;
  LastReadLink:=nil;
 end;

function TJamMessageBase.Exist(const Path: String): Boolean;
 var
  APath: String;
 begin
  APath:=Path;

  if APath[Length(APath)] <> '.' then
   APath:=Concat(APath, '.');

  Exist:=FileExists(APath + 'JHR') and
         FileExists(APath + 'JDX') and
         FileExists(APath + 'JDT') and
         FileExists(APath + 'JLR');
 end;

procedure TJamMessageBase.Close;
 begin
  if (HeaderLink = nil) or
     (IndexLink = nil) or
     (DataLink = nil) or
     (LastReadLink = nil) then Exit;

  CloseMessage;

  ResetAll;

  HeaderLink^.Seek(0);
  HeaderLink^.Write(JamBaseHeader, SizeOf(JamBaseHeader));
  HeaderLink^.Flush;

  IndexLink^.Flush;
  DataLink^.Flush;
  LastReadLink^.Flush;

  Dispose(DataLink, Done);
  Dispose(IndexLink, Done);
  Dispose(HeaderLink, Done);
  Dispose(LastReadLink, Done);

  HeaderLink:=nil;
  IndexLink:=nil;
  DataLink:=nil;
  LastReadLink:=nil;

  SetOpened(False);
 end;

function TJamMessageBase.Exists(Message: Longint): Boolean;
 var
  AJamIndexPosition: Longint;
  AJamIndex: TJamIndex;
 begin
  Exists:=False;

  if (Message >= JamBaseHeader.BaseMsgNum) and (Message < JamBaseHeader.BaseMsgNum + JamBaseHeader.ActiveMsgs) then
   begin
    AJamIndexPosition:=(Message - JamBaseHeader.BaseMsgNum) * SizeOf(AJamIndex);

    IndexLink^.Seek(AJamIndexPosition);

    if IndexLink^.Status <> smOk then
     Exit;

    IndexLink^.Read(AJamIndex, SizeOf(AJamIndex));

    if AJamIndex.HdrLoc = -1 then
     Exit;

    Exists:=True;
   end;
 end;

function TJamMessageBase.GetLocation: Longint;
 begin
  GetLocation:=Current;
 end;

procedure TJamMessageBase.SetLocation(Location: Longint);
 begin
  SetCurrent(Location);
 end;

function TJamMessageBase.OpenMessage: Boolean;
 var
  Size, Count, SubFieldType: Longint;
  S: String;
 begin
  CloseMessage;

  ResetAll;

  OpenMessage:=False;

  JamIndexPosition:=(Current - JamBaseHeader.BaseMsgNum) * SizeOf(JamIndex);

  IndexLink^.Seek(JamIndexPosition);

  if IndexLink^.Status <> smOk then
   begin
    SetStatus(jmbStreamProblems);

    Exit;
   end;

  IndexLink^.Read(JamIndex, SizeOf(JamIndex));

  if (JamIndex.HdrLoc < 0) or (JamIndex.HdrLoc > HeaderLink^.GetSize) then
   begin
    SetStatus(jmbWrongHeaderLocation);

    Exit;
   end;

  JamMessageHeaderPosition:=JamIndex.HdrLoc;

  HeaderLink^.Seek(JamIndex.HdrLoc);

  if HeaderLink^.Status <> smOk then
   begin
    SetStatus(jmbStreamProblems);

    Exit;
   end;

  Size:=HeaderLink^.GetSize - HeaderLink^.GetPos;

  if Size > SizeOf(JamMessageHeader) then
   Size:=SizeOf(JamMessageHeader);

  HeaderLink^.Read(JamMessageHeader, Size);

  if HeaderLink^.Status <> smOk then
   begin
    SetStatus(jmbStreamProblems);

    Exit;
   end;

  if (JamMessageHeader.JamHeader.TextOfs < 0) or (JamMessageHeader.JamHeader.TextOfs > DataLink^.GetSize) then
   begin
    SetStatus(jmbWrongDataLocation);

    Exit;
   end;

  FillChar(JamInfo, SizeOf(JamInfo), 0);

  Count:=1;

  while GetSubField(Count, SubFieldType, S) do
   begin
    case SubFieldType of
     jamSource: StrToAddress(S, JamInfo.SourceAddress);
     jamDestination: StrToAddress(S, JamInfo.DestinationAddress);
     jamFrom: JamInfo.SourceName:=S;
     jamTo: JamInfo.DestinationName:=S;
     jamSubject: JamInfo.Subject:=S;
     jamAttaches: if GetAttribute(maAttach) then JamInfo.Subject:=S;
     jamFreqs: if GetAttribute(maFRq) then JamInfo.Subject:=S;
    end;

    Inc(Count);
   end;

  SetMessageTextStream(CreateMessageBaseMemoryStream(MaxMessageSize));

  DataLink^.Seek(JamMessageHeader.JamHeader.TextOfs);

  GetMessageTextStream^.Seek(0);

  Count:=1;
  while GetSubField(Count, SubFieldType, S) do
   begin
    case SubFieldType of
     jamMSGID: PutString('MSGID: ' + S);
     jamREPLY: PutString('REPLY: ' + S);
     jamPID: PutString('PID: ' + S);
     jamUnknown: PutString('' + S);
     jamFLAGS: PutString('FLAGS: ' + S);
    end;
    Inc(Count);
   end;

  GetMessageTextStream^.CopyFrom(DataLink^, JamMessageHeader.JamHeader.TextLen);

  Count:=1;
  while GetSubField(Count, SubFieldType, S) do
   begin
    if SubFieldType = jamVia then
     PutString('Via ' + S);

    Inc(Count);
   end;

  Count:=1;
  while GetSubField(Count, SubFieldType, S) do
   begin
    if SubFieldType = jamSEENBY then
     PutString('SEEN-BY: ' + S);

    Inc(Count);
   end;

  Count:=1;
  while GetSubField(Count, SubFieldType, S) do
   begin
    if SubFieldType = jamPATH then
     PutString('PATH: ' + S);

    Inc(Count);
   end;

  if JamInfo.SourceAddress.Zone <> 0 then
   SetFromAddress(JamInfo.SourceAddress, False);

  if JamInfo.DestinationAddress.Zone <> 0 then
   SetToAddress(JamInfo.DestinationAddress);

  GetMessageTextStream^.Seek(0);

  OpenMessage:=True;
 end;

function TJamMessageBase.OpenMessageHeader: Boolean;
 var
  Size, Count, SubFieldType: Longint;
  S: String;
 begin
  CloseMessage;

  ResetAll;

  OpenMessageHeader:=False;

  JamIndexPosition:=(Current - JamBaseHeader.BaseMsgNum) * SizeOf(JamIndex);

  IndexLink^.Seek(JamIndexPosition);

  if IndexLink^.Status <> smOk then
   begin
    SetStatus(jmbStreamProblems);

    Exit;
   end;

  IndexLink^.Read(JamIndex, SizeOf(JamIndex));

  if (JamIndex.HdrLoc < 0) or (JamIndex.HdrLoc > HeaderLink^.GetSize) then
   begin
    SetStatus(jmbWrongHeaderLocation);

    Exit;
   end;

  JamMessageHeaderPosition:=JamIndex.HdrLoc;

  HeaderLink^.Seek(JamIndex.HdrLoc);

  if HeaderLink^.Status <> smOk then
   begin
    SetStatus(jmbStreamProblems);

    Exit;
   end;

  Size:=HeaderLink^.GetSize - HeaderLink^.GetPos;

  if Size > SizeOf(JamMessageHeader) then
   Size:=SizeOf(JamMessageHeader);

  HeaderLink^.Read(JamMessageHeader, Size);

  if HeaderLink^.Status <> smOk then
   begin
    SetStatus(jmbStreamProblems);

    Exit;
   end;

  if (JamMessageHeader.JamHeader.TextOfs < 0) or (JamMessageHeader.JamHeader.TextOfs > DataLink^.GetSize) then
   begin
    SetStatus(jmbWrongDataLocation);

    Exit;
   end;

  FillChar(JamInfo, SizeOf(JamInfo), 0);

  Count:=1;

  while GetSubField(Count, SubFieldType, S) do
   begin
    case SubFieldType of
     jamSource: StrToAddress(S, JamInfo.SourceAddress);
     jamDestination: StrToAddress(S, JamInfo.DestinationAddress);
     jamFrom: JamInfo.SourceName:=S;
     jamTo: JamInfo.DestinationName:=S;
     jamSubject: JamInfo.Subject:=S;
     jamAttaches: if GetAttribute(maAttach) then JamInfo.Subject:=S;
     jamFreqs: if GetAttribute(maFRq) then JamInfo.Subject:=S;
    end;

    Inc(Count);
   end;

  SetMessageTextStream(CreateMessageBaseMemoryStream(MaxMessageSize));

  Count:=1;

  while GetSubField(Count, SubFieldType, S) do
   begin
    case SubFieldType of
     jamMSGID: PutString('MSGID: ' + S);
     jamREPLY: PutString('REPLY: ' + S);
     jamPID: PutString('PID: ' + S);
     jamUnknown: PutString('' + S);
     jamFLAGS: PutString('FLAGS: ' + S);
    end;

    Inc(Count);
   end;

  if JamInfo.SourceAddress.Zone <> 0 then
   SetFromAddress(JamInfo.SourceAddress, False);

  if JamInfo.DestinationAddress.Zone <> 0 then
   SetToAddress(JamInfo.DestinationAddress);

  GetMessageTextStream^.Seek(0);

  OpenMessageHeader:=True;
 end;

function TJamMessageBase.CloseMessage: Boolean;
 begin
  if GetMessageTextStream = nil then
   CloseMessage:=False
  else
   begin
    CloseMessage:=True;

    SetMessageTextStream(nil);
   end;
 end;

function TJamMessageBase.GetHighest: Longint;
 begin
  GetHighest:=JamBaseHeader.BaseMsgNum + JamBaseHeader.ActiveMsgs;
 end;

function TJamMessageBase.GetCount: Longint;
 begin
  GetCount:=JamBaseHeader.ActiveMsgs;
 end;

function TJamMessageBase.GetFrom: String;
 begin
  GetFrom:=JamInfo.SourceName;
 end;

function TJamMessageBase.GetTo: String;
 begin
  GetTo:=JamInfo.DestinationName;
 end;

function TJamMessageBase.GetSubject: String;
 begin
  GetSubject:=JamInfo.Subject;
 end;

procedure TJamMessageBase.SetFrom(const S: String);
 begin
  JamInfo.SourceName:=S;
 end;

procedure TJamMessageBase.SetTo(const S: String);
 begin
  JamInfo.DestinationName:=S;
 end;

procedure TJamMessageBase.SetSubject(const S: String);
 begin
  JamInfo.Subject:=S;
 end;

procedure TJamMessageBase.GetFromAddress(var Address: TAddress);
 begin
  ClearAddress(Address);

  inherited GetFromAddress(Address);
 end;

procedure TJamMessageBase.GetToAddress(var Address: TAddress);
 begin
  ClearAddress(Address);

  inherited GetToAddress(Address);
 end;

procedure TJamMessageBase.GetFromAndToAddress(var FromAddress, ToAddress: TAddress);
 begin
  ClearAddress(FromAddress);
  ClearAddress(ToAddress);

  inherited GetFromAndToAddress(FromAddress, ToAddress);
 end;

function TJamMessageBase.GetAttribute(Attribute: Longint): Boolean;
 begin
  if not MapAttribute(Attribute) then
   begin
    GetAttribute:=False;

    Exit;
   end;

  GetAttribute:=JamMessageHeader.JamHeader.Attr1 and Attribute <> 0;
 end;

procedure TJamMessageBase.SetAttribute(Attribute: Longint; Enable: Boolean);
 begin
  if not MapAttribute(Attribute) then
   Exit;

  if Enable then
   JamMessageHeader.JamHeader.Attr1:=JamMessageHeader.JamHeader.Attr1 or Attribute
  else
   JamMessageHeader.JamHeader.Attr1:=JamMessageHeader.JamHeader.Attr1 and not Attribute
 end;

procedure TJamMessageBase.GetWrittenDateTime(var DateTime: TMessageBaseDateTime);
 begin
  UnixDateTimeToMessageBaseDateTime(JamMessageHeader.JamHeader.DateWritten, DateTime);
 end;

procedure TJamMessageBase.GetArrivedDateTime(var DateTime: TMessageBaseDateTime);
 begin
  UnixDateTimeToMessageBaseDateTime(JamMessageHeader.JamHeader.DateArrived, DateTime);
 end;

procedure TJamMessageBase.SetWrittenDateTime(var DateTime: TMessageBaseDateTime);
 begin
  MessageBaseDateTimeToUnixDateTime(DateTime, JamMessageHeader.JamHeader.DateWritten);
 end;

procedure TJamMessageBase.SetArrivedDateTime(var DateTime: TMessageBaseDateTime);
 begin
  MessageBaseDateTimeToUnixDateTime(DateTime, JamMessageHeader.JamHeader.DateArrived);
 end;

function TJamMessageBase.WriteMessage: Boolean;
 var
  Buffer: PMessageBaseStream;
  Line: PChar;
  S: String;
  LineLength: Byte absolute S;
 const
  CR: Byte = 13;
 begin
  ResetAll;

  { creating subfields and repacking message text }

  JamMessageHeader.JamHeader.SubFieldLen:=0;
  GetMem(Line, MaxLineSize);
  Buffer:=CreateMessageBaseMemoryStream(MaxMessageSize);

  GetFromAndToAddress(JamInfo.SourceAddress, JamInfo.DestinationAddress);

  AddSubField(jamFrom, JamInfo.SourceName);
  AddSubField(jamSource, AddressToStr(JamInfo.SourceAddress));

  AddSubField(jamTo, JamInfo.DestinationName);

  if AddressCompare(JamInfo.DestinationAddress, NullAddress) <> 0 then
   AddSubField(jamDestination, AddressToStr(JamInfo.DestinationAddress))
  else
   AddSubField(jamDestination, AddressToStr(JamInfo.SourceAddress));

  if JamInfo.Subject <> '' then
   AddSubField(jamSubject, JamInfo.Subject);

  if GetAttribute(maAttach) then
   AddSubField(jamAttaches, JamInfo.Subject);

  if GetAttribute(maFRq) then
   AddSubField(jamFreqs, JamInfo.Subject);

  SetTextPos(0);
  while not EndOfMessage do
   begin
    GetStringPChar(Line, MaxLineSize);

    LineLength:=LenASCIIZ(Line) and $FF;

    Move(Line^, S[1], LineLength);
    if Copy(S, 1, 8) = 'MSGID: ' then
     begin
      AddSubField(jamMSGID, Copy(S, 9, 255));

      JamMessageHeader.JamHeader.MSGIDCrc:=StrCrc32(Copy(S, 9, 255));
     end else
    if Copy(S, 1, 8) = 'REPLY: ' then
     begin
      AddSubField(jamREPLY, Copy(S, 9, 255));

      JamMessageHeader.JamHeader.REPLYCrc:=StrCrc32(Copy(S, 9, 255));
     end else
    if Copy(S, 1, 6) = 'PID: ' then AddSubField(jamPID, Copy(S, 7, 255)) else
    if Copy(S, 1, 8) = 'FLAGS: ' then AddSubField(jamFLAGS, Copy(S, 9, 255)) else
    if Copy(S, 1, 5) = 'Via ' then AddSubField(jamVia, Copy(S, 6, 255)) else
    if Copy(S, 1, 9) = 'SEEN-BY: ' then AddSubField(jamSEENBY, Copy(S, 10, 255)) else
    if Copy(S, 1, 7) = 'PATH: ' then AddSubField(jamPATH, Copy(S, 8, 255)) else
    if Copy(S, 1, 5) = 'INTL' then else
    if Copy(S, 1, 5) = 'FMPT' then else
    if Copy(S, 1, 5) = 'TOPT' then else
    if Copy(S, 1, 1) = '' then AddSubField(jamUnknown, Copy(S, 2, 255)) else
     begin
      Buffer^.Write(Line^, LenASCIIZ(Line));

      Buffer^.Write(CR, SizeOf(CR));
     end;
   end;

  FreeMem(Line, MaxLineSize);

  Buffer^.Seek(0);

  { update message }
  SetAttribute(jamaTypeEcho, GetFlag(afEchomail));
  SetAttribute(jamaTypeNet, GetFlag(afNetmail));
  SetAttribute(jamaTypeLocal, GetFlag(afLocal));

  { update index }
  JamMessageHeaderPosition:=HeaderLink^.GetSize;
  JamIndex.MsgToCrc:=StrCrc32(JamInfo.DestinationName);
  JamIndex.HdrLoc:=JamMessageHeaderPosition;

  { write index }
  IndexLink^.Seek(JamIndexPosition);
  IndexLink^.Write(JamIndex, SizeOf(JamIndex));

  { update header }
  JamDataPosition:=DataLink^.GetSize;
  JamDataSize:=Buffer^.GetSize;

  JamMessageHeader.JamHeader.TextOfs:=JamDataPosition;
  JamMessageHeader.JamHeader.TextLen:=JamDataSize;
  Inc(JamBaseHeader.ModCounter);

  { write header }
  HeaderLink^.Seek(JamMessageHeaderPosition);
  HeaderLink^.Write(JamMessageHeader, SizeOf(JamMessageHeader.JamHeader) + JamMessageHeader.JamHeader.SubFieldLen);

  { write text }
  DataLink^.Seek(JamDataPosition);
  DataLink^.CopyFrom(Buffer^, JamDataSize);

  { flush'em }
  IndexLink^.Flush;
  HeaderLink^.Flush;
  DataLink^.Flush;
  LastReadLink^.Flush;

  Dispose(Buffer, Done);

  WriteMessage:=True;
 end;

function TJamMessageBase.WriteMessageHeader: Boolean;
 begin
  HeaderLink^.Seek(JamMessageHeaderPosition);
  HeaderLink^.Write(JamMessageHeader, SizeOf(JamMessageHeader.JamHeader));

  WriteMessageHeader:=True;
 end;

function TJamMessageBase.CreateNewMessage: Boolean;
 var
  DateTime: TMessageBaseDateTime;
 begin
  ResetAll;

  Inc(JamBaseHeader.ActiveMsgs);

  SetCurrent(JamBaseHeader.BaseMsgNum + GetCount - 1);

  FillChar(JamMessageHeader, SizeOf(JamMessageHeader), 0);
  FillChar(JamInfo, SizeOf(JamInfo), 0);

  JamIndex.MsgToCrc:=-1;

  JamMessageHeader.JamHeader.Signature[1]:='J';
  JamMessageHeader.JamHeader.Signature[2]:='A';
  JamMessageHeader.JamHeader.Signature[3]:='M';
  JamMessageHeader.JamHeader.Signature[4]:=#0;

  JamMessageHeader.JamHeader.Rev:=1;

  JamIndexPosition:=IndexLink^.GetSize;

  JamMessageHeader.JamHeader.MSGIDCrc:=$FFFFFFFF;
  JamMessageHeader.JamHeader.REPLYCrc:=$FFFFFFFF;
  JamMessageHeader.JamHeader.PwdCrc:=$FFFFFFFF;

  JamMessageHeader.JamHeader.MsgNum:=Current;

  GetCurrentMessageBaseDateTime(DateTime);
  SetWrittenDateTime(DateTime);

  SetMessageTextStream(CreateMessageBaseMemoryStream(MaxMessageSize));

  ResetDateTime;

  CreateNewMessage:=True;
 end;

function TJamMessageBase.KillMessage: Boolean;
 begin
  ResetAll;

  JamIndex.MsgToCrc:=-1;
  JamIndex.HdrLoc:=-1;

  JamIndexPosition:=(Current - JamBaseHeader.BaseMsgNum) * SizeOf(JamIndex);

  IndexLink^.Seek(JamIndexPosition);
  IndexLink^.Write(JamIndex, SizeOf(JamIndex));

  Dec(JamBaseHeader.ActiveMsgs);

  RepackIndex;

  SetCurrent(Current - 1);

  KillMessage:=True;
 end;

function TJamMessageBase.GetLastRead(const UserNumber: Longint): Longint;
 var
  Countdown: Longint;
  LastRead: TJamLastRead;
 begin
  ResetAll;

  LastReadLink^.Seek(0);

  Countdown:=LastReadLink^.GetSize div SizeOf(TJamLastRead);

  while Countdown <> 0 do
   begin
    LastReadLink^.Read(LastRead, SizeOf(LastRead));

    if LastRead.UserNumber = UserNumber then
     begin
      GetLastRead:=LastRead.LastRead;

      Exit;
     end;

    Dec(Countdown);
   end;

  GetLastRead:=0;
 end;

procedure TJamMessageBase.SetLastRead(const UserNumber: Longint; const Value: Longint);
 var
  Countdown: Longint;
  LastRead: TJamLastRead;
 begin
  ResetAll;

  LastReadLink^.Seek(0);

  Countdown:=LastReadLink^.GetSize div SizeOf(TJamLastRead);

  while Countdown <> 0 do
   begin
    LastReadLink^.Read(LastRead, SizeOf(LastRead));

    if LastRead.UserNumber = UserNumber then
     begin
      LastRead.LastRead:=Value;

      LastRead.HighRead:=Value;

      LastReadLink^.Seek(LastReadLink^.GetPos - SizeOf(LastRead));

      LastReadLink^.Write(LastRead, SizeOf(LastRead));

      Exit;
     end;

    Dec(Countdown);
   end;

  LastRead.UserNumber:=UserNumber;
  LastRead.HighRead:=Value;
  LastRead.NameCrc:=UserNumber;
  LastRead.LastRead:=Value;

  LastReadLink^.Seek(LastReadLink^.GetSize);

  LastReadLink^.Write(LastRead, SizeOf(LastRead));
 end;

function TJamMessageBase.GetReplyTo: LongInt;
 begin
  GetReplyTo:=JamMessageHeader.JamHeader.ReplyTo;
 end;

function TJamMessageBase.GetReplyFirst: LongInt;
 begin
  GetReplyFirst:=JamMessageHeader.JamHeader.ReplyFirst;
 end;

function TJamMessageBase.GetReplyNext: LongInt;
 begin
  GetReplyNext:=JamMessageHeader.JamHeader.ReplyNext;
 end;

procedure TJamMessageBase.SetReplyTo(const AReplyTo: LongInt);
 begin
  JamMessageHeader.JamHeader.ReplyTo:=AReplyTo;
 end;

procedure TJamMessageBase.SetReplyFirst(const AReplyFirst: LongInt);
 begin
  JamMessageHeader.JamHeader.ReplyFirst:=AReplyFirst;
 end;

procedure TJamMessageBase.SetReplyNext(const AReplyNext: LongInt);
 begin
  JamMessageHeader.JamHeader.ReplyNext:=AReplyNext;
 end;

procedure TJamMessageBase.GetMessageHeader(var AHeader: TJamMessageHeaderFirst);
 begin
  AHeader:=JamMessageHeader.JamHeader;
 end;

procedure TJamMessageBase.GetStreams(var AHeaderLink, AIndexLink, ADataLink: PMessageBaseStream);
 begin
  AHeaderLink:=HeaderLink;
  AIndexLink:=IndexLink;
  ADataLink:=DataLink;
 end;

function TJamMessageBase.GetRead: Boolean;
 begin
  GetRead:=JamMessageHeader.JamHeader.TimesRead <> 0;
 end;

procedure TJamMessageBase.SetRead(const Value: Boolean);
 begin
  if Value then
   Inc(JamMessageHeader.JamHeader.TimesRead)
  else
   JamMessageHeader.JamHeader.TimesRead:=0;
 end;

{ private methods }

function TJamMessageBase.GetSubField(const Number: Longint; var ID: Longint; var S: String): Boolean;
 var
  K, L: Longint;
  Length: Byte absolute S;
  SubField: PSubField;
 begin
  L:=0;
  K:=1;

  while (K <= JamMessageHeader.JamHeader.SubFieldLen) and (K < JamSubBufferSize) do
   begin
    SubField:=@JamMessageHeader.SubBuffer[K];

    Inc(K, SubField^.Length + 8);
    Inc(L);

    if L = Number then
     begin
      GetSubField:=True;
      ID:=SubField^.LoID;
      Length:=SubField^.Length and $FF;

      Move(SubField^.Data, S[1], Length);

      Exit;
     end;
   end;

  GetSubField:=False;
 end;

procedure TJamMessageBase.AddSubField(const ID: Longint; const S: String);
 var
  SubField: PSubField;
 begin
  SubField:=@JamMessageHeader.SubBuffer[JamMessageHeader.JamHeader.SubFieldLen + 1];

  SubField^.LoID:=ID;
  SubField^.HiID:=0;
  SubField^.Length:=Length(S);

  Move(S[1], SubField^.Data, SubField^.Length);

  Inc(JamMessageHeader.JamHeader.SubFieldLen, 8 + Length(S));
 end;

function TJamMessageBase.MapAttribute(var Attribute: Longint): Boolean;
 begin
  MapAttribute:=True;

  case Attribute of
   maPrivate: Attribute:=jamaPrivate;
   maCrash: Attribute:=jamaCrash;
   maReceived: Attribute:=jamaReceived;
   maSent: Attribute:=jamaSent;
   maAttach: Attribute:=jamaAttach;
   maTransit: Attribute:=jamaTransit;
   maOrphan: Attribute:=jamaOrphan;
   maKill: Attribute:=jamaKill;
   maLocal: Attribute:=jamaLocal;
   maHold: Attribute:=jamaHold;
   maFRq: Attribute:=jamaFRq;
   maRRq: Attribute:=jamaRRq;
   maARq: Attribute:=jamaARq;
  else
   MapAttribute:=(Attribute = jamaTypeLocal) or
                 (Attribute = jamaTypeEcho) or
                 (Attribute = jamaTypeNet);
  end;
 end;

function TJamMessageBase.StrCrc32(const S: String): Longint;
 var
  I: Byte;
  L: Longint;
 begin
  L:=-1;

  for I:=1 to Length(S) do
   L:=UpdateCRC32(Byte(LoCase(S[I])), L);

  StrCrc32:=L;
 end;

procedure TJamMessageBase.ResetAll;
 begin
  IndexLink^.Reset;
  DataLink^.Reset;
  HeaderLink^.Reset;
  LastReadLink^.Reset;
 end;

procedure TJamMessageBase.RepackIndex;
 var
  Index: TJamIndex;
  TempStream: PMessageBaseStream;
  TempSize: Longint;
 begin
  IndexLink^.Seek(0);

  TempStream:=CreateMessageBaseMemoryStream(IndexLink^.GetSize);
  TempSize:=IndexLink^.GetSize;

  JamBaseHeader.ActiveMsgs:=0;

  TempStream^.Seek(TempSize);
  TempStream^.Truncate;

  TempStream^.Seek(0);

  while TempSize > 0 do
   begin
    IndexLink^.Read(Index, SizeOf(Index));

    if Index.HdrLoc <> -1 then
     begin
      TempStream^.Write(Index, SizeOf(Index));

      Inc(JamBaseHeader.ActiveMsgs);
     end;

    Dec(TempSize, SizeOf(Index));
   end;

  TempStream^.Truncate;

  IndexLink^.Seek(0);
  TempStream^.Seek(0);

  IndexLink^.CopyFrom(TempStream^, TempStream^.GetSize);

  IndexLink^.Truncate;

  IndexLink^.Flush;

  Dispose(TempStream, Done);
 end;

end.