{$B-}
{$IFDEF MSDOS}
  {$IFNDEF DPMI32}
    {$F+}
  {$IFNDEF DPMI}
    {$O+}
  {$ENDIF}
  {$ENDIF}
{$ENDIF}
{&Use32-}
unit skMHLsq;

{$IFDEF DELPHI}
 {$HINTS OFF}
 {$WARNINGS OFF}
{$ENDIF}

interface
uses
 skMHL,
 skCommon;

const
 squishFrameID          = $AFAE4453;
 squishFrameMessage     = $0000;
 squishFrameFree        = $0001;

 squishaPrivate         = $00001;
 squishaCrash           = $00002;
 squishaReceived        = $00004;
 squishaSent            = $00008;
 squishaAttach          = $00010;
 squishaTransit         = $00020;
 squishaOrphan          = $00040;
 squishaKill            = $00080;
 squishaLocal           = $00100;
 squishaHold            = $00200;
 squishaXX2             = $00400;
 squishaFRq             = $00800;
 squishaRRq             = $01000;
 squishaRRc             = $02000;
 squishaARq             = $04000;
 squishaURq             = $08000;
 squishaScanned         = $10000;
 squishaUID             = $20000;
 squishaRead            = $80000;

 SquishMaxMsg           : Longint = $FFFF;
 SquishKeepDays         : Longint = $7FFF;

type
 PControlInformationBuffer = ^TControlInformationBuffer;
 TControlInformationBuffer = array[1..65521] of Char;

 PSquishMessageBase = ^TSquishMessageBase;
 TSquishMessageBase = object(TMessageBase)
  constructor Init;
  destructor Done; virtual;
  function Open(const Path: String): Boolean; virtual;
  function Create(const Path: String): Boolean; virtual;
  function Exist(const Path: String): Boolean; virtual;
  procedure Close; virtual;
  procedure PreparePath(const Path: String); virtual;
  procedure Seek(Message: Longint); virtual;
  procedure SeekNext; virtual;
  procedure SeekPrev; virtual;
  function SeekFound: Boolean; virtual;
  function GetLocation: Longint; virtual;
  procedure SetLocation(Location: Longint); virtual;
  function OpenMessage: Boolean; virtual;
  function OpenMessageHeader: Boolean; virtual;
  function CloseMessage: Boolean; virtual;
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
  procedure GetReplies(var Replies: TSquishMessageReplies);
  procedure SetReplies(var Replies: TSquishMessageReplies);
  function GetReplyTo: Longint; virtual;
  procedure SetReplyTo(const Value: Longint); virtual;
  procedure GetMessageHeader(var AHeader: TSquishMessageHeader);
  procedure GetBaseHeader(var AHeader: TSquishBaseHeader);
  procedure GetStreams(var ADataLink, AIndexLink: PMessageBaseStream);
  function GetHighWater: Longint;
  procedure SetHighWater(const AHighWater: Longint);
  procedure SeekHighWater;
  function GetRead: Boolean; virtual;
  procedure SetRead(const Value: Boolean); virtual;
  function GetFirstReply: Longint; virtual;
  procedure SetFirstReply(const Value: Longint); virtual;
 private
  SquishIndex: TSquishIndex;
  SquishFrame: TSquishFrame;
  SquishFramePosition: Longint;
  Created: Boolean;
  SquishBaseHeader: TSquishBaseHeader;
  SquishMessageHeader: TSquishMessageHeader;
  SquishIndexPos: Longint;
  DataLink: PMessageBaseStream;
  IndexLink: PMessageBaseStream;
  procedure GetIndex(const Pos: Longint; var Index: TSquishIndex);
  procedure SetIndex(const Pos: Longint; var Index: TSquishIndex);
  function CheckIndex(const Message: Longint; var Index: TSquishIndex; var IndexPos: Longint; const Nearest: Boolean): Boolean;
  function MapAttribute(var Attribute: Longint): Boolean;
  procedure SquishSwap(var A: Longint);
  function MakeHash(const Name: String): Longint;
  function GetFrame(const Position: Longint; var Frame: TSquishFrame): Boolean;
  function SetFrame(const Position: Longint; var Frame: TSquishFrame): Boolean;
  procedure SaveBaseHeader;
  procedure LinkToPrev(var Frame: TSquishFrame; const VictimPosition, FramePosition: Longint);
  procedure LinkToNext(var Frame: TSquishFrame; const VictimPosition, FramePosition: Longint);
  procedure LinkUpdate(var Frame: TSquishFrame; var FirstFrame, LastFrame: Longint; const FramePosition: Longint);
  procedure UnlinkFrame(var Frame: TSquishFrame; var FirstFrame, LastFrame: Longint);
  procedure InitRelativeTable; virtual;
 end;

implementation

constructor TSquishMessageBase.Init;
 begin
  inherited Init;

  SetBasePath('');

  SetCurrent(0);

  DataLink:=nil;
  IndexLink:=nil;
 end;

destructor TSquishMessageBase.Done;
 begin
  Close;

  inherited Done;
 end;

function TSquishMessageBase.Open(const Path: String): Boolean;
 begin
  if GetOpened then
   Close;

  Open:=False;

  PreparePath(Path);

  if not FileExists(GetBasePath + 'sqd') then
   begin
    SetStatus(smbAbsentSQD);

    Exit;
   end;

  if not FileExists(GetBasePath + 'sqi') then
   begin
    SetStatus(smbAbsentSQI);

    Exit;
   end;

  DataLink:=nil;
  IndexLink:=nil;

  repeat
   DataLink:=CreateMessageBaseFileStream(GetBasePath + 'sqd', smOpen or smDenyWrite);

   if DataLink^.Status <> smOk then
    begin
     SetStatus(smbCantOpenSQD);

     Break;
    end;

   IndexLink:=CreateMessageBaseFileStream(GetBasePath + 'sqi', smOpen or smDenyWrite);

   if IndexLink^.Status <> smOk then
    begin
     SetStatus(smbCantOpenSQI);

     Break;
    end;

   DataLink^.Read(SquishBaseHeader, SizeOf(SquishBaseHeader));

   Open:=True;
   SetOpened(True);

   InitRelativeTable;

   Exit;
  until False;

  if DataLink <> nil then
   Dispose(DataLink, Done);

  if IndexLink <> nil then
   Dispose(IndexLink, Done);

  DataLink:=nil;
  IndexLink:=nil;

  SetBasePath('');
 end;

function TSquishMessageBase.Create(const Path: String): Boolean;
 begin
  if GetOpened then
   Close;

  Create:=False;

  SetOpened(False);

  PreparePath(Path);

  CreateDirectory(ExtractPathname(Path));

  repeat
   DataLink:=nil;
   IndexLink:=nil;

   DataLink:=CreateMessageBaseFileStream(GetBasePath + 'sqd', smCreate or smDenyWrite);

   if DataLink^.Status <> smOk then
    begin
     SetStatus(smbCantCreateSQD);

     Break;
    end;

   IndexLink:=CreateMessageBaseFileStream(GetBasePath + 'sqi', smCreate or smDenyWrite);

   if IndexLink^.Status <> smOk then
    begin
     SetStatus(smbCantCreateSQI);

     Break;
    end;

   with SquishBaseHeader do
    begin
     Len:=SizeOf(SquishBaseHeader);
     Rsvd1:=0;
     NumMsg:=0;
     HighMsg:=0;
     SkipMsg:=0;
     HighWater:=0;
     UID:=1;

     FillChar(Base, SizeOf(Base), 0);

     ToASCIIZ(Copy(Copy(GetBasePath, 1, Length(GetBasePath) - 1), 1, 79), @Base);

     FirstFrame:=0;
     LastFrame:=0;
     FirstFree:=0;
     LastFree:=0;

     EndFrame:=SizeOf(SquishBaseHeader);
{
     MaxMsg:=SquishMaxMsg;
     KeepDays:=SquishKeepDays;
}
     SqHdrSize:=SizeOf(TSquishFrame);

     FillChar(Rsvd2, SizeOf(Rsvd2), 0);
    end;

   DataLink^.Write(SquishBaseHeader, SizeOf(SquishBaseHeader));

   Create:=True;
   SetOpened(True);

   New(RelativeTable, Init(1, 1));

   Exit;
  until True;

  if DataLink <> nil then
   Dispose(DataLink, Done);

  if IndexLink <> nil then
   Dispose(IndexLink, Done);

  DataLink:=nil;
  IndexLink:=nil;

  SetBasePath('');
 end;

function TSquishMessageBase.Exist(const Path: String): Boolean;
 begin
  PreparePath(Path);

  Exist:=FileExists(GetBasePath + 'sqd') and FileExists(GetBasePath + 'sqi');
 end;

procedure TSquishMessageBase.Close;
 begin
  if not GetOpened then
   Exit;

  SetOpened(False);

  SaveBaseHeader;

  Dispose(DataLink, Done);
  Dispose(IndexLink, Done);
  Dispose(RelativeTable, Done);

  DataLink:=nil;
  IndexLink:=nil;
  RelativeTable:=nil;
 end;

procedure TSquishMessageBase.PreparePath(const Path: String);
 begin
  if Copy(Path, Length(Path), 1) <> '.' then
   SetBasePath(Path + '.')
  else
   SetBasePath(Path);
 end;

procedure TSquishMessageBase.Seek(Message: Longint);
 begin
  if CheckIndex(RelativeToAbsolute(Message), SquishIndex, SquishIndexPos, True) then
   SetCurrent(AbsoluteToRelative(SquishIndex.Number))
  else
   begin
    SetCurrent(0);
    SquishIndexPos:=0;
   end;
 end;

procedure TSquishMessageBase.SeekNext;
 begin
  Inc(SquishIndexPos, SizeOf(SquishIndex));

  if SquishIndexPos >= IndexLink^.GetSize then
   begin
    SetCurrent(0);
    SquishIndexPos:=0;
   end
  else
   begin
    GetIndex(SquishIndexPos, SquishIndex);

    SetCurrent(AbsoluteToRelative(SquishIndex.Number));
   end;
 end;

procedure TSquishMessageBase.SeekPrev;
 begin
  Dec(SquishIndexPos, SizeOf(SquishIndex));

  if SquishIndexPos < 0 then
   begin
    SetCurrent(0);

    SquishIndexPos:=0;
   end
  else
   begin
    GetIndex(SquishIndexPos, SquishIndex);

    SetCurrent(AbsoluteToRelative(SquishIndex.Number));
   end;
 end;

function TSquishMessageBase.SeekFound: Boolean;
 begin
  SeekFound:=Current <> 0;
 end;

function TSquishMessageBase.GetLocation: Longint;
 begin
  GetLocation:=SquishIndexPos;
 end;

procedure TSquishMessageBase.SetLocation(Location: Longint);
 begin
  SquishIndexPos:=Location;

  GetIndex(SquishIndexPos, SquishIndex);

  if IndexLink^.Status <> smOk then
   begin
    SetCurrent(0);

    SquishIndexPos:=0;

    IndexLink^.Reset;
   end
  else
   SetCurrent(AbsoluteToRelative(SquishIndex.Number));
 end;

function TSquishMessageBase.OpenMessage: Boolean;
 var
  CIB: PControlInformationBuffer;
  CIBLength, K: Longint;
  Line: PChar;
  LineCh: Array[1..2] Of Char;
  EmptyCIB: Boolean;
 begin
  CloseMessage;

  OpenMessage:=False;

  if not GetFrame(SquishIndex.Offset, SquishFrame) then
   Exit;

  SquishFramePosition:=SquishIndex.Offset;

  if SquishFrame.Id <> squishFrameID then
   begin
    SetStatus(smbGrungedFrame);

    Exit;
   end;

  if SquishFrame.FrameType <> squishFrameMessage then
   begin
    SetStatus(smbGrungedFrame);

    Exit;
   end;

  DataLink^.Read(SquishMessageHeader, SizeOf(SquishMessageHeader));

  CIBLength:=SquishFrame.ControlLength + 1;

  if CIBLength > SizeOf(TControlInformationBuffer) - 1 then
   begin
    SetStatus(smbGrungedFrame);

    Exit;
   end;

  SetMessageTextStream(CreateMessageBaseMemoryStream(MaxMessageSize));

  GetMem(Line, MaxLineSize);

  GetMem(CIB, CIBLength);

  LineCh[2]:=#0;

  DataLink^.Read(CIB^, CIBLength - 1);

  CIB^[CIBLength]:=#1;

  EmptyCIB:=True;

  ToASCIIZ('', Line);

  for K:=1 to CIBLength do
   begin
    LineCh[1]:=CIB^[K];

    if LineCh[1] = #1 then
     if EmptyCIB then
      begin
       if K <> CIBLength then
        begin
         ToASCIIZ('', Line);
         EmptyCIB:=False;
        end;
      end
     else
      begin
       LineCh[1]:=#13;

       GetMessageTextStream^.Write(Line^, LenASCIIZ(Line));

       GetMessageTextStream^.Write(LineCh[1], 1);

       LineCh[1]:=#1;

       ToASCIIZ('', Line);
      end;

    ConcatASCIIZ(Line, @LineCh);
   end;

  FreeMem(CIB, CIBLength);

  FreeMem(Line, MaxLineSize);

  if SquishFrame.ControlLength > SizeOf(TControlInformationBuffer) - 1 then
   Exit;

  GetMessageTextStream^.CopyFrom(DataLink^, SquishFrame.MsgLength - SizeOf(SquishMessageHeader) -
   SquishFrame.ControlLength - 1);

  GetMessageTextStream^.Seek(0);

  Created:=False;

  OpenMessage:=True;
 end;

function TSquishMessageBase.OpenMessageHeader: Boolean;
 var
  CIB: PControlInformationBuffer;
  CIBLength, K: Longint;
  Line: PChar;
  LineCh: Array[1..2] Of Char;
  EmptyCIB: Boolean;
 begin
  CloseMessage;

  OpenMessageHeader:=False;

  if not GetFrame(SquishIndex.Offset, SquishFrame) then
   Exit;

  SquishFramePosition:=SquishIndex.Offset;

  if SquishFrame.Id <> squishFrameID then
   begin
    SetStatus(smbGrungedFrame);

    Exit;
   end;

  if SquishFrame.FrameType <> squishFrameMessage then
   begin
    SetStatus(smbGrungedFrame);

    Exit;
   end;

  DataLink^.Read(SquishMessageHeader, SizeOf(SquishMessageHeader));

  CIBLength:=SquishFrame.ControlLength + 1;

  if CIBLength > SizeOf(TControlInformationBuffer) - 1 then
   begin
    SetStatus(smbGrungedFrame);

    Exit;
   end;

  SetMessageTextStream(CreateMessageBaseMemoryStream(MaxMessageSize));

  GetMem(Line, MaxLineSize);

  GetMem(CIB, CIBLength);

  LineCh[2]:=#0;

  DataLink^.Read(CIB^, CIBLength - 1);

  CIB^[CIBLength]:=#1;

  EmptyCIB:=True;

  ToASCIIZ('', Line);

  for K:=1 to CIBLength do
   begin
    LineCh[1]:=CIB^[K];

    if LineCh[1] = #1 then
     if EmptyCIB then
      begin
       if K <> CIBLength then
        begin
         ToASCIIZ('', Line);
         EmptyCIB:=False;
        end;
      end
     else
      begin
       LineCh[1]:=#13;

       GetMessageTextStream^.Write(Line^, LenASCIIZ(Line));

       GetMessageTextStream^.Write(LineCh[1], 1);

       LineCh[1]:=#1;

       ToASCIIZ('', Line);
      end;

    ConcatASCIIZ(Line, @LineCh);
   end;

  FreeMem(CIB, CIBLength);

  FreeMem(Line, MaxLineSize);

  if SquishFrame.ControlLength > SizeOf(TControlInformationBuffer) - 1 then
   Exit;

  GetMessageTextStream^.Seek(0);

  Created:=False;

  OpenMessageHeader:=True;
 end;

function TSquishMessageBase.CloseMessage: Boolean;
 begin
  if GetMessageTextStream = nil then
   CloseMessage:=False
  else
   begin
    CloseMessage:=True;

    SetMessageTextStream(nil);
   end;
 end;

function TSquishMessageBase.GetFrom: String;
 begin
  GetFrom:=FromASCIIZ(@SquishMessageHeader.MsgFrom);
 end;

function TSquishMessageBase.GetTo: String;
 begin
  GetTo:=FromASCIIZ(@SquishMessageHeader.MsgTo);
 end;

function TSquishMessageBase.GetSubject: String;
 begin
  GetSubject:=FromASCIIZ(@SquishMessageHeader.Subj);
 end;

procedure TSquishMessageBase.SetFrom(const S: String);
 begin
  ToASCIIZ(Copy(S, 1, 35), @SquishMessageHeader.MsgFrom);
 end;

procedure TSquishMessageBase.SetTo(const S: String);
 begin
  ToASCIIZ(Copy(S, 1, 35), @SquishMessageHeader.MsgTo);
 end;

procedure TSquishMessageBase.SetSubject(const S: String);
 begin
  ToASCIIZ(Copy(S, 1, 71), @SquishMessageHeader.Subj);
 end;

procedure TSquishMessageBase.GetFromAddress(var Address: TAddress);
 begin
  ClearAddress(Address);

  inherited GetFromAddress(Address);
 end;

procedure TSquishMessageBase.GetToAddress(var Address: TAddress);
 begin
  ClearAddress(Address);

  inherited GetToAddress(Address);
 end;

procedure TSquishMessageBase.GetFromAndToAddress(var FromAddress, ToAddress: TAddress);
 begin
  ClearAddress(FromAddress);
  ClearAddress(ToAddress);

  inherited GetFromAndToAddress(FromAddress, ToAddress);
 end;

function TSquishMessageBase.GetAttribute(Attribute: Longint): Boolean;
 begin
  if not MapAttribute(Attribute) then
   begin
    GetAttribute:=False;

    Exit;
   end;

  GetAttribute:=SquishMessageHeader.Attr and Attribute <> 0;
 end;

procedure TSquishMessageBase.SetAttribute(Attribute: Longint; Enable: Boolean);
 begin
  if not MapAttribute(Attribute) then
   Exit;

  if Enable then
   SquishMessageHeader.Attr:=SquishMessageHeader.Attr or Attribute
  else
   SquishMessageHeader.Attr:=SquishMessageHeader.Attr and not Attribute
 end;

procedure TSquishMessageBase.GetWrittenDateTime(var DateTime: TMessageBaseDateTime);
 var
  A: Longint;
 begin
  A:=SquishMessageHeader.DateWritten;

  SquishSwap(A);

  DosDateTimeToMessageBaseDateTime(A, DateTime);
 end;

procedure TSquishMessageBase.GetArrivedDateTime(var DateTime: TMessageBaseDateTime);
 var
  A: Longint;
 begin
  A:=SquishMessageHeader.DateArrived;

  SquishSwap(A);

  DosDateTimeToMessageBaseDateTime(A, DateTime);
 end;

procedure TSquishMessageBase.SetWrittenDateTime(var DateTime: TMessageBaseDateTime);
 begin
  MessageBaseDateTimeToDosDateTime(DateTime, SquishMessageHeader.DateWritten);

  SquishSwap(SquishMessageHeader.DateWritten);
 end;

procedure TSquishMessageBase.SetArrivedDateTime(var DateTime: TMessageBaseDateTime);
 begin
  MessageBaseDateTimeToDosDateTime(DateTime, SquishMessageHeader.DateArrived);

  SquishSwap(SquishMessageHeader.DateArrived);
 end;

function TSquishMessageBase.WriteMessage: Boolean;
 function lz(const Number: Longint): String;
  var
   S: String[2];
  begin
   Str(Number, S);

   if S[0] = #1 then
    S:=Concat('0', S);

   lz:=S;
  end;
 var
  CIB: PControlInformationBuffer;
  CIBLength, TextStart: Longint;
  Line: PChar;
  FromAddress, ToAddress: TAddress;
  DateTime: TMessageBaseDateTime;
  TempSquishFrame: TSquishFrame;
  FixEndFrame: Boolean;
 const
  C: Char = #0;
 begin
  WriteMessage:=False;

  GetMem(Line, MaxLineSize);

  New(CIB);

  GetMessageTextStream^.Seek(0);

  CIBLength:=1;

  TextStart:=0;

  while not EndOfMessage do
   begin
    TextStart:=GetMessageTextStream^.GetPos;

    GetStringPChar(Line, MaxLineSize);

    if Line[0] <> #1 then
     Break;

    Move(Line[0], CIB^[CIBLength], LenASCIIZ(Line));

    Inc(CIBLength, LenASCIIZ(Line));
   end;

  if (TextStart = 0) and (Line[0] = #1) then
   TextStart:=GetMessageTextStream^.GetPos;

  CIB^[CIBLength]:=#0;

  TempSquishFrame:=SquishFrame;

  SquishFrame.MsgLength:=SizeOf(TSquishMessageHeader) + CIBLength + (GetMessageTextStream^.GetSize - TextStart) + 1;

  SquishFrame.FrameLength:=SquishFrame.MsgLength;

  if Created then
   begin
    SquishFrame.PrevFrame:=SquishBaseHeader.LastFrame;

    SquishFrame.NextFrame:=0;
   end;

  if TempSquishFrame.FrameLength >= SquishFrame.FrameLength then
   SquishFrame.FrameLength:=TempSquishFrame.FrameLength
  else
   begin
    if not Created then
     begin
      UnlinkFrame(TempSquishFrame, SquishBaseHeader.FirstFrame, SquishBaseHeader.LastFrame);

      TempSquishFrame.NextFrame:=0;

      TempSquishFrame.FrameType:=SquishFrameFree;

      LinkToPrev(TempSquishFrame, SquishBaseHeader.LastFree, SquishFramePosition);
      LinkUpdate(TempSquishFrame, SquishBaseHeader.FirstFree, SquishBaseHeader.LastFree, SquishFramePosition);

      SetFrame(SquishFramePosition, TempSquishFrame);
     end;

    SquishFramePosition:=SquishBaseHeader.FirstFree;

    while SquishFramePosition <> 0 do
     begin
      GetFrame(SquishFramePosition, TempSquishFrame);

      if TempSquishFrame.FrameLength >= SquishFrame.MsgLength then
       Break;

      SquishFramePosition:=TempSquishFrame.NextFrame;
     end;
   end;

  if SquishFramePosition = 0 then
   begin
{
    SquishFramePosition:=SquishBaseHeader.EndFrame;
    Inc(SquishBaseHeader.EndFrame, SquishFrame.FrameLength + SizeOf(TSquishFrame));
}

    SquishFramePosition:=DataLink^.GetSize;

    FixEndFrame:=True;
   end
  else
   begin
    SquishFrame.FrameLength:=TempSquishFrame.FrameLength;

    UnlinkFrame(TempSquishFrame, SquishBaseHeader.FirstFree, SquishBaseHeader.LastFree);

    FixEndFrame:=False;
   end;

  SquishIndex.Offset:=SquishFramePosition;
  SquishIndex.Hash:=MakeHash(GetTo);

  SquishFrame.ID:=SquishFrameID;
  SquishFrame.ControlLength:=CIBLength;
  SquishFrame.FrameType:=SquishFrameMessage;
  SquishFrame.Rsvd:=0;

  LinkToNext(SquishFrame, SquishFrame.NextFrame, SquishFramePosition);
  LinkToPrev(SquishFrame, SquishFrame.PrevFrame, SquishFramePosition);
  LinkUpdate(SquishFrame, SquishBaseHeader.FirstFrame, SquishBaseHeader.LastFrame, SquishFramePosition);

  GetFromAndToAddress(FromAddress, ToAddress);

  SquishMessageHeader.Orig.Zone:=FromAddress.Zone;
  SquishMessageHeader.Orig.Net:=FromAddress.Net;
  SquishMessageHeader.Orig.Node:=FromAddress.Node;
  SquishMessageHeader.Orig.Point:=FromAddress.Point;
  SquishMessageHeader.Dest.Zone:=ToAddress.Zone;
  SquishMessageHeader.Dest.Net:=ToAddress.Net;
  SquishMessageHeader.Dest.Node:=ToAddress.Node;
  SquishMessageHeader.Dest.Point:=ToAddress.Point;
  SquishMessageHeader.UTCOffset:=0;

  GetWrittenDateTime(DateTime);

  ToASCIIZ(lz(DateTime.Day) + ' ' +
           MonthNumberToMonthString(DateTime.Month) + ' ' +
           lz(Y2ToDouble(DateTime.Year)) + '  ' +
           lz(DateTime.Hour) + ':' +
           lz(DateTime.Min) + ':' +
           lz(DateTime.Sec),
           @SquishMessageHeader.AzDate);

  SetIndex(SquishIndexPos, SquishIndex);

  GetMessageTextStream^.Seek(TextStart);

  DataLink^.Seek(SquishFramePosition);
  DataLink^.Write(SquishFrame, SizeOf(SquishFrame));
  DataLink^.Write(SquishMessageHeader, SizeOf(SquishMessageHeader));
  DataLink^.Write(CIB^, CIBLength);
  DataLink^.CopyFrom(GetMessageTextStream^, GetMessageTextStream^.GetSize - TextStart);
  DataLink^.Write(C, SizeOf(C));

  Dispose(CIB);

  FreeMem(Line, MaxLineSize);

  if FixEndFrame then
   SquishBaseHeader.EndFrame:=DataLink^.GetSize;

  SaveBaseHeader;

  WriteMessage:=True;
 end;

function TSquishMessageBase.WriteMessageHeader: Boolean;
 begin
  DataLink^.Seek(SquishFramePosition + SizeOf(SquishFrame));
  DataLink^.Write(SquishMessageHeader, SizeOf(SquishMessageHeader));

  WriteMessageHeader:=True;
 end;

function TSquishMessageBase.CreateNewMessage: Boolean;
 begin
  FillChar(SquishMessageHeader, SizeOf(SquishMessageHeader), 0);
  FillChar(SquishFrame, SizeOf(SquishFrame), 0);
  FillChar(SquishIndex, SizeOf(SquishIndex), 0);

  SquishMessageHeader.UID:=SquishBaseHeader.UID;

  RelativeTable^.Insert(Pointer(SquishMessageHeader.UID));

  Inc(SquishBaseHeader.UID);

  Inc(SquishBaseHeader.NumMsg);

  Inc(SquishBaseHeader.HighMsg);

  SetCurrent(GetCount);

  SquishFrame.ID:=SquishFrameID;

  SquishFramePosition:=0;
  SquishMessageHeader.Attr:=squishaUID;

  SquishIndex.Number:=SquishMessageHeader.UID;

  IndexLink^.Reset;

  SquishIndexPos:=IndexLink^.GetSize;

  SetMessageTextStream(CreateMessageBaseMemoryStream(MaxMessageSize));

  ResetDateTime;

  Created:=True;
  CreateNewMessage:=True;
 end;

function TSquishMessageBase.KillMessage: Boolean;
 var
  Stream: PMessageBaseStream;
 begin
  GetFrame(SquishIndex.Offset, SquishFrame);

  SquishFramePosition:=SquishIndex.Offset;

  UnlinkFrame(SquishFrame, SquishBaseHeader.FirstFrame, SquishBaseHeader.LastFrame);

  Stream:=CreateMessageBaseMemoryStream(IndexLink^.GetSize - SquishIndexPos - SizeOf(SquishIndex));

  IndexLink^.Seek(SquishIndexPos + SizeOf(SquishIndex));

  Stream^.CopyFrom(IndexLink^, IndexLink^.GetSize - IndexLink^.GetPos);

  Stream^.Seek(0);

  IndexLink^.Seek(SquishIndexPos);

  IndexLink^.CopyFrom(Stream^, Stream^.GetSize);

  IndexLink^.Truncate;

  Dispose(Stream, Done);

  SquishFrame.NextFrame:=0;
  SquishFrame.FrameType:=SquishFrameFree;

  LinkToPrev(SquishFrame, SquishBaseHeader.LastFree, SquishFramePosition);
  LinkUpdate(SquishFrame, SquishBaseHeader.FirstFree, SquishBaseHeader.LastFree, SquishFramePosition);

  SetFrame(SquishFramePosition, SquishFrame);

  Dec(SquishBaseHeader.NumMsg);
  Dec(SquishBaseHeader.HighMsg);

  SaveBaseHeader;

  Dec(SquishIndexPos, SizeOf(SquishIndex));

  RelativeTable^.AtDelete(Current - 1);

  KillMessage:=True;
 end;

function TSquishMessageBase.GetLastRead(const UserNumber: Longint): Longint;
 var
  Stream: PMessageBaseStream;
  LastRead: Longint;
 begin
  Stream:=CreateMessageBaseFileStream(GetBasePath + 'sql', smOpenRead or smDenyWrite);

  if Stream^.Status <> smOk then
   GetLastRead:=0
  else
   if (UserNumber + 1) * SizeOf(LastRead) > Stream^.GetSize then
    GetLastRead:=0
   else
    begin
     Stream^.Seek(UserNumber * SizeOf(LastRead));

     Stream^.Read(LastRead, SizeOf(LastRead));

     GetLastRead:=AbsoluteToRelative(LastRead);
    end;

  Dispose(Stream, Done);
 end;

procedure TSquishMessageBase.SetLastRead(const UserNumber: Longint; const Value: Longint);
 var
  Stream: PMessageBaseStream;
  LastRead: Longint;
 begin
  Stream:=CreateMessageBaseFileStream(GetBasePath + 'sql', smOpenWrite or smDenyWrite);

  if Stream^.Status <> smOk then
   begin
    Dispose(Stream, Done);

    Stream:=CreateMessageBaseFileStream(GetBasePath + 'sql', smCreate or smDenyWrite);

    if Stream^.Status <> smOk then
     begin
      Dispose(Stream, Done);

      Exit;
     end;
   end;

  LastRead:=RelativeToAbsolute(Value);

  Stream^.Seek(UserNumber * SizeOf(LastRead));

  Stream^.Write(LastRead, SizeOf(LastRead));

  Dispose(Stream, Done);
 end;

procedure TSquishMessageBase.GetReplies(var Replies: TSquishMessageReplies);
 begin
  Replies:=SquishMessageHeader.Replies;
 end;

procedure TSquishMessageBase.SetReplies(var Replies: TSquishMessageReplies);
 begin
  SquishMessageHeader.Replies:=Replies;
 end;

function TSquishMessageBase.GetReplyTo: Longint;
 begin
  GetReplyTo:=SquishMessageHeader.ReplyTo;
 end;

procedure TSquishMessageBase.SetReplyTo(const Value: Longint);
 begin
  SquishMessageHeader.ReplyTo:=Value;
 end;

procedure TSquishMessageBase.GetMessageHeader(var AHeader: TSquishMessageHeader);
 begin
  AHeader:=SquishMessageHeader;
 end;

procedure TSquishMessageBase.GetBaseHeader(var AHeader: TSquishBaseHeader);
 begin
  AHeader:=SquishBaseHeader;
 end;

procedure TSquishMessageBase.GetStreams(var ADataLink, AIndexLink: PMessageBaseStream);
 begin
  ADataLink:=DataLink;
  AIndexLink:=IndexLink;
 end;

function TSquishMessageBase.GetHighWater: Longint;
 begin
  GetHighWater:=SquishBaseHeader.HighWater;
 end;

procedure TSquishMessageBase.SetHighWater(const AHighWater: Longint);
 begin
  SquishBaseHeader.HighWater:=AHighWater;

  SaveBaseHeader;
 end;

procedure TSquishMessageBase.SeekHighWater;
 begin
  if CheckIndex(SquishBaseHeader.HighWater, SquishIndex, SquishIndexPos, True) then
   SetCurrent(AbsoluteToRelative(SquishIndex.Number))
  else
   begin
    SetCurrent(0);

    SquishIndexPos:=0;
   end;
 end;

function TSquishMessageBase.GetRead: Boolean;
 begin
  GetRead:=SquishMessageHeader.Attr and squishaRead <> 0;
 end;

procedure TSquishMessageBase.SetRead(const Value: Boolean);
 begin
  if Value then
   SquishMessageHeader.Attr:=SquishMessageHeader.Attr or squishaRead
  else
   SquishMessageHeader.Attr:=SquishMessageHeader.Attr and not squishaRead;
 end;

function TSquishMessageBase.GetFirstReply: Longint;
 begin
  GetFirstReply:=SquishMessageHeader.Replies[1];
 end;

procedure TSquishMessageBase.SetFirstReply(const Value: Longint);
 begin
  SquishMessageHeader.Replies[1]:=Value;
 end;

{ private methods }

procedure TSquishMessageBase.GetIndex(const Pos: Longint; var Index: TSquishIndex);
 begin
  if IndexLink^.Status <> smOk then
   IndexLink^.Reset;

  IndexLink^.Seek(Pos);

  IndexLink^.Read(Index, SizeOf(Index));
 end;

procedure TSquishMessageBase.SetIndex(const Pos: Longint; var Index: TSquishIndex);
 begin
  if IndexLink^.Status <> smOk then
   IndexLink^.Reset;

  IndexLink^.Seek(Pos);

  IndexLink^.Write(Index, SizeOf(Index));
 end;

function TSquishMessageBase.CheckIndex(const Message: Longint; var Index: TSquishIndex; var IndexPos: Longint;
 const Nearest: Boolean): Boolean;
 var
  Countdown: Longint;
 begin
  IndexLink^.Seek(0);

  Countdown:=IndexLink^.GetSize div SizeOf(TSquishIndex);

  IndexPos:=0;

  while Countdown <> 0 do
   begin
    IndexLink^.Read(Index, SizeOf(Index));

    if (Index.Number = Message) or (Nearest and (Index.Number >= Message)) then
     begin
      CheckIndex:=True;

      Exit;
     end;

    Dec(Countdown);
    Inc(IndexPos, SizeOf(Index));
   end;

  CheckIndex:=False;
 end;

function TSquishMessageBase.MapAttribute(var Attribute: Longint): Boolean;
 begin
  MapAttribute:=True;

  case Attribute of
   maPrivate: Attribute:=squishaPrivate;
   maCrash: Attribute:=squishaCrash;
   maReceived: Attribute:=squishaReceived;
   maSent: Attribute:=squishaSent;
   maAttach: Attribute:=squishaAttach;
   maTransit: Attribute:=squishaTransit;
   maOrphan: Attribute:=squishaOrphan;
   maKill: Attribute:=squishaKill;
   maLocal: Attribute:=squishaLocal;
   maHold: Attribute:=squishaHold;
   maFRq: Attribute:=squishaFRq;
   maRRq: Attribute:=squishaRRq;
   maARq: Attribute:=squishaARq;
  else
   if Attribute = maScanned then
    Attribute:=squishaScanned
   else
    MapAttribute:=False;
  end;
 end;

procedure TSquishMessageBase.SquishSwap(var A: Longint);
 begin
  A:=(A shr 16) + ((A and $FFFF) shl 16);
 end;

function TSquishMessageBase.MakeHash(const Name: String): Longint;
 var
  K: Byte;
  Hash, Tmp: Longint;
 begin
  Hash:=0;

  for K:=1 to Length(Name) do
   begin
    Hash:=(Hash shl 4) + Byte(LoCase(Name[K]));

    Tmp:=Hash and $F0000000;

    if Tmp <> 0 then
     Hash:=(Hash or (Tmp shr 24)) or Tmp;
   end;

  MakeHash:=Hash and $7FFFFFFF;
 end;

function TSquishMessageBase.GetFrame(const Position: Longint; var Frame: TSquishFrame): Boolean;
 begin
  DataLink^.Reset;

  if Position = 0 then
   begin
    GetFrame:=False;

    Exit;
   end;

  DataLink^.Seek(Position);

  if DataLink^.Status <> smOk then
   begin
    SetStatus(smbDataProblems);

    GetFrame:=False;

    Exit;
   end;

  DataLink^.Read(Frame, SizeOf(Frame));

  if DataLink^.Status <> smOk then
   begin
    SetStatus(smbDataProblems);

    GetFrame:=False;
   end
  else
   GetFrame:=True;
 end;

function TSquishMessageBase.SetFrame(const Position: Longint; var Frame: TSquishFrame): Boolean;
 begin
  DataLink^.Reset;

  if Position = 0 then
   begin
    SetStatus(smbDataProblems);

    SetFrame:=False;

    Exit;
   end;

  DataLink^.Seek(Position);

  if DataLink^.Status <> smOk then
   begin
    SetStatus(smbDataProblems);

    SetFrame:=False;

    Exit;
   end;

  DataLink^.Write(Frame, SizeOf(Frame));

  if DataLink^.Status <> smOk then
   begin
    SetStatus(smbDataProblems);

    SetFrame:=False;
   end
  else
   SetFrame:=True;
 end;

procedure TSquishMessageBase.SaveBaseHeader;
 begin
  DataLink^.Reset;

  DataLink^.Seek(0);

  DataLink^.Write(SquishBaseHeader, SizeOf(SquishBaseHeader));
 end;

procedure TSquishMessageBase.LinkToPrev(var Frame: TSquishFrame; const VictimPosition, FramePosition: Longint);
 var
  TheVictim: TSquishFrame;
 begin
  if GetFrame(VictimPosition, TheVictim) then
   begin
    TheVictim.NextFrame:=FramePosition;

    Frame.PrevFrame:=VictimPosition;

    SetFrame(VictimPosition, TheVictim);
   end
  else
   Frame.PrevFrame:=0;
 end;

procedure TSquishMessageBase.LinkToNext(var Frame: TSquishFrame; const VictimPosition, FramePosition: Longint);
 var
  TheVictim: TSquishFrame;
 begin
  if GetFrame(VictimPosition, TheVictim) then
   begin
    TheVictim.PrevFrame:=FramePosition;

    Frame.NextFrame:=VictimPosition;

    SetFrame(VictimPosition, TheVictim);
   end
  else
   Frame.NextFrame:=0;
 end;

procedure TSquishMessageBase.LinkUpdate(var Frame: TSquishFrame; var FirstFrame, LastFrame: Longint;
  const FramePosition: Longint);
 begin
  if Frame.NextFrame = 0 then LastFrame:=FramePosition;
  if Frame.PrevFrame = 0 then FirstFrame:=FramePosition;
 end;

procedure TSquishMessageBase.UnlinkFrame(var Frame: TSquishFrame; var FirstFrame, LastFrame: Longint);
 var
  TempSquishFrame: TSquishFrame;
 begin
  if GetFrame(Frame.PrevFrame, TempSquishFrame) then
   begin
    LinkToNext(TempSquishFrame, Frame.NextFrame, Frame.PrevFrame);

    SetFrame(Frame.PrevFrame, TempSquishFrame);
   end
  else
   FirstFrame:=Frame.NextFrame;

  if GetFrame(Frame.NextFrame, TempSquishFrame) then
   begin
    LinkToPrev(TempSquishFrame, Frame.PrevFrame, Frame.NextFrame);

    SetFrame(Frame.NextFrame, TempsquishFrame);
   end
  else
   LastFrame:=Frame.PrevFrame;
 end;

procedure TSquishMessageBase.InitRelativeTable;
 var
  Countdown: Longint;
  Index: TSquishIndex;
 begin
  Countdown:=IndexLink^.GetSize div SizeOf(TSquishIndex);

  if Countdown > MaxMessages then
   begin
    IndexLink^.Seek((Countdown - MaxMessages) * SizeOf(TSquishIndex));

    Countdown := MaxMessages;
   end else
    IndexLink^.Seek(0);

  New(RelativeTable, Init(Countdown, 1));

  while Countdown <> 0 do
   begin
    IndexLink^.Read(Index, SizeOf(Index));

    RelativeTable^.Insert(Pointer(Index.Number));

    Dec(Countdown);
   end;
 end;

end.
