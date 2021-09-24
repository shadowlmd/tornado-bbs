{ TV objects code (VP/BP) }

{ TMessageBaseFileStream }

constructor TMessageBaseFileStream.Init;
 var
  BMode: Longint;
 begin
  case AMode and $FF of
   smCreate: BMode:=stCreate;
   smOpen: BMode:=stOpen;
   smOpenRead: BMode:=stOpenRead;
   smOpenWrite: BMode:=stOpenWrite;
  else
   Fail;
  end;

  BMode:=(BMode and not $F0) or (AMode and $FF00 div $100);

  inherited Init(AFileName, AMode);

  Link:=New(PDosStream, Init(AFileName, BMode));

  UpdateStatus;
 end;

destructor TMessageBaseFileStream.Done;
 begin
  Dispose(Link, Done);

  inherited Done;
 end;

procedure TMessageBaseFileStream.Read(var Buf; Count: Word);
 begin
  Link^.Read(Buf, Count);

  UpdateStatus;
 end;

procedure TMessageBaseFileStream.Write(var Buf; Count: Word);
 begin
  Link^.Write(Buf, Count);

  UpdateStatus;
 end;

procedure TMessageBaseFileStream.Seek(Position: Longint);
 begin
  Link^.Seek(Position);

  UpdateStatus;
 end;

function TMessageBaseFileStream.GetPos: Longint;
 begin
  GetPos:=Link^.GetPos;

  UpdateStatus;
 end;

function TMessageBaseFileStream.GetSize: Longint;
 begin
  GetSize:=Link^.GetSize;

  UpdateStatus;
 end;

procedure TMessageBaseFileStream.Flush;
 begin
  Link^.Flush;

  UpdateStatus;
 end;

procedure TMessageBaseFileStream.Truncate;
 begin
  Link^.Truncate;

  UpdateStatus;
 end;

procedure TMessageBaseFileStream.Reset;
 begin
  Link^.Reset;

  UpdateStatus;
 end;

procedure TMessageBaseFileStream.UpdateStatus;
 begin
  Status:=Link^.Status;
 end;

{ TMessageBaseFileFind }

function TMessageBaseFileFind.StartSearch(const Mask: String; Attributes: Longint): Boolean;
 begin
  FindFirst(Mask, Attributes, Sr);

  Fresh;

  StartSearch:=DosError = 0;
 end;

function TMessageBaseFileFind.NextSearch: Boolean;
 begin
  FindNext(Sr);

  Fresh;

  NextSearch:=DosError = 0;
 end;

procedure TMessageBaseFileFind.StopSearch;
 begin
  {$IFDEF VIRTUALPASCAL}
  FindClose(Sr);
  {$ENDIF}
 end;

procedure TMessageBaseFileFind.Fresh;
 begin
  iName:=Sr.Name;
  iAttributes:=Sr.Attr;
  iTime:=Sr.Time;
  iSize:=Sr.Size;
 end;

{ TMessageBaseMemoryStream }

constructor TMessageBaseRamStream.Init;
 begin
  inherited Init('', 0);

  New(First);

  First^.Size:=0;
  First^.Next:=nil;

  GetMem(First^.Buffer, First^.Size);

  CurPos:=0;
  CurSize:=0;
  VirtualSize:=First^.Size;
 end;

destructor TMessageBaseRamStream.Done;
 var
  Block, SavedBlock: PMessageBaseRamBlock;
 begin
  Block:=First;

  while Block <> nil do
   begin
    FreeMem(Block^.Buffer, Block^.Size);

    SavedBlock:=Block;

    Block:=Block^.Next;

    Dispose(SavedBlock);
   end;

  inherited Done;
 end;

function TMessageBaseRamStream.GetPos: Longint;
 begin
  GetPos:=CurPos;
 end;

function TMessageBaseRamStream.GetSize: Longint;
 begin
  GetSize:=VirtualSize;
 end;

procedure TMessageBaseRamStream.Read(var Buf; Count: Word);
 var
  Buffer: TMessageBaseRamBuffer absolute Buf;
  Block: PMessageBaseRamBlock;
  K, Offset, Remain: Longint;
 begin
  K:=0;

  while K < Count do
   begin
    GetPosEx(CurPos, Block, Offset, Remain);

    if Block = nil then
     Break;

    if Remain > Count - K then
     Remain:=Count - K;

    Move(Block^.Buffer^[Offset], Buffer[K], Remain);

    Inc(CurPos, Remain);

    Inc(K, Remain);
   end;
 end;

procedure TMessageBaseRamStream.Seek(Pos: Longint);
 begin
  CurPos:=Pos;
 end;

procedure TMessageBaseRamStream.Truncate;
 begin
  VirtualSize:=CurPos;

  SetSize(CurPos);
 end;

procedure TMessageBaseRamStream.Write(var Buf; Count: Word);
 var
  Buffer: TMessageBaseRamBuffer absolute Buf;
  Block: PMessageBaseRamBlock;
  K, Offset, Remain: Longint;
 begin
  if CurPos + Count > CurSize then
   SetSize(CurPos + Count);

  if CurPos + Count > VirtualSize then
   VirtualSize:=CurPos + Count;

  K:=0;

  while K < Count do
   begin
    GetPosEx(CurPos, Block, Offset, Remain);

    if Block = nil then
     Break;

    if Remain > Count - K then
     Remain:=Count - K;

    Move(Buffer[K], Block^.Buffer^[Offset], Remain);

    Inc(CurPos, Remain);

    Inc(K, Remain);
   end;
 end;

procedure TMessageBaseRamStream.CopyFrom(var S: TMessageBaseStream; Count: Longint);
 begin
  if CurPos + Count > CurSize then
   SetSize(CurPos + Count);

  if CurPos + Count > VirtualSize then
   VirtualSize:=CurPos + Count;

  inherited CopyFrom(S, Count);
 end;

procedure TMessageBaseRamStream.Flush;
 begin
 end;

procedure TMessageBaseRamStream.SetSize(NewSize: Longint);
 var
  NextBlockSize: Longint;
  Block, AnotherBlock, Last: PMessageBaseRamBlock;
  Temp: Pointer;
 begin
  NewSize:=(NewSize div BlockSize + 1) * BlockSize;

  if NewSize > CurSize then
   begin
    Last:=First;

    while Last^.Next <> nil do
     Last:=Last^.Next;

    repeat
     NextBlockSize:=NewSize - CurSize;

     if NextBlockSize = 0 then
      Break;

     if NextBlockSize > BlockSize then
      NextBlockSize:=BlockSize;

     Block:=Last;

     if Block^.Size < NextBlockSize then
      begin
       Dec(CurSize, Block^.Size);
       Inc(CurSize, NextBlockSize);

       GetMem(Temp, Block^.Size);
       Move(Block^.Buffer^, Temp^, Block^.Size);
       FreeMem(Block^.Buffer, Block^.Size);

       GetMem(Block^.Buffer, NextBlockSize);
       Move(Temp^, Block^.Buffer^, Block^.Size);
       FreeMem(Temp, Block^.Size);

       Block^.Size:=NextBlockSize;
      end
     else
      begin
       New(AnotherBlock);

       AnotherBlock^.Size:=NextBlockSize;
       AnotherBlock^.Next:=nil;

       GetMem(AnotherBlock^.Buffer, NextBlockSize);

       Block^.Next:=AnotherBlock;

       Inc(CurSize, AnotherBlock^.Size);

       Last:=AnotherBlock;
      end;
    until False;
   end else
  if NewSize < CurSize then
   repeat
    NextBlockSize:=CurSize - NewSize;

    if NextBlockSize = 0 then
     Break;

    Block:=First;

    while Block^.Next <> nil do
     begin
      AnotherBlock:=Block;

      Block:=Block^.Next;
     end;

    if NextBlockSize > Block^.Size then
     begin
      AnotherBlock^.Next:=nil;

      FreeMem(Block^.Buffer, Block^.Size);

      Dec(CurSize, Block^.Size);

      Dispose(Block);

      Last:=AnotherBlock;
     end
    else
     begin
      NextBlockSize:=Block^.Size - NextBlockSize;

      Dec(CurSize, Block^.Size);

      Inc(CurSize, NextBlockSize);

      GetMem(Temp, NextBlockSize);
      Move(Block^.Buffer^, Temp^, NextBlockSize);
      FreeMem(Block^.Buffer, Block^.Size);

      GetMem(Block^.Buffer, NextBlockSize);
      Move(Temp^, Block^.Buffer^, NextBlockSize);
      FreeMem(Temp, NextBlockSize);

      Block^.Size:=NextBlockSize;
     end;
   until False;

  CurSize:=NewSize;
 end;

procedure TMessageBaseRamStream.GetPosEx(Position: Longint; var Block: PMessageBaseRamBlock; var Offset, Remain: Longint);
 begin
  Block:=First;

  repeat
   if Position < Block^.Size then
    begin
     Offset:=Position;

     Remain:=Block^.Size - Position;

     Exit;
    end;

   Dec(Position, Block^.Size);

   Block:=Block^.Next;
  until Block = nil;

  Offset:=-1;

  Remain:=-1;
 end;

{ wrapper }

function CreateMessageBaseFileStream(const FileName: String; Mode: Longint): PMessageBaseStream;
 begin
  CreateMessageBaseFileStream:=New(PMessageBaseFileStream, Init(FileName, Mode));
 end;

function CreateMessageBaseMemoryStream(Maximum: Longint): PMessageBaseStream;
 begin
  CreateMessageBaseMemoryStream:=New(PMessageBaseRamStream, Init);
 end;

function CreateMessageBaseFind: PMessageBaseFind;
 begin
  CreateMessageBaseFind:=New(PMessageBaseFileFind, Init);
 end;
