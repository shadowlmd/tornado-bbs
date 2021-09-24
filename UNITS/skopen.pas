{$IFDEF MSDOS}
  {$IFNDEF DPMI32}
    {$F+}
  {$IFNDEF DPMI}
    {$O+}
  {$ENDIF}
  {$ENDIF}
{$ENDIF}
{&Use32-}
unit skOpen;

interface
uses
     skMHL,

     skMHLmsg,
     skMHLjam,
     skMHLsq,

     skCommon,
     tMisc;

var
 OpenStatus: Longint;

type
 TMessageBaseOpenProcess = function(var Base: PMessageBase; const ID: String): Boolean;

const
 AttemptDelay   = 1000;

procedure SplitID(const ID: String; var Format: TMessageBaseFormat; var Path: String);

function OpenMessageBaseEx(var Base: PMessageBase; const ID: String; const Process: TMessageBaseOpenProcess;
 Attempts: Longint): Boolean;

function OpenMessageBase(var Base: PMessageBase; const ID: String): Boolean;
function OpenOrCreateMessageBase(var Base: PMessageBase; const ID: String): Boolean;
function CloseMessageBase(var Base: PMessageBase): Boolean;
function ExistMessageBase(const ID: String): Boolean;

function InitMessageBase(var Base: PMessageBase; const ID: String): Boolean;
function DoneMessageBase(var Base: PMessageBase): Boolean;

implementation

procedure SplitID(const ID: String; var Format: TMessageBaseFormat; var Path: String);
 begin
  if Length(ID) = 0 then
   begin
    Format:=mbfUnknown;

    Exit;
   end;

  case ID[1] of
   'f', 'F', 'm', 'M', '*': Format:=mbfMSG;
   'j', 'J': Format:=mbfJam;
   's', 'S': Format:=mbfSquish;
  else
   Format:=mbfUnknown;
  end;

  case Format of
   mbfMSG, mbfJam, mbfSquish: Path:=Copy(ID, 2, 255);
  else
   Path:=ID;
  end;
 end;

function OpenMessageBaseEx(var Base: PMessageBase; const ID: String; const Process: TMessageBaseOpenProcess;
 Attempts: Longint): Boolean;
 begin
  repeat
   if Process(Base, ID) then
    begin
     OpenMessageBaseEx:=True;

     Exit;
    end;

   if OpenStatus <> ombLocked then
    begin
     OpenMessageBaseEx:=False;

     Exit;
    end;

   Pause(AttemptDelay);

   Dec(Attempts);
  until Attempts < 1;

  OpenMessageBaseEx:=False;

  OpenStatus:=ombLockedAttemptsExpired;
 end;

function OpenMessageBase(var Base: PMessageBase; const ID: String): Boolean;
 var
  Format: TMessageBaseFormat;
  Path: String;
 begin
  if InitMessageBase(Base, ID) then
   begin
    SplitID(ID, Format, Path);

    if not Base^.Open(Path) then
     begin
      OpenStatus:=Base^.GetStatus;

      OpenMessageBase:=False;

      DoneMessageBase(Base);
     end
    else
     OpenMessageBase:=True;
   end
  else
   OpenMessageBase:=False;
 end;

function OpenOrCreateMessageBase(var Base: PMessageBase; const ID: String): Boolean;
 var
  Format: TMessageBaseFormat;
  Path: String;
 begin
  if InitMessageBase(Base, ID) then
   begin
    SplitID(ID, Format, Path);

    if not Base^.Open(Path) then
     if Base^.Exist(Path) then
      begin
       OpenStatus:=ombLocked;

       OpenOrCreateMessageBase:=False;

       Exit;
      end
     else
      if not Base^.Create(Path) then
       begin
        OpenStatus:=Base^.GetStatus;

        OpenOrCreateMessageBase:=False;

        Exit;
       end;

    OpenOrCreateMessageBase:=True;
   end
  else
   OpenOrCreateMessageBase:=False;
 end;

function CloseMessageBase(var Base: PMessageBase): Boolean;
 begin
  if Base <> nil then
   begin
    Base^.Close;

    CloseMessageBase:=DoneMessageBase(Base);
   end
  else
   CloseMessageBase:=False;
 end;

function ExistMessageBase(const ID: String): Boolean;
 var
  Format: TMessageBaseFormat;
  Base: PMessageBase;
  Path: String;
 begin
  if InitMessageBase(Base, ID) then
   begin
    SplitID(ID, Format, Path);

    ExistMessageBase:=Base^.Exist(Path);

    DoneMessageBase(Base);
   end
  else
   ExistMessageBase:=False;
 end;

function InitMessageBase(var Base: PMessageBase; const ID: String): Boolean;
 var
  Format: TMessageBaseFormat;
  Path: String;
 begin
  Base:=nil;

  InitMessageBase:=False;

  OpenStatus:=ombUnknownIDorUnsupported;

  if Length(ID) = 0 then
   Exit;

  SplitID(ID, Format, Path);

  case Format of
   mbfMSG: Base:=New(PFidoMessageBase, Init);
   mbfSquish: Base:=New(PSquishMessageBase, Init);
   mbfJam: Base:=New(PJamMessageBase, Init);
  else
   Exit;
  end;

  InitMessageBase:=True;
 end;

function DoneMessageBase(var Base: PMessageBase): Boolean;
 begin
  DoneMessageBase:=Base <> nil;

  if Base <> nil then
   Dispose(Base, Done);

  Base:=nil;
 end;

end.