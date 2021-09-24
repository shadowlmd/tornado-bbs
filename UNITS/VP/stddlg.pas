
{*******************************************************}
{                                                       }
{       Turbo Pascal Version 7.0                        }
{       Turbo Vision Unit                               }
{                                                       }
{       Copyright (c) 1992 Borland International        }
{                                                       }
{       Virtual Pascal v2.1                             }
{       Copyright (C) 1996-2000 vpascal.com             }
{                                                       }
{*******************************************************}

unit StdDlg;

{$V-,X+,I-,S-,Cdecl-,Use32+}

interface

uses Objects, Drivers, Views, Dialogs, Dos;

const

{ Commands }

  cmFileOpen    = 800;   { Returned from TFileDialog when Open pressed }
  cmFileReplace = 801;   { Returned from TFileDialog when Replace pressed }
  cmFileClear   = 802;   { Returned from TFileDialog when Clear pressed }
  cmFileInit    = 803;   { Used by TFileDialog internally }
  cmChangeDir   = 804;   { Used by TChDirDialog internally }
  cmRevert      = 805;   { Used by TChDirDialog internally }

{ Messages }

  cmFileFocused = 806;    { A new file was focused in the TFileList }
  cmFileDoubleClicked     { A file was selected in the TFileList }
                = 807;

type

  { TSearchRec }

  {  Record used to store directory information by TFileDialog }

  TSearchRec = record
    Attr: Byte;
    Time: Longint;
    Size: Longint;
    Name: String;
  end;

type

  { TFileInputLine is a special input line that is used by      }
  { TFileDialog that will update its contents in response to a  }
  { cmFileFocused command from a TFileList.                     }

  PFileInputLine = ^TFileInputLine;
  TFileInputLine = object(TInputLine)
    constructor Init(var Bounds: TRect; AMaxLen: Integer);
    procedure HandleEvent(var Event: TEvent); virtual;
  end;

  { TFileCollection is a collection of TSearchRec's.            }

  PFileCollection = ^TFileCollection;
  TFileCollection = object(TSortedCollection)
    function Compare(Key1, Key2: Pointer): Integer; virtual;
    procedure FreeItem(Item: Pointer); virtual;
    function GetItem(var S: TStream): Pointer; virtual;
    procedure PutItem(var S: TStream; Item: Pointer); virtual;
  end;

  { TSortedListBox is a TListBox that assumes it has a          }
  { TStoredCollection instead of just a TCollection.  It will   }
  { perform an incremental search on the contents.              }

  PSortedListBox = ^TSortedListBox;
  TSortedListBox = object(TListBox)
    SearchPos: Word;
    ShiftState: Byte;
    constructor Init(var Bounds: TRect; ANumCols: Word;
      AScrollBar: PScrollBar);
    procedure HandleEvent(var Event: TEvent); virtual;
    function GetKey(var S: String): Pointer; virtual;
    procedure NewList(AList: PCollection); virtual;
  end;

  { TFileList is a TSortedList box that assumes it contains     }
  { a TFileCollection as its collection.  It also communicates  }
  { through broadcast messages to TFileInput and TInfoPane      }
  { what file is currently selected.                            }

  PFileList = ^TFileList;
  TFileList = object(TSortedListBox)
    constructor Init(var Bounds: TRect; AScrollBar: PScrollBar);
    destructor Done; virtual;
    function DataSize: Word; virtual;
    procedure FocusItem(Item: Integer); virtual;
    procedure GetData(var Rec); virtual;
    function GetText(Item: Integer; MaxLen: Integer): String; virtual;
    function GetKey(var S: String): Pointer; virtual;
    procedure HandleEvent(var Event: TEvent); virtual;
    procedure ReadDirectory(AWildCard: PathStr); virtual;
    procedure SetData(var Rec); virtual;
  end;

  { TFileInfoPane is a TView that displays the information      }
  { about the currently selected file in the TFileList          }
  { of a TFileDialog.                                           }

  PFileInfoPane = ^TFileInfoPane;
  TFileInfoPane = object(TView)
    S: TSearchRec;
    constructor Init(var Bounds: TRect);
    procedure Draw; virtual;
    function GetPalette: PPalette; virtual;
    procedure HandleEvent(var Event: TEvent); virtual;
  end;

  { TFileDialog is a standard file name input dialog            }

  TWildStr = PathStr;

const
  fdOkButton      = $0001;      { Put an OK button in the dialog }
  fdOpenButton    = $0002;      { Put an Open button in the dialog }
  fdReplaceButton = $0004;      { Put a Replace button in the dialog }
  fdClearButton   = $0008;      { Put a Clear button in the dialog }
  fdHelpButton    = $0010;      { Put a Help button in the dialog }
  fdNoLoadDir     = $0100;      { Do not load the current directory }
                                { contents into the dialog at Init. }
                                { This means you intend to change the }
                                { WildCard by using SetData or store }
                                { the dialog on a stream. }

type

  PFileDialog = ^TFileDialog;
  TFileDialog = object(TDialog)
    FileName: PFileInputLine;
    FileList: PFileList;
    WildCard: TWildStr;
    Directory: PString;
    constructor Init(AWildCard: TWildStr; const ATitle,
      InputName: String; AOptions: Word; HistoryId: Byte);
    constructor Load(var S: TStream);
    destructor Done; virtual;
    procedure GetData(var Rec); virtual;
    procedure GetFileName(var S: PathStr);
    procedure HandleEvent(var Event: TEvent); virtual;
    procedure SetData(var Rec); virtual;
    procedure Store(var S: TStream);
    function Valid(Command: Word): Boolean; virtual;
    procedure ReadDirectory;
  end;

  { TDirEntry }

  PDirEntry = ^TDirEntry;
  TDirEntry = record
    DisplayText: PString;
    Directory: PString;
  end;

  { TDirCollection is a collection of TDirEntry's used by       }
  { TDirListBox.                                                }

  PDirCollection = ^TDirCollection;
  TDirCollection = object(TCollection)
    function GetItem(var S: TStream): Pointer; virtual;
    procedure FreeItem(Item: Pointer); virtual;
    procedure PutItem(var S: TStream; Item: Pointer); virtual;
  end;

  { TDirListBox displays a tree of directories for use in the }
  { TChDirDialog.                                               }

  PDirListBox = ^TDirListBox;
  TDirListBox = object(TListBox)
    Dir: DirStr;
    Cur: Word;
    constructor Init(var Bounds: TRect; AScrollBar: PScrollBar);
    destructor Done; virtual;
    function GetText(Item: Integer; MaxLen: Integer): String; virtual;
    procedure HandleEvent(var Event: TEvent); virtual;
    function IsSelected(Item: Integer): Boolean; virtual;
    procedure NewDirectory(var ADir: DirStr);
    procedure SetState(AState: Word; Enable: Boolean); virtual;
  end;

  { TChDirDialog is a standard change directory dialog.         }

const
  cdNormal     = $0000; { Option to use dialog immediately }
  cdNoLoadDir  = $0001; { Option to init the dialog to store on a stream }
  cdHelpButton = $0002; { Put a help button in the dialog }

type

  PChDirDialog = ^TChDirDialog;
  TChDirDialog = object(TDialog)
    DirInput: PInputLine;
    DirList: PDirListBox;
    OkButton: PButton;
    ChDirButton: PButton;
    constructor Init(AOptions: Word; HistoryId: Word);
    constructor Load(var S: TStream);
    function DataSize: Word; virtual;
    procedure GetData(var Rec); virtual;
    procedure HandleEvent(var Event: TEvent); virtual;
    procedure SetData(var Rec); virtual;
    procedure Store(var S: TStream);
    function Valid(Command: Word): Boolean; virtual;
    procedure SetUpDialog;
  end;

const

  CInfoPane = #30;

  { TStream registration records }

const
  RFileInputLine: TStreamRec = (
     ObjType: 60;
     VmtLink: Ofs(TypeOf(TFileInputLine)^);
     Load:    @TFileInputLine.Load;
     Store:   @TFileInputLine.Store
  );

const
  RFileCollection: TStreamRec = (
     ObjType: 61;
     VmtLink: Ofs(TypeOf(TFileCollection)^);
     Load:    @TFileCollection.Load;
     Store:   @TFileCollection.Store
  );

const
  RFileList: TStreamRec = (
     ObjType: 62;
     VmtLink: Ofs(TypeOf(TFileList)^);
     Load:    @TFileList.Load;
     Store:   @TFileList.Store
  );

const
  RFileInfoPane: TStreamRec = (
     ObjType: 63;
     VmtLink: Ofs(TypeOf(TFileInfoPane)^);
     Load:    @TFileInfoPane.Load;
     Store:   @TFileInfoPane.Store
  );

const
  RFileDialog: TStreamRec = (
     ObjType: 64;
     VmtLink: Ofs(TypeOf(TFileDialog)^);
     Load:    @TFileDialog.Load;
     Store:   @TFileDialog.Store
  );

const
  RDirCollection: TStreamRec = (
     ObjType: 65;
     VmtLink: Ofs(TypeOf(TDirCollection)^);
     Load:    @TDirCollection.Load;
     Store:   @TDirCollection.Store
  );

const
  RDirListBox: TStreamRec = (
     ObjType: 66;
     VmtLink: Ofs(TypeOf(TDirListBox)^);
     Load:    @TDirListBox.Load;
     Store:   @TDirListBox.Store
  );

const
  RChDirDialog: TStreamRec = (
     ObjType: 67;
     VmtLink: Ofs(TypeOf(TChDirDialog)^);
     Load:    @TChDirDialog.Load;
     Store:   @TChDirDialog.Store
  );

const
  RSortedListBox: TStreamRec = (
     ObjType: 68;
     VmtLink: Ofs(TypeOf(TSortedListBox)^);
     Load:    @TSortedListBox.Load;
     Store:   @TSortedListBox.Store
  );

procedure RegisterStdDlg;
function PathValid(var Path: PathStr): Boolean; { !!! made public }
function IsWild(const S: String): Boolean;      { !!! made public }
function IsDir(const S: String): Boolean;       { !!! made public }

implementation

uses
  App, Memory, HistList, MsgBox, VpSysLow, VpString;

function DriveValid(Drive: Char): Boolean;
begin
  DriveValid := ((1 shl (Ord(UpCase(Drive)) - Ord('A'))) and SysGetValidDrives) <> 0;
end;

function PathValid(var Path: PathStr): Boolean;
var
  ExpPath: PathStr;
  SR: SearchRec;
begin
  ExpPath := FExpand(Path);

  if (Length(ExpPath) <= 3)
{$IfDef Linux}
  and (FileSystem <> fsUnix)
{$EndIf}
   then PathValid := DriveValid(ExpPath[1])
  else
  begin
    if (Length(ExpPath) > 1) and (ExpPath[Length(ExpPath)] = SysPathSep) then
      Dec(ExpPath[0]);
    FindFirst(ExpPath, Directory shl 8 or AnyFile, SR);
    PathValid := (DosError = 0); // and (SR.Attr and Directory <> 0);
    FindClose(SR);
  end;
end;

function ValidFileName(var FileName: PathStr): Boolean;
const
{$Ifdef VirtualPascal}
  IllegalChars = '?*<>|';
{$Else}
  IllegalChars = ';,=+<>|"[] ';
{$Endif}
var
  Dir: DirStr;
  Name: NameStr;
  Ext: ExtStr;

{ Contains returns true if S1 contains any characters in S2 }
function Contains(S1, S2: String): Boolean; assembler; {$USES esi,edi}{$FRAME-}
asm
                cld
                xor     eax,eax
                xor     ecx,ecx
                mov     esi,S1
                mov     edx,S2
                lodsb
                test    al,al
                jz      @@4
                mov     ah,al
                mov     cl,[edx]
                inc     edx
              @@1:
                push    ecx
                mov     edi,edx
                lodsb
                repne   scasb
                pop     ecx
                je      @@3
                dec     ah
                jnz     @@1
              @@2:
                xor     al,al
                jmp     @@4
              @@3:
                mov     al,1
              @@4:
end;

begin
  ValidFileName := True;
  FSplit(FileName, Dir, Name, Ext);
  if not ((Dir = '') or PathValid(Dir)) or Contains(Name, IllegalChars) or
    Contains(Dir, IllegalChars) then ValidFileName := False;
end;

function GetCurDir: DirStr;
var
  CurDir: DirStr;
begin
  GetDir(0, CurDir);
  if Length(CurDir) > 3 then
  begin
    Inc(CurDir[0]);
    CurDir[Length(CurDir)] := SysPathSep;
  end;
  GetCurDir := CurDir;
end;

type
  PSearchRec = ^TSearchRec;

function IsWild(const S: String): Boolean;
begin
  IsWild := (Pos('?',S) > 0) or (Pos('*',S) > 0);
end;

function IsDir(const S: String): Boolean;
var
  SR: SearchRec;
begin
  if (Length(S) = 3) and (S[2] = ':') and (S[3] = '\') and DriveValid(S[1])
  {$IfDef Linux}
  and (FileSystem <> fsUnix)
  {$EndIf}
   then
    IsDir := True
  else
    begin
      FindFirst(S, Directory shl 8 or AnyFile, SR);
      IsDir := DosError = 0;
      FindClose(SR);
    end;
end;

{ TFileInputLine }

constructor TFileInputLine.Init(var Bounds: TRect; AMaxLen: Integer);
begin
  TInputLine.Init(Bounds, AMaxLen);
  EventMask := EventMask or evBroadcast;
end;

procedure TFileInputLine.HandleEvent(var Event: TEvent);
var
  Dir: DirStr;
  Name: NameStr;
  Ext: ExtStr;
begin
  TInputLine.HandleEvent(Event);
  if (Event.What = evBroadcast) and (Event.Command = cmFileFocused) and
    (State and sfSelected = 0) then
  begin
     if PSearchRec(Event.InfoPtr)^.Attr and Directory <> 0 then
        Data^ := PSearchRec(Event.InfoPtr)^.Name + SysPathSep+
          PFileDialog(Owner)^.WildCard
     else Data^ := PSearchRec(Event.InfoPtr)^.Name;
     DrawView;
  end;
end;

{ TFileCollection }

function TFileCollection.Compare(Key1, Key2: Pointer): Integer;
begin
  if PSearchRec(Key1)^.Name = PSearchRec(Key2)^.Name then Compare := 0
  else if PSearchRec(Key1)^.Name = '..' then Compare := 1
  else if PSearchRec(Key2)^.Name = '..' then Compare := -1
  else if (PSearchRec(Key1)^.Attr and Directory <> 0) and
     (PSearchRec(Key2)^.Attr and Directory = 0) then Compare := 1
  else if (PSearchRec(Key2)^.Attr and Directory <> 0) and
     (PSearchRec(Key1)^.Attr and Directory = 0) then Compare := -1
  else if PSearchRec(Key1)^.Name > PSearchRec(Key2)^.Name then
    Compare := 1
  else Compare := -1;
end;

procedure TFileCollection.FreeItem(Item: Pointer);
begin
  Dispose(PSearchRec(Item));
end;

function TFileCollection.GetItem(var S: TStream): Pointer;
var
  Item: PSearchRec;
begin
  New(Item);
  S.Read(Item^, SizeOf(TSearchRec));
  GetItem := Item;
end;

procedure TFileCollection.PutItem(var S: TStream; Item: Pointer);
begin
  S.Write(Item^, SizeOf(TSearchRec));
end;

{ TSortedListBox }

constructor TSortedListBox.Init(var Bounds: TRect; ANumCols: Word;
  AScrollBar: PScrollBar);
begin
  TListBox.Init(Bounds, ANumCols, AScrollBar);
  SearchPos := 0;
  ShowCursor;
  SetCursor(1,0);
end;

procedure TSortedListBox.HandleEvent(var Event: TEvent);
var
  CurString, NewString: String;
  K: Pointer;
  Value, OldPos, OldValue: Integer;
  T: Boolean;

function Equal(const S1, S2: String; Count: Word): Boolean;
var
  I: Word;
begin
  Equal := False;
  if (Length(S1) < Count) or (Length(S2) < Count) then Exit;
  for I := 1 to Count do
    if UpCase(S1[I]) <> UpCase(S2[I]) then Exit;
  Equal := True;
end;

begin
  OldValue := Focused;
  TListBox.HandleEvent(Event);
  if (OldValue <> Focused) or
     (
      (Event.What = evBroadcast) and
      (Event.InfoPtr = @Self) and
      (Event.Command = cmReleasedFocus)
     )
    then SearchPos := 0;
  if Event.What = evKeyDown then
  begin
    if Event.CharCode <> #0 then
    begin
      Value := Focused;
      if Value < Range then CurString := GetText(Value, 255)
      else CurString := '';
      OldPos := SearchPos;
      if Event.KeyCode = kbBack then
      begin
        if SearchPos = 0 then Exit;
        Dec(SearchPos);
        if SearchPos = 0 then ShiftState := GetShiftState;
        CurString[0] := Char(SearchPos);
      end
      else if (Event.CharCode = '.') then SearchPos := Pos('.',CurString)
      else
      begin
        Inc(SearchPos);
        if SearchPos = 1 then ShiftState := GetShiftState;
        CurString[0] := Char(SearchPos);
        CurString[SearchPos] := Event.CharCode;
      end;
      K := GetKey(CurString);
      T := PSortedCollection(List)^.Search(K, Value);
      if Value < Range then
      begin
        if Value < Range then NewString := GetText(Value, 255)
        else NewString := '';
        if Equal(NewString, CurString, SearchPos) then
        begin
          if Value <> OldValue then
          begin
            FocusItem(Value);
            { Assumes ListControl will set the cursor to the first character }
            { of the sfFocused item }
            SetCursor(Cursor.X+SearchPos, Cursor.Y);
          end
          else SetCursor(Cursor.X+(SearchPos-OldPos), Cursor.Y);
        end
        else SearchPos := OldPos;
      end
      else SearchPos := OldPos;
      if (SearchPos <> OldPos) or (Event.CharCode in ['A'..'Z','a'..'z']) then
        ClearEvent(Event);
    end;
  end;
end;

function TSortedListBox.GetKey(var S: String): Pointer;
begin
  GetKey := @S;
end;

procedure TSortedListBox.NewList(AList: PCollection);
begin
  TListBox.NewList(AList);
  SearchPos := 0;
end;

{ TFileList }

constructor TFileList.Init(var Bounds: TRect; AScrollBar: PScrollBar);
begin
  TSortedListBox.Init(Bounds, 2, AScrollBar);
end;

destructor TFileList.Done;
begin
  if List <> nil then Dispose(List, Done);
  TListBox.Done;
end;

function TFileList.DataSize: Word;
begin
  DataSize := 0;
end;

procedure TFileList.FocusItem(Item: Integer);
begin
  TSortedListBox.FocusItem(Item);
  Message(Owner, evBroadcast, cmFileFocused, List^.At(Item));
end;

procedure TFileList.GetData(var Rec);
begin
end;

function TFileList.GetKey(var S: String): Pointer;
const
  SR: TSearchRec = ();

procedure UpStr(var S: String);
var
  I: Integer;
begin
  for I := 1 to Length(S) do S[I] := UpCase(S[I]);
end;

begin
  if (ShiftState and $03 <> 0) or ((S <> '') and (S[1]='.')) then
    SR.Attr := Directory
  else SR.Attr := 0;
  SR.Name := S;
  UpStr(SR.Name);
  GetKey := @SR;
end;

function TFileList.GetText(Item: Integer; MaxLen: Integer): String;
var
  S: String;
  SR: PSearchRec;
begin
  SR := PSearchRec(List^.At(Item));
  S := SR^.Name;
  if SR^.Attr and Directory <> 0 then
  begin
    S[Length(S)+1] := SysPathSep;
    Inc(S[0]);
  end;
  GetText := S;
end;

procedure TFileList.HandleEvent(var Event: TEvent);
begin
  if (Event.What = evMouseDown) and (Event.Double) then
  begin
    Event.What := evCommand;
    Event.Command := cmOK;
    PutEvent(Event);
    ClearEvent(Event);
  end
  else TSortedListBox.HandleEvent(Event);
end;

procedure TFileList.ReadDirectory(AWildCard: PathStr);
const
  FindAttr = ReadOnly + Archive;
  PrevDir  = '..';
var
  S: SearchRec;
  P: PSearchRec;
  FileList: PFileCollection;
  NumFiles: Word;
  CurPath: PathStr;
  Dir: DirStr;
  Name: NameStr;
  Ext: ExtStr;
  Event: TEvent;
  Tmp: PathStr;
  Flag: Integer;
  Wildcard: PathStr;
  FirstDir: PathStr;
begin
  NumFiles := 0;
  // AWildcard is now a PathDelim-delimited list of wildcards
  FirstDir := '';
  FileList := New(PFileCollection, Init(5, 5));
  while ExtractFirst(AWildCard, WildCard, PathSeparator) do
    begin
      // All of the files must be in the same dir
      WildCard := FExpand(WildCard);
      FSplit(WildCard, Dir, Name, Ext);
      if FirstDir = '' then
        FirstDir := Dir
      else
        Dir := FirstDir;
      FindFirst(Dir+Name+Ext, FindAttr, S);
      P := @P;
      while (P <> nil) and (DosError = 0) do
      begin
        if (S.Attr and Directory = 0) then
        begin
          P := MemAlloc(SizeOf(P^));
          if P <> nil then
          begin
            Move(S.Attr, P^, SizeOf(P^));
            FileList^.Insert(P);
          end;
        end;
        FindNext(S);
      end;
      FindClose(S);
    end;
  Tmp := FirstDir + AllFilesMask;
  FindFirst(Tmp, Directory shl 8 or AnyFile, S);
  while (P <> nil) and (DosError = 0) do
  begin
    if (S.Attr and Directory <> 0) and (S.Name <> '.') and (S.Name <> '..') then
    begin
      P := MemAlloc(SizeOf(P^));
      if P <> nil then
      begin
        Move(S.Attr, P^, SizeOf(P^));
        FileList^.Insert(PObject(P));
      end;
    end;
    FindNext(S);
  end;
  FindClose(S);
  if Length(FirstDir) > 4 then
  begin
    P := MemAlloc(SizeOf(P^));
    if P <> nil then
    begin
      FindFirst(Tmp, Directory shl 8 or AnyFile, S);
      FindNext(S);
      if (DosError = 0) and (S.Name = PrevDir) then
        Move(S.Attr, P^, SizeOf(P^))
      else
      begin
        P^.Name := PrevDir;
        P^.Size := 0;
        P^.Time := $210000;
        P^.Attr := Directory;
      end;
      FindClose(S);
      FileList^.Insert(PObject(P));
    end;
  end;
  if P = nil then MessageBox('Too many files.', nil, mfOkButton + mfWarning);
  NewList(FileList);
  if List^.Count > 0 then
  begin
    Event.What := evBroadcast;
    Event.Command := cmFileFocused;
    Event.InfoPtr := List^.At(0);
    Owner^.HandleEvent(Event);
  end;
end;

procedure TFileList.SetData(var Rec);
begin
  with PFileDialog(Owner)^ do
    Self.ReadDirectory(Directory^ + WildCard);
end;

{ TFileInfoPane }

constructor TFileInfoPane.Init(var Bounds: TRect);
begin
  TView.Init(Bounds);
  EventMask := EventMask or evBroadcast;
end;

procedure TFileInfoPane.Draw;
var
  B: TDrawBuffer;
  D: String[9];
  M: String[3];
  PM: Boolean;
  Color: Word;
  Time: DateTime;
  Path: PathStr;
  FmtId: String;
  Params: array[0..7] of LongInt;
  Str: String[80];
const
  sDirectoryLine = ' %-12s %-9s %3s %2d, %4d  %2d:%02d%cm';
  sFileLine      = ' %-12s %-9d %3s %2d, %4d  %2d:%02d%cm';
  Month: array[1..12] of String[3] =
    ('Jan','Feb','Mar','Apr','May','Jun',
     'Jul','Aug','Sep','Oct','Nov','Dec');
begin
  { Display path }
  Path := FExpand(PFileDialog(Owner)^.Directory^+PFileDialog(Owner)^.WildCard);
  Color := GetColor($01);
  MoveChar(B, ' ', Color, Size.X);
  MoveStr(B[1], Path, Color);
  WriteLine(0, 0, Size.X, 1, B);

  { Display file }
  Params[0] := LongInt(@S.Name);
  MoveChar(B, ' ', Color, Size.X);
  Params[0] := LongInt(@S.Name);
  if S.Attr and Directory <> 0 then
  begin
    FmtId := sDirectoryLine;
    D := 'Directory';
    Params[1] := LongInt(@D);
  end else
  begin
    FmtId := sFileLine;
    Params[1] := S.Size;
  end;
  UnpackTime(S.Time, Time);
  M := Month[Time.Month];
  Params[2] := LongInt(@M);
  Params[3] := Time.Day;
  Params[4] := Time.Year;
  PM := Time.Hour >= 12;
  Time.Hour := Time.Hour mod 12;
  if Time.Hour = 0 then Time.Hour := 12;
  Params[5] := Time.Hour;
  Params[6] := Time.Min;
  if PM then Params[7] := Byte('p')
  else Params[7] := Byte('a');
  FormatStr(Str, FmtId, Params);
  MoveStr(B, Str, Color);
  WriteLine(0, 1, Size.X, 1, B);

  { Fill in rest of rectangle }
  MoveChar(B, ' ', Color, Size.X);
  WriteLine(0, 2, Size.X, Size.Y-2, B);
end;

function TFileInfoPane.GetPalette: PPalette;
const
  P: String[Length(CInfoPane)] = CInfoPane;
begin
  GetPalette := @P;
end;

procedure TFileInfoPane.HandleEvent(var Event: TEvent);
begin
  TView.HandleEvent(Event);
  if (Event.What = evBroadcast) and (Event.Command = cmFileFocused) then
  begin
    S := PSearchRec(Event.InfoPtr)^;
    DrawView;
  end;
end;

{ TFileDialog }

constructor TFileDialog.Init(AWildCard: TWildStr; const ATitle,
  InputName: String; AOptions: Word; HistoryId: Byte);
var
  Control: PView;
  R: TRect;
  Opt: Word;
begin
  R.Assign(15,1,64,20);
  TDialog.Init(R, ATitle);
  Options := Options or ofCentered;
  WildCard := AWildCard;

  R.Assign(3,3,31,4);
  FileName := New(PFileInputLine, Init(R, 79));
  FileName^.Data^ := WildCard;
  Insert(FileName);
  R.Assign(2,2,3+CStrLen(InputName),3);
  Control := New(PLabel, Init(R, InputName, FileName));
  Insert(Control);
  R.Assign(31,3,34,4);
  Control := New(PHistory, Init(R, FileName, HistoryId));
  Insert(Control);

  R.Assign(3,14,34,15);
  Control := New(PScrollBar, Init(R));
  Insert(Control);
  R.Assign(3,6,34,14);
  FileList := New(PFileList, Init(R, PScrollBar(Control)));
  Insert(FileList);
  R.Assign(2,5,8,6);
  Control := New(PLabel, Init(R, '~F~iles', FileList));
  Insert(Control);

  R.Assign(35,3,46,5);
  Opt := bfDefault;
  if AOptions and fdOpenButton <> 0 then
  begin
    Insert(New(PButton, Init(R, '~O~pen', cmFileOpen, Opt)));
    Opt := bfNormal;
    Inc(R.A.Y,3); Inc(R.B.Y,3);
  end;
  if AOptions and fdOkButton <> 0 then
  begin
    Insert(New(PButton, Init(R, 'O~K~', cmFileOpen, Opt)));
    Opt := bfNormal;
    Inc(R.A.Y,3); Inc(R.B.Y,3);
  end;
  if AOptions and fdReplaceButton <> 0 then
  begin
    Insert(New(PButton, Init(R, '~R~eplace',cmFileReplace, Opt)));
    Opt := bfNormal;
    Inc(R.A.Y,3); Inc(R.B.Y,3);
  end;
  if AOptions and fdClearButton <> 0 then
  begin
    Insert(New(PButton, Init(R, '~C~lear',cmFileClear, Opt)));
    Opt := bfNormal;
    Inc(R.A.Y,3); Inc(R.B.Y,3);
  end;
  Insert(New(PButton, Init(R, 'Cancel', cmCancel, bfNormal)));
  Inc(R.A.Y,3); Inc(R.B.Y,3);
  if AOptions and fdHelpButton <> 0 then
  begin
    Insert(New(PButton, Init(R, 'Help',cmHelp, bfNormal)));
    Inc(R.A.Y,3); Inc(R.B.Y,3);
  end;

  R.Assign(1,16,48,18);
  Control := New(PFileInfoPane, Init(R));
  Insert(Control);

  SelectNext(False);

  if AOptions and fdNoLoadDir = 0 then ReadDirectory;
end;

constructor TFileDialog.Load(var S: TStream);
var
  ACurDir: DirStr;
  ViewId: Word;
begin
  TDialog.Load(S);
  S.Read(WildCard, SizeOf(TWildStr));
  GetSubViewPtr(S, FileName);
  GetSubViewPtr(S, FileList);

  ReadDirectory;
end;

destructor TFileDialog.Done;
begin
  DisposeStr(Directory);
  TDialog.Done;
end;

procedure TFileDialog.GetData(var Rec);
begin
  GetFilename(PathStr(Rec));
end;

procedure TFileDialog.GetFileName(var S: PathStr);
var
  Path: PathStr;
  Name: NameStr;
  Ext: ExtStr;
  TPath: PathStr;
  TName: NameStr;
  TExt: NameStr;

  function LTrim(const S: String): String;
  var
    I: Integer;
  begin
    I := 1;
    while (I < Length(S)) and (S[I] = ' ') do Inc(I);
    LTrim := Copy(S, I, 255);
  end;

  function RTrim(const S: String): String;
  var
    I: Integer;
  begin
    I := Length(S);
    while S[I] = ' ' do Dec(I);
    RTrim := Copy(S, 1, I);
  end;

  function RelativePath(var S: PathStr): Boolean;
  begin
    S := LTrim(RTrim(S));
    {$IfDef Linux}
    if FileSystem = fsUnix then
      RelativePath := (S='') or (S[1] <> '/')
    else
    {$EndIf}
    RelativePath := not ((S <> '') and ((S[1] = SysPathSep) or (S[2] = ':')));
  end;

  function NoWildChars(S: String): String; assembler; {$USES esi,edi} {$FRAME-}
  asm
                  mov     esi,S
                  xor     eax,eax
                  cld
                  lodsb
                  mov     ecx,eax
                  mov     edx,@Result
                  lea     edi,[edx+1]
                  jecxz   @@3
                @@1:
                  lodsb
                  cmp     al,'?'
                  je      @@2
                  cmp     al,'*'
                  je      @@2
                  stosb
                @@2:
                  loop    @@1
                @@3:
                  mov     eax,edi
                  sub     eax,edx
                  dec     eax
                  mov     [edx],al
  end;

  function DoGetFileName: String;
  begin
    if ((Name = '') and (Ext = '')) then
      DoGetFileName := Path + TName + TExt
    else if Name = '' then
      DoGetFileName := Path + TName + Ext
    else if Ext = '' then
    begin
      if IsWild(Name) then
        DoGetFileName := Path + Name + TExt
      else
        DoGetFileName := Path + Name + NoWildChars(TExt);
    end;
  end;


var
  FullWC: String;
  WC: String;
  FirstName: String;
  Dummy: String;

begin
  S := FileName^.Data^;
  if RelativePath(S) then S := FExpand(Directory^ + S)
  else S := FExpand(S);
  FSplit(S, Path, Name, Ext);
  // DPMI32 requires Name to be non-empty
  // Linux, OS/2 and Windows allow '.bashrc' - on filsystems that allow that.
  if (((Name = '') and (WildCard = '*.*')) or (Ext = '')) and not IsDir(S) then
  begin
    FSplit(WildCard, TPath, TName, TExt);
    if Pos(PathSeparator, TName) > 0 then // Multiple wildcards
      begin
        FullWC := TName + TExt;
        FirstName := '';
        while ExtractFirst(FullWC, WC, PathSeparator) do
          begin
            FSplit(WC, Dummy, TName, TExt);
            S := DoGetFileName;
            if FirstName = '' then
              FirstName := S;
            S[Length(S)+1] := #0;
            if SysFileExists(PChar(@S[1])) then
              break;
          end;
        S := FirstName; // None of them existed: use the first one
      end
    else
      S := DoGetFileName;
  end;
end;

procedure TFileDialog.HandleEvent(var Event: TEvent);
begin
  TDialog.HandleEvent(Event);
  if Event.What = evCommand then
    case Event.Command of
      cmFileOpen, cmFileReplace, cmFileClear:
        begin
          EndModal(Event.Command);
          ClearEvent(Event);
        end;
    end;
end;

procedure TFileDialog.SetData(var Rec);
begin
  TDialog.SetData(Rec);
  if (PathStr(Rec) <> '') and (IsWild(TWildStr(Rec))) then
  begin
    Valid(cmFileInit);
    FileName^.Select;
  end;
end;

procedure TFileDialog.ReadDirectory;
begin
  FileList^.ReadDirectory(WildCard);
  Directory := NewStr(GetCurDir);
end;

procedure TFileDialog.Store(var S: TStream);
begin
  TDialog.Store(S);
  S.Write(WildCard, SizeOf(TWildStr));
  PutSubViewPtr(S, FileName);
  PutSubViewPtr(S, FileList);
end;

function TFileDialog.Valid(Command: Word): Boolean;
var
  T: Boolean;
  FName: PathStr;
  Dir: DirStr;
  Name: NameStr;
  Ext: ExtStr;

function CheckDirectory(var S: PathStr): Boolean;
begin
  if not PathValid(S) then
  begin
    MessageBox('Invalid drive or directory.', nil, mfError + mfOkButton);
    FileName^.Select;
    CheckDirectory := False;
  end else CheckDirectory := True;
end;

begin
  if Command = 0 then
  begin
    Valid := True;
    Exit;
  end else Valid := False;
  if TDialog.Valid(Command) then
  begin
    GetFileName(FName);
    if (Command <> cmCancel) and (Command <> cmFileClear) then
    begin
      if IsWild(FName) then
      begin
        FSplit(FName, Dir, Name, Ext);
        if CheckDirectory(Dir) then
        begin
          DisposeStr(Directory);
          Directory := NewStr(Dir);
          WildCard := Name+Ext;
          if Command <> cmFileInit then FileList^.Select;
          FileList^.ReadDirectory(Directory^+WildCard);
        end
      end
      else if IsDir(FName) then
      begin
        if CheckDirectory(FName) then
        begin
          DisposeStr(Directory);
          if Copy(FName, 2, Length(FName) - 1) = ':\' then
            Directory := NewStr(FName)
          else
            Directory := NewStr(FName+SysPathSep);
          if Command <> cmFileInit then FileList^.Select;
          FileList^.ReadDirectory(Directory^+WildCard);
        end
      end else if ValidFileName(FName) then Valid := True
      else
      begin
        MessageBox('Invalid file name.', nil, mfError + mfOkButton);
        Valid := False;
      end
    end
    else Valid := True;
  end;
end;

{ TDirCollection }

function TDirCollection.GetItem(var S: TStream): Pointer;
var
  DirItem: PDirEntry;
begin
  New(DirItem);
  DirItem^.DisplayText := S.ReadStr;
  DirItem^.Directory := S.ReadStr;
  GetItem := DirItem;
end;

procedure TDirCollection.FreeItem(Item: Pointer);
var
  DirItem: PDirEntry absolute Item;
begin
  DisposeStr(DirItem^.DisplayText);
  DisposeStr(DirItem^.Directory);
  Dispose(DirItem);
end;

procedure TDirCollection.PutItem(var S: TStream; Item: Pointer);
var
  DirItem: PDirEntry absolute Item;
begin
  S.WriteStr(DirItem^.DisplayText);
  S.WriteStr(DirItem^.Directory);
end;

{ TDirListBox }

const
  DrivesS: String[6] = 'Drives';
  DrivesStr: PString = @DrivesS;

constructor TDirListBox.Init(var Bounds: TRect; AScrollBar:
  PScrollBar);
begin
  TListBox.Init(Bounds, 1, AScrollBar);
  Dir := '';
end;

destructor TDirListBox.Done;
begin
  if List <> nil then Dispose(List, Done);
  TListBox.Done;
end;

function TDirListBox.GetText(Item: Integer; MaxLen: Integer): String;
begin
  GetText := PDirEntry(List^.At(Item))^.DisplayText^;
end;

procedure TDirListBox.HandleEvent(var Event: TEvent);
begin
  if (Event.What = evMouseDown) and (Event.Double) then
  begin
    Event.What := evCommand;
    Event.Command := cmChangeDir;
    PutEvent(Event);
    ClearEvent(Event);
  end
  else TListBox.HandleEvent(Event);
end;

function TDirListBox.IsSelected(Item: Integer): Boolean;
begin
  IsSelected := Item = Cur;
end;

procedure TDirListBox.NewDirectory(var ADir: DirStr);
var
  AList: PCollection;
  NewDir, Dirct: DirStr;
  C, OldC: Char;
  S, Indent: String[80];
  P: PString;
  isFirst: Boolean;
  SR: SearchRec;
  I: Integer;
  DirEntry: PDirEntry;

  function NewDirEntry(const DisplayText, Directory: String): PDirEntry;
  var
    DirEntry: PDirEntry;
  begin
    New(DirEntry);
    DirEntry^.DisplayText := NewStr(DisplayText);
    DirEntry^.Directory := NewStr(Directory);
    NewDirEntry := DirEntry;
  end;

  function GetCurDrive: Char;
  var
    Path: array[0..259] of Char;
  begin
    SysDirGetCurrent(0, Path);
    GetCurDrive := Path[0];
  end;

begin
  Dir := ADir;
  AList := New(PDirCollection, Init(5,5));
  if SysGetValidDrives <> 0 then
    AList^.Insert(NewDirEntry(DrivesStr^, DrivesStr^));
  if (Dir = DrivesStr^) {$IfDef Linux} and (FileSystem<>fsUnix){$EndIf} then
  begin
    isFirst := True;
    OldC := ' ';
    for C := 'A' to 'Z' do
    begin
      if (C < 'C') or DriveValid(C) then
      begin
        if OldC <> ' ' then
        begin
          if isFirst then
          begin
            S := ldFirstDir + OldC;
            isFirst := False;
          end
          else S := ldMiddleDir + OldC;
          AList^.Insert(NewDirEntry(S, OldC + ':\'));
        end;
        if C = GetCurDrive then Cur := AList^.Count;
        OldC := C;
      end;
    end;
    if OldC <> ' ' then
      AList^.Insert(NewDirEntry(ldLastDir + OldC, OldC + ':\'));
  end
  else
  begin
    Indent := ldIndentSize;
    NewDir := Dir;
    if NewDir[2] = ':' then
      begin
        Dirct := Copy(NewDir,1,3);
        NewDir := Copy(NewDir,4, 255)  // Windows: remove C:\
      end
    else
      begin
        Dirct := NewDir[1];
        NewDir := Copy(NewDir,2, 255); // Unix: Remove /
      end;
    AList^.Insert(NewDirEntry(ldPathDir + Dirct, Dirct));
    while NewDir <> '' do
    begin
      I := Pos(SysPathSep, NewDir);
      if I <> 0 then
      begin
        S := Copy(NewDir,1,I-1);
        Dirct := Dirct + S;
        AList^.Insert(NewDirEntry(Indent + ldPathDir + S, Dirct));
        NewDir := Copy(NewDir,I+1,255);
      end
      else
      begin
        Dirct := Dirct + NewDir;
        AList^.Insert(NewDirEntry(Indent + ldPathDir + NewDir, Dirct));
        NewDir := '';
      end;
      Indent := Indent + ldIndentSize;
      Dirct := Dirct + SysPathSep;
    end;
    Cur := AList^.Count-1;
    isFirst := True;
    NewDir := Dirct + AllFilesMask;
    FindFirst(NewDir, Directory shl 8 or AnyFile, SR);
    while DosError = 0 do
    begin
      if (SR.Name <> '.') and (SR.Name <> '..') then
      begin
        if isFirst then
        begin
          S := ldFirstDir;
          isFirst := False;
        end else S := ldMiddleDir;
        AList^.Insert(NewDirEntry(Indent + S + SR.Name, Dirct + SR.Name));
      end;
      FindNext(SR);
    end;
    FindClose(SR);
    P := PDirEntry(AList^.At(AList^.Count-1))^.DisplayText;
    I := Pos(ldPathDir[0],P^);
    if I = 0 then
    begin
      I := Pos(ldMiddleDir[1],P^);
      if I <> 0 then P^[I] := ldPathDir[0];
    end else
    begin
      P^[I+1] := ldPathDir[1];
      P^[I+2] := ldPathDir[1];
    end;
  end;
  NewList(AList);
  FocusItem(Cur);
end;

procedure TDirListBox.SetState(AState: Word; Enable: Boolean);
begin
  TListBox.SetState(AState, Enable);
  if AState and sfFocused <> 0 then
    PChDirDialog(Owner)^.ChDirButton^.MakeDefault(Enable);
end;

{ TChDirDialog }

constructor TChDirDialog.Init(AOptions: Word; HistoryId: Word);
var
  R: TRect;
  Control: PView;
  CurDir: DirStr;
begin
  R.Assign(16, 2, 64, 20);
  TDialog.Init(R, 'Change Directory');

  Options := Options or ofCentered;

  R.Assign(3, 3, 30, 4);
  DirInput := New(PInputLine, Init(R, 68));
  Insert(DirInput);
  R.Assign(2, 2, 17, 3);
  Control := New(PLabel, Init(R, 'Directory ~n~ame', DirInput));
  Insert(Control);
  R.Assign(30, 3, 33, 4);
  Control := New(PHistory, Init(R, DirInput, HistoryId));
  Insert(Control);

  R.Assign(32, 6, 33, 16);
  Control := New(PScrollBar, Init(R));
  Insert(Control);
  R.Assign(3, 6, 32, 16);
  DirList := New(PDirListBox, Init(R, PScrollBar(Control)));
  Insert(DirList);
  R.Assign(2, 5, 17, 6);
  Control := New(PLabel, Init(R, 'Directory ~t~ree', DirList));
  Insert(Control);

  R.Assign(35, 6, 45, 8);
  OkButton := New(PButton, Init(R, 'O~K~', cmOK, bfDefault));
  Insert(OkButton);
  Inc(R.A.Y,3); Inc(R.B.Y,3);
  ChDirButton := New(PButton, Init(R, '~C~hdir', cmChangeDir, bfNormal));
  Insert(ChDirButton);
  Inc(R.A.Y,3); Inc(R.B.Y,3);
  Insert(New(PButton, Init(R, '~R~evert', cmRevert, bfNormal)));
  if AOptions and cdHelpButton <> 0 then
  begin
    Inc(R.A.Y,3); Inc(R.B.Y,3);
    Insert(New(PButton, Init(R, 'Help', cmHelp, bfNormal)));
  end;

  if AOptions and cdNoLoadDir = 0 then SetUpDialog;

  SelectNext(False);
end;

constructor TChDirDialog.Load(var S: TStream);
var
  CurDir: DirStr;
begin
  TDialog.Load(S);
  GetSubViewPtr(S, DirList);
  GetSubViewPtr(S, DirInput);
  GetSubViewPtr(S, OkButton);
  GetSubViewPtr(S, ChDirbutton);
  SetUpDialog;
end;

function TChDirDialog.DataSize: Word;
begin
  DataSize := 0;
end;

procedure TChDirDialog.GetData(var Rec);
begin
end;

procedure TChDirDialog.HandleEvent(var Event: TEvent);
var
  CurDir: DirStr;
  P: PDirEntry;
begin
  TDialog.HandleEvent(Event);
  case Event.What of
    evCommand:
      begin
        case Event.Command of
          cmRevert: GetDir(0,CurDir);
          cmChangeDir:
            begin
              P := DirList^.List^.At(DirList^.Focused);
              if (P^.Directory^ = DrivesStr^) or
                {$IFDEF LINUX} PathValid(P^.Directory^)
                {$ELSE}DriveValid(P^.Directory^[1]) {$ENDIF} then
                CurDir := P^.Directory^
              else Exit;
            end;
        else
          Exit;
        end;
        if (Length(CurDir) > 3) and (CurDir[Length(CurDir)] = SysPathSep) then
          CurDir := Copy(CurDir,1,Length(CurDir)-1);
        DirList^.NewDirectory(CurDir);
        DirInput^.Data^ := CurDir;
        DirInput^.DrawView;
        DirList^.Select;
        ClearEvent(Event);
      end;
  end;
end;

procedure TChDirDialog.SetData(var Rec);
begin
end;

procedure TChDirDialog.SetUpDialog;
var
  CurDir: DirStr;
begin
  if DirList <> nil then
  begin
    CurDir := GetCurDir;
    DirList^.NewDirectory(CurDir);
    if (Length(CurDir) > 3) and (CurDir[Length(CurDir)] = SysPathSep) then
      CurDir := Copy(CurDir,1,Length(CurDir)-1);
    if DirInput <> nil then
    begin
      DirInput^.Data^ := CurDir;
      DirInput^.DrawView;
    end;
  end;
end;

procedure TChDirDialog.Store(var S: TStream);
begin
  TDialog.Store(S);
  PutSubViewPtr(S, DirList);
  PutSubViewPtr(S, DirInput);
  PutSubViewPtr(S, OkButton);
  PutSubViewPtr(S, ChDirButton);
end;

function TChDirDialog.Valid(Command: Word): Boolean;
var
  P: PathStr;
begin
  Valid := True;
  if Command = cmOk then
  begin
    P := FExpand(DirInput^.Data^);
    if (Length(P) > 3) and (P[Length(P)] = SysPathSep) then Dec(P[0]);
    {$I-}
    ChDir(P);
    if IOResult <> 0 then
    begin
      MessageBox('Invalid directory.', nil, mfError + mfOkButton);
      Valid := False;
    end;
    {$I+}
  end;
end;

procedure RegisterStdDlg;
begin
  RegisterType(RFileInputLine);
  RegisterType(RFileCollection);
  RegisterType(RFileList);
  RegisterType(RFileInfoPane);
  RegisterType(RFileDialog);
  RegisterType(RDirCollection);
  RegisterType(RDirListBox);
  RegisterType(RSortedListBox);
  RegisterType(RChDirDialog);
end;

end.
