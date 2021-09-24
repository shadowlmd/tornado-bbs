
{*******************************************************}
{                                                       }
{       Turbo Pascal Version 7.0                        }
{       Turbo Vision Unit                               }
{                                                       }
{       Copyright (c) 1992 Borland International        }
{                                                       }
{*******************************************************}

unit Dialogs;

{$O+,F+,X+,I-,S-}

interface

uses Objects, Drivers, Views, Validate;

const

{ Color palettes }

  CGrayDialog    = #32#33#34#35#36#37#38#39#40#41#42#43#44#45#46#47 +
                   #48#49#50#51#52#53#54#55#56#57#58#59#60#61#62#63;
  CBlueDialog    = #64#65#66#67#68#69#70#71#72#73#74#75#76#77#78#79 +
                   #80#81#82#83#84#85#86#87#88#89#90#91#92#92#94#95;
  CCyanDialog    = #96#97#98#99#100#101#102#103#104#105#106#107#108 +
                   #109#110#111#112#113#114#115#116#117#118#119#120 +
                   #121#122#123#124#125#126#127;

  CDialog        = CGrayDialog;

  CStaticText    = #6;
  CLabel         = #7#8#9#9;
  CButton        = #10#11#12#13#14#14#14#15;
  CCluster       = #16#17#18#18#31;
  CInputLine     = #19#19#20#21;
  CHistory       = #22#23;
  CHistoryWindow = #19#19#21#24#25#19#20;
  CHistoryViewer = #6#6#7#6#6;

{ TDialog palette entires }

  dpBlueDialog = 0;
  dpCyanDialog = 1;
  dpGrayDialog = 2;

{ TButton flags }

  bfNormal    = $00;
  bfDefault   = $01;
  bfLeftJust  = $02;
  bfBroadcast = $04;
  bfGrabFocus = $08;

{ TMultiCheckboxes flags }
{ hibyte = number of bits }
{ lobyte = bit mask }

  cfOneBit       = $0101;
  cfTwoBits      = $0203;
  cfFourBits     = $040F;
  cfEightBits    = $08FF;

type

{ TDialog object }

  { Palette layout }
  {  1 = Frame passive }
  {  2 = Frame active }
  {  3 = Frame icon }
  {  4 = ScrollBar page area }
  {  5 = ScrollBar controls }
  {  6 = StaticText }
  {  7 = Label normal }
  {  8 = Label selected }
  {  9 = Label shortcut }
  { 10 = Button normal }
  { 11 = Button default }
  { 12 = Button selected }
  { 13 = Button disabled }
  { 14 = Button shortcut }
  { 15 = Button shadow }
  { 16 = Cluster normal }
  { 17 = Cluster selected }
  { 18 = Cluster shortcut }
  { 19 = InputLine normal text }
  { 20 = InputLine selected text }
  { 21 = InputLine arrows }
  { 22 = History arrow }
  { 23 = History sides }
  { 24 = HistoryWindow scrollbar page area }
  { 25 = HistoryWindow scrollbar controls }
  { 26 = ListViewer normal }
  { 27 = ListViewer focused }
  { 28 = ListViewer selected }
  { 29 = ListViewer divider }
  { 30 = InfoPane }
  { 31 = Cluster disabled }
  { 32 = Reserved }

  PDialog = ^TDialog;
  TDialog = object(TWindow)
    constructor Init(var Bounds: TRect; ATitle: TTitleStr);
    constructor Load(var S: TStream);
    function GetPalette: PPalette; virtual;
    procedure HandleEvent(var Event: TEvent); virtual;
    function Valid(Command: Word): Boolean; virtual;
  end;

{ TSItem }

  PSItem = ^TSItem;
  TSItem = record
    Value: PString;
    Next: PSItem;
  end;

{ TInputLine object }

  { Palette layout }
  { 1 = Passive }
  { 2 = Active }
  { 3 = Selected }
  { 4 = Arrows }

  PInputLine = ^TInputLine;
  TInputLine = object(TView)
    Data: PString;
    MaxLen: Integer;
    CurPos: Integer;
    FirstPos: Integer;
    SelStart: Integer;
    SelEnd: Integer;
    Validator: PValidator;
    constructor Init(var Bounds: TRect; AMaxLen: Integer);
    constructor Load(var S: TStream);
    destructor Done; virtual;
    function DataSize: Word; virtual;
    procedure Draw; virtual;
    procedure GetData(var Rec); virtual;
    function GetPalette: PPalette; virtual;
    procedure HandleEvent(var Event: TEvent); virtual;
    procedure SelectAll(Enable: Boolean);
    procedure SetData(var Rec); virtual;
    procedure SetState(AState: Word; Enable: Boolean); virtual;
    procedure SetValidator(AValid: PValidator);
    procedure Store(var S: TStream);
    function Valid(Command: Word): Boolean; virtual;
  private
    function CanScroll(Delta: Integer): Boolean;
  end;

{ TButton object }

  { Palette layout }
  { 1 = Normal text }
  { 2 = Default text }
  { 3 = Selected text }
  { 4 = Disabled text }
  { 5 = Normal shortcut }
  { 6 = Default shortcut }
  { 7 = Selected shortcut }
  { 8 = Shadow }

  PButton = ^TButton;
  TButton = object(TView)
    Title: PString;
    Command: Word;
    Flags: Byte;
    AmDefault: Boolean;
    constructor Init(var Bounds: TRect; ATitle: TTitleStr; ACommand: Word;
      AFlags: Word);
    constructor Load(var S: TStream);
    destructor Done; virtual;
    procedure Draw; virtual;
    procedure DrawState(Down: Boolean);
    function GetPalette: PPalette; virtual;
    procedure HandleEvent(var Event: TEvent); virtual;
    procedure MakeDefault(Enable: Boolean);
    procedure Press; virtual;
    procedure SetState(AState: Word; Enable: Boolean); virtual;
    procedure Store(var S: TStream);
  end;

{ TCluster }

  { Palette layout }
  { 1 = Normal text }
  { 2 = Selected text }
  { 3 = Normal shortcut }
  { 4 = Selected shortcut }
  { 5 = Disabled text }

  PCluster = ^TCluster;
  TCluster = object(TView)
    Value: LongInt;
    Sel: Integer;
    EnableMask: LongInt;
    Strings: TStringCollection;
    constructor Init(var Bounds: TRect; AStrings: PSItem);
    constructor Load(var S: TStream);
    destructor Done; virtual;
    function ButtonState(Item: Integer): Boolean;
    function DataSize: Word; virtual;
    procedure DrawBox(const Icon: String; Marker: Char);
    procedure DrawMultiBox(const Icon, Marker: String);
    procedure GetData(var Rec); virtual;
    function GetHelpCtx: Word; virtual;
    function GetPalette: PPalette; virtual;
    procedure HandleEvent(var Event: TEvent); virtual;
    function Mark(Item: Integer): Boolean; virtual;
    function MultiMark(Item: Integer): Byte; virtual;
    procedure Press(Item: Integer); virtual;
    procedure MovedTo(Item: Integer); virtual;
    procedure SetButtonState(AMask: Longint; Enable: Boolean);
    procedure SetData(var Rec); virtual;
    procedure SetState(AState: Word; Enable: Boolean); virtual;
    procedure Store(var S: TStream);
  private
    function Column(Item: Integer): Integer;
    function FindSel(P: TPoint): Integer;
    function Row(Item: Integer): Integer;
  end;

{ TRadioButtons }

  { Palette layout }
  { 1 = Normal text }
  { 2 = Selected text }
  { 3 = Normal shortcut }
  { 4 = Selected shortcut }

  PRadioButtons = ^TRadioButtons;
  TRadioButtons = object(TCluster)
    procedure Draw; virtual;
    function Mark(Item: Integer): Boolean; virtual;
    procedure MovedTo(Item: Integer); virtual;
    procedure Press(Item: Integer); virtual;
    procedure SetData(var Rec); virtual;
  end;

{ TCheckBoxes }

  { Palette layout }
  { 1 = Normal text }
  { 2 = Selected text }
  { 3 = Normal shortcut }
  { 4 = Selected shortcut }

  PCheckBoxes = ^TCheckBoxes;
  TCheckBoxes = object(TCluster)
    procedure Draw; virtual;
    function Mark(Item: Integer): Boolean; virtual;
    procedure Press(Item: Integer); virtual;
  end;

{ TMultiCheckBoxes }

  { Palette layout }
  { 1 = Normal text }
  { 2 = Selected text }
  { 3 = Normal shortcut }
  { 4 = Selected shortcut }

  PMultiCheckBoxes = ^TMultiCheckBoxes;
  TMultiCheckBoxes = object(TCluster)
    SelRange: Byte;
    Flags: Word;
    States: PString;
    constructor Init(var Bounds: TRect; AStrings: PSItem;
      ASelRange: Byte; AFlags: Word; const AStates: String);
    constructor Load(var S: TStream);
    destructor Done; virtual;
    function DataSize: Word; virtual;
    procedure Draw; virtual;
    procedure GetData(var Rec); virtual;
    function MultiMark(Item: Integer): Byte; virtual;
    procedure Press(Item: Integer); virtual;
    procedure SetData(var Rec); virtual;
    procedure Store(var S: TStream);
  end;

{ TListBox }

  { Palette layout }
  { 1 = Active }
  { 2 = Inactive }
  { 3 = Focused }
  { 4 = Selected }
  { 5 = Divider }

  PListBox = ^TListBox;
  TListBox = object(TListViewer)
    List: PCollection;
    constructor Init(var Bounds: TRect; ANumCols: Word;
      AScrollBar: PScrollBar);
    constructor Load(var S: TStream);
    function DataSize: Word; virtual;
    procedure GetData(var Rec); virtual;
    function GetText(Item: Integer; MaxLen: Integer): String; virtual;
    procedure NewList(AList: PCollection); virtual;
    procedure SetData(var Rec); virtual;
    procedure Store(var S: TStream);
  end;

{ TStaticText }

  { Palette layout }
  { 1 = Text }

  PStaticText = ^TStaticText;
  TStaticText = object(TView)
    Text: PString;
    constructor Init(var Bounds: TRect; const AText: String);
    constructor Load(var S: TStream);
    destructor Done; virtual;
    procedure Draw; virtual;
    function GetPalette: PPalette; virtual;
    procedure GetText(var S: String); virtual;
    procedure Store(var S: TStream);
  end;

{ TParamText }

  { Palette layout }
  { 1 = Text }

  PParamText = ^TParamText;
  TParamText = object(TStaticText)
    ParamCount: Integer;
    ParamList: Pointer;
    constructor Init(var Bounds: TRect; const AText: String;
      AParamCount: Integer);
    constructor Load(var S: TStream);
    function DataSize: Word; virtual;
    procedure GetText(var S: String); virtual;
    procedure SetData(var Rec); virtual;
    procedure Store(var S: TStream);
  end;

{ TLabel }

  { Palette layout }
  { 1 = Normal text }
  { 2 = Selected text }
  { 3 = Normal shortcut }
  { 4 = Selected shortcut }

  PLabel = ^TLabel;
  TLabel = object(TStaticText)
    Link: PView;
    Light: Boolean;
    constructor Init(var Bounds: TRect; const AText: String; ALink: PView);
    constructor Load(var S: TStream);
    procedure Draw; virtual;
    function GetPalette: PPalette; virtual;
    procedure HandleEvent(var Event: TEvent); virtual;
    procedure Store(var S: TStream);
  end;

{ THistoryViewer }

  { Palette layout }
  { 1 = Active }
  { 2 = Inactive }
  { 3 = Focused }
  { 4 = Selected }
  { 5 = Divider }

  PHistoryViewer = ^THistoryViewer;
  THistoryViewer = object(TListViewer)
    HistoryId: Word;
    constructor Init(var Bounds: TRect; AHScrollBar, AVScrollBar: PScrollBar;
      AHistoryId: Word);
    function GetPalette: PPalette; virtual;
    function GetText(Item: Integer; MaxLen: Integer): String; virtual;
    procedure HandleEvent(var Event: TEvent); virtual;
    function HistoryWidth: Integer;
  end;

{ THistoryWindow }

  { Palette layout }
  { 1 = Frame passive }
  { 2 = Frame active }
  { 3 = Frame icon }
  { 4 = ScrollBar page area }
  { 5 = ScrollBar controls }
  { 6 = HistoryViewer normal text }
  { 7 = HistoryViewer selected text }

  PHistoryWindow = ^THistoryWindow;
  THistoryWindow = object(TWindow)
    Viewer: PListViewer;
    constructor Init(var Bounds: TRect; HistoryId: Word);
    function GetPalette: PPalette; virtual;
    function GetSelection: String; virtual;
    procedure InitViewer(HistoryId: Word); virtual;
  end;

{ THistory }

  { Palette layout }
  { 1 = Arrow }
  { 2 = Sides }

  PHistory = ^THistory;
  THistory = object(TView)
    Link: PInputLine;
    HistoryId: Word;
    constructor Init(var Bounds: TRect; ALink: PInputLine; AHistoryId: Word);
    constructor Load(var S: TStream);
    procedure Draw; virtual;
    function GetPalette: PPalette; virtual;
    procedure HandleEvent(var Event: TEvent); virtual;
    function InitHistoryWindow(var Bounds: TRect): PHistoryWindow; virtual;
    procedure RecordHistory(const S: String); virtual;
    procedure Store(var S: TStream);
  end;

{ SItem routines }

function NewSItem(const Str: String; ANext: PSItem): PSItem;

{ Dialogs registration procedure }

procedure RegisterDialogs;

{ Stream Registration Records }

const
  RDialog: TStreamRec = (
     ObjType: 10;
     VmtLink: Ofs(TypeOf(TDialog)^);
     Load:    @TDialog.Load;
     Store:   @TDialog.Store
  );

const
  RInputLine: TStreamRec = (
     ObjType: 11;
     VmtLink: Ofs(TypeOf(TInputLine)^);
     Load:    @TInputLine.Load;
     Store:   @TInputLine.Store
  );

const
  RButton: TStreamRec = (
     ObjType: 12;
     VmtLink: Ofs(TypeOf(TButton)^);
     Load:    @TButton.Load;
     Store:   @TButton.Store
  );

const
  RCluster: TStreamRec = (
     ObjType: 13;
     VmtLink: Ofs(TypeOf(TCluster)^);
     Load:    @TCluster.Load;
     Store:   @TCluster.Store
  );

const
  RRadioButtons: TStreamRec = (
     ObjType: 14;
     VmtLink: Ofs(TypeOf(TRadioButtons)^);
     Load:    @TRadioButtons.Load;
     Store:   @TRadioButtons.Store
  );

const
  RCheckBoxes: TStreamRec = (
     ObjType: 15;
     VmtLink: Ofs(TypeOf(TCheckBoxes)^);
     Load:    @TCheckBoxes.Load;
     Store:   @TCheckBoxes.Store
  );

const
  RMultiCheckBoxes: TStreamRec = (
     ObjType: 27;
     VmtLink: Ofs(TypeOf(TMultiCheckBoxes)^);
     Load:    @TMultiCheckBoxes.Load;
     Store:   @TMultiCheckBoxes.Store
  );

const
  RListBox: TStreamRec = (
     ObjType: 16;
     VmtLink: Ofs(TypeOf(TListBox)^);
     Load:    @TListBox.Load;
     Store:   @TListBox.Store
  );

const
  RStaticText: TStreamRec = (
     ObjType: 17;
     VmtLink: Ofs(TypeOf(TStaticText)^);
     Load:    @TStaticText.Load;
     Store:   @TStaticText.Store
  );

const
  RLabel: TStreamRec = (
     ObjType: 18;
     VmtLink: Ofs(TypeOf(TLabel)^);
     Load:    @TLabel.Load;
     Store:   @TLabel.Store
  );

const
  RHistory: TStreamRec = (
     ObjType: 19;
     VmtLink: Ofs(TypeOf(THistory)^);
     Load:    @THistory.Load;
     Store:   @THistory.Store
  );

const
  RParamText: TStreamRec = (
     ObjType: 20;
     VmtLink: Ofs(TypeOf(TParamText)^);
     Load:    @TParamText.Load;
     Store:   @TParamText.Store
  );

const

{ Dialog broadcast commands }

  cmRecordHistory = 60;

implementation

uses HistList;

const

{ TButton messages }

  cmGrabDefault    = 61;
  cmReleaseDefault = 62;

{ Utility functions }

function IsBlank(Ch: Char): Boolean;
begin
  IsBlank := (Ch = ' ') or (Ch = #13) or (Ch = #10);
end;

{ TDialog }

constructor TDialog.Init(var Bounds: TRect; ATitle: TTitleStr);
begin
  inherited Init(Bounds, ATitle, wnNoNumber);
  Options := Options or ofVersion20;
  GrowMode := 0;
  Flags := wfMove + wfClose;
  Palette := dpGrayDialog;
end;

constructor TDialog.Load(var S: TStream);
begin
  inherited Load(S);
  if Options and ofVersion = ofVersion10 then
  begin
    Palette := dpGrayDialog;
    Inc(Options, ofVersion20);
  end;
end;

function TDialog.GetPalette: PPalette;
const
  P: array[dpBlueDialog..dpGrayDialog] of string[Length(CBlueDialog)] =
    (CBlueDialog, CCyanDialog, CGrayDialog);
begin
  GetPalette := @P[Palette];
end;

procedure TDialog.HandleEvent(var Event: TEvent);
begin
  TWindow.HandleEvent(Event);
  case Event.What of
    evKeyDown:
      case Event.KeyCode of
        kbEsc:
          begin
            Event.What := evCommand;
            Event.Command := cmCancel;
            Event.InfoPtr := nil;
            PutEvent(Event);
            ClearEvent(Event);
          end;
        kbEnter:
          begin
            Event.What := evBroadcast;
            Event.Command := cmDefault;
            Event.InfoPtr := nil;
            PutEvent(Event);
            ClearEvent(Event);
          end;
      end;
    evCommand:
      case Event.Command of
        cmOk, cmCancel, cmYes, cmNo:
          if State and sfModal <> 0 then
          begin
            EndModal(Event.Command);
            ClearEvent(Event);
          end;
      end;
  end;
end;

function TDialog.Valid(Command: Word): Boolean;
begin
  if Command = cmCancel then Valid := True
  else Valid := TGroup.Valid(Command);
end;

function NewSItem(const Str: String; ANext: PSItem): PSItem;
var
  Item: PSItem;
begin
  New(Item);
  Item^.Value := NewStr(Str);
  Item^.Next := ANext;
  NewSItem := Item;
end;

function Max(A, B: Integer): Integer;
inline(
   $58/     {pop   ax   }
   $5B/     {pop   bx   }
   $3B/$C3/ {cmp   ax,bx}
   $7F/$01/ {jg    @@1  }
   $93);    {xchg  ax,bx}
       {@@1:            }

function HotKey(const S: String): Char;
var
  P: Word;
begin
  P := Pos('~',S);
  if P <> 0 then HotKey := UpCase(S[P+1])
  else HotKey := #0;
end;

{ TInputLine }

constructor TInputLine.Init(var Bounds: TRect; AMaxLen: Integer);
begin
  TView.Init(Bounds);
  State := State or sfCursorVis;
  Options := Options or (ofSelectable + ofFirstClick + ofVersion20);
  GetMem(Data, AMaxLen + 1);
  Data^ := '';
  MaxLen := AMaxLen;
end;

constructor TInputLine.Load(var S: TStream);
begin
  TView.Load(S);
  S.Read(MaxLen, SizeOf(Integer) * 5);
  GetMem(Data, MaxLen + 1);
  S.Read(Data^[0], 1);
  S.Read(Data^[1], Length(Data^));
  if Options and ofVersion >= ofVersion20 then
    Validator := PValidator(S.Get);
  Options := Options or ofVersion20;
end;

destructor TInputLine.Done;
begin
  FreeMem(Data, MaxLen + 1);
  SetValidator(nil);
  TView.Done;
end;

function TInputLine.CanScroll(Delta: Integer): Boolean;
begin
  if Delta < 0 then
    CanScroll := FirstPos > 0 else
  if Delta > 0 then
    CanScroll := Length(Data^) - FirstPos + 2 > Size.X else
    CanScroll := False;
end;

function TInputLine.DataSize: Word;
var
  DSize: Word;
begin
  DSize := 0;

  if Validator <> nil then
    DSize := Validator^.Transfer(Data^, nil, vtDataSize);

  if DSize <> 0 then
    DataSize := DSize
  else
    DataSize := MaxLen + 1;
end;

procedure TInputLine.Draw;
var
  Color: Byte;
  L, R: Integer;
  B: TDrawBuffer;
begin
  if State and sfFocused = 0 then
    Color := GetColor(1) else
    Color := GetColor(2);
  MoveChar(B, ' ', Color, Size.X);
  MoveStr(B[1], Copy(Data^, FirstPos + 1, Size.X - 2), Color);
  if CanScroll(1) then MoveChar(B[Size.X - 1], #16, GetColor(4), 1);
  if State and sfFocused <> 0 then
  begin
    if CanScroll(-1) then MoveChar(B[0], #17, GetColor(4), 1);
    L := SelStart - FirstPos;
    R := SelEnd - FirstPos;
    if L < 0 then L := 0;
    if R > Size.X - 2 then R := Size.X - 2;
    if L < R then MoveChar(B[L + 1], #0, GetColor(3), R - L);
  end;
  WriteLine(0, 0, Size.X, Size.Y, B);
  SetCursor(CurPos - FirstPos + 1, 0);
end;

procedure TInputLine.GetData(var Rec);
begin
  if (Validator = nil) or
    (Validator^.Transfer(Data^, @Rec, vtGetData) = 0) then
  begin
    FillChar(Rec, DataSize, #0);
    Move(Data^, Rec, Length(Data^) + 1);
  end;
end;

function TInputLine.GetPalette: PPalette;
const
  P: String[Length(CInputLine)] = CInputLine;
begin
  GetPalette := @P;
end;

procedure TInputLine.HandleEvent(var Event: TEvent);
const
  PadKeys = [$47, $4B, $4D, $4F, $73, $74];
var
  Delta, Anchor, I: Integer;
  ExtendBlock: Boolean;
  OldData: string;
  OldCurPos, OldFirstPos,
  OldSelStart, OldSelEnd: Integer;
  WasAppending: Boolean;

function MouseDelta: Integer;
var
  Mouse: TPoint;
begin
  MakeLocal(Event.Where, Mouse);
  if Mouse.X <= 0 then MouseDelta := -1 else
  if Mouse.X >= Size.X - 1 then MouseDelta := 1 else
  MouseDelta := 0;
end;

function MousePos: Integer;
var
  Pos: Integer;
  Mouse: TPoint;
begin
  MakeLocal(Event.Where, Mouse);
  if Mouse.X < 1 then Mouse.X := 1;
  Pos := Mouse.X + FirstPos - 1;
  if Pos < 0 then Pos := 0;
  if Pos > Length(Data^) then Pos := Length(Data^);
  MousePos := Pos;
end;

procedure DeleteSelect;
begin
  if SelStart <> SelEnd then
  begin
    Delete(Data^, SelStart + 1, SelEnd - SelStart);
    CurPos := SelStart;
  end;
end;

procedure AdjustSelectBlock;
begin
  if CurPos < Anchor then
  begin
    SelStart := CurPos;
    SelEnd := Anchor;
  end else
  begin
    SelStart := Anchor;
    SelEnd := CurPos;
  end;
end;

procedure SaveState;
begin
  if Validator <> nil then
  begin
    OldData := Data^;
    OldCurPos := CurPos;
    OldFirstPos := FirstPos;
    OldSelStart := SelStart;
    OldSelEnd := SelEnd;
    WasAppending := Length(Data^) = CurPos;
  end;
end;

procedure RestoreState;
begin
  if Validator <> nil then
  begin
    Data^ := OldData;
    CurPos := OldCurPos;
    FirstPos := OldFirstPos;
    SelStart := OldSelStart;
    SelEnd := OldSelEnd;
  end;
end;

function CheckValid(NoAutoFill: Boolean): Boolean;
var
  OldLen: Integer;
  NewData: String;
begin
  if Validator <> nil then
  begin
    CheckValid := False;
    OldLen := Length(Data^);
    if (Validator^.Options and voOnAppend = 0) or
      (WasAppending and (CurPos = OldLen)) then
    begin
      NewData := Data^;
      if not Validator^.IsValidInput(NewData, NoAutoFill) then
        RestoreState
      else
      begin
        if Length(NewData) > MaxLen then NewData[0] := Char(MaxLen);
        Data^ := NewData;
        if (CurPos >= OldLen) and (Length(Data^) > OldLen) then
          CurPos := Length(Data^);
        CheckValid := True;
      end;
    end
    else
    begin
      CheckValid := True;
      if CurPos = OldLen then
        if not Validator^.IsValidInput(Data^, False) then
        begin
          Validator^.Error;
          CheckValid := False;
        end;
    end;
  end
  else
    CheckValid := True;
end;

begin
  TView.HandleEvent(Event);
  if State and sfSelected <> 0 then
  begin
    case Event.What of
      evMouseDown:
        begin
          Delta := MouseDelta;
          if CanScroll(Delta) then
          begin
            repeat
              if CanScroll(Delta) then
              begin
                Inc(FirstPos, Delta);
                DrawView;
              end;
            until not MouseEvent(Event, evMouseAuto);
          end else
          if Event.Double then SelectAll(True) else
          begin
            Anchor := MousePos;
            repeat
              if Event.What = evMouseAuto then
              begin
                Delta := MouseDelta;
                if CanScroll(Delta) then Inc(FirstPos, Delta);
              end;
              CurPos := MousePos;
              AdjustSelectBlock;
              DrawView;
            until not MouseEvent(Event, evMouseMove + evMouseAuto);
          end;
          ClearEvent(Event);
        end;
      evKeyDown:
        begin
          SaveState;
          Event.KeyCode := CtrlToArrow(Event.KeyCode);
          if (Event.ScanCode in PadKeys) and
             (GetShiftState and $03 <> 0) then
          begin
            Event.CharCode := #0;
            if CurPos = SelEnd then Anchor := SelStart
            else Anchor := SelEnd;
            ExtendBlock := True;
          end
          else
            ExtendBlock := False;
          case Event.KeyCode of
            kbLeft:
              if CurPos > 0 then Dec(CurPos);
            kbRight:
              if CurPos < Length(Data^) then
              begin
                Inc(CurPos);
                CheckValid(True);
              end;
            kbHome:
              CurPos := 0;
            kbEnd:
              begin
                CurPos := Length(Data^);
                CheckValid(True);
              end;
            kbBack:
              if CurPos > 0 then
              begin
                Delete(Data^, CurPos, 1);
                Dec(CurPos);
                if FirstPos > 0 then Dec(FirstPos);
                CheckValid(True);
              end;
            kbDel:
              begin
                if SelStart = SelEnd then
                  if CurPos < Length(Data^) then
                  begin
                    SelStart := CurPos;
                    SelEnd := CurPos + 1;
                  end;
                DeleteSelect;
                CheckValid(True);
              end;
            kbIns:
              SetState(sfCursorIns, State and sfCursorIns = 0);
          else
            case Event.CharCode of
              ' '..#255:
                begin
                  if State and sfCursorIns <> 0 then
                    Delete(Data^, CurPos + 1, 1) else DeleteSelect;
                  if CheckValid(True) then
                  begin
                    if Length(Data^) < MaxLen then
                    begin
                      if FirstPos > CurPos then FirstPos := CurPos;
                      Inc(CurPos);
                      Insert(Event.CharCode, Data^, CurPos);
                    end;
                    CheckValid(False);
                  end;
                end;
              ^Y:
                begin
                  Data^ := '';
                  CurPos := 0;
                end;
            else
              Exit;
            end
          end;
          if ExtendBlock then
            AdjustSelectBlock
          else
          begin
            SelStart := CurPos;
            SelEnd := CurPos;
          end;
          if FirstPos > CurPos then FirstPos := CurPos;
          I := CurPos - Size.X + 2;
          if FirstPos < I then FirstPos := I;
          DrawView;
          ClearEvent(Event);
        end;
    end;
  end;
end;

procedure TInputLine.SelectAll(Enable: Boolean);
begin
  CurPos := 0;
  FirstPos := 0;
  SelStart := 0;
  if Enable then SelEnd := Length(Data^) else SelEnd := 0;
  DrawView;
end;

procedure TInputLine.SetData(var Rec);
begin
  if (Validator = nil) or
    (Validator^.Transfer(Data^, @Rec, vtSetData) = 0) then
    Move(Rec, Data^[0], DataSize);

  SelectAll(True);
end;

procedure TInputLine.SetState(AState: Word; Enable: Boolean);
begin
  TView.SetState(AState, Enable);
  if (AState = sfSelected) or ((AState = sfActive) and
     (State and sfSelected <> 0)) then
    SelectAll(Enable)
  else if AState = sfFocused then
    DrawView;
end;

procedure TInputLine.SetValidator(AValid: PValidator);
begin
  if Validator <> nil then Validator^.Free;
  Validator := AValid;
end;

procedure TInputLine.Store(var S: TStream);
begin
  TView.Store(S);
  S.Write(MaxLen, SizeOf(Integer) * 5);
  S.WriteStr(Data);
  S.Put(Validator);
end;

function TInputLine.Valid(Command: Word): Boolean;

  function AppendError(Validator: PValidator): Boolean;
  begin
    AppendError := False;
    with Validator^ do
      if (Options and voOnAppend <> 0) and (CurPos <> Length(Data^))
          and not IsValidInput(Data^, True) then
      begin
        Error;
        AppendError := True;
      end;
  end;

begin
  Valid := inherited Valid(Command);
  if (Validator <> nil) and (State and sfDisabled = 0) then
    if Command = cmValid then
      Valid := Validator^.Status = vsOk
    else if Command <> cmCancel then
      if AppendError(Validator) or not Validator^.Valid(Data^) then
      begin
        Select;
        Valid := False;
      end;
end;

{ TButton }

constructor TButton.Init(var Bounds: TRect; ATitle: TTitleStr;
  ACommand: Word; AFlags: Word);
begin
  TView.Init(Bounds);
  Options := Options or (ofSelectable + ofFirstClick +
    ofPreProcess + ofPostProcess);
  EventMask := EventMask or evBroadcast;
  if not CommandEnabled(ACommand) then State := State or sfDisabled;
  Flags := AFlags;
  if AFlags and bfDefault <> 0 then AmDefault := True
  else AmDefault := False;
  Title := NewStr(ATitle);
  Command := ACommand;
end;

constructor TButton.Load(var S: TStream);
begin
  TView.Load(S);
  Title := S.ReadStr;
  S.Read(Command, SizeOf(Word) + SizeOf(Byte) + SizeOf(Boolean));
  if not CommandEnabled(Command) then State := State or sfDisabled
  else State := State and not sfDisabled;
end;

destructor TButton.Done;
begin
  DisposeStr(Title);
  TView.Done;
end;

procedure TButton.Draw;
begin
  DrawState(False);
end;

procedure TButton.DrawState(Down: Boolean);
var
  CButton, CShadow: Word;
  Ch: Char;
  I, S, Y, T: Integer;
  B: TDrawBuffer;

procedure DrawTitle;
var
  L, SCOff: Integer;
begin
  if Flags and bfLeftJust <> 0 then L := 1 else
  begin
    L := (S - CStrLen(Title^) - 1) div 2;
    if L < 1 then L := 1;
  end;
  MoveCStr(B[I + L], Title^, CButton);
  if ShowMarkers and not Down then
  begin
    if State and sfSelected <> 0 then SCOff := 0 else
      if AmDefault then SCOff := 2 else SCOff := 4;
    WordRec(B[0]).Lo := Byte(SpecialChars[SCOff]);
    WordRec(B[S]).Lo := Byte(SpecialChars[SCOff + 1]);
  end;
end;

begin
  if State and sfDisabled <> 0 then CButton := GetColor($0404) else
  begin
    CButton := GetColor($0501);
    if State and sfActive <> 0 then
      if State and sfSelected <> 0 then CButton := GetColor($0703) else
        if AmDefault then CButton := GetColor($0602);
  end;
  CShadow := GetColor(8);
  S := Size.X - 1;
  T := Size.Y div 2 - 1;
  for Y := 0 to Size.Y - 2 do
  begin
    MoveChar(B, ' ', Byte(CButton), Size.X);
    WordRec(B[0]).Hi := CShadow;
    if Down then
    begin
      WordRec(B[1]).Hi := CShadow;
      Ch := ' ';
      I := 2;
    end else
    begin
      WordRec(B[S]).Hi := Byte(CShadow);
      if ShowMarkers then Ch := ' ' else
      begin
        if Y = 0 then
          WordRec(B[S]).Lo := Byte('Ü') else
          WordRec(B[S]).Lo := Byte('Û');
        Ch := 'ß';
      end;
      I := 1;
    end;
    if (Y = T) and (Title <> nil) then DrawTitle;
    if ShowMarkers and not Down then
    begin
      WordRec(B[1]).Lo := Byte('[');
      WordRec(B[S - 1]).Lo := Byte(']');
    end;
    WriteLine(0, Y, Size.X, 1, B);
  end;
  MoveChar(B[0], ' ', Byte(CShadow), 2);
  MoveChar(B[2], Ch, Byte(CShadow), S - 1);
  WriteLine(0, Size.Y - 1, Size.X, 1, B);
end;

function TButton.GetPalette: PPalette;
const
  P: String[Length(CButton)] = CButton;
begin
  GetPalette := @P;
end;

procedure TButton.HandleEvent(var Event: TEvent);
var
  Down: Boolean;
  C: Char;
  Mouse: TPoint;
  ClickRect: TRect;
begin
  GetExtent(ClickRect);
  Inc(ClickRect.A.X);
  Dec(ClickRect.B.X);
  Dec(ClickRect.B.Y);
  if Event.What = evMouseDown then
  begin
    MakeLocal(Event.Where, Mouse);
    if not ClickRect.Contains(Mouse) then ClearEvent(Event);
  end;
  if Flags and bfGrabFocus <> 0 then
    TView.HandleEvent(Event);
  case Event.What of
    evMouseDown:
      begin
        if State and sfDisabled = 0 then
        begin
          Inc(ClickRect.B.X);
          Down := False;
          repeat
            MakeLocal(Event.Where, Mouse);
            if Down <> ClickRect.Contains(Mouse) then
            begin
              Down := not Down;
              DrawState(Down);
            end;
          until not MouseEvent(Event, evMouseMove);
          if Down then
          begin
            Press;
            DrawState(False);
          end;
        end;
        ClearEvent(Event);
      end;
    evKeyDown:
      begin
        C := HotKey(Title^);
        if (Event.KeyCode = GetAltCode(C)) or
          (Owner^.Phase = phPostProcess) and (C <> #0) and
            (Upcase(Event.CharCode) = C) or
          (State and sfFocused <> 0) and (Event.CharCode = ' ') then
        begin
          Press;
          ClearEvent(Event);
        end;
      end;
    evBroadcast:
      case Event.Command of
        cmDefault:
          if AmDefault then
          begin
            Press;
            ClearEvent(Event);
          end;
        cmGrabDefault, cmReleaseDefault:
          if Flags and bfDefault <> 0 then
          begin
            AmDefault := Event.Command = cmReleaseDefault;
            DrawView;
          end;
        cmCommandSetChanged:
          begin
            SetState(sfDisabled, not CommandEnabled(Command));
            DrawView;
          end;
      end;
  end;
end;

procedure TButton.MakeDefault(Enable: Boolean);
var
  C: Word;
begin
  if Flags and bfDefault = 0 then
  begin
    if Enable then C := cmGrabDefault else C := cmReleaseDefault;
    Message(Owner, evBroadcast, C, @Self);
    AmDefault := Enable;
    DrawView;
  end;
end;

procedure TButton.Press;
var
  E: TEvent;
begin
  Message(Owner, evBroadcast, cmRecordHistory, nil);
  if Flags and bfBroadcast <> 0 then
    Message(Owner, evBroadcast, Command, @Self) else
  begin
    E.What := evCommand;
    E.Command := Command;
    E.InfoPtr := @Self;
    PutEvent(E);
  end;
end;

procedure TButton.SetState(AState: Word; Enable: Boolean);
begin
  TView.SetState(AState, Enable);
  if AState and (sfSelected + sfActive) <> 0 then DrawView;
  if AState and sfFocused <> 0 then MakeDefault(Enable);
end;

procedure TButton.Store(var S: TStream);
begin
  TView.Store(S);
  S.WriteStr(Title);
  S.Write(Command, SizeOf(Word) + SizeOf(Byte) + SizeOf(Boolean));
end;

{ TCluster }

constructor TCluster.Init(var Bounds: TRect; AStrings: PSItem);
var
  I: Integer;
  P: PSItem;
begin
  TView.Init(Bounds);
  Options := Options or (ofSelectable + ofFirstClick + ofPreProcess +
    ofPostProcess + ofVersion20);
  I := 0;
  P := AStrings;
  while P <> nil do
  begin
    Inc(I);
    P := P^.Next;
  end;
  Strings.Init(I,0);
  while AStrings <> nil do
  begin
    P := AStrings;
    Strings.AtInsert(Strings.Count, AStrings^.Value);
    AStrings := AStrings^.Next;
    Dispose(P);
  end;
  Value := 0;
  Sel := 0;
  SetCursor(2,0);
  ShowCursor;
  EnableMask := $FFFFFFFF;
end;

constructor TCluster.Load(var S: TStream);
begin
  TView.Load(S);
  if (Options and ofVersion) >= ofVersion20 then
  begin
    S.Read(Value, SizeOf(Longint) * 2 + SizeOf(Integer));
  end
  else
  begin
    S.Read(Value, SizeOf(Word));
    S.Read(Sel, SizeOf(Integer));
    EnableMask := $FFFFFFFF;
    Options := Options or ofVersion20;
  end;
  Strings.Load(S);
  SetButtonState(0, True);
end;

destructor TCluster.Done;
begin
  Strings.Done;
  TView.Done;
end;

function TCluster.ButtonState(Item: Integer): Boolean; assembler;
asm
        XOR     AL,AL
        MOV     CX,Item
        CMP     CX,31
        JA      @@3
        MOV     AX,1
        XOR     DX,DX
        JCXZ    @@2
@@1:    SHL     AX,1
        RCL     DX,1
        LOOP    @@1
@@2:    LES     DI,Self
        AND     AX,ES:[DI].TCluster.EnableMask.Word[0]
        AND     DX,ES:[DI].TCluster.EnableMask.Word[2]
        OR      AX,DX
        JZ      @@3
        MOV     AL,1
@@3:
end;

function TCluster.DataSize: Word;
begin
  DataSize := SizeOf(Word);
end;

procedure TCluster.DrawBox(const Icon: String; Marker: Char);
begin
  DrawMultiBox(Icon, ' '+Marker);
end;

procedure TCluster.DrawMultiBox(const Icon, Marker: String);
var
  I,J,Cur,Col: Integer;
  CNorm, CSel, CDis, Color: Word;
  B: TDrawBuffer;
  SCOff: Byte;
begin
  CNorm := GetColor($0301);
  CSel := GetColor($0402);
  CDis := GetColor($0505);
  for I := 0 to Size.Y do
  begin
    MoveChar(B, ' ', Byte(CNorm), Size.X);
    for J := 0 to (Strings.Count - 1) div Size.Y + 1 do
    begin
      Cur := J*Size.Y + I;
      if Cur < Strings.Count then
      begin
        Col := Column(Cur);
        if (Col + CStrLen(PString(Strings.At(Cur))^) + 5 <
          Sizeof(TDrawBuffer) div SizeOf(Word)) and (Col < Size.X) then
        begin
          if not ButtonState(Cur) then
            Color := CDis
          else if (Cur = Sel) and (State and sfFocused <> 0) then
            Color := CSel
          else
            Color := CNorm;
          MoveChar(B[Col], ' ', Byte(Color), Size.X - Col);
          MoveStr(B[Col], Icon, Byte(Color));
          WordRec(B[Col+2]).Lo := Byte(Marker[MultiMark(Cur) + 1]);
          MoveCStr(B[Col+5], PString(Strings.At(Cur))^, Color);
          if ShowMarkers and (State and sfFocused <> 0) and (Cur = Sel) then
          begin
            WordRec(B[Col]).Lo := Byte(SpecialChars[0]);
            WordRec(B[Column(Cur+Size.Y)-1]).Lo := Byte(SpecialChars[1]);
          end;
        end;
      end;
    end;
    WriteBuf(0, I, Size.X, 1, B);
  end;
  SetCursor(Column(Sel)+2,Row(Sel));
end;

procedure TCluster.GetData(var Rec);
begin
  Word(Rec) := Value;
end;

function TCluster.GetHelpCtx: Word;
begin
  if HelpCtx = hcNoContext then GetHelpCtx := hcNoContext
  else GetHelpCtx := HelpCtx + Sel;
end;

function TCluster.GetPalette: PPalette;
const
  P: String[Length(CCluster)] = CCluster;
begin
  GetPalette := @P;
end;

procedure TCluster.HandleEvent(var Event: TEvent);
var
  Mouse: TPoint;
  I, S: Integer;
  C: Char;

procedure MoveSel;
begin
  if I <= Strings.Count then
  begin
    Sel := S;
    MovedTo(Sel);
    DrawView;
  end;
end;

begin
  TView.HandleEvent(Event);
  if (Options and ofSelectable) = 0 then Exit;
  if Event.What = evMouseDown then
  begin
    MakeLocal(Event.Where, Mouse);
    I := FindSel(Mouse);
    if I <> -1 then if ButtonState(I) then Sel := I;
    DrawView;
    repeat
      MakeLocal(Event.Where, Mouse);
      if FindSel(Mouse) = Sel then
        ShowCursor else
        HideCursor;
    until not MouseEvent(Event,evMouseMove); {Wait for mouse up}
    ShowCursor;
    MakeLocal(Event.Where, Mouse);
    if (FindSel(Mouse) = Sel) and ButtonState(Sel) then
    begin
      Press(Sel);
      DrawView;
    end;
    ClearEvent(Event);
  end else if Event.What = evKeyDown then
  begin
    S := Sel;
    case CtrlToArrow(Event.KeyCode) of
      kbUp:
        if State and sfFocused <> 0 then
        begin
          I := 0;
          repeat
            Inc(I);
            Dec(S);
            if S < 0 then S := Strings.Count - 1;
          until ButtonState(S) or (I > Strings.Count);
          MoveSel;
          ClearEvent(Event);
        end;
      kbDown:
        if State and sfFocused <> 0 then
        begin
          I := 0;
          repeat
            Inc(I);
            Inc(S);
            if S >= Strings.Count then S := 0;
          until ButtonState(S) or (I > Strings.Count);
          MoveSel;
          ClearEvent(Event);
        end;
      kbRight:
        if State and sfFocused <> 0 then
        begin
          I := 0;
          repeat
            Inc(I);
            Inc(S,Size.Y);
            if S >= Strings.Count then
            begin
              S := (S+1) mod Size.Y;
              if S >= Strings.Count then S := 0;
            end;
          until ButtonState(S) or (I > Strings.Count);
          MoveSel;
          ClearEvent(Event);
        end;
      kbLeft:
        if State and sfFocused <> 0 then
        begin
          I := 0;
          repeat
            Inc(I);
            if S > 0 then
            begin
              Dec(S, Size.Y);
              if S < 0 then
              begin
                S := ((Strings.Count + Size.Y - 1) div Size.Y)*Size.Y + S - 1;
                if S >= Strings.Count then S := Strings.Count-1;
              end;
            end else S := Strings.Count-1;
          until ButtonState(S) or (I > Strings.Count);
          MoveSel;
          ClearEvent(Event);
        end;
    else
      begin
        for I := 0 to Strings.Count-1 do
        begin
          C := HotKey(PString(Strings.At(I))^);
          if (GetAltCode(C) = Event.KeyCode) or
             (((Owner^.Phase = phPostProcess) or (State and sfFocused <> 0))
               and (C <> #0) and (UpCase(Event.CharCode) = C)) then
          begin
            if ButtonState(I) then
            begin
              if Focus then
              begin
                Sel := I;
                MovedTo(Sel);
                Press(Sel);
                DrawView;
              end;
              ClearEvent(Event);
            end;
            Exit;
          end;
        end;
        if (Event.CharCode = ' ') and (State and sfFocused <> 0)
          and ButtonState(Sel)then
        begin
          Press(Sel);
          DrawView;
          ClearEvent(Event);
        end;
      end
    end
  end;
end;

procedure TCluster.SetButtonState(AMask: Longint; Enable: Boolean); assembler;
asm
        LES     DI,Self
        MOV     AX,AMask.Word[0]
        MOV     DX,AMask.Word[2]
        TEST    Enable,0FFH
        JNZ     @@1
        NOT     AX
        NOT     DX
        AND     ES:[DI].TCluster.EnableMask.Word[0],AX
        AND     ES:[DI].TCluster.EnableMask.Word[2],DX
        JMP     @@2
@@1:    OR      ES:[DI].TCluster.EnableMask.Word[0],AX
        OR      ES:[DI].TCluster.EnableMask.Word[2],DX
@@2:    MOV     CX,ES:[DI].Strings.TCollection.Count
        CMP     CX,32
        JA      @@6
        MOV     BX,ES:[DI].TCluster.Options
        AND     BX,not ofSelectable
        MOV     AX,ES:[DI].TCluster.EnableMask.Word[0]
        MOV     DX,ES:[DI].TCluster.EnableMask.Word[2]
@@3:    SHR     DX,1
        RCR     AX,1
        JC      @@4
        LOOP    @@3
        JMP     @@5
@@4:    OR      BX,ofSelectable
@@5:    MOV     ES:[DI].TCluster.Options,BX
@@6:
end;

procedure TCluster.SetData(var Rec);
begin
  Value := Word(Rec);
  DrawView;
end;

procedure TCluster.SetState(AState: Word; Enable: Boolean);
begin
  TView.SetState(AState, Enable);
  if AState = sfFocused then DrawView;
end;

function TCluster.Mark(Item: Integer): Boolean;
begin
  Mark := False;
end;

function TCluster.MultiMark(Item: Integer): Byte;
begin
  MultiMark := Byte(Mark(Item) = True);
end;

procedure TCluster.MovedTo(Item: Integer);
begin
end;

procedure TCluster.Press(Item: Integer);
begin
end;

procedure TCluster.Store(var S: TStream);
begin
  TView.Store(S);
  S.Write(Value, SizeOf(Longint) * 2 + SizeOf(Integer));
  Strings.Store(S);
end;

function TCluster.Column(Item: Integer): Integer;
var
  I, Col, Width, L: Integer;
begin
  if Item < Size.Y then Column := 0
  else
  begin
    Width := 0;
    Col := -6;
    for I := 0 to Item do
    begin
      if I mod Size.Y = 0 then
      begin
        Inc(Col, Width + 6);
        Width := 0;
      end;
      if I < Strings.Count then
        L := CStrLen(PString(Strings.At(I))^);
      if L > Width then Width := L;
    end;
    Column := Col;
  end;
end;

function TCluster.FindSel(P: TPoint): Integer;
var
  I, S: Integer;
  R: TRect;
begin
  GetExtent(R);
  if not R.Contains(P) then FindSel := -1
  else
  begin
    I := 0;
    while P.X >= Column(I+Size.Y) do
      Inc(I, Size.Y);
    S := I + P.Y;
    if S >= Strings.Count then
      FindSel := -1 else
      FindSel := S;
  end;
end;

function TCluster.Row(Item: Integer): Integer;
begin
  Row := Item mod Size.Y;
end;

{ TRadioButtons }

procedure TRadioButtons.Draw;
const
  Button = ' ( ) ';
begin
  DrawMultiBox(Button, #32#7);
end;

function TRadioButtons.Mark(Item: Integer): Boolean;
begin
  Mark := Item = Value;
end;

procedure TRadioButtons.Press(Item: Integer);
begin
  Value := Item;
end;

procedure TRadioButtons.MovedTo(Item: Integer);
begin
  Value := Item;
end;

procedure TRadioButtons.SetData(var Rec);
begin
  TCluster.SetData(Rec);
  Sel := Integer(Value);
end;

{ TCheckBoxes }

procedure TCheckBoxes.Draw;
const
  Button = ' [ ] ';
begin
  DrawMultiBox(Button, ' X');
end;

function TCheckBoxes.Mark(Item: Integer): Boolean;
begin
  Mark := Value and (1 shl Item) <> 0;
end;

procedure TCheckBoxes.Press(Item: Integer);
begin
  Value := Value xor (1 shl Item);
end;

{ TMultiCheckBoxes }

constructor TMultiCheckBoxes.Init(var Bounds: TRect; AStrings: PSItem;
  ASelRange: Byte; AFlags: Word; const AStates: String);
begin
  Inherited Init(Bounds, AStrings);
  SelRange := ASelRange;
  Flags := AFlags;
  States := NewStr(AStates);
end;

constructor TMultiCheckBoxes.Load(var S: TStream);
begin
  TCluster.Load(S);
  S.Read(SelRange, SizeOf(Byte));
  S.Read(Flags, SizeOf(Word));
  States := S.ReadStr;
end;

destructor TMultiCheckBoxes.Done;
begin
  DisposeStr(States);
  TCluster.Done;
end;

procedure TMultiCheckBoxes.Draw;
const
  Button = ' [ ] ';
begin
  DrawMultiBox(Button, States^);
end;

function TMultiCheckBoxes.DataSize: Word;
begin
  DataSize := SizeOf(Longint);
end;

function TMultiCheckBoxes.MultiMark(Item: Integer): Byte;
begin
  MultiMark := (Value shr (Word(Item) * WordRec(Flags).Hi))
    and WordRec(Flags).Lo;
end;

procedure TMultiCheckBoxes.GetData(var Rec);
begin
  Longint(Rec) := Value;
end;

procedure TMultiCheckBoxes.Press(Item: Integer);
var
  CurState: ShortInt;
begin
  CurState := (Value shr (Word(Item) * WordRec(Flags).Hi))
    and WordRec(Flags).Lo;

  Dec(CurState);
  if (CurState >= SelRange) or (CurState < 0) then
    CurState := SelRange - 1;
  Value := (Value and not (LongInt(WordRec(Flags).Lo)
    shl (Word(Item) * WordRec(Flags).Hi))) or
    (LongInt(CurState) shl (Word(Item) * WordRec(Flags).Hi));
end;

procedure TMultiCheckBoxes.SetData(var Rec);
begin
  Value := Longint(Rec);
  DrawView;
end;

procedure TMultiCheckBoxes.Store(var S: TStream);
begin
  TCluster.Store(S);
  S.Write(SelRange, SizeOf(Byte));
  S.Write(Flags, SizeOf(Word));
  S.WriteStr(States);
end;

{ TListBox }

type
  TListBoxRec = record
    List: PCollection;
    Selection: Word;
  end;

constructor TListBox.Init(var Bounds: TRect; ANumCols: Word;
  AScrollBar: PScrollBar);
begin
  TListViewer.Init(Bounds, ANumCols, nil, AScrollBar);
  List := nil;
  SetRange(0);
end;

constructor TListBox.Load(var S: TStream);
begin
  TListViewer.Load(S);
  List := PCollection(S.Get);
end;

function TListBox.DataSize: Word;
begin
  DataSize := SizeOf(TListBoxRec);
end;

procedure TListBox.GetData(var Rec);
begin
  TListBoxRec(Rec).List := List;
  TListBoxRec(Rec).Selection := Focused;
end;

function TListBox.GetText(Item: Integer; MaxLen: Integer): String;
begin
  if List <> nil then GetText := PString(List^.At(Item))^
  else GetText := '';
end;

procedure TListBox.NewList(AList: PCollection);
begin
  if List <> nil then Dispose(List, Done);
  List := AList;
  if AList <> nil then SetRange(AList^.Count)
  else SetRange(0);
  if Range > 0 then FocusItem(0);
  DrawView;
end;

procedure TListBox.SetData(var Rec);
begin
  NewList(TListBoxRec(Rec).List);
  FocusItem(TListBoxRec(Rec).Selection);
  DrawView;
end;

procedure TListBox.Store(var S: TStream);
begin
  TListViewer.Store(S);
  S.Put(List);
end;

{ TStaticText }

constructor TStaticText.Init(var Bounds: TRect; const AText: String);
begin
  TView.Init(Bounds);
  Text := NewStr(AText);
end;

constructor TStaticText.Load(var S: TStream);
begin
  TView.Load(S);
  Text := S.ReadStr;
end;

destructor TStaticText.Done;
begin
  DisposeStr(Text);
  TView.Done;
end;

procedure TStaticText.Draw;
var
  Color: Byte;
  Center: Boolean;
  I, J, L, P, Y: Integer;
  B: TDrawBuffer;
  S: String;
begin
  Color := GetColor(1);
  GetText(S);
  L := Length(S);
  P := 1;
  Y := 0;
  Center := False;
  while Y < Size.Y do
  begin
    MoveChar(B, ' ', Color, Size.X);
    if P <= L then
    begin
      if S[P] = #3 then
      begin
        Center := True;
        Inc(P);
      end;
      I := P;
      repeat
        J := P;
        while (P <= L) and (S[P] = ' ') do Inc(P);
        while (P <= L) and (S[P] <> ' ') and (S[P] <> #13) do Inc(P);
      until (P > L) or (P >= I + Size.X) or (S[P] = #13);
      if P > I + Size.X then
        if J > I then P := J else P := I + Size.X;
      if Center then J := (Size.X - P + I) div 2 else J := 0;
      MoveBuf(B[J], S[I], Color, P - I);
      while (P <= L) and (S[P] = ' ') do Inc(P);
      if (P <= L) and (S[P] = #13) then
      begin
        Center := False;
        Inc(P);
        if (P <= L) and (S[P] = #10) then Inc(P);
      end;
    end;
    WriteLine(0, Y, Size.X, 1, B);
    Inc(Y);
  end;
end;

function TStaticText.GetPalette: PPalette;
const
  P: String[Length(CStaticText)] = CStaticText;
begin
  GetPalette := @P;
end;

procedure TStaticText.GetText(var S: String);
begin
  if Text <> nil then S := Text^
  else S := '';
end;

procedure TStaticText.Store(var S: TStream);
begin
  TView.Store(S);
  S.WriteStr(Text);
end;

{ TParamText }

constructor TParamText.Init(var Bounds: TRect; const AText: String;
  AParamCount: Integer);
begin
  TStaticText.Init(Bounds, AText);
  ParamCount := AParamCount;
end;

constructor TParamText.Load(var S: TStream);
begin
  TStaticText.Load(S);
  S.Read(ParamCount, SizeOf(Integer));
end;

function TParamText.DataSize: Word;
begin
  DataSize := ParamCount * SizeOf(Longint);
end;

procedure TParamText.GetText(var S: String);
begin
  if Text <> nil then FormatStr(S, Text^, ParamList^)
  else S := '';
end;

procedure TParamText.SetData(var Rec);
begin
  ParamList := @Rec;
  DrawView;
end;

procedure TParamText.Store(var S: TStream);
begin
  TStaticText.Store(S);
  S.Write(ParamCount, SizeOf(Integer));
end;

{ TLabel }

constructor TLabel.Init(var Bounds: TRect; const AText: String; ALink: PView);
begin
  TStaticText.Init(Bounds, AText);
  Link := ALink;
  Options := Options or (ofPreProcess + ofPostProcess);
  EventMask := EventMask or evBroadcast;
end;

constructor TLabel.Load(var S: TStream);
begin
  TStaticText.Load(S);
  GetPeerViewPtr(S, Link);
end;

procedure TLabel.Draw;
var
  Color: Word;
  B: TDrawBuffer;
  SCOff: Byte;
begin
  if Light then
  begin
    Color := GetColor($0402);
    SCOff := 0;
  end
  else
  begin
    Color := GetColor($0301);
    SCOff := 4;
  end;
  MoveChar(B[0], ' ', Byte(Color), Size.X);
  if Text <> nil then MoveCStr(B[1], Text^, Color);
  if ShowMarkers then WordRec(B[0]).Lo := Byte(SpecialChars[SCOff]);
  WriteLine(0, 0, Size.X, 1, B);
end;

function TLabel.GetPalette: PPalette;
const
  P: String[Length(CLabel)] = CLabel;
begin
  GetPalette := @P;
end;

procedure TLabel.HandleEvent(var Event: TEvent);
var
  C: Char;

  procedure FocusLink;
  begin
    if (Link <> nil) and (Link^.Options and ofSelectable <> 0) then
      Link^.Focus;
    ClearEvent(Event);
  end;

begin
  TStaticText.HandleEvent(Event);
  if Event.What = evMouseDown then FocusLink
  else if Event.What = evKeyDown then
  begin
    C := HotKey(Text^);
    if (GetAltCode(C) = Event.KeyCode) or
       ((C <> #0) and (Owner^.Phase = phPostProcess) and
        (UpCase(Event.CharCode) = C)) then FocusLink
  end
  else if Event.What = evBroadcast then
    if ((Event.Command = cmReceivedFocus) or
       (Event.Command = cmReleasedFocus)) and
       (Link <> nil) then
    begin
      Light := Link^.State and sfFocused <> 0;
      DrawView;
    end;
end;

procedure TLabel.Store(var S: TStream);
begin
  TStaticText.Store(S);
  PutPeerViewPtr(S, Link);
end;

{ THistoryViewer }

constructor THistoryViewer.Init(var Bounds: TRect; AHScrollBar,
  AVScrollBar: PScrollBar; AHistoryId: Word);
begin
  TListViewer.Init(Bounds, 1, AHScrollBar, AVScrollBar);
  HistoryId := AHistoryId;
  SetRange(HistoryCount(AHistoryId));
  if Range > 1 then FocusItem(1);
  HScrollBar^.SetRange(1, HistoryWidth-Size.X + 3);
end;

function THistoryViewer.GetPalette: PPalette;
const
  P: String[Length(CHistoryViewer)] = CHistoryViewer;
begin
  GetPalette := @P;
end;

function THistoryViewer.GetText(Item: Integer; MaxLen: Integer): String;
begin
  GetText := HistoryStr(HistoryId, Item);
end;

procedure THistoryViewer.HandleEvent(var Event: TEvent);
begin
  if ((Event.What = evMouseDown) and (Event.Double)) or
     ((Event.What = evKeyDown) and (Event.KeyCode = kbEnter)) then
  begin
    EndModal(cmOk);
    ClearEvent(Event);
  end else if ((Event.What = evKeyDown) and (Event.KeyCode = kbEsc)) or
    ((Event.What = evCommand) and (Event.Command = cmCancel)) then
  begin
    EndModal(cmCancel);
    ClearEvent(Event);
  end else TListViewer.HandleEvent(Event);
end;

function THistoryViewer.HistoryWidth: Integer;
var
  Width, T, Count, I: Integer;
begin
  Width := 0;
  Count := HistoryCount(HistoryId);
  for I := 0 to Count-1 do
  begin
    T := Length(HistoryStr(HistoryId, I));
    if T > Width then Width := T;
  end;
  HistoryWidth := Width;
end;

{ THistoryWindow }

constructor THistoryWindow.Init(var Bounds: TRect; HistoryId: Word);
begin
  TWindow.Init(Bounds, '', wnNoNumber);
  Flags := wfClose;
  InitViewer(HistoryId);
end;

function THistoryWindow.GetPalette: PPalette;
const
  P: String[Length(CHistoryWindow)] = CHistoryWindow;
begin
  GetPalette := @P;
end;

function THistoryWindow.GetSelection: String;
begin
  GetSelection := Viewer^.GetText(Viewer^.Focused,255);
end;

procedure THistoryWindow.InitViewer(HistoryId: Word);
var
  R: TRect;
begin
  GetExtent(R);
  R.Grow(-1,-1);
  Viewer := New(PHistoryViewer, Init(R,
    StandardScrollBar(sbHorizontal + sbHandleKeyboard),
    StandardScrollBar(sbVertical + sbHandleKeyboard),
    HistoryId));
  Insert(Viewer);
end;

{ THistory }

constructor THistory.Init(var Bounds: TRect; ALink: PInputLine;
  AHistoryId: Word);
begin
  TView.Init(Bounds);
  Options := Options or ofPostProcess;
  EventMask := EventMask or evBroadcast;
  Link := ALink;
  HistoryId := AHistoryId;
end;

constructor THistory.Load(var S: TStream);
begin
  TView.Load(S);
  GetPeerViewPtr(S, Link);
  S.Read(HistoryId, SizeOf(Word));
end;

procedure THistory.Draw;
var
  B: TDrawBuffer;
begin
  MoveCStr(B, #222'~'#25'~'#221, GetColor($0102));
  WriteLine(0, 0, Size.X, Size.Y, B);
end;

function THistory.GetPalette: PPalette;
const
  P: String[Length(CHistory)] = CHistory;
begin
  GetPalette := @P;
end;

procedure THistory.HandleEvent(var Event: TEvent);
var
  HistoryWindow: PHistoryWindow;
  R,P: TRect;
  C: Word;
  Rslt: String;
begin
  TView.HandleEvent(Event);
  if (Event.What = evMouseDown) or
     ((Event.What = evKeyDown) and (CtrlToArrow(Event.KeyCode) = kbDown) and
      (Link^.State and sfFocused <> 0)) then
  begin
    if not Link^.Focus then
    begin
      ClearEvent(Event);
      Exit;
    end;
    RecordHistory(Link^.Data^);
    Link^.GetBounds(R);
    Dec(R.A.X); Inc(R.B.X); Inc(R.B.Y,7); Dec(R.A.Y,1);
    Owner^.GetExtent(P);
    R.Intersect(P);
    Dec(R.B.Y,1);
    HistoryWindow := InitHistoryWindow(R);
    if HistoryWindow <> nil then
    begin
      C := Owner^.ExecView(HistoryWindow);
      if C = cmOk then
      begin
        Rslt := HistoryWindow^.GetSelection;
        if Length(Rslt) > Link^.MaxLen then Rslt[0] := Char(Link^.MaxLen);
        Link^.Data^ := Rslt;
        Link^.SelectAll(True);
        Link^.DrawView;
      end;
      Dispose(HistoryWindow, Done);
    end;
    ClearEvent(Event);
  end
  else if (Event.What = evBroadcast) then
    if ((Event.Command = cmReleasedFocus) and (Event.InfoPtr = Link))
      or (Event.Command = cmRecordHistory) then
    RecordHistory(Link^.Data^);
end;

function THistory.InitHistoryWindow(var Bounds: TRect): PHistoryWindow;
var
  P: PHistoryWindow;
begin
  P := New(PHistoryWindow, Init(Bounds, HistoryId));
  P^.HelpCtx := Link^.HelpCtx;
  InitHistoryWindow := P;
end;

procedure THistory.RecordHistory(const S: String);
begin
  HistoryAdd(HistoryId, S);
end;

procedure THistory.Store(var S: TStream);
begin
  TView.Store(S);
  PutPeerViewPtr(S, Link);
  S.Write(HistoryId, SizeOf(Word));
end;

{ Dialogs registration procedure }

procedure RegisterDialogs;
begin
  RegisterType(RDialog);
  RegisterType(RInputLine);
  RegisterType(RButton);
  RegisterType(RCluster);
  RegisterType(RRadioButtons);
  RegisterType(RCheckBoxes);
  RegisterType(RMultiCheckBoxes);
  RegisterType(RListBox);
  RegisterType(RStaticText);
  RegisterType(RLabel);
  RegisterType(RHistory);
  RegisterType(RParamText);
end;

end.
