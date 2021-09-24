{$S-,R-,V-,I-,B-,F-,O-,A-}

{$IFDEF Dpmi}
{$C FIXED PRELOAD PERMANENT}
{$ENDIF}

{$I OPDEFINE.INC}

{*********************************************************}
{*                    OPCRT.PAS 1.30                     *}
{*     Copyright (c) TurboPower Software 1987, 1992.     *}
{*                 All rights reserved.                  *}
{*********************************************************}
{*   Compatibility with Virtual Pascal for OS/2 v1.0:    *}
{*             Copyright (c) fPrint UK Ltd 1995          *}
{*                 All rights reserved.                  *}
{*********************************************************}

Unit OpCrt;
  {-Alternate CRT interface unit. Replaces Turbo's CRT unit.}

{$IFDEF Windows}
  {$IFNDEF WIN32}
  !! ERROR: This unit is not compatible with Windows applications !!
  {$ENDIF}
{$ENDIF}

interface

{$IFDEF VIRTUALPASCAL}
  {&Use32+}
{$ENDIF}

uses
  OpInline,
  {$IFDEF VIRTUALPASCAL}
  VpSysLow,
  {$IFDEF DPMI32}
  Dpmi32,
  Dpmi32df,
  {$ENDIF}
  {$IFDEF WIN32}
  Windows,
  VPUtils,
  {$ENDIF}
  {$ELSE}
  Dpmi,
  {$ENDIF}
  {$IFDEF UseCrt}
  Crt,
  {$ENDIF}
  Dos;

  {low-level frame stuff}
const
  frTL = 0;
  frBL = 1;
  frTR = 2;
  frBR = 3;
  frTT = 4;
  frBB = 5;
  frLL = 6;
  frRR = 7;

type
  FrameCharType = frTL..frRR;
  FrameArray    = array [FrameCharType] of Char;
  CursorType    = (cuNormal, cuFat, cuBlock, cuHidden, cuUnknown);

const
  {video mode constants}
  BW40    = 0;
  CO40    = 1;
  C40     = CO40;
  BW80    = 2;
  CO80    = 3;
  C80     = CO80;
  Mono    = 7;
  Font8x8 = 256;

  {color constants}
  Black        = 0;
  Blue         = 1;
  Green        = 2;
  Cyan         = 3;
  Red          = 4;
  Magenta      = 5;
  Brown        = 6;
  LightGray    = 7;
  DarkGray     = 8;
  LightBlue    = 9;
  LightGreen   = 10;
  LightCyan    = 11;
  LightRed     = 12;
  LightMagenta = 13;
  Yellow       = 14;
  White        = 15;
  Blink        = 128;

  {special flag used by MapMono}
  AutoMapMono  = $FF;

const
  {Set to True to allow programs to run as background tasks under
   DesqView/TaskView. Must be set False for TSR's.}
  DetectMultitasking : Boolean = False;
  BiosScroll : Boolean = False; {False to use OPCRT routines for clean scrolling}

const
  FrameChars : FrameArray = 'ÕÔ¸¾ÍÍ³³';

const
  InsertCursor   : CursorType = cuFat;
  OvertypeCursor : CursorType = cuNormal;
  CtrlCharCursor : CursorType = cuBlock;

type
  DisplayType = (MonoHerc, CGA, MCGA, EGA, VGA, PGC);
  HercCardType = (HercNone, HercPlain, HercPlus, HercInColor);
  FlexAttrs = array [0..3] of Byte; {attributes for FlexWrite}

  {record used to save/restore window coordinates}
  WindowCoordinates =
    record
      XL, YL, XH, YH : Byte;
    end;

{from Turbo's CRT unit}

{$IFNDEF UseCrt}

var
  CheckBreak : Boolean;      {enable Ctrl-Break checking}
  CheckEOF : Boolean;        {enable Ctrl-Z checking}
  DirectVideo : Boolean;     {write directly to screen?}
  CheckSnow : Boolean;       {True to prevent snow on CGA's}
  TextAttr : Byte;           {current video attribute}
  WindMin : Word;            {Window XLow and YLow: 0..24, 0..79 format}
  WindMax : Word;            {Window XHigh and YHigh: 0..24, 0..79 format}
  LastMode : Word;           {current video mode in low byte / 8x8 flag in high byte}
const
  CrtCheck : Boolean = True; {for internal use}

{$ELSE}

var
  CheckBreak : Boolean absolute Crt.CheckBreak;
  CheckEOF : Boolean absolute Crt.CheckEOF;
  DirectVideo : Boolean absolute Crt.DirectVideo;
  CheckSnow : Boolean absolute Crt.CheckSnow;
  TextAttr : Byte absolute Crt.TextAttr;
  WindMin : Word absolute Crt.WindMin;
  WindMax : Word absolute Crt.WindMax;
  LastMode : Word absolute Crt.LastMode;

{$ENDIF}

{unique to Object Professional version}

var
  CtrlBreakFlag : Boolean;   {set to true when ^Break pressed}
  CurrentPage : Byte;        {current video page}
  CurrentMode : Byte absolute LastMode; {current video mode}
  ScreenWidth : Word;        {current width of screen}
  ScreenHeight : Word;       {current height of screen}
  CurrentDisplay : DisplayType; {currently selected display adapter}
  EnhancedDisplay : DisplayType; {meaningful only if set to MCGA, VGA, or EGA}
  WhichHerc : HercCardType;  {type of Hercules card installed}
  InTextMode : Boolean;      {set to false when in graphics mode}
  NormalAttr : Byte;         {attribute for NormVideo}
  VideoSegment : Word;       {current segment for video memory}
  BufLen : Word;             {maximum length of string for Read/Ln}
  MultiTaskingOn : Boolean;  {needed to support DesqView, TaskView}
  (*
  OneMS : Word;              {loop count used for a 1 ms delay}
  LongOneMS : Longint;       {long loop count used for a 1 ms delay}  {!!.31}
  *)
  SaveCtrlC : Boolean;       {was ^C checking on when program started?}
  {for backward compatibility}
  CurrentWidth : Word absolute ScreenWidth; {current width of display}
  CurrentHeight : Word;      {current height of display - 1}
  {hooks for virtual screens in OPWINDOW}
  VirtualSegment : Word;     {alternate segment for video memory}
{$IFDEF VIRTUALPASCAL}
  VideoLength     : SmallWord;
{$ENDIF}
  VirtualWidth : Word;       {alternate width of display}
  VirtualHeight : Word;      {alternate height of display}

const
  DisplayOverride : ShortInt = -1; {use to override auto-detection of displays}
  UseEnhancedKbd : Boolean = False; {use to activate enhanced keyboard calls}
  {background character used by ClrScr, ScrollWindowUp/Down; gets reset to ' '
   automatically}
  TextChar : Char = ' ';
  HercExceptionReg : Byte = $27;

  {------------- miscellaneous default colors, etc. --------------}
type
  ColorChoice = (UseDefault, ForceColor, ForceMono);

const
  DefColorChoice : ColorChoice = UseDefault;

type
  ColorSetPtr = ^ColorSet;   {!!.20}
  ColorSet =
    object
      TextColor, TextMono : Byte;             {ordinary text}
      CtrlColor, CtrlMono : Byte;             {control characters}
      FrameColor, FrameMono : Byte;           {window frames}
      HeaderColor, HeaderMono : Byte;         {window headers}
      ShadowColor, ShadowMono : Byte;         {window shadows}
      HighlightColor, HighlightMono : Byte;   {found text, etc.}
      PromptColor, PromptMono : Byte;         {prompts}
      SelPromptColor, SelPromptMono : Byte;   {selected prompts}
      ProPromptColor, ProPromptMono : Byte;   {protected prompts}
      FieldColor, FieldMono : Byte;           {fields}
      SelFieldColor, SelFieldMono : Byte;     {selected fields}
      ProFieldColor, ProFieldMono : Byte;     {protected fields}
      ScrollBarColor, ScrollBarMono : Byte;   {scroll bars}
      SliderColor, SliderMono : Byte;         {slider in scroll bars}
      HotSpotColor, HotSpotMono : Byte;       {hot spots in frame corners}
      BlockColor, BlockMono : Byte;           {marked blocks}
      MarkerColor, MarkerMono : Byte;         {text markers}
      DelimColor, DelimMono : Byte;           {delimiters}
      SelDelimColor, SelDelimMono : Byte;     {selected delimiters}
      ProDelimColor, ProDelimMono : Byte;     {protected delimiters}
      SelItemColor, SelItemMono : Byte;       {selected pick and menu items}
      ProItemColor, ProItemMono : Byte;       {protected pick and menu items}
      HighItemColor, HighItemMono : Byte;     {highlight chars in pick and menu}
      AltItemColor, AltItemMono : Byte;       {alternate pick items}
      AltSelItemColor, AltSelItemMono : Byte; {alternate selected pick items}
      FlexAHelpColor, FlexAHelpMono : Byte;   {help flex highlight for ^A}
      FlexBHelpColor, FlexBHelpMono : Byte;   {help flex highlight for ^B}
      FlexCHelpColor, FlexCHelpMono : Byte;   {help flex highlight for ^C}
      UnselXrefColor, UnselXrefMono : Byte;   {unselected help xref}
      SelXrefColor, SelXrefMono : Byte;       {selected help xref}
      MouseColor, MouseMono : Byte;           {mouse colors}

      procedure SetTextAttr(Color, Mono : Byte);
        {-Set attributes for ordinary text}
      procedure SetCtrlAttr(Color, Mono : Byte);
        {-Set attributes for control characters}
      procedure SetFrameAttr(Color, Mono : Byte);
        {-Set attributes for window frames}
      procedure SetHeaderAttr(Color, Mono : Byte);
        {-Set attributes for window headers}
      procedure SetShadowAttr(Color, Mono : Byte);
        {-Set attributes for window shadows}
      procedure SetHighlightAttr(Color, Mono : Byte);
        {-Set attributes used to highlight found text}
      procedure SetPromptAttr(Color, Mono : Byte);
        {-Set attributes for entry field prompts}
      procedure SetSelectedPromptAttr(Color, Mono : Byte);
        {-Set attributes for selected entry field prompts}
      procedure SetProtectedPromptAttr(Color, Mono : Byte);
        {-Set attributes for protected entry field prompts}
      procedure SetFieldAttr(Color, Mono : Byte);
        {-Set attributes for unselected entry fields}
      procedure SetSelectedFieldAttr(Color, Mono : Byte);
        {-Set attributes for selected entry field fields}
      procedure SetProtectedFieldAttr(Color, Mono : Byte);
        {-Set attributes for protected entry fields}
      procedure SetScrollBarAttr(Color, Mono : Byte);
        {-Set attributes for scroll bars}
      procedure SetSliderAttr(Color, Mono : Byte);
        {-Set attributes for sliders in scroll bars}
      procedure SetHotSpotAttr(Color, Mono : Byte);
        {-Sets attributes for mouse hot spots}
      procedure SetBlockAttr(Color, Mono : Byte);
        {-Set attributes for marked blocks}
      procedure SetMarkerAttr(Color, Mono : Byte);
        {-Set attributes for text markers}
      procedure SetDelimAttr(Color, Mono : Byte);
        {-Set attributes for unselected entry field delimiters}
      procedure SetSelectedDelimAttr(Color, Mono : Byte);
        {-Set attributes for selected entry field delimiters}
      procedure SetProtectedDelimAttr(Color, Mono : Byte);
        {-Set attributes for protected entry field delimiters}
      procedure SetSelectedItemAttr(Color, Mono : Byte);
        {-Set attributes for selected pick and menu items}
      procedure SetProtectedItemAttr(Color, Mono : Byte);
        {-Set attributes for protected pick and menu items}
      procedure SetHighlightItemAttr(Color, Mono : Byte);
        {-Set attributes for highlighted selection characters for menu items}
      procedure SetAltItemAttr(Color, Mono : Byte);          {!!.01}
        {-Set attributes for alternate pick items}
      procedure SetAltSelItemAttr(Color, Mono : Byte);       {!!.01}
        {-Set attributes for alternate selected pick items}
      procedure SetFlexAHelpAttr(Color, Mono : Byte);
        {-Set attributes for help text surrounded by ^A}
      procedure SetFlexBHelpAttr(Color, Mono : Byte);
        {-Set attributes for help text surrounded by ^B}
      procedure SetFlexCHelpAttr(Color, Mono : Byte);
        {-Set attributes for help text surrounded by ^C}
      procedure SetUnselXrefAttr(Color, Mono : Byte);
        {-Set attributes for unselected cross-references in help text}
      procedure SetSelXrefAttr(Color, Mono : Byte);
        {-Set attributes for selected cross-references in help text}
      procedure SetMouseAttr(Color, Mono : Byte);
        {-Set attributes for mouse cursor}
    end;

const
  DefaultPasswordChar = ^G;
  DefaultPadChar = ' ';
(*
  DefaultColorSet : ColorSet = (
    TextColor       : $07; TextMono       : $07;
    CtrlColor       : $07; CtrlMono       : $07;
    FrameColor      : $0F; FrameMono      : $0F;
    HeaderColor     : $0F; HeaderMono     : $0F;
    ShadowColor     : $07; ShadowMono     : $07;
    HighlightColor  : $70; HighlightMono  : $70;
    PromptColor     : $07; PromptMono     : $07;
    SelPromptColor  : $07; SelPromptMono  : $07;
    ProPromptColor  : $07; ProPromptMono  : $07;
    FieldColor      : $0F; FieldMono      : $0F;
    SelFieldColor   : $70; SelFieldMono   : $70;
    ProFieldColor   : $07; ProFieldMono   : $07;
    ScrollBarColor  : $07; ScrollBarMono  : $07;
    SliderColor     : $07; SliderMono     : $07;
    HotSpotColor    : $70; HotSpotMono    : $07;

    BlockColor      : $0F; BlockMono      : $0F;
    MarkerColor     : $70; MarkerMono     : $70;
    DelimColor      : $0F; DelimMono      : $0F;
    SelDelimColor   : $70; SelDelimMono   : $70;
    ProDelimColor   : $07; ProDelimMono   : $07;
    SelItemColor    : $70; SelItemMono    : $70;
    ProItemColor    : $00; ProItemMono    : $01;
    HighItemColor   : $0F; HighItemMono   : $0F;
    AltItemColor    : $0F; AltItemMono    : $0F;
    AltSelItemColor : $70; AltSelItemMono : $70;
    FlexAHelpColor  : $0F; FlexAHelpMono  : $0F;
    FlexBHelpColor  : $01; FlexBHelpMono  : $01;
    FlexCHelpColor  : $70; FlexCHelpMono  : $70;
    UnselXrefColor  : $09; UnselXrefMono  : $09;
    SelXrefColor    : $70; SelXrefMono    : $70;
    MouseColor      : $4F; MouseMono      : $70
  );
*)

  function MakeHiddenAttr(A : Byte) : Byte;
    {-Make foreground and background colors the same}
  {$IFDEF VIRTUALPASCAL}
  inline;
  begin
    MakeHiddenAttr := ( A shr 4 ) or A and $F0;
  end;
  {$ELSE}
  inline(
    $58/                   {pop ax}
    $24/$F0/               {and al,$f0  ;isolate background color}
    $88/$C4/               {mov ah,al   ;get new foreground into AH}
    $B1/$04/               {mov cl,4}
    $D2/$EC/               {shr ah,cl}
    $08/$E0);              {or al,ah    ;merge it into AL}
  {$ENDIF}

type
  ColorMapTable = array[0..255] of Byte;

const
  ColorMap : ColorMapTable = (
      {00   01   02   03   04   05   06   07   08   09   0A   0B   0C   0D   0E   0F}
 {00} $00, $01, $07, $07, $07, $07, $07, $07, $07, $07, $07, $07, $07, $07, $0F, $0F,
 {10} $70, $01, $07, $07, $07, $07, $07, $07, $07, $07, $07, $07, $07, $07, $0F, $0F,
 {20} $70, $70, $70, $70, $70, $70, $70, $70, $70, $70, $70, $70, $70, $70, $70, $70,
 {30} $70, $70, $70, $70, $70, $70, $70, $70, $70, $70, $70, $70, $70, $70, $70, $70,
 {40} $70, $70, $70, $70, $70, $70, $70, $70, $70, $70, $70, $70, $70, $70, $70, $70,
 {50} $70, $70, $70, $70, $70, $70, $70, $70, $70, $70, $70, $70, $70, $70, $70, $70,
 {60} $70, $70, $70, $70, $70, $70, $70, $70, $70, $70, $70, $70, $70, $70, $70, $70,
 {70} $70, $70, $70, $70, $70, $70, $70, $70, $70, $70, $70, $70, $70, $70, $70, $70,
 {80} $80, $81, $87, $87, $87, $87, $87, $87, $87, $87, $87, $87, $87, $87, $8F, $8F,
 {90} $F0, $81, $87, $87, $87, $87, $87, $87, $87, $87, $87, $87, $87, $87, $8F, $8F,
 {A0} $F0, $F0, $F0, $F0, $F0, $F0, $F0, $F0, $F0, $F0, $F0, $F0, $F0, $F0, $F0, $F0,
 {B0} $F0, $F0, $F0, $F0, $F0, $F0, $F0, $F0, $F0, $F0, $F0, $F0, $F0, $F0, $F0, $F0,
 {C0} $F0, $F0, $F0, $F0, $F0, $F0, $F0, $F0, $F0, $F0, $F0, $F0, $F0, $F0, $F0, $F0,
 {D0} $F0, $F0, $F0, $F0, $F0, $F0, $F0, $F0, $F0, $F0, $F0, $F0, $F0, $F0, $F0, $F0,
 {E0} $F0, $F0, $F0, $F0, $F0, $F0, $F0, $F0, $F0, $F0, $F0, $F0, $F0, $F0, $F0, $F0,
 {F0} $F0, $F0, $F0, $F0, $F0, $F0, $F0, $F0, $F0, $F0, $F0, $F0, $F0, $F0, $F0, $F0
     );

procedure TextMode(Mode : Word);
  {-Switch to/set text mode}

procedure Window(XLow, YLow, XHigh, YHigh : Byte);
  {-Set current window coordinates}

procedure ClrScr;
  {-Clear the current window}

procedure ClrEol;
  {-Clear the remainder of the current screen line}

procedure InsLine;
  {-Insert a new line at the position of the cursor}

procedure DelLine;
  {-Delete current screen line}

procedure GoToXY(X, Y : Byte);
  {-Move cursor to column X, row Y, relative to Window}

function WhereX : Byte;
  {-Return column coordinate of cursor, relative to Window}

function WhereY : Byte;
  {-Return row coordinate of cursor, relative to Window}

procedure TextColor(Color : Byte);
  {-Set foreground color for screen writes}

procedure TextBackground(Color : Byte);
  {-Set background color for screen writes}

procedure LowVideo;
  {-Select low intensity}

procedure HighVideo;
  {-Select high intensity}

procedure NormVideo;
  {-Select video attribute used at start of program}

function KeyPressed : Boolean;
  {-Return true if a key has been pressed}

function ReadKey : Char;
  {-Read a character from the keyboard}

procedure AssignCrt(var F : Text);
  {-Routes input and output through our routines}

{procedure Delay(MS : Word);}
  {-Delay for MS milliseconds}

{procedure PlaySound(Freq,Duration: Longint);}
  {-Play sound}

{$IfDef Ver70}
procedure Sound(Hz : Word);
  {-Turn on the sound at the designated frequency}

procedure NoSound;
  {-Turn off the sound}
{$ENDIF}

  {****** extensions to Turbo's CRT unit ******}

function GetCrtMode : Byte;
 {-Get the current video mode. Also reinitializes internal variables. May
   reset: CurrentMode, ScreenWidth, ScreenHeight, CurrentPage, and
   VideoSegment.}

procedure GotoXYAbs(X, Y : Byte);
  {-Move cursor to column X, row Y. No error checking done.}

function WhereXY : Word;
 {-Return absolute row and column coordinates of cursor. High byte has current
   row (Y), low byte has current column (X).}

function WhereYAbs : Byte;
  {-Return absolute row coordinate of cursor}

function WhereXAbs : Byte;
  {-Return absolute column coordinate of cursor}

procedure WhereXYdirect(var X, Y : Byte);
  {-Read the current position of the cursor directly from the CRT controller}

procedure SetVisiblePage(PageNum : Byte);
  {-Set current video page}

procedure SetActivePage(PageNo : Byte);
 {-Selects the video page that will be written to with subsequent operations
   on the screen. Does not affect Write/Ln or Read/Ln.}

function GetPageSegment(PageNo : Byte) : Word;
  {-Get the video segment corresponding to the specified video page}

function GetPagePointer(PageNo : Byte) : Pointer;
  {-Get a pointer to the start of the specified video page}

procedure ClearPage(PageNo : Byte);
  {-Clear the specified video page with TextChar and TextAttr}

procedure ScrollWindowUp(XLo, YLo, XHi, YHi, Lines : Byte);
  {-Scrolls the designated window up the specified number of lines.}

procedure ScrollWindowDown(XLo, YLo, XHi, YHi, Lines : Byte);
  {-Scrolls the designated window down the specified number of lines.}

function CursorTypeSL : Word;
  {-Returns a word. High byte has starting scan line, low byte has ending.}

function CursorStartLine : Byte;
  {-Returns the starting scan line of the cursor}

function CursorEndLine : Byte;
  {-Returns the ending scan line of the cursor.}

procedure SetCursorSize(Startline, EndLine : ShortInt);
  {-Sets the cursor's starting and ending scan lines.}

procedure NormalCursor;
  {-Set normal scan lines for cursor based on current video mode}

procedure FatCursor;
  {-Set larger scan lines for cursor based on current video mode}

procedure BlockCursor;
  {-Set scan lines for a block cursor}

procedure HiddenCursor;
  {-Hide the cursor}

procedure SetCursorType(CT : CursorType);
  {-Sets the cursor shape to the specified type}

function ClassifyCursorType : CursorType;
  {-Classify cursor type based on scan lines}

function ReadCharAtCursor : Char;
  {-Returns character at the current cursor location on the selected page.}

function ReadAttrAtCursor : Byte;
  {-Returns attribute at the current cursor location on the selected page.}

procedure GetCursorState(var XY, ScanLines : Word);
  {-Return the current position and size of the cursor}

procedure RestoreCursorState(XY, ScanLines : Word);
  {-Reset the cursor to a position and size saved with GetCursorState}

procedure FastWrite(St : string; Row, Col : Word; Attr : Byte);
  {-Write St at Row,Col in Attr (video attribute) without snow}

procedure FastText(St : string; Row, Col : Word);
  {-Write St at Row,Col without changing the underlying video attribute.}

procedure FastVert(St : string; Row, Col : Word; Attr : Byte);
  {-Write St vertically at Row,Col in Attr (video attribute)}

procedure FastFill(Number : Word; Ch : Char; Row, Col : Word; Attr : Byte);
  {-Fill Number chs at Row,Col in Attr (video attribute) without snow}

procedure FastFillVert(Number : Word; Ch : Char; Row, Col : Word; Attr : Byte); {!!.03}
  {-Fill Number chs vertically at Row,Col in Attr (video attribute)} {!!.03}

procedure FastCenter(St : string; Row, Attr : Byte);
  {-Write St centered on window Row in Attr (video attribute) without snow}

procedure FastFlush(St : string; Row, Attr : Byte);
  {-Write St flush right on window Row in Attr (video attribute) without snow}

procedure FastRead(Number : Byte; Row, Col : Word; var St : string);
  {-Read Number characters from the screen into St starting at Row,Col}

procedure ReadAttribute(Number : Byte; Row, Col : Word; var St : string);
  {-Read Number attributes from the screen into St starting at Row,Col}

procedure WriteAttribute(St : String; Row, Col : Word);
  {-Write string of attributes St at Row,Col without changing characters}

procedure ChangeAttribute(Number : Word; Row, Col : Word; Attr : Byte);
  {-Change Number video attributes to Attr starting at Row,Col}

procedure MoveScreen(var Source, Dest; Length : Word);
  {-Move Length words from Source to Dest without snow}

procedure FastWriteAttr(St : String; Row, Col : Word; AttrSt : String);
  {-Write St at Row,Col using attributes in AttrSt}

procedure FlexWrite(St : string; Row, Col : Word; var FAttrs : FlexAttrs);
  {-Write St at Row,Col with flexible color handling}

function FlexLen(S : string) : Byte;
  {-Return actual (visible) length of a flex string}

procedure FastWriteCtrl(St : String; Row, Col : Word; Attr, Ctrl : Byte);
  {-Write St at Row,Col in Attr (video attribute) without snow.
    Control characters displayed in Ctrl as upper-case letters}

function SaveWindow(XLow, YLow, XHigh, YHigh : Byte; Allocate : Boolean;
                    var Covers : Pointer) : Boolean;
  {-Allocate buffer space if requested and save window contents}

procedure RestoreWindow(XLow, YLow, XHigh, YHigh : Byte;
                        Deallocate : Boolean; var Covers : Pointer);
  {-Restore screen contents and deallocate buffer space if requested}

procedure ClearWindow(XLow, YLow, XHigh, YHigh : Word; Ch : Char; A : Byte);
  {-Clear a window using the specified character and attribute}

procedure FrameWindow(LeftCol, TopRow, RightCol, BotRow : Word;
                      FAttr, HAttr : Byte;
                      Header : string);
  {-Draws a frame around a window}

procedure StoreWindowCoordinates(var WC : WindowCoordinates);
  {-Store the window coordinates for the active window}

procedure RestoreWindowCoordinates(WC : WindowCoordinates);
  {-Restore previously saved window coordinates}

function MapColor(C : Byte) : Byte;
  {-Map a video attribute for visibility on mono/bw displays}

function UseColor : Boolean;
  {-Returns True if color video attributes should be used}

function ColorMono(Color, Mono : Byte) : Byte;
  {-Choose between a color and a monochrome video attribute}

function MapMono(Color, Mono : Byte) : Byte;
  {-Return a mapped color video attribute if Mono = $FF}

procedure SetBlink(IsOn : Boolean);
  {-Enable text mode attribute blinking if On is True}

procedure SetCrtBorder(Attr : Byte);
  {-Set border to background color if card type and mode allow}

function Font8x8Selected : Boolean;
  {-Return True if EGA or VGA is active and in 8x8 font}

procedure SelectFont8x8(IsOn : Boolean);
  {-Toggle 8x8 font on or off}

function HercPresent : Boolean;
  {-Return true if a Hercules graphics card is present}

procedure SwitchInColorCard(ColorOn : Boolean);
  {-Activate or deactivate colors on a Hercules InColor card}

function HercGraphicsMode : Boolean;
  {-Return True if a Hercules card is in graphics mode}

function HercModeTestWorks : Boolean;
  {-Return True if HercGraphicsMode will work}

procedure SetHercMode(GraphMode : Boolean; GraphPage : Byte);
 {-Set Hercules card to graphics mode or text mode, and activate specified
   graphics page (if switching to graphics mode).}

function ReadKeyWord : Word;
 {-Waits for keypress, then returns scan and character codes together}

function CheckKbd(var KeyCode : Word) : Boolean;
  {-Returns True (and the key codes) if a keystroke is waiting}

function KbdFlags : Byte;
  {-Returns keyboard status flags as a bit-coded byte}

function EnhancedKbdInstalled : Boolean;
  {-Returns True if an enhanced keyboard is installed}

procedure StuffKey(W : Word);
  {-Stuff one key into the keyboard buffer}

procedure StuffString(S : string);
  {-Stuff the contents of S into the keyboard buffer}

procedure ReInitCrt;
  {-Reinitialize CRT unit's internal variables. For TSR's or programs with
    DOS shells. May reset: CurrentMode, ScreenWidth, ScreenHeight,
    WindMin/WindMax, CurrentPage, CurrentDisplay, CheckSnow, and VideoSegment.}

{$IFNDEF VIRTUALPASCAL}
{procedure DelayCalibrate;}
  {-Perform delay calibration}
{$ENDIF}

  {==========================================================================}

implementation

type
  BufPtr = ^BufferArray;
  BufferArray = array[0..MaxInt] of Char;
  BiosNameType = Array[1..6] of Char;                               {!!.20}

var
  {$IFNDEF UseCrt}
  SystemSeg : Word;
  {$ENDIF}
  SaveExitProc : Pointer;
{$IFNDEF VIRTUALPASCAL}
  BiosScanLines : ^Word; {absolute $40 : $60;}                      {!!.20}
  IsCompaq : Boolean;
  CompaqBiosName : BiosNameType; {absolute $FFFE : $000A;}          {!!.20}
  IsZenith : Boolean absolute IsCompaq;
  ZenithBiosName : BiosNameType; {absolute $FB00 : $0000;}          {!!.20}
  NextChar : Byte;
{$ENDIF}

{$IFDEF VIRTUALPASCAL}

  Procedure Unsupported;
    begin
      Writeln('Function not supported!');
      Halt;
    end;


  {$I VPCRT.IN1}

{$ELSE}
  {$L OPCRT1.OBJ}
  {$L OPCRT2.OBJ}
  {$L OPFAST.OBJ}
  {$L OPFAST2.OBJ}
  {$L OPCMISC.OBJ}
  {$L OPFLEX.OBJ}

  {local routines in OPCRT.OBJ}
  procedure ReadCursorPrim; external;
  procedure SetCursorPrim; external;
  procedure GetCursorPrim; external;
  procedure GetCrtModePrim; external;
  procedure ScrollUpPrim; external;
  procedure ScrollDownPrim; external;
  procedure AdapterCheck; external;
  {procedure DelayMS; external;}
  procedure GetCharAttr; external;
  procedure SetWindowPrim; external;
  procedure FullWindow; external;
  procedure GetAttribute; external;
  procedure InitCrt; external;
  procedure CalcOffset; external;

  {global routines in OPCRT.OBJ}
  function ReadKeyWord : Word; external;
  function KeyPressed : Boolean; external;
  function ReadKey : Char; external;
  procedure AssignCrt(var F : Text); external;
  procedure ReInitCrt; external;
  {procedure DelayCalibrate; external;}

  {routines in OPCRT2.OBJ}
  procedure TextMode(Mode : Word); external;
  procedure Window(XLow, YLow, XHigh, YHigh : Byte); external;
  procedure ClrScr; external;
  procedure GoToXY(X, Y : Byte); external;
  function WhereX : Byte; external;
  function WhereY : Byte; external;
  procedure TextColor(Color : Byte); external;
  procedure TextBackground(Color : Byte); external;
  procedure LowVideo; external;
  procedure HighVideo; external;
  procedure NormVideo; external;
  {procedure Delay(MS : Word); external;}
  function GetCrtMode : Byte; external;
  procedure GotoXYAbs(X, Y : Byte); external;
  function Font8x8Selected : Boolean; external;
  procedure SelectFont8x8(IsOn : Boolean); external;

  {routines in OPCMISC.OBJ}
  procedure ClrEol; external;
  procedure InsLine; external;
  procedure DelLine; external;
  procedure Sound(Hz : Word); external;
  procedure NoSound; external;
  function WhereXY : Word; external;
  function WhereXAbs : Byte; external;
  function WhereYAbs : Byte; external;
  function ReadCharAtCursor : Char; external;
  function ReadAttrAtCursor : Byte; external;
  procedure SetVisiblePage(PageNum : Byte); external;
  procedure ScrollWindowUp(XLo, YLo, XHi, YHi, Lines : Byte); external;
  procedure ScrollWindowDown(XLo, YLo, XHi, YHi, Lines : Byte); external;
  function CursorTypeSL : Word; external;
  function CursorStartLine : Byte; external;
  function CursorEndLine : Byte; external;
  procedure SetCursorSize(Startline, EndLine : ShortInt); external;
  function KbdFlags : Byte; external;
  function CheckKbd(var KeyCode : Word) : Boolean; external;

  {routines in OPFAST.OBJ}
  procedure FastWrite(St : string; Row, Col : Word; Attr : Byte); external;
  procedure FastFill(Number : Word; Ch : Char; Row, Col : Word; Attr : Byte); external;
  procedure FastFillVert(Number : Word; Ch : Char; Row, Col : Word; Attr : Byte); external; {!!.03}
  procedure FastVert(St : string; Row, Col : Word; Attr : Byte); external;
  procedure ChangeAttribute(Number : Word; Row, Col : Word; Attr : Byte); external;
  procedure MoveScreen(var Source, Dest; Length : Word); external;

  {routines in OPFAST2.OBJ}
  procedure FastText(St : string; Row, Col : Word); external;
  procedure WriteAttribute(St : String; Row, Col : Word); external;
  procedure FastRead(Number : Byte; Row, Col : Word; var St : string); external;
  procedure ReadAttribute(Number : Byte; Row, Col : Word; var St : string); external;
  procedure FastCenter(St : string; Row, Attr : Byte); external;
  procedure FastFlush(St : string; Row, Attr : Byte); external;

  {routines in OPFLEX.OBJ}
  procedure FlexWrite(St : string; Row, Col : Word; var FAttrs : FlexAttrs); external;
  function FlexLen(S : string) : Byte; external;
  procedure FastWriteCtrl(St : String; Row, Col : Word; Attr, Ctrl : Byte); external;
  procedure FastWriteAttr(St : String; Row, Col : Word; AttrSt : String); external;
{$ENDIF}

  {$I OPCRT.IN1}    {Hercules stuff, alternate video page routines}

{$IfDef Ver70}
  {procedure PlaySound;
   begin
    Sound(Freq);
    Delay(Duration);
    NoSound;
   end;}
{$EndIf}

  procedure InitMemorySegments;                               {!!.20 - new}
    {-initialize important vars from segment/selector data}
  begin
    {$IFNDEF VIRTUALPASCAL}
    BiosScanLines := Ptr(BiosDataSele, $60);
    Move(Ptr(BiosSele, $FFEA)^, CompaqBiosName, SizeOf(BiosNameType));
    Move(Ptr(BiosSele, $B000)^, ZenithBiosName, SizeOf(BiosNameType));
    {$ENDIF}
  end;

  procedure CrtTest;
    {-Test for presence of CRT in program}
  begin
    {$IFNDEF UseCrt}
    if CrtCheck then
{$IFDEF VIRTUALPASCAL}
    If TextRec( Input ).OpenFunc <> TextRec( System.Input ).OpenFunc then
{$ELSE}
      with OS(TextRec(Input).OpenFunc) do {!!.21}
        if (S <> CSeg) and (S <> SystemSeg) then
{$ENDIF}
        begin
          WriteLn('CRT/OPCRT conflict');
          Halt(1);
        end;
    {$ENDIF}
  end;

  {$IFDEF VirtualPascal}
  procedure InitCursor;
  var
    cStart, cEnd: Integer;
    cVisible: Boolean;
  begin
    SysTVGetCurType(cStart, cEnd, cVisible);
    SaveCursor := cStart shl 8 + cEnd;
  end;

  procedure NormalCursor;
  begin
    SetCursorSize(SaveCursor shr 8, SaveCursor and $FF);
  end;

  procedure FatCursor;
  begin
    SetCursorSize(-50, -100);
  end;

  procedure BlockCursor;
  begin
    SetCursorSize(0, -100);
  end;

  procedure HiddenCursor;
  begin
    SetCursorSize($20, 0);
  end;
  {$ELSE}

  procedure NormalCursor;
    {-Set normal scan lines for cursor based on current video mode}
  var
    ScanLines : Word;
  begin
    if (Hi(LastMode) <> 0) then
      ScanLines := $0507
    else
      if CurrentMode = 7 then
        ScanLines := $0B0C
      else
        ScanLines := $0607;
    SetCursorSize(Hi(ScanLines), Lo(ScanLines));
  end;

  procedure FatCursor;
    {-Set larger scan lines for cursor based on current video mode}
  var
    ScanLines : Word;
  begin
    if (Hi(LastMode) <> 0) then
      ScanLines := $0307
    else if CurrentMode = 7 then
      ScanLines := $090C
    else
      ScanLines := $0507;
    SetCursorSize(Hi(ScanLines), Lo(ScanLines));
  end;

  procedure BlockCursor;
    {-Set scan lines for a block cursor}
  var
    EndLine : Byte;
  begin
    if (Hi(LastMode) <> 0) or (CurrentMode <> 7) then
      EndLine := $07
    else
      EndLine := $0C;
    SetCursorSize(0, EndLine);
  end;

  procedure HiddenCursor;
    {-Hide the cursor}
  begin
    SetCursorSize($20, 0);
  end;
  {$ENDIF}

  procedure SetCursorType(CT : CursorType);
    {-Sets the cursor shape to the specified type}
  begin
    case CT of
      cuNormal : NormalCursor;
      cuFat    : FatCursor;
      cuBlock  : BlockCursor;
      else       HiddenCursor;
    end;
  end;

  function ClassifyCursorType : CursorType;
    {-Classify cursor type based on scan lines}
  var
    CT : Word;
  begin
    CT := CursorTypeSL;
    if CT = $2000 then
      ClassifyCursorType := cuHidden
    else if Hi(LastMode) <> 0 then
      case CT of
        $0507 : ClassifyCursorType := cuNormal;
        $0307 : ClassifyCursorType := cuFat;
        $0007 : ClassifyCursorType := cuBlock;
        else    ClassifyCursorType := cuUnknown;
      end
    else if CurrentMode = 7 then
      case CT of
        $0B0C : ClassifyCursorType := cuNormal;
        $090C : ClassifyCursorType := cuFat;
        $000C : ClassifyCursorType := cuBlock;
        else    ClassifyCursorType := cuUnknown;
      end
    else
      case CT of
        $0607 : ClassifyCursorType := cuNormal;
        $0507 : ClassifyCursorType := cuFat;
        $0007 : ClassifyCursorType := cuBlock;
        else    ClassifyCursorType := cuUnknown;
      end;
  end;

  procedure GetCursorState(var XY, ScanLines : Word);
    {-Return the current position and size of the cursor}
  begin
    XY := WhereXY;
    ScanLines := CursorTypeSL;
  end;

  procedure RestoreCursorState(XY, ScanLines : Word);
    {-Reset the cursor to a position and size saved with GetCursorState}
  begin
    SetCursorSize(Hi(ScanLines), Lo(ScanLines));
    GotoXYAbs(Lo(XY), Hi(XY));
  end;

  procedure WhereXYdirect(var X, Y : Byte);
    {-Read the current position of the cursor directly from the CRT controller}
  {$IFDEF VIRTUALPASCAL}
    begin
      X := WhereX;
      Y := WhereY;
    end;
  {$ELSE}
  var
    CrtPort : Word; {absolute $40:$63;}                               {!!.20}
    CrtWidth : Word; {absolute $40:$4A;}                              {!!.20}
    CrtLen   : Word; {absolute $40:$4C;}                              {!!.20}
    XP, XY : Word;
  begin
    CrtPort := Word(Ptr(BiosDataSele, $63)^);                         {!!.20}
    CrtWidth := Word(Ptr(BiosDataSele, $4A)^);                        {!!.20}
    CrtLen := Word(Ptr(BiosDataSele, $4C)^);                          {!!.20}
    Port[CrtPort] := 14;
    XP := Port[CrtPort+1];
    Port[CrtPort] := 15;
    XY := ((XP shl 8)+Port[CrtPort+1]) mod (CrtLen shr 1);
    Y := Succ(XY div CrtWidth);
    X := Succ(XY mod CrtWidth);
  end;
  {$ENDIF}

  function EnhancedKbdInstalled : Boolean;
    {-Returns True if an enhanced keyboard is installed}
  begin
  {$IFDEF VIRTUALPASCAL}
    EnhancedKbdInstalled := (SysGetSystemSettings and 1) <> 0;
  {$ELSE}
    EnhancedKbdInstalled := (Byte(Ptr(BiosDataSele, $96)^) and $10 <> 0); {!!.20}
  {$ENDIF}
  end;

  procedure StuffKey(W : Word);
    {-Stuff one key into the keyboard buffer}
  {$IFDEF VIRTUALPASCAL}
    begin
      Unsupported;
    end;
  {$ELSE}
  const
    KbdStart = $1E;
    KbdEnd = $3C;
  var
    KbdHead : ^Word;{absolute $40 : $1A;}                             {!!.20}
    KbdTail : ^Word;{absolute $40 : $1C;}                             {!!.20}
    SaveKbdTail : Word;
  begin
    KbdHead := Ptr(BiosDataSele, $1A);                                {!!.20}
    KbdTail := Ptr(BiosDataSele, $1C);                                {!!.20}
    SaveKbdTail := KbdTail^;                                          {!!.20}
    if KbdTail^ = KbdEnd then                                         {!!.20}
      KbdTail^ := KbdStart                                            {!!.20}
    else
      Inc(KbdTail^, 2);                                               {!!.20}
    if KbdTail^ = KbdHead^ then                                       {!!.20}
      KbdTail^ := SaveKbdTail                                         {!!.20}
    else
      Word(Ptr(BiosDataSele, SaveKbdTail)^) := W;                     {!!.20}
  end;
  {$ENDIF}

  procedure StuffString(S : string);
    {-Stuff the contents of S into the keyboard buffer}
  var
    I : Byte;
  begin
    {allow at most 15 characters}
    if Length(S) > 15 then
      S[0] := #15;

    {stuff each key}
    for I := 1 to Length(S) do
      StuffKey(Ord(S[I]));
  end;

  function SaveWindow(XLow, YLow, XHigh, YHigh : Byte; Allocate : Boolean;
                      var Covers : Pointer) : Boolean;
    {-Allocate buffer space if requested and save window contents}
  var
    CoversP : BufPtr absolute Covers;
    WordsPerRow : Word;
    BufBytes : Word;
    SrcPos : Word;
    DestPos : Word;
    Row : Word;
  begin
    {assume success}
    SaveWindow := True;

    {compute number of words to move per row}
    WordsPerRow := Succ(XHigh-XLow);

    if Allocate then begin
      {compute bytes needed for screen buffer}
      BufBytes := (WordsPerRow*Succ(YHigh-YLow)) shl 1;

      {make sure enough memory is available}
      if MaxAvail < LongInt(BufBytes) then begin
        SaveWindow := False;
        Exit;
      end
      else
        {allocate the screen buffer}
        GetMem(CoversP, BufBytes);
    end;

    {save current contents to the screen buffer}
    DestPos := 0;
    SrcPos := (Pred(YLow)*VirtualWidth+Pred(XLow)) shl 1;
    for Row := YLow to YHigh do begin
    {$IFDEF VIRTUALPASCAL}
      MoveScreen(Mem[VirtualSegment+SrcPos], CoversP^[DestPos], WordsPerRow);
    {$ELSE}
      MoveScreen(Mem[VirtualSegment:SrcPos], CoversP^[DestPos], WordsPerRow);
    {$ENDIF}
      Inc(SrcPos, VirtualWidth shl 1);
      Inc(DestPos, WordsPerRow shl 1);
    end;
  end;

  procedure RestoreWindow(XLow, YLow, XHigh, YHigh : Byte;
                          Deallocate : Boolean; var Covers : Pointer);
    {-Restore screen contents and deallocate buffer space if requested}
  var
    CoversP : BufPtr absolute Covers;
    WordsPerRow : Word;
    SrcPos : Word;
    DestPos : Word;
    Row : Word;
  begin
    {compute number of words to move per row}
    WordsPerRow := Succ(XHigh-XLow);

    {Restore current contents to the screen buffer}
    DestPos := 0;
    SrcPos := (Pred(YLow)*VirtualWidth+Pred(XLow)) shl 1;
    for Row := YLow to YHigh do begin
    {$IFDEF VIRTUALPASCAL}
      MoveScreen(CoversP^[DestPos], Mem[VirtualSegment+SrcPos], WordsPerRow);
    {$ELSE}
      MoveScreen(CoversP^[DestPos], Mem[VirtualSegment:SrcPos], WordsPerRow);
    {$ENDIF}
      Inc(SrcPos, VirtualWidth shl 1);
      Inc(DestPos, WordsPerRow shl 1);
    end;

    {deallocate buffer space if requested}
    if Deallocate then begin
      FreeMem(CoversP, (WordsPerRow*Succ(YHigh-YLow)) shl 1);
      CoversP := nil;
    end;
  end;

  procedure ClearWindow(XLow, YLow, XHigh, YHigh : Word; Ch : Char; A : Byte);
    {-Clear a window using the specified character and attribute}
  var
    Row, Width : Word;
  begin
    Width := Succ(XHigh-XLow);
    for Row := YLow to YHigh do
      FastFill(Width, Ch, Row, XLow, A);
  end;

  procedure FrameWindow(LeftCol, TopRow, RightCol, BotRow : Word;
                        FAttr, HAttr : Byte; Header : string);
    {-Draws a frame around a window}
  var
    HeaderLen : Byte absolute Header;
    Row, Width, HeaderPos : Word;
  begin
    {calculate width of window}
    if RightCol <= LeftCol then     {!!.03}
      Width := 0                    {!!.03}
    else                            {!!.03}
      Width := RightCol-LeftCol-1;

    {draw the upper border}
    FastFill(1,     FrameChars[frTL], TopRow, LeftCol, FAttr);
    if Width > 0 then               {!!.03}
      FastFill(Width, FrameChars[frTT], TopRow, LeftCol+1, FAttr);
    FastFill(1,     FrameChars[frTR], TopRow, RightCol, FAttr);

    {draw the header}
    if HeaderLen > 0 then begin
      if HeaderLen > Width then
        HeaderLen := Width;
      HeaderPos := (Width-HeaderLen) shr 1;
      FastWrite(Header, TopRow, LeftCol+HeaderPos+1, HAttr);
    end;

    {draw the vertical bars}
    for Row := Succ(TopRow) to Pred(BotRow) do begin
      FastFill(1, FrameChars[frLL], Row, LeftCol, FAttr);
      FastFill(1, FrameChars[frRR], Row, RightCol, FAttr);
    end;

    {draw the bottom border}
    FastFill(1,     FrameChars[frBL], BotRow, LeftCol, FAttr);
    if Width > 0 then      {!!.03}
      FastFill(Width, FrameChars[frBB], BotRow, LeftCol+1, FAttr);
    FastFill(1,     FrameChars[frBR], BotRow, RightCol, FAttr);
  end;

  procedure SetBlink(IsOn : Boolean);
    {-Enable text mode attribute blinking if On is True}
  {$IFDEF DPMI32}
  Var
    DRegs : real_mode_call_structure_typ;
  {$ELSE}
  Const
    PortVal : array[0..4] of Byte = ($0C, $08, $0D, $09, $09);
  Var
    PortNum : Word;
    Index : Byte;
    PVal : Byte;
  {$ENDIF}

  begin
  {$IFDEF VIRTUALPASCAL}
  {$IFDEF DPMI32}
    FillChar (DRegs, SizeOf (DRegs), 0);
    DRegs. AX_ := $1003;
    DRegs. BL_ := Byte (IsOn);
    intr_RealMode (DRegs, $10);
  {$ENDIF}
  {$ELSE}
    case CurrentDisplay of
      MonoHerc :
        begin
          PortNum := $3B8;
          Index := 4;
        end;
      CGA :
        begin
          PortNum := $3D8;
          case LastMode of
            0..3 : Index := LastMode;
            else Exit;
          end;
        end;
      MCGA..VGA :
        begin
          inline(
            $8A/$5E/<IsOn/     {mov bl,[bp+<On]}
            $B8/$03/$10/     {mov ax,$1003}
            $CD/$10);        {int $10}
          Exit;
        end;
      else
        Exit;
    end;
    PVal := PortVal[Index];
    if IsOn then
      PVal := PVal or $20;
    Port[PortNum] := PVal;
  {$ENDIF}
  end;

  procedure SetCrtBorder(Attr : Byte); {!!.01}
    {-Set border to background color if card type and mode allow}
  begin
    {$IFDEF VIRTUALPASCAL}
    Unsupported;
    {$ELSE}inline(
      $8A/$5E/<Attr/         {mov BL,[BP+<Attr]  ;get attribute}
      $B1/$04/               {mov CL,4           ;shift count}
      $D2/$EB/               {shr BL,CL          ;get background color}
      $80/$E3/$0F/           {and BL,$0F         ;make sure it's valid}
      $8A/$26/>CurrentMode/  {mov AH,[>CurrentMode]    ;get current mode}
      $A0/>CurrentDisplay/   {mov AL,[>CurrentDisplay] ;get current display}
      $3C/$01/               {cmp AL,CGA         ;check for CGA}
      $75/$0D/               {jne ChkEga         ;nope, go check EGA}
      $80/$FC/$03/           {cmp AH,3           ;check for modes 0-3}
      $77/$3C/               {ja  Exit           ;forget it}
      $B4/$0B/               {mov AH,$0B         ;set color palette}
      $B7/$00/               {mov BH,$00         ;color palette ID}
      $CD/$10/               {int $10            ;let BIOS do it}
      $EB/$34/               {jmp short Exit     ;and get out}
                             {ChkEga:}
      $3C/$03/               {cmp AL,EGA         ;check for EGA/VGA}
      $72/$30/               {jb  Exit           ;forget it}
      $80/$FC/$03/           {cmp AH,3           ;check for modes 0-3}
      $77/$2B/               {ja  Exit           ;forget it}
                             {Check6:}
      $80/$FB/$06/           {cmp BL,6           ;brown is a special case}
      $75/$04/               {jne ChkEga2}
      $B3/$14/               {mov BL,$14         ;BL = $14}
      $EB/$1A/               {jmp short GoEga}
                             {ChkEga2:}
      $80/$FB/$08/           {cmp BL,$08         ;Is blink bit set?}
      $72/$15/               {jb  GoEga}
    {.$B8/$40/$00/}          {mov ax,$40         ;Is blinking enabled?} {!!.20}
      $A1/>BiosDataSele/                                                {!!.20}
      $8E/$C0/               {mov es,ax}
      $26/$F6/$06/>$65/$20/  {test es:[>$65],$20 ;bit 5 set if blinking on}
      $74/$05/               {jz  Intense        ;if it's on, add $30}
      $80/$E3/$07/           {and BL,$07         ;else, clear blink bit}
      $EB/$E0/               {jmp short Check6   ;and check for $06 again}
                             {Intense:}
      $80/$C3/$30/           {add BL,$30         ;select high-intensity}
                             {GoEga:}
      $B4/$10/               {mov AH,$10         ;set color palette}
      $B0/$01/               {mov AL,$01         ;set border color}
      $88/$DF/               {mov BH,BL          ;the color}
      $CD/$10);              {int $10            ;let BIOS do it}
                             {Exit:}
    {$ENDIF}
  end;

  function MapColor(C : Byte) : Byte;
    {-Map a video attribute for visibility on mono/bw displays}
  begin
    if UseColor then
      MapColor := C
    else
      MapColor := ColorMap[C];
  end;

  function UseColor : Boolean;
    {-Returns True if color video attributes should be used}
  begin
    if (DefColorChoice <> UseDefault) then
      UseColor := (DefColorChoice = ForceColor)
    else case CurrentMode of
      0, 2 :
        UseColor := False;
      7 :
        UseColor := (WhichHerc = HercInColor);
      else
        UseColor := True;
    end;
  end;

  function ColorMono(Color, Mono : Byte) : Byte;
    {-Choose between a color and a monochrome video attribute}
  begin
    if UseColor then
      ColorMono := Color
    else
      ColorMono := Mono;
  end;

  function MapMono(Color, Mono : Byte) : Byte;
    {-Return a mapped video attribute if Mono = $FF}
  begin
    if Mono = AutoMapMono then
      MapMono := ColorMap[Color]
    else
      MapMono := Mono;
  end;

  procedure StoreWindowCoordinates(var WC : WindowCoordinates);
    {-Store the window coordinates for the active window}
  type
    XY = record
           X, Y : Byte;
         end;
  begin
    with WC do begin
      XL := Succ(XY(WindMin).X);
      YL := Succ(XY(WindMin).Y);
      XH := Succ(XY(WindMax).X);
      YH := Succ(XY(WindMax).Y);
    end;
  end;

  procedure RestoreWindowCoordinates(WC : WindowCoordinates);
    {-Restore previously saved window coordinates}
  begin
    with WC do
      Window(XL, YL, XH, YH);
  end;

  procedure ColorSet.SetTextAttr(Color, Mono : Byte);
    {-Set attribute for ordinary text}
  begin
    TextColor := Color;
    TextMono := MapMono(Color, Mono);
  end;

  procedure ColorSet.SetCtrlAttr(Color, Mono : Byte);
    {-Set attribute for control characters}
  begin
    CtrlColor := Color;
    CtrlMono := MapMono(Color, Mono);
  end;

  procedure ColorSet.SetFrameAttr(Color, Mono : Byte);
    {-Set attribute for frames}
  begin
    FrameColor := Color;
    FrameMono := MapMono(Color, Mono);
  end;

  procedure ColorSet.SetHeaderAttr(Color, Mono : Byte);
    {-Set attribute for headers}
  begin
    HeaderColor := Color;
    HeaderMono := MapMono(Color, Mono);
  end;

  procedure ColorSet.SetShadowAttr(Color, Mono : Byte);
    {-Set attribute for shadows}
  begin
    ShadowColor := Color;
    ShadowMono := MapMono(Color, Mono);
  end;

  procedure ColorSet.SetHighlightAttr(Color, Mono : Byte);
    {-Set attribute for highlighted text}
  begin
    HighlightColor := Color;
    HighlightMono := MapMono(Color, Mono);
  end;

  procedure ColorSet.SetPromptAttr(Color, Mono : Byte);
    {-Set attribute for prompts}
  begin
    PromptColor := Color;
    PromptMono := MapMono(Color, Mono);
  end;

  procedure ColorSet.SetSelectedPromptAttr(Color, Mono : Byte);
    {-Set attribute for prompts}
  begin
    SelPromptColor := Color;
    SelPromptMono := MapMono(Color, Mono);
  end;

  procedure ColorSet.SetProtectedPromptAttr(Color, Mono : Byte);
    {-Set attribute for protected prompts}
  begin
    ProPromptColor := Color;
    ProPromptMono := MapMono(Color, Mono);
  end;

  procedure ColorSet.SetFieldAttr(Color, Mono : Byte);
    {-Set attribute for unselected fields}
  begin
    FieldColor := Color;
    FieldMono := MapMono(Color, Mono);
  end;

  procedure ColorSet.SetSelectedFieldAttr(Color, Mono : Byte);
    {-Set attribute for selected fields}
  begin
    SelFieldColor := Color;
    SelFieldMono := MapMono(Color, Mono);
  end;

  procedure ColorSet.SetProtectedFieldAttr(Color, Mono : Byte);
    {-Set attribute for protected/disabled fields}
  begin
    ProFieldColor := Color;
    ProFieldMono := MapMono(Color, Mono);
  end;

  procedure ColorSet.SetScrollBarAttr(Color, Mono : Byte);
    {-Set attribute for scroll bars}
  begin
    ScrollBarColor := Color;
    ScrollBarMono := MapMono(Color, Mono);
  end;

  procedure ColorSet.SetSliderAttr(Color, Mono : Byte);
    {-Set attribute for sliders in scroll bars}
  begin
    SliderColor := Color;
    SliderMono := MapMono(Color, Mono);
  end;

  procedure ColorSet.SetHotSpotAttr(Color, Mono : Byte);
    {-Set attribute for hot spots in frame corners}
  begin
    HotSpotColor := Color;
    HotSpotMono := MapMono(Color, Mono);
  end;

  procedure ColorSet.SetBlockAttr(Color, Mono : Byte);
    {-Set attribute for marked blocks}
  begin
    BlockColor := Color;
    BlockMono := MapMono(Color, Mono);
  end;

  procedure ColorSet.SetMarkerAttr(Color, Mono : Byte);
    {-Set attribute for text markers}
  begin
    MarkerColor := Color;
    MarkerMono := MapMono(Color, Mono);
  end;

  procedure ColorSet.SetDelimAttr(Color, Mono : Byte);
    {-Set attribute for unselected delimiters}
  begin
    DelimColor := Color;
    DelimMono := MapMono(Color, Mono);
  end;

  procedure ColorSet.SetSelectedDelimAttr(Color, Mono : Byte);
    {-Set attribute for selected delimiters}
  begin
    SelDelimColor := Color;
    SelDelimMono := MapMono(Color, Mono);
  end;

  procedure ColorSet.SetProtectedDelimAttr(Color, Mono : Byte);
    {-Set attribute for protected/disabled delimiters}
  begin
    ProDelimColor := Color;
    ProDelimMono := MapMono(Color, Mono);
  end;

  procedure ColorSet.SetSelectedItemAttr(Color, Mono : Byte);
    {-Set attribute for selected pick and menu items}
  begin
    SelItemColor := Color;
    SelItemMono := MapMono(Color, Mono);
  end;

  procedure ColorSet.SetProtectedItemAttr(Color, Mono : Byte);
    {-Set attribute for protected pick and menu items}
  begin
    ProItemColor := Color;
    ProItemMono := MapMono(Color, Mono);
  end;

  procedure ColorSet.SetHighlightItemAttr(Color, Mono : Byte);
    {-Set attribute for protected pick and menu items}
  begin
    HighItemColor := Color;
    HighItemMono := MapMono(Color, Mono);
  end;

  procedure ColorSet.SetAltItemAttr(Color, Mono : Byte);    {!!.01}
    {-Set attributes for alternate pick items}
  begin
    AltItemColor := Color;
    AltItemMono := MapMono(Color, Mono);
  end;

  procedure ColorSet.SetAltSelItemAttr(Color, Mono : Byte); {!!.01}
    {-Set attributes for alternate selected pick items}
  begin
    AltSelItemColor := Color;
    AltSelItemMono := MapMono(Color, Mono);
  end;

  procedure ColorSet.SetFlexAHelpAttr(Color, Mono : Byte);
    {-Set attribute for help text surrounded by ^A}
  begin
    FlexAHelpColor := Color;
    FlexAHelpMono := MapMono(Color, Mono);
  end;

  procedure ColorSet.SetFlexBHelpAttr(Color, Mono : Byte);
    {-Set attribute for help text surrounded by ^B}
  begin
    FlexBHelpColor := Color;
    FlexBHelpMono := MapMono(Color, Mono);
  end;

  procedure ColorSet.SetFlexCHelpAttr(Color, Mono : Byte);
    {-Set attribute for help text surrounded by ^C}
  begin
    FlexCHelpColor := Color;
    FlexCHelpMono := MapMono(Color, Mono);
  end;

  procedure ColorSet.SetUnselXrefAttr(Color, Mono : Byte);
    {-Set attribute for unselected help cross-references}
  begin
    UnselXrefColor := Color;
    UnselXrefMono := MapMono(Color, Mono);
  end;

  procedure ColorSet.SetSelXrefAttr(Color, Mono : Byte);
    {-Set attribute for selected help cross-references}
  begin
    SelXrefColor := Color;
    SelXrefMono := MapMono(Color, Mono);
  end;

  procedure ColorSet.SetMouseAttr(Color, Mono : Byte);
    {-Set attribute for mouse cursor}
  begin
    MouseColor := Color;
    MouseMono := MapMono(Color, Mono);
  end;

{$IFDEF UseCrt}
{$IFNDEF VIRTUALPASCAL}

(*
  procedure PatchCrt;
    {-Patch the CRT unit so that all calls are directed to us}
  type
    PatchRec =
      record
        JmpFar : Byte;
        Addr : Pointer;
      end;

    procedure Patch(CrtAddr, OpCrtAddr : Pointer);
      {-Patch in a long jump to OpCrtAddr at CrtAddr}
    begin
      {$IFDEF DPMI}                                     {!!.21}
      {Get a writeable alias for the CRT code segment}  {!!.21}
      inc(OS(CrtAddr).S, SelectorInc);                  {!!.21}
      {$ENDIF}                                          {!!.21}
      with PatchRec(CrtAddr^) do begin
        JmpFar := $EA;
        Addr := OpCrtAddr;
      end;
    end;

  begin
    Patch(@Crt.AssignCrt,      @OpCrt.AssignCrt);
    Patch(@Crt.ReadKey,        @OpCrt.ReadKey);
    Patch(@Crt.TextMode,       @OpCrt.TextMode);
    Patch(@Crt.Window,         @OpCrt.Window);
    Patch(@Crt.GotoXy,         @OpCrt.GotoXy);
    Patch(@Crt.WhereX,         @OpCrt.WhereX);
    Patch(@Crt.WhereY,         @OpCrt.WhereY);
    Patch(@Crt.ClrScr,         @OpCrt.ClrScr);
    Patch(@Crt.ClrEol,         @OpCrt.ClrEol);
    Patch(@Crt.InsLine,        @OpCrt.InsLine);
    Patch(@Crt.DelLine,        @OpCrt.DelLine);
    Patch(@Crt.TextColor,      @OpCrt.TextColor);
    Patch(@Crt.TextBackground, @OpCrt.TextBackground);
    Patch(@Crt.LowVideo,       @OpCrt.LowVideo);
    Patch(@Crt.NormVideo,      @OpCrt.NormVideo);
    Patch(@Crt.Delay,          @OpCrt.Delay);
    Patch(@Crt.KeyPressed,     @OpCrt.KeyPressed);
    Patch(@Crt.Sound,          @OpCrt.Sound);
    Patch(@Crt.NoSound,        @OpCrt.NoSound);
  end;
*)
{$ENDIF}
{$ELSE}

  procedure FindSystemSeg;
    {-Find the segment for SYSTEM}
    {!!.21 rewritten for compatibility with pmode}
  var
    F : Text;
  begin
    {$IFDEF Dpmi}
    if PrefixSeg = 0 then begin  {in a DLL, need special handling}
      {reopen Input}
      AssignCrt(Input);
      Reset(Input);
    end;
    {$ENDIF}
    {try to detect presence of CRT in the same program}
    Assign(F, '');
    {$IFDEF VIRTUALPASCAL}
    If TextRec(F).OpenFunc <> TextRec(Input).OpenFunc then
    {$ELSE}
    SystemSeg := OS(TextRec(F).OpenFunc).S;

    if OS(TextRec(Input).OpenFunc).S <> SystemSeg then
    {$ENDIF}
      CrtTest;
  end;

{$ENDIF}

{$IFNDEF VIRTUALPASCAL}
  {$F+}
  procedure OurExitProc;
    {-Exit handler}
  begin
    {restore previous exit handler}
    ExitProc := SaveExitProc;

    {restore ^C checking to its original state}
    SetCBreak(SaveCtrlC);
  end;
{$ENDIF}

  procedure Break; {!!.22}
    {-Halt if break pressed (called from ASM)}
  begin
    Halt(255);
  end;

begin
  InitMemorySegments;  {initialize important pointers/selectors}      {!!.20}

  {$IFDEF UseCrt}
    {patch the entry points in the CRT unit}
    {$IFNDEF VIRTUALPASCAL}
    PatchCrt;
    {$ENDIF}
  {$ELSE}
    {find the segment for SYSTEM}
    FindSystemSeg;
  {$ENDIF}

  {initialize global variables}
  CheckBreak := True;
  CheckEOF := False;

  {$IFNDEF VIRTUALPASCAL}
  {save ^C checking state and turn it off}
  GetCBreak(SaveCtrlC);
  SetCBreak(False);

  {install exit handler}
  SaveExitProc := ExitProc;
  ExitProc := @OurExitProc;

  {for internal use}
  IsCompaq := (CompaqBiosName = 'COMPAQ');
  if (ZenithBiosName = 'Zenith') then
    IsZenith := True;
  {$ENDIF}

  {initialize internal variables}
  InitCrt;

  {$IFNDEF VIRTUALPASCAL}
  {make sure sound chip is initialized properly}             {!!.13}
  inline(                                                    {!!.13}
    $E4/$61/   {IN  AL,$61 ;Get current value of port $61}   {!!.13}
    $24/$FC/   {AND AL,$FC ;Turn off bits 0 and 1}           {!!.13}
    $E6/$61);  {OUT $61,AL ;Reset the port}                  {!!.13}

  {activate colors on Hercules InColor card}
  if WhichHerc = HercInColor then
    SwitchInColorCard(True);
  {$ENDIF}

  {reopen Input}
  AssignCrt(Input);
  Reset(Input);

  {reopen Output}
  AssignCrt(Output);
  Rewrite(Output);

  {$IFNDEF VIRTUALPASCAL}
  {correct some BIOS bugs involving cursor scan lines}
  case BiosScanLines^ of                                        {!!.20}
    $0607 :
      if CurrentMode = 7 then
        {mono adapter, but CGA scan lines -- happens on most mono systems}
        BiosScanLines^ := $0B0C;                                {!!.20}
    $0067 :
      {incorrect scan lines bug -- most often seen on Compaqs}
      BiosScanLines^ := $0607;                                  {!!.20}
  end;
  {$ENDIF}

  {$IFDEF VIRTUALPASCAL}
  InitCursor;
  {$ENDIF}
end.
