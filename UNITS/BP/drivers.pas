
{*******************************************************}
{                                                       }
{       Turbo Pascal Version 7.0                        }
{       Turbo Vision Unit                               }
{                                                       }
{       Copyright (c) 1992 Borland International        }
{                                                       }
{*******************************************************}

unit Drivers;

{$X+,I-,S-,P-}
{$C FIXED PRELOAD PERMANENT}

interface

uses Objects;

{ ******** EVENT MANAGER ******** }

const

{ Event codes }

  evMouseDown = $0001;
  evMouseUp   = $0002;
  evMouseMove = $0004;
  evMouseAuto = $0008;
  evKeyDown   = $0010;
  evCommand   = $0100;
  evBroadcast = $0200;

{ Event masks }

  evNothing   = $0000;
  evMouse     = $000F;
  evKeyboard  = $0010;
  evMessage   = $FF00;

{ Extended key codes }

  kbEsc       = $011B;  kbAltSpace  = $0200;  kbCtrlIns   = $0400;
  kbShiftIns  = $0500;  kbCtrlDel   = $0600;  kbShiftDel  = $0700;
  kbBack      = $0E08;  kbCtrlBack  = $0E7F;  kbShiftTab  = $0F00;
  kbTab       = $0F09;  kbAltQ      = $1000;  kbAltW      = $1100;
  kbAltE      = $1200;  kbAltR      = $1300;  kbAltT      = $1400;
  kbAltY      = $1500;  kbAltU      = $1600;  kbAltI      = $1700;
  kbAltO      = $1800;  kbAltP      = $1900;  kbCtrlEnter = $1C0A;
  kbEnter     = $1C0D;  kbAltA      = $1E00;  kbAltS      = $1F00;
  kbAltD      = $2000;  kbAltF      = $2100;  kbAltG      = $2200;
  kbAltH      = $2300;  kbAltJ      = $2400;  kbAltK      = $2500;
  kbAltL      = $2600;  kbAltZ      = $2C00;  kbAltX      = $2D00;
  kbAltC      = $2E00;  kbAltV      = $2F00;  kbAltB      = $3000;
  kbAltN      = $3100;  kbAltM      = $3200;  kbF1        = $3B00;
  kbF2        = $3C00;  kbF3        = $3D00;  kbF4        = $3E00;
  kbF5        = $3F00;  kbF6        = $4000;  kbF7        = $4100;
  kbF8        = $4200;  kbF9        = $4300;  kbF10       = $4400;
  kbHome      = $4700;  kbUp        = $4800;  kbPgUp      = $4900;
  kbGrayMinus = $4A2D;  kbLeft      = $4B00;  kbRight     = $4D00;
  kbGrayPlus  = $4E2B;  kbEnd       = $4F00;  kbDown      = $5000;
  kbPgDn      = $5100;  kbIns       = $5200;  kbDel       = $5300;
  kbShiftF1   = $5400;  kbShiftF2   = $5500;  kbShiftF3   = $5600;
  kbShiftF4   = $5700;  kbShiftF5   = $5800;  kbShiftF6   = $5900;
  kbShiftF7   = $5A00;  kbShiftF8   = $5B00;  kbShiftF9   = $5C00;
  kbShiftF10  = $5D00;  kbCtrlF1    = $5E00;  kbCtrlF2    = $5F00;
  kbCtrlF3    = $6000;  kbCtrlF4    = $6100;  kbCtrlF5    = $6200;
  kbCtrlF6    = $6300;  kbCtrlF7    = $6400;  kbCtrlF8    = $6500;
  kbCtrlF9    = $6600;  kbCtrlF10   = $6700;  kbAltF1     = $6800;
  kbAltF2     = $6900;  kbAltF3     = $6A00;  kbAltF4     = $6B00;
  kbAltF5     = $6C00;  kbAltF6     = $6D00;  kbAltF7     = $6E00;
  kbAltF8     = $6F00;  kbAltF9     = $7000;  kbAltF10    = $7100;
  kbCtrlPrtSc = $7200;  kbCtrlLeft  = $7300;  kbCtrlRight = $7400;
  kbCtrlEnd   = $7500;  kbCtrlPgDn  = $7600;  kbCtrlHome  = $7700;
  kbAlt1      = $7800;  kbAlt2      = $7900;  kbAlt3      = $7A00;
  kbAlt4      = $7B00;  kbAlt5      = $7C00;  kbAlt6      = $7D00;
  kbAlt7      = $7E00;  kbAlt8      = $7F00;  kbAlt9      = $8000;
  kbAlt0      = $8100;  kbAltMinus  = $8200;  kbAltEqual  = $8300;
  kbCtrlPgUp  = $8400;  kbAltBack   = $0800;  kbNoKey     = $0000;

{ Keyboard state and shift masks }

  kbRightShift  = $0001;
  kbLeftShift   = $0002;
  kbCtrlShift   = $0004;
  kbAltShift    = $0008;
  kbScrollState = $0010;
  kbNumState    = $0020;
  kbCapsState   = $0040;
  kbInsState    = $0080;

{ Mouse button state masks }

  mbLeftButton  = $01;
  mbRightButton = $02;

type

{ Event record }

  PEvent = ^TEvent;
  TEvent = record
    What: Word;
    case Word of
      evNothing: ();
      evMouse: (
        Buttons: Byte;
        Double: Boolean;
        Where: TPoint);
      evKeyDown: (
        case Integer of
	  0: (KeyCode: Word);
          1: (CharCode: Char;
              ScanCode: Byte));
      evMessage: (
        Command: Word;
        case Word of
          0: (InfoPtr: Pointer);
          1: (InfoLong: Longint);
          2: (InfoWord: Word);
          3: (InfoInt: Integer);
          4: (InfoByte: Byte);
          5: (InfoChar: Char));
  end;

const

{ Initialized variables }

  ButtonCount: Byte = 0;
  MouseEvents: Boolean = False;
  MouseReverse: Boolean = False;
  DoubleDelay: Word = 8;
  RepeatDelay: Word = 8;

var

{ Uninitialized variables }

  MouseIntFlag: Byte;
  MouseButtons: Byte;
  MouseWhere: TPoint;

{ Event manager routines }

procedure InitEvents;
procedure DoneEvents;
procedure ShowMouse;
procedure HideMouse;
procedure GetMouseEvent(var Event: TEvent);
procedure GetKeyEvent(var Event: TEvent);
function GetShiftState: Byte;

{ ******** SCREEN MANAGER ******** }

const

{ Screen modes }

  smBW80    = $0002;
  smCO80    = $0003;
  smMono    = $0007;
  smFont8x8 = $0100;

const

{ Initialized variables }

  StartupMode: Word = $FFFF;

var

{ Uninitialized variables }

  ScreenMode: Word;
  ScreenWidth: Byte;
  ScreenHeight: Byte;
  HiResScreen: Boolean;
  CheckSnow: Boolean;
  ScreenBuffer: Pointer;
  CursorLines: Word;

{ Screen manager routines }

procedure InitVideo;
procedure DoneVideo;
procedure SetVideoMode(Mode: Word);
procedure ClearScreen;

{ ******** SYSTEM ERROR HANDLER ******** }

type

{ System error handler function type }

  TSysErrorFunc = function(ErrorCode: Integer; Drive: Byte): Integer;

{ Default system error handler routine }

function SystemError(ErrorCode: Integer; Drive: Byte): Integer;

const

{ Initialized variables }

  SaveInt09: Pointer = nil;
  SysErrorFunc: TSysErrorFunc = SystemError;
  SysColorAttr: Word = $4E4F;
  SysMonoAttr: Word = $7070;
  CtrlBreakHit: Boolean = False;
  SaveCtrlBreak: Boolean = False;
  SysErrActive: Boolean = False;
  FailSysErrors: Boolean = False;

{ System error handler routines }

procedure InitSysError;
procedure DoneSysError;

{ ******** UTILITY ROUTINES ******** }

{ Keyboard support routines }

function GetAltChar(KeyCode: Word): Char;
function GetAltCode(Ch: Char): Word;
function GetCtrlChar(KeyCode: Word): Char;
function GetCtrlCode(Ch: Char): Word;
function CtrlToArrow(KeyCode: Word): Word;

{ String routines }

procedure FormatStr(var Result: String; const Format: String; var Params);
procedure PrintStr(const S: String);

{ Buffer move routines }

procedure MoveBuf(var Dest; var Source; Attr: Byte; Count: Word);
procedure MoveChar(var Dest; C: Char; Attr: Byte; Count: Word);
procedure MoveCStr(var Dest; const Str: String; Attrs: Word);
procedure MoveStr(var Dest; const Str: String; Attr: Byte);
function CStrLen(const S: String): Integer;

implementation

{ ******** EVENT MANAGER ******** }

const

{ Event manager constants }

  EventQSize = 16;

var

{ Event manager variables }

  LastButtons: Byte;
  DownButtons: Byte;
  LastDouble: Boolean;
  LastWhere: TPoint;
  DownWhere: TPoint;
  DownTicks: Word;
  AutoTicks: Word;
  AutoDelay: Word;
  EventCount: Word;
  EventQHead: Word;
  EventQTail: Word;
  EventQueue: array[0..EventQSize - 1] of TEvent;
  EventQLast: record end;

var
  ShiftState: Byte absolute $40:$17;
  Ticks: Word absolute $40:$6C;

{ Detect mouse driver }

procedure DetectMouse; near; assembler;
asm
	MOV	AX,3533H
	INT	21H
	MOV	AX,ES
	OR	AX,BX
	JE	@@1
	XOR	AX,AX
	INT	33H
	OR	AX,AX
	JE	@@1
	PUSH	BX
	MOV	AX,4
	XOR	CX,CX
	XOR	DX,DX
	INT	33H
	POP	AX
@@1:	MOV	ButtonCount,AL
end;

{ Store event in GetMouseEvent and GetKeyEvent }

procedure StoreEvent; near; assembler;
asm
	MOV	DI,SP
	LES	DI,SS:[DI+8]
	CLD
	STOSW
	XCHG	AX,BX
	STOSW
	XCHG	AX,CX
	STOSW
	XCHG	AX,DX
	STOSW
end;

{ Get mouse state }
{ Out	BL = Button mask }
{	CX = X coordinate }
{	DX = Y coordinate }
{	DI = Timer ticks }

procedure GetMouseState; near; assembler;
asm
	CLI
	CMP	EventCount,0
	JNE	@@1
	MOV	BL,MouseButtons
	MOV	CX,MouseWhere.Word[0]
	MOV	DX,MouseWhere.Word[2]
	MOV	ES,Seg0040
	MOV	DI,ES:Ticks
	JMP	@@3
@@1:	MOV	SI,EventQHead
	CLD
	LODSW
	XCHG	AX,DI
	LODSW
	XCHG	AX,BX
	LODSW
	XCHG	AX,CX
	LODSW
	XCHG	AX,DX
	CMP	SI,OFFSET EventQLast
	JNE	@@2
	MOV	SI,OFFSET EventQueue
@@2:	MOV	EventQHead,SI
	DEC	EventCount
@@3:	STI
	CMP	MouseReverse,0
	JE	@@4
	MOV	BH,BL
	AND	BH,3
	JE	@@4
	CMP	BH,3
	JE	@@4
	XOR	BL,3
@@4:
end;

procedure MouseInt; far; assembler;
asm
	MOV	SI,SEG @DATA
	MOV	DS,SI
	MOV	SI,CX
	MOV	CL,3
	SHR	SI,CL
	SHR	DX,CL
	MOV	MouseButtons,BL
	MOV	MouseWhere.X,SI
	MOV	MouseWhere.Y,DX
	TEST	AX,11110B
	JE	@@2
	CMP	EventCount,EventQSize
	JE	@@2
	MOV	ES,Seg0040
	MOV	AX,ES:Ticks
	MOV	DI,EventQTail
	PUSH	DS
	POP	ES
	CLD
	STOSW
	XCHG	AX,BX
	STOSW
	XCHG	AX,SI
	STOSW
	XCHG	AX,DX
	STOSW
	CMP	DI,OFFSET EventQLast
	JNE	@@1
	MOV	DI,OFFSET EventQueue
@@1:	MOV	EventQTail,DI
	INC	EventCount
@@2:	MOV	MouseIntFlag,1
end;

procedure InitEvents; assembler;
asm
	XOR	AX,AX
	CMP	AL,ButtonCount
	JE	@@1
	MOV	DownButtons,AL
	MOV	LastDouble,AL
	MOV	EventCount,AX
	MOV	AX,OFFSET DS:EventQueue
	MOV	EventQHead,AX
	MOV	EventQTail,AX
	MOV	AX,3
	INT	33H
	XCHG	AX,CX
	MOV	CL,3
	SHR	AX,CL
	SHR	DX,CL
	MOV	MouseButtons,BL
	MOV	MouseWhere.X,AX
	MOV	MouseWhere.Y,DX
	MOV	LastButtons,BL
	MOV	LastWhere.X,AX
	MOV	LastWhere.Y,DX
	MOV	AX,12
	MOV	CX,0FFFFH
	MOV	DX,OFFSET CS:MouseInt
	PUSH	CS
	POP	ES
	INT	33H
        MOV     AX,1
        INT     33H
	MOV	MouseEvents,1
@@1:
end;

procedure DoneEvents; assembler;
asm
	CMP	ButtonCount,0
	JE	@@1
	CMP	MouseEvents,0
	JE	@@1
	MOV	MouseEvents,0
        MOV     AX,2
        INT     33H
	MOV	AX,12
	XOR	CX,CX
	MOV	DX,CX
	MOV	ES,CX
	INT	33H
@@1:
end;

procedure ShowMouse; assembler;
asm
	CMP	ButtonCount,0
	JE	@@1
	PUSH	AX
	MOV	AX,1
	INT	33H
	POP	AX
@@1:
end;

procedure HideMouse; assembler;
asm
	CMP	ButtonCount,0
	JE	@@1
	PUSH	AX
	MOV	AX,2
	INT	33H
	POP	AX
@@1:
end;

procedure GetMouseEvent(var Event: TEvent); assembler;
asm
	CMP	MouseEvents,0
	JE	@@2
	CALL	GetMouseState
	MOV	BH,LastDouble
	MOV	AL,LastButtons
	CMP	AL,BL
	JE	@@1
	OR	AL,AL
	JE	@@3
	OR	BL,BL
	JE	@@5
	MOV	BL,AL
@@1:	CMP	CX,LastWhere.X
	JNE	@@6
	CMP	DX,LastWhere.Y
	JNE	@@6
	OR	BL,BL
	JE	@@2
	MOV	AX,DI
	SUB	AX,AutoTicks
	CMP	AX,AutoDelay
	JAE	@@7
@@2:	XOR	AX,AX
	MOV	BX,AX
	MOV	CX,AX
	MOV	DX,AX
	JMP	@@9
@@3:	MOV	BH,0
	CMP	BL,DownButtons
	JNE	@@4
	CMP	CX,DownWhere.X
	JNE	@@4
	CMP	DX,DownWhere.Y
	JNE	@@4
	MOV	AX,DI
	SUB	AX,DownTicks
	CMP	AX,DoubleDelay
	JAE	@@4
	MOV	BH,1
@@4:	MOV	DownButtons,BL
	MOV	DownWhere.X,CX
	MOV	DownWhere.Y,DX
	MOV	DownTicks,DI
	MOV	AutoTicks,DI
	MOV	AX,RepeatDelay
	MOV	AutoDelay,AX
	MOV	AX,evMouseDown
	JMP	@@8
@@5:	MOV	AX,evMouseUp
	JMP	@@8
@@6:	MOV	AX,evMouseMove
	JMP	@@8
@@7:	MOV	AutoTicks,DI
	MOV	AutoDelay,1
	MOV	AX,evMouseAuto
@@8:    MOV	LastButtons,BL
	MOV	LastDouble,BH
	MOV	LastWhere.X,CX
	MOV	LastWhere.Y,DX
@@9:	CALL	StoreEvent
end;

procedure GetKeyEvent(var Event: TEvent); assembler;
asm
	MOV	AH,1
	INT	16H
	MOV	AX,0
	MOV	BX,AX
	JE	@@1
	MOV	AH,0
	INT	16H
	XCHG	AX,BX
	MOV	AX,evKeyDown
@@1:	XOR	CX,CX
	MOV	DX,CX
	CALL	StoreEvent
end;

function GetShiftState: Byte; assembler;
asm
	MOV	ES,Seg0040
	MOV	AL,ES:ShiftState
end;

{ ******** SCREEN MANAGER ******** }

var
  Equipment: Word absolute $40:$10;
  CrtRows: Byte absolute $40:$84;
  CrtInfo: Byte absolute $40:$87;

{ Save registers and call video interrupt }

procedure VideoInt; near; assembler;
asm
	PUSH	BP
	PUSH	ES
	INT	10H
	POP	ES
	POP	BP
end;

{ Return CRT mode in AX and dimensions in DX }

procedure GetCrtMode; near; assembler;
asm
	MOV	AH,0FH
	CALL	VideoInt
	PUSH	AX
	MOV	AX,1130H
	MOV	BH,0
	MOV	DL,0
	CALL	VideoInt
	POP	AX
	MOV	DH,AH
	CMP	DL,25
	SBB	AH,AH
	INC	AH
end;

{ Set CRT mode to value in AX }

procedure SetCrtMode; near; assembler;
asm
	MOV	ES,Seg0040
	MOV	BL,20H
	CMP	AL,smMono
	JNE	@@1
	MOV	BL,30H
@@1:	AND	ES:Equipment.Byte,0CFH
	OR	ES:Equipment.Byte,BL
	AND	ES:CrtInfo,0FEH
	PUSH	AX
	MOV	AH,0
	CALL	VideoInt
	POP	AX
	OR	AH,AH
	JE	@@2
	MOV	AX,1112H
	MOV	BL,0
	CALL	VideoInt
	MOV	AX,1130H
	MOV	BH,0
	MOV	DL,0
	CALL	VideoInt
	CMP	DL,42
	JNE	@@2
	OR	ES:CrtInfo,1
	MOV	AH,1
	MOV	CX,600H
	CALL	VideoInt
	MOV	AH,12H
	MOV	BL,20H
	CALL	VideoInt
@@2:
end;

{ Fix CRT mode in AX if required }

procedure FixCrtMode; near; assembler;
asm
	CMP	AL,smMono
	JE	@@1
	CMP	AL,smCO80
	JE	@@1
	CMP	AL,smBW80
	JE	@@1
	MOV	AX,smCO80
@@1:
end;

{ Set CRT data areas and mouse range }

procedure SetCrtData; near; assembler;
asm
	CALL	GetCrtMode
	MOV	CL,1
	OR	DL,DL
	JNE	@@1
	MOV	CL,0
	MOV	DL,24
@@1:	INC	DL
	MOV	ScreenMode,AX
	MOV	ScreenWidth,DH
	MOV	ScreenHeight,DL
	MOV	HiResScreen,CL
	XOR	CL,1
	MOV	BX,SegB800
	CMP	AL,smMono
	JNE	@@2
	MOV	CL,0
	MOV	BX,SegB000
@@2:	MOV	CheckSnow,CL
	XOR	AX,AX
	MOV	ScreenBuffer.Word[0],AX
	MOV	ScreenBuffer.Word[2],BX
	MOV	AH,3
	MOV	BH,0
	CALL	VideoInt
	MOV	CursorLines,CX
	MOV	AH,1
	MOV	CX,2000H
	CALL	VideoInt
	CMP	ButtonCount,0
	JE	@@4
	MOV	AX,7
	MOV	DL,ScreenWidth
	CALL	@@3
	MOV	AX,8
	MOV	DL,ScreenHeight
@@3:	XOR	DH,DH
	MOV	CL,3
	SHL	DX,CL
	DEC	DX
	XOR	CX,CX
	INT	33H
@@4:
end;

{ Detect video modes }

procedure DetectVideo; assembler;
asm
	CALL	GetCrtMode
	CALL	FixCrtMode
	MOV	ScreenMode,AX
end;

procedure InitVideo; assembler;
asm
	CALL	GetCrtMode
	MOV	StartupMode,AX
	CMP	AX,ScreenMode
	JE	@@1
	MOV	AX,ScreenMode
	CALL	SetCrtMode
@@1:	CALL	SetCrtData
end;

procedure DoneVideo; assembler;
asm
	MOV	AX,StartupMode
	CMP	AX,0FFFFH
	JE	@@2
	CMP	AX,ScreenMode
	JE	@@1
	CALL	SetCrtMode
	JMP	@@2
@@1:	CALL	ClearScreen
	MOV	AH,1
	MOV	CX,CursorLines
	CALL	VideoInt
@@2:
end;

procedure SetVideoMode(Mode: Word); assembler;
asm
	MOV	AX,Mode
	CALL	FixCrtMode
	CALL	SetCrtMode
	CALL	SetCrtData
end;

procedure ClearScreen; assembler;
asm
	MOV	AX,600H
	MOV	BH,07H
	XOR	CX,CX
	MOV	DL,ScreenWidth
	DEC	DL
	MOV	DH,ScreenHeight
	DEC	DH
	CALL	VideoInt
	MOV	AH,2
	MOV	BH,0
	XOR	DX,DX
	CALL	VideoInt
end;

{ ******** SYSTEM ERROR HANDLER ******** }

{$IFDEF DPMI}
{$L SYSINT.OBP}
{$ELSE}
{$L SYSINT.OBJ}
{$ENDIF}

const

{ System error messages }

  SCriticalError:  string[31] = 'Critical disk error on drive %c';
  SWriteProtected: string[35] = 'Disk is write-protected in drive %c';
  SDiskNotReady:   string[29] = 'Disk is not ready in drive %c';
  SDataIntegrity:  string[32] = 'Data integrity error on drive %c';
  SSeekError:      string[22] = 'Seek error on drive %c';
  SUnknownMedia:   string[30] = 'Unknown media type in drive %c';
  SSectorNotFound: string[28] = 'Sector not found on drive %c';
  SOutOfPaper:     string[20] = 'Printer out of paper';
  SWriteFault:     string[23] = 'Write fault on drive %c';
  SReadFault:      string[22] = 'Read fault on drive %c';
  SGeneralFailure: string[28] = 'Hardware failure on drive %c';
  SBadImageOfFAT:  string[32] = 'Bad memory image of FAT detected';
  SDeviceError:    string[19] = 'Device access error';
  SInsertDisk:     string[27] = 'Insert diskette in drive %c';
  SRetryOrCancel:  string[27] = '~Enter~ Retry  ~Esc~ Cancel';

{ Critical error message translation table }

  ErrorString: array[0..15] of Word = (
    Ofs(SWriteProtected),
    Ofs(SCriticalError),
    Ofs(SDiskNotReady),
    Ofs(SCriticalError),
    Ofs(SDataIntegrity),
    Ofs(SCriticalError),
    Ofs(SSeekError),
    Ofs(SUnknownMedia),
    Ofs(SSectorNotFound),
    Ofs(SOutOfPaper),
    Ofs(SWriteFault),
    Ofs(SReadFault),
    Ofs(SGeneralFailure),
    Ofs(SBadImageOfFAT),
    Ofs(SDeviceError),
    Ofs(SInsertDisk));

{ System error handler routines }

procedure InitSysError; external;
procedure DoneSysError; external;

procedure SwapStatusLine(var Buffer); near; assembler;
asm
	MOV	CL,ScreenWidth
	XOR	CH,CH
	MOV	AL,ScreenHeight
	DEC	AL
	MUL	CL
	SHL	AX,1
	LES	DI,ScreenBuffer
	ADD	DI,AX
	PUSH	DS
	LDS	SI,Buffer
@@1:	MOV	AX,ES:[DI]
	MOVSW
	MOV	DS:[SI-2],AX
	LOOP	@@1
	POP	DS
end;

function SelectKey: Integer; near; assembler;
asm
	MOV	AH,3
	MOV	BH,0
	CALL	VideoInt
	PUSH	CX
	MOV	AH,1
	MOV	CX,2000H
	CALL	VideoInt
@@1:	MOV	AH,1
	INT	16H
	PUSHF
	MOV	AH,0
	INT	16H
	POPF
	JNE	@@1
	XOR	DX,DX
	CMP	AL,13
	JE	@@2
	INC	DX
	CMP	AL,27
	JNE	@@1
@@2:	POP	CX
	PUSH	DX
	MOV	AH,1
	CALL	VideoInt
	POP	AX
end;

{$V-}

function SystemError(ErrorCode: Integer; Drive: Byte): Integer;
var
  C: Word;
  P: Pointer;
  S: string[63];
  B: array[0..79] of Word;
begin
  if FailSysErrors then
  begin
    SystemError := 1;
    Exit;
  end;

  if Lo(ScreenMode) = smMono then
    C := SysMonoAttr else
    C := SysColorAttr;
  P := Pointer(Drive + Ord('A'));
  FormatStr(S, PString(Ptr(DSeg, ErrorString[ErrorCode]))^, P);
  MoveChar(B, ' ', Byte(C), 80);
  MoveCStr(B[1], S, C);
  MoveCStr(B[79 - CStrLen(SRetryOrCancel)], SRetryOrCancel, C);
  SwapStatusLine(B);
  SystemError := SelectKey;
  SwapStatusLine(B);
end;

{$V+}

{ ******** UTILITY ROUTINES ******** }

{ Keyboard support routines }

const

  AltCodes1: array[$10..$32] of Char =
    'QWERTYUIOP'#0#0#0#0'ASDFGHJKL'#0#0#0#0#0'ZXCVBNM';

  AltCodes2: array[$78..$83] of Char =
    '1234567890-=';

function GetAltChar(KeyCode: Word): Char;
begin
  GetAltChar := #0;
  if Lo(KeyCode) = 0 then
    case Hi(KeyCode) of
      $02: GetAltChar := #240;
      $10..$32: GetAltChar := AltCodes1[Hi(KeyCode)];
      $78..$83: GetAltChar := AltCodes2[Hi(KeyCode)];
    end;
end;

function GetAltCode(Ch: Char): Word;
var
  I: Word;
begin
  GetAltCode := 0;
  if Ch = #0 then Exit;
  Ch := UpCase(Ch);
  if Ch = #240 then
  begin
    GetAltCode := $0200;
    Exit;
  end;
  for I := $10 to $32 do
    if AltCodes1[I] = Ch then
    begin
      GetAltCode := I shl 8;
      Exit;
    end;
  for I := $78 to $83 do
    if AltCodes2[I] = Ch then
    begin
      GetAltCode := I shl 8;
      Exit;
    end;
end;

function GetCtrlChar(KeyCode: Word): Char;
begin
  GetCtrlChar := #0;
  if (Lo(KeyCode) <> 0) and (Lo(KeyCode) <= Byte('Z') - Byte('A') + 1) then
    GetCtrlChar := Char(Lo(KeyCode) + Byte('A') - 1);
end;

function GetCtrlCode(Ch: Char): Word;
begin
  GetCtrlCode := GetAltCode(Ch) or (Byte(UpCase(Ch)) - Byte('A') + 1);
end;

function CtrlToArrow(KeyCode: Word): Word;
const
  NumCodes = 11;
  CtrlCodes: array[0..NumCodes-1] of Char = ^S^D^E^X^A^F^G^V^R^C^H;
  ArrowCodes: array[0..NumCodes-1] of Word =
    (kbLeft, kbRight, kbUp, kbDown, kbHome, kbEnd, kbDel, kbIns,
     kbPgUp, kbPgDn, kbBack);
var
  I: Integer;
begin
  CtrlToArrow := KeyCode;
  for I := 0 to NumCodes - 1 do
    if WordRec(KeyCode).Lo = Byte(CtrlCodes[I]) then
    begin
      CtrlToArrow := ArrowCodes[I];
      Exit;
    end;
end;

{ String formatting routines }

{$L FORMAT.OBJ}

procedure FormatStr(var Result: String; const Format: String; var Params);
external {FORMAT};

procedure PrintStr(const S: String); assembler;
asm
	PUSH	DS
        LDS	SI,S
	CLD
	LODSB
	XOR	AH,AH
        XCHG	AX,CX
        MOV	AH,40H
        MOV	BX,1
        MOV	DX,SI
        INT	21H
        POP	DS
end;

{ Buffer move routines }

procedure MoveBuf(var Dest; var Source; Attr: Byte; Count: Word); assembler;
asm
	MOV	CX,Count
	JCXZ	@@5
	MOV	DX,DS
	LES	DI,Dest
	LDS	SI,Source
	MOV	AH,Attr
	CLD
	OR	AH,AH
	JE	@@3
@@1:	LODSB
	STOSW
	LOOP	@@1
	JMP	@@4
@@2:	INC	DI
@@3:	MOVSB
	LOOP	@@2
@@4:	MOV	DS,DX
@@5:
end;

procedure MoveChar(var Dest; C: Char; Attr: Byte; Count: Word); assembler;
asm
	MOV	CX,Count
	JCXZ	@@4
	LES	DI,Dest
	MOV	AL,C
	MOV	AH,Attr
	CLD
	OR	AL,AL
	JE	@@1
	OR	AH,AH
	JE	@@3
	REP	STOSW
	JMP	@@4
@@1:	MOV	AL,AH
@@2:	INC	DI
@@3:	STOSB
	LOOP	@@2
@@4:
end;

procedure MoveCStr(var Dest; const Str: String; Attrs: Word); assembler;
asm
	MOV	DX,DS
	LDS	SI,Str
	CLD
	LODSB
	MOV	CL,AL
	XOR	CH,CH
	JCXZ	@@3
	LES	DI,Dest
	MOV	BX,Attrs
	MOV	AH,BL
@@1:	LODSB
	CMP	AL,'~'
	JE	@@2
	STOSW
	LOOP	@@1
	JMP	@@3
@@2:	XCHG	AH,BH
	LOOP	@@1
@@3:	MOV	DS,DX
end;

procedure MoveStr(var Dest; const Str: String; Attr: Byte); assembler;
asm
	MOV	DX,DS
	LDS	SI,Str
	CLD
	LODSB
	MOV	CL,AL
	XOR	CH,CH
	JCXZ	@@4
	LES	DI,Dest
	MOV	AH,Attr
	OR	AH,AH
	JE	@@3
@@1:	LODSB
	STOSW
	LOOP	@@1
	JMP	@@4
@@2:	INC	DI
@@3:	MOVSB
	LOOP	@@2
@@4:	MOV	DS,DX
end;

function CStrLen(const S: String): Integer; assembler;
asm
	LES	DI,S
	MOV	CL,ES:[DI]
	INC	DI
	XOR	CH,CH
	MOV	BX,CX
        JCXZ    @@2
	MOV	AL,'~'
        CLD
@@1:	REPNE	SCASB
	JNE	@@2
	DEC	BX
	JMP	@@1
@@2:	MOV	AX,BX
end;

{ Drivers unit initialization and shutdown }

var
  SaveExit: Pointer;

procedure ExitDrivers; far;
begin
  DoneSysError;
  DoneEvents;
  ExitProc := SaveExit;
end;

begin
  DetectMouse;
  DetectVideo;
  SaveExit := ExitProc;
  ExitProc := @ExitDrivers;
end.
