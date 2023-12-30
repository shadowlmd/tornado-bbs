{$IFDEF MSDOS}
  {$IFNDEF DPMI32}
    {$F+}
  {$IFNDEF DPMI}
    {$DEFINE RealMode}
  {$ENDIF}
  {$ENDIF}
{$ENDIF}
{$I-,B-,X+}
{&Delphi+}

{*********************************************************}
{*                       TMISC.PAS                       *}
{*     Copyright (c) TurboPower Software 1987, 1992.     *}
{*     Copyright (c) Konstantin Klyagin 1996.            *}
{*                 All rights reserved.                  *}
{*********************************************************}

Unit tMisc;
  {-Basic string manipulation routines}

Interface

Uses
{$IFDEF OS2}
  Os2Base,
{$ENDIF}
{$IFDEF VirtualPascal}
  vpSysLow,
{$ENDIF}
{$IFNDEF WIN32}
  OpCrt,
{$ENDIF}
  DOS,
  ApSame,
  skCommon;

Type
  CharSet = Set Of Char;
  SysInt = {$IFDEF VirtualPascal} LongInt {$ELSE} Word {$ENDIF};

  tTime = System. Word;

{$IFNDEF VirtualPascal}
  OS = Record
    O, S : System. Word;
  End;
{$ELSE}
  OS = Record
    O : LongInt;
  End;
{$ENDIF}

  Long = Record
    LowWord, HighWord : System. Word;
  End;

  tTimePeriod = Record
    rtBegin,          {начало и}
    rtEnd    : tTime; {конец действия пpавила}
    rDOW     : Byte;  {пеpвый и последний день действия пpавила -
                         мл. 4 бита от rDOW : day begin
                         ст. 4 бита от rDOW : day end }
  End;

  TimeArray = Record
    nTPer : Byte;
    TPer  : Array [1..7] Of tTimePeriod;
  End;

  YesNoAuto = (ynYes, ynNo, ynAuto);
  AskType   = (atAsk, atYes, atNo);
  LayerType = (Fossil, Uart, Digi14, Int14);
  tAbortKey = (akAny, akEsc, akNone);

Const
  ExtLen = 3;
  DefaultDateMask = 'MM-DD-YYYY';
  MaxTime = 1439;                       {= 23:59 }
  BadTime = $FFFF;
  MinInHour = 60;                       {минут в часу}
  MinYear  = 1600;
  First2Months = 59;
  HoursInDay = 24;
  SecondsInDay = 86400;
  SecondsInHour = 3600;
  SecondsInMinute = 60;

  Threshold2000 = 1970;

  sDOW : Array [DayType] Of String [9] =
    ('Sunday', 'Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday',
     'Saturday');

  sMonths : Array [1..12] Of String [3] =
    ('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct',
     'Nov', 'Dec');

  SpaceOnly     : Set Of Char = [' '];
  CommaOnly     : Set Of Char = [','];
  SpaceAndComma : Set Of Char = [' ', ','];
  MinusOnly     : Set Of Char = ['-'];
  BracketsOnly  : Set Of Char = ['(', ')'];

  ZeroOne       : Array [Boolean] Of Char = ('0', '1');

{$I INC\config.inc}

  {-------- Numeric conversion -----------}

Function HexB (B: Byte): String;      {-Return hex string for byte}
Function HexW (W: Word): String;      {-Return hex string for word}
Function HexL (L: LongInt): String;   {-Return hex string for longint}
Function HexPtr (P: Pointer): String; {-Return hex string for pointer}

Function Long2Str (L: LongInt): String;
Function Str2Long (Const S: String): LongInt;

Function Hex2Byte (Const S: String): Byte;
Function Hex2Word (Const S: String): Word;
Function Hex2Long (Const S: String): LongInt;

Function InRange (Num: LongInt; Const Range: String): Boolean;

  {-------- General purpose string manipulation --------}

Function LoCaseMac (CH : Char) : Char;
  {-Lowercase character macro, no international character support}
{$IFNDEF VirtualPascal}
  Inline (
    $58 /                       {POP    AX}
    $3C / $41 /                 {CMP    AL,'A'}
    $72 / $06 /                 {JB     No}
    $3C / $5A /                 {CMP    AL,'Z'}
    $77 / $02 /                 {JA     No}
    $0C / $20);                 {OR     AL,$20}
{$ELSE}
  Inline;
  Begin
    if CH in ['A'..'Z'] Then LoCaseMac := Chr (Ord (CH) + 32)
                        Else LoCaseMac := CH;
  End;
{$ENDIF}

(*
{$IFDEF WIN32}
Function UpCase (Const C: Char): Char;
Function LoCase (Const C: Char): Char;
Function UpString (Const S: String): String;
Function LoString (Const S: String): String;
{$ELSE}
*)
Function UpCase (C: Char): Char;
Function LoCase (C: Char): Char;
Function UpString (S: String): String;
Function LoString (S: String): String;
(*
{$ENDIF}
*)

Function EngLoString (S: String): String;
Function PrString (Const St: String): String;

Function PadCh (S: String; Chr: Char; Len: Byte): String;
  {-Return a string right-padded to length len with ch}

Function Pad (S: String; Len: Byte): String;
  {-Return a string right-padded to length len with blanks}

Function LeftPadCh (S: String; Chr: Char; Len: Byte): String;
  {-Return a string left-padded to length len with ch}

Function LeftPad (S: String; Len: Byte): String;
  {-Return a string left-padded to length len with blanks}

Function TrimLead (S: String): String;
  {-Return a string with leading white space removed}

Function TrimTrail (S: String): String;
  {-Return a string with trailing white space removed}

Function Trim (S: String): String;
  {-Return a string with leading and trailing white space removed}

Function CenterCh (S: String; Chr: Char; Width: Byte): String;
  {-Return a string centered in a string of Ch with specified width}

Function Replicate (c: Char; n: Integer): String;
Function PlaceSubStr (Const InSt, WhatSt, ToSt: String): String;
Procedure PlaceSubStrP (Var S: String; Const WhatSt, ToSt: String);
Function PlaceSubStrNoCase (Const InSt, WhatSt, ToSt: String): String;
Procedure PlaceSubStrNoCaseP (Var S: String; Const WhatSt, ToSt: String);
Function ConsistsOf (S: String; Symbols: CharSet): Boolean;
Function StrContains (S: String; Symbols: CharSet): Boolean;
Function StrCompare (S1, S2: String): Integer;
Function DelChars (Ch: CharSet; S: String): String;
Function DelSpaces (Const S: String): String;
Function RPos (Const SubStr, Str: String; StartPos: Byte): Byte;
Function AsciiCode2Str (Const S: String): String;
Function Az2Str (Const Str: String; MaxLen: Byte): String; {Convert asciiz to string}
Procedure Str2Az (Const Str: String; MaxLen: Byte; Var AZStr); {Convert string to asciiz}
Function PosLastChar (CH: Char; Const St: String): Word;
Function XlatStr (S: String; Var Table): String;
Procedure SetString (Var Source, Dest; Count: Byte);
Function MaskMatch (Var Str; Var Mask): Boolean;
Function StrMaskMatch (Const Str, Mask: String): Boolean;
Procedure FillLong (Var Dest; Count: Word; Filler: LongInt);
Function NPos (Const SubStr, S: String; FromPos: Byte): Byte;
{Function PosBM (Const S, Buf: String): Byte;}

{$IFNDEF VirtualPascal}
Procedure SetLength (Var S: String; Len: Byte);
{$ENDIF}

  {--------------- Word manipulation -------------------------------}

Function WordCount (S: String; WordDelims: CharSet): Byte;
  {-Given a set of word delimiters, return number of words in S}

Function WordPosition (N: Byte; S: String; WordDelims: CharSet): Byte;
  {-Given a set of word delimiters, return start position of N'th word in S}

Function ExtractWord (N: Byte; S: String; WordDelims: CharSet): String;
  {-Given a set of word delimiters, return the N'th word in S}

Function SymbolCount (Const S: String; Symbol: Char): Byte;
  {-Return number of symbols in S}

Function AsciiCount (S: String; WordDelims: CharSet; Quote: Char): Byte;
  {!!.13-Given a set of word delimiters, return number of words in S}

Function AsciiPosition (N: Byte; S: String; WordDelims: CharSet; Quote: Char): Byte;
  {!!.13-Given a set of word delimiters, return start position of N'th word in S}

Function ExtractAscii (N: Byte; S: String; WordDelims: CharSet; Quote: Char): String;
  {!!.13-Given a set of word delimiters, return the N'th word in S}

Function AsciiPosCh (S: String; Ch, Quote: Char): Byte;
  { Finds Ch in S, except quoted parts }

Function AsciiPos (S, SubStr: String; Quote: Char): Byte;
  { Finds SubStr in S, except quoted parts }

Function SplitString (Var S: String; Len: Byte): String;
Function SplitStringPChar (Var Buf: PChar; Len: Byte): String;
Function WordInString (Const W, S: String): Boolean;
Procedure Str2Set (Const S: String; Var CS: CharSet);

  {--------------- DOS pathname parsing -----------------}

Function HasExtension (Const Name: String; Var DotPos: Integer): Boolean;

Function ForceExtension (Const Name: String; Const Ext: ExtStr): String;
  {-Force the specified extension onto the file name}

Function JustFilename (Const PathName: String): String;
  {-Return just the filename and extension of a pathname}

Function JustName (Const PathName: String): String;
  {-Return just the name (no extension, no path) of a pathname}

Function JustExtension (Const Name: String): ExtStr;
  {-Return just the extension of a pathname}

Function JustPathname (Const PathName: String): String;
  {-Return just the drive:directory portion of a pathname}

Function AddBackSlash (Const DirName: String): String;
  {-Add a default backslash to a directory name}

Function StripTrailBackSlashes (Const S: String): String;
  {- Strips trailing slashes, if any -}

Function NiceFileName (Const FName: String; Len: Byte): String;
Function NiceFileSize (Size: LongInt): String;

Function CompletePath (Const Path: String): String;
Function DefaultName (Const F, Ext, Dir: String): String;
Function TempFName: String;

  {--------------- files -----------------}

Function FileExists (Const Name: String): Boolean;
Function MultiFileExists (Const Path, FileName: String): Boolean;
Function DirExists (Const Dir: String): Boolean;
Function FileDate (Const Name: String): LongInt;
Function gFileSize (Const Name: String): LongInt;
Function gFileAttr (Const Name: String): Byte;
Procedure SmartChDir (Const S: String);
Function MatchWildCard (S1, Mask: String): Boolean;
Function MatchMultiCard (S1: String; Const Mask: String): Boolean;
Function tRenameFile (Const SourceFile, TargetFile: String): Boolean;
Function tCopyFile (Const SourceFile, TargetFile: String; OverWrite: Boolean): Boolean;
Function tDeleteFile (Const FileName: String): Boolean;

Function TextSeek (Var F: Text; Target: LongInt): Boolean;
Function TextPos (Var F: Text): LongInt;

  {--------------- date / time -----------------}

Function DateStr (DosDate: LongInt): String;
Function TimeStr (DosDate: LongInt): String;

Function FormattedDate (DT: DateTime; Const Mask: String): String;
Function ReformatDate (Const Date, InMask, OutMask: String): String;
Function Date2Long (Const Date: String): LongInt;
Procedure Long2DT (Julian: LongInt; Var DT: DateTime);
Function Long2Date (Julian: LongInt; Const Mask: String): String;

Function Word2Time (W: Word): String;
Function Time2Word (Const S: String): Word;
Function CurrTime2Word: Word;
Function PackStrTime (Const Date, Mask: String): LongInt;
Function Time2Long (Const Time: String): LongInt;
Function GetDosDate: LongInt;

Function FormattedCurrDT (Const Mask: String): String;
Function StrDate: String;
Function StrTime: String;
Function ShortStrTime: String;

Function DateMaskMatch (Const Date, Mask: String): Boolean;
Procedure UnixDate2DateTime (UnixDate: LongInt; Var DT: DateTime);
Function DateTime2UnixDate (DT: DateTime): LongInt;
Function ToUnixDate (FDate: LongInt): LongInt;
Function GetUnixDate: LongInt;
Function FromUnixDate (UnixDate: LongInt): LongInt;
Function UnixDate2Time (UnixDate: LongInt): String;
Function UnixDate2Date (UnixDate: LongInt): String;
Procedure GetDateTime (Var DT: DateTime);
Function DateMatch (Const Date, Mask: String): Boolean;
Function ExtractDT (Const sDate, DateMask: String; Var DT: DateTime): Boolean;
Function MidSec: LongInt;
Function TimeDiff (T1, T2: LongInt): LongInt;
Function HowTime (Time: LongInt): String;
Function CompareDates (Const S1, S2: String): Byte;
Function CompareDT (Const D1, D2: DateTime): Byte;
Function DateL: LongInt;
function DMYtoDate (Day, Month, Year: Integer): LongInt;
function HMStoTime (Hours, Minutes, Seconds: Byte): LongInt;
Function HoursDiff (DT1, DT2: DateTime): LongInt;
Function MonthStr (Num: Byte): String;
Function ValidDate (DT: DateTime): Boolean;
Function IsLeapYear (Year: Integer): Boolean;
Function DaysInMonth (Month, Year: Integer): Integer;

(* DateTime by Anton the Deinow *)

Function Time2Str (T: tTime): String;
  {-из tTime в стpоку `HH:MM'}

Function Str2Time (Const S: String): tTime;
  {-пеpевод из стpоки в tTime}

Function Str2TimePeriod (S: String; Var TPer: tTimePeriod): Boolean;
  {-конвеpтиpуем из стpоки в стpуктуpу timePeriod или пpосто в time}

Function Str2TimeArray (Const S: String; Var TA: TimeArray): Boolean;
  {-интеpвалы вpемений, pазделенные `,' из стpоки S в TA}

Function TimePeriod2Str (Const TPer: tTimePeriod): String;
  {-из пеpиода в стpоку}

Function MatchTimePeriod (Const TPer: tTimePeriod): Boolean;
  {-подходит ли тек. вpемя к заданому в TPer}

Function MatchTimeArray (Const TA: TimeArray): Boolean;
  {-сканит массив пеpиодов pаботы TA.TPer и возвpащает:
   TRUE==тек.вpемя/день входит в один из пеpиодов }

  {--------------- addr's -----------------}

Function RelativeAddr (Const S: String; Addr: TAddress): String;
Function To4D (Const S: String): String;
Procedure ParseStrAddr (Const StrAddr: String; Var Result: TAddress);
Function Addr2Str (Addr: TAddress): String;
Procedure MBDateTime2DosDateTime (Const MBDateTime: TMessageBaseDateTime; Var DosDateTime: DateTime);
Procedure DosDateTime2MBDateTime (Const DosDateTime: DateTime; Var MBDateTime: TMessageBaseDateTime);
{Function IsValidAddr (Addr: TAddress): Boolean;}
{Function AddrEqual (Addr1: TAddress; Addr2: TAddress): Boolean;}
{Function PointlessAddrStr (Var Addr: TAddress): String;}
{Function ParseAddr (Const AStr: String; CurrAddr: TAddress; Var DestAddr: TAddress): Boolean;}

  {--------------- misc -----------------}

Function PhoneValid (Const S: String): Boolean;
Procedure SoundOf (Const S: String);
Function Color2Byte (Const Color: String): Byte;
Function EstimatedTransferTime (Size, CPS, Speed: LongInt): LongInt;
Function YNA2Bool (YNA: AskType): Boolean;
Function FlagsValid (UserFlags, NeedFlags: String): Boolean;
Procedure Str2Security (Const S: String; Var Security: System. Word; Var Flags: String);
Function GetInitials (Const Name: String; MaxLen: Integer): String;
Procedure PlaySound (Freq, Dur: System. Word);
Procedure Pause (Duration: LongInt);
{$IFDEF MSDOS}
Procedure tmDelay (Ms: System. Word);
{$ENDIF}

Implementation

Uses
  Strings,
{$IFDEF MSDOS}
{$IFNDEF DPMI32}
  NTVDMSvc,
{$ENDIF}
{$ENDIF}
  tGlob,
  OpInline;

Const
  DosDelimSet : Set Of Char = ['\', ':', #0];

Const
  C1970 = 2440588;
  D0    = 1461;
  D1    = 146097;
  D2    = 1721119;

Function HexPtr (P: Pointer): String;
Begin
{$IFNDEF VirtualPascal}
  HexPtr := HexW (OS (P). S) + ':' + HexW (OS (P). O);
{$ELSE}
  HexPtr := HexL (OS (P). O);
{$ENDIF}
End;

Function Long2Str (L: LongInt): String;
{$IFNDEF VirtualPascal}
Var
  Result : String;
{$ENDIF}

Begin
  Str (L, Result);
{$IFNDEF VirtualPascal}
  Long2Str := Result;
{$ENDIF}
End;

Function Str2Long (Const S: String): LongInt;
Var
  i      : SysInt;
{$IFNDEF VirtualPascal}
  Result : LongInt;
{$ENDIF}

Begin
  Val (Trim (S), Result, i);
{$IFNDEF VirtualPascal}
  If i = 0 Then Str2Long := Result
           Else Str2Long := 0;
{$ELSE}
  If i <> 0 Then
    Result := 0;
{$ENDIF}
End;

{$IFDEF DPMI}
  {$F+}
{$ENDIF}
{$IFDEF VirtualPascal}
  {$L TMisc32.Obj}
{$ELSE}
  {$L TMisc.Obj}
  Procedure SetLength (Var S: String; Len: Byte); External;
{$ENDIF}
  Function PadCh (S: String; Chr: Char; Len: Byte): String; External;
  Function Pad (S: String; Len: Byte): String; External;
  Function LeftPadCh (S: String; Chr: Char; Len: Byte): String; External;
  Function LeftPad (S: String; Len: Byte): String; External;
  Function TrimLead (S: String): String; External;
  Function TrimTrail (S: String): String; External;
  Function Trim (S: String): String; External;
  Function CenterCh (S: String; Chr: Char; Width: Byte): String; External;
  Function Replicate (c: Char; n: Integer): String; External;
  Function UpCase (C: Char): Char; External;
  Function LoCase (C: Char): Char; External;
  Function UpString (S: String): String; External;
  Function LoString (S: String): String; External;
  Function EngLoString (S: String): String; External;
  Function WordCount (S: String; WordDelims: CharSet): Byte; External;
  Function WordPosition (N: Byte; S: String; WordDelims: CharSet): Byte; External;
  Function ExtractWord (N: Byte; S: String; WordDelims: CharSet): String; External;
  Function AsciiCount (S: String; WordDelims: CharSet; Quote: Char): Byte; External;
  Function AsciiPosition (N: Byte; S: String; WordDelims: CharSet; Quote: Char): Byte; External;
  Function ExtractAscii (N: Byte; S: String; WordDelims: CharSet; Quote: Char): String; External;
  Function AsciiPosCh (S: String; Ch, Quote: Char): Byte; External;
  Function AsciiPos (S, SubStr: String; Quote: Char): Byte; External;
  Function ConsistsOf (S: String; Symbols: CharSet): Boolean; External;
  Function StrContains (S: String; Symbols: CharSet): Boolean; External;
  Function DelChars (Ch: CharSet; S: String): String; External;
  Procedure Str2Set (Const S: String; Var CS: CharSet); External;
  Function StrCompare (S1, S2: String): Integer; External;
  Procedure SetString (Var Source, Dest; Count: Byte); External;
  Function FlagsValid (UserFlags, NeedFlags: String): Boolean; External;
  Function MaskMatch (Var Str; Var Mask): Boolean; External;
  Function XlatStr (S: String; Var Table): String; External;
  Function HexB (B: Byte): String; External;
  Function HexW (W: Word): String; External;
  Function HexL (L: LongInt): String; External;
  Procedure FillLong (Var Dest; Count: Word; Filler: LongInt); External;
  Function Hex2Byte (Const S: String): Byte; External;
  Function Hex2Word (Const S: String): Word; External;
  Function Hex2Long (Const S: String): LongInt; External;
  Function NPos (Const SubStr, S: String; FromPos: Byte): Byte; External;
  {Function PosBM (Const S, Buf: String): Byte; External;}
{$IFDEF DPMI}
  {$F-}
{$ENDIF}

(*
{$IFDEF WIN32}
Function UpCase (Const C: Char): Char;
Var
  PS: Array [0..1] of Char;
Begin
  PS[0] := C;
  PS[1] := #0;
  OemToChar (@PS, @PS);
  CharUpper (@PS);
  CharToOem (@PS, @PS);
  Result := PS[0];
End;

Function LoCase (Const C: Char): Char;
Var
  PS: Array [0..1] of Char;
Begin
  PS[0] := C;
  PS[1] := #0;
  OemToChar (@PS, @PS);
  CharLower (@PS);
  CharToOem (@PS, @PS);
  Result := PS[0];
End;

Function UpString (Const S: String): String;
Var
  PS: Array [0..255] of Char;
Begin
  StrPCopy (@PS, S);
  OemToChar (@PS, @PS);
  CharUpper (@PS);
  CharToOem (@PS, @PS);
  Result := StrPas (@PS);
End;

Function LoString (Const S: String): String;
Var
  PS: Array [0..255] of Char;
Begin
  StrPCopy (@PS, S);
  OemToChar (@PS, @PS);
  CharLower (@PS);
  CharToOem (@PS, @PS);
  Result := StrPas (@PS);
End;
{$ENDIF}
*)

Function MatchWildCard (S1, Mask: String): Boolean;
Var
  Len : Integer;

Begin
  If (Mask = '*') Or (Mask = '*.*') Then
    MatchWildCard := True
  Else
  Begin
    If Pos ('.', S1) = 0 Then
      S1 := S1 + '.';
    If (Pos ('*', Mask) = 0) And (Pos ('?', Mask) = 0) Then
      Mask := '*' + Mask + '*';

    Len := Length (S1);
    Move (S1 [1], S1 [0], Len);
    S1 [Len] := #0;
    Len := Length (Mask);
    Move (Mask [1], Mask [0], Len);
    Mask [Len] := #0;

    MatchWildCard := MaskMatch (S1, Mask);
  End;
End;

Function StrMaskMatch (Const Str, Mask: String): Boolean;
Var
  Len  : Integer;
  S, M : String;

Begin
  Len := Length (Str);
  Move (Str [1], S [0], Len);
  S [Len] := #0;
  Len := Length (Mask);
  Move (Mask [1], M [0], Len);
  M [Len] := #0;

  StrMaskMatch := MaskMatch (S, M);
End;

Function PlaceSubStr (Const InSt, WhatSt, ToSt: String): String;
Var
  P, PS, WLen, TLen : Integer;
{$IFNDEF VirtualPascal}
  Result            : String;
{$ENDIF}

Label
  Loop;

Begin
  Result := InSt;
  WLen := Length (WhatSt);
  TLen := Length (ToSt);
  PS := 1;

Loop:
  P := NPos (WhatSt, Result, PS);

  If P > 0 Then
  Begin
    Delete (Result, P, WLen);
    Insert (ToSt, Result, P);
    PS := P + TLen;
    If PS <= 255 Then
      Goto Loop;
  End;

{$IFNDEF VirtualPascal}
  PlaceSubStr := Result;
{$ENDIF}
End;

Procedure PlaceSubStrP (Var S: String; Const WhatSt, ToSt: String);
Var
  P, PS, WLen, TLen : Integer;

Label
  Loop;

Begin
  WLen := Length (WhatSt);
  TLen := Length (ToSt);
  PS := 1;

Loop:
  P := NPos (WhatSt, S, PS);

  If P > 0 Then
  Begin
    Delete (S, P, WLen);
    Insert (ToSt, S, P);
    PS := P + TLen;
    If PS <= 255 Then
      Goto Loop;
  End;
End;

Function PlaceSubStrNoCase (Const InSt, WhatSt, ToSt: String): String;
Var
  P, PS, WLen, TLen : Integer;
  UpWhat            : String;
{$IFNDEF VirtualPascal}
  Result            : String;
{$ENDIF}

Label
  Loop;

Begin
  Result := InSt;
  UpWhat := UpString (WhatSt);
  WLen := Length (UpWhat);
  TLen := Length (ToSt);
  PS := 1;

Loop:
  P := NPos (UpWhat, UpString (Result), PS);

  If P > 0 Then
  Begin
    Delete (Result, P, WLen);
    Insert (ToSt, Result, P);
    PS := P + TLen;
    If PS <= 255 Then
      Goto Loop;
  End;

{$IFNDEF VirtualPascal}
  PlaceSubStrNoCase := Result;
{$ENDIF}
End;

Procedure PlaceSubStrNoCaseP (Var S: String; Const WhatSt, ToSt: String);
Var
  P, PS, WLen, TLen : Integer;
  UpWhat            : String;

Label
  Loop;

Begin
  UpWhat := UpString (WhatSt);
  WLen := Length (UpWhat);
  TLen := Length (ToSt);
  PS := 1;

Loop:
  P := NPos (UpWhat, UpString (S), PS);

  If P > 0 Then
  Begin
    Delete (S, P, WLen);
    Insert (ToSt, S, P);
    PS := P + TLen;
    If PS <= 255 Then
      Goto Loop;
  End;
End;

Function PrString (Const St: String): String;
Var
  i      : Integer;
  C      : ^Char;
  NextUp : Boolean;
{$IFNDEF VirtualPascal}
  Result : String;
{$ENDIF}

  (*
  {$IFDEF WIN32}
  function IsAlpha (Const Ch: Char): Boolean;
  Var
    PS: Array [0..1] of Char;
  Begin
    PS[0] := Ch;
    PS[1] := #0;
    OemToChar(@PS, @PS);
    Result := IsCharAlpha(PS[0]);
  End;
  {$ENDIF}
  *)

Begin
  Result := St;
  C := @Result [1];
  NextUp := True;

  For i := 1 To Length (Result) Do
  Begin
    (*
    {$IFDEF WIN32}
    If Not IsAlpha(C^) Then
    {$ELSE}
    *)
    If Not (C^ in ['A'..'Z', 'a'..'z', 'А'..'п', 'р'..'я']) Then
    (*
    {$ENDIF}
    *)
      NextUp := True
    Else
      If NextUp Then
      Begin
        NextUp := False;
        C^ := UpCase (C^);
      End
      Else
        C^ := LoCase (C^);

    Inc (C);
  End;

{$IFNDEF VirtualPascal}
  PrString := Result;
{$ENDIF}
End;

Function HasExtension (Const Name: String; Var DotPos: Integer): Boolean;
Var
  i : Integer;

Begin
  For i := Length (Name) DownTo 1 Do
    Case Name [i] Of
      '.' : Begin
              DotPos := i;
              HasExtension := True;
              Exit;
            End;

      '\' : Break;
    End;

  DotPos := 0;
  HasExtension := False;
End;

Function ForceExtension (Const Name: String; Const Ext: ExtStr): String;
Var
  DotPos : Integer;

Begin
  If HasExtension (Name, DotPos) Then
    ForceExtension := Copy (Name, 1, DotPos) + Ext
  Else
    If Name = '' Then ForceExtension := ''
                 Else ForceExtension := Name + '.' + Ext;
End;

Function JustExtension (Const Name: String): ExtStr;
Var
  DotPos : Integer;

Begin
  If HasExtension (Name, DotPos) Then
    JustExtension := Copy (Name, Succ (DotPos), ExtLen)
  Else
    JustExtension := '';
End;

Function JustFilename (Const PathName: String): String;
Var
  i : Integer;

Begin
  i := Length (PathName);

  While (i > 0) And Not (PathName [i] in DosDelimSet) Do
    Dec (i);

  JustFilename := Copy (PathName, Succ (i), 255);
End;

Function JustName (Const PathName: String): String;
Var
  DotPos : Integer;
{$IFNDEF VirtualPascal}
  Result : String;
{$ENDIF}

Begin
  Result := JustFileName (PathName);
  DotPos := Pos ('.', Result);
  If DotPos > 0 Then
    SetLength (Result, DotPos - 1);
{$IFNDEF VirtualPascal}
  JustName := Result;
{$ENDIF}
End;

Function JustPathname (Const PathName: String): String;
Var
  i      : Integer;
{$IFNDEF VirtualPascal}
  Result : String;
{$ENDIF}

Begin
  Result := PathName;

  If Result <> '.' Then
  Begin
    i := Length (Result);

    While (i > 0) And Not (PathName [i] in DosDelimSet) Do
      Dec (i);

    If (i >= 2) And (Result [i] = '\') And (Result [i - 1] <> ':') Then
      Dec (i);

    SetLength (Result, i);
  End;

{$IFNDEF VirtualPascal}
  JustPathName := Result;
{$ENDIF}
End;

Function AddBackSlash (Const DirName: String): String;
Begin
  If Trim (DirName) <> '' Then
  Begin
    If DirName [Length (DirName)] In DosDelimSet Then
      AddBackSlash := DirName
    Else
      AddBackSlash := DirName + '\';
  End
  Else
    AddBackSlash := '';
End;

Function StripTrailBackSlashes (Const S: String): String;
Var
  i      : Integer;
{$IFNDEF VirtualPascal}
  Result : String;
{$ENDIF}

Begin
  Result := S;
  i := Length (Result);

  While (i > 0) And (Result [i] = '\') Do
    Dec (i);

  SetLength (Result, i);
{$IFNDEF VirtualPascal}
  StripTrailBackSlashes := Result;
{$ENDIF}
End;

Function DefaultName (Const F, Ext, Dir: String): String;
Var
  S      : String;
{$IFNDEF VirtualPascal}
  Result : String;
{$ENDIF}

Begin
  Result := Trim (F);

  If Result <> '' Then
  Begin
    S := JustPathName (Result);
    If S <> '' Then
      S := CompletePath (S);
    Result := JustFileName (Result);
    If NPos ('.', Result, 2) = 0 Then
      Result := Result + '.' + Ext;
    Result := S + Result;
    If Pos ('\', Result) = 0 Then
      Result := AddBackSlash (Dir) + Result;
  End;

{$IFNDEF VirtualPascal}
  DefaultName := Result;
{$ENDIF}
End;

Function CompletePath (Const Path: String): String;
Var
  i               : Integer;
  DrCh            : Char;
  SaveDir, CurDir : String;
{$IFNDEF VirtualPascal}
  Result          : String;
{$ENDIF}

Begin
  If (Length (Path) = 3) And (Copy (Path, 2, 2) = ':\') Then
  Begin
    CompletePath := Path;
    Exit;
  End;

  Result := Path;
  i := Length (Result);
  If (i > 1) And (Result [i] = '\') Then
    SetLength (Result, i - 1);

  GetDir (0, CurDir);

  i := Pos (':', Result);
  If i = 2 Then
  Begin
    {Get current directory on specified drive}
    DrCh := UpCase (Result [1]);
    If (DrCh >= 'A') And
       Not ((Pos (':', CurDir) = 2) And (DrCh = UpCase (CurDir [1])))
    Then
      GetDir (Byte (DrCh) - Byte ('A') + 1, SaveDir)
    Else
      i := 0;
  End;

  ChDir (Result);
  DosError := IOResult;

  If DosError = 0 Then
  Begin
    GetDir (0, Result);

    If i = 2 Then
      ChDir (SaveDir); {Restore current directory on other drive}

    If Result <> CurDir Then
      ChDir (CurDir);
  End
  Else
    ChDir (CurDir);

  If IOResult <> 0 Then;
  CompletePath := AddBackSlash (Result);
End;

Procedure SmartChDir (Const S: String);
Var
  Len : Integer;

Begin
  Len := Length (S);
  If (S [Len] = '\') And Not ((Len = 3) And (S [Len - 1] = ':')) Then
    ChDir (Copy (S, 1, Len - 1))
  Else
    ChDir (S);
End;

Function FileExists (Const Name: String): Boolean;
Var
  DirInfo : SearchRec;

Begin
  FindFirst (Name, AnyFile-VolumeID-Directory-Hidden, DirInfo);
  FileExists := DosError = 0;
{$IFNDEF MSDOS}
  FindClose (DirInfo);
{$ENDIF}
End;

Function MultiFileExists (Const Path, FileName: String): Boolean;
Var
  i, WC : Integer;

Begin
  WC := WordCount (Path, SpaceAndComma);

  For i := 1 To WC Do
    If FileExists (AddBackSlash (ExtractWord (i, Path, SpaceAndComma)) +
       FileName) Then
    Begin
      MultiFileExists := True;
      Exit;
    End;

  MultiFileExists := False;
End;

Function DirExists (Const Dir: String): Boolean;
Var
  DirInfo : SearchRec;

Begin
  FindFirst (AddBackSlash (Dir) + AllFilesMask, AnyFile, DirInfo);
  DirExists := DosError = 0;
{$IFNDEF MSDOS}
  FindClose (DirInfo);
{$ENDIF}
End;

Function FileDate (Const Name: String): LongInt;
Var
  DirInfo : SearchRec;

Begin
  FindFirst (Name, AnyFile-VolumeID-Directory, DirInfo);
  If DosError = 0 Then FileDate := DirInfo. Time
                  Else FileDate := -1;
{$IFNDEF MSDOS}
  FindClose (DirInfo);
{$ENDIF}
End;

Function gFileSize (Const Name: String): LongInt;
Var
  DirInfo : SearchRec;

Begin
  FindFirst (Name, AnyFile-VolumeID-Directory, DirInfo);
  If DosError = 0 Then gFileSize := DirInfo. Size
                  Else gFileSize := 0;
{$IFNDEF MSDOS}
  FindClose (DirInfo);
{$ENDIF}
End;

Function gFileAttr (Const Name: String): Byte;
Var
  DirInfo : SearchRec;

Begin
  FindFirst (Name, AnyFile-VolumeID-Directory, DirInfo);
  If DosError = 0 Then gFileAttr := DirInfo. Attr
                  Else gFileAttr := 0;
{$IFNDEF MSDOS}
  FindClose (DirInfo);
{$ENDIF}
End;

Function tCopyFile (Const SourceFile, TargetFile: String; OverWrite: Boolean): Boolean;
Const
{$IFDEF RealMode}
  BufSize = 2048;
{$ELSE}
  BufSize = 4096;
{$ENDIF}

Type
  Buffer              = Array [0..BufSize-1] Of Byte;
  PBuffer             = ^Buffer;

Var
  FTime               : LongInt;
  FileBuf             : PBuffer;
  BRead, BWrite, Attr : Word;
  Source, Target      : File;

Begin
  tCopyFile := False;
  If SourceFile = TargetFile Then
    Exit;

  Assign (Source, SourceFile);
  Reset (Source, 1);
  If IOResult <> 0 Then
    Exit;

  Assign (Target, TargetFile);
  ReWrite (Target, 1);
  If IOResult <> 0 Then
  Begin
    Close (Source);
    Exit;
  End;

  New (FileBuf);

  Repeat
    BlockRead (Source, FileBuf^, BufSize, BRead);
    BlockWrite (Target, FileBuf^, BRead, BWrite);

    If BRead <> BWrite Then
    Begin
      Dispose (FileBuf);
      Close (Source);
      Close (Target);
      Erase (Target);
      Exit;
    End;
  Until BRead <> BufSize;

  Dispose (FileBuf);
  GetFTime (Source, FTime);
  SetFTime (Target, FTime);
  Close (Source);
  Close (Target);
  GetFAttr (Source, Attr);
  SetFAttr (Target, Attr);
  If IOResult <> 0 Then;

  tCopyFile := True;
End;

Function tRenameFile (Const SourceFile, TargetFile: String): Boolean;
Const
{$IFDEF RealMode}
  BufSize = 2048;
{$ELSE}
  BufSize = 4096;
{$ENDIF}

Type
  Buffer  = Array [0..BufSize-1] Of Byte;
  PBuffer = ^Buffer;

Var
  FTime          : LongInt;
  BRead, BWrite  : SysInt;
  Source, Target : File;
  FileBuf        : PBuffer;

Begin
  If SourceFile = TargetFile Then
  Begin
    tRenameFile := True;
    Exit;
  End;

  Assign (Source, SourceFile);
  Rename (Source, TargetFile);
  If IOResult = 0 Then
  Begin
    tRenameFile := True;
    Exit;
  End;

  tRenameFile := False;

  Reset (Source, 1);
  If IOResult <> 0 Then
    Exit;

  Assign (Target, TargetFile);
  Rewrite (Target, 1);
  If IOResult <> 0 Then
  Begin
    Close (Source);
    Exit;
  End;

  New (FileBuf);

  Repeat
    BlockRead (Source, FileBuf^, BufSize, BRead);
    BlockWrite (Target, FileBuf^, BRead, BWrite);

    If BRead <> BWrite Then
    Begin
      Dispose (FileBuf);
      Close (Source);
      Close (Target);
      Erase (Target);
      Exit;
    End;
  Until BRead <> BufSize;

  Dispose (FileBuf);
  GetFTime (Source, FTime);
  SetFTime (Target, FTime);
  Close (Source);
  Close (Target);
  Erase (Source);

  tRenameFile := True;
End;

Function tDeleteFile (Const FileName: String): Boolean;
Var
  F       : File;
  DirInfo : SearchRec;
  PName   : String;
{$IFNDEF VirtualPascal}
  Result  : Boolean;
{$ENDIF}

Begin
  Result := False;
  PName := AddBackSlash (JustPathName (FileName));
  FindFirst (FileName, AnyFile-VolumeID-Directory, DirInfo);

  While DosError = 0 Do
  Begin
    Assign (F, PName + DirInfo. Name);
    Erase (F);
    If IOResult <> 0 Then
    Begin
      SetFAttr (F, 0);
      Erase (F);
    End;
    Result := Result Or (IOResult = 0);
    FindNext (DirInfo);
  End;

{$IFNDEF MSDOS}
  FindClose (DirInfo);
{$ENDIF}

{$IFNDEF VirtualPascal}
  tDeleteFile := Result;
{$ENDIF}
End;

Function NiceFileName (Const FName: String; Len: Byte): String;
Var
  Path, Name : String;
{$IFNDEF VirtualPascal}
  Result     : String;
{$ENDIF}

Begin
  If Trim (FName) <> '' Then
  Begin
    Result := LoString (FName);
    Path := JustPathName (Result);
    Name := JustFileName (Result);

    If (Path <> '') And (Name <> '') Then
      Result := AddBackSlash (UpString (Path)) + Name;

    If Length (Result) > Len Then
      NiceFileName := Copy (Result, 1, 3) + '..' + Copy (Result,
        Length (Result) - Len + 6, Len - 5)
    Else
      {$IFNDEF VirtualPascal} NiceFileName := Result {$ENDIF};
  End
  Else
    NiceFileName := '';
End;

Function NiceFileSize (Size: LongInt): String;
Var
  sSize : String [5];

Begin
  If Size < 1024 Then
  Begin
    If Size < 0 Then
      Size := 0;
    Str (Size, sSize);
    NiceFileSize := sSize + 'b';
  End
  Else
    If Size < 1048576 Then
    Begin
      Str (Size Shr 10, sSize);
      NiceFileSize := sSize + 'k';
    End Else
    Begin
      If Size < 104857600 Then
        Str ((Size / 1048576): 0: 1, sSize)
      Else
        Str (Size Shr 20, sSize);
      NiceFileSize := sSize + 'M';
    End;
End;

Function TempFName: String;
Var
  h, m, s, s100 : Word;

Begin
  GetTime (h, m, s, s100);
  TempFName := Long2Str (m) + Long2Str (s) + Long2Str (s100);
End;

Function Az2Str (Const Str: String; MaxLen: Byte): String;
Var
  i      : Integer;
{$IFNDEF VirtualPascal}
  Result : String;
{$ENDIF}

Begin
  Move (Str, Result [1], MaxLen);
  SetLength (Result, MaxLen);
  i := Pos (#0, Result);
  If i > 0 Then
    SetLength (Result, i - 1);
{$IFNDEF VirtualPascal}
  Az2Str := Result;
{$ENDIF}
End;

Procedure Str2Az (Const Str: String; MaxLen: Byte; Var AZStr);
Type
  StrArr = Array [0..255] Of Char;

Var
  OutArr : StrArr Absolute AZStr;

Begin
  If Length (Str) < MaxLen Then
    MaxLen := Length (Str);
  Move (Str [1], OutArr, MaxLen);
  OutArr [MaxLen] := #0;
End;

Function PosLastChar (CH: Char; Const St: String): Word;
{$IFNDEF VirtualPascal}
Var
  Result : Word;
{$ENDIF}

Begin
  Result := Length (St);

  While (Result > 0) And (St [Result] <> CH) Do
    Dec (Result);

{$IFNDEF VirtualPascal}
  PosLastChar := Result;
{$ENDIF}
End;

Function AsciiCode2Str (Const S: String): String;
Var
  i      : Integer;
{$IFNDEF VirtualPascal}
  Result : String;
{$ENDIF}

Begin
  Result := '';
  i := 0;

  While i < Length (S) Do
  Begin
    Inc (i);

    If (S [i] = '\') And (i + 2 <= Length (S)) And
       (S [i + 1] in ['0'..'9', 'A'..'F']) And
       (S [i + 2] in ['0'..'9', 'A'..'F']) Then
    Begin
      Result := Result + Chr (Hex2Byte (Copy (S, i + 1, 2)));
      Inc (i, 2);
    End
    Else
      Result := Result + S [i];
  End;

{$IFNDEF VirtualPascal}
  AsciiCode2Str := Result;
{$ENDIF}
End;

Function InRange (Num: LongInt; Const Range: String): Boolean;
Var
  sItem, eItem : LongInt;
  i, Words     : Integer;
  Item         : String;

Begin
  Words := WordCount (Range, SpaceAndComma);

  For i := 1 To Words Do
  Begin
    Item := ExtractWord (i, Range, SpaceAndComma);
    If Pos ('-', Item) > 0 Then
    Begin
      sItem := Str2Long (ExtractWord (1, Item, MinusOnly));
      eItem := Str2Long (ExtractWord (2, Item, MinusOnly));
      If (Num >= sItem) And (Num <= eItem) Then
      Begin
        InRange := True;
        Exit;
      End;
    End
    Else
      If Num = Str2Long (Item) Then
      Begin
        InRange := True;
        Exit;
      End;
  End;

  InRange := False;
End;

Function RPos (Const SubStr, Str: String; StartPos: Byte): Byte;
Var
  i, SubLen : Integer;

Begin
  SubLen := Length (SubStr);

  If SubLen > 0 Then
  Begin
    Dec (SubLen);

    For i := StartPos DownTo 1 Do
      If Str [i] = SubStr [1] Then
        If Copy (Str, i + 1, SubLen) = Copy (SubStr, 2, SubLen) Then
        Begin
          RPos := i;
          Exit;
        End;
  End;

  RPos := 0;
End;

Function DateStr (DosDate: LongInt): String;
Var
  DT : DateTime;

Begin
  UnpackTime (DosDate, DT);
  DateStr := FormattedDate (DT, 'MM-DD-YY');
End;

Function TimeStr (DosDate: LongInt): String;
Var
  DT : DateTime;

Begin
  UnpackTime (DosDate, DT);
  TimeStr := FormattedDate (DT, 'HH:II:SS');
End;

Function MonthStr (Num: Byte): String;
Begin
  If (Num >= 1) And (Num <= 12) Then MonthStr := sMonths [Num]
                                Else MonthStr := '???';
End;

Function ReformatDate (Const Date, InMask, OutMask: String): String;
Var
  Code   : SysInt;
  TmpPos : Integer;
  DT     : DateTime;

Begin
  TmpPos := Pos ('YY', InMask);
  If Copy (InMask, TmpPos + 2, 2) = 'YY' Then
    Val (Copy (Date, TmpPos, 4), DT. Year, Code)
  Else
  Begin
    Val (Copy (Date, TmpPos, 2), DT. Year, Code);
    If DT. Year < 80 Then Inc (DT. Year, 2000)
                     Else Inc (DT. Year, 1900);
  End;

  Val (Copy (Date, Pos ('MM', InMask), 2), DT. Month, Code);
  Val (Copy (Date, Pos ('DD', InMask), 2), DT. Day, Code);

  ReformatDate := FormattedDate (DT, OutMask);
End;

Function FormattedDate (DT: DateTime; Const Mask: String): String;
Var
  CurrPos : Integer;
  S       : String [4];
  M       : String;
{$IFNDEF VirtualPascal}
  Result  : String;
{$ENDIF}

Begin
  Result := Mask;
  M := UpString (Mask);

  CurrPos := Pos ('DD', M);
  If CurrPos > 0 Then
  Begin
    S := LeftPadCh (Long2Str (DT. Day), '0', 2);
    Move (S [1], Result [CurrPos], 2);
  End;

  CurrPos := Pos ('MM', M);
  If CurrPos > 0 Then
  Begin
    S := LeftPadCh (Long2Str (DT. Month), '0', 2);
    Move (S [1], Result [CurrPos], 2);
  End;

  CurrPos := Pos ('NNN', M);
  If CurrPos > 0 Then
  Begin
    S := MonthStr (DT. Month);
    Move (S [1], Result [CurrPos], 3);
  End;

  CurrPos := Pos ('YY', M);
  If CurrPos > 0 Then
  Begin
    S := LeftPadCh (Long2Str (DT. Year), '0', 4);
    If Copy (M, CurrPos + 2, 2) = 'YY' Then
      Move (S [1], Result [CurrPos], 4)
    Else
      Move (S [3], Result [CurrPos], 2);
  End;

  CurrPos := Pos ('HH', M);
  If CurrPos > 0 Then
  Begin
    S := LeftPadCh (Long2Str (DT. Hour), '0', 2);
    Move (S [1], Result [CurrPos], 2);
  End;

  CurrPos := Pos ('II', M);
  If CurrPos > 0 Then
  Begin
    S := LeftPadCh (Long2Str (DT. Min), '0', 2);
    Move (S [1], Result [CurrPos], 2);
  End;

  CurrPos := Pos ('SS', M);
  If CurrPos > 0 Then
  Begin
    S := LeftPadCh (Long2Str (DT. Sec), '0', 2);
    Move (S [1], Result [CurrPos], 2);
  End;

{$IFNDEF VirtualPascal}
  FormattedDate := Result;
{$ENDIF}
End;

Const
  BadDate = $FFFF;
  MaxYear = 2078;

Function IsLeapYear (Year: Integer): Boolean;
Begin
  IsLeapYear := (Year Mod 4 = 0) And (Year Mod 4000 <> 0) And
                ((Year Mod 100 <> 0) Or (Year Mod 400 = 0));
End;

Function DaysInMonth (Month, Year: Integer): Integer;
Begin
  If Word (Year) < 100 Then
  Begin
    Inc (Year, 1900);
    If Year < Threshold2000 Then
      Inc (Year, 100);
  End;

  Case Month Of
      1, 3, 5, 7, 8, 10, 12 : DaysInMonth := 31;
      4, 6, 9, 11           : DaysInMonth := 30;
      2                     : DaysInMonth := 28 + Ord (IsLeapYear (Year));
  Else
    DaysInMonth := 0;
  End;
End;

Function iDate2Long (Day, Month, Year: SysInt): LongInt;
Begin
  If Word (Year) < 100 Then
  Begin
    Inc (Year, 1900);
    If Year < Threshold2000 Then
      Inc (Year, 100);
  End;

  If (Day > 31) Or (Day < 1) Or (Month > 12) Or (Month < 1) Or (Year < 0) Then
    iDate2Long := BadDate
  Else
    If (Year = MinYear) And (Month < 3) Then
      If Month = 1 Then
        iDate2Long := Pred (Day)
      Else
        iDate2Long := Day + 30
    Else
    Begin
      If Month > 2 Then
        Dec (Month, 3)
      Else
      Begin
        Inc (Month, 9);
        Dec (Year);
      End;
      Dec (Year, MinYear);
      iDate2Long := ((LongInt (Year) * 1461) Div 4) +
                    (((153 * Month) + 2) Div 5) + Day + First2Months;
    End;
End;

Function DMYtoDate (Day, Month, Year: Integer): LongInt;
Begin
  If Word (Year) < 100 Then
  Begin
    Inc(Year, 1900);
    If Year < Threshold2000 Then
      Inc (Year, 100);
  End;

  If (Year = MinYear) And (Month < 3) Then
    If Month = 1 Then
      DMYtoDate := Pred (Day)
    Else
      DMYtoDate := Day + 30
  Else
  Begin
    If Month > 2 Then
      Dec (Month, 3)
    Else
    Begin
      Inc (Month, 9);
      Dec (Year);
    End;
    Dec (Year, MinYear);
    DMYtoDate :=
      {$IFDEF FourByteDates}
        ((LongInt (Year Div 100) * 146097) Div 4) +
        ((LongInt (Year Mod 100) * 1461) Div 4) +
      {$ELSE}
        ((LongInt (Year) * 1461) Div 4) +
      {$ENDIF}
        (((153 * Month) + 2) Div 5) + Day + First2Months;
  End;
End;

Function HMStoTime (Hours, Minutes, Seconds: Byte): LongInt;
Begin
  HMStoTime := ((LongInt (Hours Mod HoursInDay) * SecondsInHour) +
    (LongInt (Minutes) * SecondsInMinute) + Seconds) Mod SecondsInDay;
End;

Function HoursDiff (DT1, DT2: DateTime): LongInt;
Var
  Days, Secs : LongInt;
  D1, D2     : DateTimeRec;

Begin
  D1. D := DMYtoDate (DT1. Day, DT1. Month, DT1. Year);
  D2. D := DMYtoDate (DT2. Day, DT2. Month, DT2. Year);

  D1. T := HMStoTime (DT1. Hour, DT1. Min, DT1. Sec);
  D2. T := HMStoTime (DT2. Hour, DT2. Min, DT2. Sec);

  If (D1.D > D2.D) Or ((D1.D = D2.D) And (D1.T > D2.T)) Then
    ExchangeStructs (D1, D2, SizeOf(DateTimeRec));

  {the difference in days is easy}
  Days := D2.D - D1.D;

  {difference in seconds}
  If D2.T < D1.T Then
  Begin
    {subtract one day, add 24 hours}
    Dec (Days);
    Inc (D2.T, SecondsInDay);
  End;

  Secs := D2.T - D1.T;

  HoursDiff := Days * 24 + Trunc (Secs / 3600);
End;

Function Date2Long (Const Date: String): LongInt;
Var
  Day, Month, Year, Err : SysInt;

Begin
  Val (Copy (Date, 1, 2), Month, Err);
  Val (Copy (Date, 4, 2), Day, Err);
  Val (Copy (Date, Length (Date) - 3, 4), Year, Err);

  Date2Long := iDate2Long (Day, Month, Year);
End;

Procedure Long2DT (Julian: LongInt; Var DT: DateTime);
Var
  i : LongInt;

Begin
  If Julian = BadDate Then
  Begin
    DT. Day := 0;
    DT. Month := 0;
    DT. Year := 0;
  End
  Else
    If Julian <= First2Months Then
    Begin
      DT. Year := MinYear;
      If Julian <= 30 Then
      Begin
        DT. Month := 1;
        DT. Day := Succ (Julian);
      End Else
      Begin
        DT. Month := 2;
        DT. Day := Julian - 30;
      End;
    End Else
    Begin
      i := (4 * LongInt (Julian - First2Months)) - 1;
      DT. Year := i Div 1461;
      i := (5 * ((i Mod 1461) Div 4)) + 2;
      DT. Month := i Div 153;
      DT. Day := ((i Mod 153) + 5) Div 5;
      If DT. Month < 10 Then
        Inc (DT. Month, 3)
      Else
      Begin
        Dec (DT. Month, 9);
        Inc (DT. Year);
      End;
      Inc (DT. Year, MinYear);
    End;
End;

Function Long2Date (Julian: LongInt; Const Mask: String): String;
Var
  DT : DateTime;

Begin
  Long2DT (Julian, DT);
  Long2Date := FormattedDate (DT, Mask);
End;

Function GetDosDate: LongInt;
Var
{$IFNDEF VirtualPascal}
  Result : LongInt;
{$ENDIF}
  DT     : DateTime;

Begin
  GetDateTime (DT);
  PackTime (DT, Result);
{$IFNDEF VirtualPascal}
  GetDosDate := Result;
{$ENDIF}
End;

Function Time2Long (Const Time: String): LongInt;
Var
  iMin, iHour, iSec : Word;
  Err               : SysInt;

Begin
  Val (Copy (Time, 1, 2), iHour, Err);
  Val (Copy (Time, 4, 2), iMin, Err);
  Val (Copy (Time, Length (Time) - 1, 2), iSec, Err);
  Time2Long := LongInt (iHour) * SecondsInHour + iMin * SecondsInMinute + iSec;
End;

Function MidSec: LongInt;
Var
  Hour, Min, Sec, Sec100 : Word;
{$IFNDEF VirtualPascal}
  Result : LongInt;
{$ENDIF}

Begin
  GetTime (Hour, Min, Sec, Sec100);
  Result := Hour;
  Result := (Result * SecondsInHour) + (Min * SecondsInMinute) + Sec;
{$IFNDEF VirtualPascal}
  MidSec := Result;
{$ENDIF}
End;

Function TimeDiff (T1, T2: LongInt): LongInt;
Begin
  If T2 < T1 Then
  Begin
    If (T1 - T2) < SecondsInHour Then
      TimeDiff := 0
    Else
      TimeDiff := T2 + SecondsInDay - T1;
  End Else
  Begin
    If (T2 - T1) > (SecondsInDay - SecondsInHour) Then
      TimeDiff := 0
    Else
      TimeDiff := T2 - T1;
  End;
End;

Function HowTime (Time: LongInt): String;
Var
  Hour, Min, Sec : Integer;

Begin
  Sec := Time Mod 60;
  Min := Time Div 60;
  Hour := Min Div 60;
  Dec (Min, Hour * 60);
  HowTime := LeftPadCh (Long2Str (Hour), '0', 2) + ':' +
             LeftPadCh (Long2Str (Min), '0', 2) + ':' +
             LeftPadCh (Long2Str (Sec), '0', 2);
End;

Function FormattedCurrDT (Const Mask: String): String;
Var
  DT : DateTime;

Begin
  GetDateTime (DT);
  FormattedCurrDT := FormattedDate (DT, Mask);
End;

Function StrDate: String;
Var
  D, M, Y, DOw : Word;

Begin
  GetDate (Y, M, D, DOw);
  StrDate := LeftPadCh (Long2Str (D), '0', 2) + '-' +
             LeftPadCh (Long2Str (M), '0', 2) + '-' + Long2Str (Y);
End;

Function StrTime: String;
Var
  h, m, s, s100 : Word;

Begin
  GetTime (h, m, s, s100);
  StrTime := LeftPadCh (Long2Str (h), '0', 2) + ':' +
             LeftPadCh (Long2Str (m), '0', 2) + ':' +
             LeftPadCh (Long2Str (s), '0', 2);
End;

Function ShortStrTime: String;
Var
  h, m, s, s100 : Word;

Begin
  GetTime (h, m, s, s100);
  ShortStrTime := LeftPadCh (Long2Str (h), '0', 2) + ':' +
                  LeftPadCh (Long2Str (m), '0', 2);
End;

Function CompareDates (Const S1, S2: String): Byte;
Var
  T1, T2 : LongInt;
  Tmp    : String [10];

Begin
  Tmp := ExtractWord (1, S1, SpaceOnly);
  T1 := Str2Long (ExtractWord (3, Tmp, MinusOnly)) Shl 9 +
        Str2Long (ExtractWord (2, Tmp, MinusOnly)) Shl 5 +
        Str2Long (ExtractWord (1, Tmp, MinusOnly));

  Tmp := ExtractWord (1, S2, SpaceOnly);
  T2 := Str2Long (ExtractWord (3, Tmp, MinusOnly)) Shl 9 +
        Str2Long (ExtractWord (2, Tmp, MinusOnly)) Shl 5 +
        Str2Long (ExtractWord (1, Tmp, MinusOnly));

  If T1 = T2 Then
  Begin
    T1 := 0;
    T2 := 0;

    Tmp := ExtractWord (2, S1, SpaceOnly);
    If Tmp <> '' Then
      T1 := Str2Long (ExtractWord (1, Tmp, [':'])) Shl 6 +
            Str2Long (ExtractWord (2, Tmp, [':']));

    Tmp := ExtractWord (2, S2, SpaceOnly);
    If Tmp <> '' Then
      T2 := Str2Long (ExtractWord (1, Tmp, [':'])) Shl 6 +
            Str2Long (ExtractWord (2, Tmp, [':']));
  End;

  If T1 > T2 Then
    CompareDates := 1
  Else
    If T1 < T2 Then
      CompareDates := 2
    Else
      CompareDates := 0;
End;

Function CompareDT (Const D1, D2: DateTime): Byte;
Var
  T1, T2 : LongInt;

Begin
  T1 := LongInt (D1. Year) Shl 9 +
        D1. Month Shl 5 +
        D1. Day;
  T2 := LongInt (D2. Year) Shl 9 +
        D2. Month Shl 5 +
        D2. Day;

  If T1 = T2 Then
  Begin
    T1 := LongInt (D1. Hour) Shl 12 +
          D1. Min Shl 6 +
          D1. Sec;
    T2 := LongInt (D2. Hour) Shl 12 +
          D2. Min Shl 6 +
          D2. Sec;
  End;

  If T1 > T2 Then
    CompareDT := 1
  Else
    If T1 < T2 Then
      CompareDT := 2
    Else
      CompareDT := 0;
End;

Function ExtractDT (Const sDate, DateMask: String; Var DT: DateTime): Boolean;
Var
  Err                : SysInt;
  i                  : Integer;
  j, W               : Word;
  Delim              : Char;
  M                  : String [4];
  DatePart, MaskPart : String [10];

Begin
  ExtractDT := False;

  FillChar (DT, SizeOf (DT), 0);
  MaskPart := ExtractWord (1, DateMask, SpaceOnly);
  DatePart := ExtractWord (1, sDate, SpaceOnly);

  If Pos ('-', MaskPart) <> 0 Then
    Delim := '-'
  Else
    If Pos ('/', MaskPart) <> 0 Then
      Delim := '/'
    Else
      If Pos ('.', MaskPart) <> 0 Then
        Delim := '.'
      Else
        Exit;

  W := WordCount (MaskPart, [Delim]);

  For i := 1 To W Do
  Begin
    Val (Trim (ExtractWord (i, DatePart, [Delim])), j, Err);
    If Err <> 0 Then
      Exit;

    M := ExtractWord (i, MaskPart, [Delim]);
    Case UpCase (M [1]) Of
      'D' : DT. Day := j;
      'M' : DT. Month := j;
      'Y' : Begin
              DT. Year := j;
              If DT. Year < 100 Then
                If DT. Year > 80 Then Inc (DT. Year, 1900)
                                 Else Inc (DT. Year, 2000);
            End;
    Else
      Exit;
    End;
  End;

  MaskPart := ExtractWord (2, DateMask, SpaceOnly);
  DatePart := ExtractWord (2, sDate, SpaceOnly);

  If (MaskPart <> '') And (DatePart <> '') Then
  Begin
    If Pos (':', MaskPart) <> 0 Then
      Delim := ':'
    Else
      If Pos ('.', MaskPart) <> 0 Then
        Delim := '.'
      Else
        Exit;

    W := WordCount (MaskPart, [Delim]);

    For i := 1 To W Do
    Begin
      Val (Trim (ExtractWord (i, DatePart, [Delim])), j, Err);
      If Err <> 0 Then
        Exit;

      M := ExtractWord (i, MaskPart, [Delim]);
      Case UpCase (M [1]) Of
        'H' : DT. Hour := j;
        'I' : DT. Min := j;
        'S' : DT. Sec := j;
      Else
        Exit;
      End;
    End;
  End;

  ExtractDT := True;
End;

Function DateMaskMatch (Const Date, Mask: String): Boolean;
Var
  Err : SysInt;
  i   : Integer;
  Tmp : Word;

Begin
  DateMaskMatch := False;
  If Length (Date) <> Length (Mask) Then
    Exit;

  Val (Copy (Date, Pos ('DD', Mask), 2), Tmp, Err);
  If (Err <> 0) Or (Tmp = 0) Or (Tmp > 31) Then
    Exit;

  Val (Copy (Date, Pos ('MM', Mask), 2), Tmp, Err);
  If (Err <> 0) Or (Tmp = 0) Or (Tmp > 12) Then
    Exit;

  i := Pos ('YY', Mask);
  If Copy (Mask, i + 2, 2) = 'YY' Then
  Begin
    Val (Copy (Date, i, 4), Tmp, Err);
    If (Err <> 0) Or (Tmp = 0) Then
      Exit;
  End Else
  Begin
    Val (Copy (Date, i, 2), Tmp, Err);
    If Err <> 0 Then
      Exit;
  End;

  For i := 1 To Length (Mask) Do
    If Not (Mask [i] in ['D', 'M', 'Y']) Then
      If Date [i] <> Mask [i] Then
        Exit;

  DateMaskMatch := True;
End;

Function DateMatch (Const Date, Mask: String): Boolean;
Var
  i : Integer;

Begin
  For i := 1 To Length (Mask) Do
    If Mask [i] <> '?' Then
      If Mask [i] <> Date [i] Then
      Begin
        DateMatch := False;
        Exit;
      End;

  DateMatch := True;
End;

Procedure GregorianToJulianDN (Year, Month, Day: Integer; Var JulianDN: LongInt);
Var
  Century, XYear : LongInt;

Begin
  If Month <= 2 Then
  Begin
    Dec (Year);
    Month := Month + 12;
  End;

  Month := Month - 3;
  Century := Year Div 100;
  XYear := Year Mod 100;
  Century := (Century * D1) Shr 2;
  XYear := (XYear * D0) Shr 2;
  JulianDN := ((((Month * 153) + 2) Div 5) + Day) + D2 + XYear + Century;
End;

Procedure JulianDNToGregorian (JulianDN: LongInt; Var Year, Month, Day : Word);
Var
  Temp, XYear         : LongInt;
  YYear, YMonth, YDay : Integer;

Begin
  Temp := (((JulianDN - D2) Shl 2) - 1);
  XYear := (Temp Mod D1) Or 3;
  JulianDN := Temp Div D1;
  YYear := (XYear Div D0);
  Temp := ((((XYear Mod D0) + 4) Shr 2) * 5) - 3;
  YMonth := Temp Div 153;
  If YMonth >= 10 Then
  Begin
    Inc (YYear);
    YMonth := YMonth - 12;
  End;

  YMonth := YMonth + 3;
  YDay := Temp Mod 153;
  YDay := (YDay + 5) Div 5;
  Year := YYear + (JulianDN * 100);
  Month := YMonth;
  Day := YDay;
End;

Function ToUnixDate (FDate: LongInt): LongInt;
Var
  DT : DateTime;

Begin
  UnpackTime (FDate, DT);
  ToUnixDate := DateTime2UnixDate (DT);
End;

Function DateTime2UnixDate (DT: DateTime): LongInt;
Var
  DateNum : LongInt;

Begin
  GregorianToJulianDN (DT. Year, DT. Month, DT. Day, DateNum);
  DateTime2UnixDate := (DateNum - c1970) * SecondsInDay + LongInt (DT. Hour) *
    SecondsInHour + DT. Min * SecondsInMinute + DT. Sec;
End;

Function GetUnixDate;
Var
  DateNum                 : LongInt;
  DTYear, DTMonth, DTDay,
  DTHour, DTMin, DTSec,
  DOW, Sec100             : Word;

Begin
  GetDate (DTYear, DTMonth, DTDay, DOW);
  GetTime (DTHour, DTMin, DTSec, Sec100);
  GregorianToJulianDN (DTYear, DTMonth, DTDay, DateNum);
  GetUnixDate := (DateNum - c1970) * SecondsInDay + LongInt (DTHour) *
    SecondsInHour + DTMin * SecondsInMinute + DTSec;
End;

Function UnixDate2Time (UnixDate: LongInt): String;
Var
  DT : DateTime;

Begin
  UnixDate := UnixDate Mod SecondsInDay;
  DT. Hour := UnixDate Div SecondsInHour;
  UnixDate := UnixDate Mod SecondsInHour;
  DT. Min := UnixDate Div SecondsInMinute;
  DT. Sec := UnixDate Mod SecondsInMinute;
  UnixDate2Time := LeftPadCh (Long2Str (DT. Hour), '0', 2) + ':' +
                   LeftPadCh (Long2Str (DT. Min), '0', 2) + ':' +
                   LeftPadCh (Long2Str (DT. Sec), '0', 2);
End;

Function UnixDate2Date (UnixDate: LongInt): String;
Var
  Year, Month, Day : Word;
  YStr             : String [4];

Begin
  JulianDNToGregorian ((UnixDate Div SecondsInDay) + c1970, Year, Month, Day);
  Str (Year, YStr);
  UnixDate2Date := LeftPadCh (Long2Str (Month), '0', 2) + '-' +
                   LeftPadCh (Long2Str (Day), '0', 2) + '-' +
                   LeftPadCh (Copy (YStr, Length (YStr) - 1, 2), '0', 2);
End;

Function FromUnixDate (UnixDate: LongInt): LongInt;
Var
  DT : DateTime;

Begin
  UnixDate2DateTime (UnixDate, DT);
  PackTime (DT, UnixDate);
  FromUnixDate := UnixDate;
End;

Procedure UnixDate2DateTime (UnixDate: LongInt; Var DT: DateTime);
Begin
  JulianDNToGregorian ((UnixDate Div SecondsInDay) + c1970, Word (DT. Year),
    Word (DT. Month), Word (DT. Day));
  UnixDate := UnixDate Mod SecondsInDay;
  DT. Hour := UnixDate Div SecondsInHour;
  UnixDate := UnixDate Mod SecondsInHour;
  DT. Min := UnixDate Div SecondsInMinute;
  DT. Sec := UnixDate Mod SecondsInMinute;
End;

Function DateL;
Var
  D, M, Y, DOw : SysInt;

Begin
  GetDate (Y, M, D, DOw);
  DateL := iDate2Long (D, M, Y);
End;

Procedure GetDateTime (Var DT: DateTime);
Var
  Cargo : Word;

Begin
  GetDate (DT. Year, DT. Month, DT. Day, Cargo);
  GetTime (DT. Hour, DT. Min, DT. Sec, Cargo);
End;

Function Word2Time (W: Word): String;
Begin
  Word2Time := LeftPadCh (Long2Str (Hi (W)), '0', 2) + ':' +
               LeftPadCh (Long2Str (Lo (W)), '0', 2);
End;

Function Time2Word (Const S: String): Word;
Begin
  Time2Word := Str2Long (ExtractWord (1, S, [':'])) Shl 8 +
               Str2Long (ExtractWord (2, S, [':']));
End;

Function CurrTime2Word: Word;
Var
  h, m, s, s100 : Word;

Begin
  GetTime (h, m, s, s100);
  CurrTime2Word := (h Shl 8) + m;
End;

Function PackStrTime (Const Date, Mask: String): LongInt;
Var
{$IFNDEF VirtualPascal}
  Result : LongInt;
{$ENDIF}
  DT     : DateTime;
  D      : String;

Begin
  D := ReFormatDate (Date, Mask, 'DD-MM-YYYY');
  FillChar (DT, SizeOf (DT), 0);
  DT. Day := Str2Long (Copy (D, 1, 2));
  DT. Month := Str2Long (Copy (D, 4, 2));
  DT. Year := Str2Long (Copy (D, 7, 4));
  PackTime (DT, Result);
{$IFNDEF VirtualPascal}
  PackStrTime := Result;
{$ENDIF}
End;

Function Time2Str (T: tTime): String;
Begin
  Time2Str := LeftPadCh (Long2Str (T Div MinInHour), '0', 2) + ':' +
              LeftPadCh (Long2Str (T Mod MinInHour), '0', 2);
End;

Function Str2Word (S: String; Var I: Word): Boolean;
Var
  Code : SysInt;
  SLen : Integer;

Begin
  S := Trim (S);
  SLen := Length (S);

  If (SLen > 1) And (UpCase (S [SLen]) = 'H') Then
  Begin
    Move (S [1], S [2], SLen - 1);
    S [1] := '$';
  End
  Else
    If (SLen > 2) And (S [1] = '0') And (UpCase (S [2]) = 'X') Then
    Begin
      Dec (SLen);
      Move (S [3], S [2], SLen - 1);
      S [1] := '$';
    End;

  Val (S, i, Code);
  If Code <> 0 Then
  Begin
    i := Code;
    Str2Word := False;
  End
  Else
    Str2Word := True;
End;

Function CurTime: tTime;
Var
  H, M, S, Cargo : Word;

Begin
  GetTime (H, M, S, Cargo);
  CurTime := H * MinInHour + M;
End;

Function ValidDate (DT: DateTime): Boolean;
Const
  DOM: Array [1..12] Of Byte =
       (31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31);

{$IFNDEF VirtualPascal}
Var
  Result : Boolean;
{$ENDIF}

Begin
  Result := True;

  If ((DT. Month < 1) Or (DT. Month > 12)) Then
    Result := False
  Else
    If ((DT. Day < 1) Or (DT. Day > DOM [DT. Month])) Then
      Result := False
    Else
      If (DT. Day = 29) And (DT. Month = 2) Then
        If Not IsLeapYear (DT. Year) Then
          Result := False;

{$IFNDEF VirtualPascal}
  ValidDate := Result;
{$ENDIF}
End;

Function TimePeriod2Str (Const TPer: tTimePeriod): String;
Begin
  With TPer Do
    TimePeriod2Str := Copy (sDOW [DayType (rDOW And $F)], 1, 3) + '─' +
                      Copy (sDOW [DayType (rDOW ShR 4) ], 1, 3) + ' / ' +
                      Time2Str (rtBegin) + '-' + Time2Str (rtEnd);
End;

Function MatchTimePeriod (Const TPer: tTimePeriod): Boolean;
Var
  w, DoW : Word;
  cT     : tTime;

  Function MatchDay: Boolean;
  Begin
    With TPer Do
      {День пеpвой гpаницы > Дня втоpой гpаницы 6.00:00-3.22:10}
      If (rDOW And $F) > (rDOW ShR 4) Then
        MatchDay := (DoW <= (rDOW ShR 4)) Or (DoW >= (rDOW And $F))
      Else
        {День пеpвой гpаницы <= Дню втоpой гpаницы}
        MatchDay := ((rDOW And $F) <= DoW) And {тек.день >= нач.пеpиода _И_}
                    ((rDOW ShR 4) >= DoW);     {тек.день <= кон.пеpиода}
  End;

  Function MatchTime: Boolean;
  Begin
    With TPer Do
      {Если вpемя пеpвой гpаницы > вpемени втоpой гpаницы 21:00-03:00 }
      If rtBegin > rtEnd Then
        MatchTime := (cT >= rtBegin) Or (cT <= rtEnd)
      Else
        MatchTime := (cT >= rtBegin) And (cT <= rtEnd);
  End;

Begin
  GetDate (w, w, w, DoW);
  cT := CurTime;
  MatchTimePeriod := MatchTime And MatchDay;
End;

Function MatchTimeArray (Const TA: TimeArray): Boolean;
Var
  i : Integer;

Begin
  For i := 1 To TA. nTPer Do
    If MatchTimePeriod (TA. TPer [i]) Then
    Begin
      MatchTimeArray := True;
      Exit;
    End;

  MatchTimeArray := False;
End;

Function Str2Time (Const S: String): tTime;
Var
  H, M, P : Word;

Begin
  Str2Time := BadTime;

  P := Pos (':', S);
  If (P >= 2) And (Length (S) >= 4) Then
    If Str2Word (Copy (S, 1, P - 1), H) And
       Str2Word (Copy (S, P + 1, 2), M)
    Then
      If (H < 24) And (M < 60) Then
        Str2Time := H * MinInHour + M;
End;

Function Str2TimePeriod (S: String; Var TPer: tTimePeriod): Boolean;
Var
  i      : SysInt;
  P1, P2 : Integer;
  b      : Byte;
  S1     : String;

Begin
  FillChar (TPer, SizeOf (tTimePeriod), 0);

  With TPer Do
  Begin
    rDOW := $60;

    If UpString (S) = 'CM' Then
    Begin
      rtBegin := 0;
      rtEnd := MaxTime;
    End Else
    Begin
      P1 := Pos ('-', S);
      S1 := Copy (S, 1, P1 - 1);
      Delete (S, 1, P1);

      P1 := Pos ('.', S1);
      P2 := Pos ('.', S);

      If P2 <> 0 Then
      Begin
        Str2TimePeriod := False;

        Val (Copy (S1, 1, 1), b, i);
        If b > 7 Then
          Exit;

        If b = 7 Then
          b := 0;
        rDOW := b;

        Val (Copy (S, 1, 1), b, i);
        If b > 7 Then
          Exit;

        If b = 7 Then
          b := 0;
        rDOW := rDOW Or (b Shl 4);
      End;

      rtEnd := Str2Time (Copy (S, P2 + 1, 5));
      If rtEnd = BadTime Then
        rtEnd := MaxTime;

      rtBegin := Str2Time (Copy (S1, P1 + 1, 5));
      If rtBegin = BadTime Then
        rtBegin := rtEnd;
    End;
  End;

  Str2TimePeriod := True;
End;

Function Str2TimeArray (Const S: String; Var TA: TimeArray): Boolean;
Var
  i : Integer;

Begin
  FillChar (TA, SizeOf (TA), #0);

  With TA Do
  Begin
    For i := 1 To WordCount (S, CommaOnly) Do
      If Str2TimePeriod (ExtractWord (i, S, CommaOnly),
         TPer [nTPer + 1]) Then
      Begin
        Inc (nTPer);
        If nTPer = 7 Then
          Break;
      End;

    Str2TimeArray := nTPer <> 0;
  End;
End;

Function Color2Byte (Const Color: String): Byte;
Const
  Colors : Array [0..15] Of String [12] =
    ('black', 'blue', 'green', 'cyan', 'red', 'magenta', 'brown',
     'lightgray', 'darkgray', 'lightblue', 'lightgreen', 'lightcyan',
     'lightred', 'lightmagenta', 'yellow', 'white');

Var
{$IFNDEF VirtualPascal}
  Result : Byte;
{$ENDIF}
  i      : Integer;
  S      : String [12];

Begin
  S := LoString (ExtractWord (1, Color, ['/']));
  Result := 7;

  For i := 0 To 15 Do
    If S = Colors [i] Then
    Begin
      Result := i;
      Break;
    End;

  S := LoString (ExtractWord (2, Color, ['/']));

  For i := 0 To 15 Do
    If S = Colors [i] Then
    Begin
      Inc (Result, i Shl 4);
      Break;
    End;

{$IFNDEF VirtualPascal}
  Color2Byte := Result;
{$ENDIF}
End;

Function YNA2Bool (YNA: AskType): Boolean;
Begin
  YNA2Bool := YNA in [atYes, atAsk];
End;

Function SymbolCount (Const S: String; Symbol: Char): Byte;
Var
  i      : Integer;
{$IFNDEF VirtualPascal}
  Result : Byte;
{$ENDIF}

Begin
  Result := 0;

  For i := 1 To Length (S) Do
    If S [i] = Symbol Then
      Inc (Result);

{$IFNDEF VirtualPascal}
  SymbolCount := Result;
{$ENDIF}
End;

Function PhoneValid (Const S: String): Boolean;
Begin
  If Length (S) < 4 Then
    PhoneValid := False
  Else
    PhoneValid := ConsistsOf (S, ['0'..'9', '-', '+', '(', ')', ' ', '[', ']']);
End;

Function EstimatedTransferTime (Size, CPS, Speed: LongInt): LongInt;
Begin
  If CPS <= 0 Then
    If Speed >= 10 Then CPS := Speed Div 10
                   Else CPS := 240;

  EstimatedTransferTime := Size Div CPS;
End;

Function To4D (Const S: String): String;
Var
  P      : Integer;
{$IFNDEF VirtualPascal}
  Result : String;
{$ENDIF}

Begin
  Result := S;
  P := Pos ('@', Result);
  If P > 0 Then
    SetLength (Result, P - 1);
  If Pos ('.', Result) = 0 Then
    Result := Result + '.0';
{$IFNDEF VirtualPascal}
  To4D := Result;
{$ENDIF}
End;

Function Addr2Str (Addr: TAddress): String;
{$IFNDEF VirtualPascal}
Var
  Result : String;
{$ENDIF}

Begin
  If (Addr. Zone = 0) And (Addr. Net = 0) And
     (Addr. Node = 0) And (Addr. Point = 0)
  Then
    Result := ''
  Else
  Begin
    Result := Long2Str (Addr. Zone) + ':' + Long2Str (Addr. Net) + '/' +
              Long2Str (Addr. Node);
    If Addr. Point > 0 Then
      Result := Result + '.' + Long2Str (Addr. Point);
  End;

{$IFNDEF VirtualPascal}
  Addr2Str := Result;
{$ENDIF}
End;

{Function PointlessAddrStr (Var Addr: Taddress): String;
Begin
  PointlessAddrStr := Long2Str (Addr. Zone) + ':' + Long2Str (Addr. Net) +
    '/' + Long2Str (Addr. Node);
End;}

{Function AddrEqual (Addr1: TAddress; Addr2: TAddress): Boolean;
Begin
  AddrEqual := ((Addr1. Zone = Addr2. Zone) And (Addr1. Net = Addr2. Net)
    And (Addr1. Node = Addr2. Node) And (Addr1. Point = Addr2. Point));
End;}

{Function IsValidAddr (Addr: TAddress): Boolean;
Begin
  IsValidAddr := Addr. Zone Or Addr. Net Or Addr. Node Or Addr. Point <> 0;
End;}

Procedure ParseStrAddr (Const StrAddr: String; Var Result: TAddress);
Var
  P1, P2, P3 : Integer;
  A          : String;

Begin
  A := To4D (StrAddr);
  P1 := Pos (':', A);
  P2 := Pos ('/', A);
  P3 := Pos ('.', A);

  With Result Do
  Begin
    Zone := Str2Long (Copy (A, 1, P1 - 1));
    Net := Str2Long (Copy (A, P1 + 1, P2 - P1 - 1));
    Node := Str2Long (Copy (A, P2 + 1, P3 - P2 - 1));
    Point := Str2Long (Copy (A, P3 + 1, 255));
  End;
End;

Function RelativeAddr (Const S: String; Addr: TAddress): String;
Var
  TmpAddr : TAddress;
  A       : String;

Begin
  A := Trim (S);
  If A [1] = '/' Then
    Delete (A, 1, 1);
  ParseStrAddr (A, TmpAddr);

  With TmpAddr Do
  Begin
    If (Zone = 0) And (Pos (':', A) = 0) Then
      Zone := Addr. Zone;
    If (Net = 0) And (Pos ('/', A) = 0) Then
      Net := Addr. Net;
    If (Node = 0) And ((A = '') Or (A [1] = '.')) Then
      Node := Addr. Node;
    If (Point = 0) And (A = '') Then
      Point := Addr. Point;
  End;

  RelativeAddr := Addr2Str (TmpAddr);
End;

{Function ParseAddr (Const AStr: String; CurrAddr: TAddress;
                    Var DestAddr: TAddress): Boolean;
Var
  SPos    : Word;
  Code    : SysInt;
  BadAddr : Boolean;
  A       : String;

Begin
  BadAddr := False;
  A := Trim (AStr);
  SPos := Pos ('@', A);
  If SPos > 0 Then
    SetLength (A, SPos - 1);
  SPos := Pos (':', A);
  If SPos > 0 Then
  Begin
    Val (Trim (Copy (A, 1, SPos - 1)), CurrAddr. Zone, Code);
    BadAddr := Code <> 0;
    Delete (A, 1, SPos);
  End;
  SPos := Pos ('/', A);
  If SPos > 0 Then
  Begin
    Val (Trim (Copy (A, 1, SPos - 1)), CurrAddr. Net, Code);
    BadAddr := BadAddr Or (Code <> 0);
    Delete (A, 1, SPos);
  End;
  SPos := Pos ('.', A);
  If SPos > 0 Then
  Begin
    Val (Trim (Copy (A, SPos + 1, Length (A))), CurrAddr. Point, Code);
    BadAddr := BadAddr Or (Code <> 0);
    SetLength (A, SPos - 1);
  End
  Else
    CurrAddr. Point := 0;
  A := Trim (A);
  If Length (A) > 0 Then
  Begin
    Val (A, CurrAddr. Node, Code);
    BadAddr := BadAddr Or (Code <> 0);
  End;
  If Not BadAddr Then
    DestAddr := CurrAddr;
  ParseAddr := Not BadAddr;
End;}

Procedure MBDateTime2DosDateTime (Const MBDateTime: TMessageBaseDateTime; Var DosDateTime: DateTime);
Begin
  DosDateTime. Hour   := MBDateTime. Hour;
  DosDateTime. Min    := MBDateTime. Min;
  DosDateTime. Sec    := MBDateTime. Sec;
  DosDateTime. Day    := MBDateTime. Day;
  DosDateTime. Month  := MBDateTime. Month;
  DosDateTime. Year   := MBDateTime. Year;
End;

Procedure DosDateTime2MBDateTime (Const DosDateTime: DateTime; Var MBDateTime: TMessageBaseDateTime);
Begin
  MBDateTime. Sec100 := 0;
  MBDateTime. Hour   := DosDateTime. Hour;
  MBDateTime. Min    := DosDateTime. Min;
  MBDateTime. Sec    := DosDateTime. Sec;
  MBDateTime. Day    := DosDateTime. Day;
  MBDateTime. Month  := DosDateTime. Month;
  MBDateTime. Year   := DosDateTime. Year;
End;

Function SplitString (Var S: String; Len: Byte): String;
Var
  WC     : Integer;
  W      : String;
{$IFNDEF VirtualPascal}
  Result : String;
{$ENDIF}

Begin
  If Length (S) <= Len Then
  Begin
    SplitString := S;
    S := '';
  End Else
  Begin
    Result := Copy (S, 1, Len);
    S := Copy (S, Len + 1, 255);
    If (Result [Len] <> ' ') And (S [1] <> ' ') Then
    Begin
      WC := WordCount (Result, SpaceOnly);
      If WC > 1 Then
      Begin
        W := ExtractWord (WC, Result, SpaceOnly);
        S := W + S;
        SetLength (Result, Len - Length (W));
      End;
    End;

  {$IFNDEF VirtualPascal}
    SplitString := Result;
  {$ENDIF}
  End;
End;

Function SplitStringPChar (Var Buf: PChar; Len: Byte): String;
Var
  WC     : Integer;
  W      : String;
  Tmp    : Array [0..255] Of Char;
{$IFNDEF VirtualPascal}
  Result : String;
{$ENDIF}

Begin
  If Buf = Nil Then
  Begin
    SplitStringPChar := '';
    Exit;
  End;

  If StrLen (Buf) <= Len Then
  Begin
    SplitStringPChar := StrPas (Buf);
    StrCopy (Buf, '');
  End Else
  Begin
    Result := StrPas (StrLCopy (@Tmp, Buf, Len));
    If (Result [Len] <> ' ') And (Buf [Len] <> ' ') Then
    Begin
      WC := WordCount (Result, SpaceOnly);
      If WC > 1 Then
      Begin
        W := ExtractWord (WC, Result, SpaceOnly);
        SetLength (Result, Len - Length (W));
      End;
    End;

    StrCopy (Buf, Buf + Length (Result));

  {$IFNDEF VirtualPascal}
    SplitStringPChar := Result;
  {$ENDIF}
  End;
End;

Function DelSpaces (Const S: String): String;
Var
  WC, i  : Integer;
  S1     : String;
{$IFNDEF VirtualPascal}
  Result : String;
{$ENDIF}

Begin
  S1 := Trim (S);

  If S1 <> '' Then
  Begin
    WC := WordCount (S1, SpaceOnly);
    Result := ExtractWord (1, S1, SpaceOnly);

    For i := 2 To WC Do
      Result := Result + ' ' + ExtractWord (i, S1, SpaceOnly);

  {$IFNDEF VirtualPascal}
    DelSpaces := Result;
  {$ENDIF}
  End
  Else
    DelSpaces := '';
End;

Procedure Str2Security (Const S: String; Var Security: System. Word; Var Flags: String);
Const
  Delims = [' ', ',', '|', ':', #0, #9];

Begin
  Security := Str2Long (ExtractWord (1, S, Delims));
  Flags := ExtractWord (2, S, Delims);
End;

Function MatchMultiCard (S1: String; Const Mask: String): Boolean;
Const
  Delims = [' ', ',', '|', ';'];

Var
  i, WC, Len : Integer;
  M          : String;

Begin
  If Pos ('.', S1) = 0 Then
    S1 := S1 + '.';
  Len := Length (S1);
  Move (S1 [1], S1 [0], Len);
  S1 [Len] := #0;
  WC := WordCount (Mask, Delims);

  For i := 1 To WC Do
  Begin
    M := ExtractWord (i, Mask, Delims);
    If (Pos ('*', M) = 0) And (Pos ('?', M) = 0) Then
      M := '*' + M + '*';
    Len := Length (M);
    Move (M [1], M [0], Len);
    M [Len] := #0;
    If MaskMatch (S1, M) Then
    Begin
      MatchMultiCard := True;
      Exit;
    End;
  End;

  MatchMultiCard := False;
End;

Function WordInString (Const W, S: String): Boolean;
Const
  Delims = [' ', ',', #0, #9];

Var
  i, WC  : Integer;
  W1, S1 : String;

Begin
  W1 := UpString (Trim (W));
  S1 := UpString (Trim (S));
  WC := WordCount (S1, Delims);

  For i := 1 To WC Do
    If W1 = ExtractWord (i, S1, Delims) Then
    Begin
      WordInString := True;
      Exit;
    End;

  WordInString := False;
End;

{$IFDEF MSDOS}
Procedure tmDelay (Ms: System. Word);
Var
  Hour, Min, Sec, Sec100 : Word;
  StartTime, EndTime     : Longint;

Begin
  GetTime (Hour, Min, Sec, Sec100);
  StartTime := (LongInt (Hour) * SecondsInHour + Min * SecondsInMinute + Sec) * 100 + Sec100;
  repeat
    GetTime (Hour, Min, Sec, Sec100);
    EndTime := (LongInt (Hour) * SecondsInHour + Min * SecondsInMinute + Sec) * 100 + Sec100;
  until TimeDiff (StartTime, EndTime) * 10 >= Ms;
End;
{$ENDIF}

Procedure Pause (Duration: LongInt);
Begin
  {$IFDEF MSDOS}
  {$IFNDEF DPMI32}
  If NTVDMInitOk Then
    NTVDMSleep (Duration)
  Else
  {$ENDIF}
  tmDelay (Duration);
  {$ENDIF}
  {$IFDEF OS2}
  DosSleep (Duration);
  {$ENDIF}
  {$IFDEF WIN32}
  SysCtrlSleep (Duration);
  {$ENDIF}
End;

Procedure PlaySound (Freq, Dur: System. Word);
{$IFDEF VirtualPascal}
Begin
  If SysPlatformID = 1 Then
  Begin
    Asm
       mov bx, Freq;
       mov ax, 34DDh;
       mov dx, 0012h;
       cmp dx, bx;
       jnb @stop;
       div bx;
       mov bx, ax;
       in al, 61h;
       test al, 3;
       jne @j1;
       or al, 3;
       out 61h, al;
       mov al, 0B6h;
       out 43h, al;
      @j1:
       mov al, bl;
       out 42h, al;
       mov al, bh;
       out 42h, al;
      @stop:
    End;
    SysCtrlSleep (Dur);
    Asm
       in al, 61h;
       and al, 0FCh;
       out 61h, al;
    End;
  End Else
    SysBeepEx (Freq, Dur);
End;
{$ELSE}
Begin
  Sound (Freq);
  Pause (Dur);
  NoSound;
End;
{$ENDIF}

Procedure SoundOf (Const S: String);
{$IFNDEF VirtualPascal}
Var
 i, Count, MaxCount,
 NoteCount, Rep, DelayTime : Word;
 j, Hz, HzInc              : Integer;
 S1                        : String;
{$ENDIF}

Begin
{$IFNDEF VirtualPascal}
  S1 := DelSpaces (S);
  NoteCount := WordCount (S1, SpaceOnly) - 1;
  Rep := Str2Long (ExtractWord (1, S1, SpaceOnly));

  For j := 1 To Rep Do
  Begin
    i := 0;

    Repeat
      Hz := Str2Long (ExtractWord (i + 2, S1, SpaceOnly));
      HzInc := Str2Long (ExtractWord (i + 3, S1, SpaceOnly));
      DelayTime := Str2Long (ExtractWord (i + 4, S1, SpaceOnly));
      MaxCount := Str2Long (ExtractWord (i + 5, S1, SpaceOnly));
      Count := 0;

      Repeat
        If Hz <> -1 Then Sound (Hz)
                    Else NoSound;
        Inc (Hz, HzInc);

        Pause (DelayTime);

        Inc (Count);
      Until Count = MaxCount;

      Inc (i, 4);
    Until i >= NoteCount;

    NoSound;
  End;
{$ELSE}
  PlaySound (600, 50);
{$ENDIF}
End;

Function GetInitials (Const Name: String; MaxLen: Integer): String;
Const
  UserNameDelims = [' ', ',', '.', '-'];

Var
  i, j   : Integer;
{$IFNDEF VirtualPascal}
  Result : String;
{$ENDIF}

Begin
  Result := '';
  j := WordCount (Name, UserNameDelims);
  If j > MaxLen Then
    j := MaxLen;

  For i := 1 To j Do
    Result := Result + Copy (ExtractWord (i, Name, UserNameDelims), 1, 1);

{$IFNDEF VirtualPascal}
  GetInitials := Result;
{$ENDIF}
End;

{$IFDEF MSDOS}
{$IFNDEF DPMI32}

{ The following part of code has been cut from
  Turbo Professional 5.21 (c) by TurboPower Software, 1987, 1992. }

Type
  PTextBuffer = ^TTextBuffer;
  TTextBuffer = Array [0..65520] of Byte;

  TText = Record
    Handle    : Word;
    Mode      : Word;
    BufSize   : Word;
    Priv      : Word;
    BufPos    : Word;
    BufEnd    : Word;
    BufPtr    : PTextBuffer;
    OpenProc  : Pointer;
    InOutProc : Pointer;
    FlushProc : Pointer;
    CloseProc : Pointer;
    UserData  : Array [1..16] of Byte;
    Name      : Array [0..79] of Char;
    Buffer    : Array [0..127] of Char;
  End;

Const
  FMClosed = $D7B0;
  FMInput  = $D7B1;
  FMOutput = $D7B2;
  FMInOut  = $D7B3;

Function TextSeek (Var F: Text; Target: LongInt): Boolean;
Var
  TF   : TText Absolute F;
  T    : Long Absolute Target;
  Pos  : LongInt;
  Regs : Registers;

Begin
  TextSeek := False;

  With Regs, TF Do
  Begin
    If Mode <> FMInput Then
      Exit;

    AX := $4201;
    BX := Handle;
    CX := 0;
    DX := 0;
    MsDos (Regs);
    If Odd (Flags) Then
      Exit;

    Long (Pos). HighWord := DX;
    Long (Pos). LowWord := AX;
    Dec (Pos, BufEnd);
    Pos := Target - Pos;
    If (Pos >= 0) And (Pos < BufEnd) Then
      BufPos := Pos
    Else
    Begin
      AX := $4200;
      BX := Handle;
      CX := T. HighWord;
      DX := T. LowWord;
      MsDos (Regs);
      If Odd (Flags) Then
        Exit;

      BufEnd := 0;
      BufPos := 0;
    End;
  End;

  TextSeek := True;
End;

Function TextPos (Var F: Text): LongInt;
Var
  TF       : TText Absolute F;
  Position : LongInt;
  Regs     : Registers;

Begin
  With Regs, TF Do
  Begin
    If Mode = FMClosed Then
    Begin
      TextPos := -1;
      Exit;
    End;

    AX := $4201;
    BX := Handle;
    CX := 0;
    DX := 0;
    MsDos (Regs);
    If Odd (Flags) Then
    Begin
      TextPos:= -1;
      Exit;
    End;

    Long (Position). HighWord := DX;
    Long (Position). LowWord := AX;
    If Mode = FMOutput Then
      Inc (Position, BufPos)
    Else
      If BufEnd <> 0 Then
        Dec (Position, BufEnd - BufPos);

    TextPos := Position;
  End;
End;

{$ENDIF}
{$ENDIF}

{$IFDEF VirtualPascal}

Function TextSeek (Var F: Text; Target: LongInt): Boolean;
Var
  T : TextRec Absolute F;
  P : LongInt;

Begin
  TextSeek := True;
  SysFileSeek (T. Handle, 0, 1, P);
  Dec (P, T. BufEnd);
  P := Target - P;

  If (P >= 0) And (P < T. BufEnd) Then
    T. BufPos := P
  Else
  Begin
    SysFileSeek (T. Handle, Target, 0, P);
    T. BufEnd := 0;
    T. BufPos := 0;
  End;
End;

Function TextPos (Var F: Text): LongInt;
Var
  T : TextRec Absolute F;

Begin
  SysFileSeek (T. Handle, 0, 1, Result);

  If T. Mode = fmOutput Then
    Inc (Result, T. BufPos)
  Else
    If T. BufEnd <> 0 Then
      Dec (Result, T. BufEnd - T. BufPos);
End;

{$ENDIF}

{$IFDEF DELPHI}

function TextSeek(var F: Text; Target: LongInt): Boolean;
 var
  T: TextRec absolute F;
  P: Longint;
 begin
  TextSeek:=True;

  P:=FileSeek(T.Handle, 0, 1);

  Dec(P, T.BufEnd);

  P:=Target - P;

  if (P >= 0) and (P < T.BufEnd) then
   T.BufPos:=P
  else
   begin
    FileSeek(T.Handle, Target, 0);

    T.BufEnd:=0;
    T.BufPos:=0;
   end;
 end;

function TextPos(var F: Text): LongInt;
 var
  T: TextRec absolute F;
 begin
  Result:=FileSeek(T.Handle, 0, 1);

  if T.Mode = fmOutput then
   Inc(Result, T.BufPos)
  else
   if T.BufEnd <> 0 then
    Dec(Result, T.BufEnd - T.BufPos);
 end;

{$ENDIF}

End.
