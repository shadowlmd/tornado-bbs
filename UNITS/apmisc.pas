{$I-,S-}
{$IFNDEF OS2}
{$R-,V-,B-,F+,O+,A-}
{$ENDIF}

{Conditional defines that may affect this unit}
{$I APDEFINE.INC}

{*********************************************************}
{*                   APMISC.PAS 2.03                     *}
{*        Copyright (c) TurboPower Software 1991.        *}
{*                 All rights reserved.                  *}
{*********************************************************}

Unit ApMisc;
  {-Error codes, checksuming, date and other miscellaneous routines}

Interface

Uses
  DOS,
{$IFDEF DPMI}
  WinApi,
{$ENDIF}
  tMisc,
  ApSame,
  Crc;

Const
  indexAPMISC   = 0;
  indexAPPORT   = 100;
  indexAPTIMER  = 200;
  indexAPUART   = 300;
  indexAPFOSSIL = 400;
  indexAPDIGI14 = 500;
  indexAPINT14  = 600;
  indexAPCOM    = 700;

  indexAPARCHIV = 1000;
  indexAPZIP    = 1100;
  indexAPLZH    = 1200;

  indexAPINI    = 1300;
  indexAPINIDB  = 1400;
  indexAPMODDB  = 1500;
  indexAPMODEM2 = 1600;

Type
  {For accessing hi/lo words of a longint}
  LH =
  Record
    L, H : Word;
  End;

  {For internal date/time manipulations}
  Date = Word;
  Time = LongInt;
  DateTimeRec =
  Record
    D : Date;
    T : Time;
  End;
  DayType = (Sunday, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday);

  StringPtr = ^String;

  CharSet = Set Of Char;

Const
  MinYear  = 1900;
  MaxYear  = 2078;
  MinDate  = $0000;        {= 01/01/1900}
  MaxDate  = $FF62;        {= 12/31/2078}
  Date1900 = $0000;        {= 01/01/1900}
  Date1980 = $7223;        {= 01/01/1980}
  Date2000 = $8EAC;        {= 01/01/2000}
  BadDate  = $FFFF;

  Threshold2000 : Integer = 1900;

  MinTime = 0;               {= 00:00:00 am}
  MaxTime = 86399;           {= 23:59:59 pm}
  BadTime = $FFFFFFFF;
  First2Months = 58;         {1900 was not a leap year}
  FirstDayOfWeek = Monday;   {01/01/1900 was a Monday}

  SecondsInDay = 86400;      {number of seconds in a day}
  SecondsInHour = 3600;      {number of seconds in an hour}
  SecondsInMinute = 60;      {number of seconds in a minute}
  HoursInDay = 24;           {number of hours in a day}
  MinutesInHour = 60;        {number of minutes in an hour}

Const
  {Error types}
  etFatal          = 0;          {Fatal errors}
  etNonFatal       = 1;          {Non-fatal I/O errors}
  etWarning        = 2;          {Warning messages (currently not used)}
  etMessage        = 3;          {Status information (generally should
                                  not be acted on by an error handler)}

{$IFDEF DPMI}
{$IFDEF UsePmodeDLL}
function GlobalAllocCheck(var P; Flags : Word; Size : LongInt) : Boolean; Export;
procedure GlobalFreeCheck(var P); Export;
{$ELSE}
function GlobalAllocCheck(var P; Flags : Word; Size : LongInt) : Boolean;
procedure GlobalFreeCheck(var P);
{$ENDIF}
{$ENDIF}

Implementation

{$IFDEF DPMI}
  function GlobalAllocCheck(var P; Flags : Word; Size : LongInt) : Boolean;
    {-Allocate memory under DPMI, returning True if successful}
  var
    Pt : Pointer absolute P;
  begin
     Pt := GlobalAllocPtr(Flags, Size);
     GlobalAllocCheck := Pt <> nil;
   end;

  procedure GlobalFreeCheck(var P);
    {-Free memory under DPMI}
  var
    Pt : Pointer absolute P;
  begin
    if Pt <> nil then begin
      if GlobalFreePtr(Pt) <> 0 then ;
      Pt := nil;
    end;
  end;
{$ENDIF}

End.
