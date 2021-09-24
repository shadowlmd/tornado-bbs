{$IFDEF MSDOS}
  {$IFNDEF DPMI32}
    {$F+}
  {$IFNDEF DPMI}
    {$O+}
  {$ENDIF}
  {$ENDIF}
{$ENDIF}

Unit UserEd;

{*********************************************************}
{*                     USERED.PAS                        *}
{*                                                       *}
{*  Copyright (c) Vlad Bakaev, 1995-98,                  *}
{*  Copyright (c) Konstantin Klyagin, 1995-98,           *}
{*                exspecially for Tornado BBS System     *}
{*                                                       *}
{*********************************************************}

Interface

Uses
  DOS,
  tWin,
  Iface,
  Iline,
  TGlob,
  OpCrt,
  tMisc,
  SysMsgs,
  ApAbsPCL;

Function EditUser (Var User : tUser): Boolean;

Implementation

Procedure ValidDataBox (X, Y : LongInt);
Var
  oX, oY : Byte;

Begin
  oX := WhereX;
  oY := WhereY;
  HiddenCursor;
  CenterTempBox (Cnf. ColorScheme [cdFrame], Cnf. ColorScheme [cdButton],
    1, ' Value must be in range|' + Long2Str (X) + ' - ' + ' ' + Long2Str (Y) +
    '| ', ZoomSpeed, sm (smWarningWinTitle));
  NormalCursor;
  GotoXY (oX, oY);
End;

Function EditUser (Var User : tUser): Boolean;
Const
  Up       = 72;
  Down     = 80;
  Esc      = 27;
  CtrlHome = 119;
  CtrlEnd  = 117;

Var
  T                : SysInt;
  I, Keyb, oX, oY,
  BoolLength       : Byte;
  CH               : Char;
  Changes, Ex      : Boolean;
  FullBox          : PBoxRec;
  tEmu             : tEmulation;
  Temp, TempS      : String [80];

Label
  _Location, _Organization, _1st_address, _2nd_address, _3rd_address,
  _Password, _Home_phone, _Data_phone, _Birthdate, _MsgsPosted, _MsgsLoop,
  _Lines, _LinesLoop, _Language, _Protocol, _Emulation, _EmulationMenu,
  _Comment, _Downloads, _DownloadsLoop, _DownloadsK, _DownloadsKLoop, _TodayK,
  _TodayKLoop, _Uploads, _UploadsLoop, _UploadsK, _UploadsKLoop, _Pause,
  _HotKeys, _Finished_, _Flags, _Alias, _FSed;

Begin
  Changes := False;
  oX := WhereX;
  oY := WhereY;
  BoolLength := Length (BoolMsg [True]);
  i := Length (BoolMsg [False]);
  If i > BoolLength Then
    BoolLength := i;

  HiddenCursor;
  InitWindow (FullBox, 1, 1, 80, 21, 4, Cnf. ColorScheme [cdFrame],
    ' User: ' + User. Name + ' ', Cnf. ColorScheme [cdTitle], ZoomSpeed,
    True);
  DrawWindow (FullBox);

  FastWrite (sm (smueLocation), FullBox^. Y1 + 2, 3, Cnf. ColorScheme [cdText]);
  FastWrite (User. Location, FullBox^. Y1 + 2, 17, Cnf. ColorScheme [cdInput]);
  FastWrite (sm (smueOrganization), FullBox^. Y1 + 3, 3, Cnf. ColorScheme [cdText]);
  FastWrite (User. Organization, FullBox^. Y1 + 3, 17, Cnf. ColorScheme [cdInput]);
  FastWrite (sm (smue1stAddress), FullBox^. Y1 + 4,  3, Cnf. ColorScheme [cdText]);
  FastWrite (User. Address1, FullBox^. Y1 + 4, 17, Cnf. ColorScheme [cdInput]);
  FastWrite (sm (smue2ndAddress), FullBox^. Y1 + 5, 3, Cnf. ColorScheme [cdText]);
  FastWrite (User. Address2, FullBox^. Y1 + 5, 17, Cnf. ColorScheme [cdInput]);
  FastWrite (sm (smue3rdAddress), FullBox^. Y1 + 6,  3, Cnf. ColorScheme [cdText]);
  FastWrite (User. Address3, FullBox^. Y1 + 6, 17, Cnf. ColorScheme [cdInput]);
  FastWrite (sm (smueComment), FullBox^. Y1 + 7, 3, Cnf. ColorScheme [cdText]);
  FastWrite (User. Comment, FullBox^. Y1 + 7, 17, Cnf. ColorScheme [cdInput]);
  FastWrite (sm (smueAlias), FullBox^. Y1 + 8, 3, Cnf. ColorScheme [cdText]);
  FastWrite (User. Alias, FullBox^. Y1 + 8, 17, Cnf. ColorScheme [cdInput]);
  FastWrite (sm (smuePassword), FullBox^. Y1 + 9,  3, Cnf. ColorScheme [cdText]);
  FastWrite (User. Password, FullBox^. Y1 + 9, 17, Cnf. ColorScheme [cdInput]);

  FastWrite (sm (smueHomePhone), FullBox^. Y1 + 10, 3, Cnf. ColorScheme [cdText]);
  FastWrite (User. HPhone, FullBox^. Y1 + 10, 17, Cnf. ColorScheme [cdInput]);
  FastWrite (sm (smueDataPhone), FullBox^. Y1 + 11,  3, Cnf. ColorScheme [cdText]);
  FastWrite (User. BPhone, FullBox^. Y1 + 11, 17, Cnf. ColorScheme [cdInput]);
  FastWrite (sm (smueBirthdate), FullBox^. Y1 + 12, 3, Cnf. ColorScheme [cdText]);
  FastWrite (Long2Date (User. BirthDate, Cnf. DateMask), FullBox^. Y1 + 12, 17, Cnf. ColorScheme [cdInput]);
  FastWrite (sm (smueMsgsPosted), FullBox^. Y1 + 13,  3, Cnf. ColorScheme [cdText]);
  FastWrite (Long2Str (User. MsgsPosted), FullBox^. Y1 + 13, 17, Cnf. ColorScheme [cdInput]);
  FastWrite (sm (smueProtocol), FullBox^. Y1 + 14, 3, Cnf. ColorScheme [cdText]);
  FastWrite (User. Protocol, FullBox^. Y1 + 14, 17, Cnf. ColorScheme [cdInput]);
  FastWrite (sm (smueLines), FullBox^. Y1 + 15, 3, Cnf. ColorScheme [cdText]);
  FastWrite (Long2Str (User. Lines), FullBox^. Y1 + 15, 17, Cnf. ColorScheme [cdInput]);
  FastWrite (sm (smueLanguage), FullBox^. Y1 + 16, 3, Cnf. ColorScheme [cdText]);
  FastWrite (User. Lang, FullBox^. Y1 + 16, 17, Cnf. ColorScheme [cdInput]);
  FastWrite (sm (smueEmulation), FullBox^. Y1 + 17,  3, Cnf. ColorScheme [cdText]);
  FastWrite (Pad (EmuName [User. Emu], 6), FullBox^. Y1 + 17, 17, Cnf. ColorScheme [cdInput]);
  FastWrite (sm (smueFlags), FullBox^. Y1 + 18, 3, Cnf. ColorScheme [cdText]);
  FastWrite (Pad (UpString (User. Flags), 16), FullBox^. Y1 + 18, 17, Cnf. ColorScheme [cdInput]);

  FastWrite (sm (smueDownloads), FullBox^. Y1 + 8, 37, Cnf. ColorScheme [cdText]);
  FastWrite (Long2Str (User. Downloads), FullBox^. Y1 + 8, 51, Cnf. ColorScheme [cdInput]);
  FastWrite (sm (smueDownloadsK), FullBox^. Y1 + 9, 37, Cnf. ColorScheme [cdText]);
  FastWrite (Long2Str (User. DownloadsK), FullBox^. Y1 + 9, 51, Cnf. ColorScheme [cdInput]);
  FastWrite (sm (smueTodayK), FullBox^. Y1 + 10, 37, Cnf. ColorScheme [cdText]);
  FastWrite (Long2Str (User. TodayK), FullBox^. Y1 + 10, 51, Cnf. ColorScheme [cdInput]);
  FastWrite (sm (smueUploads), FullBox^. Y1 + 11, 37, Cnf. ColorScheme [cdText]);
  FastWrite (Long2Str (User. Uploads), FullBox^. Y1 + 11, 51, Cnf. ColorScheme [cdInput]);
  FastWrite (sm (smueUploadsK), FullBox^. Y1 + 12, 37, Cnf. ColorScheme [cdText]);
  FastWrite (Long2Str (User. UploadsK), FullBox^. Y1 + 12, 51, Cnf. ColorScheme [cdInput]);

  FastWrite (sm (smuePausing), FullBox^. Y1 + 13, 37, Cnf. ColorScheme [cdText]);
  FastWrite (BoolMsg [User. More], FullBox^. Y1 + 13, 51, Cnf. ColorScheme [cdInput]);
  FastWrite (sm (smueHotKeys), FullBox^. Y1 + 14, 37, Cnf. ColorScheme [cdText]);
  FastWrite (BoolMsg [User. HotKeys], FullBox^. Y1 + 14, 51, Cnf. ColorScheme [cdInput]);
  FastWrite (sm (smueFSeditor), FullBox^. Y1 + 15, 37, Cnf. ColorScheme [cdText]);
  FastWrite (BoolMsg [User. FSeditor], FullBox^. Y1 + 15, 51, Cnf. ColorScheme [cdInput]);

  (*------ Static Data ------*)
  FastWrite (sm (smueLastDate), FullBox^. Y1 + 16, 37, Cnf. ColorScheme [cdText]);
  FastWrite (Long2Date (User. LastDate, Cnf. DateMask), FullBox^. Y1 + 16, 51, Cnf. ColorScheme [cdText]);
  FastWrite (sm (smueFirstCall), FullBox^. Y1 + 17, 37, Cnf. ColorScheme [cdText]);
  FastWrite (Long2Date (User. FirstDate, Cnf. DateMask), FullBox^. Y1 + 17, 51, Cnf. ColorScheme [cdText]);
  FastWrite (sm (smueCalls), FullBox^. Y1 + 18, 37, Cnf. ColorScheme [cdText]);
  FastWrite (Long2Str (User. NoCalls), FullBox^. Y1 + 18, 51, Cnf. ColorScheme [cdText]);
  (*-------------------------*)

  NormalCursor;

_Location :
  User. Location := Input (17, FullBox^. Y1 + 2, User. Location, ' ', '', 50,
    0, AllChars, True, Keyb, Cnf. ColorScheme [cdInput], Changes);
  FastWrite (Pad (User. Location, 50), FullBox^. Y1 + 2, 17,
    Cnf. ColorScheme [cdInput]);
  Case Keyb Of
    Up, CtrlEnd : Goto _FSed;
    Esc         : Goto _Finished_;
    CtrlHome    : Goto _Location;
  End;

_Organization :
  User. Organization := Input (17, FullBox^. Y1 + 3, User. Organization, ' ',
    '', 50, 0, AllChars, True, Keyb, Cnf. ColorScheme [cdInput], Changes);
  FastWrite (Pad (User. Organization, 50), FullBox^. Y1 + 3, 17,
    Cnf. ColorScheme [cdInput]);
  Case Keyb Of
    Up, CtrlHome : Goto _Location;
    Esc          : Goto _Finished_;
    CtrlEnd      : Goto _FSed;
  End;

_1st_address :
  User. Address1 := Input (17, FullBox^. Y1 + 4, User. Address1, ' ', '', 50,
    0, AllChars, True, Keyb, Cnf. ColorScheme [cdInput], Changes);
  FastWrite (Pad (User. Address1, 50), FullBox^. Y1 + 4, 17,
    Cnf. ColorScheme [cdInput]);
  Case Keyb Of
    Up       : Goto _Organization;
    Esc      : Goto _Finished_;
    CtrlHome : Goto _Location;
    CtrlEnd  : Goto _FSed;
  End;

_2nd_address:
  User. Address2 := Input (17, FullBox^. Y1 + 5, User. Address2, ' ', '', 50,
    0, AllChars, True, Keyb, Cnf. ColorScheme [cdInput], Changes);
  FastWrite (Pad (User. Address2, 50), FullBox^. Y1 + 5, 17,
    Cnf. ColorScheme [cdInput]);
  Case Keyb Of
    Up       : Goto _1st_address;
    Esc      : Goto _Finished_;
    CtrlHome : Goto _Location;
    CtrlEnd  : Goto _FSed;
  End;

_3rd_address:
  User. Address3 := Input (17, FullBox^. Y1 + 6, User. Address3, ' ', '', 50,
    0, AllChars, True, Keyb, Cnf. ColorScheme [cdInput], Changes);
  FastWrite (Pad (User. Address3, 50), FullBox^. Y1 + 6, 17,
    Cnf. ColorScheme [cdInput]);
  Case Keyb Of
    Up       : Goto _2nd_address;
    Esc      : Goto _Finished_;
    CtrlHome : Goto _Location;
    CtrlEnd  : Goto _FSed;
  End;

_Comment:
  User. Comment := Input (17, FullBox^. Y1 + 7, User. Comment, ' ', '', 61, 0,
    AllChars, True, Keyb, Cnf. ColorScheme [cdInput], Changes);
  FastWrite (Pad (User. Comment, 61), FullBox^. Y1 + 7, 17,
    Cnf. ColorScheme [cdInput]);
  Case Keyb Of
    Up       : Goto _3rd_address;
    Esc      : Goto _Finished_;
    CtrlHome : Goto _Location;
    CtrlEnd  : Goto _FSed;
  End;

_Alias:
  User. Alias := Input (17, FullBox^. Y1 + 8, User. Alias, ' ', '', 15, 0,
    AllChars, True, Keyb, Cnf. ColorScheme [cdInput], Changes);
  FastWrite (Pad (User. Alias, 15), FullBox^. Y1 + 8, 17,
    Cnf. ColorScheme [cdInput]);
  Case Keyb Of
    Up       : Goto _Comment;
    Esc      : Goto _Finished_;
    CtrlHome : Goto _Location;
    CtrlEnd  : Goto _FSed;
  End;

_Password:
  User. Password := Input (17, FullBox^. Y1 + 9, User. Password, ' ', {'*'}'',
    16, 0, AllChars, True, Keyb, Cnf. ColorScheme [cdInput], Changes);
  FastWrite (Pad (User. Password, 15), FullBox^. Y1 + 9, 17,
    Cnf. ColorScheme [cdInput]);
  Case Keyb Of
    Up       : Goto _Alias;
    Esc      : Goto _Finished_;
    CtrlHome : Goto _Location;
    CtrlEnd  : Goto _FSed;
  End;

_Home_phone:
  User. HPhone := Input (17, FullBox^. Y1 + 10, User. HPhone, ' ', '', 15, 0,
    AllChars, True, Keyb, Cnf. ColorScheme [cdInput], Changes);
  FastWrite (Pad (User. HPhone, 15), FullBox^. Y1 + 10, 17,
    Cnf. ColorScheme [cdInput]);
  Case Keyb Of
    Up       : Goto _Password;
    Esc      : Goto _Finished_;
    CtrlHome : Goto _Location;
    CtrlEnd  : Goto _FSed;
  End;

_Data_phone:
  User. BPhone := Input (17, FullBox^. Y1 + 11, User. BPhone, ' ', '', 15, 0,
    AllChars, True, Keyb, Cnf. ColorScheme [cdInput], Changes);
  FastWrite (Pad (User. BPhone, 15), FullBox^. Y1 + 11, 17,
    Cnf. ColorScheme [cdInput]);
  Case Keyb Of
    Up       : Goto _Home_phone;
    Esc      : Goto _Finished_;
    CtrlHome : Goto _Location;
    CtrlEnd  : Goto _FSed;
  End;

_Birthdate:
  TempS := Input (17, FullBox^. Y1 + 12, Long2Date (User. BirthDate,
    Cnf. DateMask), ' ', '', Length (Cnf. DateMask), 0, ['-', '0'..'9'], True,
    Keyb, Cnf. ColorScheme [cdInput], Changes);
  User. BirthDate := Date2Long (ReFormatDate (TempS, Cnf. DateMask,
    DefaultDateMask));
  FastWrite (Pad (TempS, Length (Cnf. DateMask)), FullBox^. Y1 + 12, 17,
    Cnf. ColorScheme [cdInput]);
  Case Keyb Of
    Up       : Goto _Data_phone;
    Esc      : Goto _Finished_;
    CtrlHome : Goto _Location;
    CtrlEnd  : Goto _FSed;
  End;

_MsgsPosted:
  Str (User. MsgsPosted, TempS);

_MsgsLoop:
  TempS := Input (17, FullBox^. Y1 + 13, TempS, ' ', '', 10, 0, NumbersOnly,
    True, Keyb, Cnf. ColorScheme [cdInput], Changes);
  Val (TempS, User. MsgsPosted, T);
  Str (User. MsgsPosted, Temp);
  If Temp <> TempS Then
  Begin
    ValidDataBox (0, 2147483647);
    Goto _MsgsLoop;
  End;

  FastWrite (Pad (TempS, 10), FullBox^. Y1 + 13, 17,
    Cnf. ColorScheme [cdInput]);
  Case Keyb Of
    Up       : Goto _Birthdate;
    Esc      : Goto _Finished_;
    CtrlHome : Goto _Location;
    CtrlEnd  : Goto _FSed;
  End;

_Protocol:
  TempS := Input (17, FullBox^. Y1 + 14, User. Protocol, ' ', '', 1, 0,
    AllChars, False, Keyb, Cnf. ColorScheme [cdInput], Changes);
  User. Protocol := TempS [1];

  FastWrite (User. Protocol, FullBox^. Y1 + 14, 17, Cnf. ColorScheme [cdInput]);
  Case Keyb Of
    Up       : Goto _MsgsPosted;
    Esc      : Goto _Finished_;
    CtrlHome : Goto _Location;
    CtrlEnd  : Goto _FSed;
  End;

_Lines:
  Str (User. Lines, TempS);

_LinesLoop:
  TempS := Input (17, FullBox^. Y1 + 15, TempS, ' ', '', 3, 0, NumbersOnly,
    True, Keyb, Cnf. ColorScheme [cdInput], Changes);
  Val (TempS, User. Lines, T);
  Str (User. Lines, Temp);
  If Temp <> TempS Then
  Begin
    ValidDataBox (0, 255);
    Goto _LinesLoop;
  End;
  FastWrite (Pad (TempS, 3), FullBox^. Y1 + 15, 17,
    Cnf. ColorScheme [cdInput]);
  Case Keyb Of
    Up       : Goto _Protocol;
    Esc      : Goto _Finished_;
    CtrlHome : Goto _Location;
    CtrlEnd  : Goto _FSed;
  End;

_Language:
  User. Lang := Input (17, FullBox^. Y1 + 16, User. Lang, ' ', '', 8, 0,
    AllChars, True, Keyb, Cnf. ColorScheme [cdInput], Changes);
  FastWrite (Pad (User. Lang, 8), FullBox^. Y1 + 16, 17,
    Cnf. ColorScheme [cdInput]);
  Case Keyb Of
    Up       : Goto _Lines;
    Esc      : Goto _Finished_;
    CtrlHome : Goto _Location;
    CtrlEnd  : Goto _FSed;
  End;

_Emulation:
  GoToXY (17, FullBox^. Y1 + 17);
  FastWrite (Pad (EmuName [User. Emu], 6), FullBox^. Y1 + 17, 17,
    Cnf. ColorScheme [cdInput]);
  Ex := False;

  While Not Ex Do
  Begin
    If Not WaitForKey (CH) Then
      Ex := True;
    Case Ord (CH) Of
      {Tab}  9,
      {Esc} 27 : Begin
                   Keyb := Ord (CH);
                   Ex := True;
                 End;
      {CR } 13,
      {Spc} 32 : Goto _EmulationMenu;
             0 : Begin
                   If Not WaitForKey (CH) Then
                     Ex := True;
                   Keyb := Ord (CH);
                   Case Keyb Of
                     Up, Down, CtrlHome, CtrlEnd : Ex := True;
                   End;
                 End;
      End;
  End;

  FastWrite (Pad (EmuName [User. Emu], 6), FullBox^. Y1 + 17, 17,
    Cnf. ColorScheme [cdInput]);
  Case Keyb Of
    Up       : Goto _Language;
    Esc      : Goto _Finished_;
    CtrlHome : Goto _Location;
    CtrlEnd  : Goto _FSed;
  End;
  Goto _Flags;

_EmulationMenu:
  HiddenCursor;
  i := SelectBox (EmuName [teAnsi] + '|' + EmuName [teTty] + '|' +
    EmuName [teAvatar], Cnf. ColorScheme [cdFrame], Cnf. ColorScheme [cdButton],
    Cnf. ColorScheme [cdTitle], ZoomSpeed, 'Emulation:');
  NormalCursor;
  If i > 0 Then
  Begin
    Case i Of
      1 : tEmu := teAnsi;
      2 : tEmu := teTty;
      3 : tEmu := teAvatar;
    End;
    If tEmu <> User. Emu Then
    Begin
      Changes := True;
      User. Emu := tEmu;
    End;
  End;
  Goto _Emulation;

_Flags:
  User. Flags := UpString (Input (17, FullBox^. Y1 + 18, UpString (User. Flags),
    ' ', '', 16, 0, ['A'..'Z','a'..'z'], True, Keyb, Cnf. ColorScheme [cdInput],
    Changes));
  FastWrite (Pad (User. Flags, 16), FullBox^. Y1 + 18, 17,
    Cnf. ColorScheme [cdInput]);
  Case Keyb Of
    Up       : Goto _Emulation;
    Esc      : Goto _Finished_;
    CtrlHome : Goto _Location;
    CtrlEnd  : Goto _FSed;
  End;

_Downloads:
  Str (User. Downloads, TempS);

_DownloadsLoop:
  TempS := Input (51, FullBox^. Y1 + 8, TempS, ' ', '', 10, 0, NumbersOnly,
    True, Keyb, Cnf. ColorScheme [cdInput], Changes);
  Val (TempS, User. Downloads, T);
  Str (User. Downloads, Temp);
  If Temp <> TempS Then
  Begin
    ValidDataBox (0, 2147483647);
    Goto _DownloadsLoop;
  End;
  FastWrite (Pad (TempS, 10), FullBox^. Y1 + 8, 51, Cnf. ColorScheme [cdInput]);
  Case Keyb Of
    Up       : Goto _Flags;
    Esc      : Goto _Finished_;
    CtrlHome : Goto _Location;
    CtrlEnd  : Goto _FSed;
  End;

_DownloadsK:
  Str (User. DownloadsK, TempS);

_DownloadsKLoop:
  TempS := Input (51, FullBox^. Y1 + 9, TempS, ' ', '', 10, 0, NumbersOnly,
    True, Keyb, Cnf. ColorScheme [cdInput], Changes);
  Val (TempS, User. DownloadsK, T);
  Str (User. DownloadsK, Temp);
  If Temp <> TempS Then
  Begin
    ValidDataBox (0, 2147483647);
    Goto _DownloadsKLoop;
  End;
  FastWrite (Pad (TempS, 10), FullBox^. Y1 + 9, 51, Cnf. ColorScheme [cdInput]);
  Case Keyb Of
    Up       : Goto _Downloads;
    Esc      : Goto _Finished_;
    CtrlHome : Goto _Location;
    CtrlEnd  : Goto _FSed;
  End;

_TodayK:
  Str (User. TodayK, TempS);

_TodayKLoop:
  TempS := Input (51, FullBox^. Y1 + 10, TempS, ' ', '', 10, 0, NumbersOnly,
    True, Keyb, Cnf. ColorScheme [cdInput], Changes);
  Val (TempS, User. TodayK, T);
  Str (User. TodayK, Temp);
  If Temp <> TempS Then
  Begin
    ValidDataBox (0, 2147483647);
    Goto _TodayKLoop;
  End;
  FastWrite (Pad (TempS, 10), FullBox^. Y1 + 10, 51,
    Cnf. ColorScheme [cdInput]);
  Case Keyb Of
    Up       : Goto _DownloadsK;
    Esc      : Goto _Finished_;
    CtrlHome : Goto _Location;
    CtrlEnd  : Goto _FSed;
  End;

_Uploads:
  Str (User. Uploads, TempS);

_UploadsLoop:
  TempS := Input (51, FullBox^. Y1 + 11, TempS, ' ', '', 10, 0, NumbersOnly,
    True, Keyb, Cnf. ColorScheme [cdInput], Changes);
  Val (TempS, User. Uploads, T);
  Str (User. Uploads, Temp);
  If Temp <> TempS Then
  Begin
    ValidDataBox (0, 2147483647);
    Goto _UploadsLoop;
  End;
  FastWrite (Pad (TempS, 10), FullBox^. Y1 + 11, 51,
    Cnf. ColorScheme [cdInput]);
  Case Keyb Of
    Up       : Goto _TodayK;
    Esc      : Goto _Finished_;
    CtrlHome : Goto _Location;
    CtrlEnd  : Goto _FSed;
  End;

_UploadsK:
  Str (User. UploadsK, TempS);

_UploadsKLoop:
  TempS := Input (51, FullBox^. Y1 + 12, TempS, ' ', '', 10, 0, NumbersOnly,
    True, Keyb, Cnf. ColorScheme [cdInput], Changes);
  Val (TempS, User. UploadsK, T);
  Str (User. UploadsK, Temp);
  If Temp <> TempS Then
  Begin
    ValidDataBox (0, 2147483647);
    Goto _UploadsKLoop;
  End;
  FastWrite (Pad (TempS, 10), FullBox^. Y1 + 12, 51,
    Cnf. ColorScheme [cdInput]);
  Case Keyb Of
    Up       : Goto _Uploads;
    Esc      : Goto _Finished_;
    CtrlHome : Goto _Location;
    CtrlEnd  : Goto _FSed;
  End;

_Pause:
  GoToXY (51, FullBox^. Y1 + 13);
  ex := False;

  While Not ex Do
  Begin
    If Not WaitForKey (CH) Then
      Ex := True;
    Keyb := Ord (CH);
    Case Keyb Of
      32, 13 : Begin
                 User. More := Not User. More;
                 Changes := True;
                 FastWrite (Pad (BoolMsg [User. More], BoolLength),
                   FullBox^. Y1 + 13, 51, Cnf. ColorScheme [cdInput]);
               End;
      27, 9  : Ex := True;
      0      : Begin
                 If Not WaitForKey (CH) Then
                   Ex := True;
                 Keyb := Ord (CH);
                 If Keyb in [Up, Down, CtrlHome, CtrlEnd] Then
                   Ex := True;
               End;
    End;
  End;

  Case Keyb Of
    Up       : Goto _UploadsK;
    Esc      : Goto _Finished_;
    CtrlHome : Goto _Location;
    CtrlEnd  : Goto _FSed;
  End;

_HotKeys:
  GoToXY (51, FullBox^. Y1 + 14);
  ex := False;

  While Not ex Do
  Begin
    If Not WaitForKey (CH) Then
      Ex := True;
    Keyb := Ord (CH);
    Case Keyb Of
      32, 13 : Begin
                 User. HotKeys := Not User. HotKeys;
                 Changes := True;
                 FastWrite (Pad (BoolMsg [User. HotKeys], BoolLength),
                   FullBox^. Y1 + 14, 51, Cnf. ColorScheme [cdInput]);
               End;
      27, 9  : Ex := True;
      0      : Begin
                 If Not WaitForKey (CH) Then
                   Ex := True;
                 Keyb := Ord (CH);
                 If Keyb in [Up, Down, CtrlHome, CtrlEnd] Then
                   Ex := True;
               End;
    End;
  End;

  Case Keyb Of
    Up       : Goto _Pause;
    Esc      : Goto _Finished_;
    CtrlHome : Goto _Location;
  End;

_FSed:
  GoToXY (51, FullBox^. Y1 + 15);
  ex := False;

  While Not ex Do
  Begin
    If Not WaitForKey (CH) Then
      Ex := True;
    Keyb := Ord (CH);
    Case Keyb Of
      32, 13 : Begin
                 User. FSeditor := Not User. FSeditor;
                 Changes := True;
                 FastWrite (Pad (BoolMsg [User. FSeditor], BoolLength),
                   FullBox^. Y1 + 15, 51, Cnf. ColorScheme [cdInput]);
               End;
      27, 9  : Ex := True;
      0      : Begin
                 If Not WaitForKey (CH) Then
                   Ex := True;
                 Keyb := Ord (CH);
                 If Keyb in [Up, Down, CtrlHome, CtrlEnd] Then
                   Ex := True;
               End;
    End;
  End;

  Case Keyb Of
    Up      : Goto _HotKeys;
    Esc     : Goto _Finished_;
    CtrlEnd : Goto _FSed;
  End;
  Goto _Location;

_Finished_ :

  If Changes Then
  Begin
    HiddenCursor;

    i := SelectBox ('Yes|No', Cnf. ColorScheme [cdFrame],
      Cnf. ColorScheme [cdButton], Cnf. ColorScheme [cdTitle], ZoomSpeed,
      sm (smueSaveYN));

    If i = 0 Then
      Goto _Location;

    EditUser := (i = 1);
  End
  Else
    EditUser := False;

  CloseWindow (FullBox);
  GoToXY (oX, oY);
  NormalCursor;
End;

End.
