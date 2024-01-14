{$IFDEF MSDOS}
  {$IFNDEF DPMI32}
    {$F+}
  {$IFNDEF DPMI}
    {$O+}
  {$ENDIF}
  {$ENDIF}
{$ENDIF}
{$I-}

Unit Upgrader;

{*********************************************************}
{*                   UPGRADER.PAS                        *}
{*                                                       *}
{*  Copyright (c) Konstantin Klyagin, 1995-98,           *}
{*                exspecially for Tornado BBS System     *}
{*                                                       *}
{*********************************************************}

Interface

Procedure Upgrade;

Implementation

Uses
  tMisc,
  Parse,
  Log,
  Users,
  Areas,
  SysMsgs,
  TGlob,
  MainComm;

Type
  UURec = Record
    Date   : LongInt;
    OldSec : Word;
    Name   : String [36];
  End;

Procedure CheckTodayList;
Var
  DLong            : LongInt;
  F, FO            : File Of UURec;
  UUR              : UURec;
  TmpName          : String [36];
  UpgFile, TmpFile : String;

Begin
  UpgFile := Cnf. Path + 'upgrdat.tor';
  Assign (F, UpgFile);
  ReSet (F);
  If IOResult <> 0 Then
    Exit;

  DLong := DateL;
  TmpName := UpString (R. Name);

  While Not EoF (F) Do
  Begin
    Read (F, UUR);

    If UpString (UUR. Name) = TmpName Then
      If UUR. Date <> DLong Then
      Begin
        R. Security := UUR. OldSec;      {восстанавливаем security}
        ReadLimit (Lim, R. Security);
        R. TotalTime := Lim. Time * 60;

        TmpFile := Cnf. TempDir + 'upgrdat.$$$';
        Assign (FO, TmpFile);
        ReWrite (FO);
        Seek (F, 0);

        While Not EoF (F) Do
        Begin
          Read (F, UUR);
          If UpString (UUR. Name) <> TmpName Then
            Write (FO, UUR);
        End;
        Close (FO);

        tRenameFile (TmpFile, UpgFile);
        Break;
      End;
  End;

  Close (F);
End;

Function AddToTodayList: Boolean;
Var
  UUR : UURec;
  F   : File Of UURec;

Begin
  Assign (F, Cnf. Path + 'upgrdat.tor');
  System. FileMode := Open_Access_ReadWrite Or Open_Share_DenyWrite;
  ReSet (F);
  System. FileMode := Open_Access_ReadOnly Or Open_Share_DenyNone;
  If IOResult <> 0 Then
  Begin
    ReWrite (F);
    If IOResult <> 0 Then
    Begin
      AddToTodayList := False;
      Exit;
    End;
  End
  Else
    Seek (F, FileSize (F));

  UUR. Name := R. Name;
  UUR. Date := DateL;
  UUR. OldSec := R. Security;

  Write (F, UUR);
  Close (F);
  AddToTodayList := True;
End;

Procedure Upgrade;
Var
  i         : LongInt;
  Sec, oSec : Word;
  UR        : UpgradeRec;
  OK        : Boolean;
  CheckItem : String [10];

Label
  EoP;

Begin
  oSec := R. Security;

  CheckTodayList;
  If Not opUpgrades (Cnf. UpgraderCTL) Then
    Exit;

  i := DateL;
  If Long2Date (R. BirthDate, 'DDMM') =
     Long2Date (i, 'DDMM') Then
  Begin
    If (R. LastDate <> i) Or (R. NoCalls = 1) Then
    Begin
      If Not rUpgrade (-1, UR) Then
        If Not rUpgrade (R. NoCalls, UR) Then
          Exit;

      LogWrite ('+', sm (smUserBirthDay));
    End
    Else
      Exit;
  End
  Else
    If Not rUpgrade (R. NoCalls, UR) Then
      Goto EoP;

  OK := True;

  For i := 1 To WordCount (UR. Check, SpaceAndComma) Do
  Begin
    CheckItem := ExtractWord (i, UR. Check, SpaceAndComma);
    Sec := Str2Long (Copy (CheckItem, 2, 255));

    Case CheckItem [1] Of
      '!' : If R. Security = Sec Then
            Begin
              OK := False;
              Goto EoP;
            End;
      '>' : If R. Security <= Sec Then
            Begin
              OK := False;
              Goto EoP;
            End;
      '<' : If R. Security >= Sec Then
            Begin
              OK := False;
              Goto EoP;
            End;
    End;
  End;

  Case UR. Mode Of
    umForever : OK := True;
    umToday   : OK := AddToTodayList;
  End;

  If OK Then
  Begin
    Case UR. Security [1] Of
      '+' : Begin
              Inc (R. Security, Str2Long (Copy (UR. Security, 2, 255)));
              LogWrite ('+', sm (smSecRaiseBy) + Copy (UR. Security, 2, 255));
            End;

      '-' : Begin
              Dec (R. Security, Str2Long (Copy (UR. Security, 2, 255)));
              LogWrite ('+', sm (smSecDecBy) + Copy (UR. Security, 2, 255));
            End;
      Else
            Begin
              R. Security := Str2Long (UR. Security);
              LogWrite ('+', sm (smSecChangeTo) + UR. Security);
            End;
    End;

    If UR. InformVia = 'DISPLAY' Then
      EmuDispFile (UR. Filename)
    Else
      If FileExists (UR. FileName) Then
        PostFile (pmNew, UR. FileName, Str2Long (ExtractWord (2, UR. InformVia,
          BracketsOnly)), Cnf. SysOp, R. Name,
          'Message from the Tornado upgrade manager', '', '', MsgArea. Address,
          MsgArea. Address, 0, pfAutoOpen + pfUpgrader+pfUseDefaultAddr)
      Else
        LogWrite ('!', sm (smFile) + UR. FileName + sm (smNotFound));
  End
  Else
    LogWrite ('!', 'Can''t write file ' + Cnf. Path + 'upgrdat.tor');

EoP:
  pUpgradeDone;
  If R. Security <> oSec Then
    SetSecurity;
End;

End.
