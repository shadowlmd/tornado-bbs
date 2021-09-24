{$IFDEF MSDOS}
  {$IFNDEF DPMI32}
    {$F+}
  {$IFNDEF DPMI}
    {$O+}
  {$ENDIF}
  {$ENDIF}
{$ENDIF}
{$B-}

Unit
  iEMSI;

Interface

Uses
  tMisc,
  Crc,
{$IFDEF OS2}
  Os2Base,
{$ENDIF}
  ApSame,
  DOS,
  ApCom,
  tWin,
  OpCrt,
  tGlob,
  SysMsgs,
  Log,
  MainComm,
  Parse;

Procedure DoIEMSI;

Procedure InitIEMSIBox;
Procedure AddIEMSIMsg (S: String);
Procedure DisplayParms;
Procedure DoneIEMSIBox;

Implementation

Type
  HexString = String [8];

Var
  IEMSIBox1, IEMSIBox2          : PBoxRec;
  EMSI_ICI, EMSI_ISI, Packet, S : String;
  C                             : Char;
  EMSI_Len, CRC_                : HexString;
  i, Tryes                      : Byte;

Const
  EMSI_ACK : String [15] = '**EMSI_ACKA490'#13;
  EMSI_NAK : String [15] = '**EMSI_NAKEEC3'#13;
  EMSI_IIR : String [15] = '**EMSI_IIR61E2'#13;

Function CRC16 (Str: String): LongInt;
Var
  Tmp   : LongInt;
  i     : Byte;

Begin
  Tmp := 0;
  For i := 1 To Length (Str) Do
  Tmp := UpdateCrc (Byte (Str [i]), tmp);
  CRC16 := Tmp;
End;

Procedure InitIEMSIBox;
Begin
  HiddenCursor;
  InitWindow (IEMSIBox1, 1, 2, 49, 8, 1, Cnf. ColorScheme [cdFrame],
              sm (smeHead1), Cnf. ColorScheme [cdTitle],
              ZoomSpeed, False);

  InitWindow (IEMSIBox2, 1, 10, 49, 18, 1, Cnf. ColorScheme [cdFrame],
              sm (smeHead2), Cnf. ColorScheme [cdTitle],
              ZoomSpeed, False);

  DrawWindow (IEMSIBox2);
  DrawWindow (IEMSIBox1);

  FastWrite (sm (smeUserName), 11, 4, Cnf. ColorScheme [cdText]);
  FastWrite (sm (smeLocation), 12, 4, Cnf. ColorScheme [cdFrame]);
  FastWrite (sm (smeDPhone), 13, 4, Cnf. ColorScheme [cdFrame]);
  FastWrite (sm (smeVPhone), 14, 4, Cnf. ColorScheme [cdFrame]);
  FastWrite (sm (smePassword), 15, 4, Cnf. ColorScheme [cdFrame]);
  FastWrite (sm (smeBirthdate), 16, 4, Cnf. ColorScheme [cdFrame]);
  FastWrite (sm (smeSoftware), 17, 4, Cnf. ColorScheme [cdFrame]);
End;

Procedure AddIEMSIMsg (S: String);
Var
  oAttr : Byte;

Begin
  oAttr := TextAttr;
  TextAttr := Cnf. ColorScheme [cdText];
  ScrollWindowUp (4, 3, 48, 7, 1);
  FastWrite (ShortStrTime + ' ' + S, 7, 4, Cnf. ColorScheme [cdText]);
  TextAttr := oAttr;
End;

Procedure DisplayParms;
Begin
  FastWrite (PadCh (R. Name, ' ', 30), 11, 18, Cnf. ColorScheme [cdInput]);
  FastWrite (PadCh (R. Location, ' ', 30), 12, 18, Cnf. ColorScheme [cdInput]);
  FastWrite (PadCh (R. BPhone, ' ', 30), 13, 18, Cnf. ColorScheme [cdInput]);
  FastWrite (PadCh (R. HPhone, ' ', 30), 14, 18, Cnf. ColorScheme [cdInput]);
  FastWrite (PadCh (EMSI. Password, ' ', 30), 15, 18, Cnf. ColorScheme [cdInput]);
  FastWrite (PadCh (Long2Date (R. BirthDate, Cnf. DateMask), ' ', 30), 16, 18,
    Cnf. ColorScheme [cdInput]);
  FastWrite (Copy (EMSI. Software, 1, 29), 17, 18, Cnf. ColorScheme [cdInput]);

  LogWrite ('+', sm (smeOk));
  LogWrite ('+', sm (smeUserName) + R. Name);
  LogWrite ('+', sm (smeLocation) + R. Location);
  LogWrite ('+', sm (smeDPhone) + R. BPhone);
  LogWrite ('+', sm (smeVPhone) + R. HPhone);
  LogWrite ('+', sm (smePassword) + EMSI. Password);
  LogWrite ('+', sm (smeBirthdate) + Long2Date (R. BirthDate, Cnf. DateMask));
  LogWrite ('+', sm (smeSoftware) + EMSI. Software);
End;

Procedure DoneIEMSIBox;
Begin
  CloseWindow (IEMSIBox2);
  CloseWindow (IEMSIBox1);
  NormalCursor;
  EMSI. Allowed := False;
End;

Procedure Parse_EMSI_ICI (Str: String);
Var
  i, k, j       : Byte;
  Temp          : String [70];
  Err           : SysInt;
  StrLines      : String [3];
  DT            : DateTime;

Begin
  i := 0;

  LogWrite ('&', Str);

  For k := 1 To 12 Do
  Begin
    Temp := '';
    Inc (i, 2);
    While Str [i] <> '}' Do
    Begin
      Temp := Temp + Str [i];
      Inc (i);
    End;

    Case k Of
       1 : Begin
             R. Name := AsciiCode2Str (Temp);
             R. Name := DelChars ([#0..#31, '!'..'+', '/'..'@', '['..'`',
             '{'..#127, #176..#223, #246..#255], R. Name);
             R. Name := DelSpaces (R. Name);
             If Cnf. CapitalizeNames Then R. Name := PrString (R. Name);
           End;

       3 : R. Location := AsciiCode2Str (Temp);
       4 : R. BPhone := Temp;
       5 : R. HPhone := Temp;
       6 : EMSI. Password := Temp;
       7 : Begin
             UnixDate2DateTime (Hex2Long (Temp), DT);
             R. BirthDate := Date2Long (LeftPadCh (Long2Str (DT. Month),
                             '0', 2) + '-' + LeftPadCh (Long2Str (DT. Day),
                             '0', 2) + '-' + Long2Str (DT. Year));
           End;

       8 : Begin
             If Pos ('TTY', Temp) <> 0 Then R. Emu := teTty
                                       Else R. Emu := teAnsi;
             j := Pos (',', Temp) + 1; StrLines := '';
             While Temp [j] <> ',' Do
             Begin
               StrLines := StrLines + Temp [j];
               Inc (j);
             End;
             Val (StrLines, R. Lines, Err);
             If ((R. Lines = 0) Or (Err <> 0)) Then R. Lines := 24;
           End;

      11 : Begin
             If Pos ('MORE', Temp) <> 0 Then R. More := True
                                        Else R. More := False;
             If Pos ('MAIL', Temp) <> 0 Then EMSI. CheckMail := True
                                        Else EMSI. CheckMail := False;
             If Pos ('FILE', Temp) <> 0 Then EMSI. CheckNewFiles := True
                                        Else EMSI. CheckNewFiles := False;
             If Pos ('HOT', Temp) <> 0  Then R. HotKeys := True
                                        Else R. HotKeys := False;
           End;
      12 : EMSI. Software := AsciiCode2Str (Temp);
    End;
  End;
  DisplayParms;
End;

Procedure DoIEMSI;

Label
  Loop, ISI;

Begin
  Tryes := 0;
  InitIEMSIBox;
  AddIEMSIMsg (sm (smeStart));
  LogWrite ('+', sm (smeStart));

  Loop:

  If Tryes > 3 Then
  Begin
    R. Name := '';
    EMSI. Session := False;
    EMSI. Allowed := False;
    PutStringTimeOut (Port, EMSI_IIR, 10);
    AddIEMSIMsg (sm (smeFail));
    Pause (3000);
    DoneIEMSIBox;
    Exit;
  End;

  If Tryes > 1 Then
  For i := 1 To 10 Do
  GetChar (Port, C);

  C             := ' ';
  EMSI_Len      := '';
  Packet        := '';
  CRC_          := '';
  EMSI_ICI      := 'EMSI_ICI';

  EMSI_ISI := '{' + NameVer  + '}{' + Cnf. BBSName +
              '}{' + Cnf. Location + '}{' + Cnf. SysOp +
              '}{' + HexL (GetUnixDate) +
              '}{(C) Konstantin Klyagin & Tornado team, 1995-98}{\01}{}';

  EMSI_ISI := '**EMSI_ISI' + HexW (Length (EMSI_ISI)) + EMSI_ISI +
              HexL (CRC32Str ('EMSI_ISI' + HexW (Length (EMSI_ISI)) +
              EMSI_ISI)) + #13;

  AddIEMSIMsg (sm (smeClient));
  Pause (800);

  For i := 1 To 4 Do
  Begin
    GetCharTimeOut (Port, C, 10);
    EMSI_Len := EMSI_Len + C;
  End;

  EMSI_ICI := EMSI_ICI + EMSI_Len;

  For i := 1 To Hex2Long (EMSI_Len) Do
  Begin
    GetCharTimeOut (Port, C, 10);
    Packet := Packet + C;
  End;

  EMSI_ICI := EMSI_ICI + Packet;

  For i := 1 To 8 Do
  Begin
    GetCharTimeOut (Port, C, 10);
    CRC_ := CRC_ + C;
  End;

  GetChar (Port, C);

  If HexL (Crc32Str (EMSI_ICI)) <> CRC_ Then
  Begin
    PutString (Port, EMSI_NAK);
    Inc (Tryes);
    Goto Loop;
  End Else
  Begin
    Tryes := 0;

    ISI:
    If Tryes >= 3 Then
    Begin
      AddIEMSIMsg (sm (smeFail));
      Pause (3000);
      LogWrite ('!', sm (smeFail));
      R. Name := '';
      EMSI. Session := False;
      EMSI. Allowed := False;
      DoneIEMSIBox;
      Exit;
    End;

    AddIEMSIMsg (sm (smeServ));
    PutStringTimeOut (Port, EMSI_ISI, 10)
  End;

  S := '';
  C := ' ';
  Pause (2000);

  If CharReady (Port) Then
  While (C <> #13) Do
  Begin
    GetCharTimeOut (Port, C, 5);
    S := S + C;
    If Not CheckDCD (Port) Then
    Begin
      Pause (2000);
      If Not CheckDCD (Port) Then Exit;
    End;
  End;

  S := UpString (S);

  If Pos (EMSI_ACK, S) <> 0 Then
  Begin
    GetStringTimeOut (Port, S, 16, [#13], 10);
    AddIEMSIMsg (sm (smeOk));
    Parse_EMSI_ICI (Packet);
    EMSI. Session := True;
    Pause (3000);
    DoneIEMSIBox;
    If Not InList (Cnf. BadPasswordsList, R. Password) Then R. Password := '';
    Exit;
  End;

  If Pos (EMSI_IIR, S) <> 0 Then
  Begin
    AddIEMSIMsg (sm (smeFail));
    Pause (3000);
    LogWrite ('!', sm (smeFail));
    R. Name := '';
    EMSI. Session := False;
    EMSI. Allowed := False;
    DoneIEMSIBox;
    Exit;
  End;

  Inc (Tryes);
  GoTo ISI;
End;

End.
