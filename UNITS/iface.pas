{$IFNDEF VirtualPascal}
  {$F+}
{$ENDIF}

Unit IFace;

{*********************************************************}
{*                      IFACE.PAS                        *}
{*                                                       *}
{*  Copyright (c) Konstantin Klyagin, 1995-98,           *}
{*                exspecially for Tornado BBS System     *}
{*                                                       *}
{*********************************************************}

Interface

Uses
  MainComm,
  ApAbsPcl,
  ApCom,
  DOS,
  tWin,
  TGlob,
  OpCrt,
  tMisc,
  SysMsgs,
  TimeTask;

Var
  TotalBytesTrans : LongInt;
  oX              : Byte;

Procedure DrawTornadoScreen (TotalCalls, UpLoads, DownLoads, TotalUsers, MsgsPosted: LongInt);
Procedure AddResponse (Const S: String; Attribute: Byte);
Procedure AddMsg (Const Msg: String; CR: Boolean; Attribute: Byte);
Procedure AddUsr (Const Name, LogTime: String; Time, DL, UL: Integer);
Procedure DoneTornadoScreen;
Procedure Clock;
Procedure InitTimer (Row, Col: Byte; Sec: Word; Attr: Byte);
Procedure TickTimer;
Procedure DoneTimer;

Procedure InitProtBox (Prot: ProtocolRecPtr; Mode: TransferModeType);
Procedure UpdateProtBox (Prot: ProtocolRecPtr);
Procedure ProtocolMsg (Msg: String);
Procedure DoneProtBox;

Implementation

Var
  oFName                     : ^String;
  Coverz, TimerCovers        : Pointer;
  MainBox, ProtBox           : PBoxRec;
  FilesSize, LastTotalTime,
  LastClock, StartTime,
  LastUpd                    : LongInt;
  LastPos1, LastPos2         : Integer;
  LastBlockSize, LastErrors,
  TimerCounter               : Word;
  TimerRow, TimerCol,
  TimerAttr                  : Byte;
  trMode                     : TransferModeType;

Procedure DrawTornadoScreen (TotalCalls, UpLoads, DownLoads, TotalUsers,
                             MsgsPosted: LongInt);
Var
  MaxY : Integer;
  Clr  : Byte;

Begin
  MaxY := Hi (WindMax);
  Clr := Cnf. ColorScheme [cmFrame];

  SaveWindow (1, 1, ScrX + 1, ScrY + 1, True, Coverz);
  InitWindow (MainBox, 1, 1, ScrX + 1, MaxY, 4, Clr, '', $00, 0, False);
  DrawWindow (MainBox);

  FastWrite ('Ã' + Replicate ('Ä', 58) + 'Â' + Replicate ('Ä', 19) + '´', 3,
    1, Clr);
  FastFillVert (10, '³', MaxY - 10, 33, Clr);
  FastWrite ('Ï', MaxY, 33, Clr);
  FastWrite ('Ã' + Replicate ('Ä', 31) + 'Â' + Replicate ('Ä', 26) + 'Á' +
    Replicate ('Ä', 19) + '´', MaxY - 11, 1, Clr);
  FastWrite ('Ã' + Replicate ('Ä', 31) + 'Å' + Replicate ('Ä', 46) + '´',
    MaxY - 9, 1, Clr);
  FastFillVert (MaxY - 15, '³', 4, 60, Clr);
  FastWrite (Pad (sm (smsStatLine), 56), MaxY + 1, 1,
    Cnf. ColorScheme [cmStatusLine]);
  FastWrite (' Com0 CTS DSR DR RI CD  ', MaxY + 1, 57,
    Cnf. ColorScheme [cmLamps]);
  Clr := Cnf. ColorScheme [cmMsgs];
  FastWrite (sm (smsLogEcho), 2, 2, Clr);
  FastWrite (sm (smsStatus), MaxY - 10, 2, Clr);
  FastWrite (sm (smsUsers), MaxY - 10, 34, Clr);
  Clr := Cnf. ColorScheme [cmOther];
  FastWrite (sm (smsSysCalls) + Long2Str (TotalCalls), MaxY - 8, 2, Clr);
  FastWrite (sm (smsUsersNum) + Long2Str (TotalUsers), MaxY - 7, 2, Clr);
  FastWrite (sm (smsPosted) + Long2Str (MsgsPosted), MaxY - 6, 2, Clr);
  FastWrite (sm (smsDownLoads) + Long2Str (DownLoads), MaxY - 5, 2, Clr);
  FastWrite (sm (smsUpLoads) + Long2Str (UpLoads), MaxY - 4, 2, Clr);
  oX := 2;
  If Cnf. Clock Then
    FastWrite (StrTime, 1, ScrX - 6, Cnf. ColorScheme [cmClock]);
  TextAttr := Cnf. ColorScheme [cmFrame];

  CLamps [True] := @Cnf. ColorScheme [cmHighlight];
  CLamps [False] := @Cnf. ColorScheme [cmLamps];
End;

Procedure AddResponse (Const S: String; Attribute: Byte);
Begin
  FastWrite (CenterCh (Copy (S, 1, 19), ' ', 19), Hi (WindMax) - 12, 61,
    Attribute);
End;

Procedure AddMsg (Const Msg: String; CR: Boolean; Attribute: Byte);
Var
  Len : Integer;

Begin
  Len := Length (Msg);
  If Len > 60 - oX Then
    Len := 60 - oX;
  FastWrite (Copy (Msg, 1, Len), Hi (WindMax) - 12, oX, Attribute);

  If CR Then
  Begin
    ScrollWindowUp (2, 4, ScrX - 20, Hi (WindMax) - 12, 1);
    ScrollWindowUp (61, 4, ScrX, Hi (WindMax) - 12, 1);
    oX := 2;
  End
  Else
    Inc (oX, Len);
End;

Procedure AddUsr (Const Name, LogTime: String; Time, DL, UL: Integer);
Var
  UsrStr : String;

Begin
  ScrollWindowUp (34, Hi (WindMax) - 8, ScrX, Hi (WindMax) - 1, 1);
  If Time < 0 Then
    Time := 0;
  UsrStr := LogTime + ' ' + Name + ' (' + Long2Str (Time) + ' min., ' +
    Long2Str (DL) + '/' + Long2Str (UL) + ')';
  If Length (UsrStr) > 46 Then
    UsrStr := Copy (UsrStr, 1, 44) + '..';
  FastWrite (UsrStr, Hi (WindMax) - 1, 34, Cnf. ColorScheme [cmUsers]);
End;

Procedure DoneTornadoScreen;
Begin
  CloseWindow (MainBox);
  RestoreWindow (1, 1, ScrX + 1, ScrY + 1, True, Coverz);
End;

Procedure Clock;
Var
  i : LongInt;

Begin
  i := MidSec;

  If i <> LastClock Then
  Begin
    LastClock := i;

    If Cnf. Clock Then
      FastWrite (StrTime, 1, ScrX - 6, Cnf. ColorScheme [cmClock]);

    If Not Local Then
    Begin
    {$IFNDEF OS2}
      FastWrite (' Com' + ComPorts [GetComName (Port)] + ' ', Hi (WindMax) + 1,
        57, Cnf. ColorScheme [cmLamps]);
    {$ELSE}
      FastWrite (CenterCh (Cnf. ComPort, ' ', 6), Hi (WindMax) + 1, 57,
        Cnf. ColorScheme [cmLamps]);
    {$ENDIF}

      FastWrite ('CTS ', Hi (WindMax) + 1, 63, CLamps [CheckCTS (Port)]^);
      FastWrite ('DSR ', Hi (WindMax) + 1, 67, CLamps [CheckDSR (Port)]^);
      FastWrite ('DR ', Hi (WindMax) + 1, 71,
        CLamps [{$IFDEF MSDOS} CheckDataReady (Port) {$ELSE} True {$ENDIF}]^);
      FastWrite ('RI ', Hi (WindMax) + 1, 74, CLamps [CheckRI (Port)]^);
      FastWrite ('CD ', Hi (WindMax) + 1, 77, CLamps [CheckDCD (Port)]^);
    End;
  End;
End;

Function Sec2Period (Sec: LongInt): String;
Begin
  Sec2Period := LeftPadCh (Long2Str (Sec Div 60), '0', 2) + ':' +
                LeftPadCh (Long2Str (Sec Mod 60), '0', 2);
End;

Procedure InitTimer (Row, Col: Byte; Sec: Word; Attr: Byte);
Begin
  StartTime := MidSec;
  LastUpd := StartTime;
  TimerCounter := Sec;
  TimerRow := Row;
  TimerCol := Col;
  TimerAttr := Attr;

  SaveWindow (Row - 1, Col - 1, Row + 5, Col + 1, True, TimerCovers);
  FastWrite (Sec2Period (Sec), Col, Row, Attr);
End;

Procedure TickTimer;
Var
  i : LongInt;

Begin
  i := MidSec;
  If i > LastUpd Then
  Begin
    LastUpd := i;
    i := TimerCounter - (i - StartTime);
    If i >= 0 Then
      FastWrite (Sec2Period (i), TimerCol, TimerRow, TimerAttr);
  End;
End;

Procedure DoneTimer;
Begin
  RestoreWindow (TimerRow - 1, TimerCol - 1, TimerRow + 5, TimerCol + 1, True,
    TimerCovers);
End;

Procedure InitProtBox (Prot: ProtocolRecPtr; Mode: TransferModeType);
Var
  i     : Integer;
  C     : Char;
  Clr : Byte;

Begin
  If R. AvgCPS > 0 Then ApAbsPcl. AvgCPS := R. AvgCPS
                   Else ApAbsPcl. AvgCPS := Round (GetConnectSpeed / 9);
  trMode := Mode;

  InitWindow (ProtBox, 0, 0, 60, 15, 4, Cnf. ColorScheme [cpFrame],
    ' ' + ULorDL [Mode] + ' ', Cnf. ColorScheme [cpHeader], ZoomSpeed, True);
  DrawWindow (ProtBox);

  With ProtBox^ Do
  Begin
    Clr := Cnf. ColorScheme [cpFrame];
    FastWrite ('Æ' + Replicate ('Í', 31) + 'Ñ' + Replicate ('Í', 27) + 'µ',
      Y1 + 4, X1, Clr);

    If trMode = Transmit Then C := 'Ä'
                         Else C := 'ú';
    FastWrite ('Æ' + Replicate ('Í', 31) + 'ÏÍÍÍ[' + Replicate (C, 20) +
      ']ÍÍµ', Y1 + 10, X1, Clr);

    FastFillVert (5, '³', Y1 + 5, X1 + 32, Clr);
    Clr := Cnf. ColorScheme [cpFieldNames];
    FastWrite (sm (smpFileName), Y1 + 5, X1 + 2, Clr);
    FastWrite (sm (smpDate), Y1 + 6, X1 + 2, Clr);
    FastWrite (sm (smpBlock), Y1 + 7, X1 + 2, Clr);
    FastWrite (sm (smpTotalErr), Y1 + 8, X1 + 2, Clr);
    FastWrite ('CPS    :', Y1 + 9, X1 + 2, Clr);
    FastWrite (sm (smpMode), Y1 + 5, X1 + 34, Clr);
    FastWrite (sm (smpSize), Y1 + 6, X1 + 34, Clr);
    FastWrite (sm (smpBytesTransf), Y1 + 7, X1 + 34, Clr);
    FastWrite (sm (smpTime), Y1 + 8, X1 + 34, Clr);
    FastWrite (sm (smpTimeLeft), Y1 + 9, X1 + 34, Clr);

    Clr := Cnf. ColorScheme [cpHeadText];
    FastWrite (CenterCh (NameVer + ' Internal ' + ProtocolTypeString
      [GetProtocol (Prot)], ' ', 59), Y1 + 1, X1 + 1, Clr);
    FastWrite (CenterCh ('(c) 1995-98, Konstantin Klyagin & Tornado Team,',
      ' ', 59), Y1 + 2, X1 + 1, Clr);
    FastWrite (CenterCh ('Kharkov, Ukraine.', ' ', 59), Y1 + 3, X1 + 1, Clr);
  End;

  GetMem (oFName, SizeOf (oFName^));
  oFName^ := '';
  TotalBytesTrans := 0;
  LastTotalTime := -1;
  LastPos1 := 0;
  LastPos2 := 0;
  LastBlockSize := 65535;
  LastErrors := 65535;
  FilesSize := 0;

  For i := 0 To F2Transfer^. Count-1 Do
    Inc (FilesSize, PTagFileRec (F2Transfer^. At (i))^. Size);
End;

Procedure SizeScroller (CurTot,            { ’¥ªãé¨© ä ©« : ®¡ê¥¬        }
                        CurPos,            { ’¥ªãé¨© ä ©« : ¯®§¨æ¨ï      }
                        Total,             { Ž¡é. ®¡ê¥¬   : ¢á¥£®        }
                        Current: LongInt;  { Ž¡é. ®¡ê¥¬   : ¯®§¨æ¨ï      }
                        Len,               { „«¨­  áªà®««¥à              }
                        X,                 { Š®®à¤¨­ â  X                }
                        Y,                 { Š®®à¤¨­ â  X                }
                        BkgCol,            { BackGround Color            }
                        TotCol,            { –¢¥â áâà®ª¨ ®¡é¥£® ®¡ê¥¬    }
                        CurCol: Byte);     { –¢¥â áâà®ª¨ â¥ªãé¥£® ä ©«   }
Var
  i, CLen, TLen : Integer;

Begin
  If (CurTot > 0) And (CurPos > 0) Then
  Begin
    CLen := Round (CurPos / (CurTot / Len));
    If CLen > Len Then
      CLen := Len;
  End
  Else
    CLen := 0;

  If (Total > 0) And (Current > 0) Then
  Begin
    TLen := Round (Current / (Total / Len));
    If TLen > Len Then
      TLen := Len;
  End
  Else
    TLen := 0;

  If (CLen <> LastPos1) Or (TLen <> LastPos2) Then
  Begin
    LastPos1 := CLen;
    LastPos2 := TLen;

    If CLen <= TLen Then i := CLen
                    Else i := TLen;
    If i > 0 Then
      FastFill (i, 'ß', Y, X, (TotCol Shl 4) + CurCol);

    If CLen > i Then
    Begin
      FastFill (CLen - i, 'ß', Y, X + i, (BkgCol Shl 4) + CurCol);
      i := CLen;
    End
    Else
      If TLen > i Then
      Begin
        FastFill (TLen - i, 'ß', Y, X + i, (TotCol Shl 4) + BkgCol);
        i := TLen;
      End;

    If i < Len Then
      FastFill (Len - i, 'Ä', Y, X + i, (BkgCol Shl 4) + TotCol);
  End;
End;

Procedure UpdateProtBox (Prot: ProtocolRecPtr);
Var
  FSize, BytesTransf,
  ActualCPS, Speed, l : LongInt;
  i                   : Integer;
  j                   : Word;
  DT                  : DateTime;
  DirInfo             : SearchRec;
  S                   : String [18];
  Clr                 : Byte;

Begin
  If GetFileName (Prot) = '' Then
    Exit;

  BytesTransf := GetBytesTransferred (Prot);
  ActualCPS := GetElapsedTics (Prot);
  If ActualCPS > 0 Then
  Begin
    ActualCPS := Trunc ((BytesTransf - GetInitialFilePos (Prot)) /
      (ActualCPS / 18.2));
    If ActualCPS < 0 Then
      ActualCPS := 0;
  End
  Else
    ActualCPS := 0;

  Clr := Cnf. ColorScheme [cpFieldValues];

  With ProtBox^ Do
  Begin
    If oFName^ <> GetPathName (Prot) Then
    Begin
      oFName^ := GetPathName (Prot);

      FastWrite (Pad (UpString (JustFileName (oFName^)), 12), Y1 + 5, X1 + 11,
        Clr);

      If trMode = Receive Then
      Begin
        UnixDate2DateTime (Prot^. PData^. SrcFileDate, DT);
        FSize := GetFileSize (Prot);
      End Else
      Begin
        FindFirst (oFName^, AnyFile-VolumeID-Directory, DirInfo);
        FSize := DirInfo. Size;
        UnpackTime (DirInfo. Time, DT);
      {$IFNDEF MSDOS}
        FindClose (DirInfo);
      {$ENDIF}
      End;

      FastWrite (FormattedDate (DT, 'DD-NNN-YYYY HH:II'), Y1 + 6, X1 + 11, Clr);

      If GetInitialFilePos (Prot) = 0 Then S := 'New '
                                      Else S := 'Recovery ';
      Case GetCheckType (Prot) Of
        bcCrc16 : S := S + '(CRC-16)';
        bcCrc32 : S := S + '(CRC-32)';
      End;

      FastWrite (Pad (S, 17), Y1 + 5, X1 + 43, Clr);
      FastWrite (Pad (Long2Str (FSize), 15), Y1 + 6, X1 + 43, Clr);
    End
    Else
      FSize := GetFileSize (Prot);

    j := GetBlockSize (Prot);
    If j <> LastBlockSize Then
    Begin
      LastBlockSize := j;
      FastWrite (Pad (Long2Str (j), 15), Y1 + 7, X1 + 11, Clr);
    End;

    j := GetTotalErrors (Prot);
    If j <> LastErrors Then
    Begin
      LastErrors := j;
      FastWrite (Pad (Long2Str (j), 15), Y1 + 8, X1 + 11, Clr);
    End;

    FastWrite (Pad (Long2Str (ActualCPS), 15), Y1 + 9, X1 + 11, Clr);

    If FSize < 0 Then
      Exit;

    Speed := GetConnectSpeed;
    l := EstimatedTransferTime (FSize, ActualCPS, Speed);
    If l <> LastTotalTime Then
    Begin
      LastTotalTime := l;
      FastWrite (HowTime (l), Y1 + 8, X1 + 43, Clr);
    End;

    FastWrite (HowTime (EstimatedTransferTime (FSize - BytesTransf, ActualCPS,
      Speed)), Y1 + 9, X1 + 43, Clr);

    If trMode = Receive Then
    Begin
      If (FSize > 0) And (BytesTransf > 0) Then
      Begin
        i := Round (BytesTransf / (FSize / 20));
        If i > 20 Then
          i := 20;
      End
      Else
        i := 0;

      If i <> LastPos1 Then
      Begin
        LastPos1 := i;
        FastFill (i, 'Û', Y1 + 10, X1 + 37, Cnf. ColorScheme [cpScrollBar]);
        FastFill (20 - i, 'ú', Y1 + 10, X1 + 37 + i,
          Cnf. ColorScheme [cpFrame]);
      End;
    End
    Else
      SizeScroller (FSize, BytesTransf, FilesSize, TotalBytesTrans +
        BytesTransf, 20, X1 + 37, Y1 + 10, Cnf. ColorScheme [cpScrollBar] Shr 4,
        7, Cnf. ColorScheme [cpScrollBar] And $0F);

    FastWrite (Pad (Long2Str (BytesTransf), 15), Y1 + 7, X1 + 43, Clr);
  End;
End;

Procedure ProtocolMsg (Msg: String);
Var
  oAttr : Byte;

Begin
  oAttr := TextAttr;
  TextAttr := Cnf. ColorScheme [cpMessages];
  With ProtBox^ Do
  Begin
    ScrollWindowUp (X1 + 2, Y1 + 11, X1 + 58, Y1 + 14, 1);
    If Length (Msg) > 57 Then
      Msg := Copy (Msg, 1, 55) + '..';
    FastWrite (Msg, Y1 + 14, X1 + 2, Cnf. ColorScheme [cpMessages]);
  End;
  TextAttr := oAttr;
End;

Procedure DoneProtBox;
Begin
  FreeMem (oFName, SizeOf (oFName^));
  CloseWindow (ProtBox);
End;

End.
