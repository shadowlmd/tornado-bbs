{$IFDEF MSDOS}
  {$IFNDEF DPMI32}
    {$F+}
  {$IFNDEF DPMI}
    {$O+}
  {$ENDIF}
  {$ENDIF}
{$ENDIF}
{$B-,I-}

Unit
  Multi;

Interface

Uses
  tMisc,
  tGlob,
  DOS,
  Log,
  SysMsgs;

Procedure InitMultiUnit;

Implementation

Var
  LineRec : tLine;

Function fmL_Init (LineNo: Byte): Boolean;
Var
  tLF, LineFile : File Of tLine;
  Present       : Boolean;

Begin
  If (LineNo = 0) Or R. Guest Then
  Begin
    fmL_Init := True;
    Exit;
  End;

  BbsLine := LineNo;
  Present := False;

  WaitAndSetFlag ('linelist.tbf');

  Assign (LineFile, Cnf. FlagsDir + '!llist.tor');
  If FileExists (Cnf. FlagsDir + '!llist.tor') Then
    Reset (LineFile)
  Else
    ReWrite (LineFile);

  Assign (tLF, Cnf. FlagsDir + '!llist.t$$');
  ReWrite (tLF);

  While Not EoF (LineFile) Do
  Begin
    Read (LineFile, LineRec);
    If UpString (LineRec. UserName) = UpString (R. Name) Then
    Begin
      Present := True;
      Break;
    End;

    Write (tLF, LineRec);
  End;

  If Not Present Then
  Begin
    LineRec. UserName := R. Name;
    LineRec. Number := BbsLine;
    LineRec. Logged := True;
    LineRec. LoginTime := StrTime;

    Write (tLF, LineRec);
    Close (LineFile);
    Erase (LineFile);
    Close (tLF);
    Rename (tLF, Cnf. FlagsDir + '!llist.tor');
    tDeleteFile (Cnf. FlagsDir + '!msg' + Long2Str (BbsLine) + '.*');
  End Else
  Begin
    Close (tLF);
    Erase (tLF);
  End;

  DelFlag ('linelist.tbf');
  fmL_Init := Not Present;
End;

Function fmL_MsgWaiting: Boolean;
Begin
  If BbsLine = 0 Then
    fmL_MsgWaiting := False
  Else
    fmL_MsgWaiting := Not FileExists (Cnf. FlagsDir + '!msg' +
      Long2Str (BbsLine) + '.*');
End;

Procedure fmL_SendMsg (LineNo: Byte; MsgText: String; MsgType: tMsgType);
Var
  i, l    : Integer;
  Err     : SysInt;
  F       : Text;
  DirInfo : SearchRec;

Begin
  If BbsLine = 0 Then
    Exit;

  If MsgType = mtConference Then
  Begin
    If Cnf. TRCLog <> '' Then
    Begin
      WaitAndSetFlag ('trclog.tbf');

      Assign (F, Cnf. TRCLog);
      If FileExists (Cnf. TRCLog) Then
        Append (F)
      Else
        ReWrite (F);

      If IOResult = 0 Then
      Begin
        If MsgText [1] <> trcSysMsgPrefix Then
          WriteLn (F, FormattedCurrDT (Cnf. DateMask + ' HH:II') + ' [' +
            R. Name + ']: ' + MsgText)
        Else
          WriteLn (F, FormattedCurrDT (Cnf. DateMask + ' HH:II ') +
            Copy (MsgText, 2, 255));

        Close (F);
      End;

      DelFlag ('trclog.tbf');
    End;

    FindFirst (Cnf. FlagsDir + '!conf*.tbf', AnyFile-Directory-VolumeID,
      DirInfo);

    While DOSerror = 0 Do
    Begin
      i := 6;

      While (i <= 12) And (DirInfo. Name [i] in NumbersOnly) Do
        Inc (i);

      If i > 6 Then
      Begin
        Val (Copy (DirInfo. Name, 6, i - 6), i, Err);
        If (Err = 0) And (i > 0) And (i <= 255) And (i <> LineRec. Number) Then
          For l := 1 To 255 Do
          Begin
            Assign (F, Cnf. FlagsDir + '!msg' + Long2Str (i) + '.' +
              Long2Str (l));
            ReSet (F);

            If IOResult <> 2 Then
            Begin
              Close (F);
              Continue;
            End;

            ReWrite (F);
            WriteLn (F, 'c;' + Long2Str (BbsLine) + ';' + R. Name);
            WriteLn (F, MsgText);
            Close (F);

            Break;
          End;
      End;

      FindNext (DirInfo);
    End;

  {$IFNDEF MSDOS}
    FindClose (DirInfo);
  {$ENDIF}
  End
  Else
    For l := 1 To 255 Do
      If l <> LineRec. Number Then
        For i := 1 To 255 Do
        Begin
          Assign (F, Cnf. FlagsDir + '!msg' + Long2Str (LineNo) + '.' +
            Long2Str (i));
          ReSet (F);

          If IOResult <> 2 Then
          Begin
            Close (F);
            Continue;
          End;

          ReWrite (F);
          WriteLn (F, 'u;' + Long2Str (BbsLine) + ';' + R. Name);
          WriteLn (F, MsgText);
          Close (F);

          Exit;
        End;
End;

Function fmL_GetMsg (Var Msg: tMsg; MsgType: tMsgType): Boolean;
Var
  F       : Text;
  DirInfo : SearchRec;
  S       : String;
  cT      : Char;

Begin
  If BbsLine = 0 Then
    Exit;

  fmL_GetMsg := False;

  FindFirst (Cnf. FlagsDir + '!msg' + Long2Str (BbsLine) + '.*',
    AnyFile-Directory-VolumeID, DirInfo);

  While DOSerror = 0 Do
  Begin
    Assign (F, Cnf. FlagsDir + DirInfo. Name);
    ReSet (F);
    If IOResult <> 0 Then
      Continue;

    ReadLn (F, S);
    cT := S [1];

    If Not (((cT = 'u') And (MsgType = mtUserMsg)) Or
            ((cT = 'c') And (MsgType = mtConference))) Then
    Begin
      Close (F);
      If (cT <> 'c') And (cT <> 'u') Then
        Erase (F);

      Break;
    End;

    Msg. FromUserName := ExtractWord (3, S, [';']);
    Msg. FromLineNum  := Str2Long (ExtractWord (2, S, [';']));
    ReadLn (F, S);
    Msg. MessageText := S;
    Close (F);
    Erase (F);
    fmL_GetMsg := True;

    Break;
  End;

{$IFNDEF MSDOS}
  FindClose (DirInfo);
{$ENDIF}
End;

Function fmL_GetMesgStat (LineNo: Byte): Boolean;
Begin
  If BbsLine = 0 Then
    fmL_GetMesgStat := False
  Else
    fmL_GetMesgStat := CheckFlag ('!mena' + Long2Str (LineNo) + '!.tbf');
End;

Procedure fmL_DisableMsg;
Begin
  If BbsLine <> 0 Then
    DelFlag ('!mena' + Long2Str (BbsLine) + '!.tbf');
End;

Procedure fmL_EnableMsg;
Begin
  If BbsLine <> 0 Then
    SetFlag ('!mena' + Long2Str (BbsLine) + '!.tbf');
End;

Procedure fmL_LineMsg;
Var
  Msg : tMsg;

Begin
  If BbsLine <> 0 Then
  Begin
    If fmL_GetMsg (Msg, mtUserMsg) Then
    Begin
      SmartLine;
      ComWriteLn (#7, eoNoFlush);
      ComWriteLn (lang (laMlMessageFrom) + Msg. FromUserName + ' (' +
        lang (lamlLine) + Long2Str (Msg. FromLineNum) + ')..',
        eoCodes + eoMacro);

      ComWriteLn (Msg. MessageText, 0);
      Message ('');
    End;
  End;
End;

Procedure fmL_WhoDo (ListMode: tListMode);
Var
  tmpLine  : tLine;
  LineFile : File Of tLine;

Begin
  If BbsLine = 0 Then
    Exit;

  If Not InConference Then Cls
                      Else ComWrite (#13 + EmuClrEOL, 0);

  ComWriteLn (lang (laLineStatus), eoMacro + eoCodes);
  ComWriteLn (EmuRelColor (Cnf. ColorScheme [umSeparator]) +
    '컴컴 컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴 컴컴컴컴컴�',
    eoMacro + eoCodes);

  WaitAndSetFlag ('linelist.tbf');

  Assign (LineFile, Cnf. FlagsDir + '!llist.tor');
  ReSet (LineFile);
  If IOResult <> 0 Then
    Exit;

  While Not EoF (LineFile) Do
  Begin
    Read (LineFile, tmpLine);

    If ((ListMode = lmExcludeCurrent) And (tmpLine. Number = BbsLine)) Or
       ((ListMode = lmConference) And (Not CheckFlag ('!conf' +
         Long2Str (tmpLine. Number) + '!.tbf')))
    Then
      Continue;

    If tmpLine. Logged Then
      ComWriteLn (EmuRelColor ($0E) + LeftPad (Long2Str (tmpLine. Number), 3) +
        EmuRelColor ($0F) + '. ' + EmuRelColor ($0A) + Pad (tmpLine. UserName,
        36) + ' ' + tmpLine. LoginTime, 0);
  End;

  If InConference Then
    ComWriteLn ('', 0);

  Close (LineFile);
  DelFlag ('linelist.tbf');
End;

Function fmL_ChooseLine: Byte;
Var
  Err     : SysInt;
  NumLine : Byte;

Begin
  If BbsLine = 0 Then
    Exit;
  fmL_WhoDo (lmExcludeCurrent);
  ComWriteLn ('', 0);
  Val (GetAnswer (lang (laLineNumber), 4, ofFramed + ofAllowEmpty, ''),
    NumLine, Err);
  If Err <> 0 Then fmL_ChooseLine := 0
              Else fmL_ChooseLine := NumLine;
End;

Procedure fmL_RealTimeConference;
Var
  Err      : SysInt;
  cLine, i : Byte;
  S, From  : String;

Begin
  If BbsLine = 0 Then
    Exit;

  fmL_EnableMsg;
  LogWrite ('+', sm (smTRCjoin));
  SetFlag ('!conf' + Long2Str (BbsLine) + '!.tbf');
  InConference := True;

  EmuDispFile ('~trchelp');
  ClearInputHistory;
  If R. Alias <> '' Then From := R. Alias
                    Else From := R. Name;
  S := '';

  fmL_SendMsg (0, trcSysMsgPrefix + '** ' + R. Name + lang (laTRCline) +
    Long2Str (BbsLine) + lang (laTRCjoined), mtConference);

  Repeat
    ComWrite (#13, 0);
    If S <> '' Then
    Begin
      fmL_SendMsg (0, S, mtConference);
      ComWrite (#13 + EmuClrEoL, eoNoFlush);
      ComWriteLn (EmuRelColor ($0F) + '<' + From + '> ' + EmuRelColor ($0B) + S,
        eoCodes);
    End;

    ComWrite (#13 + EmuClrEoL, eoNoFlush);
    ComWrite (EmuRelColor ($0B) + '> ' + EmuRelColor ($0E), 0);
    S := '';
    ComRead (S, 76 - Length (From), ofHistory);

    If (Length (S) > 1) And (S [1] = '/') Then
    Begin
      Case UpCase (S [2]) Of
        'Q' : Break;
        'W' : fmL_WhoDo (lmConference);
        'O' : fmL_WhoDo (lmAll);
        'H' : EmuDispFile ('~trchelp');
        'M' : Begin
                Val (ExtractWord (2, S, SpaceOnly), cLine, Err);
                If (Err = 0) And (cLine <> 0) And (cLine <> BbsLine) Then
                Begin
                  i := WordPosition (3, S, SpaceOnly);
                  If (i > 0) And fmL_GetMesgStat (cLine) Then
                  Begin
                    fmL_SendMsg (cLine, ' ' + Copy (S, i, 255), mtUserMsg);
                    LogWrite ('+', sm (smlSendingMsg) + Long2Str (cLine));
                  End;
                End;
              End;
      End;

      S := '';
    End;
  Until False;

  fmL_SendMsg (0, trcSysMsgPrefix + '** ' + R. Name + lang (laTRCline) +
    Long2Str (BbsLine) + lang (laTRCleft), mtConference);

  DelFlag ('!conf' + Long2Str (BbsLine) + '!.tbf');
  LogWrite ('+', sm (smTRCleave));
  InConference := False;
End;

Procedure fmL_Done;
Var
  tLF, LineFile : File Of tLine;
  S             : String [20];
  Empty         : Boolean;

Begin
  If BbsLine = 0 Then
    Exit;

  S := Long2Str (LineRec. Number);
  tDeleteFile (Cnf. FlagsDir + '!msg' + S + '.*');
  tDeleteFile (Cnf. FlagsDir + '!mena' + S + '!.tbf');
  tDeleteFile (Cnf. FlagsDir + '!conf' + S + '!.tbf');

  WaitAndSetFlag ('linelist.tbf');
  Assign (LineFile, Cnf. FlagsDir + '!llist.tor');
  If FileExists (Cnf. FlagsDir + '!llist.tor') Then
    Reset (LineFile)
  Else
    ReWrite (LineFile);

  Assign (tLF, Cnf. FlagsDir + '!llist.t$$');
  ReWrite (tLF);
  Empty := True;

  While Not EoF (LineFile) Do
  Begin
    Read (LineFile, LineRec);
    If LineRec. Number <> BbsLine Then
    Begin
      Write (tLF, LineRec);
      Empty := False;
    End;
  End;

  Close (LineFile);
  Erase (LineFile);
  Close (tLF);

  If Not Empty Then Rename (tLF, Cnf. FlagsDir + '!llist.tor')
               Else Erase (tLF);

  DelFlag ('linelist.tbf');
End;

Procedure InitMultiUnit;
Begin
  mL_Init := fmL_Init;
  mL_MsgWaiting := fmL_MsgWaiting;
  mL_GetMsg := fmL_GetMsg;
  mL_GetMesgStat := fmL_GetMesgStat;
  mL_SendMsg := fmL_SendMsg;
  mL_WhoDo := fmL_WhoDo;
  mL_ChooseLine := fmL_ChooseLine;
  mL_RealTimeConference := fmL_RealTimeConference;
  mL_DisableMsg := fmL_DisableMsg;
  mL_EnableMsg := fmL_EnableMsg;
  mL_LineMsg := fmL_LineMsg;
  mL_Done := fmL_Done;
End;

End.
