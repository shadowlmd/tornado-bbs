{$IFDEF MSDOS}
  {$IFNDEF DPMI32}
    {$F+}
  {$IFNDEF DPMI}
    {$O+}
  {$ENDIF}
  {$ENDIF}
{$ENDIF}

Unit MainCOvr;

{*********************************************************}
{*                     MAINCOVR.PAS                      *}
{*                                                       *}
{*  Copyright (c) Konstantin Klyagin, 1995-98,           *}
{*                exspecially for Tornado BBS System     *}
{*                                                       *}
{*********************************************************}

Interface

Uses
  DOS,
  tWin,
  ApTimer,
  ApCom,
  MainComm,
  Log,
  tGlob,
  tMisc,
  iEMSI,
  OpCrt,
  Parse,
  Parser,
  Objects,
  SysMsgs,
  TimeTask,
  Users;

Function fQuery (Const What: String; IsYes: Boolean; Flags: Byte): Boolean;
Function GetMaxStr (Const Msg: String): String;
Procedure InitMainCOvr;

Implementation

Const
  cm_NumLen  = 4;
  cm_ItemLen = 33;

Procedure fComRead (Var S: String; Max: Integer; Options: Byte);
Var
  MLtimer                  : LongInt;
  i, Index, CurHistoryItem : Integer;
  k                        : Char;
  Changed                  : Boolean;
  S1                       : String;
  MultiMsg                 : tMsg;

  Procedure SetChanged;
  Begin
    Changed := True;
    If Options And ofHistory <> 0 Then
      CurHistoryItem := ReadHistory^. Count - 1;
  End;

  Procedure StoreInHistory;
  Begin
    If Changed Then
    Begin
      ReadHistory^. AtFree (ReadHistory^. Count - 1);
      ReadHistory^. InsLine (S);
    End;
  End;

  Procedure UpdateString;
  Var
    Len : Integer;

  Begin
    StoreInHistory;

    Len := Length (S);
    S := PString (ReadHistory^. At (CurHistoryItem))^;
    ComWrite (Replicate (#8, Index - 1) + S, 0);

    Index := Length (S);
    Dec (Len, Index);
    If Len > 0 Then
      ComWrite (Replicate (' ', Len) + Replicate (#8, Len), 0);
    Inc (Index);

    Changed := False;
  End;

Begin
  If InConference Then
    MLtimer := MidSec;

  If HotKeysStr <> '' Then
    If Not R. HotKeys Then
    Begin
      S := HotKeysStr;
      HotKeysStr := '';

      Case Mode Of
         Upper : S := UpString (S);
         Lower : S := LoString (S);
        Proper : S := PrString (S);
      End;

      ComWrite (S, 0);
      Exit;
    End
    Else
      HotKeysStr := '';

  ComWrite (S, 0);
  Index := Length (S) + 1;
  Changed := False;

  If Options And ofHistory <> 0 Then
  Begin
    ReadHistory^. InsLine (S);
    CurHistoryItem := ReadHistory^. Count-1;
  End;

  Repeat
    If Registering And EMSI. Allowed Then
      If Pos ('**EMSI_ICI', UpString (S)) <> 0 Then
      Begin
        ComWriteLn ('', 0);
        DoIEMSI;
        S := R. Name;
        If S <> '' Then
          Exit;
      End;

    Repeat
      k := ComReadKey;
    Until k <> #0;

    If InConference Then
      If TimeDiff (MLtimer, MidSec) >= 5 Then
      Begin
        While mL_GetMsg (MultiMsg, mtConference) Do
        Begin
          If MultiMsg. MessageText [1] <> trcSysMsgPrefix Then
          Begin
            ComWrite (#13 + EmuRelColor ($0F) + EmuClrEOL, eoNoFlush);
            ComWriteLn ('<' + MultiMsg. FromUserName + '> ' + EmuRelColor ($0B)
              + MultiMsg. MessageText, eoCodes);
          End Else
          Begin
            ComWrite (#13 + EmuRelColor ($0A) + EmuClrEOL, eoNoFlush);
            ComWriteLn (Copy (MultiMsg. MessageText, 2, 255),
              eoMacro + eoCodes);
          End;

          ComWrite (EmuRelColor ($0B) + '> ', eoNoFlush);
          ComWrite (EmuRelColor ($0F) + S, 0);
        End;

        While mL_GetMsg (MultiMsg, mtUserMsg) Do
        Begin
          ComWrite (#13#7 + EmuRelColor ($0C) + '[' + MultiMsg. FromUserName
            + ']', eoNoFlush);
          ComWriteLn (EmuRelColor ($0B) + MultiMsg. MessageText, eoCodes);
          ComWrite (EmuRelColor ($0B) + '> ', eoNoFlush);
          ComWrite (EmuRelColor ($0E) + S, 0);
        End;

        MLtimer := MidSec;
      End;

    Case k Of
        #13 : Begin
                i := Length (Trim (S));

                If Not ((i = 0) And (Options And ofAllowEmpty = 0)) Then
                Begin
                  If Options And ofSpaceAdd <> 0 Then
                    ComWrite (Replicate (' ', Max - Length (S)), 0);

                  If Options And ofHistory <> 0 Then
                  Begin
                    ReadHistory^. AtFree (ReadHistory^. Count - 1);
                    If Changed Then
                    Begin
                      If i > 0 Then
                        ReadHistory^. InsLine (S);
                    End
                    Else
                      If CurHistoryItem < ReadHistory^. Count - 1 Then
                      Begin
                        ReadHistory^. AtFree (CurHistoryItem);
                        ReadHistory^. InsLine (S);
                      End;
                  End;

                  Exit;
                End;
              End;
         #8 : If Index > 1 Then
              Begin
                Dec (Index);
                Delete (S, Index, 1);
                SetChanged;

                If Index = Length (S) + 1 Then
                  ComWrite (#8' '#8, 0)
                Else
                Begin
                  ComWrite (#8 + Copy (S, Index, 255) + ' ', eoNoFlush);
                  ComWrite (EmuCursorLeft (Length (S) - Index + 2), 0);
                End;
              End;
      kbDel : If Index <= Length (S) Then
              Begin
                Delete (S, Index, 1);
                SetChanged;

                ComWrite (Copy (S, Index, 255) + ' ', eoNoFlush);
                ComWrite (EmuCursorLeft (Length (S) - Index + 2), 0);
              End;
     kbLeft : If Index > 1 Then
              Begin
                ComWrite (EmuCursorLeft (1), 0);
                Dec (Index);
              End;
    kbRight : If Index <= Length (S) Then
              Begin
                ComWrite (S [Index], 0);
                Inc (Index);
              End;
     kbHome : If Index > 1 Then
              Begin
                ComWrite (EmuCursorLeft (Index - 1), 0);
                Index := 1;
              End;
      kbEnd : If Index <= Length (S) Then
              Begin
                ComWrite (EmuCursorRight (Length (S) - Index + 1), 0);
                Index := Length (S) + 1;
              End;
       kbUp : If Options And ofHistory <> 0 Then
                If CurHistoryItem > 0 Then
                Begin
                  Dec (CurHistoryItem);
                  UpdateString;
                End;
     kbDown : If Options And ofHistory <> 0 Then
                If CurHistoryItem < ReadHistory^. Count - 1 Then
                Begin
                  Inc (CurHistoryItem);
                  UpdateString;
                End;
        #24 : Begin                                          { Ctrl-X }
                ComWrite (Replicate (#8, Index - 1), eoNoFlush);
                ComWrite (Replicate (' ', Length (S)), eoNoFlush);
                ComWrite (Replicate (#8, Length (S)), 0);
                Index := 1;
                S := '';
                SetChanged;
              End;
    Else
      If k in InputAccept Then
        If Length (S) < Max Then
        Begin
          Case Mode Of
             Upper : k := UpCase (k);
             Lower : k := LoCase (k);
            Proper : If (Index > 1) And Not (S [Index - 1] in
                        [' ', '.', ',', '/', '-'])
                     Then
                       k := LoCase (k)
                     Else
                       k := UpCase (k);
          End;

          Insert (k, S, Index);
          Inc (Index);
          SetChanged;

          ComWrite (k + Copy (S, Index, 255), 0);
          i := Length (S) + 1 - Index;
          If i > 0 Then
            ComWrite (EmuCursorLeft (i), 0);
        End
        Else
          If InConference Then
          Begin
            If Pos (' ', S) <> 0 Then
            Begin
              S1 := ExtractWord (WordCount (S, SpaceOnly), S, SpaceOnly);
              S := TrimTrail (Copy (S, 1, Length (S) - Length (S1)));
            End
            Else
              S1 := '';

            KeyBufAdd (S1 + k);
            Exit;
          End;
    End;
  Until False;
End;

Procedure fComReadLn (Var S: String; Max: Integer; Options: Byte);
Begin
  fComRead (S, Max, Options);
  ComWriteLn ('', 0);
End;

Function fComMenu (Const Header, ChoiceStr: String;
                   Items: PNotSortedCollection): Integer;
Const
  NumMargin : Array [Boolean] Of Byte = (cm_NumLen, cm_NumLen + 1);

Var
  PS     : PString;
  i, Num : Integer;
  Err    : Word;
  T      : String;

Label
  EndOfProc;

Begin
  fComMenu := 0;

  If Items^. Count = 0 Then
    Exit;

  T := '';

  If HotKeysStr <> '' Then
  Begin
    SetOutput (False, False);
    ComReadLn (T, 4, ofAllowEmpty);
    SetOutput (True, Not Local);
    Goto EndOfProc;
  End;

  i := 1;

  ComWriteLn (Header + #10, eoMacro + eoCodes);
  InitMore (WhereY);

  While i <= Items^. Count Do
  Begin
    ComWrite (EmuRelColor (Cnf. ColorScheme [umNumber]) + LeftPad
      (Long2Str (i), NumMargin [Cnf. OneColMenus]), eoNoFlush);
    ComWrite (EmuRelColor (Cnf. ColorScheme [umDot]) + '. ', eoNoFlush);

    If Cnf. OneColMenus Then
      ComWriteLn (EmuRelColor (Cnf. ColorScheme [umItem]) +
        PString (Items^. At (i - 1))^, eoMacro + eoCodes)
    Else
    Begin
      ComWrite (EmuRelColor (Cnf. ColorScheme [umItem]), 0);

      PS := Items^. At (i - 1);
      If i < Items^. Count Then
      Begin
        If Pos ('|', PS^) = 0 Then mWriteLen (PS^, cm_ItemLen, pmPadRight)
                              Else mWriteLen (PS^, cm_ItemLen, pmNone);
        Inc (i);
        ComWrite (EmuRelColor (Cnf. ColorScheme [umNumber]) + LeftPad
          (Long2Str (i), NumMargin [Cnf. OneColMenus]), eoNoFlush);
        ComWrite (EmuRelColor (Cnf. ColorScheme [umDot]) + '. ', eoNoFlush);
        ComWrite (EmuRelColor (Cnf. ColorScheme [umItem]), 0);
        mWriteLen (PString (Items^. At (i - 1))^, cm_ItemLen, pmNone);
      End
      Else
        mWriteLen (PS^, cm_ItemLen, pmNone);

      ComWriteLn ('', 0);
    End;

    If Not MoreNums (T, ChoiceStr, 4, False) Then
      Exit;

    If Length (T) > 0 Then
    Begin
      Val (T, Num, Err);
      If (Err = 0) And (Num > 0) And (Num <= Items^. Count) Then
      Begin
        fComMenu := Num;
        Exit;
      End;

      T := '';
    End;

    Inc (i);
  End;

  ComWrite (#10 + ChoiceStr, eoMacro + eoCodes + eoNoFlush);
  SetInputCap (NoCaps, NumbersOnly);
  ComReadLn (T, 4, ofAllowEmpty);
  SetInputCap (NoCaps, AllChars);

EndOfProc:
  Val (T, Num, Err);
  If (Err = 0) And (Num > 0) And (Num <= Items^. Count) Then
    fComMenu := Num;
End;

Procedure fSetProtocol (C: Char);
Var
  i                      : LongInt;
  PS                     : PString;
  Prots                  : PNotSortedCollection;
  PKey                   : Char;
  Avail, Select          : Boolean;
  P                      : tConfigParser;
  PName, ProtStr, S1, S2 : String;

Begin
  Select := (C = #0) And (HotKeysStr = '');

  If ParserOpen (P, Cnf. ProtocolCTL, tpoWriteLog) Then
  Begin
    PName := '';
    ProtStr := '';
    New (Prots, Init (8, 4));

    While Not ParserEnd (P) Do
    Begin
      S1 := ParserRead (P, S2);

      If (S2 = 'EXTERNAL') Or (ParserEnd (P) And (S2 = '')) Then
      Begin
        If PName <> '' Then
          If (PKey <> #255) And Avail Then
          Begin
            Prots^. Insert (NewStr (PKey + PName));
            ProtStr := ProtStr + PKey;
          End;

        PKey := #255;
        Avail := True;
        PName := '';
        S2 := '';
      End;

      If S2 = 'INTERNAL' Then
      Begin
        ParserGetParam (P, tptBoolean, '', Avail);
        If Avail Then
        Begin
          If S1 = 'XMODEM' Then PKey := '1' Else
          If S1 = 'XMODEMCRC' Then PKey := '2' Else
          If S1 = 'XMODEM1K' Then PKey := '3' Else
          If S1 = 'XMODEM1KG' Then PKey := '4' Else
          If S1 = 'YMODEM' Then PKey := '5' Else
          If S1 = 'YMODEMG' Then PKey := '6' Else
          If S1 = 'ZMODEM' Then PKey := '7' Else
          If S1 = 'ZMODEM8K' Then PKey := '8'
                             Else PKey := #0;

          If PKey <> #0 Then
          Begin
            Prots^. Insert (NewStr (PKey + ExtractWord (1, P. S, SpaceOnly)));
            ProtStr := ProtStr + PKey;
          End;
        End;
      End
      Else
        If S2 = '' Then
        Begin
          If S1 = 'NAME' Then ParserGetParam (P, tptString, '', PName) Else
          If S1 = 'AVAILABLE' Then ParserGetParam (P, tptBoolean, '', Avail) Else
          If S1 = 'SELECTION' Then
          Begin
            ParserGetParam (P, tptString, '', S1);
            If Length (S1) > 0 Then PKey := S1 [1]
                               Else PKey := #255;
          End;
        End;
    End;

    ParserClose (P);

    If Length (ProtStr) = 1 Then
      C := ProtStr [1]
    Else
      If Select Or (Pos (C, ProtStr) = 0) Then
      Begin
        ComWrite ('|' + lang (laSelectProtocol) + '|', eoMacro + eoCodes);

        For i := 0 To Prots^. Count-1 Do
        Begin
          PS := PString (Prots^. At (i));
          ComWriteLn (PlaceSubStrNoCase (PlaceSubStrNoCase (lang
            (laProtocolStr), '@Key', PS^ [1]), '@Name', Copy (PS^, 2, 255)),
            eoMacro + eoCodes);
        End;

        ComWrite ('|' + lang (laYourChoice), eoMacro + eoCodes);

        Repeat
          ProcessChoices;
          C := UpCase (ComReadKey);
        Until Pos (C, ProtStr) <> 0;
      End;

    FillChar (ProtocolDef, SizeOf (ProtocolDef), #0);

    For i := 0 To Prots^. Count-1 Do
    Begin
      PS := PString (Prots^. At (i));

      If PS^ [1] = C Then
        With ProtocolDef Do
        Begin
          Name := Copy (PS^, 2, 255);
          Available := True;
          Selection := C;
          Break;
        End;
    End;

    If ProtocolDef. Available Then
      If Not (C in ['1'..'8']) Then
        If ParserOpen (P, Cnf. ProtocolCTL, tpoWriteLog) Then
        Begin
          ProtocolDef. Name := '';

          While Not ParserEnd (P) Do
          Begin
            S1 := ParserRead (P, S2);

            If (S2 = 'EXTERNAL') Or (ParserEnd (P) And (S2 = '')) Then
            Begin
              If (ProtocolDef. Name <> '') And
                 (ProtocolDef. Selection = C) Then Break;
              S2 := '';
            End;

            If S2 = '' Then
              With ProtocolDef Do
              Begin
                If S1 = 'NAME' Then ParserGetParam (P, tptString, '', Name) Else
                If S1 = 'AVAILABLE' Then ParserGetParam (P, tptBoolean, '', Available) Else
                If S1 = 'BATCH' Then ParserGetParam (P, tptBoolean, '', Batch) Else
                If S1 = 'LOG' Then
                Begin
                  ParserGetParam (P, tptFilePath, '', S1);
                  Log := DefaultName (S1, 'log', Cnf. Path);
                End Else
                If S1 = 'SELECTION' Then
                Begin
                  ParserGetParam (P, tptString, '', S1);
                  If Length (S1) > 0 Then Selection := S1 [1] Else Selection := #255;
                End Else
                If S1 = 'DL_COMMAND' Then ParserGetParam (P, tptString, '', DLCommand) Else
                If S1 = 'UL_COMMAND' Then ParserGetParam (P, tptString, '', ULCommand) Else
                If S1 = 'DL_KEYWORD' Then ParserGetParam (P, tptQuote, '', DLKeyWord) Else
                If S1 = 'UL_KEYWORD' Then ParserGetParam (P, tptQuote, '', ULKeyWord) Else
                If S1 = 'WORD_OFFSET' Then ParserGetParam (P, tptByte, '', WordOffs) Else
                If S1 = 'LIST' Then
                Begin
                  ParserGetParam (P, tptFilePath, '', S1);
                  List := DefaultName (S1, 'lst', Cnf. DoorInfoDir);
                End;
              End;
          End;

          ParserClose (P);
        End;

    Dispose (Prots, Done);
  End;

  If Select Then
    ComWriteLn (ProtocolDef. Name, 0);
  R. Protocol := C;
  UpdateUserMacro;
End;

Function fGetAnswer (Const Quest: String; Len, Options: Byte; Default: String): String;
Var
  oAttr : Byte;

Begin
  oAttr := TextAttr;
  If Options And ofFramed <> 0 Then
    Frame;
  ComWrite (Quest, eoMacro + eoCodes + eoNoFlush);
  ComReadLn (Default, Len, Options);
  ComWrite (EmuRelColor (oAttr), 0);
  fGetAnswer := Default;
End;

Function fQuery (Const What: String; IsYes: Boolean; Flags: Byte): Boolean;
Var
  YesPos, NoPos,
  MaxLength, i        : Integer;
  oX                  : Byte;
  C                   : Char;
  AnswerYes, AnswerNo : String [2];
  sYes, sNo           : String [20];
  S                   : String [40];

  Procedure WriteYesNo;
  Var
    S1, S2 : String [20];

  Begin
    S1 := Copy (sYes, 1, YesPos - 1);
    S2 := Copy (sYes, YesPos + 1, 255);

    ComWrite (EmuRelColor ($1B) + ' ', eoNoFlush);
    If S1 <> '' Then
      ComWrite (EmuRelColor ($1F) + S1, eoNoFlush);
    ComWrite (EmuRelColor ($1E) + Copy (sYes, YesPos, 1), eoNoFlush);
    If S2 <> '' Then
      ComWrite (EmuRelColor ($1F) + S2, eoNoFlush);
    ComWrite (EmuRelColor ($1B) + ' ', eoNoFlush);
    ComWrite (EmuRelColor ($0F) + '  ', eoNoFlush);

    S1 := Copy (sNo, 1, NoPos - 1);
    S2 := Copy (sNo, NoPos + 1, 255);

    If S1 <> '' Then
      ComWrite (S1, eoNoFlush);
    ComWrite (EmuRelColor ($0E) + Copy (sNo, NoPos, 1), eoNoFlush);
    If S2 <> '' Then
      ComWrite (EmuRelColor ($0F) + S2, eoNoFlush);
    ComWrite (' ', 0);
  End;

  Procedure WriteNoYes;
  Var
    S1, S2 : String [20];

  Begin
    S1 := Copy (sYes, 1, YesPos - 1);
    S2 := Copy (sYes, YesPos + 1, 255);

    ComWrite (EmuRelColor ($0F) + ' ', eoNoFlush);
    If S1 <> '' Then
      ComWrite (S1, eoNoFlush);
    ComWrite (EmuRelColor ($0E) + Copy (sYes, YesPos, 1), eoNoFlush);
    If S2 <> '' Then
      ComWrite (EmuRelColor ($0F) + S2, eoNoFlush);
    ComWrite ('  ' + EmuRelColor ($1B) + ' ', eoNoFlush);

    S1 := Copy (sNo, 1, NoPos - 1);
    S2 := Copy (sNo, NoPos + 1, 255);

    If S1 <> '' Then
      ComWrite (EmuRelColor ($1F) + S1, eoNoFlush);
    ComWrite (EmuRelColor ($1E) + Copy (sNo, NoPos, 1), eoNoFlush);
    If S2 <> '' Then
      ComWrite (EmuRelColor ($1F) + S2, eoNoFlush);
    ComWrite (EmuRelColor ($1B) + ' ', 0);
  End;

Begin
  fQuery := False;

  If Flags And ofFramed <> 0 Then
    Frame;
  ComWrite (What + ' ', eoMacro + eoCodes);

  S := UpString (lang (laYesNoKeys));

  If Cnf. YesNoStyle And (R. Emu <> teTty) Then
  Begin
    sYes := ZeroMsg (lang (laYes), True);
    sNo := ZeroMsg (lang (laNo), True);
    AnswerYes := S [1];
    AnswerNo := S [2];
    YesPos := Pos (AnswerYes, sYes);
    If YesPos = 0 Then
    Begin
      Inc (YesPos);
      AnswerYes := AnswerYes + UpCase (sYes [YesPos]);
    End;
    NoPos := Pos (AnswerNo, sNo);
    If NoPos = 0 Then
    Begin
      Inc (NoPos);
      AnswerNo := AnswerNo + UpCase (sNo [NoPos]);
    End;
    MaxLength := Length (sYes) + Length (sNo) + 5;

    If isYes Then WriteYesNo
             Else WriteNoYes;

    Repeat
      C := UpCase (ComReadKey);

      Case C Of

          #13: Break;

      kbRight: If isYes Then
               Begin
                 isYes := False;
                 ComWrite (EmuCursorLeft (MaxLength), eoNoFlush);
                 WriteNoYes;
               End;

       kbLeft: If Not isYes Then
               Begin
                 isYes := True;
                 ComWrite (EmuCursorLeft (MaxLength), eoNoFlush);
                 WriteYesNo;
               End;
      Else
        If Pos (C, AnswerYes) > 0 Then
        Begin
          isYes := True;
          Break;
        End;

        If Pos (C, AnswerNo) > 0 Then
        Begin
          isYes := False;
          Break;
        End;
      End;
    Until False;

    If isYes Then S := lang (laYes)
             Else S := lang (laNo);
    ComWrite (EmuCursorLeft (MaxLength) + EmuRelColor ($07), eoNoFlush);
    ComWriteLn (Pad (S, MaxLength + Length (S) - Length (ZeroMsg (S, True))),
      eoMacro + eoCodes);
    If Flags And ofNoCR = 0 Then
      ComWriteLn ('', 0);
  End Else
  Begin
    S := S + #13;
    oX := WhereX;
    If IsYes Then ComWrite (lang (laYesNo), eoMacro + eoCodes)
             Else ComWrite (lang (laNoYes), eoMacro + eoCodes);

    Repeat
      ProcessChoices;
      i := Pos (UpCase (ComReadKey), S);
    Until i > 0;

    Case i Of
      1 : IsYes := True;
      2 : IsYes := False;
    End;

    i := WhereX - oX;
    oX := WhereX;
    If i > 0 Then
      ComWrite (EmuCursorLeft (i), eoNoFlush);
    If isYes Then ComWrite (lang (laYes), eoMacro + eoCodes)
             Else ComWrite (lang (laNo), eoMacro + eoCodes);
    i := oX - WhereX;
    If i > 0 Then
      ComWrite (Replicate (' ', i), eoNoFlush);
    ComWrite (#13#10#10, 0);
  End;

  fQuery := IsYes;
End;

Function fQuery_YNQ (Const What: String; IsYes: Boolean): Char;
Var
  i         : Integer;
  oAttr, oX : Byte;
  S         : String [80];

Begin
  oAttr := TextAttr;
  ComWrite (What + ' ', eoMacro + eoCodes);
  oX := WhereX;
  If IsYes Then i := laYesNoQuit
           Else i := laNoYesQuit;
  ComWrite (lang (i), eoMacro + eoCodes);
  S := UpString (lang (laYNQKeys)) + #13#27;

  Repeat
    ProcessChoices;
    i := Pos (UpCase (ComReadKey), S);
  Until i > 0;

  If i = 4 Then
    If IsYes Then i := 1
             Else i := 2;

  Case i Of
       1 : Begin
             S := lang (laYes);
             fQuery_YNQ := 'y';
           End;
       2 : Begin
             S := lang (laNo);
             fQuery_YNQ := 'n';
           End;
  Else
    S := lang (laQuit);
    fQuery_YNQ := 'q';
  End;

  i := WhereX - oX;
  If i > 0 Then
    ComWrite (EmuCursorLeft (i), eoNoFlush);
  ComWrite (S, eoMacro + eoCodes + eoNoFlush);
  ComWriteLn (EmuRelColor (oAttr) + EmuClrEOL, 0);
End;

Function fMenuBar (Const Text: String; Hot: String): Byte;
Var
  i      : Integer;
  Choice : Char;

Begin
  Hot := UpString (Hot);

  If HotKeysStr = '' Then
    ComWrite (Text, eoMacro + eoCodes);

  Repeat
    ProcessChoices;
    Choice := UpCase (ComReadKey);
    i := Pos (Choice, Hot);
  Until i > 0;

  fMenuBar := i;
End;

Function GetMaxStr (Const Msg: String): String;
Begin
  GetMaxStr := GetAnswer (Msg, 78 - Length (ZeroMsg (Copy (Msg,
    PosLastChar ('|', Msg) + 1, 255), True)), ofAllowEmpty, '');
End;

Procedure InitMainCOvr;
Begin
  ComRead := fComRead;
  ComReadLn := fComReadLn;
  ComMenu := fComMenu;
  SetProtocol := fSetProtocol;
  GetAnswer := fGetAnswer;
  Query := fQuery;
  Query_YNQ := fQuery_YNQ;
  MenuBar := fMenuBar;
End;

End.
