{$IFDEF MSDOS}
  {$IFNDEF DPMI32}
    {$F+}
  {$IFNDEF DPMI}
    {$DEFINE RealMode}
    {$O+}
  {$ENDIF}
  {$ENDIF}
{$ENDIF}
{$I-}

Unit
  tFSed;

Interface

Uses
  Objects,
  tGlob,
  tMisc,
  MainComm,
  skMHL;

Type
  tsChar = Set Of Char;
  tLineEditResult = (mpSave, mpAbort, mpContinue, mpEditLine, mpShow,
                     mpDeleteLine);
  tEditMenuProc = Function (Var S: String): tLineEditResult;
  tEditLineProc = Procedure (Var S: String);

Const
  ofsQuoting = $01;

Function QuoteMsg (Var Msg: PMessageBase; Var F: Text; QuotePrefix: String): Integer;
Function fsEditFile (Const FN: String; ExitKeys: tsChar; StartLine: Integer;
                     Options, aQuote, aNormal, X1, Y1, X2, Y2: Byte): Char;
Function lnEditFile (Const FN: String; Menu: tEditMenuProc;
                     EditLineProc: tEditLineProc; aQuote,
                     aNormal: Byte): tLineEditResult;

Implementation

Uses
  Strings,
  skCommon;

Type
  PLineCollection = ^TLineCollection;
  TLineCollection = Object (TCollection)
    Procedure FreeItem (Item: Pointer); Virtual;
    Procedure ChangeLine (Index: Integer; Const S: String);
  End;

Const
{$IFDEF RealMode}
  LineBlockSize = 8;
  TextBufStart  = 32;
  TextBufDelta  = 16;
{$ELSE}
  LineBlockSize = 16;
  TextBufStart  = 64;
  TextBufDelta  = 32;
{$ENDIF}
  LineBlockMask = $FFFF - (LineBlockSize - 1);

  kbBackSpace    = #8;
  kbEnter        = #13;
  kbEscape       = #27;
  kbCtrlY        = #25;

  EditWordDelims = [' '];

Var
  MaxStrLen, OldAttr : Integer;
  TextBuf            : PLineCollection;

Function NewLine (Const S: String): Pointer;
Var
  P : PString;

Begin
  GetMem (P, (Length (S) + LineBlockSize) And LineBlockMask);
  P^ := S;
  NewLine := P;
End;

Procedure TLineCollection. FreeItem (Item: Pointer);
Begin
  If Item <> Nil Then
    FreeMem (Item, (Length (PString (Item)^) + LineBlockSize) And
      LineBlockMask);
End;

Procedure TLineCollection. ChangeLine (Index: Integer; Const S: String);
Var
  Old            : PString;
  NewLen, OldLen : Integer;

Begin
  NewLen := (Length (S) + LineBlockSize) And LineBlockMask;
  Old := At (Index);

  If Old <> Nil Then
  Begin
    OldLen := (Length (Old^) + LineBlockSize) And LineBlockMask;

    If OldLen = NewLen Then
    Begin
      Old^ := S;
      Exit;
    End;

    FreeMem (Old, OldLen);
  End;

  GetMem (Old, NewLen);
  Old^ := S;
  AtPut (Index, Old);
End;

Procedure Load (Const FName: String);
Var
  F       : Text;
  S       : String;
  FileBuf : FileBufArr;

Begin
  New (TextBuf, Init (TextBufStart, TextBufDelta));

  Assign (F, FName);
  ReSet (F);
  SetTextBuf (F, FileBuf, FileBufSize);

  If IOResult = 0 Then
  Begin
    While Not EoF (F) Do
    Begin
      ReadLn (F, S);
      TextBuf^. Insert (NewLine (S));
    End;

    Close (F);
  End;
End;

Procedure Save (Const FName: String);
Var
  i       : Integer;
  F       : Text;
  FileBuf : FileBufArr;

Begin
  Assign (F, FName);
  ReWrite (F);
  SetTextBuf (F, FileBuf, FileBufSize);

  For i := 0 To TextBuf^. Count-1 Do
    WriteLn (F, TrimTrail (PString (TextBuf^. At (i))^));

  Close (F);
End;

Procedure SetAttr (A: Byte); {$IFDEF VirtualPascal} Inline; {$ENDIF}
Begin
  If A <> OldAttr Then
  Begin
    OldAttr := A;
    ComWrite (EmuRelColor (A), eoNoFlush);
  End;
End;

Function GetLineColor (Quote, Normal: Byte; Const S: String): Byte;
         {$IFDEF VirtualPascal} Inline; {$ENDIF}
Begin
  If Pos (QuoteChar, Copy (S, 1, MaxQuotePos)) > 0 Then
    GetLineColor := Quote
  Else
    GetLineColor := Normal;
End;

Function GetQuoteLen (Const S: String): Integer;
Var
  QPos : Integer;

Begin
  QPos := Pos (QuoteChar, Copy (S, 1, MaxQuotePos));

  If QPos > 0 Then
  Begin
    While (QPos < Length (S)) And (S [QPos + 1] = QuoteChar) Do
      Inc (QPos);

    If (QPos < Length (S)) And (S [QPos + 1] = ' ') Then
      Inc (QPos);
    If QPos > MaxQuoteLen Then
      QPos := MaxQuoteLen;
  End;

  GetQuoteLen := QPos;
End;

Function SameQuote (Const S1, S2: String): Integer;
Var
  QLen : Integer;

Begin
  SameQuote := 0;
  QLen := GetQuoteLen (S1);

  If QLen > 0 Then
    If GetQuoteLen (S2) = QLen Then
      If Copy (S1, 1, QLen) = Copy (S2, 1, QLen) Then
        SameQuote := QLen;
End;

Function QuoteMsg (Var Msg: PMessageBase; Var F: Text; QuotePrefix: String): Integer;
Var
  LineBuf : PChar;

  Function GetStr (Var S: String): Boolean;
  Begin
    If StrLen (LineBuf) <> 0 Then
    Begin
      S := SplitStringPChar (LineBuf, 255, True);
      GetStr := True;
      Exit;
    End;

    While Not Msg^. EndOfMessage Do
    Begin
      Msg^. GetStringPChar (LineBuf, MaxLineSize);
      S := SplitStringPChar (LineBuf, 255, True);

      If S <> '' Then
        If (S [1] = #1) Or
           (S = '---') Or
           (Copy (S, 1, 4) = '--- ') Or
           (Copy (S, 1, 10) = ' * Origin:')
        Then
          Continue;

      GetStr := True;
      Exit;
    End;

    S := '';
    GetStr := False;
  End;

Type
  TextStatus = (tNone, tQuoted, tNotQuoted);

Var
  Lines, OldQLen,
  NewQLen, PrefixLen : Integer;
  Status             : TextStatus;
  EndOfMsg           : Boolean;
  OldQ, NewQ, S, S1  : String;

Label
  Again, EndOfProc;

Begin
  If Not Msg^. OpenMessage Then
  Begin
    QuoteMsg := 0;
    Exit;
  End;

  Msg^. SetTextPos (Msg^. AfterLastKludge);

  QuotePrefix := ' ' + QuotePrefix + QuoteChar + ' ';
  PrefixLen := Length (QuotePrefix);
  Status := tNone;
  Lines := 0;

  GetMem (LineBuf, MaxLineSize);
  StrCopy (LineBuf, '');

  While GetStr (S) Do
  Begin

  Again:
    If Length (S) = 0 Then
    Begin
      If Status <> tNone Then
      Begin
        WriteLn (F, '');
        Inc (Lines);
        Status := tNone;
      End;
    End Else
    Begin
      OldQLen := GetQuoteLen (S);

      If OldQLen > 0 Then
      Begin
        If Status = tNotQuoted Then
        Begin
          WriteLn (F, '');
          Inc (Lines);
        End;

        Status := tQuoted;
        OldQ := Copy (S, 1, OldQLen);
        Delete (S, 1, OldQLen);
        NewQ := OldQ;
        If OldQLen < MaxQuoteLen Then
          Insert (QuoteChar, NewQ, Pos (QuoteChar, NewQ));
        NewQLen := Length (NewQ);
        WriteLn (F, NewQ + TrimTrail (SplitString (S, QuoteMargin - NewQLen)));
        Inc (Lines);

        While Length (S) > 0 Do
        Begin
          EndOfMsg := Not GetStr (S1);
          If EndOfMsg Or (GetQuoteLen (S1) <> OldQLen) Or
             (Copy (S1, 1, OldQLen) <> OldQ) Then
          Begin
            While Length (S) > 0 Do
            Begin
              WriteLn (F, NewQ + TrimTrail (SplitString (S, QuoteMargin -
                NewQLen)));
              Inc (Lines);
            End;

            If EndOfMsg Then
              Goto EndOfProc;

            S := S1;
            Goto Again;
          End;

          Delete (S1, 1, OldQLen);
          S1 := ' ' + TrimLead (S1);

          While (Length (S) + Length (S1) > 255) Do
          Begin
            WriteLn (F, NewQ + TrimTrail (SplitString (S, QuoteMargin -
              NewQLen)));
            Inc (Lines);
          End;

          S := S + S1;
          WriteLn (F, NewQ + TrimTrail (SplitString (S, QuoteMargin -
            NewQLen)));
          Inc (Lines);
        End;
      End Else
      Begin
        If Status = tQuoted Then
        Begin
          WriteLn (F, '');
          Inc (Lines);
        End;

        Status := tNotQuoted;
        WriteLn (F, QuotePrefix + TrimTrail (SplitString (S, QuoteMargin -
          PrefixLen)));
        Inc (Lines);

        While Length (S) > 0 Do
        Begin
          EndOfMsg := Not GetStr (S1);
          If (Length (S1) = 0) Or (GetQuoteLen (S1) > 0) Then
          Begin
            While Length (S) > 0 Do
            Begin
              WriteLn (F, QuotePrefix + TrimTrail (SplitString (S,
                QuoteMargin - PrefixLen)));
              Inc (Lines);
            End;

            If EndOfMsg Then
              Goto EndOfProc;

            S := S1;
            Goto Again;
          End;

          S1 := ' ' + TrimLead (S1);
          While (Length (S) + Length (S1) > 255) Do
          Begin
            WriteLn (F, QuotePrefix + TrimTrail (SplitString (S, QuoteMargin -
              PrefixLen)));
            Inc (Lines);
          End;

          S := S + S1;
          WriteLn (F, QuotePrefix + TrimTrail (SplitString (S, QuoteMargin -
            PrefixLen)));
          Inc (Lines);
        End;
      End;
    End;
  End;

EndOfProc:
  FreeMem (LineBuf, MaxLineSize);
  QuoteMsg := Lines;
  Msg^. CloseMessage;
End;

Function fsEditFile;
Var
  Current, TopLine    : Integer;
  X, Y, CurrColor     : Byte;
  Vizualize, Modified : Boolean;
  S, S1, S2           : String;

  Procedure GetCurrColor;
  Begin
    CurrColor := GetLineColor (aQuote, aNormal, PString (TextBuf^. At (Current
      - 1))^);
  End;

  Procedure WriteLine (Const S: String);
  Begin
    SetAttr (GetLineColor (aQuote, aNormal, S));
    ComWrite (S + EmuClrEOL, 0);
  End;

  Procedure ReWriteCurrentLine;
  Begin
    ComWrite (EmuGotoXY (X1, Y1 + Y - 1), eoNoFlush);
    WriteLine (PString (TextBuf^. At (Current - 1))^);
  End;

  Procedure DispCursor;
  Begin
    ComWrite (EmuGotoXY (X1 + X - 1, Y1 + Y - 1), 0);
  End;

  Procedure ReDraw;
  Var
    i, j : Integer;

  Begin
    If Vizualize Then
    Begin
      j := TopLine + Y2 - Y1;
      If j > TextBuf^. Count Then
        j := TextBuf^. Count;

      For i := TopLine To j Do
      Begin
        ComWrite (EmuGotoXY (X1, Y1 + i - TopLine), eoNoFlush);
        WriteLine (PString (TextBuf^. At (i - 1))^);
      End;
    End;
  End;

  Procedure MoveTextUp;
  Var
    Len : Integer;

  Begin
    If Current > 1 Then
    Begin
      Dec (Current);
      Len := Length (PString (TextBuf^. At (Current - 1))^) + 1;
      If X > Len Then
        X := Len;

      Dec (Y);
      If Y < 1 Then
      Begin
        Y := (Y2 - Y1 + 2) Shr 1;
        Dec (TopLine, Y);
        If TopLine < 1 Then
        Begin
          TopLine := 1;
          Y := Current;
        End;

        ReDraw;
      End;

      GetCurrColor;
      If Vizualize Then
        DispCursor;
    End;
  End;

  Procedure MoveTextDown;
  Var
    Len : Integer;

  Begin
    If Current < TextBuf^. Count Then
    Begin
      Inc (Current);
      Len := Length (PString (TextBuf^. At (Current - 1))^) + 1;
      If X > Len Then
        X := Len;

      Inc (Y);
      If Y > Y2 - Y1 + 1 Then
      Begin
        Inc (TopLine, (Y2 - Y1 + 2) Shr 1);
        If TopLine + Y2 - Y1 > TextBuf^. Count Then
          TopLine := TextBuf^. Count - Y2 + Y1;
        Y := Current - TopLine + 1;

        ReDraw;
      End;

      GetCurrColor;
      If Vizualize Then
        DispCursor;
    End;
  End;

  Procedure MoveTextLeft;
  Begin
    If X > 1 Then
    Begin
      Dec (X);
      ComWrite (EmuCursorLeft (1), 0);
    End
    Else
      If Current > 1 Then
      Begin
        MoveTextUp;
        X := Length (PString (TextBuf^. At (Current - 1))^) + 1;
        DispCursor;
      End;
  End;

  Procedure MoveTextRight;
  Var
    P : PString;

  Begin
    P := TextBuf^. At (Current - 1);
    If X <= Length (P^) Then
    Begin
      SetAttr (CurrColor);
      ComWrite (P^ [X], 0);
      Inc (X);
    End
    Else
      If Current < TextBuf^. Count Then
      Begin
        MoveTextDown;
        X := 1;
        DispCursor;
      End;
  End;

  Procedure InsLine (Num: Integer);
  Begin
    TextBuf^. AtInsert (Num - 1, NewLine (''));
  End;

  Procedure InsCurrentLine;
  Var
    i, j : Integer;

  Begin
    InsLine (Current);
    If Y = Y2 - Y1 + 1 Then
      MoveTextDown
    Else
    Begin
      Inc (Y);
      Inc (Current);
      If Vizualize Then
      Begin
        j := TopLine + Y2 - Y1;
        If j > TextBuf^. Count Then
          j := TextBuf^. Count;

        For i := TopLine + Y - 2 To j Do
        Begin
          ComWrite (EmuGotoXY (X1, Y1 + i - TopLine), eoNoFlush);
          WriteLine (PString (TextBuf^. At (i - 1))^);
        End;

        DispCursor;
      End;
    End;
  End;

  Procedure DelLine;
  Var
    i : Integer;

  Begin
    Modified := True;

    If TextBuf^. Count = 1 Then
    Begin
      ComWrite (EmuGotoXY (X1, Y1) + EmuClrEOL, 0);
      TextBuf^. ChangeLine (0, '');
      GetCurrColor;
      Exit;
    End;

    TextBuf^. AtFree (Current - 1);
    If Current > TextBuf^. Count Then
      MoveTextUp;

    GetCurrColor;
    If Vizualize Then
    Begin
      For i := TopLine + Y - 1 To TopLine + Y2 - Y1 Do
      Begin
        ComWrite (EmuGotoXY (X1, Y1 + i - TopLine), eoNoFlush);
        If i <= TextBuf^. Count Then
          WriteLine (PString (TextBuf^. At (i - 1))^)
        Else
          ComWrite (EmuClrEOL, 0);
      End;

      DispCursor;
    End;
  End;

  Procedure InsChar (C: Char);
  Var
    QLen : Integer;

    Procedure SplitStr;
    Var
      i, Len : Integer;

    Begin
      S := TrimTrail (S);
      i := MaxStrLen - QLen;
      S1 := Copy (S, i + 1, 255);
      S := Copy (S, 1, i);
      If Length (S1) > 0 Then
      Begin
        S1 := S1 + ' ';
        i := Length (S);
        If Not (S [i] in EditWordDelims) Then
        Begin
          S2 := ExtractWord (WordCount (S, EditWordDelims), S, EditWordDelims);
          Len := Length (S2);
          If i <> Len Then
          Begin
            S1 := S2 + S1;
            S := TrimTrail (Copy (S, 1, i - Len));
            If S = '' Then
            Begin
              S := S1;
              SplitStr;
            End;
          End
          Else
            If Not (S1 [1] in EditWordDelims) And
               (WordCount (S1, EditWordDelims) = 1)
            Then
              S1 := TrimTrail (S1);
        End;
      End;
    End;

  Var
    i, NextLen            : Integer;
    OldCurrColor          : Byte;
    PosStr, NextStr, QStr : String;

  Begin
    Modified := True;

    S := PString (TextBuf^. At (Current - 1))^;
    Insert (C, S, X);
    i := Length (S);
    Inc (X);

    If i <= MaxStrLen Then
    Begin
      TextBuf^. ChangeLine (Current - 1, S);
      OldCurrColor := CurrColor;
      GetCurrColor;
      If OldCurrColor = CurrColor Then
      Begin
        SetAttr (CurrColor);
        ComWrite (Copy (S, X - 1, i - X + 2), 0);
        If X <= i Then
          DispCursor;
      End Else
      Begin
        ReWriteCurrentLine;
        DispCursor;
      End;
    End Else
    Begin
      PosStr := TrimTrail (Copy (S, X, 255));
      QLen := GetQuoteLen (S);
      If QLen > 0 Then
      Begin
        QStr := Copy (S, 1, QLen);
        Delete (S, 1, QLen);
      End
      Else
        QStr := '';

      If X > i Then
      Begin
        SplitStr;
        TextBuf^. ChangeLine (Current - 1, QStr + S);
        TextBuf^. AtInsert (Current, NewLine (QStr + TrimTrail (S1)));

        Vizualize := False;
        MoveTextDown;
        Vizualize := True;
      End Else
      Begin
        i := Current;

        Repeat
          SplitStr;
          TextBuf^. ChangeLine (i - 1, QStr + S);
          Inc (i);
          If i > TextBuf^. Count Then
            TextBuf^. Insert (NewLine (QStr));

          NextStr := PString (TextBuf^. At (i - 1))^;
          If Not ((GetQuoteLen (NextStr) = QLen) And
                  (SameQuote (QStr, NextStr) = QLen)) Then
          Begin
            InsLine (i);
            NextStr := QStr;
          End;

          NextStr := TrimLead (Copy (NextStr, QLen + 1, 255));
          NextLen := Length (NextStr);

          While (Length (S1) + NextLen > 255) Do
          Begin
            InsLine (i);
            S := S1;
            SplitStr;
            TextBuf^. ChangeLine (i - 1, QStr + S);
            Inc (i);
          End;

          S := S1 + NextStr;
        Until Length (S) <= MaxStrLen - QLen;

        TextBuf^. ChangeLine (i - 1, QStr + S);

        If X > Length (PString (TextBuf^. At (Current - 1))^) + 1 Then
        Begin
          Vizualize := False;
          MoveTextDown;
          Vizualize := True;

          X := Pos (PosStr, PString (TextBuf^. At (Current - 1))^);
          If X = 0 Then
            X := QLen + 1;
        End;
      End;

      GetCurrColor;
      ReDraw;
      DispCursor;
    End;
  End;

  Procedure DelChar;

    Procedure KillNextLine;
    Var
      OldX : Byte;
      Dont : Boolean;

    Begin
      OldX := X;
      Dont := TextBuf^. Count - Current = 1;

      Vizualize := False;
      MoveTextDown;
      Vizualize := True;

      DelLine;

      If Not Dont Then
      Begin
        Vizualize := False;
        MoveTextUp;
        Vizualize := True;
      End;
      X := OldX;
    End;

  Var
    P, P1        : PString;
    QLen         : Integer;
    OldCurrColor : Byte;

  Begin
    Modified := True;

    P := TextBuf^. At (Current - 1);
    If P^ = '' Then
      DelLine
    Else
      If X > Length (P^) Then
      Begin
        If Current = TextBuf^. Count Then
          Exit;

        P1 := TextBuf^. At (Current);
        If P1^ = '' Then
          KillNextLine
        Else
        Begin
          QLen := SameQuote (P^, P1^);
          S := Copy (P1^, 1 + QLen, MaxStrLen - Length (P^));

          If (Length (P1^) - QLen > Length (S) + 1) And
             Not (P1^ [Length (S) + 1 + QLen] in EditWordDelims) Then
          Begin
            S2 := ExtractWord (WordCount (S, EditWordDelims), S, EditWordDelims);
            S := Copy (S, 1, Length (S) - Length (S2) - 1);
          End;

          TextBuf^. ChangeLine (Current - 1, P^ + S);
          S := Trim (Copy (P1^, 1 + QLen + Length (S), 255));

          If Length (S) = 0 Then
            KillNextLine
          Else
            TextBuf^. ChangeLine (Current, Copy (P1^, 1, QLen) + S);

          GetCurrColor;
          ReDraw;
        End;
      End Else
      Begin
        S := P^;
        Delete (S, X, 1);
        TextBuf^. ChangeLine (Current - 1, S);
        OldCurrColor := CurrColor;
        GetCurrColor;
        If CurrColor = OldCurrColor Then
        Begin
          SetAttr (CurrColor);
          ComWrite (Copy (S, X, MaxStrLen - X) + ' ', 0);
        End
        Else
          ReWriteCurrentLine;
      End;

    DispCursor;
  End;

  Procedure BackSpace;
  Var
    P, P1        : PString;
    i, QLen      : Integer;
    OldCurrColor : Byte;
    Also         : Boolean;

  Begin
    Modified := True;

    P := TextBuf^. At (Current - 1);
    If X > 1 Then
    Begin
      Dec (X);
      S := P^;
      Delete (S, X, 1);
      TextBuf^. ChangeLine (Current - 1, S);
      OldCurrColor := CurrColor;
      GetCurrColor;
      If CurrColor = OldCurrColor Then
      Begin
        SetAttr (CurrColor);
        ComWrite (#8 + Copy (S, X, MaxStrLen - X) + ' ', 0);
      End
      Else
        ReWriteCurrentLine;
      DispCursor;
    End
    Else
      If Current > 1 Then
      Begin
        P1 := TextBuf^. At (Current - 2);
        QLen := SameQuote (P^, P1^);
        i := Length (P1^);
        If Length (P^) - QLen <= MaxStrLen - i Then
        Begin
          Also := TextBuf^. Count - Current = 1;
          TextBuf^. ChangeLine (Current - 2, P1^ + Copy (P^, QLen + 1,
            MaxStrLen - QLen));
          DelLine;

          If (Current <> TextBuf^. Count) Or Also Then
          Begin
            Vizualize := False;
            MoveTextUp;
            Vizualize := True;
          End;

          X := i + 1;
          GetCurrColor;
          ReDraw;
          DispCursor;
        End;
      End;
  End;

  Procedure Enter;
  Var
    P    : PString;
    QLen : Integer;

  Begin
    Modified := True;

    If X = 1 Then
      InsCurrentLine
    Else
    Begin
      P := TextBuf^. At (Current - 1);
      If X <= Length (P^) Then
      Begin
        QLen := GetQuoteLen (P^);
        If X < QLen + 1 Then
          X := QLen + 1;
        TextBuf^. AtInsert (Current, NewLine (Copy (P^, 1, QLen) +
          Copy (P^, X, 255)));
        TextBuf^. ChangeLine (Current - 1, Copy (P^, 1, X - 1));
      End
      Else
        TextBuf^. AtInsert (Current, NewLine (''));

      Vizualize := False;
      MoveTextDown;
      Vizualize := True;

      X := 1;
      ReDraw;
      DispCursor;
    End;

    GetCurrColor;
  End;

Var
  i : Integer;
  C : Char;

Begin
  Vizualize := True;
  MaxStrLen := X2 - X1;
  Load (FN);
  TextBuf^. Insert (NewLine (''));

  Y := (Y2 - Y1 + 2) Shr 1;
  X := 1;
  If StartLine <= TextBuf^. Count Then Current := StartLine
                                  Else Current := TextBuf^. Count;
  TopLine := Current - Y + 1;
  If TopLine < 1 Then
  Begin
    TopLine := 1;
    Y := Current;
  End;

  Modified := False;
  OldAttr := -1;

  For i := TopLine To TopLine + Y2 - Y1 Do
  Begin
    ComWrite (EmuGotoXY (X1, Y1 + i - TopLine), eoNoFlush);
    If i <= TextBuf^. Count Then
      WriteLine (PString (TextBuf^. At (i - 1))^)
    Else
      ComWrite (EmuClrEOL, 0);
  End;

  DispCursor;
  GetCurrColor;

  Repeat
    C := ComReadKey;

    Case C Of
        kbUp    : MoveTextUp;
        kbDown  : MoveTextDown;
        kbLeft  : MoveTextLeft;
        kbRight : MoveTextRight;
        kbEnter : Enter;
    kbBackSpace : BackSpace;
          kbDel : DelChar;
        kbCtrlY : DelLine;
         kbHome : If X > 1 Then
                  Begin
                    X := 1;
                    DispCursor;
                  End;
          kbEnd : Begin
                    i := Length (PString (TextBuf^. At (Current - 1))^) + 1;
                    If X < i Then
                    Begin
                      X := i;
                      DispCursor;
                    End;
                  End;
    Else
      If C in ExitKeys Then Break
                       Else InsChar (C);
    End;
  Until False;

  If Modified Then
    Save (FN);
  Dispose (TextBuf, Done);

  fsEditFile := C;
End;

Function lnEditFile;
Var
  S, S1 : String;

  Procedure NewStr (Num: Integer);
  Begin
    SetAttr (aNormal);
    ComWrite (LeftPad (Long2Str (Num), 3) + ': ' , 0);
  End;

  Procedure EnterText;
  Var
    Finish        : Boolean;
    K             : Char;

    Procedure EndOfLine;
    Begin
      If Length (S) > 0 Then
      Begin
        TextBuf^. Insert (NewLine (S));
        ComWriteLn ('', 0);
        NewStr (TextBuf^. Count + 1);
        S := '';
      End Else
      Begin
        Finish := True;
        ComWriteLn ('', 0);
      End;
    End;

    Procedure DoChar (K: Char);
    Var
      Words : Integer;

    Begin
      S := S + K;
      If Length (S) > MaxStrLen - 5 Then
      Begin
        Words := WordCount (S, EditWordDelims);
        If (Words > 1) And (Not (S [Length (S)] in EditWordDelims)) Then
          S1 := ExtractWord (Words, S, EditWordDelims)
        Else
          S1 := Copy (S, MaxStrLen - 4, 255);
        ComWrite (Replicate (#8, Length (S1) - 1), eoNoFlush);
        ComWrite (EmuClrEOL, 0);
        S := Copy (S, 1, Length (S) - Length (S1));
        EndOfLine;
        If Not Finish Then
        Begin
          S := S1;
          ComWrite (S, 0);
        End;
      End
      Else
        ComWrite (K, 0);
    End;

  Begin
    Finish := False;
    NewStr (TextBuf^. Count + 1);
    S := '';

    Repeat
      K := ComReadKey;

      Case K Of
        #8: If Length (S) > 0 Then
            Begin
              SetLength (S, Length (S) - 1);
              ComWrite (#8#32#8, 0);
            End;
       #13: EndOfLine;
      Else
        If K in [#32..#255] Then
          DoChar (K);
      End;
    Until Finish;
  End;

  Procedure ListMsg (Pause: Boolean);
  Var
    i : Integer;
    P : PString;

  Begin
    If Pause Then
      InitMore (0);
    For i := 1 To TextBuf^. Count Do
    Begin
      NewStr (i);
      P := TextBuf^. At (i - 1);
      SetAttr (GetLineColor (aQuote, aNormal, P^));
      ComWriteLn (Copy (P^, 1, MaxStrLen - 5), 0);
      If Pause Then
        If Not More Then
          Exit;
    End;
  End;

Var
  i, j, sLine, eLine, Words : Integer;
  R                         : tLineEditResult;

Const
  DelMark = #253#254#255;

Begin
  MaxStrLen := 78;
  Load (FN);
  OldAttr := -1;
  ListMsg (False);
  EnterText;

  Repeat
    R := Menu (S);
    OldAttr := -1;

    Case R Of
       mpSave, mpAbort : Break;
            mpContinue : EnterText;
            mpEditLine : Begin
                           i := Str2Long (S);
                           If (i >= 1) And (i <= TextBuf^. Count) Then
                           Begin
                             S := PString (TextBuf^. At (i - 1))^;
                             NewStr (i);
                             SetAttr (GetLineColor (aQuote, aNormal, S));
                             S := Copy (S, 1, MaxStrLen - 5);
                             EditLineProc (S);
                             TextBuf^. ChangeLine (i - 1, S);
                           End
                           Else
                             Continue;
                         End;
                mpShow : ListMsg (True);
          mpDeleteLine : If TextBuf^. Count > 1 Then
                         Begin
                           Words := WordCount (S, SpaceAndComma);
                           For i := 1 To Words Do
                           Begin
                             S1 := ExtractWord (i, S, SpaceAndComma);
                             If Not ConsistsOf (S1, ['0'..'9', '-']) Then
                               Continue;

                             If Pos ('-', S1) > 0 Then
                             Begin
                               sLine := Str2Long (ExtractWord (1, S1, MinusOnly));
                               eLine := Str2Long (ExtractWord (2, S1, MinusOnly));

                               If (sLine >= 1) And (sLine <= TextBuf^. Count)
                                  And (sLine <= eLine) Then
                               Begin
                                 If eLine > TextBuf^. Count Then
                                   eLine := TextBuf^. Count;
                                 For j := sLine To eLine Do
                                   TextBuf^. ChangeLine (j - 1, DelMark);
                               End;
                             End Else
                             Begin
                               j := Str2Long (S1);
                               If (j >= 1) And (j <= TextBuf^. Count) Then
                                 TextBuf^. ChangeLine (j - 1, DelMark);
                             End;
                           End;

                           i := TextBuf^. Count;

                           While i > 0 Do
                           Begin
                             If PString (TextBuf^. At (i - 1))^ = DelMark Then
                               TextBuf^. AtFree (i - 1);
                             Dec (i);
                           End;
                         End;
    End;

  Until False;

  Save (FN);
  Dispose (TextBuf, Done);

  lnEditFile := R;
End;

End.
