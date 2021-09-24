{$IFDEF MSDOS}
  {$IFNDEF DPMI32}
    {$F+}
  {$IFNDEF DPMI}
    {$DEFINE RealMode}
  {$ENDIF}
  {$ENDIF}
{$ENDIF}
{$I-}

Unit
  Parser;

Interface

Uses
  tMisc,
  DOS,
  Objects,
  tGlob,
  Log;

Type
  PConfigParser = ^tConfigParser;
  tConfigParser = Record
    S                  : String;
    Handle, Define     : PCollection;
    Options            : Byte;
    GB, SectionChanged : Boolean;
  End;

  tParType = (tptBoolean, tptExtBoolean, tptString, tptUpString, tptFilePath,
              tptQuote, tptFixedList, tptByte, tptWord, tptTime, tptLongInt,
              tptColor);

Const
  tpoWriteLog    = 1;
  tpoWriteScreen = 2;

Function ParserOpen (Var P: tConfigParser; Const FN: String; Opts: Byte): Boolean;
Function ParserRead (Var P: tConfigParser; Var Section: String): String;
Procedure ParserGetParam (Var P: tConfigParser; ParType: tParType; Valid: String; Var R);
Function ParserEnd (Var P: tConfigParser): Boolean;
Function ParserEndSection (Var P: tConfigParser): Boolean;
Procedure ParserUnknown (Var P: tConfigParser);
Procedure ParserGetBack (Var P: tConfigParser);
Procedure ParserClose (Var P: tConfigParser);

Implementation

Const
  FileBufSize = {$IFDEF RealMode} 2048 {$ELSE} 4096 {$ENDIF};

Type
  FileBufArr = Array [0..FileBufSize-1] Of Char;

Type
  PHandle = ^THandle;
  THandle = Record
    F       : Text;
    LineNo  : LongInt;
    FName   : String;
    FileBuf : FileBufArr;
  End;

  PDefine = ^TDefine;
  TDefine = Record
    KeyWord, Replace : PString;
  End;

Var
  PH : PHandle;
  PD : PDefine;

Function ParserOpen (Var P: tConfigParser; Const FN: String; Opts: Byte): Boolean;
Begin
  ParserOpen := False;

  If FN <> '' Then
  Begin
    New (PH);

    With P Do
    Begin
      Options := Opts;
      GB := False;
      SectionChanged := False;
      PH^. LineNo := 0;
      PH^. FName := Trim (FN);
      Assign (PH^. F, PH^. FName);
      SetTextBuf (PH^. F, PH^. FileBuf, FileBufSize);
      ReSet (PH^. F);

      If IOResult = 0 Then
      Begin
        Handle := New (PCollection, Init (1, 1));
        Define := New (PCollection, Init (0, 4));
        Handle^. Insert (PH);
        ParserOpen := True;
      End
      Else
        Dispose (PH);
    End;
  End;
End;

Procedure ErrWrite (Var P: tConfigParser; Wrong: String);
Begin
  With P Do
    If Handle^. Count > 0 Then
    Begin
      With PHandle (Handle^. At (Handle^. Count-1))^ Do
        Wrong := UpString (FName) + ' (' + Long2Str (LineNo) + '): ' + Wrong;
      If Options And tpoWriteLog <> 0 Then
        LogWrite ('!', Wrong);
      If Options And tpoWriteScreen <> 0 Then
        WriteLn ('! ', Wrong);
    End;
End;

Function ParserRead (Var P: tConfigParser; Var Section: String): String;
Var
  i  : Word;
  H  : PHandle;
  S1 : String;

Begin
  With P Do
  Begin
    SectionChanged := False;

    While Handle^. Count > 0 Do
    Begin
      H := Handle^. At (Handle^. Count-1);

      While Not EoF (H^. F) Do
      Begin
        If Not GB Then
        Begin
          Inc (H^. LineNo);
          ReadLn (H^. F, S);
          i := AsciiPosCh (S, ';', '"');
          If i > 0 Then
            SetLength (S, i - 1);
          If S <> '' Then
            S := Trim (PlaceSubStr (S, #9, ReplaceTabSpaces));
        End
        Else
          GB := False;

        If S = '' Then
          Continue;

        Case S [1] Of

          '[': Begin
                 Section := UpString (Trim (ExtractWord (1, S, ['[', ']'])));
                 SectionChanged := True;
               End;

          '#': Begin
                 S1 := UpString (ExtractWord (1, S, SpaceOnly));

                 If S1 = '#INCLUDE' Then
                 Begin
                   New (PH);
                   PH^. LineNo := 0;
                   PH^. FName := ExtractWord (2, S, SpaceOnly);
                   Assign (PH^. F, PH^. FName);
                   SetTextBuf (PH^. F, PH^. FileBuf, FileBufSize);
                   Reset (PH^. F);
                   If IOResult = 0 Then
                   Begin
                     Handle^. Insert (PH);
                     H := PH;
                   End Else
                   Begin
                     Dispose (PH);
                     ErrWrite (P, 'Unable to find the file specified by #INCLUDE directive');
                   End;
                 End
                 Else
                   If S1 = '#DEFINE' Then
                   Begin
                     If WordCount (S, SpaceOnly) >= 3 Then
                     Begin
                       New (PD);
                       PD^. KeyWord := NewStr (ExtractWord (2, S, SpaceOnly));
                       PD^. Replace := NewStr (Copy (S, WordPosition (3, S,
                         SpaceOnly), 255));
                       Define^. Insert (PD);
                     End
                     Else
                       ErrWrite (P, 'Too few arguments for the #DEFINE directive');
                   End
                   Else
                     ErrWrite (P, 'Invalid CTL directive');
               End;

          '{': Begin
                 i := Pos ('}', S);
                 If i >= 3 Then
                 Begin
                   S1 := DelChars (SpaceOnly, Copy (S, 2, i - 2));
                   If InRange (BBSline, S1) Then
                   Begin
                     S := TrimLead (Copy (S, i + 1, 255));
                     ParserRead := UpString (ExtractAscii (1, S, SpaceOnly,
                       '"'));
                     Exit;
                   End;
                 End;
               End;
        Else
          ParserRead := UpString (ExtractAscii (1, S, SpaceOnly, '"'));
          Exit;
        End;
      End;

      Close (H^. F);
      Dispose (H);
      Handle^. AtDelete (Handle^. Count-1);
    End;
  End;

  ParserRead := '';
End;

Procedure ParserGetParam (Var P: tConfigParser; ParType: tParType; Valid: String; Var R);
Var
  i   : Integer;
  L   : LongInt;
  W   : System. Word;
  Err : SysInt;
  B   : Byte;
  S1  : String;

  Procedure ReplaceEnv;
  Var
    j      : Integer;
    S2, S3 : String;

  Begin
    S3 := S1;
    j := 2;

    Repeat
      S2 := ExtractAscii (j, S3, ['%'], '"');
      If ConsistsOf (S2, ['A'..'Z','a'..'z']) Then
        PlaceSubStrP (S1, '%'+S2+'%', GetEnv (S2));
      Inc (j, 2);
    Until j > i;
  End;

Begin
  i := AsciiPosCh (P. S, ' ', '"');
  If i > 0 Then
  Begin
    S1 := Trim (Copy (P. S, i + 1, 255));

    i := AsciiCount (S1, ['%'], '"');
    If i > 1 Then
      ReplaceEnv;

    For i := 0 To P. Define^. Count-1 Do
      With PDefine (P. Define^. At (i))^ Do
        PlaceSubStrNoCaseP (S1, KeyWord^, Replace^);
  End
  Else
    S1 := '';

  Case ParType Of

    tptString :
      String (R) := S1;

    tptQuote :
      If ((S1 [1] = '"') Or (S1 [1] = '''')) And (S1 [1] = S1 [Length (S1)])
      Then
        String (R) := Copy (S1, 2, Length (S1) - 2)
      Else
        ErrWrite (P, 'Value of the parameter must be in quotes');

    tptFilePath :
      If ConsistsOf (S1, ['0'..'9', ':', '-', '(', ')', '.', '@'..#255, '#',
         '!', '%', '$', '&', ''''])
      Then
        String (R) := S1
      Else
        ErrWrite (P, 'Value of the parameter isn''t a path or a filename');

    tptUpString :
      String (R) := UpString (S1);

    tptBoolean :
      Begin
        S1 := UpString (S1);

        If S1 = 'YES' Then
          Boolean (R) := True
        Else
          If S1 = 'NO' Then
            Boolean (R) := False
          Else
            ErrWrite (P, 'Value of the parameter must be Yes or No');
      End;

    tptLongInt :
      Begin
        Val (S1, L, Err);
        If Err = 0 Then
        Begin
          if Not ((Valid <> '') And Not InRange (L, Valid)) Then
            LongInt (R) := L
          Else
            ErrWrite (P, 'Value of the parameter is out of range');
        End
        Else
          ErrWrite (P, 'Value of the parameter must be a number between -2147483648 and 2147483647');
      End;

    tptWord :
      Begin
        Val (S1, W, Err);
        If Err = 0 Then
        Begin
          If Not ((Valid <> '') And Not InRange (W, Valid)) Then
            System. Word (R) := W
          Else
            ErrWrite (P, 'Value of the parameter is out of range');
        End
        Else
          ErrWrite (P, 'Value of the parameter must be a number between 0 and 65535');
      End;

    tptByte :
      Begin
        Val (S1, B, Err);
        If Err = 0 Then
        Begin
          If Not ((Valid <> '') And Not InRange (B, Valid)) Then
            Byte (R) := B
          Else
            ErrWrite (P, 'Value of the parameter is out of range');
        End
        Else
          ErrWrite (P, 'Value of the parameter must be a number between 0 and 255');
      End;

    tptFixedList :
      Begin
        S1 := UpString (S1);
        Valid := UpString (Valid);

        For B := 1 To WordCount (Valid, SpaceOnly) Do
          If ExtractWord (B, Valid, SpaceOnly) = S1 Then
          Begin
            Byte (R) := B - 1;
            Exit;
          End;

        ErrWrite (P, 'Value of the parameter is out of a fixed range of valid variants');
      End;

    tptTime :
      If ConsistsOf (S1, ['0'..'9', ':', '-', '.', ',']) Then
        String (R) := S1
      Else
        ErrWrite (P, 'Invalid time specified');

    tptExtBoolean :
      Begin
        S1 := UpString (S1);

        If S1 = 'YES' Then
          AskType (R) := atYes
        Else
          If S1 = 'NO' Then
            AskType (R) := atNo
          Else
            If S1 = 'ASK' Then
              AskType (R) := atAsk
            Else
              ErrWrite (P, 'Value of the parameter must be Yes, No or Ask');
      End;

    tptColor :
      Begin
        Byte (R) := Color2Byte (S1);
        If (Byte (R) = 7) And (UpString (S1) <> 'LIGHTGRAY/BLACK') Then
          ErrWrite (P, 'Invalid name of color');
      End;
  End;
End;

Procedure ParserGetBack (Var P: tConfigParser);
Begin
  P. GB := True;
End;

Function ParserEnd (Var P: tConfigParser): Boolean;
Begin
  ParserEnd := P. Handle^. Count = 0;
End;

Function ParserEndSection (Var P: tConfigParser): Boolean;
Begin
  ParserEndSection := P. SectionChanged Or (P. Handle^. Count = 0);
End;

Procedure ParserUnknown (Var P: tConfigParser);
Begin
  ErrWrite (P, 'Unknown parameter ' + ExtractWord (1, P. S, SpaceOnly));
End;

Procedure ParserClose (Var P: tConfigParser);
Var
  i : Integer;
  H : PHandle;
  D : PDefine;

Begin
  With P Do
  Begin
    For i := 0 To Handle^. Count-1 Do
    Begin
      H := Handle^. At (i);
      Close (H^. F);
      Dispose (H);
    End;

    Handle^. DeleteAll;
    Dispose (Handle, Done);

    For i := 0 To Define^. Count-1 Do
    Begin
      D := Define^. At (i);
      DisposeStr (D^. Keyword);
      DisposeStr (D^. Replace);
      Dispose (D);
    End;

    Define^. DeleteAll;
    Dispose (Define, Done);
  End;
End;

End.
