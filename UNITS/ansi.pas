{$IFDEF MSDOS}
  {$IFNDEF DPMI32}
    {$F+}
  {$ENDIF}
{$ENDIF}

{*********************************************************}
{*                                                       *}
{*                       ANSI.PAS                        *}
{*                                                       *}
{*     Copyright (c) Konstantin Klyagin, 1995-98.        *}
{*                   expecialy for Tornado BBS system    *}
{*                                                       *}
{*********************************************************}

Unit Ansi; {-Does ANSI screen writing and produces ANSI sequences}

Interface

Uses
  OpCrt,
  OpInline,
  tMisc,
  tGlob;

Const
  MaskBlinkOnInverse = True;

Procedure WriteStringAnsi (Const S: String);

Function AnsiColor (Attr: Byte): String;
Function AnsiColorRelative (OldAttr, NewAttr: Byte): String;
Function AnsiGotoXY (X, Y: Byte): String;
Function AnsiUp (Lines: Byte): String;
Function AnsiDown (Lines: Byte): String;
Function AnsiRight (Cols: Byte): String;
Function AnsiLeft (Cols: Byte): String;

Procedure AnsiUpdateAttr (C: Char; Var Attr: Byte);

Implementation

Uses
  MainComm;

Type
  ParserType = (GotNone, GotEscape, GotBracket, GotSemiColon, GotParm);

Const
  Escape       = #27;
  LeftBracket  = #91;
  Semicolon    = #59;
  LineFeed     = #10;
  FormFeed     = #12;
  BellChar     = #07;
  EqualSign    = #61;
  QuestionMark = #63;

  {For sizing parser}
  MaxQueueChars = 16;
  MaxParms      = 5;

  {For sizing the screen}
  AnsiWidth  = 80;
  AnsiHeight = 25;

  IntenseBit = $08;
  BlinkBit   = $80;

  {For saving TextAttr states}
  Inverse     : Boolean = False;
  IntenseMask : Byte = 0;

  {For color filter}
  fInverse     : Boolean = False;
  fIntenseMask : Byte = 0;

  {For saving and restoring the cursor state}
  SaveX : Byte = 1;
  SaveY : Byte = 1;

Var
  ParmInt       : Array [1..MaxParms] Of Integer;
  Parms, fParms : Array [1..MaxParms] Of String [5];
  SaveCharQueue : String [MaxQueueChars];
  ParmIndex,
  fParmIndex    : Byte;
  ParserState,
  fParserState  : ParserType;

Procedure InitParser;
Var
  i : Integer;

Begin
  ParmIndex := 1;
  ParserState := GotNone;

  For i := 1 To MaxParms Do
    Parms [i] := '';
End;

Procedure WriteSavedQueue;
Var
  i : Integer;
  c : Char;

Begin
  For i := 1 To Length (SaveCharQueue) Do
  Begin
    C := SaveCharQueue [i];
    Case C Of
      FormFeed : ClrScr;
      BellChar : TorSoundBell;
    Else
      Write (C);
    End;
  End;
End;

Procedure ClearEndOfLine; {-Clear to the end of the line}
Begin
  If Inverse Then
  Begin
    {Un-invert TextAttr before clearing}
    TextAttr := (TextAttr ShL 4) Or (TextAttr ShR 4);
    ClrEol;
    {Restore inverted TextAttr}
    TextAttr := (TextAttr ShL 4) Or (TextAttr ShR 4);
  End
  Else
    ClrEol;
End;

Procedure ClearPart (X1, Y1, X2, Y2: Integer); {-Clear from X1, Y1 to X2, Y2}
Var
  i            : Integer;
  SaveX, SaveY : Word;

Begin
  {Save cursor position}
  SaveX := WhereX;
  SaveY := WhereY;
  GotoXY (X1, Y1);

  {Un-invert TextAttr before clearing}
  If Inverse Then
    TextAttr := (TextAttr ShL 4) Or (TextAttr ShR 4);

  If Y1 = Y2 Then
  Begin
    If X2 = AnsiWidth Then ClrEol
                      Else Write (Replicate (' ', X2 - X1 + 1));
  End Else
  Begin
    ClrEol;

    For i := Y1 + 1 To Y2 - 1 Do
    Begin
      GotoXY (1, i);
      ClrEol;
    End;

    GotoXY (1, Y2);
    If X2 = AnsiWidth Then ClrEol
                      Else Write (Replicate (' ', X2 - X1 + 1));
  End;

  GotoXY (SaveX, SaveY);

  {Restore inverted TextAttr}
  If Inverse Then
    TextAttr := (TextAttr ShL 4) Or (TextAttr ShR 4);
End;

Procedure GotoXYCheck (X, Y: Integer);
 {-GotoXY that checks against negative numbers}
Begin
  If X < 1 Then X := 1;
  If Y < 1 Then Y := 1;
  GotoXY (X, Y);
End;

Procedure ProcessCommand (C: Char);
Var
  Code : SysInt;
  i    : Integer;

Begin
{$IFDEF USE32}
  FillLong (ParmInt [ParmIndex + 1], MaxParms - ParmIndex, 1);
{$ELSE}
  FillWord (ParmInt [ParmIndex + 1], MaxParms - ParmIndex, 1);
{$ENDIF}

  For i := 1 To ParmIndex Do
  Begin
    Val (Parms [i], ParmInt [i], Code);
    Parms [i] := '';

    If Code <> 0 Then
      Case C Of
        'm', 'J', 'K' : ParmInt [i] := 0;
      Else
        ParmInt [i] := 1;
      End;
  End;

  Case C Of
    'm',
    'l' : Begin {SGR - set graphics rendition (set background color)}
            For i := 1 To ParmIndex Do
            Begin
              If Inverse Then    {Restore inverted TextAttr before continuing}
                TextAttr := (TextAttr Shl 4) Or (TextAttr Shr 4);

              Case ParmInt [i] Of
                 0 : Begin                                    {White on black}
                       TextAttr := $07;
                       IntenseMask := 0;
                       Inverse := False;
                       Continue;
                     End;
              1, 4 : IntenseMask := IntenseMask Or IntenseBit;{Set intense bit}
                40 : TextAttr := TextAttr And $0F;          {Black background}
                30 : TextAttr := TextAttr And $F0;          {Black foreground}
                31 : TextAttr := (TextAttr And $F0) Or $04; {Red foreground}
                32 : TextAttr := (TextAttr And $F0) Or $02; {Green foreground}
                33 : TextAttr := (TextAttr And $F0) Or $06; {Yellow forground}
                34 : TextAttr := (TextAttr And $F0) Or $01; {Blue foreground}
                35 : TextAttr := (TextAttr And $F0) Or $05; {Magenta foreground}
                36 : TextAttr := (TextAttr And $F0) Or $03; {Cyan foreground}
                37 : TextAttr := (TextAttr And $F0) Or $07; {White foreground}
                41 : TextAttr := (TextAttr And $0F) Or $40; {Red background}
                42 : TextAttr := (TextAttr And $0F) Or $20; {Green background}
                43 : TextAttr := (TextAttr And $0F) Or $60; {Yellow background}
                44 : TextAttr := (TextAttr And $0F) Or $10; {Blue background}
                45 : TextAttr := (TextAttr And $0F) Or $50; {Magenta background}
                46 : TextAttr := (TextAttr And $0F) Or $30; {Cyan background}
                47 : TextAttr := (TextAttr And $0F) Or $70; {White background}
              5, 6 : IntenseMask := IntenseMask Or BlinkBit;{Set blinking on}
                 8 : TextAttr := $00;                {Invisible}
                 7 : Inverse  := True;               {Invert TextAttr later}
                27 : Inverse  := False;              {Stop inverting TextAttr}
              End;

              If Inverse Then {Fix up TextAttr for intense and blink}
              Begin
                TextAttr := (TextAttr Shl 4) Or (TextAttr Shr 4);
                If MaskBlinkOnInverse Then
                  TextAttr := TextAttr And $7F;
              End;

              TextAttr := TextAttr Or IntenseMask;
            End;
          End;

    'f',  {HVP - horizontal and vertical position}
    'H' : {CUP - cursor position}
          GotoXYCheck (ParmInt [2], ParmInt [1]);

    'C' : Begin {CUF - cursor forward}
            If (ParmInt [i] = 0) Then
              ParmInt [i] := 1;
            GotoXYCheck (WhereX + ParmInt [1], WhereY);
          End;

    'D' : Begin {CUB - cursor back}
            If (ParmInt [i] = 0) Then
              ParmInt [i] := 1;
            GotoXYCheck (WhereX - ParmInt [1], WhereY);
          End;

    'A' : Begin {CUU - cursor up}
            If (ParmInt [i] = 0) Then
              ParmInt [i] := 1;
            GotoXYCheck (WhereX, WhereY - ParmInt [1]);
          End;

    'B' : Begin {CUD - cursor down}
            If (ParmInt [i] = 0) Then
              ParmInt [i] := 1;
            GotoXYCheck (WhereX, WhereY + ParmInt [1]);
          End;

    'J' : {ED - erase display}
          Case ParmInt [1] Of
            2 : ClrScr;
            0 : ClearPart (WhereX, WhereY, AnsiWidth, AnsiHeight);
            1 : ClearPart (1, 1, WhereX, WhereY);
          End;

    'K' : {EL - erase in line}
          Case ParmInt [1] Of
            0 : ClearEndOfLine;
            1 : ClearPart (1, WhereY, WhereX, WhereY);
            2 : ClearPart (1, WhereY, AnsiWidth, WhereY);
          End;

    's' : {SCP - save cursor position}
          Begin
            SaveX := WhereX;
            SaveY := WhereY;
          End;

    'u' : {RCP - restore cursor position}
          GotoXY (SaveX, SaveY);
  Else
    WriteSavedQueue;
  End;

  ParmIndex := 1;
  ParserState := GotNone;
End;

Procedure WriteStringAnsi (Const S: String);
Var
  i, j, Len : Integer;
  C         : Char;

Label
  CheckBracket,
  ProcessSequence;

Begin
  Len := Length (S);
  i := 1;

  If ParserState <> GotNone Then
    If ParserState <> GotEscape Then Goto ProcessSequence
                                Else Goto CheckBracket;

  While i <= Len Do
  Begin
    C := S [i];
    Inc (i);

    Case C Of
        Escape : Begin
                   ParserState := GotEscape;
                   SaveCharQueue := C;

                 CheckBracket:
                   If i <= Len Then
                   Begin
                     C := S [i];
                     Inc (i);
                     SaveCharQueue := SaveCharQueue + C;

                     If C = LeftBracket Then
                       ParserState := GotBracket
                     Else
                     Begin
                       WriteSavedQueue;
                       InitParser;
                       Continue;
                     End;
                   End
                   Else
                     Break;

                 ProcessSequence:
                   While i <= Len Do
                   Begin
                     C := S [i];
                     Inc (i);
                     SaveCharQueue := SaveCharQueue + C;

                     If (C <= '9') And (C >= '0') Then
                     Begin
                       Parms [ParmIndex] := Parms [ParmIndex] + C;
                       ParserState := GotParm;
                     End
                     Else
                       If C = Semicolon Then
                       Begin                     {prepare for next parameter}
                         If ParserState <> GotSemicolon Then
                         Begin
                           ParserState := GotSemicolon;
                           Inc (ParmIndex);
                           If ParmIndex > MaxParms Then
                           Begin
                             WriteSavedQueue;
                             InitParser;
                             Break;
                           End;
                         End;
                       End
                       Else
                         If (C <> EqualSign) And (C <> QuestionMark) Then
                         Begin
                           ProcessCommand (C);
                           Break;
                         End;
                   End;
                 End;
      LineFeed : Begin {Clear background before scrolling window}
                   j := TextAttr;
                   TextAttr := TextAttr And $0F;
                   Write (C);
                   TextAttr := j;
                 End;
      BellChar : TorSoundBell;
      FormFeed : ClrScr;
    Else
      j := i - 1;

      While (i <= Len) And Not (S [i] in [Escape, LineFeed, BellChar, FormFeed])
      Do
        Inc (i);

      Write (Copy (S, j, i - j));
    End;
  End;
End;

Procedure fInitParser;
Var
  i : Integer;

Begin
  fParmIndex := 1;
  fParserState := GotNone;

  For i := 1 To MaxParms Do
    fParms [i] := '';
End;

Procedure AnsiUpdateAttr (C: Char; Var Attr: Byte);
Var
  i, j : Integer;
  Code : SysInt;

Begin
  Case fParserState Of
    GotNone : If C = Escape Then
                fParserState := GotEscape;
  GotEscape : If C = LeftBracket Then fParserState := GotBracket
                                 Else fParserState := GotNone;
  Else
    If (C <= '9') And (C >= '0') Then
    Begin
      fParms [fParmIndex] := fParms [fParmIndex] + C;
      fParserState := GotParm;
    End
    Else
      If C = Semicolon Then
      Begin
        If fParserState <> GotSemicolon Then
        Begin
          fParserState := GotSemicolon;
          Inc (fParmIndex);

          If fParmIndex > MaxParms Then
            fInitParser;
        End;
      End
      Else
        If (C <> EqualSign) And (C <> QuestionMark) Then
        Begin
          If (C = 'm') Or (C = 'l') Then
          Begin
            For i := 1 To fParmIndex Do
            Begin
              Val (fParms [i], j, Code);
              If Code <> 0 Then
                j := 0;
              fParms [i] := '';

              If fInverse Then
                Attr := (Attr Shl 4) Or (Attr Shr 4);

              Case j Of
                 0 : Begin
                       Attr := $07;
                       fIntenseMask := 0;
                       fInverse := False;
                       Continue;
                     End;
              1, 4 : fIntenseMask := fIntenseMask Or IntenseBit;
                40 : Attr := Attr And $0F;
                30 : Attr := Attr And $F0;
                31 : Attr := (Attr And $F0) Or $04;
                32 : Attr := (Attr And $F0) Or $02;
                33 : Attr := (Attr And $F0) Or $06;
                34 : Attr := (Attr And $F0) Or $01;
                35 : Attr := (Attr And $F0) Or $05;
                36 : Attr := (Attr And $F0) Or $03;
                37 : Attr := (Attr And $F0) Or $07;
                41 : Attr := (Attr And $0F) Or $40;
                42 : Attr := (Attr And $0F) Or $20;
                43 : Attr := (Attr And $0F) Or $60;
                44 : Attr := (Attr And $0F) Or $10;
                45 : Attr := (Attr And $0F) Or $50;
                46 : Attr := (Attr And $0F) Or $30;
                47 : Attr := (Attr And $0F) Or $70;
              5, 6 : fIntenseMask := fIntenseMask Or BlinkBit;
                 8 : Attr := $00;
                 7 : fInverse  := True;
                27 : fInverse  := False;
              End;

              If fInverse Then
              Begin
                Attr := (Attr Shl 4) Or (Attr Shr 4);
                If MaskBlinkOnInverse Then
                  Attr := Attr And $7F;
              End;

              Attr := Attr Or fIntenseMask;
            End;
          End
          Else
            For i := 1 To fParmIndex Do
              fParms [i] := '';

          fParmIndex := 1;
          fParserState := GotNone;
        End;
  End;
End;

Const
  ColorArray : Array [0..7] Of Char = ('0', '4', '2', '6', '1', '5', '3', '7');

Function AnsiColor (Attr: Byte): String;
Const
  C1 : Array [Boolean] Of Char = ('0', '5');
  C2 : Array [Boolean] Of Char = ('2', '1');

Begin
  AnsiColor := #27'[' + C1 [Attr > $7F] + ';' + C2 [(Attr And $0F) > 7] + ';4'
    + ColorArray [(Attr Shr 4) And 7] + ';3' + ColorArray [Attr And 7] + 'm';
End;

Function AnsiColorRelative (OldAttr, NewAttr: Byte): String;
Const
  DecisionTree : Array [Boolean, Boolean, Boolean, Boolean, Boolean] of Byte =
    (((((0,1), (2,0)),
       ((1,1), (3,3))),
      (((4,5), (6,4)),
       ((0,5), (2,0)))),
     ((((7,1), (8,7)),
       ((1,1), (3,3))),
      (((9,5), (10,9)),
       ((7,5), (8,7)))));
Var
  BgC : Char;
  S   : String [8];

Begin
  If OldAttr <> NewAttr Then
  Begin
    BgC := ColorArray [(NewAttr Shr 4) And 7];

    If ((NewAttr Xor OldAttr) And $8F) <> 0 Then
    Begin
      Case DecisionTree [((NewAttr Xor OldAttr) And $70) <> 0,
                         (NewAttr And $80) = $80,
                         (OldAttr And $80) = $80,
                         (NewAttr And 8) = 8,
                         (OldAttr And 8) = 8] Of
         0 : S := '';
         1 : S := '0;4' + BgC + ';';
         2 : S := '1;';
         3 : S := '0;1;4' + BgC + ';';
         4 : S := '5;';
         5 : S := '0;5;4' + BgC + ';';
         6 : S := '1;5;';
         7 : S := '4' + BgC + ';';
         8 : S := '1;4' + BgC + ';';
         9 : S := '5;4' + BgC + ';';
        10 : S := '1;5;4' + BgC + ';';
      End;

      AnsiColorRelative := #27'[' + S + '3' + ColorArray [NewAttr And 7] + 'm';
    End
    Else
      AnsiColorRelative := #27'[4' + BgC + 'm';
  End
  Else
    AnsiColorRelative := '';
End;

Function AnsiGotoXY (X, Y: Byte): String;
Var
  XStr, YStr : String [3];

Begin
  Str (X, XStr);
  Str (Y, YStr);
  ANSIGotoXY := #27'[' + YStr + ';' + XStr + 'H';
End;

Function AnsiUp (Lines: Byte): String;
Var
  LinesStr : String [3];

Begin
  Str (Lines, LinesStr);
  ANSIUp := #27'[' + LinesStr + 'A';
End;

Function AnsiDown (Lines: Byte): String;
Var
  LinesStr : String [3];

Begin
  Str (Lines, LinesStr);
  ANSIDown := #27'[' + LinesStr + 'B';
End;

Function AnsiRight (Cols: Byte): String;
Var
  ColsStr : String [3];

Begin
  Str (Cols, ColsStr);
  ANSIRight := #27'[' + ColsStr + 'C';
End;

Function AnsiLeft (Cols: Byte): String;
Var
  ColsStr : String [3];

Begin
  Str (Cols, ColsStr);
  ANSILeft := #27'[' + ColsStr + 'D';
End;

Begin
  InitParser;
  fInitParser;
End.
