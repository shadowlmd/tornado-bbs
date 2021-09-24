{$IFDEF MSDOS}
  {$IFNDEF DPMI32}
    {$F+}
  {$ENDIF}
{$ENDIF}

Unit Avatar;

Interface

Procedure WriteStringAvt (Const S: String);

Function AvtGotoXY (X, Y: Byte): String;
Function AvtUp (Lines: Byte): String;
Function AvtDown (Lines: Byte): String;
Function AvtLeft (Cols: Byte): String;
Function AvtRight (Cols: Byte): String;
Function AvtColor (Attr: Byte): String;
Function AvtColorRelative (OldAttr, NewAttr: Byte): String;

Procedure AvtUpdateAttr (C: Char; Var Attr: Byte);

Implementation

Uses
  OpCrt,
  tMisc,
  tGlob,
  MainComm;

Const
  DefAttr = 3;
  BC      : Byte = 0;
  fBC     : Byte = 0;

Var
  Buf, fBuf : Array [1..3] Of Char;

Procedure WriteStringAvt (Const S: String);
Var
  i, j : Integer;
  Ch   : Char;

Begin
  For i := 1 To Length (S) Do
  Begin
    Ch := S [i];

    Case BC Of
      0 : Case Ch Of
          #1, #22: Begin
                     BC := 1;
                     Buf [1] := Ch;
                   End;

              #25: Begin
                     BC := 2;
                     Buf [2] := Ch;
                   End;

               #7: TorSoundBell;

              #12: Begin
                     TextAttr := DefAttr;
                     ClrScr;
                   End;
          Else
            Write (Ch);
          End;

      1 : Begin
            Case Ch Of
              #1,
              #8,
              #25: Begin
                     BC := 2;
                     Buf [2] := Ch;
                     Continue;
                   End;

              #2 : Begin
                     TextAttr := TextAttr Or $80;
                   End;

              #3 : If WhereY > 0 Then
                     GoToXY (WhereX, WhereY - 1);

              #4 : If WhereY < 24 Then
                     GoToXY (WhereX, WhereY + 1);

              #5 : If WhereX > 0 Then
                     GoToXY (WhereX - 1, WhereY);

              #6 : If WhereX < 79 Then
                     GoToXY (WhereX + 1, WhereY);

              #7 : ClrEoL;

            Else
              Case Buf [1] Of
                #1 : If StopCodeEnable Then WaitReturn
                                       Else Write (#1);
                #7 : TorSoundBell;
              Else
                Write (Buf [1]);
              End;

              Write (Ch);
            End;

            BC := 0;
          End;

      2 : If Buf [2] = #1 Then
          Begin
            TextAttr := Byte (Ch);
            BC := 0;
          End Else
          Begin
            BC := 3;
            Buf [3] := Ch;
          End;

      3 : Begin
            If Buf [2] = #25 Then
            Begin
              Case Buf [3] Of
                #1 : If StopCodeEnable Then
                       For j := 1 To Ord (Ch) Do
                         WaitReturn
                     Else
                       Write (Replicate (#1, Ord (Ch)));
                #7 : For j := 1 To Ord (Ch) Do
                       TorSoundBell;
              Else
                Write (Replicate (Buf [3], Ord (Ch)));
              End;
            End
            Else
              GoToXY (Byte (Ch), Byte (Buf [3]));

            BC := 0;
          End;
    End;
  End;
End;

Procedure AvtUpdateAttr (C: Char; Var Attr: Byte);
Begin
  Case fBC Of
    0 : Case C Of
        #1, #22: Begin
                   fBC := 1;
                   fBuf [1] := C;
                 End;

            #25: Begin
                   fBC := 2;
                   fBuf [2] := C;
                 End;

            #12: Attr := DefAttr;
        End;

    1 : Case C Of
          #1,
          #8,
          #25: Begin
                 fBC := 2;
                 fBuf [2] := C;
               End;

          #2 : Begin
                 Attr := Attr Or $80;
                 fBC := 0;
               End;
        Else
          fBC := 0;
        End;

    2 : If fBuf [2] = #1 Then
        Begin
          Attr := Byte (C);
          fBC := 0;
        End
        Else
          fBC := 3;

    3 : fBC := 0;
  End;
End;

Function AvtGotoXY (X, Y: Byte): String;
Begin
  AvtGoToXY := ^V + ^H + Chr (Y) + Chr (X);
End;

Function AvtUp (Lines: Byte): String;
Var
  i : Byte;
  S : String;

Begin
  S := ^V + ^C;
  For i := 2 To Lines Do
    S := S + ^V + ^C;

  AvtUp := S;
End;

Function AvtDown (Lines: Byte): String;
Var
  i : Byte;
  S : String;

Begin
  S := ^V + ^D;
  For i := 2 To Lines Do
    S := S + ^V + ^D;

  AvtDown := S;
End;

Function AvtLeft (Cols: Byte): String;
Var
  i : Byte;
  S : String;

Begin
  S := ^V + ^E;
  For i := 2 To Cols Do
    S := S + ^V + ^E;

  AvtLeft := S;
End;

Function AvtRight (Cols: Byte): String;
Var
  i : Byte;
  S : String;

Begin
  S := ^V + ^F;
  For i := 2 To Cols Do
    S := S + ^V + ^F;

  AvtRight := S;
End;

Function AvtColor (Attr: Byte): String;
Var
  S : String [5];

Begin
  S := ^V + ^A + Chr (Attr And $7F);
  If Attr > $7F Then
    S := S + ^V + ^B;
  AvtColor := S;
End;

Function AvtColorRelative (OldAttr, NewAttr: Byte): String;
Var
  S : String [5];

Begin
  If OldAttr <> NewAttr Then
  Begin
    S := ^V + ^A + Chr (NewAttr And $7F);
    If NewAttr > $7F Then
      S := S + ^V + ^B;
    AvtColorRelative := S;
  End
  Else
    AvtColorRelative := '';
End;

End.
