{$F+}
Unit ILine;

{*********************************************************}
{*                     ILINE.PAS                         *}
{*                                                       *}
{*  Copyright (c) unknown author,                        *}
{*  Copyright (c) Vlad Bakaev, 1995-98,                  *}
{*  Copyright (c) Konstantin Klyagin, 1995-98,           *}
{*                exspecially for Tornado BBS System     *}
{*                                                       *}
{*********************************************************}

(*
Usage of the Input Routine:

Function Input (X, Y: Byte; StartStr, BackG, PassChar: String; MaxLen,
                StartPos: Integer; AcceptSet: CharSet; Ins: Boolean; Var
                InputStatus, Attr: Byte): String;

X,Y         Where on screen to put the input.
StartStr    Default input string.
BackG       Background Character, eg ' ' or '°' etc.
PassChar    If defined this character will be displyed instead of the input
            stream.
MaxLen      MaxLen of Input.
StartPos    Where in input string to place the cursor, -1 = End of StartStr
AcceptSet   Which characters should be accepted as input, often [#32..#255]
            NOTE: if you include #8 in this mask, you cannot use delete.
Ins         Begin in INSERT or OVERWRITE mode (Boolean)
InputStatus Upon exit from the input routine this variable will hold:
            13 = Input terminated with Enter.
            27 = Input terminated with ESC.
            72 = User pressed UpArrow
            80 = User pressed DownArrow
            73 = User pressed Page Up
            81 = User pressed Page Down
            etc...

 Next Version: Window (ie; edit 255 chars in a 16 char window)
               ExitChar Mask

*)

Interface

Uses
  tWin,
  OpCrt,
  tMisc;

Type
  CharSet = Set Of #0..#255;

Function Input (X, Y: Byte; StartStr, BackG, PassChar: String; MaxLen, StartPos:
               Integer; AcceptSet: CharSet; Ins: Boolean; Var InputStatus: Byte; Attr: Byte;
               var Changed: Boolean): String;

Implementation

Function Input (X, Y: Byte; StartStr, BackG, PassChar: String; MaxLen, StartPos:
               Integer; AcceptSet: CharSet; Ins: Boolean; Var InputStatus: Byte; Attr: Byte;
               var Changed: Boolean): String;
Var
  P             : Byte;
  Quit, Initial : Boolean;
  CH, ext       : Char;
  s,s1          : String;
  t             : String [1];

Begin
  Quit := False;                                      { Don't quit on me yet! }
  Initial := True;
  If Length (PassChar) > 1 Then PassChar := PassChar [1]; { Just in Case... ;-) }
  If Length (BackG) > 1 Then BackG := BackG [1];
  If Length (BackG) = 0 Then BackG := ' ';
  If Length (StartStr) > MaxLen Then StartStr := Copy (StartStr, 1, MaxLen);
  If StartPos > Length (StartStr) Then StartPos := Length (StartStr);
  If StartPos = -1 Then StartPos := Length (StartStr);
  If StartPos >= MaxLen Then StartPos := MaxLen - 1;

  s := StartStr;                                { Put StartStr into Edit Buffer }
  s1 := StartStr;
  FastWrite (Replicate (BackG [1], MaxLen), Y, X, Attr);

  If StartStr <> '' Then Begin
    If passchar = '' Then FastWrite (StartStr, Y, X, Attr) Else
      FastWrite (Replicate (PassChar [1], Length (StartStr) ), Y, X, Attr );
  End;

  p := StartPos;
  GotoXY (X + StartPos, Y);

  Repeat
    If Ins Then NormalCursor
           Else BlockCursor;

    ext := #0;

    If Not WaitForKey (CH) Then
    Begin
      InputStatus := 27;
      Quit := True;
    End;

    If CH = #0 Then
    Begin
      If Not WaitForKey (Ext) Then
      Begin
        InputStatus := 27;
        Quit := True;
      End;
      If ext <> #83 Then Initial := False;
    End;

    If CH = #27 Then
    Begin
      InputStatus := 27;
      Quit := True;
    End;

    If CH In AcceptSet Then
    Begin   { Welcome to the jungle...}

      If Initial and (StartPos = 0) Then
      Begin
        Initial := False;
        FastWrite (Replicate (BackG [1], Length (S) ), Y, X, Attr );
        P := 0; S := ''; GotoXY (X, Y);
      End;

      t := CH;
      If (p = Length (s) ) And (Length (s) < MaxLen) Then
      Begin
        s := s + t;
        If PassChar = '' Then FastWrite (T, Y, X + P, Attr) Else FastWrite (PassChar, Y, X + P, Attr);
        Inc (p);
      End Else
        If Length (s) < MaxLen Then Begin
          If Ins Then Insert (T, S, P + 1) Else s [p + 1] := CH;
          If PassChar = '' Then FastWrite (Copy (S, P + 1, Length (S) ), Y, X + P, Attr ) Else
            FastWrite (PassChar, Y, X + Length (S) - 1, Attr); Inc (p);
        End Else If (Length (s) = MaxLen) And (Not Ins) Then
        Begin
          s [p + 1] := CH;
          If PassChar = '' Then FastWrite (T, Y, X + P, Attr) Else FastWrite (PassChar, Y, X + P, Attr);
          Inc (p);
        End;
      CH := #0;
      If p > MaxLen - 1 Then p := MaxLen - 1;
      GotoXY (X + P, Y);
    End Else Begin

      Case CH Of { CTRL-Y }
        #25:
               Begin
                 FastWrite (Replicate (BackG [1], Length (S) ), Y, X, Attr );
                 P := 0;
                 S := '';
                 GotoXY (X, Y);
               End;

        {Backspace}
        #8: If (P > 0) Then
        Begin
          If (p + 1 = MaxLen) And (p < Length (s) ) Then Ext := #83 Else
          Begin
            Delete (S, P, 1);
            Dec (P);
            GotoXY (X + P, Y);
            If PassChar = '' Then FastWrite (Copy (S, P + 1, Length (s) ) + BackG, Y, X + P, Attr) Else
              If P > 0 Then FastWrite (PassChar + BackG, Y, X + Length (s) - 1, Attr) Else
                FastWrite (BackG, Y, X + Length (s), Attr);
          End;
        End;

        #9:
            Begin { Exit on TAB }
              InputStatus := 9;
              Quit := True;
            End;

        #13:
             Begin
               InputStatus := 13;
               Quit := True;
             End;
      End; { Case CH of }

      Case ext Of
        #75: If P > 0 Then Begin
          {Left Arrow}      Dec (P);
          GotoXY (X + P, Y);
        End;

        #77: If (P < Length (s) ) And (P + 1 < MaxLen) Then Begin
          {Right Arrow}             Inc (P);
          GotoXY (X + P, Y);
        End;

        #82: Ins := Not (Ins); {Insert}

        #83: If Initial Then {Delete}
             Begin
               Initial := False;
               FastWrite (Replicate (BackG [1], Length (S) ), Y, X, Attr );
               P := 0; S := ''; GotoXY (X, Y);
             End Else
             If P < Length (s) Then
             Begin
               Delete (S, P + 1, 1);
               If PassChar = '' Then FastWrite (Copy (S, P + 1, Length (s) ) + BackG, Y, X + P, Attr) Else
               If p > 0
               Then FastWrite (PassChar + BackG, Y, X + Length (S) - 1, Attr)
               Else FastWrite (BackG, Y, X + Length (S), Attr);
             End;

        #71: Begin
               p := 0;
               GotoXY (X + P, Y);
             End;

        #79: Begin
               p := Length (s);
               If p >= MaxLen Then P := MaxLen - 1;
               GotoXY (X + P, Y);
             End;

        #72, #73, #80, #81, #59..#68, #117, #119, #15:
                                     Begin
                                       InputStatus := Ord (Ext);
                                       Quit := True;
                                     End;

      End; {Case of EXT }

    End; { if not normal char }

  Until Quit;
  if S<>S1 then Changed := True;
  Input := S;
End;

End.
