{$IFDEF MSDOS}
  {$IFNDEF DPMI32}
    {$F+}
  {$IFNDEF DPMI}
    {$O+}
  {$ENDIF}
  {$ENDIF}
{$ENDIF}

Unit tWin;

{*********************************************************}
{*                      TWIN.PAS                         *}
{*                                                       *}
{*  Copyright (c) Alex Radzishevskiy, 1995-96,           *}
{*  Copyright (c) Konstantin Klyagin, 1995-98,           *}
{*                exspecially for Tornado BBS System     *}
{*                                                       *}
{*********************************************************}

Interface

Uses
{$IFDEF OS2}
  Os2Base,
{$ENDIF}
  DOS,
  ApCom,
  TimeTask,
  Objects,
  tMisc,
  TGlob,
  OpCrt;

Type
  PBoxRec = ^TBoxRec;
  TBoxRec = Record
    X1, Y1, X2, Y2,               { координаты окна           }
    FrameType,                    { тип линий (см. ниже)      }
    Color,                        { цвет окна                 }
    DelayTime,                    { задержка зумминга         }
    TitleAttr      : Byte;        { цвет заголовка            }
    Covers         : Pointer;     { ссылка на сохраненный фон }
    Title          : String [40]; { заголовок окошка          }
  End;

  PMarkItem = ^TMarkItem;
  TMarkItem = Record
    Text    : String {$IFDEF MSDOS} [78] {$ELSE} [100] {$ENDIF};
    Marked  : Boolean;
  End;

  GetItemFunc = Function (N: LongInt): String;
  tInfoProc   = Procedure (S: String);

Const
  ScrX      : Byte = 79;
  ScrY      : Byte = 24;
  ZoomSpeed : Byte = 5;

Procedure TWinInit;

{Устанавливает заданные параметры в запись}
Procedure InitWindow (Var Box: PBoxRec; CX1, CY1, CX2, CY2, CFrameType,
  Clr: Byte; Const CTitle: String; TitleAtt: Byte; CDelayTime: Byte;
  Center: Boolean);

{Рисует окно с зуммингом}
Procedure DrawWindow (Var Box: PBoxRec);

Procedure TitleColor (W: PBoxRec; Num, Attr: Byte);

{Unzoom заданной области экрана. Запоминает текущие координаты курсора}
Procedure CloseWindow (Var Box: PBoxRec);

{Рисует трехмерную кнопочку}
Procedure Key (X, Y, TxtClr, Background: Byte; Const Txt: String);

{Рисует нажатую трехмерную кнопочку}
Procedure PressedKey (X, Y, TxtClr, BkgClr: Byte; Const Txt: String);

{Рисует рамочку, в ней пишет текст. Рисует 3х мерную кнопочку OK и ждет}
{нажатия любой клавиши после чего зуммится обратно                     }
Procedure CenterTempBox (Color, KeyColor: Byte; cFrameType: Byte;
  Const cTitle: String; cDelayTime: Byte; Const WindowTitle: String);

{Тоже что и предыдущ., но не с кнопочкой а ждет заданное кол-во времени}
Procedure CenterDelayTempBox (Color, TxtColor, cFrameType: Byte;
  Const cTitle: String; cDelayTime: Byte; Const WindowTitle: String);

Procedure DoneTempBox;

Procedure Rotate (X, Y: Byte; Color: Word);

Procedure ScrollTextWindow (X1, Y1, X2, Y2, TxtClr, TxtBkg, SNormalClr,
  SNormalBkg, SLightClr, SLightBkg: Byte; TextBody: PNotSortedCollection);
 { - Рисует окно с заданными координатами и скроллит в нем текст }

Function tWinMenu (X1, Y1, X2, Y2, CLightColor, CDarkColor: Byte;
  Var StartDisp: LongInt; StartItem: LongInt; Items: PNotSortedCollection;
  Checks: Boolean; Const LightWords: String; InfoProc: tInfoProc): LongInt;

Function SelectBox (Const Choices: String; WAttr, BSAttr, TAttr, Delay: Byte;
  Const Text: String): Byte;

Function NewMarkItem (Const S: tMarkItem): PMarkItem;
Procedure DisposeMarkItem (P: PMarkItem);
Function WaitForKey (Var C: Char): Boolean;

Implementation

Const
  RotState : Byte = 1;

Var
  MyBox : PBoxRec;

Function NewMarkItem (Const S: tMarkItem): PMarkItem;
Var
  P : PMarkItem;

Begin
  New (P);
  P^ := S;
  NewMarkItem := P;
End;

Procedure DisposeMarkItem (P: PMarkItem);
Begin
  If P <> Nil Then
    Dispose (P);
End;

Function WaitForKey (Var C: Char): Boolean;
Begin
  WaitForKey := False;
  C := #0;

  While Not KeyPressed Do
  Begin
    If Not Local Then
    Begin
    {$IFDEF OS2}
      If NeedThreadClose Then
        Exit;
    {$ENDIF}
      If Not CheckDCD (Port) Then
      Begin
        Pause (Cnf. CDLostDelay);
        If Not CheckDCD (Port) Then
          Exit;
      End;
    End;

    TimeSlice;
  End;

  C := ReadKey;
  WaitForKey := True;
End;

Procedure ChangeFrameChars (T: Byte);
Begin
  Case T Of
    1, 5 : OpCrt. FrameChars := '┌└┐┘──││';
       2 : OpCrt. FrameChars := '╔╚╗╝══║║';
       3 : OpCrt. FrameChars := '        ';
       4 : OpCrt. FrameChars := '╒╘╕╛══││';
  End;
End;

Procedure InitWindow (Var Box: PBoxRec; CX1, CY1, CX2, CY2, CFrameType,
  Clr: Byte; Const CTitle: String; TitleAtt: Byte; CDelayTime: Byte;
  Center: Boolean);
Begin
  New (Box);

  With Box^ Do
  Begin
    If Not Center Then
    Begin
      X1 := cX1;
      Y1 := cY1;
      X2 := cX2;
      Y2 := cY2;
    End Else
    Begin
      Dec (cX2, cX1);
      Dec (cY2, cY1);
      X1 := ((ScrX - cX2) Shr 1) + 1;
      X2 := X1 + cX2;
      Y1 := ((ScrY - cY2) Shr 1) + 1;
      Y2 := Y1 + cY2;
    End;

    FrameType := CFrameType;
    Color := Clr;
    Title := CTitle;
    DelayTime := CDelayTime;
    TitleAttr := TitleAtt;
  End;
End;

Procedure Shadow (X1, Y1, X2, Y2: Byte);
Var
  i, Len : Integer;

Begin
  Len := ScrX - X2;
  If Len > 2 Then
    Len := 2;

  If Y2 < ScrY Then
    ChangeAttribute (X2 - (X1 + 1), Y2 + 1, X1 + 2, $08)
  Else
    Y2 := ScrY - 1;

  If Len > 0 Then
    For i := Y1+1 To Y2+1 Do
      ChangeAttribute (Len, i, X2 + 1, $08);
End;

Procedure CalcSaveArea (X2, Y2: Byte; Var NewX2, NewY2: Byte);
Begin
  NewX2 := X2 + 2;
  If NewX2 > ScrX + 1 Then
    NewX2 := ScrX + 1;
  NewY2 := Y2 + 1;
  If NewY2 > ScrY + 1 Then
    NewY2 := ScrY + 1;
End;

Procedure DrawWindow (Var Box: PBoxRec);
Var
  i, j, ie, je : Byte;
  Update       : Boolean;

Begin
  With Box^ Do
  Begin
    CalcSaveArea (X2, Y2, i, j);
    SaveWindow (X1, Y1, i, j, True, Covers);

    ChangeFrameChars (FrameType);

    If DelayTime <> 0 Then
    Begin
      i := Y1 + ((Y2 - Y1 + 1) Shr 1);
      ie := i;
      j := X1 + ((X2 - X1 + 1) Shr 1);
      je := j;
      Update := False;

      Repeat
        If i > Y1 Then Dec (i)
                  Else i := Y1;
        If ie < Y2 Then Inc (ie)
                   Else ie := Y2;
        If j > X1 Then Dec (j)
                  Else j := X1;
        If je < X2 Then Inc (je)
                   Else je := X2;

        Update := Not Update;

        If Update Or ((i = Y1) And (ie = Y2) And (j = X1) And (je = X2)) Then
        Begin
          ClearWindow (j + 1, i + 1, je - 1, ie - 1, ' ', Color);
          FrameWindow (j, i, je, ie, Color, TitleAttr, '');
          If (X2 < ScrX) Or (Y2 < ScrY) Then
            Shadow (j, i, je, ie);
          Pause (DelayTime);
        End;
      Until (i = Y1) And (j = X1) And (ie = Y2) And (je = X2);

      i := Length (Title);
      If i > 0 Then
      Begin
        If X2 <= X1 Then j := 0
                    Else j := X2 - X1 - 1;
        If i > j Then
          i := j;
        Title [0] := Chr (i);
        FastWrite (Title, Y1, X1 + ((j - i) Shr 1) + 1, TitleAttr);
      End;
    End Else
    Begin
      ClearWindow (X1, Y1, X2, Y2, ' ', Color);
      FrameWindow (X1, Y1, X2, Y2, Color, TitleAttr, Title);
      If (X2 < ScrX) Or (Y2 < ScrY) Then
        Shadow (X1, Y1, X2, Y2);
    End;

    If Title [1] in [#176..#223] Then
    Begin
      i := X1 + (((X2 - X1) - Length (Title)) Shr 1);
      ChangeAttribute (2, Y1, i, Color);
      ChangeAttribute (2, Y1, i + Length (Title) - 1, Color);
    End;
  End;
End;

Procedure CloseWindow (Var Box: PBoxRec);
Var
  i, j : Byte;

Begin
  If Box <> Nil Then
  Begin
    With Box^ Do
    Begin
      CalcSaveArea (X2, Y2, i, j);
      RestoreWindow (X1, Y1, i, j, True, Covers);
    End;

    Dispose (Box);
  End;
End;

Procedure TitleColor (W: PBoxRec; Num, Attr: Byte);
Begin
  With W^ Do
    ChangeAttribute (Length (Title) - Num * 2 + 1, Y1,
      X1 + (((X2 - X1) - Length (Title)) Shr 1) + Num, Attr);
End;

Procedure Key (X, Y, TxtClr, Background: Byte; Const Txt: String);
Begin
  FastWrite (Txt, Y, X, TxtClr);
  FastWrite ('▄', Y, X + Length (Txt), Background);
  FastFill (Length (Txt), '▀', Y + 1, X + 1, Background);
End;

Procedure PressedKey (X, Y, TxtClr, BkgClr: Byte; Const Txt: String);
Begin
  FastWrite (' ', Y, X, BkgClr);
  FastWrite (Txt, Y, X + 1, TxtClr);
  FastFill (Length (Txt), ' ', Y + 1, X + 1, BkgClr);
End;

Procedure CenterTempBox (Color, KeyColor: Byte; cFrameType: Byte;
            Const cTitle: String; cDelayTime: Byte; Const WindowTitle: String);
Const
  MaxStrings = 10;

Var
  i, Len, Counter, HowMany : Byte;
  C                        : Char;
  MyBox                    : PBoxRec;
  TitleStr                 : Array [1..MaxStrings] Of String [70];

Label
  ShutDown;

Begin
  While KeyPressed Do
    ReadKey;

  HowMany := WordCount (cTitle, ['|']);
  If HowMany > MaxStrings Then
    HowMany := MaxStrings;
  Counter := 0;

  For i := 1 To HowMany Do
  Begin
    TitleStr [i] := ExtractWord (i, cTitle, ['|']);
    Len := Length (TitleStr [i]);
    If Len > Counter Then
      Counter := Len;
  End;

  If Not Odd (HowMany) And (HowMany < MaxStrings) Then
  Begin
    Inc (HowMany);
    TitleStr [HowMany] := ' ';
    If Counter = 0 Then
      Counter := 1;
  End;

  InitWindow (MyBox, 1, 1, Counter + 6, HowMany + 6, cFrameType, Color,
    ' ' + WindowTitle + ' ', Color, cDelayTime, True);
  DrawWindow (MyBox);

  With MyBox^ Do
  Begin
    For i := 1 To HowMany Do
      FastWrite (TitleStr [i], Y1 + i + 1, X1 + 1 + (X2 - X1 -
        Length (TitleStr [i])) Shr 1, Color);

    Key (36, Y2 - 2, KeyColor, Color And $F0, '   Ok   ');

    Repeat
      If Not WaitForKey (C) Then
        Goto ShutDown;
    Until C in ['O', 'o', 'K', 'k', 'Y', 'y', #27, #13, #32];

    PressedKey (36, Y2 - 2, KeyColor, Color And $F0, '   Ok   ');
    Pause (200);

    Key (36, Y2 - 2, KeyColor, Color And $F0, '   Ok   ');
    Pause (50);
  End;

ShutDown:
  CloseWindow (MyBox);
  While KeyPressed Do
    ReadKey;
End;

Procedure CenterDelayTempBox (Color, TxtColor, cFrameType: Byte;
            Const cTitle: String; cDelayTime: Byte; Const WindowTitle: String);
Const
  MaxStrings = 10;

Var
  i, Len, Delta, Counter, HowMany : Byte;
  TitleStr                        : Array [1..MaxStrings] Of String [70];

Begin
  While KeyPressed Do
    ReadKey;

  HowMany := WordCount (cTitle, ['|']);
  If HowMany > MaxStrings Then
    HowMany := MaxStrings;
  Counter := 0;

  For i := 1 To HowMany Do
  Begin
    TitleStr [i] := ExtractWord (i, cTitle, ['|']);
    Len := Length (TitleStr [i]);
    If Len > Counter Then
      Counter := Len;
  End;

  If Not Odd (HowMany) And (HowMany < MaxStrings) Then
  Begin
    Inc (HowMany);
    TitleStr [HowMany] := ' ';
    If Counter = 0 Then
      Counter := 1;
    Delta := 2;
  End
  Else
    Delta := 4;

  InitWindow (MyBox, 1, 1, Counter + 6, HowMany + Delta, cFrameType, Color,
    WindowTitle, Color, cDelayTime, True);
  DrawWindow (MyBox);

  With MyBox^ Do
    For i := 1 To HowMany Do
      FastWrite (TitleStr [i], Y1 + i + 1, X1 + 1 + (X2 - X1 -
        Length (TitleStr [i])) Shr 1, TxtColor);
End;

Procedure DoneTempBox;
Begin
  CloseWindow (MyBox);
End;

Function SelectBox (Const Choices: String; WAttr, BSAttr, TAttr, Delay: Byte;
           Const Text: String): Byte;
Const
  MaxStrings = 10;

Var
  i, Len, TWC, CWC, MaxLen : Byte;
  Y, ActiveItem            : LongInt;
  MyBox                    : PBoxRec;
  Items                    : PNotSortedCollection;
  S                        : String [70];
  TxtArr                   : Array [1..MaxStrings] Of String [70];

Begin
  TWC := WordCount (Text, ['|']);
  If TWC > MaxStrings Then
    TWC := MaxStrings;
  MaxLen := 0;

  For i := 1 To TWC Do
  Begin
    TxtArr [i] := ExtractWord (i, Text, ['|']);
    Len := Length (TxtArr [i]);
    If Len > MaxLen Then
      MaxLen := Len;
  End;

  Inc (MaxLen, 2);
  ActiveItem := 0;
  CWC := WordCount (Choices, ['|']);
  Items := New (PNotSortedCollection, Init (CWC, 1));

  For i := 1 To CWC Do
  Begin
    S := ExtractWord (i, Choices, ['|']);
    Len := Length (S);

    If (Len > 0) And (S [1] = '~') Then
    Begin
      ActiveItem := i - 1;
      Delete (S, 1, 1);
      Inc (Len);
    End
    Else
      Inc (Len, 2);

    If Len > MaxLen Then
      MaxLen := Len;

    Items^. Insert (NewStr (' ' + S));
  End;

  InitWindow (MyBox, 1, 1, MaxLen + 2, TWC + CWC + 3, 1, WAttr, '', TAttr,
    Delay, True);
  DrawWindow (MyBox);

  For i := 1 To TWC Do
    FastWrite (TxtArr [i], MyBox^. Y1 + i, MyBox^. X1 + 2, TAttr);
  FastFill (MaxLen, '─', MyBox^. Y1 + TWC + 1, MyBox^. X1 + 1, WAttr);

  If ActiveItem > 0 Then Y := Items^. Count - ActiveItem + 1
                    Else Y := 0;
  If ActiveItem = Items^. Count - 1 Then
    Dec (Y);

  SelectBox := tWinMenu (MyBox^. X1 + 1, MyBox^. Y1 + TWC + 2, MyBox^. X2,
    MyBox^. Y2 - 1, BSAttr, WAttr, Y, ActiveItem, Items, False, '', Nil);
  CloseWindow (MyBox);
  Dispose (Items, Done);
End;

Procedure ScrollTextWindow;
Var
  i, Len, MaxLen, Offs, XOffs : Byte;
  C                           : Char;
  HorScroll                   : Boolean;

Begin
  If TextBody^. Count <= Y2 - Y1 Then
  Begin
    For i := 0 To TextBody^. Count-1 Do
      FastWrite (Pad (PString (TextBody^. At (i))^, X2 - X1), Y1 + i, X1,
        (TxtBkg Shl 4) + TxtClr);

    Repeat
      If Not WaitForKey (C) Then
        Exit;
    Until C in [#27, #13, #32];

    Exit;
  End;

  MaxLen := 0;
  For i := 0 To TextBody^. Count-1 Do
  Begin
    Len := Length (PString (TextBody^. At (i))^);
    If Len > MaxLen Then
      MaxLen := Len;
  End;

  HorScroll := MaxLen > X2 - X1;
  Offs := 0;
  XOffs := 1;

  FastWrite (#30, Y1, X2, (SlightBkg Shl 4) + SLightClr);
  FastWrite (#31, Y2 - 1, X2, (SlightBkg Shl 4) + SLightClr);

  If HorScroll Then
  Begin
    FastWrite (#17, Y2, X1, (SlightBkg Shl 4) + SLightClr);
    FastWrite (#16, Y2, X2 - 1, (SlightBkg Shl 4) + SLightClr);
  End;

  Repeat
    For i := 1 To Y2 - Y1 Do
      FastWrite (Pad (Copy (PString (TextBody^. At (i - 1 + Offs))^, XOffs,
        X2 - X1), X2 - X1), Y1 + i - 1, X1, (TxtBkg Shl 4) + TxtClr);

    For i := 1 To Y2 - Y1 - 2 Do
      FastWrite ('▒', Y1 + i, X2, (SNormalBkg Shl 4) + SNormalClr);
    FastWrite ('■', Y1 + 1 + Round ((Y2 - Y1 - 3) * Offs / (TextBody^. Count -
      (Y2 - Y1))), X2, (SlightBkg Shl 4) + SLightClr);

    If HorScroll Then
    Begin
      FastFill (X2 - X1 - 2, '▒', Y2, X1 + 1, (SNormalBkg Shl 4) + SNormalClr);
      FastWrite ('■', Y2, X1 + 1 + Round ((X2 - X1 - 3) * (XOffs - 1) /
        (MaxLen - (X2 - X1))), (SlightBkg Shl 4) + SLightClr);
    End;

    If Not WaitForKey (C) Then
      Exit;

    If C = #0 Then
    Begin
      Case ReadKey Of
        #80: {Down}
             If Offs + (Y2 - Y1) < TextBody^. Count Then
               Inc (Offs);
        #72: {Up}
             If Offs > 0 Then
               Dec (Offs);
        #73: {PgUp}
             If Offs - (Y2 - Y1) > 0 Then Offs := Offs - Y2 + Y1
                                     Else Offs := 0;
        #81: {PgDown}
             If Offs + Y2 - Y1 >= TextBody^. Count - (Y2 - Y1) Then
               Offs := (TextBody^. Count - (Y2 - Y1))
             Else
               Offs := Offs + Y2 - Y1;
        #79: {End}
             Offs := TextBody^. Count - (Y2 - Y1);
        #71: {Home}
             Offs := 0;
        #77: {Right}
             If HorScroll Then
               If XOffs + X2 - X1 <= MaxLen Then
                 Inc (XOffs);
        #75: {Left}
             If HorScroll Then
               If XOffs > 1 Then
                 Dec (XOffs);
      End;
    End;
  Until C in [#27, #13, #32];
End;

Function tWinMenu (X1, Y1, X2, Y2, CLightColor, CDarkColor: Byte;
                   Var StartDisp: LongInt; StartItem: LongInt;
                   Items: PNotSortedCollection; Checks: Boolean;
                   Const LightWords: String; InfoProc: tInfoProc): LongInt;
Const
  MaxStrings = 10;
  Mark       : Array [Boolean] Of String [2] = ('  ', '√ ');

Var
  i, MaxLen, Height, sX, sY,
  sPos, Current              : LongInt;
  SaveWind                   : Pointer;
  LightNum, WC               : Word;
  Key, Delta                 : Byte;
  LightStrs                  : Array [1..MaxStrings] Of String [50];
  LightAttrs                 : Array [1..MaxStrings] Of Word;

  Procedure DrawItem (Const S: String; Y, X: Byte);
  Var
    i, P : Byte;

  Begin
    FastWrite (Pad (S, MaxLen), Y, X, cDarkColor);

    For i := 1 To LightNum Do
    Begin
      P := Pos (LightStrs [i], S);
      If P > 0 Then
        ChangeAttribute (Length (LightStrs [i]), Y, X + P - 1,
          LightAttrs [i]);
    End;
  End;

  Procedure DispMenuItems (Top, Bottom: LongInt);
  Var
    i, Y : LongInt;

  Begin
    For i := Top To Bottom Do
    Begin
      Y := Y1 + i - Current;
      If Y <= Y2 Then
        If Not Checks Then
          DrawItem (PString (Items^. At (i))^, Y, X1)
        Else
          With PMarkItem (Items^. At (i))^ Do
            DrawItem (Mark [Marked] + Text, Y, X1);
    End;
  End;

Label
  Down,
  Check;

Begin
  sX := WhereX;
  sY := WhereY;

  Delta := 0;

  MaxLen := X2 - X1;
  Height := Y2 - Y1;

  If (Height > Items^. Count) And (StartDisp = 0) And (StartItem > 0) Then
    StartDisp := StartItem;

  If (Height <= Items^. Count) And (Items^. Count - StartItem <= Height) Then
    StartDisp := Height - (Items^. Count - StartItem) + 1;

  If Height = Items^. Count Then
  Begin
    If StartDisp > 0 Then
      Dec (StartDisp);
    Dec (Height);
  End;

  If Height > Items^. Count Then
  Begin
    Height := Items^. Count - 1;
    Y2 := Items^. Count + Y1 - 1;
  End;

  If LightWords <> '' Then
  Begin
    WC := WordCount (LightWords, ['|']);
    If WC > MaxStrings * 2 Then
      WC := MaxStrings * 2;

    For LightNum := 1 To WC Do
      If Not Odd (LightNum) Then
        LightAttrs [LightNum Div 2] := Hex2Word (ExtractWord (LightNum,
          LightWords, ['|']))
      Else
        LightStrs [(LightNum + 1) Div 2] := ExtractWord (LightNum, LightWords,
          ['|']);

    LightNum := LightNum Div 2;
  End
  Else
    LightNum := 0;

  SaveWindow (X1, Y1, X1 + MaxLen - 1, Y2, True, SaveWind);
  ClearWindow (X1, Y1, X1 + MaxLen - 1, Y2, ' ', CDarkColor);

  If StartItem < Items^. Count Then Current := StartItem
                               Else Current := 0;
  sPos := Current - StartDisp;
  If sPos < 0 Then
  Begin
    sPos := 1;
    StartDisp := 0;
    Delta := 1;
  End;

  For i := sPos To sPos+Height Do
    If i < Items^. Count Then
      If Not Checks Then
        DrawItem (PString (Items^. At (i))^, Y1 + i - sPos + Delta, X1)
      Else
        With PMarkItem (Items^. At (i))^ Do
          DrawItem (Mark [Marked] + Text, Y1 + i - sPos + Delta, X1);

  GoToXY (X1, Y1 + StartDisp);

  If Not Checks Then
    FastWrite (Pad (PString (Items^. At (Current))^, MaxLen), WhereY, X1,
      CLightColor)
  Else
    With PMarkItem (Items^. At (Current))^ Do
      FastWrite (Pad (Mark [Marked] + Text, MaxLen), WhereY, X1, CLightColor);

  If Assigned (InfoProc) Then
    InfoProc (PString (Items^. At (Current))^);

  Repeat
    Key := Ord (ReadKey);

    If Key = 0 Then
    Begin
      Key := Ord (ReadKey);

      If Key in [71..73, 75, 77, 79..82] Then
      Begin
        Case Key Of
         75, 72 : If Current > 0 Then                 { .Up.    }
                  Begin
                    If (WhereY > Y1) And (WhereY <= Y2) Then
                    Begin
                      If Checks Then
                        With PMarkItem (Items^. At (Current))^ Do
                          DrawItem (Mark [Marked] + Text, WhereY, X1)
                      Else
                        DrawItem (PString (Items^. At (Current))^, WhereY, X1);

                      ChangeAttribute (MaxLen, WhereY - 1, X1, CLightColor);
                      GotoXY (X1, WhereY - 1);
                    End Else
                    Begin
                      ScrollWindowDown (X1, Y1, X2-1, Y2, 1);
                      If Checks Then
                      Begin
                        With PMarkItem (Items^. At (Current))^ Do
                          DrawItem (Mark [Marked] + Text, WhereY + 1, X1);
                        With PMarkItem (Items^. At (Current-1))^ Do
                          FastWrite (Pad (Mark [Marked] + Text, MaxLen), Y1,
                            X1, cLightColor);
                      End Else
                      Begin
                        DrawItem (PString (Items^. At (Current))^, WhereY + 1,
                          X1);
                        FastWrite (Pad (PString (Items^. At (Current-1))^,
                          MaxLen), Y1, X1, cLightColor);
                      End;
                    End;
                    Dec (Current);
                  End;

         77, 80 :
                Down:
                  If Current < Items^. Count-1 Then   { .Down.  }
                  Begin
                    If (WhereY >= Y1) And (WhereY < Y2) Then
                    Begin
                      If Checks Then
                        With PMarkItem (Items^. At (Current))^ Do
                          DrawItem (Mark [Marked] + Text, WhereY, X1)
                      Else
                        DrawItem (PString (Items^. At (Current))^, WhereY, X1);
                      ChangeAttribute (MaxLen, WhereY + 1, X1, CLightColor);
                      GotoXY (X1, WhereY + 1);
                    End Else
                    Begin
                      ScrollWindowUp (X1, Y1, X2-1, Y2, 1);
                      If Checks Then
                      Begin
                        With PMarkItem (Items^. At (Current))^ Do
                          DrawItem (Mark [Marked] + Text, WhereY - 1, X1);
                        With PMarkItem (Items^. At (Current+1))^ Do
                          FastWrite (Pad (Mark [Marked] + Text, MaxLen), Y2,
                            X1, CLightColor);
                      End Else
                      Begin
                        DrawItem (PString (Items^. At (Current))^, WhereY - 1,
                          X1);
                        FastWrite (Pad (PString (Items^. At (Current+1))^,
                          MaxLen), Y2, X1, CLightColor);
                      End;
                    End;
                    Inc (Current);
                  End;

             73 : Begin                               { .PgUp.  }
                    If Current = 0 Then
                      Continue;

                    If WhereY = Y1 Then
                    Begin
                      If Current - Y2 + Y1 >= 0 Then
                      Begin
                        i := Current;
                        Dec (Current, Y2 - Y1);
                      End Else
                      Begin
                        i := Height;
                        Current := 0;
                      End;
                      DispMenuItems (Current, i);
                    End Else
                    Begin
                      If Checks Then
                        With PMarkItem (Items^. At (Current))^ Do
                          DrawItem (Mark [Marked] + Text, WhereY, X1)
                      Else
                        DrawItem (PString (Items^. At (Current))^, WhereY, X1);
                      Current := Current - WhereY + Y1;
                    End;

                    ChangeAttribute (MaxLen, Y1, X1, CLightColor);
                    GotoXY (X1, Y1);
                  End;

             81 : Begin                               { .PgDn.  }
                    If Current = Items^. Count-1 Then
                      Continue;

                    If WhereY = Y2 Then
                    Begin
                      If Current + Height > Items^. Count-1 Then
                      Begin
                        i := Items^. Count - 1;
                        Current := i - Height;
                      End
                      Else
                        i := Current + Height;
                      DispMenuItems (Current, i);
                      Current := i;
                    End Else
                    Begin
                      If Checks Then
                        With PMarkItem (Items^. At (Current))^ Do
                          DrawItem (Mark [Marked] + Text, WhereY, X1)
                      Else
                        DrawItem (PString (Items^. At (Current))^, WhereY, X1);
                      Current := Current + Y2 - WhereY;
                      If Current > Items^. Count - 1 Then
                        Current := Items^. Count - 1;
                    End;

                    ChangeAttribute (MaxLen, Y1 + Height, X1, CLightColor);
                    GotoXY (X1, Y1 + Height);
                  End;

             71 : If Current <> 0 Then { Home }
                  Begin
                    Current := 0;
                    DispMenuItems (Current, Height);
                    ChangeAttribute (MaxLen, Y1, X1, CLightColor);
                    GotoXY (X1, Y1);
                  End;

             79 : If Current <> Items^. Count-1 Then { End }
                  Begin
                    i := Items^. Count-1;
                    Current := i-Height;
                    If Current < 0 Then
                      Current := 0;
                    DispMenuItems (Current, i);
                    ChangeAttribute (MaxLen, Y1 + Height, X1, CLightColor);
                    GotoXY (X1, Y1 + Height);
                    Current := Items^. Count-1;
                  End;

             82 :
                Check:
                  If Checks Then
                  Begin
                    With PMarkItem (Items^. At (Current))^ Do
                    Begin
                      Marked := Not Marked;
                      FastWrite (Mark [Marked], WhereY, WhereX, CLightColor);
                    End;
                    Goto Down;
                  End;
        End;

        If Assigned (InfoProc) Then
          InfoProc (PString (Items^. At (Current))^);
      End;
    End
    Else
      Case Key Of
        13 : Begin                               { .Enter. }
               tWinMenu := Current + 1;
               Break;
             End;
        27 : Begin                               { .Esc.   }
               tWinMenu := 0;
               Break;
             End;
        32 : Goto Check;
      End;
  Until False;

  StartDisp := WhereY - Y1;
  RestoreWindow (X1, Y1, X1 + MaxLen - 1, Y2, True, SaveWind);
  GotoXY (sX, sY);
End;

Procedure Rotate (X, Y: Byte; Color: Word);
Begin
  FastWrite (RotBar [RotState], Y, X, Color);
  Inc (RotState);
  If RotState > RotNum Then
    RotState := 1;
End;

Procedure TWinInit;
Begin
  ScrX := Lo (WindMax);
  ScrY := Hi (WindMax);
End;

End.
