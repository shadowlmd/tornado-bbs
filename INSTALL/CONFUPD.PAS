{$IFNDEF WIN32}
{$O+,F+}
{$ENDIF}
{$I-}
{&Use32-}

Unit
  ConfUpd;

Interface

Uses
{$IFNDEF WIN32}
  DOS,
{$ELSE}
  OpCrt,
  SysUtils,
{$ENDIF}
  Objects,
  tGlob,
  tMisc;

Const
  Alpha : Boolean = False;

Procedure UpdateConfigFiles (UpdFileName: PathStr; Ver: String);
Function DetectVer (InstDir: PathStr): String;

Implementation

Procedure UpdateConfigFiles (UpdFileName: PathStr; Ver: String);
Type
  tAction = (Delete, Add, Change, AddSection);

Var
  UpdFile                       : Text;
  S, NextUpdStr                 : String;
  ForAll, PrevFileExist, First  : Boolean;
  Txt                           : PNotSortedCollection;

Procedure Operate (FileName, Section, ParamName: String; Action: tAction);
Var
  CurString                    : String;
  F1, F2                       : Text;
  i                            : LongInt;
  SectionReached, ParamReached : Boolean;

Label
  EoP,
  Loop;

Begin
  PrevFileExist := False;
  If Not FileExists (FileName) Then Exit;

  PrevFileExist := True;

  Assign (F1, FileName);
  Assign (F2, 'install.tmp');
  ReSet (F1); ReWrite (F2);

  ParamName := UpString (ParamName);

  If Action in [Change, Add] Then
  Begin
    Txt^. DeleteAll;
    ReadLn (UpdFile, S);

    While Not (S [1] in ['%', '+', '!'])
          And (Copy (S, 1, 2) <> 'V ')
          And Not EoF (UpdFile) Do
    Begin
      If S = '' Then S := ' ';
      Txt^. Insert (NewStr (S));
      ReadLn (UpdFile, S);
    End;

    NextUpdStr := S;
    If EoF (UpdFile) Then Txt^. Insert (NewStr (S));
  End;

  First := True;
  Loop:

  { Ищем нужную секцию }

  While (Trim (UpString (ExtractWord (1, S, [' ']))) <> '[' +
         UpString (Section) + ']') And Not EoF (F1) Do
  Begin
    ReadLn (F1, S);
    WriteLn (F2, S);
  End;

  SectionReached := Trim (UpString (ExtractWord (1, S, [' ']))) = '[' + UpString (Section) + ']';

  If (EoF (F1) And (Action <> AddSection) And Not SectionReached) Or
     ((Action = AddSection) And SectionReached)
  Then GoTo EoP;

  If Action = AddSection Then
  Begin
    { добавляем секцию }
    WriteLn (F2);
    WriteLn (F2, '[' + Section + ']');
    GoTo EoP;
  End;

  ReadLn (F1, S);
  CurString := Trim (UpString (ExtractWord (1, S, [' '])));

  { Ищем нужный параметр в секции }

  While (CurString <> ParamName) And Not EoF (F1) And (CurString [1] <> '[') Do
  Begin
    WriteLn (F2, S);
    ReadLn (F1, S);
    CurString := Trim (UpString (ExtractWord (1, S, [' '])));
  End;

  ParamReached := CurString = ParamName;

  Case Action Of
    Delete : Begin
               CurString := ';';

               While (CurString = ';') And Not EoF (F1) Do
               Begin
                 ReadLn (F1, S);
                 CurString := Trim (UpString (ExtractWord (1, S, [' '])));
               End;

               If Not EoF (F1) Then WriteLn (F2, S);

               First := False;
               If ForAll Then GoTo Loop;
             End;

    Add    : Begin
               If EoF (F1) And Not ParamReached Then WriteLn (F2, S);
               If ParamReached Then WriteLn (F2, S) Else
               Begin
                 If First Then
                 Begin
                   For i := 0 To Txt^. Count-1 Do
                   If CurString <> ParamName Then
                     WriteLn (F2, PString (Txt^. At (i))^);
                 End Else
                   WriteLn (F2, ExtractWord (1,
                   PString (Txt^. At (0))^, [';']));

                 If ForAll Then
                 Begin
                   If (CurString <> ParamName) And Not EoF (F1) Then WriteLn (F2, S);
                   First := False;
                   GoTo Loop;
                 End Else
                   If Not EoF (F1) Then WriteLn (F2, S);
               End;
             End;

    Change : Begin
               If CurString = ParamName Then
               Begin
                 CurString := ';';

                 While (CurString = ';') And Not EoF (F1) Do
                 Begin
                   ReadLn (F1, S);
                   CurString := Trim (UpString (ExtractWord (1, S, [' '])));
                 End;

                 For i := 0 To Txt^. Count-1 Do
                   If CurString <> ParamName Then
                     WriteLn (F2, PString (Txt^. At (i))^);

                 If Not EoF (F1) Then WriteLn (F2, S);
                 First := False;
                 If ForAll Then GoTo Loop;
               End;

               If ForAll Then
               Begin
                 WriteLn (F2, S);
                 First := False;
                 GoTo Loop;
               End;

             End;

  End;

  EoP:

  While Not EoF (F1) Do
  Begin
    ReadLn (F1, S);
    WriteLn (F2, S);
  End;

  Close (F1);
  Close (F2);
  Erase (F1);
  Rename (F2, FileName);
End;

Var
  Action                : tAction;
  Section, Parameter    : String [30];
  FileName              : String [12];
  sAction               : String [ 2];
  i                     : LongInt;
  Found                 : Boolean;

Begin
  If Not FileExists (UpdFileName) Then Exit;
  Txt := New (PNotSortedCollection, Init (5, 5));

  Assign (UpdFile, UpdFileName);
  ReSet (UpdFile);

  If Alpha Then
  Begin
    While Not EoF (UpdFile) Do
    Begin
      ReadLn (UpdFile, S);
      If (Length (S) > 2) And (Copy (S, 1, 2) = 'V ') Then Txt^. Insert (NewStr (Copy (S, 3, 255)));
    End;

    Found := False;
    For i := 0 To Txt^. Count-1 Do
    Begin
      If PString (Txt^. At (i))^ = Ver Then
      Begin
        Found := True;
        If i > 0 Then Ver := PString (Txt^. At (i-1))^;
      End;
    End;
    If Not Found Then Ver := PString (Txt^. At (Txt^. Count-2))^;

    Close (UpdFile);
    ReSet (UpdFile);
    Txt^. FreeAll;
    Txt^. DeleteAll;
  End;

  Action := Delete;
  PrevFileExist := False;

  If Ver <> '' Then
  While Not EoF (UpdFile) Do
  Begin
    ReadLn (UpdFile, S);
    If S [1] = 'V' Then If ExtractWord (2, S, [' ']) = Ver Then Break;
  End;

  While Not EoF (UpdFile) Do
  Begin
    If (Action in [Change, Add]) And PrevFileExist
    Then
      S := NextUpdStr
    Else
      ReadLn (UpdFile, S);

    If ExtractWord (2, S, [' ']) = 'V' Then Continue;

    sAction := ExtractWord (1, S, [' ']);
    FileName := ExtractWord (2, S, [' ']);
    Section := ExtractWord (3, S, [' ']);
    Parameter := ExtractWord (4, S, [' ']);

    ForAll := (Length (sAction) = 2) And (sAction [2] = 'A');

    Case sAction [1] Of
      '!' : Action := Delete;
      '+' : Action := Add;
      '%' : Action := Change;
      '~' : Action := AddSection;
    Else
      Action := Delete;
      Continue;
    End;

    Operate (FileName, Section, Parameter, Action);

  End;

  Close (UpdFile);
{$IFNDEF AUTHOR}
  Erase (UpdFile);
{$ENDIF}
End;

Function DetectVer (InstDir: PathStr): String;
Var
  F                 : File;
  S                 : String;
  i                 : Byte;

Begin
  Alpha := False;

  Assign (F, AddBackSlash (InstDir) + 'tornado.msg');
  ReSet (F, 1);

  If IOResult = 0 Then
  Begin
    BlockRead (F, S, 50);
    Alpha := Pos ('alpha', S) <> 0;
    i := Pos ('alpha', S);
    If i = 0 Then i := Pos ('beta', S);
    If i = 0 Then i := Pos ('gamma', S);
    S := Copy (S, 1, i-1);
  End;

  Close (F);
  DetectVer := S;
End;

End.