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
  SaveTag;

Interface

Procedure SaveTagList;
Function RestoreTagList: Boolean;

Implementation

Uses
  DOS,
  Objects,
  tGlob,
  tMisc,
  Crc,
  BinCfg,
  SysMsgs,
  Log;

Const
  Separator = #255;
  SepSet {$IFDEF VirtualPascal} : Set of Char {$ENDIF} = [Separator];

Procedure SaveTagList;
Const
  NChar : Array [Boolean] Of Char = ('+', '#');

Var
  LastGroup, LastArea,
  OldGroup, OldArea    : LongInt;
  i                    : Integer;
  GSum, ASum           : tChecksum;
  F                    : Text;
  Id1                  : String;
  FileBuf              : FileBufArr;

Begin
  If (F2Transfer^. Count = 0) Or Not Cnf. SaveTagList Or Registering Or
     (R. Name = '')
  Then
    Exit;

  Assign (F, Cnf. SaveTagPath + HexL (Crc32Str (R. Name)) + '.lst');
  SetTextBuf (F, FileBuf, FileBufSize);
  ReWrite (F);
  If IOResult <> 0 Then
  Begin
    LogWrite ('!', sm (smTagSaveErr));
    Exit;
  End;

  WriteLn (F, '; ' + R. Name);
  Id1 := UpString (Cnf. TempDir + 'file_id.1');

  OldGroup := R. FileGroup;
  OldArea := R. FileArea;
  LastGroup := OldGroup;
  LastArea := OldArea;

  OpenFileGroups;
  OpenFileAreas;

  For i := 0 To F2Transfer^. Count-1 Do
    With PTagFileRec (F2Transfer^. At (i))^ Do
      If (PathName^ <> '!') And (UpString (PathName^) <> Id1) Then
        If FromName = Nil Then
          If AreaNum > 0 Then
          Begin
            SmartChangeFArea (GroupNum, AreaNum, LastGroup, LastArea);
            GetChecksum (FileGroup. Name + FileGroup. Tag, GSum);
            GetChecksum (FileArea. Name + FileArea. Group, ASum);
            WriteLn (F, NChar [Free] + PathName^ + Separator + HexL (GSum. CRC)
              + Separator + HexW (GSum. Sum) + Separator + HexL (ASum. CRC) +
              Separator + HexW (ASum. Sum));
          End
          Else
            WriteLn (F, NChar [Free] + PathName^)
        Else
          WriteLn (F, '*' + FromName^ + '|' + PathName^);

  Close (F);

  SmartChangeFArea (OldGroup, OldArea, LastGroup, LastArea);
  CloseFileGroups;
  CloseFileAreas;
End;

Function RestoreTagList: Boolean;
Var
  i, G, A, MaxGroups : Integer;
  OldGroup, OldArea  : LongInt;
  F                  : Text;
  FileRec            : TTagFileRec;
  DirInfo            : SearchRec;
  CSum, GSum, ASum   : tChecksum;
  S, UName           : String;
  FileBuf            : FileBufArr;
  C                  : Char;
  AreaReady          : Boolean;

Label
  CheckCurrArea;

Begin
  RestoreTagList := False;
  If Not Cnf. SaveTagList Then
    Exit;

  Assign (F, Cnf. SaveTagPath + HexL (Crc32Str (R. Name)) + '.lst');
  SetTextBuf (F, FileBuf, FileBufSize);
  ReSet (F);
  If IOResult <> 0 Then
    Exit;

  OldGroup := R. FileGroup;
  OldArea := R. FileArea;
  OpenFileGroups;
  OpenFileAreas;
  SetFileGroup (0);
  SetFileArea (0);

  MaxGroups := ffGroupsAmount;
  If MaxGroups = 0 Then
    MaxGroups := 1;
  G := 1;
  A := 1;
  AreaReady := True;

  With FileRec Do
  Begin
    UName := '';

    While Not EOF (F) Do
    Begin
      ReadLn (F, S);
      If (S = '') Or (S [1] = ';') Then
        Continue;

      GroupNum := 0;
      Free := False;

      C := S [1];
      If C in ['+', '*', '#'] Then
        Delete (S, 1, 1);

      If C = '*' Then
      Begin
        AreaNum := -20;

        i := Pos ('|', S);
        If i <> 0 Then
        Begin
          UName := Copy (S, 1, i - 1);
          Delete (S, 1, i);
        End
        Else
          UName := 'Unknown';
      End Else
      Begin
        AreaNum := -10;
        If C = '#' Then
          Free := True;

        i := WordCount (S, SepSet);
        If i > 1 Then
        Begin
          If i = 5 Then
          Begin
            GSum. CRC := Hex2Long (ExtractWord (2, S, SepSet));
            GSum. Sum := Hex2Word (ExtractWord (3, S, SepSet));
            ASum. CRC := Hex2Long (ExtractWord (4, S, SepSet));
            ASum. Sum := Hex2Word (ExtractWord (5, S, SepSet));
            i := 0;

            Repeat
              GetChecksum (FileGroup. Name + FileGroup. Tag, CSum);
              If (CSum. CRC = GSum. CRC) And (CSum. Sum = GSum. Sum) Then
              Begin
                i := 0;

                If AreaReady Then
                  Goto CheckCurrArea;

                AreaReady := True;

                Repeat
                  Inc (i);
                  If i > fAreasGroup Then
                    Break;

                  SetFileArea (A);
                  Inc (A);
                  If A > fAreasGroup Then
                    A := 1;

                CheckCurrArea:
                  GetChecksum (FileArea. Name + FileArea. Group, CSum);
                  If (CSum. CRC = ASum. CRC) And (CSum. Sum = ASum. Sum) Then
                  Begin
                    GroupNum := R. FileGroup;
                    AreaNum := R. FileArea;

                    Break;
                  End;
                Until False;

                Break;
              End;

              Inc (i);
              If i > MaxGroups Then
                Break;

              SetFileGroup (G);
              Inc (G);
              If G > MaxGroups Then
                G := 1;
              A := 1;
              AreaReady := False;
            Until False;
          End;

          S := ExtractWord (1, S, SepSet);
        End;
      End;

      If S <> '' Then
      Begin
        FindFirst (S, AnyFile-VolumeID-Directory-Hidden, DirInfo);

        If DosError = 0 Then
        Begin
          Size := DirInfo. Size;
          PathName := NewStr (S);
          If UName <> '' Then
          Begin
            FromName := NewStr (UName);
            UName := '';
          End
          Else
            FromName := Nil;

          InsertInTagList (FileRec);
          RestoreTagList := True;
        End;

      {$IFNDEF MSDOS}
        FindClose (DirInfo);
      {$ENDIF}
      End;
    End;
  End;

  Close (F);
  Erase (F);

  SetFileGroup (OldGroup);
  SetFileArea (OldArea);
  CloseFileGroups;
  CloseFileAreas;
End;

End.
