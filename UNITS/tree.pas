{$IFDEF MSDOS}
  {$IFNDEF DPMI32}
    {$F+}
  {$IFNDEF DPMI}
    {$O+}
  {$ENDIF}
  {$ENDIF}
{$ENDIF}
{$I-}

Unit Tree;

Interface

Uses
  DOS,
  Objects,
  tGlob,
  tMisc;

Type
  ShowFileFunc = Function (Const FName: PathStr): Boolean;

Function DirSizeWSb (Const Target: String): LongInt;
Procedure DelWithSubDirs (Const Target: String);
Procedure fLocate (Const Mask: PathStr; ShowFunc: ShowFileFunc);

Implementation

Function DirSizeWSb (Const Target: String): LongInt;
Var
  GlobalSize : LongInt;

  Procedure SearchDir (Const Dir: PathStr);
  Var
    SRec : SearchRec;

  Begin
    FindFirst (Dir + '\' + AllFilesMask, AnyFile, SRec);

    While DosError = 0 Do
    Begin
      If SRec. Attr And Directory <> 0 Then
      Begin
        If SRec. Name [1] <> '.' Then
          SearchDir (Dir + '\' + SRec. Name);
      End
      Else
        Inc (GlobalSize, SRec. Size);

      FindNext (SRec);
    End;

  {$IFNDEF MSDOS}
    FindClose (SRec);
  {$ENDIF}
  End;

Var
  Dir  : DirStr;
  Name : NameStr;
  Ext  : ExtStr;

Begin
  GlobalSize := 0;
  SmartChDir (Target);
  FSplit (FExpand (''), Dir, Name, Ext);
  If Dir [Length (Dir)] = '\' Then
    Dir := Copy (Dir, 1, Length (Dir) - 1);
  SearchDir (Dir);
  DirSizeWSb := GlobalSize;
End;

Procedure DelWithSubDirs (Const Target: String);

  Procedure SearchDir (Const Dir: PathStr);
  Var
    SRec : SearchRec;
    F    : File;

  Begin
    FindFirst (Dir + '\' + AllFilesMask, AnyFile, SRec);

    While DosError = 0 Do
    Begin
      If (SRec. Attr And Directory <> 0) Then
      Begin
        If SRec. Name [1] <> '.' Then
        Begin
          SearchDir (Dir + '\' + SRec. Name);
          RmDir (Dir + '\' + SRec. Name);
        End;
      End Else
      Begin
        Assign (F, Dir + '\' + SRec. Name);
        SetFAttr (F, 0);
        Erase (F);
      End;

      FindNext (SRec);
    End;

  {$IFNDEF MSDOS}
    FindClose (SRec);
  {$ENDIF}
  End;

Var
  Dir  : DirStr;
  Name : NameStr;
  Ext  : ExtStr;

Begin
  SmartChDir (Target);
  FSplit (FExpand (''), Dir, Name, Ext);
  If Dir [Length (Dir)] = '\' Then
    Dir := Copy (Dir, 1, Length (Dir) - 1);
  SearchDir (Dir);
  ChDir ('..');
  RmDir (Dir);
End;

Procedure fLocate (Const Mask: PathStr; ShowFunc: ShowFileFunc);
Var
  MaskName : PathStr;
  Aborted  : Boolean;

  Procedure SearchDir (Const Dir: PathStr);
  Var
    SRec : SearchRec;

  Begin
    If Aborted Then
      Exit;

    FindFirst (Dir + '\' + AllFilesMask, AnyFile, SRec);

    While DosError = 0 Do
    Begin
      If SRec. Attr And Directory <> 0 Then
      Begin
        If SRec. Name [1] <> '.' Then
        Begin
          SearchDir (Dir + '\' + SRec. Name);
          If Aborted Then
            Break;
        End;
      End
      Else
        If SRec. Attr And VolumeID = 0 Then
          If MatchWildCard (SRec. Name, MaskName) Then
          Begin
            Aborted := Not ShowFunc (Dir + '\' + SRec. Name);
            If Aborted Then
              Break;
          End;

      FindNext (SRec);
    End;

  {$IFNDEF MSDOS}
    FindClose (SRec);
  {$ENDIF}
  End;

Begin
  Aborted := False;
  MaskName := JustFileName (Mask);
  SearchDir (JustPathName (Mask));
End;

End.
