{$IFDEF MSDOS}
  {$IFNDEF DPMI32}
    {$F+}
  {$IFNDEF DPMI}
    {$DEFINE RealMode}
  {$ENDIF}
  {$ENDIF}
{$ENDIF}
{$I-}

Unit BinCfg;

Interface

Uses
  tGlob;

Const
  faOpen : Boolean = False;
  maOpen : Boolean = False;
  fgOpen : Boolean = False;
  mgOpen : Boolean = False;

Function OpenFileGroups: Boolean;
Function ReadFileGroup (Var FileGroup: tFileGroup; Num: System. Word): Boolean;
Procedure StartFGroupReadSequence (From: Word);
Function ReadNextFileGroup (Var FileGroup: tFileGroup): Boolean;
Procedure CloseFileGroups;

Function OpenFileAreas: Boolean;
Function ReadFileArea (Var FileArea: tFileArea; Num: System. Word): Boolean;
Procedure StartFAreaReadSequence (From: Word);
Function ReadNextFileArea (Var FileArea: tFileArea): Boolean;
Procedure CloseFileAreas;

Procedure SetFileGroup (Num: LongInt);
Procedure SetFileArea (Num: LongInt);
Procedure SmartChangeFArea (NewGroup, NewArea: LongInt;
                            Var LastGroup, LastArea: LongInt);

Function OpenMsgGroups: Boolean;
Function ReadMsgGroup (Var MsgGroup: tMsgGroup; Num: System. Word): Boolean;
Procedure StartMGroupReadSequence (From: Word);
Function ReadNextMsgGroup (Var MsgGroup: tMsgGroup): Boolean;
Procedure CloseMsgGroups;

Function OpenMsgAreas: Boolean;
Function ReadMsgArea (Var MsgArea: tMsgArea; Num: System. Word): Boolean;
Procedure StartMAreaReadSequence (From: Word);
Function ReadNextMsgArea (Var MsgArea: tMsgArea): Boolean;
Procedure CloseMsgAreas;

Procedure SetMsgGroup (Num: LongInt);
Procedure SetMsgArea (Num: LongInt);
Procedure SmartChangeMArea (NewGroup, NewArea: LongInt;
                            Var LastGroup, LastArea: LongInt);

Procedure SetDefaultAreas;
Procedure DoneAreasGroups;

Implementation

Uses
  DOS,
  Objects,
{$IFDEF RealMode}
  Streams,
{$ENDIF}
  TMisc,
  Parse;

Const
{$IFDEF RealMode}
  IndexGroupDelta = 2;
  IndexAreaDelta  = 4;
  FileBufSize     = 2048;
{$ELSE}
  IndexGroupDelta = 4;
  IndexAreaDelta  = 8;
  FileBufSize     = 4096;
{$ENDIF}

Type
  PGroupIndexRec = ^GroupIndexRec;
  GroupIndexRec = Record
    PhysNum : Word;
    Areas   : PLongIntCollection;
  End;

  PGroupCollection = ^TGroupCollection;
  TGroupCollection = Object (TCollection)
    Procedure FreeItem (Item: Pointer); Virtual;
  End;

  PIndexRec = ^TIndexRec;
  TIndexRec = Record
    Groups : TGroupCollection;
    Sec    : System. Word;
    Flags  : String [26];
  End;
  PFileBufArr = ^FileBufArr;
  FileBufArr = Array [0..FileBufSize-1] Of Byte;

Const
  faStream : PStream = Nil;
  maStream : PStream = Nil;
  fgStream : PStream = Nil;
  mgStream : PStream = Nil;
  fIndex   : PIndexRec = Nil;
  mIndex   : PIndexRec = Nil;

Var
  FAreaSeqNum, MAreaSeqNum, FGroupSeqNum, MGroupSeqNum : Word;
  faFile, maFile, fgFile, mgFile                       : File;

Procedure TGroupCollection. FreeItem (Item: Pointer);
Var
  G : PGroupIndexRec Absolute Item;

Begin
  If G <> Nil Then
  Begin
    If G^. Areas <> Nil Then
      Dispose (G^. Areas, Done);
    Dispose (G);
  End;
End;

Procedure KillStream (Var S: PStream);
Begin
  If S <> Nil Then
  Begin
    Dispose (S, Done);
    S := Nil;
  End;
End;

Procedure AllocStream (Var S: PStream; Size: LongInt);
Begin
{$IFDEF RealMode}
  If S = Nil Then
  Begin
    S := New (PXMSStream, Init (Size, Size));
    If S <> Nil Then
      If S^. Status <> stOk Then
        KillStream (S);

    If S = Nil Then
    Begin
      S := New (PEMSStream3, Init (Size, Size));
      If S <> Nil Then
        If S^. Status <> stOk Then
          KillStream (S);
    End;
  End;
{$ELSE}
  S := New (PMemoryStream, Init (Size, 0));
  If S <> Nil Then
    If S^. Status <> stOk Then
      KillStream (S);
{$ENDIF}
End;

Procedure ReInitIndex (Var Index: PIndexRec; NewSec: Word;
                       Const NewFlags: String);
Begin
  If Index <> Nil Then
  Begin
    If (Index^. Sec = NewSec) And (Index^. Flags = NewFlags) Then
      Exit;

    Index^. Groups. FreeAll;
  End Else
  Begin
    New (Index);
    Index^. Groups. Init (IndexGroupDelta, IndexGroupDelta);
  End;

  Index^. Sec := NewSec;
  Index^. Flags := NewFlags;
End;

Procedure KillIndex (Var Index: PIndexRec);
Begin
  If Index <> Nil Then
  Begin
    Index^. Groups. Done;
    Dispose (Index);
    Index := Nil;
  End;
End;

Procedure DoneAreasGroups;
Begin
  KillStream (fgStream);
  KillStream (faStream);
  KillStream (mgStream);
  KillStream (maStream);
  KillIndex (fIndex);
  KillIndex (mIndex);
End;

Function OpenFileGroups: Boolean;
Var
  fTime1, fTime2,
  fSize2          : LongInt;
  Buf             : PFileBufArr;
  i               : Integer;
  DirInfo         : SearchRec;
  Hdr             : tBinaryHdr;
  R               : tFileGroup;
  BinName         : String;

Begin
  OpenFileGroups := True;
  If fgOpen Then
    Exit;

  FindFirst (Cnf. FileGroupsFile, AnyFile-VolumeID-Directory-Hidden, DirInfo);
  fTime1 := DirInfo. Time;
{$IFNDEF MSDOS}
  FindClose (DirInfo);
{$ENDIF}

  If DosError <> 0 Then
  Begin
    OpenFileGroups := False;
    Exit;
  End;

  BinName := AddBackSlash (JustPathName (Cnf. FileGroupsFile)) + 'fgroups' +
    LineExt;

  FindFirst (BinName, AnyFile-VolumeID-Directory-Hidden, DirInfo);
  If DosError = 0 Then
  Begin
    fTime2 := DirInfo. Time;
    fSize2 := DirInfo. Size;
  End
  Else
    fTime2 := -1;

{$IFNDEF MSDOS}
  FindClose (DirInfo);
{$ENDIF}

  If (fTime1 = fTime2) And (fgStream <> Nil) Then
    fGroupsAmount := fgStream^. GetSize Div SizeOf (tFileGroup)
  Else
  Begin
    FillChar (Hdr, SizeOf (Hdr), #0);
    Assign (fgFile, BinName);
    ReSet (fgFile, 1);

    If IOResult = 0 Then
      BlockRead (fgFile, Hdr, SizeOf (Hdr));

    If (fTime1 = fTime2) And BinaryHdrValid (Hdr) Then
    Begin
      fGroupsAmount := (fSize2 - SizeOf (Hdr)) Div SizeOf (tFileGroup);

      AllocStream (fgStream, fGroupsAmount * LongInt (SizeOf (tFileGroup)));
      If fgStream <> Nil Then
      Begin
        New (Buf);

        Repeat
          BlockRead (fgFile, Buf^, FileBufSize, i);
          fgStream^. Write (Buf^, i);
        Until (i <> FileBufSize) Or (fgStream^. Status <> stOk);

        Dispose (Buf);

        If fgStream^. Status = stOk Then Close (fgFile)
                                    Else KillStream (fgStream);
      End;
    End Else
    Begin
      ReWrite (fgFile, 1);
      BlockWrite (fgFile, OrigHdr, SizeOf (OrigHdr));
      fGroupsAmount := 0;

      KillIndex (fIndex);
      KillStream (fgStream);
      AllocStream (fgStream, 0);

      If opFileGroups (Cnf. FileGroupsFile) Then
      Begin
        While rFileGroup (R) Do
        Begin
          BlockWrite (fgFile, R, SizeOf (R));
          Inc (fGroupsAmount);

          If fgStream <> Nil Then
          Begin
            fgStream^. Write (R, SizeOf (R));
            If fgStream^. Status <> stOk Then
              KillStream (fgStream);
          End;
        End;

        pFileGroupDone;
      End;

      SetFTime (fgFile, fTime1);
      Close (fgFile);
      If fgStream = Nil Then
        ReSet (fgFile, 1);
    End;
  End;

  fgOpen := True;
End;

Function ReadFileGroup (Var FileGroup: tFileGroup; Num: System. Word): Boolean;
Var
  FPos : LongInt;
  R    : SysInt;

Begin
  FillChar (FileGroup, SizeOf (FileGroup), 0);
  If Num > fGroupsAmount Then
    ReadFileGroup := False
  Else
  Begin
    FPos := LongInt (SizeOf (FileGroup)) * (Num - 1);

    If fgStream <> Nil Then
    Begin
      fgStream^. Seek (FPos);
      fgStream^. Read (FileGroup, SizeOf (FileGroup));
      ReadFileGroup := fgStream^. Status = stOk;
    End Else
    Begin
      Seek (fgFile, SizeOf (tBinaryHdr) + FPos);
      BlockRead (fgFile, FileGroup, SizeOf (FileGroup), R);
      ReadFileGroup := R = SizeOf (FileGroup);
    End;
  End;
End;

Procedure StartFGroupReadSequence (From: Word);
Var
  FPos : LongInt;

Begin
  FGroupSeqNum := From;
  FPos := LongInt (SizeOf (FileGroup)) * From;
  If fgStream <> Nil Then fgStream^. Seek (FPos)
                     Else Seek (fgFile, SizeOf (tBinaryHdr) + FPos);
End;

Function ReadNextFileGroup (Var FileGroup: tFileGroup): Boolean;
Var
  R : SysInt;

Begin
  FillChar (FileGroup, SizeOf (FileGroup), 0);

  Inc (FGroupSeqNum);
  If FGroupSeqNum > fGroupsAmount Then
    ReadNextFileGroup := False
  Else
    If fgStream <> Nil Then
    Begin
      fgStream^. Read (FileGroup, SizeOf (FileGroup));
      ReadNextFileGroup := fgStream^. Status = stOk;
    End Else
    Begin
      BlockRead (fgFile, FileGroup, SizeOf (FileGroup), R);
      ReadNextFileGroup := R = SizeOf (FileGroup);
    End;
End;

Procedure CloseFileGroups;
Begin
  If fgOpen Then
  Begin
    If fgStream = Nil Then
      Close (fgFile);
    fgOpen := False;
  End;
End;

Function OpenFileAreas: Boolean;
Var
  fTime1, fTime2,
  fSize2          : LongInt;
  Buf             : PFileBufArr;
  i               : Integer;
  DirInfo         : SearchRec;
  Hdr             : tBinaryHdr;
  R               : tFileArea;
  BinName         : String;

Begin
  OpenFileAreas := True;
  If faOpen Then
    Exit;

  FindFirst (Cnf. FileAreasFile, AnyFile-VolumeID-Directory-Hidden, DirInfo);
  fTime1 := DirInfo. Time;
{$IFNDEF MSDOS}
  FindClose (DirInfo);
{$ENDIF}

  If DosError <> 0 Then
  Begin
    OpenFileAreas := False;
    Exit;
  End;

  BinName := AddBackSlash (JustPathName (Cnf. FileAreasFile)) + 'filearea' +
    LineExt;

  FindFirst (BinName, AnyFile-VolumeID-Directory-Hidden, DirInfo);
  If DosError = 0 Then
  Begin
    fTime2 := DirInfo. Time;
    fSize2 := DirInfo. Size;
  End
  Else
    fTime2 := -1;

{$IFNDEF MSDOS}
  FindClose (DirInfo);
{$ENDIF}

  If (fTime1 = fTime2) And (faStream <> Nil) Then
    fAreasAmount := faStream^. GetSize Div SizeOf (tFileArea)
  Else
  Begin
    FillChar (Hdr, SizeOf (Hdr), #0);
    Assign (faFile, BinName);
    ReSet (faFile, 1);

    If IOResult = 0 Then
      BlockRead (faFile, Hdr, SizeOf (Hdr));

    If (fTime1 = fTime2) And BinaryHdrValid (Hdr) Then
    Begin
      fAreasAmount := (fSize2 - SizeOf (Hdr)) Div SizeOf (tFileArea);

      AllocStream (faStream, fAreasAmount * LongInt (SizeOf (tFileArea)));
      If faStream <> Nil Then
      Begin
        New (Buf);

        Repeat
          BlockRead (faFile, Buf^, FileBufSize, i);
          faStream^. Write (Buf^, i);
        Until (i <> FileBufSize) Or (faStream^. Status <> stOk);

        Dispose (Buf);

        If faStream^. Status = stOk Then Close (faFile)
                                    Else KillStream (faStream);
      End;
    End Else
    Begin
      ReWrite (faFile, 1);
      BlockWrite (faFile, OrigHdr, SizeOf (OrigHdr));
      fAreasAmount := 0;

      KillIndex (fIndex);
      KillStream (faStream);
      AllocStream (faStream, 0);

      If opFileAreas (Cnf. FileAreasFile) Then
      Begin
        While rFileArea (R) Do
        Begin
          BlockWrite (faFile, R, SizeOf (R));
          Inc (fAreasAmount);

          If faStream <> Nil Then
          Begin
            faStream^. Write (R, SizeOf (R));
            If faStream^. Status <> stOk Then
              KillStream (faStream);
          End;
        End;

        pFileDone;
      End;

      SetFTime (faFile, fTime1);
      Close (faFile);
      If faStream = Nil Then
        ReSet (faFile, 1);
    End;
  End;

  faOpen := True;
End;

Function ReadFileArea (Var FileArea: tFileArea; Num: System. Word): Boolean;
Var
  FPos : LongInt;
  R    : SysInt;

Begin
  FillChar (FileArea, SizeOf (FileArea), #0);

  If Num > fAreasAmount Then
    ReadFileArea := False
  Else
  Begin
    FPos := LongInt (SizeOf (FileArea)) * (Num - 1);

    If faStream <> Nil Then
    Begin
      faStream^. Seek (FPos);
      faStream^. Read (FileArea, SizeOf (FileArea));
      ReadFileArea := faStream^. Status = stOk;
    End Else
    Begin
      Seek (faFile, SizeOf (tBinaryHdr) + FPos);
      BlockRead (faFile, FileArea, SizeOf (FileArea), R);
      ReadFileArea := R = SizeOf (FileArea);
    End;
  End;
End;

Procedure StartFAreaReadSequence (From: Word);
Var
  FPos : LongInt;

Begin
  FAreaSeqNum := From;
  FPos := LongInt (SizeOf (FileArea)) * From;
  If faStream <> Nil Then faStream^. Seek (FPos)
                     Else Seek (faFile, SizeOf (tBinaryHdr) + FPos);
End;

Function ReadNextFileArea (Var FileArea: tFileArea): Boolean;
Var
  R : SysInt;

Begin
  FillChar (FileArea, SizeOf (FileArea), #0);

  Inc (FAreaSeqNum);
  If FAreaSeqNum > fAreasAmount Then
    ReadNextFileArea := False
  Else
    If faStream <> Nil Then
    Begin
      faStream^. Read (FileArea, SizeOf (FileArea));
      ReadNextFileArea := faStream^. Status = stOk;
    End Else
    Begin
      BlockRead (faFile, FileArea, SizeOf (FileArea), R);
      ReadNextFileArea := R = SizeOf (FileArea);
    End;
End;

Procedure CloseFileAreas;
Begin
  If faOpen Then
  Begin
    If faStream = Nil Then
      Close (faFile);
    faOpen := False;
  End;
End;

Procedure SetFileGroup (Num: LongInt);
Var
  i, C        : Word;
  G           : PGroupIndexRec;
  ta          : tFileGroup;
  tFA         : tFileArea;
  AlreadyOpen : Boolean;

Begin
  If Num <= 0 Then
    If R. FileGroup > 0 Then Num := R. FileGroup
                        Else Num := 1;
  R. FileGroup := 0;
  ReInitIndex (fIndex, R. Security, R. Flags);

  AlreadyOpen := fgOpen;
  If AlreadyOpen Or OpenFileGroups Then
  Begin
    If Num <= fGroupsAmount Then
    Begin
      C := fIndex^. Groups. Count;
      If C > 0 Then
      Begin
        If Num <= C Then
        Begin
          R. FileGroup := Num;
          ReadFileGroup (FileGroup, PGroupIndexRec (fIndex^. Groups. At
            (Num - 1))^. PhysNum);
        End;
      End Else
      Begin
        i := 0;
        StartFGroupReadSequence (i);

        While ReadNextFileGroup (ta) Do
        Begin
          Inc (i);

          If (ta. ShowSec <= R. Security) And
             FlagsValid (R. Flags, ta. ShowFlags) Then
          Begin
            New (G);
            G^. PhysNum := i;
            G^. Areas := Nil;
            fIndex^. Groups. Insert (G);
            Inc (C);
            If C = Num Then
            Begin
              R. FileGroup := C;
              FileGroup := ta;
            End;
          End;

          If i = fGroupsAmount Then
            Break;
        End;

        ffGroupsAmount := C;
      End;
    End
    Else
      If fIndex^. Groups. Count > 0 Then
      Begin
        R. FileGroup := 1;
        ReadFileGroup (FileGroup, PGroupIndexRec (fIndex^. Groups. At (0))^.
          PhysNum);
      End Else
      Begin
        StartFGroupReadSequence (0);

        While ReadNextFileGroup (FileGroup) Do
          If (FileGroup. ShowSec <= R. Security) And
             FlagsValid (R. Flags, FileGroup. ShowFlags) Then
          Begin
            R. FileGroup := 1;
            Break;
          End;
      End;

    If Not AlreadyOpen Then
      CloseFileGroups;
  End;

  If R. FileGroup = 0 Then
    FillChar (FileGroup, SizeOf (FileGroup), 0);

  If (R. FileGroup > 0) And (fIndex^. Groups. Count >= R. FileGroup) Then
  Begin
    G := fIndex^. Groups. At (R. FileGroup - 1);
    If G^. Areas = Nil Then
      G^. Areas := New (PLongIntCollection, Init (IndexAreaDelta,
        IndexAreaDelta));
    C := G^. Areas^. Count;
  End Else
  Begin
    G := Nil;
    C := 0;
  End;

  If C = 0 Then
  Begin
    AlreadyOpen := faOpen;
    If AlreadyOpen Or OpenFileAreas Then
    Begin
      i := 0;
      StartFAreaReadSequence (i);

      While ReadNextFileArea (tFA) Do
      Begin
        Inc (i);

        If (tFA. Name <> '') And
           (WordInString (FileGroup. Tag, tFA. Group) Or
           (tFA. Group = '') Or (fGroupsAmount = 0)) And
           (tFA. ShowSec <= R. Security) And
           FlagsValid (R. Flags, tFA. ShowFlags) Then
        Begin
          If G <> Nil Then
            G^. Areas^. Insert (Pointer (LongInt (i)));
          Inc (C);
        End;

        If i = fAreasAmount Then
          Break;
      End;

      If Not AlreadyOpen Then
        CloseFileAreas;
    End;
  End;

  fAreasGroup := C;
End;

Procedure SetFileArea (Num: LongInt);
Var
  i, C        : Word;
  G           : PGroupIndexRec;
  AlreadyOpen : Boolean;

Label
  AreaFound;

Begin
  If Num <= 0 Then
    If R. FileArea > 0 Then Num := R. FileArea
                       Else Num := 1;
  R. FileArea := 0;

  AlreadyOpen := faOpen;
  If AlreadyOpen Or OpenFileAreas Then
  Begin
    ReInitIndex (fIndex, R. Security, R. Flags);

    G := Nil;
    C := 0;

    If (R. FileGroup > 0) And (fIndex^. Groups. Count >= R. FileGroup) Then
    Begin
      G := fIndex^. Groups. At (R. FileGroup - 1);
      If G^. Areas <> Nil Then
        C := G^. Areas^. Count;
    End;

    If Num <= fAreasGroup Then
    Begin
      If C > 0 Then
      Begin
        If Num <= C Then
        Begin
          R. FileArea := Num;
          PhysFileArea := LongInt (G^. Areas^. At (Num - 1));
          ReadFileArea (FileArea, PhysFileArea);
          Goto AreaFound;
        End;
      End Else
      Begin
        i := 0;
        StartFAreaReadSequence (i);

        While ReadNextFileArea (FileArea) Do
        Begin
          Inc (i);

          If (FileArea. Name <> '') And
             (WordInString (FileGroup. Tag, FileArea. Group) Or
             (FileArea. Group = '') Or (fGroupsAmount = 0)) And
             (FileArea. ShowSec <= R. Security) And
             FlagsValid (R. Flags, FileArea. ShowFlags) Then
          Begin
            Inc (C);
            If C = Num Then
            Begin
              R. FileArea := C;
              PhysFileArea := FAreaSeqNum;
              Break;
            End;
          End;

          If i = fAreasAmount Then
            Break;
        End;
      End;
    End
    Else
      If C > 0 Then
      Begin
        R. FileArea := 1;
        PhysFileArea := LongInt (G^. Areas^. At (0));
        ReadFileArea (FileArea, PhysFileArea);
      End Else
      Begin
        StartFAreaReadSequence (0);

        While ReadNextFileArea (FileArea) Do
          If (FileArea. Name <> '') And
             (WordInString (FileGroup. Tag, FileArea. Group) Or
             (FileArea. Group = '') Or (fGroupsAmount = 0)) And
             (FileArea. ShowSec <= R. Security) And
             FlagsValid (R. Flags, FileArea. ShowFlags) Then
          Begin
            R. FileArea := 1;
            PhysFileArea := FAreaSeqNum;
            Break;
          End;
      End;

  AreaFound:
    If Not AlreadyOpen Then
      CloseFileAreas;
  End;

  If R. FileArea = 0 Then
    FillChar (FileArea, SizeOf (FileArea), 0);
End;

Procedure SmartChangeFArea (NewGroup, NewArea: LongInt;
                            Var LastGroup, LastArea: LongInt);
Var
  GroupsAlreadyOpen, AreasAlreadyOpen : Boolean;

Begin
  If NewGroup <> LastGroup Then
  Begin
    LastGroup := NewGroup;
    LastArea := NewArea;

    GroupsAlreadyOpen := fgOpen;
    AreasAlreadyOpen := faOpen;
    If Not GroupsAlreadyOpen Then
      OpenFileGroups;
    If Not AreasAlreadyOpen Then
      OpenFileAreas;

    SetFileGroup (NewGroup);
    SetFileArea (NewArea);

    If Not GroupsAlreadyOpen Then
      CloseFileGroups;
    If Not AreasAlreadyOpen Then
      CloseFileAreas;
  End
  Else
    If NewArea <> LastArea Then
    Begin
      LastArea := NewArea;
      SetFileArea (NewArea);
    End;
End;

Function OpenMsgGroups: Boolean;
Var
  fTime1, fTime2,
  fSize2          : LongInt;
  Buf             : PFileBufArr;
  i               : Integer;
  DirInfo         : SearchRec;
  Hdr             : tBinaryHdr;
  R               : tMsgGroup;
  BinName         : String;

Begin
  OpenMsgGroups := True;
  If mgOpen Then
    Exit;

  FindFirst (Cnf. MsgGroupsFile, AnyFile-VolumeID-Directory-Hidden, DirInfo);
  fTime1 := DirInfo. Time;
{$IFNDEF MSDOS}
  FindClose (DirInfo);
{$ENDIF}

  If DosError <> 0 Then
  Begin
    OpenMsgGroups := False;
    Exit;
  End;

  BinName := AddBackSlash (JustPathName (Cnf. MsgGroupsFile)) + 'mgroups' +
    LineExt;

  FindFirst (BinName, AnyFile-VolumeID-Directory-Hidden, DirInfo);
  If DosError = 0 Then
  Begin
    fTime2 := DirInfo. Time;
    fSize2 := DirInfo. Size;
  End
  Else
    fTime2 := -1;

{$IFNDEF MSDOS}
  FindClose (DirInfo);
{$ENDIF}

  If (fTime1 = fTime2) And (mgStream <> Nil) Then
    mGroupsAmount := mgStream^. GetSize Div SizeOf (tMsgGroup)
  Else
  Begin
    FillChar (Hdr, SizeOf (Hdr), #0);
    Assign (mgFile, BinName);
    ReSet (mgFile, 1);

    If IOResult = 0 Then
      BlockRead (mgFile, Hdr, SizeOf (Hdr));

    If (fTime1 = fTime2) And BinaryHdrValid (Hdr) Then
    Begin
      mGroupsAmount := (fSize2 - SizeOf (Hdr)) Div SizeOf (tMsgGroup);

      AllocStream (mgStream, mGroupsAmount * LongInt (SizeOf (tMsgGroup)));
      If mgStream <> Nil Then
      Begin
        New (Buf);

        Repeat
          BlockRead (mgFile, Buf^, FileBufSize, i);
          mgStream^. Write (Buf^, i);
        Until (i <> FileBufSize) Or (mgStream^. Status <> stOk);

        Dispose (Buf);

        If mgStream^. Status = stOk Then Close (mgFile)
                                    Else KillStream (mgStream);
      End;
    End Else
    Begin
      ReWrite (mgFile, 1);
      BlockWrite (mgFile, OrigHdr, SizeOf (OrigHdr));
      mGroupsAmount := 0;

      KillIndex (mIndex);
      KillStream (mgStream);
      AllocStream (mgStream, 0);

      If opMsgGroups (Cnf. MsgGroupsFile) Then
      Begin
        While rMsgGroup (R) Do
        Begin
          BlockWrite (mgFile, R, SizeOf (R));
          Inc (mGroupsAmount);

          If mgStream <> Nil Then
          Begin
            mgStream^. Write (R, SizeOf (R));
            If mgStream^. Status <> stOk Then
              KillStream (mgStream);
          End;
        End;

        pMsgGroupDone;
      End;

      SetFTime (mgFile, fTime1);
      Close (mgFile);
      If mgStream = Nil Then
        ReSet (mgFile, 1);
    End;
  End;

  mgOpen := True;
End;

Function ReadMsgGroup (Var MsgGroup: tMsgGroup; Num: System. Word): Boolean;
Var
  FPos : LongInt;
  R    : SysInt;

Begin
  FillChar (MsgGroup, SizeOf (MsgGroup), 0);
  If Num > mGroupsAmount Then
    ReadMsgGroup := False
  Else
  Begin
    FPos := LongInt (SizeOf (MsgGroup)) * (Num - 1);

    If mgStream <> Nil Then
    Begin
      mgStream^. Seek (FPos);
      mgStream^. Read (MsgGroup, SizeOf (MsgGroup));
      ReadMsgGroup := mgStream^. Status = stOk;
    End Else
    Begin
      Seek (mgFile, SizeOf (tBinaryHdr) + FPos);
      BlockRead (mgFile, MsgGroup, SizeOf (MsgGroup), R);
      ReadMsgGroup := R = SizeOf (MsgGroup);
    End;
  End;
End;

Procedure StartMGroupReadSequence (From: Word);
Var
  FPos : LongInt;

Begin
  MGroupSeqNum := From;
  FPos := LongInt (SizeOf (MsgGroup)) * From;
  If mgStream <> Nil Then mgStream^. Seek (FPos)
                     Else Seek (mgFile, SizeOf (tBinaryHdr) + FPos);
End;

Function ReadNextMsgGroup (Var MsgGroup: tMsgGroup): Boolean;
Var
  R : SysInt;

Begin
  FillChar (MsgGroup, SizeOf (MsgGroup), 0);

  Inc (MGroupSeqNum);
  If MGroupSeqNum > mGroupsAmount Then
    ReadNextMsgGroup := False
  Else
    If mgStream <> Nil Then
    Begin
      mgStream^. Read (MsgGroup, SizeOf (MsgGroup));
      ReadNextMsgGroup := mgStream^. Status = stOk;
    End Else
    Begin
      BlockRead (mgFile, MsgGroup, SizeOf (MsgGroup), R);
      ReadNextMsgGroup := R = SizeOf (MsgGroup);
    End;
End;

Procedure CloseMsgGroups;
Begin
  If mgOpen Then
  Begin
    If mgStream = Nil Then
      Close (mgFile);
    mgOpen := False;
  End;
End;

Function OpenMsgAreas: Boolean;
Var
  fTime1, fTime2,
  fSize2          : LongInt;
  Buf             : PFileBufArr;
  i               : Integer;
  DirInfo         : SearchRec;
  Hdr             : tBinaryHdr;
  R               : tMsgArea;
  BinName         : String;

Begin
  OpenMsgAreas := True;
  If maOpen Then
    Exit;

  FindFirst (Cnf. MsgAreasFile, AnyFile-VolumeID-Directory-Hidden, DirInfo);
  fTime1 := DirInfo. Time;
{$IFNDEF MSDOS}
  FindClose (DirInfo);
{$ENDIF}

  If DosError <> 0 Then
  Begin
    OpenMsgAreas := False;
    Exit;
  End;

  BinName := AddBackSlash (JustPathName (Cnf. MsgAreasFile)) + 'msgarea' +
    LineExt;

  FindFirst (BinName, AnyFile-VolumeID-Directory-Hidden, DirInfo);
  If DosError = 0 Then
  Begin
    fTime2 := DirInfo. Time;
    fSize2 := DirInfo. Size;
  End
  Else
    fTime2 := -1;

{$IFNDEF MSDOS}
  FindClose (DirInfo);
{$ENDIF}

  If (fTime1 = fTime2) And (maStream <> Nil) Then
    mAreasAmount := maStream^. GetSize Div SizeOf (tMsgArea)
  Else
  Begin
    FillChar (Hdr, SizeOf (Hdr), #0);
    Assign (maFile, BinName);
    ReSet (maFile, 1);

    If IOResult = 0 Then
      BlockRead (maFile, Hdr, SizeOf (Hdr));

    If (fTime1 = fTime2) And BinaryHdrValid (Hdr) Then
    Begin
      mAreasAmount := (fSize2 - SizeOf (Hdr)) Div SizeOf (tMsgArea);

      AllocStream (maStream, mAreasAmount * LongInt (SizeOf (tMsgArea)));
      If maStream <> Nil Then
      Begin
        New (Buf);

        Repeat
          BlockRead (maFile, Buf^, FileBufSize, i);
          maStream^. Write (Buf^, i);
        Until (i <> FileBufSize) Or (maStream^. Status <> stOk);

        Dispose (Buf);

        If maStream^. Status = stOk Then Close (maFile)
                                    Else KillStream (maStream);
      End;
    End Else
    Begin
      ReWrite (maFile, 1);
      BlockWrite (maFile, OrigHdr, SizeOf (OrigHdr));
      mAreasAmount := 0;

      KillIndex (mIndex);
      KillStream (maStream);
      AllocStream (maStream, 0);

      If opMsgAreas (Cnf. MsgAreasFile) Then
      Begin
        While rMsgArea (R) Do
        Begin
          BlockWrite (maFile, R, SizeOf (R));
          Inc (mAreasAmount);

          If maStream <> Nil Then
          Begin
            maStream^. Write (R, SizeOf (R));
            If maStream^. Status <> stOk Then
              KillStream (maStream);
          End;
        End;

        pMsgDone;
      End;

      SetFTime (maFile, fTime1);
      Close (maFile);
      If maStream = Nil Then
        ReSet (maFile, 1);
    End;
  End;

  maOpen := True;
End;

Function ReadMsgArea (Var MsgArea: tMsgArea; Num: System. Word): Boolean;
Var
  FPos : LongInt;
  R    : SysInt;

Begin
  FillChar (MsgArea, SizeOf (MsgArea), #0);
  If Num > mAreasAmount Then
    ReadMsgArea := False
  Else
  Begin
    FPos := LongInt (SizeOf (MsgArea)) * (Num - 1);

    If maStream <> Nil Then
    Begin
      maStream^. Seek (FPos);
      maStream^. Read (MsgArea, SizeOf (MsgArea));
      ReadMsgArea := maStream^. Status = stOk;
    End Else
    Begin
      Seek (maFile, SizeOf (tBinaryHdr) + FPos);
      BlockRead (maFile, MsgArea, SizeOf (MsgArea), R);
      ReadMsgArea := R = SizeOf (MsgArea);
    End;
  End;
End;

Procedure StartMAreaReadSequence (From: Word);
Var
  FPos : LongInt;

Begin
  MAreaSeqNum := From;
  FPos := LongInt (SizeOf (MsgArea)) * From;
  If maStream <> Nil Then maStream^. Seek (FPos)
                     Else Seek (maFile, SizeOf (tBinaryHdr) + FPos);
End;

Function ReadNextMsgArea (Var MsgArea: tMsgArea): Boolean;
Var
  R : SysInt;

Begin
  FillChar (MsgArea, SizeOf (MsgArea), #0);

  Inc (MAreaSeqNum);
  If MAreaSeqNum > mAreasAmount Then
    ReadNextMsgArea := False
  Else
    If maStream <> Nil Then
    Begin
      maStream^. Read (MsgArea, SizeOf (MsgArea));
      ReadNextMsgArea := maStream^. Status = stOk;
    End Else
    Begin
      BlockRead (maFile, MsgArea, SizeOf (MsgArea), R);
      ReadNextMsgArea := R = SizeOf (MsgArea);
    End;
End;

Procedure CloseMsgAreas;
Begin
  If maOpen Then
  Begin
    If maStream = Nil Then
      Close (maFile);
    maOpen := False;
  End;
End;

Procedure SetMsgGroup (Num: LongInt);
Var
  i, C        : Word;
  G           : PGroupIndexRec;
  ta          : tMsgGroup;
  tMA         : tMsgArea;
  AlreadyOpen : Boolean;

Begin
  If Num <= 0 Then
    If R. MsgGroup > 0 Then Num := R. MsgGroup
                       Else Num := 1;
  R. MsgGroup := 0;

  ReInitIndex (mIndex, R. Security, R. Flags);

  AlreadyOpen := mgOpen;
  If AlreadyOpen Or OpenMsgGroups Then
  Begin
    If Num <= mGroupsAmount Then
    Begin
      C := mIndex^. Groups. Count;
      If C > 0 Then
      Begin
        If Num <= C Then
        Begin
          R. MsgGroup := Num;
          ReadMsgGroup (MsgGroup, PGroupIndexRec (mIndex^. Groups. At
            (Num - 1))^. PhysNum);
        End;
      End Else
      Begin
        i := 0;
        StartMGroupReadSequence (i);

        While ReadNextMsgGroup (ta) Do
        Begin
          Inc (i);

          If (ta. ShowSec <= R. Security) And
             FlagsValid (R. Flags, ta. ShowFlags) Then
          Begin
            New (G);
            G^. PhysNum := i;
            G^. Areas := Nil;
            mIndex^. Groups. Insert (G);
            Inc (C);
            If C = Num Then
            Begin
              R. MsgGroup := C;
              MsgGroup := ta;
            End;
          End;

          If i = mGroupsAmount Then
            Break;
        End;

        mmGroupsAmount := C;
      End;
    End
    Else
      If mIndex^. Groups. Count > 0 Then
      Begin
        R. MsgGroup := 1;
        ReadMsgGroup (MsgGroup, PGroupIndexRec (mIndex^. Groups. At (0))^.
          PhysNum);
      End Else
      Begin
        StartMGroupReadSequence (0);

        While ReadNextMsgGroup (MsgGroup) Do
          If (MsgGroup. ShowSec <= R. Security) And
             FlagsValid (R. Flags, MsgGroup. ShowFlags) Then
          Begin
            R. MsgGroup := 1;
            Break;
          End;
      End;

    If Not AlreadyOpen Then
      CloseMsgGroups;
  End;

  If R. MsgGroup = 0 Then
    FillChar (MsgGroup, SizeOf (MsgGroup), 0);

  If (R. MsgGroup > 0) And (mIndex^. Groups. Count >= R. MsgGroup) Then
  Begin
    G := mIndex^. Groups. At (R. MsgGroup - 1);
    If G^. Areas = Nil Then
      G^. Areas := New (PLongIntCollection, Init (IndexAreaDelta,
        IndexAreaDelta));
    C := G^. Areas^. Count;
  End Else
  Begin
    G := Nil;
    C := 0;
  End;

  If C = 0 Then
  Begin
    AlreadyOpen := maOpen;
    If AlreadyOpen Or OpenMsgAreas Then
    Begin
      i := 0;
      StartMAreaReadSequence (i);

      While ReadNextMsgArea (tMA) Do
      Begin
        Inc (i);

        If (tMA. Name <> '') And
           (WordInString (MsgGroup. Tag, tMA. Group) Or
           (tMA. Group = '') Or (mGroupsAmount = 0)) And
           (tMA. ShowSec <= R. Security) And
           FlagsValid (R. Flags, tMA. ShowFlags) Then
        Begin
          If G <> Nil Then
            G^. Areas^. Insert (Pointer (LongInt (i)));
          Inc (C);
        End;

        If i = mAreasAmount Then
          Break;
      End;

      If Not AlreadyOpen Then
        CloseMsgAreas;
    End;
  End;

  mAreasGroup := C;
End;

Procedure SetMsgArea (Num: LongInt);
Var
  i, C        : Word;
  G           : PGroupIndexRec;
  AlreadyOpen : Boolean;

Label
  AreaFound;

Begin
  If Num <= 0 Then
    If R. MsgArea > 0 Then Num := R. MsgArea
                      Else Num := 1;
  R. MsgArea := 0;

  AlreadyOpen := maOpen;
  If AlreadyOpen Or OpenMsgAreas Then
  Begin
    ReInitIndex (mIndex, R. Security, R. Flags);

    G := Nil;
    C := 0;

    If (R. MsgGroup > 0) And (mIndex^. Groups. Count >= R. MsgGroup) Then
    Begin
      G := mIndex^. Groups. At (R. MsgGroup - 1);
      If G^. Areas <> Nil Then
        C := G^. Areas^. Count;
    End;

    If Num <= mAreasGroup Then
    Begin
      If C > 0 Then
      Begin
        If Num <= C Then
        Begin
          R. MsgArea := Num;
          PhysMsgArea := LongInt (G^. Areas^. At (Num - 1));
          ReadMsgArea (MsgArea, PhysMsgArea);
          Goto AreaFound;
        End;
      End Else
      Begin
        i := 0;
        StartMAreaReadSequence (i);

        While ReadNextMsgArea (MsgArea) Do
        Begin
          Inc (i);

          If (MsgArea. Name <> '') And
             (WordInString (MsgGroup. Tag, MsgArea. Group) Or
             (MsgArea. Group = '') Or (mGroupsAmount = 0)) And
             (MsgArea. ShowSec <= R. Security) And
             FlagsValid (R. Flags, MsgArea. ShowFlags) Then
          Begin
            Inc (C);
            If C = Num Then
            Begin
              R. MsgArea := C;
              PhysMsgArea := MAreaSeqNum;
              Break;
            End;
          End;

          If i = mAreasAmount Then
            Break;
        End;
      End;
    End
    Else
      If C > 0 Then
      Begin
        R. MsgArea := 1;
        PhysMsgArea := LongInt (G^. Areas^. At (0));
        ReadMsgArea (MsgArea, PhysMsgArea);
      End Else
      Begin
        StartMAreaReadSequence (0);

        While ReadNextMsgArea (MsgArea) Do
          If (MsgArea. Name <> '') And
             (WordInString (MsgGroup. Tag, MsgArea. Group) Or
             (MsgArea. Group = '') Or (mGroupsAmount = 0)) And
             (MsgArea. ShowSec <= R. Security) And
             FlagsValid (R. Flags, MsgArea. ShowFlags) Then
          Begin
            R. MsgArea := 1;
            PhysMsgArea := MAreaSeqNum;
            Break;
          End;
      End;

  AreaFound:
    If Not AlreadyOpen Then
      CloseMsgAreas;
  End;

  If R. MsgArea = 0 Then
    FillChar (MsgArea, SizeOf (MsgArea), 0);
End;

Procedure SmartChangeMArea (NewGroup, NewArea: LongInt;
                            Var LastGroup, LastArea: LongInt);
Var
  GroupsAlreadyOpen, AreasAlreadyOpen : Boolean;

Begin
  If NewGroup <> LastGroup Then
  Begin
    LastGroup := NewGroup;
    LastArea := NewArea;

    GroupsAlreadyOpen := mgOpen;
    AreasAlreadyOpen := maOpen;
    If Not GroupsAlreadyOpen Then
      OpenMsgGroups;
    If Not AreasAlreadyOpen Then
      OpenMsgAreas;

    SetMsgGroup (NewGroup);
    SetMsgArea (NewArea);

    If Not GroupsAlreadyOpen Then
      CloseMsgGroups;
    If Not AreasAlreadyOpen Then
      CloseMsgAreas;
  End
  Else
    If NewArea <> LastArea Then
    Begin
      LastArea := NewArea;
      SetMsgArea (NewArea);
    End;
End;

Procedure SetDefaultAreas;
Begin
  OpenFileGroups;
  OpenFileAreas;
  SetFileGroup (0);
  SetFileArea (0);
  CloseFileGroups;
  CloseFileAreas;

  OpenMsgGroups;
  OpenMsgAreas;
  SetMsgGroup (0);
  SetMsgArea (0);
  CloseMsgGroups;
  CloseMsgAreas;
End;

End.
