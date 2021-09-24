{$IFDEF MSDOS}
  {$IFNDEF DPMI32}
    {$F+}
  {$IFNDEF DPMI}
    {$DEFINE RealMode}
  {$ENDIF}
  {$ENDIF}
{$ENDIF}
Unit MFind;

Interface

Uses
  DOS,
  tMisc;

Type
  mSearchRec = Record
    mfPathNum, mfCountPath              : Integer;
    mfAttr                              : Word;
    mfAllPath, mfCurrPath, mfMask, Name : String;
    Info                                : SearchRec;
  End;

Procedure mFindFirst (Const Path, Mask: String; Attr: Word; Var F: mSearchRec);
Procedure mFindNext (Var F: mSearchRec);
Procedure mFindDone (Var F: mSearchRec);
Function mFileExist (Const Path, Mask: String): String;

Procedure mFindFillBuffer (Const Path: String; Attr: Word);
Procedure mFindSearchInBuffer (Const Name: String; Var F: mSearchRec);
Procedure mFindDoneBuffer;

Implementation

Uses
{$IFDEF RealMode}
  Streams,
{$ENDIF}
  Objects,
  TGlob;

Type
  SmallSearchRec = Record
    Time   : LongInt;
    Size   : LongInt;
    Attr   : Byte;
    DirNum : Byte;
  End;

Const
{$IFDEF RealMode}
  BufferTableBits = 6;
{$ELSE}
  BufferTableBits = 8;
{$ENDIF}

Var
  BufferTable  : PHashTable;
  BufferStream : PStream;
  BufferDirs   : PNotSortedCollection;

Procedure mFindFirst (Const Path, Mask: String; Attr: Word; Var F: mSearchRec);
Begin
  With F Do
  Begin
    mfAllPath := Path;
    mfMask := Mask;
    mfAttr := Attr;
    mfCountPath := WordCount (mfAllPath, [#255]);
    mfPathNum := 0;
    DOSerror := 2;

    Repeat
      Inc (mfPathNum);
      If mfPathNum > mfCountPath Then
      Begin
        Name := '';
        Exit;
      End;

      mfCurrPath := AddBackSlash (Trim (ExtractWord (mfPathNum, mfAllPath,
        [#255])));

      FindFirst (mfCurrPath + mfMask, mfAttr, Info);
    Until DOSerror = 0;

    Name := mfCurrPath + Info. Name;
  End;
End;

Procedure mFindNext (Var F: mSearchRec);
Begin
  With F Do
  Begin
    FindNext (Info);

    While DOSerror <> 0 Do
    Begin
      Inc (mfPathNum);
      If mfPathNum > mfCountPath Then
      Begin
        Name := '';
        Exit;
      End;

      mfCurrPath := AddBackSlash (Trim (ExtractWord (mfPathNum, mfAllPath,
        [#255])));

      FindFirst (mfCurrPath + mfMask, mfAttr, Info);
    End;

    Name := mfCurrPath + Info. Name;
  End;
End;

Procedure mFindDone (Var F: mSearchRec);
Begin
{$IFNDEF MSDOS}
  FindClose (F. Info);
{$ENDIF}
End;

Function mFileExist (Const Path, Mask: String): String;
Var
  SR : mSearchRec;

Begin
  mFindFirst (Path, Mask, AnyFile-VolumeID-Directory, SR);
  mFileExist := SR. Name;
  mFindDone (SR);
End;

Procedure mFindFillBuffer (Const Path: String; Attr: Word);
Var
  i, j     : Integer;
  SPos     : LongInt;
  SSR      : SmallSearchRec;
  Info     : SearchRec;
  StreamOK : Boolean;

Begin
  BufferTable := New (PHashTable, Init (BufferTableBits));
  j := WordCount (Path, [#255]);
  BufferDirs := New (PNotSortedCollection, Init (j, 1));

  For i := 1 To j Do
    BufferDirs^. Insert (NewStr (AddBackSlash (Trim (ExtractWord (i, Path,
      [#255])))));

{$IFDEF RealMode}
  StreamOK := False;

  BufferStream := New (PXMSStream, Init (0, 0));
  If BufferStream <> Nil Then
    If BufferStream^. Status <> stOk Then
      Dispose (BufferStream, Done)
    Else
      StreamOK := True;

  If Not StreamOK Then
  Begin
    BufferStream := New (PEMSStream3, Init (0, 0));
    If BufferStream <> Nil Then
      If BufferStream^. Status <> stOk Then
        Dispose (BufferStream, Done)
      Else
        StreamOK := True;
  End;

  If Not StreamOK Then
{$ENDIF}
    BufferStream := New (PMemoryStream, Init (0, 0));

  SPos := 1;

  For i := 0 To j - 1 Do
  Begin
    FindFirst (PString (BufferDirs^. At (i))^ + AllFilesMask, Attr, Info);
    SSR. DirNum := i;

    While DOSerror = 0 Do
    Begin
      SSR. Time := Info. Time;
      SSR. Size := Info. Size;
      SSR. Attr := Info. Attr;
      BufferStream^. Write (SSR, SizeOf (SSR));
      BufferTable^. Insert (UpString (Info. Name), Pointer (SPos));
      Inc (SPos, SizeOf (SSR));
      FindNext (Info);
    End;
  End;

{$IFNDEF MSDOS}
  FindClose (Info);
{$ENDIF}
End;

Procedure mFindSearchInBuffer (Const Name: String; Var F: mSearchRec);
Var
  SPos : LongInt;
  SSR  : SmallSearchRec;
  S    : String;

Begin
  S := UpString (Name);
  SPos := LongInt (BufferTable^. Search (S));

  If SPos > 0 Then
  Begin
    BufferStream^. Seek (SPos - 1);
    BufferStream^. Read (SSR, SizeOf (SSR));

    With F Do
    Begin
      Info. Time := SSR. Time;
      Info. Size := SSR. Size;
      Info. Attr := SSR. Attr;
      Info. Name := S;
      Name := PString (BufferDirs^. At (SSR. DirNum))^ + S;
    End;

    DOSerror := 0;
  End Else
  Begin
    F. Name := '';
    DOSerror := 2;
  End;
End;

Procedure mFindDoneBuffer;
Begin
  Dispose (BufferStream, Done);
  Dispose (BufferDirs, Done);
  Dispose (BufferTable, Done);
End;

End.
