{ Resource cache manager (c) '01 Alexey Kljatow }

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

Unit RCache;

Interface

Uses
  Objects,
  TGlob,
  Crc,
  FCache,
  TMisc;

Procedure OpenResourceCache;
Procedure CloseResourceCache;
Procedure PutBlockResource (Const Name: String; Buffer: Pointer; Len: LongInt);
Procedure PutStreamResource (Const Name: String; S: PStream);
Function GetBlockResource (Const Name: String; Var Len: LongInt): Pointer;

Implementation

Const
  ResCacheBlockBits = 9;
  ResCacheBlockLen  = 1 Shl ResCacheBlockBits;
  ResCacheHeaderLen = 4;
  ResCacheDataLen   = ResCacheBlockLen - ResCacheHeaderLen;
  ValidVersion      = 5;
  RepackThreshold   = 8;
  CacheFName        = 'rescache';
  CacheTmpFName     = 'resc$tmp';
{$IFDEF RealMode}
  ResCacheBlockSize = 1024;
  ResCacheMaxBlocks = 128;
  TmpCacheBlockSize = 1024;
  TmpCacheMaxBlocks = 32;
  EntryTableBits    = 5;
{$ELSE}
  ResCacheBlockSize = 4096;
  ResCacheMaxBlocks = 64;
  TmpCacheBlockSize = 4096;
  TmpCacheMaxBlocks = 16;
  EntryTableBits    = 6;
{$ENDIF}

  ResCache : PCache = Nil;

Type
  PByteArr = ^ByteArr;
  ByteArr = Array [0..65520] Of Byte;

  HeaderRec = Record
    Version, EntriesStartBlock, EntriesCount, Consistency : LongInt;
  End;

  ResEntryRec = Record
    PathnameCSum                   : tCheckSum;
    ModifyTime, StartBlock, ResLen : LongInt;
  End;

  PEntryTableRec = ^EntryTableRec;
  EntryTableRec = Record
    ModifyTime, StartBlock, ResLen : LongInt;
  End;

  GetDataObj = Object
    Data : Pointer;
    Size : LongInt;

    Constructor Init (NewData: Pointer; NewSize: LongInt);
    Destructor Done;

    Function Len : LongInt; Virtual;
    Procedure Get (Var Buffer; Pos, Count: LongInt); Virtual;
  End;

  GetStreamObj = Object (GetDataObj)
    Function Len : LongInt; Virtual;
    Procedure Get (Var Buffer; Pos, Count: LongInt); Virtual;
  End;

  PEntryTable = ^TEntryTable;
  TEntryTable = Object (THashTable)
    Function NewData (P: Pointer): Pointer; Virtual;
    Procedure ReplaceData (Var Old: Pointer; New: Pointer); Virtual;
    Procedure DisposeData (P: Pointer); Virtual;
  End;

Const
  MaxResEntries = ResCacheDataLen Div SizeOf (ResEntryRec);

Type
  PResCacheBlock = ^ResCacheBlock;
  ResCacheBlock = Record
    NextBlock : LongInt;
    Case Byte Of
      0 : (Data       : Array [0..ResCacheDataLen - 1] Of Byte);
      1 : (HeaderData : HeaderRec);
      2 : (EntryData  : Array [0..MaxResEntries - 1] Of ResEntryRec);
  End;

Var
  Header     : HeaderRec;
  EntryTable : PEntryTable;

{ --- Transports --- }

Constructor GetDataObj. Init (NewData: Pointer; NewSize: LongInt);
Begin
  Data := NewData;
  Size := NewSize;
End;

Destructor GetDataObj. Done;
Begin
End;

Function GetDataObj. Len : LongInt;
Begin
  Len := Size;
End;

Procedure GetDataObj. Get (Var Buffer; Pos, Count: LongInt);
Begin
  Move (PByteArr (Data)^ [Pos], Buffer, Count);
End;

Function GetStreamObj. Len : LongInt;
Begin
  Len := PStream (Data)^. GetSize;
End;

Procedure GetStreamObj. Get (Var Buffer; Pos, Count: LongInt);
Begin
  PStream (Data)^. Seek (Pos);
  PStream (Data)^. Read (Buffer, Count);
End;

{ --- Entry table --- }

Function TEntryTable. NewData (P: Pointer): Pointer;
Var
  E : PEntryTableRec;

Begin
  New (E);
  E^ := PEntryTableRec (P)^;
  NewData := E;
End;

Procedure TEntryTable. ReplaceData (Var Old: Pointer; New: Pointer);
Begin
  PEntryTableRec (Old)^ := PEntryTableRec (New)^;
End;

Procedure TEntryTable. DisposeData (P: Pointer);
Begin
  If P <> Nil Then
    Dispose (PEntryTableRec (P));
End;

{ --- ResCache manager --- }

Procedure OpenResourceCache;
Var
  i, j, Entries, Next : LongInt;
  E                   : EntryTableRec;
  Block               : ResCacheBlock;
  CacheName           : String;

Begin
  If ResCache <> Nil Then
    Exit;

  ResCache := NewCache (ResCacheBlockSize, ResCacheMaxBlocks);
  EntryTable := New (PEntryTable, Init (EntryTableBits));
  CacheName := Cnf. Path + CacheFName + LineExt;

  If ResCache^. Open (CacheName, fmReadWrite + fmDenyWrite, 1) Then
  Begin
    If (ResCache^. Read (Block, SizeOf (Block)) = SizeOf (Block)) Then
      If Block. HeaderData. Version = ValidVersion Then
      Begin
        Move (Block. HeaderData, Header, SizeOf (Header));
        Entries := Header. EntriesCount;
        Next := Header. EntriesStartBlock;

        While (Entries > 0) And (Next > 0) Do
        Begin
          ResCache^. Seek (Next Shl ResCacheBlockBits);
          ResCache^. Read (Block, SizeOf (Block));
          If MaxResEntries < Entries Then j := MaxResEntries
                                     Else j := Entries;
          Dec (Entries, j);

          For i := 0 To j-1 Do
            With Block. EntryData [i] Do
            Begin
              E. ModifyTime := ModifyTime;
              E. StartBlock := StartBlock;
              E. ResLen := ResLen;
              EntryTable^. InsertByChecksum (PathnameCSum, @E);
            End;

          Next := Block. NextBlock;
        End;

        Exit;
      End;

    ResCache^. Kill;
  End;

  If ResCache^. Create (CacheName, fmReadWrite + fmDenyWrite, 1) Then
  Begin
    Block. NextBlock := -1;
    FillChar (Block. Data, SizeOf (Block. Data), 0);
    Header. Version := ValidVersion;
    Header. EntriesStartBlock := 1;
    Header. EntriesCount := 0;
    Header. Consistency := 0;
    Move (Header, Block. HeaderData, SizeOf (Header));
    ResCache^. Write (Block, SizeOf (Block));
    FillChar (Block. Data, SizeOf (Block. Data), 0);
    ResCache^. Write (Block, SizeOf (Block));
  End Else
  Begin
    Dispose (EntryTable, Done);
    DisposeCache (ResCache);
  End;
End;

Procedure RepackResourceCache;
Var
  TmpCache           : PCache;
  i, j, k, Next,
  Next1, LastBlock   : LongInt;
  Block, Block1      : ResCacheBlock;
  CacheName, TmpName : String;
  F                  : File;

Begin
  CacheName := Cnf. Path + CacheFName + LineExt;
  TmpName := Cnf. Path + CacheTmpFName + LineExt;

  TmpCache := NewCache (TmpCacheBlockSize, TmpCacheMaxBlocks);
  If Not TmpCache^. Create (TmpName, fmReadWrite + fmDenyWrite, 1) Then
  Begin
    DisposeCache (TmpCache);
    Exit;
  End;

  ResCache^. Seek (0);
  ResCache^. Read (Block, SizeOf (Block));
  With Block. HeaderData Do
  Begin
    Next := EntriesStartBlock;
    EntriesStartBlock := 1;
    Consistency := 0;
  End;
  TmpCache^. Write (Block, SizeOf (Block));
  Move (Block. HeaderData, Header, SizeOf (Header));
  i := 1;

  While Next > 0 Do
  Begin
    Inc (i);
    ResCache^. Seek (Next Shl ResCacheBlockBits);
    ResCache^. Read (Block, SizeOf (Block));
    Next := Block. NextBlock;
    If Next > 0 Then Block. NextBlock := i
                Else Block. NextBlock := -1;
    TmpCache^. Write (Block, SizeOf (Block));
  End;

  LastBlock := i;
  Next := Header. EntriesStartBlock;
  j := Header. EntriesCount;

  While (j > 0) And (Next > 0) Do
  Begin
    TmpCache^. Seek (Next Shl ResCacheBlockBits);
    TmpCache^. Read (Block, SizeOf (Block));
    TmpCache^. Seek (TmpCache^. Size);
    If MaxResEntries < j Then i := MaxResEntries
                         Else i := j;
    Dec (j, i);

    For k := 0 To i-1 Do
      With Block. EntryData [k] Do
      Begin
        Next1 := StartBlock;
        StartBlock := LastBlock;

        While Next1 > 0 Do
        Begin
          Inc (LastBlock);
          ResCache^. Seek (Next1 Shl ResCacheBlockBits);
          ResCache^. Read (Block1, SizeOf (Block1));
          Next1 := Block1. NextBlock;
          If Next1 > 0 Then Block1. NextBlock := LastBlock
                       Else Block1. NextBlock := -1;
          TmpCache^. Write (Block1, SizeOf (Block1));
        End;
      End;

    TmpCache^. Seek (Next Shl ResCacheBlockBits);
    TmpCache^. Write (Block, SizeOf (Block));
    Next := Block. NextBlock;
  End;

  DisposeCache (TmpCache);
  ResCache^. Kill;

  Assign (F, TmpName);
  Rename (F, CacheName);
  If IOResult <> 0 Then;
End;

Procedure CloseResourceCache;
Begin
  If ResCache <> Nil Then
  Begin
    Dispose (EntryTable, Done);

    If Header. Consistency >= RepackThreshold Then
      RepackResourceCache;

    DisposeCache (ResCache);
  End;
End;

Procedure PutResource (Const Name: String; Var Transport: GetDataObj);
Var
  i, j, Len, Entries, NextB, CurrB : LongInt;
  E                                : EntryTableRec;
  Block                            : ResCacheBlock;
  CSum                             : tCheckSum;

Begin
  GetChecksum (UpString (Name), CSum);
  Len := Transport. Len;
  Entries := Header. EntriesCount;
  CurrB := Header. EntriesStartBlock;
  NextB := CurrB;
  i := 0;

  While (Entries > 0) And (NextB > 0) Do
  Begin
    CurrB := NextB;
    ResCache^. Seek (NextB Shl ResCacheBlockBits);
    ResCache^. Read (Block, SizeOf (Block));
    If MaxResEntries < Entries Then j := MaxResEntries
                               Else j := Entries;
    Dec (Entries, j);
    i := 0;

    While i < j Do
    Begin
      With Block. EntryData [i] Do
        If (CSum. CRC = PathnameCSum. CRC) And
           (CSum. Sum = PathnameCSum. Sum) Then
        Begin
          ModifyTime := FileDate (Name);
          ResLen := Len;

          E. ModifyTime := ModifyTime;
          E. StartBlock := StartBlock;
          E. ResLen := Len;
          EntryTable^. InsertByChecksum (CSum, @E);

          ResCache^. Seek (NextB Shl ResCacheBlockBits);
          ResCache^. Write (Block, SizeOf (Block));
          NextB := StartBlock;
          j := 0;

          While j < Len Do
          Begin
            i := Len - j;
            If i > ResCacheDataLen Then
              i := ResCacheDataLen;

            NextB := NextB Shl ResCacheBlockBits;
            ResCache^. Seek (NextB);
            ResCache^. Read (Block, ResCacheHeaderLen);
            ResCache^. Seek (NextB);
            NextB := Block. NextBlock;
            Transport. Get (Block. Data, j, i);
            Inc (j, i);

            If j < Len Then
            Begin
              If NextB <= 0 Then
              Begin
                i := ResCache^. Size;
                Block. NextBlock := i Shr ResCacheBlockBits;
                ResCache^. Write (Block, SizeOf (Block));

                If ResCache^. Pos <> i Then
                Begin
                  Inc (Header. Consistency);
                  ResCache^. Seek (ResCacheHeaderLen);
                  ResCache^. Write (Header, SizeOf (Header));
                  ResCache^. Seek (Block. NextBlock Shl ResCacheBlockBits);
                End;

                Repeat
                  i := Len - j;
                  If i > ResCacheDataLen Then
                    i := ResCacheDataLen;

                  Transport. Get (Block. Data, j, i);
                  Inc (j, i);

                  If j < Len Then Inc (Block. NextBlock)
                             Else Block. NextBlock := -1;
                  ResCache^. Write (Block, SizeOf (Block));
                Until j = Len;

                Exit;
              End;
            End
            Else
              Block. NextBlock := -1;

            ResCache^. Write (Block, SizeOf (Block));
          End;

          If NextB > 0 Then
          Begin
            Repeat
              Inc (Header. Consistency);
              ResCache^. Seek (NextB Shl ResCacheBlockBits);
              ResCache^. Read (Block, ResCacheHeaderLen);
              NextB := Block. NextBlock;
            Until NextB <= 0;

            ResCache^. Seek (ResCacheHeaderLen);
            ResCache^. Write (Header, SizeOf (Header));
          End;

          Exit;
        End;

      Inc (i);
    End;

    NextB := Block. NextBlock;
  End;

  Inc (Header. EntriesCount);
  NextB := ResCache^. Size Shr ResCacheBlockBits;
  ResCache^. Seek (CurrB Shl ResCacheBlockBits);

  If i = MaxResEntries Then
  Begin
    If CurrB <> NextB - 1 Then
      Inc (Header. Consistency);

    CurrB := NextB;
    Inc (NextB);
    ResCache^. Write (CurrB, SizeOf (CurrB));
    Block. NextBlock := -1;
    FillChar (Block. Data, SizeOf (Block. Data), 0);
    i := 0;
  End
  Else
    ResCache^. Read (Block, SizeOf (Block));

  With Block. EntryData [i] Do
  Begin
    PathnameCSum := CSum;
    ModifyTime := FileDate (Name);
    StartBlock := NextB;
    ResLen := Len;

    E. ModifyTime := ModifyTime;
    E. StartBlock := NextB;
    E. ResLen := Len;
    EntryTable^. InsertByChecksum (CSum, @E);
  End;

  ResCache^. Seek (ResCacheHeaderLen);
  ResCache^. Write (Header, SizeOf (Header));

  ResCache^. Seek (CurrB Shl ResCacheBlockBits);
  ResCache^. Write (Block, SizeOf (Block));

  ResCache^. Seek (NextB Shl ResCacheBlockBits);
  Block. NextBlock := NextB;
  j := 0;

  While j < Len Do
  Begin
    i := Len - j;
    If i > ResCacheDataLen Then
      i := ResCacheDataLen;

    Transport. Get (Block. Data, j, i);
    Inc (j, i);

    If j < Len Then Inc (Block. NextBlock)
               Else Block. NextBlock := -1;
    ResCache^. Write (Block, SizeOf (Block));
  End;
End;

Procedure PutBlockResource (Const Name: String; Buffer: Pointer; Len: LongInt);
Var
  Block : GetDataObj;

Begin
  If ResCache <> Nil Then
  Begin
    Block. Init (Buffer, Len);
    PutResource (Name, Block);
    Block. Done;
  End;
End;

Procedure PutStreamResource (Const Name: String; S: PStream);
Var
  Stream : GetStreamObj;

Begin
  If ResCache <> Nil Then
  Begin
    Stream. Init (S, 0);
    PutResource (Name, Stream);
    Stream. Done;
  End;
End;

Function GetBlockResource (Const Name: String; Var Len: LongInt): Pointer;
Var
  i, j, Next : LongInt;
  Entry      : PEntryTableRec;
  Buf        : PByteArr;
  Block      : ResCacheBlock;

Begin
  GetBlockResource := Nil;
  Len := 0;

  If ResCache <> Nil Then
  Begin
    Entry := EntryTable^. Search (UpString (Name));

    If Entry <> Nil Then
      If FileDate (Name) = Entry^. ModifyTime Then
      Begin
      {$IFNDEF VirtualPascal}
        If Entry^. ResLen > 65520 Then
          Exit;
      {$ENDIF}

        Buf := Nil;
        GetMem (Buf, Entry^. ResLen);
        If Buf = Nil Then
          Exit;

        Len := Entry^. ResLen;
        Next := Entry^. StartBlock;
        j := 0;

        While (j < Len) And (Next > 0) Do
        Begin
          ResCache^. Seek (Next Shl ResCacheBlockBits);
          ResCache^. Read (Block, SizeOf (Block));
          i := Len - j;
          If i > ResCacheDataLen Then
            i := ResCacheDataLen;

          Move (Block. Data, Buf^ [j], i);
          Inc (j, i);
          Next := Block. NextBlock;
        End;

        GetBlockResource := Buf;
      End;
  End;
End;

End.
