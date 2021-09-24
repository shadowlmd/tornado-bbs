{ Block-based file caching object (c) '01 Alexey Kljatow }

{$IFDEF MSDOS}
  {$IFNDEF DPMI32}
    {$F+}
  {$IFNDEF DPMI}
    {$DEFINE RealMode}
  {$ENDIF}
  {$ENDIF}
{$ENDIF}
{$I-}
{&Use32+}
{&Delphi+}

Unit FCache;

Interface

Uses
{$IFDEF RealMode}
  Streams,
{$ENDIF}
  Objects,
  TGlob;

Const
  MaxBlockSize = 65520;

  fmReadOnly   = 0;          { FileMode constants }
  fmWriteOnly  = 1;
  fmReadWrite  = 2;
  fmDenyAll    = 16;
  fmDenyWrite  = 32;
  fmDenyRead   = 48;
  fmDenyNone   = 64;
  fmNoInherit  = 128;

Type
  PByteArr = ^ByteArr;
  ByteArr = Array [0..MaxBlockSize-1] Of Byte;

  PCacheRec = ^CacheRec;
  CacheRec = Record
    FileBlock  : LongInt;
    StreamPos  : LongInt;
    TimesUsed  : LongInt;
    LastAccess : LongInt;
    UsedLen    : Word;
    Modified   : Boolean;
  End;

  PCacheBlocks = ^TCacheBlocks;
  TCacheBlocks = Object (TSortedCollection)
    Function Compare (Key1, Key2: Pointer): Integer; Virtual;
    Procedure FreeItem (Item: Pointer); Virtual;
  End;

  PCache = ^TCache;                      { Base object - no any caching, just }
  TCache = Object                        { passthru to pascal file functions. }
     F      : File;                      { Used if TRealCache failed to init. }
     FError : Integer;

     Constructor Init;
     Destructor Done; Virtual;
     Function Open (Const FName: String; FMode, NewRecSize: Word): Boolean; Virtual;
     Function Create (Const FName: String; FMode, NewRecSize: Word): Boolean; Virtual;
     Procedure Close; Virtual;
     Procedure Kill; Virtual;
     Function Read (Var Buf; ReadNum: Word): Word; Virtual;
     Function Write (Var Buf; WriteNum: Word): Boolean; Virtual;
     Function Seek (NewPos: LongInt): Boolean; Virtual;
     Function Size: LongInt; Virtual;
     Function Pos: LongInt; Virtual;
     Function EoF: Boolean; Virtual;
     Function Error: Integer;
  End;

  PRealCache = ^TRealCache;              { Real caching object }
  TRealCache = Object (TCache)
     DataStream                      : PStream;
     Blocks                          : PCacheBlocks;
     MemBlock                        : PByteArr;
     FSize, FPos, PhysSize,
     PhysPos, AccessCounter, MaxUsed : LongInt;
     BlockSize, MaxBlocks, RecSize   : Word;

     Constructor Init (NewBlockSize, NewMaxBlocks: Word);
     Destructor Done; Virtual;
     Function Open (Const FName: String; FMode, NewRecSize: Word): Boolean; Virtual;
     Function Create (Const FName: String; FMode, NewRecSize: Word): Boolean; Virtual;
     Procedure Close; Virtual;
     Procedure Kill; Virtual;
     Function Read (Var Buf; ReadNum: Word): Word; Virtual;
     Function Write (Var Buf; WriteNum: Word): Boolean; Virtual;
     Function Seek (NewPos: LongInt): Boolean; Virtual;
     Function Size: LongInt; Virtual;
     Function Pos: LongInt; Virtual;
     Function EoF: Boolean; Virtual;

  Private
     Function FindWorstBlock: Integer;
     Procedure PhysRead (NewPos: LongInt; Var Buf; Count: Word; Var Result: Word);
     Procedure PhysWrite (NewPos: LongInt; Var Buf; Count: Word; Var Result: Word);
  End;

Function NewCache (BlockSize, MaxBlocks: Word): PCache;
Procedure DisposeCache (Var PC: PCache);
Procedure DisposeAllCaches;

Implementation

Const
{$IFDEF RealMode}
  CacheBlocksDelta = 16;
{$ELSE}
  CacheBlocksDelta = 64;
{$ENDIF}

Const
  CacheList : PCollection = Nil;

{ --- TCacheBlocks --- }

Function TCacheBlocks. Compare (Key1, Key2: Pointer): Integer;
{$IFNDEF VirtualPascal}
Var
  Result : LongInt;
{$ENDIF}

Begin
  Result := PCacheRec (Key1)^. FileBlock - PCacheRec (Key2)^. FileBlock;

{$IFNDEF VirtualPascal}
  If Result < 0 Then
    Compare := -1
  Else
    If Result > 0 Then
      Compare := 1
    Else
      Compare := 0;
{$ENDIF}
End;

Procedure TCacheBlocks. FreeItem (Item: Pointer);
Begin
  Dispose (PCacheRec (Item));
End;

{ --- TCache --- }

Constructor TCache. Init;
Begin
End;

Destructor TCache. Done;
Begin
  Close;
End;

Function TCache. Open (Const FName: String; FMode, NewRecSize: Word): Boolean;
Var
  OldMode : Word;

Begin
  Assign (F, FName);

  OldMode := FileMode;
  FileMode := FMode;
  ReSet (F, NewRecSize);
  FileMode := OldMode;

  FError := IOResult;
  Open := FError = 0;
End;

Function TCache. Create (Const FName: String; FMode, NewRecSize: Word): Boolean;
Var
  OldMode : Word;

Begin
  Assign (F, FName);

  OldMode := FileMode;
  FileMode := FMode;
  ReWrite (F, NewRecSize);
  FileMode := OldMode;

  FError := IOResult;
  Create := FError = 0;
End;

Procedure TCache. Close;
Begin
  System. Close (F);
  FError := IOResult;
End;

Procedure TCache. Kill;
Begin
  System. Close (F);
  System. Erase (F);
  FError := IOResult;
End;

Function TCache. Read (Var Buf; ReadNum: Word): Word;
Var
  ReadCount : Word;

Begin
  BlockRead (F, Buf, ReadNum, ReadCount);
  FError := IOResult;
  Read := ReadCount;
End;

Function TCache. Write (Var Buf; WriteNum: Word): Boolean;
Var
  WriteCount : Word;

Begin
  BlockWrite (F, Buf, WriteNum, WriteCount);
  FError := IOResult;
  Write := WriteCount = WriteNum;
End;

Function TCache. Seek (NewPos: LongInt): Boolean;
Begin
  System. Seek (F, NewPos);
  FError := IOResult;
  Seek := Not Boolean (FError);
End;

Function TCache. Size: LongInt;
Begin
  Size := FileSize (F);
End;

Function TCache. Pos: LongInt;
Begin
  Pos := FilePos (F);
End;

Function TCache. EoF: Boolean;
Begin
  EoF := System. EoF (F);
End;

Function TCache. Error: Integer;
Begin
  Error := FError;
  FError := 0;
End;

{ --- TRealCache --- }

Constructor TRealCache. Init (NewBlockSize, NewMaxBlocks: Word);
Var
  Success : Boolean;

Begin
  Success := False;

{$IFDEF RealMode}
  DataStream := New (PEMSStream3, Init (0, 0));
  If DataStream <> Nil Then
    If DataStream^. Status <> stOk Then
      Dispose (DataStream, Done)
    Else
      Success := True;

  If Not Success Then
  Begin
    DataStream := New (PXMSStream, Init (0, 0));
    If DataStream <> Nil Then
      If DataStream^. Status <> stOk Then
        Dispose (DataStream, Done)
      Else
        Success := True;
  End;
{$ELSE}
  DataStream := New (PMemoryStream, Init (0, 0));
  If DataStream <> Nil Then
    If DataStream^. Status <> stOk Then
      Dispose (DataStream, Done)
    Else
      Success := True;
{$ENDIF}

  If Not Success Then
    Fail;

  BlockSize := NewBlockSize;
  MaxBlocks := NewMaxBlocks;
  If NewMaxBlocks > CacheBlocksDelta Then
    NewMaxBlocks := CacheBlocksDelta;

  GetMem (MemBlock, BlockSize);
  Blocks := New (PCacheBlocks, Init (NewMaxBlocks, CacheBlocksDelta));
End;

Destructor TRealCache. Done;
Begin
  Close;
  Dispose (Blocks, Done);
  FreeMem (MemBlock, BlockSize);
  Dispose (DataStream, Done);
End;

Function TRealCache. Open (Const FName: String; FMode, NewRecSize: Word): Boolean;
Var
  OldMode : Word;

Begin
  Assign (F, FName);

  OldMode := FileMode;
  FileMode := FMode;
  ReSet (F, 1);
  FileMode := OldMode;

  FError := IOResult;
  If FError = 0 Then
  Begin
    RecSize := NewRecSize;
    PhysSize := FileSize (F);
    FSize := PhysSize;
    PhysPos := 0;
    FPos := 0;
    AccessCounter := 0;
    MaxUsed := 0;
    Open := True;
  End
  Else
    Open := False;
End;

Function TRealCache. Create (Const FName: String; FMode, NewRecSize: Word): Boolean;
Var
  OldMode : Word;

Begin
  Assign (F, FName);

  OldMode := FileMode;
  FileMode := FMode;
  ReWrite (F, 1);
  FileMode := OldMode;

  FError := IOResult;
  If FError = 0 Then
  Begin
    RecSize := NewRecSize;
    PhysSize := 0;
    PhysPos := 0;
    FSize := 0;
    FPos := 0;
    AccessCounter := 0;
    MaxUsed := 0;
    Create := True;
  End
  Else
    Create := False;
End;

Procedure TRealCache. Close;
Var
  i   : Integer;
  PCR : PCacheRec;

Begin
  FError := 0;

  For i := 0 To Blocks^. Count-1 Do       { write modified blocks & free all }
  Begin
    PCR := PCacheRec (Blocks^. At (i));
    With PCR^ Do
      If Modified Then
      Begin
        DataStream^. Seek (StreamPos);
        DataStream^. Read (MemBlock^, UsedLen);
        PhysWrite (FileBlock * BlockSize, MemBlock^, UsedLen, UsedLen);
      End;

    Blocks^. FreeItem (PCR);
  End;

  Blocks^. DeleteAll;
  DataStream^. Seek (0);
  DataStream^. Truncate;
  System. Close (F);
  If FError = 0 Then
    FError := IOResult;
End;

Procedure TRealCache. Kill;
Begin
  Blocks^. FreeAll;
  DataStream^. Seek (0);
  DataStream^. Truncate;
  System. Close (F);
  System. Erase (F);
  FError := IOResult;
End;

Function TRealCache. FindWorstBlock: Integer;
Const
  Precision = 1000;

Var
  LastWeight, W : Word;
  i             : Integer;
{$IFDEF VirtualPascal}
  AW            : Extended;
{$ELSE}
  AW            : Real;
  Result        : Integer;
{$ENDIF}

Begin                                             { search best to replace }
  Result := 0;

  If (AccessCounter > 0) And (MaxUsed > 0) Then
  Begin
    LastWeight := 65535;

    For i := 0 To Blocks^. Count-1 Do
      With PCacheRec (Blocks^. At (i))^ Do
        If Not (Modified And (FileBlock * BlockSize > PhysSize)) Then
        Begin
          AW := LastAccess / AccessCounter;
          W := Round ((AW * 3 + (TimesUsed / MaxUsed) * ((1 - AW) * 1.5 + 1)) *
            Precision);
          If W < LastWeight Then
          Begin
            LastWeight := W;
            Result := i;
          End;
        End;
  End;

{$IFNDEF VirtualPascal}
  FindWorstBlock := Result;
{$ENDIF}
End;

Function TRealCache. Read (Var Buf; ReadNum: Word): Word;
Var
  ByteBuf                       : ByteArr Absolute Buf;
  i, j, StartBlock, EndBlock,
  BlockPos, EndOfs, CurrEnd     : LongInt;
  PCR                           : PCacheRec;
  CurrStart, CurrLen, ReadCount : Word;
  BlockNum                      : Integer;
  CR                            : CacheRec;

Begin
  StartBlock := FPos Div BlockSize;
  BlockPos := StartBlock * BlockSize;
  CurrStart := FPos - BlockPos;
  EndOfs := FPos + ReadNum * RecSize;
  If EndOfs > FSize Then
    EndOfs := FSize;
  Dec (EndOfs);
  EndBlock := EndOfs Div BlockSize;
  CurrEnd := BlockSize - 1;
  FError := 0;
  ReadCount := 0;

  For i := StartBlock To EndBlock Do
  Begin
    If i = EndBlock Then
      CurrEnd := EndOfs - BlockPos;
    CurrLen := CurrEnd + 1 - CurrStart;

    CR. FileBlock := i;
    If Blocks^. Search (@CR, BlockNum) Then         { cache hit }
      With PCacheRec (Blocks^. At (BlockNum))^ Do
      Begin
        j := UsedLen - CurrStart;
        If j < CurrLen Then
          If j > 0 Then CurrLen := j
                   Else CurrLen := 0;
        If CurrLen > 0 Then
        Begin
          Inc (TimesUsed);
          If TimesUsed > MaxUsed Then
            MaxUsed := TimesUsed;
          LastAccess := AccessCounter;
          DataStream^. Seek (StreamPos + CurrStart);
          DataStream^. Read (ByteBuf [ReadCount], CurrLen);
        End;
      End
    Else                                            { cache miss }
      If Blocks^. Count < MaxBlocks Then            { add new block }
      Begin
        PhysRead (BlockPos, MemBlock^, BlockSize, CR. UsedLen);
        j := CR. UsedLen - CurrStart;
        If j < CurrLen Then
          If j > 0 Then CurrLen := j
                   Else CurrLen := 0;
        If CurrLen > 0 Then
        Begin
          CR. StreamPos := DataStream^. GetSize;
          DataStream^. Seek (CR. StreamPos);
          DataStream^. Write (MemBlock^, BlockSize);
          If DataStream^. Status = stOk Then
          Begin
            CR. TimesUsed := 1;
            CR. LastAccess := AccessCounter;
            CR. Modified := False;
            New (PCR);
            PCR^ := CR;
            Blocks^. AtInsert (BlockNum, PCR);
          End Else
          Begin
            DataStream^. Reset;
            MaxBlocks := Blocks^. Count;
          End;
          Move (MemBlock^ [CurrStart], ByteBuf [ReadCount], CurrLen);
        End;
      End Else
      Begin                                         { replace old block }
        BlockNum := FindWorstBlock;
        PCR := Blocks^. At (BlockNum);

        With PCR^ Do
        Begin
          If Modified Then
          Begin
            DataStream^. Seek (StreamPos);
            DataStream^. Read (MemBlock^, UsedLen);
            PhysWrite (FileBlock * BlockSize, MemBlock^, UsedLen, UsedLen);
          End;

          PhysRead (BlockPos, MemBlock^, BlockSize, UsedLen);
          FileBlock := i;
          TimesUsed := 1;
          LastAccess := AccessCounter;
          Modified := False;
          j := UsedLen - CurrStart;
          If j < CurrLen Then
            If j > 0 Then CurrLen := j
                     Else CurrLen := 0;
          If CurrLen > 0 Then
          Begin
            DataStream^. Seek (StreamPos);
            DataStream^. Write (MemBlock^, UsedLen);
            Move (MemBlock^ [CurrStart], ByteBuf [ReadCount], CurrLen);
          End;
        End;

        Blocks^. AtDelete (BlockNum);
        Blocks^. Insert (PCR);
      End;

    Inc (ReadCount, CurrLen);
    Inc (BlockPos, BlockSize);
    CurrStart := 0;
  End;

  Inc (AccessCounter);
  Inc (FPos, ReadCount);
  Read := ReadCount Div RecSize;
End;

Function TRealCache. Write (Var Buf; WriteNum: Word): Boolean;
Var
  ByteBuf                        : ByteArr Absolute Buf;
  i, j, StartBlock, EndBlock,
  BlockPos, EndOfs, CurrEnd      : LongInt;
  PCR                            : PCacheRec;
  CurrStart, CurrLen, WriteCount : Word;
  BlockNum                       : Integer;
  CR                             : CacheRec;

Begin
  StartBlock := FPos Div BlockSize;
  BlockPos := StartBlock * BlockSize;
  CurrStart := FPos - BlockPos;
  EndOfs := FPos + WriteNum * RecSize - 1;
  EndBlock := EndOfs Div BlockSize;
  CurrEnd := BlockSize - 1;
  FError := 0;
  WriteCount := 0;

  For i := StartBlock To EndBlock Do
  Begin
    If i = EndBlock Then
      CurrEnd := EndOfs - BlockPos;
    CurrLen := CurrEnd + 1 - CurrStart;

    CR. FileBlock := i;
    If Blocks^. Search (@CR, BlockNum) Then         { cache hit }
      With PCacheRec (Blocks^. At (BlockNum))^ Do
      Begin
        If CurrLen > 0 Then
        Begin
          Inc (TimesUsed);
          If TimesUsed > MaxUsed Then
            MaxUsed := TimesUsed;
          LastAccess := AccessCounter;
          DataStream^. Seek (StreamPos + CurrStart);
          DataStream^. Write (ByteBuf [WriteCount], CurrLen);
          j := CurrStart + CurrLen;
          If UsedLen < j Then
            UsedLen := j;
          Modified := True;
        End;
      End
    Else                                            { cache miss }
      If Blocks^. Count < MaxBlocks Then            { add new block }
      Begin
        If (CurrLen < BlockSize) And (BlockPos < PhysSize) Then
          PhysRead (BlockPos, MemBlock^, BlockSize, CR. UsedLen)
        Else
          CR. UsedLen := 0;
        Move (ByteBuf [WriteCount], MemBlock^ [CurrStart], CurrLen);
        j := CurrStart + CurrLen;
        If CR. UsedLen < j Then
          CR. UsedLen := j;
        CR. StreamPos := DataStream^. GetSize;
        DataStream^. Seek (CR. StreamPos);
        DataStream^. Write (MemBlock^, BlockSize);
        If DataStream^. Status = stOk Then
        Begin
          CR. TimesUsed := 1;
          CR. LastAccess := AccessCounter;
          CR. Modified := True;
          New (PCR);
          PCR^ := CR;
          Blocks^. AtInsert (BlockNum, PCR);
        End Else
        Begin
          DataStream^. Reset;
          MaxBlocks := Blocks^. Count;
          PhysWrite (BlockPos, MemBlock^, CR. UsedLen, CR. UsedLen);
        End;
      End Else
      Begin                                         { replace old block }
        BlockNum := FindWorstBlock;
        PCR := Blocks^. At (BlockNum);

        With PCR^ Do
        Begin
          If Modified Then
          Begin
            DataStream^. Seek (StreamPos);
            DataStream^. Read (MemBlock^, UsedLen);
            PhysWrite (FileBlock * BlockSize, MemBlock^, UsedLen, UsedLen);
          End;

          If (CurrLen < BlockSize) And (BlockPos < PhysSize) Then
            PhysRead (BlockPos, MemBlock^, BlockSize, UsedLen)
          Else
            UsedLen := 0;
          Move (ByteBuf [WriteCount], MemBlock^ [CurrStart], CurrLen);
          FileBlock := i;
          TimesUsed := 1;
          LastAccess := AccessCounter;
          j := CurrStart + CurrLen;
          If UsedLen < j Then
            UsedLen := j;
          Modified := True;
          DataStream^. Seek (StreamPos);
          DataStream^. Write (MemBlock^, UsedLen);
        End;

        Blocks^. AtDelete (BlockNum);
        Blocks^. Insert (PCR);
      End;

    Inc (WriteCount, CurrLen);
    Inc (BlockPos, BlockSize);
    CurrStart := 0;
  End;

  Inc (AccessCounter);
  Inc (FPos, WriteCount);
  If FPos > FSize Then
    FSize := FPos;
  Write := (WriteCount Div RecSize) = WriteNum;
End;

Function TRealCache. Seek (NewPos: LongInt): Boolean;
Begin
  FPos := NewPos * RecSize;
  FError := Byte (FPos > FSize);
  Seek := Not Boolean (FError);
End;

Function TRealCache. Size: LongInt;
Begin
  Size := FSize Div RecSize;
End;

Function TRealCache. Pos: LongInt;
Begin
  Pos := FPos Div RecSize;
End;

Function TRealCache. EoF: Boolean;
Begin
  EoF := FPos >= FSize;
End;

Procedure TRealCache. PhysRead (NewPos: LongInt; Var Buf; Count: Word;
                                Var Result: Word);
Begin
  If PhysPos <> NewPos Then
    System. Seek (F, NewPos);
  BlockRead (F, Buf, Count, Result);
  PhysPos := NewPos + Result;
  If FError = 0 Then
    FError := IOResult;
End;

Procedure TRealCache. PhysWrite (NewPos: LongInt; Var Buf; Count: Word;
                                 Var Result: Word);
Begin
  If PhysPos <> NewPos Then
    System. Seek (F, NewPos);
  BlockWrite (F, Buf, Count, Result);
  PhysPos := NewPos + Result;
  If PhysPos > PhysSize Then
    PhysSize := PhysPos;
  If FError = 0 Then
    FError := IOResult;
End;

{ --- Cache queue manager --- }

Function NewCache (BlockSize, MaxBlocks: Word): PCache;
Var
  PC : PCache;

Begin
  If CacheList = Nil Then
    CacheList := New (PCollection, Init (4, 4));

  PC := New (PRealCache, Init (BlockSize, MaxBlocks));
  If PC = Nil Then
    PC := New (PCache, Init);

  CacheList^. Insert (PC);
  NewCache := PC;
End;

Procedure DisposeCache (Var PC: PCache);
Begin
  CacheList^. Delete (PC);
  Dispose (PC, Done);
  PC := Nil;
End;

Procedure DisposeAllCaches;
Var
  i : Integer;

Begin
  If CacheList <> Nil Then
  Begin
    For i := 0 To CacheList^. Count-1 Do
      Dispose (PCache (CacheList^. At (i)), Done);

    CacheList^. DeleteAll;
    Dispose (CacheList, Done);
    CacheList := Nil;
  End;
End;

End.
