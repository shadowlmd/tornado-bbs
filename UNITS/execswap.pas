{$R-,S-,O-,I-,B-}

Unit ExecSwap;

{*********************************************************}
{*                    EXECSWAP.PAS                       *}
{*                                                       *}
{*    Copyright (c) TurboPower Software, 1988-89         *}
{*                                                       *}
{*********************************************************}

Interface

Const
  UseEmsIfAvailable : Boolean = False{True};     {True to use EMS if available}
  BytesSwapped      : LongInt = 0;        {Bytes to swap to EMS/disk}
  EmsAllocated      : Boolean = False;    {True when EMS allocated for swap}
  FileAllocated     : Boolean = False;    {True when file allocated for swap}

Function ExecWithSwap (Path, CmdLine : String) : Word;
  {-DOS EXEC supporting swap to EMS or disk}

Function InitExecSwap (LastToSave : Pointer; SwapFileName : String) : Boolean;
  {-Initialize for swapping, returning TRUE if successful}

Procedure ShutdownExecSwap;
  {-Deallocate swap area}

Implementation

Var
  EmsHandle  : Word;              {Handle of EMS allocation block}
  FrameSeg   : Word;              {Segment of EMS page frame}
  FileHandle : Word;              {DOS handle of swap file}
  SwapName   : String [80];       {ASCIIZ name of swap file}
  SaveExit   : Pointer;           {Exit chain pointer}

  {$L EXECSWAP}
  Function ExecWithSwap (Path, CmdLine : String) : Word; External;
  Procedure FirstToSave; External;
  Function AllocateSwapFile : Boolean; External;
  Procedure DeallocateSwapFile; External;

  {$F+}     {These routines could be interfaced for general use}
  Function EmsInstalled : Boolean; External;
  Function EmsPageFrame : Word; External;
  Function AllocateEmsPages (NumPages : Word) : Word; External;
  Procedure DeallocateEmsHandle (Handle : Word); External;
  Function DefaultDrive : Char; External;
  Function DiskFree (Drive : Byte) : LongInt; External;

  Procedure ExecSwapExit;
  Begin
    ExitProc := SaveExit;
    ShutdownExecSwap;
  End;
  {$F-}

  Procedure ShutdownExecSwap;
  Begin
    If EmsAllocated Then
    Begin
      DeallocateEmsHandle (EmsHandle);
      EmsAllocated := False;
    End
    Else
      If FileAllocated Then
      Begin
        DeallocateSwapFile;
        FileAllocated := False;
      End;
  End;

  Function PtrDiff (H, L : Pointer) : LongInt;
  Type
    OS = Record
      O, S : Word;
    End;   {Convenient typecast}

  Begin
    PtrDiff := (LongInt (OS (H).S) ShL 4 + OS (H).O) -
               (LongInt (OS (L).S) ShL 4 + OS (L).O);
  End;

  Function InitExecSwap (LastToSave : Pointer; SwapFileName : String) : Boolean;
  Const
    EmsPageSize = 16384;            {Bytes in a standard EMS page}

  Var
    PagesInEms : Word;              {Pages needed in EMS}
    BytesFree  : LongInt;           {Bytes free on swap file drive}
    DriveChar  : Char;              {Drive letter for swap file}

  Begin
    InitExecSwap := False;
    If EmsAllocated Or FileAllocated Then
      Exit;

    BytesSwapped := PtrDiff (LastToSave, @FirstToSave);
    If BytesSwapped <= 0 Then
      Exit;

    If UseEmsIfAvailable And EmsInstalled Then
    Begin
      PagesInEms := (BytesSwapped + EmsPageSize - 1) Div EmsPageSize;
      EmsHandle := AllocateEmsPages (PagesInEms);
      If EmsHandle <> $FFFF Then
      Begin
        EmsAllocated := True;
        FrameSeg := EmsPageFrame;
        If FrameSeg <> 0 Then
        Begin
          InitExecSwap := True;
          Exit;
        End;
      End;
    End;
    If Length (SwapFileName) <> 0 Then
    Begin
      SwapName := SwapFileName + #0;
      If Pos (':', SwapFileName) = 2 Then
        DriveChar := UpCase (SwapFileName [1])
      Else
        DriveChar := DefaultDrive;
      BytesFree := DiskFree (Byte (DriveChar) - $40);
      FileAllocated := (BytesFree > BytesSwapped) And AllocateSwapFile;
      InitExecSwap := FileAllocated;
    End;
  End;

Begin
  SaveExit := ExitProc;
  ExitProc := @ExecSwapExit;
End.
