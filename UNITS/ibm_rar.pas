{$O+,F+}
{ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป}
{บ      -- THIS FILE IS PART OF THE LIVESYSTEMS COMPRESSOR TOOLBOX. --      บ}
{บ          ALL RIGHTS RESERVED  (C) COPYRIGHTED G. HOOGTERP 1994           บ}
{บ                                                                          บ}
{บ             See the documentation for details on the license.            บ}
{บ                                                                          บ}
{ศออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ}

Unit IBM_RAR;

Interface

Uses
{$IFDEF VirtualPascal}
  Use16,
{$ENDIF}
  BSC,
  tMisc;

Type
  RARObject = Object (BasicCompressorObject)
    Constructor RARInit;
    Procedure FindFirstEntry; Virtual;
    Procedure FindNextEntry; Virtual;
    Procedure CheckProtection; Virtual;
    Function IsThisTypeFile (Var B; Size: Word): Boolean; Virtual;
  End;

  RARPtr = ^RARObject;

Var
  CO : RARPtr;

Implementation

Const
  RARMethodes : Array [$30..$35] Of String [10] =
  ( 'Storing   ', 'Fastest   ', 'Fast      ',
    'Normal    ', 'Good      ', 'Best      ');

Type
  HeaderType   = Record
    CRC         : Word;
    Typ         : Byte;
    Flags, Size : Word;
  End;

  ArcHeader    = Record
    Res1 : Word;
    Res2 : LongInt;
  End;

  FileHeader   = Record
    PackSize, UnpSize : LongInt;
    HostOS            : Byte;
    CRClo, CRChi      : Word;
    FileDate          : LongInt;
    UnpVer, Methode   : Byte;
    NameSize          : Word;
    Attr              : LongInt;
  End;

  CommHeader   = Record
    UnpSize         : Word;
    UnpVer, Methode : Byte;
    CommCrc         : Word;
  End;

  NameBuffer = Array [1..255] Of Char;

Constructor RARObject. RARInit;
Begin
  Init;
  Platform := ID_IBM;
  CompressorType := 'RAR';
  CompressorName := 'RAR';
  Magic := RAR_Type;  { A unique number within the toolbox }
End;

Procedure RARObject. FindFirstEntry;
Var
  Stop     : Boolean;
  AddSize  : LongInt;
  F        : File;
  Header   : HeaderType;
  FileHead : FileHeader;
  AName    : NameBuffer;

Begin
  SetFileMode (ReadOnly+ShareDenyNone);
  Assign (F, FileName);
  Reset (F, 1);
  Stop := False;
  Broken := False;

  Repeat
    Seek (F, WhereInFile);
    BlockRead (F, Header, SizeOf (Header), RR);
    If RR <> SizeOf (Header) Then
    Begin
      Close (F);
      LastEntry := True;
      Broken := True;
      ResetFileMode;
      Exit;
    End;

    If (Header. Typ = $74) Then WhereInFile := WhereInFile+Header. Size Else
    Begin
      If (Header. Flags And $8000) = 0
      Then
        WhereInFile := WhereInFile+Header. Size
      Else
      Begin
        BlockRead (F, AddSize, 4, RR);
        WhereInFile := WhereInFile+Header. Size+AddSize;
      End;
    End;

    Case Header. Typ Of
      $73 : Begin
              SolidArchive := Header.Flags and $0008 <> 0;
              ProtectedFile := Header.Flags and $0020 <> 0;
              Locked := Header.Flags and $0004 <> 0;
              If FileExtra <> '' Then Dec (FileExtra [0], 2);
            End;

      $74 : Begin
              BlockRead (F, FileHead, SizeOf (FileHead), RR);
              FillChar (AName, SizeOf (AName), 0);
              WhereInFile := WhereInFile+FileHead. PackSize;
              Stop := True;

              If FileHead. NameSize < FileSize (F)-FilePos (F) Then
              Begin
                If Not BeQuick Then
                With IBM (Entry) Do
                Begin
                  BlockRead (F, AName, FileHead. NameSize, RR);
                  FileName := Az2Str (AName, 255);
                  ContainsPaths :=Pos ('/',FileName) > 0;
                  OriginalSize := FileHead. UnpSize;
                  CompressedSize := FileHead. PackSize;
                  If (FileHead. Attr And $10) = $10
                  Then
                    CompressionName := '<DIR>     '
                  Else
                    CompressionName := RARMethodes [FileHead. Methode];

                  FileCRC := HexW (FileHead. CRChi) + HexW (FileHead. CRClo);
                  FileDate := TimeStamp (FileHead. FileDate);
                  If ProtectedFile Then SaveID:='-SE' Else SaveID:='';
                End;

                UnpackVersion := FileHead. UnpVer;
                HasPassword := Header. Flags and $0004 <> 0;
              End Else
                Broken := True;

            End;

    End;
  Until Stop;

  Close (F);
  ResetFileMode;
End;

Procedure RARObject. FindNextEntry;
Var
  AddSize           : LongInt;
  Stop              : Boolean;
  F        : File;
  Header   : HeaderType;
  FileHead : FileHeader;
  AName    : NameBuffer;

Begin
  SetFileMode (ReadOnly+ShareDenyNone);
  Assign (F, FileName);
  Reset (F, 1);
  Stop := False;

  Repeat
    If (WhereInFile < FileSize (F)) And (WhereInFile >= 0)
    Then
      Seek (F, WhereInFile)
    Else If WhereInFile = FileSize (F) Then
    Begin
      LastEntry := True;
      Close (F);
      ResetFileMode;
      Exit;
    End Else
    Begin
      Close (F);
      LastEntry := True;
      Broken := True;
      ResetFileMode;
      Exit;
    End;

    BlockRead (F, Header, SizeOf (Header), RR);
    If RR <> SizeOf (Header) Then
    Begin
      Close (F);
      LastEntry := True;
      ResetFileMode;
      Exit;
    End;

    If (Header. Typ = $74) Then WhereInFile := WhereInFile+Header.Size Else
    Begin
      If (Header. Flags And $8000) = 0 Then
        WhereInFile := WhereInFile+Header.Size Else
      Begin
        BlockRead (F, AddSize, 4, RR);
        WhereInFile := WhereInFile+Header.Size+AddSize;
      End;
    End;

    If Header. Typ = $74 Then
    Begin
      BlockRead (F, FileHead, SizeOf (FileHead), RR);
      Stop := True;
      WhereInFile := WhereInFile+FileHead. PackSize;
      If Not BeQuick Then
      With IBM (Entry) Do
      Begin
        Fillchar (AName, SizeOf (AName), 0);
        BlockRead (F, AName, FileHead. NameSize, RR);
        FileName := Az2Str (AName, 255);
        ContainsPaths := (Pos ('/', FileName) > 0) Or (Pos ('\', FileName) > 0);
        OriginalSize := FileHead. UnpSize;
        CompressedSize := FileHead. PackSize;
        If (FileHead. Attr And $10) = $10
        Then CompressionName := '<DIR>     '
        Else CompressionName := RARMethodes [FileHead. Methode];
        FileCRC := HexW (FileHead. CRChi) + HexW (FileHead. CRClo);
        {FileCRC:=HexLong(FileHead.FCRC);}
        FileDate := TimeStamp (FileHead. FileDate);
        If ProtectedFile Then SaveID := '-SE' Else SaveID := '';
        If FileHead. UnpVer > UnpackVersion Then UnpackVersion := FileHead. UnpVer;
      End;

      HasPassword := Header. Flags and $0004 <> 0;
      If FileHead. UnpVer > UnpackVersion Then UnpackVersion := FileHead. UnpVer;
    End;
  Until Stop;

  Close (F);
  ResetFileMode;
End;

Procedure RARObject.CheckProtection;
Var Old : LongInt;
Begin
Old:=WhereInFile;
BeQuick:=True;

FindFirstEntry;
While Not LastEntry Do
 FindNextEntry;

BeQuick:=False;
WhereInFile:=Old;
LastEntry:=False;
End;

Function RARObject.IsThisTypeFile(Var B ;Size : Word):Boolean;
Begin
RARInit;  { Reinit the current object }
IsThisTypeFile:=True;
WhereInFile:=0;

With HeaderType(B) Do
 If (CRC=$6152)   And
    (Typ=$72)     And
    (Flags=$1A21) And
    (Size=$007)
    Then Exit;

If IsExeFile(B)
   Then Begin
        SelfExtractor:=True;
        If SearchBuffer(B,Size,6000,7500,#$52#$61#$72#$21#$1A#$07#$00,WhereInFile)
           Then Exit;
        If SearchBuffer(B,Size,9000,9500,#$52#$61#$72#$21#$1A#$07#$00,WhereInFile)
           Then Exit;
        End;

IsThisTypeFile:=False;
End;

End.
