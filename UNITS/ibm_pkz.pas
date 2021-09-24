{$O+,F+}
{ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป}
{บ      -- THIS FILE IS PART OF THE LIVESYSTEMS COMPRESSOR TOOLBOX. --      บ}
{บ          ALL RIGHTS RESERVED  (C) COPYRIGHTED G. HOOGTERP 1994           บ}
{บ                                                                          บ}
{บ             See the documentation for details on the license.            บ}
{บ                                                                          บ}
{ศออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ}

Unit IBM_PKZ;

Interface

Uses
{$IFDEF VirtualPascal}
  Use16,
{$ENDIF}
  BSC,
  tMisc;

Type PkZipObject = Object(BasicCompressorObject)
       Constructor ZIPInit;
       Procedure FindFirstEntry;   Virtual;
       Procedure FindNextEntry;    Virtual;
       Procedure CheckProtection;  Virtual;
       Function IsThisTypeFile(Var B; Size : Word):Boolean; Virtual;
     End; {PkzipObject}

     PkzipPtr = ^PkzipObject;
Var
     CO : PkZipPtr;

Implementation

Const LocalHeaderSize = 26;
      BegCentrDirSize = 42;
      EndCentrDirSize = 18;
      BufferSize      = 42;

Type  Buffer          = Array [1..42] Of Byte;
      NameBuffer      = Array [0..255] Of Char;
      LocalHeader     = Record
        Version       : Word;
        GenBits       : Word;
        Methode       : Word;
        Time          : Longint;
        CrcLo         : Word;
        CrcHi         : Word;
        CompSize      : LongInt;
        RealSize      : LongInt;
        NameLen       : Word;
        ExtraLen      : Word;
        Fill          : Array [1..16] Of Char;
      End;

      StartCentralDir = Record
        VersionUsed   : Word;
        VersionNeeded : Word;
        GenBits       : Word;
        Meth          : Word;
        Time          : LongInt;
        CRC           : Longint;
        CompSize      : LongInt;
        RealSize      : LongInt;
        NameLen       : Word;
        ExtraLen      : Word;
        CommLen       : Word;
        DiskStart     : Word;
        IntAttr       : Word;
        ExtAttr       : LongInt;
        LocHeadOfs    : LongInt;
      End;
      EndCentralDir   = Record
        DiskNr        : Word;
        SOCDdiskNr    : Word;
        CDDiskCount   : Word;
        CDTotCount    : Word;
        CDSize        : LongInt;
        CDOfs         : LongInt;
        ZipComment    : Word;
        Fill          : Array[1..24] Of Char;
      End;

Const ZipMethodes  : Array[0..8] Of NameString =
                     ('Stored    ',
                      'Shrunk    ',
                      'Reduced 1 ',
                      'Reduced 2 ',
                      'Reduced 3 ',
                      'Reduced 4 ',
                      'Imploded  ',
                      'Tokenized ',
                      'DeflateN  '
                     );

Var
     HighVersion : Word;

Constructor PkzipObject.ZIPInit;
Begin
Init;
Platform:=ID_IBM;
CompressorType:='ZIP2';
CompressorName:='PK(un)zip';
Magic:=ZIP_Type;
End;

Procedure PkzipObject.FindFirstEntry;
Var HeaderID : LongInt;
  F           : File;
     Buf         : Buffer;
     AName       : NameBuffer;
Begin
HighVersion:=0;
SetFileMode(ReadOnly+ShareDenyNone);
Assign(F,FileName);
Reset(F,1);
Seek(F,WhereInFile);
BlockRead(F,HeaderID,4,RR);
If RR<>4
   Then Begin
        Close(F);
        LastEntry:=True;
        ResetFileMode;
        Exit;
        End;

BlockRead(F,Buf,BufferSize,RR);
If RR<>BufferSize
   Then Begin
        Close(F);
        LastEntry:=True;
        ResetFileMode;
        Exit;
        End;

Case (HeaderID Shr 16) of
 $0403    : Begin
            If Not BeQuick Then
            Begin
              Seek(F,WhereInFile+LocalHeaderSize+4);
              FillChar(AName,SizeOf(AName),#00);
              BlockRead(F,AName,LocalHeader (Buf). NameLen);
              With IBM (Entry), LocalHeader (Buf) Do
              Begin
                FileName       := Az2Str (String (AName), 255);
                ContainsPaths  := Pos ('/',FileName) > 0;
                OriginalSize   := RealSize;
                CompressedSize := CompSize;
                CompressionName:= ZipMethodes [Methode];

                FileCRC        := HexW (CRCHi) + HexW (CRCLo);

                FileDate       := TimeStamp (Time);
                If ProtectedFile Then SaveID := '-AV'
                                 Else SaveID := '';
              End; {With}
            End;

            If LocalHeader(Buf).Version>HighVersion Then HighVersion:=LocalHeader(Buf).Version;
            With LocalHeader(Buf) Do
              WhereInFile:=WhereInFile+4+LocalHeaderSize+NameLen+ExtraLen+CompSize;
         End;
 $0201    : LastEntry:=True;
 $0605    : LastEntry:=True;
 Else
End;
Close(F);
ResetFileMode;
End;

Procedure PkzipObject.FindNextEntry;
Var HeaderID : LongInt;
    ExtraTag : Word;
  F           : File;
     Buf         : Buffer;
     AName       : NameBuffer;
Begin
SetFileMode(ReadOnly+ShareDenyNone);
Assign(F,FileName);
Reset(F,1);
Seek(F,WhereInFile);
BlockRead(F,HeaderID,4,RR);
If RR<>4
   Then Begin
        Close(F);
        ResetFileMode;
        LastEntry:=True;
        Exit;
        End;

BlockRead(F,Buf,BufferSize,RR);
If RR<>BufferSize
   Then Begin
        Close(F);
        LastEntry:=True;
        ResetFileMode;
        Exit;
        End;

Case (HeaderID Shr 16) of
 $0403    : Begin { Local Header Block }
            If Not BeQuick
               Then Begin
                    Seek(F,WhereInFile+LocalHeaderSize+4);
                    FillChar(AName,SizeOf(AName),#00);
                    BlockRead(F,AName,LocalHeader(Buf).NameLen);
                    With IBM(Entry),LocalHeader(Buf) Do
                     Begin
                     FileName       := Az2Str (String (AName), 255);
                     OriginalSize   := RealSize;
                     CompressedSize := CompSize;
                     CompressionName:= ZipMethodes [Methode];
                     FileCRC        := HexW (CRCHi) + HexW (CRCLo);
                     FileDate       := TimeStamp (Time);
                     If ProtectedFile
                        Then SaveID := '-AV'
                        Else SaveID := '';
                     Extra          := '';
                     End; {With}
                     End;
            If LocalHeader(Buf).Version>HighVersion
               Then HighVersion:=LocalHeader(Buf).Version;
            With LocalHeader(Buf) Do
             WhereInFile:=WhereInFile+4+LocalHeaderSize+NameLen+ExtraLen+CompSize;
            End;
 $0201    : Begin { Central Dir Block }
            With StartCentralDir(Buf) Do
             Begin
             UnpackVersion:=VersionNeeded;
             CompressorName:='PK(un)zip ' + LeftPadCh (Long2Str (VersionNeeded),
               '0', 2);
             Insert('.',CompressorName,Length(CompressorName));
             If ExtraLen>0
                Then Begin
                     Seek(F,WhereInFile+BegCentrDirSize+NameLen+4);
                     BlockRead(F,ExtraTag,2,RR);
                     ProtectedFile:=ExtraTag=7;
                     End;
             End;
            LastEntry:=True;
            End;
 Else       LastEntry:=True;
End;
Close(F);
ResetFileMode;
End;

Procedure PkzipObject.CheckProtection;
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
If HighVersion=20
   Then CompressorType:='ZIP2'
   Else CompressorType:='ZIP1';
End;


Function PkZipObject.IsThisTypeFile(Var B ;Size : Word):Boolean;
Begin
ZIPInit;
IsThisTypeFile:=True;

If IsExeFile(B)
   Then Begin
        SelfExtractor:=True;
        If SearchBuffer(B,Size,14000,16000,'PK'#03#04,WhereInFile) Then Exit;
        End;
WhereInFile:=0;

If LongInt(B) =$04034B50
   Then Exit;
IsThisTypeFile:=False;
End;

End.
