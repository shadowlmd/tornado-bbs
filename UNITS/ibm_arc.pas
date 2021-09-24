{$O+,F+}
{ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป}
{บ      -- THIS FILE IS PART OF THE LIVESYSTEMS COMPRESSOR TOOLBOX. --      บ}
{บ          ALL RIGHTS RESERVED  (C) COPYRIGHTED G. HOOGTERP 1994           บ}
{บ                                                                          บ}
{บ             See the documentation for details on the license.            บ}
{บ                                                                          บ}
{ศออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ}

Unit IBM_ARC;

Interface

Uses
{$IFDEF VirtualPascal}
  Use16,
{$ENDIF}
  BSC,
  tMisc;

Type
  ARCObject = Object(BasicCompressorObject)
     Constructor ARCInit;
     Procedure FindFirstEntry;   Virtual;
     Procedure FindNextEntry;    Virtual;
     Procedure CheckProtection;  Virtual;
     Function IsThisTypeFile(Var B ;Size : Word):Boolean; Virtual;
   End; {Object}

   ARCPtr = ^ARCObject;

Var
   CO : ArcPtr;

Implementation

Type LocalHeader = Record
       Mark      : Byte;
       Version   : Byte;
       Name      : Array[1..13] Of Char;
       CompSize  : LongInt;
       Date      : Word;
       Time      : Word;
       Crc       : Word;
       RealSize  : LongInt;
     End;

Const ArcMethodes  : Array[1..11] Of String[10] =
                    ('Stored    ',
                     'Stored    ',
                     'Packed    ',
                     'Squeezed  ',
                     'Crunched  ',
                     'Crunched  ',
                     'Crunched  ',
                     'Crunched  ',
                     'Squased   ',
                     'Crushed   ',
                     'Distill   '
                     );

Constructor ARCObject.ARCInit;
Begin
Init;
Platform:=ID_IBM;
CompressorType:='ARC';
CompressorName:='ARC/PAK/ARC7';
Magic:=ARC_Type; { Unique number }
End;

Procedure ARCObject.FindFirstEntry;
Var  F           : File;
     Buf         : LocalHeader;
Begin
SetFileMode(ReadOnly+ShareDenyNone);
Assign(F,FileName);
Reset(F,1);
Seek(F,WhereInFile);

BlockRead(F,Buf,SizeOf(Buf),RR);

If Buf.Mark=0
   Then Begin
        LastEntry:=True;
        Close(F);
        ResetFileMode;
        Exit;
        End;

If Not BeQuick
   Then Begin
        With IBM(Entry),Buf Do
         Begin
         FileName       := Az2Str (Name, 255);
         CompressedSize := CompSize;
         OriginalSize   := RealSize;
         If Version<=11
            Then CompressionName:= ArcMethodes[Version]
            Else CompressionName:= 'Unknown   ';
         FileCRC        := HexW (CRC)+'    ';
         FileDate       := TimeStamp((LongInt(Date) Shl 16)+LongInt(Time));
         SaveID         := '';
         End; {With}
        End;

Case Buf.Version of
 10 : CompressorName:='PAK';    { Cannot be trusted! }
 11 : CompressorName:='ARC7';
End; {Case}

WhereInFile:=WhereInFile+SizeOf(Buf)+Buf.CompSize;
Close(F);
ResetFileMode;
End;

Procedure ARCObject.FindNextEntry;
Var  F           : File;
     Buf         : LocalHeader;
Begin
SetFileMode(ReadOnly+ShareDenyNone);
Assign(F,FileName);
Reset(F,1);
Seek(F,WhereInFile);

BlockRead(F,Buf,SizeOf(Buf),RR);
If Buf.Version=0
   Then Begin
        LastEntry:=True;
        Close(F);
        ResetFileMode;
        Exit;
        End;

If Not BeQuick
   Then Begin
        With IBM(Entry),Buf Do
         Begin
         FileName       := Az2Str (Name, 255);
         CompressedSize := CompSize;
         OriginalSize   := RealSize;
         If Version<=11
            Then CompressionName:= ArcMethodes[Version]
            Else CompressionName:= 'Unknown   ';
         FileCRC        := HexW (CRC)+'    ';
         FileDate       := TimeStamp((LongInt(Date) Shl 16)+LongInt(Time));
         SaveID         := '';
         End; {With}
        End;

Case Buf.Version of
 10 : CompressorName:='PAK';    { Cannot be trusted! }
 11 : CompressorName:='ARC7';
End; {Case}

WhereInFile:=WhereInFile+SizeOf(Buf)+Buf.CompSize;

Close(F);
ResetFileMode;
End;

Procedure ARCObject.CheckProtection;
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

Function ARCObject.IsThisTypeFile(Var B ;Size : Word):Boolean;
Type Check = Array[0..2] Of Char;
Begin
ARCInit;
IsThisTypeFile:=True;

If IsExeFile(B) and
   SearchBuffer(B,Size,8400,9000,'it?'#00#$1A,WhereInFile)
   Then Begin
        SelfExtractor:=True;
        Inc(WhereInFile,4);
        Exit;
        End;
WhereInFile:=0;


If (Byte(B)=$1A) And
   (Check(B) <> #$1A'HP') And   { Check HYPER! }
   (Check(B) <> #$1A'ST')
   Then Exit;
IsThisTypeFile:=False;
End;

End.
