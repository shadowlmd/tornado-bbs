{$O+,F+}
{ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป}
{บ      -- THIS FILE IS PART OF THE LIVESYSTEMS COMPRESSOR TOOLBOX. --      บ}
{บ          ALL RIGHTS RESERVED  (C) COPYRIGHTED G. HOOGTERP 1994           บ}
{บ                                                                          บ}
{บ             See the documentation for details on the license.            บ}
{บ                                                                          บ}
{ศออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ}

Unit IBM_ZOO;

Interface

Uses
{$IFDEF VirtualPascal}
  Use16,
{$ENDIF}
  BSC,
  tMisc;

Type ZOOObject = Object(BasicCompressorObject)
       Constructor ZOOInit;
       Procedure FindFirstEntry;   Virtual;
       Procedure FindNextEntry;    Virtual;
       Procedure CheckProtection;  Virtual;
       Function IsThisTypeFile(Var B ;Size : Word):Boolean; Virtual;
     End; {Object}

     ZOOPtr = ^ZOOObject;

Var
     CO : ZooPtr;

Implementation



Type MainHeader  = Record
       ID        : Array[1..20] Of Char;
       LoTag     : Word;
       HiTag     : Word;
       Start     : LongInt;
       Minus     : Longint;
       MajVers   : Byte;
       MinVers   : Byte;
     End;
     LocalHeader = Record
       LoTag     : Word;
       HiTag     : Word;
       CType     : Char;
       Methode   : Byte;
       Next      : LongInt;
       Offset    : LongInt;
       Date      : Word;
       Time      : Word;
       CRC       : Word;
       RealSize  : LongInt;
       CompSize  : LongInt;
       MajVer    : Byte;
       MinVer    : Byte;
       Del       : Boolean;
       CommPtr   : LongInt;
       CommLen   : Word;
       Name      : Array[0..13] Of Char;
     End;

Const ZooMethodes : Array[0..1] Of String[10] =
                    (
                    'Stored',
                    'LZW-compr.'
                    );




Constructor ZOOObject.ZOOInit;
Begin
Init;
Platform:=ID_IBM;
CompressorType:='ZOO';
CompressorName:='ZOO';
Magic:=ZOO_Type;
End;



Procedure ZOOObject.FindFirstEntry;
Var  Main     : MainHeader;
  F           : File;
     Buf         : LocalHeader;
     EXEofs      : LongInt;
Begin
SetFileMode(ReadOnly+ShareDenyNone);
Assign(F,FileName);
Reset(F,1);


Seek(F,WhereInFile);
BlockRead(F,Main,SizeOf(Main),RR);
CompressorName:=Main.ID;
CompressorName[0]:=#8;

EXEofs:=WhereInFile;
Seek(F,EXEofs+Main.Start);
BlockRead(F,Buf,SizeOf(Buf),RR);

If Not BeQuick
   Then Begin
        With Buf,IBM(Entry) Do
         Begin
         FileName       := Az2Str (Name, 255);
         OriginalSize   := RealSize;
         CompressedSize := CompSize;
         CompressionName:= ZooMethodes [Methode];
         FileCRC        := HexW (CRC) + '    ';
         FileDate       := TimeStamp( (LongInt(Date) Shl 16) + LongInt (Time));
         SaveID         := '';
         End; {with}
        End;

WhereInFile:=EXEofs+Buf.Next;
Close(F);
ResetFileMode;
End;

Procedure ZOOObject.FindNextEntry;
Var
  F           : File;
     Buf         : LocalHeader;
     EXEofs      : LongInt;
Begin
SetFileMode(ReadOnly+ShareDenyNone);
Assign(F,FileName);
Reset(F,1);
Seek(F,WhereInFile);

BlockRead(F,Buf,SizeOf(Buf),RR);
If Buf.CompSize=0
   Then Begin
        LastEntry:=True;
        Close(F);
        ResetFileMode;
        Exit;
        End;


If Not BeQuick
   Then Begin
        With Buf,IBM(Entry) Do
         Begin
         FileName       := Az2Str (Name, 255);
         OriginalSize   := RealSize;
         CompressedSize := CompSize;
         CompressionName:= ZooMethodes[Methode];
         FileCRC        := HexW (CRC) + '    ';
         FileDate       := TimeStamp((LongInt(Date) Shl 16)+LongInt(Time));
         SaveID         := '';
         If Del
            Then Extra  := 'Deleted'
            Else Extra  := '';
         End; {with}
        End;

WhereInFile:=EXEofs+Buf.Next;

Close(F);
ResetFileMode;
End;

Procedure ZOOObject.CheckProtection;
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

Function ZOOObject.IsThisTypeFile(Var B ;Size : Word):Boolean;
Type Check = Array[0..2] of Char;
Begin
ZOOInit;
IsThisTypeFile:=True;

If IsExeFile(B)
   Then Begin
        SelfExtractor:=True;
        If SearchBuffer(B,Size,2400,2500,'ZOO',WhereInFile) Then Exit;
        End;
WhereInFile:=0;

If Check(B)='ZOO'
   Then Exit;
IsThisTypeFile:=False;
End;

End.

