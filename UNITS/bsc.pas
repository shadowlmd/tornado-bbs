{$A-,F+}
{浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融}
{�      -- THIS FILE IS PART OF THE LIVESYSTEMS COMPRESSOR TOOLBOX. --      �}
{�          ALL RIGHTS RESERVED  (C) COPYRIGHTED G. HOOGTERP 1994           �}
{�                                                                          �}
{�             See the documentation for details on the license.            �}
{�                                                                          �}
{藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様夕}

{$IFDEF MSDOS}
  {$IFNDEF DPMI}
  {$IFNDEF DPMI32}
    {$Define UseASM}
  {$ENDIF}
  {$ENDIF}
{$ENDIF}

Unit
  BSC;

Interface

Uses
{$IFDEF VirtualPascal}
  Use16,
{$ENDIF}
  DOS,
  Objects,
  tGlob,
  tMisc;

Type
{$IFDEF VirtualPascal}
  LongWord = System. Cardinal;
  RR_Type  = LongWord;          { Used as SIZE type in BlockRead/BlockWrite }
{$ELSE}
  RR_Type  = SysInt;
{$ENDIF}

  ComStr = String [127];

{$I INC\arcstruc.inc}

Const
  MaxCompressors     = 16;  { Maximum number of compressors that can be   }
                            { Can be maximal 255 but 16 is enough for now }
  ReadOnly           = $00; { Filemode constants }
  WriteOnly          = $01;
  ReadWrite          = $02;

  ShareCompatible    = $00;
  ShareDenyReadWrite = $10;
  ShareDenyWrite     = $20;
  ShareDenyRead      = $30;
  ShareDenyNone      = $40;

  Inheritance        = $80;
  DefaultFileMode    = ReadOnly+ShareCompatible;

Type
  BasicCompressorObject = Object        { Basic compressor object     }
    FileName           : ComStr;       { Current filename            }
    CompressorType     : CompressorID; { Unique short compressor ID  }
    CompressorName     : NameString;   { Full compressor name        }
    Magic              : MagicTypes;   { A unique number             }
    WhereInFile        : LongInt;      { Filepointer                 }

    ProtectedFile,                     { Sec. Env. boolean           }
    SelfExtractor,                     { SelfExtractor boolean       }
    ContainsPaths,                     { Contains paths boolean      }
    HasPassword,                       { Password protected          }
    SolidArchive,                      { Is solid                    }
    Locked,
    Broken             : Boolean;      { is Locked                   }
    UnpackVersion      : Byte;         { Unpack version. 0 -> unknown}

    FileExtra          : String[132];  { Extra info found in the file}
    Entry              : InfoBlock;    { Internal entry buffer       }

    Platform           : PlatformID;   { Compressors platform        }
    LastEntry          : Boolean;      { True if end of file         }
    BeQuick            : Boolean;      { Don't show so don't conv.   }
    PreviouseMode      : Byte;         { Memory byte last filemode   }

    RR, RW             : RR_Type;      { RealRead variable for Blockread/write }

    Constructor Init;

    { � Compressor dependend functions 陳陳陳陳陳陳陳陳陳陳陳陳陳陳� }

    Procedure FindFirstEntry; Virtual;
    Procedure FindNextEntry; Virtual;
    Procedure CheckProtection; Virtual;
    Procedure PrintEntry; Virtual;
    Function IsThisTypeFile (Var B; Size: Word):Boolean; Virtual;
    Procedure ReturnEntry(Var E); Virtual;

    { � Compressor independend functions 陳陳陳陳陳陳陳陳陳陳陳陳陳� }

    Function IsProtected: Boolean;           { has Security envelope    }
    Function IsSelfExtractor: Boolean;       { is selfextracting file   }
    Function HasPaths: Boolean;              { Contains dir. structure  }
    Function IsSolidArchive: Boolean;        { Is solid                 }
    Function IsPasswordProtected: Boolean;   { Has passwords            }
    Function IsLocked: Boolean;              { Is Locked                }

    Function WhichType: CompressorID;        { Return Compressor ID     }
    Function WhichPlatform: PlatFormID;      { Return current platform  }
    Function PlatformName: String;           { The name of the platform }
    Procedure WriteHeader;                   { Write a header on screen }

    { � Misc. tools 陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳 }

    Function TimeStamp (Time: LongInt):TimeString;
    Function UnixTime (Time: LongInt):TimeString;
    Function ShortFileName (FileSpec: ComStr):ComStr;
    Function StripPath (F: ComStr):PathStr;
    Function SearchBuffer (Var B; Size, Start, Stop: Word; Check: String; Var InFile: LongInt):Boolean;
    Function IsEXEFile (Var B):Boolean;
    Function LongSwap (L: LongInt):LongInt;
    Procedure SetFileMode (Mode: Byte);
    Procedure ResetFileMode;
  End;

{陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳
  Create an array of pointers to compressionobjects.
 陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳}

Type
  ObjectList = Array [1..MaxCompressors] Of ^BasicCompressorObject;
  FFunc = Function (FName, FileDate: String; OrigSize, PackSize: LongInt): String;

Var
  OList             : ObjectList;
  OPtr              : Byte;
  ExitSave          : Pointer;
  ArcFiles          : PNotSortedCollection;
  FormArcStringFunc : FFunc;

Procedure AddToList (P: Pointer);

Implementation

Constructor BasicCompressorObject. Init;
{陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�
  Initialize the object, fill all the fields.
 陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�}
Begin
  BeQuick := False;
  LastEntry := False;
  SelfExtractor := False;
  ProtectedFile := False;
  ContainsPaths := False;
  HasPassword := False;
  SolidArchive  := False;
  Locked := False;
  Broken := False;
  UnpackVersion := 0;

  CompressorType := 'UNK';
  CompressorName := '* Unknown *';
  Magic := None;

  Platform := ID_IBM;
  FileExtra := '';
End;

Procedure BasicCompressorObject.FindFirstEntry;
Begin
End;

Procedure BasicCompressorObject.FindNextEntry;
Begin
End;

Procedure BasicCompressorObject.CheckProtection;
Begin
End;

Procedure BasicCompressorObject.WriteHeader;
Begin
End;

Procedure BasicCompressorObject. PrintEntry;
Begin
  With IBM (Entry) Do
    ArcFiles^. Insert (NewStr (FormArcStringFunc (FileName, FileDate,
      OriginalSize, CompressedSize)));
End;

Procedure BasicCompressorObject.ReturnEntry (Var E);
{陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�
  Return an entry as untyped variable.   VIRTUAL procedure.
 陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�}
Begin
  Move(IBM(Entry), E, SizeOf(Entry)); {???}
End;

Function BasicCompressorObject.IsThisTypeFile (Var B; Size: Word): Boolean;
{陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�
  Detect if the current file is of this type. VIRTUAL procedure
 陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�}
Begin
  IsThisTypeFile:=False;
End;

{様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様
  Non-virtual procedures and functions
 様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様}

Function BasicCompressorObject. IsProtected: Boolean;
{陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�
  Return the value of the ProtectedFile boolean.
 陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�}
Begin
  IsProtected:=ProtectedFile;
End;

Function BasicCompressorObject. IsSelfExtractor: Boolean;
{陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�
  Return the value of the SelfExtractor boolean.
 陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�}
Begin
  IsSelfExtractor:=SelfExtractor;
End;

Function BasicCompressorObject. IsLocked: Boolean;
{陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�
  Return the value of the Locked boolean.
 陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�}
Begin
  IsLocked:=Locked;
End;

Function BasicCompressorObject. HasPaths: Boolean;
{陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�
  Return the value of the haspaths boolean.
 陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�}
Begin
  HasPaths:=ContainsPaths;
End;

Function BasicCompressorObject. IsPasswordProtected: Boolean;
{陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�
  Return the value of the HasPassword boolean.
 陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�}
Begin
  IsPasswordProtected:=HasPassword;
End;

Function BasicCompressorObject. IsSolidArchive: Boolean;
{陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�
  Return the value of the HasPassword boolean.
 陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�}
Begin
  IsSolidArchive:=SolidArchive;
End;

Function BasicCompressorObject. WhichType: CompressorID;
{陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�
  Return the value of the CompressorType field.
 陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�}
Begin
  WhichType:=CompressorType;
End;

Function BasicCompressorObject. WhichPlatform: PlatformID;
{陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�
  Return the value of the Platform field.
 陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�}
Begin
  WhichPlatform:=PlatForm;
End;

Function BasicCompressorObject. PlatformName: String;
{陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�
  Return a description of the platform
 陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�}
Begin
  Case Platform Of
      ID_IBM : PlatformName := 'IBM or compatible';
      ID_MAC : PlatformName := 'Apple MacIntosh';
    ID_MULTI : PlatformName := 'Platform independend';
  Else
    PlatformName := 'Unknown platform';
  End;
End;

{様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
  LowLevel utility routines.
 様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�}

Function BasicCompressorObject. TimeStamp (Time: Longint): TimeString;
{陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�
  Create a timestamp string from a MSdos timestamp longint.
 陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�}
Var
  DateRec : DateTime;

Begin
  UnpackTime (Time, DateRec);
  TimeStamp := FormattedDate (DateRec, 'MM-DD-YY');
End;

Function BasicCompressorObject. UnixTime (Time: LongInt): TimeString;
Begin
  UnixTime := ' Unsupported format ';
End;

Function BasicCompressorObject. ShortFileName (FileSpec: ComStr): ComStr;
{陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�
  Shorten a full filespecifier to a filename with pathindication
    F.e.: C:\TEST\PROG\BLABLA.PAS becomes
          ...\BLABLA.PAS
 陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�}
Var
  Dum  : DirStr;
  Name : NameStr;
  Ext  : ExtStr;

Begin
  FSplit (PlaceSubStr (FileSpec, '/', '\'), Dum, Name, Ext);
  If Dum <> '' Then Dum := '...\'
               Else Dum := '    ';
  ShortFileName := Pad (Dum + Name + Ext, 16);
End;

Function BasicCompressorObject. StripPath (F: ComStr): PathStr;
{陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�
  Strip the path and return only the filename.
 陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�}
Begin
  StripPath := JustFilename (F);
End;

{陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�
  SearchBuffer searches a buffer of a certain size for a certain string.
  The Start and stop offset can be given to limit the search range.
  InFile returns the position of the string within the buffer if found.
 陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�}

{$IfNDef UseASM}
Function BasicCompressorObject. SearchBuffer (Var B; Size, Start, Stop: Word;
           Check: String; Var InFile: LongInt): Boolean;
Type
  TC = Array [0..$FFFE] of Char;

Var
  BufPtr : Word;
  Found  : Boolean;
  Ok     : Boolean;
  TmpPtr : Word;

Begin
  BufPtr := Start;
  Found := False;
  While (Not Found) And (BufPtr < Stop) Do
  Begin
  If Check[1]=TC(B)[BufPtr]
     Then Begin
          Ok:=True;
          TmpPtr:=BufPtr+1;
          While Ok And ((TmpPtr-BufPtr)<Length(Check)) Do
            Begin
            Ok:=TC(B)[TmpPtr]=Check[TmpPtr-BufPtr+1];
            Inc(TmpPtr);
            End;
          Found:=Ok;
          End;

  Inc (BufPtr);
  End;
  SearchBuffer:=Found;
  InFile:=BufPtr-1;
End;

{$ELSE}

Function BasicCompressorObject.SearchBuffer (Var B;
                                                 Size  : Word;
                                                 Start : Word;
                                                 Stop  : Word;
                                                 Check : String;
                                             Var InFile: LongInt): Boolean;
                                             External;
{$L SEARCH.OBJ}

{$ENDIF}

Function BasicCompressorObject.IsEXEFile (Var B): Boolean;
{陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�
  Check if the file is an exe file.
 陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�}
Type
  Check = Array [0..1] of Char;

Begin
  isEXEFile := Check(B) = 'MZ';
End;

Function BasicCompressorObject. LongSwap(L: LongInt):LongInt;
{陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�
  Swap a longint from INTEL to MOTEROLA format or vice versa
 陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�}
Type
  TC = Record
         W1,W2 : Word;
       End;

Begin
  LongSwap := (LongInt (SWAP (TC (L). W1)) Shl 16) +
    LongInt (SWAP (TC (L). W2));
End;

{陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�
 Store, set and reset the filemode variable
 陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�}

Procedure BasicCompressorObject.SetFileMode (Mode: Byte);
Begin
  PreviouseMode:=FileMode;
  FileMode:=Mode;
End;

Procedure BasicCompressorObject.ResetFileMode;
Begin
  FileMode:=PreviouseMode;
  PreviouseMode:=DefaultFileMode;
End;

{様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
  The Object list support
 様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�}

Procedure AddToList (P: Pointer);
{陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�
  Add an object to the list.
 陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�}
Begin
  If OPtr < MaxCompressors Then
  Begin
    Inc (OPtr);
    OList [OPtr] := P;
  End;
End;


{$F+}
Procedure MyExitProc;
{陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�
  Dispose the objects in the list. Clean up!
 陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�}
Begin
  ExitProc:=ExitSave;
  While OPtr>0 Do
   Begin
     If OList[OPtr] <> Nil Then
     Begin
       Dispose (OList [OPtr]);
       OLIst [OPtr] := Nil;
     End;
     Dec (OPtr);
   End;
End;
{$F-}

Begin
  ExitSave:=ExitProc;
  ExitProc:=@MyExitProc;   { Install the cleanup procedure in the exitlist }

  OPtr:=0;                 { Init the ObjectList                           }
  FillChar(OList,SizeOf(OList),#00);
End.
