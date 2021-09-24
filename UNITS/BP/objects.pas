Unit Objects;

{$IFDEF MSDOS}
{$F+,X+,I-,S-}

Interface

Const

  { Cyrillics keys flags }

  {Cyrillics : Boolean = True;}

  { TStream access modes }

  stCreate    = $3C00;           { Create new file }
  stOpenRead  = $3D00;           { Read access only }
  stOpenWrite = $3D01;           { Write access only }
  stOpen      = $3D02;           { Read and write access }

  { TStream error codes }

  stOk         =  0;              { No error }
  stError      = - 1;              { Access error }
  stInitError  = - 2;              { Cannot initialize stream }
  stReadError  = - 3;              { Read beyond end of stream }
  stWriteError = - 4;              { Cannot expand stream }
  stGetError   = - 5;              { Get of unregistered object type }
  stPutError   = - 6;              { Put of unregistered object type }

  { Maximum TCollection size }

  MaxCollectionSize = 65520 Div SizeOf (Pointer);

  { TCollection error codes }

  coIndexError = - 1;              { Index out of range }
  coOverflow   = - 2;              { Overflow }

  { VMT header size }

  vmtHeaderSize = 8;

Type

  { Type conversion records }

  WordRec = Record
              Lo, Hi: Byte;
            End;

  LongRec = Record
              Lo, Hi: Word;
            End;

  PtrRec = Record
             Ofs, Seg: Word;
           End;

  { String pointers }

  PString = ^String;

  { Character set type }

  PCharSet = ^TCharSet;
  TCharSet = Set Of Char;

  { General arrays }

  PByteArray = ^TByteArray;
  TByteArray = Array [0..32767] Of Byte;

  PWordArray = ^TWordArray;
  TWordArray = Array [0..16383] Of Word;

  { TObject base object }

  PObject = ^TObject;
  TObject = Object
              Constructor Init;
              Procedure Free;
              Destructor Done; Virtual;
            End;

  { TStreamRec }

  PStreamRec = ^TStreamRec;
  TStreamRec = Record
                 ObjType: Word;
                 VmtLink: Word;
                 Load: Pointer;
                 Store: Pointer;
                 Next: Word;
               End;

  { TStream }

  PStream = ^TStream;
  TStream = Object (TObject)
              Status: Integer;
              ErrorInfo: Integer;
              Constructor Init;
              Procedure CopyFrom (Var S: TStream; Count: LongInt);
              Procedure Error (Code, Info: Integer); Virtual;
              Procedure Flush; Virtual;
              Function Get: PObject;
              Function GetPos: LongInt; Virtual;
              Function GetSize: LongInt; Virtual;
              Procedure Put (P: PObject);
              Procedure Read (Var Buf; Count: Word); Virtual;
              Function ReadStr: PString;
              Procedure Reset;
              Procedure Seek (Pos: LongInt); Virtual;
              Function StrRead: PChar;
              Procedure StrWrite (P: PChar);
              Procedure Truncate; Virtual;
              Procedure Write (Var Buf; Count: Word); Virtual;
              Procedure WriteStr (P: PString);
            End;

  { DOS file name string }

  {$IFDEF Windows}
  FNameStr = PChar;
  {$ELSE}
  FNameStr = String [79];
  {$ENDIF}

  { TDosStream }

  PDosStream = ^TDosStream;
  TDosStream = Object (TStream)
                 Handle: Word;
                 Constructor Init (FileName: FNameStr; Mode: Word);
                 Destructor Done; Virtual;
                 Function GetPos: LongInt; Virtual;
                 Function GetSize: LongInt; Virtual;
                 Procedure Read (Var Buf; Count: Word); Virtual;
                 Procedure Seek (Pos: LongInt); Virtual;
                 Procedure Truncate; Virtual;
                 Procedure Write (Var Buf; Count: Word); Virtual;
                 Procedure ReadBlock (Var Buf; Count: Word; Var BytesRead: Word);
               End;

  { TBufStream }

  PBufStream = ^TBufStream;
  TBufStream = Object (TDosStream)
                 Buffer: Pointer;
                 BufSize: Word;
                 BufPtr: Word;
                 BufEnd: Word;
                 Constructor Init (FileName: FNameStr; Mode, Size: Word);
                 Destructor Done; Virtual;
                 Procedure Flush; Virtual;
                 Function GetPos: LongInt; Virtual;
                 Function GetSize: LongInt; Virtual;
                 Procedure Read (Var Buf; Count: Word); Virtual;
                 Procedure Seek (Pos: LongInt); Virtual;
                 Procedure Truncate; Virtual;
                 Procedure Write (Var Buf; Count: Word); Virtual;
               End;

  { TEmsStream }

  PEmsStream = ^TEmsStream;
  TEmsStream = Object (TStream)
                 Handle: Word;
                 PageCount: Word;
                 Size: LongInt;
                 Position: LongInt;
                 Constructor Init (MinSize, MaxSize: LongInt);
                 Destructor Done; Virtual;
                 Function GetPos: LongInt; Virtual;
                 Function GetSize: LongInt; Virtual;
                 Procedure Read (Var Buf; Count: Word); Virtual;
                 Procedure Seek (Pos: LongInt); Virtual;
                 Procedure Truncate; Virtual;
                 Procedure Write (Var Buf; Count: Word); Virtual;
               End;

  { TMemoryStream }

  PMemoryStream = ^TMemoryStream;
  TMemoryStream = Object (TStream)
                    SegCount: Integer;
                    SegList: PWordArray;
                    CurSeg: Integer;
                    BlockSize: Integer;
                    Size: LongInt;
                    Position: LongInt;
                    Constructor Init (ALimit: LongInt; ABlockSize: Word);
                    Destructor Done; Virtual;
                    Function GetPos: LongInt; Virtual;
                    Function GetSize: LongInt; Virtual;
                    Procedure Read (Var Buf; Count: Word); Virtual;
                    Procedure Seek (Pos: LongInt); Virtual;
                    Procedure Truncate; Virtual;
                    Procedure Write (Var Buf; Count: Word); Virtual;
                    Private
                    Function ChangeListSize (ALimit: Word): Boolean;
                  End;

  { TCollection types }

  PItemList = ^TItemList;
  TItemList = Array [0..MaxCollectionSize - 1] Of Pointer;

  { TCollection object }

  PCollection = ^TCollection;
  TCollection = Object (TObject)
                  Items: PItemList;
                  Count: Integer;
                  Limit: Integer;
                  Delta: Integer;
                  Constructor Init (ALimit, ADelta: Integer);
                  Constructor Load (Var S: TStream);
                  Destructor Done; Virtual;
                  Function At (Index: Integer): Pointer;
                  Procedure AtDelete (Index: Integer);
                  Procedure AtFree (Index: Integer);
                  Procedure AtInsert (Index: Integer; Item: Pointer);
                  Procedure AtPut (Index: Integer; Item: Pointer);
                  Procedure Delete (Item: Pointer);
                  Procedure DeleteAll;
                  Procedure Error (Code, Info: Integer); Virtual;
                  Function FirstThat (Test: Pointer): Pointer;
                  Procedure ForEach (Action: Pointer);
                  Procedure Free (Item: Pointer);
                  Procedure FreeAll;
                  Procedure FreeItem (Item: Pointer); Virtual;
                  Function GetItem (Var S: TStream): Pointer; Virtual;
                  Function IndexOf (Item: Pointer): Integer; Virtual;
                  Procedure Insert (Item: Pointer); Virtual;
                  Function LastThat (Test: Pointer): Pointer;
                  Procedure Pack;
                  Procedure PutItem (Var S: TStream; Item: Pointer); Virtual;
                  Procedure SetLimit (ALimit: Integer); Virtual;
                  Procedure Store (Var S: TStream);
                End;

  { TSortedCollection object }

  PSortedCollection = ^TSortedCollection;
  TSortedCollection = Object (TCollection)
                        Duplicates: Boolean;
                        Constructor Init (ALimit, ADelta: Integer);
                        Constructor Load (Var S: TStream);
                        Function Compare (Key1, Key2: Pointer): Integer; Virtual;
                        Function IndexOf (Item: Pointer): Integer; Virtual;
                        Procedure Insert (Item: Pointer); Virtual;
                        Function KeyOf (Item: Pointer): Pointer; Virtual;
                        Function Search (Key: Pointer; Var Index: Integer): Boolean; Virtual;
                        Procedure Store (Var S: TStream);
                      End;

  { TStringCollection object }

  PStringCollection = ^TStringCollection;
  TStringCollection = Object (TSortedCollection)
                        Function Compare (Key1, Key2: Pointer): Integer; Virtual;
                        Procedure FreeItem (Item: Pointer); Virtual;
                        Function GetItem (Var S: TStream): Pointer; Virtual;
                        Procedure PutItem (Var S: TStream; Item: Pointer); Virtual;
                      End;

  { TStrCollection object }

  PStrCollection = ^TStrCollection;
  TStrCollection = Object (TSortedCollection)
                     Function Compare (Key1, Key2: Pointer): Integer; Virtual;
                     Procedure FreeItem (Item: Pointer); Virtual;
                     Function GetItem (Var S: TStream): Pointer; Virtual;
                     Procedure PutItem (Var S: TStream; Item: Pointer); Virtual;
                   End;

  {$IFNDEF Windows}

  { TResourceCollection object }

  PResourceCollection = ^TResourceCollection;
  TResourceCollection = Object (TStringCollection)
                          Procedure FreeItem (Item: Pointer); Virtual;
                          Function GetItem (Var S: TStream): Pointer; Virtual;
                          Function KeyOf (Item: Pointer): Pointer; Virtual;
                          Procedure PutItem (Var S: TStream; Item: Pointer); Virtual;
                        End;

  { TResourceFile object }

  PResourceFile = ^TResourceFile;
  TResourceFile = Object (TObject)
                    Stream: PStream;
                    Modified: Boolean;
                    Constructor Init (AStream: PStream);
                    Destructor Done; Virtual;
                    Function Count: Integer;
                    Procedure Delete (Key: String);
                    Procedure Flush;
                    Function Get (Key: String): PObject;
                    Function KeyAt (I: Integer): String;
                    Procedure Put (Item: PObject; Key: String);
                    Function SwitchTo (AStream: PStream; Pack: Boolean): PStream;
                    Private
                    BasePos: LongInt;
                    IndexPos: LongInt;
                    Index: TResourceCollection;
                  End;

  { TStringList object }

  TStrIndexRec = Record
                   Key, Count, Offset: Word;
                 End;

  PStrIndex = ^TStrIndex;
  TStrIndex = Array [0..9999] Of TStrIndexRec;

  PStringList = ^TStringList;
  TStringList = Object (TObject)
                  Constructor Load (Var S: TStream);
                  Destructor Done; Virtual;
                  Function Get (Key: Word): String;
                  Private
                  Stream: PStream;
                  BasePos: LongInt;
                  IndexSize: Integer;
                  Index: PStrIndex;
                  Procedure ReadStr (Var S: String; Offset, Skip: Word);
                End;

  { TStrListMaker object }

  PStrListMaker = ^TStrListMaker;
  TStrListMaker = Object (TObject)
                    Constructor Init (AStrSize, AIndexSize: Word);
                    Destructor Done; Virtual;
                    Procedure Put (Key: Word; S: String);
                    Procedure Store (Var S: TStream);
                    Private
                    StrPos: Word;
                    StrSize: Word;
                    Strings: PByteArray;
                    IndexPos: Word;
                    IndexSize: Word;
                    Index: PStrIndex;
                    Cur: TStrIndexRec;
                    Procedure CloseCurrent;
                  End;

  { TPoint object }

  TPoint = Object
             X, Y: Integer;
           End;

  { Rectangle object }

  TRect = Object
            A, B: TPoint;
            Procedure Assign (XA, YA, XB, YB: Integer);
            Procedure Copy (R: TRect);
            Procedure Move (ADX, ADY: Integer);
            Procedure Grow (ADX, ADY: Integer);
            Procedure Intersect (R: TRect);
            Procedure Union (R: TRect);
            Function Contains (P: TPoint): Boolean;
            Function Equals (R: TRect): Boolean;
            Function Empty: Boolean;
          End;

  {$ENDIF}

  { Dynamic string handling routines }

Function NewStr (Const S: String): PString;
Procedure DisposeStr (P: PString);

{ Longint routines }

Function LongMul (X, Y: Integer): LongInt;
Inline ($5A / $58 / $F7 / $EA);

Function LongDiv (X: LongInt; Y: Integer): Integer;
Inline ($59 / $58 / $5A / $F7 / $F9);

{ Stream routines }

Procedure RegisterType (Var S: TStreamRec);

{ Abstract notification procedure }

Procedure Abstract;

{ Objects registration procedure }

Procedure RegisterObjects;

Const

  { Stream error procedure }

  StreamError: Pointer = Nil;

  { EMS stream state variables }

  EmsCurHandle: Word = $FFFF;
  EmsCurPage: Word = $FFFF;

  { Stream registration records }

Const
  RCollection: TStreamRec = (
  ObjType: 50;
  VmtLink: Ofs (TypeOf (TCollection)^);
  Load: @TCollection. Load;
  Store: @TCollection. Store);

Const
  RStringCollection: TStreamRec = (
  ObjType: 51;
  VmtLink: Ofs (TypeOf (TStringCollection)^);
  Load: @TStringCollection. Load;
  Store: @TStringCollection. Store);

Const
  RStrCollection: TStreamRec = (
  ObjType: 69;
  VmtLink: Ofs (TypeOf (TStrCollection)^);
  Load:    @TStrCollection. Load;
  Store:   @TStrCollection. Store);

  {$IFNDEF Windows }

Const
  RStringList: TStreamRec = (
  ObjType: 52;
  VmtLink: Ofs (TypeOf (TStringList)^);
  Load: @TStringList. Load;
  Store: Nil);

Const
  RStrListMaker: TStreamRec = (
  ObjType: 52;
  VmtLink: Ofs (TypeOf (TStrListMaker)^);
  Load: Nil;
  Store: @TStrListMaker. Store);

  {$ENDIF}

Implementation

{$IFDEF Windows}
Uses WinProcs, Strings, OMemory;
{$ELSE}
Uses Memory, Strings;
{$ENDIF}

{$IFDEF Windows}
  {$DEFINE NewExeFormat}
{$ENDIF}

{$IFDEF DPMI}
  {$DEFINE NewExeFormat}
{$ENDIF}

Procedure Abstract;
Begin
  RunError (211);
End;

{ TObject }

Constructor TObject. Init;
Type
  Image = Record
            Link: Word;
            Data: Record End;
          End;
Begin
  {$IFNDEF Windows}
  FillChar (Image (Self).Data, SizeOf (Self) - SizeOf (TObject), 0);
  {$ENDIF}
End;

{ Shorthand procedure for a done/dispose }

Procedure TObject. Free;
Begin
  Dispose (PObject (@Self), Done);
End;

Destructor TObject. Done;
Begin
End;

{ TStream type registration routines }

Const
  StreamTypes: Word = 0;

Procedure RegisterError;
Begin
  RunError (212);
End;

Procedure RegisterType (Var S: TStreamRec); Assembler;
Asm
  MOV     AX, DS
  CMP     AX, S. Word [2]
  JNE     @@1
  MOV     SI, S. Word [0]
  MOV     AX, [SI].TStreamRec. ObjType
  Or      AX, AX
  JE      @@1
  MOV     DI, StreamTypes
  MOV     [SI].TStreamRec. Next, DI
  JMP     @@3
  @@1:    JMP     RegisterError
  @@2:    CMP     AX, [DI].TStreamRec. ObjType
  JE      @@1
  MOV     DI, [DI].TStreamRec. Next
  @@3:    Or      DI, DI
  JNE     @@2
  MOV     StreamTypes, SI
End;

{ TStream support routines }

Const
  TStream_Error = vmtHeaderSize + $04;
  TStream_Flush = vmtHeaderSize + $08;
  TStream_Read  = vmtHeaderSize + $14;
  TStream_Write = vmtHeaderSize + $20;

  { Stream error handler                                  }
  { In    AX    = Error info                              }
  {       DX    = Error code                              }
  {       ES:DI = Stream object pointer                   }
  { Uses  AX,BX,CX,DX,SI                                  }

Procedure DoStreamError; Near; Assembler;
Asm
  PUSH    ES
  PUSH    DI
  PUSH    DX
  PUSH    AX
  PUSH    ES
  PUSH    DI
  MOV     DI, ES: [DI]
  Call    DWord Ptr [DI].TStream_Error
  POP     DI
  POP     ES
End;

{ TStream }

Constructor TStream. Init;
Begin
  TObject. Init;
  Status := 0;
  ErrorInfo := 0;
End;

Procedure TStream. CopyFrom (Var S: TStream; Count: LongInt);
Var
  N: Word;
  Buffer: Array [0..1023] Of Byte;
Begin
  While Count > 0 Do
  Begin
    If Count > SizeOf (Buffer) Then N := SizeOf (Buffer) Else N := Count;
    S. Read (Buffer, N);
    Write (Buffer, N);
    Dec (Count, N);
  End;
End;

Procedure TStream. Error (Code, Info: Integer);
Type
  TErrorProc =
Procedure (Var S: TStream);
Begin
  Status := Code;
  ErrorInfo := Info;
  If StreamError <> Nil Then TErrorProc (StreamError) (Self);
End;

Procedure TStream. Flush;
Begin
End;

Function TStream. Get: PObject; Assembler;
Asm
  PUSH    AX
  MOV     AX, SP
  PUSH    SS
  PUSH    AX
  MOV     AX, 2
  PUSH    AX
  LES     DI, Self
  PUSH    ES
  PUSH    DI
  MOV     DI, ES: [DI]
  Call    DWord Ptr [DI].TStream_Read
  POP     AX
  Or      AX, AX
  JE      @@3
  MOV     BX, StreamTypes
  JMP     @@2
  @@1:    CMP     AX, [BX].TStreamRec. ObjType
  JE      @@4
  MOV     BX, [BX].TStreamRec. Next
  @@2:    Or      BX, BX
  JNE     @@1
  LES     DI, Self
  MOV     DX, stGetError
  Call    DoStreamError
  @@3:    XOr     AX, AX
  MOV     DX, AX
  JMP     @@5
  @@4:    LES     DI, Self
  PUSH    ES
  PUSH    DI
  PUSH    [BX].TStreamRec. VmtLink
  XOr     AX, AX
  PUSH    AX
  PUSH    AX
  Call    [BX].TStreamRec. Load
  @@5:
End;

Function TStream. GetPos: LongInt;
Begin
  Abstract;
End;

Function TStream. GetSize: LongInt;
Begin
  Abstract;
End;

Procedure TStream. Put (P: PObject); Assembler;
Asm
  LES     DI, P
  MOV     CX, ES
  Or      CX, DI
  JE      @@4
  MOV     AX, ES: [DI]
  MOV     BX, StreamTypes
  JMP     @@2
  @@1:    CMP     AX, [BX].TStreamRec. VmtLink
  JE      @@3
  MOV     BX, [BX].TStreamRec. Next
  @@2:    Or      BX, BX
  JNE     @@1
  LES     DI, Self
  MOV     DX, stPutError
  Call    DoStreamError
  JMP     @@5
  @@3:    MOV     CX, [BX].TStreamRec. ObjType
  @@4:    PUSH    BX
  PUSH    CX
  MOV     AX, SP
  PUSH    SS
  PUSH    AX
  MOV     AX, 2
  PUSH    AX
  LES     DI, Self
  PUSH    ES
  PUSH    DI
  MOV     DI, ES: [DI]
  Call    DWord Ptr [DI].TStream_Write
  POP     CX
  POP     BX
  JCXZ    @@5
  LES     DI, Self
  PUSH    ES
  PUSH    DI
  PUSH    P. Word [2]
  PUSH    P. Word [0]
  Call    [BX].TStreamRec. Store
  @@5:
End;

Procedure TStream. Read (Var Buf; Count: Word);
Begin
  Abstract;
End;

Function TStream. ReadStr: PString;
Var
  L: Byte;
  P: PString;
Begin
  Read (L, 1);
  If L > 0 Then
  Begin
    GetMem (P, L + 1);
    P^ [0] := Char (L);
    Read (P^ [1], L);
    ReadStr := P;
  End Else ReadStr := Nil;
End;

Procedure TStream. Reset;
Begin
  Status := 0;
  ErrorInfo := 0;
End;

Procedure TStream. Seek (Pos: LongInt);
Begin
  Abstract;
End;

Function TStream. StrRead: PChar;
Var
  L: Word;
  P: PChar;
Begin
  Read (L, SizeOf (Word));
  If L = 0 Then StrRead := Nil Else
  Begin
    GetMem (P, L + 1);
    Read (P [0], L);
    P [L] := #0;
    StrRead := P;
  End;
End;

Procedure TStream. StrWrite (P: PChar);
Var
  L: Word;
Begin
  If P = Nil Then L := 0 Else L := StrLen (P);
  Write (L, SizeOf (Word));
  If P <> Nil Then Write (P [0], L);
End;

Procedure TStream. Truncate;
Begin
  Abstract;
End;

Procedure TStream. Write (Var Buf; Count: Word);
Begin
  Abstract;
End;

Procedure TStream. WriteStr (P: PString);
Const
  Empty: String [1] = '';
Begin
  If P <> Nil Then Write (P^, Length (P^) + 1) Else Write (Empty, 1);
End;

{ TDosStream }

Constructor TDosStream. Init (FileName: FNameStr; Mode: Word); Assembler;
Var
  NameBuf: Array [0..79] Of Char;
  Asm
    XOr     AX, AX
    PUSH    AX
    LES     DI, Self
    PUSH    ES
    PUSH    DI
    Call    TStream. Init
    {$IFDEF Windows}
    LEA DI, NameBuf
    PUSH        SS
    PUSH        DI
    LES DI, FileName
    PUSH        ES
    PUSH        DI
    MOV AX, 79
    PUSH        AX
    Call        StrLCopy
    PUSH        DX
    PUSH        AX
    PUSH        DX
    PUSH        AX
    Call        AnsiToOem
    PUSH        DS
    LEA DX, NameBuf
    {$ELSE}
    PUSH    DS
    LDS     SI, FileName
    LEA     DI, NameBuf
    MOV     DX, DI
    PUSH    SS
    POP     ES
    CLD
    LODSB
    CMP     AL, 79
    JB      @@1
    MOV     AL, 79
    @@1:    CBW
    XCHG    AX, CX
    REP     MOVSB
    XCHG    AX, CX
    STOSB
    {$ENDIF}
    PUSH    SS
    POP     DS
    XOr     CX, CX
    MOV     AX, Mode
    Int     21H
    POP     DS
    JNC     @@2
    LES     DI, Self
    MOV     DX, stInitError
    Call    DoStreamError
    MOV     AX, - 1
    @@2:    LES     DI, Self
    MOV     ES: [DI].TDosStream. Handle, AX
  End;

  Destructor TDosStream. Done; Assembler;
  Asm
    LES     DI, Self
    MOV     BX, ES: [DI].TDosStream. Handle
    CMP     BX, - 1
    JE      @@1
    MOV     AH, 3EH
    Int     21H
    @@1:    XOr     AX, AX
    PUSH    AX
    PUSH    ES
    PUSH    DI
    Call    TStream. Done
  End;

Function TDosStream. GetPos: LongInt; Assembler;
Asm
  LES     DI, Self
  XOr     DX, DX
  CMP     DX, ES: [DI].TDosStream. Status
  JNE     @@1
  MOV     CX, DX
  MOV     BX, ES: [DI].TDosStream. Handle
  MOV     AX, 4201H
  Int     21H
  JNC     @@2
  MOV     DX, stError
  Call    DoStreamError
  @@1:    MOV     AX, - 1
  CWD
  @@2:
End;

Function TDosStream. GetSize: LongInt; Assembler;
Asm
  LES     DI, Self
  XOr     DX, DX
  CMP     DX, ES: [DI].TDosStream. Status
  JNE     @@1
  MOV     CX, DX
  MOV     BX, ES: [DI].TDosStream. Handle
  MOV     AX, 4201H
  Int     21H
  PUSH    DX
  PUSH    AX
  XOr     DX, DX
  MOV     CX, DX
  MOV     AX, 4202H
  Int     21H
  POP     SI
  POP     CX
  PUSH    DX
  PUSH    AX
  MOV     DX, SI
  MOV     AX, 4200H
  Int     21H
  POP     AX
  POP     DX
  JNC     @@2
  MOV     DX, stError
  Call    DoStreamError
  @@1:    MOV     AX, - 1
  CWD
  @@2:
End;

Procedure TDosStream. Read (Var Buf; Count: Word); Assembler;
Asm
  LES     DI, Self
  CMP     ES: [DI].TDosStream. Status, 0
  JNE     @@2
  PUSH    DS
  LDS     DX, Buf
  MOV     CX, Count
  MOV     BX, ES: [DI].TDosStream. Handle
  MOV     AH, 3FH
  Int     21H
  POP     DS
  MOV     DX, stError
  JC      @@1
  CMP     AX, CX
  JE      @@3
  XOr     AX, AX
  MOV     DX, stReadError
  @@1:    Call    DoStreamError
  @@2:    LES     DI, Buf
  MOV     CX, Count
  XOr     AL, AL
  CLD
  REP     STOSB
  @@3:
End;

Procedure TDOSStream. ReadBlock (Var Buf; Count: Word; Var BytesRead: Word); Assembler;
Asm
  LES     DI,Self
  CMP     ES:[DI].TDosStream.Status,0
  JNE     @@2
  PUSH    DS
  LDS     DX,Buf
  MOV     CX,Count
  MOV     BX,ES:[DI].TDosStream.Handle
  MOV     AH,3FH
  INT     21H
  POP     DS
  MOV     DX,stError
  JC      @@1
  PUSH    ES
  LES     BX,BytesRead
  MOV     ES:[BX],AX
  POP     ES
  JMP     @@3
@@1:
  CALL    DoStreamError
@@2:
  LES     DI,Buf
  MOV     CX,Count
  XOR     AL,AL
  CLD
  REP     STOSB
@@3:
End;

Procedure TDosStream. Seek (Pos: LongInt); Assembler;
Asm
  LES     DI, Self
  CMP     ES: [DI].TDosStream. Status, 0
  JNE     @@2
  MOV     DX, Pos. Word [0]
  MOV     CX, Pos. Word [2]
  Or      CX, CX
  JNS     @@1
  XOr     DX, DX
  XOr     CX, CX
  @@1:    MOV     BX, ES: [DI].TDosStream. Handle
  MOV     AX, 4200H
  Int     21H
  JNC     @@2
  MOV     DX, stError
  Call    DoStreamError
  @@2:
End;

Procedure TDosStream. Truncate; Assembler;
Asm
  LES     DI, Self
  XOr     CX, CX
  CMP     CX, ES: [DI].TDosStream. Status
  JNE     @@1
  MOV     BX, ES: [DI].TDosStream. Handle
  MOV     AH, 40H
  Int     21H
  JNC     @@1
  MOV     DX, stError
  Call    DoStreamError
  @@1:
End;

Procedure TDosStream. Write (Var Buf; Count: Word); Assembler;
Asm
  LES     DI, Self
  CMP     ES: [DI].TDosStream. Status, 0
  JNE     @@2
  PUSH    DS
  LDS     DX, Buf
  MOV     CX, Count
  MOV     BX, ES: [DI].TDosStream. Handle
  MOV     AH, 40H
  Int     21H
  POP     DS
  MOV     DX, stError
  JC      @@1
  CMP     AX, CX
  JE      @@2
  XOr     AX, AX
  MOV     DX, stWriteError
  @@1:    Call    DoStreamError
  @@2:
End;

{ TBufStream }

{ Flush TBufStream buffer                               }
{ In    AL    = Flush mode (0=Read, 1=Write, 2=Both)    }
{       ES:DI = TBufStream pointer                      }
{ Out   ZF    = Status test                             }

Procedure FlushBuffer; Near; Assembler;
Asm
  MOV     CX, ES: [DI].TBufStream. BufPtr
  SUB     CX, ES: [DI].TBufStream. BufEnd
  JE      @@3
  MOV     BX, ES: [DI].TDosStream. Handle
  JA      @@1
  CMP     AL, 1
  JE      @@4
  MOV     DX, CX
  MOV     CX, - 1
  MOV     AX, 4201H
  Int     21H
  JMP     @@3
  @@1:    CMP     AL, 0
  JE      @@4
  PUSH    DS
  LDS     DX, ES: [DI].TBufStream. Buffer
  MOV     AH, 40H
  Int     21H
  POP     DS
  MOV     DX, stError
  JC      @@2
  CMP     AX, CX
  JE      @@3
  XOr     AX, AX
  MOV     DX, stWriteError
  @@2:    Call    DoStreamError
  @@3:    XOr     AX, AX
  MOV     ES: [DI].TBufStream. BufPtr, AX
  MOV     ES: [DI].TBufStream. BufEnd, AX
  CMP     AX, ES: [DI].TStream. Status
  @@4:
End;

Constructor TBufStream. Init (FileName: FNameStr; Mode, Size: Word);
Begin
  TDosStream. Init (FileName, Mode);
  BufSize := Size;
  If Size = 0 Then Error (stInitError, 0)
  Else GetMem (Buffer, Size);
  BufPtr := 0;
  BufEnd := 0;
End;

Destructor TBufStream. Done;
Begin
  TBufStream. Flush;
  TDosStream. Done;
  FreeMem (Buffer, BufSize);
End;

Procedure TBufStream. Flush; Assembler;
Asm
  LES     DI, Self
  CMP     ES: [DI].TBufStream. Status, 0
  JNE     @@1
  MOV     AL, 2
  Call    FlushBuffer
  @@1:
End;

Function TBufStream. GetPos: LongInt; Assembler;
Asm
  LES     DI, Self
  PUSH    ES
  PUSH    DI
  Call    TDosStream. GetPos
  Or      DX, DX
  JS      @@1
  LES     DI, Self
  SUB     AX, ES: [DI].TBufStream. BufEnd
  SBB     DX, 0
  ADD     AX, ES: [DI].TBufStream. BufPtr
  ADC     DX, 0
  @@1:
End;

Function TBufStream. GetSize: LongInt; Assembler;
Asm
  LES     DI, Self
  PUSH    ES
  PUSH    DI
  PUSH    ES
  PUSH    DI
  Call    TBufStream. Flush
  Call    TDosStream. GetSize
End;

Procedure TBufStream. Read (Var Buf; Count: Word); Assembler;
Asm
  LES     DI, Self
  CMP     ES: [DI].TBufStream. Status, 0
  JNE     @@6
  MOV     AL, 1
  Call    FlushBuffer
  JNE     @@6
  XOr     BX, BX
  @@1:    MOV     CX, Count
  SUB     CX, BX
  JE      @@7
  LES     DI, Self
  MOV     AX, ES: [DI].TBufStream. BufEnd
  SUB     AX, ES: [DI].TBufStream. BufPtr
  JA      @@2
  PUSH    DS
  PUSH    CX
  PUSH    BX
  LDS     DX, ES: [DI].TBufStream. Buffer
  MOV     CX, ES: [DI].TBufStream. BufSize
  MOV     BX, ES: [DI].TBufStream. Handle
  MOV     AH, 3FH
  Int     21H
  POP     BX
  POP     CX
  POP     DS
  MOV     DX, stError
  JC      @@5
  MOV     ES: [DI].TBufStream. BufPtr, 0
  MOV     ES: [DI].TBufStream. BufEnd, AX
  Or      AX, AX
  JE      @@4
  @@2:    CMP     CX, AX
  JB      @@3
  MOV     CX, AX
  @@3:    PUSH    DS
  LDS     SI, ES: [DI].TBufStream. Buffer
  ADD     SI, ES: [DI].TBufStream. BufPtr
  ADD     ES: [DI].TBufStream. BufPtr, CX
  LES     DI, Buf
  ADD     DI, BX
  ADD     BX, CX
  CLD
  REP     MOVSB
  POP     DS
  JMP     @@1
  @@4:    MOV     DX, stReadError
  @@5:    Call    DoStreamError
  @@6:    LES     DI, Buf
  MOV     CX, Count
  XOr     AL, AL
  CLD
  REP     STOSB
  @@7:
End;

Procedure TBufStream. Seek (Pos: LongInt); Assembler;
Asm
  LES     DI, Self
  PUSH    ES
  PUSH    DI
  Call    TDosStream. GetPos
  Or      DX, DX
  JS      @@2
  LES     DI, Self
  SUB     AX, Pos. Word [0]
  SBB     DX, Pos. Word [2]
  JNE     @@1
  Or      AX, AX
  JE      @@1
  MOV     DX, ES: [DI].TBufStream. BufEnd
  SUB     DX, AX
  JB      @@1
  MOV     ES: [DI].TBufStream. BufPtr, DX
  JMP     @@2
  @@1:    PUSH    Pos. Word [2]
  PUSH    Pos. Word [0]
  PUSH    ES
  PUSH    DI
  PUSH    ES
  PUSH    DI
  Call    TBufStream. Flush
  Call    TDosStream. Seek
  @@2:
End;

Procedure TBufStream. Truncate;
Begin
  TBufStream. Flush;
  TDosStream. Truncate;
End;

Procedure TBufStream. Write (Var Buf; Count: Word); Assembler;
Asm
  LES     DI, Self
  CMP     ES: [DI].TBufStream. Status, 0
  JNE     @@4
  MOV     AL, 0
  Call    FlushBuffer
  JNE     @@4
  XOr     DX, DX
  @@1:    MOV     CX, Count
  SUB     CX, DX
  JE      @@4
  LES     DI, Self
  MOV     AX, ES: [DI].TBufStream. BufSize
  SUB     AX, ES: [DI].TBufStream. BufPtr
  JA      @@2
  PUSH    CX
  PUSH    DX
  MOV     AL, 1
  Call    FlushBuffer
  POP     DX
  POP     CX
  JNE     @@4
  MOV     AX, ES: [DI].TBufStream. BufSize
  @@2:    CMP     CX, AX
  JB      @@3
  MOV     CX, AX
  @@3:    PUSH    DS
  MOV     AX, ES: [DI].TBufStream. BufPtr
  ADD     ES: [DI].TBufStream. BufPtr, CX
  LES     DI, ES: [DI].TBufStream. Buffer
  ADD     DI, AX
  LDS     SI, Buf
  ADD     SI, DX
  ADD     DX, CX
  CLD
  REP     MOVSB
  POP     DS
  JMP     @@1
  @@4:
End;

{ TEmsStream }

Const
  EmsPageSize = $4000;

Var
  EmsBaseSeg: Word;
  EmsVersion: Byte;

Procedure EmsSelectPage; Near; Assembler;
Asm
  MOV     AX, ES: [DI].TEmsStream. Position. Word [0]
  MOV     DX, ES: [DI].TEmsStream. Position. Word [2]
  MOV     CX, EmsPageSize
  Div     CX
  SUB     CX, DX
  MOV     SI, DX
  MOV     DX, ES: [DI].TEmsStream. Handle
  CMP     DX, EmsCurHandle
  JNE     @@1
  CMP     AX, EmsCurPage
  JE      @@3
  @@1:    MOV     BX, AX
  MOV     AX, 4400H
  Int     67H
  MOV     AL, AH
  And     AX, 0FFH
  JE      @@2
  MOV     DX, stError
  JMP     @@3
  @@2:    MOV     EmsCurHandle, DX
  MOV     EmsCurPage, BX
  @@3:
End;

Procedure EmsSetPages; Near; Assembler;
Asm
  CMP     EmsVersion, 40H
  JAE     @@1
  MOV     AX, 84H
  JMP     @@2
  @@1:    MOV     DX, ES: [DI].TEmsStream. Handle
  MOV     BX, AX
  MOV     AH, 51H
  Int     67H
  MOV     AL, AH
  And     AX, 0FFH
  JNE     @@2
  MOV     ES: [DI].TEmsStream. PageCount, BX
  @@2:
End;

Constructor TEmsStream. Init (MinSize, MaxSize: LongInt); Assembler;
Const
  EmsDeviceLen = 8;
  EmsDeviceStr: Array [1..EmsDeviceLen] Of Char = 'EMMXXXX0';
  Asm
    XOr     AX, AX
    PUSH    AX
    LES     DI, Self
    PUSH    ES
    PUSH    DI
    Call    TStream. Init
    MOV     AX, 3567H
    Int     21H
    MOV     CX, EmsDeviceLen
    MOV     SI, Offset EmsDeviceStr
    MOV     DI, 0AH
    CLD
    REP     CMPSB
    LES     DI, Self
    MOV     AX, - 1
    JNE     @@3
    MOV     AH, 41H
    Int     67H
    MOV     EmsBaseSeg, BX
    MOV     AH, 46H
    Int     67H
    MOV     EmsVersion, AL
    MOV     CX, EmsPageSize
    MOV     AX, MinSize. Word [0]
    MOV     DX, MinSize. Word [2]
    ADD     AX, EmsPageSize - 1
    ADC     DX, 0
    Div     CX
    MOV     BX, AX
    CMP     EmsVersion, 40H
    JAE     @@2
    PUSH    AX
    MOV     AX, MaxSize. Word [0]
    MOV     DX, MaxSize. Word [2]
    ADD     AX, EmsPageSize - 1
    ADC     DX, 0
    Div     CX
    MOV     CX, AX
    MOV     AH, 42H
    Int     67H
    POP     AX
    CMP     BX, CX
    JB      @@1
    MOV     BX, CX
    @@1:    CMP     BX, AX
    JA      @@2
    MOV     BX, AX
    @@2:    MOV     AH, 43H
    Int     67H
    MOV     AL, AH
    And     AX, 0FFH
    JE      @@4
    @@3:    MOV     DX, stInitError
    Call    DoStreamError
    MOV     DX, - 1
    XOr     BX, BX
    @@4:    MOV     ES: [DI].TEmsStream. Handle, DX
    MOV     ES: [DI].TEmsStream. PageCount, BX
    XOr AX, AX
    ADD DI, Offset TEmsStream. Size
    MOV CX, 4
    REP STOSW
  End;

  Destructor TEmsStream. Done; Assembler;
  Asm
    LES     DI, Self
    MOV     DX, ES: [DI].TEmsStream. Handle
    CMP     DX, - 1
    JE      @@1
    MOV     AH, 45H
    Int     67H
    @@1:    XOr     AX, AX
    PUSH    AX
    PUSH    ES
    PUSH    DI
    Call    TStream. Done
  End;

Function TEmsStream. GetPos: LongInt; Assembler;
Asm
  LES     DI, Self
  CMP     ES: [DI].TEmsStream. Status, 0
  JNE     @@1
  MOV     AX, ES: [DI].TEmsStream. Position. Word [0]
  MOV     DX, ES: [DI].TEmsStream. Position. Word [2]
  JMP     @@2
  @@1:    MOV     AX, - 1
  CWD
  @@2:
End;

Function TEmsStream. GetSize: LongInt; Assembler;
Asm
  LES     DI, Self
  CMP     ES: [DI].TEmsStream. Status, 0
  JNE     @@1
  MOV     AX, ES: [DI].TEmsStream. Size. Word [0]
  MOV     DX, ES: [DI].TEmsStream. Size. Word [2]
  JMP     @@2
  @@1:    MOV     AX, - 1
  CWD
  @@2:
End;

Procedure TEmsStream. Read (Var Buf; Count: Word); Assembler;
Asm
  LES     DI, Self
  XOr     BX, BX
  CMP     BX, ES: [DI].TEmsStream. Status
  JNE     @@3
  MOV     AX, ES: [DI].TEmsStream. Position. Word [0]
  MOV     DX, ES: [DI].TEmsStream. Position. Word [2]
  ADD     AX, Count
  ADC     DX, BX
  CMP     DX, ES: [DI].TEmsStream. Size. Word [2]
  JA      @@1
  JB      @@7
  CMP     AX, ES: [DI].TEmsStream. Size. Word [0]
  JBE     @@7
  @@1:    XOr     AX, AX
  MOV     DX, stReadError
  @@2:    Call    DoStreamError
  @@3:    LES     DI, Buf
  MOV     CX, Count
  XOr     AL, AL
  CLD
  REP     STOSB
  JMP     @@8
  @@5:    PUSH    BX
  Call    EmsSelectPage
  POP     BX
  JNE     @@2
  MOV     AX, Count
  SUB     AX, BX
  CMP     CX, AX
  JB      @@6
  MOV     CX, AX
  @@6:    ADD     ES: [DI].TEmsStream. Position. Word [0], CX
  ADC     ES: [DI].TEmsStream. Position. Word [2], 0
  PUSH    ES
  PUSH    DS
  PUSH    DI
  LES     DI, Buf
  ADD     DI, BX
  ADD     BX, CX
  MOV     DS, EmsBaseSeg
  CLD
  REP     MOVSB
  POP     DI
  POP     DS
  POP     ES
  @@7:    CMP     BX, Count
  JB      @@5
  @@8:
End;

Procedure TEmsStream. Seek (Pos: LongInt); Assembler;
Asm
  LES     DI, Self
  MOV     AX, Pos. Word [0]
  MOV     DX, Pos. Word [2]
  Or      DX, DX
  JNS     @@1
  XOr     AX, AX
  CWD
  @@1:    MOV     ES: [DI].TEmsStream. Position. Word [0], AX
  MOV     ES: [DI].TEmsStream. Position. Word [2], DX
End;

Procedure TEmsStream. Truncate; Assembler;
Asm
  LES     DI, Self
  XOr     BX, BX
  CMP     ES: [DI].TEmsStream. Status, BX
  JNE     @@2
  CMP     EmsVersion, 40H
  JB      @@1
  MOV     AX, ES: [DI].TEmsStream. Position. Word [0]
  MOV     DX, ES: [DI].TEmsStream. Position. Word [2]
  ADD     AX, EmsPageSize - 1
  ADC     DX, BX
  MOV     CX, EmsPageSize
  Div     CX
  Call    EmsSetPages
  JE      @@1
  MOV     DX, stError
  Call    DoStreamError
  JMP     @@2
  @@1:    MOV     AX, ES: [DI].TEmsStream. Position. Word [0]
  MOV     DX, ES: [DI].TEmsStream. Position. Word [2]
  MOV     ES: [DI].TEmsStream. Size. Word [0], AX
  MOV     ES: [DI].TEmsStream. Size. Word [2], DX
  @@2:
End;

Procedure TEmsStream. Write (Var Buf; Count: Word); Assembler;
Asm
  LES     DI, Self
  XOr     BX, BX
  CMP     BX, ES: [DI].TEmsStream. Status
  JNE     @@7
  MOV     AX, ES: [DI].TEmsStream. Position. Word [0]
  MOV     DX, ES: [DI].TEmsStream. Position. Word [2]
  ADD     AX, Count
  ADC     DX, BX
  ADD     AX, EmsPageSize - 1
  ADC     DX, BX
  MOV     CX, EmsPageSize
  Div     CX
  CMP     AX, ES: [DI].TEmsStream. PageCount
  JBE     @@4
  PUSH    BX
  Call    EmsSetPages
  POP     BX
  JE      @@4
  @@1:    MOV     DX, stWriteError
  Call    DoStreamError
  JMP     @@7
  @@2:    PUSH    BX
  Call    EmsSelectPage
  POP     BX
  JNE     @@1
  MOV     AX, Count
  SUB     AX, BX
  CMP     CX, AX
  JB      @@3
  MOV     CX, AX
  @@3:    ADD     ES: [DI].TEmsStream. Position. Word [0], CX
  ADC     ES: [DI].TEmsStream. Position. Word [2], 0
  PUSH    ES
  PUSH    DS
  PUSH    DI
  MOV     DI, SI
  MOV     ES, EmsBaseSeg
  LDS     SI, Buf
  ADD     SI, BX
  ADD     BX, CX
  CLD
  REP     MOVSB
  POP     DI
  POP     DS
  POP     ES
  @@4:    CMP     BX, Count
  JB      @@2
  @@5:    MOV     AX, ES: [DI].TEmsStream. Position. Word [0]
  MOV     DX, ES: [DI].TEmsStream. Position. Word [2]
  CMP     DX, ES: [DI].TEmsStream. Size. Word [2]
  JB      @@7
  JA      @@6
  CMP     AX, ES: [DI].TEmsStream. Size. Word [0]
  JBE     @@7
  @@6:    MOV     ES: [DI].TEmsStream. Size. Word [0], AX
  MOV     ES: [DI].TEmsStream. Size. Word [2], DX
  @@7:
End;

{ TMemoryStream }

Const
  MaxSegArraySize = 16384;

  {$IFDEF NewExeFormat}

  DefaultBlockSize = $2000;

  {$ELSE}

  DefaultBlockSize = $0800;

  {$ENDIF}

Procedure MemSelectSeg; Near; Assembler;
Asm
  MOV     AX, ES: [DI].TMemoryStream. Position. Word [0]
  MOV     DX, ES: [DI].TMemoryStream. Position. Word [2]
  MOV     CX, ES: [DI].TMemoryStream. BlockSize
  Div     CX
  SUB     CX, DX
  MOV     SI, DX
  ShL     AX, 1
  MOV     ES: [DI].TMemoryStream. CurSeg, AX
End;

Const
  MemStreamSize = (SizeOf (TMemoryStream) - SizeOf (TStream) ) Div 2;

  Constructor TMemoryStream. Init (ALimit: LongInt; ABlockSize: Word); Assembler;
  Asm
    XOr     AX, AX
    PUSH    AX
    LES     DI, Self
    PUSH    ES
    PUSH    DI
    Call    TStream. Init
    LES     DI, Self
    {$IFDEF Windows}
    XOr     AX, AX
    PUSH    DI
    ADD     DI, Offset TMemoryStream. SegCount
    MOV     CX, MemStreamSize
    REP     STOSW
    POP     DI
    {$ENDIF}
    CMP     ABlockSize, 0
    JNZ     @@1
    MOV     ABlockSize, DefaultBlockSize
    @@1:    MOV     AX, ALimit. Word [0]
    MOV     DX, ALimit. Word [2]
    Div     ABlockSize
    NEG     DX
    ADC     AX, 0
    MOV     DX, ABlockSize
    MOV     ES: [DI].TMemoryStream. BlockSize, DX
    PUSH    AX
    PUSH    ES
    PUSH    DI
    Call    ChangeListSize
    LES     DI, Self
    Or      AX, AX
    JNZ     @@2
    MOV     DX, stInitError
    Call    DoStreamError
    MOV     ALimit. Word [0], 0
    MOV     ALimit. Word [2], 0
    @@2:    MOV     AX, ALimit. Word [0]
    MOV     DX, ALimit. Word [2]
    MOV     ES: [DI].TMemoryStream. Size. Word [0], AX
    MOV     ES: [DI].TMemoryStream. Size. Word [2], DX
  End;

Destructor TMemoryStream. Done;
Begin
  ChangeListSize (0);
  Inherited Done;
End;

Function TMemoryStream. ChangeListSize (ALimit: Word): Boolean;
Var
  AItems: PWordArray;
  Dif, Term: Word;
  NewBlock: Pointer;
Begin
  ChangeListSize := False;
  If ALimit > MaxSegArraySize Then ALimit := MaxSegArraySize;
  If ALimit <> SegCount Then
  Begin
    If ALimit = 0 Then AItems := Nil Else
    Begin
      AItems := MemAlloc (ALimit * SizeOf (Word) );
      If AItems = Nil Then Exit;
      If (SegCount <> 0) And (SegList <> Nil) Then
        If SegCount > ALimit Then
          Move (SegList^, AItems^, ALimit * SizeOf (Word) )
        Else
          Move (SegList^, AItems^, SegCount * SizeOf (Word) );
    End;
    If ALimit < SegCount Then
    Begin
      Dif  := ALimit;
      Term := SegCount - 1;
      While Dif <= Term Do
      Begin
        FreeMem (Ptr (SegList^ [Dif], 0), BlockSize);
        Inc (Dif);
      End;
    End
    Else
    Begin
      Dif := SegCount;
      Term := ALimit - 1;
      While Dif <= Term Do
      Begin
        NewBlock := MemAllocSeg (BlockSize);
        If NewBlock = Nil Then Exit
        Else AItems^ [Dif] := PtrRec (NewBlock).Seg;
        Inc (Dif);
      End;
    End;
    If SegCount <> 0 Then FreeMem (SegList, SegCount * SizeOf (Word) );
    SegList := AItems;
    SegCount := ALimit;
  End;
  ChangeListSize := True;
End;

Function TMemoryStream. GetPos: LongInt; Assembler;
Asm
  LES     DI, Self
  CMP     ES: [DI].TMemoryStream. Status, 0
  JNE     @@1
  MOV     AX, ES: [DI].TMemoryStream. Position. Word [0]
  MOV     DX, ES: [DI].TMemoryStream. Position. Word [2]
  JMP     @@2
  @@1:    MOV     AX, - 1
  CWD
  @@2:
End;

Function TMemoryStream. GetSize: LongInt; Assembler;
Asm
  LES     DI, Self
  CMP     ES: [DI].TMemoryStream. Status, 0
  JNE     @@1
  MOV     AX, ES: [DI].TMemoryStream. Size. Word [0]
  MOV     DX, ES: [DI].TMemoryStream. Size. Word [2]
  JMP     @@2
  @@1:    MOV     AX, - 1
  CWD
  @@2:
End;

Procedure TMemoryStream. Read (Var Buf; Count: Word); Assembler;
Asm
  LES     DI, Self
  XOr     BX, BX
  CMP     BX, ES: [DI].TMemoryStream. Status
  JNE     @@3
  MOV     AX, ES: [DI].TMemoryStream. Position. Word [0]
  MOV     DX, ES: [DI].TMemoryStream. Position. Word [2]
  ADD     AX, Count
  ADC     DX, BX
  CMP     DX, ES: [DI].TMemoryStream. Size. Word [2]
  JA      @@1
  JB      @@7
  CMP     AX, ES: [DI].TMemoryStream. Size. Word [0]
  JBE     @@7
  @@1:    XOr     AX, AX
  MOV     DX, stReadError
  @@2:    Call    DoStreamError
  @@3:    LES     DI, Buf
  MOV     CX, Count
  XOr     AL, AL
  CLD
  REP     STOSB
  JMP     @@8
  @@5:    Call    MemSelectSeg
  MOV     AX, Count
  SUB     AX, BX
  CMP     CX, AX
  JB      @@6
  MOV     CX, AX
  @@6:    ADD     ES: [DI].TMemoryStream. Position. Word [0], CX
  ADC     ES: [DI].TMemoryStream. Position. Word [2], 0
  PUSH    ES
  PUSH    DS
  PUSH    DI
  MOV     DX, ES: [DI].TMemoryStream. CurSeg
  LES     DI, ES: [DI].TMemoryStream. SegList
  ADD     DI, DX
  MOV     DS, Word Ptr ES: [DI]
  LES     DI, Buf
  ADD     DI, BX
  ADD     BX, CX
  CLD
  REP     MOVSB
  POP     DI
  POP     DS
  POP     ES
  @@7:    CMP     BX, Count
  JB      @@5
  @@8:
End;

Procedure TMemoryStream. Seek (Pos: LongInt); Assembler;
Asm
  LES     DI, Self
  MOV     AX, Pos. Word [0]
  MOV     DX, Pos. Word [2]
  Or      DX, DX
  JNS     @@1
  XOr     AX, AX
  CWD
  @@1:    MOV     ES: [DI].TMemoryStream. Position. Word [0], AX
  MOV     ES: [DI].TMemoryStream. Position. Word [2], DX
End;

Procedure TMemoryStream. Truncate; Assembler;
Asm
  LES     DI, Self
  XOr     BX, BX
  CMP     ES: [DI].TMemoryStream. Status, BX
  JNE     @@2
  MOV     AX, ES: [DI].TMemoryStream. Position. Word [0]
  MOV     DX, ES: [DI].TMemoryStream. Position. Word [2]
  Div     ES: [DI].TMemoryStream. BlockSize
  NEG     DX
  ADC     AX, BX
  PUSH    AX
  PUSH    ES
  PUSH    DI
  Call    ChangeListSize
  Or      AX, AX
  JNZ     @@1
  MOV     DX, stError
  Call    DoStreamError
  JMP     @@2
  @@1:    LES     DI, Self
  MOV     AX, ES: [DI].TMemoryStream. Position. Word [0]
  MOV     DX, ES: [DI].TMemoryStream. Position. Word [2]
  MOV     ES: [DI].TMemoryStream. Size. Word [0], AX
  MOV     ES: [DI].TMemoryStream. Size. Word [2], DX
  @@2:
End;

Procedure TMemoryStream. Write (Var Buf; Count: Word); Assembler;
Asm
  LES     DI, Self
  XOr     BX, BX
  CMP     BX, ES: [DI].TMemoryStream. Status
  JNE     @@7
  MOV     AX, ES: [DI].TMemoryStream. Position. Word [0]
  MOV     DX, ES: [DI].TMemoryStream. Position. Word [2]
  ADD     AX, Count
  ADC     DX, BX
  Div     ES: [DI].TMemoryStream. BlockSize
  NEG     DX
  ADC     AX, BX
  CMP     AX, ES: [DI].TMemoryStream. SegCount
  JBE     @@4
  PUSH    BX
  PUSH    ES
  PUSH    DI
  PUSH    AX
  PUSH    ES
  PUSH    DI
  Call    ChangeListSize
  POP     DI
  POP     ES
  POP     BX
  Or      AX, AX
  JNZ     @@4
  @@1:    MOV     DX, stWriteError
  Call    DoStreamError
  JMP     @@7
  @@2:    Call    MemSelectSeg
  MOV     AX, Count
  SUB     AX, BX
  CMP     CX, AX
  JB      @@3
  MOV     CX, AX
  @@3:    ADD     ES: [DI].TMemoryStream. Position. Word [0], CX
  ADC     ES: [DI].TMemoryStream. Position. Word [2], 0
  PUSH    ES
  PUSH    DS
  PUSH    DI
  MOV     DX, ES: [DI].TMemoryStream. CurSeg
  LES     DI, ES: [DI].TMemoryStream. SegList
  ADD     DI, DX
  MOV     ES, Word Ptr ES: [DI]
  MOV     DI, SI
  LDS     SI, Buf
  ADD     SI, BX
  ADD     BX, CX
  CLD
  REP     MOVSB
  POP     DI
  POP     DS
  POP     ES
  @@4:    CMP     BX, Count
  JB      @@2
  @@5:    MOV     AX, ES: [DI].TMemoryStream. Position. Word [0]
  MOV     DX, ES: [DI].TMemoryStream. Position. Word [2]
  CMP     DX, ES: [DI].TMemoryStream. Size. Word [2]
  JB      @@7
  JA      @@6
  CMP     AX, ES: [DI].TMemoryStream. Size. Word [0]
  JBE     @@7
  @@6:    MOV     ES: [DI].TMemoryStream. Size. Word [0], AX
  MOV     ES: [DI].TMemoryStream. Size. Word [2], DX
  @@7:
End;

{ TCollection }

Const
  TCollection_Error    = vmtHeaderSize + $04;
  TCollection_SetLimit = vmtHeaderSize + $1C;

Procedure CollectionError; Near; Assembler;
Asm
  PUSH    AX
  PUSH    BX
  PUSH    ES
  PUSH    DI
  MOV     DI, ES: [DI]
  Call    DWord Ptr [DI].TCollection_Error
End;

Constructor TCollection. Init (ALimit, ADelta: Integer);
Begin
  TObject. Init;
  Items := Nil;
  Count := 0;
  Limit := 0;
  Delta := ADelta;
  SetLimit (ALimit);
End;

Constructor TCollection. Load (Var S: TStream);
Var
  C, I  : Integer;
Begin
  S. Read (Count, SizeOf (Integer) * 3);
  Items := Nil;
  C := Count;
  I := Limit;
  Count := 0;
  Limit := 0;
  SetLimit (I);
  Count := C;
  For I := 0 To C - 1 Do AtPut (I, GetItem (S) );
End;

Destructor TCollection. Done;
Begin
  FreeAll;
  SetLimit (0);
End;

Function TCollection. At (Index: Integer): Pointer; Assembler;
Asm
  LES     DI, Self
  MOV     BX, Index
  Or      BX, BX
  JL      @@1
  CMP     BX, ES: [DI].TCollection. Count
  JGE     @@1
  LES     DI, ES: [DI].TCollection. Items
  ShL     BX, 1
  ShL     BX, 1
  MOV     AX, ES: [DI + BX]
  MOV     DX, ES: [DI + BX + 2]
  JMP     @@2
  @@1:    MOV     AX, coIndexError
  Call    CollectionError
  XOr     AX, AX
  MOV     DX, AX
  @@2:
End;

Procedure TCollection. AtDelete (Index: Integer); Assembler;
Asm
  LES     DI, Self
  MOV     BX, Index
  Or      BX, BX
  JL      @@1
  CMP     BX, ES: [DI].TCollection. Count
  JGE     @@1
  Dec     ES: [DI].TCollection. Count
  MOV     CX, ES: [DI].TCollection. Count
  SUB     CX, BX
  JE      @@2
  CLD
  LES     DI, ES: [DI].TCollection. Items
  ShL     BX, 1
  ShL     BX, 1
  ADD     DI, BX
  LEA     SI, [DI + 4]
  ShL     CX, 1
  PUSH    DS
  PUSH    ES
  POP     DS
  REP     MOVSW
  POP     DS
  JMP     @@2
  @@1:    MOV     AX, coIndexError
  Call    CollectionError
  @@2:
End;

Procedure TCollection. AtFree (Index: Integer);
Begin
  FreeItem (At (Index));
  AtDelete (Index);
End;

Procedure TCollection. AtInsert (Index: Integer; Item: Pointer); Assembler;
Asm
  LES     DI, Self
  MOV     BX, Index
  Or      BX, BX
  JL      @@3
  MOV     CX, ES: [DI].TCollection. Count
  CMP     BX, CX
  JG      @@3
  CMP     CX, ES: [DI].TCollection. Limit
  JNE     @@1
  PUSH    CX
  PUSH    BX
  ADD     CX, ES: [DI].TCollection. Delta
  PUSH    CX
  PUSH    ES
  PUSH    DI
  MOV     DI, ES: [DI]
  Call    DWord Ptr [DI].TCollection_SetLimit
  POP     BX
  POP     CX
  LES     DI, Self
  CMP     CX, ES: [DI].TCollection. Limit
  JE      @@4
  @@1:    Inc     ES: [DI].TCollection. Count
  STD
  LES     DI, ES: [DI].TCollection. Items
  ShL     CX, 1
  ADD     DI, CX
  ADD     DI, CX
  Inc     DI
  Inc     DI
  ShL     BX, 1
  SUB     CX, BX
  JE      @@2
  LEA     SI, [DI - 4]
  PUSH    DS
  PUSH    ES
  POP     DS
  REP     MOVSW
  POP     DS
  @@2:    MOV     AX, Word Ptr [Item + 2]
  STOSW
  MOV     AX, Word Ptr [Item]
  STOSW
  CLD
  JMP     @@6
  @@3:    MOV     AX, coIndexError
  JMP     @@5
  @@4:    MOV     AX, coOverflow
  MOV     BX, CX
  @@5:    Call    CollectionError
  @@6:
End;

Procedure TCollection. AtPut (Index: Integer; Item: Pointer); Assembler;
Asm
  MOV   AX, Item. Word [0]
  MOV   DX, Item. Word [2]
  LES   DI, Self
  MOV     BX, Index
  Or      BX, BX
  JL      @@1
  CMP     BX, ES: [DI].TCollection. Count
  JGE     @@1
  LES     DI, ES: [DI].TCollection. Items
  ShL     BX, 1
  ShL     BX, 1
  MOV     ES: [DI + BX], AX
  MOV     ES: [DI + BX + 2], DX
  JMP     @@2
  @@1:    MOV     AX, coIndexError
  Call    CollectionError
  @@2:
End;

Procedure TCollection. Delete (Item: Pointer);
Begin
  AtDelete (IndexOf (Item) );
End;

Procedure TCollection. DeleteAll;
Begin
  Count := 0;
End;

Procedure TCollection. Error (Code, Info: Integer);
Begin
  RunError (212 - Code);
End;

Function TCollection. FirstThat (Test: Pointer): Pointer; Assembler;
Asm
  LES     DI, Self
  MOV     CX, ES: [DI].TCollection. Count
  JCXZ    @@2
  LES     DI, ES: [DI].TCollection. Items
  @@1:    PUSH    ES
  PUSH    DI
  PUSH    CX
  PUSH    Word Ptr ES: [DI + 2]
  PUSH    Word Ptr ES: [DI]
  {$IFDEF Windows}
  MOV   AX, [BP]
  And   AL, 0FEH
  PUSH  AX
  {$ELSE}
  PUSH    Word Ptr [BP]
  {$ENDIF}
  Call    Test
  POP     CX
  POP     DI
  POP     ES
  Or      AL, AL
  JNE     @@3
  ADD     DI, 4
  LOOP    @@1
  @@2:    XOr     AX, AX
  MOV     DX, AX
  JMP     @@4
  @@3:  MOV     AX, ES: [DI]
  MOV   DX, ES: [DI + 2]
  @@4:
End;

Procedure TCollection. ForEach (Action: Pointer); Assembler;
Asm
  LES     DI, Self
  MOV     CX, ES: [DI].TCollection. Count
  JCXZ    @@2
  LES     DI, ES: [DI].TCollection. Items
  @@1:    PUSH    ES
  PUSH    DI
  PUSH    CX
  PUSH    Word Ptr ES: [DI + 2]
  PUSH    Word Ptr ES: [DI]
  {$IFDEF Windows}
  MOV   AX, [BP]
  And   AL, 0FEH
  PUSH  AX
  {$ELSE}
  PUSH    Word Ptr [BP]
  {$ENDIF}
  Call    Action
  POP     CX
  POP     DI
  POP     ES
  ADD     DI, 4
  LOOP    @@1
  @@2:
End;

Procedure TCollection. Free (Item: Pointer);
Begin
  Delete (Item);
  FreeItem (Item);
End;

Procedure TCollection. FreeAll;
Var
  I: Integer;
Begin
  For I := 0 To Count - 1 Do FreeItem (At (I));
  Count := 0;
End;

Procedure TCollection. FreeItem (Item: Pointer);
Begin
  If Item <> Nil Then Dispose (PObject (Item), Done);
End;

Function TCollection. GetItem (Var S: TStream): Pointer;
Begin
  GetItem := S. Get;
End;

Function TCollection. IndexOf (Item: Pointer): Integer; Assembler;
Asm
  MOV   AX, Item. Word [0]
  MOV   DX, Item. Word [2]
  LES     DI, Self
  MOV     CX, ES: [DI].TCollection. Count
  JCXZ    @@3
  LES     DI, ES: [DI].TCollection. Items
  MOV     BX, DI
  ShL     CX, 1
  CLD
  @@1:    REPNE   SCASW
  JCXZ    @@3
  Test    CX, 1
  JE      @@1
  XCHG    AX, DX
  SCASW
  XCHG    AX, DX
  LOOPNE  @@1
  JNE     @@3
  MOV     AX, DI
  SUB     AX, BX
  ShR     AX, 1
  ShR     AX, 1
  Dec     AX
  JMP     @@2
  @@3:    MOV     AX, - 1
  @@2:
End;

Procedure TCollection. Insert (Item: Pointer); Assembler;
Asm
  LES     DI, Self
  MOV     BX, ES: [DI].TCollection. Count
  MOV     CX, BX
  CMP     CX, ES: [DI].TCollection. Limit
  JNE     @@1
  PUSH    CX
  PUSH    BX
  ADD     CX, ES: [DI].TCollection. Delta
  PUSH    CX
  PUSH    ES
  PUSH    DI
  MOV     DI, ES: [DI]
  Call    DWord Ptr [DI].TCollection_SetLimit
  POP     BX
  POP     CX
  LES     DI, Self
  CMP     CX, ES: [DI].TCollection. Limit
  JE      @@4
@@1:
  Inc     ES: [DI].TCollection. Count
  LES     DI, ES: [DI].TCollection. Items
  ShL     CX, 2
  ADD     DI, CX
  MOV     AX, Word Ptr [Item]
  Mov     ES:[DI], AX
  MOV     AX, Word Ptr [Item + 2]
  Mov     ES:[DI + 2], AX
  JMP     @@6
@@4:
  MOV     AX, coOverflow
  MOV     BX, CX
  Call    CollectionError
@@6:
End;

Function TCollection. LastThat (Test: Pointer): Pointer; Assembler;
Asm
  LES     DI, Self
  MOV     CX, ES: [DI].TCollection. Count
  JCXZ    @@2
  LES     DI, ES: [DI].TCollection. Items
  MOV     AX, CX
  ShL     AX, 1
  ShL     AX, 1
  ADD     DI, AX
  @@1:    SUB     DI, 4
  PUSH    ES
  PUSH    DI
  PUSH    CX
  PUSH    Word Ptr ES: [DI + 2]
  PUSH    Word Ptr ES: [DI]
  {$IFDEF Windows}
  MOV   AX, [BP]
  And   AL, 0FEH
  PUSH  AX
  {$ELSE}
  PUSH    Word Ptr [BP]
  {$ENDIF}
  Call    Test
  POP     CX
  POP     DI
  POP     ES
  Or      AL, AL
  JNE     @@3
  LOOP    @@1
  @@2:    XOr     AX, AX
  MOV     DX, AX
  JMP     @@4
  @@3:  MOV     AX, ES: [DI]
  MOV   DX, ES: [DI + 2]
  @@4:
End;

Procedure TCollection. Pack; Assembler;
Asm
  LES     DI, Self
  MOV     CX, ES: [DI].TCollection. Count
  JCXZ    @@3
  LES     DI, ES: [DI].TCollection. Items
  MOV     SI, DI
  PUSH    DS
  PUSH    ES
  POP     DS
  CLD
  @@1:    LODSW
  XCHG    AX, DX
  LODSW
  MOV     BX, AX
  Or      BX, DX
  JE      @@2
  XCHG    AX, DX
  STOSW
  XCHG    AX, DX
  STOSW
  @@2:    LOOP    @@1
  POP     DS
  LES     BX, Self
  SUB     DI, Word Ptr ES: [BX].TCollection. Items
  ShR     DI, 1
  ShR     DI, 1
  MOV     ES: [BX].TCollection. Count, DI
  @@3:
End;

Procedure TCollection. PutItem (Var S: TStream; Item: Pointer);
Begin
  S. Put (Item);
End;

Procedure TCollection. SetLimit (ALimit: Integer);
Var
  AItems: PItemList;
Begin
  If ALimit < Count Then ALimit := Count;
  If ALimit > MaxCollectionSize Then ALimit := MaxCollectionSize;
  If ALimit <> Limit Then
  Begin
    If ALimit = 0 Then AItems := Nil Else
    Begin
      GetMem (AItems, ALimit * SizeOf (Pointer) );
      If (Count <> 0) And (Items <> Nil) Then
        Move (Items^, AItems^, Count * SizeOf (Pointer) );
    End;
    If Limit <> 0 Then FreeMem (Items, Limit * SizeOf (Pointer) );
    Items := AItems;
    Limit := ALimit;
  End;
End;

Procedure TCollection. Store (Var S: TStream);

Procedure DoPutItem (P: Pointer); Far;
Begin
  PutItem (S, P);
End;

Begin
  S. Write (Count, SizeOf (Integer) * 3);
  ForEach (@DoPutItem);
End;

{ TSortedCollection }

Constructor TSortedCollection. Init (ALimit, ADelta: Integer);
Begin
  TCollection. Init (ALimit, ADelta);
  Duplicates := False;
End;

Constructor TSortedCollection. Load (Var S: TStream);
Begin
  TCollection. Load (S);
  S. Read (Duplicates, SizeOf (Boolean));
End;

Function TSortedCollection. Compare (Key1, Key2: Pointer): Integer;
Begin
  Abstract;
End;

Function TSortedCollection. IndexOf (Item: Pointer): Integer;
Var
  I: Integer;
Begin
  IndexOf := - 1;
  If Search (KeyOf (Item), I) Then
  Begin
    If Duplicates Then
      While (I < Count) And (Item <> Items^ [I] ) Do Inc (I);
    If I < Count Then IndexOf := I;
  End;
End;

Procedure TSortedCollection. Insert (Item: Pointer);
Var
  I: Integer;
Begin
  If Not Search (KeyOf (Item), I) Or Duplicates Then AtInsert (I, Item);
End;

Function TSortedCollection. KeyOf (Item: Pointer): Pointer;
Begin
  KeyOf := Item;
End;

Function TSortedCollection. Search (Key: Pointer; Var Index: Integer): Boolean;
Var
  L, H, I, C: Integer;
Begin
  Search := False;
  L := 0;
  H := Count - 1;
  While L <= H Do
  Begin
    I := (L + H) ShR 1;
    C := Compare (KeyOf (Items^ [I] ), Key);
    If C < 0 Then L := I + 1 Else
    Begin
      H := I - 1;
      If C = 0 Then
      Begin
        Search := True;
        If Not Duplicates Then L := I;
      End;
    End;
  End;
  Index := L;
End;

Procedure TSortedCollection. Store (Var S: TStream);
Begin
  TCollection. Store (S);
  S. Write (Duplicates, SizeOf (Boolean));
End;

{ TStringCollection }

Function TStringCollection. Compare (Key1, Key2: Pointer): Integer;
Assembler;
Asm
  PUSH    DS
  CLD
  LDS     SI, Key1
  LES     DI, Key2
  LODSB
  MOV     AH, ES: [DI]
  Inc     DI
  MOV     CL, AL
  CMP     CL, AH
  JBE     @@1
  MOV     CL, AH
  @@1:    XOr     CH, CH
  REP     CMPSB
  JE      @@2
  MOV     AL, DS: [SI - 1]
  MOV     AH, ES: [DI - 1]
  @@2:    SUB     AL, AH
  SBB     AH, AH
  POP     DS
End;

Procedure TStringCollection. FreeItem (Item: Pointer);
Begin
  DisposeStr (Item);
End;

Function TStringCollection. GetItem (Var S: TStream): Pointer;
Begin
  GetItem := S. ReadStr;
End;

Procedure TStringCollection. PutItem (Var S: TStream; Item: Pointer);
Begin
  S. WriteStr (Item);
End;

{ TStrCollection }

Function TStrCollection. Compare (Key1, Key2: Pointer): Integer;
Begin
  Compare := StrComp (Key1, Key2);
End;

Procedure TStrCollection. FreeItem (Item: Pointer);
Begin
  StrDispose (Item);
End;

Function TStrCollection. GetItem (Var S: TStream): Pointer;
Begin
  GetItem := S. StrRead;
End;

Procedure TStrCollection. PutItem (Var S: TStream; Item: Pointer);
Begin
  S. StrWrite (Item);
End;

{$IFNDEF Windows }

{ Private resource manager types }

Const
  RStreamMagic: LongInt = $52504246; { 'FBPR' }
  RStreamBackLink: LongInt = $4C424246; { 'FBBL' }

Type
  PResourceItem = ^TResourceItem;
  TResourceItem = Record
                    Pos: LongInt;
                    Size: LongInt;
                    Key: String;
                  End;

  { TResourceCollection }

Procedure TResourceCollection. FreeItem (Item: Pointer);
Begin
  FreeMem (Item, Length (PResourceItem (Item)^. Key) +
  (SizeOf (TResourceItem) - SizeOf (String) + 1) );
End;

Function TResourceCollection. GetItem (Var S: TStream): Pointer;
Var
  Pos: LongInt;
  Size: LongInt;
  L: Byte;
  P: PResourceItem;
Begin
  S. Read (Pos, SizeOf (LongInt));
  S. Read (Size, SizeOf (LongInt));
  S. Read (L, 1);
  GetMem (P, L + (SizeOf (TResourceItem) - SizeOf (String) + 1) );
  P^. Pos := Pos;
  P^. Size := Size;
  P^. Key [0] := Char (L);
  S. Read (P^. Key [1], L);
  GetItem := P;
End;

Function TResourceCollection. KeyOf (Item: Pointer): Pointer; Assembler;
Asm
  MOV     AX, Item. Word [0]
  MOV     DX, Item. Word [2]
  ADD     AX, Offset TResourceItem. Key
End;

Procedure TResourceCollection. PutItem (Var S: TStream; Item: Pointer);
Begin
  S. Write (PResourceItem (Item)^, Length (PResourceItem (Item)^. Key) +
  (SizeOf (TResourceItem) - SizeOf (String) + 1) );
End;

{ TResourceFile }

Constructor TResourceFile. Init (AStream: PStream);
Type

  {$IFDEF NewExeFormat}

  TExeHeader = Record
                 eHdrSize:   Word;
                 eMinAbove:  Word;
                 eMaxAbove:  Word;
                 eInitSS:    Word;
                 eInitSP:    Word;
                 eCheckSum:  Word;
                 eInitPC:    Word;
                 eInitCS:    Word;
                 eRelocOfs:  Word;
                 eOvlyNum:   Word;
                 eRelocTab:  Word;
                 eSpace:     Array [1..30] Of Byte;
                 eNewHeader: Word;
               End;

  {$ENDIF}

  THeader = Record
              Signature: Word;
              Case Integer Of
                0: (
                     LastCount: Word;
                PageCount: Word;
                ReloCount: Word);
                1: (
                     InfoType: Word;
                InfoSize: LongInt);
              End;
              Var
                Found, Stop: Boolean;
                Header: THeader;

                {$IFDEF NewExeFormat}

                ExeHeader: TExeHeader;

                {$ENDIF}

              Begin
                TObject. Init;
                Stream := AStream;
                BasePos := Stream^. GetPos;
                Found := False;
                Repeat
                  Stop := True;
                  If BasePos <= Stream^. GetSize - SizeOf (THeader) Then
                  Begin
                    Stream^. Seek (BasePos);
                    Stream^. Read (Header, SizeOf (THeader));
                    Case Header. Signature Of

                      {$IFDEF NewExeFormat}

                      $5A4D:
                            Begin
                              Stream^. Read (ExeHeader, SizeOf (TExeHeader));
                              BasePos := ExeHeader. eNewHeader;
                              Stop := False;
                            End;
                      $454E:
                            Begin
                              BasePos := Stream^. GetSize - 8;
                              Stop := False;
                            End;
                      $4246:
                            Begin
                              Stop := False;
                              Case Header. Infotype Of
                                $5250:                                    {Found Resource}
                                                                          Begin
                                                                            Found := True;
                                                                            Stop := True;
                                                                          End;
                                $4C42: Dec (BasePos, Header. InfoSize - 8); {Found BackLink}
                                $4648: Dec (BasePos, SizeOf (THeader) * 2); {Found HelpFile}
                                Else
                                  Stop := True;
                              End;
                            End;
                      $424E:
                              If Header. InfoType = $3230 Then               {Found Debug Info}
                              Begin
                                Dec (BasePos, Header. InfoSize);
                                Stop := False;
                              End;

                      {$ELSE}

                      $5A4D:
                            Begin
                              Inc (BasePos, LongMul (Header. PageCount, 512) -
                              ( - Header. LastCount And 511) );
                              Stop := False;
                            End;
                      $4246:
                              If Header. InfoType = $5250 Then Found := True Else
                              Begin
                                Inc (BasePos, Header. InfoSize + 8);
                                Stop := False;
                              End;

                      {$ENDIF}

                    End;
                  End;
                Until Stop;
                If Found Then
                Begin
                  Stream^. Seek (BasePos + SizeOf (LongInt) * 2);
                  Stream^. Read (IndexPos, SizeOf (LongInt));
                  Stream^. Seek (BasePos + IndexPos);
                  Index. Load (Stream^);
                End Else
                Begin
                  IndexPos := SizeOf (LongInt) * 3;
                  Index. Init (0, 8);
                End;
              End;

              Destructor TResourceFile. Done;
            Begin
              Flush;
              Index. Done;
              Dispose (Stream, Done);
            End;

Function TResourceFile. Count: Integer;
Begin
  Count := Index. Count;
End;

Procedure TResourceFile. Delete (Key: String);
Var
  I: Integer;
Begin
  If Index. Search (@Key, I) Then
  Begin
    Index. Free (Index. At (I) );
    Modified := True;
  End;
End;

Procedure TResourceFile. Flush;
Var
  ResSize: LongInt;
  LinkSize: LongInt;
Begin
  If Modified Then
  Begin
    Stream^. Seek (BasePos + IndexPos);
    Index. Store (Stream^);
    ResSize := Stream^. GetPos - BasePos;
    LinkSize := ResSize + SizeOf (LongInt) * 2;
    Stream^. Write (RStreamBackLink, SizeOf (LongInt));
    Stream^. Write (LinkSize, SizeOf (LongInt));
    Stream^. Seek (BasePos);
    Stream^. Write (RStreamMagic, SizeOf (LongInt) );
    Stream^. Write (ResSize, SizeOf (LongInt) );
    Stream^. Write (IndexPos, SizeOf (LongInt) );
    Stream^. Flush;
    Modified := False;
  End;
End;

Function TResourceFile. Get (Key: String): PObject;
Var
  I: Integer;
Begin
  If Not Index. Search (@Key, I) Then Get := Nil Else
  Begin
    Stream^. Seek (BasePos + PResourceItem (Index. At (I) )^. Pos);
    Get := Stream^. Get;
  End;
End;

Function TResourceFile. KeyAt (I: Integer): String;
Begin
  KeyAt := PResourceItem (Index. At (I) )^. Key;
End;

Procedure TResourceFile. Put (Item: PObject; Key: String);
Var
  I: Integer;
  P: PResourceItem;
Begin
  If Index. Search (@Key, I) Then P := Index. At (I) Else
  Begin
    GetMem (P, Length (Key) + (SizeOf (TResourceItem) - SizeOf (String) + 1) );
    P^. Key := Key;
    Index. AtInsert (I, P);
  End;
  P^. Pos := IndexPos;
  Stream^. Seek (BasePos + IndexPos);
  Stream^. Put (Item);
  IndexPos := Stream^. GetPos - BasePos;
  P^. Size := IndexPos - P^. Pos;
  Modified := True;
End;

Function TResourceFile. SwitchTo (AStream: PStream; Pack: Boolean): PStream;
Var
  NewBasePos: LongInt;

Procedure DoCopyResource (Item: PResourceItem); Far;
Begin
  Stream^. Seek (BasePos + Item^. Pos);
  Item^. Pos := AStream^. GetPos - NewBasePos;
  AStream^. CopyFrom (Stream^, Item^. Size);
End;

Begin
  SwitchTo := Stream;
  NewBasePos := AStream^. GetPos;
  If Pack Then
  Begin
    AStream^. Seek (NewBasePos + SizeOf (LongInt) * 3);
    Index. ForEach (@DoCopyResource);
    IndexPos := AStream^. GetPos - NewBasePos;
  End Else
  Begin
    Stream^. Seek (BasePos);
    AStream^. CopyFrom (Stream^, IndexPos);
  End;
  Stream := AStream;
  Modified := True;
  BasePos := NewBasePos;
End;

{ TStringList }

Constructor TStringList. Load (Var S: TStream);
Var
  Size: Word;
Begin
  Stream := @S;
  S. Read (Size, SizeOf (Word));
  BasePos := S. GetPos;
  S. Seek (BasePos + Size);
  S. Read (IndexSize, SizeOf (Integer));
  GetMem (Index, IndexSize * SizeOf (TStrIndexRec) );
  S. Read (Index^, IndexSize * SizeOf (TStrIndexRec));
End;

Destructor TStringList. Done;
Begin
  FreeMem (Index, IndexSize * SizeOf (TStrIndexRec) );
End;

Function TStringList. Get (Key: Word): String; Assembler;
Asm
  PUSH    DS
  LDS     SI, Self
  LES     DI, @Result
  CLD
  MOV     CX, DS: [SI].TStringList. IndexSize
  JCXZ    @@2
  MOV     BX, Key
  LDS     SI, DS: [SI].TStringList. Index
  @@1:    MOV     DX, BX
  LODSW
  SUB     DX, AX
  LODSW
  CMP     DX, AX
  LODSW
  JB      @@3
  LOOP    @@1
  @@2:    POP     DS
  XOr     AL, AL
  STOSB
  JMP     @@4
  @@3:    POP     DS
  PUSH    ES
  PUSH    DI
  PUSH    AX
  PUSH    DX
  LES     DI, Self
  PUSH    ES
  PUSH    DI
  Call    TStringList. ReadStr
  @@4:
End;

Procedure TStringList. ReadStr (Var S: String; Offset, Skip: Word);
Begin
  Stream^. Seek (BasePos + Offset);
  Inc (Skip);
  Repeat
    Stream^. Read (S [0], 1);
    Stream^. Read (S [1], Ord (S [0] ) );
    Dec (Skip);
  Until Skip = 0;
End;

{ TStrListMaker }

Constructor TStrListMaker. Init (AStrSize, AIndexSize: Word);
Begin
  TObject. Init;
  StrSize := AStrSize;
  IndexSize := AIndexSize;
  GetMem (Strings, AStrSize);
  GetMem (Index, AIndexSize * SizeOf (TStrIndexRec) );
End;

Destructor TStrListMaker. Done;
Begin
  FreeMem (Index, IndexSize * SizeOf (TStrIndexRec) );
  FreeMem (Strings, StrSize);
End;

Procedure TStrListMaker. CloseCurrent;
Begin
  If Cur. Count <> 0 Then
  Begin
    Index^ [IndexPos] := Cur;
    Inc (IndexPos);
    Cur. Count := 0;
  End;
End;

Procedure TStrListMaker. Put (Key: Word; S: String);
Begin
  If (Cur. Count = 16) Or (Key <> Cur. Key + Cur. Count) Then CloseCurrent;
  If Cur. Count = 0 Then
  Begin
    Cur. Key := Key;
    Cur. Offset := StrPos;
  End;
  Inc (Cur. Count);
  Move (S, Strings^ [StrPos], Length (S) + 1);
  Inc (StrPos, Length (S) + 1);
End;

Procedure TStrListMaker. Store (Var S: TStream);
Begin
  CloseCurrent;
  S. Write (StrPos, SizeOf (Word));
  S. Write (Strings^, StrPos);
  S. Write (IndexPos, SizeOf (Word));
  S. Write (Index^, IndexPos * SizeOf (TStrIndexRec));
End;

{ TRect }

Procedure CheckEmpty; Near; Assembler;
Asm
  MOV     AX, ES: [DI].TRect. A. X
  CMP     AX, ES: [DI].TRect. B. X
  JGE     @@1
  MOV     AX, ES: [DI].TRect. A. Y
  CMP     AX, ES: [DI].TRect. B. Y
  JL      @@2
  @@1:    CLD
  XOr     AX, AX
  STOSW
  STOSW
  STOSW
  STOSW
  @@2:
End;

Procedure TRect. Assign (XA, YA, XB, YB: Integer); Assembler;
Asm
  LES     DI, Self
  CLD
  MOV     AX, XA
  STOSW
  MOV     AX, YA
  STOSW
  MOV     AX, XB
  STOSW
  MOV     AX, YB
  STOSW
End;

Procedure TRect. Copy (R: TRect); Assembler;
Asm
  PUSH    DS
  LDS     SI, R
  LES     DI, Self
  CLD
  MOVSW
  MOVSW
  MOVSW
  MOVSW
  POP     DS
End;

Procedure TRect. Move (ADX, ADY: Integer); Assembler;
Asm
  LES     DI, Self
  MOV     AX, ADX
  ADD     ES: [DI].TRect. A. X, AX
  ADD     ES: [DI].TRect. B. X, AX
  MOV     AX, ADY
  ADD     ES: [DI].TRect. A. Y, AX
  ADD     ES: [DI].TRect. B. Y, AX
End;

Procedure TRect. Grow (ADX, ADY: Integer); Assembler;
Asm
  LES     DI, Self
  MOV     AX, ADX
  SUB     ES: [DI].TRect. A. X, AX
  ADD     ES: [DI].TRect. B. X, AX
  MOV     AX, ADY
  SUB     ES: [DI].TRect. A. Y, AX
  ADD     ES: [DI].TRect. B. Y, AX
  Call    CheckEmpty
End;

Procedure TRect. Intersect (R: TRect); Assembler;
Asm
  PUSH    DS
  LDS     SI, R
  LES     DI, Self
  CLD
  LODSW
  SCASW
  JLE     @@1
  Dec     DI
  Dec     DI
  STOSW
  @@1:    LODSW
  SCASW
  JLE     @@2
  Dec     DI
  Dec     DI
  STOSW
  @@2:    LODSW
  SCASW
  JGE     @@3
  Dec     DI
  Dec     DI
  STOSW
  @@3:    LODSW
  SCASW
  JGE     @@4
  Dec     DI
  Dec     DI
  STOSW
  @@4:    POP     DS
  SUB     DI, 8
  Call    CheckEmpty
End;

Procedure TRect. Union (R: TRect); Assembler;
Asm
  PUSH    DS
  LDS     SI, R
  LES     DI, Self
  CLD
  LODSW
  SCASW
  JGE     @@1
  Dec     DI
  Dec     DI
  STOSW
  @@1:    LODSW
  SCASW
  JGE     @@2
  Dec     DI
  Dec     DI
  STOSW
  @@2:    LODSW
  SCASW
  JLE     @@3
  Dec     DI
  Dec     DI
  STOSW
  @@3:    LODSW
  SCASW
  JLE     @@4
  Dec     DI
  Dec     DI
  STOSW
  @@4:    POP     DS
End;

Function TRect. Contains (P: TPoint): Boolean; Assembler;
Asm
  LES     DI, Self
  MOV     AL, 0
  MOV     DX, P. X
  CMP     DX, ES: [DI].TRect. A. X
  JL      @@1
  CMP     DX, ES: [DI].TRect. B. X
  JGE     @@1
  MOV     DX, P. Y
  CMP     DX, ES: [DI].TRect. A. Y
  JL      @@1
  CMP     DX, ES: [DI].TRect. B. Y
  JGE     @@1
  Inc     AX
  @@1:
End;

Function TRect. Equals (R: TRect): Boolean; Assembler;
Asm
  PUSH    DS
  LDS     SI, R
  LES     DI, Self
  MOV     CX, 4
  CLD
  REP     CMPSW
  MOV     AL, 0
  JNE     @@1
  Inc     AX
  @@1:    POP     DS
End;

Function TRect. Empty: Boolean; Assembler;
Asm
  LES     DI, Self
  MOV     AL, 1
  MOV     DX, ES: [DI].TRect. A. X
  CMP     DX, ES: [DI].TRect. B. X
  JGE     @@1
  MOV     DX, ES: [DI].TRect. A. Y
  CMP     DX, ES: [DI].TRect. B. Y
  JGE     @@1
  Dec     AX
  @@1:
End;

{$ENDIF}

{ Dynamic string handling routines }

Function NewStr (Const S: String): PString;
Var
  P: PString;
Begin
  GetMem (P, Length (S) + 1);
  P^ := S;
  NewStr := P;
End;

Procedure DisposeStr (P: PString);
Begin
  If P <> Nil Then FreeMem (P, Length (P^) + 1);
End;

{ Objects registration procedure }

Procedure RegisterObjects;
Begin
  RegisterType (RCollection);
  RegisterType (RStringCollection);
  RegisterType (RStrCollection);
End;

{$ENDIF}

End.