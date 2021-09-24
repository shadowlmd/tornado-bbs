
{*******************************************************}
{                                                       }
{       Turbo Pascal Version 7.0                        }
{       Standard Objects Unit                           }
{                                                       }
{       Copyright (c) 1992 Borland International        }
{                                                       }
{       Virtual Pascal v2.1                             }
{       Copyright (C) 1996-2000 vpascal.com             }
{                                                       }
{*******************************************************}


{ NOTE: TEmsStream is not implemented.                  }

unit Objects;

{$X+,H-,I-,S-,B-,Cdecl-,Use32+}

interface

const

{ TStream access modes }

  stCreate    = $3C00;           { Create new file }
  stOpenRead  = $3D40;           { Read access only }
  stOpenWrite = $3D41;           { Write access only }
  stOpen      = $3D42;           { Read and write access }

{ TStream error codes }

  stOk         =  0;              { No error }
  stError      = -1;              { Access error }
  stInitError  = -2;              { Cannot initialize stream }
  stReadError  = -3;              { Read beyond end of stream }
  stWriteError = -4;              { Cannot expand stream }
  stGetError   = -5;              { Get of unregistered object type }
  stPutError   = -6;              { Put of unregistered object type }

{ Maximum TCollection size }

  MaxCollectionSize = 512*1024*1024 div SizeOf(Pointer);

{ TCollection error codes }

  coIndexError = -1;              { Index out of range }
  coOverflow   = -2;              { Overflow }

{ VMT header size }

  vmtHeaderSize = 12;

type

{ Type conversion records }

  WordRec = record
    Lo, Hi: Byte;
  end;

  LongRec = record
    Lo, Hi: SmallWord;
  end;

  PtrRec = record
    Ofs: Longint;
  end;

{ String pointers }

  PString = ^String;

{ Character set type }

  PCharSet = ^TCharSet;
  TCharSet = set of Char;

{ General arrays }

  PByteArray = ^TByteArray;
  TByteArray = array[0..512*1024*1024] of Byte;

  PWordArray = ^TWordArray;
  TWordArray = array[0..512*1024*1024 div 2] of SmallWord;

  PLongArray = ^TLongArray;
  TLongArray = array[0..512*1024*1024 div 4] of Longint;

  PPtrArray = ^TPtrArray;
  TPtrArray = array[0..512*1024*1024 div 4] of Pointer;

{ TObject base object }

  PObject = ^TObject;
  TObject = object
    constructor Init;
    procedure Free;
    destructor Done; virtual;
  end;

{ TStreamRec }

  PStreamRec = ^TStreamRec;
  TStreamRec = record
    ObjType: Word;
    VmtLink: Word;
    Load: Pointer;
    Store: Pointer;
    Next: PStreamRec;
  end;

{ TStream }

  PStream = ^TStream;
  TStream = object(TObject)
    Status: Integer;
    ErrorInfo: Integer;
    constructor Init;
    procedure CopyFrom(var S: TStream; Count: Longint);
    procedure Error(Code, Info: Integer); virtual;
    procedure Flush; virtual;
    function Get: PObject;
    function GetPos: Longint; virtual;
    function GetSize: Longint; virtual;
    procedure Put(P: PObject);
    procedure Read(var Buf; Count: Word); virtual;
    function ReadStr: PString;
    procedure Reset;
    procedure Seek(Pos: Longint); virtual;
    function StrRead: PChar;
    procedure StrWrite(P: PChar);
    procedure Truncate; virtual;
    procedure Write(var Buf; Count: Word); virtual;
    procedure WriteStr(P: PString);
  end;

{ DOS file name string }

{$IFDEF OWL}
  FNameStr = PChar;
{$ELSE}
  FNameStr = string[255];
{$ENDIF}

{ TDosStream }

  PDosStream = ^TDosStream;
  TDosStream = object(TStream)
    Handle: Word;
    constructor Init({$IfNDef OWL}const{$EndIf} FileName: FNameStr; Mode: Word);
    destructor Done; virtual;
    function GetPos: Longint; virtual;
    function GetSize: Longint; virtual;
    procedure Read(var Buf; Count: Word); virtual;
    procedure Seek(Pos: Longint); virtual;
    procedure Truncate; virtual;
    procedure Write(var Buf; Count: Word); virtual;
  end;

{ TBufStream }

  PBufStream = ^TBufStream;
  TBufStream = object(TDosStream)
    Buffer: Pointer;
    BufSize: Word;
    BufPtr: Word;
    BufEnd: Word;
    constructor Init(const FileName: FNameStr; Mode, Size: Word);
    destructor Done; virtual;
    procedure Flush; virtual;
    function GetPos: Longint; virtual;
    function GetSize: Longint; virtual;
    procedure Read(var Buf; Count: Word); virtual;
    procedure Seek(Pos: Longint); virtual;
    procedure Truncate; virtual;
    procedure Write(var Buf; Count: Word); virtual;
  end;

{ TMemoryStream }

  PMemoryStream = ^TMemoryStream;
  TMemoryStream = object(TStream)
    BlockCount: Integer;
    BlockList: PPtrArray;
    CurBlock: Integer;
    BlockSize: Integer;
    Size: Longint;
    Position: Longint;
    constructor Init(ALimit: Longint; ABlockSize: Word);
    destructor Done; virtual;
    function GetPos: Longint; virtual;
    function GetSize: Longint; virtual;
    procedure Read(var Buf; Count: Word); virtual;
    procedure Seek(Pos: Longint); virtual;
    procedure Truncate; virtual;
    procedure Write(var Buf; Count: Word); virtual;
  private
    function ChangeListSize(ALimit: Word): Boolean;
  end;

{ TCollection types }

  PItemList = ^TItemList;
  TItemList = array[0..MaxCollectionSize - 1] of Pointer;

{ TCollection object }

  PCollection = ^TCollection;
  TCollection = object(TObject)
    Items: PItemList;
    Count: Integer;
    Limit: Integer;
    Delta: Integer;
    constructor Init(ALimit, ADelta: Integer);
    constructor Load(var S: TStream);
    destructor Done; virtual;
    function At(Index: Integer): Pointer;
    procedure AtDelete(Index: Integer);
    procedure AtFree(Index: Integer);
    procedure AtInsert(Index: Integer; Item: Pointer);
    procedure AtPut(Index: Integer; Item: Pointer);
    procedure Delete(Item: Pointer);
    procedure DeleteAll;
    procedure Error(Code, Info: Integer); virtual;
    function FirstThat(Test: Pointer): Pointer;
    procedure ForEach(Action: Pointer);
    procedure Free(Item: Pointer);
    procedure FreeAll;
    procedure FreeItem(Item: Pointer); virtual;
    function GetItem(var S: TStream): Pointer; virtual;
    function IndexOf(Item: Pointer): Integer; virtual;
    procedure Insert(Item: Pointer); virtual;
    function LastThat(Test: Pointer): Pointer;
    procedure Pack;
    procedure PutItem(var S: TStream; Item: Pointer); virtual;
    procedure SetLimit(ALimit: Integer); virtual;
    procedure Store(var S: TStream);
  end;

{ TSortedCollection object }

  PSortedCollection = ^TSortedCollection;
  TSortedCollection = object(TCollection)
    Duplicates: Boolean;
    constructor Init(ALimit, ADelta: Integer);
    constructor Load(var S: TStream);
    function Compare(Key1, Key2: Pointer): Integer; virtual;
    function IndexOf(Item: Pointer): Integer; virtual;
    procedure Insert(Item: Pointer); virtual;
    function KeyOf(Item: Pointer): Pointer; virtual;
    function Search(Key: Pointer; var Index: Integer): Boolean; virtual;
    procedure Store(var S: TStream);
  end;

{ TStringCollection object }

  PStringCollection = ^TStringCollection;
  TStringCollection = object(TSortedCollection)
    function Compare(Key1, Key2: Pointer): Integer; virtual;
    procedure FreeItem(Item: Pointer); virtual;
    function GetItem(var S: TStream): Pointer; virtual;
    procedure PutItem(var S: TStream; Item: Pointer); virtual;
  end;

{ TStrCollection object }

  PStrCollection = ^TStrCollection;
  TStrCollection = object(TSortedCollection)
    function Compare(Key1, Key2: Pointer): Integer; virtual;
    procedure FreeItem(Item: Pointer); virtual;
    function GetItem(var S: TStream): Pointer; virtual;
    procedure PutItem(var S: TStream; Item: Pointer); virtual;
  end;

{$IFNDEF OWL}

{ TResourceCollection object }

  PResourceCollection = ^TResourceCollection;
  TResourceCollection = object(TStringCollection)
    procedure FreeItem(Item: Pointer); virtual;
    function GetItem(var S: TStream): Pointer; virtual;
    function KeyOf(Item: Pointer): Pointer; virtual;
    procedure PutItem(var S: TStream; Item: Pointer); virtual;
  end;

{ TResourceFile object }

  PResourceFile = ^TResourceFile;
  TResourceFile = object(TObject)
    Stream: PStream;
    Modified: Boolean;
    constructor Init(AStream: PStream);
    destructor Done; virtual;
    function Count: Integer;
    procedure Delete(Key: String);
    procedure Flush;
    function Get(Key: String): PObject;
    function KeyAt(I: Integer): String;
    procedure Put(Item: PObject; Key: String);
    function SwitchTo(AStream: PStream; Pack: Boolean): PStream;
  private
    BasePos: Longint;
    IndexPos: Longint;
    Index: TResourceCollection;
  end;

{ TStringList object }

  TStrIndexRec = record
    Key, Count, Offset: Word;
  end;

  PStrIndex = ^TStrIndex;
  TStrIndex = array[0..9999] of TStrIndexRec;

  PStringList = ^TStringList;
  TStringList = object(TObject)
    constructor Load(var S: TStream);
    destructor Done; virtual;
    function Get(Key: Word): String;
  private
    Stream: PStream;
    BasePos: Longint;
    IndexSize: Integer;
    Index: PStrIndex;
    procedure ReadStr(var S: String; Offset, Skip: Word);
  end;

{ TStrListMaker object }

  PStrListMaker = ^TStrListMaker;
  TStrListMaker = object(TObject)
    constructor Init(AStrSize, AIndexSize: Word);
    destructor Done; virtual;
    procedure Put(Key: Word; S: String);
    procedure Store(var S: TStream);
  private
    StrPos: Word;
    StrSize: Word;
    Strings: PByteArray;
    IndexPos: Word;
    IndexSize: Word;
    Index: PStrIndex;
    Cur: TStrIndexRec;
    procedure CloseCurrent;
  end;

{ TPoint object }

  TPoint = object
    X, Y: Integer;
  end;

{ Rectangle object }

  TRect = object
    A, B: TPoint;
    procedure Assign(XA, YA, XB, YB: Integer);
    procedure Copy(R: TRect);
    procedure Move(ADX, ADY: Integer);
    procedure Grow(ADX, ADY: Integer);
    procedure Intersect(R: TRect);
    procedure Union(R: TRect);
    function Contains(P: TPoint): Boolean;
    function Equals(R: TRect): Boolean;
    function Empty: Boolean;
  end;

{$ENDIF}

{ Dynamic string handling routines }

function NewStr(const S: String): PString;
procedure DisposeStr(P: PString);

{ Stream routines }

procedure RegisterType(var S: TStreamRec);

{ Abstract notification procedure }

procedure Abstract;

{ Objects registration procedure }

procedure RegisterObjects;

//{ Analog to DOS int 21h I/O functions }
//
//procedure DosFn;

const

{ Stream error procedure }

  StreamError: Pointer = nil;

{ Stream registration records }

const
  RCollection: TStreamRec = (
    ObjType: 50;
    VmtLink: Ofs(TypeOf(TCollection)^);
    Load: @TCollection.Load;
    Store: @TCollection.Store);

const
  RStringCollection: TStreamRec = (
    ObjType: 51;
    VmtLink: Ofs(TypeOf(TStringCollection)^);
    Load: @TStringCollection.Load;
    Store: @TStringCollection.Store);

const
  RStrCollection: TStreamRec = (
    ObjType: 69;
    VmtLink: Ofs(TypeOf(TStrCollection)^);
    Load:    @TStrCollection.Load;
    Store:   @TStrCollection.Store);

{$IFNDEF OWL}

const
  RStringList: TStreamRec = (
    ObjType: 52;
    VmtLink: Ofs(TypeOf(TStringList)^);
    Load: @TStringList.Load;
    Store: nil);

const
  RStrListMaker: TStreamRec = (
    ObjType: 52;
    VmtLink: Ofs(TypeOf(TStrListMaker)^);
    Load: nil;
    Store: @TStrListMaker.Store);

{$ENDIF}

implementation

uses
{$IFDEF OWL}
  OMemory
{$ELSE}
  Memory
{$ENDIF},
  Strings, VpSysLow;

const
  stCreate_FileMode=open_share_DenyNone or open_access_ReadWrite;

{ Analog to DOS int 21h I/O functions }
procedure DosFn; forward;

procedure Abstract;
begin
  RunError(211);
end;

{ TObject }

constructor TObject.Init;
type
  Image = record
    Link: Word;
    Data: record end;
  end;
begin
  FillChar(Image(Self).Data, SizeOf(Self) - SizeOf(TObject), 0);
end;

{ Shorthand procedure for a done/dispose }

procedure TObject.Free;
begin
  Dispose(PObject(@Self), Done);
end;

destructor TObject.Done;
begin
end;

{ TStream type registration routines }

const
  StreamTypes: PStreamRec = nil;

procedure RegisterError;
begin
  RunError(212);
end;

procedure RegisterType(var S: TStreamRec);
var
  P: PStreamRec;
begin
  P := StreamTypes;
  while (P <> nil) and (P^.ObjType <> S.ObjType) do P := P^.Next;
  if (P <> nil) or (S.ObjType = 0) then RegisterError;
  S.Next := StreamTypes;
  StreamTypes := @S;
end;

{ TStream support routines }

const
  TStream_Error = vmtHeaderSize + $04;
  TStream_Flush = vmtHeaderSize + $08;
  TStream_Read  = vmtHeaderSize + $14;
  TStream_Write = vmtHeaderSize + $20;

{ Stream error handler                                  }
{ In    eax   = Error info                              }
{       dl    = Error code                              }
{       ecx   = Stream object pointer                   }
{ Uses  eax,edx                                         }


procedure DoStreamError; assembler; {$USES ecx} {$FRAME-}
asm
                movsx   edx,dl
                push    edx             { [1]:Integer = Code    }
                push    eax             { [2]:Integer = Info    }
                push    ecx             { [3]:Pointer = Self    }
                mov     eax,[ecx]
                Call    DWord Ptr [eax].TStream_Error
end;

procedure DoStreamError_Pas(const Code:ShortInt;const Info:Integer;const Self:pointer); assembler; {$Uses All} {$Frame+}
  asm
                movsx   edx,Code
                push    edx             { [1]:Integer = Code    }
                push    Info            { [2]:Integer = Info    }
                mov     ecx,Self
                push    ecx             { [3]:Pointer = Self    }
                mov     eax,[ecx]
                Call    DWord Ptr [eax].TStream_Error
  end;

{ TStream }

constructor TStream.Init;
begin
  TObject.Init;
  Status := 0;
  ErrorInfo := 0;
end;

procedure TStream.CopyFrom(var S: TStream; Count: Longint);
var
  N: Word;
  Buffer: array[0..1023] of Byte;
begin
  while Count > 0 do
  begin
    if Count > SizeOf(Buffer) then N := SizeOf(Buffer)
    else
      N := Count;
    S.Read(Buffer, N);
    Write(Buffer, N);
    Count:=Count-N;
  end;
end;

procedure TStream.Error(Code, Info: Integer);
type
  TErrorProc = procedure(var S: TStream);
begin
  Status := Code;
  ErrorInfo := Info;
  if StreamError <> nil then TErrorProc(StreamError)(Self);
end;

procedure TStream.Flush;
begin
end;

function TStream.Get: PObject; assembler; {$USES None} {$FRAME+}
asm
                push    eax
                mov     eax,esp
                push    eax                     { [1]:Pointer = Buf   }
                push    4                       { [2]:DWord   = Count }
                mov     eax,Self
                push    eax                     { [3]:Pointer = Self  }
                mov     eax,[eax]
                Call    DWord Ptr [eax].TStream_Read
                pop     eax
                test    eax,eax                 { Return nil }
                jz      @@4
                mov     edx,StreamTypes
                jmp     @@2
              @@1:
                cmp     eax,[edx].TStreamRec.ObjType
                je      @@3
                mov     edx,[edx].TStreamRec.Next
              @@2:
                test    edx,edx
                jnz     @@1
                mov     ecx,Self
                mov     dl,stGetError
                Call    DoStreamError
                xor     eax,eax                 { Return nil }
                jmp     @@4
              @@3:
                push    Self                    { [1]:Pointer = TStream }
                push    [edx].TStreamRec.VmtLink{ [2]:DWord   = VMT     }
                push    0                       { [3]:Pointer = Self = nil: allocate in dynamic memory }
                Call    [edx].TStreamRec.Load
              @@4:                              { Return Self or nil }
end;

function TStream.GetPos: Longint;
begin
  Abstract;
end;

function TStream.GetSize: Longint;
begin
  Abstract;
end;

procedure TStream.Put(P: PObject); assembler; {$USES None} {$FRAME+}
asm
                mov     ecx,P
                jecxz   @@4
                mov     eax,[ecx]               { VMT pointer }
                mov     edx,StreamTypes
                jmp     @@2
              @@1:
                cmp     eax,[edx].TStreamRec.VmtLink
                je      @@3
                mov     edx,[edx].TStreamRec.Next
              @@2:
                test    edx,edx
                jne     @@1
                mov     ecx,Self
                mov     dl,stPutError
                Call    DoStreamError
                jmp     @@5
              @@3:
                mov     ecx,[edx].TStreamRec.ObjType
              @@4:
                push    edx
                push    ecx                     { Write object type  }
                mov     eax,esp
                push    eax                     { [1]:Pointer = Buf  }
                push    4                       { [2]:DWord   = Size }
                mov     eax,Self                { [3]:Pointer = Self }
                push    eax
                mov     eax,[eax]
                Call    DWord Ptr [eax].TStream_Write
                pop     ecx
                pop     edx
                jecxz   @@5
                push    Self                    { [1]:Pointer = TStream }
                push    P                       { [2]:Pointer = Self    }
                Call    [edx].TStreamRec.Store
              @@5:
end;

procedure TStream.Read(var Buf; Count: Word);
begin
  Abstract;
end;

function TStream.ReadStr: PString;
var
  L: Byte;
  P: PString;
begin
  Read(L, 1);
  if L > 0 then
  begin
    GetMem(P, L + 1);
    P^[0] := Char(L);
    Read(P^[1], L);
    ReadStr := P;
  end else ReadStr := nil;
end;

procedure TStream.Reset;
begin
  Status := 0;
  ErrorInfo := 0;
end;

procedure TStream.Seek(Pos: Longint);
begin
  Abstract;
end;

function TStream.StrRead: PChar;
var
  L: Word;
  P: PChar;
begin
  Read(L, SizeOf(Word));
  if L = 0 then StrRead := nil else
  begin
    GetMem(P, L + 1);
    Read(P[0], L);
    P[L] := #0;
    StrRead := P;
  end;
end;

procedure TStream.StrWrite(P: PChar);
var
  L: Word;
begin
  if P = nil then L := 0 else L := StrLen(P);
  Write(L, SizeOf(Word));
  if P <> nil then Write(P[0], L);
end;

procedure TStream.Truncate;
begin
  Abstract;
end;

procedure TStream.Write(var Buf; Count: Word);
begin
  Abstract;
end;

procedure TStream.WriteStr(P: PString);
const
  Empty: String[1] = '';
begin
  if P <> nil then Write(P^, Length(P^) + 1) else Write(Empty, 1);
end;

{ TDosStream }

{$USES ebx,esi,edi} {$FRAME+}

constructor TDosStream.Init({$IfNDef OWL}const{$EndIf} FileName: FNameStr; Mode: Word); assembler;
var
  NameBuf: array[0..255] of Char;
asm
                push    0                       { [1]:DWord = VMT       }
                push    Self                    { [2]:Pointer = Self    }
                Call    TStream.Init            { Inherited Init;       }
                mov     esi,FileName
                lea     edi,NameBuf
{$IFDEF OWL}
                // OWL requires special code, because FNameStr = PChar
                push    edi
                push    esi
                push    255
                call    StrLCopy
                lea     edx,NameBuf
{$ELSE}
                mov     edx,edi                 { edx = @FName (ASCIIZ) }
                xor     eax,eax
                cld
                lodsb
                xchg    ecx,eax
                rep     movsb                   { File name             }
                xchg    eax,ecx
                stosb                           { Null terminator       }
{$ENDIF}
                xor     ecx,ecx                 { ecx = File attribute  }
                mov     eax,Mode                { ah=DosFn,al=Open mode }
                Call    DosFn
                jnc     @@2                     { eax = File Handle     }
                mov     ecx,Self
                mov     dl,stInitError
                Call    DoStreamError
                or      eax,-1
              @@2:
                mov     ecx,Self
                mov     [ecx].TDosStream.Handle,eax
end;

destructor TDosStream.Done; assembler; {$USES ebx} {$FRAME+}
asm
                mov     eax,Self
                mov     ebx,[eax].TDosStream.Handle
                cmp     ebx,-1
                je      @@1
                mov     ah,3Eh                  { Close file            }
                Call    DosFn
              @@1:
                push    0                       { [1]:DWord = VMT       }
                push    Self                    { [2]:Pointer = Self    }
                Call    TStream.Done            { Inherited Done;       }
end;

function TDosStream.GetPos: Longint; assembler; {$USES ebx} {$FRAME-}
asm
                mov     eax,Self
                cmp     [eax].TDosStream.Status,stOk
                jne     @@1
                xor     ecx,ecx                 { ecx = Distance        }
                mov     ebx,[eax].TDosStream.Handle { ebx = File Handle }
                mov     ax,4201h                { Get current position  }
                Call    DosFn
                jnc     @@2
                mov     ecx,Self
                mov     dl,stError
                Call    DoStreamError           { eax = Current FilePtr }
              @@1:
                or      eax,-1
              @@2:
end;

function TDosStream.GetSize: Longint; assembler; {$USES ebx} {$FRAME-}
asm
                mov     eax,Self
                cmp     [eax].TDosStream.Status,stOk
                jne     @@1
                xor     ecx,ecx                 { ecx = Distance        }
                mov     ebx,[eax].TDosStream.Handle
                mov     ax,4201h                { ebx = Handle          }
                Call    DosFn
                push    eax                     { Save current position }
                xor     ecx,ecx
                mov     ax,4202h                { Move to the EOF       }
                Call    DosFn
                pop     ecx
                push    eax
                mov     ax,4200h                { Restore old position  }
                Call    DosFn
                pop     eax
                jnc     @@2
                mov     ecx,Self
                mov     dl,stError
                Call    DoStreamError
              @@1:
                or      eax,-1
              @@2:
end;

procedure TDosStream.Read(var Buf; Count: Word); assembler; {$USES ebx,edi} {$FRAME-}
asm
                mov     edi,Self
                cmp     [edi].TDosStream.Status,stOk
                jne     @@2
                mov     edx,Buf                 { edx = Buffer@         }
                mov     ecx,Count               { ecx = Count           }
                mov     ebx,[edi].TDosStream.Handle { ebx = File Handle }
                mov     ah,3Fh                  { Read file             }
                Call    DosFn
                mov     dl,stError
                jc      @@1
                cmp     eax,ecx
                je      @@3
                xor     eax,eax
                mov     dl,stReadError
              @@1:
                mov     ecx,edi
                Call    DoStreamError
              @@2:
                mov     edi,Buf
                mov     ecx,Count
                xor     al,al
                cld
                rep     stosb
              @@3:
end;

procedure TDosStream.Seek(Pos: Longint); assembler; {$USES ebx} {$FRAME-}
asm
                mov     eax,Self
                cmp     [eax].TDosStream.Status,stOk
                jne     @@2
                mov     ecx,Pos
                test    ecx,ecx
                jns     @@1
                xor     ecx,ecx
              @@1:
                mov     ebx,[eax].TDosStream.Handle
                mov     ax,4200h
                Call    DosFn
                jnc     @@2
                mov     ecx,Self
                mov     dl,stError
                Call    DoStreamError
              @@2:
end;

procedure TDosStream.Truncate; assembler; {$USES ebx} {$FRAME-}
asm
                mov     eax,Self
                cmp     [eax].TDosStream.Status,stOk
                jne     @@1
                xor     ecx,ecx                 { ecx=0: Truncate file  }
                mov     ebx,[eax].TDosStream.Handle
                mov     ah,40h                  { Write file            }
                Call    DosFn
                jnc     @@1
                mov     ecx,Self
                mov     dl,stError
                Call    DoStreamError
              @@1:
end;

procedure TDosStream.Write(var Buf; Count: Word); assembler; {$USES ebx} {$FRAME-}
asm
                mov     eax,Self
                cmp     [eax].TDosStream.Status,stOk
                jne     @@2
                mov     edx,Buf
                mov     ecx,Count
                mov     ebx,[eax].TDosStream.Handle
                mov     ah,40h
                Call    DosFn
                mov     dl,stError
                jc      @@1
                cmp     eax,ecx
                je      @@2
                xor     eax,eax
                mov     dl,stWriteError
              @@1:
                mov     ecx,Self
                Call    DoStreamError
              @@2:
end;

{ TBufStream }

{ Flush TBufStream buffer                               }
{ In    AL    = Flush mode (0=Read, 1=Write, 2=Both)    }
{       edi   = TBufStream pointer                      }
{ Out   ZF    = Status test                             }

procedure FlushBuffer; assembler; {$USES ebx} {$FRAME-}
asm
                mov     ecx,[edi].TBufStream.BufPtr
                sub     ecx,[edi].TBufStream.BufEnd
                je      @@3
                mov     ebx,[edi].TDosStream.Handle
                ja      @@1
                cmp     al,1
                je      @@4
                mov     ax,4201h                { Seek from current position }
                Call    DosFn
                jmp     @@3
              @@1:
                cmp     al,0
                je      @@4
                mov     edx,[edi].TBufStream.Buffer
                mov     ah,40h
                Call    DosFn
                mov     dl,stError
                jc      @@2
                cmp     eax,ecx
                je      @@3
                xor     eax,eax
                mov     dl,stWriteError
              @@2:
                mov     ecx,edi
                Call    DoStreamError
              @@3:
                xor     eax,eax
                mov     [edi].TBufStream.BufPtr,eax
                mov     [edi].TBufStream.BufEnd,eax
                cmp     [edi].TStream.Status,stOk
              @@4:
end;

constructor TBufStream.Init(const FileName: FNameStr; Mode, Size: Word);
begin
  TDosStream.Init(FileName, Mode);
  BufSize := Size;
  if Size = 0 then Error(stInitError, 0)
  else GetMem(Buffer, Size);
  BufPtr := 0;
  BufEnd := 0;
end;

destructor TBufStream.Done;
begin
  TBufStream.Flush;
  TDosStream.Done;
  FreeMem(Buffer, BufSize);
end;

procedure TBufStream.Flush; assembler;  {$USES edi} {$FRAME-}
asm
                mov     edi,Self
                cmp     [edi].TBufStream.Status,stOk
                jne     @@1
                mov     al,2                    { Read/Write mode }
                Call    FlushBuffer
              @@1:
end;

function TBufStream.GetPos: Longint; assembler; {$USES edi} {$FRAME-}
asm
                mov     edi,Self
                push    edi
                Call    TDosStream.GetPos
                test    eax,eax
                js      @@1
                sub     eax,[edi].TBufStream.BufEnd
                add     eax,[edi].TBufStream.BufPtr
              @@1:
end;

function TBufStream.GetSize: Longint; assembler; {$USES None} {$FRAME-}
asm
                mov     eax,Self
                push    eax
                push    eax
                Call    TBufStream.Flush
                Call    TDosStream.GetSize
end;

procedure TBufStream.Read(var Buf; Count: Word); assembler; {$USES ebx,esi,edi} {$FRAME-}
asm
                mov     edi,Self
                cmp     [edi].TBufStream.Status,stOk
                jne     @@6
                mov     al,1                    { Write mode }
                Call    FlushBuffer
                jne     @@6
                xor     ebx,ebx
              @@1:
                mov     ecx,Count
                sub     ecx,ebx
                je      @@7
                mov     edi,Self
                mov     eax,[edi].TBufStream.BufEnd
                sub     eax,[edi].TBufStream.BufPtr
                ja      @@2
                push    ecx
                push    ebx
                mov     edx,[edi].TBufStream.Buffer
                mov     ecx,[edi].TBufStream.BufSize
                mov     ebx,[edi].TBufStream.Handle
                mov     ah,3Fh
                Call    DosFn
                pop     ebx
                pop     ecx
                mov     dl,stError
                jc      @@5
                and     [edi].TBufStream.BufPtr,0
                mov     [edi].TBufStream.BufEnd,eax
                test    eax,eax
                je      @@4
              @@2:
                cmp     ecx,eax
                jb      @@3
                mov     ecx,eax
              @@3:
                mov     esi,[edi].TBufStream.Buffer
                add     esi,[edi].TBufStream.BufPtr
                add     [edi].TBufStream.BufPtr,ecx
                mov     edi,Buf
                add     edi,ebx
                add     ebx,ecx
                cld
                rep     movsb
                jmp     @@1
              @@4:
                mov     dl,stReadError
              @@5:
                mov     ecx,edi
                Call    DoStreamError
              @@6:
                mov     edi,Buf
                mov     ecx,Count
                xor     al,al
                cld
                rep     stosb
              @@7:
end;

procedure TBufStream.Seek(Pos: Longint); assembler; {$USES edi} {$FRAME-}
asm
                mov     edi,Self
                push    edi
                Call    TDosStream.GetPos
                test    eax,eax
                js      @@2
                sub     eax,Pos
                jne     @@1
                test    eax,eax
                je      @@1
                mov     edx,[edi].TBufStream.BufEnd
                sub     edx,eax
                jb      @@1
                mov     [edi].TBufStream.BufPtr,edx
                jmp     @@2
              @@1:
                push    edi
                Call    TBufStream.Flush
                push    Pos
                push    edi
                Call    TDosStream.Seek
              @@2:
end;

procedure TBufStream.Truncate;
begin
  TBufStream.Flush;
  TDosStream.Truncate;
end;

procedure TBufStream.Write(var Buf; Count: Word); assembler; {$USES esi,edi} {$FRAME-}
asm
                mov     edi,Self
                cmp     [edi].TBufStream.Status,stOk
                jne     @@4
                mov     al,0                    { Read mode }
                Call    FlushBuffer
                jne     @@4
                xor     edx,edx
              @@1:
                mov     ecx,Count
                sub     ecx,edx
                je      @@4
                mov     edi,Self
                mov     eax,[edi].TBufStream.BufSize
                sub     eax,[edi].TBufStream.BufPtr
                ja      @@2
                push    ecx
                push    edx
                mov     al,1                    { Write mode }
                Call    FlushBuffer
                pop     edx
                pop     ecx
                jne     @@4
                mov     eax,[edi].TBufStream.BufSize
              @@2:
                cmp     ecx,eax
                jb      @@3
                mov     ecx,eax
              @@3:
                mov     eax,[edi].TBufStream.BufPtr
                add     [edi].TBufStream.BufPtr,ecx
                mov     edi,[edi].TBufStream.Buffer
                add     edi,eax
                mov     esi,Buf
                add     esi,edx
                add     edx,ecx
                cld
                rep     movsb
                jmp     @@1
              @@4:
end;

{ TMemoryStream }

const
  MaxBlockArraySize = 512 * 1024 * 1024 div 4;
  DefaultBlockSize = 8 * 1024;

{ Selects TMemoryStream memory block                            }
{ In    edi   = TMemoryStream pointer                           }
{ Out   ecx   = Distance between position and end of block      }
{       esi   = Position within the selected block              }

procedure MemSelectBlock; assembler; {$USES None} {$FRAME-}
asm
                mov     eax,[edi].TMemoryStream.Position
                xor     edx,edx
                mov     ecx,[edi].TMemoryStream.BlockSize
                div     ecx
                sub     ecx,edx
                mov     esi,edx
                shl     eax,2
                mov     [edi].TMemoryStream.CurBlock,eax
end;

const
  MemStreamSize = (SizeOf(TMemoryStream) - SizeOf(TStream)) div 2;

constructor TMemoryStream.Init(ALimit: Longint; ABlockSize: Word); assembler; {$USES edi} {$FRAME+}
asm
                push    0
                push    Self
                Call    TStream.Init
                mov     edi,Self
                cmp     ABlockSize,0
                jnz     @@1
                mov     ABlockSize,DefaultBlockSize
              @@1:
                mov     ecx,ABlockSize
                mov     eax,ALimit
                xor     edx,edx
                div     ecx
                neg     edx
                adc     eax,0
                mov     [edi].TMemoryStream.BlockSize,ecx
                push    eax                     { [1]:DWord = ALimit    }
                push    edi                     { [2]:Pointer = Self    }
                Call    ChangeListSize
                test    al,al
                jnz     @@2
                mov     dl,stInitError
                mov     ecx,edi
                Call    DoStreamError
                and     ALimit,0
              @@2:
                mov     eax,ALimit
                mov     [edi].TMemoryStream.Size,eax
end;

destructor TMemoryStream.Done;
begin
  ChangeListSize(0);
  inherited Done;
end;

{ from rtl_changes.txt }
function TMemoryStream.ChangeListSize(ALimit: Word): Boolean;
var
  AItems : PPtrArray;
  P      : Pointer;
  i      : Word;
begin
  if ALimit > MaxBlockArraySize then
    ALimit := MaxBlockArraySize;

  if ALimit <> BlockCount then
  begin
    ChangeListSize := False;

    if ALimit = 0 then
      AItems := nil
    else
    begin
      AItems := MemAlloc(ALimit * SizeOf(Pointer));
      if AItems = nil then
        Exit;

      FillChar(AItems^, ALimit * SizeOf(Pointer), 0);
      if (BlockCount <> 0) and (BlockList <> nil) then
      begin
        if ALimit < BlockCount then i := ALimit
                               else i := BlockCount;
        Move(BlockList^, AItems^, i * SizeOf(Pointer));
      end;
    end;

    if ALimit < BlockCount then
    begin
      for i := ALimit To BlockCount - 1 do
      begin
        P := BlockList^ [i];
        if P <> nil then
          FreeMem(P, BlockSize);
      end;

      ChangeListSize := True;
    end else
    begin
      i := BlockCount;

      while i < ALimit do
      begin
        P := MemAlloc(BlockSize);
        if P <> nil then
          AItems^ [i] := P
        else
          Break;

        Inc(i);
      end;

      if i = ALimit then
        ChangeListSize := True;
    end;

    if BlockCount > 0 then
      FreeMem(BlockList, BlockCount * SizeOf(Pointer));
    BlockList := AItems;
    BlockCount := ALimit;
  end else
    ChangeListSize := True;
end;

function TMemoryStream.GetPos: Longint;
begin
  if Status = stOk then GetPos := Position else GetPos := -1;
end;

function TMemoryStream.GetSize: Longint;
begin
  if Status = stOk then GetSize := Size else GetSize := -1;
end;

procedure TMemoryStream.Read(var Buf; Count: Word); assembler; {$USES ebx,esi,edi} {$FRAME+}
asm
                mov     edi,Self
                cmp     [edi].TMemoryStream.Status,stOk
                jne     @@3
                xor     ebx,ebx
                mov     eax,[edi].TMemoryStream.Position
                add     eax,Count
                cmp     eax,[edi].TMemoryStream.Size
                jbe     @@7
                xor     eax,eax
                mov     ecx,edi
                mov     dl,stReadError
                Call    DoStreamError
              @@3:
                mov     edi,Buf
                mov     ecx,Count
                xor     al,al
                cld
                rep     stosb
                jmp     @@8
              @@5:
                Call    MemSelectBlock
                mov     eax,Count
                sub     eax,ebx
                cmp     ecx,eax
                jb      @@6
                mov     ecx,eax
              @@6:
                add     [edi].TMemoryStream.Position,ecx
                push    edi
                mov     edx,[edi].TMemoryStream.CurBlock
                mov     eax,[edi].TMemoryStream.BlockList
                add     esi,[eax+edx]           { Block base pointer }
                mov     edi,Buf
                add     edi,ebx
                add     ebx,ecx
                mov     al,cl
                shr     ecx,2
                and     al,11b
                cld
                rep     movsd
                mov     cl,al
                rep     movsb
                pop     edi
              @@7:
                cmp     ebx,Count
                jb      @@5
              @@8:
end;

procedure TMemoryStream.Seek(Pos: Longint);
begin
  if Status = stOk then
    if Pos > 0 then Position := Pos else Position := 0;
end;

procedure TMemoryStream.Truncate; assembler; {$USES None} {$FRAME-}
asm
                mov     ecx,Self
                cmp     [ecx].TMemoryStream.Status,stOk
                jne     @@2
                mov     eax,[ecx].TMemoryStream.Position
                xor     edx,edx
                div     [ecx].TMemoryStream.BlockSize
                neg     edx
                adc     eax,0
                push    eax                     { [1]:DWord = ALimit    }
                push    ecx                     { [2]:Pointer = Self    }
                Call    ChangeListSize
                mov     ecx,Self
                test    al,al
                jnz     @@1
                mov     dl,stError
                Call    DoStreamError
                jmp     @@2
              @@1:
                mov     eax,[ecx].TMemoryStream.Position
                mov     [ecx].TMemoryStream.Size,eax
              @@2:
end;

procedure TMemoryStream.Write(var Buf; Count: Word); assembler; {$USES ebx,esi,edi} {$FRAME+}
asm
                mov     edi,Self
                cmp     [edi].TMemoryStream.Status,stOk
                jne     @@7
                xor     ebx,ebx
                mov     eax,[edi].TMemoryStream.Position
                add     eax,Count
                xor     edx,edx
                div     [edi].TMemoryStream.BlockSize
                neg     edx
                adc     eax,0
                cmp     eax,[edi].TMemoryStream.BlockCount
                jbe     @@4
                push    eax                     { [1]:DWord = ALimit    }
                push    edi                     { [2]:Pointer = Self    }
                Call    ChangeListSize
                test    al,al
                jnz     @@4
              @@1:
                mov     ecx,edi
                mov     dl,stWriteError
                Call    DoStreamError
                jmp     @@7
              @@2:
                Call    MemSelectBlock
                mov     eax,Count
                sub     eax,ebx
                cmp     ecx,eax
                jb      @@3
                mov     ecx,eax
              @@3:
                add     [edi].TMemoryStream.Position,ecx
                push    edi
                mov     edx,[edi].TMemoryStream.CurBlock
                mov     eax,[edi].TMemoryStream.BlockList
                add     esi,[eax+edx]
                mov     edi,esi
                mov     esi,Buf
                add     esi,ebx
                add     ebx,ecx
                mov     al,cl
                shr     ecx,2
                and     al,11b
                cld
                rep     movsd
                mov     cl,al
                rep     movsb
                pop     edi
              @@4:
                cmp     ebx,Count
                jb      @@2
              @@5:
                mov     eax,[edi].TMemoryStream.Position
                cmp     eax,[edi].TMemoryStream.Size
                jbe     @@7
              @@6:
                mov     [edi].TMemoryStream.Size,eax
              @@7:
end;

{ TCollection }

const
  TCollection_Error    = vmtHeaderSize + $04;
  TCollection_SetLimit = vmtHeaderSize + $1C;

{ Reports collection error                                      }
{ In     al   = Error code                                      }
{       edx   = Error info                                      }
{       edi   = TCollection pointer                             }

procedure CollectionError; assembler; {$USES None} {$FRAME-}
asm
                movsx   eax,al
                push    eax                     { [1]:DWord = Error code }
                push    edx                     { [2]:DWord = Error info }
                push    edi                     { [3]:Pointer = Self     }
                mov     eax,[edi]
                Call    DWord Ptr [eax].TCollection_Error
end;

constructor TCollection.Init(ALimit, ADelta: Integer);
begin
  TObject.Init;
  Items := nil;
  Count := 0;
  Limit := 0;
  Delta := ADelta;
  SetLimit(ALimit);
end;

constructor TCollection.Load(var S: TStream);
var
  C, I: Integer;
begin
  S.Read(Count, SizeOf(Integer) * 3);
  Items := nil;
  C := Count;
  I := Limit;
  Count := 0;
  Limit := 0;
  SetLimit(I);
  Count := C;
  for I := 0 to C - 1 do AtPut(I, GetItem(S));
end;

destructor TCollection.Done;
begin
  FreeAll;
  SetLimit(0);
end;

function TCollection.At(Index: Integer): Pointer; assembler; {$USES edi} {$FRAME-}
asm
                mov     edi,Self
                mov     edx,Index
                test    edx,edx
                jl      @@1
                cmp     edx,[edi].TCollection.Count
                jge     @@1
                mov     edi,[edi].TCollection.Items
                mov     eax,[edi+edx*4]
                jmp     @@2
              @@1:
                mov     al,coIndexError
                Call    CollectionError
                xor     eax,eax
              @@2:
end;

procedure TCollection.AtDelete(Index: Integer); assembler; {$USES esi,edi} {$FRAME-}
asm
                mov     edi,Self
                mov     edx,Index
                test    edx,edx
                jl      @@1
                cmp     edx,[edi].TCollection.Count
                jge     @@1
                dec     [edi].TCollection.Count
                mov     ecx,[edi].TCollection.Count
                sub     ecx,edx
                je      @@2
                cld
                mov     edi,[edi].TCollection.Items
                lea     edi,[edi+edx*4]
                lea     esi,[edi+4]
                rep     movsd
                jmp     @@2
              @@1:
                mov     al,coIndexError
                Call    CollectionError
              @@2:
end;

procedure TCollection.AtFree(Index: Integer);
var
  Item: Pointer;
begin
  Item := At(Index);
  AtDelete(Index);
  FreeItem(Item);
end;

procedure TCollection.AtInsert(Index: Integer; Item: Pointer); assembler; {&USES esi, edi} {$FRAME-}
asm
                mov     edi,Self
                mov     edx,Index
                test    edx,edx
                jl      @@3
                mov     ecx,[edi].TCollection.Count
                cmp     edx,ecx
                jg      @@3
                cmp     ecx,[edi].TCollection.Limit
                jne     @@1
                push    ecx
                push    edx
                add     ecx,[edi].TCollection.Delta
                push    ecx                     { [1]:DWord = ALimit    }
                push    edi                     { [2]:Pointer = Self    }
                mov     eax,[edi]
                Call    DWord Ptr [eax].TCollection_SetLimit
                pop     edx
                pop     ecx
                cmp     ecx,[edi].TCollection.Limit
                je      @@4
              @@1:
                inc     [edi].TCollection.Count
                std
                mov     edi,[edi].TCollection.Items
                lea     edi,[edi+ecx*4]
                sub     ecx,edx
                je      @@2
                lea     esi,[edi-4]
                rep     movsd
              @@2:
                mov     eax,Item
                stosd
                cld
                jmp     @@6
              @@3:
                mov     al,coIndexError
                jmp     @@5
              @@4:
                mov     al,coOverflow
                mov     edx,ecx
              @@5:
                Call    CollectionError
              @@6:
end;

procedure TCollection.AtPut(Index: Integer; Item: Pointer); assembler; {$USES edi} {$FRAME-}
asm
                mov     eax,Item
                mov     edi,Self
                mov     edx,Index
                test    edx,edx
                jl      @@1
                cmp     edx,[edi].TCollection.Count
                jge     @@1
                mov     edi,[edi].TCollection.Items
                mov     [edi+edx*4],eax
                jmp     @@2
              @@1:
                mov     al,coIndexError
                Call    CollectionError
              @@2:
end;

procedure TCollection.Delete(Item: Pointer);
begin
  AtDelete(IndexOf(Item));
end;

procedure TCollection.DeleteAll;
begin
  Count := 0;
end;

procedure TCollection.Error(Code, Info: Integer);
begin
  RunError(212 - Code);
end;

function TCollection.FirstThat(Test: Pointer): Pointer; assembler; {$USES ebx} {$FRAME-}
asm
                mov     edx,Self
                mov     ecx,[edx].TCollection.Count
                jecxz   @@3
                mov     ebx,Test
                mov     edx,[edx].TCollection.Items
              @@1:
                push    edx
                push    ecx
                push    DWord Ptr [edx]         { [1]:Pointer = Item }
                Call    ebx
                pop     ecx
                pop     edx
                test    al,al
                jnz     @@2
                add     edx,4
                loop    @@1
                jmp     @@3
              @@2:
                mov     ecx,[edx]
              @@3:
                mov     eax,ecx
end;

procedure TCollection.ForEach(Action: Pointer); assembler; {$USES ebx} {$FRAME-}
asm
                mov     edx,Self
                mov     ecx,[edx].TCollection.Count
                jecxz   @@2
                mov     ebx,Action
                mov     edx,[edx].TCollection.Items
              @@1:
                push    edx
                push    ecx
                push    DWord Ptr [edx]         { [1]:Pointer = Item }
                Call    ebx
                pop     ecx
                pop     edx
                add     edx,4
                loop    @@1
              @@2:
end;

procedure TCollection.Free(Item: Pointer);
begin
  Delete(Item);
  FreeItem(Item);
end;

procedure TCollection.FreeAll;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do FreeItem(At(I));
  Count := 0;
end;

procedure TCollection.FreeItem(Item: Pointer);
begin
  if Item <> nil then Dispose(PObject(Item), Done);
end;

function TCollection.GetItem(var S: TStream): Pointer;
begin
  GetItem := S.Get;
end;

function TCollection.IndexOf(Item: Pointer): Integer; assembler; {$USES edi} {$FRAME-}
asm
                mov     eax,Item
                mov     edi,Self
                mov     ecx,[edi].TCollection.Count
                jecxz   @@1
                mov     edi,[edi].TCollection.Items
                mov     edx,edi
                cld
                repne   scasd
                jne     @@1
                mov     eax,edi
                sub     eax,edx
                shr     eax,2
                dec     eax
                jmp     @@2
              @@1:
                xor     eax,eax
                dec     eax
              @@2:
end;

{ from rtl_changes.txt }
procedure TCollection.Insert(Item: Pointer); assembler; {&USES edi} {$FRAME-}
asm
                mov     edi,Self
                mov     edx,[edi].TCollection.Count
                mov     ecx,edx
                cmp     ecx,[edi].TCollection.Limit
                jne     @@1
                push    ecx
                push    edx
                add     ecx,[edi].TCollection.Delta
                push    ecx                     { [1]:DWord = ALimit    }
                push    edi                     { [2]:Pointer = Self    }
                mov     eax,[edi]
                Call    DWord Ptr [eax].TCollection_SetLimit
                pop     edx
                pop     ecx
                cmp     ecx,[edi].TCollection.Limit
                je      @@4
              @@1:
                inc     [edi].TCollection.Count
                mov     edi,[edi].TCollection.Items
                mov     eax,Item
                mov     [edi+ecx*4], eax
                jmp     @@6
              @@4:
                mov     al,coOverflow
                mov     edx,ecx
                Call    CollectionError
              @@6:
end;

function TCollection.LastThat(Test: Pointer): Pointer; assembler; {$USES ebx} {$FRAME-}
asm
                mov     edx,Self
                mov     ecx,[edx].TCollection.Count
                jecxz   @@3
                mov     edx,[edx].TCollection.Items
                lea     edx,[edx+ecx*4]
                mov     ebx,Test
              @@1:
                sub     edx,4
                push    edx
                push    ecx
                push    DWord Ptr [edx]         { [1]:Pointer = Item }
                Call    ebx
                pop     ecx
                pop     edx
                test    al,al
                jnz     @@2
                loop    @@1
                jmp     @@3
              @@2:
                mov     ecx,[edx]
              @@3:
                mov     eax,ecx
end;

procedure TCollection.Pack; assembler; {$USES esi,edi} {$FRAME-}
asm
                mov     edx,Self
                mov     ecx,[edx].TCollection.Count
                jecxz   @@3
                mov     edi,[edx].TCollection.Items
                mov     esi,edi
                cld
              @@1:
                lodsd
                test    eax,eax
                jz      @@2
                stosd
              @@2:
                loop    @@1
                sub     edi,[edx].TCollection.Items
                shr     edi,2
                mov     [edx].TCollection.Count,edi
              @@3:
end;

procedure TCollection.PutItem(var S: TStream; Item: Pointer);
begin
  S.Put(Item);
end;

procedure TCollection.SetLimit(ALimit: Integer);
var
  AItems: PItemList;
begin
  if ALimit < Count then ALimit := Count;
  if ALimit > MaxCollectionSize then ALimit := MaxCollectionSize;
  if ALimit <> Limit then
  begin
    if ALimit = 0 then
      AItems := nil
    else
      GetMem(AItems, ALimit * SizeOf(Pointer));
    if (AItems <> nil) or (ALimit = 0) then begin
      if (Count <> 0) and (Items <> nil) then
        Move(Items^, AItems^, Count * SizeOf(Pointer));
      if Limit <> 0 then FreeMem(Items, Limit * SizeOf(Pointer));
      Items := AItems;
      Limit := ALimit;
    end;
  end;
end;

procedure TCollection.Store(var S: TStream);

procedure DoPutItem(P: Pointer);
begin
  PutItem(S, P);
end;

begin
  S.Write(Count, SizeOf(Integer) * 3);
  ForEach(@DoPutItem);
end;

{ TSortedCollection }

constructor TSortedCollection.Init(ALimit, ADelta: Integer);
begin
  TCollection.Init(ALimit, ADelta);
  Duplicates := False;
end;

constructor TSortedCollection.Load(var S: TStream);
begin
  TCollection.Load(S);
  S.Read(Duplicates, SizeOf(Boolean));
end;

function TSortedCollection.Compare(Key1, Key2: Pointer): Integer;
begin
  Abstract;
end;

function TSortedCollection.IndexOf(Item: Pointer): Integer;
var
  I: Integer;
begin
  IndexOf := -1;
  if Search(KeyOf(Item), I) then
  begin
    if Duplicates then
      while (I < Count) and (Item <> Items^[I]) do Inc(I);
    if I < Count then IndexOf := I;
  end;
end;

procedure TSortedCollection.Insert(Item: Pointer);
var
  I: Integer;
begin
  if not Search(KeyOf(Item), I) or Duplicates then AtInsert(I, Item);
end;

function TSortedCollection.KeyOf(Item: Pointer): Pointer;
begin
  KeyOf := Item;
end;

function TSortedCollection.Search(Key: Pointer; var Index: Integer): Boolean;
var
  L, H, I, C: Integer;
begin
  Search := False;
  L := 0;
  H := Count - 1;
  while L <= H do
  begin
    I := (L + H) shr 1;
    C := Compare(KeyOf(Items^[I]), Key);
    if C < 0 then L := I + 1 else
    begin
      H := I - 1;
      if C = 0 then
      begin
        Search := True;
        if not Duplicates then L := I;
      end;
    end;
  end;
  Index := L;
end;

procedure TSortedCollection.Store(var S: TStream);
begin
  TCollection.Store(S);
  S.Write(Duplicates, SizeOf(Boolean));
end;

{ TStringCollection }

{$USES esi,edi} {$FRAME-}

function TStringCollection.Compare(Key1, Key2: Pointer): Integer; assembler;
asm
                cld
                xor     eax,eax
                xor     edx,edx
                mov     esi,Key1
                mov     edi,Key2
                lodsb
                mov     dl,[edi]
                inc     edi
                mov     ecx,eax
                cmp     cl,dl
                jbe     @@1
                mov     cl,dl
              @@1:
                repe    cmpsb
                je      @@2
                mov     al,[esi-1]
                mov     dl,[edi-1]
              @@2:
                sub     eax,edx
end;

procedure TStringCollection.FreeItem(Item: Pointer);
begin
  DisposeStr(Item);
end;

function TStringCollection.GetItem(var S: TStream): Pointer;
begin
  GetItem := S.ReadStr;
end;

procedure TStringCollection.PutItem(var S: TStream; Item: Pointer);
begin
  S.WriteStr(Item);
end;

{ TStrCollection }

function TStrCollection.Compare(Key1, Key2: Pointer): Integer;
begin
  Compare := StrComp(Key1, Key2);
end;

procedure TStrCollection.FreeItem(Item: Pointer);
begin
  StrDispose(Item);
end;

function TStrCollection.GetItem(var S: TStream): Pointer;
begin
  GetItem := S.StrRead;
end;

procedure TStrCollection.PutItem(var S: TStream; Item: Pointer);
begin
  S.StrWrite(Item);
end;

{$IFNDEF OWL}

{ Private resource manager types }

const
  RStreamMagic          = $52504246; { 'FBPR' }
  RStreamBackLink       = $4C424246; { 'FBBL' }

type
  TStreamHeader         =
    packed record
      Signature:        Longint;
      ResSize:          Longint;
      IndexPos_:        Longint;
    end;

  TStreamBackLinkHeader =
    packed record
      Signature:        Longint;
      LinkSize:         Longint;
    end;

type
  PResourceItem = ^TResourceItem;
  TResourceItem = record
    Pos: Longint;
    Size: Longint;
    Key: String;
  end;

{ TResourceCollection }

procedure TResourceCollection.FreeItem(Item: Pointer);
begin
  FreeMem(Item, Length(PResourceItem(Item)^.Key) +
    (SizeOf(TResourceItem) - SizeOf(String) + 1));
end;

function TResourceCollection.GetItem(var S: TStream): Pointer;
var
  Pos: Longint;
  Size: Longint;
  L: Byte;
  P: PResourceItem;
begin
  S.Read(Pos, SizeOf(Pos));
  S.Read(Size, SizeOf(Size));
  S.Read(L, SizeOf(L));
  GetMem(P, L + (SizeOf(TResourceItem) - SizeOf(String) + 1));
  P^.Pos := Pos;
  P^.Size := Size;
  P^.Key[0] := Char(L);
  S.Read(P^.Key[1], L);
  GetItem := P;
end;

function TResourceCollection.KeyOf(Item: Pointer): Pointer; assembler; {$USES None} {$FRAME-}
asm
                mov     eax,Item
                add     eax,OFFSET TResourceItem.Key
end;

procedure TResourceCollection.PutItem(var S: TStream; Item: Pointer);
begin
  S.Write(PResourceItem(Item)^, Length(PResourceItem(Item)^.Key) +
    (SizeOf(TResourceItem) - SizeOf(String) + 1));
end;

{ TResourceFile }

constructor TResourceFile.Init(AStream: PStream);
type

  TExeHeader = record
    Signature:  SmallWord;
    LastCount:  SmallWord;
    PageCount:  SmallWord;
    ReloCount:  SmallWord;
    eHdrSize:   SmallWord;
    eMinAbove:  SmallWord;
    eMaxAbove:  SmallWord;
    eInitSS:    SmallWord;
    eInitSP:    SmallWord;
    eCheckSum:  SmallWord;
    eInitPC:    SmallWord;
    eInitCS:    SmallWord;
    eRelocOfs:  SmallWord;
    eOvlyNum:   SmallWord;
    eRelocTab:  SmallWord;
    eSpace:     array [1..30] of Byte;
    eNewHeader: Word;
  end;

  TInfoHeader = packed record
    Signature: SmallWord;
    InfoType: SmallWord;
    InfoSize: Longint;
  end;


  THeader = packed record
    case Integer of
      0:(Exe: TExeHeader);
      1:(BackLink: TStreamBackLinkHeader);
      2:(SH: TStreamHeader);
      3:(Info: TInfoHeader); (* debuginfo *)
    end;

var
  Found, Stop: Boolean;
  Header: THeader;

begin
  TObject.Init;
  Stream := AStream;
  BasePos := Stream^.GetPos;
  Found := False;
  repeat
    Stop := True;
    if (BasePos>=0) and (BasePos <= Stream^.GetSize - SizeOf(THeader)) then
    begin
      Stream^.Seek(BasePos);
      FillChar(Header, SizeOf(Header), 0);
      Stream^.Read(Header, SizeOf(Header));
      case Header.Exe.Signature of
        $5A4D,$4d4a,                            { 'MZ','ZM' }
        $454e,$584C,$4550,$454C,                { 'NE','LX','PE','LE' }
        $457f:                                  { #$7F'ELF' }
          begin
            BasePos := Stream^.GetSize - SizeOf(TStreamBackLinkHeader);
            Stop := False;
          end;
        (RStreamMagic and $ffff) xor $2020,
        (RStreamMagic and $ffff):               { 'FB' }
          begin
            Stop := False;
            case Header.Info.Infotype of
              (RStreamMagic shr 16):            { 'PR': Found Resource }
                begin
                  Found := True;
                  Stop := True;
                end;
              (RStreamBackLink shr 16):         { 'BL': Found BackLink }
                BasePos:=BasePos - Header.BackLink.LinkSize - SizeOf(TStreamBackLinkHeader);
              $4648:                            { 'HF': Found HelpFile }
                //BasePos:=BasePos - SizeOf(THeader) * 2;
                BasePos := BasePos - SizeOf(TStreamBackLinkHeader)
            else
              Stop := True;
            end;
          end;
        $424E:                                  { 'NB' }
          if Header.Info.InfoType = $3230 then  { '02': Found Debug Info}
          begin
            BasePos:=BasePos-Header.Info.InfoSize;
            Stop := False;
          end;
      end;
    end;
  until Stop;
  if Found then
  begin
    Stream^.Seek(BasePos);
    Stream^.Read(Header.SH,SizeOf(Header.SH));
    IndexPos := Header.SH.IndexPos_;
    Stream^.Seek(BasePos + IndexPos);
    Index.Load(Stream^);
  end else
  begin
    IndexPos := SizeOf(TStreamHeader);
    Index.Init(0, 8);
  end;
end;

destructor TResourceFile.Done;
begin
  Flush;
  Index.Done;
  Dispose(Stream, Done);
end;

function TResourceFile.Count: Integer;
begin
  Count := Index.Count;
end;

procedure TResourceFile.Delete(Key: String);
var
  I: Integer;
begin
  if Index.Search(@Key, I) then
  begin
    Index.Free(Index.At(I));
    Modified := True;
  end;
end;

procedure TResourceFile.Flush;
var
  StreamBackLinkHeader:TStreamBackLinkHeader;
  StreamHeader:TStreamHeader;

begin
  if Modified then
  begin
    Stream^.Seek(BasePos + IndexPos);
    Index.Store(Stream^);
    with StreamHeader do
      begin
        Signature := RStreamMagic;
        ResSize := Stream^.GetPos - BasePos;;
        IndexPos_ := IndexPos;
      end;
    with StreamBackLinkHeader do
      begin
        Signature := RStreamBackLink;
        LinkSize := StreamHeader.ResSize + SizeOf(StreamBackLinkHeader);
      end;
    Stream^.Write(StreamBackLinkHeader, SizeOf(StreamBackLinkHeader));
    Stream^.Seek(BasePos);

    Stream^.Write(StreamHeader, SizeOf(StreamHeader));
    Stream^.Flush;
    Modified := False;
  end;
end;

function TResourceFile.Get(Key: String): PObject;
var
  I: Integer;
begin
  if not Index.Search(@Key, I) then Get := nil else
  begin
    Stream^.Seek(BasePos + PResourceItem(Index.At(I))^.Pos);
    Get := Stream^.Get;
  end;
end;

function TResourceFile.KeyAt(I: Integer): String;
begin
  KeyAt := PResourceItem(Index.At(I))^.Key;
end;

procedure TResourceFile.Put(Item: PObject; Key: String);
var
  I: Integer;
  P: PResourceItem;
begin
  if Index.Search(@Key, I) then P := Index.At(I) else
  begin
    GetMem(P, Length(Key) + (SizeOf(TResourceItem) - SizeOf(String) + 1));
    P^.Key := Key;
    Index.AtInsert(I, P);
  end;
  P^.Pos := IndexPos;
  Stream^.Seek(BasePos + IndexPos);
  Stream^.Put(Item);
  IndexPos := Stream^.GetPos - BasePos;
  P^.Size := IndexPos - P^.Pos;
  Modified := True;
end;

function TResourceFile.SwitchTo(AStream: PStream; Pack: Boolean): PStream;
var
  NewBasePos: Longint;

procedure DoCopyResource(Item: PResourceItem);
begin
  Stream^.Seek(BasePos + Item^.Pos);
  Item^.Pos := AStream^.GetPos - NewBasePos;
  AStream^.CopyFrom(Stream^, Item^.Size);
end;

begin
  SwitchTo := Stream;
  NewBasePos := AStream^.GetPos;
  if Pack then
  begin
    AStream^.Seek(NewBasePos + SizeOf(TStreamHeader));
    Index.ForEach(@DoCopyResource);
    IndexPos := AStream^.GetPos - NewBasePos;
  end else
  begin
    Stream^.Seek(BasePos);
    AStream^.CopyFrom(Stream^, IndexPos);
  end;
  Stream := AStream;
  Modified := True;
  BasePos := NewBasePos;
end;

{ TStringList }

constructor TStringList.Load(var S: TStream);
var
  Size: Word;
begin
  Stream := @S;
  S.Read(Size, SizeOf(Word));
  BasePos := S.GetPos;
  S.Seek(BasePos + Size);
  S.Read(IndexSize, SizeOf(Integer));
  GetMem(Index, IndexSize * SizeOf(TStrIndexRec));
  S.Read(Index^, IndexSize * SizeOf(TStrIndexRec));
end;

destructor TStringList.Done;
begin
  FreeMem(Index, IndexSize * SizeOf(TStrIndexRec));
end;

function TStringList.Get(Key: Word): String; assembler; {$USES ebx,esi,edi} {$FRAME+}
asm
                mov     esi,Self
                mov     edi,@Result
                cld
                mov     ecx,[esi].TStringList.IndexSize
                jecxz   @@2
                mov     ebx,Key
                mov     esi,[esi].TStringList.Index
              @@1:
                mov     edx,ebx
                lodsd
                sub     edx,eax
                lodsd
                cmp     edx,eax
                lodsd
                jb      @@3
                loop    @@1
              @@2:
                xor     al,al                   { Empty string }
                stosb
                jmp     @@4
              @@3:
                push    edi                     { [1]:Pointer = String  }
                push    eax                     { [2]:DWord   = Offset  }
                push    edx                     { [3]:DWord   = Skip    }
                push    Self                    { [4]:Pointer = Self    }
                Call    TStringList.ReadStr
              @@4:
end;

procedure TStringList.ReadStr(var S: String; Offset, Skip: Word);
begin
  Stream^.Seek(BasePos + Offset);
  Inc(Skip);
  repeat
    Stream^.Read(S[0], 1);
    Stream^.Read(S[1], Ord(S[0]));
    Dec(Skip);
  until Skip = 0;
end;

{ TStrListMaker }

constructor TStrListMaker.Init(AStrSize, AIndexSize: Word);
begin
  TObject.Init;
  StrSize := AStrSize;
  IndexSize := AIndexSize;
  GetMem(Strings, AStrSize);
  GetMem(Index, AIndexSize * SizeOf(TStrIndexRec));
end;

destructor TStrListMaker.Done;
begin
  FreeMem(Index, IndexSize * SizeOf(TStrIndexRec));
  FreeMem(Strings, StrSize);
end;

procedure TStrListMaker.CloseCurrent;
begin
  if Cur.Count <> 0 then
  begin
    Index^[IndexPos] := Cur;
    Inc(IndexPos);
    Cur.Count := 0;
  end;
end;

procedure TStrListMaker.Put(Key: Word; S: String);
begin
  if (Cur.Count = 16) or (Key <> Cur.Key + Cur.Count) then CloseCurrent;
  if Cur.Count = 0 then
  begin
    Cur.Key := Key;
    Cur.Offset := StrPos;
  end;
  Inc(Cur.Count);
  Move(S, Strings^[StrPos], Length(S) + 1);
  Inc(StrPos, Length(S) + 1);
end;

procedure TStrListMaker.Store(var S: TStream);
begin
  CloseCurrent;
  S.Write(StrPos, SizeOf(Word));
  S.Write(Strings^, StrPos);
  S.Write(IndexPos, SizeOf(Word));
  S.Write(Index^, IndexPos * SizeOf(TStrIndexRec));
end;

{ TRect }

procedure CheckEmpty; assembler; {$USES None} {$FRAME-}
asm
                mov     eax,[edi].TRect.A.X
                cmp     eax,[edi].TRect.B.X
                jge     @@1
                mov     eax,[edi].TRect.A.Y
                cmp     eax,[edi].TRect.B.Y
                jl      @@2
              @@1:
                cld
                xor     eax,eax
                stosd
                stosd
                stosd
                stosd
              @@2:
end;

procedure TRect.Assign(XA, YA, XB, YB: Integer); assembler; {$USES edi} {$FRAME-}
asm
                mov     edi,Self
                cld
                mov     eax,XA
                stosd
                mov     eax,YA
                stosd
                mov     eax,XB
                stosd
                mov     eax,YB
                stosd
end;

procedure TRect.Copy(R: TRect); assembler; {$USES esi,edi} {$FRAME-}
asm
                mov     esi,R
                mov     edi,Self
                cld
                movsd
                movsd
                movsd
                movsd
end;

procedure TRect.Move(ADX, ADY: Integer); assembler; {$USES None} {$FRAME-}
asm
                mov     ecx,Self
                mov     eax,ADX
                add     [ecx].TRect.A.X,eax
                add     [ecx].TRect.B.X,eax
                mov     eax,ADY
                add     [ecx].TRect.A.Y,eax
                add     [ecx].TRect.B.Y,eax
end;

procedure TRect.Grow(ADX, ADY: Integer); assembler; {$USES edi} {$FRAME-}
asm
                mov     edi,Self
                mov     eax,ADX
                sub     [edi].TRect.A.X,eax
                add     [edi].TRect.B.X,eax
                mov     eax,ADY
                sub     [edi].TRect.A.Y,eax
                add     [edi].TRect.B.Y,eax
                Call    CheckEmpty
end;

procedure TRect.Intersect(R: TRect); assembler; {$USES esi,edi} {$FRAME-}
asm
                mov     esi,R
                mov     edi,Self
                cld
                lodsd
                scasd
                jle     @@1
                sub     edi,4
                stosd
              @@1:
                lodsd
                scasd
                jle     @@2
                sub     edi,4
                stosd
              @@2:
                lodsd
                scasd
                jge     @@3
                sub     edi,4
                stosd
              @@3:
                lodsd
                scasd
                jge     @@4
                sub     edi,4
                stosd
              @@4:
                sub     edi,TYPE TRect
                Call    CheckEmpty
end;

procedure TRect.Union(R: TRect); assembler; {$USES esi,edi} {$FRAME-}
asm
                mov     esi,R
                mov     edi,Self
                cld
                lodsd
                scasd
                jge     @@1
                sub     edi,4
                stosd
              @@1:
                lodsd
                scasd
                jge     @@2
                sub     edi,4
                stosd
              @@2:
                lodsd
                scasd
                jle     @@3
                sub     edi,4
                stosd
              @@3:
                lodsd
                scasd
                jle     @@4
                sub     edi,4
                stosd
              @@4:
end;

function TRect.Contains(P: TPoint): Boolean; assembler; {$USES None} {$FRAME-}
asm
                mov     ecx,Self
                mov     al,0
                mov     edx,P.X
                cmp     edx,[ecx].TRect.A.X
                jl      @@1
                cmp     edx,[ecx].TRect.B.X
                jge     @@1
                mov     edx,P.Y
                cmp     edx,[ecx].TRect.A.Y
                jl      @@1
                cmp     edx,[ecx].TRect.B.Y
                setl    al
              @@1:
end;

function TRect.Equals(R: TRect): Boolean; assembler; {$USES esi,edi} {$FRAME-}
asm
                mov     esi,R
                mov     edi,Self
                mov     ecx,4
                cld
                repe    cmpsd
                sete    al
end;

function TRect.Empty: Boolean; assembler;
asm
                mov     ecx,Self
                mov     al,1
                mov     edx,[ecx].TRect.A.X
                cmp     edx,[ecx].TRect.B.X
                jge     @@1
                mov     edx,[ecx].TRect.A.Y
                cmp     edx,[ecx].TRect.B.Y
                setge   al
              @@1:
end;

{$ENDIF}

{ Dynamic string handling routines }

{ from rtl_changes.txt }
function NewStr(const S: String): PString;
var
  P: PString;
begin
  GetMem(P, Length(S) + 1);
  P^ := S;
  NewStr := P;
end;

procedure DisposeStr(P: PString);
begin
  if P <> nil then FreeMem(P, Length(P^) + 1);
end;

{ Objects registration procedure }

procedure RegisterObjects;
begin
  RegisterType(RCollection);
  RegisterType(RStringCollection);
  RegisterType(RStrCollection);
end;

{ Peforms services analogous to DOS INT 21h Fns: 3Ch,3Dh,3Eh,3Fh,40h,42h }

procedure DosFn; {&USES ecx,edx} {&FRAME-}
asm
                cmp     ah,42h
                je      @@Seek
                cmp     ah,3Fh
                je      @@Read
                cmp     ah,40h
                je      @@Write
                cmp     ah,3Eh
                je      @@Close         { 3Ch, 3Dh                      }
                cmp     ah,3Ch
                je      @@Create
                cmp     ah,3Dh
                je      @@Open

                int     3

              @@Open:
                movzx   eax,al          // OPEN
                push    eax             // Handle
                push    edx             // [1]:Pointer = @File name
                push    eax             // [2]:DWord   = Mode
                lea     eax,[esp+4*2]   // [3]:Dword   = Handle
                push    eax
                Call    SysFileOpen
                jmp     @@SetResult

              @@Create:                 // CREATE
                push    eax             // Handle
                push    edx             // [1]:Pointer = @File name
              //push    $42             // [2]:DWord   = Mode (R/W deny none)
                push    stCreate_FileMode
                push    ecx             // [3]:DWord   = Attribute
                lea     eax,[esp+4*3]   // [4]:Dword   = Handle
                push    eax
                Call    SysFileCreate
                jmp     @@SetResult

              @@Seek:                   // SEEK
                movzx   eax,al
                push    eax             // New Position
                push    ebx             // [1]:DWord   = File Handle
                push    ecx             // [2]:DWord   = Distance
                push    eax             // [3]:DWord   = Method
                lea     eax,[esp+4*3]   // [4]:Pointer = @NewPtr
                push    eax
                Call    SysFileSeek
                jmp     @@SetResult

              @@Read:                   // READ
                push    eax             // Bytes read
                push    ebx             // [1]:DWord   = File Handle
                push    edx             // [2]:Pointer = @Buffer
                push    ecx             // [3]:DWord   = ReadCount
                lea     eax,[esp+4*3]   // [4]:Pointer = @BytesRead
                push    eax
                Call    SysFileRead
                jmp     @@SetResult

              @@Write:                  // WRITE
                jecxz   @@Truncate
                push    eax             // Bytes write
                push    ebx             // [1]:DWord   = File Handle
                push    edx             // [2]:Pointer = @Buffer
                push    ecx             // [3]:DWord   = WriteCount
                lea     eax,[esp+4*3]   // [4]:Pointer = @BytesWritten
                push    eax
                Call    SysFileWrite

              @@SetResult:
                pop     edx             // Result
                test    eax,eax
                stc
                jnz     @@RET
                mov     eax,edx
                jmp     @@OK

              @@Truncate:               // TRUNCATE
                mov     ax,4201h        // ebx = Handle, 1=CurPtr
                Call    DosFn           // ecx = 0 = Distance
                jc      @@RET           // eax = Current File Pointer
                push    ebx             // [1]:Longint = File Handle
                push    eax             // [2]:Longint = New File Size
                Call    SysFileSetSize
                test    eax,eax
                stc
                jnz     @@RET
                jmp     @@OK

              @@Close:                  // CLOSE
                push    ebx             // [1]:DWord = File Handle
                Call    SysFileClose
                test    eax,eax
                stc
                jnz     @@RET

              @@OK:
                clc
              @@RET:
end;

end.
