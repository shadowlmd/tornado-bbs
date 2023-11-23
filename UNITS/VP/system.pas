// |---------------------------------------------------------|
// |                                                         |
// |     Virtual Pascal Runtime Library.  Version 2.1.       |
// |     System unit                                         |
// |      ---------------------------------------------------|
// |      Copyright (C) 1995-2003 vpascal.com                |
// |                                                         |
// |---------------------------------------------------------|

{$S-,D-,M+,H+,X+,Speed+,Delphi+,Cdecl-,OrgName-,AlignRec-,SmartLink+,Optimise+,W-}
{.$D+,L+}

// common Linux and DPMI32 code: RaiseException, I/O port access
{$IfDef Linux}  {$Define LNX_DPMI} {$EndIf}
{$IfDef DPMI32} {$Define LNX_DPMI} {$EndIf}
{$IfDef LINUX}  {$Define LNX_DPMI} {$EndIf}

// common OS/2, Linux and DPMI32 code: DLL initialisation
{$IfDef OS2}    {$Define OS2_LNX_DPMI} {$EndIf}
{$IfDef DPMI32} {$Define OS2_LNX_DPMI} {$EndIf}
{$IfDef LINUX}  {$Define OS2_LNX_DPMI} {$EndIf}


unit System;

interface

type
  Integer = SmallInt;
  Word = SmallWord;
  TDateTime = Double;
  TProcedure = procedure;
  PExtended = ^Extended;
  PCurrency = ^Currency;
  PShortString = ^ShortString;
  PAnsiString = ^AnsiString;
  PString = PAnsiString;

procedure AddExitProc(Proc: TProcedure);                        // Unit finalisation support
procedure UniqueString(var LStr: String);
procedure _Abstract;                                            // Entry point for any abstract virtual method
procedure _Atan;                                                // 'ArcTan' standard function
procedure _BlockRead (FileVar,Buffer:Pointer;Count:Longint;Result:Pointer); // 'BlockRead' standard procedure
procedure _BlockWrite(FileVar,Buffer:Pointer;Count:Longint;Result:Pointer); // 'BlockWrite' standard procedure
procedure _ClsAs(AClass,VMT: Pointer);                          // 'AS' class operator
procedure _ClsCallDynCls(Self,Index: Longint);                  // Calls a dynamic class method using class reference
procedure _ClsCallDynInst(Self,Index: Longint);                 // Calls a dynamic method using class instance
procedure _ClsCtr;                                              // Class constructor support
procedure _ClsDtr;                                              // Class destructor support
procedure _ClsFindDynCls(Self,Index: Longint);                  // Finds a dynamic class method using class reference
procedure _ClsFindDynInst(Self,Index: Longint);                 // Finds a dynamic method using class instance
procedure _ClsIs(AClass,VMT: Pointer);                          // 'IS' class operator
procedure _CopyOpArr   (ElementSize,Src: Longint);              // Open array copying support
procedure _CopyOpArrChk(ElementSize,Src: Longint);              // The same, but with stack checking
procedure _CopyParms(Data: Pointer);                            // Structured parameter copying support
procedure _Cos;                                                 // 'Cos' standard function
procedure _DirCh;                                               // 'ChDir' standard procedure (ShortString)
procedure _DirChPCh;                                            // 'ChDir' standard procedure (PChar/AnsiString)
procedure _DirGet(Drive: Byte; S: Pointer; SLen: Longint);      // 'GetDir' standard procedure (ShortString)
procedure _DirGetL(Drive: Byte; var LStr: Pointer);             // 'GetDir' standard procedure (AnsiString)
procedure _DirMk;                                               // 'MkDir' standard procedure (ShortString)
procedure _DirMkPCh;                                            // 'MkDir' standard procedure (PChar/AnsiString)
procedure _DirRm;                                               // 'RmDir' standard procedure (ShortString)
procedure _DirRmPCh;                                            // 'RmDir' standard procedure (PChar/AnsiString)
procedure _DmtCall(DynIndex: Longint);                          // Dynamic method call routine (objects)
procedure _Eof(FileVar: Pointer);                               // 'Eof' standard function
procedure _Erase(FileVar: Pointer);                             // 'Erase' standard procedure
procedure _ErrOverflow;                                         // Arithmetic overflow error
procedure _ErrRange;                                            // Range check error
procedure _Exp;                                                 // 'Exp' standard function
procedure _Ext2Real48(Dest: Pointer);                           // Converts Extended to Real48
procedure _Far16Pas;                                            // Thunk support for calling Far16 routines
procedure _FileAssign(FileVar,S: Pointer);                      // 'Assign' standard procedure (String)
procedure _FileAssignPCh(FileVar,S: Pointer);                   // 'Assign' standard procedure (PChar)
procedure _FileClose(FileVar: Pointer);                         // 'Close' standard procedure
{$IfDef LargeFileSupport}
// renamed, to have something for the compiler magic (system.vps)
// 64 bit functions are declared later down
procedure _FilePos_dummy(FileVar: Pointer);                     // 'FilePos' standard function
{$Else LargeFileSupport}
procedure _FilePos (FileVar: Pointer);                          // 'FilePos' standard function
{$EndIf LargeFileSupport}
procedure _FileRead(FileVar,Buffer: Pointer);                   // 'Read' standard procedure
procedure _FileReset(FileVar: Pointer; RecSize: Longint);       // 'Reset' standard procedure
procedure _FileRewrite(FileVar: Pointer; RecSize: Longint);     // 'Rewrite' standard procedure
{$IfDef LargeFileSupport}
// renamed, to have something for the compiler magic (system.vps)
// 64 bit functions are declared later down
procedure _FileSeek_dummy(FileVar: Pointer; FilePos: Longint);  // 'Seek' standard procedure
procedure _FileSize_dummy(FileVar: Pointer);                    // 'FileSize' standard function
{$Else LargeFileSupport}
procedure _FileSeek(FileVar: Pointer; FilePos: Longint);        // 'Seek' standard procedure
procedure _FileSize(FileVar: Pointer);                          // 'FileSize' standard function
{$EndIf LargeFileSupport}
procedure _FileTrunc(FileVar: Pointer);                         // 'Truncate' standard procedure
procedure _FileWrite(FileVar,Buffer: Pointer);                  // 'Write' standard procedure
procedure _Frac;                                                // 'Frac' standard function
procedure _GetIORes;                                            // 'IOResult' standard function
function  _GetTlsVar(var TlsVar): Pointer;                      // Returns an address of the THREADVAR variable
procedure _Halt(ExitCode: Longint);                             // 'Halt' standard procedure
procedure _IOChk;                                               // I/O result check
procedure _In16(PortNo: Longint);                               // Inputs Word from I/O Port
procedure _In32(PortNo: Longint);                               // Inputs DWord from I/O Port
procedure _In8(PortNo: Longint);                                // Inputs Byte from I/O Port
procedure _InitDll;                                             // DLL initialisation/termination start
procedure _InitDllEnd(ExitCode: Longint);                       // DLL initialisation/termination end
procedure _InitExe(Params,EnvPtr: Pointer; Reserved,ModHandle,RetAddr:Longint); // Program initialisation
procedure _Int;                                                 // 'Int' standard function
procedure _Ln;                                                  // 'Ln' standard function
procedure _LStr2Str(SStr,LStr: Pointer; MaxLen: Longint);
procedure _LStrAddRef(LStr: Pointer);
procedure _LStrArray(Dest,Src: Pointer; Size: Longint);
procedure _LStrAsn(var Dest: Pointer; Src: Pointer);
procedure _LStrChar(LStr: Pointer; C: Char);
procedure _LStrClr(LStr: Pointer);
procedure _LStrCmp(LStr1,LStr2: Pointer);
procedure _LStrConcat(var Dest: Pointer; Src: Pointer);
procedure _LStrCopy(var Dest: Pointer; Src: Pointer; Index,Count: Longint);
procedure _LStrDel(LStr: Pointer; Index,Count: Longint);
procedure _LStrIns(Src: Pointer; var Dest: Pointer; Index: Longint);
procedure _LStrLoad(var Dest: Pointer; Src: Pointer);
procedure _LStrNew(Len: Longint);
procedure _LStrOfChar(LStr: Pointer; C: Char; Count: Longint);
procedure _LStrPChar(LStr: Pointer; Str: PChar);
procedure _LStrPacked(Dest,Src: Pointer; Len: Longint);
procedure _LStrPos(SubStr,LStr: Pointer);
procedure _LStrSetLen(var LStr: Pointer; Len: Longint);
procedure _LStrStr(var LStr: Pointer; SStr: Pointer);
procedure _LStrToPChar(LStr: Pointer);
procedure _MemAddRef(P,TypeInfo: Pointer);                      // Adds reference to all long string fields
procedure _MemFill(Dest: Pointer; Count: Longint; Value: Byte); // 'FillChar' standard procedure
procedure _MemFin(P,TypeInfo: Pointer);                         // 'Finalize' standard procedure
procedure _MemFinCnt(P,TypeInfo: Pointer; Count: Longint);      // 'Finalize' standard procedure with Count optional parameter
procedure _MemFree(P: Pointer);                                 // 'Dispose','FreeMem' standard procedures
procedure _MemFreeFin(P,TypeInfo: Pointer);                     // 'Dispose' standard procedure (finalization is needed)
procedure _MemInit(P,TypeInfo: Pointer);                        // 'Initialize' standard procedure
procedure _MemInitCnt(P,TypeInfo: Pointer; Count: Longint);     // 'Initialize' standard procedure with Count optional parameter
procedure _MemLocFin(Data: Pointer);                            // Finalization of the local memory
procedure _MemLocInit(Data,Handler: Pointer);                   // Initialization of the local memory
procedure _MemMove(Src,Dest: Pointer; Count: Longint);          // 'Move' standard procedure
procedure _MemNew(Size: Longint);                               // 'New','GetMem' standard procedures
procedure _MemNewInit(Size: Longint; TypeInfo: Pointer);        // 'New' standard procedure(initialization is needed)
procedure _MemRealloc(var P: Pointer; Size: Longint);           // 'ReallocMem' standard procedure
procedure _ObjChk(VmtPtr: Longint);                             // Object initialisation check
procedure _ObjCopy(Src,Dest: Pointer; VmtPtr: Longint);         // Object assignment support routine
procedure _ObjCopyInit(Src,Dest: Pointer; VmtPtr: Longint; RTTI: Pointer); // Object assignment support routine for types that need initialization
procedure _ObjCtr(VmtPtr: Longint);                             // Constructor support routine
procedure _ObjDtr;                                              // Destructor support routine
procedure _Out16(Value,PortNo: Longint);                        // Outputs Word to I/O Port
procedure _Out32(Value,PortNo: Longint);                        // Outputs DWord to I/O Port
procedure _Out8(Value,PortNo: Longint);                         // Outputs Byte to I/O Port
procedure _RandFlt;                                             // 'Random' standard function (Float)
procedure _RandInt(Range: Longint);                             // 'Random' standard function (Integer)
procedure _Real482Ext(Src: Pointer);                            // Converts Real48 to Extended
procedure _Rename(FileVar,NewName: Pointer);                    // 'Rename' standard procedure (String)
procedure _RenamePCh(FileVar,NewName: Pointer);                 // 'Rename' standard procedure (PChar)
procedure _Round;                                               // 'Round' standard function
procedure _RunError(ErrorCode: Longint);                        // 'RunError' standard procedure
procedure _SetAddRange(Dest: Pointer; Lower,Upper: Byte);       // Loads dword sized set
procedure _SetDWordLoad(Dest: Pointer; Value: Longint);         // Loads dword sized set
procedure _SetDif(Dest,Src : Pointer);                          // '-' operator for unpacked sets
procedure _SetEqual(Dest,Src : Pointer);                        // '=','<>' operators for unpacked sets
procedure _SetInter(Dest,Src : Pointer);                        // '*' operator for unpacked sets
procedure _SetLoad(Dest,Src: Pointer; SetData: Longint);        // Loads packed set
procedure _SetRel(Dest,Src : Pointer);                          // '<','>' operators for unpacked sets
procedure _SetStore(Src,Dest: Pointer; SetData: Longint);       // Stores unpacked set
procedure _SetUnion(Dest,Src : Pointer);                        // '+' operator for unpacked sets
procedure _Sin;                                                 // 'Sin' standard function
procedure _Sqrt;                                                // 'Sqrt' standard function
procedure _StkChk(LocalSize: Longint);                          // Stack check routine
procedure _StkPrb(LocalSize: Longint);                          // Stack probing for routines with more than 4K local variables
procedure _StrChar(Dest: Pointer; Char: Byte);                  // Converts char to string
procedure _StrCmp(S1,S2: Pointer);                              // String relation operators
procedure _StrConcat(Dest,Src: Pointer);                        // 'Concat' standard function
procedure _StrCopy(Dest,Src: Pointer; Index,Count: Longint);    // Copy standard function
procedure _StrDel(S: Pointer; Index,Count: Longint);            // 'Delete' standard procedure
procedure _StrFlt(Width,Dec:Longint; S: Pointer; SLen: Longint);// 'Str' standard procedure (Float,ShortString)
procedure _StrFltLStr(Width,Dec: Longint; var S: Pointer);      // 'Str' standard procedure (Float,AnsiString)
procedure _StrFltPCh(Width,Dec:Longint;S:Pointer;SLen: Longint);// 'Str' standard procedure (Float, PChar)
procedure _StrIns(Src,Dest: Pointer; DestLen,Index: Longint);   // 'Insert' standard procedure
procedure _StrInt(Value,Width:Longint; S:Pointer; SLen:Longint);// 'Str' standard procedure (Integer,ShortString)
procedure _StrIntLStr(Value,Width: Longint; var S: Pointer);    // 'Str' standard procedure (Integer,AnsiString)
procedure _StrIntPCh(Value,Width:Longint;S:Pointer;SLen:Longint);//'Str' standard procedure (Integer,PChar)
procedure _StrLoad(Dest,Src: Pointer);                          // Loads string
procedure _StrPacked(Dest,Src: Pointer; Len: Longint);          // Converts packed string to string
procedure _StrPos(SubStr,S: Pointer);                           // 'Pos' standard function
procedure _StrSet(S: Pointer; Buffer: PChar; Len: Longint);     // 'SetString' standard procedure (ShortString)
procedure _StrStore(Src,Dest: Pointer; MaxLen: Longint);        // Stores string
procedure _Terminate;                                           // Terminates program with exit code = 0
procedure _Trunc;                                               // 'Trunc' standard function
procedure _TxtAppend(FileVar: Pointer);                         // 'Append' standard procedure
procedure _TxtAssign(FileVar,S: Pointer);                       // 'Assign' standard procedure (String)
procedure _TxtAssignPCh(FileVar,S: Pointer);                    // 'Assign' standard procedure (PChar)
procedure _TxtClose(FileVar: Pointer);                          // 'Close' standard procedure
procedure _TxtEof(FileVar: Pointer);                            // 'Eof' standard function
procedure _TxtEoln(FileVar: Pointer);                           // 'Eoln' standard function
procedure _TxtFlush(FileVar: Pointer);                          // 'Flush' standard procedure
procedure _TxtRChar(FileVar: Pointer);                          // 'Read' standard procedure (Char)
procedure _TxtREnd(FileVar: Pointer);                           // End of read
procedure _TxtRFlt(FileVar: Pointer);                           // 'Read' standard procedure (Float)
procedure _TxtRInt(FileVar: Pointer);                           // 'Read' standard procedure (Integer)
procedure _TxtRLn(FileVar: Pointer);                            // 'ReadLn' standard procedure
procedure _TxtRLStr(FileVar,LStr: Pointer);                     // 'Read' standard procedure for long string type
procedure _TxtRPChar(FileVar,S: Pointer; SLen: Longint);        // 'Read' standard procedure (PChar)
procedure _TxtRStr  (FileVar,S: Pointer; SLen: Longint);        // 'Read' standard procedure (String)
procedure _TxtReset(FileVar: Pointer);                          // 'Reset' standard procedure
procedure _TxtRewrite(FileVar: Pointer);                        // 'Rewrite' standard procedure
procedure _TxtSEof(FileVar: Pointer);                           // 'SeekEof' standard function
procedure _TxtSEoln(FileVar: Pointer);                          // 'SeekEoln' standard function
procedure _TxtSetBuf(FileVar,Buffer: Pointer; BufSize: Longint);// 'SetTextBuf' standard procedure
procedure _TxtWBool(FileVar:Pointer; Value:Byte; Width:Longint);// 'Write' standard procedure (Boolean)
procedure _TxtWChar(FileVar:Pointer; Value:Byte; Width:Longint);// 'Write' standard procedure (Char)
procedure _TxtWEnd(FileVar: Pointer);                           // End of write
procedure _TxtWFlt(FileVar: Pointer; Width,Dec: Longint);       // 'Write' standard procedure (Float)
procedure _TxtWInt(FileVar: Pointer; Value,Width: Longint);     // 'Write' standard procedure (Integer)
procedure _TxtWLn(FileVar: Pointer);                            // 'WriteLn' standard procedure
procedure _TxtWPChar(FileVar,S: Pointer; Width: Longint);       // 'Write' standard procedure (PChar)
procedure _TxtWStr  (FileVar,S: Pointer; Width: Longint);       // 'Write' standard procedure (String)
procedure _TxtWLStr (FileVar,S: Pointer; Width: Longint);       // 'Write' standard procedure (Long String)
procedure _UpCase(Char: Byte);                                  // 'UpCase' standard function
procedure _ValFlt(S,Code: Pointer);                             // 'Val' standard procedure (Float)
procedure _ValFltPCh(S,Code: Pointer);                          // 'Val' standard procedure (Float,PChar)
procedure _ValInt(S,Code: Pointer);                             // 'Val' standard procedure (Integer)
procedure _ValIntPCh(S,Code: Pointer);                          // 'Val' standard procedure (Integer,PChar)
procedure _VarMove(Src,Dest: Pointer; Count: Longint);          // Variable assignment support routine
procedure _VarMoveInit(Src,Dest: Pointer; Count: Longint; RTTI: Pointer); // Variable assignment support routine for types that need initialization
{&Cdecl+}
procedure _XcptAny    (Report,Registration,Context,Void: Pointer); // Handler for any exception
procedure _XcptDone   (Report,Registration,Context,Void: Pointer); // Exit exception block routine
procedure _XcptFinally(Report,Registration,Context,Void: Pointer); // Finally block handler
procedure _XcptOn     (Report,Registration,Context,Void: Pointer); // Handler for ON exception handlers
procedure _XcptRaise  (Exception: Pointer); pascal;                // 'raise' statement support routine
procedure _XcptRaiseAg(Report,Registration,Context,Void: Pointer); // Re-raise form of the 'raise' statement
procedure _XcptTryExit; pascal;                                    // Exception block exit support routine
{&Cdecl-}

{ TVarRec.VType values }

const
  vtInteger    = 0;
  vtBoolean    = 1;
  vtChar       = 2;
  vtExtended   = 3;
  vtString     = 4;
  vtPointer    = 5;
  vtPChar      = 6;
  vtObject     = 7;
  vtClass      = 8;
  vtCurrency   = 9;
  vtAnsiString = 10;

{ The ultimate ancestor for all class types }

type
  TObject = class;
  TClass = class of TObject;
  TObject = class
  public
    constructor Create;
    class function ClassInfo: Pointer;
    class function ClassName: ShortString;
    class function ClassNameIs(const Name: String): Boolean;
    class function ClassParent: TClass;
    function ClassType: TClass;
    procedure CleanupInstance;
    procedure Dispatch(var Message);
    function FieldAddress(const Name: ShortString): Pointer;
    procedure Free;
    class function InheritsFrom(AClass: TClass): Boolean;
    class function InitInstance(Instance: Pointer): TObject;
    class function InstanceSize: Longint;
    class function MethodAddress(const Name: ShortString): Pointer;
    class function MethodName(Address: Pointer): ShortString;
    { virtual methods: the order is significant }
    procedure DefaultHandler(var Message); virtual;
    class function NewInstance: TObject; virtual;
    procedure FreeInstance; virtual;
    destructor Destroy; virtual;
  end;

{ The record used for passing type variant open array parameters }

  PVarRec = ^TVarRec;
  TVarRec = record
    case Byte of
      vtInteger:   (VInteger: Longint; VType: Byte; VFiller: array[0..2] of Byte);
      vtBoolean:   (VBoolean: Boolean);
      vtChar:      (VChar: Char);
      vtExtended:  (VExtended: PExtended);
      vtString:    (VString: PShortString);
      vtPointer:   (VPointer: Pointer);
      vtPChar:     (VPChar: PChar);
      vtObject:    (VObject: TObject);
      vtClass:     (VClass: TClass);
      vtCurrency:  (VCurrency: PCurrency);
      vtAnsiString:(VAnsiString: Pointer);
  end;

{ Thread local storage variables }

threadvar
  InOutRes: Longint;            // Result of the last I/O operation
  RaiseList: Pointer;           // Head of the list of current exception class instances
  FileMode: Longint;            // Mode for Reset: Default=Read/Write, Deny None
  FileModeReadWrite: Longint;   // File mode for Rewrite (typed/untyped files), Default: Read/Write, Deny None
  TextModeRead: Longint;        // File mode for Reset on text files: Default=Read, Deny None
  TextModeReadWrite: Longint;   // File mode for Rewrite/Append on text files: Default=Read/Write, Deny None

var
  Input:            Text;               // Standard input file
  Output:           Text;               // Standard output file

const
  CurrScale1:       Single   = 1.0e+4;  // Scale factors for Currency values
  CurrScale2:       Extended = 1.0e-4;

const
  ExitCode:         Longint = 0;        // Exit/Error code
  ErrorAddr:        Pointer = nil;      // Flat address of a Runtime error
  ExceptionNo:      Longint = 0;        // OS/2 exception number
  TlsSharedMem:     Pointer = nil;      //  / Used internally
  TlsSharedMemSize: Longint = 0;        // <  by
  DebugHook:        Boolean = False;    //  \ the debugger
  IsConsole:        Boolean = False;    // True if NOVIO/VIO, False if PM application
  IsMultiThread:    LongBool= False;    // True if more than one thread exists
  ExitProc:         Pointer = nil;      // Exit procedure
  XcptProc:         Pointer = nil;      // Exception Handler for BP7 compatible programs
  ExceptProc:       Pointer = nil;      // Handler for unhandled exceptions
  ErrorProc:        Pointer = nil;      // Handler for RTL errors
//HeapList:         Pointer = nil;      // Head of the list of memory blocks
  SmHeapList:       Pointer = nil;      // Head of list of small memory blocks
  LgHeapList:       Pointer = nil;      // Head of list of large blocks
  HeapError:        Pointer = nil;      // Handler for heap errors
  Environment:      Pointer = nil;      // Pointer to the environment
  ExceptClsProc:    Pointer = nil;      // Map an OS/2 Exception to a language class reference
  ExceptObjProc:    Pointer = nil;      // Map an OS/2 Exception to a language class instance
  ExceptionClass:   TClass  = nil;      // Exception base class (must be SysUtils.Exception)
  CmdLine:          PChar   = nil;      // Points to the command line
  ModuleHandle:     Longint = 0;        // Module Handle
  RandSeed:         Longint = 0;        // Random Number Generator Seed
  AllocMemCount:    Longint = 0;        // Number of allocated memory blocks
  AllocMemSize:     Longint = 0;        // Total size of allocated memory blocks
//HeapBlock:        Longint = 8*1024;   // 8K
  {$IfDef OS2}                          // OS/2 allocates at least 64K addresses for every allocation
  SmHeapBlock:      Longint =  64*1024; // Size of block to allocate from OS
  LgHeapBlock:      Longint = 128*1024; // Size of block to allocate from OS
  HeapLimit:        Longint =  32*1024; // Blocks up to this size are "small"
  {$Else ~OS2}
  SmHeapBlock:      Longint =  16*1024; // Size of block to allocate from OS
  LgHeapBlock:      Longint =  64*1024; // Size of block to allocate from OS
  HeapLimit:        Longint =   8*1024; // Blocks up to this size are "small"
  {$EndIf OS2}
  HeapAllocFlags:   Longint = {$IfDef OS2}
                              $0053; // obj_Tile+pag_Commit+pag_Read+pag_Write / mem_Commit
                              {$EndIf OS2}
                              {$IfDef Linux}
                              $0003; // $0006; // PROT_READ or PROT_WRITE
                              {$EndIf Linux}
                              {$IfDef DPMI32}
                              $0000; // ignored
                              {$EndIf DPMI32}
                              {$IfDef Win32}
                              $1000;
                              {$EndIf Win32}
  HeapControl:      Pointer = nil;      // PHeapControl used to control heap access
  Test8086:         Byte    = 2;        // 386 or better
  Test8087:         Byte    = 3;        // 387 or better
  IsDll:            Boolean = False;    // True if this System unit is part of a DLL

{-------[ Standard routines that do not need compiler magic ]-----------}

type
  {$IfDef LargeFileSupport}             // 63+1 bit integer
  TFileSize                     = Comp;
  TFileSizeRec                  =
    record
      lo32,hi32                 : Longint;
    end;
  {$Else}                               // 31+1 bit integer
  TFileSize                     = LongInt;
  {$EndIf}

{$IfDef LargeFileSupport}
// reimplementation for large filesizes to bypass compiler magic
function  FilePos(Var F):TFileSize;
procedure Seek(Var F; FilePos: TFileSize);
function  FileSize(Var F):TFileSize;
{$EndIf LargeFileSupport}


procedure FlatToSel(var P: Pointer);    // Converts FLAT (0:32) pointer to Selector:Offset (16:16) form
procedure SelToFlat(var P: Pointer);    // Converts Selector:Offset (16:16) pointer to FLAT (0:32) form
procedure FPower10;                     // Used internally by System & SysUtils units

{ Thread support }

type
  TThreadFunc = function(Parameter: Pointer): Longint;

function BeginThread(SecurityAttributes: Pointer; StackSize: Longint; ThreadFunc: TThreadFunc;
  Parameter: Pointer; CreationFlags: Longint; var ThreadId: Longint): Longint;
procedure EndThread(ExitCode: Longint);
function KillThread(Handle: Longint): Longint;
function SuspendThread(Handle: Longint): Longint;
function ResumeThread(Handle: Longint): Longint;
function GetThreadId: Longint;
function GetLocationInfo(Addr: Pointer; var AFileName: ShortString; var ALineNo: Longint): Pointer;
procedure _RunErrorStr(var ErrStr: ShortString);

{ Memory manager }

type
  PMemoryManager = ^TMemoryManager;
  TMemoryManager = record
    GetMem: function(Size: Longint): Pointer;
    FreeMem: function(P: Pointer): Longint;
    ReallocMem: function(P: Pointer; Size: Longint): Pointer;
  end;
  THeapStatus = record
    TotalAddrSpace: Cardinal;
    TotalUncommitted: Cardinal;
    TotalCommitted: Cardinal;
    TotalAllocated: Cardinal;
    TotalFree: Cardinal;
    FreeSmall: Cardinal;
    FreeBig: Cardinal;
    Unused: Cardinal;
    Overhead: Cardinal;
    HeapErrorCode: Cardinal;
  end;

function SysGetMem(Size: Longint): Pointer;
function SysFreeMem(P: Pointer): Longint;
function SysReallocMem(P: Pointer; Size: Longint): Pointer;

procedure GetMemoryManager(var MemMgr: TMemoryManager);
function GetPMemoryManager: PMemoryManager;
procedure SetMemoryManager(const MemMgr: TMemoryManager);
function GetHeapStatus: THeapStatus;

function MaxAvail: Longint;
function MemAvail: Longint;
procedure Randomize;
function ParamCount: Longint;
function ParamStr(Index: Longint): ShortString;

{ Operating System interface }

{&OrgName+}
function SysFileStdIn: Longint;
function SysFileStdOut: Longint;
function SysFileStdErr: Longint;
function SysFileOpen(FileName: PChar; Mode: Longint; var Handle: Longint): Longint;
function SysFileCreate(FileName: PChar; Mode,Attr: Longint; var Handle: Longint): Longint;
function SysFileSeek(Handle: Longint;Distance: TFileSize;Method: Longint; var Actual: TFileSize): Longint;
function SysFileRead(Handle: Longint; var Buffer; Count: Longint; var Actual: Longint): Longint;
function SysFileWrite(Handle: Longint; const Buffer; Count: Longint; var Actual: Longint): Longint;
function SysFileSetSize(Handle: Longint; NewSize: TFileSize): Longint;
function SysFileClose(Handle: Longint): Longint;
function SysFileDelete(FileName: PChar): Longint;
function SysFileMove(OldName,NewName: PChar): Longint;
function SysFileIsDevice(Handle: Longint): Longint;
function SysDirGetCurrent(Drive: Longint; Path: PChar): Longint;
function SysDirSetCurrent(Path: PChar): Longint;
function SysDirCreate(Path: PChar): Longint;
function SysDirDelete(Path: PChar): Longint;
function SysMemAvail: Longint;
function SysMemAlloc(Size,Flags: Longint; var MemPtr: Pointer): Longint;
function SysMemFree(MemPtr: Pointer): Longint;
procedure SysCtrlSleep(Delay: Integer);
function SysGetThreadId: Longint;
function SysSysMsCount: Longint;
procedure SysSysWaitSem(var Sem: Longint);
procedure SysSysSelToFlat(var P: Pointer);
procedure SysSysFlatToSel(var P: Pointer);
function SysCtrlSelfAppType: Longint;
function SysCtrlCreateThread(Attrs: Pointer; StackSize: Longint; Func,Param: Pointer; Flags: Longint; var Tid: Longint): Longint;
function SysCtrlKillThread(Handle: Longint): Longint;
function SysCtrlSuspendThread(Handle: Longint): Longint;
function SysCtrlResumeThread(Handle: Longint): Longint;
procedure SysCtrlExitThread(ExitCode: Longint);
procedure SysCtrlExitProcess(ExitCode: Longint);
function SysCtrlGetModuleName(Handle: Longint; Buffer: PChar): Longint;
function SysCtrlGetTlsMapMem: Pointer;
function SysCmdln: PChar;
function SysCmdlnCount: Longint;
procedure SysCmdlnParam(Index: Longint; var Param: ShortString);
function SysGetProcessId: Longint;
function SemCreateMutex(_Name: PChar; _Shared, _State: Boolean): Longint;
procedure SemCloseMutex(_Handle: Longint);
function SemReleaseMutex(_Handle: Longint): Boolean;
function SemRequestMutex(_Handle: Longint; _TimeOut: Longint): Boolean;
function SemAccessMutex(_Name: PChar): Longint;
procedure SysLowInitPreTLS;
procedure SysLowInitPostTLS;
{$IfDef LNX_DPMI}
procedure RaiseNotification(ArgCount,Arg1,Arg2,Code: Longint);
procedure AllocateDLLInitTib(const Termination: Pointer);
procedure ReleaseDLLInitTib;
{$EndIf LNX_DPMI}
{$IfDef Linux}
procedure Process_ArgC_ArgV_Env(const _ArgC: Longint; _ArgV: Pointer);
{$EndIf Linux}


{&OrgName-}

const
{$IFDEF OS2}
  xcpt_Access_Violation         = $C0000005;
  xcpt_Guard_Page_Violation     = $80000001;
  xcpt_In_Page_Error            = $C0000006;
  xcpt_Array_Bounds_Exceeded    = $C0000093;
  xcpt_Float_Denormal_Operand   = $C0000094;
  xcpt_Float_Divide_By_Zero     = $C0000095;
  xcpt_Float_Inexact_Result     = $C0000096;
  xcpt_Float_Invalid_Operation  = $C0000097;
  xcpt_Float_Overflow           = $C0000098;
  xcpt_Float_Stack_Check        = $C0000099;
  xcpt_Float_Underflow          = $C000009A;
  xcpt_Integer_Divide_By_Zero   = $C000009B;
  xcpt_Integer_Overflow         = $C000009C;
  xcpt_Privileged_Instruction   = $C000009D;
  xcpt_Unable_To_Grow_Stack     = $80010001;
  xcpt_Illegal_Instruction      = $C000001C;
  xcpt_DataType_Misalignment    = $C000009E;
  xcpt_NonContinuable_Exception = $C0000024;
  xcpt_Invalid_Disposition      = $C0000025;
  // OS/2 specific
  xcpt_Process_Terminate        = $C0010001;
  xcpt_Async_Process_Terminate  = $C0010002;
  xcpt_Invalid_Lock_Sequence    = $C000001D;
  xcpt_B1npx_Errata_02          = $C0010004;
  xcpt_Bad_Stack                = $C0000027;
  xcpt_Invalid_Unwind_Target    = $C0000028;
  xcpt_Unwind                   = $C0000026;
  xcpt_Signal                   = $C0010003;
{$ENDIF}
{$IFDEF WIN32}
  xcpt_Access_Violation         = $C0000005;
  xcpt_Guard_Page_Violation     = $80000001;
  xcpt_In_Page_Error            = $C0000006;
  xcpt_Array_Bounds_Exceeded    = $C000008C;
  xcpt_Float_Denormal_Operand   = $C000008D;
  xcpt_Float_Divide_By_Zero     = $C000008E;
  xcpt_Float_Inexact_Result     = $C000008F;
  xcpt_Float_Invalid_Operation  = $C0000090;
  xcpt_Float_Overflow           = $C0000091;
  xcpt_Float_Stack_Check        = $C0000092;
  xcpt_Float_Underflow          = $C0000093;
  xcpt_Integer_Divide_By_Zero   = $C0000094;
  xcpt_Integer_Overflow         = $C0000095;
  xcpt_Privileged_Instruction   = $C0000096;
  xcpt_Unable_To_Grow_Stack     = $C00000FD; // STATUS_STACK_OVERFLOW
  xcpt_Illegal_Instruction      = $C000001D;
  xcpt_DataType_Misalignment    = $80000002;
  xcpt_NonContinuable_Exception = $C0000025;
  xcpt_Invalid_Disposition      = $C0000026;
  // Win32 specific
  xcpt_Control_C_Exit           = $C000013A;
{$ENDIF}
{$IFDEF DPMI32}
  xcpt_Integer_Divide_By_Zero   = $c0000000;
  xcpt_Integer_Overflow         = $c0000004;
  xcpt_Array_Bounds_Exceeded    = $c0000005;
  xcpt_Illegal_Instruction      = $c0000006;
  xcpt_Privileged_Instruction   = $c0000006;
  xcpt_Unable_To_Grow_Stack     = $c000000c;
  xcpt_Access_Violation         = $c000000d;
  xcpt_In_Page_Error            = $c000000e;
  xcpt_Float_generic            = $c0000010;
  xcpt_Float_Denormal_Operand   = $c0000110;
  xcpt_Float_Divide_By_Zero     = $c0000210;
  xcpt_Float_Inexact_Result     = $c0000310;
  xcpt_Float_Invalid_Operation  = $c0000410;
  xcpt_Float_Overflow           = $c0000510;
  xcpt_Float_Stack_Check        = $c0000610;
  xcpt_Float_Underflow          = $c0000710;
  xcpt_DataType_Misalignment    = $c0000011;
  xcpt_Ctrl_Break               = $c00000cc;
  //----xcpt_Guard_Page_Violation     = $c000000;
  //----xcpt_NonContinuable_Exception = $C0000024;
  //----xcpt_Invalid_Disposition      = $C0000025;
{$ENDIF}
{$IFDEF LINUX}
  xcpt_Integer_Divide_By_Zero   = $C0080000;
  xcpt_Integer_Overflow         = $C00B0400;
  xcpt_Array_Bounds_Exceeded    = $C00B0500;
  xcpt_Illegal_Instruction      = $C0040600;
  xcpt_Privileged_Instruction   = $C0040600; // ?
  xcpt_Unable_To_Grow_Stack     = $C0040C00; // ?
  xcpt_Access_Violation         = $C00B0D00;
  xcpt_In_Page_Error            = $C00B0E00;

  xcpt_Float_Generic            = $C0081000;
  xcpt_Float_Denormal_Operand   = $C0081002;
  xcpt_Float_Divide_By_Zero     = $C0081004;
  xcpt_Float_Inexact_Result     = $C0081020;
  xcpt_Float_Invalid_Operation  = $C0081001;
  xcpt_Float_Overflow           = $C0081008;
  xcpt_Float_Stack_Check        = $C0081040; // ?
  xcpt_Float_Underflow          = $C0081010;
  xcpt_DataType_Misalignment    = $C0071100;
  xcpt_Ctrl_Break               = $C0020000;
{$ENDIF}

type
  PXcptReportRecord = ^TXcptReportRecord;
  TXcptReportRecord = record
    ExceptionNum: Longint;
    fHandlerFlags: Longint;
    NestedXcptReportRecord: PXcptReportRecord;
    ExceptionAddress: Pointer;
    cParameters: Longint;
    case Integer of
      0: (ExceptionInfo: array [0..3] of Longint);
      1: (ExceptAddr: Pointer;
          ExceptObject: Pointer);
  end;

{ BP7/Delphi Windows compatibility variables }

type
  Real = Double;                // The 48-bit TP-compatible Real type is called Real48

const
  HInstance: Longint = -1;      // Handle of this instance: -1 in Open32
  HPrevInst: Longint = 0;       // Handle of previous instance: 0 in Open32
  CmdShow:   Longint = 10;      // CmdShow parameter for CreateWindow: sw_ShowDefault

{$IFDEF DPMI32}
var
  code_base  : Longint;         // First byte of code object
  seldata    : SmallWord;       // Default DS selector
  sel_psp    : SmallWord;       // Process Segment Prefix selector
  sel_fs     : SmallWord;       // Selector for TIB emulation
  stacksize  : Longint;         // Needed for TIB emulation
  seg_psp    : Longint;         // Ofs( Mem[psp:0000] )

const
  seg0000    = $0000 shl 4;     // Ofs( Mem[$0000:0000] )
  seg0040    = $0040 shl 4;     // Ofs( Mem[$0040:0000] )
  sega000    = $a000 shl 4;     // Ofs( Mem[$a000:0000] )
  segb000    = $b000 shl 4;     // Ofs( Mem[$b000:0000] )
  segb800    = $b800 shl 4;     // Ofs( Mem[$b800:0000] )
{$ENDIF}

{$IFDEF LINUX}

{ File system settings for Linux. The TFileSystem type together with
  the FileSystem variable allows to select the kind of file system
  that the application sees. The selection affects the way in which
  the VpSysLow unit (and possibly other units, too) handle file names.

  The following values are possible:

  fsUnix ....... The application sees the real Unix file system. File
                 names are expected to use foreslashes as separators.
                 Backslashes and drive letters are not allowed. This
                 file system selection should be used when writing
                 pure Linux applications. No automatic conversion is
                 needed in this case.

  fsDos ........ The application sees a Dos-like, but case-sensitive
                 file system. File names are expected to contain
                 backslashes as separators. Foreslashes are not
                 allowed. The drive letter 'c:' (or 'C:') is allowed,
                 that is, the application sees a single drive. No
                 floppy drives do exist. The application can be sure
                 that all file names returned from system calls
                 (VpSysLow and other units) will conform to these
                 rules. This mode should be used when porting old
                 Dos, Windows or OS/2 applications to Linux. It
                 should allow a simple recompile of the programs
                 without having to change file handling.

  fsDosUpper ... The application sees a Dos-like, case-insensitive
                 file system. This file system selection is similar
                 to fsDos, with these two exceptions:

                 - All file names are converted to upper case before
                   passing them to the operating system. As an effect,
                   the application can only create or modify files
                   with upper case names.

                 - File search functions (FindFirst et. al.) filter
                   all file names that are not completely upper case.
                   As an effect, the application sees only upper case
                   names.

                 Use this mode to port existing Dos application to
                 Linux whose source code refers to the same disk file
                 with different spelling ('MyFile.Txt', 'myfile.txt).

  fsDosLower ... Same as fsDosUpper, but all file names are lower
                 case.                                                 }

type
  TFileSystem = (fsUnix, fsDos, fsDosUpper, fsDosLower);

const
  FileSystem: TFileSystem = fsUnix;
  RaiseNotification_Hook : Pointer = nil;
{$ENDIF}

implementation

{ OS interface }

function SysFileStdIn;          external;
function SysFileStdOut;         external;
function SysFileStdErr;         external;
function SysFileOpen;           external;
function SysFileCreate;         external;
function SysFileSeek;           external;
function SysFileRead;           external;
function SysFileWrite;          external;
function SysFileSetSize;        external;
function SysFileClose;          external;
function SysFileDelete;         external;
function SysFileMove;           external;
function SysFileIsDevice;       external;
function SysDirGetCurrent;      external;
function SysDirSetCurrent;      external;
function SysDirCreate;          external;
function SysDirDelete;          external;
function SysMemAvail;           external;
function SysMemAlloc;           external;
function SysMemFree;            external;
procedure SysCtrlSleep;         external;
function SysGetThreadId;        external;
function SysSysMsCount;         external;
procedure SysSysWaitSem;        external;
procedure SysSysSelToFlat;      external;
procedure SysSysFlatToSel;      external;
function SysCtrlSelfAppType;    external;
function SysCtrlCreateThread;   external;
function SysCtrlKillThread;     external;
function SysCtrlSuspendThread;  external;
function SysCtrlResumeThread;   external;
procedure SysCtrlExitThread;    external;
procedure SysCtrlExitProcess;   external;
function SysCtrlGetModuleName;  external;
procedure SysCtrlEnterCritSec;  external;
procedure SysCtrlLeaveCritSec;  external;
function SysCtrlGetTlsMapMem;   external;
function SysCmdln;              external;
function SysCmdlnCount;         external;
procedure SysCmdlnParam;        external;
function SysGetProcessId;       external;
function SemCreateMutex;        external;
procedure SemCloseMutex;        external;
function SemReleaseMutex;       external;
function SemRequestMutex;       external;
function SemAccessMutex;        external;
procedure SysLowInitPreTLS;     external;
procedure SysLowInitPostTLS;    external;
{$IfDef LNX_DPMI}
procedure AllocateDLLInitTib;   external;
procedure ReleaseDLLInitTib;    external;
{$EndIf LNX_DPMI}
{$IfDef Linux}
procedure Process_ArgC_ArgV_Env(const _ArgC: Longint; _ArgV: Pointer);  external;
{$EndIf Linux}


// Known problems:
// 1. The TLS memory allocated for a DLL loaded dynamically must be freed
//    after the DLL is unloaded. Otherwise the RTL will run out TLS shared
//    memory.

// Forward declaration

procedure RtlError; forward;
procedure FreeTLS; forward;
function ThreadStartup(P: Longint): Longint; {$IFDEF OS2} cdecl {$ELSE} stdcall {$ENDIF}; forward;
procedure _ExceptionHandler(Report,Registration,Context,Void: Pointer); cdecl; forward;

//±±±±±±±±±±±±±±±[ RECORDS AND CONSTANTS ]±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±

type
  PByte         = ^Byte;
  PSmallWord    = ^SmallWord;
  PLongint      = ^Longint;

// VMT header

type
  VMT = record
    InstanceSize:  Longint;     // Size of the object instance
    InstanceCheck: Longint;     // Negative size of the object instance
    DMTPointer:    Pointer;     // Pointer to the Dynamic Method Table
    EntryTable: record end;     // Pointers to the virtual methods start here
  end;

// DMT Header

  DMT = record
    Parent:         Pointer;    // Offset of the parent's DMT
    Cache_Entry:    Longint;    // Last used dynamic method entry
    Cache_Index:    Longint;    // Last used dynamic index
    Entry_Count:    Longint;    // Number of entries in the DMT
    Entry_Table: record end;    // Dynamic indices start here, pointers
  end;                          // to the method entries follow them

// Text file variable record

  TextBuf = array[0..127] of Char;
  TextRec = record
    Handle:    Longint;         // +00 File Handle
    Mode:      Longint;         // +04 Current file mode
    BufSize:   Longint;         // +08 Text File buffer size
    BufPos:    Longint;         // +0C Buffer current position
    BufEnd:    Longint;         // +10 Buffer ending position
    BufPtr:    PChar;           // +14 Pointer to the buffer
    OpenFunc:  Pointer;         // +18 Open Text File function @
    InOutFunc: Pointer;         // +1C In/Out ...
    FlushFunc: Pointer;         // +20 Flush ...
    CloseFunc: Pointer;         // +24 Close ...
    UserData:  array [1..32] of Byte;   // +28 User data area
    Name:      array [0..259] of Char;  // +48 File name (ASCIIZ)
    Buffer:    TextBuf;         // +14C Default I/O buffer
  end;                          // +1CC SizeOf(TextRec)

// Control Characters

const
  ccLF  = #$0A;                 // Line Feed
  ccCR  = #$0D;                 // Carriage Return
  ccEOF = #$1A;                 // EOF character

// File mode constants

  fmClosed =  $A55AD7B0;
  fmInput  =  $A55AD7B1;
  fmOutput =  $A55AD7B2;
  fmInOut  =  $A55AD7B3;

// Size of the buffer for direcory path

  PATH_BUFFER_SIZE = 260;

// Run-time error codes

  RTE_Disk_Read_Error           = 100;
  RTE_Disk_Write_Error          = 101;
  RTE_File_Not_Assigned         = 102;
  RTE_File_Not_Open             = 103;
  RTE_File_Not_Open_For_Input   = 104;
  RTE_File_Not_Open_For_Output  = 105;
  RTE_Invalid_Numeric_Format    = 106;
  RTE_Zero_Divide               = 200;
  RTE_Range_Check               = 201;
  RTE_Stack_Overflow            = 202;
  RTE_Heap_Overflow             = 203;
  RTE_Invalid_Pointer           = 204;
  RTE_FP_Overflow               = 205;
  RTE_FP_Underflow              = 206;
  RTE_Invalid_FP_Operation      = 207;
  RTE_FP_Inexact_Result         = 208;
  RTE_FP_Denormal_Operand       = 209;
  RTE_Object_Not_Initialized    = 210;
  RTE_Abstruct_Method_Call      = 211;
  RTE_Stream_Registration_Error = 212;
  RTE_Invalid_Collection_Index  = 213;
  RTE_Collection_Overflow       = 214;
  RTE_Integer_Overflow          = 215;
  RTE_Access_Violation          = 216;
  RTE_Signal                    = 217;
  RTE_Exception                 = 217;
  RTE_Privileged_Instruction    = 218;
  RTE_Invalid_Cast              = 219;

// 80x87 Status Word
const
  mSW_IE        = $0001;        // Invalid Operation exception
  wSW_IE        = 1;
  mSW_DE        = $0002;        // Denormalized Operand exception
  wSW_DE        = 1;
  mSW_ZE        = $0004;        // Zero-Divide exception
  wSW_ZE        = 1;
  mSW_OE        = $0008;        // Overflow exception
  wSW_OE        = 1;
  mSW_UE        = $0010;        // Underflow exception
  wSW_UE        = 1;
  mSW_PE        = $0020;        // Precision exception
  wSW_PE        = 1;
  mSW_SF        = $0040;        // Stack flag (387+ only)
  wSW_SF        = 1;
  mSW_ES        = $0080;        // Error summary
  wSW_ES        = 1;
  mSW_C0        = $0100;        // Condition bit 0
  wSW_C0        = 1;
  sSW_C0        = 8;
  mSW_C1        = $0200;        // Condition bit 1
  wSW_C1        = 1;
  mSW_C2        = $0400;        // Condition bit 2
  wSW_C2        = 1;
  mSW_ST        = $3800;        // Stack top
  wSW_ST        = 3;
  sSW_ST        = 11;
  mSW_C3        = $4000;        // Condition bit 3
  wSW_C3        = 1;
  mSW_B         = $8000;        // Busy bit
  wSW_B         = 1;

// 80x87 Control Word

  mCW_IM        = $0001;        // Invalid Operation mask
  wCW_IM        = 1;            // Bit = 1 if Exception is masked
  mCW_DM        = $0002;        // Denormalized Operand mask
  wCW_DM        = 1;
  mCW_ZM        = $0004;        // Zero-Divide mask
  wCW_ZM        = 1;
  mCW_OM        = $0008;        // Overflow mask
  wCW_OM        = 1;
  mCW_UM        = $0010;        // Underflow mask
  wCW_UM        = 1;
  mCW_PM        = $0020;        // Precision mask
  wCW_PM        = 1;
  mCW_PC        = $0300;        // Precision control
  wCW_PC        = 2;
  sCW_PC        = 8;
  mCW_RC        = $0C00;        // Rounding control
  wCW_RC        = 2;
  sCW_RC        = 10;
  mCW_IC        = $1000;        // Infinity control
  sCW_IC        = 12;
  wCW_IC        = 1;

  IC_Projective = 0;            // Projective closure (387 doesn't support it)
  IC_Affine     = 1;            // Affine mode

  RC_Nearest    = 0;            // Rounding to nearest (the default)
  RC_Down       = 1;            // Rounding down (towards "-" infinity)
  RC_Up         = 2;            // Rounding up (towards "+" infinity)
  RC_To_Zero    = 3;            // Rounding toward zero.

  PC_Single     = 0;            // Round to single precision
  PC_Reserved   = 1;            // Reserved ( should not be specified)
  PC_Double     = 2;            // Round to double precision
  PC_Extended   = 3;            // Round to extended precision (the default)

  TAG_Valid     = 0;            // Tag values
  TAG_Zero      = 1;
  TAG_Spec      = 2;
  TAG_Empty     = 3;

// x86 flags definition

  mCF           = $0001;
  mPF           = $0004;
  mAF           = $0010;
  mZF           = $0040;
  mSF           = $0080;
  mIF           = $0200;
  mDF           = $0400;
  mOF           = $0800;

// Descriptor definition

  desAttrBig      = $40;        // Attribute byte
  desAttrGran     = $80;

  mEXP_Sign       = $8000;      // Exponent field sign
  mEXP_Exponent   = $7FFF;      // Exponent field exponent
  EXP_Spec_Value  = $7FFF;      // Exponent value for NANs and INF
  SIGN_Inf_Value  = $8000;      // Value of the ER_Signifcand3 for infinity

type
  ExtRec = record               // Extended coprocessor value
    ER_Significand0: Word;      // low word of the significand field
    ER_Significand1: Word;      // second word of the significand field
    ER_Significand2: Word;      // third word of the significand field
    ER_Significand3: Word;      // high word of the significand field
    ER_Exponent    : Word;      // Exponent & Sign
  end;

// Class Virtual Method Table

const
  vtInitTable      = -48;
  vtTypeInfo       = -44;
  vtFieldTable     = -40;
  vtMethodTable    = -36;
  vtDynamicTable   = -32;
  vtClassName      = -28;
  vtInstanceSize   = -24;
  vtParent         = -20;
  vtDefaultHandler = -16;
  vtNewInstance    = -12;
  vtFreeInstance   = -8;
  vtDestroy        = -4;
  clVTable         = 0;

// Language Exception codes

  cContinuable          = 0;
  cNonContinuable       = 1;        // eh_NonContinuable
  cUnwinding            = 2;        // eh_Unwinding
  cUnwindingForExit     = 4;        // eh_Exit_Unwind
  cUnwindInProgress     = cUnwinding or cUnwindingForExit;
  cLanguageException    = $0EEDFACE;
  cLanguageReRaise      = $0EEDFACF;
  cLanguageExcept       = $0EEDFAD0;
  cLanguageFinally      = $0EEDFAD1;
  cLanguageTerminate    = $0EEDFAD2;
  cLanguageUnhandled    = $0EEDFAD3;
  cNonLanguageException = $0EEDFAD4;

// Run-time error codes

  reInOutError      = 0;
  reOutOfMemory     = 1;
  reInvalidPtr      = 2;
  reDivByZero       = 3;
  reRangeError      = 4;
  reIntOverflow     = 5;
  reInvalidOp       = 6;
  reZeroDivide      = 7;
  reOverflow        = 8;
  reUnderflow       = 9;
  reInvalidCast     = 10;
  reAccessViolation = 11;
  reStackOverflow   = 12;
  reSignal          = 13;
  rePrivilegedInstr = 14;

// Some RTTI type kinds

  tkLString = 10;
  tkArray   = 13;
  tkRecord  = 14;

// Subtables of the Virtual Method table

type
  TMethodTable = record
    Count:      SmallWord;
    Entries:    record end;
  end;

  TMethodEntry = record
    Address:    Longint;
    Name:       ShortString;
  end;

  TFieldTable = record
    Count:      SmallWord;
    ClassTable: Pointer;
    Entries:    record end;
  end;

  TFieldEntry = record
    Ofs:        Longint;
    ClassIndex: SmallWord;
    Name:       Byte;
  end;

  TDynamicTable = record
    Count:      Longint;
    Indices:    array[0..0] of Longint;
  end;

// Run-time type information header record

  PTypeInfo = ^TTypeInfo;
  TTypeInfo = record
    Kind: Byte;
    Name: ShortString;
  end;

// Class Run-time type information

  TClassRTTI = record
    ClassType:  TClass;
    ParentInfo: PTypeInfo;
    PropCount:  SmallWord;
    UnitName:   ShortString;
  end;

  TFieldRec = record
    TypeInfo: Pointer;
    Offset:   Longint;
  end;

  TRecType = record
    FieldCount: Longint;
    FieldTable: array[0..10] of TFieldRec;
  end;

  PTypeData = ^TTypeData;
  TTypeData = record
    case Byte of
      tkLString: ();
      tkArray: (
        ArrSize:   Longint;
        ElemCount: Longint;
        ElemRTTI:  Longint);
      tkRecord: (
        RecSize:   Longint;
        RecData:   TRecType);
  end;

  JmpInstruction = record
    OpCode:   Byte;
    Distance: Longint;
  end;

  TExcDescEntry = record
    vTable:  Pointer;
    Handler: Pointer;
  end;

  PExcDesc = ^TExcDesc;
  TExcDesc = record
    Jmp: JmpInstruction;
    case Integer of
      0: (Instructions: array [0..0] of Byte);
      1: (Cnt: Longint; ExcTab: array [0..0] of TExcDescEntry);
  end;

  PExcFrame = ^TExcFrame;
  TExcFrame = record
    Next: PExcFrame;
    Desc: PExcDesc;
    hEBP: Pointer;
    ConstructedObject: Pointer;
  end;

  PRaiseFrame = ^TRaiseFrame;
  TRaiseFrame = record
    NextRaise: PRaiseFrame;
    ExceptAddr: Pointer;
    ExceptObject: TObject;
    ExceptionRecord: PXcptReportRecord;
  end;

//±±±±±±±±±±±±±±±[ LOW LEVEL ROUTINES ]±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±

// Compiler should only use procedures that are implemented in this
// unit because dynamic version of the System unit exports all interface
// procedures and functions. For this reason all procedures that have macro
// assembler implementation should be called from System unit code.

// Compiler itself is unable to generate 16-bit code, so low level
// routines that have 16-bit code are implemented in macro assembler and
// placed in the SYSTEM.LIB

// 32 to 16 bit far pascal calling thunk support: OS/2 only
procedure __Far16Pas; external;
// Direct I/O Port access: OS/2, Win95 & DOS
{$IFDEF OS2}
procedure __IOPort;   external;
{$ENDIF}
{$IFDEF WIN32}
procedure __IOPort;   external;
{$ENDIF}
{$IFDEF LNX_DPMI}
procedure __IOPort;{$FRAME-}{$USES NONE}
  asm
    jmp [Offset @IOFunctionTable+ecx]

  {****************}

  @IOFunctionTable:
    dd      @InputByte
    dd      @InputWord
    dd      @InputDWord
    dd      @OutputByte
    dd      @OutputWord
    dd      @OutputDWord

  {****************}

  @InputByte:
    in al,dx
    ret

  {****************}

  @InputWord:
    in ax,dx
    ret

  {****************}

  @InputDWord:
    in eax,dx
    ret

  {****************}

  @OutputByte:
    out dx,al
    ret

  {****************}

  @OutputWord:
    out dx,ax
    ret

  {****************}

  @OutputDWord:
    out dx,eax

  {****************}

  end;
{$ENDIF}

// Performs a call of the far16 Pascal routine
// Two extra DWORD parameter are the last arguments passed
// ProcAddr: Longint;  FLAT address of the 16-bit routine entry point
// Parms:    Longint;  DWORD describing up to 16 parameters

procedure _Far16Pas; {&USES None} {&FRAME-}
asm
                jmp     __Far16Pas;
end;

// __IOPort: Direct I/O Port access
// EXPECTS:      [1]:DWord   = Value to write (output only)
//               [1/2]:DWord = Port number
// RETURNS:      eax         = Value that have been read (input only)

procedure _In8(PortNo: Longint); {&USES ecx,edx} {&FRAME-}
asm
                xor     ecx,ecx
                mov     edx,PortNo
                Call    __IOPort
end;

procedure _In16(PortNo: Longint); {&USES ecx,edx} {&FRAME-}
asm
                mov     ecx,1*4
                mov     edx,PortNo
                Call    __IOPort
end;

procedure _In32(PortNo: Longint); {&USES ecx,edx} {&FRAME-}
asm
                mov     ecx,2*4
                mov     edx,PortNo
                Call    __IOPort
end;

procedure _Out8(Value,PortNo: Longint); {&USES eax,ecx,edx} {&FRAME-}
asm
                mov     ecx,3*4
                mov     eax,Value
                mov     edx,PortNo
                Call    __IOPort
end;

procedure _Out16(Value,PortNo: Longint); {&USES eax,ecx,edx} {&FRAME-}
asm
                mov     ecx,4*4
                mov     eax,Value
                mov     edx,PortNo
                Call    __IOPort
end;

procedure _Out32(Value,PortNo: Longint); {&USES eax,ecx,edx} {&FRAME-}
asm
                mov     ecx,5*4
                mov     eax,Value
                mov     edx,PortNo
                Call    __IOPort
end;

procedure _FpuInit; assembler; {&USES None} {&FRAME-}
const
  Default: Word = $1332;                // Enabled: IM,ZM,OM
asm
                fninit
                fwait
                fldcw   Default
end;

// Compares the value of InOutRes with 0
// RETURNS:     ZF      = 1 if InOtRes = 0

procedure TestInOutRes; {&USES eax} {&FRAME-}
asm
                push    OFFSET InOutRes
                Call    _GetTlsVar
                cmp     [eax].Longint,0
end;

// Performs the following assignment: InOutRes := EAX

procedure SetInOutRes; {&USES EFL} {&FRAME-}
asm
                push    eax
                push    OFFSET InOutRes
                Call    _GetTlsVar
                pop     [eax].Longint
end;

// IOResult standard function
// function IOResult: Longint;
// RETURNS:     eax     = I/O result

procedure _GetIORes; {&USES ecx} {&FRAME-}
asm
                push    OFFSET InOutRes
                Call    _GetTlsVar
                mov     ecx,eax
                xor     eax,eax
                xchg    eax,[ecx]
end;

// Pointer conversion routines

procedure SelToFlat; {&USES None} {&FRAME-}
asm
                jmp     SysSysSelToFlat
end;

procedure FlatToSel; {&USES None} {&FRAME-}
asm
                jmp     SysSysFlatToSel
end;

// Checks I/O result. Called after any I/O standard routine
// when compiled in the {$I+} state.

procedure _IOChk; {&USES eax} {&FRAME-}
asm
                push    OFFSET InOutRes
                Call    _GetTlsVar
                mov     eax,[eax]
                test    eax,eax
                jz      @@RET
                add     esp,@Uses
                mov     al,reInOutError
                jmp     RtlError
              @@RET:
end;

//±±±±±±±±±±±±±±[ UTILITY ROUTINES ]±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±

// Calculates the length of the null-terminated string
// EXPECTS:     edi     = source string
// RETURNS:     eax     = Length

procedure PCharLength; {&Uses edi} {&Frame-}
asm

                lea     eax,[edi-1]      // eax = ptr to string-1
              @@Begin:
                inc     eax
                test    eax,3            // Check that eax is dword-aligned
                jnz     @@Odd
              @@More:
                mov     ecx,[eax]        // Get the next 4 bytes to check
                mov     edx,ecx          // Check if any of them are 0
                add     eax,4            // .. in just 3 clocks for 4 bytes
                not     ecx
                sub     edx,$01010101
                and     ecx,$80808080
                and     ecx,edx
                jz      @@More           // Zero flag: No 0 bytes in the dword
                sub     eax,4            // nz: a 0 was found
               @@Odd:
                cmp     [eax].Byte,0     // Search individual characters
                jne     @@Begin
                sub     eax,edi          // Calculate the result
end;

//±±±±±±±±±±±±±±[ HEAP MEMORY MANAGER ]±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±

type
  PBlockRec = ^TBlockRec;
  TBlockRec = record            // Heap free sub-block record
    Next:      PBlockRec;       // Pointer to the next free sub-block
    Size:      Longint;         // Size of the sub-block
  end;

  PHeapRec = ^THeapRec;
  THeapRec = record             // Heap Block record
    Signature: Longint;         // Signature = 'VPSM' or 'VPLG'
    FreeList:  TBlockRec;       // Free sub-block list head
    MemFree:   Longint;         // Number of free bytes in the Heap Block
    TotalSize: Longint;         // Total size of the Heap Block
    NextHeap:  Pointer;         // Pointer to the next Heap Block
    HeapOrg:   TBlockRec;       // Heap memory starts here, marks header end
  end;

  PHeapBlockList = ^THeapBlockList;
  THeapBlockList = Array[0..MaxLongInt div 4] of Longint;

  PHeapControl = ^THeapControl;
  THeapControl = record
    Semaphore : Longint;        // Internal: semaphore used to control heap access
    LockCount : Longint;        // Internal: How many locks the current thread has on the heap
    OwnerTid  : Longint;        // Internal: Id of thread currently locking the heap
  end;

const
  HeapBlockList : PHeapBlockList = nil; // Sorted list of heap blocks
  hblNext       : Longint = 0;          // Next entry to use
  hblAlloc      : Longint = 0;          // Number of entries allocated

const
  MemoryManager: TMemoryManager = (
    GetMem: SysGetMem;
    FreeMem: SysFreeMem;
    ReallocMem: SysReallocMem);

// Return the state of the heap, in a Delphi compatible THeapStatus
// record.

const
  OSCommitkB  = 4*1024-1;  // The OS commits memory in 4kB chunks
  OSAddresskB = 64*1024-1; // Each OS allocation uses 64kB Address space

procedure EnterHeap; {&Uses All} {&Frame-}
asm
        xor     ebx,ebx
      @@retry:
        mov     esi,[HeapControl]
        test    esi,esi
        jz      @@Ret           // Heap access control not initialized
   lock bts     [esi].THeapControl.Semaphore,0
        jnc     @@First
        call    SysGetThreadId
        cmp     eax,[esi].THeapControl.OwnerTid
        je      @@Subsequent
        inc     ebx
        cmp     ebx,25          // Spin 25 times without sleeping
        jl      @@retry
        xor     ebx,ebx
        push    31              // Wait for at least one timer slice
        Call    SysCtrlSleep    // and try to check again
        jmp     @@retry
      @@Subsequent:
        inc     [esi].THeapControl.LockCount
        jmp     @@Ret
      @@First:
        mov     [esi].THeapControl.LockCount,1
        call    SysGetThreadId
        mov     [esi].THeapControl.OwnerTid,eax
      @@Ret:
end;

procedure LeaveHeap; {&Uses All} {&Frame-}
asm
        mov     ebx,[HeapControl]
        test    ebx,ebx
        jz      @@Ret           // Heap access control not initialized
{$IFOPT D+}
        // For debug: Check that the right thread leaves the heap!
        call    SysGetThreadId
        cmp     [ebx].THeapControl.OwnerTid,eax
        je      @@TidOk
        int     3                   // Very bad!
      @@TidOk:
{$ENDIF}
        dec     [ebx].THeapControl.LockCount
        jnz     @@Ret               // Heap is still locked
        mov     [ebx].THeapControl.OwnerTid,0
        btr     [ebx].THeapControl.Semaphore,0
      @@Ret:
end;

function GetHeapStatus: THeapStatus;

  procedure ParseHeapList(P: PHeapRec);
  var
    Starting: PHeapRec;
  begin
    Starting := P;
    if P <> nil then
      repeat
        Inc(Result.TotalAddrSpace, (P^.TotalSize + OSAddresskB) and not OSAddresskB);
        Inc(Result.TotalCommitted, (P^.TotalSize + OSCommitkB) and not OSCommitkB);
        Inc(Result.FreeSmall, P^.MemFree);
        Inc(Result.Overhead, SizeOf(THeapRec) - SizeOf(TBlockRec));
        P := P^.NextHeap;
      until P = Starting;
  end;

begin
  FillChar(Result, SizeOf(Result), 0);
  Result.TotalAllocated := AllocMemSize;
  ParseHeapList(SmHeapList);
  ParseHeapList(LgHeapList);

  Result.TotalFree := Result.FreeSmall + Result.FreeBig;
  Result.TotalUnCommitted := Result.TotalAddrSpace - Result.TotalCommitted;
  Inc(Result.Overhead, AllocMemCount * 4);
  Dec(Result.TotalAllocated, AllocMemCount * 4);
end;

// New and GetMem standard procedures
// procedure New(var P: Pointer);
// procedure GetMem(var P: Pointer; Size: Longint);
// RETURNS:     eax     = Pointer to allocated memory block

procedure _MemNew(Size: Longint); {&USES ecx,edx} {&Frame-}
asm
                mov     eax,Size
                test    eax,eax
                jz      @@RET

                push    eax
                call    EnterHeap
                Call    MemoryManager.GetMem
                call    LeaveHeap

                test    eax,eax
                jnz     @@RET                   // Success?
                add     esp,@Uses               // No, report a run-time error
                mov     al,reOutOfMemory
                jmp     RtlError
              @@RET:
end;

// Dispose and FreeMem standard procedures
// procedure Dispose(var P: Pointer);
// procedure FreeMem(var P: Pointer[; Size: Longint]);

procedure _MemFree(P: Pointer); {&USES eax,ecx,edx} {&FRAME-}
asm
                mov     eax,P
                test    eax,eax
                jz      @@RET

                push    eax
                call    EnterHeap
                Call    MemoryManager.FreeMem
                call    LeaveHeap

                test    eax,eax
                jz      @@RET
                add     esp,@Uses
                mov     al,reInvalidPtr
                jmp     RtlError
              @@RET:
end;

// procedure ReallocMem(var P: Pointer; Size: Longint);
// The algorithm is the following:
//   þ if (P = nil) and (Size = 0), reallocMem does nothing
//   þ if (P = nil) and (Size <> 0), ReallocMem allocates new block of the given
//     size and sets P to point to the block. This corresponds to call to GetMem.
//     Additionally, the entire block of memory is cleared.
//   þ if (P <> nil) and (Size = 0), ReallocMem disposes the block referenced by
//      P and sets P to nil. This corresponds to a call to FreeMem, except that
//      FreeMem does not clear the pointer.
//   In the MemoryManager.ReallocMem case, we do not know the old size, and the
//   zero-fill is done in MemoryManager.ReallocMem.

procedure _MemRealloc(var P: Pointer; Size: Longint); {&USES eax,ecx,edx} {&FRAME-}
asm
                mov     eax,P
                mov     ecx,[eax]
                mov     edx,Size
                test    ecx,ecx                 // P = nil?
                jz      @@Alloc                 // yes, GetMem
                test    edx,edx                 // newsize = 0, P <> nill
                jz      @@Free                  // yes, the free P
              @@Resize:
                push    ecx                     // [1]:Pointer = OldMem
                push    edx                     // [2]:Longint = NewSize
                call    EnterHeap
                Call    MemoryManager.ReallocMem
                call    LeaveHeap
                test    eax,eax
                jnz     @@Set
              @@AllocError:
                add     esp,@Uses
                mov     al,reOutOfMemory
                jmp     RtlError
              @@Free:
                push    ecx                     // FreeMem(P)
                call    EnterHeap
                Call    MemoryManager.FreeMem
                call    LeaveHeap
                test    eax,eax
                jz      @@Set                   // Zero pointer
                add     esp,@Uses
                mov     al,reInvalidPtr
                jmp     RtlError
              @@Alloc:
                test    edx,edx                 // P=nil and NewSize=0?
                jz      @@RET                   // yes, done
                push    edx
                call    EnterHeap
                Call    MemoryManager.GetMem    // GetMem(NewSize)
                call    LeaveHeap
                test    eax,eax
                jz      @@AllocError
                mov     edx,Size                // Restore edx
                push    eax                     // new P
                push    edx                     // NewSize
                push    0                       // value (zero-fill)
                Call    _MemFill
              @@Set:
                mov     ecx,P
                mov     [ecx],eax
              @@RET:
end;

function GetPMemoryManager: PMemoryManager;
begin
  Result := @MemoryManager;
end;

procedure GetMemoryManager(var MemMgr: TMemoryManager);
begin
  MemMgr := MemoryManager;
end;

procedure SetMemoryManager(const MemMgr: TMemoryManager);
begin
  MemoryManager := MemMgr;
end;

function MemAvail: Longint; {&USES None} {&FRAME-}
asm
                call    EnterHeap
                Call    SysMemAvail
                mov     ecx,LgHeapList          // eax := Total Size
                test    ecx,ecx
                jz      @@Small
              @@2:
                add     eax,[ecx].THeapRec.MemFree
                mov     ecx,[ecx].THeapRec.NextHeap
                cmp     ecx,LgHeapList
                jne     @@2
              @@Small:
                mov     ecx,SmHeapList
                test    ecx,ecx
                jz      @@RET
              @@3:
                add     eax,[ecx].THeapRec.MemFree
                mov     ecx,[ecx].THeapRec.NextHeap
                cmp     ecx,SmHeapList
                jne     @@3
              @@RET:                            // Unlock heap manager
                test    eax,$80000000
                jz      @@ok
                mov     eax,$7fffffff            // Make sure result is 31 bits only
              @@ok:
                call    LeaveHeap
end;

function MaxAvail: Longint; {&USES None} {&FRAME-}
asm
                call    EnterHeap
                Call    SysMemAvail             // eax := Total size
                mov     ecx,LgHeapList
                call    @@Adjust                // Adjust with Large blocks
                mov     ecx,SmHeapList
                call    @@Adjust                // Adjust with small blocks
                jmp     @@RET

              @@Adjust:
                test    ecx,ecx
                jz      @@Exit
                mov     edx,ecx
              @@2:
                cmp     eax,[ecx].THeapRec.MemFree
                jae     @@3
                mov     eax,[ecx].THeapRec.MemFree
              @@3:
                mov     ecx,[ecx].THeapRec.NextHeap
                cmp     ecx,edx
                jne     @@2
              @@Exit:
                ret

              @@RET:
                test    eax,$80000000
                jz      @@ok
                mov     eax,$7fffffff            // Make sure result is 31 bits only
              @@ok:
                call    LeaveHeap
end;

procedure NewHeapBlock; forward;
procedure NewSubBlock;  forward;

// Allocates memory
// EXPECTS:     eax     = Size of the memory block to allocate
// RETURNS:     eax     = Pointer to allocated memory block (nil if failed)

function SysGetMem(Size: Longint): Pointer; assembler; {&USES ebx,esi,edi} {&FRAME-}
var
  BlockSize: Longint;
asm
                mov     eax,Size
                add     eax,(TYPE TBlockRec-1+4)  // Align size to a qword boundary
                and     al,NOT (TYPE TBlockRec-1) // +4 = Store BlockSize
                mov     BlockSize,eax
              @@0:

// Small allocations <= HeapLimit must be allocated from a small
// memory block to avoid putting a small allocation in a large
// block, which then can become almost empty and cause overcommitment
                cmp     eax,HeapLimit
                jge     @@ScanLarge             // Large block

                mov     ecx,SmHeapList
                test    ecx,ecx
                jz      @@AllocSmall

              @@NextSmall:
                Call    NewSubBlock             // Successfully allocated ?
                jnc     @@SmallOK               // Yes, exit
                mov     ecx,[ecx].THeapRec.NextHeap
                cmp     ecx,SmHeapList          // No, goto next heap block
                jne     @@NextSmall             // Are all blocks searched?
              @@AllocSmall:
                xor     ecx,ecx
                Call    NewHeapBlock            // Fail ?
                jc      @@ERROR                 // Yes, error

                Call    NewSubBlock             // No, success, allocate new
              @@SmallOK:                        // memory block
                mov     SmHeapList,ecx          // eax = Pointer to
                jmp     @@Done                  // allocated memory block

// Allocate a large block > HeapLimit
              @@ScanLarge:                      // Scan available Heap Blocks
                mov     ecx,LgHeapList
                test    ecx,ecx
                jz      @@AllocLarge

              @@NextLarge:                      // Scan available Heap Blocks
                Call    NewSubBlock             // Successfully allocated ?
                jnc     @@LargeOK
                mov     ecx,[ecx].THeapRec.NextHeap
                cmp     ecx,LgHeapList          // No, goto next heap block
                jne     @@NextLarge             // Are all blocks searched?
              @@AllocLarge:
                mov     ecx,1
                Call    NewHeapBlock            // Fail ?
                jc      @@ERROR                 // Yes, error

                Call    NewSubBlock             // No, success, allocate new
              @@largeOK:                        // memory block
                mov     LgHeapList,ecx          // eax = Pointer to
                jmp     @@Done                  // allocated memory block

              @@ERROR:
                mov     ecx,HeapError           // If HeapError = nil then
                test    ecx,ecx
                jz      @@Fail                  // exit
                push    eax                     // [1]:Longint = Failed size
                Call    ecx                     // Call HeapError
                cmp     eax,1
                mov     eax,BlockSize           // Restore requested size
// 0 = Failure with run-time error \  Now merged, always
// 1 = Failure: return NIL pointer /  causes RTE to occur
// 2 = Success: retry operation
                ja      @@0                     // Retry
              @@Fail:
                xor     eax,eax                 // Fail, return NIL pointer
                jmp     @@RET

              @@Done:
                mov     ecx,BlockSize
                mov     [eax],ecx
                inc     AllocMemCount
                add     AllocMemSize,ecx
                add     eax,4
              @@RET:                            // Unlock heap manager
end;

// Re-allocate a memory block allocated directly from the operating system
// all sizes are muliple of 4 bytes
function OSReallocMem(_P: Pointer; _Old, _New: Longint): Pointer;
  assembler; {&uses esi,edi} {&frame+}
asm
                mov     ecx,_New
                push    0                   // MemPtr
                mov     eax,esp
                push    ecx                 // [1]:DWord = Size
                push    HeapAllocFlags      // [2]:DWord = Flags
                push    eax                 // [3]:Pointer = @MemPtr
                Call    SysMemAlloc
                test    eax,eax             // Success?
                stc
                pop     eax                 // MemPtr
                jnz     @@RET               // SysMemAlloc failed

                test    eax,eax             // nil?
                stc                         // Set carry to indicate error
                jz      @@RET               // Error: Return

                push    eax
                mov     esi,_P              // Previous block
                mov     edi,eax             // New block
                mov     eax,_Old
                mov     ecx,_New
                cmp     eax,ecx             // Compare _Old and _New
                jg      @@1                 // _Old > _New
                mov     ecx,eax             // _New > _Old
              @@1:
                cld
                shr     ecx,2
                rep     movsd               // Copy data
                mov     eax,_P
                push    eax
                call    SysMemFree          // Free old block
                pop     eax                 // Return new mem block
              @@RET:
end;

// Insert the new heap block, in sorted order.  This dramatically
// speeds up freeing memory, as a binary search on a large number
// of heap blocks can be performed, instead of a linear one.

procedure InsertNewheapBlock( _P: Longint); {&uses ebx,esi} {&Frame+}
asm
                mov     eax,hblAlloc
                test    eax,eax             // Is HeapBlockList nil?
                jnz     @@Not0

// Allocate initial memory for HeapBlockList
                mov     ecx,512             // Yes, initial allocation in DWords
                mov     hblAlloc,ecx        // Save this value
                shl     ecx,2
                push    0                   // MemPtr
                mov     eax,esp
                push    ecx                 // [1]:DWord = Size
                push    HeapAllocFlags      // [2]:DWord = Flags
                push    eax                 // [3]:Pointer = @MemPtr
                Call    SysMemAlloc
                test    eax,eax             // function result=0?
                stc
                pop     eax                 // MemPtr
                jnz     @@RET               // Error: Return

                test    eax,eax             // nil?
                stc                         // Set carry to indicate error
                jz      @@RET               // Error: Return

                mov     HeapBlockList,eax
                jmp     @@Insert

              @@Not0:
                cmp     eax,hblNext         // Room for another entry?
                ja      @@Insert
                shl     eax,1               // No, allocate more memory -- double the size
                mov     hblAlloc,eax
                push    HeapBlockList       // [1]:DWord = MemPtr
                shl     eax,1
                push    eax                 // [2]:DWord = Old size
                shl     eax,1
                push    eax                 // [3]:DWord = New size
                call    OSReAllocMem
                jc      @@RET               // Error
                mov     HeapBlockList,eax

// Binary search for the insert location
              @@Insert:
                mov     edx,HeapBlockList
                mov     ebx,_P              // N
                mov     eax,0               // Lo := 0
                mov     ecx,hblNext         // Hi := Max
                mov     esi,eax
              @@1:
                cmp     eax,ecx             // Lo >= Hi?
                jae     @@Found
                mov     esi,eax             // j := (Lo+Hi) div 2
                add     esi,ecx
                shr     esi,1
                cmp     ebx,[edx+esi*4]     // N < x[j]?
                jb      @@2
                mov     eax,esi             // No; Lo := j+1
                inc     eax
                jmp     @@1
              @@2:
                mov     ecx,esi             // Yes: Hi := j
                jmp     @@1

// Perform the insert
              @@Found:
                mov     ecx,hblNext         // MoveCount := (Hi-j) div 4
                mov     esi,eax
                sub     ecx,esi
                shl     esi,2
                add     esi,edx
                mov     edx,esi
                test    ecx,ecx
                jz      @@NoMove            // Insert at end of list: No move
                mov     eax,ecx
                shl     eax,2
                add     esi,eax
                mov     edi,esi
                sub     esi,4
                std
                rep     movsd               // Make room
                cld
              @@NoMove:
                mov     [edx],ebx           // Copy new entry
                inc     hblNext             // Increase heap block count
              @@RET:
end;

// Allocates system memory for a Heap Block
// EXPECTS:     eax     = Minimum Size ofblock to allocate
//              ecx     = 0: Allocate small block
//              ecx     = 1: Allocate large block
// RETURNS:     ecx     = Heap block address
//              CF      = 1 if error

procedure NewHeapBlock; {&USES eax,ebx} {&FRAME-}
asm
                mov     ebx,eax
                test    ecx,ecx
                jnz     @@AllocLarge
                // Allocate block for small allocations
                mov     ecx,SmHeapBlock
                add     eax,THeapRec.HeapOrg.Longint
                cmp     eax,ecx
                jbe     @@1
                dec     ecx                     // Round up to boundary
                add     eax,ecx                 // Inc(Size, BlockSize-1)
                not     ecx
                and     eax,ecx                 // And not (Blocksize-1)
                mov     ecx,eax
                jmp     @@1

              @@AllocLarge:
                mov     ecx,LgHeapBlock
                add     eax,THeapRec.HeapOrg.Longint
                cmp     eax,ecx
                jbe     @@1
                dec     ecx
                add     eax,ecx                   // Round up to 64K boundary
                not     ecx
                and     eax,ecx
                mov     ecx,eax

              @@1:
                push    ecx                     // Size
                push    0                       // MemPtr
                mov     eax,esp
                push    ecx                     // [1]:DWord = Size
                push    HeapAllocFlags          // [2]:DWord = Flags
                push    eax                     // [3]:Pointer = @MemPtr
                Call    SysMemAlloc
                test    eax,eax
                pop     eax                     // MemPtr
                pop     ecx                     // Size
                stc
                jnz     @@RET                   // Error

                test    eax,eax                 // MemPtr = nil?
                stc
                jz      @@RET                   // Error

                push    eax
                push    ecx
                push    edx
                push    eax
                call    InsertNewheapBlock      // Insert in list
                pop     edx
                pop     ecx
                pop     eax
                jc      @@RET                   // Return if error
                mov     [eax].THeapRec.TotalSize,ecx
                sub     ecx,THeapRec.HeapOrg.Longint
                mov     [eax].THeapRec.MemFree,ecx
                mov     [eax].THeapRec.Signature,'GLPV' // For large blocks
                mov     edx,LgHeapList
                cmp     ebx,HeapLimit
                jge     @@Large
                mov     edx,SmHeapList
                mov     [eax].THeapRec.Signature,'MSPV' // For small blocks
              @@Large:
                clc
                mov     [eax].THeapRec.HeapOrg.Size,ecx
                and     [eax].THeapRec.HeapOrg.Next,0
                lea     ecx,[eax].THeapRec.HeapOrg
                and     [eax].THeapRec.FreeList.Size,0
                mov     [eax].THeapRec.FreeList.Next,ecx
                mov     ecx,eax                 // ring-linked list of blocks
                test    edx,edx
                jz      @@2                     // Empty? Yes, NextHeap := Self@
                xchg    eax,[edx].THeapRec.NextHeap
              @@2:                              // CF=0! (after TEST)
                mov     [ecx].THeapRec.NextHeap,eax
              @@RET:
end;

// Allocates new sub-block within Heap Block
// EXPECTS:     ecx     = Heap block base address
//              eax     = Size of the sub-block to allocate
// RETURNS:     eax     = Allocated sub-block base address (CF=0)
//              CF      = 1 if requested sub-block cannot be allocated

procedure NewSubBlock; {&USES None} {&FRAME-}
asm
                cmp     [ecx].THeapRec.MemFree,eax // Is there enough memory in
                jb      @@RET                   // this block ?, No, exit CF=1
                lea     edi,[ecx].THeapRec.FreeList
              @@1:
                mov     esi,edi                 // esi = Previous block
                mov     edi,[edi].TBlockRec.Next// Get next block
                cmp     edi,1                   // Next = nil ?
                jb      @@RET                   // Yes, exit CF=1
                mov     edx,[edi].TBlockRec.Size// Is block big enough ?
                sub     edx,eax                 // No, continue scanning free
                jb      @@1                     // blocks (@@1)
                mov     ebx,[edi].TBlockRec.Next// Yes, Size = Requested ?
                je      @@2                     // Yes, perfect match
// No, block is bigger, split it into 2 pieces: one of the requested size,
// and the other of the Old-Requested size (Since each memory request is
// aligned on (TYPE TBlockRec) boundary there is always enough space for
// TBlockRec.) Then insert second block into free block list.
                mov     [edi+eax].TBlockRec.Next,ebx
                mov     [edi+eax].TBlockRec.Size,edx
                lea     ebx,[edi+eax]
              @@2:
                mov     [esi].TBlockRec.Next,ebx   // Remove allocated block from
                sub     [ecx].THeapRec.MemFree,eax // free block list
                mov     eax,edi
                clc
              @@RET:
end;

// Merges current sub-block with the next adjacent
// EXPECTS:     ebx     = Current Sub-Block address

procedure MergeSubBlock; {&USES None} {&FRAME-}
asm
                mov     esi,ebx
                add     esi,[ebx].TBlockRec.Size
                cmp     esi,[ebx].TBlockRec.Next
                jne     @@RET
                mov     eax,[esi].TBlockRec.Next
                mov     [ebx].TBlockRec.Next,eax
                mov     eax,[esi].TBlockRec.Size
                add     [ebx].TBlockRec.Size,eax
              @@RET:
end;

// Disposes the memory block
// RETURNS:      eax    = <> 0 if error

function SysFreeMem(P: Pointer): Longint; {&USES ebx,esi,edi} {&FRAME-}
asm
                mov     ebx,P
                sub     ebx,4
                mov     eax,[ebx]
                test    bl,TYPE TBlockRec-1     // Is block base qword-aligned?
                jnz     @@ERROR                 // No, error
// Find Heap Block that contains this base address

                push    eax
              //push    esi                     // register in is &Uses list

                mov     ecx,HeapBlockList
                xor     eax,eax
                mov     edx,hblNext

              @@Next:                           // search index eax..edx
                mov     esi,eax                 // esi := (eax+edx) / 2
                add     esi,edx
                shr     esi,1
                cmp     ebx,[ecx+esi*4]
                jb      @@2                     // Nope, address is less
                mov     eax,[ecx+esi*4]
                add     eax,[eax].THeapRec.TotalSize
                cmp     ebx,eax
                jb      @@Found                 // Found insert point
                mov     eax,esi                 // More
                inc     eax
                jmp     @@Next

              @@2:
                mov     edx,esi
                dec     edx
                jmp     @@Next

              @@Found:
                mov     ecx,[ecx+esi*4]
                cmp     [ecx].THeapRec.Signature,'GLPV'
                je      @@SigOK
                cmp     [ecx].THeapRec.Signature,'MSPV'
                jne     @@ERROR
              @@SigOK:
                mov     edx,esi                 // Save index in edx
              //pop     esi                     // register in is &Uses list
                pop     eax
                jmp     @@BlockFound
              @@ERROR:
                mov     al,1
                jmp     @@Done

// Heap Block is found: ecx = base address
// þ Check for signature
// þ Scan free list and find out the right place to insert new free block
              @@BlockFound:
                lea     esi,[ecx].THeapRec.FreeList
              @@3:
                mov     edi,esi         // edi = Previous Block
                mov     esi,[esi].TBlockRec.Next
                test    esi,esi
                jz      @@4
                cmp     ebx,esi         // Request base > Current Free Block ?
                ja      @@3             // No, found
                je      @@ERROR         // Exactly equal ? Yes, probably double
              @@4:                      // dispose request, error

                dec     AllocMemCount
                sub     AllocMemSize,eax
                mov     [ebx].TBlockRec.Next,esi
                mov     [ebx].TBlockRec.Size,eax
                add     eax,[ecx].THeapRec.MemFree
                mov     [ecx].THeapRec.MemFree,eax
                add     eax,THeapRec.HeapOrg.Longint
                cmp     eax,[ecx].THeapRec.TotalSize
                jne     @@MergeBlock
// All heap block is empty, remove it from the list
                push    edx             // Save index

                lea     esi,SmHeapList
                cmp     [ecx].THeapRec.Signature,'MSPV'
                je      @@Small
                lea     esi,LgHeapList
              @@Small:

                xor     eax,eax
                mov     edx,[ecx].THeapRec.NextHeap
                cmp     ecx,edx
                je      @@6             // Only one list in chain: Set to nil

                mov     ebx,[esi]
              @@5:
                mov     eax,ebx
                mov     ebx,[ebx].THeapRec.NextHeap
                cmp     ebx,ecx
                jne     @@5
                mov     [eax].THeapRec.NextHeap,edx
              @@6:
                mov     [esi],eax

                pop     edx             // Restore index
// Free HeapListBlock reference
                push    edi
                push    ecx
                mov     esi,HeapBlockList
                dec     hblNext
                mov     ecx,hblNext
                sub     ecx,edx
                jle     @@FreeSystemMem
                shl     edx,2
                add     esi,edx
                mov     edi,esi
                add     esi,4
                cld
                rep     movsd

              @@FreeSystemMem:
                pop     ecx
                pop     edi
// Release system memory
                push    ecx                     // eax := Base address
                Call    SysMemFree
                jmp     @@OK
// There are allocated memory in the block, try to merge current sub-block
// with two adjacent ones.
              @@MergeBlock:
                Call    MergeSubBlock           // Merge with next sub-block
                mov     [edi].TBlockRec.Next,ebx
                mov     ebx,edi                 // Merge with previous sub-block
                Call    MergeSubBlock
              @@OK:
                xor     eax,eax
              @@Done:                           // Unlock heap manager
end;

// Reallocates a memory block

function SysReallocMem(P: Pointer; Size: Longint): Pointer; {&USES ebx,edx} {&FRAME-}
asm
                push    Size                    // [1]:Longint = Size
                Call    SysGetMem
                mov     ebx,eax
                mov     ecx,P
                mov     eax,Size
                mov     edx,[ecx-4]
                sub     edx,4                   // OldSize
                cmp     edx,eax                 // OldSize < Size
                jae     @@1                     // no, new Size is smaller, keep eax

                                                // yes, FillChar(New(Size)^[OldSize],,0)
                add     ebx,edx                 // @New()^[OldSize]
                push    ebx
                sub     ebx,edx
                sub     eax,edx                 // Size-OldSize
                push    eax
                push    0                       // Value = zero
                call    _MemFill
                mov     eax,edx                 // copy only OldSize bytes

              @@1:                              // Min(Size, OldSize)
                push    ecx                     // [1]:Pointer = Src
                push    ebx                     // [2]:Pointer = Dest
                push    eax                     // [3]:Pointer = Count
                Call    _MemMove
                push    ecx                     // [1]:Pointer = Mem
                Call    SysFreeMem
                test    eax,eax
                jz      @@2
                push    ebx
                Call    SysFreeMem
                xor     ebx,ebx
              @@2:
                mov     eax,ebx
end;

//±±±±±±±±±±±±±±[ RANDOM NUMBER GENERATOR ]±±±±±±±±±±±±±±±±±±±±±±±±±±±±±

// RETURNS:      eax =  Next Random Number

procedure NextRandom; {&USES edx} {&FRAME-}
asm
                mov     eax,8088405h            // New := 8088405h * Old + 1
                mul     RandSeed
                inc     eax
                mov     RandSeed,eax
end;

// Random standard function (Integer)
// function Random [ ( Range: Longint) ]: < Same type as parameter >;
// EXPECTS:       [1]:Longint = Upper Bound of the Range
// RETURNS:       eax = Random number X: 0 <= X < Range

procedure _RandInt(Range: Longint); {&USES edx} {&FRAME-}
asm
                Call    NextRandom
                mul     Range           // Random * Range / 1 0000 0000h
                mov     eax,edx         // 0 <= eax < Range
end;

// Random standard function (Float)
// function Random [ ( Range: Word) ]: < Same type as parameter >;
// RETURNS:     ST(0)   = Random Number X: 0 <= X < 1

procedure _RandFlt; assembler; {&USES eax} {&FRAME-}
const
  ConstDelta: Single   = 2147483648.0;          // 80000000h
  ConstScale: SmallInt = -32;                   // -32
asm
                Call    NextRandom              // Compute next random number
                fild    ConstScale              // Load -32
                fild    RandSeed                // Load 32-bit random integer
                fadd    ConstDelta              // Scale to 32-bit positive integer
                fscale                          // Scale so 0 <= ST < 1
                fstp    st(1)                   // Remove scaling factor
end;

procedure Randomize; {&USES eax,ecx,edx} {&FRAME-}
asm
                Call    SysSysMsCount
                mov     RandSeed,eax            // Init Random Seed
end;

//±±±±±±±±±±±±±±[ STRING HANDLING ROUTINES ]±±±±±±±±±±±±±±±±±±±±±±±±±±±±

// Loads a string
// Important!:  Doesn't pop destination pointer

procedure _StrLoad(Dest,Src: Pointer); {&USES eax,ecx,esi,edi} {&FRAME-}
asm
                cld
                mov     esi,Src
                mov     edi,Dest
                movzx   eax,[esi].Byte
                inc     esi
                mov     [edi],al
                inc     edi
                mov     ecx,eax
                shr     ecx,2
                and     al,11b
                rep     movsd
                mov     ecx,eax
                rep     movsb
                PopArgs @Params - Type Dest
end;

// Stores a string

procedure _StrStore(Src,Dest: Pointer; MaxLen: Longint); {&USES eax,ecx,esi,edi} {&FRAME-}
asm
                cld
                mov     esi,Src
                mov     edi,Dest
                mov     ecx,MaxLen
                movzx   eax,[esi].Byte
                inc     esi
                cmp     al,cl
                jbe     @@1
                mov     al,cl
              @@1:
                mov     [edi],al
                inc     edi
                mov     ecx,eax
                shr     ecx,2
                and     al,11b
                rep     movsd
                mov     ecx,eax
                rep     movsb
end;

// Converts Char to String
// Important!:  Doesn't pop destination pointer

procedure _StrChar(Dest: Pointer; Char: Byte); {&USES eax,edx} {&FRAME-}
asm
                mov     ah,Char
                mov     al,1
                mov     edx,Dest
                mov     [edx],ax
                PopArgs @Params - Type Dest
end;

// Converts packed string to string
// Important!:  Doesn't pop destination pointer

procedure _StrPacked(Dest,Src: Pointer; Len: Longint); {&USES eax,ecx,esi,edi} {&FRAME-}
asm
                cld
                mov     esi,Src
                mov     edi,Dest
                mov     eax,Len
                mov     [edi],al
                inc     edi
                mov     ecx,eax
                shr     ecx,2
                and     al,11b
                rep     movsd
                mov     ecx,eax
                rep     movsb
                PopArgs @Params - Type Dest
end;

// Copy standard function
// function Copy(S: String; Index: Integer; Count: Integer): String;
// Important!:  Doesn't pop destination pointer


procedure _StrCopy(Dest,Src: Pointer; Index,Count: Longint); {&USES eax,ecx,esi,edi} {&FRAME-}
asm
                cld
                mov     esi,Src
                mov     edi,Dest
                movzx   eax,Byte Ptr [esi]      // eax := Length(S)
                mov     ecx,Index               // ecx := Index
                test    ecx,ecx                 // If Index < 0 then Index := 1
                jg      @@1
                mov     ecx,1
              @@1:
                add     esi,ecx                 // esi := @S[Index]
                sub     eax,ecx                 // eax := Number of bytes from
                jb      @@Empty                 // Index to the end of string-1
                inc     eax
                mov     ecx,Count               // If Count < 0 then Count := 0
                test    ecx,ecx
                jge     @@2
                xor     ecx,ecx
              @@2:
                cmp     eax,ecx
                jbe     @@3                     // if eax > Count then eax := Count
                mov     eax,ecx
                jmp     @@3
              @@Empty:
                xor     eax,eax
              @@3:
                mov     [edi],al                // Store string length
                inc     edi
                mov     ecx,eax
                shr     ecx,2
                and     al,11b
                rep     movsd
                mov     ecx,eax
                rep     movsb
                PopArgs @Params - Type Dest
end;

// Concat standard function
// function Concat(s1 [,s2,..,sn] : String): String;
// Important!:  Doesn't pop destination pointer

procedure _StrConcat(Dest,Src: Pointer); {&USES eax,ecx,esi,edi} {&FRAME-}
asm
                cld
                mov     esi,Src
                mov     edi,Dest
                movzx   ecx,Byte Ptr [edi]      // ecx := Length(Dest)
                mov     al,[esi]                // al := Length(Src)
                inc     esi
                add     [edi],al                // Dest[0] := Length(Dest)+Length(Src)
                jnc     @@1                     // If Total Length > 255 then
                mov     Byte Ptr [edi],255      // Dest[0] := 255;
                mov     al,cl                   // Truncate source string
                not     al                      // al := truncated src length
              @@1:
                add     edi,ecx                 // edi := @Dest[Length(Dest)+1
                inc     edi
                mov     cl,al                   // Append source string
                shr     ecx,2
                and     al,11b
                rep     movsd
                mov     cl,al
                rep     movsb
                PopArgs @Params - Type Dest
end;

// Pos standard function
// function Pos(SubStr: String; S: String): Byte;
// RETURNS:     eax     = Index of the first char of SubStr within S or 0 if SubStr is not found

procedure _StrPos(SubStr,S: Pointer); {&USES ebx,ecx,edx,esi,edi} {&FRAME-}
asm
                cld
                mov     esi,SubStr
                movzx   eax,[esi].Byte
                inc     esi
                test    al,al                   // If SubStr = '' then Pos := 0
                jz      @@Not_Found             // (Not found)
                mov     edx,eax                 // edx := Length(SubStr)
                mov     edi,S                   // If Length(SubStr) > Length(S)
                movzx   ecx,Byte Ptr [edi]      // then Pos := 0 (Not found)
                sub     ecx,edx                 // þþþþþþþþþþþþþþþþþþþþ <=S
                jb      @@Not_Found             // þþþþþþþþþ            <=SubStr
                inc     ecx                     // ecx := <ÄÁÄÄÄÄÄÄÄÄÄÙ
                inc     edi                     // edi := @S[1]
              @@1:
                mov     al,[esi]                // Search SubStr[1] in S
                inc     esi
                repne   scasb
                jne     @@Not_Found
                mov     eax,edi                 // Compare other characters
                mov     ebx,ecx
                lea     ecx,[edx-1]
                shr     ecx,2                   // FAST String compare
                repe    cmpsd                   // if ecx = 0 then ZF = 1
                jne     @@2
                lea     ecx,[edx-1]
                and     ecx,11b
                repe    cmpsb
                je      @@Found
              @@2:
                mov     edi,eax                 // Restore edi, ecx
                mov     ecx,ebx
                mov     esi,SubStr
                inc     esi                     // esi := @SubStr[1]
                jmp     @@1

              @@Not_Found:
                xor     eax,eax                 // Pos := 0
                jmp     @@RET

              @@Found:
                dec     eax
                sub     eax,S
              @@RET:
end;

// String Compare
// RETURNS:     ZF = 1 if S1 = S2
//              CF = 1 if S1 < S2

procedure _StrCmp(S1,S2: Pointer); {&USES eax,ecx,esi,edi} {&FRAME-}
asm
                cld
                xor     ecx,ecx
                mov     esi,S1
                mov     edi,S2
                mov     al,[esi]                // al := Length(S1)
                inc     esi
                mov     ah,[edi]                // ah := Length(S2)
                inc     edi
                mov     cl,al
                cmp     cl,ah
                jbe     @@1
                mov     cl,ah                   // ecx := Min(al,ah)
              @@1:
                test    ecx,ecx
                jz      @@CmpLen
                repe    cmpsb
                jne     @@RET
              @@CmpLen:
                cmp     al,ah
              @@RET:
end;

// Insert standard procedure
// procedure Insert(Src: String; var S: String; Index: Integer);
// Insert(Src,Dest,Index) = Copy(Dest,1,Index-1) + Src + Copy(Dest,Index,255)

{ from rtl_changes.txt }
// Original code from Borland-Pascal 7.0 Runtime Libary Update (C) 1988-1994 Norbert Juffa

procedure _StrIns(Src,Dest: Pointer; DestLen,Index: Longint); assembler; {&USES eax,ebx,ecx,edx,esi,edi,ebp} {&FRAME-}
asm
                Mov     ESI, Src
                Xor     ECX, ECX
                Mov     EDI, Dest
                Xor     EBX, EBX
                Or      CL, [ESI]
                Jz      @@End_Ins
                Mov     EDX, Index
                Mov     EAX, DestLen
                Dec     EDX
                Mov     BL, [EDI]
                Cmp     EDX, 80000000h
                Sbb     EBP, EBP
                And     EDX, EBP
                Sub     EDX, EBX
                Sbb     EBP, EBP
                And     EDX, EBP
                Add     EDX, EBX
                Mov     EBP, EAX
                Sub     EBP, EDX
                Jbe     @@End_Ins
                Sub     EBP, ECX
                Ja      @@Make_Gap
                Mov     [EDI], AL
                Inc     EDI
                Add     ECX, EBP
                Add     EDI, EDX
                Jmp     @@Fill
        @@Make_Gap:
                Mov     EDX, ECX
                Add     ECX, EBX
                Sub     ECX, EAX
                Sbb     EBX, EBX
                And     ECX, EBX
                Add     EAX, ECX
                Mov     [EDI], AL
                Add     ECX, EBP
                Add     EDI, EAX
                Mov     EAX, ESI
                Mov     ESI, EDI
                Sub     ESI, EDX
                STD
                Rep     MovSB
                Mov     EDI, ESI
                Mov     ECX, EDX
                Mov     ESI, EAX
                Inc     EDI
        @@Fill:
                Mov     EAX, ECX
                Inc     ESI
                And     ECX, 3
                Shr     EAX, 2
                CLD
                Rep     MovSB
                Mov     ECX, EAX
                Rep     MovSD
        @@End_Ins:
end;

// Delete standard procedure
// procedure Delete(var S: String; Index: Integer; Count: Integer);
// Delete(S,Index,Count) = Copy(S,1,Index-1) + Copy(S,Index+Count,255)

{ from rtl_changes.txt }
// Original code from Borland-Pascal 7.0 Runtime Libary Update (C) 1988-1994 Norbert Juffa

procedure _StrDel(S: Pointer; Index,Count: Longint); assembler; {&USES eax,ecx,edx,esi,edi} {&FRAME-}
asm
                Mov     EAX, Index
                Mov     ECX, Count
                Dec     EAX
                CDQ
                And     EDX, EAX
                Sub     EAX, EDX
                Add     EDX, ECX
                Jle     @@End_Del
                Mov     ESI, S
                Xor     ECX, ECX
                Mov     CL, [ESI]
                Sub     ECX, EAX
                Jle     @@End_Del
                Sub     ECX, EDX
                Jg      @@Continue
                Mov     [ESI], AL
                Jmp     @@End_Del
        @@Continue:
                Sub     [ESI], DL
                Add     ESI, EAX
                Mov     EAX, ECX
                Inc     ESI
                Shr     EAX, 2
                Mov     EDI, ESI
                And     ECX, 3
                Add     ESI, EDX
                CLD
                Rep     MovSB
                Mov     ECX, EAX
                Rep     MovSD
        @@End_Del:
end;

// 'SetString' standard procedure

procedure _StrSet(S: Pointer; Buffer: PChar; Len: Longint); {&USES eax,ecx} {&FRAME-}
asm
                movzx   ecx,Len.Byte
                mov     eax,S
                mov     [eax],cl
                cmp     Buffer,0
                jz      @@RET
                inc     eax
                push    Buffer          // [1]: Source
                push    eax             // [2]: Dest
                push    ecx             // [3]: Length
                Call    _VarMove
              @@RET:
end;

//±±±±±±±±±±±±±±[ Long string support ]±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±

type
  TStrRec = record
    RefCnt:    Longint;         // String reference count
    Length:    Longint;         // Current dynamic string size
  end;

const
  SHS = SizeOf(TStrRec);        // String header size

// Cleans up the long string variable. If the reference count is zero, the
// dynamic variable is freed

procedure _LStrClr(LStr: Pointer); {&USES eax,ecx} {&FRAME-}
asm
                mov     eax,LStr
                mov     ecx,[eax]                       // eax = @LStr
                test    ecx,ecx
                jz      @@RET                           // Already cleared
                and     [eax].Longint,0
                mov     eax,[ecx-SHS].TStrRec.RefCnt
                dec     eax                             // < 0: literal string
                js      @@RET
           lock dec     [ecx-SHS].TStrRec.RefCnt
                jne     @@RET
                sub     ecx,SHS                         // Free
                push    ecx
                Call    _MemFree
              @@RET:
end;

// Creates a new long string. Returns a pointer to the allocated buffer.

procedure _LStrNew(Len: Longint); {&USES edx} {&Frame-}
asm
                xor     eax,eax
                mov     edx,Len
                test    edx,edx
                jle     @@RET
                lea     eax,[edx+SHS+1]
                push    eax                     // [1]:Longint = Size
                Call    _MemNew
                mov     [eax].TStrRec.Length,edx
                mov     [eax].TStrRec.RefCnt,1
                add     eax,SHS
                mov     [eax+edx].Byte,0
              @@RET:
end;

// Converts a packed string to a long string.

procedure _LStrPacked(Dest,Src: Pointer; Len: Longint); {&USES eax,ecx,edx} {&FRAME-}
asm
                mov     ecx,Dest
                mov     edx,Len
                push    ecx                     // Free previous Dest value
                Call    _LStrClr
                push    edx                     // [1]:Longint = Length
                Call    _LStrNew
                mov     [ecx],eax
                mov     ecx,Src
                test    ecx,ecx
                jz      @@RET
                push    ecx                     // [1]:Pointer = Src
                push    eax                     // [2]:Pointer = Dest
                push    edx                     // [3]:Longint = Count
                Call    _MemMove
              @@RET:
end;

// Converts a character to a long string

procedure _LStrChar(LStr: Pointer; C: Char); {&USES eax} {&FRAME-}
asm
                lea     eax,C
                push    LStr                    // [1]:Pointer = Dest
                push    eax                     // [2]:Pointer = Src
                push    1                       // [3]:Longint = Length
                Call    _LStrPacked
                PopArgs @Params - TYPE LStr
end;

// Converts a short string to a long string

procedure _LStrStr(var LStr: Pointer; SStr: Pointer); {&USES eax,ecx} {&FRAME-}
asm
                mov     eax,SStr
                movzx   ecx,[eax].Byte
                inc     eax
                push    LStr                    // [1]:Pointer = Dest
                push    eax                     // [2]:Pointer = Src
                push    ecx                     // [3]:Longint = Len
                Call    _LStrPacked
                PopArgs @Params - TYPE LStr
end;

// Converts a null-terminated string to a long string

procedure _LStrPChar(LStr: Pointer; Str: PChar); {&USES eax,ecx,edi} {&FRAME-}
asm
                xor     eax,eax
                mov     edi,Str
                test    edi,edi
                jz      @@1
                Call    PCharLength
              @@1:
                push    LStr                    // [1]:Pointer = Dest
                push    edi                     // [2]:Pointer = Src
                push    eax                     // [3]:Longint = Length
                Call    _LStrPacked
                PopArgs @Params - TYPE LStr
end;

procedure _LStrArray(Dest,Src: Pointer; Size: Longint); {&USES eax,ecx,edi} {&FRAME-}
asm
                mov     edi,Src
                mov     ecx,Size
                push    Dest                    // [1]:Pointer = Dest
                push    edi                     // [2]:Pointer = Src
                push    ecx
                xor     eax,eax
                cld
                repne   scasb
                jne     @@1
                not     ecx
              @@1:
                pop     eax
                add     ecx,eax
                push    ecx                     // [3]:Longint = Length
                Call    _LStrPacked
                PopArgs @Params - TYPE Dest
end;

// Sets a new length of a long string

procedure _LStrSetLen(var LStr: Pointer; Len: Longint); {&USES eax,ebx,ecx,edx} {&FRAME-}
asm
                mov     ecx,Len
                mov     ebx,LStr
                xor     eax,eax
                test    ecx,ecx
                jle     @@3                     // Len <= 0: Return empty string
                mov     eax,[ebx]
                test    eax,eax
                jz      @@1
                cmp     [eax-SHS].TStrRec.RefCnt,1
                jne     @@1
                sub     eax,SHS
                lea     edx,[ecx+SHS+1]         // +1 for null terminate
                push    eax
                push    esp                     // [1]:Pointer = @Ptr
                push    edx                     // [2]:Longint = Counter
                Call    _MemRealloc
                pop     eax                     // New reallocated pointer
                add     eax,SHS
                mov     [ebx],eax
                mov     [eax-SHS].TStrRec.Length,ecx
                mov     [eax+ecx].Byte,0
                jmp     @@RET
              @@1:
                push    ecx                     // [1]:Longint = Length
                Call    _LStrNew
                mov     edx,[ebx]
                test    edx,edx
                jz      @@3
                push    edx                     // [1]:Pointer = Src
                push    eax                     // [2]:Pointer = Dest
                mov     edx,[edx-SHS].TStrRec.Length
                cmp     edx,ecx
                jl      @@2
                mov     edx,ecx
              @@2:
                push    edx                     // [3]:Longint = Count
                Call    _MemMove
              @@3:                              // Free old string contents
                push    ebx                     // [1]:Pointer = Old string
                Call    _LStrClr
                mov     [ebx],eax
              @@RET:
end;

// Converts a long string to a short string

procedure _LStr2Str(SStr,LStr: Pointer; MaxLen: Longint); {&USES eax,ecx,edx} {&FRAME-}
asm
                mov     ecx,LStr
                mov     eax,SStr
                test    ecx,ecx
                jz      @@RET
                mov     edx,ecx
                mov     ecx,[edx-SHS].TStrRec.Length
                test    ecx,ecx
                jz      @@RET
                cmp     ecx,MaxLen
                jl      @@1
                mov     ecx,MaxLen
              @@1:
                inc     eax
                push    edx                     // [1]:Pointer = Src
                push    eax                     // [2]:Pointer = Dest
                push    ecx                     // [3]:Longint = Length
                Call    _MemMove
                dec     eax
              @@RET:
                mov     [eax],cl
                PopArgs @Params - TYPE SStr
end;

// Creates a long string containing Count characters Ch.
// function StringOfChar(Ch: Char; Count: Longint): String;

procedure _LStrOfChar(LStr: Pointer; C: Char; Count: Longint); {&USES eax,ecx,edx} {&FRAME-}
asm
                mov     edx,Count
                mov     ecx,LStr
                push    ecx
                Call    _LStrClr
                test    edx,edx
                jle     @@RET
                push    edx
                Call    _LStrNew
                mov     [ecx],eax
                push    eax                     // [1]:Pointer = Dest
                push    edx                     // [2]:Longint = Count
                push    C[8].Longint            // [3]:Longint = Char
                Call    _MemFill
              @@RET:
                PopArgs @Params - TYPE LStr
end;

// Reads a long string from a text file
// ! Does not pop out the file variable address

procedure _TxtRLStr(FileVar,LStr: Pointer); assembler; {&USES eax,ecx} {&FRAME+}
var
  TempLStr: Pointer;
  SStr: ShortString;
asm
                mov     ecx,LStr
                push    ecx
                Call    _LStrClr                // Free dest string
                lea     eax,SStr
                push    FileVar                 // [1]:Pointer = File
                push    eax                     // [2]:Pointer = String
                push    255                     // [3]:Longint = MaxLen
                Call    _TxtRStr
                pop     eax                     // Pop file@
                lea     eax,SStr
                push    ecx                     // [1]:Pointer = LStr
                push    eax                     // [2]:Pointer = SStr
                Call    _LStrStr
                pop     eax                     // Pop out String@
                cmp     SStr.Byte,255
                jne     @@RET
              @@1:
                lea     eax,SStr
                push    FileVar                 // [1]:Pointer = FileVar
                push    eax                     // [2]:Pointer = String
                push    255                     // [3]:Longint = MaxLen
                Call    _TxtRStr
                pop     eax                     // Pop file@
                lea     eax,TempLStr
                and     [eax].Longint,0
                push    eax                     // [1]:Pointer = LStr
                lea     eax,SStr
                push    eax                     // [2]:Pointer = SStr
                Call    _LStrStr
                pop     eax                     // Pop out String@
                push    ecx                     // [1]:Pointer = Dest LStr
                push    TempLStr                // [2]:Pointer = Src LStr
                Call    _LStrConcat             // Pop out Dest@
                pop     ecx                     // Clear the Src string
                push    eax                     // [1]:Pointer = LStr
                Call    _LStrClr
                cmp     SStr.Byte,255
                je      @@1
              @@RET:
                PopArgs @Params - TYPE FileVar
end;

// Assignement operator for long strings

procedure _LStrAsn(var Dest: Pointer; Src: Pointer); {&USES eax,ecx,edx} {&FRAME-}
asm
                mov     ecx,Src
                mov     eax,Dest
                test    ecx,ecx
                jz      @@1
                mov     edx,[ecx-SHS].TStrRec.RefCnt
                inc     edx
                jle     @@1
           lock inc     [ecx-SHS].TStrRec.RefCnt
              @@1:
                xchg    ecx,[eax]               // Do assign
                test    ecx,ecx
                jz      @@RET                   // Free the old contents of Dest
                mov     edx,[ecx-SHS].TStrRec.RefCnt
                dec     edx
                jl      @@RET
           lock dec     [ecx-SHS].TStrRec.RefCnt
                jne     @@RET
                sub     ecx,SHS
                push    ecx
                Call    _MemFree
              @@RET:
end;

procedure _LStrLoad(var Dest: Pointer; Src: Pointer); {&USES None} {&FRAME-}
asm
                push    Dest
                push    Src[4]
                Call    _LStrAsn
                PopArgs @Params - TYPE Dest
end;

procedure _LStrAsg(Dest,Src: Pointer); {&USES eax,ecx,edx} {&FRAME-}
asm
                mov     edx,Src
                mov     eax,Dest
                test    edx,edx
                jz      @@2
                mov     ecx,[edx-SHS].TStrRec.RefCnt
                inc     ecx
                jg      @@1
                push    eax
                push    [edx-SHS].TStrRec.Length        // Copy a literal string
                Call    _LStrNew
                push    edx                             // [1]:Pointer = Src
                push    eax                             // [2]:Pointer = Dest
                push    [edx-SHS].TStrRec.Length        // [3]:Longint = Size
                Call    _MemMove
                mov     edx,eax
                pop     eax
                jmp     @@2
              @@1:
           lock inc     [edx-SHS].TStrRec.RefCnt
              @@2:
                xchg    edx,[eax]
                test    edx,edx
                jz      @@3
                mov     ecx,[edx-SHS].TStrRec.RefCnt
                dec     ecx
                jl      @@3
           lock dec     [edx-SHS].TStrRec.RefCnt
                jne     @@3
                sub     edx,SHS
                push    edx                             // [1]:Pointer = Free memory
                Call    _MemFree
              @@3:
end;

// Concatenates two long strings

procedure _LStrConcat(var Dest: Pointer; Src: Pointer); {&USES ALL} {&FRAME-}
asm
                mov     esi,Src
                mov     ebx,Dest
                test    esi,esi
                jz      @@RET
                mov     ecx,[ebx]
                test    ecx,ecx
                jz      @@2
                mov     edi,[ecx-SHS].TStrRec.Length
                mov     edx,[esi-SHS].TStrRec.Length
                add     edx,edi
                push    ebx                     // [1]:Pointer = Dest
                push    edx                     // [2]:Pointer = New length
                Call    _LStrSetLen
                cmp     esi,ecx
                je      @@0
                mov     eax,esi
                mov     ecx,[esi-SHS].TStrRec.Length
                jmp     @@1
              @@0:
                mov     eax,[ebx]               // Append to itself
                mov     ecx,edi
              @@1:
                mov     edx,[ebx]               // Dest[Length(Dest)]
                add     edx,edi
                push    eax                     // [1]:Pointer = Src
                push    edx                     // [2]:Pointer = Dest
                push    ecx                     // [3]:Longint = Size
                Call    _MemMove
                jmp     @@RET
              @@2:                              // Assign: Dest := Src (Dest = nil)
                push    ebx                     // [1]:Pointer = Dest
                push    esi                     // [2]:Pointer = Src
                Call    _LStrAsg
              @@RET:
                PopArgs @Params - TYPE Dest
end;

// Compares two long strings. Returns the result in the CPU flags.

procedure _LStrCmp(LStr1,LStr2: Pointer); {&USES eax,ecx,esi,edi} {&FRAME-}
asm
                mov     esi,LStr1
                mov     edi,LStr2
                cmp     esi,edi
                je      @@RET
                test    esi,esi
                jz      @@2
                test    edi,edi
                jz      @@3
                mov     eax,[esi-SHS].TStrRec.Length
                mov     ecx,[edi-SHS].TStrRec.Length
                sub     eax,ecx         // ecx = Len1
                ja      @@1
                add     ecx,eax         // ecx = Len2 + (Len1 - Len2) = Len1
              @@1:                      // ecx := Min(Len1, Len2)
                cld
                repe    cmpsb           // Compare string
                jne     @@RET           // if equal, compare lengths
                add     eax,eax         // if eax < 0 then CF:=1 (JB=JC),
                jmp     @@RET           // if eax > 0 then CF := 0, eax = 0 ZF=1
              @@2:
                cmp     esi,[edi-SHS].TStrRec.Length
                jmp     @@RET
              @@3:
                cmp     [esi-SHS].TStrRec.Length,edi
              @@RET:
end;

// Increments the usage count of the long string

procedure _LStrAddRef(LStr: Pointer); {&USES eax,ecx} {&FRAME-}
asm
                mov     ecx,LStr
                test    ecx,ecx
                jz      @@RET
                mov     eax,[ecx-SHS].TStrRec.RefCnt
                inc     eax
                jle     @@RET
           lock inc     [ecx-SHS].TStrRec.RefCnt
              @@RET:
end;

// Type cast routine from long string to PChar

procedure _LStrToPChar(LStr: Pointer); {&USES None} {&FRAME-}
asm
                mov     eax,LStr
                test    eax,eax
                jz      @@1
                ret     @Params
              @@Zero:
                db      0
              @@1:
                mov     eax,OFFSET @@Zero
              @@RET:
end;

// Creates a unique copy of a long string

procedure UniqueString(var LStr: String); {&USES ecx,edx} {&FRAME-}
asm
                mov     edx,LStr
                mov     ecx,[edx]
                test    ecx,ecx
                jz      @@RET
                mov     eax,[ecx-SHS].TStrRec.RefCnt
                dec     eax
                jz      @@RET                           // Already unique
                jl      @@1                             // Literal: Skip
           lock dec     [ecx-SHS].TStrRec.RefCnt
              @@1:
                push    [ecx-SHS].TStrRec.Length        // [1]:Longint = Length
                Call    _LStrNew
                mov     [edx],eax
                push    ecx                             // [1]:Pointer = Src
                push    eax                             // [2]:Pointer = Dest
                push    [ecx-SHS].TStrRec.Length        // [3]:Longint = Size
                Call    _MemMove
              @@RET:
                mov     eax,[edx]
end;

// function Copy(S: AnsiString; Index,Count: Longint): AnsiString;

procedure _LStrCopy(var Dest: Pointer; Src: Pointer; Index,Count: Longint); {&USES eax,ebx,ecx,edx} {&FRAME-}
asm
                mov     eax,Src
                mov     edx,Index
                mov     ecx,Count
                test    eax,eax
                jz      @@Empty
                mov     ebx,[eax-SHS].TStrRec.Length
                test    ebx,ebx
                jz      @@Empty
                dec     edx                     // edx = 0-based index
                jge     @@1                     // Make sure it is within the
                xor     edx,edx                 // range: 0..Length(Src)-1
              @@1:
                cmp     edx,ebx
                jge     @@Empty
                sub     ebx,edx                 // Length(Src) - Index
                test    ecx,ecx                 // Make sure count is within the
                jl      @@Empty                 // range: 0..Length(Src)-Index
                cmp     ecx,ebx
                jl      @@2
                mov     ecx,ebx
              @@2:
                add     eax,edx
                push    Dest                    // [1]:Pointer = Dest
                push    eax                     // [2]:Pointer = Src
                push    ecx                     // [3]:Longint = Length
                Call    _LStrPacked
                jmp     @@RET
              @@Empty:
                push    Dest
                Call    _LStrClr
              @@RET:
                PopArgs @Params - Type Dest
end;

// procedure Delete(var LStr: AnsiString; Index,Count: Longint);

procedure _LStrDel(LStr: Pointer; Index,Count: Longint); {&USES ALL} {&FRAME-}
asm
                mov     ebx,LStr
                mov     esi,Index
                cmp     esi,0

                jle     @@RET           // Index <= 0: nop
                mov     edi,Count
                cmp     edi,0
                jle     @@RET           // Count <= 0: nop

                push    ebx                     // [1]:Pointer = LStr
                Call    UniqueString
                mov     edx,[ebx]               // Source is already empty
                test    edx,edx
                jz      @@RET
                mov     ecx,[edx-SHS].TStrRec.Length
                dec     esi                     // Make 0-based index
                jl      @@RET                   // Make sure index is within the
                cmp     esi,ecx                 // range: 0..Length(LStr)-1
                jge     @@RET
                test    edi,edi                 // Make sure Index is within the
                jle     @@RET                   // range: 0..Length(LStr)-Index
                sub     ecx,esi
                cmp     edi,ecx
                jle     @@1
                mov     edi,ecx
              @@1:
                sub     ecx,edi                 // Length(LStr)-Index-Count
                add     edx,esi                 // LStr[Index]
                lea     eax,[edx+edi]           // LStr[Index+Count]
                push    eax                     // [1]:Pointer = Src
                push    edx                     // [2]:Pointer = Dest
                push    ecx                     // [3]:Longint = Count
                Call    _MemMove
                mov     eax,[ebx]
                mov     eax,[eax-SHS].TStrRec.Length
                sub     eax,edi
                push    ebx                     // [1]:Pointer = LStr
                push    eax                     // [2]:Longint = NewLength
                Call    _LStrSetLen
              @@RET:
end;

// procedure Insert(Src: String; var S: String; Index: Integer);

procedure _LStrIns(Src: Pointer; var Dest: Pointer; Index: Longint); {&USES ALL} {&FRAME-}
asm
                mov     ebx,Src
                mov     esi,Dest
                mov     edi,Index
                test    ebx,ebx
                jz      @@RET
                mov     edx,[esi]
                push    edx
                test    edx,edx
                jz      @@1
                mov     edx,[edx-SHS].TStrRec.Length
              @@1:
                dec     edi                     // Make index 0-based
                jge     @@2                     // Make sure index is within the
                xor     edi,edi                 // range: 0..Length(S)
              @@2:
                cmp     edi,edx
                jle     @@3
                mov     edi,edx
              @@3:
                mov     ecx,[ebx-SHS].TStrRec.Length
                add     edx,ecx
                push    esi                     // [1]:Pointer = LStr
                push    edx                     // [2]:Longint = New length
                Call    _LStrSetLen
                pop     eax
                cmp     eax,ebx
                jne     @@4
                mov     ebx,[esi]               // Insert Self
              @@4:
// Move(Dest[Index], Dest[Index+Length(Src)], Length(Dest)-Length(Src)-Index);
                mov     eax,[esi]
                lea     edx,[eax+edi]
                push    edx             // [1]:Pointer = Src : Dest[Index]
                add     edx,ecx
                push    edx             // [2]:Pointer = Dest: Dest[Index+Length(Src)]
                mov     edx,[eax-SHS].TStrRec.Length
                sub     edx,ecx
                sub     edx,edi
                push    edx             // [3]:Longint = Size: Length(Dest)-Length(Src)-Index
                Call    _MemMove
                mov     eax,[esi]
                add     eax,edi
                push    ebx             // [1]:Pointer = Src : Src
                push    eax             // [2]:Pointer = Dest: Dest[Index]
                push    ecx             // [3]:Longint = Size: Length(Src)
                Call    _MemMove
              @@RET:
end;

// function Pos(SubStr,S: AnsiString): Byte;

procedure _LStrPos(SubStr,LStr: Pointer); {&USES ecx,edx,esi,edi} {&FRAME-}
asm
                mov     esi,SubStr
                mov     edi,LStr
                test    esi,esi
                jz      @@Zero
                test    edi,edi
                jz      @@Zero
                mov     ecx,[edi-SHS].TStrRec.Length
                mov     edx,[esi-SHS].TStrRec.Length
                dec     edx
                js      @@Zero
                cld
                mov     al,[esi]
                inc     esi
                sub     ecx,edx
                jle     @@Zero
              @@1:
                repne   scasb
                jne     @@Zero
                push    ecx
                push    esi
                push    edi
                mov     ecx,edx
                repe    cmpsb
                pop     edi
                pop     esi
                pop     ecx
                jne     @@1
                mov     eax,edi
                sub     eax,LStr
                jmp     @@RET
              @@Zero:
                xor     eax,eax
              @@RET:
end;

//±±±±±±±±±±±±±±[ CHAR HANDLING ROUTINES ]±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±

// UpCase standard function

procedure _UpCase(Char: Byte); {&USES None} {&FRAME-}
asm
                mov     al,Char
                cmp     al,'a'
                jb      @@RET
                cmp     al,'z'
                ja      @@RET
                sub     al,'a'-'A'
              @@RET:
end;

//±±±±±±±±±±±±±±[ SET HANDLING ROUTINES ]±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±

// Loads packed set
// Important!:  Doesn't pop destination pointer

procedure _SetLoad(Dest,Src: Pointer; SetData: Longint); {&USES eax,ebx,ecx,esi,edi} {&FRAME-}
asm
                cld
                mov     esi,Src
                mov     edi,Dest
                mov     ebx,SetData
                movzx   ecx,bh          // High Byte = Set Offset
                xor     eax,eax         // Zero fill to the set start
                shr     ecx,2
                rep     stosd           // FAST STOS
                mov     cl,bh
                and     cl,11b
                rep     stosb
                mov     cl,bl           // Low Byte = Set Size
                shr     ecx,2           // Copy Set value itself
                rep     movsd
                mov     cl,bl           // FAST MOVS
                and     cl,11b
                rep     movsb
                mov     cl,32           // Zero fill to the end of
                sub     cl,bl           // set variable
                sub     cl,bh
                mov     bl,cl           // FAST STOS
                shr     ecx,2
                rep     stosd
                mov     cl,bl
                and     cl,11b
                rep     stosb
                PopArgs @Params - Type Dest
end;

// Loads dword sized set
// Important!:  Doesn't pop destination pointer

procedure _SetDWordLoad(Dest: Pointer; Value: Longint); {&USES eax,ecx,edi} {&FRAME-}
asm
                cld
                mov     edi,Dest
                mov     eax,Value
                mov     [edi],eax
                add     edi,4
                mov     ecx,(32-4)/4
                xor     eax,eax
                rep     stosd
                PopArgs @Params - Type Dest
end;

// Adds specified range to the set
// Important!:  Doesn't pop destination pointer

procedure _SetAddRange(Dest: Pointer; Lower,Upper: Byte); {&USES eax,ebx,ecx,edx,edi} {&FRAME-}
asm
                mov     edi,Dest
                movzx   ecx,Lower
                movzx   edx,Upper
                sub     edx,ecx         // Upper bound < Lower bound, do nothing
                jb      @@RET
                mov     eax,ecx
                shr     eax,3
                add     edi,eax         // edi = first byte of the range to set
                inc     edx             // edx = # of bits to set to 1's
//         ³0 1 2 3 4 5 6 7³
//     ÄÄÄÄÁÄÁÄÁÄÁÄÁÄÁÄÁÄÁÄÁÄÄÄÄ....
//[1]:         ÃÄÄÄÄÄ´
//[2]:       ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ....ÄÄÄ´
//
//þ Handle first byte
                mov     eax,ecx
                and     eax,00000111b   // eax: Bit Position in byte (0..7)
                mov     ebx,edx
                lea     ecx,[eax+edx]   // Bit Pos + Length < 8 ?
                cmp     ecx,8
                jbe     @@1             // Case [1]
                mov     ebx,8           // Case [2]
                sub     ebx,eax
              @@1:                      // ebx: Bit length           (1..8)
                mov     al,Byte Ptr @@AddRangeTable[eax*8+ebx-1]
                or      [edi],al
                inc     edi
// þ Handle full bytes
                sub     edx,ebx         // # of bit remains
                jz      @@RET
                mov     ebx,edx
                shr     ebx,3           // Number of bytes
                mov     ecx,ebx
                shr     ecx,2           // # of full DWords
                or      eax,-1          // eax := all 1's
                cld
                rep     stosd
                mov     cl,bl
                and     cl,11b          // # of full bytes
                rep     stosb
// þ Handle last byte
                and     edx,00000111b   // # of bit remains = Length (1..7)
                jz      @@RET           // Position in byte = 0
                mov     al,Byte Ptr @@AddRangeTable[edx-1]
                or      [edi],al
                jmp     @@RET

// TYPE  BitLength = 1..8;
//       BitPos    = 0..7;
// AddRangeTable : ARRAY [BitPos, BitLength] OF BYTE; 64 elements
//  Length:                  1         2          3          4          5          6          7          8    Position
@@AddRangeTable:
                db      00000001b, 00000011b, 00000111b, 00001111b, 00011111b, 00111111b, 01111111b, 11111111b // 0
                db      00000010b, 00000110b, 00001110b, 00011110b, 00111110b, 01111110b, 11111110b, 11111110b // 1
                db      00000100b, 00001100b, 00011100b, 00111100b, 01111100b, 11111100b, 11111100b, 11111100b // 2
                db      00001000b, 00011000b, 00111000b, 01111000b, 11111000b, 11111000b, 11111000b, 11111000b // 3
                db      00010000b, 00110000b, 01110000b, 11110000b, 11110000b, 11110000b, 11110000b, 11110000b // 4
                db      00100000b, 01100000b, 11100000b, 11100000b, 11100000b, 11100000b, 11100000b, 11100000b // 5
                db      01000000b, 11000000b, 11000000b, 11000000b, 11000000b, 11000000b, 11000000b, 11000000b // 6
                db      10000000b, 10000000b, 10000000b, 10000000b, 10000000b, 10000000b, 10000000b, 10000000b // 7

              @@RET:
                PopArgs @Params - Type Dest
end;

// Stores unpacked set

procedure _SetStore(Src,Dest: Pointer; SetData: Longint); {&USES eax,ecx,esi,edi} {&FRAME-}
asm
                cld
                xor     ecx,ecx
                mov     esi,Src
                mov     edi,Dest
                mov     eax,SetData
                mov     cl,ah           // High Byte = Set Offset
                add     esi,ecx
                mov     cl,al           // Low Byte = Set Size
                shr     ecx,2
                and     al,11b
                rep     movsd           // FAST MOVS
                mov     cl,al
                rep     movsb
end;

// Operators on two unpacked sets
// _SetUnion     '+' operator
// _SetDif       '-' operator
// _SetInter     '*' operator
// _SetRel       ? ( '>','<' ) set operators
// _SetEqual     ? ( '=','<>') set operators
// Important!:  Doesn't pop destination pointer

procedure _SetUnion(Dest,Src : Pointer); {&USES eax,ecx,esi,edi} {&FRAME-}
asm
                mov     esi,Src
                mov     edi,Dest
                mov     ecx,32/4
              @@1:
                mov     eax,[esi]
                add     esi,4
                or      [edi],eax
                add     edi,4
                dec     ecx
                jnz     @@1
                PopArgs @Params - Type Dest
end;

procedure _SetDif(Dest,Src : Pointer); {&USES eax,ecx,esi,edi} {&FRAME-}
asm
                mov     esi,Src
                mov     edi,Dest
                mov     ecx,32/4
              @@1:
                mov     eax,[esi]
                add     esi,4
                not     eax
                and     [edi],eax
                add     edi,4
                dec     ecx
                jnz     @@1
                PopArgs @Params - Type Dest
end;

procedure _SetInter(Dest,Src : Pointer); {&USES eax,ecx,esi,edi} {&FRAME-}
asm
                mov     esi,Src
                mov     edi,Dest
                mov     ecx,32/4
              @@1:
                mov     eax,[esi]
                add     esi,4
                and     [edi],eax
                add     edi,4
                dec     ecx
                jnz     @@1
                PopArgs @Params - Type Dest
end;

// RETURNS:     ZF = 1 if Destination >= Source

procedure _SetRel(Dest,Src : Pointer); {&USES eax,ecx,esi,edi} {&FRAME-}
asm
                mov     esi,Src
                mov     edi,Dest
                mov     ecx,32/4
              @@1:
                mov     eax,[esi]
                add     esi,4
                add     edi,4
                cmp     eax,[edi-4]
                jne     @@RET
                dec     ecx
                jnz     @@1
              @@RET:
end;

// RETURNS:     ZF = 1 if Destination = Source

procedure _SetEqual(Dest,Src : Pointer); {&USES ecx,esi,edi} {&FRAME-}
asm
                cld
                mov     esi,Src
                mov     edi,Dest
                mov     ecx,32/4
                repe    cmpsd
end;

//±±±±±±±±±±±±±±[ INTEGER STRING ROUTINES ]±±±±±±±±±±±±±±±±±±±±±±±±±±±±±

// Converts integer to string
// EXPECTS:     eax    = Value to convert
//              edi    = @ of the buffer to hold output string
// RETURNS:     ecx    = Output string length
//              edi    = @ of the buffer just after the string written

procedure Int2Str; {&USES eax,ebx,edx} {&FRAME-}
asm
                push    edi
                test    eax,eax
                jge     @@1
                neg     eax
                mov     [edi].Byte,'-'
                inc     edi
              @@1:
                mov     ebx,10
                xor     ecx,ecx
              @@2:
                xor     edx,edx
                div     ebx
                add     dl,'0'
                push    edx
                inc     ecx
                test    eax,eax
                jnz     @@2
              @@3:
                pop     eax
                mov     [edi],al
                inc     edi
                dec     ecx
                jnz     @@3
                mov     ecx,edi
                pop     eax
                sub     ecx,eax
end;

// Converts string to integer
// EXPECTS:     ecx     = Length of the source string
//              edi     = @ of the buffer with source string
// RETURNS:     ecx     = Number of remaining characters
//              edi     = @ of the buffer just after parsed str
//              eax     = Converted value
//              CF      = 0 if success

procedure Str2Int; {&USES ebx,edx,esi} {&FRAME-}
asm
                test    ecx,ecx
                jz      @@Failed
                xor     eax,eax                 // Result
                xor     ebx,ebx                 // Current digit
                xor     esi,esi                 // Sign: 0 = '+', 1 = '-'
                cmp     [edi].Byte,'+'
                je      @@Positive
                cmp     [edi].Byte,'-'
                jne     @@No_Sign
                inc     esi
              @@Positive:
                inc     edi
                dec     ecx
                test    ecx,ecx
                jz      @@Failed
              @@No_Sign:
                cmp     [edi].Byte,'$'          // Hexadecimal ?
                jne     @@Decimal
// Integer is in Hexadecimal form
                inc     edi                     // Skip '$'
                dec     ecx
                test    ecx,ecx
                jz      @@Failed
              @@1:
                mov     bl,[edi]
                cmp     bl,'a'                  // Letter ?
                jb      @@2
                sub     bl,'a'-'A'              // Yes, Convert to upper case
              @@2:
                sub     bl,'0'+10               // Decimal digit 0..9 ?
                add     bl,10                   // 10 digits
                jc      @@3                     // Yes
                sub     bl,'A'-'9' + 15         // Is it hex letter A..F ?
                add     bl,6                    // 6 letters
                jnc     @@OK
                add     bl,10
              @@3:
                test    eax,0F0000000h          // bl = Current Hex Digit 0..15
                jnz     @@Failed
                shl     eax,4                   // eax * 16
                or      al,bl                   // + Current Digit
                inc     edi
                dec     ecx
                jnz     @@1
                jmp     @@OK

              @@Failed:
                stc
                jmp     @@RET
// Integer is in Decimal form
              @@Decimal:
                mov     bl,[edi]
                sub     bl,'0'+10
                add     bl,10
                jnc     @@OK
                shl     eax,1                   // FAST MUL: eax*10
                lea     eax,[eax+eax*4]
                add     eax,ebx                 // + Current digit
                jc      @@Failed
                inc     edi
                dec     ecx
                jnz     @@Decimal

              @@OK:
                dec     esi
                jnz     @@Done
                neg     eax                     // eax := - eax
              @@Done:
                clc
              @@RET:
end;

//±±±±±±±±±±±±±±[ 80X87 BINARY/DECIMAL ROUTINES ]±±±±±±±±±±±±±±±±±±±±±±±

const
  CWNear: Word = (IC_Affine   shl sCW_IC) or    // Affine mode
                 (RC_Nearest  shl sCW_RC) or    // Round to nearest
                 (PC_Extended shl sCW_PC) or    // Round to extended
                                  mCW_PM  or    // Masked
                                  mCW_UM  or    // Masked
                                  mCW_OM  or    // Masked
                                  mCW_ZM  or    // Masked
                                  mCW_DM  or    // Masked
                                  mCW_IM ;      // Masked

const
  LongP10Table: array [0..7] of Longint =
    (1,                 // 0
     10,                // 1
     100,               // 2
     1000,              // 3
     10000,             // 4
     100000,            // 5
     1000000,           // 6
     10000000);         // 7

// Multiplies ST(0) by 10^EAX
// EXPECTS:     eax     = Power of 10
// RETURNS:     ST(0)   = Result

procedure FPower10; assembler; {&USES eax,ebx,edx} {&FRAME-}
const
  ExtendedP10Table: array [0..9] of Extended =
    (1e8,               // 0
     1e16,              // 1
     1e32,              // 2
     1e64,              // 3
     1e128,             // 4
     1e256,             // 5
     1e512,             // 6
     1e1024,            // 7
     1e2048,            // 8
     1e4096);           // 9
asm
                cmp     eax,4096
                jle     @@1
                fld     ExtendedP10Table[9*TYPE Extended].Extended // 1.0e4096
                fmul
                sub     eax,4096
              @@1:
                cmp     eax,-4096
                jge     @@2
                fld     ExtendedP10Table[9*TYPE Extended].Extended // 1.0e4096
                fdiv
                add     eax,4096
              @@2:
                mov     ebx,eax
                test    eax,eax                         // 10^0 = 1, @@Done
                jz      @@Done
                jns     @@3
                neg     eax                             // Make eax positive
              @@3:
                mov     edx,eax
                and     edx,111b
                fild    LongP10Table.Longint[edx*4]
                shr     eax,3
                mov     edx,OFFSET ExtendedP10Table // 1.0e8....
                jmp     @@6
              @@4:
                shr     eax,1
                jnc     @@5
                fld     [edx].Extended
                fmul
              @@5:
                add     edx,TYPE Extended
              @@6:
                test    eax,eax
                jne     @@4
                test    ebx,ebx
                jns     @@7
                fdiv                            // Power < 0 =>ST(0)/Power10
                jmp     @@Done
              @@7:
                fmul                            // Power >= 0 =>ST(0)*Power10
              @@Done:
end;

// Converts float to string
// EXPECTS:     edi     = Offset of the buffer to hold output string
//              ST(0)   = Extended floating point value to convert
//              ecx     = Digit count (Float<0, Fixed>=0
// RETURNS:     ecx     = Output string length
//              edi     = Offset of the buffer (not changed)

procedure Float2Str; assembler; {&USES eax,ebx,edx,esi} {&FRAME+}
var
  Digits,Exponent: Longint;
  Value: Extended;
  CtrlWord: Word;
  Sign: Byte;
  DigitBuf: array[0..19] of Byte;
const
  C1e18: Extended = 1e18;

// Get digit from digit buffer

procedure GetDigit; {&USES None} {&FRAME-}
asm
                mov     al,DigitBuf.Byte[esi]
                inc     esi
                test    al,al
                jnz     @@RET
                mov     al,'0'
                dec     esi
              @@RET:
end;

asm
                fstcw   CtrlWord        // Save x87 control word to the temp var
                fldcw   CWNear          // Load new control word
                fstp    Value           // Save value to the temp variable
                push    edi             // Save buffer pointer to determine later string length
                cmp     ecx,18          // Maximum number of digits = 18
                jle     @@1             // convert digit count to the range of
                mov     ecx,18          // -18 .. +18
              @@1:
                cmp     ecx,-18
                jge     @@2
                mov     ecx,-18
              @@2:
                mov     Digits,ecx      // Number of digits (Float<0, Fixed>=0)
                cld
                fwait                   // Wait for coprocessor
                movzx   eax,Value.ExtRec.ER_Exponent // ax := Exponent & Sign
                mov     Sign,ah
                and     eax,mEXP_Exponent
                jz      @@Zero_Exponent
                cmp     eax,EXP_Spec_Value // Is value special (NAN or infinity) ?
                jne     @@Not_Spec         // if not then normal value
                mov     dx,Value.ExtRec.ER_Significand0
                or      dx,Value.ExtRec.ER_Significand1
                or      dx,Value.ExtRec.ER_Significand2
                jnz     @@NaN // Must be NAN
                cmp     Value.ExtRec.ER_Significand3,SIGN_Inf_Value // Infinity?
                je      @@Infinity
              @@NaN:
                mov     ax,'AN'         // Output 'NAN'
                mov     [edi],ax
                mov     [edi+2],al      // AL already = 'N'
                add     edi,3
                jmp     @@Done

              @@Infinity:
                cmp     Sign,0          // Output 'INF' for +infinity
                jns     @@Plus_Inf      // and   '-INF' for -infinity
                mov     [edi].Byte,'-'
                inc     edi
              @@Plus_Inf:
                mov     [edi].Word,'NI'
                mov     [edi+2].Byte,'F'
                add     edi,3
                jmp     @@Done

              @@Zero_Exponent:
                mov     Exponent,eax    // eax = 0
                mov     DigitBuf.Byte,al
                jmp     @@Make_String

              @@Not_Spec:
                mov     Value.ExtRec.ER_Exponent,ax // Clear sign bit
                fld     Value           // Load positive value
                sub     ax,3FFFh        // Obtain signed binary exponent
                mov     dx,19728
                imul    dx              // 2^X = 10^Y => Y=LN(2)*X/LN(10)
                movsx   edx,dx
                mov     Exponent,edx    // 19728,301 =LN(2)*65536/LN(10)
                sub     edx,17
                neg     edx
                mov     eax,edx
                Call    FPower10
                frndint
                fld     C1e18           // ST(0) ? 1.0e18
                fcomp
                fnstsw  ax
                test    ah,(mSW_C0+mSW_C3) shr 8
                jz      @@Below_1e18    // If Significand >= 1.0e18 then
                fidiv   LongP10Table[1*4].Longint // significand := significand /10
                inc     Exponent        // Inc(Exponent)
              @@Below_1e18:
                fbstp   Value           //  Store in BCD form
                lea     ebx,DigitBuf
                mov     esi,9           // Packed decimal: 9 bytes(72 bits)
                fwait
              @@3:
                mov     al,Value[esi-1].Byte // Get two nibbles
                mov     ah,al
                shr     al,4            // Put them: one to AL and the other
                and     ah,0Fh          // to AH
                add     ax,'00'         // Convert them to ASCII form
                mov     [ebx],ax
                add     ebx,2
                dec     esi
                jnz     @@3
                mov     [ebx].Byte,0    // Terminate with '\0'

                cmp     Digits,0        // If Fixed point and
                jl      @@4
                cmp     Exponent,36     // exponent => 36 then display
                jl      @@4
                mov     Digits,-18      // as floating point with 18 digits
              @@4:
                mov     esi,Digits
                test    esi,esi
                js      @@Float
                add     esi,Exponent
                inc     esi
                jns     @@5
                mov     DigitBuf.Byte,0
                jmp     @@Make_String

              @@Float:
                neg     esi
              @@5:
                cmp     esi,18                  // if > 18 then no rounding is needed
                jae     @@Make_String           // else
                cmp     DigitBuf.Byte[esi],'5'  // Round significand to the           }
                mov     DigitBuf.Byte[esi],0    // specified number of digits         }
                jb      @@Make_String           // if last digit < '5' then truncate it
              @@6:
                dec     esi                     // else
                js      @@Rounding_Done
                inc     DigitBuf.Byte[esi]      // Inc(LastDigit)
                cmp     DigitBuf.Byte[esi],'9'  // if digit < 9 then done
                jbe     @@Make_String           // else truncate string
                mov     DigitBuf.Byte[esi],0    // E.g: 0.596 => 0.6
                jmp     @@6

              @@Rounding_Done:
                mov     DigitBuf.Word,'1'
                inc     Exponent

              @@Make_String:
                xor     esi,esi
                mov     edx,Digits
                test    edx,edx
                js      @@As_Float
// ....... Output as fixed point number ............
                cmp     Sign,0
                jns     @@Positive
                mov     [edi].Byte,'-'          // Output '-'
                inc     edi
              @@Positive:
                mov     ecx,Exponent
                test    ecx,ecx
                jns     @@7
                mov     [edi].Byte,'0'
                inc     edi
                jmp     @@Fract_Part

              @@7:
                Call    GetDigit
                mov     [edi],al
                inc     edi
                dec     ecx
                jns     @@7

              @@Fract_Part:
                test    edx,edx
                jz      @@Done
                mov     [edi].Byte,'.'
                inc     edi
              @@8:
                inc     ecx
                jz      @@9
                mov     [edi].Byte,'0'
                inc     edi
                dec     edx
                jnz     @@8
              @@9:
                dec     edx
                js      @@Done
                Call    GetDigit
                mov     [edi],al
                inc     edi
                jmp     @@9
// ........ Output as floating point number .........
              @@As_Float:
                mov     al,' '
                cmp     Sign,0
                jns     @@Pos
                mov     al,'-'
              @@Pos:
                mov     [edi],al
                inc     edi
                Call    GetDigit
                mov     [edi],al
                inc     edi
                inc     edx
                jz      @@Exponent
                mov     [edi].Byte,'.'
                inc     edi
              @@10:
                Call    GetDigit
                mov     [edi],al
                inc     edi
                inc     edx
                jne     @@10

              @@Exponent:
                mov     [edi].Byte,'E'
                inc     edi
                mov     al,'+'
                mov     edx,Exponent
                test    edx,edx
                jns     @@Positive_Exp
                mov     al,'-'
                neg     edx
              @@Positive_Exp:
                mov     [edi],al
                inc     edi
                mov     ah,100
                mov     al,10
                xchg    eax,edx         // Convert Number in eax to the ASCII form
                div     dh              // dh = 100
                mov     dh,ah
                cbw
                div     dl              // dl = 10
                add     ax,'00'
                mov     [edi],ax
                mov     al,dh
                cbw
                div     dl
                add     ax,'00'
                mov     [edi+2],ax
                add     edi,4

              @@Done:
                mov     ecx,edi
                pop     edi             // Pop previous buffer pointer
                sub     ecx,edi         // ecx := Output string length
                fclex                   // clear exceptions (if any)
                fldcw   CtrlWord        // Restore old value of the control word
                fwait
end;

// Converts sequence of digits to float
// EXPECTS:     edi     = @ of the buffer with packed string
//              ecx     = Packed string length
// RETURNS:     ebx     = Digit count

procedure DigitStr; {&USES None} {&FRAME-}
asm
                xor     ebx,ebx                 // Digit Count
                test    ecx,ecx
                jz      @@Done
              @@1:
                mov     al,[edi]                // Get char
                sub     al,'0'+10
                add     al,10                   // Is char in ['0'..'9'] ?
                jnc     @@Done                  // No, @@Done
                fimul   LongP10Table[1*4].Longint // *10
                and     eax,7Fh
                push    eax
                fiadd   Word Ptr [esp]
                pop     eax
                inc     ebx
                inc     edi
                dec     ecx
                jnz     @@1
              @@Done:
end;

Const
  Factor16 : Longint = 16;

procedure HexDigitStr; {&USES None} {&FRAME-}
asm
                test    ecx,ecx
                jz      @@Done
              @@1:
                mov     al,[edi]                // Get char
                cmp     al,'a'                  // Letter ?
                jb      @@2
                sub     al,'a'-'A'              // Yes, Convert to upper case
              @@2:
                sub     al,'0'+10               // Decimal digit 0..9 ?
                add     al,10                   // 10 digits
                jc      @@3                     // Yes
                sub     al,'A'-'9' + 15         // Is it hex letter A..F ?
                add     al,6                    // 6 letters
                jnc     @@Done
                add     al,10
              @@3:
                fimul   Factor16
                and     eax,7Fh
                push    eax
                fiadd   Word Ptr [esp]
                pop     eax
                inc     edi
                dec     ecx
                jnz     @@1
              @@Done:
end;


//±±±±±±±±±±±±±±[ Str2Float ]±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±

// Converts string to float
// EXPECTS:     edi     = @ of the buffer with packed string
//              ecx     = Packed string length
// RETURNS:     ecx     = Number of the remaining characters
//              edi     = @ just after the parsed string
//              ST(0)   = Converted value
//              CF      = 1 if error occurred ( 0  otherwise)

procedure Str2Float; assembler; {&USES eax,ebx,edx} {&FRAME+}
var
  CtrlWord: Word;
  SignChar,ExpoChar: Byte;
asm
                fstcw   CtrlWord        // Save x87 control word
                fclex                   // Clear exceptions
                fldcw   CWNear          // Load new control word
                fldz                    // ST(0) := 0
                test    ecx,ecx         // jecxz cannot access target
                jz      @@Failed
                mov     al,[edi]
                mov     SignChar,al     // Record the sign
                cmp     al,' '
                je      @@1
                cmp     al,'+'
                je      @@1
                cmp     al,'-'
                jne     @@2
              @@1:
                inc     edi
                dec     ecx
                jz      @@Failed
              @@2:
                cmp     [edi].byte,'$'
                jne     @@Decimal

                inc     edi
                dec     ecx
                jz      @@Failed
                call    HexDigitStr
                test    ecx,ecx
                jnz     @@Failed
                jmp     @@6

              @@Decimal:
                mov     edx,ecx
                Call    DigitStr        // Read significand before
                xor     ebx,ebx         // decimal point
                test    ecx,ecx
                jz      @@3
                mov     al,[edi]
                cmp     al,'.'
                jne     @@3
                inc     edi
                dec     ecx
                Call    DigitStr        // after decimal point
                neg     ebx             // ebx = Exponent
              @@3:
                cmp     edx,ecx         // Is anything parsed ?
                je      @@Failed        // No, @@Failed
                test    ecx,ecx
                jz      @@5
                mov     al,[edi]
                cmp     al,'E'          // Parse exponent(if any)
                je      @@4
                cmp     al,'e'
                jne     @@5
              @@4:
                inc     edi
                dec     ecx
                Call    Str2Int         // RETURNS: eax = Exponent
                jc      @@Failed
                cmp     eax,4999
                jge     @@Failed
                cmp     eax,-4999
                jle     @@Failed
                add     ebx,eax
              @@5:
                mov     eax,ebx
                Call    FPower10        // Significand * 10^Exponent
              @@6:
                cmp     SignChar,'-'
                jne     @@7
                fchs
              @@7:
                fnstsw  ax
                test    al,mSW_IE or mSW_OE
                jz      @@OK            // CF = 0
              @@Failed:
                stc
              @@OK:
                fclex                   // Clear exceptions
                fldcw   CtrlWord        // Restore old control word
                fwait
end;

//±±±±±±±±±±±±±±[ STR/VAL INTEGER ROUTINES ]±±±±±±±±±±±±±±±±±±±±±±±±±±±±

// procedure Str(X:[:Width ]; var S: ShortString);

procedure _StrInt(Value,Width: Longint; S: Pointer; SLen: Longint); assembler; {&USES ALL} {&FRAME-}
var
  Buffer: array[0..31] of Byte;
asm
                mov     eax,Value
                lea     edi,Buffer
                mov     esi,edi
                Call    Int2Str
                mov     edi,S
                mov     edx,SLen
                mov     eax,Width
                cmp     eax,edx
                jle     @@1
                mov     eax,edx
              @@1:
                cmp     ecx,edx
                jle     @@2
                mov     ecx,edx
              @@2:
                cmp     eax,ecx
                jge     @@3
                mov     eax,ecx                 // ecx = String Length
              @@3:                              // eax = Field width
                cld
                mov     [edi],al                // String Length
                inc     edi
                sub     eax,ecx
                jz      @@4
                push    ecx
                mov     ecx,eax
                mov     al,' '
                rep     stosb
                pop     ecx
              @@4:
                rep     movsb
end;

// procedure Str(X:[:Width ]; var S: AnsiString);

procedure _StrIntLStr(Value,Width: Longint; var S: Pointer); assembler; {&USES eax} {&FRAME-}
var
  Buffer: ShortString;
asm
                lea     eax,Buffer
                push    Value                   // [1]:Longint = Value
                push    Width[4]                // [2]:Longint = Width
                push    eax                     // [3]:Pointer = @String
                push    255                     // [4]:Longint = MaxLen
                Call    _StrInt
                push    S                       // [1]:Pointer = @Dest LStr
                push    eax                     // [2]:Pointer = Src SStr
                Call    _LStrStr
                pop     eax
end;

// procedure Str(X:[:Width ]; var S: PChar);

procedure _StrIntPCh(Value,Width: Longint; S: Pointer; SLen: Longint); assembler; {&USES ALL} {&FRAME-}
var
  Buffer: array[0..31] of Byte;
asm
                mov     eax,Value
                lea     edi,Buffer
                mov     esi,edi
                Call    Int2Str                 // Convert Integer to String
                mov     edi,S
                mov     edx,SLen
                mov     eax,Width
                cmp     eax,edx
                jle     @@1
                mov     eax,edx
              @@1:
                cmp     ecx,edx
                jle     @@2
                mov     ecx,edx
              @@2:
                cmp     eax,ecx
                jge     @@3
                mov     eax,ecx                 // ecx = String Length
              @@3:                              // eax = Field width
                cld
                sub     eax,ecx
                jz      @@4
                push    ecx
                mov     ecx,eax
                mov     al,' '
                rep     stosb
                pop     ecx
              @@4:
                rep     movsb
                mov     [edi],cl                // Terminate it with #0
end;

// procedure Val(const S: ShortString; var V; var Code: IntegerType);
// RETURNS:      eax   = Integer value

procedure _ValInt(S, Code: Pointer); {&USES ecx,edi} {&FRAME-}
asm
                mov     edi,S
                movzx   ecx,Byte Ptr [edi]      // ecx := String Length
                inc     edi                     // edi := @S[1]
                test    ecx,ecx
                jz      @@2
              @@1:
                cmp     [edi].Byte,' '          // Skip blanks
                jne     @@2
                inc     edi
                dec     ecx
                jnz     @@1
              @@2:
                Call    Str2Int                 // Convert String to Integer
                jc      @@ERROR
                test    ecx,ecx
                jz      @@OK                    // OK, Error position = 0
              @@ERROR:
                mov     ecx,edi
                sub     ecx,S                   // ecx := Error position
                xor     eax,eax                 // Return 0
              @@OK:
                mov     edi,Code
                mov     [edi],ecx
end;

// procedure Val(const S: [PChar | AnsiString]; var V; var Code: IntegerType);
// RETURNS:     eax     = Integer value
//              edx     = Error position

procedure _ValIntPCh(S, Code: Pointer); {&USES ecx,edi} {&FRAME-}
asm
                mov     edi,S
                test    edi,edi
                jz      @@ERROR
                or      ecx,-1                  // ecx := -1
                mov     al,' '
                cld
                repe    scasb                   // Skip blanks
                dec     edi
                or      ecx,-1                  // Calculate string length
                mov     al,0
                repne   scasb
                not     ecx
                sub     edi,ecx                 // edi := String offset
                dec     ecx                     // ecx := Length of the string
                Call    Str2Int                 // Convert string to float
                jc      @@ERROR                 // Result in ST(0)
                test    ecx,ecx
                jz      @@OK                    // OK, Error position = 0
              @@ERROR:
                mov     ecx,edi                 // Pop out invalid value
                sub     ecx,S                   // and return 0
                inc     ecx                     // ecx := Error position
                xor     eax,eax
              @@OK:
                mov     edi,Code
                mov     [edi],ecx
end;

//±±±±±±±±±±±±±±[ STR/VAL FLOATING POINT ROUTINES ]±±±±±±±±±±±±±±±±±±±±±

// procedure Str(X:[:Width [:Decimals ]]; var S: ShortString);
// EXPECTS:     ST(0)   = Floating point value

procedure _StrFlt(Width,Dec: Longint; S: Pointer; SLen: Longint); assembler; {&USES ALL} {&FRAME-}
var
  Buffer: array[0..63] of Byte;
asm
                mov     ecx,Dec
                test    ecx,ecx                 // Setup parameters
                jns     @@0
                mov     ecx,8
                sub     ecx,Width
                cmp     ecx,-2
                jle     @@0
                mov     ecx,-2
              @@0:
                lea     edi,Buffer
                Call    Float2Str               // Convert float to string
                mov     esi,edi
                mov     edi,S
                mov     edx,SLen
                mov     eax,Width
                cmp     eax,edx
                jle     @@1
                mov     eax,edx
              @@1:
                cmp     ecx,edx
                jle     @@2
                mov     ecx,edx
              @@2:
                cmp     eax,ecx
                jge     @@3
                mov     eax,ecx                 // ecx = String Length
              @@3:                              // eax = Field width
                cld
                mov     [edi],al                // String Length
                inc     edi
                sub     eax,ecx
                jz      @@4
                push    ecx
                mov     ecx,eax
                mov     edx,eax
                shr     ecx,2
                and     dl,11b
                mov     eax,'    '              // FAST STOS
                rep     stosd                   // Right-justify output string
                mov     cl,dl
                rep     stosb
                pop     ecx
              @@4:
                mov     eax,ecx                 // Copy string itself
                shr     ecx,2
                and     al,11b
                rep     movsd                   // FAST MOVS
                mov     cl,al
                rep     movsb
end;

// procedure Str(X:[:Width [:Decimals ]]; var S: AnsiString);

procedure _StrFltLStr(Width,Dec: Longint; var S: Pointer); assembler; {&USES eax} {&FRAME-}
var
  Buffer: ShortString;
asm
                lea     eax,Buffer
                push    Width                   // [1]:Longint = Width
                push    Dec[4]                  // [2]:Longint = Dec
                push    eax                     // [3]:Pointer = @String
                push    255                     // [4]:Longint = MaxLen
                Call    _StrFlt
                push    S                       // [1]:Pointer = @Dest LStr
                push    eax                     // [2]:Pointer = Src SStr
                Call    _LStrStr
                pop     eax
end;

// procedure Str(X:[:Width [:Decimals ]]; var S: PChar);
// EXPECTS:      ST(0) = Floating point value

procedure _StrFltPCh(Width,Dec: Longint; S: Pointer; SLen: Longint); assembler; {&USES ALL} {&FRAME-}
var
  Buffer: array[0..63] of Byte;
asm
                mov     ecx,Dec
                test    ecx,ecx                 // Setup parameters
                jns     @@0
                mov     ecx,8
                sub     ecx,Width
                cmp     ecx,-2
                jle     @@0
                mov     ecx,-2
              @@0:
                lea     edi,Buffer
                Call    Float2Str               // Convert float to string
                mov     esi,edi
                mov     edi,S
                mov     edx,SLen
                mov     eax,Width
                cmp     eax,edx
                jle     @@1
                mov     eax,edx
              @@1:
                cmp     ecx,edx
                jle     @@2
                mov     ecx,edx
              @@2:
                cmp     eax,ecx
                jge     @@3
                mov     eax,ecx                 // ecx = String Length
              @@3:                              // eax = Field width
                cld
                sub     eax,ecx
                jz      @@4
                push    ecx
                mov     ecx,eax
                mov     edx,eax
                shr     ecx,2
                and     dl,11b
                mov     eax,'    '              // FAST STOS
                rep     stosd                   // Right-justify output string
                mov     cl,dl
                rep     stosb
                pop     ecx
              @@4:
                mov     eax,ecx                 // Copy string itself
                shr     ecx,2
                and     al,11b
                rep     movsd                   // FAST MOVS
                mov     cl,al
                rep     movsb
                mov     [edi],cl                // Terminate it with #0
end;

// procedure Val(const S: ShortString; var V; var Code: IntegerType);
// RETURNS:     ST(0) = Floating point value

procedure _ValFlt(S, Code: Pointer); {&USES ecx,edi} {&FRAME-}
asm
                mov     edi,S
                movzx   ecx,Byte Ptr [edi]      // ecx := String Length
                inc     edi                     // edi := @S[1]
                test    ecx,ecx
                jz      @@2
              @@1:
                cmp     [edi].Byte,' '          // Skip blanks
                jne     @@2
                inc     edi
                dec     ecx
                jnz     @@1
              @@2:
                Call    Str2Float               // Convert string to float
                jc      @@ERROR                 // Result in ST(0)
                test    ecx,ecx
                jz      @@OK                    // OK, Error position = 0
              @@ERROR:
                fstp    st(0)                   // Pop out invalid value
                fldz                            // and return 0
                mov     ecx,edi                 // ecx := Error position
                sub     ecx,S
                fwait                           // Wait for result
              @@OK:
                mov     edi,Code
                mov     [edi],ecx
end;

// procedure Val(const S:[PChar | AnsiString]; var V; var Code: IntegerType);
// RETURNS:     ST(0)   = Floating point value

procedure _ValFltPCh(S,Code: Pointer); {&USES eax,ecx,edi} {&FRAME-}
asm
                mov     edi,S
                test    edi,edi
                jz      @@Fail
                or      ecx,-1                  // ecx := -1
                mov     al,' '
                cld                             // Skip Blanks
                repe    scasb
                dec     edi
                or      ecx,-1                  // Calculate string length
                mov     al,0
                repne   scasb
                not     ecx
                sub     edi,ecx                 // edi := String offset
                dec     ecx                     // ecx := Length of the string
                Call    Str2Float               // Convert string to float
                jc      @@ERROR                 // Result in ST(0)
                test    ecx,ecx
                jz      @@OK                    // OK, Error position = 0
              @@ERROR:
                fstp    st(0)                   // Pop out invalid value
              @@Fail:
                fldz                            // and return 0
                mov     ecx,edi                 // ecx := Error position
                sub     ecx,S
                inc     ecx
                fwait                           // Wait for result
              @@OK:
                mov     edi,Code
                mov     [edi],ecx
end;

//±±±±±±±±±±±±±±[ MEMORY BLOCK OPERATIONS ]±±±±±±±±±±±±±±±±±±±±±±±±±±±±±

// procedure Move(var Source, Dest, Count: LongInt);
// Important! Memory regions may overlap

procedure _MemMove(Src,Dest: Pointer; Count: Longint); {&USES eax,ecx,esi,edi} {&FRAME-}
asm
                mov     esi,Src
                mov     edi,Dest
                mov     ecx,Count
                test    ecx,ecx
                jz      @@RET
                cmp     esi,edi
                jae     @@Forward

                // esi<edi, but is esi+ecx>edi -- Src and Dest overlaps?
                lea     eax,[esi+ecx]
                cmp     eax,edi
                jbe     @@Forward

                // Move backwards
                std
                add     esi,ecx
                add     edi,ecx
                mov     eax,ecx
                and     ecx,11b
                dec     esi
                dec     edi
                rep     movsb
                mov     ecx,eax
                shr     ecx,2
                jz      @@RET
                sub     esi,3
                sub     edi,3
                rep     movsd
                jmp     @@RET

                // Move forward
              @@Forward:
                cld
                // Make sure data is well aligned
              @@Align:
                test    edi,3
                jz      @@Aligned
                movsb
                dec     ecx
                jz      @@RET
                jmp     @@Align

              @@Aligned:
                mov     eax,ecx
                shr     ecx,2
                and     al,11b
                rep     movsd
                mov     cl,al
                rep     movsb
              @@RET:
                cld
end;

// FillChar standard procedure
// procedure FillChar(var Dest; Count: LongInt, Value);

procedure _MemFill(Dest: Pointer; Count: Longint; Value: Byte); {&USES eax,ecx,edi} {&FRAME-}
asm
                cld
                mov     al,Value                // Fill all bytes of the eax
                mov     ah,al                   // with Value byte
                mov     ecx,eax
                shl     eax,16
                mov     ax,cx
                mov     edi,Dest
                mov     ecx,Count
                push    ecx
                shr     ecx,2
                rep     stosd
                pop     ecx
                and     ecx,11b
                rep     stosb
end;

// Support routine for structured variable assignments
// procedure _VarMove(var Dest; Count: LongInt, Value);
// Important! Memory regions must NOT overlap

procedure _VarMove(Src,Dest: Pointer; Count: Longint); {&USES ecx,esi,edi} {&FRAME-}
asm
                cld
                mov     esi,Src
                mov     edi,Dest
                mov     ecx,Count
                push    ecx
                shr     ecx,2
                rep     movsd
                pop     ecx
                and     ecx,11b
                rep     movsb
end;

procedure _VarMoveInit(Src,Dest: Pointer; Count: Longint; RTTI: Pointer); {&USES None} {&FRAME-}
asm
                push    Src  [0]
                push    Dest [4]
                push    Count[8]
                Call    _VarMove
                push    Dest [0]
                push    RTTI [4]
                Call    _MemAddRef
end;

//--------------[ INITIALIZATION/FINALIZATION ]-------------------------------

// Initialize standard procedure without Count
// procedure Initialize(var V);

procedure _MemInit(P,TypeInfo: Pointer); {&USES None} {&FRAME-}
asm
                pop     eax             // Return address
                push    1               // [3]:Longint = Count
                push    eax
                jmp     _MemInitCnt
                PopArgs 0
end;

// Initialize standard procedure with Count optional parameter
// procedure Initialize(var V; Count: Longint);

procedure _MemInitCnt(P,TypeInfo: Pointer; Count: Longint); {&USES ebx,ecx,edx,esi} {&FRAME-}
asm
                mov     ebx,P
                mov     esi,TypeInfo
                mov      al,[esi].TTypeInfo.Kind
                movzx   ecx,[esi].TTypeInfo.Name.Byte
                lea     esi,[esi+ecx].TTypeInfo.Name[1]
                cmp     al,tkLString
                je      @@LString
                cmp     al,tkArray
                je      @@Array
                cmp     al,tkRecord
                je      @@Record
                mov     al,reInvalidPtr
                add     esp,@Uses
                jmp     RtlError
// Long String
              @@LString:
                xor     eax,eax
                mov     ecx,Count
              @@1:
                mov     [ebx],eax
                add     ebx,4
                dec     ecx
                jg      @@1
                jmp     @@RET
// Array
              @@Array:
                push    ebx                         // [1]:Pointer = @Variable
                add     ebx,[esi].TTypeData.ArrSize
                push    [esi].TTypeData.ElemRTTI    // [2]:Pointer = RTTI
                push    [esi].TTypeData.ElemCount   // [3]:Longint = Count
                Call    _MemInitCnt
                dec     Count
                jg      @@Array
                jmp     @@RET
// Record
              @@Record:
                mov     eax,ebx
                add     eax,[esi].TTypeData.RecSize
                push    eax
                mov     ecx,[esi].TTypeData.RecData.FieldCount
                lea     edx,[esi].TTypeData.RecData.FieldTable
              @@2:
                mov     eax,ebx
                add     eax,[edx].TFieldRec.&Offset
                push    eax                     // [1]:Pointer = @Variable
                push    [edx].TFieldRec.TypeInfo// [2]:Pointer = RTTI
                Call    _MemInit
                add     edx,TYPE TFieldRec
                dec     ecx
                jg      @@2
                pop     ebx
                dec     Count
                jg      @@Record
              @@RET:
end;

// Finalize standard procedure without Count
// procedure Initialize(var V);

procedure _MemFin(P,TypeInfo: Pointer); {&USES None} {&FRAME-}
asm
                pop     eax             // Return address
                push    1               // [3]:Longint = Count
                push    eax
                jmp     _MemFinCnt
                PopArgs 0
end;

// Finalizes the fields of a record, object or class
// EXPECTS:     ebx     = @Memory
//              edx     = Record type data

procedure _MemFinRec; {&USES ecx} {&FRAME-}
asm
                mov     ecx,[edx].TRecType.FieldCount
                add     edx,TRecType.FieldTable
              @@1:
                mov     eax,ebx
                add     eax,[edx].TFieldRec.&Offset
                push    eax                     // [1]:Pointer = @Variable
                push    [edx].TFieldRec.TypeInfo// [2]:Pointer = RTTI
                Call    _MemFin
                add     edx,TYPE TFieldRec
                dec     ecx
                jg      @@1
end;

// Initialize standard procedure with Count optional parameter
// procedure Initialize(var V; Count: Longint);

procedure _MemFinCnt(P,TypeInfo: Pointer; Count: Longint); {&USES ebx,ecx,edx,esi} {&FRAME-}
asm
                mov     ebx,P
                mov     esi,TypeInfo
                mov      al,[esi].TTypeInfo.Kind
                movzx   ecx,[esi].TTypeInfo.Name.Byte
                lea     esi,[esi+ecx].TTypeInfo.Name[1]
                cmp     al,tkLString
                je      @@LString
                cmp     al,tkArray
                je      @@Array
                cmp     al,tkRecord
                je      @@Record
                mov     al,reInvalidPtr
                add     esp,@Uses
                jmp     RtlError
// Long String
              @@LString:
                push    ebx                     // [1]:Pointer = @LStr
                Call    _LStrClr
                add     ebx,4
                dec     Count
                jg      @@LString
                jmp     @@RET
// Array
              @@Array:
                push    ebx                         // [1]:Pointer = @Variable
                add     ebx,[esi].TTypeData.ArrSize
                push    [esi].TTypeData.ElemRTTI    // [2]:Pointer = RTTI
                push    [esi].TTypeData.ElemCount   // [3]:Longint = Count
                Call    _MemFinCnt
                dec     Count
                jg      @@Array
                jmp     @@RET
// Record
              @@Record:
                mov     eax,ebx
                add     eax,[esi].TTypeData.RecSize
                push    eax
                lea     edx,[esi].TTypeData.RecData
                Call    _MemFinRec
                pop     ebx
                dec     Count
                jg      @@Record
              @@RET:
end;

procedure _MemAddRefCnt(P,TypeInfo: Pointer; Count: Longint); {&USES ebx,ecx,edx,esi} {&FRAME-}
asm
                mov     ebx,P
                mov     esi,TypeInfo
                mov      al,[esi].TTypeInfo.Kind
                movzx   ecx,[esi].TTypeInfo.Name.Byte
                lea     esi,[esi+ecx].TTypeInfo.Name[1]
                cmp     al,tkLString
                je      @@LString
                cmp     al,tkArray
                je      @@Array
                cmp     al,tkRecord
                je      @@Record
                mov     al,reInvalidPtr
                add     esp,@Uses
                jmp     RtlError
// Long String
              @@LString:
                push    [ebx].Longint               // [1]:Pointer = @LStr
                Call    _LStrAddRef
                add     ebx,4
                dec     Count
                jg      @@LString
                jmp     @@RET
// Array
              @@Array:
                push    ebx                         // [1]:Pointer = @Variable
                add     ebx,[esi].TTypeData.ArrSize
                push    [esi].TTypeData.ElemRTTI    // [2]:Pointer = RTTI
                push    [esi].TTypeData.ElemCount   // [3]:Longint = Count
                Call    _MemAddRefCnt
                dec     Count
                jg      @@Array
                jmp     @@RET
// Record
              @@Record:
                mov     eax,ebx
                add     eax,[esi].TTypeData.RecSize
                push    eax
                mov     ecx,[esi].TTypeData.RecData.FieldCount
                lea     edx,[esi].TTypeData.RecData.FieldTable
              @@2:
                mov     eax,ebx
                add     eax,[edx].TFieldRec.&Offset
                push    eax                     // [1]:Pointer = @Variable
                push    [edx].TFieldRec.TypeInfo// [2]:Pointer = RTTI
                Call    _MemAddRef
                add     edx,TYPE TFieldRec
                dec     ecx
                jg      @@2
                pop     ebx
                dec     Count
                jg      @@Record
              @@RET:
end;

// Adds references to all long string fields of the specified variable.

procedure _MemAddRef(P,TypeInfo: Pointer); {&USES None} {&FRAME-}
asm
                pop     eax             // Return address
                push    1               // [3]:Longint = Count
                push    eax
                jmp     _MemAddRefCnt
                PopArgs 0
end;

// New standard procedure. Pointer to the type that needs initialization is used

procedure _MemNewInit(Size: Longint; TypeInfo: Pointer); {&USES None} {&Frame-}
asm
                push    Size
                Call    _MemNew
                test    eax,eax
                jz      @@RET
                push    eax
                push    eax             // [1]:Pointer = @Memory
                push    TypeInfo[8]     // [2]:Pointer = RTTI
                Call    _MemInit
                pop     eax
              @@RET:
end;

// Dispose standard procedure. Pointer to the type that needs finalization is used

procedure _MemFreeFin(P,TypeInfo: Pointer); {&USES None} {&FRAME-}
asm
                push    P               // [1]:Pointer = @Memory
                push    TypeInfo[4]     // [2]:Pointer = RTTI
                Call    _MemFin
                push    P               // [1]:Pointer = Memory
                Call    _MemFree
end;

type
  TParamInit = record
    itFlag: Byte;
    itOfs:  Longint;
    itRTTI: Longint;
  end;

// Initializes the subprogram memory: both parameters and local variables
// The values of the flag field are as follows:
//      0: END
//      1: value parameter that must be copied
//      2: value parameter
//    $80: function result (no finalization is needed)

procedure _MemLocInit(Data,Handler: Pointer); {&USES None} {&FRAME-}
asm
                push    ecx
                push    edx
                mov     edx,ebp
                xchg    edx,Data[8]             // XCPT[1] = EBP
              @@1:
                mov     al,[edx].TParamInit.itFlag
                and     al,7Fh
                cmp     al,1
                jb      @@RET                   // 0: END
                mov     ecx,OFFSET _MemAddRef
                mov     eax,[edx].TParamInit.itOfs
                ja      @@2
                mov     eax,[eax+ebp]           // 1: Value parameter that is copied
                test    [edx].TParamInit.itFlag,80h
                jz      @@4
                mov     ecx,OFFSET _MemFin
                jmp     @@4
              @@2:
                test    eax,eax
                lea     eax,[eax+ebp]
                jg      @@4
                mov     ecx,OFFSET _MemInit
              @@4:
                push    eax                     // [1]:Pointer = @Memory
                push    [edx].TParamInit.itRTTI // [2]:Pointer = RTTI
                Call    ecx
                add     edx,TYPE TParamInit
                jmp     @@1
              @@RET:
                pop     edx
                pop     ecx
                pop     eax                     // RET@
                push    fs:[0].Longint          // XCPT[3] = NextRec
                mov     fs:[0].Longint,esp
                jmp     eax
                PopArgs 0
end;

procedure _MemLocFin(Data: Pointer); {&USES eax,edx} {&FRAME-}
asm
                mov     edx,Data
              @@1:
                mov     al,[edx].TParamInit.itFlag
                test    al,80h                  // Do not finalize function result
                jnz     @@3
                and     al,7Fh
                cmp     al,1
                jb      @@RET
                mov     eax,[edx].TParamInit.itOfs
                lea     eax,[eax+ebp]
                ja      @@2
                mov     eax,[eax]               // 1: Address is on stack
              @@2:
                push    eax                     // [1]:Pointer = @Memory
                push    [edx].TParamInit.itRTTI // [2]:Pointer = RTTI
                Call    _MemFin
              @@3:
                add     edx,TYPE TParamInit
                jmp     @@1
              @@RET:
end;

//±±±±±±±±±±±±±±[ OBJECT HANDLING ROUTINES ]±±±±±±±±±±±±±±±±±±±±±±±±±±±±

// _DmtCall: Calls dynamic method
// EXPECTS:  eax         = Object's VMT address
//           [1]:Longint = Dynamic Index of the method
// Important!: Changes only value of eax

// Stack Layout:
//   ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿    ÚÄÄÄÄÄÄÄÄÄÄÄ¿    ÚÄÄÄÄÄÄÄÄÄÄÄ¿
//   ³   Arguments   ³    ³ Arguments ³    ³ Arguments ³
//   ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´    ÃÄÄÄÄÄÄÄÄÄÄÄ´    ÃÄÄÄÄÄÄÄÄÄÄÄ´
//   ³   DynIndex    ³ -> ³ Return @  ³ -> ³ Return @  ³
//   ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´    ÃÄÄÄÄÄÄÄÄÄÄÄ´    ÀÄÄÄÄÄÄÄÄÄÄÄÙ
//   ³   Return @    ³    ³ Return @  ³
//   ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ    ÀÄÄÄÄÄÄÄÄÄÄÄÙ

procedure _DmtCall(DynIndex: Longint); {&USES None} {&FRAME-}
asm
                push    edi
                push    edx                        // eax = VMT@
                mov     edx,[eax].VMT.DMTPointer   // edx := DMT@
                mov     eax,[esp+8]                // Swap Return@
                xchg    eax,DynIndex[8]            // & DynIndex
                mov     edi,[edx].DMT.Cache_Entry  // Cache method offset
                cmp     eax,[edx].DMT.Cache_Index  // Is it last method used?
                je      @@Done                     // Yes, Done
                push    ebx
                push    ecx
                push    edx                        // Original DMT@
                cld
              @@1:
                mov     ecx,[edx].DMT.Entry_Count
                mov     ebx,ecx                    // ebx := Dynamic Method#
                lea     edi,[edx].DMT.Entry_Table  // eax = Dynamic Index
                repne   scasd                      // Is such index found ?
                je      @@Found                    // Yes, OK
                mov     edx,[edx].DMT.Parent       // No, search in parent DMT
                test    edx,edx
                jne     @@1                        // Not found, error
                add     esp,4*(5+1)                // 5 regs, former RET@
                pop     eax
                push    RTE_Object_Not_Initialized // [1]: Error Code
                push    eax                        // Return address
                jmp     _RunError
              @@Found:
                mov     edi,[edi+ebx*4-4]
                pop     edx                        // Original DMT@
                mov     [edx].DMT.Cache_Index,eax
                mov     [edx].DMT.Cache_Entry,edi
                pop     ecx
                pop     ebx
              @@Done:
                mov     eax,edi
                pop     edx
                pop     edi
                add     esp,4*1                    // Pop out former Ret@
                jmp     eax
end;

const
  SelfOfs = $08;
  VmtOfs  = $0C;

// _ObjCtr: Constructor support routine
// _ObjDtr: Destructor support routine
// EXPECTS:     ebp     = Constructor/Destructor EBP
// RETURNS:     ZF      = 1 if failed (constructor only)

procedure _ObjCtr(VmtPtr: Longint); {&USES eax,ecx,edx} {&FRAME-}
asm
                mov     ecx,[ebp].VmtOfs        // VMT=0 while qualified or
                cmp     ecx,1                   // inherited constructor call
                jb      @@RET                   // Don't init object (ZF = 0)
                mov     eax,[ebp].SelfOfs       // else VMT = VMT offset
                test    eax,eax                 // Self = nil?
                jz      @@GetMemory             // Yes, allocate
                mov     [ebp].VmtOfs.Longint,0  // No deallocation on Fail
                jmp     @@StoreLink             // (VMT := 0)

              @@GetMemory:
                push    [ecx].VMT.InstanceSize  // Memory Size to allocate
                Call    _MemNew                 // Allocate dynamic object
              @@OK:
                test    eax,eax                 // Out of memory?
                jz      @@RET                   // Yes, exit with ZF = 1
                mov     [ebp].SelfOfs,eax       // Store in Self pointer
                mov     edx,VmtPtr
                mov     [eax+edx].Longint,0     // Set VMT to nil

              @@StoreLink:
                add     eax,VmtPtr              // VMT Ptr offset within object
// The following 3 lines fix bug #81, but also breaks code where a
// static object is constructed.  This happens because the stack is not
// cleared so the VMT starts off as a random value :(
//              mov     edx,[eax]               // Read VMT pointer
//              test    edx,edx                 // Is it already set?
//              jnz     @@RET
                mov     [eax],ecx               // Store VMT link in object
                test    esp,esp                 // Exit with ZF=0
              @@RET:
end;

procedure _ObjDtr; {&USES None} {&FRAME-}
asm
                cmp     [ebp].VmtOfs.Longint,0  // Inherited call?
                je      @@OK                    // Yes, skip
                push    [ebp].SelfOfs.Longint   // Extended syntax of Dispose
                Call    _MemFree                // Dispose dynamic object(Self)
              @@OK:
                and     [ebp].SelfOfs.Longint,0 // Self := nil
end;

// Object assignment support routine

procedure _ObjCopy(Src,Dest: Pointer; VmtPtr: Longint); {&USES ebx,ecx,esi,edi} {&FRAME-}
asm
                cld
                mov     esi,Src
                mov     edi,Dest
                mov     ebx,VmtPtr
                add     ebx,edi
                mov     ecx,[ebx]                       // ecx := VMT offset from Dest
                push    ecx                             // Save it
                mov     ecx,[ecx].VMT.InstanceSize      // Get Dest Size
                push    ecx                             // Copy object
                shr     ecx,2
                rep     movsd                           // FAST MOVS
                pop     ecx
                and     ecx,11b
                rep     movsb
                pop     [ebx].Longint                   // Restore Dest VMT offset
end;

procedure _ObjCopyInit(Src,Dest: Pointer; VmtPtr: Longint; RTTI: Pointer); {&USES None} {&FRAME-}
asm
                push    Src   [0]
                push    Dest  [4]
                push    VmtPtr[8]
                Call    _ObjCopy
                push    Dest  [0]
                push    RTTI  [4]
                Call    _MemAddRef
end;

// Checks VMT ptr offset within object instance

procedure _ObjChk(VmtPtr: Longint); {&USES eax,ecx} {&FRAME-}
asm
                mov     eax,VmtPtr
                test    eax,eax
                jz      @@Error
                mov     ecx,[eax].VMT.InstanceSize      // if Size = 0
                test    ecx,ecx
                jz      @@ERROR                         // or
                add     ecx,[eax].VMT.InstanceCheck     // Size + Negative Size
                jz      @@OK                            // <> 0 then Error
              @@Error:
                add     esp,@Uses                       // Remove used registers
                pop     eax
                push    RTE_Object_Not_Initialized
                push    eax                             // [1]: Error Code
                jmp     _RunError                       // Return address
              @@OK:
end;

//±±±±±±±±±±±±±±[ CLASS SUPPORT ROUTINES ]±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±

// EXPECTS:     eax     = Dynamic method index
//              ecx     = @VMT
// RETURNS:     ZF      = 1 if found
//              eax     = Method entry point address

procedure GetDynaMethod(Index,Self: Longint); {&USES ebx,ecx,edx,edi} {&FRAME-}
asm
                cld
                mov     eax,Index
                mov     ecx,Self
              @@1:
                mov     edi,[ecx].vtDynamicTable
                mov     edx,ecx
                test    edi,edi
                je      @@2
                mov     ecx,[edi].TDynamicTable.Count
                mov     ebx,ecx
                add     edi,TDynamicTable.Indices
                repne   scasd
                je      @@3
              @@2:
                mov     ecx,[edx].vtParent
                cmp     ecx,1
                jb      @@RET                   { ZF = 0 }
                jmp     @@1
              @@3:
                mov     eax,[edi+ebx*4-4]       { ZF = 1 }
              @@RET:
end;

procedure _ClsCtr; {&USES None} {&FRAME-}
asm
                cmp     [ebp+VmtOfs].Longint,0          // Inherited call ?
                jz      @@RET
                sub     esp,TYPE TExcFrame
                push    eax
                push    ecx
                push    edx
                mov     eax,[esp+4*3][TYPE TExcFrame]   // Move return@
                mov     [esp+4*3],eax
                mov     eax,[ebp+SelfOfs]               // VMT Ptr
                push    eax                             // [1]:Self = VMT@
                Call    DWord Ptr [eax].vtNewInstance
                mov     [ebp+SelfOfs],eax               // Self
                lea     edx,[esp+4*4]
                mov     ecx,fs:[0]
                mov     [edx].TExcFrame.Next,ecx
                mov     [edx].TExcFrame.hEBP,ebp
                mov     [edx].TExcFrame.Desc,OFFSET @@Desc
                mov     [edx].TExcFrame.ConstructedObject,eax
                mov     fs:[0],edx
                pop     edx
                pop     ecx
                pop     eax
              @@RET:
                ret
              @@Desc:
                jmp     _XcptAny
//              Destroy the object
                mov     eax,[esp+8+9*4]         // Registration[9*4]
                push    [eax].TExcFrame.ConstructedObject
                Call    TObject.Free
// Re-raise the exception
                Call    _XcptRaiseAg
end;

procedure _ClsDtr; {&USES None} {&FRAME-}
asm
                cmp     [ebp+VmtOfs].Longint,0  // Inherited call ?
                je      @@RET
                push    eax
                push    ecx
                push    edx
                mov     eax,[ebp+SelfOfs]
                push    eax
                mov     eax,[eax].clVTable
                Call    DWord Ptr [eax].vtFreeInstance
                pop     edx
                pop     ecx
                pop     eax
                and     [ebp].SelfOfs.Longint,0 // Self := nil
              @@RET:
end;

// Abstruct method handler

procedure _Abstract; {&USES None} {&FRAME-}
asm
                pop     eax
                push    RTE_Object_Not_Initialized
                push    eax
                jmp     _RunError
end;

// EXPECTS:     eax     = Instance pointer

procedure _ClsCallDynInst(Self,Index: Longint); {&USES None} {&FRAME-}
asm
                pop     eax                     // Return@
                xchg    eax,[esp]
                push    eax                     // Index
                mov     eax,Self
                push    [eax].clVTable.Longint  // VMT@
                Call    GetDynaMethod
                jne     _Abstract
                jmp     eax
end;

procedure _ClsCallDynCls(Self,Index: Longint); {&USES None} {&FRAME-}
asm
                pop     eax                     // Return@
                xchg    eax,[esp]
                push    eax                     // Index
                push    Self                    // VMT@
                Call    GetDynaMethod
                jne     _Abstract
                jmp     eax
end;

procedure _ClsFindDynInst(Self,Index: Longint); {&USES None} {&FRAME-}
asm
                mov     eax,Self
                push    Index                   // Index
                push    [eax].clVTable.Longint  // Vmt@
                Call    GetDynaMethod
                jne     _Abstract
end;

procedure _ClsFindDynCls(Self,Index: Longint); {&USES None} {&FRAME-}
asm
                push    Index                   // Index
                push    Self[4]                 // Vmt@
                Call    GetDynaMethod
                jne     _Abstract
end;

// 'IS' class operator
// EXPECTS:     [1]:DWord = Left operand (class)
//              [2]:DWord = Right operand (class reference)
// RETURNS:     al        = Boolean result

procedure _ClsIs(AClass,VMT: Pointer); {&USES ecx} {&FRAME-}
asm
                mov     eax,AClass
                test    eax,eax
                jz      @@RET
                mov     ecx,VMT
                mov     eax,[eax].clVTable
              @@1:
                cmp     eax,ecx
                je      @@2
                mov     eax,[eax].vtParent
                test    eax,eax
                jnz     @@1
                jmp     @@RET
              @@2:
                mov     al,1
              @@RET:
end;

// 'AS' class operator
// EXPECTS:     [1]:DWord = Left operand (class)
//              [2]:DWord = Right operand (class reference)
// RETURNS:     eax       = Left operand if left is derived from right, else error

procedure _ClsAs(AClass,VMT: Pointer); {&USES ecx,edx} {&FRAME-}
asm
                mov     eax,AClass
                test    eax,eax
                jz      @@RET
                mov     edx,VMT
                mov     ecx,[eax].clVTable
              @@1:
                cmp     ecx,edx
                je      @@RET
                mov     ecx,[ecx].vtParent
                test    ecx,ecx
                jnz     @@1
                add     esp,@Uses
                mov     al,reInvalidCast
                jmp     RtlError
              @@RET:
end;

//±±±±±±±±±±±±±±[ TOBJECT ]±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±

constructor TObject.Create;
begin
end;

destructor TObject.Destroy;
begin
end;

procedure TObject.CleanupInstance; {&USES ebx} {&FRAME-}
asm
                mov     ebx,Self                // ebx = @Memory
                mov     ecx,[ebx]
              @@1:
                mov     edx,[ecx].vtInitTable   // edx = RTTI
                mov     ecx,[ecx].vtParent
                test    edx,edx
                jz      @@2
                Call    _MemFinRec
              @@2:
                test    ecx,ecx
                jnz     @@1
end;

procedure TObject.DefaultHandler(var Message);
begin
end;

procedure TObject.Free; {&USES None} {&FRAME-}
asm
                mov     ecx,Self
                test    ecx,ecx
                jz      @@RET
                push    1
                push    ecx
                mov     eax,[ecx]
                Call    DWord Ptr [eax].vtDestroy
              @@RET:
end;

class function TObject.NewInstance: TObject; {&USES edi} {&Frame-}
asm
                mov     ecx,Self
                mov     edx,[ecx].vtInstanceSize
                push    edx
                Call    _MemNew
                push    eax
                cld
                mov     edi,eax
                mov     eax,ecx
                mov     [edi],eax                       { VMT pointer  }
                add     edi,4
                mov     ecx,edx
                xor     eax,eax                         { Clear object }
                shr     ecx,2
                and     dl,11b
                dec     ecx
                rep     stosd
                mov     cl,dl
                rep     stosb
                pop     eax
end;

procedure TObject.FreeInstance; {&USES None} {&FRAME-}
asm
                push    Self                    // [1]:Pointer = @Self
                Call    CleanupInstance
                push    Self                    // [1]:Pointer = @Memory
                Call    _MemFree
end;

class function TObject.InitInstance(Instance: Pointer): TObject; {&USES edi} {&FRAME-}
asm
                cld
                mov     eax,Self                { VMT address }
                mov     edi,Instance
                mov     [edi],eax               { VMT pointer (offset = 0)  }
                add     edi,4
                mov     ecx,[eax].vtInstanceSize
                xor     eax,eax
                push    ecx
                shr     ecx,2
                dec     ecx
                rep     stosd
                pop     ecx
                and     ecx,3
                rep     stosb
                mov     edi,Instance
end;

function TObject.ClassType: TClass; {&USES None} {&FRAME-}
asm
                mov     eax,Self
                mov     eax,[eax].clVTable
end;

class function TObject.ClassName: ShortString; {&USES esi,edi} {&FRAME-}
asm
                cld
                mov     edx,Self
                mov     edi,@Result
                mov     esi,[edx].vtClassName
                movzx   ecx,[esi].Byte
                inc     ecx
                rep     movsb
end;

class function TObject.ClassNameIs(const Name: String): Boolean; {&USES esi} {&FRAME-}
asm
                mov     esi,Name
                mov     edx,Self
                test    esi,esi
                mov     al,0
                jz      @@RET
                mov     edx,[edx].vtClassName
                movzx   ecx,[edx].Byte
                cmp     ecx,[esi-4]
                jne     @@RET
                dec     esi
              @@1:
                mov     ah,[edx+ecx]
                xor     ah,[esi+ecx]
                and     ah,0DFh
                jne     @@RET
                dec     ecx
                jnz     @@1
                inc     eax
              @@RET:
end;

class function TObject.ClassParent: TClass; {&USES None} {&FRAME-}
asm
                mov     eax,Self
                mov     eax,[eax].vtParent
end;

class function TObject.InstanceSize: Longint; {&USES None} {&FRAME-}
asm
                mov     eax,Self
                mov     eax,[eax].vtInstanceSize
end;

class function TObject.ClassInfo: Pointer; {&USES None} {&FRAME-}
asm
                mov     eax,Self
                mov     eax,[eax].vtTypeInfo
end;

class function TObject.MethodAddress(const Name: ShortString): Pointer; {&USES ebx,esi,edi} {&FRAME-}
asm
                mov     eax,Self
                mov     edi,Name
              @@1:
                mov     esi,[eax].vtMethodTable
                test    esi,esi
                jz      @@Parent
                movzx   ecx,[esi].TMethodTable.Count
                add     esi,TMethodTable.Entries
              @@2:
                movzx   edx,[esi].TMethodEntry.Name.Byte
                cmp     dl,[edi]
                je      @@CmpName
              @@3:
                mov     dl,[esi].TMethodEntry.Name.Byte
                lea     esi,[esi+edx].TMethodEntry.Name[1]
                loop    @@2
              @@Parent:
                mov     eax,[eax].vtParent
                test    eax,eax
                jne     @@1
                jmp     @@RET           // Not found, return nil
// Lengths are equal, compare names themselves ignoring letter case
              @@CmpName:
                mov     bl,[esi+edx].TMethodEntry.Name.Byte
                xor     bl,[edi+edx]
                and     bl,$DF
                jne     @@3
                dec     edx
                jnz     @@CmpName
                mov     eax,[esi].TMethodEntry.Address
              @@RET:
end;

class function TObject.MethodName(Address: Pointer): ShortString; {&USES ebx,esi,edi} {&FRAME-}
asm
                cld
                mov     eax,Self
                mov     edx,Address
                mov     edi,@Result
              @@1:
                mov     esi,[eax].vtMethodTable
                test    esi,esi
                jz      @@Parent
                movzx   ecx,[esi].TMethodTable.Count
                add     esi,TMethodTable.Entries
              @@2:
                cmp     edx,[esi].TMethodEntry.Address
                je      @@Found
              @@3:
                movzx   ebx,[esi].TMethodEntry.Name.Byte
                lea     esi,[esi+ebx].TMethodEntry.Name[1]
                loop    @@2
              @@Parent:
                mov     eax,[eax].vtParent
                test    eax,eax
                jne     @@1
                mov     [edi],al        // Not found, return ''
                jmp     @@RET
              @@Found:
                add     esi,TMethodEntry.Name
                movzx   ecx,[esi].Byte
                inc     ecx
                rep     movsb
              @@RET:
end;

function TObject.FieldAddress(const Name: ShortString): Pointer; {&USES ebx,esi,edi} {&FRAME-}
asm
                mov     eax,Self
                mov     edi,Name
                mov     eax,[eax].clVTable
                xor     edx,edx
              @@1:
                mov     esi,[eax].vtFieldTable
                test    esi,esi
                jz      @@Parent
                movzx   ecx,[esi].TFieldTable.Count
                add     esi,TFieldTable.Entries
              @@2:
                mov     dl,[esi].TFieldEntry.Name.Byte
                cmp     dl,[edi]
                je      @@CmpName
              @@3:
                lea     esi,[esi+edx].TFieldEntry.Name[1]
                loop    @@2
              @@Parent:
                mov     eax,[eax].vtParent
                test    eax,eax
                jne     @@1
                jmp     @@RET           // Not found, return nil
              @@4:
                mov     dl,[esi].TFieldEntry.Name.Byte
                jmp     @@3
// Lengths are equal, compare names themselves ignoring letter case
              @@CmpName:
                mov     bl,[esi+edx].TFieldEntry.Name.Byte
                xor     bl,[edi+edx]
                and     bl,$DF
                jne     @@4
                dec     edx
                jnz     @@CmpName
                mov     eax,Self
                add     eax,[esi].TFieldEntry.Ofs
              @@RET:
end;

class function TObject.InheritsFrom(AClass: TClass): Boolean; {&USES None} {&FRAME-}
asm
                mov     eax,Self
                mov     ecx,AClass
              @@1:
                cmp     eax,ecx
                je      @@2
                mov     eax,[eax].vtParent
                test    eax,eax
                jnz     @@1
                jmp     @@RET           // Not found, return False
              @@2:
                mov     al,1
              @@RET:
end;

procedure TObject.Dispatch(var Message); {&USES None} {&FRAME-}
asm
                mov     eax,Message
                mov     ecx,Self
                mov     eax,[eax]
                test    eax,eax
                jl      @@Default
                push    eax                     // Index
                push    [ecx].clVTable.Longint  // VMT@
                Call    GetDynaMethod
                je      @@1
              @@Default:
                mov     eax,[ecx]
                mov     eax,[eax].vtDefaultHandler
              @@1:
                push    Message                 // [1]:Message
                push    ecx                     // Self
                Call    eax
end;

//±±±±±±±±±±±±±±[ 80X87 NUMERIC FUNCTIONS ]±±±±±±±±±±±±±±±±±±±±±±±±±±±±±

const
  CWChop: Word = (IC_Affine   shl sCW_IC) or    // Affine mode
                 (RC_To_Zero  shl sCW_RC) or    // Round towards 0
                 (PC_Extended shl sCW_PC) or    // Round to extended
                                  mCW_PM  or    // Masked
                                  mCW_UM  or    // Masked
                                  mCW_OM  or    // Masked
                                  mCW_ZM  or    // Masked
                                  mCW_DM  or    // Masked
                                  mCW_IM ;      // Masked
  C0:   Single = 0.0;
  C0_5: Single = 0.5;
  C1:   Single = 1.0;
  C2:   Single = 2.0;
  PI_Mul_2: Extended = 6.2831853071795864769252;  // 2*PI (SIN,COS period)

const
  C1024     :Single = 1024.0;
  HexPosINF : array[0..4] of SmallWord = ($0000, $0000, $0000, $8000, $7FFF);

var
  PosInf: Extended absolute HexPosINF;

// Trunc standard function
// EXPECTS:     ST(0)   = Argument
// RETURNS:     eax     = Result

procedure _Trunc; assembler; {&USES None} {&FRAME-}
var
  TempLong: Longint;
  CtrlWord: Word;
asm
                fstcw   CtrlWord        // Save control word
                fldcw   CWChop          // Set Rounding towards zero
                fistp   TempLong        // Save ST(0) as 32-bit integer
                fldcw   CtrlWord        // Restore previous control word
                fwait                   // Wait for result
                mov     eax,TempLong    // Return Longint result in eax
end;

// Round standard function
// COMMENTS: Coprocessor has a special rounding mode:
// rounding to nearest. However, it rounds numbers ending
// with .5 in a very strange way: towards even, so
// Round(4.5) = 4 (not 5 as one expects to be). That is
// why this routine does not use this mode.
// EXPECTS:     ST(0)   = Argument
// RETURNS:     eax     = Result

procedure _Round; assembler; {&USES None} {&FRAME-}
var
  TempLong: Longint;
  TempWord: Word;
asm
        ftst                            // X ? 0
        fnstsw   ax                     // SW -> AX
        sahf                            // AH -> FLAGS
        ja      @@1
        fsub    C0_5                    // X <= 0  -> X := X - 0.5
        jmp     @@Trunc
      @@1:
        fadd    C0_5                    // X > 0   -> X := X + 0.5
      @@Trunc:
        fstcw   TempWord                // Save control word
        fldcw   CWChop                  // Set Rounding towards zero
        fistp   TempLong                // Save ST(0) as 32-bit integer
        fldcw   TempWord                // Restore previous control word
        fwait                           // Wait for result
        mov     eax,TempLong            // Return Longint result in eax
end;

// Int standard function
// EXPECTS:     ST(0) = Argument
// RETURNS:     ST(0) = Result

procedure _Int; assembler; {&USES None} {&FRAME-}
var
  CtrlWord: Word;
asm
                fstcw   CtrlWord        // Save control word
                fldcw   CWChop          // Set Rounding toward zero
                frndint                 // Round st to integer
                fldcw   CtrlWord        // Restore previous control word
end;

// Frac standard function
// EXPECTS:      ST(0) = Argument
// RETURNS:      ST(0) = Result

procedure _Frac; assembler; {&USES None} {&FRAME-}
var
  CtrlWord: Word;
asm
                fstcw   CtrlWord        // Save control word
                fldcw   CWChop          // Set Rounding toward zero
                fld     st              // st = st(1) = argument
                frndint                 // Round st to integer
                fsubp   st(1),st        // st := st(1)-st ; pop
                fldcw   CtrlWord        // Restore previous control word
end;

// Sqrt standard function
// EXPECTS:     ST(0) = Argument
// RETURNS:     ST(0) = Result

procedure _Sqrt; {&USES None} {&FRAME-}
asm
                fsqrt                   // st := Sqrt(st)
end;

// _Sin:  Sin standard function
// _Cos:  Cos standard function
// _ATan: ArcTan standard function
// _Ln:   Ln standard function
// _Exp:  Exp standard function
// EXPECTS:     ST(0) = Argument
// RETURNS:     ST(0) = Result

// COMMENTS: The range of allowable inputs for FSIN and FCOS cannot
// exceed 2^63. If input is out of range, FSIN will leave NCP stack
// unchanged, and set C2 bit in the status word
// RETURNS:     CF = 1 if FSIN/FCOS operation is successful

procedure ChkResult; {&USES eax} {&FRAME-}
asm
                fstsw   ax                      // SW -> AX
                or      ah,mCF                  // Return CF=1
                sahf                            // C2 -> PF
                jnp     @@RET
                fld     PI_Mul_2                // Load period
                fxch    st(1)                   // ST(0) = X, ST(1) = 2*PI
              @@1:
                fprem1                          // Reduce the input modulo 2*PI
                fstsw   ax                      // SW -> AX
                sahf                            // C2 -> PF
                jp      @@1
                fstp    st(1)                   // Discard 2*PI from stack
                fdiv    st(1),st                // Obtain result
                clc
              @@RET:
end;

procedure _Sin; {&USES None} {&FRAME-}
asm
              @@1:
                fsin                            // Partial Sine
                Call    ChkResult
                jnc     @@1
end;

procedure _Cos; {&USES None} {&FRAME-}
asm
              @@1:
                fcos                            // Partial Cosine
                Call    ChkResult
                jnc     @@1
end;

procedure _ATan; {&USES None} {&FRAME-}
asm                                             // FLD Mem4r is faster than FLD1
                fld     C1                      // ST(0) := 1
                fpatan                          // ArcTan(ST(1)/ST(0))
end;

// Ln(X) = Ln(2) * Log2(X)

procedure _Ln; {&USES None} {&FRAME-}
asm
                fldln2
                fxch    st(1)
                fyl2x
end;

{ More accurate Exp routine supplied by Norbert }

// EXPECTS: ST(0) = Argument
// RETURNS: ST(0) = Exp(x)

const
  ln2_hi: array[0..7] of byte = ($00, $00, $E0, $FE, $42, $2E, $E6, $3F);
  ln2_lo: array[0..7] of byte = ($76, $3C, $79, $35, $EF, $39, $EA, $3D);

procedure _Exp; assembler; {&Frame-} {&Uses None}
asm
        fldl2e                   // log2(e) x
        fmul st, st(1)           // z = x * log2(e) x
        frndint                  // int(z) x
        fld qword ptr [ln2_hi]   // ln2_hi int(z) x
        fmul st, st(1)           // int(z)*ln2_hi int(z) x
        fsubp st(2), st          // int(z) x-int(z)*ln2_hi
        fld qword ptr [ln2_lo]   // ln2_lo int(z) x-int(z)*ln2_hi
        fmul st, st(1)           // int(z)*ln2_lo int(z) x-int(z)*ln2_hi
        fsubp st(2), st          // int(z) (x-int(z)*ln2_hi)-int(z)*ln2_lo
        fxch st(1)               // (x-int(z)*ln2_hi)-int(z)*ln2_lo int(z)
        fldl2e                   // log2(e) (x-int(z)*ln2_hi)-int(z)*ln2_lo int(z)
        fmulp st(1), st          // frac(z) int(z)
        f2xm1                    // 2^frac(z)-1 int(z)
        fld1                     // 1 2^frac(z)-1 int(z)
        faddp st(1), st          // 2^frac(z) int(z)
        fscale                   // 2^z int(z)
        fstp st(1)               // 2^z = e^x
end;

// Converts Real48 to Extended

procedure _Real482Ext(Src: Pointer); assembler; {&USES eax,ebx} {&FRAME-}
var
  Ext: ExtRec;
asm
                mov     ebx,Src
                mov     al,[ebx]
                test    al,al
                je      @@Zero
                mov     ah,[ebx+5]              // Sign
                and     ah,80h
                add     ax,3F7Eh
                mov     Ext.ER_Exponent,ax
                mov     al,[ebx+1]
                shl     eax,24
                mov     Ext.ER_Significand0.Longint,eax
                mov     eax,[ebx+2]
                or      eax,80000000h
                mov     Ext.ER_Significand2.Longint,eax
                fld     Ext
                jmp     @@RET
              @@Zero:
                fldz
              @@RET:
end;

// Converts Extended to Real48

procedure _Ext2Real48(Dest: Pointer); {&USES eax,ebx,ecx} {&FRAME-}
asm
                sub     esp,4*3
                fstp    TByte Ptr [esp]
                pop     eax
                pop     ebx
                pop     ecx
                shr     eax,24
                adc     al,0
                adc     ebx,0
                adc     cx,0
                jo      @@ERROR
                add     ebx,ebx
                add     cx,cx
                rcr     ebx,1
                shr     cx,1
                sub     cx,3F7Eh
                jg      @@1
                xor     eax,eax         // Zero
                xor     ebx,ebx
                xor     ecx,ecx
              @@1:
                test    ch,ch
                jg      @@ERROR
                mov     ch,al
                mov     eax,Dest
                mov     [eax],cx
                mov     [eax+2],ebx
                jmp     @@RET
              @@ERROR:
                add     esp,@Uses
                mov     al,reOverflow
                jmp     RtlError
              @@RET:
end;

//±±±±±±±±±±±±±±[ DIRECTORY HANDLING ROUTINES ]±±±±±±±±±±±±±±±±±±±±±±±±±

// Converts either Pascal string or PChar path to PChar path name
// EXPECTS:     esi     = Source buffer
//              edi     = Output buffer
//              bl      = Bit 6 = 0: Pascal string / 1: PChar

const
  cpShortString = 0;
  cpPChar       = $40;

procedure ConvertPath; {&USES None} {&FRAME-}
asm
                cld
                test    esi,esi
                jz      @@2
                mov     ecx,PATH_BUFFER_SIZE-1
                test    bl,cpPChar              // Is it PASCAL style string ?
                jnz     @@1                     // No, ASCIIZ
                mov     al,[esi]                // Yes, get string length
                inc     esi
                movzx   ecx,al
                test    ecx,ecx
                jz      @@2
              @@1:
                mov     al,[esi]                // Copy name
                inc     esi
                test    al,al
                jz      @@2
                mov     [edi],al
                inc     edi
                dec     ecx
                jnz     @@1
              @@2:
                mov     al,0                    // Terminate name with #0
                mov     [edi],al
                inc     edi
end;

// procedure ChDir(S: String);
// procedure MkDir(S: String);
// procedure RmDir(S: String);
// Sets InOutRes <> 0 if error occurred
// EXPECTS:      S      = function # (ChDir = 0, MkDir = 2, RmDir = 3)
//               AL     = Source string

procedure DoDirFunction(S: Pointer); assembler; {&USES ALL} {&FRAME-}
var
  Path: array [1..PATH_BUFFER_SIZE] of Byte;
const
  ProcTable: array[0..2] of Pointer = (@SysDirSetCurrent, @SysDirCreate, @SysDirDelete);
asm
                mov     ebx,eax
                mov     esi,S
                lea     edi,Path        // Convert String -> PChar
                push    edi             // [1]:PChar = Current Dir
                Call    ConvertPath
                and     ebx,3Fh
                Call    ProcTable[ebx*4].Pointer
                test    eax,eax
                jz      @@RET
                Call    SetInOutRes
              @@RET:
end;

procedure _DirCh; {&USES None} {&FRAME-}
asm
                mov     al,0+cpShortString
                jmp     DoDirFunction
end;

procedure _DirChPCh; {&USES None} {&FRAME-}
asm
                mov     al,0+cpPChar
                jmp     DoDirFunction
end;

procedure _DirMk; {&USES None} {&FRAME-}
asm
                mov     al,1+cpShortString
                jmp     DoDirFunction
end;

procedure _DirMkPCh; {&USES None} {&FRAME-}
asm
                mov     al,1+cpPChar
                jmp     DoDirFunction
end;

procedure _DirRm; {&USES None} {&FRAME-}
asm
                mov     al,2+cpShortString
                jmp     DoDirFunction
end;

procedure _DirRmPCh; {&USES None} {&FRAME-}
asm
                mov     al,2+cpPChar
                jmp     DoDirFunction
end;

// GetDir standard procedure
// procedure GetDir(D: Byte, var S: String);
// Drive number (0=default, 1=A, 2=B ...)

procedure _DirGet(Drive: Byte; S: Pointer; SLen: Longint); assembler; {&USES ALL} {&FRAME-}
var
  Path: array [1..PATH_BUFFER_SIZE] of Byte;
asm
                movzx   eax,Drive
                lea     edi,Path
                push    eax                     // [1]:DWord = Drive
                push    edi                     // [2]:PChar = Path
                Call    SysDirGetCurrent
                mov     esi,edi
                mov     edi,S
                mov     ecx,SLen                // Copy PChar =>  String
                xor     ebx,ebx
              @@1:
                mov     al,[esi]
                inc     esi
                test    al,al
                je      @@2
                inc     ebx
                mov     [edi+ebx],al
                loop    @@1
              @@2:
                mov     [edi],bl                // Write String length byte
end;

procedure _DirGetL(Drive: Byte; var LStr: Pointer); assembler; {&USES ALL} {&FRAME-}
var
  Path: array [1..PATH_BUFFER_SIZE] of Byte;
asm
                movzx   eax,Drive
                lea     edi,Path
                push    eax                     // [1]:DWord = Drive
                push    edi                     // [2]:PChar = Path
                Call    SysDirGetCurrent
                Call    PCharLength             // edi = PChar dir
                push    LStr                    // [1]:Pointer = LStr
                push    edi                     // [2]:Pointer = Src
                push    eax                     // [3]:Longint = Length
                Call    _LStrPacked
end;

//±±±±±±±±±±±±±±[ ERROR CHECK ROUTINES ]±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±

// Checks for stack overflow. This procedure is called on
// entry to any procedure or function compiled in the $S+ state

procedure _StkChk(LocalSize: Longint); {&USES None} {&FRAME-}
asm
{$IFNDEF WIN32}
                cmp     ExitCode,RTE_Stack_Overflow
                je      @@RET                   // Prevent re-raising error
                xchg    eax,LocalSize           // Size of the subprogram's
                add     eax,4*1024              // local data allocated on
                sub     eax,esp                 // stack; add extra "SAFE" 4K
                jae     @@ERROR
                neg     eax
                cmp     esp,fs:[8]              // Tib_PStackLimit
                jae     @@ERROR
                cmp     eax,fs:[4]              // Tib_PStack
                jae     @@OK
              @@ERROR:
                pop     eax
                push    RTE_Stack_Overflow      // [1]:Error code
                push    eax                     // Return address
                jmp     _RunError
              @@OK:
                mov     eax,LocalSize           // Restore EAX
              @@RET:
{$ENDIF}
end;

// Probes each stack page allocated for the caller routine
// Equivalent to
//      SUB  ESP,LocalSize
// but makes sure that stack does not fall out of the guard page

procedure _StkPrb(LocalSize: Longint); {&USES None} {&FRAME-}
asm
                push    ecx
                push    edx
                push    eax
                mov     ecx,esp
                mov     eax,LocalSize[4*3]
                sub     eax,5*4         // 3 used registers, Ret@, local size
                mov     edx,4*1024      // Page size
              @@1:
                cmp     eax,edx
                jae     @@2
                mov     edx,eax
              @@2:
                {$IfDef Linux}          // works only if esp is used
                sub     esp,edx
                mov     [esp],esp       // Probe this page
                {$Else}                 // DPMI32,OS/2,Win32: better change esp later
                sub     ecx,edx
                mov     [ecx],ecx       // Probe this page
                {$EndIf}
                sub     eax,edx
                jnz     @@1
              @@RET:
                {$IfNDef Linux}
                xchg    esp,ecx         // now really use the stack
                {$EndIf}
                push    [ecx+12].Longint// Return address
                mov     eax,[ecx+0]     // Restore all registers
                mov     edx,[ecx+4]
                mov     ecx,[ecx+8]
                PopArgs 0
end;

// Arithmetic overflow error. This procedure is called by statement code
// compiled in $Q+ state when an integer arithmetic operation overflow occured.
// Terminates program with Arithmetic Overflow run-time error.

procedure _ErrOverflow; {&USES None} {&FRAME-}
asm
                mov     al,reIntOverflow
                jmp     RtlError
end;

// Range check error. This procedure is called by statement code compiled
// in $R+ state when one of the following conditions are met:
//   þ The index of the array is out of range.
//   þ Assignment out-of-ranges value to variable.
//   þ Pass out-of-range value as a parameter.

procedure _ErrRange; {&USES None} {&FRAME-}
asm
                mov     al,reRangeError
                jmp     RtlError
end;

//±±±±±±±±±±±±±±[ PARAMETER COPYING SUPPORT ]±±±±±±±±±±±±±±±±±±±±±±±±±±±

const
  ptEnd         = 0;            // End of parameter list
  ptString      = 1;            // String
  ptSet         = 2;            // Set
  ptObject      = 3;            // Object with VMT
  ptOther       = 4;            // All other parameters

// The data structure for one parameter is as follows:

type
  ParmRec = record
    ParmType:  Byte;            // ParameterType (see values above)
    Filler:    Byte;            // Filler to the word boundary
    Src:       Word;            // Parameter EBP relative offset
    Dest:      Longint;         // Destination EBP relative offset
    Size:      Longint;         // Size of the parameter in bytes
  end;

  ParmObj = record              // This fields are present for Object with VMT
    VmtPtrOfs: Longint;         // Virtual Pointer Offset
    VmtOfs:    Longint;         // VMT address
  end;

procedure _CopyParms(Data: Pointer); {&USES eax,ecx,edx,esi,edi} {&FRAME-}
asm
                cld
                mov     edx,Data
              @@1:
                xor     eax,eax
                add     al,[edx].ParmRec.ParmType
                jz      @@Done
                movzx   esi,[edx].ParmRec.Src
                mov     edi,[edx].ParmRec.Dest
                mov     ecx,[edx].ParmRec.Size
                mov     esi,[ebp+esi]
                add     edi,ebp
                jmp     DWord Ptr @@Jmp_Table[eax*4-4]
@@Jmp_Table:    dd      OFFSET @@String
                dd      OFFSET @@Set
                dd      OFFSET @@Object
                dd      OFFSET @@Other

              @@String:
                dec     ecx             // Size-1 = Max Length
                mov     al,[esi]
                inc     esi
                cmp     al,cl
                jb      @@2
                mov     al,cl
              @@2:
                mov     [edi],al        // String Length
                inc     edi
                mov     ecx,eax         // ah = 0
                jmp     @@3

              @@Object:
                mov     ah,1            // Object with virtual methods
                jmp     @@Other

              @@Set:
                mov     al,ch           // Starting set offset
                and     ecx,0FFh        // Set Size
                add     esi,eax         // ah = 0

              @@Other:
                mov     al,cl
              @@3:
                shr     ecx,2
                and     al,11b
                rep     movsd
                mov     cl,al
                rep     movsb
                add     edx,TYPE ParmRec
                test    ah,ah           // Object with virtual methods ?
                jz      @@1             // No, copy next parameter
                                        // Yes, setup VmtPtr within object
                mov     edi,[edx-TYPE ParmRec].ParmRec.Dest
                add     edi,[edx].ParmObj.VmtPtrOfs
                mov     eax,[edx].ParmObj.VmtOfs
                mov     [edi+ebp],eax
                add     edx,TYPE ParmObj
                jmp     @@1
              @@Done:
end;

// Allocates space for open array parameter. It's actual size is known
// only at run time.

// ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿     ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿     ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿     }
// ³     Params     ³     ³    Params      ³     ³                ³     }
// ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´     ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´     ³                ³     }
// ³    Return@     ³     ³    Return@     ³     ³                ³     }
// ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´     ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´     ³     Array      ³     }
// ³ Used Registers ³     ³ Used Registers ³     ³                ³     }
// ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ<ESP ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´     ³                ³     }
//                        ³                ³     ³                ³     }
//                        ³                ³     ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´<ESP }
//                        ³    Reserved    ³     ³    Return@     ³     }
//                        ³                ³     ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´     }
//                        ³                ³     ³ Used Registers ³     }
//                        ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ<ESP>ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ     }



procedure _CopyOpArr(ElementSize,Src: Longint); {&USES ecx,edx,esi,edi} {&FRAME-}
asm
                mov     esi,Src         // Calculate array size in bytes
                mov     eax,[esi+ebp]   // Array Size - 1
                inc     eax             // Array Size
                imul    ElementSize     // Array Size * Element Size
                mov     edx,esi
                mov     esi,esp
                lea     ecx,[eax+3]
                and     cl,NOT 11b      // Align to the DWord boundary
                sub     ecx,@Params
                jnc     @@1
                xor     ecx,ecx
              @@1:
                cmp     ecx,4*1024-64
                jae     @@2
                sub     esp,ecx
                jmp     @@3
              @@2:
                push    ecx             // Probe the stack if the allocation is greater than 4K
                Call    _StkPrb
              @@3:
                mov     edi,esp         // Copy used registers and return address
                mov     ecx,(@Uses+4) / 4
                cld
                rep     movsd
                mov     esi,edi         // Copy Array itself
                xchg    esi,[edx+ebp+4] // Set new array address
                mov     ecx,eax         // edi = Copied array address
                and     al,11b
                shr     ecx,2
                rep     movsd
                mov     cl,al
                rep     movsb
                PopArgs 0
end;

procedure _CopyOpArrChk(ElementSize,Src: Longint); {&USES ecx,edx,esi,edi} {&FRAME-}
asm
                mov     esi,Src         // Calculate array size in
                mov     eax,[esi+ebp]   // Array Size - 1
                inc     eax             // Array Size
                imul    ElementSize     // Array Size * Element Size
                mov     edx,esi
                mov     esi,esp
                lea     ecx,[eax+3]
                and     cl,NOT 11b      // Align to the DWord boundary
                sub     ecx,@Params
                jnc     @@1
                xor     ecx,ecx
              @@1:
                push    ecx             // [1]:Longint = LocalSize
                Call    _StkChk
                cmp     ecx,4*1024-64
                jae     @@2
                sub     esp,ecx
                jmp     @@3
              @@2:
                push    ecx             // Probe the stack if the allocation is greater than 4K
                Call    _StkPrb
              @@3:
                mov     edi,esp         // Copy used registers and return address
                mov     ecx,(@Uses+4) / 4
                cld
                rep     movsd
                mov     esi,edi         // Copy Array itself
                xchg    esi,[edx+ebp+4] // Set new array address
                mov     ecx,eax         // edi = Copied array address
                and     al,11b
                shr     ecx,2
                rep     movsd
                mov     cl,al
                rep     movsb
                PopArgs 0
end;

//±±±±±±±±±±±±±±[ TEXT FILE I/O ]±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±

// InOutProc: Performs Text file InOutFunc
// FlushProc: Performs Text file FlushFunc
// EXPECTS:     ebx     = @ of the file variable
// Important!
// If Operation failed then InOutRes will hold error code

procedure InOutProc; {&USES ALL} {&FRAME-}
asm
                push    ebx             // [1]:Pointer = Text File variable
                Call    [ebx].TextRec.InOutFunc
                test    eax,eax
                jz      @@RET
                Call    SetInOutRes
              @@RET:
end;

procedure FlushProc; {&USES eax,ebx,ecx,edx,esi,edi} {&FRAME-}
asm
                cmp     [ebx].TextRec.FlushFunc,0  // Is FlushFunc installed ?
                jz      @@RET                      // No, skip
                push    ebx                        // [1]:Pointer = Text File variable
                Call    [ebx].TextRec.FlushFunc
                test    eax,eax
                jz      @@RET
                Call    SetInOutRes
              @@RET:
end;

// Read from Text file
// EXPECTS:     ebx     = @ of the file variable
//              eax     = @ of the Callback procedure
// Upon CallBack procedure entry:
//          ecx,edi     = ecx,edi of the _TxtRead Caller
//              ebx     = Ending buffer pointer
//              esi     = Current buffer pointer
// RETURNS:     ZF      = 1 if no error
// CallBack:    eax     = @ of the restart entry point or 0 if
//                        the operation is completed.
//              esi     = @ of the char just after last processed

procedure _TxtRead; {&USES edx,esi} {&FRAME-}
asm
                Call    TestInOutRes              // If InOutRes <> 0 then
                jnz     @@RET                     // do nothing (ZF=0: error)
                cmp     [ebx].TextRec.Mode,fmInput// File must be opened for
                je      @@OK                      // input
                mov     eax,RTE_File_Not_Open_For_Input
                Call    SetInOutRes
                jmp     @@RET                     // It's no so,report an error
                                                  // ZF=0: error
              @@OK:
                mov     esi,[ebx].TextRec.BufPos  // Is I/O Buffer exhausted ?
                cmp     esi,[ebx].TextRec.BufEnd
                jne     @@1

              @@Read_File:                        // Yes, Read from text file
                Call    InOutProc
                mov     esi,[ebx].TextRec.BufPos
                cmp     esi,[ebx].TextRec.BufEnd  // EOF ?
                je      @@RET                     // Yes, exit (ZF=1: success)

              @@1:
                push    ebx
                mov     edx,[ebx].TextRec.BufPtr  // ebx := Ending pointer
                mov     ebx,[ebx].TextRec.BufEnd  // esi := Current pointer
                add     esi,edx
                add     ebx,edx
                cld
                Call    eax                       // Call CallBack procedure
                pop     ebx
                sub     esi,edx
                mov     [ebx].TextRec.BufPos,esi
                test    eax,eax                   // Has CallBack read all the
                jne     @@Read_File               // buffer? Yes, @@Read_File
              @@RET:                              // ZF=1: success
end;

// Write blanks to the text file
// EXPECTS:     ebx     = @ of the file variable
//              edx     = Number of blanks to write
// RETURNS:     ZF      = 1 if no error

procedure _TxtWBlanks; {&USES eax,ecx,edx,esi,edi} {&FRAME-}
asm
                Call    TestInOutRes               // If InOutRes <> 0 then
                jnz     @@RET                      // do nothing (ZF=0: error)
                cmp     [ebx].TextRec.Mode,fmOutput// file must be opened for
                je      @@1                        // output
                mov     eax,RTE_File_Not_Open_For_Output
                Call    SetInOutRes
                jmp     @@RET                      // It's no so,report an error
                                                   // ZF=0: error
              @@1:
                mov     ecx,[ebx].TextRec.BufSize
                mov     edi,[ebx].TextRec.BufPos
                sub     ecx,edi
                sub     edx,ecx                    // edx = Remaining blanks to
                jae     @@2                        // write
                add     ecx,edx                    // ecx = Blanks# to write
                xor     edx,edx

              @@2:
                mov     esi,[ebx].TextRec.BufPtr   // Position in the buffer to
                add     edi,esi                    // write to
                mov     eax,'    '
                cld
                push    ecx
                shr     ecx,2                      // FAST STOS
                rep     stosd
                pop     ecx
                and     ecx,11b
                rep     stosb
                sub     edi,esi
                mov     [ebx].TextRec.BufPos,edi
                cmp     edi,[ebx].TextRec.BufSize  // Is buffer full ?
                jne     @@3                        // No, @@3
                Call    InOutProc                  // Yes,
                                                   // Flush Buffer to the file
              @@3:
                test    edx,edx                    // Have all blanks written ?
                jne     @@1                        // No, @@1
                                                   // Yes, ZF=1: success
              @@RET:
end;

// Write Data to Text file
// EXPECTS:     ebx     = @ of the file variable
//              esi     = Offset of the data to write
//              eax     = Number of bytes to write
// RETURNS:     ZF      = 1 if no error

procedure _TxtWBuf; {&USES eax,ecx,edx,esi,edi} {&FRAME-}
asm
                Call    TestInOutRes               // If InOutRes <> 0 then
                jnz     @@RET                      // do nothing (ZF=0: error)
                cmp     [ebx].TextRec.Mode,fmOutput// file must be opened for
                je      @@1                        // output
                mov     eax,RTE_File_Not_Open_For_Output
                Call    SetInOutRes
                jmp     @@RET                      // It's no so,report an error
                                                   // ZF=0: error
              @@1:
                mov     ecx,[ebx].TextRec.BufSize
                mov     edi,[ebx].TextRec.BufPos
                sub     ecx,edi                    // eax = Remaining bytes to
                sub     eax,ecx                    // write
                jae     @@2                        // ecx = # of bytes to copy
                add     ecx,eax
                xor     eax,eax

              @@2:
                mov     edx,[ebx].TextRec.BufPtr
                add     edi,edx                    // Position in the buffer to
                cld                                // write to
                push    ecx
                shr     ecx,2
                rep     movsd                      // FAST MOVS
                pop     ecx
                and     ecx,11b
                rep     movsb
                sub     edi,edx
                mov     [ebx].TextRec.BufPos,edi
                cmp     edi,[ebx].TextRec.BufSize  // Is buffer full ?
                jne     @@3                        // No, @@3
                Call    InOutProc                  // Yes,
                                                   // Flush Buffer to the file
              @@3:
                test    eax,eax                    // Have all data written ?
                jnz     @@1                        // No, @@1
                                                   // Yes, ZF=1: success
              @@RET:
end;

// _TxtRLn: ReadLn standard procedure
// _TxtWLn: WriteLn standard procedure
// _TxtREnd: End of read
// _TxtWEnd: End of write


procedure _TxtRLn(FileVar: Pointer); assembler; {&USES eax,ebx,ecx,edi} {&FRAME-}

// EXPECTS:     esi     = Current Buffer Pointer
//              ebx     = Ending Buffer Offset

procedure Read_Callback; {&USES None } {&FRAME-}
asm
              @@1:
                mov     al,[esi]                // Get Character
                inc     esi
                cmp     al,ccLF                 // Is it LF ?
                je      @@Done                  // Yes, @@Done
                cmp     al,ccCR                 // Is it CR ?
                je      @@CR                    // Yes, @@CR
                cmp     al,ccEOF                // Is it EOF
                je      @@EOF                   // Yes, @@EOF
                cmp     esi,ebx                 // Is buffer exhausted ?
                jne     @@1                     // No, get next character
                mov     eax,OFFSET @@1          // Yes, exit with restart point
                jmp     @@RET                   // in @@1

              @@CR:
                mov     eax,OFFSET @@2
                cmp     esi,ebx                 // Is buffer exhausted ?
                je      @@RET                   // Yes, exit with restart
              @@2:                              // point in @@2
                mov     al,[esi]                // Get next char
                inc     esi
                cmp     al,ccLF                 // Is it LF (CR/LF encounted) ?
                je      @@Done                  // Yes, @@Done
              @@EOF:
                dec     esi                     // Return character back
              @@Done:
                xor     eax,eax
              @@RET:
end;
asm // _TxtRLn body
                mov     ebx,FileVar
                mov     eax,OFFSET Read_Callback
                Call    _TxtRead
                jnz     @@RET
                Call    FlushProc
              @@RET:
end;

procedure _TxtWLn(FileVar: Pointer); assembler; {&USES eax,ebx,esi} {&FRAME-}
const
  {$IFDEF LINUX}
  NewLineStr: Char = ccLF;
  {$ELSE}
  NewLineStr: array[0..1] of Char = ccCR + ccLF;
  {$ENDIF}
asm
                mov     ebx,FileVar
                mov     esi,OFFSET NewLineStr   // Write CF/LF to the buffer
                mov     eax,TYPE NewLineStr
                Call    _TxtWBuf                // Has error occured?
                jnz     @@RET                   // Yes, skip flush
                Call    FlushProc               // Do flush
              @@RET:
end;

procedure _TxtREnd(FileVar: Pointer); {&USES ebx} {&FRAME-}
asm
                Call    TestInOutRes            // Is previous I/O operation
                jnz     @@RET                   // terminated successfully ?
                mov     ebx,FileVar             // Yes,
                Call    FlushProc               // Do flush
              @@RET:
end;

procedure _TxtWEnd(FileVar: Pointer); {&USES ebx} {&FRAME-}
asm
                Call    TestInOutRes            // Is previous operation
                jnz     @@RET                   // terminated successfully ?
                mov     ebx,FileVar             // Yes,
                Call    FlushProc               // Do flush
              @@RET:
end;

// Read standard procedure (String)
// Important!:  Doesn't pop file variable address

procedure _TxtRStr(FileVar,S: Pointer; SLen: Longint); assembler; {&USES eax,ebx,ecx,edi} {&FRAME-}

// EXPECTS:     esi     = Current Buffer Pointer
//              ebx     = Ending Buffer Offset

procedure Read_Callback; {&USES None} {&FRAME-}
asm
              @@1:
                mov     al,[esi]
                inc     esi
                cmp     al,ccLF                 // Is it LF ?
                je      @@2                     // Yes, @@2
                cmp     al,ccCR         // CR
                je      @@skip
                cmp     al,ccEOF        // EOF
                je      @@2
                mov     [edi],al
                inc     edi
              @@skip:
                cmp     esi,ebx
                loopne  @@1
                test    ecx,ecx
                jz      @@Done
                mov     eax,OFFSET @@1  // Restart entry = @@1
                jmp     @@RET
              @@2:
                dec     esi
              @@Done:
                xor     eax,eax
              @@RET:
end;
asm // _TxtRStr body
                mov     ebx,FileVar
                mov     edi,S
                mov     ecx,SLen
                push    edi                     // Save string start@
                inc     edi                     // Reserve 1 byte for length
                mov     eax,OFFSET Read_Callback
                Call    _TxtRead
                mov     eax,edi
                pop     edi
                sub     eax,edi                 // eax := Length+1
                dec     eax
                mov     [edi],al                // Write string length
                inc     edi
                PopArgs @Params -  TYPE FileVar
end;

// Write standard procedure (String)
// Important!:  Doesn't pop file variable address

procedure _TxtWStr(FileVar,S: Pointer; Width: Longint); {&USES eax,ebx,edx,esi} {&FRAME-}
asm
                mov     ebx,FileVar
                mov     esi,S
                movzx   eax,Byte Ptr [esi]      // eax := Length(S)
                mov     edx,Width               // If Length(S) < Width then
                sub     edx,eax                 // S will be right-justified
                jle     @@1
                Call    _TxtWBlanks             // Write blanks before string
              @@1:
                test    eax,eax
                jz      @@RET
                inc     esi                     // Skip Length byte
                Call    _TxtWBuf
              @@RET:
                PopArgs @Params -  TYPE FileVar
end;

// Write standard procedure (Long String)
// Important!:  Doesn't pop file variable address

procedure _TxtWLStr (FileVar,S: Pointer; Width: Longint); {&USES eax,ebx,edx,esi} {&FRAME-}
asm
                mov     ebx,FileVar
                mov     eax,S
                mov     esi,eax
                test    eax,eax
                jz      @@0
                mov     eax,[esi-SHS].TStrRec.Length
              @@0:
                mov     edx,Width               // If Length(S) < Width then
                sub     edx,eax                 // S will be right-justified
                jle     @@1
                Call    _TxtWBlanks             // Write blanks before string
              @@1:
                test    eax,eax
                jz      @@RET
                Call    _TxtWBuf
              @@RET:
                PopArgs @Params -  TYPE FileVar
end;

// Write standard procedure (Boolean)
// Important!:  Doesn't pop file variable address

procedure _TxtWBool(FileVar: Pointer; Value: Byte; Width: Longint); assembler; {&USES None} {&FRAME+}
const
  TrueStr:  String[4] = 'TRUE' ;
  FalseStr: String[5] = 'FALSE';
asm
                push    FileVar                 // [1]:Pointer = File Variable
                cmp     Value,0
                jnz     @@True
                push    OFFSET FalseStr         // [2]:Pointer = String Offset
                jmp     @@1
              @@True:
                push    OFFSET TrueStr
              @@1:
                push    Width                   // [3]:Longint = Width
                Call    _TxtWStr
                add     esp,4                   // _TxtWStr doesn't pop FileVar
                PopArgs @Params - TYPE FileVar
end;

// Read standard procedure (PChar)
// Important!:  Doesn't pop file variable address

procedure _TxtRPChar(FileVar,S: Pointer; SLen: Longint); assembler; {&USES eax,ebx,ecx,edi} {&FRAME-}

// EXPECTS:     esi     = Current Buffer Pointer
//              ebx     = Ending Buffer Offset

procedure Read_Callback; {&USES None} {&FRAME-}
asm
                test    ecx,ecx
                jz      @@3
              @@1:
                mov     al,[esi]
                inc     esi
                cmp     al,ccLF                 // Is it LF ?
                je      @@2                     // Yes, @@2
                cmp     al,ccCR
                je      @@skip
                cmp     al,ccEOF
                je      @@2
                mov     [edi],al
                inc     edi
              @@skip:
                cmp     esi,ebx
                loopne  @@1
                test    ecx,ecx
                jz      @@3
                mov     eax,OFFSET @@1          // Restart entry = @@1
                jmp     @@RET
              @@2:
                dec     esi
              @@3:
                xor     eax,eax
              @@RET:
end;
asm //_TxtRPChar body
                mov     ebx,FileVar
                mov     edi,S
                mov     ecx,SLen
                mov     eax,OFFSET Read_Callback
                Call    _TxtRead
                xor     eax,eax
                mov     [edi],al
                inc     edi
                PopArgs @Params - TYPE FileVar
end;

// Write standard procedure (PChar)
// Important!:  Doesn't pop file variable address

procedure _TxtWPChar(FileVar,S: Pointer; Width: Longint); {&USES ALL} {&FRAME-}
asm
                mov     eax,S
                test    eax,eax
                jz      @@1
                mov     edi,eax
                Call    PCharLength
              @@1:
                mov     ebx,FileVar
                test    eax,eax
                js      @@2
                mov     edx,Width
                sub     edx,eax
                jle     @@2
                Call    _TxtWBlanks
              @@2:
                test    eax,eax
                jz      @@RET
                mov     esi,S
                Call    _TxtWBuf
              @@RET:
                PopArgs @Params - TYPE FileVar
end;

// Read standard procedure (integer)
// RETURNS:     eax = Integer value
// Important!:  Doesn't pop file variable address

procedure _TxtRInt(FileVar: Pointer); assembler; {&USES ebx,ecx,edi} {&FRAME-}

// EXPECTS:     esi     = Current Buffer Pointer
//              ebx     = Ending Buffer Offset

procedure Read_Callback; {&USES None} {&FRAME-}
asm
              @@1:
                mov     al,[esi]                // Skip blanks and
                inc     esi
                cmp     al,' '                  // control characters
                ja      @@Copy
                cmp     al,ccEOF                // EOF ?
                je      @@3                     // Put it back and exit
                cmp     esi,ebx                 // Is buffer exhausted ?
                jne     @@1                     // No, get next char
                mov     eax,OFFSET @@1          // Yes, Restart entry = @@1
                jmp     @@RET

              @@2:                              // Read string with number
                mov     al,[esi]                // and copy it to the buffer
                inc     esi
                cmp     al,' '
                jbe     @@3

              @@Copy:
                mov     [edi],al
                inc     edi
                cmp     esi,ebx
                loopne  @@2
                test    ecx,ecx
                jz      @@Done
                mov     eax,OFFSET @@2          // Restart entry = @@2
                jmp     @@RET

              @@3:
                dec     esi                     // Return back control char
              @@Done:
                xor     eax,eax
              @@RET:
end;
var
  Buffer: array[0..31] of Byte;
asm // _TxtRInt body
                mov     ebx,FileVar
                mov     eax,OFFSET Read_Callback
                mov     ecx,TYPE Buffer
                lea     edi,Buffer
                push    edi
                Call    _TxtRead
                mov     ecx,edi
                pop     edi                     // ecx := String Length
                sub     ecx,edi                 // Is anything being read ?
                jz      @@Zero                  // No, return zero
                Call    Str2Int                 // Yes, Convert String to
                jc      @@ERROR                 // Integer (Result in EAX)
                test    ecx,ecx
                jz      @@RET
              @@ERROR:
                mov     eax,RTE_Invalid_Numeric_Format
                Call    SetInOutRes
              @@Zero:
                xor     eax,eax
              @@RET:
                PopArgs @Params - Type FileVar
end;

// Write standard procedure (integer)
// Important!:  Doesn't pop file variable address

procedure _TxtWInt(FileVar: Pointer; Value,Width: Longint); assembler; {&USES ALL} {&FRAME-}
var
  Buffer: array[0..31] of Byte;
asm
                mov     eax,Value
                mov     ebx,FileVar
                mov     edx,Width
                lea     edi,Buffer              // Allocate buffer
                Call    Int2Str
                mov     eax,ecx                 // Write blanks (if necessary)
                sub     edx,ecx                 // Output is right-justified
                jle     @@1                     // edx = Number of blanks
                Call    _TxtWBlanks             // to write
              @@1:
                lea     esi,Buffer              // esi <= buffer@
                Call    _TxtWBuf
                PopArgs @Params - Type FileVar
end;

// Read standard procedure (float)
// Important!:  Doesn't pop file variable adderss

procedure _TxtRFlt(FileVar: Pointer); assembler; {&USES ALL} {&FRAME-}

// EXPECTS:     esi     = Current Buffer Pointer
//              ebx     = Ending Buffer Offset

procedure Read_Callback; {&USES None} {&FRAME-}
asm
              @@1:
                mov     al,[esi]                // Skip blanks and
                inc     esi
                cmp     al,' '                  // control characters
                ja      @@Copy
                cmp     al,ccEOF                // EOF ?
                je      @@3                     // Put it back and exit
                cmp     esi,ebx                 // Is buffer exhausted ?
                jne     @@1                     // No, get next char
                mov     eax,OFFSET @@1          // Yes, Restart entry = @@1
                jmp     @@RET

              @@2:                              // Read string with number
                mov     al,[esi]                // and copy it to the buffer
                inc     esi
                cmp     al,' '
                jbe     @@3

              @@Copy:
                mov     [edi],al
                inc     edi
                cmp     esi,ebx
                loopne  @@2
                test    ecx,ecx
                jz      @@Done
                mov     eax,OFFSET @@2          // Restart entry = @@2
                jmp     @@RET

              @@3:
                dec     esi                     // Return back control char
              @@Done:
                xor     eax,eax
              @@RET:
end;
var
  Buffer: array[0..79] of Byte;
asm
                mov     ebx,FileVar
                mov     eax,OFFSET Read_Callback
                mov     ecx,TYPE Buffer
                lea     edi,Buffer
                push    edi
                Call    _TxtRead
                mov     ecx,edi
                pop     edi                     // ecx := String Length
                sub     ecx,edi                 // Is anything being read ?
                jz      @@Zero                  // No, Return zero
                Call    Str2Float               // Yes, Convert String to Float
                jc      @@ERROR                 // Returns result in ST(0)
                test    ecx,ecx
                jz      @@RET
              @@ERROR:
                mov     eax,RTE_Invalid_Numeric_Format
                Call    SetInOutRes
                fstp    st                      // Pop out ST(0)
              @@Zero:
                fldz
                wait                            // Wait for result
              @@RET:
                PopArgs @Params - Type FileVar
end;

// Write standard procedure (float)
// EXPECTS:      ST(0) = Floating point value
// Important!:  Doesn't pop file variable adderss

procedure _TxtWFlt(FileVar: Pointer; Width,Dec: Longint); assembler; {&USES ALL} {&FRAME+}
var
  Buffer: array[0..63] of Byte;
asm
                mov     ecx,Dec
                test    ecx,ecx
                jns     @@1
                mov     ecx,8
                sub     ecx,Width
                cmp     ecx,-2
                jle     @@1
                mov     ecx,-2
              @@1:
                lea     edi,Buffer              // Convert float in ST(0) to
                Call    Float2Str               // string
                mov     eax,ecx
                mov     ebx,FileVar
                mov     edx,Width               // Write blanks (if necessary)
                sub     edx,ecx                 // Output is right-justified
                jle     @@2                     // edx = Number of blanks
                Call    _TxtWBlanks             // to write
              @@2:
                lea     esi,Buffer              // esi := buffer@
                Call    _TxtWBuf
                PopArgs @Params - Type FileVar
end;

// Read standard procedure (Char)
// Important!:  Doesn't pop file variable address
// RETURNS:     al = Char

procedure _TxtRChar(FileVar: Pointer); {&USES ebx,edi} {&FRAME-}
asm
                Call    TestInOutRes            // If InOutRes <> 0 then
                jnz     @@EOF                   // return EOF
                mov     ebx,FileVar
                cmp     [ebx].TextRec.Mode,fmInput // Is file opened for Input?
                jne     @@ERROR                 // No, error
                mov     edi,[ebx].TextRec.BufPos
                cmp     edi,[ebx].TextRec.BufEnd// Is buffer exhausted ?
                jne     @@1                     // No, get from buffer
                Call    InOutProc               // Yes, read from the file
                mov     edi,[ebx].TextRec.BufPos// EOF ?
                cmp     edi,[ebx].TextRec.BufEnd// Yes, return EOF character
                je      @@EOF

              @@1:
                inc     [ebx].TextRec.BufPos
                add     edi,[ebx].TextRec.BufPtr// Get char from the buffer
                mov     al,[edi]
                jmp     @@RET

              @@ERROR:
                mov     eax,RTE_File_Not_Open_For_Input
                Call    SetInOutRes

              @@EOF:
                mov     al,ccEOF

              @@RET:
                PopArgs @Params - TYPE FileVar
end;

// Write standard procedure (Char)
// Important!:  Doesn't pop file variable address

procedure _TxtWChar(FileVar: Pointer; Value: Byte; Width: Longint); {&USES ALL} {&FRAME-}
asm
                mov     ebx,FileVar
                mov     edx,Width
                dec     edx                     // Right-justify output
                jle     @@1                     // edx := Number of blanks
                Call    _TxtWBlanks             // to write
              @@1:                              // Is previous I/O operation
                Call    TestInOutRes            // terminated successfully ?
                jnz     @@RET                   // No, do nothing
                cmp     [ebx].TextRec.Mode,fmOutput//Is file opened for Output?
                jne     @@ERROR                 // No, report an error
                inc     [ebx].TextRec.BufPos
                mov     edi,[ebx].TextRec.BufPos
                add     edi,[ebx].TextRec.BufPtr// Write char to the buffer
                mov     dl,Value
                mov     [edi-1],dl
                mov     edx,[ebx].TextRec.BufSize
                add     edx,[ebx].TextRec.BufPtr
                cmp     edi,edx                 // Is buffer full ?
                jne     @@RET
                Call    InOutProc               // Yes, flush it to file
                jmp     @@RET
              @@ERROR:
                mov     eax,RTE_File_Not_Open_For_Output
                Call    SetInOutRes
              @@RET:
                PopArgs @Params - TYPE FileVar
end;

// _TxtSEoln    SeekEoln standard function
// _TxtSEof     SeekEof  standard function
// _TxtEoln     Eoln     standard function
// _TxtEof      Eof      standard function

const
  tsEof      = $0;
  tsEoln     = $1;
  tsSeekEof  = $2;
  tsSeekEoln = $3;

// EXPECTS:      ebx = File Variable address

procedure TextFile_Status; assembler; {&USES ecx,esi,edi} {&FRAME-}

// EXPECTS:      esi    = Current Buffer Pointer
//               ebx    = Ending Buffer Offset

procedure Read_Callback; {&USES None} {&FRAME-}
asm
              @@1:
                cmp     ebx,esi         // Bufptr = Buffer end?
                je      @@3
                mov     al,[esi]
                inc     esi
                cmp     al,ccEOF
                je      @@True
                test    cl,01b          // EOL bit
                jz      @@2
                cmp     al,ccCR
                je      @@1
                cmp     al,ccLF                 // Is it LF ?
                je      @@True                  // Yes, @@True

              @@2:
                test    cl,10b          // SEEK bit
                jz      @@False
                cmp     al,' '
                ja      @@False
                cmp     esi,ebx
                jne     @@1
              @@3:
                mov     eax,OFFSET @@1  // Restart entry = @@1
                jmp     @@RET

              @@False:
                mov     ch,0            // Return false
              @@True:
                dec     esi             // Return back control character
                xor     eax,eax
              @@RET:
end;
asm // TextFile_Status body
                mov     ch,1            // True
                mov     eax,OFFSET Read_Callback
                Call    _TxtRead
                mov     al,ch
end;

procedure _TxtSEoln(FileVar: Pointer); {&USES ebx,ecx} {&FRAME-}
asm
                mov     ebx,FileVar
                mov     cl,tsSeekEoln
                Call    TextFile_Status
end;

procedure _TxtSEof(FileVar: Pointer); {&USES ebx,ecx} {&FRAME-}
asm
                mov     ebx,FileVar
                mov     cl,tsSeekEof
                Call    TextFile_Status
end;

procedure _TxtEoln(FileVar: Pointer); {&USES ebx,ecx} {&FRAME-}
asm
                mov     ebx,FileVar
                mov     cl,tsEoln
                Call    TextFile_Status
end;

procedure _TxtEof(FileVar: Pointer); {&USES ebx,ecx} {&FRAME-}
asm
                mov     ebx,FileVar
                mov     cl,tsEof
                Call    TextFile_Status
end;

// Forward declarations

procedure AppendFile; forward;
procedure _TxtFOpen(FileVar: Pointer); forward;

// Do Text File function call
// EXPECTS:     ebx     = Function offset within TextRec
//              edi     = Text file variable address
// RETURNS:     ZF      = 0 if error occurred

procedure Do_Function; {&USES ALL} {&FRAME-}
asm
                and     ebx,7Fh
                push    edi                     // [1]:Pointer = FileVar
                Call    DWord Ptr [edi+ebx]
                test    eax,eax
                jz      @@RET
                Call    SetInOutRes
              @@RET:
end;

// _TxtAssign: Assign standard procedure (String)
// _TxtAssignPCh: Assign standard procedure (PChar)
// procedure Assign( var F; String);
// String can be either ShortString, AnsiString or PChar

procedure Assign_Text; {&USES eax,ecx,edx} {&FRAME-}
asm
                cld
                xor     eax,eax                 // Fill in TextRec fields
                mov     [edi].TextRec.Handle,eax
                mov     [edi].TextRec.Mode,fmClosed
                mov     [edi].TextRec.BufSize,TYPE TextRec.Buffer
                mov     [edi].TextRec.BufPos,eax
                mov     [edi].TextRec.BufEnd,eax
                lea     edx,[edi].TextRec.Buffer
                mov     [edi].TextRec.BufPtr,edx
                mov     [edi].TextRec.OpenFunc,OFFSET _TxtFOpen
                add     edi,TextRec.InOutFunc   // Wipe out user data
                mov     ecx,(TextRec.Name-TextRec.InOutFunc) / 4
                rep     stosd
                Call    ConvertPath
end;

procedure _TxtAssignPCh(FileVar,S: Pointer); {&USES ebx,esi,edi} {&FRAME-}
asm
                mov     bl,cpPChar              // bl: Flag
                mov     esi,S
                mov     edi,FileVar
                Call    Assign_Text
end;

procedure _TxtAssign(FileVar,S: Pointer); {&USES ebx,esi,edi} {&FRAME-}
asm
                mov     bl,cpShortString        // bl: Flag
                mov     esi,S
                mov     edi,FileVar
                Call    Assign_Text
end;

// SetTextBuf standard procedure
// procedure SetTextBuf(var F:Text; var Buf [; Size: Longint]);

procedure _TxtSetBuf(FileVar,Buffer: Pointer; BufSize: Longint); {&USES eax,edi} {&FRAME-}
asm
                mov     edi,FileVar             // Fill in Buffer@ and Length
                mov     eax,BufSize             // fields
                mov     [edi].TextRec.BufSize,eax
                mov     eax,Buffer
                mov     [edi].TextRec.BufPtr,eax
                xor     eax,eax                 // Initialize BufPos, BufEnd
                mov     [edi].TextRec.BufPos,eax
                mov     [edi].TextRec.BufEnd,eax
end;

// _TxtReset:   Reset standard procedure
// _TxtRewrite: Rewrite standard procedure
// _TxtAppend:  Append standard procedure
// procedure Reset  (var F[:File; RecSize: Longint] );
// procedure Rewrite(var F[:File; RecSize: Longint] );
// procedure Append (var F: Text);

// EXPECTS:     edi     = File Variable address
//              eax     = Open Mode

procedure Text_Open; {&USES ebx} {&FRAME-}
asm
                mov     ebx,[edi].TextRec.Mode
                cmp     ebx,fmInput
                je      @@Close
                cmp     ebx,fmOutput
                je      @@Close
                cmp     ebx,fmClosed
                je      @@SkipClose
                mov     eax,RTE_File_Not_Assigned
                Call    SetInOutRes
                jmp     @@RET

              @@Close:
                push    edi                     // [1]:Pointer = FileVar
                Call    _TxtClose

              @@SkipClose:
                xor     ebx,ebx
                mov     [edi].TextRec.Mode,eax
                mov     [edi].TextRec.BufPos,ebx
                mov     [edi].TextRec.BufEnd,ebx
                mov     bl,TextRec.OpenFunc
                Call    Do_Function
                jz      @@RET
                mov     [edi].TextRec.Mode,fmClosed
              @@RET:
end;

procedure _TxtReset(FileVar: Pointer); {&USES eax,edi} {&FRAME-}
asm
                mov     eax,fmInput
                mov     edi,FileVar
                Call    Text_Open
end;

procedure _TxtRewrite(FileVar: Pointer); {&USES eax,edi} {&FRAME-}
asm
                mov     eax,fmOutput
                mov     edi,FileVar
                Call    Text_Open
end;

procedure _TxtAppend(FileVar: Pointer); {&USES eax,edi} {&FRAME-}
asm
                mov     eax,fmInOut
                mov     edi,FileVar
                Call    Text_Open
end;

// _TxtFlush:   Flush standard procedure
// _TxtClose:   Close standard procedure
// procedure Close(var F );
// procedure Flush(var F: Text);

procedure Text_FlushClose; {&USES ebx} {&FRAME-}
asm
                cmp     [edi].TextRec.Mode,fmInput
                je      @@2
                cmp     [edi].TextRec.Mode,fmOutput
                je      @@1
                mov     eax,RTE_File_Not_Open
                Call    SetInOutRes
                jmp     @@RET
              @@1:
                mov     bl,TextRec.InOutFunc
                Call    Do_Function
              @@2:
                test    al,al                   // Flush ?
                jz      @@RET                   // Yes, skip close
                mov     bl,TextRec.CloseFunc    // No, it's Close, close file
                Call    Do_Function
                mov     [edi].TextRec.Mode,fmClosed
              @@RET:
end;

procedure _TxtFlush(FileVar: Pointer); {&USES eax,edi} {&FRAME-}
asm
                mov     al,0                    // Flag: 0 = Flush
                mov     edi,FileVar
                Call    Text_FlushClose
end;

procedure _TxtClose(FileVar: Pointer); {&USES eax,edi} {&FRAME-}
asm
                mov     al,1                    // Flag: 1 = Close
                mov     edi,FileVar
                Call    Text_FlushClose
end;

// Standart text file driver I/O procedures
// _TxtFOpen    Open file
// _TxtFRead    Read file
// _TxtFWrite   Write to disk file
// _TxtFClose   Close file
// RETURNS:     eax = Error code

procedure _TxtFRead(FileVar: Pointer); {&USES edi} {&FRAME-}
asm
                mov     edi,FileVar
                push    0                       // Bytes Read
                mov     ecx,esp
                push    [edi].TextRec.Handle    // [1]:DWord = File Handle
                push    [edi].TextRec.BufPtr    // [2]:PChar = Buffer
                push    [edi].TextRec.BufSize   // [3]:DWord = Count
                push    ecx                     // [4]:DWord = @ByteRead
                Call    SysFileRead
                pop     ecx                     // Ignore actual
                mov     [edi].TextRec.BufEnd,ecx
                and     [edi].TextRec.BufPos,0  // eax = Error Code

                cmp     eax,109                 // msg_Broken_Pipe
                jne     @@RET
                xor     eax,eax
              @@RET:
end;

procedure _TxtFWrite(FileVar: Pointer); {&USES None} {&FRAME-}
asm
                mov     edx,FileVar
                xor     eax,eax
                xchg    eax,[edx].TextRec.BufPos
                push    0                       // Actual
                mov     ecx,esp
                push    [edx].TextRec.Handle    // [1]:DWord = File Handle
                push    [edx].TextRec.BufPtr    // [2]:PChar = Buffer
                push    eax                     // [3]:DWord = Count
                push    ecx                     // [4]:DWord = @ByteWrite
                Call    SysFileWrite
                pop     ecx                     // Ignore actual
              @@RET:
end;

procedure _TxtFClose(FileVar: Pointer); {&USES None} {&FRAME-}
asm
                mov     eax,FileVar
                push    [eax].TextRec.Handle    // [1]:DWord = File Handle
                Call    SysFileClose
end;

procedure _TxtFOpen(FileVar: Pointer); {&USES ebx,esi,edi} {&FRAME-}
asm
                mov     edi,FileVar
                cmp     [edi].TextRec.Mode,fmInput // Output: StdOut Handle
                je      @@Input
                Call    SysFileStdOut
                jmp     @@1
              @@Input:
                Call    SysFileStdIn
              @@1:
                mov     ecx,eax
                lea     edx,[edi].TextRec.Name     // If name is empty then
                cmp     [edx].Byte,0               // file will refer to StdIn
                je      @@SkipOpen                 // or StdOut
                push    OFFSET TextModeRead
                Call    _GetTlsVar
                mov     ecx,[eax]                  // TextModeRead
                test    ecx,ecx
                jnz     @@2
                mov     cl,40h
              @@2:
                mov     eax,[eax+4]                // TextModeReadWrite
                test    eax,eax
                jnz     @@3
                mov     al,42h
              @@3:
                cmp     [edi].TextRec.Mode,fmInOut
                je      @@Open                     // Append
                cmp     [edi].TextRec.Mode,fmInput
                jne     @@Output                   // Rewrite
                xchg    eax,ecx
// Reset -> Open existing file
              @@Open:
// why?         movzx   eax,al
                push    0                       // Handle
                mov     ecx,esp
                push    edx                     // [1]:PChar = FileName
                push    eax                     // [2]:DWord = Mode
                push    ecx                     // [3]:DWord = @Handle
                Call    SysFileOpen
                pop     ecx                     // Handle
                jmp     @@OpenDone
// Rewrite -> Create a file
              @@Output:
// why?         movzx   eax,al
                push    0
                mov     ecx,esp
                push    edx                     // [1]:PChar = FileName
                push    eax                     // [2]:DWord = Mode
                push    0                       // [3]:DWord = Attr = Normal file
                push    ecx                     // [4]:DWord = @Handle
                Call    SysFileCreate
                pop     ecx
              @@OpenDone:
                test    eax,eax
                jnz     @@RET                   // Error occurred
// EXPECTS: eax = File Handle
              @@SkipOpen:
                mov     [edi].TextRec.Handle,ecx
                mov     ebx,ecx
                mov     eax,OFFSET _TxtFRead            // InOutFunc
                xor     ecx,ecx                         // FlushFunc = None
                cmp     [edi].TextRec.Mode,fmInput
                je      @@Done
                push    [edi].TextRec.Handle            // [1]:DWord = Handle
                Call    SysFileIsDevice                 // Device ?
                mov     dl,al
                mov     eax,OFFSET _TxtFWrite           // InOutFunc
                mov     ecx,eax                         // FlushFunc
                test    dl,dl
                jnz     @@Device
                cmp     [edi].TextRec.Mode,fmInOut
                je      @@Append
                cmp     [edi].TextRec.Name.Byte,0       // For redirected StdOut
                jz      @@Device
                jmp     @@4
              @@Append:
                Call    AppendFile
              @@4:
                xor     ecx,ecx                         // FlushFunc = None
              @@Device:
                mov     [edi].TextRec.Mode,fmOutput
              @@Done:
                mov     [edi].TextRec.InOutFunc,eax
                mov     [edi].TextRec.FlushFunc,ecx
                mov     [edi].TextRec.CloseFunc,OFFSET _TxtFClose
                xor     eax,eax                         // Error Code := 0
              @@RET:
end;

// Truncates the file at the current file position

function TruncateFile(FileHandle: Longint): Longint;
var
  FilePos: TFileSize;
begin
  Result := SysFileSeek(FileHandle, 0, 1, FilePos);
  if Result = 0 then
    Result := SysFileSetSize(FileHandle, FilePos);
end;

// Prepares Text File for appending.
// EXPECTS:     edi     = Offset of the file variable

procedure AppendFile; {&USES ALL} {&FRAME-}
asm
// Get File Size
                {$IfDef LargeFileSupport}
                push    0                       // Actual: Bit 63..32
                {$EndIf}
                push    0                       // Actual
                mov     ecx,esp
                push    [edi].TextRec.Handle    // [1]:DWord = File Handle
                {$IfDef LargeFileSupport}
                push    0                       //           = Distance: Bit 63..32
                {$EndIf}
                push    0                       // [2]:DWord = Distance
                push    2                       // [3]:DWord = Method (EOF)
                push    ecx                     // [4]:DWord = @Actual
                Call    SysFileSeek
                pop     eax                     // File size
                {$IfDef LargeFileSupport}
                pop     ebx                     // File size: Bit 63..32
                {$EndIf}
// Set File Pointer 128 bytes ahead of EOF (If possible)
                sub     eax,TYPE TextRec.Buffer
                {$IfDef LargeFileSupport}
                sbb     ebx,0
                test    ebx,ebx                 // non-negative?
                jns     @@1
                xor     ebx,ebx                 // else from begin
                {$Else}
                jge     @@1
                {$EndIf}
                xor     eax,eax                 // else from begin
              @@1:
                {$IfDef LargeFileSupport}
                push    0                       // Actual: Bit 63..32
                {$EndIf}
                push    0                       // Actual: Bit 31..00
                mov     ecx,esp
                push    [edi].TextRec.Handle    // [1]:DWord = File Handle
                {$IfDef LargeFileSupport}
                push    ebx                     //             Distance: Bit 63..32
                {$EndIf}
                push    eax                     // [2]:DWord = Distance
                push    0                       // [3]:DWord = Method (Start file)
                push    ecx                     // [4]:DWord = @Actual
                Call    SysFileSeek
                pop     eax
                {$IfDef LargeFileSupport}
                pop     ebx                     // Actual: Bit 63..32 (ignored)
                {$EndIf}
// Fill in the text file buffer
                lea     edx,[edi].TextRec.Buffer// Fill buffer
                push    0                       // Bytes Read
                mov     ecx,esp
                push    [edi].TextRec.Handle    // [1]:DWord = File Handle
                push    edx                     // [2]:PChar = Buffer
                push    TYPE TextRec.Buffer     // [3]:DWord = Count
                push    ecx                     // [4]:DWord = @ByteRead
                Call    SysFileRead
                pop     eax                     // Bytes Read
                xor     edx,edx                 // Buffer pointer := 0
              @@2:
                cmp     edx,eax                 // Is all buffer done ?
                je      @@RET                   // Yes, exit
                cmp     [edi].TextRec.Buffer[edx].Byte,ccEOF
                je      @@EOF
                inc     edx
                jmp     @@2
// EOF is encountered
              @@EOF:
                sub     ebx,ebx                 // ebx:edx:=SizeOf(TextRec.Buffer)
                sub     edx,eax                 //         -Position(^Z)
                sbb     ebx,0
// Set File Pointer again to EOF
                {$IfDef LargeFileSupport}
                push    0                       // Actual: Bit 63..32
                {$EndIf}
                push    0                       // Actual: Bit 31..00
                mov     ecx,esp
                push    [edi].TextRec.Handle    // [1]:DWord = File Handle
                {$IfDef LargeFileSupport}
                push    ebx
                {$EndIf}
                push    edx                     // [2]:DWord = Distance
                push    2                       // [3]:DWord = Method (EOF)
                push    ecx                     // [4]:DWord = @Actual
                Call    SysFileSeek
                pop     eax                     // File size: Bit 31..00
                {$IfDef LargeFileSupport}
                pop     ebx                     // File size: Bit 63..32
                {$EndIf}
                push    [edi].TextRec.Handle
                Call    TruncateFile
              @@RET:
end;

//±±±±±±±±±±±±±±[ BINARY FILE ROUTINES ]±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±

// Erase standard procedure
// procedure Erase (Var F);

procedure _Erase(FileVar: Pointer); {&USES eax,ecx,edx} {&FRAME-}
asm
                mov     edx,FileVar
                add     edx,TextRec.Name.Longint
                push    edx                     // [1]:PChar = FileName
                Call    SysFileDelete
                test    eax,eax
                jz      @@RET
                Call    SetInOutRes
              @@RET:
end;

// Rename standard procedure
// procedure Rename (var F; NewName: String);

procedure RenameFile; assembler; {&USES eax,ecx,edi} {&FRAME+}
var
  Buffer: array[1..PATH_BUFFER_SIZE] of Byte;
asm
                cld
                lea     edi,Buffer
                mov     ecx,TYPE TextRec.Name - 1
                test    bl,bl                   // PChar ?
                jnz     @@1                     // Yes
                movzx   eax,[esi].Byte          // No, String: Get length
                inc     esi
                mov     ecx,eax
                test    ecx,ecx
                jz      @@2
              @@1:                              // Copy new name
                mov     al,[esi]
                inc     esi
                test    al,al
                jz      @@2
                mov     [edi],al
                inc     edi
                dec     ecx
                jnz     @@1
              @@2:
                mov     al,0                    // Terminate int with #0
                mov     [edi],al
                lea     esi,[edx].TextRec.Name
                lea     edi,Buffer
                push    esi                     // [1]:PChar = Old Name
                push    edi                     // [2]:PChar = New Name
                Call    SysFileMove
                test    eax,eax
                jz      @@OK
                Call    SetInOutRes
                jmp     @@RET
              @@OK:
                xchg    esi,edi
              @@3:
                mov     al,[esi]                // Copy new name to the text
                inc     esi
                mov     [edi],al
                inc     edi
                test    al,al                   // file variable
                jne     @@3
              @@RET:
end;

procedure _Rename(FileVar,NewName: Pointer); {&USES ebx,edx,esi} {&FRAME-}
asm
                mov     edx,FileVar
                mov     esi,NewName
                mov     bl,0                    // Flag: 0 = String
                Call    RenameFile
end;

procedure _RenamePCh(FileVar,NewName: Pointer); assembler; {&USES ebx,edx,esi} {&FRAME-}
const
  Zero: Byte = 0;
asm
                mov     edx,FileVar
                mov     esi,NewName
                test    esi,esi
                jnz     @@1
                lea     esi,Zero
              @@1:
                mov     bl,1                    // Flag: 1 = PChar
                Call    RenameFile
end;


{$IfDef LargeFileSupport} {64 bit size}

{&Cdecl+} {to return 2*64 bit on stack}
procedure GetFileInfo(file_size,file_pos:TFileSize); {&Uses All} {&Frame+}

  asm
                mov     eax,RTE_File_Not_Open
                cmp     [edi].TextRec.Mode,fmInOut
                jne     @@ERROR
// Get Current Position
                mov     ebx,[edi].TextRec.Handle
                lea     ecx,file_pos            // Actual
                push    ebx                     // [1]:DWord = File Handle
                push    0                       // [2]:DWord = Distance
                push    0
                push    1                       // [3]:DWord = Method (Current)
                push    ecx                     // [4]:DWord = @Actual
                Call    SysFileSeek
                test    eax,eax
                jnz     @@ERROR
// Get File Size
                lea     ecx,file_size           // Actual
                push    ebx                     // [1]:DWord = File Handle
                push    0                       // [2]:DWord = Distance
                push    0
                push    2                       // [3]:DWord = Method (EOF)
                push    ecx                     // [4]:DWord = @Actual
                Call    SysFileSeek
// Restore current position
                push    0                       // Actual
                push    0
                mov     ecx,esp
                push    ebx                     // [1]:DWord = File Handle
                push    DWord Ptr [file_pos+4]  // [2]:DWord = Distance
                push    DWord Ptr [file_pos+0]
                push    0                       // [3]:DWord = Method (Start file)
                push    ecx                     // [4]:DWord = @Actual
                Call    SysFileSeek
                add     esp,2*4                 // Current file position
                test    eax,eax
                jz      @@RET                   // CF = 0
              @@ERROR:
                Call    SetInOutRes
                sub     eax,eax
                mov     DWord Ptr [file_pos +0],eax
                mov     DWord Ptr [file_pos +4],eax
                mov     DWord Ptr [file_size+0],eax
                mov     DWord Ptr [file_size+4],eax
                stc                             // CF := 1
              @@RET:
  end;
{&Cdecl-}

procedure _FilePos_dummy(FileVar: Pointer);
  {&Frame-}{&Uses None}
  asm
    // dummy for compiler magic, should never called!
  end;

function FilePos(Var F):TFileSize; {&Uses All} {&Frame-}
  asm
                mov     edi,F
                sub     eax,eax
                push    eax                     // file_pos
                push    eax
                push    eax                     // file_size
                push    eax
                Call    GetFileInfo
                jc      @error

                fild    [esp+4+4].TFileSize     // get file_pos (in bytes)
                fidiv   [edi].TextRec.BufSize   // Result := FilePos / RecSize
                call    _Int                    // round down
                jmp     @RET

              @error:
                fldz                            // return 0 on error

              @RET:
                wait
                add     esp,4*4                 // pop out file_size and file_pos  end;
  end;

procedure _FileSize_dummy(FileVar: Pointer);
  {&Frame-}{&Uses None}
  asm
    // dummy for compiler magic, should never called!
  end;

function FileSize(Var F):TFileSize; {&USES All} {&FRAME-}
  asm
                mov     edi,F
                sub     eax,eax
                push    eax                     // file_pos
                push    eax
                push    eax                     // file_size
                push    eax
                Call    GetFileInfo             // Returns: ebx = File Position
                jc      @error

                fild    [esp].TFileSize         // get file_size (in bytes)
                fidiv   [edi].TextRec.BufSize   // Result := FileSize / RecSize
                call    _Int                    // round down
                jmp     @RET

              @error:
                fldz                            // return 0 on error

              @RET:
                wait
                add     esp,4*4                 // pop out file_size and file_pos
  end;


procedure _Eof(FileVar: Pointer); {&USES ebx,ecx,esi,edi} {&FRAME-}
  asm
                mov     edi,FileVar
                sub     eax,eax
                push    eax                     // file_pos
                push    eax
                push    eax                     // file_size
                push    eax
                Call    GetFileInfo
                pop     ebx
                pop     ecx
                pop     esi
                pop     edi
                mov     al,FALSE
                jc      @@RET
                                                // EOF := FilePos = FileSize;
                sub     ebx,esi                 // low part equal ?
                sub     ecx,edi                 // high part equal ?
                or      ebx,ecx                 // got no different bit ?
                setz    al                      // then return TRUE
              @@RET:
  end;

{$Else LargeFileSupport} {32 bit size}

// _FilePos:  FilePos standard function
// _FileSize: FileSize standard function
// _Eof:      Eof standard function
// function FilePos(var F): Longint;
// function FileSize(var F): Longint;
// function Eof(var F): Boolean;
//      where   F = File Variable other than TEXT
//
// Sets InOutRes <> 0 if error

// Returns current file position and size
// EXPECTS:      edi     = @ of the File Variable
// RETURNS:[edx:]ebx     = File position
//               esi     = File size
//               CF      = 1 if file is not open

procedure GetFileInfo; {&USES eax,ecx,edx} {&FRAME-}
asm
                mov     eax,RTE_File_Not_Open
                cmp     [edi].TextRec.Mode,fmInOut
                jne     @@ERROR
// Get Current Position
                push    0                       // Actual
                mov     ecx,esp
                push    [edi].TextRec.Handle    // [1]:DWord = File Handle
                push    0                       // [2]:DWord = Distance
                push    1                       // [3]:DWord = Method (Current)
                push    ecx                     // [4]:DWord = @Actual
                Call    SysFileSeek
                pop     ebx                     // Current file position
                test    eax,eax
                jnz     @@ERROR
// Get File Size
                push    0                       // Actual
                mov     ecx,esp
                push    [edi].TextRec.Handle    // [1]:DWord = File Handle
                push    0                       // [2]:DWord = Distance
                push    2                       // [3]:DWord = Method (EOF)
                push    ecx                     // [4]:DWord = @Actual
                Call    SysFileSeek
                pop     esi                     // File Size
// Restore current position
                push    0                       // Actual
                mov     ecx,esp
                push    [edi].TextRec.Handle    // [1]:DWord = File Handle
                push    ebx                     // [2]:DWord = Distance
                push    0                       // [3]:DWord = Method (Start file)
                push    ecx                     // [4]:DWord = @Actual
                Call    SysFileSeek
                pop     ebx                     // Current file position
                test    eax,eax
                jz      @@RET                   // CF = 0
              @@ERROR:
                Call    SetInOutRes
                xor     ebx,ebx
                xor     esi,esi
                stc                             // CF := 1
              @@RET:
end;

procedure _FilePos(FileVar: Pointer); {&USES ebx,edx,esi,edi} {&FRAME-}
asm
                mov     edi,FileVar
                Call    GetFileInfo             // Returns: ebx = File Position
                mov     eax,ebx
                jc      @@RET
                mov     esi,[edi].TextRec.BufSize
                cmp     esi,1
                jbe     @@RET                   // if RecSize > 1 then
                xor     edx,edx                 //   Result := FilePos / RecSize
                div     esi
              @@RET:
end;

procedure _FileSize(FileVar: Pointer); {&USES ebx,edx,esi,edi} {&FRAME-}
asm
                mov     edi,FileVar
                Call    GetFileInfo             // Returns: ecx = File Size
                mov     eax,esi
                jc      @@RET
                mov     esi,[edi].TextRec.BufSize
                cmp     esi,1
                jbe     @@RET                   // if RecSize > 1 then
                xor     edx,edx                 //   Result := FileSize / RecSize
                div     esi
              @@RET:
end;

procedure _Eof(FileVar: Pointer); {&USES ebx,esi,edi} {&FRAME-}
asm
                mov     edi,FileVar
                Call    GetFileInfo
                mov     al,false
                jc      @@RET
                cmp     ebx,esi
                sete    al                      // EOF := FilePos = FileSize;
              @@RET:
end;

{$EndIf LargeFileSupport}

// Checks that file is open
// EXPECTS:     edi     = File Variable @
// RETURNS:     ZF      = 1 if file is opened

procedure OpenCheck; {&USES eax} {&FRAME-}
asm
                cmp     [edi].TextRec.Mode,fmInOut
                je      @@RET
                mov     eax,RTE_File_Not_Open
                Call    SetInOutRes
              @@RET:
end;

// Assign standard procedure (typed and untyped files)
// procedure Assign(var F; String);

// EXPECTS:      dl     = String/PChar flag
//              esi     = File Name String/PChar
//              edi     = File Variable @

procedure Assign_File; {&USES eax,ecx} {&FRAME-}
asm
                cld
                xor     eax,eax                      // Initialize file
                mov     [edi].TextRec.Handle,eax     // variable
                mov     [edi].TextRec.Mode,fmClosed
                add     edi,TextRec.BufSize
                mov     ecx,(TextRec.Name - TextRec.BufSize)/4
                rep     stosd
                mov     ecx,TYPE TextRec.Name - 1
                test    dl,dl                        // PChar ?
                jnz     @@1                          // Yes, @@1
                movzx   eax,[esi].Byte               // No, Get string length
                inc     esi
                mov     ecx,eax
                test    ecx,ecx
                jz      @@Done
              @@1:
                mov     al,[esi]
                inc     esi
                test    al,al
                jz      @@Done
                mov     [edi],al
                inc     edi
                dec     ecx
                jnz     @@1
              @@Done:
                mov     al,0                         // Terminate it with #0
                mov     [edi],al
                inc     edi
end;

procedure _FileAssign(FileVar,S: Pointer); {&USES edx,esi,edi} {&FRAME-}
asm
                mov     edi,FileVar
                mov     esi,S
                mov     dl,0            // Flag: 0 = String
                Call    Assign_File
end;

procedure _FileAssignPCh(FileVar,S: Pointer); {&USES edx,esi,edi} {&FRAME-}
asm
                mov     edi,FileVar
                mov     esi,S
                mov     dl,1            // Flag: 1 = PChar
                Call    Assign_File
end;

// Reset:   Reset standard procedure
// Rewrite: Rewrite standard procedure
// procedure Reset(var F [:File; RecSize:Longint]);
// procedure Rewrite(var F [:File; RecSize:Longint]);
//      where F is file variable other than TEXT.
//
// Sets InOutRes <> 0 if error occurred

procedure OpenFile; {&USES None} {&FRAME-}
asm
                mov     esi,eax
                cmp     [edi].TextRec.Mode,fmClosed
                je      @@OK
                mov     eax,RTE_File_Not_Assigned
                cmp     [edi].TextRec.Mode,fmInOut
                jne     @@Error
                push    edi                             // [1]:Pointer=FileVar
                Call    _FileClose
              @@OK:
                lea     edx,[edi].TextRec.Name
                cmp     [edx].Byte,0                    // Is file name empty ?
                je      @@Done                          // Yes, StdIn or StdOut
                push    OFFSET FileMode
                Call    _GetTlsVar
                test    cl,cl                           // Open or Create ?
                push    0                               // Handle
                mov     ecx,esp
                jnz     @@Create
// Reset -> Open an existing file
                mov     eax,[eax]               // Open mode for Reset
                push    edx                     // [1]:PChar = FileName
                push    eax                     // [2]:DWord = Mode
                push    ecx                     // [3]:DWord = @Handle
                Call    SysFileOpen
                pop     esi                     // Handle
                jmp     @@2
// Rewrite -> Create a file
              @@Create:
                mov     eax,[eax+4]             // FileModeReadWrite
                test    eax,eax
                jnz     @@1
                mov     al,42h
              @@1:
                push    edx                     // [1]:PChar = FileName
                push    eax                     // [2]:DWord = Mode
                push    0                       // [3]:DWord = Attr = Normal file
                push    ecx                     // [4]:DWord = @Handle
                Call    SysFileCreate
                pop     esi                     // Handle
              @@2:
                test    eax,eax
                jz      @@Done
              @@Error:
                Call    SetInOutRes
                jmp     @@RET
              @@Done:
                mov     [edi].TextRec.Mode,fmInOut
                mov     [edi].TextRec.Handle,esi
                mov     [edi].TextRec.BufSize,ebx       // Record Size
              @@RET:
end;

procedure _FileReset(FileVar: Pointer; RecSize: Longint); {&USES ALL} {&FRAME-}
asm
                Call    SysFileStdIn
                mov     edi,FileVar
                mov     ebx,RecSize
                mov     cl,0
                Call    OpenFile
end;

procedure _FileRewrite(FileVar: Pointer; RecSize: Longint); {&USES ALL} {&FRAME-}
asm
                Call    SysFileStdOut
                mov     edi,FileVar
                mov     ebx,RecSize
                mov     cl,1
                Call    OpenFile
end;

// Truncate standard procedure (typed and untyped files)
// procedure Truncate(var F);
// Sets InOutRes <> 0 if error occurred

procedure _FileTrunc(FileVar: Pointer); {&USES ALL} {&FRAME-}
asm
                mov     edi,FileVar
                Call    OpenCheck               // Is file Opened ?
                jne     @@RET                   // No, exit with error
                push    [edi].TextRec.Handle
                Call    TruncateFile
                test    eax,eax
                jz      @@RET
                Call    SetInOutRes
              @@RET:
end;

// Seek standard procedure (typed and untyped files)
// procedure Seek(var F; N: TFileSize);
// Sets InOutRes <> 0 if error occurred

{$IfDef LargeFileSupport}
procedure _FileSeek_dummy(FileVar: Pointer; FilePos: Longint);
  {&Frame-}{&Uses None}
  asm
    // dummy for compiler magic, should never called!
  end;

procedure Seek(Var F; FilePos: TFileSize);
  {&Frame-}{&Uses All}
  asm
                mov     edi,F
                Call    OpenCheck
                jne     @@RET

                fild    FilePos                 // st0       = FilePos

                push    eax                     // Actual, Bit 63..32
                push    eax                     // Actual, Bit 31..00
                mov     ecx,esp

                push    [edi].TextRec.Handle    // [1]       = File Handle

                push    eax                     // [2]       = Distance, Bit 63..32
                push    eax                     //           = Distance, Bit 31..00

                fild    [edi].TextRec.BufSize
                fmulp                           // FilePos*BufSize
                fistp   [esp].TFileSize         // setup distance in bytes
                wait

                push    0                       // [3]:DWord = Method(Start file)
                push    ecx                     // [4]:DWord = @Actual
                Call    SysFileSeek

                pop     ecx                     // pop away actual^
                pop     ecx

                test    eax,eax
                jz      @@RET
                Call    SetInOutRes
              @@RET:
  end;
{$Else LargeFileSupport} {32 bit}

procedure _FileSeek(FileVar: Pointer; FilePos: TFileSize); {&USES ALL} {&FRAME-}
asm
                mov     edi,FileVar
                Call    OpenCheck
                jne     @@RET
                mov     eax,FilePos             // FilePtr := FilePos * BufSize
                mul     [edi].TextRec.BufSize   // (edx:eax)
                push    0                       // Actual
                mov     ecx,esp
                push    [edi].TextRec.Handle    // [1]:DWord = File Handle
                push    eax                     // [2]:DWord = Distance
                push    0                       // [3]:DWord = Method(Start file)
                push    ecx                     // [4]:DWord = @Actual
                Call    SysFileSeek
                pop     ecx
                test    eax,eax
                jz      @@RET
                Call    SetInOutRes
              @@RET:
end;

{$EndIf LargeFileSupport}


// Close standard procedure (typed and untyped files)
// procedure Close(var F);
// Sets InOutRes <> 0 if error occurred

procedure _FileClose(FileVar: Pointer); {&USES eax,ecx,edx,edi} {&FRAME-}
asm
                mov     edi,FileVar
                Call    OpenCheck               // Is file Opened ?
                jne     @@RET                   // No, exit with error
                push    [edi].TextRec.Handle
                Call    SysFileClose
                test    eax,eax
                jz      @@OK
                Call    SetInOutRes
              @@OK:
                mov     [edi].TextRec.Mode,fmClosed
              @@RET:
end;

// _FileRead:  Read  standard procedure (typed files)
// _FileWrite: Write standard procedure (typed files)
// Sets InOutRes <> 0 if error occurred

// Performs typed file I/O
// EXPECTS:      eax     = Error Code
//               edx     = Buffer @
//               edi     = File Variable @

procedure InOutFile; {&USES ecx} {&FRAME-}
asm
                Call    OpenCheck               // Is file Opened ?
                jne     @@RET                   // No, exit with error
                push    eax                     // Save Error code
                push    0                       // Bytes Read
                mov     ecx,esp
                push    [edi].TextRec.Handle    // [1]:DWord = File Handle
                push    edx                     // [2]:PChar = Buffer
                push    [edi].TextRec.BufSize   // [3]:DWord = Count
                push    ecx                     // [4]:DWord = @ByteRead
                cmp     eax,RTE_Disk_Write_Error
                je      @@Write
                Call    SysFileRead
                jmp     @@Done
              @@Write:
                Call    SysFileWrite
              @@Done:
                pop     ecx                     // Actual bytes done
                pop     edx                     // Error code
                test    eax,eax
                jnz     @@ERROR
                cmp     ecx,[edi].TextRec.BufSize // All data processed?
                je      @@RET
                mov     eax,edx                 // No, set I/O Error
              @@ERROR:
                Call    SetInOutRes
              @@RET:
end;

procedure _FileRead(FileVar,Buffer: Pointer); {&USES eax,edx,edi} {&FRAME-}
asm
                mov     eax,RTE_Disk_Read_Error
                mov     edi,FileVar
                mov     edx,Buffer
                Call    InOutFile
                PopArgs @Params - TYPE FileVar
end;

procedure _FileWrite(FileVar,Buffer: Pointer); {&USES eax,edx,edi} {&FRAME-}
asm
                mov     eax,RTE_Disk_Write_Error
                mov     edi,FileVar
                mov     edx,Buffer
                Call    InOutFile
                PopArgs @Params - TYPE FileVar
end;

// BlockRead:  BlockRead  standard procedure (untyped files)
// BlockWrite: BlockWrite standard procedure (untyped files)
// procedure BlockRead (var F: file; var Buf; Cnt: Longint; Res:Longint);
// procedure BlockWrite(var F: file; var Buf; Cnt: Longint; Res:Longint);
// Sets InOutRes <> 0 if error occurred
// If result address is <> 0 then number of bytes that have been
// actually processed is stored in Result

// Performs untyped file I/O }

// EXPECTS:     esi     = Error code
//              edi     = File Variable @
//              ecx     = Count
//              edx     = Buffer @
//              ebx     = Result @

procedure InOutBlock; {&USES None} {&FRAME-}
asm
                test    ebx,ebx
                jz      @@1
                and     DWord Ptr [ebx],0       // Result := 0
              @@1:
                Call    OpenCheck               // Is file Opened ?
                jne     @@RET                   // No, exit with error(Res=0)
                mov     eax,ecx
                test    ecx,ecx
                jz      @@2
                push    edx
                mul     [edi].TextRec.BufSize
                pop     edx
                mov     ecx,eax                 // ecx := Number of bytes
                push    ebx                     // @Result
                push    ecx                     // Count
                push    0                       // Bytes Read
                mov     eax,esp
                push    [edi].TextRec.Handle    // [1]:DWord = File Handle
                push    edx                     // [2]:PChar = Buffer
                push    ecx                     // [3]:DWord = Count
                push    eax                     // [4]:DWord = @ByteRead
                cmp     esi,RTE_Disk_Write_Error
                je      @@Write
                Call    SysFileRead
                jmp     @@Done
              @@Write:
                Call    SysFileWrite
              @@Done:
                pop     ecx                     // Bytes processed
                pop     edx                     // Count
                pop     ebx                     // @Result
                test    eax,eax
                jnz     @@ERROR
                mov     eax,edx
              @@2:
                test    ebx,ebx                 // @Result = nil ?
                jz      @@3
                mov     eax,ecx                 // Bytes processed
                xor     edx,edx
                div     [edi].TextRec.BufSize
                mov     [ebx],eax               // Return result
                jmp     @@RET
              @@3:
                cmp     ecx,eax                 // if Actual <> Count then
                je      @@RET                   //   Return error
                xchg    eax,esi
              @@ERROR:
                Call    SetInOutRes
              @@RET:
end;

procedure _BlockRead(FileVar,Buffer: Pointer; Count: Longint; Result: Pointer); {&USES ALL} {&FRAME-}
asm
                mov     esi,RTE_Disk_Read_Error
                mov     edi,FileVar
                mov     ecx,Count
                mov     edx,Buffer
                mov     ebx,Result
                Call    InOutBlock
end;

procedure _BlockWrite(FileVar,Buffer: Pointer; Count: Longint; Result: Pointer); {&USES ALL} {&FRAME-}
asm
                mov     esi,RTE_Disk_Write_Error
                mov     edi,FileVar
                mov     ecx,Count
                mov     edx,Buffer
                mov     ebx,Result
                Call    InOutBlock
end;

//±±±±±±±±±±±±±±[ COMMAND LINE PARAMETERS ]±±±±±±±±±±±±±±±±±±±±±±±±±±±±±

function ParamCount: Longint;
begin
  Result := SysCmdlnCount;
end;

function ParamStr(Index: Longint): ShortString;
begin
  SysCmdlnParam(Index, Result);
end;

//±±±±±±±±±±±±±±±[ SYSTEM INDEPENDENT INTERFACE ]±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±

// Exit procedure handling

type
  PExitProcInfo = ^TExitProcInfo;
  TExitProcInfo = record
    Next: PExitProcInfo;
    SaveExit: Pointer;
    Proc: TProcedure;
  end;

const
  ExitProcList: PExitProcInfo = nil;

procedure DoExitProc;
var
  P: PExitProcInfo;
  Proc: TProcedure;
begin
  P := ExitProcList;
  ExitProcList := P^.Next;
  ExitProc := P^.SaveExit;
  Proc := P^.Proc;
  Dispose(P);
  Proc;
end;

procedure AddExitProc(Proc: TProcedure);
var
  P: PExitProcInfo;
begin
  New(P);
  P^.Next := ExitProcList;
  P^.SaveExit := ExitProc;
  P^.Proc := Proc;
  ExitProcList := P;
  ExitProc := @DoExitProc;
end;

procedure DoExitProcs; {&USES ALL} {&FRAME-}
asm
              @@1:
                mov     ecx,ExitProc
                test    ecx,ecx
                jz      @@RET
                xor     eax,eax
                mov     ExitProc,eax            // Clear ExitProc
                Call    SetInOutRes             // Clear InOutRes
                Call    ecx
                jmp     @@1
              @@RET:
end;

// Standard exit procedure

procedure _ExitProc;
begin
  if IsDll then exit;
  _TxtClose(@Input);
  _TxtClose(@Output);
end;

// Standard initialization

procedure DoInit; assembler; {&USES NONE} {&FRAME-}
const
  EmptyStr: Byte = 0;
asm
// Assign(Input, ''); Reset(Input);
                mov     eax,OFFSET Input
                push    eax                     // [1]:Pointer = FileVar(Reset)
                push    eax                     // [1]:Pointer = FileVar
                push    OFFSET EmptyStr         // [2]:Pointer = Name
                Call    _TxtAssign
                Call    _TxtReset
// Assign(Output, ''); Rewrite(Output);
                mov     eax,OFFSET Output
                push    eax                     // [1]:Pointer = FileVar(Rewrite)
                push    eax                     // [1]:Pointer = FileVar
                push    OFFSET EmptyStr         // [2]:Pointer = Name
                Call    _TxtAssign
                Call    _TxtRewrite
                mov     ExitProc,OFFSET _ExitProc // Default ExitProc
                Call    _FpuInit
                Call    SysCmdln
                mov     CmdLine,eax
end;

// Converts hexadecimal number to string
// EXPECTS:     eax     = Number
//              edi     = Buffer pointer

procedure Hex2Str; {&USES None} {&FRAME-}
asm
                mov     cl,8
              @@1:
                rol     eax,4
                push    eax
                and     al,0Fh
//              add     al,'0'
//              cmp     al,'9'
//              jbe     @@2
//              add     al,'A'-'0'-10
//            @@2:
                                        // Norbert Juffa:
                cmp     al,10           // if x < 10, set CF = 1
                sbb     al,69h          // 0-9: 96h .. 9Fh,  A-F: A1h..A6h
                das                     // 0-9: subtr. 66h -> 30h..39h,
                                        // A-F: subtr. 60h -> 41h..46h
                mov     [edi],al
                inc     edi
                pop     eax
                dec     cl
                jnz     @@1
end;

// RunError standard procedure
// procedure RunError[(ErrorCode: Longint)];
// EXPECTS:     eax     = Error Code
//              Error address on stack

procedure _RunError(ErrorCode: Longint); {&USES None} {&FRAME-}
asm
                mov     eax,ErrorCode
                pop     ecx             // ecx = Return address
                jmp     _Terminate
end;

// Halt standard procedure
// procedure Halt[(ExitCode: Longint)];
// EXPECTS:     eax     = Error Code
//              Error address on stack

procedure _Halt(ExitCode: Longint); {&USES None} {&FRAME-}
asm
                mov     eax,ExitCode    // eax = Exit Code
                xor     ecx,ecx         // ecx = Return address
                jmp     _Terminate
end;

// Converts error number to run-time error
// EXPECTS:     al      = reXXX style error code
//              [ESP]   = Error address on stack

procedure RtlError; {&USES None} {&FRAME-}
asm
                and     eax,7Fh
                mov     ecx,ErrorProc
                test    ecx,ecx
                jz      @@1
                mov     edx,[esp]
                push    eax             // [1]:Byte    = Error number
                push    edx             // [2]:Pointer = Error address
                Call    ecx
              @@1:
                dec     eax
                mov     al,Byte Ptr @@ErrorTable[eax]
                jns     @@2
                push    OFFSET InOutRes // reInOutError
                Call    _GetTlsVar
                mov     eax,[eax]
              @@2:
                pop     edx
                push    eax             // [1]:Error number
                push    edx             // Return address
                jmp     _RunError
@@ErrorTable:   db      RTE_Heap_Overflow          // reOutOfMemory
                db      RTE_Invalid_Pointer        // reInvalidPtr
                db      RTE_Zero_Divide            // reDivByZero
                db      RTE_Range_Check            // reRangeError
                db      RTE_Integer_Overflow       // reIntOverflow
                db      RTE_Invalid_FP_Operation   // reInvalidOp
                db      RTE_Zero_Divide            // reZeroDivide
                db      RTE_FP_Overflow            // reOverflow
                db      RTE_FP_Underflow           // reUnderflow
                db      RTE_Invalid_Cast           // reInvalidCast
                db      RTE_Access_Violation       // reAccessViolation
                db      RTE_Stack_Overflow         // reStackOverflow
                db      RTE_Signal                 // reSignal
                db      RTE_Privileged_Instruction // rePrivilegedInstr
end;

{$IfDef Linux}
const
  SignalNameLow  =  1; // SIGHUP
  SignalNameHigh = 27; // SIGPROF
  SignalName: array[SignalNameLow..SignalNameHigh] of array[1..6] of Char=
              ('HUP'   ,
               'INT'   ,
               'QUIT'  ,
               'ILL'   ,
               'TRAP'  ,
               'ABRT'  ,
               'BUS'   , {not used}
               'FPE'   ,
               'KILL'  ,
               'USR1'  ,
               'SEGV'  ,
               'USR2'  ,
               'PIPE'  ,
               'ALRM'  ,
               'TERM'  ,
               'STKFLT',
               'CHLD'  ,
               'CONT'  ,
               'STOP'  ,
               'TSTP'  ,
               'TTIN'  ,
               'TTOU'  ,
               'URG'   ,
               'XCPU'  ,
               'XFSZ'  ,
               'VTALRM',
               'PROF'  );
{$EndIf Linux}

procedure _RunErrorStr(var ErrStr: ShortString); assembler; {&USES esi,edi} {&FRAME-}
const
   RuntimeStr: array [1..14] of Char = 'Runtime error ';
   Copyright:  array [1..54] of Char = 'Virtual Pascal - Copyright (C) 1996-2000 vpascal.com';
asm
                cld
                mov     edi,ErrStr
                push    edi
                inc     edi
                mov     esi,OFFSET RuntimeStr   // 'Runtime error '
                mov     ecx,TYPE RuntimeStr
                rep     movsb
                mov     eax,ExitCode
                Call    Int2Str
                mov     [edi].Longint,' ta '    // ' at '
                add     edi,4
                mov     eax,ErrorAddr
                Call    Hex2Str
                {$IFDEF DPMI32}
                push    eax                     // save ErrorAddr

                // ' (Base=........ rel=........) '
                mov     [edi].Longint,'aB( '    // ' (Ba' 'se=_'
                mov     [edi+4].Longint,' =es'
                add     edi,8
                dec     edi
                mov     eax,code_base
                Call    Hex2Str

                mov     [edi].Byte,' '          // ' ','rel='
                mov     [edi+1].Longint,'=ler'
                add     edi,5
                pop     eax
                push    eax
                sub     eax,code_base
                add     eax,$401000
                Call    Hex2Str

                mov     [edi].Word,' )'         // ') '
                add     edi,2

                pop     eax                     // restore ErrorAddr
                {$ELSE}
                mov     [edi].Byte,' '
                inc     edi
                {$ENDIF}
                push    eax
                mov     ecx,esp
                push    eax                     // [1]:Addr
                push    edi                     // [2]:FileName
                push    ecx                     // [3]:Line#
                Call    GetLocationInfo
                pop     ecx                     // Line#
                dec     edi
                test    eax,eax
                jz      @@1
                inc     edi
                movzx   eax,[edi].Byte
                mov     [edi].Byte,'('
                lea     edi,[edi+eax+1]
                mov     [edi].Byte,'#'
                inc     edi
                xchg    eax,ecx
                Call    Int2Str
                mov     al,')'
                mov     [edi],al
                inc     edi
              @@1:
                mov     ecx,ExceptionNo
                {$IfDef Linux}
                // Linux signal - $C0ss????
                mov     eax, ecx
                shr     eax, 16
                cmp     ah,$c0
                jne     @@NotSignalException
                // we provide only a limited number of signal name strings
                cmp     al, SignalNameHigh
                ja      @@NotSignalException
                sub     al, SignalNameLow
                jb      @@NotSignalException
                movzx   eax, al
                imul    eax, eax, 6
                lea     esi, SignalName[eax]
                mov     [edi].Longint,'GIS '    // ' SIG'
                add     edi, 4
                cld
                jmp @@SignalNameLoopEntry
              @@SignalNameLoop:                 // 'SEGV'
                stosb
              @@SignalNameLoopEntry:
                lodsb
                cmp al,0
                jne @@SignalNameLoop
                jmp @@WriteError

              @@NotSignalException:
                {$EndIf Linux}
                test    ecx,ecx
                jz      @@WriteError            // Exception ?
                mov     [edi].Longint,'xE( '    // ' (Exception '
                mov     [edi+4].Longint,'tpec'  //  ÀÄÄÙÀÄÄÙÀÄÄÙ
                mov     [edi+8].Longint,' noi'
                add     edi,12
                xchg    eax,ecx
                Call    Hex2Str
                mov     [edi].Byte,')'          // ')'
                inc     edi

              @@WriteError:
                mov     [edi].Longint,'DIT '    // ' TID=#'
                mov     [edi+4].Byte,'='
                add     edi,5
                Call    GetThreadId
                Call    Int2Str
                mov     [edi].Longint,0A0D00h + '.' // '.', CR, LF, #0 (Zero terminated)
                add     edi,4
                dec     edi
                pop     eax
                sub     edi,eax
                lea     ecx,[edi-1]
                mov     [eax],cl
end;

// EXPECTS:     eax     = Exit Code
//              ecx     = Return address

procedure _Terminate; {&USES None} {&FRAME-}
asm
                mov     ExitCode,eax
                mov     ErrorAddr,ecx
                Call    DoExitProcs
                cmp     ErrorAddr,0
                jz      @@NoError
                sub     esp,256
                mov     eax,esp
                push    eax                     // Result: String
                push    eax
                Call    _RunErrorStr
                Call    SysFileStdErr
                pop     edx
                movzx   ecx,[edx].Byte
                inc     edx
                push    0                          // Actual
                mov     esi,esp
                push    eax                        // [1]:DWord = Handle
                push    edx                        // [2]:PChar = Buffer
                push    ecx                        // [3]:DWord = Count
                push    esi                        // [4]:DWord = @ByteWrite
                Call    SysFileWrite
                pop     eax
              @@NoError:
                push    ExitCode                // [1]:Longint = Exit Code
                Call    SysCtrlExitProcess
end;

//±±±±±±±±±±±±±±±[ Threads and Thread Local Storage ]±±±±±±±±±±±±±±±±±±±±±±±±

// Creates a new thread and installs system exception handler for it.

type
  PThreadRec = ^TThreadRec;
  TThreadRec = record
    Func: TThreadFunc;
    Param: Pointer;
  end;

function BeginThread(SecurityAttributes: Pointer; StackSize: Longint;
                     ThreadFunc: TThreadFunc; Parameter: Pointer;
                     CreationFlags: Longint; var ThreadId: Longint): Longint;
var
  P: PThreadRec;
begin
  IsMultiThread := True;
  New(P);
  P^.Func := ThreadFunc;
  P^.Param := Parameter;
  Result := SysCtrlCreateThread(SecurityAttributes,
    StackSize, @ThreadStartup, P, CreationFlags, ThreadId);
end;

// Terminates the current thread. Note that control does not return to
// the thread code. Thread also terminates after final 'end' of the
// thread's function statement part.

procedure EndThread(ExitCode: Longint); {&USES None} {&FRAME-}
asm
                jmp     SysCtrlExitThread
end;

// Temporarily suspends execution of the thread until ResumeThread is
// issued. Returns 0 if the operation was successful, or non-zero
// System error code otherwise

function SuspendThread(Handle: Longint): Longint; {&USES None} {&FRAME-}
asm
                jmp     SysCtrlSuspendThread
end;

// Restarts the thread that was previously stopped by SuspendThread.

function ResumeThread(Handle: Longint): Longint; {&USES None} {&FRAME-}
asm
                jmp     SysCtrlResumeThread
end;

// Terminates another thread in the current process. If thread 1 is
// specified, the entire process terminates.

function KillThread(Handle: Longint): Longint; {&USES None} {&FRAME-}
asm
                jmp     SysCtrlKillThread
end;

const
  escFileName   = $3F;
  escLineNo     = $3E;
  escLineOfs    = $3D;
  escEnd        = $3C;
  escFirst      = $3C;

type
  TPointerList = Array[1..10] of Pointer;
  PPointerList = ^TPointerList;

  PSharedMem = ^TSharedMem;
  TSharedMem = record
    TlsPerThread  : PPointerList;   // Actual TLS
    MaxThreadCount: Longint;        // Max thread ID so far
    MaxThreadId   : Longint;        // Updated before MaxThreadCount
    TlsMemMgr     : TMemoryManager; // Memory Manager used by Tls Mgr
    HeapControl   : THeapControl;   // For heap synchronization
  end;

  PModuleEntry = ^TModuleEntry;
  TModuleEntry = record
    TlsStart:  Longint;
    TlsSize:   Longint;  // (*)
    CodeStart: Cardinal;
    LocInfo:   PChar;
  end;

  PThreadEntry = ^TThreadEntry;
  TThreadEntry = record
    Next: PThreadEntry;  // Next entry for thread, for next module
    TlsSize: Longint;    // Duplicate of (*)
    Data: record end;
  end;

function GetLocationInfo(Addr: Pointer; var AFileName: ShortString; var ALineNo: Longint): Pointer;
var
  P,FileName: PChar;
  LastOfs: Pointer;
  I,CodeStart,LineNo,StartLineNo,LastLineNo: Longint;
  NewFile: Boolean;
  AOfs: Cardinal absolute Addr;
begin
  LineNo := -1;
  Result := Ptr(-1);
  LastLineNo := 0;
  LastOfs := nil;
  P := PChar(TlsSharedMem) + SizeOf(TSharedMem);
  LastOfs:=P;
  while PModuleEntry(P)^.TlsStart <> -1 do
  begin
    if (PModuleEntry(P)^.CodeStart <= AOfs) and
       (PModuleEntry(P)^.CodeStart > PModuleEntry(LastOfs)^.CodeStart) then
      LastOfs := P;
    Inc(P, SizeOf(TModuleEntry));
  end;
  P := LastOfs;
  if (PModuleEntry(P)^.TlsStart <> -1) and (PModuleEntry(P)^.CodeStart <= AOfs) then
  begin
    if PModuleEntry(P)^.CodeStart <= AOfs then
    begin
      Dec(AOfs, PModuleEntry(P)^.CodeStart);
      CodeStart := PModuleEntry(P)^.CodeStart;
      P := PModuleEntry(P)^.LocInfo;
      if P <> nil then
        repeat
          case Ord(P^) of

            escEnd:
              Break;

            escFileName:
              begin
                Inc(P);
                FileName := P;
                Inc(P, Ord(P^) + 1);
                NewFile := True;
                Continue;
              end;

            escLineNo:
              begin
                Inc(LineNo, PSmallWord(@P[1])^);
                Inc(P, 3);
                Continue;
              end;

            escLineOfs:
              begin
                Inc(LineNo, PSmallWord(@P[1])^);
                Inc(Longint(Result), PLongint(@P[3])^);
                Inc(P, 7);
              end;

            else
              Inc(LineNo, PByte(P)^ shr 6);
              Inc(Longint(Result), PByte(P)^ and $3F);
              Inc(P);
          end;
          if Longint(Result) = AOfs then
          begin
            LastOfs := Result;
            LastLineNo := LineNo;
          end;
          if Longint(Result) >= AOfs then
            if (Longint(Result) - AOfs <= 10) or not NewFile then
              begin
                AFileName[0] := FileName^;        // Copy the string length
                I := 0;
                while I < Ord(FileName^) do       // Decode the name itself
                begin
                  Inc(I);
                  AFileName[I] := Chr(Ord(FileName[I]) xor $AA);
                end;
                ALineNo := SmallWord(LastLineNo);
                Result := Ptr(Longint(LastOfs) + CodeStart);
                Exit;
              end
            else
              Break;
          LastOfs := Result;
          LastLineNo := LineNo;
          NewFile := False;
        until False;
    end;
  end;
  Result := nil;
end;

function _TlsMemNew(_MemMgr: PMemoryManager; _Size: Longint): Pointer; {&USES esi} {&Frame-}
asm
                mov     eax,_Size
                test    eax,eax
                jz      @@RET
                mov     esi,_MemMgr
                push    eax
                call    [esi].TMemoryManager.GetMem
                test    eax,eax
                jnz     @@RET                   // Success?
                add     esp,@Uses               // No, report a run-time error
                mov     al,reOutOfMemory
                jmp     RtlError
              @@RET:
end;

// Reallocate Tls-per-thread pointer list, if necessary

procedure RightSizeTlsPerThread(_UpdateMaxCount: Boolean);
const
  THREAD_INCREMENT = 256;
var
  Count: Longint;
  OldSize: Longint;
  P: Pointer;
  MemMgr: TMemoryManager;
begin
  EnterHeap;
  Count := GetThreadId;
  with PSharedMem(TlsSharedMem)^ do
    if Count > MaxThreadCount then
      begin
        MemMgr := MemoryManager;
        MemoryManager := TlsMemMgr;
        GetMem(P, 4*Count);
        FillChar(P^, 4*Count, 0);
        if TlsPerThread <> nil then
          begin
            OldSize := MaxThreadCount*4;
            Move(TlsPerThread^, P^, OldSize);
            FreeMem(TlsPerThread);
          end;
        TlsPerThread := P;
        if _UpdateMaxCount then
          MaxThreadCount := Count;
        MemoryManager := MemMgr;
      end;
  LeaveHeap;
end;

// Free TLS allocated for this thread - thread is terminating

procedure FreeTLS;
var
  ID: Longint;
  p: PThreadEntry;
  pNext: PThreadEntry;
  MemMgr: TMemoryManager;

begin
  EnterHeap;
  ID := GetThreadID;
  with PSharedMem(TlsSharedMem)^ do
    begin
      MemMgr := MemoryManager;
      MemoryManager := TlsMemMgr;

      p := TlsPerThread^[ID];
      if assigned(p) then
        begin
          TlsPerThread^[ID] := nil;
          while assigned(p) do
            begin
              pNext := p^.Next;
              FreeMem(p);
              p := pNext;
            end;
        end;

      MemoryManager := MemMgr;
    end;
  LeaveHeap;
end;


// Set default values of tls for thread
procedure ThreadvarDefaults;
begin
  FileMode          := $042; // open_access_ReadOnly  or open_share_DenyNone
  FileModeReadWrite := $042; // open_access_ReadOnly  or open_share_DenyNone
  // using +$100 (OS/2: open_flags_Sequential, Win32: FILE_FLAG_SEQUENTIAL_SCAN)
  // did not have an noticeable effect
  TextModeRead      := $040; // open_access_ReadOnly  or open_share_DenyNone
  TextModeReadWrite := $042; // open_access_ReadWrite or open_share_DenyNone
end;

// Allocate TLS for a newly started thread

procedure AllocateTls_NewThread;
var
  pModule: PModuleEntry;
  pHead: PThreadEntry;
  p: PThreadEntry;
  ID: Longint;
  MemMgr: TMemoryManager;
  Bytes: Longint;
begin
  ID := GetThreadId;
  // Determine if any action is necessary
  with PSharedMem(TlsSharedMem)^ do
    if (ID <= MaxThreadCount) and (TlsPerThread^[ID] <> nil) then
      exit;

  EnterHeap;
  pHead := nil;
  MemMgr := MemoryManager;
  MemoryManager := PSharedMem(TlsSharedMem)^.TlsMemMgr;
  pModule := PModuleEntry(PChar(TlsSharedMem) + SizeOf(TSharedMem));
  while pModule^.TlsSize <> -1 do
    begin
      Bytes := pModule^.TlsSize + SizeOf(TThreadEntry);
      if pHead = nil then
        begin
          GetMem( pHead, Bytes );
          p := pHead;
        end
      else
        begin
          GetMem( p^.Next, Bytes );
          p := p^.Next;
        end;
      FillChar(P^, Bytes, 0);
      p^.TlsSize := pModule^.TlsSize;
      inc(pModule);
    end;

  // Now add pHead to the end of the TlsPerThread list
  RightSizeTlsPerThread(False);
  with PSharedMem(TlsSharedMem)^ do
    begin
      TlsPerThread^[ID] := pHead;
      if ID > MaxThreadCount then
        MaxThreadCount := ID;
    end;
  MemoryManager := MemMgr;
  LeaveHeap;
  ThreadvarDefaults;
end;

// Allocate more TLS for each thread when a new module is loaded
// This assumes the global block is already updated with module info

procedure AllocateTls_NewModule;
var
  pModule: PModuleEntry;
  pPrevModule: PModuleEntry;
  pCurModule: PModuleEntry;
  pPrevThread: PThreadEntry;
  pCurThread: PThreadEntry;
  tid: Longint;
  MemMgr: TMemoryManager;
  Bytes: Longint;
begin
  EnterHeap;
  MemMgr := MemoryManager;
  MemoryManager := PSharedMem(TlsSharedMem)^.TlsMemMgr;

  pModule := PModuleEntry(PChar(TlsSharedMem) + SizeOf(TSharedMem));
  // pPrev is now last module added; add its TLS for all threads
  RightSizeTlsPerThread(True);
  with PSharedMem(TlsSharedMem)^ do
    for tid := 1 to MaxThreadCount do
      begin
        if TlsPerThread^[tid] <> nil then
          begin
            pPrevThread := nil;
            pCurThread := TlsPerThread^[tid];
            pPrevModule := nil;
            pCurModule := pModule;
            while (pCurThread <> nil) and (pCurModule^.TlsSize <> -1) do
              begin
                pPrevModule := pCurModule;
                inc(pCurModule);
                pPrevThread := pCurThread;
                pCurThread := pCurThread^.Next;
              end;
            if (pCurThread = nil) and (pPrevThread <> nil) and (pCurModule^.TlsSize <> -1) then
              begin
                Bytes := pCurModule^.TlsSize + SizeOf(TThreadEntry);
                GetMem( pPrevThread^.Next, Bytes );
                pCurThread := pPrevThread^.Next;
              end
            else
              pCurThread := nil;
          end;
        if assigned(pCurThread) then
          begin
            FillChar(pCurThread^, Bytes, 0);
            pCurThread^.TlsSize := pCurModule^.TlsSize;
            ThreadvarDefaults;
          end;
      end;

  MemoryManager := MemMgr;
  LeaveHeap;
end;

// Adds TLS segment paramters for a current EXE or DLL to the TL table,
// located in the named shared memory
// EXPECTS:     eax     = Base TLS@
//              edx     = Unit segment map address

procedure AddToTls; {&USES ebx,esi,edi} {&FRAME-}
asm
                cld
                mov     ebx,eax
                lea     edi,[eax+TYPE TSharedMem]
                or      eax,-1
                mov     ecx,eax
                repne   scasd
                mov     eax,[edx+0]             // Starting@ of the TLS segment
                cmp     eax,[edx+4]
                je      @@1                     // No THREADVARs have been declared
                mov     [edi-4].TModuleEntry.TlsStart,eax
                sub     eax,[edx+4]             // Ending address
                neg     eax
                mov     [edi-4].TModuleEntry.TlsSize,eax
                mov     eax,[edx+8]             // Starting code offset
                mov     [edi-4].TModuleEntry.CodeStart,eax
                mov     eax,[edx+12]            // Location
                inc     eax                     // -1 => not available ?
                jz      @@NoLoc
                lea     eax,[edx+eax+12-1]      // relative offset
              @@NoLoc:
                mov     [edi-4].TModuleEntry.LocInfo,eax
              @@1:
// Calculates the total size of the TLS data found in the TLS table
                lea     edi,[ebx+TYPE TSharedMem]
                xor     ecx,ecx
              @@2:
                cmp     [edi].Longint,-1        // Done ?
                je      @@3
                add     ecx,[edi].TModuleEntry.TlsSize
                add     edi,TYPE TModuleEntry
                jmp     @@2
              @@3:
                sub     edi,ebx
                mov     TlsSharedMemSize,edi
                call    AllocateTls_NewThread   // Make sure TLS exists for current thread
                call    AllocateTls_NewModule   // Allocate more TLS for other threads
end;

// Clears the address the TLS segment in the shared memory region map
// so if the other DLL will be loaded later on by DosLoadModule
// which have the same TLS segment address, the old Tls segment will
// be ignored.
// EXCEPTS:     ecx     = Unit segment map address

procedure RemoveFromTls; {&USES None} {&FRAME-}
asm
                mov     edx,TlsSharedMem
                add     edx,TYPE TSharedMem
              @@1:
                cmp     [edx].Longint,-1        // Done ?
                je      @@2
                add     edx,TYPE TModuleEntry
                mov     eax,[edx-TYPE TModuleEntry].TModuleEntry.TlsStart
                cmp     eax,[ecx]               // Starting Tls segment@
                jne     @@1                     // Zero address, so it will not be found anymore
                and     [edx-TYPE TModuleEntry].TModuleEntry.TlsStart,0
              @@2:
end;

// Returns an address of the THREADVAR variable
function _GetTlsVar(var TlsVar): Pointer; {&USES ebx,ecx,edx,esi,edi} {&FRAME-}
asm
                call    EnterHeap
                mov     eax,TlsVar
                xor     ecx,ecx
                mov     esi,TlsSharedMem
                lea     edx,[esi+TYPE TSharedMem]
              @@1:
                mov     ebx,[edx].TModuleEntry.TlsStart
                cmp     ebx,-1                  // Not found, wrong TLS address is given
                jz      @@RET                   // return the address itself
                cmp     eax,ebx
                jb      @@2
                add     ebx,[edx].TModuleEntry.TlsSize
                cmp     eax,ebx
                jb      @@3
              @@2:
                add     ecx,[edx].TModuleEntry.TlsSize
                add     edx,TYPE TModuleEntry
                jmp     @@1
              @@3:
                sub     eax,[edx].TModuleEntry.TlsStart
                lea     ebx,[eax+ecx]           // ebx := Offset within TLS

                Call    GetThreadId
                dec     eax                     // eax := ThreadID-1

//                call    EnterTLS

                mov     edi,[esi].TSharedMem.TlsPerThread
                mov     eax,[edi+eax*4]         // Tls[Thread]


                // Walk chain of tls to find offset in ebx
              @@4:
                cmp     ebx,[eax].TThreadEntry.TlsSize
                jl      @@ThisModule
                sub     ebx,[eax].TThreadEntry.TlsSize
                mov     eax,[eax].TThreadEntry.Next
                jmp     @@4
              @@ThisModule:
                add     eax,OFFSET TThreadEntry.Data
                add     eax,ebx
              @@RET:
                call    LeaveHeap
end;

//±±±±±±±±±±±±±±±[ SYSTEM DEPENDENT INTERFACE ]±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±

const
 // User Exception Handler Return Codes
  XCPT_CONTINUE_SEARCH          = $00000000;  // exception not handled
  XCPT_CONTINUE_EXECUTION       = $FFFFFFFF;  // exception handled

{$IFDEF OS2}

type
  Tib  = record
    Tib_PExchain:         Pointer;
    Tib_PStack:           Pointer;
    Tib_PStackLimit:      Pointer;
    Tib_PTib2:            Pointer;
    Tib_Version:          Longint;
    Tib_Ordinal:          Longint;
  end;

  Tib2  = record
    Tib2_ulTid:           Longint;
    Tib2_ulPri:           Longint;
    Tib2_Version:         Longint;
    Tib2_usMCCount:       SmallWord;
    Tib2_fMCForceFlag:    SmallWord;
  end;

procedure DosRaiseException;     orgname; external;
procedure DosUnwindException;    orgname; external;
procedure SysGetEnvironment;     orgname; external;

{$ENDIF}
{$IFDEF WIN32}

procedure RtlUnwind;             orgname; external;
procedure GetEnvironmentStrings; orgname; external;
procedure GetModuleHandle;       orgname; external;
procedure TlsAlloc;              orgname; external;
procedure TlsSetValue;           orgname; external;
procedure RaiseException(Code,Flags,ArgCount: Longint; var Args: Longint); stdcall; orgname; external;
function TlsGetValue(Index: Longint): Longint; stdcall; orgname; external;
function TlsFree(TlsIndex: Longint): Boolean;  orgname; external;

const
  TlsIndex:    Longint = -1;

{$ENDIF}
{$IFDEF DPMI32}

type
  { -> DPMI32.PAS }
  Tib  = record
    Tib_PExchain:         Pointer;
    Tib_PStack:           Pointer;
    Tib_PStackLimit:      Pointer;
  end;

procedure dpmi_RaiseException;   orgname; external;
procedure dpmi_UnwindException;  orgname; external;

{$ENDIF}

{$IFDEF WIN32}
procedure AllocWin32tid; {&uses esi} {&frame-}
asm
                { Prevent multiple threads from initialising in parallel }
                call    EnterHeap

                mov     esi,TlsSharedMem

                mov     ecx,[esi].TSharedMem.MaxThreadId
                dec     ecx
                mov     eax,[esi].TSharedMem.TlsPerThread
              @@1:      // Find previously used, now free, tid
                cmp     dword ptr [eax+ecx*4],0
                je      @@2
                dec     ecx
                js      @@3
                jmp     @@1
              @@3:      // Did not find free slot: use MaxId+1
           lock inc     [esi].TSharedMem.MaxThreadId
                mov     ecx,[esi].TSharedMem.MaxThreadId
              @@2:      // Found free slot
                inc     ecx
                push    ecx             // [2]:DWord = Tid
                push    TlsIndex        // [1]:DWord = TlsIndex
                Call    TlsSetValue     // Win32 API

                call    LeaveHeap
end;
{$ENDIF WIN32}

{ Initialise Thread Local Storage for a newly started thread, or
  for a thread that was not started using BeginThread but is trying
  to access a TLS variable }
procedure InitialiseTLS; {&uses none} {&Frame-}
asm
                call    EnterHeap
{$IFDEF WIN32}
                call    AllocWin32tid
{$ENDIF}
                call    AllocateTls_NewThread
                call    LeaveHeap
end;

// Installs system exception handler and activates the thread code.

function ThreadStartup(P: Longint): Longint; {&USES None} {&FRAME+}
asm
                Call    _FpuInit
                xor     eax,eax
                push    ebp
                push    OFFSET _ExceptionHandler
                push    fs:[eax].Longint
                mov     fs:[eax],esp
                call    GetThreadId
                or      eax,eax
{$IFDEF WIN32}
                jnz     @@TLSInitialised // TLS already initialised
{$ENDIF}
                call    InitialiseTLS
              @@TLSInitialised:
                mov     ebx,P
                mov     ecx,[ebx].TThreadRec.Param
                mov     edx,[ebx].TThreadRec.Func
                push    ebx
                Call    _MemFree
                push    ecx
                Call    edx                     // Call thread function
{$IFDEF OS2}    add     esp,4  {$ENDIF}
                call    FreeTLS
                xor     edx,edx
                pop     fs:[edx].Longint
                pop     ecx
                pop     ebp
                xor     eax,eax
                push    eax             // Return code
                Call    EndThread       // OS/2 2.0 does not like RET from thread code
end;

{$IFDEF OS2}
// Returns Thread ID for the current thread

function GetThreadId: Longint; {&USES None} {&FRAME-}
asm
                mov     eax,fs:[0].Tib.Tib_PTib2
                mov     eax,[eax].Tib2.Tib2_ulTid
end;
{$ENDIF OS2}

{$IFDEF WIN32}
function GetThreadId: Longint;
begin
  Result := TlsGetValue(TlsIndex);
  { Fix designed to make sure TLS is properly initialised for threads
    started without using VP's BeginThread mechanism }
  if Result = 0 then
    begin
      InitialiseTLS;
      Result := TlsGetValue(TlsIndex);
    end;
end;

procedure InitTidTls; {&USES ALL}
asm
                cmp     TlsIndex,-1
                jne     @@RET
                Call    TlsAlloc        // Win32 API function
                mov     TlsIndex,eax
                push    1               // [2]:DWord = TID
                push    eax             // [1]:DWord = TlsIndex
                Call    TlsSetValue     // Win32 API function
              @@RET:
end;
{$ENDIF WIN32}

{$IFDEF DPMI32}
function GetThreadId: Longint;
begin
  Result := SysGetThreadId;
end;
{$ENDIF}

{$IFDEF LINUX}
type
  TThreadInfo = record
    ExceptChain: Pointer; // Head of exception registration chain
    Stack:       Pointer; // Lower limit of stack
    StackLimit:  Pointer; // Upper limit of stack
    Handle:      LongInt; // One-based thread handle
    ThreadPid:   LongInt; // PID of thread itself
    ProcessPid:  LongInt; // PID of process to which thread belongs
    State:       LongInt; // State of thread
    TibSelector: LongInt; // Selector pointing to thread information block
  end;

procedure SysRaiseException;   orgname; external;
procedure SysUnwindException;  orgname; external;

function GetThreadId: Longint; {&USES NONE} {&FRAME-}
asm
                mov     eax,fs:[0].TThreadInfo.Handle
end;
{$ENDIF LINUX}
// Exception handling variables per platform

const
  System_RaiseException: Pointer =
   {$IFDEF OS2}    @DosRaiseException    {$ENDIF}
   {$IFDEF WIN32}  @RaiseException       {$ENDIF}
   {$IFDEF DPMI32} @Dpmi_RaiseException  {$ENDIF}
   {$IFDEF LINUX}  @SysRaiseException    {$ENDIF}
   ;
  System_UnwindException : Pointer =
   {$IFDEF OS2}    @DosUnwindException   {$ENDIF}
   {$IFDEF WIN32}  @RtlUnwind            {$ENDIF}
   {$IFDEF DPMI32} @Dpmi_UnwindException {$ENDIF}
   {$IFDEF LINUX}  @SysUnwindException   {$ENDIF}
   ;
   System_Xcpt_Continue_Search =
   {$IFDEF WIN32}  1
          {$ELSE}  0 {$ENDIF}
   ;


// ±±±±±±±±±±±±±[ INITIALIZATION/TERMINATION ]±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±

// EXPECTS:     stack   = Command line parameters
//              stack   = Environment block@
//              stack   = Module Handle

// Initialization code of the SYSTEM unit
// EXPECTS:     eax     = Stack Size ($M StackSize)
//              ecx     = Offset of the unit segment map

{&USES None} {&FRAME-}
procedure _InitExe(Params,EnvPtr: Pointer; Reserved,ModHandle,RetAddr: Longint); assembler;
var
  RegRec: TExcFrame;
asm
{$IFDEF DPMI32}
                // SAVE PSP SELECTOR
                mov     sel_psp,es
                mov     seldata,ds
                mov     stacksize,eax
                // DS=ES=SS
                push    ds
                pop     es
                push    [ecx+8].Longint
                pop     code_base
{$ENDIF DPMI32}
                cld
                push    ecx                     // save @segemnt map
{$IFDEF LINUX}
                // must initialize TIB emulation to make TLS useable
                call    SysLowInitPreTLS
{$ENDIF LINUX}
{$IFDEF OS2}
// Adjust Tib_PStack, because it points to the start of the DGROUP,
// not to the end of the stack
                mov     edx,fs:[Tib.Tib_PStackLimit]
                sub     edx,eax
                mov     fs:[Tib.Tib_PStack],edx
{$ENDIF OS2}
{$IFDEF WIN32}
                Call    InitTidTls
{$ENDIF WIN32}
                Call    SysCtrlGetTlsMapMem
                mov     TlsSharedMem,eax
                lea     edx,[eax].TSharedMem.HeapControl
                mov     HeapControl,edx         // Now it's ok to allocate memory
                pop     edx                     // restore @segemnt map
                Call    AddToTls
// Initialize global variables
{$IFDEF OS2}
                mov     eax,EnvPtr
                mov     Environment,eax
                mov     eax,ModHandle
                mov     ModuleHandle,eax
{$ENDIF OS2}
{$IFDEF WIN32}
                Call    GetEnvironmentStrings
                mov     Environment,eax
                push    0
                Call    GetModuleHandle
                mov     ModuleHandle,eax
                mov     HInstance,eax
{$ENDIF WIN32}
                Call    SysCtrlSelfAppType
                cmp     eax,3                   // 1:NOVIO,2:VIO,3:PM
                setb    IsConsole
                call    SysLowInitPostTLS
// Set Exception Handler
                lea     eax,RegRec              // System error handler
                xor     edx,edx                 // Insert System handler
                mov     ecx,fs:[edx]            // into the chain
                mov     fs:[edx],eax
                mov     [eax].TExcFrame.Next,ecx
                mov     [eax].TExcFrame.Desc,OFFSET _ExceptionHandler
                mov     [eax].TExcFrame.hEBP,ebp
                PopArgs 0                       // Since parameters are pushed by OS/2
// Exception Registration records must be on stack and must reside there while
// thread's code is executed, so leave it on stack and exit via jmp
                jmp     DWord Ptr [esp+@Locals] // Return@
end;

//±±±±±±±±±±±±±±[ DLL SUPPORT ]±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±

{$IfDef LNX_DPMI}
var
  TerminatingDLL: Boolean = false; // let _InitDllEnd know in which phase we are
{$EndIf LNX_DPMI}


// DLL initialization/termination code start
// EXPECTS:     ecx     = Unit segment map@
//              ZF      = 1 if Initialization

procedure _InitDll; {&USES None} {&FRAME-}
asm
                mov     byte ptr [IsDll],true

{$IFDEF OS2_LNX_DPMI}

                // on stack:
                //  [esp+$0c]   0:initialisation 1:termination
                //  [esp+$08]   module handle
                //  [esp+$04]   return to caller of entrypoint
                //  [esp+$00]   return to caller of _InitDll

                cmp     [esp+0Ch].Longint,0     // Initialization ?
  {$IfDef LNX_DPMI}
                setne   [TerminatingDLL]
  {$EndIf LNX_DPMI}
                jnz     @@End

                {$IfDef Linux}
                // instead of the passed module handle of -1, we remember the
                // instruction pointer of the entrypoint, we can later
                // look at /proc/*/maps to get the module name
                mov     eax, [esp+00h].Longint  // caller of _InitDLL
                {$Else}
                mov     eax, [esp+08h].Longint  // DLL module handle
                {$EndIf}
                mov     ModuleHandle, eax

                // parameters and environment, platform dependend
                {$IfDef OS2}
                pushad
                  call  SysCmdln                // uses DosGetInfoBlocks
                  mov   CmdLine, eax
                  call  SysGetEnvironment       // uses DosGetInfoBlocks
                  mov   Environment, eax
                popad
                {$EndIf OS2}

                {$IfDef Linux}
                // [esp+$18]    ArgV
                // [esp+$14]    ArgC
                // [esp+$10]    caller of DL_INIT/DL_FINI
                push    [esp+$14  ].Longint
                push    [esp+$18+4].Longint
                call    Process_ArgC_ArgV_Env
                {$EndIf Linux}

                {$IfDef DPMI32}
                nop                             // not implemented yet.
                {$EndIf DPMI32}

  {$IfDef LNX_DPMI}
                // Linux and DPMI32 do not provide a TIB at FS:0.
                // since the DLL initialisation runs before the
                // main program module initialisation has setup
                // an emulation, we provide a TIB here.
                // later modules and the main program will use it.

                // guess address of DT_FINI from DT_INIT
                // this works because PE2ELF places them always in this order
                mov     eax, [esp+$04]
              @@searchTermination:
                inc     eax
                cmp     [eax].SmallWord, $016a          // search 'push 1'
                jne     @@searchTermination

                push    eax
                call    AllocateDLLInitTib
  {$EndIf LNX_DPMI}

                push    ecx
                Call    SysCtrlGetTlsMapMem
                mov     TlsSharedMem,eax
                lea     edx,[eax].TSharedMem.HeapControl
                mov     HeapControl,edx         // Now it's ok to allocate memory
                pop     edx
                Call    AddToTls
                jmp     @@RET

              @@End:
                Call    DoExitProcs
                Call    RemoveFromTls
              @@RET:
                cmp     [esp+0Ch].Longint,0     // Initialization ?
{$ENDIF OS2_LNX_DPMI}
{$IFDEF WIN32}

                // on stack: (see WIN32.HLP 'DllEntryPoint')
                //  [esp+$10]   reserved (fpvReserved)
                //  [esp+$0c]   reason for calling function (fdwReason)
                //              0: process detach 1:process attach, 2: thread attach, 3: thread detach
                //  [esp+$08]   module handle (hinstDLL)
                //  [esp+$04]   return to caller of entrypoint  (somwhere in ntdll.dll)
                //  [esp+$00]   return to caller of _InitDll    (VP startup code)

                cmp     [esp+0Ch].Longint,1     // Initialization ?
                jb      @@End
                ja      @@RET                   // 2: Thread startup; 3: Thread terminating

                mov     eax, [esp+08h].Longint  // set DLL module handle
                mov     ModuleHandle, eax

                mov     HInstance,0 // or set hinstDLL?

                // Environment is not passed, [System.DoInit will not setup]
                // CmdLine is not passed,     [System.DoInit will call SysCmdLn to setup it]

                push    ecx
                Call    InitTidTls
                Call    SysCtrlGetTlsMapMem
                mov     TlsSharedMem,eax
                lea     edx,[eax].TSharedMem.HeapControl
                mov     HeapControl,edx         // Now it's ok to allocate memory
                pop     edx
                Call    AddToTls
                jmp     @@RET
              @@End:
                Call    DoExitProcs
                Call    RemoveFromTls
              @@RET:
                cmp     [esp+0Ch].Longint,1     // Initialization ?
{$ENDIF WIN32}
end;

// DLL initialization/termination code start
// Non-zero ExitCode indicates success

procedure _InitDllEnd(ExitCode: Longint); {&USES None} {&FRAME-}
asm
{$IfDef LNX_DPMI}
                // undo the AllocateDLLInitTib call in _InitDll
                cmp     TerminatingDLL, true
                jne     @@NotReleaseDLLInitTib
                call    ReleaseDLLInitTib
  @@NotReleaseDLLInitTib:
{$EndIf LNX_DPMI}
{$IFDEF OS2_LNX_DPMI}
                mov     eax,ExitCode
                leave                           { Restore stack frame         }
                PopArgs 0                       { Return to OS/2              }
{$ENDIF OS2_LNX_DPMI}
{$IFDEF WIN32}
                mov     eax,ExitCode
                leave
                PopArgs 4*3
{$ENDIF WIN32}
end;


//±±±±±±±±±±±±±±[ EXCEPTION HANDLING ]±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±

// Notification exceptions are used to inform a debugger of the current state
// of a program while the program is handling an exception. This helps the
// debugger to trace TRY blocks even if an exception is raised. The debugger
// sets DebugHook to True to enable generation of the notification exceptions.

// Exception#           |ParamCount|  Param1            | Param2
// ---------------------+----------+--------------------+----------------
// cLanguageReRaise     |    0     |    -               |   -
// cLanguageExcept      |    2     |  @ExceptionHandler | Exception Report record
// cLanguageFinally     |    2     |  @ExceptionHandler | Exception Report record
// cLanguageTerminate   |    1     |  @ReturnAddress    |   -
// cLanguageUnhandled   |    2     |  Exception Object  | Exception address
// cNonLanguageException|    2     |  Exception Object  | Exception context record

{$IFDEF OS2}
procedure RaiseNotification(ArgCount,Arg1,Arg2,Code: Longint); assembler; {&USES eax,ecx,edx} {&FRAME-}
var
  ER: TXcptReportRecord;
asm
                cmp     DebugHook,1
                jne     @@RET
                mov     eax,Code                // Exception number
                mov     edx,ArgCount            // Number of parameters
                mov     ER.ExceptionNum,eax
                mov     ER.cParameters,edx
                xor     eax,eax
                mov     ER.ExceptionAddress,eax
                mov     ER.NestedXcptReportRecord,eax
                mov     ER.fHandlerFlags,cContinuable
                mov     eax,Arg1
                mov     edx,Arg2                // Arguments
                mov     ER.ExceptionInfo[0].Longint,eax
                mov     ER.ExceptionInfo[4].Longint,edx
                lea     eax,ER                  // [1]:Report
                push    eax
                Call    DosRaiseException
                pop     eax                     // Stack cleanup
              @@RET:
end;

// Returns ZF=1 if exception must be ignored
// EXPECTS:     eax     = @ of the exception report record

procedure XcptIgnored; {&USES ecx} {&FRAME-}
asm
                mov     ecx,[eax].TXcptReportRecord.ExceptionNum
                cmp     ecx,XCPT_GUARD_PAGE_VIOLATION
                je      @@RET
                cmp     ecx,XCPT_PROCESS_TERMINATE
                je      @@RET
                cmp     ecx,XCPT_ASYNC_PROCESS_TERMINATE
                je      @@RET
                cmp     ecx,XCPT_UNWIND
              @@RET:
end;
{$ENDIF OS2}

{$IFDEF WIN32}
{$SAVES eax,ebx,ecx,edx,esi,edi}

procedure RaiseNotification(ArgCount,Arg1,Arg2,Code: Longint);
var
  Args: array[0..1] of Longint;
begin
  if Ord(DebugHook) = 1 then
  begin
    Args[0] := Arg1;
    Args[1] := Arg2;
    RaiseException(Code, cContinuable, ArgCount, Args[0]);
  end;
end;

{$SAVES ebx,esi,edi}
{$ENDIF WIN32}

{$IFDEF DPMI32}
procedure RaiseNotification(ArgCount,Arg1,Arg2,Code: Longint); assembler;{$FRAME-}{$USES NONE}
asm
                nop
end;
{$ENDIF DPMI32}
{$IFDEF Linux}
procedure RaiseNotification(ArgCount,Arg1,Arg2,Code: Longint); assembler;{$FRAME-}{$USES NONE}
asm
                cmp [RaiseNotification_Hook],0
                je @@ret
                jmp [RaiseNotification_Hook]
              @@ret:
end;
{$ENDIF LINUX}

procedure NotifyReRaise; {&USES None} {&FRAME-}
asm
                push    0
                push    0
                push    0
                push    cLanguageReRaise
                Call    RaiseNotification
end;

procedure NotifyExcept; {&USES None} {&FRAME-}
asm
                push    2
                push    ecx
                push    edx
                push    cLanguageExcept
                Call    RaiseNotification
end;

procedure NotifyExitFinally; {&USES None} {&FRAME-}
asm
                push    2
                push    ecx
                push    edx
                push    cLanguageFinally
                Call    RaiseNotification
end;

procedure NotifyExceptFinally; {&USES ecx} {&FRAME-}
asm
                mov     eax,[ecx+1]
                cmp     [ecx].Byte,0E9h         // near jmp
                je      @@1
                cmp     [ecx].Byte,0EBh         // short jmp
                jne     @@2
                movsx   eax,al
                sub     ecx,3
              @@1:
                lea     ecx,[eax+ecx+5]
              @@2:
                Call    NotifyExitFinally
end;

procedure NotifyTerminate; {&USES None} {&FRAME-}
asm
                push    1
                push    ecx
                push    ecx
                push    cLanguageTerminate
                Call    RaiseNotification
end;

// EXPECTS:     eax     = Language exception object
//              edx     = Exception address

procedure NotifyUnhandled; {&USES None} {&FRAME-}
asm
                push    2
                push    eax
                push    edx
                push    cLanguageUnhandled
                Call    RaiseNotification
end;

// EXPECTS:     eax     = Language exception object
//              edx     = Exception context record

procedure NotifyNonLanguage; {&USES eax} {&FRAME-}
asm
                push    2
                push    eax
                push    edx
                push    cNonLanguageException
                Call    RaiseNotification
end;

// In the TRY..EXCEPT block, handles any oncoming exceptions

procedure _XcptAny(Report,Registration,Context,Void: Pointer); {&USES None} {&FRAME-}
asm
                mov     eax,Report
                mov     edx,Registration
                test    [eax].TXcptReportRecord.fHandlerFlags,cUnwindInProgress
                jnz     @@RET
                mov     edx,[eax].TXcptReportRecord.ExceptObject
                mov     ecx,[eax].TXcptReportRecord.ExceptAddr
                cmp     [eax].TXcptReportRecord.ExceptionNum,cLanguageException
                je      @@Language
{$IFDEF OS2}
                Call    XcptIgnored
                je      @@RET
{$ENDIF}
                Call    _FpuInit
                mov     edx,ExceptObjProc
                test    edx,edx
                jz      @@RET
                push    eax                     // [1]:PXcptReportRecord
                Call    edx
                test    eax,eax
                je      @@RET
                mov     edx,Context
                Call    NotifyNonLanguage
                mov     edx,eax
                mov     eax,Report
                mov     ecx,[eax].TXcptReportRecord.ExceptionAddress
              @@Language:
                or      [eax].TXcptReportRecord.fHandlerFlags,cUnwinding
                push    ebx
                push    esi
                push    edi
                push    ebp
                push    fs:[0].Longint          // Topmost frame
// Construct TRaise frame on stack
                push    eax                     // TRaiseFrame.ExceptionRecord
                push    edx                     // TRaiseFrame.ExceptObject
                push    ecx                     // TRaiseFrame.ExceptAddr
                mov     edx,Registration[8*4]
{$IFDEF WIN32}  push    0            {$ENDIF}   // Win32: Extra parameter
                push    eax                     // [3]: Report
                push    OFFSET @@TargetEIP      // [2]: Target EIP
                push    edx                     // [1]: Registration
                call    System_UnwindException  // Platform-dependent const
              @@TargetEIP:
{$IFDEF OS2}    add     esp,4*3      {$ENDIF}   // OS/2: Manual stack clean
                mov     edx,Report[8*4]
                mov     edi,Registration[8*4]
                push    OFFSET RaiseList
                Call    _GetTlsVar
                push    [eax].Longint           // TRaiseFrame.NextRaise
                mov     [eax],esp
                mov     ebp,[edi].TExcFrame.hEBP
                mov     ecx,[edi].TExcFrame.Desc
                mov     [edi].TExcFrame.Desc,OFFSET @@ExceptFinally
                add     ecx,TExcDesc.Instructions
                Call    NotifyExcept
                jmp     ecx
@@ExceptFinally:
                jmp     _XcptFinally

// Exception handler for a TRY...EXCEPT exception handler code. Control
// gets here if an execption is raised from the EXCEPT part.

                push    OFFSET RaiseList
                Call    _GetTlsVar
                mov     ecx,[eax]
                mov     edx,[ecx].TRaiseFrame.NextRaise
                mov     [eax],edx
                push    [ecx].TRaiseFrame.ExceptObject
                Call    TObject.Free
                ret
              @@RET:
                mov     eax,System_Xcpt_Continue_Search
end;

// In the TRY..EXCEPT block, handles oncoming exception by looking for a
// first matching ON exception handler

procedure _XcptOn(Report,Registration,Context,Void: Pointer); {&USES None} {&FRAME-}
asm
                mov     eax,Report
                test    [eax].TXcptReportRecord.fHandlerFlags,cUnwindInProgress
                jne     @@RET
                cmp     [eax].TXcptReportRecord.ExceptionNum,cLanguageException
                je      @@Language
{$IFDEF OS2}
                Call    XcptIgnored
                je      @@RET
{$ENDIF}
                Call    _FpuInit
                mov     edx,ExceptClsProc
                test    edx,edx
                jz      @@RET
                push    eax                     // [1]:PXcptReportRecord
                Call    edx
                test    eax,eax
                jnz     @@Find
                jmp     @@RET
              @@Language:
                mov     eax,[eax].TXcptReportRecord.ExceptObject
                mov     eax,[eax].clVTable
              @@Find:
                mov     edx,Registration        // eax = Exception VMT@
                push    ebx
                push    esi
                push    edi
                push    ebp
                mov     ecx,[edx].TExcFrame.Desc
                mov     ebx,[ecx].TExcDesc.Cnt
                lea     esi,[ecx].TExcDesc.ExcTab
                mov     ebp,eax
              @@1:
                mov     eax,[esi].TExcDescEntry.vTable
                test    eax,eax                 // ELSE part ?
                jz      @@DoHandler             // Yes: execute handler
                mov     edi,ebp                 // load VMT of exception object
              @@2:
                cmp     eax,edi
                je      @@DoHandler
                mov     ecx,[eax].vtInstanceSize// CMP Instance Sizes
                cmp     ecx,[edi].vtInstanceSize
                jne     @@Parent
                mov     eax,[eax].vtClassName   // CMP Exception Names
                mov     edx,[edi].vtClassName
                mov     cl,[eax]
                cmp     cl,[edx]
                jne     @@Parent
                push    eax                     // [1]: Str1
                push    edx                     // [2]: Str2
                Call    _StrCmp
                jne     @@Parent
                mov     eax,[esi].TExcDescEntry.vTable
                mov     edx,[edi].vtTypeInfo
                mov     eax,[eax].vtTypeInfo
                test    edx,edx
                jz      @@Parent
                test    eax,eax
                jz      @@Parent                // CMP unit names
                movzx   ecx,[eax].TTypeInfo.Name.Byte
                cmp     cl,[edx].TTypeInfo.Name.Byte
                jne     @@Parent
                lea     eax,[eax+ecx].TTypeInfo.Name[1].TClassRTTI.UnitName
                lea     edx,[edx+ecx].TTypeInfo.Name[1].TClassRTTI.UnitName
                push    eax
                push    edx
                Call    _StrCmp
                je      @@DoHandler
              @@Parent:
                mov     edi,[edi].vtParent
                mov     eax,[esi].TExcDescEntry.vTable
                test    edi,edi
                jnz     @@2
                add     esi,TYPE TExcDescEntry
                dec     ebx
                jnz     @@1
                pop     ebp
                pop     edi
                pop     esi
                pop     ebx
                jmp     @@RET
{ Exception is found }
              @@DoHandler:
                mov     eax,Report[4*4]
                mov     edx,[eax].TXcptReportRecord.ExceptObject
                mov     ecx,[eax].TXcptReportRecord.ExceptAddr
                cmp     [eax].TXcptReportRecord.ExceptionNum,cLanguageException
                je      @@HaveObject
                push    eax                     // [1]:PXcptReportRecord
                Call    ExceptObjProc
                mov     edx,Context[4*4]
                Call    NotifyNonLanguage
                mov     edx,eax
                mov     eax,Report[4*4]
                mov     ecx,[eax].TXcptReportRecord.ExceptionAddress
              @@HaveObject:
                or      [eax].TXcptReportRecord.fHandlerFlags,cUnwinding
                push    fs:[0].Longint          // Topmost frame
// Construct TRaise frame on stack
                push    eax                     // TRaiseFrame.ExceptionRecord
                push    edx                     // TRaiseFrame.ExceptObject
                push    ecx                     // TRaiseFrame.ExceptAddr
                mov     edx,Registration[8*4]
                push    esi                     // Handler entry
{$IFDEF WIN32}  push    0 {$ENDIF}
                push    eax                     // [3]: Report
                push    OFFSET @@TargetEIP      // [2]: Target EIP
                push    edx                     // [1]: Registration
                call    System_UnwindException  // Platform-dependent const
              @@TargetEIP:
{$IFDEF OS2}    add     esp,4*3      {$ENDIF}   // OS/2: Manual stack clean
                pop     ecx                     // Handler entry
                mov     edx,Report[8*4]
                mov     edi,Registration[8*4]
                push    OFFSET RaiseList
                Call    _GetTlsVar
                push    [eax].Longint           // TRaiseFrame.NextRaise
                mov     [eax],esp
                mov     ebp,[edi].TExcFrame.hEBP
                mov     [edi].TExcFrame.Desc,OFFSET @@ExceptFinally
                mov     eax,[esp].TRaiseFrame.ExceptObject
                mov     ecx,[ecx].TExcDescEntry.Handler
                Call    NotifyExcept
                jmp     ecx                     // eax = Exception object for
@@ExceptFinally:                                // >> on E: Exception <<
                jmp     _XcptFinally

// Exception handler for a TRY...EXCEPT exception handler code. Control
// gets here if an execption is raised from the EXCEPT part.

                push    OFFSET RaiseList
                CALL    _GetTlsVar
                mov     ecx,[eax]               // RaiseList
                mov     edx,[ecx].TRaiseFrame.NextRaise
                mov     [eax],edx               // RaiseList
                push    [ecx].TRaiseFrame.ExceptObject
                Call    TObject.Free
                ret
              @@RET:
                mov     eax,System_Xcpt_Continue_Search
end;

// In the TRY..FINALLY block, executes FINALLY statement part

procedure _XcptFinally(Report,Registration,Context,Void: Pointer); {&USES None} {&FRAME-}
asm
                mov     eax,Report
                test    [eax].TXcptReportRecord.fHandlerFlags,cUnwindInProgress
                jz      @@RET
{$IFDEF OS2}
                Call    XcptIgnored
                je      @@RET
{$ENDIF}
                mov     edx,eax
                mov     eax,Registration
                mov     ecx,[eax].TExcFrame.Desc
                mov     [eax].TExcFrame.Desc,OFFSET @@RET
                push    ebx
                push    esi
                push    edi
                push    ebp
                mov     ebp,[eax].TExcFrame.hEBP
                add     ecx,TExcDesc.Instructions
                Call    NotifyExceptFinally
                Call    ecx
                pop     ebp
                pop     edi
                pop     esi
                pop     ebx
              @@RET:
                mov     eax,System_Xcpt_Continue_Search
end;

// Exit from TRY statement. Here we should determine which block type is it

procedure _XcptTryExit; {&USES eax,ecx,edx} {&FRAME-}
asm
                xor     edx,edx
                mov     eax,[esp+4][@Uses].TExcFrame.Next
                mov     ecx,[esp+4][@Uses].TExcFrame.Desc
                mov     fs:[edx],eax
                mov     eax,[ecx].TExcDesc.Jmp.Distance
                lea     eax,[ecx+eax].TExcDesc.Jmp[5]
                cmp     eax,OFFSET _XcptFinally         // TRY..FINALLY block ?
                jne     @@RET                           // No, TRY..EXCEPT => exit
                add     ecx,TExcDesc.Instructions       // edx=0
                Call    NotifyExitFinally               // Yes, execute FINALLY part
                Call    ecx
                mov     ecx,[esp][@Uses]
                xor     edx,edx
                Call    NotifyExitFinally
              @@RET:
                PopArgs 12                      // Pop out exception frame
end;

// In the TRY...EXCEPT statement disposes of exception object and
// gets rid of exception

procedure _XcptDone(Report,Registration,Context,Void: Pointer); {&USES None} {&FRAME-}
asm
//              Pop RaiseList
                push    OFFSET RaiseList
                Call    _GetTlsVar
                mov     edx,[eax]
                mov     ecx,[edx].TRaiseFrame.NextRaise
                mov     [eax],ecx
//              Destroy exception object
                push    [edx].TRaiseFrame.ExceptObject
                Call    TObject.Free
                pop     ecx                     // Return address
                mov     esp,Registration[9*4]   // Exception Frame
                xor     eax,eax
                pop     fs:[eax].Longint        // Previous frame
                pop     eax                     // Exception handler@
                pop     ebp                     // Saved EBP
                Call    NotifyTerminate
                jmp     ecx
end;

// Raises an exception on the return address

procedure _XcptRaise(Exception: Pointer); {&USES None} {&FRAME-}
asm
{$IFDEF WIN32}
                mov     eax,[esp]
                push    esp                     // [4]:Pointer = @arguments: @, Exception class
                push    2                       // [3]:DWord = Argument count
                push    cNonContinuable         // [3]:DWord = Flags
                push    cLanguageException      // [1]:DWord = Exception #
                push    eax                     // Return@
                jmp     RaiseException
{$ELSE (~WIN32)}
                pop     eax                     // Return address
                pop     ecx                     // Exception object
                sub     esp,TYPE TXcptReportRecord
                mov     [esp].TXcptReportRecord.ExceptAddr,eax
                mov     [esp].TXcptReportRecord.ExceptionAddress,eax
                mov     [esp].TXcptReportRecord.ExceptObject,ecx
                mov     [esp].TXcptReportRecord.cParameters,2
                mov     [esp].TXcptReportRecord.fHandlerFlags,cNonContinuable
                mov     [esp].TXcptReportRecord.ExceptionNum,cLanguageException
                and     [esp].TXcptReportRecord.NestedXcptReportRecord,0
                mov     ecx,esp
                push    eax                     // Return address
                push    ecx                     // [1]:Exception report record
                call    System_RaiseException
{$IFDEF OS2}    pop     eax  {$ENDIF OS2}       // Stack cleanup
{$ENDIF ~WIN32}
end;

// Re-raises exception inside exception handler code

procedure _XcptRaiseAg(Report,Registration,Context,Void: Pointer); {&USES None} {&FRAME-}
asm
                mov     eax,Registration[10*4]
                mov     [eax].TExcFrame.Desc,OFFSET @@RET
//              Pop RaiseList
                push    OFFSET RaiseList
                Call    _GetTlsVar
                mov     edx,[eax]               // RaiseList
                mov     ecx,[edx].TRaiseFrame.NextRaise
                mov     [eax],ecx               // RaiseList
//              Destroy object created for a non-language exception
                mov     eax,[edx].TRaiseFrame.ExceptionRecord
                and     [eax].TXcptReportRecord.fHandlerFlags,NOT cUnwinding
                cmp     [eax].TXcptReportRecord.ExceptionNum,cLanguageException
                je      @@Language
                push    [edx].TRaiseFrame.ExceptObject
                Call    TObject.Free
                Call    NotifyReRaise
              @@Language:
                xor     eax,eax
                add     esp,5*4                 // Return@ + RaiseFrame
                pop     ecx                     // Topmost frame
                mov     edx,fs:[eax]
                mov     edx,[edx].TExcFrame.Next
                mov     [ecx].TExcFrame.Next,edx
                pop     ebp
                pop     edi
                pop     esi
                pop     ebx
              @@RET:
                mov     eax,System_Xcpt_Continue_Search
end;

procedure _ExceptionHandler(Report,Registration,Context,Void: Pointer); assembler; {&USES ebx,esi,edi} {&FRAME+}
type
  ExceptionData = record
    No: Longint;
    EC: Byte;
  end;
const
  ExcpCount = 14;
  ExcpData: array [1..ExcpCount] of ExceptionData =
  (
    (No: XCPT_ARRAY_BOUNDS_EXCEEDED;    EC: RTE_Range_Check           ),
    (No: XCPT_FLOAT_DENORMAL_OPERAND;   EC: RTE_FP_Denormal_Operand   ),
    (No: XCPT_FLOAT_DIVIDE_BY_ZERO;     EC: RTE_Zero_Divide           ),
    (No: XCPT_FLOAT_INEXACT_RESULT;     EC: RTE_FP_Inexact_Result     ),
    (No: XCPT_FLOAT_INVALID_OPERATION;  EC: RTE_Invalid_FP_Operation  ),
    (No: XCPT_FLOAT_OVERFLOW;           EC: RTE_FP_Overflow           ),
    (No: XCPT_FLOAT_STACK_CHECK;        EC: RTE_Invalid_FP_Operation  ),
    (No: XCPT_FLOAT_UNDERFLOW;          EC: RTE_FP_Underflow          ),
    (No: XCPT_INTEGER_DIVIDE_BY_ZERO;   EC: RTE_Zero_Divide           ),
    (No: XCPT_INTEGER_OVERFLOW;         EC: RTE_Integer_Overflow      ),
    (No: XCPT_PRIVILEGED_INSTRUCTION;   EC: RTE_Privileged_Instruction),
    (No: XCPT_ACCESS_VIOLATION;         EC: RTE_Access_Violation      ),
    (No: XCPT_UNABLE_TO_GROW_STACK;     EC: RTE_Stack_Overflow        ),
{$IFDEF OS2}
    (No: XCPT_SIGNAL;                   EC: 0                         )
{$ENDIF OS2}
{$IFDEF WIN32}
    (No: XCPT_CONTROL_C_EXIT;           EC: RTE_Exception             )
{$ENDIF WIN32}
{$IFDEF DPMI32}
    (No: xcpt_Ctrl_Break;               EC: 0                         )
{$ENDIF DPMI32}
{$IFDEF LINUX}
    (No: xcpt_Ctrl_Break;               EC: 0                         )
{$ENDIF LINUX}
  );
asm
                lea     esi,XcptProc
                mov     ebx,[esi]               // XcptProc
              @@1:
                mov     ecx,[esi]
                test    ecx,ecx
                jz      @@2
                and     [esi].Longint,0         // Protect from infinite loop
                push    Void                    // 4
                push    Context                 // 3
                push    Registration            // 2
                push    Report                  // 1
                Call    ecx
{$IFDEF OS2}    add     esp,4*4  {$ENDIF}       // OS/2: Stack cleanup
                test    eax,eax                 // XCPT_CONTINUE_SEARCH ?
                jz      @@1                     // Yes, search another handler
                mov     [esi],ebx
                jmp     @@RET                   // Exception is handled
              @@2:
                mov     [esi],ebx               // Restore XcptProc
                mov     eax,Report
                test    [eax].TXcptReportRecord.fHandlerFlags,cUnwindInProgress
                jnz     @@Done
{$IFDEF OS2}
                Call    XcptIgnored
                je      @@Done
{$ENDIF}
                Call    _FpuInit
                mov     edx,Registration
{$IFDEF WIN32}  push    0 {$ENDIF}
                push    eax
                push    OFFSET @@TargetEIP
                push    edx
                call    System_UnwindException  // Platform-dependent const
              @@TargetEIP:
{$IFDEF OS2}    add     esp,4*3      {$ENDIF}   // OS/2: Manual stack clean
                mov     ebx,Report
                mov     edx,[ebx].TXcptReportRecord.ExceptAddr
                mov     eax,[ebx].TXcptReportRecord.ExceptObject
                cmp     [ebx].TXcptReportRecord.ExceptionNum,cLanguageException
                je      @@Language
                mov     edx,ExceptObjProc
                test    edx,edx
                jz      @@MapToRunError
                push    ebx                     // [1]:PXcptReportRecord
                Call    edx
                test    eax,eax
                jz      @@MapToRunError
                mov     edx,[ebx].TXcptReportRecord.ExceptionAddress
              @@Language:
                Call    NotifyUnhandled
                mov     ebx,edx
                mov     ecx,ExceptProc
                test    ecx,ecx
                jz      @@NoExceptProc
                push    eax                     // [1]:Exception Object
                push    edx                     // [2]:Exception Address
                Call    ecx
              @@NoExceptProc:
                mov     ecx,Report
                push    217                     // [1]:ErrorCode
                push    ebx                     // Return address = exception@
                jmp     _RunError

// Maps OS/2 System exceptions to run-time errors

              @@MapToRunError:
                mov     ecx,Report
                lea     edx,ExcpData
              @@3:
                mov     eax,[ecx].TXcptReportRecord.ExceptionNum
                cmp     eax,[edx].ExceptionData.No
                mov      al,[edx].ExceptionData.EC
                je      @@Error
                add     edx,TYPE ExceptionData
                cmp     edx,OFFSET ExcpData[ExcpCount*TYPE ExceptionData]
                jne     @@3
                mov     al,RTE_Exception
              @@Error:
                test    al,al                   // We have to pass a few
                jz      @@Done                  // exceptions to default handler
                movzx   eax,al
                push    eax                                          // [1]:Error Code
                push    [ecx].TXcptReportRecord.ExceptionAddress // [2]:Address
                mov     eax,[ecx].TXcptReportRecord.ExceptionNum
                mov     ExceptionNo,eax
                jmp     _RunError

              @@Done:
                xor     eax,eax                 // XCPT_CONTINUE_SEARCH
              @@RET:
end;

begin
  DoInit;
end.

