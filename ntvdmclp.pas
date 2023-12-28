{&OrgName+,StdCall+,Use32+,Optimise-}
library NTVDMclp;

{
 (q) by sergey korowkin aka sk // [rAN], 2000.
 (q) by Tatyana Medvedeva aka tm // [ice-lock NPD2000].

 http://aux.ru/sk, skank@mail.ru, 2:6033/27@fidonet
}

uses
 Windows;

const
 WINOLDAPVERSION      = $001B;
                     
 clpVERSION           = $0101;
                     
 clpfnGETVERSION      = $0000;
 clpfnINIT            = $0001;
 clpfnDONE            = $0002;
                     
 clpfnIDENTIFY        = $1700;
 clpfnOPEN            = $1701;
 clpfnEMPTY           = $1702;
 clpfnSETDATA         = $1703;
 clpfnGETDATASIZE     = $1704;
 clpfnGETDATA         = $1705;
 clpfnCLOSE           = $1708;
                     
 clptitleSET          = $0010;
 clptitleGET          = $0011;
                     
 clpSLEEP             = $0020;
                     
 clpAppTerminated     = $0030;
 clpInstCtrlHandler   = $0031;
 clpUnInstCtrlHandler = $0032;

 clpTZUTC             = $0040;

 AppTerminated : Boolean = False;

procedure setCF(Value: SmallWord); external 'NTVDM.EXE';

procedure setAX(Value: SmallWord); external 'NTVDM.EXE';
procedure setCX(Value: SmallWord); external 'NTVDM.EXE';
procedure setDX(Value: SmallWord); external 'NTVDM.EXE';

procedure getMSW; external 'NTVDM.EXE';
procedure getDX; external 'NTVDM.EXE';

function getDS: SmallWord; external 'NTVDM.EXE';
function getSI: SmallWord; external 'NTVDM.EXE';
function getCX: SmallWord; external 'NTVDM.EXE';
function getBX: SmallWord; external 'NTVDM.EXE';
function getES: SmallWord; external 'NTVDM.EXE';

procedure MGetVdmPointer; external 'NTVDM.EXE';

{$I INC\win.inc}

{ ntvdmclpRegister }

procedure ntvdmclpRegister; export;
 begin
  setCF(0);
 end;

{ _GETVERSION }

procedure _GETVERSION;
 begin
  setCX(clpVERSION);
 end;

{ _INIT }

procedure _INIT;
 begin
  // reserved for future use
 end;

{ _DONE }

procedure _DONE;
 begin
  // reserved for future use
 end;

{ clipboard stuff }

procedure _clpIDENTIFY;
 begin
  setAX(WINOLDAPVERSION);
 end;

procedure _clpOPEN;
 begin
  if OpenClipboard(0) then
   setAX(1)
  else
   setAX(0);
 end;

procedure _clpEMPTY;
 begin
  if EmptyClipboard then
   setAX(1)
  else
   setAX(0);
 end;

procedure _clpSETDATA;
 var
  Format: SmallWord;
  Data, Locked: Pointer;
  Size, Handle: Longint;
 begin
  Size:=Longint(getDS) * Longint($10000) + Longint(getSI);

  asm
   call getDX

   mov Format, ax

   call getMSW

   and eax, 1
   mov esi, eax

   call getES
   xchg edi, eax
   shl edi, 16

   call getBX
   xchg di, ax

   mov ebx, Size

   push esi
   push ebx
   push edi

   call MGetVdmPointer

   mov Data, eax
  end;

  EmptyClipboard;

  Handle:=GlobalAlloc(GMEM_MOVEABLE or GMEM_DDESHARE, Size);

  Locked:=GlobalLock(Handle);

  Move(Data^, Locked^, Size);

  GlobalUnlock(Handle);

  SetLastError(0);

  SetClipboardData(Format, Handle);

  setAX(1); // always ok?
 end;

procedure _clpGETDATASIZE;
 var
  Format: SmallWord;
  Handle, Size: Longint;
 begin
  asm
   call getDX

   mov Format, ax
  end;

  Handle:=GetClipboardData(Format);

  if Handle = 0 then
   begin
    setDX(0);

    setAX(0);
   end
  else
   begin
    Size:=GlobalSize(Handle);

    setDX(Size div $10000);
    setAX(Size mod $10000);
   end;
 end;

procedure _clpGETDATA;
 var
  Format: SmallWord;
  Handle, Size: Longint;
  Source, Data: Pointer;
 begin
  asm
   call getDX

   mov Format, ax
  end;

  Handle:=GetClipboardData(Format);

  Size:=GlobalSize(Handle);

  asm
   call getMSW

   and eax, 1
   mov esi, eax

   call getES
   xchg edi, eax
   shl edi, 16

   call getBX
   xchg di, ax

   mov ebx, Size

   push esi
   push ebx
   push edi

   call MGetVdmPointer

   mov Data, eax
  end;

  if Handle = 0 then
   setAX(0)
  else
   begin
    Source:=GlobalLock(Handle);

    Move(Source^, Data^, Size);

    GlobalUnlock(Handle);

    setAX(1);
   end;
 end;

procedure _clpCLOSE;
 begin
  if CloseClipboard then
   setAX(1)
  else
   setAX(0);
 end;

{ title stuff }

procedure _titleSET;
 var
  Size: Longint;
  Data, DataANSI: PChar;
 begin
  asm
   call getMSW

   and eax, 1
   mov esi, eax

   call getDS
   xchg edi, eax
   shl edi, 16

   call getDX
   xchg di, ax

   call getBX
   movzx ebx, ax

   mov Size, ebx

   push esi
   push ebx
   push edi

   call MGetVdmPointer

   mov Data, eax
  end;

  GetMem(DataANSI, Size);

  OemToCharBuff(Data, DataANSI, Size);

  if SetConsoleTitle(Data) then
   setCF(0)
  else
   setCF(1);

  FreeMem(DataANSI, Size);
 end;

procedure _titleGET;
 var
  Size: Longint;
  Data: PChar;
 begin
  asm
   call getMSW

   and eax, 1
   mov esi, eax

   call getDS
   xchg edi, eax
   shl edi, 16

   call getDX
   xchg di, ax

   call getBX
   movzx ebx, ax

   mov Size, ebx

   push esi
   push ebx
   push edi

   call MGetVdmPointer

   mov Data, eax
  end;

  if GetConsoleTitle(Data, Size) = 0 then
   setCF(1)
  else
   setCF(0);
 end;

{ Size is not used here, but I don't know how to properly
  adjust the code, so I'll just leave it. }
procedure _TZUTC;
 var
  Size: Longint;
  Data: ^String;
 begin
  asm
   call getMSW

   and eax, 1
   mov esi, eax

   call getDS
   xchg edi, eax
   shl edi, 16

   call getDX
   xchg di, ax

   call getBX
   movzx ebx, ax

   mov Size, ebx

   push esi
   push ebx
   push edi

   call MGetVdmPointer

   mov Data, eax
  end;

  Data^ := GetSystemTZUTC;
 end;

{ sleep.. zzz... }

procedure _SLEEP;
 begin
  Sleep(getBX);
 end;

{ window closed? }

procedure _AppTerminated;
 begin
  if AppTerminated then
   setAX(1)
  else
   setAX(0);
 end;

{ signal handler }

function CtrlHandler(fdwCtrlType: DWord): Bool;
 begin
  CtrlHandler := True;
  case fdwCtrlType of
   Ctrl_Close_Event,
   {Ctrl_LogOff_Event,}
   Ctrl_ShutDown_Event: AppTerminated := True;
  end;
 end;

procedure _InstCtrlHandler;
begin
 SetConsoleCtrlHandler(@CtrlHandler, True);
end;

procedure _UnInstCtrlHandler;
begin
 SetConsoleCtrlHandler(@CtrlHandler, False);
end;

{ ntvdmclpDispatch }

procedure ntvdmclpDispatch; export;
 begin
  asm
   pusha
  end;

  setCF(0);

  case getCX of
   clpfnGETVERSION: _GETVERSION;
   clpfnINIT: _INIT;
   clpfnDONE: _DONE;

   clpfnIDENTIFY: _clpIDENTIFY;
   clpfnOPEN: _clpOPEN;
   clpfnEMPTY: _clpEMPTY;
   clpfnSETDATA: _clpSETDATA;
   clpfnGETDATASIZE: _clpGETDATASIZE;
   clpfnGETDATA: _clpGETDATA;
   clpfnCLOSE: _clpCLOSE;

   clptitleSET: _titleSET;
   clptitleGET: _titleGET;

   clpSLEEP: _SLEEP;

   clpInstCtrlHandler: _InstCtrlHandler;
   clpUnInstCtrlHandler: _UnInstCtrlHandler;
   clpAppTerminated: _AppTerminated;

   clpTZUTC: _TZUTC;
  else
   setCF(1);
  end;

  asm
   popa
  end;
 end;

exports
 ntvdmclpRegister,
 ntvdmclpDispatch;

begin
end.
