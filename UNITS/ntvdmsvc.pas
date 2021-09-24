{$R-,B-,F+}
{&Use32-}
Unit
  NTVDMSVC;

Interface

Var
  NTVDMInitOk: Boolean;

Function NTVDMAppTerminated: Boolean;
Procedure NTVDMInstallCtrlHandler;
Procedure NTVDMSetSessionTitle (Title: PChar; Sz: Word);
Procedure NTVDMGetSessionTitle (Title: PChar; Sz: Word);
Procedure NTVDMSleep (Ms: Word);

Implementation

Uses
  NTVDM;

Const
  clptitleSET          = $0010;
  clptitleGET          = $0011;
  clpSLEEP             = $0020;
  clpAppTerminated     = $0030;
  clpInstCtrlHandler   = $0031;
  clpUnInstCtrlHandler = $0032;

Var
  RC, Handle: Word;
  OldExitProc: Pointer;

Function NTVDMAppTerminated: Boolean; Assembler;
Asm
  push ds

  mov ax, Handle
  mov cx, clpAppTerminated

  db $c4, $c4, $58, $02

  pop ds
End;

Procedure NTVDMInstallCtrlHandler; Assembler;
Asm
  push ds

  mov ax, Handle
  mov cx, clpInstCtrlHandler

  db $c4, $c4, $58, $02

  pop ds
End;

Procedure NTVDMUnInstallCtrlHandler; Assembler;
Asm
  push ds

  mov ax, Handle
  mov cx, clpUnInstCtrlHandler

  db $c4, $c4, $58, $02

  pop ds
End;

Procedure NTVDMSetSessionTitle (Title: PChar; Sz: Word); Assembler;
Asm
  push ds

  mov ax, Handle
  mov bx, Sz
  mov cx, clptitleSET

  lds dx, Title

  db $c4, $c4, $58, $02

  pop ds
End;

Procedure NTVDMGetSessionTitle (Title: PChar; Sz: Word); Assembler;
Asm
  push ds

  mov ax, Handle
  mov bx, Sz
  mov cx, clptitleGET

  lds dx, Title

  db $c4, $c4, $58, $02

  pop ds
End;

Procedure NTVDMSleep (Ms: Word); Assembler;
Asm
  push ds

  mov ax, Handle
  mov bx, Ms
  mov cx, clpSLEEP

  db $c4, $c4, $58, $02

  pop ds
End;

Procedure NTVDMExitProc;
Begin
  ExitProc := OldExitProc;
  NTVDMUnInstallCtrlHandler;
  vddUnload(Handle);
End;

Begin
  If vddSupported Then
    vddLoad('ntvdmclp.dll', 'ntvdmclpRegister', 'ntvdmclpDispatch', RC, Handle)
  Else
    Handle := 0;

  NTVDMInitOk := Handle <> 0;
  If NTVDMInitOk Then
  Begin
    OldExitProc := ExitProc;
    ExitProc := @NTVDMExitProc;
  End;
End.
