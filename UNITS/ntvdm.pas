{$R-,B-,F+}
unit NTVDM;

{
 NTVDM VDD interface [rm/pm]

 (q) by sergey korowkin, 2000.
 (q) by Tatyana Medvedeva, 2000.
}

interface

function vddSupported: Boolean;

procedure vddLoad(FName, Register, Dispatch: PChar; var RC, Handle: Word);
procedure vddUnload(Handle: Word);

{
 vddDispatch:

 ax=handle
 db 0c4h, 0c4h, 58h, 02h
}

implementation

{ vddSupported }

function vddSupported: Boolean; assembler;
 asm
  mov ax, 3306h

  int 21h

  xor ax, ax

  cmp bx, 3205h

  jne @@done

  inc ax

 @@done:
 end;

{ vddLoad }

procedure vddLoad(FName, Register, Dispatch: PChar; var RC, Handle: Word); assembler;
 asm
  push ds

  lds si, FName
  les di, Register
  lds bx, Dispatch

  db $c4, $c4, $58, $00

  pushf

  xor bx, bx

  popf

  jc @@done

  xchg ax, bx

 @@done:

  les di, RC
  stosw

  xchg ax, bx

  les di, Handle
  stosw

  pop ds
 end;

{ vddUnload }

procedure vddUnload(Handle: Word); assembler;
 asm
  push ds

  mov ax, Handle

  db $c4, $c4, $58, $01

  pop ds
 end;

end.
