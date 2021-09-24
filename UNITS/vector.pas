{$F+}
Unit Vector;

Interface

Procedure RestoreVecs;

Implementation

Uses
  DOS;

(*
{$IFDEF DPMI32}
Type
  Dos32Vector = Record
    IntNo, RMSeg, RMOfs, PMSel : System. Word;
    PMOfs                      : LongInt;
  End;

Procedure GetVec32 (Num: Word; Var Vector: Dos32Vector); Assembler;
          {&USES EBX,ESI} {&FRAME-}
Asm
  Mov   ESI, Vector
  Mov   EBX, Num
  Mov   EAX, $200
  Mov   [ESI].Dos32Vector.IntNo, BX
  Int   $31
  Mov   [ESI].Dos32Vector.RMSeg, CX
  Mov   [ESI].Dos32Vector.RMOfs, DX
  Mov   EAX, $204
  Mov   EBX, Num
  Int   $31
  Mov   [ESI].Dos32Vector.PMSel, CX
  Mov   [ESI].Dos32Vector.PMOfs, EDX
End;

Procedure SetVec32 (Const Vector: Dos32Vector); Assembler;
          {&USES EBX,ESI} {&FRAME-}
Asm
  Mov   ESI, Vector
  Mov   EAX, $201
  Mov   BX, [ESI].Dos32Vector.IntNo
  Mov   CX, [ESI].Dos32Vector.RMSeg
  Mov   DX, [ESI].Dos32Vector.RMOfs
  Int   $31
  Mov   EAX, $205
  Mov   BX, [ESI].Dos32Vector.IntNo
  Mov   CX, [ESI].Dos32Vector.PMSel
  Mov   EDX, [ESI].Dos32Vector.PMOfs
  Int   $31
End;
{$ENDIF}
*)

Var
  i9h, i10h, i13h, i14h, i1BH, i21h, i23h: Pointer;

Procedure RestoreVecs;
Begin
  SetIntVec ($09, i9h);
  SetIntVec ($10, i10h);
  SetIntVec ($13, i13h);
  SetIntVec ($14, i14h);
  SetIntVec ($1B, i1Bh);
  SetIntVec ($21, i21h);
  SetIntVec ($23, i23h);
End;

Begin
  GetIntVec ($09, i9h);
  GetIntVec ($10, i10h);
  GetIntVec ($13, i13h);
  GetIntVec ($14, i14h);
  GetIntVec ($1B, i1BH);
  GetIntVec ($21, i21h);
  GetIntVec ($23, i23h);
End.
