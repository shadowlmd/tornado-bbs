{$IFDEF MSDOS}
  {$IFNDEF DPMI32}
    {$F+}
  {$IFNDEF DPMI}
    {$DEFINE RealMode}
  {$ENDIF}
  {$ENDIF}
{$ENDIF}

Unit TorMacro;

Interface

Uses
  TGlob;

Type
  PMacrosTable = ^TMacrosTable;
  TMacrosTable = Object (THashTable)
    Function NewData (P: Pointer): Pointer; Virtual;
    Procedure ReplaceData (Var Old: Pointer; New: Pointer); Virtual;
    Procedure DisposeData (P: Pointer); Virtual;

    Procedure ReplaceMacro (Const MName, MStr: String);
  End;

Implementation

Uses
  Objects,
  tMisc;

Const
  MacroBlockSize = {$IFDEF RealMode} 8 {$ELSE} 16 {$ENDIF};
  MacroBlockMask = $FFFF - (MacroBlockSize - 1);

Function TMacrosTable. NewData (P: Pointer): Pointer;
Var
  M : PString;

Begin
  GetMem (M, (Length (PString (P)^) + MacroBlockSize) And MacroBlockMask);
  M^ := PString (P)^;
  NewData := M;
End;

Procedure TMacrosTable. ReplaceData (Var Old: Pointer; New: Pointer);
Var
  NewLen, OldLen : Integer;

Label
  CopyData;

Begin
  NewLen := (Length (PString (New)^) + MacroBlockSize) And MacroBlockMask;

  If Old <> Nil Then
  Begin
    OldLen := (Length (PString (Old)^) + MacroBlockSize) And MacroBlockMask;

    If OldLen = NewLen Then
      Goto CopyData;

    FreeMem (Old, OldLen);
  End;

  GetMem (Old, NewLen);

CopyData:
  PString (Old)^ := PString (New)^;
End;

Procedure TMacrosTable. DisposeData (P: Pointer);
Begin
  If P <> Nil Then
    FreeMem (P, (Length (PString (P)^) + MacroBlockSize) And MacroBlockMask);
End;

Procedure TMacrosTable. ReplaceMacro (Const MName, MStr: String);
Begin
  Insert (UpString (MName), @MStr);
End;

End.
