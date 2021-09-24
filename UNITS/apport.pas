{$I-,S-}
{$IFNDEF OS2}
{$R-,V-,B-,F+,A-}
{$ENDIF}

{Conditional defines that may affect this unit}
{$I APDEFINE.INC}

{*********************************************************}
{*                    APPORT.PAS 2.03                    *}
{*     Copyright (c) TurboPower Software 1991.           *}
{* Portions copyright (c) Information Technology 1989,   *}
{*    and used under license to TurboPower Software      *}
{*                 All rights reserved.                  *}
{*********************************************************}

Unit ApPort;
  {-Defines an abstract port data block and some common procedures}

Interface

Uses
{$IFNDEF OS2}
  ApMisc,
{$ELSE}
  ApOS2,
{$ENDIF}

  ApSame;

Function ComNameString (ComName: ComNameType): String;
Function CheckForString (Var Index: Byte; C: Char;
                         S: String; IgnoreCase: Boolean): Boolean;
Procedure RotateIrqPriority (Irq: Byte);

Implementation

Var
  PortExitSave : Pointer;

Function ComNameString (ComName: ComNameType): String;
  {-Returns a displayable comport name string}
Var
  S : String [3];

Begin
  Str (Ord (ComName) + 1, S);
  ComNameString := 'COM' + S;
End;

Function CheckForString (Var Index: Byte; C: Char;
                        S: String; IgnoreCase: Boolean): Boolean;
  {-Checks for string S on consecutive calls, returns True when found}
Begin
  CheckForString := False;
  Inc (Index);

  {Upcase both data if ignoring case}
  If IgnoreCase Then Begin
    C := UpCase (C);
    S [Index] := UpCase (S [Index]);
  End;

  {Compare...}
  If C = S [Index] Then
    {Got match, was it complete?}
    If Index = Length (S) Then Begin
      Index := 0;
      CheckForString := True;
    End
  Else
  Else
    {No match, reset Index}
    If C = UpCase (S [1]) Then
      Index := 1
    Else
      Index := 0;
End;

Procedure RotateIrqPriority (Irq: Byte);
  {-Rotate priorities to give Irq the highest priority at the PIC}
Var
  OCW2 : Byte;

Procedure RotatePrim (PortAddr : Byte; I : Byte);
  Begin
    If I = 0 Then
      I := 7
    Else
      Dec (I);
    OCW2 := $C0 + I;
    Port [PortAddr] := OCW2;
  End;

Begin
  If (Irq >= 8) And (Irq <= $F) Then
    {Alter slave}
    RotatePrim ($A0, Irq - 8)
  Else If (Irq >= 0) And (Irq <= 7) Then
    {Alter master}
    RotatePrim ($20, Irq)
End;

Procedure PortExitProc;
  {-Exit procedure to close any open com ports}
Var
  I : Byte;
Begin
  ExitProc := PortExitSave;
  For I := 1 To MaxActivePort Do
    If ActiveComPort [I] <> Nil Then
      ActiveComPort [I]^. DoneProc (ActiveComPort [I]);
End;

Var
  I : Integer;

Begin
  {Init all ports as available}
  For I := 1 To MaxActivePort Do
    ActiveComPort [I] := Nil;

  PortExitSave := ExitProc;
  ExitProc := @PortExitProc;
End.
