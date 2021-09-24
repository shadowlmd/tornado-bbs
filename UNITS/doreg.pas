{$IFDEF MSDOS}
  {$IFNDEF DPMI32}
    {$F+}
  {$IFNDEF DPMI}
    {$DEFINE RealMode}
    {$O+}
  {$ENDIF}
  {$ENDIF}
{$ENDIF}
{$I-}

Unit DoReg;

{*********************************************************}
{*                       DOREG.PAS                       *}
{*                                                       *}
{*  Copyright (c) Konstantin Klyagin, 1995-98,           *}
{*                exspecially for Tornado BBS System     *}
{*                                                       *}
{*********************************************************}

Interface

Uses
  TGlob,
  Users;

Function DoRegExist: Boolean;
Function GetDoReg (Var User: tUser): Boolean;
Procedure SaveDoReg;

Implementation

Uses
  tMisc;

Type
  DoRegRec = Record
    Question : Byte;
    Date     : LongInt;
    User     : tUser;
  End;

Const
  ResRegName = 'resreg.tor';
  ResTmpName = 'resreg.t$$';

Function DoRegExist: Boolean;
Var
  F : File Of DoRegRec;
  D : DoRegRec;
  S : String;

Begin
  DoRegExist := False;
  If Not Cnf. RegResume Then
    Exit;

  Assign (F, Cnf. Path + ResRegName);
  ReSet (F);

  If IOResult = 0 Then
  Begin
    S := UpString (R. Name);

    While Not EoF (F) Do
    Begin
      Read (F, D);
      If UpString (D. User. Name) = S Then
      Begin
        DoRegExist := True;
        Break;
      End;
    End;

    Close (F);
  End;
End;

Function GetDoReg (Var User: tUser): Boolean;
Var
  CurrDate  : LongInt;
  Fin, Fout : File Of DoRegRec;
  D         : DoRegRec;
  S         : String;

Begin
  GetDoReg := False;
  If Not Cnf. RegResume Then
    Exit;

  Assign (Fin, Cnf. Path + ResRegName);
  ReSet (Fin);
  If IOResult <> 0 Then
    ReWrite (Fin);

  Assign (Fout, Cnf. TempDir + ResTmpName);
  ReWrite (Fout);

  CurrDate := DateL;
  S := UpString (R. Name);

  While Not EoF (Fin) Do
  Begin
    Read (Fin, D);
    If UpString (D. User. Name) = S Then
    Begin
      User := D. User;
      RegLet := D. Question;
      GetDoReg := True;
    End
    Else
      If CurrDate - D. Date < Cnf. DoRegExpire Then
        Write (Fout, D);
  End;

  Close (Fin);
  Close (Fout);
  Erase (Fin);
  tRenameFile (Cnf. TempDir + ResTmpName, Cnf. Path + ResRegName);
End;

Procedure SaveDoReg;
Var
  CurrDate  : LongInt;
  Written   : Boolean;
  Fin, Fout : File Of DoRegRec;
  D         : DoRegRec;
  S         : String;

Begin
  If Not Cnf. RegResume Or (R. Name = '') Or (RegLet = 0) Or
     Is_User (R. Name, Cnf. Aliases)
  Then
    Exit;

  Assign (Fin, Cnf. Path + ResRegName);
  ReSet (Fin);
  Assign (Fout, Cnf. TempDir + ResTmpName);
  ReWrite (Fout);

  CurrDate := DateL;
  S := UpString (R. Name);
  Written := False;

  While Not EoF (Fin) Do
  Begin
    Read (Fin, D);

    If CurrDate - D. Date < Cnf. DoRegExpire Then
    Begin
      If UpString (D. User. Name) = S Then
      Begin
        Written := True;
        D. Question := RegLet;
        D. Date := CurrDate;
        D. User := R;
      End;

      Write (Fout, D);
    End;
  End;

  If Not Written Then
  Begin
    D. Question := RegLet;
    D. Date := CurrDate;
    D. User := R;
    Write (Fout, D);
  End;

  Close (Fin);
  Close (Fout);
  Erase (Fin);
  tRenameFile (Cnf. TempDir + ResTmpName, Cnf. Path + ResRegName);
End;

End.
