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
{&Delphi+}

Unit Users;

{*********************************************************}
{*                     USERS.PAS                         *}
{*                                                       *}
{*  Copyright (c) Konstantin Klyagin, 1995-98,           *}
{*                exspecially for Tornado BBS System     *}
{*                                                       *}
{*********************************************************}

Interface

Uses
  DOS,
  tMisc,
  Log,
  SysMsgs,
{$IFDEF OS2}
  Os2Base,
{$ENDIF}
{$IFDEF RealMode}
  Streams,
{$ENDIF}
  Objects,
  tGlob;

Procedure SetBaseName (Const Name: String);
Function Is_User (Name: String; AliasAllowed: Boolean): Boolean;
Procedure SaveUser (User: tUser);
Procedure SaveAndOptimise (User: tUser);
Procedure GetUser (Name: String; Var R: tUser; AliasAllowed: Boolean);
Procedure GetUserByNum (N: LongInt; Var R: tUser);
Function GetUserRecNum (Name: String; AliasAllowed: Boolean): LongInt;
Function GetUserName (Num: LongInt): String;
Function UsersNum: LongInt;
Function NextLastRead: LongInt;

Implementation

Const
  MaxBufferedRecs = 4;
  UserBaseFlag    : String [12] = 'userbase.tbf';
  BaseName        : PString = Nil;

Procedure SetBaseName (Const Name: String);
Begin
  ChangePString (BaseName, Name);
End;

Function Is_User (Name: String; AliasAllowed: Boolean): Boolean;
Var
  i, j : Integer;
  F    : File;
  Buf  : Array [1..MaxBufferedRecs] Of tUser;

Label
  Found;

Begin
  Is_User := False;

  Name := UpString (Trim (Name));
  If Name = '' Then
    Exit;

  WaitAndSetFlag (UserBaseFlag);

  If FileExists (BaseName^) Then
  Begin
    Assign (F, BaseName^);
    System. FileMode := Open_Access_ReadOnly Or Open_Share_DenyWrite;
    Reset (F, SizeOf (tUser));
    System. FileMode := Open_Access_ReadOnly Or Open_Share_DenyNone;

    If IOResult <> 0 Then
    Begin
      DelFlag (UserBaseFlag);
      LogWrite ('!', sm (smReadError) + BaseName^);
      Halt (207);
    End;

    Repeat
      BlockRead (F, Buf, MaxBufferedRecs, j);

      For i := 1 To j Do
        If (UpString (Buf [i]. Name) = Name) Or
           (AliasAllowed And (UpString (Buf [i]. Alias) = Name)) Then
        Begin
          Is_User := True;
          Goto Found;
        End;
    Until j <> MaxBufferedRecs;

  Found:
    Close (F);
  End;

  DelFlag (UserBaseFlag);
End;

Procedure SaveUser (User: tUser);
Var
  i, j : Integer;
  F    : File;
  Buf  : Array [1..MaxBufferedRecs] Of tUser;

Label
  Writing;

Begin
  WaitAndSetFlag (UserBaseFlag);

  Assign (F, BaseName^);
  System. FileMode := Open_Access_ReadWrite Or Open_Share_DenyWrite;
  Reset (F, SizeOf (tUser));
  System. FileMode := Open_Access_ReadOnly Or Open_Share_DenyNone;
  i := IOResult;

  If i = 2 Then
  Begin
    ReWrite (F, SizeOf (tUser));
    i := IOResult;
  End;

  If i = 0 Then
  Begin
    Repeat
      BlockRead (F, Buf, MaxBufferedRecs, j);

      For i := 1 To j Do
        If Buf [i]. Name = User. Name Then
        Begin
          Seek (F, FilePos (F) - (j - i + 1));
          Goto Writing;
        End;
    Until j <> MaxBufferedRecs;

  Writing:
    BlockWrite (F, User, 1, i);
    Close (F);
  End
  Else
    LogWrite ('!', 'Users base file open error: ' + Long2Str (i));

  DelFlag (UserBaseFlag);
End;

Procedure SaveAndOptimise (User: tUser);
Type
  POptRec = ^OptRec;
  OptRec = Record
    Num           : Word;
    Calls, Weight : LongInt;
  End;

Const
{$IFDEF VirtualPascal}
  MaxUsers = 65536;
{$ELSE}
  MaxUsers = 65520 Div SizeOf (OptRec);
{$ENDIF}
  Precision = 16384;

Type
  POptArray = ^OptArray;
  OptArray = Array [0..MaxUsers-1] Of OptRec;
  PPtrArray = ^PtrArray;
  PtrArray = Array [0..MaxUsers-1] Of POptRec;

Var
  BSize, MaxCalls,
  CurrDate         : LongInt;
  S                : PStream;
  OptArr           : POptArray;
  PtrArr           : PPtrArray;
  CheckO           : POptRec;
  i, j, K, L       : Word;
{$IFDEF VirtualPascal}
  AW               : Extended;
{$ELSE}
  AW               : Double;
{$ENDIF}
  DT               : DateTime;
  NewUser, Fail    : Boolean;
  F                : File;
  Buf              : Array [1..MaxBufferedRecs] Of tUser;
  TempFName        : String;

  {$S+}
  Procedure Sort (L, R: Word);
  Var
    I, J : Word;
    Tmp  : POptRec;

  Begin
    I := L;
    J := R;
    CheckO := PtrArr^ [(LongInt (L + R)) ShR 1];

    Repeat
      While PtrArr^ [I]^. Weight > CheckO^. Weight Do
        Inc (I);

      While CheckO^. Weight > PtrArr^ [J]^. Weight Do
        Dec (J);

      If I <= J Then
      Begin
        Tmp := PtrArr^ [I];
        PtrArr^ [I] := PtrArr^ [J];
        PtrArr^ [J] := Tmp;
        Inc (I);
        Dec (J);
      End;
    Until I > J;

    If L < J Then
      Sort (L, J);

    If I < R Then
      Sort (I, R);
  End;
  {$S-}

  Procedure DoneBuffers;
  Begin
    Dispose (S, Done);
    FreeMem (OptArr, BSize * SizeOf (OptRec));
    FreeMem (PtrArr, BSize * SizeOf (POptRec));
  End;

Label
  NoOpt;

Begin
  BSize := gFileSize (BaseName^) + SizeOf (tUser);

{$IFDEF RealMode}
  S := New (PXMSStream, Init (BSize, BSize));
  If S <> Nil Then
    If S^. Status <> stOk Then
    Begin
      Dispose (S, Done);
      S := Nil;
    End;

  If S = Nil Then
  Begin
    S := New (PEMSStream3, Init (BSize, BSize));
    If S <> Nil Then
      If S^. Status <> stOk Then
      Begin
        Dispose (S, Done);
        S := Nil;
      End;
  End;
{$ELSE}
  S := New (PMemoryStream, Init (BSize, 0));
  If S <> Nil Then
    If S^. Status <> stOk Then
    Begin
      Dispose (S, Done);
      S := Nil;
    End;
{$ENDIF}

  If S <> Nil Then
  Begin
    BSize := BSize Div SizeOf (tUser);
    If BSize > MaxUsers Then
    Begin
      Dispose (S, Done);
      Goto NoOpt;
    End;

    GetMem (OptArr, BSize * SizeOf (OptRec));
    GetMem (PtrArr, BSize * SizeOf (POptRec));

    WaitAndSetFlag (UserBaseFlag);

    Assign (F, BaseName^);
    System. FileMode := Open_Access_ReadOnly Or Open_Share_DenyWrite;
    Reset (F, SizeOf (tUser));
    System. FileMode := Open_Access_ReadOnly Or Open_Share_DenyNone;

    L := 0;
    MaxCalls := 0;
    NewUser := True;

    Repeat
      BlockRead (F, Buf, MaxBufferedRecs, j);

      For i := 1 To j Do
      Begin
        If NewUser And (Buf [i]. Name = User. Name) Then
        Begin
          Buf [i] := User;
          NewUser := False;
        End;

        With Buf [i] Do
        Begin
          If NoCalls > MaxCalls Then
            MaxCalls := NoCalls;
          With OptArr^ [L] Do
          Begin
            Num := L;
            Calls := NoCalls;
            Long2DT (LastDate, DT);
            DT. Hour := Hi (LastTime);
            DT. Min := Lo (LastTime);
            DT. Sec := 0;
            PackTime (DT, Weight);
          End;
        End;

        PtrArr^ [L] := @OptArr^ [L];
        Inc (L);
      End;

      S^. Write (Buf, j * SizeOf (tUser));
      If S^. Status <> stOk Then
      Begin
        Close (F);
        DelFlag (UserBaseFlag);
        DoneBuffers;
        Goto NoOpt;
      End;
    Until j <> MaxBufferedRecs;

    Close (F);

    If NewUser Then
    Begin
      With User Do
      Begin
        If NoCalls > MaxCalls Then
          MaxCalls := NoCalls;
        With OptArr^ [L] Do
        Begin
          Num := L;
          Calls := NoCalls;
          Long2DT (LastDate, DT);
          DT. Hour := Hi (LastTime);
          DT. Min := Lo (LastTime);
          DT. Sec := 0;
          PackTime (DT, Weight);
        End;
      End;

      PtrArr^ [L] := @OptArr^ [L];
      Inc (L);

      S^. Write (User, SizeOf (tUser));
      If S^. Status <> stOk Then
      Begin
        DelFlag (UserBaseFlag);
        DoneBuffers;
        Goto NoOpt;
      End;
    End;

    CurrDate := GetDosDate;

    For i := 0 To L-1 Do
      With OptArr^ [i] Do
      Begin
        AW := Weight / CurrDate;
        Weight := Round ((AW * 1.5 + (Calls / MaxCalls * 5) * ((1 - AW) * 2 +
          1)) * Precision);
      End;

    Sort (0, L - 1);

    TempFName := Long2Str (BbsLine);
    TempFName := Cnf. TempDir + 'users.' + Copy ('tmp', 1, 3 -
      Length (TempFName)) + TempFName;
    Assign (F, TempFName);
    ReWrite (F, SizeOf (tUser));

    If IOResult <> 0 Then
    Begin
      DelFlag (UserBaseFlag);
      DoneBuffers;
      Goto NoOpt;
    End;

    K := 0;
    Fail := False;

    Repeat
      j := L - K;
      If j > MaxBufferedRecs Then
        j := MaxBufferedRecs;

      For i := 1 To j Do
        If Not Fail Then
        Begin
          S^. Seek (LongInt (PtrArr^ [K + i - 1]^. Num) * SizeOf (tUser));
          Fail := S^. Status <> stOk;
          If Not Fail Then
          Begin
            S^. Read (Buf [i], SizeOf (tUser));
            Fail := S^. Status <> stOk;
          End;
        End;

      If Not Fail Then
      Begin
        BlockWrite (F, Buf, j, i);
        Fail := j <> i;
      End;

      If Fail Then
      Begin
        DelFlag (UserBaseFlag);
        Close (F);
        Erase (F);
        DoneBuffers;
        Goto NoOpt;
      End;

      Inc (K, j);
    Until K = L;

    Close (F);
    DoneBuffers;

    Assign (F, BaseName^);
    Erase (F);
    tRenameFile (TempFName, BaseName^);

    DelFlag (UserBaseFlag);
    Exit;
  End;

NoOpt:
  SaveUser (User);
End;

Procedure GetUser (Name: String; Var R: tUser; AliasAllowed: Boolean);
Var
  i, j : Integer;
  F    : File;
  Buf  : Array [1..MaxBufferedRecs] Of tUser;

Label
  Found;

Begin
  Name := UpString (Trim (Name));

  WaitAndSetFlag (UserBaseFlag);

  Assign (F, BaseName^);
  System. FileMode := Open_Access_ReadOnly Or Open_Share_DenyWrite;
  Reset (F, SizeOf (tUser));
  System. FileMode := Open_Access_ReadOnly Or Open_Share_DenyNone;

  If IOResult <> 0 Then
  Begin
    DelFlag (UserBaseFlag);
    LogWrite ('!', sm (smReadError) + BaseName^);
    Halt (207);
  End;

  Repeat
    BlockRead (F, Buf, MaxBufferedRecs, j);

    For i := 1 To j Do
      If (UpString (Buf [i]. Name) = Name) Or
         (AliasAllowed And (UpString (Buf [i]. Alias) = Name)) Then
      Begin
        R := Buf [i];
        R. Password := LoString (R. Password);
        Goto Found;
      End;
  Until j <> MaxBufferedRecs;

Found:
  Close (F);
  DelFlag (UserBaseFlag);

  If Not AliasAllowed Then
    R. Alias := '';
End;

Procedure GetUserByNum (N: LongInt; Var R: tUser);
Var
  F : File Of tUser;

Begin
  FillChar (R, SizeOf (R), 0);

  WaitAndSetFlag (UserBaseFlag);

  Assign (F, BaseName^);
  System. FileMode := Open_Access_ReadOnly Or Open_Share_DenyWrite;
  Reset (F);
  System. FileMode := Open_Access_ReadOnly Or Open_Share_DenyNone;

  Dec (N);

  If N < FileSize (F) Then
  Begin
    Seek (F, N);
    Read (F, R);

    If IOResult <> 0 Then
    Begin
      DelFlag (UserBaseFlag);
      LogWrite ('!', sm (smReadError) + BaseName^);
      Halt (207);
    End;
  End;

  Close (F);
  DelFlag (UserBaseFlag);
End;

Function UsersNum: LongInt;
Begin
  UsersNum := gFileSize (BaseName^) Div SizeOf (tUser);
End;

Function GetUserName (Num: LongInt): String;
Var
  F   : File Of tUser;
  Rec : tUser;

Begin
  WaitAndSetFlag (UserBaseFlag);

  Assign (F, BaseName^);
  System. FileMode := Open_Access_ReadOnly Or Open_Share_DenyWrite;
  Reset (F);
  System. FileMode := Open_Access_ReadOnly Or Open_Share_DenyNone;

  Seek (F, Num - 1);
  Read (F, Rec);

  If IOResult = 100 Then
  Begin
    DelFlag (UserBaseFlag);
    LogWrite ('!', sm (smReadError) + BaseName^);
    Halt (207);
  End;

  Close (F);
  DelFlag (UserBaseFlag);

  GetUserName := Rec. Name;
End;

Type
  PLRCollection = ^TLRCollection;
  TLRCollection = TSortedLongIntCollection;

Const
{$IFDEF RealMode}
  LR_Start = 64;
  LR_Delta = 16;
{$ELSE}
  LR_Start = 128;
  LR_Delta = 32;
{$ENDIF}

Function NextLastRead: LongInt;
Var
{$IFNDEF VirtualPascal}
  Result : LongInt;
{$ENDIF}
  cLR    : PLRCollection;
  i, j   : Integer;
  F      : File;
  Buf    : Array [1..MaxBufferedRecs] Of tUser;

Label
  HoleFound;

Begin
  Result := 1;

  WaitAndSetFlag (UserBaseFlag);

  Assign (F, BaseName^);
  System. FileMode := Open_Access_ReadOnly Or Open_Share_DenyWrite;
  Reset (F, SizeOf (tUser));
  System. FileMode := Open_Access_ReadOnly Or Open_Share_DenyNone;

  If IOResult = 0 Then
  Begin
    cLR := New (PLRCollection, Init (LR_Start, LR_Delta));
    cLR^. Insert (Pointer (0));

    Repeat
      BlockRead (F, Buf, MaxBufferedRecs, j);

      For i := 1 To j Do
        cLR^. Insert (Pointer (Buf [i]. LastRead));
    Until j <> MaxBufferedRecs;

    Close (F);
    DelFlag (UserBaseFlag);

    For i := 1 To cLR^. Count-1 Do
    Begin
      Result := LongInt (cLR^. At (i - 1));
      If LongInt (cLR^. At (i)) - Result > 1 Then
      Begin
        Inc (Result);
        Goto HoleFound;
      End;
    End;

    Result := LongInt (cLR^. At (cLR^. Count-1)) + 1;

  HoleFound:
    Dispose (cLR, Done);
  End
  Else
    DelFlag (UserBaseFlag);

{$IFNDEF VirtualPascal}
  NextLastRead := Result;
{$ENDIF}
End;

Function GetUserRecNum (Name: String; AliasAllowed: Boolean): LongInt;
Var
  L    : LongInt;
  i, j : Integer;
  F    : File;
  Buf  : Array [1..MaxBufferedRecs] Of tUser;

Label
  Found;

Begin
  GetUserRecNum := 0;
  Name := UpString (Trim (Name));

  WaitAndSetFlag (UserBaseFlag);

  Assign (F, BaseName^);
  System. FileMode := Open_Access_ReadOnly Or Open_Share_DenyWrite;
  Reset (F, SizeOf (tUser));
  System. FileMode := Open_Access_ReadOnly Or Open_Share_DenyNone;

  If IOResult <> 0 Then
  Begin
    DelFlag (UserBaseFlag);
    LogWrite ('!', sm (smReadError) + BaseName^);
    Halt (207);
  End;

  L := 0;

  Repeat
    BlockRead (F, Buf, MaxBufferedRecs, j);

    For i := 1 To j Do
      If (UpString (Buf [i]. Name) = Name) Or
         (AliasAllowed And (UpString (Buf [i]. Alias) = Name)) Then
      Begin
        GetUserRecNum := L + i - 1;
        Goto Found;
      End;

    Inc (L, j);
  Until j <> MaxBufferedRecs;

Found:
  Close (F);
  DelFlag (UserBaseFlag);
End;

End.
