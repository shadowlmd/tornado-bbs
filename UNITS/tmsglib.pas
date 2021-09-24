{$F+,I-}

Unit tMsgLib;

Interface

Uses
  DOS,
  Objects,
  tGlob,
  tMisc;

Function SaveCollection (Const FileName: PathStr; Var C: PBigCollection;
         Tag, Version: String): Boolean;

Function ReadCollection (Const FileName: PathStr; Var C: TCollection;
         Const Tag: String; Version: String; NotSorted: Boolean): Boolean;

Implementation

Function SaveCollection;
Var
  i  : Integer;
  P  : PString;
  F  : File;
  S  : String [1];

Begin
  Version := ExtractWord (2, Version, SpaceOnly);

  Assign (F, FileName);
  System. FileMode := Open_Access_ReadWrite Or Open_Share_DenyWrite;
  ReSet (F, 1);
  System. FileMode := Open_Access_ReadOnly Or Open_Share_DenyNone;
  SaveCollection := False;

  Case IOResult Of
    0 : Seek (F, FileSize (F));
    2 : Begin
          ReWrite (F, 1);
          BlockWrite (F, Version, Length (Version) + 1);
        End;
  Else
    Exit;
  End;

  BlockWrite (F, Tag, Length (Tag) + 1);
  S := '';

  For i := 0 To C^. Count-1 Do
  Begin
    P := C^. At (i);
    If P <> Nil Then BlockWrite (F, P^, Length (P^) + 1)
                Else BlockWrite (F, S, Length (S) + 1);
  End;

  S := #255;
  BlockWrite (F, S, Length (S) + 1);

  Close (F);
  If IOResult <> 0 Then;

  SaveCollection := True;
End;

Function ReadCollection;
Const
  BufSize = 2048;

Type
  BufArr = Array [0..BufSize-1] Of Byte;

Var
  NC               : TNotSortedCollection Absolute C;
  BC               : TBigCollection Absolute C;
  BufPtr, BufCount : Word;
  Buf              : ^BufArr;
  F                : File;
  EndOfFile        : Boolean;

  Procedure FillBuf;
  Begin
    BlockRead (F, Buf^, BufSize, BufCount);
    EndOfFile := BufCount = 0;
    BufPtr := 0;
  End;

  Procedure GetString (Var S: String);
  Var
    Len, P : Integer;

  Begin
    If BufPtr >= BufCount Then
    Begin
      FillBuf;
      If EndOfFile Then
      Begin
        S := '';
        Exit;
      End;
    End;

    Len := Buf^ [BufPtr];
    Inc (BufPtr);
    P := BufCount - BufPtr;

    If P >= Len Then
      SetString (Buf^ [BufPtr], S, Len)
    Else
    Begin
      SetLength (S, Len);
      If P > 0 Then
      Begin
        Move (Buf^ [BufPtr], S [1], P);
        Dec (Len, P);
      End;
      FillBuf;
      If Len <= BufCount Then
        Move (Buf^ [BufPtr], S [P + 1], Len)
      Else
        S := '';
    End;

    Inc (BufPtr, Len);
  End;

Var
  S : String;

Begin
  ReadCollection := False;

  Assign (F, FileName);
  ReSet (F, 1);
  If IOResult <> 0 Then
    Exit;

  New (Buf);
  FillBuf;

  { Читаем версию }
  Version := ExtractWord (2, Version, SpaceOnly);
  GetString (S);

  If S = Version Then
    While Not EndOfFile Do
    Begin
      GetString (S);
      If S = Tag Then
      Begin
        If NotSorted Then NC. FreeAll
                     Else BC. FreeAll;

        Repeat
          GetString (S);
          If EndOfFile Or (S = #255) Then
            Break;

          If NotSorted Then NC. Insert (NewStr (S))
                       Else BC. InsLine (S);
        Until False;

        ReadCollection := True;
        Break;
      End;

      While Not EndOfFile And (S <> #255) Do
        GetString (S);
    End;

  Close (F);
  Dispose (Buf);
End;

End.
