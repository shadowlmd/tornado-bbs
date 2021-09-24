{$IFDEF MSDOS}
  {$IFNDEF DPMI32}
    {$F+}
  {$ENDIF}
{$ENDIF}

Unit
  TorInOut;

Interface

Uses
  Objects,
  tGlob,
  TorMacro,
  tMisc,
  OpCrt,
  ApCom;

Type
  TFileProc    = Procedure (Const FileNameStr: String);
  TExecProc    = Procedure (Const ExecNameStr: String);
  OutByteProc  = Procedure (B: Byte);

  ScreenOutRec = Record
    MacroTable1                     : PMacrosTable;
    XLAT                            : tArr255;
    UseMacroTable1, UseMacroTable2,
    UseMacroTable3, XLATenabled     : Boolean;
  End;

Var
  ScreenOut     : ScreenOutRec;
  ScreenOutByte : OutByteProc;
  ioTextAttr    : Byte;

Procedure InitScreenOut (LFileProc: TFileProc; LExecProc: TExecProc;
                         Size: Word; MT1: PMacrosTable);
Procedure FlushScreenOut;

Procedure SetScreenOut (State: Boolean);
Procedure SetPortOut (State: Boolean);
Function IsScreenOut: Boolean;
Function IsPortOut: Boolean;

Implementation

Uses
  Ansi,
  Avatar,
  MainComm;

Type
  UpdateAttrProc = Procedure (C: Char; Var Attr: Byte);
  ScreenWriteProc = Procedure (Const S: String);
  RelativeColorFunc = Function (OldAttr, NewAttr: Byte): String;

Const
  Macro1BeginSymbol    = Ord ('$');
  Macro1Length         = 4;
  Macro2BeginSymbol    = Ord ('\');
  Macro2Length         = 2;
  Macro3BeginSymbol    = Ord ('%');
  Macro3Length         = 2;

  MaxMacroLength       = Macro1Length;
  MaxMacroStringLength = 80;

  ExecNameLen          = 127;

  WasMacro1Begin       = 1;
  WasMacro2Begin       = 2;
  WasMacro3Begin       = 4;
  ScanForX             = 8;
  WasFileMacrosBegin   = 16;
  WasFileNameBegin     = 32;
  WasExecMacrosBegin   = 64;
  WasExecNameBegin     = 128;
  WasExecParamsBegin   = 256;
  EnabledScreenOut     = 512;
  EnabledPortOut       = 1024;

  FullOutMask          = WasMacro1Begin + WasMacro2Begin + WasMacro3Begin +
                         ScanForX + WasFileMacrosBegin + WasFileNameBegin +
                         WasExecMacrosBegin + WasExecNameBegin;
  EnableOutMask        = EnabledScreenOut + EnabledPortOut;

  FNameSymbols         = ['a'..'z', 'A'..'Z', '0'..'9', ':', '\', '_'];

Type
  PBuffer       = ^TBuffer;
  TBuffer       = Array [0..65520] Of Byte;

  TMacroBufferB = Array [0..MaxMacroLength] Of Byte;
  TMacroBufferC = Array [0..MaxMacroLength] Of Char;
  TMacroBufferS = String [MaxMacroLength];

  PMacroBuffer  = ^TMacroBuffer;
  TMacroBuffer  = Record
    Case Integer Of
      1 : (B: TMacroBufferB);
      2 : (C: TMacroBufferC);
      3 : (S: TMacroBufferS);
  End;

Var
  Buffer                   : PBuffer;
  MacroBuffer              : PMacroBuffer;
  FileProc                 : TFileProc;
  ExecProc                 : TExecProc;
  SizeOfBuffer, BufferPos  : Word;
  OutStatus                : Word;
  XNum, CurrentMacroLength : Integer;
  CurrentMacroStr          : String [MaxMacroStringLength];
  FileNameStr, ExecNameStr : String [ExecNameLen];

Function TtyColorRelative (OldAttr, NewAttr: Byte): String; Far;
Begin
  TtyColorRelative := '';
End;

Procedure TtyUpdateAttr (C: Char; Var Attr: Byte); Far;
Begin
End;

Procedure WriteStringTty (Const S: String); Far;
Var
  i, j : Integer;

Begin
  i := Pos (#12, S) + 1;

  If i = 1 Then
  Begin
    j := Pos (#7, S);

    While j > 0 Do
    Begin
      Write (Copy (S, i, j - 1));
      TorSoundBell;
      Inc (i, j);
      j := NPos (#7, S, i);
    End;

    Write (Copy (S, i, 255));
  End Else
  Begin
    Repeat
      j := NPos (#12, S, i);
      Inc (i, j);
    Until j = 0;

    ClrScr;

    j := NPos (#7, S, i);

    While j > 0 Do
    Begin
      Write (Copy (S, i, j - 1));
      TorSoundBell;
      Inc (i, j);
      j := NPos (#7, S, i);
    End;

    Write (Copy (S, i, 255));
  End;
End;

Const
  UpdateAttrProcs : Array [tEmulation] Of UpdateAttrProc =
    (AnsiUpdateAttr, TtyUpdateAttr, AvtUpdateAttr);
  ScreenWriteProcs : Array [tEmulation] Of ScreenWriteProc =
    (WriteStringAnsi, WriteStringTty, WriteStringAvt);
  RelativeColorFuncs : Array [tEmulation] Of RelativeColorFunc =
    (AnsiColorRelative, TtyColorRelative, AvtColorRelative);

Procedure SetStatus (Mask: Word; State: Boolean); Forward;

Procedure BufOutByte (B: Byte);
Begin
  If BufferPos = SizeOfBuffer Then
    FlushScreenOut;

  Buffer^ [BufferPos] := B;
  Inc (BufferPos);
  UpdateAttrProcs [R. Emu] (Char (B), ioTextAttr);
End;

Procedure BufOutString (S: String);
Var
  i, Len : Word;

Begin
  Len := Length (S);

  For i := 1 To Len Do
    UpdateAttrProcs [R. Emu] (S [i], ioTextAttr);

  While (BufferPos + Len > SizeOfBuffer) Do
  Begin
    i := SizeOfBuffer - BufferPos;
    Move (S [1], Buffer^ [BufferPos], i);
    Inc (BufferPos, i);
    FlushScreenOut;
    Delete (S, 1, i);
    Len := Length (S);
  End;

  Move (S [1], Buffer^ [BufferPos], Len);
  Inc (BufferPos, Len);
End;

Procedure NullOutByte (B: Byte); Far;
Begin
End;

Procedure NormalOutByte (B: Byte); Far;
Begin
  Case B Of
    Macro2BeginSymbol : If ScreenOut. UseMacroTable2 Then
                        Begin
                          SetStatus (WasMacro2Begin, True);
                          Exit;
                        End;
    Macro3BeginSymbol : If ScreenOut. UseMacroTable3 Then
                        Begin
                          SetStatus (WasMacro3Begin, True);
                          Exit;
                        End;
    Macro1BeginSymbol : If ScreenOut. UseMacroTable1 Then
                        Begin
                          SetStatus (WasMacro1Begin, True);
                          Exit;
                        End;
  End;

  If BufferPos = SizeOfBuffer Then
    FlushScreenOut;

  Buffer^ [BufferPos] := B;
  Inc (BufferPos);
  UpdateAttrProcs [R. Emu] (Chr (B), ioTextAttr);
End;

Procedure TrapSpecialCases (B: Byte); Far;
Var
  Ch          : Char Absolute B;
  TmpMac      : PString;
  i, j, XNumL : Integer;
  DiscardChar : Boolean;
  TmpBuf      : TMacroBuffer;
  TmpStr      : String;

Begin
  If (OutStatus And WasMacro2Begin) <> 0 Then
  Begin
    MacroBuffer^. S := MacroBuffer^. S + Ch;

    If MacroBuffer^. B [0] = Macro2Length Then
    Begin
      SetStatus (WasMacro2Begin, False);

      If ((MacroBuffer^. C [1] = '0') And
          (MacroBuffer^. C [2] in ['0'..'9'])) Or
         ((MacroBuffer^. C [1] = '1') And
          (MacroBuffer^. C [2] in ['0'..'5'])) Then
      Begin
        Val (MacroBuffer^. S, j, i);
        BufOutString (RelativeColorFuncs [R. Emu] (ioTextAttr, j +
          (ioTextAttr And $F0)));
        MacroBuffer^. B [0] := 0;
      End Else
      Begin
        BufOutByte (Macro2BeginSymbol);
        Move (MacroBuffer^. B [1], TmpBuf. B [1], Macro2Length);
        MacroBuffer^. B [0] := 0;

        For i := 1 To Macro2Length Do
          ScreenOutByte (TmpBuf. B [i]);
      End;
    End;

    Exit;
  End;

  If (OutStatus And WasMacro3Begin) <> 0 Then
  Begin
    MacroBuffer^. S := MacroBuffer^. S + Ch;

    If MacroBuffer^. B [0] = Macro3Length Then
    Begin
      SetStatus (WasMacro3Begin, False);

      If ((MacroBuffer^. C [1] = '0') And
          (MacroBuffer^. C [2] in ['0'..'9'])) Or
         ((MacroBuffer^. C [1] = '1') And
          (MacroBuffer^. C [2] in ['0'..'5'])) Then
      Begin
        Val (MacroBuffer^. S, j, i);
        BufOutString (RelativeColorFuncs [R. Emu] (ioTextAttr, (ioTextAttr And
          $0F) + (j Shl 4)));
        MacroBuffer^. B [0] := 0;
      End Else
      Begin
        BufOutByte (Macro3BeginSymbol);
        Move (MacroBuffer^. B [1], TmpBuf. B [1], Macro3Length);
        MacroBuffer^. B [0] := 0;

        For i := 1 To Macro3Length Do
          ScreenOutByte (TmpBuf. B [i]);
      End;
    End;

    Exit;
  End;

  If (OutStatus And WasMacro1Begin) <> 0 Then
  Begin
    MacroBuffer^. S := MacroBuffer^. S + Ch;

    If MacroBuffer^. B [0] = Macro1Length Then
    Begin
      SetStatus (WasMacro1Begin, False);

      If MacroBuffer^. S = 'EXEC' Then
      Begin
        SetStatus (WasExecMacrosBegin, True);
        MacroBuffer^. B [0] := 0;
        Exit;
      End;

      If MacroBuffer^. S = 'FILE' Then
      Begin
        SetStatus (WasFileMacrosBegin, True);
        MacroBuffer^. B [0] := 0;
        Exit;
      End;

      TmpMac := ScreenOut. MacroTable1^. Search (UpString (MacroBuffer^. S));

      If TmpMac <> Nil Then
      Begin
        SetStatus (ScanForX, True);
        XNum := 0;
        CurrentMacroStr := TmpMac^;
        CurrentMacroLength := Macro1Length;
        MacroBuffer^. B [0] := 0;
      End Else
      Begin
        BufOutByte (Macro1BeginSymbol);
        Move (MacroBuffer^. B [1], TmpBuf. B [1], Macro1Length);
        MacroBuffer^. B [0] := 0;

        For i := 1 To Macro1Length Do
          ScreenOutByte (TmpBuf. B [i]);
      End;
    End;

    Exit;
  End;

  If (OutStatus And ScanForX) <> 0 Then
  Begin
    If R. Emu = teAvatar Then
    Begin
      If (Ch = 'X') And (XNum < 254) Then
      Begin
        Inc (XNum);
        Exit;
      End;

      If XNum = 255 Then
      Begin
        If Ch = 'X' Then
          XNum := 254
        Else
        Begin
          XNum := 0;
          BufOutByte (25);
          BufOutByte (B);
        End;

        Exit;
      End;

      If (Ch = #25) And (XNum = 0) Then
      Begin
        XNum := 255;
        Exit;
      End;

      If XNum = 254 Then
      Begin
        XNum := B;
        Exit;
      End;
    End
    Else
      If Ch = 'X' Then
      Begin
        Inc (XNum);
        Exit;
      End;

    SetStatus (ScanForX, False);

    If XNum = 0 Then XNumL := Length (CurrentMacroStr)
                Else XNumL := CurrentMacroLength + XNum + 1;
    XNum := 0;

    TmpStr := Pad (CurrentMacroStr, XNumL);
    CurrentMacroStr := '';
    CurrentMacroLength := 0;

    For i := 1 To XNumL Do
      ScreenOutByte (Byte (TmpStr [i]));
    ScreenOutByte (B);

    Exit;
  End;

  If (OutStatus And WasExecNameBegin) <> 0 Then
  Begin
    If (OutStatus And WasExecParamsBegin) = 0 Then
    Begin
      If Ch in FNameSymbols Then
      Begin
        ExecNameStr := ExecNameStr + Ch;
        Exit;
      End
      Else
        If Ch = '{' Then
        Begin
          SetStatus (WasExecParamsBegin, True);
          ExecNameStr := ExecNameStr + ' ';
          Exit;
        End;

      DiscardChar := False;
    End Else
    Begin
      If (Ch <> '}') And (Length (ExecNameStr) < ExecNameLen) Then
      Begin
        ExecNameStr := ExecNameStr + Ch;
        Exit;
      End;

      DiscardChar := Ch = '}';
    End;

    SetStatus (WasExecNameBegin + WasExecParamsBegin, False);
    TmpStr := ExecNameStr;
    ExecNameStr := '';

    If BufferPos > 0 Then
      FlushScreenOut;
    ExecProc (TmpStr);

    If Not DiscardChar Then
      ScreenOutByte (B);

    Exit;
  End;

  If (OutStatus And WasExecMacrosBegin) <> 0 Then
  Begin
    If Ch = ':' Then
    Begin
      OutStatus := (OutStatus Or WasExecNameBegin) And Not
        (WasExecMacrosBegin + WasExecParamsBegin);
      SetStatus (OutStatus, True);
      ExecNameStr := '';
    End Else
    Begin
      SetStatus (WasExecMacrosBegin, False);
      BufOutString (Char (Macro1BeginSymbol) + 'EXEC');
      ScreenOutByte (B);
    End;

    Exit;
  End;

  If (OutStatus And WasFileNameBegin) <> 0 Then
  Begin
    If Ch in FNameSymbols Then
      FileNameStr := FileNameStr + Ch
    Else
    Begin
      SetStatus (WasFileNameBegin, False);
      TmpStr := FileNameStr;
      FileNameStr := '';

      FileProc (TmpStr);
      ScreenOutByte (B);
    End;

    Exit;
  End;

  If (OutStatus And WasFileMacrosBegin) <> 0 Then
  Begin
    SetStatus (WasFileMacrosBegin, False);

    If Ch = ':' Then
    Begin
      SetStatus (WasFileNameBegin, True);
      FileNameStr := '';
    End Else
    Begin
      BufOutString (Char (Macro1BeginSymbol) + 'FILE');
      ScreenOutByte (B);
    End;
  End;
End;

Const
  OutModes : Array [Boolean] of OutByteProc = (NormalOutByte, TrapSpecialCases);

Procedure SetStatus (Mask: Word; State: Boolean);
Begin
  If State Then OutStatus := OutStatus Or Mask
           Else OutStatus := OutStatus And Not Mask;

  If OutStatus And EnableOutMask <> 0 Then
    ScreenOutByte := OutModes [OutStatus And FullOutMask <> 0]
  Else
    ScreenOutByte := NullOutByte;
End;

Procedure SetScreenOut (State: Boolean);
Begin
  SetStatus (EnabledScreenOut, State);
End;

Procedure SetPortOut (State: Boolean);
Begin
  SetStatus (EnabledPortOut, State);
End;

Function IsScreenOut: Boolean;
Begin
  IsScreenOut := (OutStatus And EnabledScreenOut) <> 0;
End;

Function IsPortOut: Boolean;
Begin
  IsPortOut := (OutStatus And EnabledPortOut) <> 0;
End;

Procedure InitScreenOut (LFileProc: TFileProc; LExecProc: TExecProc;
                         Size: Word; MT1: PMacrosTable);
Begin
  With ScreenOut Do
  Begin
    FileProc := LFileProc;
    ExecProc := LExecProc;

    OutStatus := EnabledScreenOut + EnabledPortOut;
    ScreenOutByte := NormalOutByte;

    FillChar (XLAT, SizeOf (XLAT), 0);
    XLATenabled := False;

    SizeOfBuffer := Size;
    GetMem (Buffer, SizeOfBuffer);
    BufferPos := 0;

    New (MacroBuffer);
    MacroBuffer^. B [0] := 0;

    If MT1 = Nil Then MacroTable1 := New (PMacrosTable, Init (6))
                 Else MacroTable1 := MT1;

    UseMacroTable1 := True;
    UseMacroTable2 := True;
    UseMacroTable3 := True;
    XNum := 0;
    CurrentMacroStr := '';
    CurrentMacroLength := 0;
    ExecNameStr := '';
    FileNameStr := '';
    ioTextAttr := TextAttr;
  End;
End;

Procedure FlushScreenOut;
Var
  i, P  : Word;
  S, S1 : String;

Label
  Loop;

Begin
  P := 0;

  While P < BufferPos Do
  Begin
    i := BufferPos - P;
    If i > 255 Then
      i := 255;
    SetString (Buffer^ [P], S, i);
    Inc (P, i);

    i := 0;
    If (R. Emu <> teAvatar) And StopCodeEnable Then
    Begin

    Loop:
      i := Pos (#1, S);
      If i > 0 Then
      Begin
        S1 := Copy (S, i + 1, 255);
        SetLength (S, i - 1);
      End;
    End;

    If (OutStatus And EnabledPortOut) <> 0 Then
      If ScreenOut. XLATenabled Then
        PutString (Port, XlatStr (S, ScreenOut. XLAT))
      Else
        PutString (Port, S);

    If (OutStatus And EnabledScreenOut) <> 0 Then
      ScreenWriteProcs [R. Emu] (S);

    If i <> 0 Then
    Begin
      WaitReturn;

      If Length (S1) > 0 Then
      Begin
        S := S1;
        Goto Loop;
      End;
    End;
  End;

  BufferPos := 0;

  If ExecNameStr <> '' Then
  Begin
    SetStatus (WasExecMacrosBegin + WasExecNameBegin, False);
    S := ExecNameStr;
    ExecNameStr := '';

    ExecProc (S);
  End;

  If FileNameStr <> '' Then
  Begin
    SetStatus (WasFileMacrosBegin + WasFileNameBegin, False);
    S := FileNameStr;
    FileNameStr := '';

    FileProc (S);
  End;
End;

End.
