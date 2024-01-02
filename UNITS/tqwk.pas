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

Unit
  tQWK;

Interface

Uses
  tGlob,
  tMisc,
  Crc,
  Areas,
  skOpen,
  MainComm,
  Protocol,
  DOS,
  SysMsgs,
  Log,
  OpCrt,
  Objects,
  BinCfg;

Var
  qwkAreas : PLongIntCollection;

Procedure qwkDownLoad;
Procedure qwkUpLoad;
Procedure qwkSelect;
Procedure qwkReadList;
Procedure qwkWriteList;

Implementation

Uses
  Strings,
  skCommon;

Type
  NdxRecord = Record
    MsgPointer : LongInt;
    Conference : Byte;
  End;

  tqResult = (qAborted, qOk, qNotFound);

  PLastReadRec = ^LastReadRec;
  LastReadRec = Record
    AreaNum  : LongInt;
    LastRead : LongInt;
  End;

  PLastReadAreas = ^TLastReadAreas;
  TLastReadAreas = Object (TCollection)
    Procedure FreeItem (Item: Pointer); Virtual;
  End;

  BufArr = Array [0..65520] Of Byte;
  PBufArr = ^BufArr;

  PBufOutFile = ^tBufOutFile;
  tBufOutFile = Object
    Buf                       : PBufArr;
    FSize_, BufStart          : LongInt;
    BufPtr, BufValid, BufSize : Word;
    F                         : File;

    Constructor Init (NewBufSize: Word);
    Destructor Done;
    Procedure FOpen (Const FileName: String);
    Procedure FClose;
    Procedure FErase;
    Procedure Flush;
    Procedure Out (Var Data; DataSize: Word);
    Function FPos: LongInt;
    Function FSize: LongInt;
    Procedure FSeek (Pos: LongInt);
  End;

Var
  ID : String [8];

Function NewLastReadRec (ANum, LRead: LongInt): PLastReadRec;
Var
  P : PLastReadRec;

Begin
  New (P);
  P^. AreaNum := ANum;
  P^. LastRead := LRead;
  NewLastReadRec := P;
End;

Procedure TLastReadAreas. FreeItem (Item: Pointer);
Begin
  Dispose (PLastReadRec (Item));
End;

Constructor tBufOutFile. Init (NewBufSize: Word);
Begin
  BufSize := NewBufSize;
  GetMem (Buf, BufSize);
End;

Destructor tBufOutFile. Done;
Begin
  FreeMem (Buf, BufSize);
End;

Procedure tBufOutFile. FOpen (Const FileName: String);
Begin
  Assign (F, FileName);
  ReWrite (F, 1);
  FSize_ := 0;
  BufStart := 0;
  BufPtr := 0;
  BufValid := 0;
End;

Procedure tBufOutFile. FClose;
Begin
  Flush;
  Close (F);
  If IOResult <> 0 Then;
End;

Procedure tBufOutFile. FErase;
Begin
  Close (F);
  Erase (F);
  If IOResult <> 0 Then;
End;

Procedure tBufOutFile. Out (Var Data; DataSize: Word);
Begin
  If BufPtr + DataSize > BufSize Then
    Flush;

  Move (Data, Buf^ [BufPtr], DataSize);
  Inc (BufPtr, DataSize);
  If BufPtr > BufValid Then
    BufValid := BufPtr;
End;

Procedure tBufOutFile. Flush;
Begin
  If BufValid > 0 Then
  Begin
    BlockWrite (F, Buf^, BufValid);
    Inc (BufStart, BufValid);
    If BufStart > FSize_ Then
      FSize_ := BufStart;
    BufPtr := 0;
    BufValid := 0;
  End;
End;

Function tBufOutFile. FPos: LongInt;
Begin
  FPos := BufStart + BufPtr;
End;

Function tBufOutFile. FSize: LongInt;
{$IFNDEF VirtualPascal}
Var
  Result : LongInt;
{$ENDIF}

Begin
  Result := BufStart + BufValid;
  If Result < FSize_ Then
    Result := FSize_;
{$IFNDEF VirtualPascal}
  FSize := Result;
{$ENDIF}
End;

Procedure tBufOutFile. FSeek (Pos: LongInt);
Begin
  If (Pos >= BufStart) And (Pos <= BufStart + BufValid) Then
    BufPtr := Pos - BufStart
  Else
    If Pos <= FSize Then
    Begin
      Flush;
      Seek (F, Pos);
      BufStart := Pos;
    End;
End;

Function qwkActive (Const Name: String): Boolean;
Begin
  qwkActive := qwkAreas^. Contains (Crc32Str (Name));
End;

Function qwkGetIndex (Const Name: String): Integer;
Var
  i   : Integer;
  CRC : LongInt;

Begin
  CRC := Crc32Str (Name);

  For i := 0 To qwkAreas^. Count-1 Do
    If CRC = LongInt (qwkAreas^. At (i)) Then
    Begin
      qwkGetIndex := i;
      Exit;
    End;

  qwkGetIndex := -1;
End;

Procedure ControlDatGen (Const ID, FName: PathStr);
Var
  i       : LongInt;
  P       : PString;
  Areas   : PNotSortedCollection;
  F       : Text;
  tMA     : tMsgArea;
  S       : String;
  FileBuf : FileBufArr;

Begin
  Assign (F, FName);
  SetTextBuf (F, FileBuf, FileBufSize);
  ReWrite (F);

  If IOResult <> 0 Then
    Exit;

  WriteLn (F, Cnf. BBSname);
  WriteLn (F, Cnf. Location);
  WriteLn (F, 'XXX-XXX' {Cnf. Phone});
  WriteLn (F, Cnf. SysOp + ', Sysop');
  WriteLn (F, '0000' + ', ' + ID);
  WriteLn (F, StrDate + ',' + StrTime);
  WriteLn (F, R. Name);
  WriteLn (F);
  WriteLn (F, '0');
  WriteLn (F, '0');

  If OpenMsgAreas Then
  Begin
    Areas := New (PNotSortedCollection, Init (16, 8));
    StartMAreaReadSequence (0);
    i := 1;

    While ReadNextMsgArea (tMA) Do
    Begin
      If (tMA. ReadSec <= R. Security) And
         (tMA. ShowSec <= R. Security) And
         FlagsValid (R. Flags, tMA. ReadFlags) And
         FlagsValid (R. Flags, tMA. ShowFlags) And
         qwkActive (tMA. Name)
      Then
        Areas^. Insert (NewStr (Long2Str (i) + ' ' + ZeroMsg (tMA. Name,
          True)));

      Inc (i);
    End;

    WriteLn (F, Long2Str (Areas^. Count-1));

    For i := 0 To Areas^. Count-1 Do
    Begin
      P := PString (Areas^. At (i));
      S := ExtractWord (1, P^, SpaceOnly);
      WriteLn (F, S);
      WriteLn (F, Copy (P^, Length (S) + 2, 255));
    End;

    Dispose (Areas, Done);
    CloseMsgAreas;
  End;

  For i := 1 To WordCount (Cnf. QWKadd, SpaceAndComma) Do
  Begin
    S := DefaultName (ExtractWord (i, Cnf. QWKadd, SpaceAndComma),
      EmuExt [R. Emu], lang (laTxtFiles));
    If FileExists (S) Then
      WriteLn (F, UpString (JustFileName (S)));
  End;

  Close (F);
End;

Procedure DoorIDGen (Const FName: PathStr);
Var
  F : Text;

Begin
  Assign (F, FName);
  ReWrite (F);

  If IOResult = 0 Then
  Begin
    WriteLn (F, 'DOOR = Tornado QWK module');
    WriteLn (F, 'VERSION = ' + ExtractWord (2, NameVer, SpaceOnly));
    WriteLn (F, 'SYSTEM = ' + NameVer);
    WriteLn (F, 'CONTROLNAME = ADD');
    WriteLn (F, 'CONTROLTYPE = DROP');
    WriteLn (F, 'CONTROLTYPE = CITY');
    WriteLn (F, 'CONTROLTYPE = PASSWORD');
    WriteLn (F, 'CONTROLTYPE = BPHONE');
    WriteLn (F, 'CONTROLTYPE = HPHONE');
    Close (F);
  End;
End;

Function Long2BasicReal (InValue: LongInt): LongInt;
Var
  Expon    : LongInt;
  Negative : Boolean;

Begin
  If InValue = 0 Then
    Long2BasicReal := 0
  Else
  Begin
    Negative := InValue < 0;
    If Negative Then
      InValue := Abs (InValue);

    Expon := 152;

    If InValue < $007FFFFF Then
      While ((InValue and $00800000) = 0) Do
      Begin
        InValue := InValue Shl 1;
        Dec (Expon);
      End
    Else
      While ((InValue And $FF000000) <> 0) Do
      Begin
        InValue := InValue Shr 1;
        Inc (Expon);
      End;

    InValue := InValue And $007FFFFF;
    If Negative Then
      InValue := InValue Or $00800000;

    Long2BasicReal := InValue + (Expon Shl 24);
  End;
End;

Function QWKexport (AjustLastReads: Boolean; LRAreas: PLastReadAreas): tqResult;
Type
  MsgHdrPart2 = Record
    RecNum  : Array [1..6] Of Char;
    Term    : Byte;
    ConfNum : System. Word;
    Cargo   : Array [1..3] Of Char;
  End;

Const
{$IFDEF RealMode}
  DatBufSize = 4096;
  NdxBufSize = 1024;
{$ELSE}
  DatBufSize = 16384;
  NdxBufSize = 2048;
{$ENDIF}

Var
  i, j, MsgNumber, oArea,
  RecNumPos, MsgTxtPos    : LongInt;
  DAT, NDX                : PBufOutFile;
  S, S1                   : String;
  NDXrec                  : NdxRecord;
  MsgHdr2                 : MsgHdrPart2;
  oMsgArea                : tMsgArea;
  Len                     : Byte;
  H                       : PMsgHead;
  LineBuf                 : PChar;

Label
  EndOfProc;

Begin
  QWKexport := qOk;

  DAT := New (PBufOutFile, Init (DatBufSize));
  DAT^. FOpen (Cnf. DoorInfoDir + 'messages.dat');
  If IOResult <> 0 Then
  Begin
    Dispose (DAT, Done);
    Exit;
  End;

  NDX := New (PBufOutFile, Init (NdxBufSize));
  New (H);
  GetMem (LineBuf, MaxLineSize);

  S := Pad ('Produced by ' + NameVer + ' ' + Copyright, 128);
  DAT^. Out (S [1], Length (S));

  If OpenMsgAreas Then
  Begin
    MsgNumber := 0;
    oArea := R. MsgArea;
    oMsgArea := MsgArea;
    FillChar (MsgHdr2, SizeOf (MsgHdr2), ' ');
    MsgHdr2. Term := 225;

    StartMAreaReadSequence (0);
    i := 0;

    While ReadNextMsgArea (MsgArea) Do
    Begin
      Inc (i);

      If DrawAborted Then
      Begin
        CloseMsgAreas;
        R. MsgArea := oArea;
        MsgArea := oMsgArea;
        DAT^. FClose;
        tDeleteFile (Cnf. DoorInfoDir + 'messages.dat');
        tDeleteFile (Cnf. DoorInfoDir + 'control.dat');
        tDeleteFile (Cnf. DoorInfoDir + 'door.id');
        tDeleteFile (Cnf. DoorInfoDir + '*.ndx');
        QWKexport := qAborted;
        Goto EndOfProc;
      End;

      If (MsgArea. ReadSec > R. Security) Or
         (MsgArea. ShowSec > R. Security) Or
         (Not FlagsValid (R. Flags, MsgArea. ReadFlags)) Or
         (Not FlagsValid (R. Flags, MsgArea. ShowFlags)) Or
         (Not qwkActive (MsgArea. Name)) Or
         (Not OpenMessageArea (False, False))
      Then
        Continue;

      UpdateMAreaMacro;
      ComWrite (#13 + EmuClrEOL + EmuRelColor (Random (6) + 9), eoNoFlush);
      ComWrite (lang (laScanMsgAreas), eoMacro + eoCodes);

      NDX^. FOpen (Cnf. DoorInfoDir + LeftPadCh (Long2Str (i), '0', 3) +
        '.ndx');
      If IOResult <> 0 Then;

      j := 0;
      If R. LastRead >= 0 Then
        If MsgArea. BaseType = mbfJam Then
          j := Msg^. GetLastRead (Crc32Str (LoString (R. Name)))
        Else
          j := Msg^. GetLastRead (R. LastRead);

      If j = 0 Then
        Inc (j);

      Msg^. Seek (j);
      Msg^. SeekNext;

      If Not Msg^. SeekFound Then
        NDX^. FErase
      Else
      Begin
        NDXrec. Conference := i;
        MsgHdr2. ConfNum := i;

        While Msg^. SeekFound Do
        Begin
          Msg^. OpenMessageHeader;
          Msg^. GetWrittenDateTime (H^. MsgDateMBDT);
          MBDateTime2DosDateTime (H^. MsgDateMBDT, H^. MsgDate);
          H^. IsPriv := Msg^. GetAttribute (maPrivate);
          H^. MsgFrom := Msg^. GetFrom;
          H^. MsgTo := Msg^. GetTo;
          H^. MsgSubj := Msg^. GetSubject;
          Msg^. CloseMessage;

          If H^. IsPriv And (R. Name <> Trim (H^. MsgTo)) And
             ((MsgArea. SysOpSec > R. Security) Or
              (Not FlagsValid (R. Flags, MsgArea. SysOpFlags))) And
              (R. Name <> Trim (H^. MsgFrom)) Then
          Begin
            Msg^. SeekNext;
            Continue;
          End;

          Inc (MsgNumber);

          NDXrec. MsgPointer := Long2BasicReal ((DAT^. FPos Shr 7) + 1);
          NDX^. Out (NDXrec, SizeOf (NDXrec));

          SlashRotate;

          S := ' ' + Pad (Long2Str (MsgNumber), 7) + FormattedDate (
            H^. MsgDate, 'MM-DD-YYHH:II') + Pad (UpString (
            H^. MsgTo), 25) + Pad (UpString (H^. MsgFrom), 25) + Pad (
            H^. MsgSubj, 25) + '            0       ';
          DAT^. Out (S [1], Length (S));
          RecNumPos := DAT^. FPos;
          DAT^. Out (MsgHdr2, SizeOf (MsgHdr2));

          Msg^. OpenMessage;
          Msg^. SetTextPos (Msg^. AfterLastKludge);
          MsgTxtPos := 0;

          While Not Msg^. EndOfMessage Do
          Begin
            Msg^. GetStringPChar (LineBuf, MaxLineSize);
            If LineBuf[0] = #1 Then
              Continue;

            If StrLen (LineBuf) = 0 Then
            Begin
              S1 := #227;
              DAT^. Out (S1 [1], 1);
              Inc (MsgTxtPos);
            End Else
              While StrLen (LineBuf) <> 0 Do
              Begin
                S1 := PlaceSubStr (TrimTrail (SplitStringPChar (LineBuf, 78,
                  False)), #227, 'y') + #227;
                Len := Length (S1);
                DAT^. Out (S1 [1], Len);
                Inc (MsgTxtPos, Len);
              End;
          End;

          Msg^. CloseMessage;

          j := DAT^. FPos;
          DAT^. FSeek (RecNumPos);
          S := Pad (Long2Str (((MsgTxtPos + $7F) Shr 7) + 1),
            SizeOf (MsgHdr2. RecNum));
          DAT^. Out (S [1], Length (S));

          If IOResult <> 0 Then
          Begin
            CloseMsgAreas;
            R. MsgArea := oArea;
            MsgArea := oMsgArea;
            NDX^. FErase;
            DAT^. FErase;
            LogWrite ('!', sm (sstatDiskFull));
            QWKexport := qAborted;
            Goto EndOfProc;
          End;

          DAT^. FSeek (j);

          MsgTxtPos := 128 - (MsgTxtPos And $7F);
          If MsgTxtPos < 128 Then
          Begin
            S := Replicate (' ', MsgTxtPos);
            DAT^. Out (S [1], MsgTxtPos);
          End;

          Msg^. SeekNext;
        End;

        NDX^. FClose;
      End;

      If AjustLastReads And (R. LastRead >= 0) Then
        LRAreas^. Insert (NewLastReadRec (i, Msg^. Current));

      CloseMessageBase (Msg);
    End;

    CloseMsgAreas;
    R. MsgArea := oArea;
    MsgArea := oMsgArea;
  End;

  If DAT^. FSize <= 128 Then
  Begin
    DAT^. FErase;
    QWKexport := qNotFound;
  End
  Else
    DAT^. FClose;

EndOfProc:
  Dispose (DAT, Done);
  Dispose (NDX, Done);
  Dispose (H);
  FreeMem (LineBuf, MaxLineSize);
End;

Procedure SetLastReads (LRAreas: PLastReadAreas);
Var
  i, j, oArea, JamLR : LongInt;
  LR                 : PLastReadRec;
  oMsgArea           : tMsgArea;

Begin
  If OpenMsgAreas Then
  Begin
    JamLR := Crc32Str (LoString (R. Name));
    oArea := R. MsgArea;
    oMsgArea := MsgArea;

    For i := 0 To LRAreas^. Count-1 Do
    Begin
      LR := LRAreas^. At (i);

      If ReadMsgArea (MsgArea, LR^. AreaNum) Then
        If OpenMessageArea (False, False) Then
        Begin
          If MsgArea. BaseType = mbfJam Then j := JamLR
                                       Else j := R. LastRead;
          Msg^. SetLastRead (j, LR^. LastRead);
          CloseMessageBase (Msg);
        End;
    End;

    CloseMsgAreas;
    R. MsgArea := oArea;
    MsgArea := oMsgArea;
  End;
End;

Procedure qwkDownLoad;
Var
  LRAreas : PLastReadAreas;
  i       : Integer;
  F       : Text;
  List    : PathStr;
  S       : String;

Begin
  If qwkAreas^. Count = 0 Then
    Message (lang (laNoQWKAreas))
  Else
    If Query (lang (laQWKscan), True, ofFramed) Then
    Begin
      SetTitle ('packing QWK mail');
      LRAreas := New (PLastReadAreas, Init (0, 4));

      Case QWKexport (Query (lang (laQWKajust), True, ofFramed), LRAreas) Of
        qOk       : Begin
                      ID := UpString (DelChars ([' ', '.', ',', '/', '#', '*',
                        '?', '$'], Cnf. BBSname));
                      ControlDatGen (ID, Cnf. DoorInfoDir + 'control.dat');
                      DoorIDGen (Cnf. DoorInfoDir + 'door.id');

                      S := Long2Str (BbsLine);
                      List := Cnf. TempDir + 'qwk.' + Copy ('lst', 1, 3 -
                        Length (S)) + S;
                      Assign (F, List);
                      ReWrite (F);

                      WriteLn (F, Cnf. DoorInfoDir + 'messages.dat');
                      WriteLn (F, Cnf. DoorInfoDir + 'control.dat');
                      WriteLn (F, Cnf. DoorInfoDir + 'door.id');
                      WriteLn (F, Cnf. DoorInfoDir + '*.ndx');

                      For i := 1 To WordCount (Cnf. QWKadd, SpaceAndComma) Do
                      Begin
                        S := DefaultName (ExtractWord (i, Cnf. QWKadd,
                          SpaceAndComma), EmuExt [R. Emu], lang (laTxtFiles));
                        If FileExists (S) Then
                          WriteLn (F, S);
                      End;

                      Close (F);

                      S := Cnf. DoorInfoDir + ID + '.qwk';

                      DosShell (PlaceSubStr (PlaceSubStr (Cnf. QWKpack, '%1',
                        S), '%2', List), exCommand, False);

                      tDeleteFile (Cnf. DoorInfoDir + 'messages.dat');
                      tDeleteFile (Cnf. DoorInfoDir + 'control.dat');
                      tDeleteFile (Cnf. DoorInfoDir + 'door.id');
                      tDeleteFile (Cnf. DoorInfoDir + '*.ndx');
                      tDeleteFile (List);
                      Transfer (S, Transmit, tsNormal);
                      tDeleteFile (S);

                      If LRAreas^. Count > 0 Then
                        SetLastReads (LRAreas);
                    End;

        qNotFound : Begin
                      ComWrite (#13 + EmuClrEOL, 0);
                      Message (lang (laQWKnotFound));
                    End;
      End;

      Dispose (LRAreas, Done);
    End;
End;

Procedure qwkUpLoad;
Type
  tMsgBlock = Array [1..128] Of Char;

Var
  i, j, AreaNum, RecNum,
  Posted                  : Word;
  Block                   : tMsgBlock;
  F                       : File Of tMsgBlock;
  FMsg                    : Text;
  SR, SR1                 : SearchRec;
  MsgTo, MsgFrom, MsgSubj : String [25];
  S                       : String;
  SomeProcessed           : Boolean;
  FileBuf                 : FileBufArr;

Begin
  UpLoadToUser := '';
  AutoDL := True;
  SmartChDir (Cnf. DoorInfoDir);
  Transfer ('', Receive, tsNormal);
  AutoDL := False;
  SmartChDir (Cnf. Path);

  Posted := 0;
  SomeProcessed := False;
  ID := UpString (DelChars ([' ', '.', ',', '/', '#', '*', '?', '=', '+', '\',
    '|'], Cnf. BBSname));
  FindFirst (Cnf. DoorInfoDir + '*.REP', AnyFile-VolumeID-Hidden-Directory, SR);

  While (DosError = 0) And (SR. Name <> '') Do
  Begin
    LogWrite ('+', sm (smProcessREP) + NiceFileName (Cnf. DoorInfoDir +
      SR. Name, 60) + ' ...');
    SmartChDir (Cnf. DoorInfoDir);
    DosShell (PlaceSubStr (Cnf. QWKunpack, '%1', JustFileName (SR. Name)),
      exCommand, False);
    SmartChDir (Cnf. Path);
    If IOResult <> 0 Then;

    FindFirst (Cnf. DoorInfoDir + '*.MSG', AnyFile-VolumeID-Hidden-Directory,
      SR1);

    If DOSerror = 0 Then
    Begin
      Assign (F, Cnf. DoorInfoDir + SR1. Name);
      ReSet (F);
      Read (F, Block);

      While Not EoF (F) Do
      Begin
        Read (F, Block);

        SetString (Block [2], S, 7);
        AreaNum := Str2Long (S);
        SetString (Block [117], S, 6);
        RecNum := Str2Long (S);
        SetString (Block [22], S, 25);
        MsgTo := PrString (Trim (S));
        SetString (Block [47], S, 25);
        MsgFrom := PrString (Trim (S));
        SetString (Block [72], S, 25);
        MsgSubj := Trim (S);

        Assign (FMsg, Cnf. DoorInfoDir + 'msgtext.tor');
        SetTextBuf (FMsg, FileBuf, FileBufSize);
        ReWrite (FMsg);

        If IOResult = 0 Then
        Begin
          S := '';

          For i := 1 To RecNum Do
          Begin
            Read (F, Block);

            For j := 1 To 128 Do
              If Block [j] <> #227 Then
                S := S + Block [j]
              Else
              Begin
                WriteLn (FMsg, TrimTrail (S));
                S := '';
              End;

            If EoF (F) Then
              Break;
          End;

          Close (FMsg);

          PostFile (pmNew, Cnf. DoorInfoDir + 'msgtext.tor', AreaNum, MsgFrom, MsgTo,
            MsgSubj, '', '', MsgArea. Address, MsgArea. Address, pfAutoOpen +
            pfUseDefaultAddr);

          Inc (Posted);
          Erase (FMsg);
        End;
      End;

      Close (F);
      Erase (F);
      If IOResult <> 0 Then;

      SomeProcessed := True;
      tDeleteFile (Cnf. DoorInfoDir + SR. Name);
      LogWrite ('+', PlaceSubStr (sm (smREPok), '%1', Long2Str (Posted)));
    End Else
    Begin
      LogWrite ('!', sm (smFile) + NiceFileName (Cnf. DoorInfoDir + ID + '.MSG',
        60) + sm (smNotFound));
      Message (NiceFileName (lang (laFileName) + ID + '.MSG', 60) +
        lang (laNotFound));
    End;

  {$IFNDEF MSDOS}
    FindClose (SR1);
  {$ENDIF}
    FindNext (SR);
  End;
{$IFNDEF MSDOS}
  FindClose (SR);
{$ENDIF}
  If SomeProcessed Then
    Message (lang (laREPok));
End;

Procedure qwkSelect;
Var
  i : LongInt;
  M : PNotSortedCollection;
  S : String;

  Procedure SelectArea;
  Var
    NeedPacking : Boolean;

    Procedure SwitchArea (n: Integer);
    Var
      P : PString;
      j : Integer;

    Begin
      If (n > 0) And (n <= M^. Count) Then
      Begin
        qwkListChanged := True;
        P := PString (M^. At (n - 1));

        j := qwkGetIndex (P^);
        If j >= 0 Then
        Begin
          qwkAreas^. AtPut (j, Nil);
          NeedPacking := True;
          ComWriteLn (lang (laQWKdeleted) + P^, eoMacro + eoCodes);
        End Else
        Begin
          qwkAreas^. Insert (Pointer (Crc32Str (P^)));
          ComWriteLn (lang (laQWKadded) + P^, eoMacro + eoCodes);
        End;
      End;
    End;

  Var
    n    : Integer;
    k, P : Byte;
    S1   : String;

  Begin
    S := Trim (S);
    If S <> '' Then
    Begin
      NeedPacking := False;

      For k := 1 To WordCount (S, SpaceAndComma) Do
      Begin
        S1 := ExtractWord (k, S, SpaceAndComma);
        P := Pos ('-', S1);

        If P = 0 Then
          SwitchArea (Str2Long (S1))
        Else
          For n := Str2Long (Copy (S1, 1, P - 1)) To
                   Str2Long (Copy (S1, P + 1, 255))
          Do
            SwitchArea (n);
      End;

      If NeedPacking Then
        qwkAreas^. Pack;
    End;
  End;

Const
  Marker : Array [Boolean] Of Char = (' ', '*');

Var
  P                      : PString;
  tMA                    : tMsgArea;
  a, oAttr               : Byte;
  ShowOnlyActive, Found,
  MoreRes                : Boolean;

Label
  Loop1,
  ShowList;

Begin
  If Not OpenMsgAreas Then
    Exit;

  M := New (PNotSortedCollection, Init (16, 8));
  StartMAreaReadSequence (0);

  While ReadNextMsgArea (tMA) Do
    If (tMA. Name <> '') And (tMA. ShowSec <= R. Security) And
       FlagsValid (R. Flags, tMA. ShowFlags) And
       (WordInString (MsgGroup. Tag, tMA. Group) Or
        (MsgGroup. Tag = '') Or (tMA. Group = ''))
    Then
      M^. Insert (NewStr (tMA. Name));

  CloseMsgAreas;
  ShowOnlyActive := False;

ShowList:
  Cls;
  ComWriteLn (lang (laQWKList) + '|', eoCodes + eoMacro);
  InitMore (WhereY - 1);
  S := '';

  For i := 0 To M^. Count-1 Do
  Begin
    P := PString (M^. At (i));
    Found := qwkActive (P^);

    If Not (ShowOnlyActive And Not Found) Then
    Begin
      ComWrite (EmuRelColor (Cnf. ColorScheme [umNumber]) + LeftPad (Long2Str
        (i + 1), 5), eoNoFlush);
      ComWrite (EmuRelColor (Cnf. ColorScheme [umDot]) + '.' + Marker [Found]
        + ' ', eoNoFlush);
      ComWriteLn (EmuRelColor (Cnf. ColorScheme [umItem]) + P^, eoMacro +
        eoCodes);

      Repeat
        MoreRes := MoreNums (S, lang (laQWKenterNum), 0, True);

        If S <> '' Then
        Begin
          SelectArea;
          MoreLines := R. Lines;
          S := '';
        End Else
        Begin
          If Not MoreRes Then
            Goto Loop1;
          Break;
        End;
      Until False;
    End;
  End;

Loop1:
  ComWriteLn ('', 0);

  Repeat
    a := MenuBar (lang (laQWKcontrols), '*%-0123456789'+#13);

    Case a Of
         1 : Begin
               ShowOnlyActive := False;
               Goto ShowList;
             End;

         2 : Begin
               ShowOnlyActive := True;
               Goto ShowList;
             End;

         3 : Begin
               If qwkAreas^. Count > 0 Then
                 qwkListChanged := True;
               qwkAreas^. FreeAll;
               ComWriteLn ('|' + lang (laNoQWKAreas) + '|', eoMacro + eoCodes);
             End;

     4..13 : Begin
               S := Chr (Ord ('0') + a - 4);
               oAttr := TextAttr;
               ComWrite (#13 + EmuClrEOL, eoNoFlush);
               ComWrite (lang (laQWKenterNum), eoMacro + eoCodes);
               ComRead (S, 78 - WhereX, ofAllowEmpty);
               ComWrite (#13 + EmuClrEOL + EmuRelColor (oAttr), 0);
               SelectArea;
             End;

        14 : Break;
    End;
  Until False;

  Dispose (M, Done);
End;

Procedure qwkReadList;
Var
  F       : Text;
  S, S1   : String;
  FileBuf : FileBufArr;

Begin
  WaitAndSetFlag ('qwkfile.tbf');

  qwkAreas^. FreeAll;

  Assign (F, Cnf. Path + 'qwk.tor');
  SetTextBuf (F, FileBuf, FileBufSize);
  ReSet (F);

  If IOResult = 0 Then
  Begin
    S1 := HexL (Crc32Str (R. Name));

    While Not EoF (F) Do
    Begin
      ReadLn (F, S);

      If S [1] = '#' Then
        If Copy (S, 2, 255) = S1 Then
        Begin
          While Not EoF (F) Do
          Begin
            ReadLn (F, S);
            If S [1] = '#' Then
              Break;

            qwkAreas^. Insert (Pointer (Str2Long (S)));
          End;

          Break;
        End;
    End;

    Close (F);
  End;

  DelFlag ('qwkfile.tbf');
End;

Procedure qwkWriteList;
Var
  i           : LongInt;
  F, F1       : Text;
  S, S1       : String;
  FBuf, F1Buf : FileBufArr;

Begin
  If qwkListChanged Then
  Begin
    WaitAndSetFlag ('qwkfile.tbf');

    S1 := HexL (Crc32Str (R. Name));

    Assign (F1, Cnf. TempDir + 'qwk.tmp');
    SetTextBuf (F1, F1Buf, FileBufSize);
    ReWrite (F1);

    Assign (F, Cnf. Path + 'qwk.tor');
    SetTextBuf (F, FBuf, FileBufSize);
    ReSet (F);

    If IOResult = 0 Then
    Begin
      While Not EoF (F) Do
      Begin
        ReadLn (F, S);

        If S [1] = '#' Then
          If Copy (S, 2, 255) = S1 Then
          Begin
            While Not EoF (F) Do
            Begin
              ReadLn (F, S);
              If S [1] = '#' Then
                Break;
            End;

            Continue;
          End;

        WriteLn (F1, S);
      End;

      Close (F);
      tDeleteFile (Cnf. Path + 'qwk.tor');
    End;

    If qwkAreas^. Count > 0 Then
    Begin
      WriteLn (F1, '#' + S1);
      For i := 0 To qwkAreas^. Count-1 Do
        WriteLn (F1, Long2Str (LongInt (qwkAreas^. At (i))));
    End;

    Close (F1);
    tRenameFile (Cnf. TempDir + 'qwk.tmp', Cnf. Path + 'qwk.tor');

    DelFlag ('qwkfile.tbf');
  End;
End;

End.
