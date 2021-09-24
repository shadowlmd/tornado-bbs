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
  FilesBBS;

Interface

Uses
{$IFDEF OS2}
  Os2Base,
  VPutils,
  VPSysLow,
{$ENDIF}
  DOS,
  Objects,
  OpCrt,
  TGlob,
  tMisc,
  SysMsgs,
  BinCfg,
  MainComm,
  mFind,
  Log;

Type
  MaskType = String [50];

Function IsFree (FileName: String): Boolean;
Function InFilesBBS (FileName: String): Boolean;
Function Tag: Boolean;
Procedure InitDispFiles;
Procedure DoneDispFiles;
Function DispFilesBBS (Const WildCard, Date: String; IsNew, Raw: Boolean;
                       Var NewFound: Boolean): Boolean;
Function DispMatchedDesc (Masks: PAsciizCollection;
                          Var NewFound: Boolean): Boolean;
Procedure OutFilesBBSString (FileName : String);
Procedure AddDescription (Const FileList: String; FileName: String;
                          Const Description: String);
Function ZeroMask (Mask: String): String;
Procedure IncFilesBBSCounter (FileName, FileList: PathStr;
                              Const Mask: MaskType; SpecFormat: Boolean);
Procedure ExportDescs;

Implementation

Const
  NumOfLines   = 20;
  FindMask     = AnyFile-VolumeID-Directory-Hidden;

  MaxNumLen    = 2;
  MaxNameLen   = 12;
  MaxFSizeLen  = 8;
  MaxLSizeLen  = 5;
  MaxDateLen   = 10;
  MaxScreenLen = 79;

  NumChar      = '#';
  SpcOrNumChar = '*';
  DLC_Chars    = [NumChar, SpcOrNumChar];

  MaxSizeLen   : Array [Boolean] Of Byte = (MaxFSizeLen, MaxLSizeLen);

  MaxFilesColl = 99;

Type
  FilesCollType = Array [0..MaxFilesColl-1] Of PFileItem;

Const
  FilesColl    : ^FilesCollType = Nil;

Var
  FilesCollCount : Integer;
  NumFile        : Byte;

Function InFilesBBS (FileName: String): Boolean;
Var
  F       : Text;
  S       : String;
  FileBuf : FileBufArr;

Begin
  InFilesBBS := False;
  FileName := UpString (Trim (FileName));
  If (FileName = '') Or (FileArea. FileList = '') Then
    Exit;

  Assign (F, FileArea. FileList);
  SetTextBuf (F, FileBuf, FileBufSize);
  ReSet (F);

  If IOResult = 0 Then
  Begin
    While Not EoF (F) Do
    Begin
      ReadLn (F, S);
      If (Length (S) > 0) And (S [1] <> ' ') Then
      Begin
        S := UpString (ExtractWord (1, S, SpaceOnly));
        If S = FileName Then
        Begin
          InFilesBBS := True;
          Break;
        End;
      End;
    End;

    Close (F);
  End;
End;

Function IsFree (FileName: String): Boolean;
Var
  F       : Text;
  S, S1   : String;
  FileBuf : FileBufArr;

Begin
  IsFree := False;
  FileName := UpString (Trim (FileName));
  If (FileName = '') Or (FileArea. FileList = '') Then
    Exit;

  Assign (F, FileArea. FileList);
  SetTextBuf (F, FileBuf, FileBufSize);
  ReSet (F);

  If IOResult = 0 Then
  Begin
    While Not EoF (F) Do
    Begin
      ReadLn (F, S);
      If (Length (S) > 0) And (S [1] <> ' ') Then
      Begin
        S1 := UpString (ExtractWord (1, S, SpaceOnly));
        If S1 = FileName Then
        Begin
          If NPos (Cnf. DLFreeKey, S, Length (S1) + 2) <> 0 Then
            IsFree := True;
          Break;
        End;
      End;
    End;

    Close (F);
  End;
End;

Function Tag: Boolean;
Var
  LastGroup, LastArea, OldGroup, OldArea : LongInt;

  Procedure TagListAdd (Num: Integer);
  Var
    PF       : PFileItem;
    CalcSize : LongInt;
    FileRec  : TTagFileRec;

    Procedure Warn (Const WarnStr: String);
    Begin
      ComWrite (EmuRelColor ($0E) + JustFileName (PF^. FileName^) + ': ',
        eoNoFlush);
      ComWriteLn (WarnStr, eoMacro + eoCodes);
    End;

  Begin
    If (Num < 1) Or (Num > FilesCollCount) Then
      Exit;

    PF := FilesColl^ [Num - 1];
    SmartChangeFArea (PF^. GroupNum, PF^. AreaNum, LastGroup, LastArea);

    If (FileArea. DL_Security > R. Security) Or
       Not FlagsValid (R. Flags, FileArea. DL_Flags) Then
    Begin
      ComWrite (#13, 0);
      ComWrite (EmuClrEOL, eoNoFlush);
      Warn (lang (laSecurityLow));
      Exit;
    End;

    If Not Local And (FileArea. MinSpeed > GetConnectSpeed) Then
    Begin
      ComWrite (#13, 0);
      ComWrite (EmuClrEOL, eoNoFlush);
      Warn (lang (laTooSlow));
      Exit;
    End;

    CalcSize := R. TodayK + ((PF^. Size + SizeOfAll) Shr 10);
    If (CalcSize < R. DailySize) Or PF^. Free Then
    Begin
      If Local Or PF^. Free Or Not (Cnf. CheckDlTime And (CalcSize >
         Round ((GetConnectSpeed / 10 * (R. TotalTime - TimeDiff (EnterTime,
         MidSec))) / 1024))) Then
      Begin
        FileRec. GroupNum := R. FileGroup;
        FileRec. AreaNum := R. FileArea;
        FileRec. Size := PF^. Size;
        FileRec. PathName := NewStr (PF^. FileName^);
        FileRec. FromName := Nil;
        FileRec. Free := PF^. Free;
        InsertInTagList (FileRec);
      End
      Else
        Warn (lang (laDLTimeLimit));
    End
    Else
      Warn (lang (laDLLimitExceed));
  End;

Var
  i, j, P, Words, sItem, eItem : Integer;
  TagItem                      : String [40];
  TagStr                       : String;

Begin
  UpdateUserMacro;
  ComWrite (#13, 0);
  ComWrite (EmuClrEOL + lang (laTagFilesNum), eoMacro + eoCodes);

  TagStr := '';
  ComRead (TagStr, MaxScreenLen - WhereX, ofAllowEmpty);

  TagStr := Trim (TagStr);
  If TagStr = '' Then
  Begin
    Tag := False;
    Exit;
  End;

  OldGroup := R. FileGroup;
  OldArea := R. FileArea;
  LastGroup := OldGroup;
  LastArea := OldArea;

  ComWrite (#13, 0);

  LogWrite ('&', 'Tagging files query: ' + TagStr);
  Words := WordCount (TagStr, SpaceAndComma);

  For i := 1 To Words Do
  Begin
    TagItem := ExtractWord (i, TagStr, SpaceAndComma);

    If ConsistsOf (TagItem, ['0'..'9', '-']) Then
    Begin
      P := Pos ('-', TagItem);
      If P = 0 Then
        TagListAdd (Str2Long (TagItem))
      Else
      Begin
        sItem := Str2Long (Copy (TagItem, 1, P - 1));
        eItem := Str2Long (Copy (TagItem, P + 1, 255));
        If sItem <= 0 Then
          sItem := 1;
        If eItem > FilesCollCount Then
          eItem := FilesCollCount;

        For j := sItem To eItem Do
          TagListAdd (j);
      End;
    End
    Else
      For j := 0 To FilesCollCount-1 Do
        If MatchWildCard (JustFileName (FilesColl^ [j]^. FileName^), TagItem)
        Then
          TagListAdd (j + 1);
  End;

  SmartChangeFArea (OldGroup, OldArea, LastGroup, LastArea);

  Tag := True;
End;

Function FormDLC (Counter: LongInt; Mask: String): String;
Var
  c                   : ^Char;
  i, MaskLen, MaskPos : Integer;
  S                   : String [20];

Begin
  MaskPos := 0;

  For i := 1 To Length (Mask) Do
    If Mask [i] in DLC_Chars Then
    Begin
      MaskPos := i;
      Break;
    End;

  If MaskPos > 0 Then
  Begin
    i := MaskPos + 1;

    While (i <= Length (Mask)) And (Mask [i] in DLC_Chars) Do
      Inc (i);

    MaskLen := i - MaskPos;

    S := Long2Str (Counter);
    If Length (S) > MaskLen Then
      S := '0';

    S := LeftPad (S, MaskLen);
    c := @(Mask [MaskPos]);

    For i := 1 To MaskLen Do
    Begin
      If (c^ = NumChar) And (S [i] = ' ') Then c^ := '0'
                                          Else c^ := S [i];
      Inc (c, SizeOf (c^));
    End;
  End;

  FormDLC := Mask;
End;

Procedure GetDLC (Var Desc, DLC: String);
Var
  i, MaskLen, MaskPos : Integer;
  S                   : String [20];

Begin
  DLC := '';
  MaskPos := 0;

  For i := 1 To Length (Cnf. DLCountMask) Do
    If Cnf. DLCountMask [i] in DLC_Chars Then
      Begin
        MaskPos := i;
        Break;
      End;

  If MaskPos > 0 Then
  Begin
    i := MaskPos + 1;

    While (i <= Length (Cnf. DLCountMask)) And
          (Cnf. DLCountMask [i] in DLC_Chars)
    Do
      Inc (i);

    MaskLen := i - MaskPos;

    i := Pos (Cnf. DLCountMask [1], Desc);
    If i > 0 Then
    Begin
      S := Trim (Copy (Desc, i + MaskPos - 1, MaskLen));
      If ConsistsOf (S, NumbersOnly) Then
      Begin
        DLC := FormDLC (Str2Long (S), Cnf. DLCountMask);
        Delete (Desc, i, Pos (Cnf. DLCountMask [Length (Cnf. DLCountMask)],
          Desc) - i + 1);
        If Desc [1] = ' ' Then
          Delete (Desc, 1, 1);
      End;
    End;
  End;
End;

Function IsLongDesc (Const BufStr: String): Boolean;
Var
  S : String [1];

Begin
  If Copy (BufStr, 1, Cnf. LongDescPos - 1) =
     Replicate (' ', Cnf. LongDescPos - 1)
  Then
    IsLongDesc := True
  Else
  Begin
    S := TrimLead (BufStr);
    IsLongDesc := S [1] = Cnf. LongDescChar;
  End;
End;

Function StripLDescPrefix (Const S: String): String;
{$IFNDEF VirtualPascal}
Var
  Result: String;
{$ENDIF}

Begin
  If Copy (S, 1, Cnf. LongDescPos - 1) = Replicate (' ', Cnf. LongDescPos - 1)
  Then
    StripLDescPrefix := Copy (S, Cnf. LongDescPos, 255)
  Else
  Begin
    Result := TrimLead (S);
    If Result [1] = Cnf. LongDescChar Then
      Delete (Result, 1, 1);
  {$IFNDEF VirtualPascal}
    StripLDescPrefix := Result;
  {$ENDIF}
  End;
End;

Procedure WriteMissingStr;
Var
  i : Integer;
  S : String;

Begin
  i := 0;
  If Cnf. FAShowSize Then
    Inc (i, MaxSizeLen [Cnf. LogicalSize] + 1);
  If Cnf. FAShowDate Then
    Inc (i, Length (Cnf. DateMask) + 1);
  Dec (i);

  If i > 0 Then
  Begin
    S := lang (laMissing);
    Inc (i, Length (S) - Length (ZeroMsg (S, True)));
    ComWrite (CenterCh (Copy (S, 1, i), ' ', i) + ' ', eoMacro + eoCodes +
      eoNoFlush);
  End;
End;

Procedure GetCDListInfo (Var Desc: String; Const FileName: String;
                         Var DirInfo: mSearchRec);
Var
  i : Integer;

Begin
  With DirInfo Do
  Begin
    Desc := TrimLead (Desc);
    i := Pos (' ', Desc);
    Info. Size := Str2Long (Copy (Desc, 1, i - 1));
    Desc := TrimLead (Copy (Desc, i + 1, 255));
    i := Pos (' ', Desc);
    Info. Time := PackStrTime (Copy (Desc, 1, i - 1), Cnf. DateMask);
    Desc := TrimLead (Copy (Desc, i + 1, 255));
    Name := FileArea. DLPath + FileName;
  End;

  DOSerror := 0;
End;

Function FilesMore: Boolean;

  Function DrawMenu: Boolean;
  Var
    Len      : Integer;
    i, oAttr : Byte;
    Ch       : Char;
    S        : String;

  Begin
    DrawMenu := True;

    MoreLines := 1;
    S := #13 + lang (laFilesMore);

    If R. Emu <> teTty Then
      S := S + EmuClrEol
    Else
    Begin
      Len := 79 - Length (ZeroMsg (S, True));
      S := S + Replicate (' ', Len) + Replicate (#8, Len);
    End;

    oAttr := TextAttr;

    Repeat
      i := MenuBar (S, lang (laYesNoKeys)+#13+#27+'0123456789');

      Case i Of
         1, 3 : Break;
         2, 4 : Begin
                  DrawMenu := False;
                  Break;
                End;
        5..14 : Begin
                  Ch := Chr (Ord ('0') + i - 5);
                  If R. HotKeys Then KeyBufAdd (Ch)
                                Else HotKeysStr := Ch + HotKeysStr;
                  Tag;
                  ComWrite (#13 + EmuRelColor (oAttr), 0);
                  ComWrite (EmuClrEOL, 0);
                End;
      End;
    Until False;

    ComWrite (#13 + EmuRelColor (oAttr), 0);
    ComWrite (EmuClrEOL, 0);
  End;

Begin
  FilesMore := Not R. More Or (MoreLines < R. Lines) Or DrawMenu;
End;

Function CalcMargin (StartMargin: Byte): Byte;
Begin
  If Cnf. FAShowSize Then
    Dec (StartMargin, MaxSizeLen [Cnf. LogicalSize] + 1);
  If Cnf. FAShowDate Then
    Dec (StartMargin, Length (Cnf. DateMask) + 1);
  If Cnf. FAShowDLC Then
    Dec (StartMargin, Length (Cnf. DLCountMask) + 1);
  CalcMargin := StartMargin;
End;

Procedure FreeFilesColl;
Var
  i : Integer;

Begin
  If FilesCollCount > 0 Then
  Begin
    For i := 0 To FilesCollCount-1 Do
      DisposeFileItem (FilesColl^ [i]);

    FillChar (FilesColl^, FilesCollCount * SizeOf (PFileItem), 0);
    FilesCollCount := 0;
  End;
End;

Procedure InitDispFiles;
Begin
  If FilesColl = Nil Then
  Begin
    New (FilesColl);
    FillChar (FilesColl^, MaxFilesColl * SizeOf (PFileItem), 0);
    FilesCollCount := 0;
  End
  Else
    FreeFilesColl;

  NumFile := 1;
  InitMore (0);
End;

Procedure DoneDispFiles;
Begin
  If FilesColl <> Nil Then
  Begin
    FreeFilesColl;
    Dispose (FilesColl);
    FilesColl := Nil;
  End;
End;

Function DispFilesBBS (Const WildCard, Date: String; IsNew, Raw: Boolean;
                       Var NewFound: Boolean): Boolean;
Var
  Fin, FinByMore, Finished : Boolean;

  Procedure Finish;
  Begin
    mL_LineMsg;
    Fin := True;
    FinByMore := True;
    Finished := True;
  End;

Var
  i                    : Integer;
  F                    : Text;
  D1, D2               : DateTime;
  FileItem             : tFileItem;
  DirInfo              : mSearchRec;
  DescMargin           : Byte;
  OrdinalList,
  SomeShowed,
  CheckWildcard,
  UseBuffer            : Boolean;
  fName                : String [80];
  sName                : String [12];
  BufStr, S, Desc, DLC : String;
  FileBuf              : FileBufArr;

Label
  ProcessLine,
  SkipLongDescs,
  SkipRaw,
  DoneLoop,
  EndOfProc;

Begin
  DispFilesBBS := True;
  Fin := False;
  FinByMore := False;
  Finished := False;
  SomeShowed := False;
  NewFound := False;
  CheckWildcard := (WildCard <> '*') And (WildCard <> '*.*');
  OrdinalList := Not IsNew And Not CheckWildcard;
  UseBuffer := IsNew And Not CheckWildcard And (FileArea. FListFormat =
    fFilesBBS);
  DescMargin := CalcMargin (MaxScreenLen - (MaxNumLen + 1 + MaxNameLen + 1));

  If Not Raw Then
    If FileArea. FileList <> '' Then
    Begin
      Assign (F, FileArea. FileList);
      SetTextBuf (F, FileBuf, FileBufSize);
      Reset (F);
      Raw := IOResult <> 0;
    End
    Else
      Raw := True;

  If Date <> '' Then
    ExtractDT (Date, 'DD-MM-YYYY HH:II', D1);

  If OrdinalList Then
    Cls;

  If Raw Then
  Begin
    mFindFirst (FileArea. DLPath, WildCard, FindMask, DirInfo);
    If DOSError <> 0 Then
    Begin
      mFindDone (DirInfo);
      If OrdinalList Then
        Message (lang (laEmptyFArea));

      Exit;
    End;

    While Not Fin Do
    Begin
      fName := DirInfo. Info. Name;

      If Not (CheckWildcard And Not MatchMultiCard (fName, WildCard)) And
         Not IsDevice (fName) Then
      Begin
        sName := Pad (fName, MaxNameLen);
        UnpackTime (DirInfo. Info. Time, D2);
        If Date <> '' Then
          If CompareDT (D1, D2) < 2 Then
            Goto SkipRaw;

        If Not NewFound Then
        Begin
          NewFound := True;

          If Not OrdinalList Then
          Begin
            ComWrite (#13#10#10, eoNoFlush);
            Inc (MoreLines, 2);

            If Not FilesMore Then
            Begin
              Finish;
              Continue;
            End;
          End;
        End;

        SomeShowed := True;

        ComWrite (EmuRelColor (Cnf. ColorScheme [flTagNum]) +
          LeftPadCh (Long2Str (NumFile), '0', 2) + ' ', eoNoFlush);
        ComWrite (EmuRelColor (Cnf. ColorScheme [flFileName]) + sName + ' ',
          eoNoFlush);

        If Cnf. FAShowSize Then
          If Not Cnf. LogicalSize Then
          Begin
            Str (DirInfo. Info. Size: MaxFSizeLen, S);
            ComWrite (EmuRelColor (Cnf. ColorScheme [flSize]) + S + ' ',
              eoNoFlush);
          End
          Else
            ComWrite (EmuRelColor (Cnf. ColorScheme [flSize]) + LeftPad
              (NiceFileSize (DirInfo. Info. Size), MaxLSizeLen) + ' ',
              eoNoFlush);

        FileItem. FileName := NewStr (DirInfo. Name);
        FileItem. Size := DirInfo. Info. Size;
        FileItem. GroupNum := R. FileGroup;
        FileItem. AreaNum := R. FileArea;
        FileItem. Free := False;

        If NumFile > FilesCollCount Then
          Inc (FilesCollCount)
        Else
          DisposeFileItem (FilesColl^ [NumFile - 1]);
        FilesColl^ [NumFile - 1] := NewFileItem (FileItem);
        Inc (NumFile);
        If NumFile = 100 Then
          NumFile := 1;

        If Cnf. FAShowDate Then
          ComWrite (EmuRelColor (Cnf. ColorScheme [flDate]) +
            FormattedDate (D2, Cnf. DateMask) + ' ', eoNoFlush);

        ComWriteLn ('', 0);
        Inc (MoreLines);

        If Not FilesMore Then
        Begin
          Finish;
          Break;
        End;

        If DrawAborted Then
        Begin
          Finished := True;
          Break;
        End;
      End;

    SkipRaw:
      mFindNext (DirInfo);
      Fin := DOSError <> 0;
    End;

    mFindDone (DirInfo);
    Goto EndOfProc;
  End;

  If UseBuffer Then
    mFindFillBuffer (FileArea. DLPath, FindMask);

  While Not Fin And Not EoF (F) Do
  Begin
    ReadLn (F, BufStr);
    PlaceSubStrP (BufStr, #9, ReplaceTabSpaces);

  ProcessLine:
    If (BufStr = '') Or (BufStr [1] = ' ') Then
    Begin
      If OrdinalList Then
      Begin
        If Trim (BufStr) = '' Then
          ComWrite (#10, 0)
        Else
          ComWriteLn (EmuRelColor (Cnf. ColorScheme [flComments]) + BufStr,
            eoColorCode + eoDisable01);

        Inc (MoreLines);
        If Not FilesMore Then
        Begin
          Finish;
          Break;
        End;
      End;
    End Else
    Begin
      fName := UpString (ExtractWord (1, BufStr, SpaceOnly));

      If Not (CheckWildcard And Not MatchMultiCard (fName, WildCard)) And
         Not IsDevice (fName) Then
      Begin
        If FileArea. FListFormat = fFilesBBS Then
        Begin
          If UseBuffer Then
            mFindSearchInBuffer (fName, DirInfo)
          Else
            mFindFirst (FileArea. DLPath, fName, FindMask, DirInfo);

          If DOSerror = 0 Then
          Begin
            UnpackTime (DirInfo. Info. Time, D2);

            If Date <> '' Then
              If CompareDT (D1, D2) < 2 Then
              Begin
                If Not UseBuffer Then
                  mFindDone (DirInfo);
                Goto SkipLongDescs;
              End;
          End;
        End;

        i := Length (fName);
        If i > MaxNameLen Then sName := Copy (fName, 1, MaxNameLen - 2) + '..'
                          Else sName := Pad (fName, MaxNameLen);
        Inc (i);
        Desc := Copy (BufStr, i, 255);
        i := Cnf. LongDescPos - i;
        If (i > 0) And ConsistsOf (Copy (Desc, 1, i), SpaceOnly) Then
          Delete (Desc, 1, i)
        Else
          Desc := TrimLead (Desc);

        i := Length (Desc);
        PlaceSubStrP (Desc, Cnf. DLFreeKey, '');
        FileItem. Free := i <> Length (Desc);

        If FileArea. FListFormat = fCDList Then
        Begin
          GetCDListInfo (Desc, fName, DirInfo);
          UnpackTime (DirInfo. Info. Time, D2);

          If Date <> '' Then
            If CompareDT (D1, D2) < 2 Then
              Goto SkipLongDescs;
        End;

        If DOSerror = 0 Then
        Begin
          If Not NewFound Then
          Begin
            NewFound := True;

            If Not OrdinalList Then
            Begin
              ComWrite (#13#10#10, eoNoFlush);
              Inc (MoreLines, 2);

              If Not FilesMore Then
              Begin
                Finish;
                Break;
              End;
            End;
          End;

          SomeShowed := True;

          ComWrite (EmuRelColor (Cnf. ColorScheme [flTagNum]) +
            LeftPadCh (Long2Str (NumFile), '0', 2) + ' ', eoNoFlush);
          ComWrite (EmuRelColor (Cnf. ColorScheme [flFileName]) + sName + ' ',
            eoNoFlush);

          If Cnf. FAShowSize Then
            If Not Cnf. LogicalSize Then
            Begin
              Str (DirInfo. Info. Size: MaxFSizeLen, S);
              ComWrite (EmuRelColor (Cnf. ColorScheme [flSize]) + S + ' ',
                eoNoFlush);
            End
            Else
              ComWrite (EmuRelColor (Cnf. ColorScheme [flSize]) + LeftPad
                (NiceFileSize (DirInfo. Info. Size), MaxLSizeLen) + ' ',
                eoNoFlush);

          FileItem. FileName := NewStr (DirInfo. Name);
          FileItem. Size := DirInfo. Info. Size;
          FileItem. GroupNum := R. FileGroup;
          FileItem. AreaNum := R. FileArea;

          If (FileArea. FListFormat = fFilesBBS) And Not UseBuffer Then
            mFindDone (DirInfo);

          If NumFile > FilesCollCount Then
            Inc (FilesCollCount)
          Else
            DisposeFileItem (FilesColl^ [NumFile - 1]);
          FilesColl^ [NumFile - 1] := NewFileItem (FileItem);
          Inc (NumFile);
          If NumFile = 100 Then
            NumFile := 1;

          If Cnf. FAShowDate Then
            ComWrite (EmuRelColor (Cnf. ColorScheme [flDate]) +
              FormattedDate (D2, Cnf. DateMask) + ' ', eoNoFlush);
        End
        Else
          If Cnf. FAShowMissing And Not IsNew Then
          Begin
            SomeShowed := True;
            ComWrite ('   ' + EmuRelColor (Cnf. ColorScheme [flFileName]) +
              sName + ' ', eoNoFlush);
            WriteMissingStr;
          End
          Else
            Goto SkipLongDescs;

        If Cnf. DLCount Then
        Begin
          GetDLC (Desc, DLC);

          If Cnf. FAShowDLC Then
          Begin
            If DLC = '' Then
              DLC := ZeroMask (Cnf. DLCountMask);
            ComWrite (EmuRelColor (Cnf. ColorScheme [flDLC]) + DLC + ' ',
              eoNoFlush);
          End;
        End;

        ComWrite (EmuRelColor (Cnf. ColorScheme [flDesc]), eoNoFlush);
        Inc (MoreLines);

        If Cnf. FAShowDLC And Cnf. DescUnderDLC Then
          i := DescMargin + Length (Cnf. DLCountMask) + 1
        Else
          i := DescMargin;

        If Cnf. CutDescs Or (Length (Desc) <= DescMargin) Then
          ComWriteLn (Copy (Desc, 1, DescMargin), eoColorCode + eoDisable01)
        Else
        Begin
          ComWriteLn (SplitString (Desc, DescMargin), eoColorCode +
            eoDisable01);

          While Length (Desc) > 0 Do
          Begin
            If Not FilesMore Then
            Begin
              Finish;
              Goto DoneLoop;
            End;

            ComWriteLn (EmuCursorRight (MaxScreenLen - i) + SplitString (Desc,
              i), eoColorCode + eoDisable01);
            Inc (MoreLines);

            If DrawAborted Then
            Begin
              Finished := True;
              Goto DoneLoop;
            End;
          End;
        End;

        If Not FilesMore Then
        Begin
          Finish;
          Break;
        End;

        If DrawAborted Then
        Begin
          Finished := True;
          Break;
        End;

        While Not EoF (F) Do
        Begin
          ReadLn (F, BufStr);
          PlaceSubStrP (BufStr, #9, ReplaceTabSpaces);

          If Copy (BufStr, 1, Cnf. LongDescPos - 1) =
             Replicate (' ', Cnf. LongDescPos - 1)
          Then
            S := Copy (BufStr, Cnf. LongDescPos, 255)
          Else
          Begin
            S := TrimLead (BufStr);
            If S [1] = Cnf. LongDescChar Then Delete (S, 1, 1)
                                         Else Goto ProcessLine;
          End;

          PlaceSubStrP (S, Cnf. DLFreeKey, '');

          While Length (S) > 0 Do
          Begin
            If Cnf. CutLongDescs Then
            Begin
              ComWriteLn (EmuCursorRight (MaxScreenLen - i) + Copy (S, 1, i),
                eoColorCode + eoDisable01);
              S := '';
            End
            Else
              ComWriteLn (EmuCursorRight (MaxScreenLen - i) + SplitString (S,
                i), eoColorCode + eoDisable01);

            Inc (MoreLines);
            If Not FilesMore Then
            Begin
              Finish;
              Goto DoneLoop;
            End;

            If DrawAborted Then
            Begin
              Finished := True;
              Goto DoneLoop;
            End;
          End;
        End;
      End Else
      Begin

      SkipLongDescs:
        While Not EoF (F) Do
        Begin
          ReadLn (F, BufStr);
          PlaceSubStrP (BufStr, #9, ReplaceTabSpaces);
          If Not IsLongDesc (BufStr) Then
            Goto ProcessLine;
        End;
      End;
    End;
  End;

DoneLoop:
  Close (F);
  If UseBuffer Then
    mFindDoneBuffer;

EndOfProc:
  If OrdinalList Then
  Begin
    ComWriteLn ('', 0);

    If SomeShowed Then
    Begin
      If Not FinByMore Then
        While Tag Do;
    End
    Else
      Message (lang (laEmptyFArea));
  End;

  If NewFound Then
    ComWriteLn ('', 0);

  DispFilesBBS := Not Finished;
End;

Function DispMatchedDesc (Masks: PAsciizCollection; Var NewFound: Boolean): Boolean;
Var
  DescLines            : PNotSortedCollection;
  i                    : Integer;
  F                    : Text;
  FT                   : DateTime;
  FileItem             : tFileItem;
  DirInfo              : mSearchRec;
  sName                : String [12];
  fName                : String [80];
  BufStr, Desc, DLC, S : String;
  DescMargin           : Byte;
  Match, BufStrValid,
  Finished             : Boolean;
  FileBuf              : FileBufArr;
  MatchedMasks         : PLongIntCollection;

Label
  ProcessNextFile;

Begin
  DispMatchedDesc := True;
  NewFound := False;

  If FileArea. FileList = '' Then
    Exit;

  Assign (F, FileArea. FileList);
  SetTextBuf (F, FileBuf, FileBufSize);
  ReSet (F);
  If IOResult <> 0 Then
    Exit;

  DescLines := New (PNotSortedCollection, Init (24, 8));
  MatchedMasks := New (PLongIntCollection, Init (Masks^. Count, 0));
  DescMargin := CalcMargin (MaxScreenLen - (MaxNumLen + 1 + MaxNameLen + 1));
  Finished := False;

  While Not EoF (F) Do
  Begin
    ReadLn (F, BufStr);
    PlaceSubStrP (BufStr, #9, ReplaceTabSpaces);

  ProcessNextFile:
    If (Length (BufStr) > 0) And (BufStr [1] <> ' ') Then
    Begin
      fName := UpString (ExtractWord (1, BufStr, SpaceOnly));

      i := Length (fName) + 1;
      Desc := Copy (BufStr, i, 255);

      i := Cnf. LongDescPos - i;
      If (i > 0) And ConsistsOf (Copy (Desc, 1, i), SpaceOnly) Then
        Delete (Desc, 1, i)
      Else
        Desc := TrimLead (Desc);

      DirInfo. Info. Time := 0;
      If FileArea. FListFormat = fCDList Then
        GetCDListInfo (Desc, fName, DirInfo);

      If Cnf. DLCount Then
        GetDLC (Desc, DLC);

      i := Length (Desc);
      PlaceSubStrP (Desc, Cnf. DLFreeKey, '');
      FileItem. Free := i <> Length (Desc);

      Match := TestMasks (Masks, MatchedMasks, Desc) Or TestMasks (Masks,
        MatchedMasks, fName);
      BufStrValid := False;

      While Not EoF (F) Do
      Begin
        ReadLn (F, BufStr);
        PlaceSubStrP (BufStr, #9, ReplaceTabSpaces);

        If Copy (BufStr, 1, Cnf. LongDescPos - 1) =
           Replicate (' ', Cnf. LongDescPos - 1)
        Then
          S := Copy (BufStr, Cnf. LongDescPos, 255)
        Else
        Begin
          S := TrimLead (BufStr);
          If S [1] = Cnf. LongDescChar Then
            Delete (S, 1, 1)
          Else
          Begin
            BufStrValid := True;
            Break;
          End;
        End;

        PlaceSubStrP (S, Cnf. DLFreeKey, '');
        If Not Match Then
          Match := TestMasks (Masks, MatchedMasks, S);
        DescLines^. Insert (NewStr (S));
      End;

      If Match And Not IsDevice (fName) Then
      Begin
        If Length (fName) > MaxNameLen Then
          sName := Copy (fName, 1, MaxNameLen - 2) + '..'
        Else
          sName := Pad (fName, MaxNameLen);

        If FileArea. FListFormat = fFilesBBS Then
          mFindFirst (FileArea. DLPath, fName, FindMask, DirInfo);

        If DOSerror = 0 Then
        Begin
          UnpackTime (DirInfo. Info. Time, FT);

          If Not NewFound Then
          Begin
            NewFound := True;
            ComWrite (#13#10#10, 0);

            Inc (MoreLines, 2);
            If Not FilesMore Then
            Begin
              Finished := True;
              Break;
            End;
          End;

          ComWrite (EmuRelColor (Cnf. ColorScheme [flTagNum]) +
            LeftPadCh (Long2Str (NumFile), '0', 2) + ' ', eoNoFlush);
          ComWrite (EmuRelColor (Cnf. ColorScheme [flFileName]) + sName + ' ',
            eoNoFlush);

          If Cnf. FAShowSize Then
            If Not Cnf. LogicalSize Then
            Begin
              Str (DirInfo. Info. Size: MaxFSizeLen, S);
              ComWrite (EmuRelColor (Cnf. ColorScheme [flSize]) + S + ' ',
                eoNoFlush);
            End
            Else
              ComWrite (EmuRelColor (Cnf. ColorScheme [flSize]) + LeftPad
                (NiceFileSize (DirInfo. Info. Size), MaxLSizeLen) + ' ',
                eoNoFlush);

          FileItem. FileName := NewStr (DirInfo. Name);
          FileItem. Size := DirInfo. Info. Size;
          FileItem. GroupNum := R. FileGroup;
          FileItem. AreaNum := R. FileArea;

          If FileArea. FListFormat = fFilesBBS Then
            mFindDone (DirInfo);

          If NumFile > FilesCollCount Then
            Inc (FilesCollCount)
          Else
            DisposeFileItem (FilesColl^ [NumFile - 1]);
          FilesColl^ [NumFile - 1] := NewFileItem (FileItem);
          Inc (NumFile);
          If NumFile = 100 Then
            NumFile := 1;

          If Cnf. FAShowDate Then
            ComWrite (EmuRelColor (Cnf. ColorScheme [flDate]) +
              FormattedDate (FT, Cnf. DateMask) + ' ', eoNoFlush);

          If Cnf. DLCount And Cnf. FAShowDLC Then
          Begin
            If DLC = '' Then
              DLC := ZeroMask (Cnf. DLCountMask);
            ComWrite (EmuRelColor (Cnf. ColorScheme [flDLC]) + DLC + ' ',
              eoNoFlush);
          End;

          ComWrite (EmuRelColor (Cnf. ColorScheme [flDesc]), eoNoFlush);
          Inc (MoreLines);

          If Cnf. CutDescs Or (Length (Desc) <= DescMargin) Or
             (Cnf. CutLongDescs And (DescLines^. Count > 0))
          Then
            ComWriteLn (Copy (Desc, 1, DescMargin), eoColorCode + eoDisable01)
          Else
          Begin
            ComWriteLn (SplitString (Desc, DescMargin), eoColorCode +
              eoDisable01);

            If Cnf. FAShowDLC And Cnf. DescUnderDLC Then
              i := DescMargin + Length (Cnf. DLCountMask) + 1
            Else
              i := DescMargin;

            While Length (Desc) > 0 Do
            Begin
              If Not FilesMore Or DrawAborted Then
              Begin
                Finished := True;
                Break;
              End;

              ComWriteLn (EmuCursorRight (MaxScreenLen - i) + SplitString
                (Desc, i), eoColorCode + eoDisable01);
              Inc (MoreLines);
            End;
          End;

          Finished := Finished Or DrawAborted Or Not FilesMore;
          i := 0;

          While (i < DescLines^. Count) And Not Finished Do
          Begin
            If Cnf. CutLongDescs Then
            Begin
              ComWriteLn (EmuCursorRight (MaxScreenLen - DescMargin) +
                Copy (PString (DescLines^. At (i))^, 1, DescMargin),
                eoColorCode + eoDisable01);
              Inc (MoreLines);
              Finished := Not FilesMore Or DrawAborted;
            End Else
            Begin
              S := PString (DescLines^. At (i))^;

              While (Length (S) > 0) And Not Finished Do
              Begin
                ComWriteLn (EmuCursorRight (MaxScreenLen - DescMargin) +
                  SplitString (S, DescMargin), eoColorCode + eoDisable01);
                Inc (MoreLines);
                Finished := Not FilesMore Or DrawAborted;
              End;
            End;

            Inc (i);
          End;

          If Finished Then
            Break;
        End;
      End;

      If Not BufStrValid Then
        Break;

      DescLines^. FreeAll;
      MatchedMasks^. DeleteAll;
      Goto ProcessNextFile;
    End;
  End;

  Close (F);
  Dispose (MatchedMasks, Done);
  Dispose (DescLines, Done);

  If NewFound Then
    ComWriteLn ('', 0);

  DispMatchedDesc := Not Finished;
End;

Procedure OutFilesBBSString;
Var
  i                      : Integer;
  DescMargin             : Byte;
  NoFilesBBS, FileFound,
  NextLong               : Boolean;
  F                      : Text;
  FT                     : DateTime;
  DirInfo                : mSearchRec;
  BufStr, Desc, S        : String;
  FileBuf                : FileBufArr;

Begin
  FileName := UpString (Trim (FileName));
  DescMargin := CalcMargin (MaxScreenLen - (MaxNameLen + 1));

  Assign (F, FileArea. FileList);
  SetTextBuf (F, FileBuf, FileBufSize);
  ReSet (F);
  NoFilesBBS := (IOResult <> 0);

  FileFound := False;
  Desc := '';

  If Not NoFilesBBS Then
    While Not EoF (F) Do
    Begin
      ReadLn (F, BufStr);
      PlaceSubStrP (BufStr, #9, ReplaceTabSpaces);

      If (Length (BufStr) > 0) And (BufStr [1] <> ' ') Then
      Begin
        S := UpString (ExtractWord (1, BufStr, SpaceOnly));
        If S = FileName Then
        Begin
          i := Length (S) + 1;
          Desc := Copy (BufStr, i, 255);
          i := Cnf. LongDescPos - i;
          If (i > 0) And ConsistsOf (Copy (Desc, 1, i), SpaceOnly) Then
            Delete (Desc, 1, i)
          Else
            Desc := TrimLead (Desc);
          PlaceSubStrP (Desc, Cnf. DLFreeKey, '');
          FileFound := True;
          Break;
        End;
      End;
    End;

  DirInfo. Info. Time := 0;
  If FileFound And (FileArea. FListFormat = fCDList) Then
    GetCDListInfo (Desc, FileName, DirInfo)
  Else
    mFindFirst (FileArea. DLPath, FileName, FindMask, DirInfo);

  If Length (FileName) > MaxNameLen Then
    S := Copy (FileName, 1, MaxNameLen - 2) + '..'
  Else
    S := Pad (FileName, MaxNameLen);
  ComWrite (EmuRelColor (Cnf. ColorScheme [flFileName]) + S + ' ', eoNoFlush);

  If DirInfo. Info. Time <> 0 Then
  Begin
    If Cnf. FAShowSize Then
      If Not Cnf. LogicalSize Then
      Begin
        Str (DirInfo. Info. Size: MaxFSizeLen, S);
        ComWrite (EmuRelColor (Cnf. ColorScheme [flSize]) + S + ' ', eoNoFlush);
      End
      Else
        ComWrite (EmuRelColor (Cnf. ColorScheme [flSize]) + LeftPad
          (NiceFileSize (DirInfo. Info. Size), MaxLSizeLen) + ' ', eoNoFlush);

    If Cnf. FAShowDate Then
    Begin
      UnpackTime (DirInfo. Info. Time, FT);
      ComWrite (EmuRelColor (Cnf. ColorScheme [flDate]) +
        FormattedDate (FT, Cnf. DateMask) + ' ', eoNoFlush);
    End;
  End
  Else
    WriteMissingStr;

  If Not (FileFound And (FileArea. FListFormat = fCDList)) Then
    mFindDone (DirInfo);

  If Cnf. DLCount Then
  Begin
    GetDLC (Desc, S);
    If Cnf. FAShowDLC Then
    Begin
      If S = '' Then
        S := ZeroMask (Cnf. DLCountMask);
      ComWrite (EmuRelColor (Cnf. ColorScheme [flDLC]) + S + ' ', eoNoFlush);
    End;
  End;

  If FileFound And Not EoF (F) Then
  Begin
    ReadLn (F, BufStr);
    PlaceSubStrP (BufStr, #9, ReplaceTabSpaces);
    NextLong := IsLongDesc (BufStr);
  End
  Else
    NextLong := False;

  ComWrite (EmuRelColor (Cnf. ColorScheme [flDesc]), eoNoFlush);

  If Cnf. CutDescs Or (Length (Desc) <= DescMargin) Or
     (Cnf. CutLongDescs And NextLong)
  Then
    ComWriteLn (Copy (Desc, 1, DescMargin), eoColorCode + eoDisable01)
  Else
  Begin
    ComWriteLn (SplitString (Desc, DescMargin), eoColorCode + eoDisable01);
    If Cnf. FAShowDLC And Cnf. DescUnderDLC Then
      i := DescMargin + Length (Cnf. DLCountMask) + 1
    Else
      i := DescMargin;

    While (Length (Desc) > 0) And Not DrawAborted Do
      ComWriteLn (EmuCursorRight (MaxScreenLen - i) + SplitString (Desc, i),
        eoColorCode + eoDisable01);
  End;

  i := 0;
  While NextLong Do
  Begin
    BufStr := PlaceSubStr (StripLDescPrefix (BufStr), Cnf. DLFreeKey, '');
    If Cnf. CutLongDescs Then
    Begin
      ComWriteLn (EmuCursorRight (MaxScreenLen - DescMargin) +
        Copy (BufStr, 1, DescMargin), eoColorCode + eoDisable01);
      Inc (i);
    End
    Else
      While (Length (BufStr) > 0) And (i <= NumOfLines) Do
      Begin
        ComWriteLn (EmuCursorRight (MaxScreenLen - DescMargin) +
          SplitString (BufStr, DescMargin), eoColorCode + eoDisable01);
        Inc (i);
      End;

    If DrawAborted Or (i > NumOfLines) Or EoF (F) Then
      Break;

    ReadLn (F, BufStr);
    PlaceSubStrP (BufStr, #9, ReplaceTabSpaces);
    NextLong := IsLongDesc (BufStr);
  End;

  If Not NoFilesBBS Then
    Close (F);
End;

Function ZeroMask;
Var
  i : Byte;

Begin
  For i := 1 To Length (Mask) Do
    Case Mask [i] of
           NumChar : Mask [i] := '0';
      SpcOrNumChar : Mask [i] := ' ';
    End;
  ZeroMask := Mask;
End;

Procedure AddDescription;
Var
  F    : File;
  C    : Byte;
  IsCR : Boolean;

Begin
  Assign (F, FileList);
  System. FileMode := Open_Access_ReadWrite Or Open_Share_DenyWrite;
  Reset (F, 1);
  System. FileMode := Open_Access_ReadOnly Or Open_Share_DenyNone;
  If IOResult <> 0 Then
    ReWrite (F, 1);

  If IOResult = 0 Then
    If FileSize (F) <> 0 Then
    Begin
      Seek (F, FileSize (F) - 1);
      BlockRead (F, C, 1);
      IsCR := (C = 13) Or (C = 10);
    End
    Else
      IsCR := True;

  If Length (FileName) < 12 Then
    FileName := Pad (FileName, 12);
  If Not IsCR Then
    FileName := #13#10 + FileName;
  FileName := FileName + ' ' + Description + #13#10;

  BlockWrite (F, FileName [1], Length (FileName));
  Close (F);
End;

Procedure IncFilesBBSCounter;
Var
  R          : {$IFNDEF OS2} Integer; {$ELSE} LongInt; {$ENDIF}
  Counter    : Integer;
  DescPos    : Byte;
  Counted    : Boolean;
  FiLi, FiOu : Text;
  S          : MaskType;
  Desc       : String [242];
  T1, ZM,
  BufStr     : String;

  Procedure IncBufStr; { Увеличивает значение счетчика в строке BufStr }
  Var
    i, PosS       : Byte;
    F, Fl, Fl1    : Boolean;
    S1            : String;

  Label
    Cont;

  Begin
    {If DescPos = 1 Then DescPos := Length (T1)+2;
    DescPos := Pos (ExtractWord (2, BufStr, SpaceOnly), BufStr);}

    S := '';
    S1 := '';
    PosS := 0;
    F := Mask [1] in DLC_Chars;
    Fl := F;
    If Not F Then S1 := S1 + BufStr [DescPos];
    Fl1 := False;

    For i := 2 To Length (Mask) Do
    Begin
      F := Mask [i] in DLC_Chars;
      Fl1 := Fl And (Not F) Or Fl1;
      Fl := F;
      If F And Not Fl1 Then
      Begin
        If PosS = 0 Then PosS := i;
        S  := S  + BufStr [DescPos+i-1];
        S1 := S1 + Mask [i];
      End Else
        S1 := S1 + BufStr [DescPos+i-1];
    End;

    { Если BufStr=...'[000]' и Mask='[***]', то S='000' и S1='[***]' }

    i := Length (S);
    Val (S, Counter, R);
    Inc (Counter);
    Str (Counter, S);
    S := LeftPad (S, i);

    {If i < Length (S) Then Delete (S, 1, 1); { Inc(999, '[###]' = '[000]'}
    S := FormDLC (Counter, Mask);
    Delete (BufStr, DescPos, Length (Mask));
    Insert (S, BufStr, DescPos);

    {
    For i := 1 To Byte (S [0]) Do
     If Mask [PosS+I-1] = NumChar Then
      If S [i] = ' ' Then S [i] := '0';

    For i := 1 To Byte (S [0]) Do
     BufStr [PosS+DescPos-2+i] := S [i];
    }
  End;

Var
  TempFBBSName : PathStr;
Label
  SkipLine, Proceed;

Begin
  { Проверим на наличие files.bbs. Нет его - создадим с                     }
  { дефолтовыми описаниями. Но пока без описаний :)                         }

  FileName := Trim (FileName);
  If (FileName = '') Or (FileList = '') Then
    Exit;

  Assign (FiLi, FileList);
  ReSet (FiLi);

  If IOResult <> 0 Then
  Begin
    LogWrite ('!', sm (smFile) + FileList + sm (smNotFound));
    Exit;
  End;

  TempFBBSName := DelChars ([':'], StrTime) + '.' +
    LeftPadCh (Long2Str (Random (999)), '0', 3);

  Assign (FiOu, Cnf. TempDir + TempFBBSName);
  ReWrite (FiOu);

  If IOResult <> 0 Then
  Begin
    Close (FiLi);
    Exit;
  End;

  While Not EoF (FiLi) Do
  Begin
    ReadLn (FiLi, BufStr);
    R := Pos (' ', BufStr);
    If R <> 0 Then T1 := Copy (BufStr, 1, R - 1)
              Else T1 := BufStr;
    If (BufStr = '') Or (BufStr [1] = ' ') Or
       (UpString (T1) <> UpString (FileName))
    Then
      Goto SkipLine;

    S := '';
    If SpecFormat Then
      DescPos := WordPosition (4, BufStr, SpaceOnly)
    Else
      DescPos := Pos (' ', BufStr) + 1;

    If DescPos = 1 then
    Begin
      Desc := '';
      Counted := False;
    End Else
    Begin
      Desc := Copy (BufStr, DescPos, 255);
      Counted := True;
      While Ord (Desc [1]) <= 32 Do
      Begin
        Inc (DescPos);
        Delete (Desc, 1, 1);
      End;
    End;

    For R := 1 To Byte (Mask [0]) Do
      If Counted Then
        If (Mask [R] = NumChar) Or (Mask [R] = SpcOrNumChar) Then
        Begin
          If Not (Desc [R] in [' ', '0'..'9']) Then
            Counted := False;
        End
        Else
          If Not (Desc [R] = Mask [R]) Then
            Counted := False;

    If Counted Then
      Goto Proceed;
    ZM := ' ' + ZeroMask (Mask);
    If Byte (Desc [0]) = 0 Then BufStr := T1 + ZM
                           Else BufStr := T1 + ZM + ' ' + Desc;
    Desc := Copy (ZM, 2, 255) + ' ' + Desc;

  Proceed:
    IncBufStr;

  SkipLine:
    WriteLn (FiOu, BufStr);
  End;

  Close (FiLi);
  Close (FiOu);
  SetFAttr (FiLi, Archive);
  Erase (FiLi);
  DOSerror := IOResult;

  tRenameFile (Cnf. TempDir + TempFBBSName, FileList);
End;

Procedure ExportDescs;
Var
  FilesColl             : PStringCollection;
  InFileBuf, OutFileBuf : PFileBufArr;
  Out                   : Text;

  Procedure CopyDescriptions (Const FileList: PathStr);
  Var
    i     : Integer;
    FList : Text;
    S, S1 : String;

  Label
    ProcessNextFile;

  Begin
    Assign (FList, FileList);
    SetTextBuf (FList, InFileBuf^, FileBufSize);
    ReSet (FList);
    If IOResult <> 0 Then
      Exit;

    While Not EoF (FList) Do
    Begin
      ReadLn (FList, S);
      PlaceSubStrP (S, #9, ReplaceTabSpaces);

    ProcessNextFile:
      If (Length (S) > 0) And (S [1] <> ' ') Then
      Begin
        S1 := UpString (ExtractWord (1, S, SpaceOnly));

        If FilesColl^. Search (@S1, i) Then
        Begin
          Delete (S, 1, Length (S1) + 1);
          If FileArea. FListFormat = fCDList Then
            Delete (S, 1, WordPosition (3, S, SpaceOnly) - 1);
          If Cnf. DLCount Then
            GetDLC (S, S1);
          WriteLn (Out, PString (FilesColl^. At (i))^ + ' ' +
            PlaceSubStr (S, Cnf. DLFreeKey, ''));

          FilesColl^. AtFree (i);

          While Not EoF (FList) Do
          Begin
            ReadLn (FList, S);
            PlaceSubStrP (S, #9, ReplaceTabSpaces);

            If Not IsLongDesc (S) Then
            Begin
              If FilesColl^. Count = 0 Then
              Begin
                Close (FList);
                Exit;
              End;

              Goto ProcessNextFile;
            End;

            WriteLn (Out, Replicate (' ', MaxNameLen + 1) +
              PlaceSubStr (StripLDescPrefix (S), Cnf. DLFreeKey, ''));
          End;

          If FilesColl^. Count = 0 Then
          Begin
            Close (FList);
            Exit;
          End;
        End;
      End;
    End;

    FilesColl^. FreeAll;
    Close (FList);
  End;

Var
  LastGroup, LastArea,
  OldGroup, OldArea    : LongInt;
  i                    : Integer;
  DirInfo              : SearchRec;
  FileRec              : TTagFileRec;
  DescsFile            : PathStr;

Begin
  Case Cnf. DLDescs Of
     atNo : Exit;
    atAsk : If Not Query (lang (laDownLoadDescs), True, ofFramed) Then
              Exit;
  End;

  If AutoDL Or (F2Transfer^. Count = 0) Then
    Exit;

  New (InFileBuf);
  New (OutFileBuf);
  FilesColl := New (PStringCollection, Init (16, 8));

  DescsFile := Cnf. DoorInfoDir + 'file_id.1';
  Assign (Out, DescsFile);
  SetTextBuf (Out, OutFileBuf^, FileBufSize);
  Append (Out);
  If IOResult <> 0 Then
    ReWrite (Out);

  OldGroup := R. FileGroup;
  OldArea := R. FileArea;
  LastGroup := OldGroup;
  LastArea := OldArea;

  OpenFileGroups;
  OpenFileAreas;
  i := 0;

  While i < F2Transfer^. Count Do
    With PTagFileRec (F2Transfer^. At (i))^ Do
      If AreaNum > 0 Then
      Begin
        SmartChangeFArea (GroupNum, AreaNum, LastGroup, LastArea);
        FilesColl^. Insert (NewStr (UpString (Trim (JustFileName (PathName^)))));
        Inc (i);

        While i < F2Transfer^. Count Do
          With PTagFileRec (F2Transfer^. At (i))^ Do
            If (AreaNum = LastArea) And (GroupNum = LastGroup) Then
            Begin
              FilesColl^. Insert (NewStr (UpString (Trim (JustFileName
                (PathName^)))));
              Inc (i);
            End
            Else
              Break;

        CopyDescriptions (FileArea. FileList);
      End
      Else
        Inc (i);

  Close (Out);

  SmartChangeFArea (OldGroup, OldArea, LastGroup, LastArea);
  CloseFileGroups;
  CloseFileAreas;

  Dispose (InFileBuf);
  Dispose (OutFileBuf);
  Dispose (FilesColl, Done);

  FindFirst (DescsFile, FindMask, DirInfo);

  If DosError = 0 Then
    If DirInfo. Size > 0 Then
    Begin
      FileRec. GroupNum := 0;
      FileRec. AreaNum := 0;
      FileRec. Size := DirInfo. Size;
      FileRec. PathName := NewStr (DescsFile);
      FileRec. FromName := Nil;
      FileRec. Free := True;
      InsertInTagList (FileRec);
    End;

{$IFNDEF MSDOS}
  FindClose (DirInfo);
{$ENDIF}
End;

End.
