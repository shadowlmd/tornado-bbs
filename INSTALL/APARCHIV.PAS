{$S-,R-,V-,I-,B-,F+,O-,A-}
{&Use32+}

{Conditional defines that may affect this unit}
{$I APDEFINE.INC}

{*********************************************************}
{*                  APARCHIV.PAS 2.02                    *}
{*        Copyright (c) TurboPower Software 1991.        *}
{*                 All rights reserved.                  *}
{*********************************************************}

unit ApArchiv;
  {-Low-level routines for working with archived files}

interface

uses
  (*
  {$IFDEF VIRTUALPASCAL}
  Use32,
  {$ENDIF}
  *)
  Dos,
  {$IFDEF UseOpro}
  OpRoot,
  OpString,
  {$ENDIF}
  {$IFDEF UseTPro}
  TpMemChk,
  TpString,
  {$ENDIF}
  ApMisc;

{$I APARCHIV.PA0}

  {==========================================================================}

implementation

  procedure InitFileMaskList(var FML : FileMaskList);
    {-Initialize a FileMaskList}
  begin
    FML.Head := nil;
    FML.Tail := nil;
  end;

  function AppendFileMaskPrim(FM : PathStr; var FML : FileMaskList;
                              Expand : Boolean) : Boolean;
    {-Add FM to a list of FileMasks}
  var
    FMNP : FileMaskNodePtr;
    FMN  : FileMaskNode;
    I, Asterisk : Byte;
    Dir : DirStr;
  begin
    for I := 1 to Length(FM) do
      if FM[I] = '/' then
        FM[I] := '\';

    {Convert to uppercase}
    FM := StUpcase(FM);
    FSplit(FM, Dir, FMN.Name, FMN.Ext);
    FMN.DirPtr := StringToHeap(Dir);
    if FMN.DirPtr = nil then begin
      ArchiveStatus := ecOutOfMemory;
      Exit;
    end;

    if Expand then begin
      {Expand to ???????.??? format}
      if FMN.Name = '' then
        FMN.Name := '????????';
      if FMN.Ext = '' then
        FMN.Ext := '???'
      else if FMN.Ext = '.' then
        FMN.Ext := ''
      else
        Delete(FMN.Ext, 1, 1);

      Asterisk := Pos('*', FMN.Name);
      if Asterisk <> 0 then begin
        for I := Asterisk to 8 do
          FMN.Name[I] := '?';
        FMN.Name[0] := #8;
      end;

      Asterisk := Pos('*', FMN.Ext);
      if Asterisk <> 0 then begin
        for I := Asterisk to 3 do
          FMN.Ext[I] := '?';
        FMN.Ext[0] := #3;
      end;
    end else if FMN.Ext <> '' then
      Delete(FMN.Ext, 1, 1);

    FMN.Next := nil;

    if not GetMemCheck(FMNP, SizeOf(FMNP^)) then
      AppendFileMaskPrim := False
    else
      with FML do begin
        FMNP^ := FMN;
        if Head = nil then begin
          Head := FMNP;
          Tail := FMNP;
        end
        else begin
          Tail^.Next := FMNP;
          Tail := FMNP;
        end;
        AppendFileMaskPrim := True;
      end;
  end;

  function AppendFileMask(FM : PathStr; var FML : FileMaskList) : Boolean;
    {-Add FM mask to a list of FileMasks}
  begin
    AppendFileMask := AppendFileMaskPrim(FM, FML, True);
  end;

  function AppendFileName(FM : PathStr; var FML : FileMaskList) : Boolean;
    {-Add FM name to a list of FileMasks}
  begin
    AppendFileName := AppendFileMaskPrim(FM, FML, False);
  end;

  function MatchFileMaskPrim(var FName : PathStr; var FMN : FileMaskNode) : Boolean;
    {-Return True if FName matches specified file mask}
  var
    Dir : DirStr;
    Name : NameStr;
    Ext : ExtStr;
    I : Byte;
  begin
    {Assume failure}
    MatchFileMaskPrim := False;

    {Split FName into Name, Ext}
    FSplit(FName, Dir, Name, Ext);
    if Ext = '.' then
      Ext := ''
    else if Ext <> '' then
      Delete(Ext, 1, 1);

    {If Name or Ext is larger than the mask, we know it doesn't match}
    if (Length(Name) > Length(FMN.Name)) or
       (Length(Ext) > Length(FMN.Ext)) then
      Exit;

    {Compare directories if mask has a directory}
    if (FMN.DirPtr^ <> '') and (FMN.DirPtr^ <> Dir) then
      Exit;

    {Extend Name and Ext with characters guaranteed not to match}
    Name := PadCh(Name, #0, 8);
    Name[0] := #8;
    Ext := PadCh(Ext, #0, 3);
    Ext[0] := #3;

    {Compare Name to FMN.Name}
    for I := 1 to Length(FMN.Name) do
      if (FMN.Name[I] <> '?') then
        if (Name[I] <> FMN.Name[I]) then
          Exit;

    {Compare Ext to FMN.Ext}
    for I := 1 to Length(FMN.Ext) do
      if (FMN.Ext[I] <> '?') then
        if (Ext[I] <> FMN.Ext[I]) then
          Exit;

    {If we get here, we matched}
    MatchFileMaskPrim := True;
  end;

  function MatchFileMask(FM : PathStr; var FML : FileMaskList) : Boolean;
    {-Return True if Name is in list of file masks}
  var
    FMNP : FileMaskNodePtr;
    I : Byte;
  begin
    MatchFileMask := True;
    if FML.Head = nil then
      Exit;

    for I := 1 to Length(FM) do
      if FM[I] = '/' then
        FM[I] := '\';

    FM := StUpcase(FM);
    with FML do begin
      FMNP := Head;
      while (FMNP <> nil) do begin
        if MatchFileMaskPrim(FM, FMNP^) then
          Exit;
        FMNP := FMNP^.Next;
      end;
    end;

    MatchFileMask := False;
  end;

  procedure DoneFileMaskList(var FML : FileMaskList);
    {-Dispose of a FileMaskList}
  var
    FMNP, NP : FileMaskNodePtr;
  begin
    with FML do begin
      FMNP := Head;
      while (FMNP <> nil) do begin
        NP := FMNP^.Next;
        DisposeString(FMNP^.DirPtr);
        FreeMemCheck(FMNP, SizeOf(FMNP^));
        FMNP := NP;
      end;
      Head := nil;
      Tail := nil;
    end;
  end;

  procedure SortFileMaskList(var FML : FileMaskList);
    {-Sorts FML in ascending order}
  var
    CurHead : FileMaskNodePtr;
    Node : FileMaskNodePtr;
    Lowest : FileMaskNodePtr;
    LowDir : DirStr;
    LowName : NameStr;
    LowExt : ExtStr;
    Temp : FileMaskNode;
  begin
    CurHead := FML.Head;

    {Loop until all items in place}
    repeat
      Lowest := nil;
      LowDir := #255;
      LowName := #255;
      LowExt := #255;

      {Find the (next) lowest file name}
      Node := CurHead;
      repeat
        if (Node^.DirPtr^ < LowDir) or
           ((Node^.DirPtr^ = LowDir) and (Node^.Name < LowName)) or
           ((Node^.DirPtr^ = LowDir) and (Node^.Name = LowName) and
           (Node^.Ext < LowExt)) then begin
          LowDir := Node^.DirPtr^;
          LowName := Node^.Name;
          LowExt := Node^.Ext;
          Lowest := Node;
        end;
        Node := Node^.Next;
      until Node = nil;

      {Swap the data in the lowest node with the data in CurHead}
      Move(CurHead^, Temp, SizeOf(FileMaskNode));
      CurHead^.DirPtr := Lowest^.DirPtr;
      CurHead^.Name := Lowest^.Name;
      CurHead^.Ext := Lowest^.Ext;
      Lowest^.DirPtr := Temp.DirPtr;
      Lowest^.Name := Temp.Name;
      Lowest^.Ext := Temp.Ext;

      {Move up CurHead}
      CurHead := CurHead^.Next;
    until CurHead = nil;
  end;

  {!!.02 revised}
  procedure ExpandFileMaskList(var FML, ExpandedFML : FileMaskList);
    {-Expand all masks FML into complete file names in ExpandedFML}
  const
    AnyFileButDir = AnyFile and
      not (Directory or VolumeID or Hidden or SysFile);
  var
    Node     : FileMaskNodePtr;
    FName    : PathStr;
    SRec     : SearchRec;
    Dirr     : PathStr;

  begin
    ArchiveStatus := ecOk;
    Node := FML.Head;
    while Node <> nil do begin
      with Node^ do begin
        {Setup the file name mask to search for}
        FName := Name + '.' + Ext;
        if DirPtr^ <> '' then
          FName := AddBackSlash(DirPtr^) + FName;
        Dirr := AddBackSlash (DirPtr^);

        {Find the first matching file}
        FindFirst(FName, AnyFileButDir, SRec);
        if DosError <> 0 then begin
          ArchiveStatus := ecFileNotFound;
          Exit;
        end;

        {Append file to new list for each match}
        while DosError = 0 do begin
          if not AppendFileName (Dirr+Srec.Name, ExpandedFML) then
          begin
            ArchiveStatus := ecOutOfMemory;
            DoneFileMaskList(ExpandedFML);
            Exit;
          end;
          FindNext(SRec);
        end;

        {Finished with this mask, continue with next}
        Node := Node^.Next;
      end;
    end;
  end;

  procedure arOptionsOn(OptionFlags : Word);
    {-Activate multiple options}
  begin
    DefArchiveOptions :=
      DefArchiveOptions or (OptionFlags and not BadArchiveOptions);
  end;

  procedure arOptionsOff(OptionFlags : Word);
    {-Deactivate multiple options}
  begin
    DefArchiveOptions :=
      DefArchiveOptions and not (OptionFlags and not BadArchiveOptions);
  end;

  function arOptionsAreOn(OptionFlags : Word) : Boolean;
    {-Return true if all specified options are on}
  begin
    arOptionsAreOn := (DefArchiveOptions and OptionFlags = OptionFlags);
  end;

  procedure SetOutputPath(Path : PathStr);
    {-Set path for output files}
  begin
    arOutPath := AddBackSlash(Path);
  end;

  procedure CreateOutputFile(var F : File; FName : PathStr);
    {-Create FName. If FName specifies a directory that doesn't exist, and
      arCreateDirs is on, it will try to create it.}
  var
    I : Word;
    P, N : PathStr;
    PLen : Byte absolute P;
    Done : Boolean;
  begin
    Done := False;
    P := '';
    N := FName;
    repeat
      {try to create output file}
      Assign(F, FName);
      Rewrite(F, 1);
      ArchiveStatus := IoResult;
      if (ArchiveStatus <> 3) or not arOptionsAreOn(arCreateDirs) then
        Done := True
      else repeat
        I := Pos('\', N);
        if I = 0 then
          Done := True
        else begin
          P := AddBackslash(P)+Copy(N, 1, I-1);
          Delete(N, 1, I);
          if (PLen = 0) or (P[PLen] = ':') then
            P := P+'\'
          else begin
            I := 0;
            MkDir(P);
            ArchiveStatus := IoResult;
            Done := (ArchiveStatus <> 0) and (ArchiveStatus <> 5);
          end;
        end;
      until (I = 0);
    until Done;
  end;

  {!!.01}
  function Percentage(V1, V2 : LongInt) : Byte;
    {-Returns the ratio of V1 to V2 * 100}
  var
    Ratio : Byte;
  begin
    if V2 > 16384000 then begin  {Possible LongInt overflow}
      V1 := (V1 + $80) shr 8;  {scale down (div 256)}
      V2 := (V2 + $80) shr 8;  {scale down (div 256)}
    end;
    if V2 <= 0 then
      Ratio := 0
    else
      Ratio := (V1 * 100) div V2;
    if Ratio > 100 then
      Ratio := 100;
    Percentage := Ratio;
  end;

end.
