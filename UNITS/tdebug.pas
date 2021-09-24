{$X+,I-,O+,F+}

Unit
  tDebug;

Interface

Procedure DebugInfo;

Implementation

Uses
  tMisc,
  OpCrt,
  SysMsgs,
  MainComm,
  Objects,
{$IFDEF MSDOS}
{$IFNDEF DPMI}
{$IFNDEF DPMI32}
  Streams,
  Overlay,
{$ENDIF}
{$ENDIF}
{$ENDIF}
  tWin,
  TGlob;

{$IFDEF MSDOS}
  {$IFNDEF DPMI}
  {$IFNDEF DPMI32}
    {$DEFINE RealMode}
  {$ENDIF}
  {$ENDIF}
{$ENDIF}

{$IFDEF RealMode}
Type
  tHandleRec = Record
                 numRefer: Word; {00h    WORD    number of file handles referring to this file}
                                 {FFFFh if in use but not referenced}
                 openMode: Word; {02h    WORD    file open mode (see AX=6C00h,#0627 at AH=3Dh)}
                                 {bit 15 set if this file opened via FCB}
                 fileAttr: Byte; {04h    BYTE    file attribute (see #0643 at AX=4301h)}
                 deviceInfo: Word; {05h    WORD    device info word (see also #0646 at AX=4400h)
                                    bit 15 set if remote file
                                    bit 14 set means do not set file date/time on closing
                                    bit 13 set if named pipe
                                    bit 12 set if no inherit
                                    bit 11 set if network spooler
                                    bit 7  set if device, clear if file (only if local)
                                    bits 6-0 as for AX=4400h}
                 pDeviceDrv: pointer; {07h    DWORD   pointer to device driver header if character device}
                                      {else pointer to DOS Drive Parameter Block}
                                      {(see #0620 at AH=32h) or REDIR data}
                 startCluster: Word; {0Bh    WORD    starting cluster of file (local files only)}
                 packedTime: Word; {0Dh    WORD    file time in packed format (see #0876)}
                 packedDate: Word; {0Fh    WORD    file date in packed format (see #0877)}
                 fileSiz: LongInt; {11h    DWORD   file size}
                 curOfs: LongInt;  {15h    DWORD   current offset in file (SFT)}
                                   {LRU counters (FCB table, two WORDs)}
                 {---local file---}
                 relativCluster: Word;  {19h    WORD    relative cluster within file of last cluster accessed}
                 sectors4dire: LongInt; {1Bh    DWORD   number of sector containing directory entry}
                 direPerSector: Byte;   {1Fh    BYTE    number of dir entry within sector (byte offset/32)}
                 {------}
                 {20h 11 BYTEs   filename in FCB format (no path/period, blank-padded)}
                 fileName: Array [1..11] Of Char;
                 prevSFT: pointer; {2Bh    DWORD   (SHARE.EXE) pointer to previous SFT sharing same file}
                 netNum: Word; {2Fh    WORD    (SHARE.EXE) network machine number which opened file}
                                    {(Windows Enhanced mode DOSMGR uses the virtual machine}
                                    {ID as the machine number; see INT 2F/AX=1683h)}
                 ownerPSPseg: Word; {31h    WORD    PSP segment of file's owner (see #0603 at AH=26h)}
                                    {(first three entries for AUX/CON/PRN contain segment}
                                    {of IO.SYS startup code)}
                 shareRecOfs: Word; {33h    WORD    offset within SHARE.EXE code segment of}
                                    {sharing record (see #0808)  0000h = none}
                 absCluster: Word; {35h    WORD    (local) absolute cluster number of last clustr accessed}
                 dummy: Array [1..4] Of Byte;
               End;

  pHandleTbl = ^tHandleTbl;
  tHandleTbl = Record
                 nextHandleTbl: pointer; {00h   DWORD   pointer to next file table (offset FFFFh if last) end;}
                 numFiles: Word;         {04h   WORD    number of files in this table}
                                         {06h  3Bh bytes per file (for 4.0-6.0 DOS) }
                 Handles: Array [1..255] Of tHandleRec;
               End;

  SO = Record
         O, S: Word;
       End;

Procedure ShowOpenFiles;
Var
  P                     : Pointer;
  HandleTbl             : pHandleTbl Absolute P;
  pSO                   : SO Absolute P;
  I, Z, Y               : Byte;
  _Done, NotSupp        : Boolean;
  S                     : String;
  FilesBox              : pBoxRec;
  OpenFiles             : PNotSortedCollection;
  C                     : Char;

Begin
  InitWindow (FilesBox, 5, 5, 75, 19, 4, Cnf. ColorScheme
              [cdFrame], ' We have files: ', Cnf. ColorScheme [cdTitle], 0,
              True);

  DrawWindow (FilesBox);

  Y := 7;
  NotSupp := False;
  OpenFiles := New (PNotSortedCollection, Init (1, 1) );

  Asm
    mov AH, 52h
    Int 21h
    mov AX, 0FFFFh
    cmp AX, ES: [BX + 4]
    je  @NoHandleTbl
    mov AX, 0FFFFh
    cmp AX, ES: [BX + 6]
    je  @NoHandleTbl
    mov AX, ES: [BX + 4]
    mov pSO. O, AX
    mov AX, ES: [BX + 6]
    mov pSO. S, AX
    mov _Done, 0
    jmp @Xit
    @NoHandleTbl:
    mov _Done, 1
    @Xit:
    nop
  End;

  If Not _Done Then
  Begin
    Repeat
      With HandleTbl^ Do
      Begin
        For i := 1 To NumFiles Do
        With Handles [i] Do
        Begin
          S := '';
          For Z := 1 To 11 Do S := S + FileName [Z];
          If OwnerPSPseg = PrefixSeg Then
          Begin
            S := LoString (Trim (Copy (s, 1, 8)) + '.' + Trim (Copy (s, Length (s)-2, 3)));
            If numRefer > 0 Then
            OpenFiles^. Insert (NewStr ('[' + PadCh (s, ' ', 12) + ']: {' +
            HexW (OpenMode) + '} /' + Long2Str (numRefer) ) );
            Inc (Y);
          End;
        End;

        If SO (nextHandleTbl). O = $FFFF
        Then _Done := True
        Else Move (NextHandleTbl, P, SizeOf (Pointer) );
      End;
    Until _Done;
  End;

  If OpenFiles^. Count = 0 Then
  Begin
    FastWrite ('This feature not supported by your OS', 9, 21, Cnf. ColorScheme [cdText] );

    WaitForKey (C);
    If C = #0 Then WaitForKey (C);

    NotSupp := True;
  End;

  If Not NotSupp Then
    ScrollTextWindow (FilesBox^. X1+1, FilesBox^. Y1+1, FilesBox^. X2,
    FilesBox^. Y2, Cnf. ColorScheme [cdInput] And $0F,
    Cnf. ColorScheme [cdInput] Shr 4, Cnf. ColorScheme [cdScroller] And $0F,
    Cnf. ColorScheme [cdScroller] Shr 4, Cnf. ColorScheme [cdScroller] Shr 4,
    Cnf. ColorScheme [cdScroller] And $0F, OpenFiles);

  CloseWindow (FilesBox);
  Dispose (OpenFiles, Done);
End;

{$ENDIF}

Procedure DebugInfo;
Var
  DebugBox      : pBoxRec;
  C             : Char;

Begin
  HiddenCursor;
  InitWindow (DebugBox, 5, 5, 75, 19, 4, Cnf. ColorScheme
             [cdFrame], ' Debug Information ', Cnf. ColorScheme [cdTitle],
             ZoomSpeed, True);

  DrawWindow (DebugBox);

  FastWrite ('Free Stack space: ', DebugBox^. Y1+2, DebugBox^. X1+2, Cnf. ColorScheme [cdText]);
  FastWrite (Long2Str (SPtr), DebugBox^. Y1+2, DebugBox^. X1+24, Cnf. ColorScheme [cdInput]);
{$IFDEF RealMode}
  FastWrite ('Overlay disk reads: ', DebugBox^. Y1+3, DebugBox^. X1+2, Cnf. ColorScheme [cdText]);
  FastWrite (Long2Str (OvrDiskReads), DebugBox^. Y1+3, DebugBox^. X1+24, Cnf. ColorScheme [cdInput]);
  FastWrite ('Overlay stream reads: ', DebugBox^. Y1+4, DebugBox^. X1+2, Cnf. ColorScheme [cdText]);
  FastWrite (Long2Str (OvrMemReads), DebugBox^. Y1+4, DebugBox^. X1+24, Cnf. ColorScheme [cdInput]);
{$ENDIF}

  FastWrite ('Ã' + Replicate ('Ä', 69) + '´', DebugBox^. Y1+10, DebugBox^. X1, Cnf. ColorScheme [cdFrame]);
{$IFDEF RealMode}
  FastWrite ('  <F> show list of opened files', DebugBox^. Y1+11, DebugBox^. X1+2, Cnf. ColorScheme [cdInput]);
{$ENDIF}
  FastWrite ('<Esc> close debug window', DebugBox^. Y1+12, DebugBox^. X1+2, Cnf. ColorScheme [cdInput]);

  Repeat
    If Not WaitForKey (C) Then Break;
    Case C Of
      #0       :
                 Begin
                   WaitForKey (C);
                   Continue;
                 End;

      #27      : Break;
{$IFDEF RealMode}
      'F', 'f' : ShowOpenFiles;
{$ENDIF}
      Else
        Continue;
    End;
  Until C = #27;
  If KeyPressed Then ReadKey;

  CloseWindow (DebugBox);
  NormalCursor;
End;

End.
