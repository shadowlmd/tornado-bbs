{$IFDEF MSDOS}
  {$IFNDEF DPMI32}
    {$F+}
  {$IFNDEF DPMI}
    {$O+}
  {$ENDIF}
  {$ENDIF}
{$ENDIF}

{$I-}

Unit aDir;

{*********************************************************}
{*                      ADIR.PAS                         *}
{*                                                       *}
{*  Copyright (c) Anton the Deinow, 1995,                *}
{*  Copyright (c) Konstantin Klyagin, 1995-98,           *}
{*                exspecially for Tornado BBS System     *}
{*                                                       *}
{*********************************************************}

Interface

Uses
  DOS,
{$IFDEF OS2}
  VPutils,
  VPSysLow,
{$ENDIF}
  Objects,
  tMisc,
  OpCrt,
  OpInLine,
  tWin,
  TGlob;

Type
  tItemView = (ivAllInfo, ivOnlyName);

  tPaneMode = (
    pmMultiFull,       {¯®«­®¥ ®ª­®, ¢­¨§ã ¨­ä-ï ® â¥ª.¨ ®â¬¥ç-ëå ä ©« å}
    pmMultiHalf,       {®ª­® ­  ¯®«-íªp ­ , ®áâ «ì­®¥, ª ª ¢ ¯p¥¤ë¤ãé¥¬}
    pmSingleFull,      {¯®«­®¥, ¡¥§ ®â¬¥âª¨ ä ©«®¢}
    pmSingleHalf       {¯®«®¢¨­ , ¡¥§ ®â¬¥âª¨ ä ©«®¢}
    );

  pnState = (
    pnOk,
    pnNotEnoughMem,
    pnNoFiles,
    pnInvalidPath,
    pnDriveNotReady,
    pnQuit
    );

Function Pane_Process (xL, yL: Byte; pm_: tPaneMode; iv_: tItemView;
         Var pth: PathStr; DM: String; Blinking: Boolean;
         Var MarkedFilesColl: PNotSortedCollection): pnState;

Implementation

Function Pane_Process; {-®á­®¢­ ï ¯p®æ¥¤ãp  p ¡®âë á ¯ ­¥«ìî}

Type
  tPaneColor = (
    pcWindow,
    pcFrame,
    pcHeader,
    pcItem,
    pcMarkedItem,
    pcCursor,
    pcCursorMarked,
    pcTitle,
    pcDivider,
    pcfInfo,
    pcmInfo
    );

  PaneColorArray = Array [tPaneColor] Of Byte;

  tPaneCmd = (
    ckLeftArrow,          {Left}
    ckRightArrow,         {Right}
    ckMarkItem,           {Ins}
    ckEnter,              {Enter}
    ckUpArrow,            {Up}
    ckDownArrow,          {Down}
    ckQuit,               {Esc}
    ckNone,               {Ctrl/Enter}
    ckHome,               {Home}
    ckEnd,                {End}
    ckPgUp,               {PgUp}
    ckPgDn,               {PgDn}
    ckGoTo,               {Any letter key}
    ckBackSpace,          {BackSpace}
    ckSelectAll,          {Gray +}
    ckDeSelectAll,        {Gray -}
    ckInverse,            {Gray *}
    ckChangeDrive         {Shift-A-B-C..Z}
    );

  tPaneMetrix = Record
    Y2, TopY, BottomY, CurItemInfoY, MarkedInfoY : Byte;
  End;

  FNameStr = String [{$IFNDEF MSDOS} 90 {$ELSE} 12 {$ENDIF}];

  DirRec = Record
    oFlag, dAttr : Byte;
    dTime, dSize : LongInt;
    dName        : FNameStr;
  End;

Const
  MaxFiles = {$IFNDEF MSDOS} 500 {$ELSE} 1500 {$ENDIF};

Type
  DirRecArr = Array [1..MaxFiles] Of DirRec;
  PDirRecArr = ^DirRecArr;

  tPaneRec = Record
    ItemView                   : tItemView;
    PaneWin                    : PBoxRec;
    X1, Y1, X2                 : Byte;
    PM                         : tPaneMetrix;
    AtTop, CurItem, TotalItems : Word;
    pnPath                     : PathStr;
    Colors                     : PaneColorArray;
    MarkedSize                 : LongInt;
    MarkedCount, NextMarked    : Word;
      { -á®¤¥p¦¨â ­®¬¥p ¯®á«¥¤­¥£® ¯®¬¥ç¥­­®£® ä ©« ,
        ª®â®pë© ¢¥p­ã«  ä-ï NextMarkedFile() }
    FoundMarked                : Word;
      { -áª®«ìª® ã¦¥ ¢®§¢p é¥­® ¯®¬¥ç¥­ëå ä ©«®¢ }
    DirArray                   : PDirRecArr;
  End;

Const
  ofMarked = $01;  {ä ©« ¯®¬¥ç¥­}

  PaneMetrix: Array [tPaneMode] Of tPaneMetrix = (
    (Y2: 22; TopY: 2; BottomY: 18; CurItemInfoY: 20; MarkedInfoY: 21), {MultiFull}
    (Y2: 12; TopY: 2; BottomY: 8; CurItemInfoY: 10; MarkedInfoY: 11),  {MultiHalf}
    (Y2: 22; TopY: 2; BottomY: 19; CurItemInfoY: 21; MarkedInfoY: 0),  {SingleFull}
    (Y2: 12; TopY: 2; BottomY: 9; CurItemInfoY: 11; MarkedInfoY: 0)    {SingleHalf}
    );

  DefPaneColors1: PaneColorArray = (
    $87,  {pcWindow}
    $87,  {pcFrame}
    $30,  {pcHeader}
    $87,  {pcItem}
    $8E,  {pcMarkedItem}
    $30,  {pcCursor}
    $3E,  {pcCursorMarked}
    $8E,  {pcTitle}
    $87,  {pcDivider}
    $8F,  {pcfInfo}
    $8E   {pcmInfo}
    );

  DefPaneColors2: PaneColorArray = (
    $1b,  {pcWindow}
    $1b,  {pcFrame}
    $30,  {pcHeader}
    $1b,  {pcItem}
    $1E,  {pcMarkedItem}
    $30,  {pcCursor}
    $3E,  {pcCursorMarked}
    $1E,  {pcTitle}
    $1b,  {pcDivider}
    $1F,  {pcfInfo}
    $1E   {pcmInfo}
    );

  HiddenChar: Array [Boolean] Of Char = (' ', '°');

  sUpDir  : String [9] = #16'UPÄÄDIR'#17;
  sSubDir : String [9] = #16'SUBÄDIR'#17;

  OnlyNameDiv : Array [0..2] Of String [13] = (
    '            ³',
    '            ³',
    '            '
    );

Var
  Pane         : ^tPaneRec;
  pnStatus     : pnState;
  TmpDir       : PathStr;
  SaveDir      : PathStr;
  Opened, hpfs : Boolean;
  X, Y         : Byte;
  Drive2Change : Char;

  Function IntrnlToFname (dName: FNameStr): String;
  Const
    HidPos : Array [Boolean] Of Byte = (9, 12);

  Begin
    If dName [HidPos [hpfs]] = HiddenChar [True] Then
      dName [HidPos [hpfs]] := ' ';

    If hpfs Then
      IntrnlToFname := Copy (dName, 1, 12)
    Else
      If WordCount (dName, SpaceOnly) <> 1 Then
        IntrnlToFname := ExtractWord (1, dName, SpaceOnly) + '.' +
          ExtractWord (2, dName, SpaceOnly)
      Else
        IntrnlToFname := TrimTrail (dName);
  End;

  Function IntrnlFname (Name: String; Attr: Byte): String;
  Var
    P : Byte;

  Begin
    If Attr And Directory = 0 Then
      Name := LoString (Name)
    Else
      If Name = '..' Then
      Begin
        IntrnlFname := Pad (Name, 12);
        Exit;
      End;

    If hpfs Then
    Begin
      If Length (Name) > 12 Then
        IntrnlFname := Copy (Name, 1, 10) + '..'
      Else
        IntrnlFname := Pad (Name, 12);
    End Else
    Begin
      P := Pos ('.', Name);
      If P <> 0 Then
        IntrnlFname := Pad (Copy (Name, 1, P - 1), 8) +
          HiddenChar [(Attr And Hidden) <> 0] +
          Pad (Copy (Name, P + 1, 3), 3)
      Else
        IntrnlFname := Pad (Name, 8) +
          Pad (HiddenChar [(Attr And Hidden) <> 0], 4);
    End;
  End;

  Function FindFiles: Boolean;
  Var
    TmpElem : DirRec;

    Function Less (Var X, Y: DirRec): Boolean;
    Var
      Result     : Integer;
      Xdir, Ydir : Boolean;

    Begin
      Xdir := X. dAttr And Directory <> 0;
      Ydir := Y. dAttr And Directory <> 0;

      If Xdir <> Ydir Then
        Less := Xdir
      Else
      Begin
        Result := StrCompare (UpString (JustExtension (X. dName)),
                              UpString (JustExtension (Y. dName)));
        If Result = 0 Then
          Result := StrCompare (UpString (JustName (X. dName)),
                                UpString (JustName (Y. dName)));
        Less := Result < 0;
      End;
    End;

    {$IFNDEF OS2}
    {$S+}
    {$ENDIF}

    Procedure Sort (L, R: Word);
    {-á®pâ¨p®¢ª  á¨¬¢®«®¢ ã¢¥«¨ç¥­¨¥¬  ¤p¥á  ¢ ¯p®¬¥¦ãâª¥ [L..R] }
    Var
      I, J : Word;

    Begin
      I := L;
      J := R;
      Move (Pane^. DirArray^ [(L + R) ShR 1], TmpElem, SizeOf (DirRec));

      Repeat
        While Less (Pane^. DirArray^ [I], TmpElem) Do
          Inc (I);

        While Less (TmpElem, Pane^. DirArray^ [J]) Do
          Dec (J);

        If I <= J Then
        Begin
          { ¬¥­ï¥¬ ¬¥áâ ¬¨ í«¥¬¥­âë No I á No J }
          ExchangeStructs (Pane^. DirArray^ [I], Pane^. DirArray^ [J],
            SizeOf (DirRec));
          Inc (I);
          Dec (J);
        End;
      Until I > J;

      If L < J Then
        Sort (L, J);

      If I < R Then
        Sort (I, R);
    End;

    {$IFNDEF OS2}
    {$S-}
    {$ENDIF}

  Var
    sr: SearchRec;

  Begin
    FindFiles := False;

    With Pane^ Do
    Begin
      FindFirst (AddBackSlash (pnPath) + AllFilesMask, AnyFile
        {$IFNDEF OS2}-VolumeID {$ENDIF}, sr);

      If DosError <> 0 Then
      Begin
        Case DosError Of
            3 : pnStatus := pnInvalidPath;
           18 : pnStatus := pnNoFiles;
          152 : pnStatus := pnDriveNotReady;
        End;

      {$IFNDEF MSDOS}
        FindClose (sr);
      {$ENDIF}
        Exit;
      End;

      While (DosError = 0) And (Pane^. TotalItems < MaxFiles) Do
      Begin
        If (((sr. Attr And VolumeID) = 0) And (sr. Name <> '.')) And
           Not ((sr. Name = '..') And (Length (AddBackSlash (pnPath)) <= 3)) Then
        Begin
          Inc (TotalItems);

          With Pane^. DirArray^ [TotalItems] Do
          Begin
            oFlag := 0;
            dAttr := SR. Attr;
            dTime := SR. Time;
            dSize := SR. Size;
            dName := SR. Name;
          End;
        End;

        FindNext (sr);
      End;

    {$IFNDEF MSDOS}
      FindClose (sr);
    {$ENDIF}

      If TotalItems <> 0 Then
      Begin
        CurItem := 1;
        AtTop := 1;

        If TotalItems > 1 Then
          If Pos ('..', Pane^. DirArray^ [1]. dName) = 0 Then
            Sort (1, TotalItems)
          Else
            If TotalItems > 2 Then
              Sort (2, TotalItems);
      End;
    End;

    FindFiles := True;
  End;

  Procedure Pane_Done; {-§ ¢¥pè ¥â ¯ ­¥«ì}
  Begin
    If Pane <> Nil Then
    Begin
      CloseWindow (Pane^. PaneWin);
      Opened := False;

      If Pane^. DirArray <> Nil Then
        FreeMem (Pane^. DirArray, SizeOf (DirRec) * MaxFiles);
      FreeMem (Pane, SizeOf (tPaneRec));
    End;
  End;

  Procedure ShowColumnTitle;
  Begin
    With Pane^ Do
      Case ItemView Of
        ivAllInfo : Begin
                      FastWrite ('    Name        Size     Date    Time',
                        Y1 + 1, X1 + 1, Colors [pcTitle]);
                      FastWrite ('³', Y1 + 1, 33, Colors [pcWindow]);
                      FastWrite ('³', Y1 + 1, 43, Colors [pcWindow]);
                      FastWrite ('³', Y1 + 1, 52, Colors [pcWindow]);
                    End;
       ivOnlyName : Begin
                      FastWrite ('    Name         Name         Name',
                        Y1 + 1, X1 + 1, Colors [pcTitle]);
                      FastWrite ('³', Y1 + 1, 33, Colors [pcWindow]);
                      FastWrite ('³', Y1 + 1, 46, Colors [pcWindow]);
                    End;
      End;
  End;

  Procedure DrawDivider;
  Begin
    With Pane^ Do
      Case ItemView Of
        ivAllInfo  : FastWrite ('ÃÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄ´',
                       PM. BottomY + 1, X1, Colors [pcDivider]);
        ivOnlyName : FastWrite ('ÃÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄ´',
                       PM. BottomY + 1, X1, Colors [pcDivider]);
      End;
  End;

  Procedure ShowCurInfo;
  Var
    DT : DateTime;
    St : String;

  Begin
    With Pane^. DirArray^ [Pane^. CurItem] Do
    Begin
      St := Pad (IntrnlToFname (dName), 13);

      If dAttr And Directory <> 0 Then
        If Pos ('..', St) <> 0 Then
          St := St + sUpDir
        Else
          St := St + sSubDir
      Else
        St := St + LeftPad (Long2Str (dSize), 9);

      UnpackTime (dTime, DT);
      St := St + ' ' + FormattedDate (DT, DM + ' HH:II');
    End;

    With Pane^ Do
      FastWrite (St, PM. CurItemInfoY, X1 + 1, Colors [pcfInfo]);
  End;

  Procedure ShowMarkedInfo;
  Begin
    With Pane^ Do
    Begin
      If PM. MarkedInfoY = 0 Then
        Exit;

      If MarkedCount = 0 Then
        FastWrite (CenterCh ('No files selected', ' ', X2 - X1 - 1),
          PM. MarkedInfoY, X1 + 1, Colors [pcmInfo])
      Else
        FastWrite (CenterCh (Long2Str (MarkedSize) + ' bytes in ' +
          Long2Str (MarkedCount) + ' marked files', ' ', X2 - X1 - 1),
          PM. MarkedInfoY, X1 + 1, Colors [pcmInfo]);
    End;
  End;

  Function Pane_Open: Boolean; {-®âªpë¢ ¥â/¯¥p¥®âªpë¢ ¥â ¯ ­¥«ì}

    Procedure MarkerToDir;
    Var
      w : Word;

    Begin
      With Pane^ Do
        If TmpDir <> '' Then
          For w := 1 To TotalItems Do
            If Trim (PlaceSubStr (Pane^. DirArray^ [w]. dName, '°', '')) =
               Trim (TmpDir) Then
            Begin
              CurItem := w;
              If w > Succ (PM. BottomY - PM. TopY) Then
                AtTop := w - (PM. BottomY - PM. TopY);

              Exit;
            End;
    End;

  Var
    S : String;

  Begin
    Pane_Open := False;
    hpfs := {$IFDEF OS2} GetDriveType (Pane^. pnPath [1]) = dtHDHPFS {$ELSE}
                         False {$ENDIF};
    With Pane^ Do
    Begin
      If Not Opened Then
      Begin
        Opened := True;
        InitWindow (PaneWin, X1, Y1, X2, PM. Y2, 4, Colors [pcWindow], '', $00,
          ZoomSpeed, False);
        DrawWindow (PaneWin);
        ShowColumnTitle;
        DrawDivider;
      End;

      Case ItemView Of
        ivAllInfo  : FastWrite ('ÕÍÍÍÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍ¸', Y1,
                       X1, Colors [pcWindow]);
        ivOnlyName : FastWrite ('ÕÍÍÍÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍ¸', Y1,
                       X1, Colors [pcWindow]);
      End;

      If Length (pnPath) > 34 Then
        S := Copy (pnPath, 1, 3) + '..' + Copy (pnPath, Length (pnPath) - 28,
          29)
      Else
        S := pnPath;

      If Not hpfs Then
        S := UpString (S);

      S := ' ' + S + ' ';

      FastWrite (S, Y1, X1 + 1 + (((X2 - X1) - Length (S)) Shr 1),
        Colors [pcHeader]);

      MarkedSize := 0;
      MarkedCount := 0;
      TotalItems := 0;
      AtTop := 1;
      If Not FindFiles Then
        Exit;

      MarkerToDir;
    End;

    TmpDir := '';

    ShowCurInfo;
    ShowMarkedInfo;

    Pane_Open := True;
  End;

  Procedure CalcXY (w: Word; Var X, Y: Byte);
  Begin
    With Pane^ Do
    Begin
      X := Succ (X1) + 13 * ((w - AtTop) Div Succ (PM. BottomY - PM. TopY));
      Y := PM. TopY + ((w - AtTop) Mod Succ (PM. BottomY - PM. TopY));
    End;
  End;

  Function GetPaneCmd: tPaneCmd;
  Var
    i  : tPaneCmd;
    ck : Char;

  Begin
    i := ckNone;

    Repeat
      ck := ReadKey;

      Case ck Of
         #0 : Begin
                Case ReadKey Of
                  #75: i := ckLeftArrow;
                  #77: i := ckRightArrow;
                  #82: i := ckMarkItem;
                  #72: i := ckUpArrow;
                  #80: i := ckDownArrow;
                  #71: i := ckHome;
                  #79: i := ckEnd;
                  #73: i := ckPgUp;
                  #81: i := ckPgDn;
                End;

                HiddenCursor;
              End;

        #13 : i := ckEnter;
        #27 : i := ckQuit;
        #43 : i := ckSelectAll;
        #45 : i := ckDeSelectAll;
        #42 : i := ckInverse;
        #8  : i := ckBackSpace;

        'A'..'Z' : Begin
                     i := ckChangeDrive;
                     Drive2Change := ck;
                   End;

      End;
    Until i <> ckNone;

    GetPaneCmd := i;
  End;

  Procedure SetMarker;
  Var
    X, Y, c : Byte;

  Begin
    With Pane^ Do
      If TotalItems <> 0 Then
      Begin
        If DirArray^ [CurItem]. oFlag And ofMarked = 0 Then
          c := Colors [pcCursor]
        Else
          c := Colors [pcCursorMarked];

        Case ItemView Of
          ivAllInfo : Begin
                        ChangeAttribute (Pred (X2 - X1), PM. TopY + (CurItem -
                          AtTop), X1 + 1, c);
                        ShowCurInfo;
                      End;
         ivOnlyName : Begin
                        CalcXY (CurItem, X, Y);
                        ChangeAttribute (12, Y, X, c);
                        ShowCurInfo;
                      End;
        End;
      End;
  End;

  Procedure ClrMarker;
  Var
    X, Y, c : Byte;

  Begin
    With Pane^ Do
    Begin
      If DirArray^ [CurItem]. oFlag And ofMarked <> 0 Then
        c := Colors [pcMarkedItem]
      Else
        c := Colors [pcItem];

      Case ItemView Of
        ivAllInfo : ChangeAttribute (Pred (X2 - X1), PM. TopY + (CurItem -
                      AtTop), X1 + 1, c);
       ivOnlyName : Begin
                      CalcXY (CurItem, X, Y);
                      ChangeAttribute (12, Y, X, c);
                    End;
      End;
    End;
  End;

  Procedure ShowFile (w: Word);
  Var
    X, Y, c : Byte;
    DT      : DateTime;
    St      : String;

  Begin
    With Pane^ Do
    Begin
      c := Colors [pcItem];

      Case ItemView Of
        ivAllInfo: Begin
                     If w > TotalItems Then
                       St := '            ³         ³        ³      '
                     Else
                       With DirArray^ [w] Do
                       Begin
                         St := Pad (dName, 12) + '³';
                         If dAttr And Directory <> 0 Then
                           If dName [1] = '.' Then
                             St := St + sUpDir
                           Else
                             St := St + sSubDir
                         Else
                           St := St + LeftPad (Long2Str (dSize), 9);

                         UnpackTime (dTime, DT);

                         St := St + '³' + FormattedDate (DT, DM + '³HH:II ');

                         If oFlag And ofMarked <> 0 Then
                           c := Colors [pcMarkedItem];
                       End;

                       FastWrite (St, w - AtTop + PM. TopY, X1 + 1, c);
                     End;

        ivOnlyName: Begin
                      CalcXY (w, X, Y);
                      FastWrite (OnlyNameDiv [(w - AtTop) Div Succ (PM. BottomY
                        - PM. TopY)], Y, X, Colors [pcItem]);
                      If w <= TotalItems Then
                        With DirArray^ [w] Do
                        Begin
                          If (oFlag And ofMarked) <> 0 Then
                            c := Colors [pcMarkedItem];
                          FastWrite (IntrnlFname (dName, dAttr), Y, X, c);
                        End;
                    End;
      End;
    End;
  End;

  Procedure ShowFiles;
  Var
    w : Word;

  Begin
    With Pane^ Do
      Case ItemView Of
        ivAllInfo  : For w := AtTop To AtTop + (PM. BottomY - PM. TopY) Do
                       ShowFile (w);
        ivOnlyName : For w := AtTop To Pred (AtTop) + (PM. BottomY - PM. TopY
                         + 1) * 3
                     Do
                       ShowFile (w);
      End;
  End;

  Procedure MoveCursorDown;
  Begin
    With Pane^ Do
      If CurItem < TotalItems Then
      Begin
        ClrMarker;
        Inc (CurItem);

        Case ItemView Of
          ivAllInfo : If CurItem > (AtTop + (PM. BottomY - PM. TopY) ) Then
                      Begin
                        Inc (AtTop);
                        ScrollWindowUp (X1 + 1, Y1 + 2, X2 - 1, PM. BottomY, 1);
                        ShowFile (CurItem);
                      End;

         ivOnlyName : If CurItem > (2 + AtTop + (PM. BottomY - PM. TopY)
                         * 3) Then
                      Begin
                        Inc (AtTop);
                        ShowFiles;
                      End;
        End;
      End;
  End;

  Procedure MoveCursorUp;
  Begin
    With Pane^ Do
      If CurItem > 1 Then
      Begin
        ClrMarker;
        Dec (CurItem);

        Case ItemView Of
          ivAllInfo : If CurItem < AtTop Then
                      Begin
                        Dec (AtTop);
                        ScrollWindowDown (X1 + 1, Y1 + 2, X2 - 1, PM. BottomY,
                          1);
                        ShowFile (CurItem);
                      End;

         ivOnlyName : If CurItem < AtTop Then
                      Begin
                        Dec (AtTop);
                        ShowFiles;
                      End;
        End;
      End;
  End;

  Procedure MoveCursorRight;
  Var
    z, w : Word;
    X, Y : Byte;

  Begin
    With Pane^, PM Do
      If (ItemView = ivOnlyName) And (CurItem < TotalItems) Then
      Begin
        z := AtTop;
        ClrMarker;

        If (CurItem + (BottomY - TopY)) < TotalItems Then
        Begin
          w := CurItem;
          Inc (CurItem, Succ (BottomY - TopY));

          If ((w - AtTop) Div Succ (BottomY - TopY)) = 2 Then
            If AtTop + (BottomY - TopY + 1) * 4 <= TotalItems Then
              Inc (AtTop, Succ (BottomY - TopY))
            Else
            Begin
              CalcXY (w, X, Y);
              AtTop := Pred (TotalItems) - Succ ((BottomY - TopY) * 3);
              CurItem := TotalItems - (BottomY - TopY) + (Y - TopY);
            End;
        End Else
        Begin
          CurItem := TotalItems;
          If TotalItems < (BottomY - TopY + 1) * 3 Then
            AtTop := 1
          Else
            AtTop := Pred (TotalItems) - Succ ((BottomY - TopY) * 3);
        End;

        If AtTop <> z Then
          ShowFiles;
      End;
  End;

  Procedure MoveCursorLeft;
  Var
    z, w : Word;

  Begin
    With Pane^, PM Do
      If (ItemView = ivOnlyName) And (CurItem > 1) Then
      Begin
        z := AtTop;
        ClrMarker;

        If CurItem > Succ (BottomY - TopY) Then
        Begin
          w := CurItem;
          Dec (CurItem, Succ (BottomY - TopY) );

          If ((w - AtTop) Div Succ (BottomY - TopY)) = 0 Then
            If AtTop > Succ (BottomY - TopY)  Then
              Dec (AtTop, Succ (BottomY - TopY))
            Else
              AtTop := 1;
        End Else
        Begin
          CurItem := 1;
          AtTop := 1;
        End;

        If AtTop <> z Then
          ShowFiles;
      End;
  End;

Var
  i            : Word;
  Done         : Boolean;
  CurDir, sDir : PathStr;
  St           : String;

Label
  EoP;

Begin                        (*** Pane_Process ***)
  MarkedFilesColl^. FreeAll;
  TmpDir := '';

  GetMem (Pane, SizeOf (tPaneRec));
  If Pane = Nil Then
  Begin
    Pane_Process := pnNotEnoughMem;
    Exit;
  End;
  FillChar (Pane^, SizeOf (tPaneRec), 0);

  GetMem (Pane^. DirArray, SizeOf (DirRec) * MaxFiles);
  If Pane^. DirArray = Nil Then
  Begin
    Pane_Process := pnNotEnoughMem;
    FreeMem (Pane, SizeOf (tPaneRec));
    Exit;
  End;

  GetDir (0, SaveDir);  {save original directory}

  pth := UpString (pth);
  If (Length (pth) = 2) And (pth [2] = ':') Then
    pth := pth + '\';

  St := JustFileName (pth);
  If (Pos ('*', St) <> 0) Or (Pos ('?', St) <> 0) Then
    pth := Copy (pth, 1, Pred (Pos (St, pth)));

  With Pane^, PaneMetrix [pm_] Do
  Begin
    ItemView := iv_;
    X1 := xL;
    Y1 := yL;
    X2 := xL + 39;
    pnPath := pth;
    If Blinking Then Colors := DefPaneColors2
                Else Colors := DefPaneColors1; { ¤®¯®«­¨â¥«ì­ë¥ }
    PM. Y2 := Y1 + Y2;
    PM. TopY := Y1 + TopY;
    PM. BottomY := Y1 + BottomY;
    PM. CurItemInfoY := Y1 + CurItemInfoY;
    If MarkedInfoY <> 0 Then
      PM. MarkedInfoY := Y1 + MarkedInfoY;
  End;

  GetDir (0, CurDir);
  If CurDir <> pth Then
    ChDir (pth);
  GetDir (0, Pane^. pnPath);

  Opened := False;
  If Not Pane_Open Then
  Begin
    Pane_Process := pnStatus;
    Pane_Done;
    Exit;
  End;

  ShowFiles;
  done := False;

  {=Žá­®¢­®© æ¨ª« p ¡®âë ¯ ­¥«¨=}
  Repeat
    SetMarker;

    With Pane^ Do
      Case GetPaneCmd Of
        ckUpArrow: MoveCursorUp;
        ckDownArrow: MoveCursorDown;
        ckLeftArrow: MoveCursorLeft;
        ckRightArrow: MoveCursorRight;

        ckQuit: Begin
                  done := True;
                  Pane_Process := pnQuit;
                  pth := pnPath;
                End;

        ckEnter: With DirArray^ [CurItem] Do
                 Begin
                   GetDir (0, TmpDir);
                   If dAttr and Directory <> 0 Then
                   Begin
                      St := dName;
                      If St = '..' Then
                        If Length (TmpDir) <= 3 Then
                          Goto EoP
                        Else
                          TmpDir := Copy (TmpDir, rPos ('\', TmpDir,
                            Length (TmpDir)) + 1, 255)
                      Else
                        TmpDir := '';
                      ChDir (St);
                      DOSerror := IOResult;
                      GetDir (0, pnPath);
                      Pane_Open;
                      ShowFiles;
                   End Else
                   Begin
                     If MarkedCount = 0 Then
                       With DirArray^ [CurItem] Do
                       Begin
                         oFlag := oFlag xor ofMarked;
                         Inc (MarkedSize, dSize);
                         Inc (MarkedCount);

                         If Pos ('.', dName) = 0 Then
                           dName := dName + '.';

                         MarkedFilesColl^. Insert (NewStr (AddBackSlash
                           (Pane^. pnPath) + dName));
                       End
                     Else
                       For i := 1 To TotalItems Do
                         With DirArray^ [i] Do
                           If oFlag And ofMarked <> 0 Then
                           Begin
                             If Pos ('.', dName) = 0 Then
                               dName := dName + '.';

                             MarkedFilesColl^. Insert (NewStr (AddBackSlash
                               (Pane^. pnPath) + dName));
                           End;

                     done := True;
                     Pane_Process := pnOk;
                     pth := pnPath;
                   End;

               EoP:
                 End;

        ckHome: If CurItem > 1 Then
                Begin
                  CurItem := 1;
                  AtTop := 1;
                  ShowFiles;
                End;

        ckEnd: If CurItem < TotalItems Then
               Begin
                 CurItem := TotalItems;
                 Case ItemView Of
                   ivAllInfo: If TotalItems > (PM. bottomY - PM. TopY) Then
                                AtTop := TotalItems - (PM. bottomY - PM. TopY);
                   ivOnlyName: If TotalItems < (PM. BottomY - PM. TopY + 1) * 3
                               Then
                                 AtTop := 1
                               Else
                                 AtTop := Pred (TotalItems) - Succ
                                   ((PM. BottomY - PM. TopY) * 3);
                 End;

                 ShowFiles;
               End;

        ckMarkItem: If PM. MarkedInfoY <> 0 Then
                      With DirArray^ [CurItem] Do
                        If dAttr And Directory = 0 Then
                        Begin
                          oFlag := oFlag XOr ofMarked;
                          If oFlag And ofMarked <> 0 Then
                          Begin
                            Inc (MarkedSize, dSize);
                            Inc (MarkedCount);
                          End Else
                          Begin
                            Dec (MarkedSize, dSize);
                            Dec (MarkedCount);
                          End;

                          ShowMarkedInfo;
                          MoveCursorDown;
                        End;

         ckInverse: If PM. MarkedInfoY <> 0 Then
                    Begin
                      For i := 1 To TotalItems Do
                        With DirArray^ [i] Do
                          If dAttr And Directory = 0 Then
                          Begin
                            oFlag := oFlag XOr ofMarked;
                            If oFlag And ofMarked <> 0 Then
                            Begin
                              Inc (MarkedSize, dSize);
                              Inc (MarkedCount);
                            End Else
                            Begin
                              Dec (MarkedSize, dSize);
                              Dec (MarkedCount);
                            End;
                            ShowMarkedInfo;
                          End;

                      ShowFiles;
                    End;

        ckSelectAll: If PM. MarkedInfoY <> 0 Then
                     Begin
                       For i := 1 To TotalItems Do
                       Begin
                         With DirArray^ [i] Do
                           If dAttr And Directory = 0 Then
                           Begin
                             If oFlag And ofMarked = 0 Then
                             Begin
                               oFlag := oFlag XOr ofMarked;
                               Inc (MarkedSize, dSize);
                               Inc (MarkedCount);
                             End;
                           End;

                         ShowMarkedInfo;
                       End;

                       ShowFiles;
                     End;

        ckDeSelectAll: If PM. MarkedInfoY <> 0 Then
                       Begin
                         For i := 1 To TotalItems Do
                         Begin
                           With DirArray^ [i] Do
                             If dAttr And Directory = 0 Then
                             Begin
                               If oFlag And ofMarked <> 0 Then
                               Begin
                                 oFlag := oFlag XOr ofMarked;
                                 Dec (MarkedSize, dSize);
                                 Dec (MarkedCount);
                               End;
                             End;

                           ShowMarkedInfo;
                         End;

                         ShowFiles;
                       End;

        ckChangeDrive : Begin
                          GetDir (0, sDir);
                          ChDir (Drive2Change + ':\');

                          If IOResult <> 0 Then
                            ChDir (sDir)
                          Else
                          Begin
                            GetDir (0, pnPath);
                            Pane_Open;
                            ShowFiles;
                          End;
                        End;

        ckPgUp: Begin
                  If TotalItems - TotalItems + CurItem - 1 > 21 Then
                    Dec (CurItem, 21)
                  Else
                    CurItem := 1;

                  AtTop := CurItem;
                  ShowFiles;
                End;

        ckPgDn: Begin
                  If TotalItems - CurItem > 21 Then
                    Inc (CurItem, 21)
                  Else
                    CurItem := TotalItems;

                  AtTop := CurItem;
                  ShowFiles;
                End;

   ckBackSpace: Begin
                   GetDir (0, TmpDir);

                   If Length (TmpDir) > 3 Then
                   Begin
                     TmpDir := Copy (TmpDir, rPos ('\', TmpDir,
                       Length (TmpDir)) + 1, 255);
                     ChDir ('..');
                     DOSerror := IOResult;
                     GetDir (0, pnPath);
                     Pane_Open;
                     ShowFiles;
                   End
                   Else
                     If CurItem > 1 Then
                     Begin
                       CurItem := 1;
                       AtTop := 1;
                       ShowFiles;
                     End;
                End;
      End;
  Until done;

  ChDir (SaveDir);
  Pane_Done;
End;

End.
