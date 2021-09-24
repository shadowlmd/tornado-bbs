{  Small program, based on small unit, designed expecialy     }
{   for Tornado BBS System by Konstantin Klyagin              }
{                                                             }
{                               (C) Vlad Bakaev, 1996-97      }

{&Use32-}
{$I+}
{$Define Colored}

Program SeriousUserEditor;

{$IFNDEF VIRTUALPASCAL}
  {$A+,B-,E+,F+,G-,I+,L+,N-,O-,P-,Q+,R-,S+,T-,V-,X+,Y-}
  {$M 65520,60384,655360}
{$ENDIF}

Uses
  {$IFNDEF VIRTUALPASCAL}
  OpCrt,
  {$ELSE}
  Crt,
  {$ENDIF}
  Dos,
  Objects,
  App,
  Views,
  Dialogs,
  Drivers,
  Menus,
  Validate,
  MsgBox,
  StdDlg,
  {$IFNDEF VIRTUALPASCAL}
  TimeTask,
  {$ENDIF}
  tMisc,
  Crc;

{$I users.inc}
{$I ver.inc}

Const
  smNormal = 0; { SaveMode }
  smInsert = 1;
  smDelete = 2;

  cmSearchUser    = 201;
  cmInsertUser    = 202;
  cmDeleteUser    = 203;
  cmChangeSorting = 204;
  cmBox           = 205;
  cmAdvanced      = 206;
  cmAddFile       = 207;
  cmDelFile       = 208;
  cmTag           = 209;
  cmPvt           = 210;
  NoFile          = '0987654321';
  NotFoundStr     = '(not found)';
  UserCommandSet: TCommandSet = [0..255] - [cmClose, cmInsertUser, cmQuit,
                                           cmOk, cmCancel];
  FileCommandSet: TCommandSet = [cmDelFile];

  PathToUserTag : PathStr = 'USERTAG\';
  CTLFileName : PathStr = 'useredit.ctl';
  CTLTmpFileName : PathStr = 'useredit.tmp';
  UsersBaseFileName : PathStr = 'users.tor';
  UsersBaseTmpFileName : PathStr = 'users.tmp';
  TitleArray : array [False..True] of String[17] = ('Private', 'Previously tagged');
  Title : string[11] = ' files for ';

  OnlyNumPic     = '[-][*#]';
  AllUpperPic    = '[*!]';
  DateOnlyPic    = '{##}-{##}-{####}';
  CheckNamePic   = '[*@]'; {hack}

  {$IFNDEF MSDOS}
  AllFilesMask   = '*';
  {$ELSE}
  AllFilesMask   = '*.*';
  {$ENDIF}

Type
  FilesRecord     = record
                     Name   : PathStr;
                     Marked : Boolean;
                     Time,
                     Size   : LongInt;
                     Owned  : Boolean;
                     case Boolean of
                          False : ();
                          True  : (Owner : String[36]);
                    end;
  TrimUsersRecord = record
                     Name     : String[36];
                     Alias    : String[15];
                     Security,
                     Index    : Word;
                     NoCalls,
                     BirthDate,
                     LastDate,
                     DownloadsK,
                     UploadsK : Longint;
                    end;

  PFile = ^FilesRecord;
  PUser = ^TrimUsersRecord;

  SortType = (None, Name, BirthDate, LastDate, NoCalls, Security, DownloadsK,
              UploadsK);

  TMyApp = Object (TApplication)
             Constructor Init;
             Destructor Done;          Virtual;
             Procedure Run;            Virtual;
             Procedure InitDeskTop;    Virtual;
             Procedure InitMenuBar;    Virtual;
             Procedure InitStatusLine; Virtual;
             {$IFNDEF VIRTUALPASCAL}
             Procedure Idle;           Virtual;
             {$ENDIF}
           End;

  PEditUserDialog = ^TEditUserDialog;
  TEditUserDialog = object(TDialog)
                     procedure HandleEvent(var Event: TEvent); Virtual;
                    end;

  PWinBackground = ^TWinBackground;
  TWinBackground = object(TBackground)
                     procedure Draw; virtual;
                   end;

  PUserListBox = ^TUserListBox;
  TUserListBox = Object (TListBox)
                   Procedure HandleEvent (Var Event: TEvent); Virtual;
                   Function  GetText (
                     Item: {$IFNDEF VIRTUALPASCAL} Integer; {$ELSE} LongInt; {$ENDIF}
                     MaxLen: {$IFNDEF VIRTUALPASCAL} Integer {$ELSE} LongInt {$ENDIF}):
                     String; Virtual;
                   Function  Valid (
                     Command : {$IFNDEF VIRTUALPASCAL} Word {$ELSE} LongInt {$ENDIF}):
                     Boolean;  Virtual;
                 End;

  PFileListBox = ^TFileListBox;
  TFileListBox = Object (TListBox)
                   Procedure HandleEvent (Var Event: TEvent); Virtual;
                   Function  GetText (
                     Item: {$IFNDEF VIRTUALPASCAL} Integer; {$ELSE} LongInt; {$ENDIF}
                     MaxLen: {$IFNDEF VIRTUALPASCAL} Integer {$ELSE} LongInt {$ENDIF}):
                     String; Virtual;
                   Function  Valid (
                     Command : {$IFNDEF VIRTUALPASCAL} Word {$ELSE} LongInt {$ENDIF}):
                     Boolean;  Virtual;
                 End;

  TCompareAddr      = function(Item1, Item2 : Pointer) : ShortInt;

  PMyUserCollection = ^TMyUserCollection;
  TMyUserCollection = Object (TSortedCollection)
                        CTLSorting,
                        SortMethod  : SortType;
                        CompareAddr : TCompareAddr;
                        CurrentUser : Integer;
                        Function GetUserName(Item: Pointer): Pointer; virtual;
                        Function GetUserAlias(Item: Pointer): Pointer; virtual;
                        Function IsUser (Key: Pointer; var Index:
                          {$IFDEF VIRTUALPASCAL} LongInt {$ELSE} Integer {$ENDIF}): Boolean; virtual;
                        (*
                        Function Search (Key: Pointer; var Index:
                          {$IFDEF VIRTUALPASCAL} LongInt {$ELSE} Integer {$ENDIF}): Boolean; virtual;
                        *)
                        Function Compare (Key1, Key2 : Pointer):
                          {$IFNDEF VIRTUALPASCAL} Integer; {$ELSE} LongInt; {$ENDIF} Virtual;
                        Procedure AtPut (
                          Index: {$IFNDEF VIRTUALPASCAL} Integer; {$ELSE} LongInt; {$ENDIF}
                          Item: Pointer); Virtual;
                        Procedure FreeItem (Item: Pointer); Virtual;
                      End;

  PMyFileCollection = ^TMyFileCollection;
  TMyFileCollection = Object (TSortedCollection)
                        Function Compare (Key1, Key2 : Pointer):
                          {$IFNDEF VIRTUALPASCAL} Integer; {$ELSE} LongInt; {$ENDIF} Virtual;
                      End;

  { TMyStaticText }

  PMyStaticText = ^TMyStaticText;
  TMyStaticText = Object (TStaticText)
                    Procedure Draw; Virtual;
                    Function GetPalette: PPalette; Virtual;
                  End;

  PDisStaticText = ^TDisStaticText;
  TDisStaticText = Object (TStaticText)
                     Function GetPalette: PPalette; Virtual;
                   End;

  PMyValidator   = ^TMyValidator;
  TMyValidator   = Object (TPXPictureValidator)
                     Name : Boolean;
                     Function IsValid(const S: string): Boolean; virtual;
                     Procedure Error; Virtual;
                   End;

  PMyRadioButtons = ^TMyRadioButtons;
  TMyRadioButtons = Object (TRadioButtons)
                     procedure Draw; virtual;
                     function  Column(Item: Integer): Integer;
                     function  Row(Item: Integer): Integer;
                     procedure DrawMultiBox(const Icon, Marker: String); virtual;
                    End;

Const
  SortTypeStr : Array [None..UploadsK] Of String [10] =
    ('None',
     'Name',
     'BirthDate',
     'LastDate',
     'NoCalls',
     'Security',
     'DownloadsK',
     'UploadsK'
    );

Var
  FF                  : Text;
  FUser               : tUser;
  F, FO               : File Of tUser;
  PL                  : PListBox;
  L                   : PMyUserCollection;
  FLTag, FLPvt        : PMyFileCollection;
  PFLTag, PFLPvt      : PFileListBox;
  StTag, StPvt        : PStaticText;
  LSN, Foc, SS, TempS : String;
  PS                  : TMyApp;
  Count, CurPos       : System. Word;
  Index               : PMyStaticText;
  W                   : PDialog;
  R                   : TRect;
  B, BTag, BPvt       : PScrollBar;
  TagVisible, Changed : Boolean;
  {$IFNDEF VIRTUALPASCAL}
  SaveScr             : Pointer;
  WX, WY              : Byte;
  {$ENDIF}

Procedure InitUserIndex; Forward;

Procedure ReadUser (Number: System. Word);
Var
  i: Word;

Begin
  Assign (F, UsersBaseFileName);
  Reset (F);
  Seek (F, Number);
  Read (F, FUser);
  Close (F);
End;

Function NewPUser (Const User: tUser; Index: Word): PUser;
Var
  P: PUser;

Begin
  GetMem (P, SizeOf (TrimUsersRecord));
  P^. Name       := User. Name;
  P^. Alias      := User. Alias;
  P^. Security   := User. Security;
  P^. NoCalls    := User. NoCalls;
  P^. BirthDate  := User. BirthDate;
  P^. LastDate   := User. LastDate;
  P^. DownloadsK := User. DownloadsK;
  P^. UploadsK   := User. UploadsK;
  P^. Index      := Index;
  NewPUser := P;
End;

Procedure DisposePUser (P: PUser);
Begin
  If P <> Nil Then FreeMem (P, SizeOf (TrimUsersRecord));
End;

Function NewPFile (F: FilesRecord): PFile;
Var
  P: PFile;

Begin
  GetMem (P, SizeOf (F));
  P^ := F;
  NewPFile := P;
End;

Procedure DisposePFile (P: PFile);
Begin
  If P <> Nil Then FreeMem (P, SizeOf (FilesRecord));
End;

Procedure SaveUser (Number: System. Word; SaveMode : Byte);
Var
  i     : System. Word;
  Tmp   : tUser;

Begin
  Assign (F,  UsersBaseFileName);
  Assign (FO, UsersBaseTmpFileName);

  Reset (F);
  Rewrite (FO);
  Inc (Number);

  If SaveMode <> smInsert Then
  For i := 1 To Number-1 Do
  Begin
    If Not EoF (F)
    Then Read (F, Tmp)
    Else FillChar (Tmp, SizeOf (tUser), #0);
    Write (FO, Tmp);
  End;

  Case SaveMode Of
    smNormal : Begin
                 If Not EoF (F) Then Read (F, Tmp);
                 Write (FO, FUser);
               End;

    smInsert : Begin
                 While Not EoF (F) Do
                 Begin
                   Read (F, Tmp);
                   Write (FO, Tmp);
                 End;

                 Write (FO, FUser);
               End;

    smDelete : If Not EoF (F) Then Read (F, Tmp);
  End;

  While Not EoF (F) Do
  Begin
    Read (F, Tmp);
    Write (FO, Tmp);
  End;

  Close (F);
  Erase (F);
  Close (FO);
  Rename (FO, UsersBaseFileName);
End;

Function ConfirmDelete (const InS1, InS2 : String): Boolean;
Var
  R                : TRect;
  D                : PDialog;
  DX, DX1, DX2, i  : Byte;
  S1, S2           : String;

Begin
  S1 := CenterCh (InS1, ' ', 28);
  DX1 := Length (S1) div 2;

  If Length (InS2) > 0 Then
  Begin
    i := 1;
    S2  := CenterCh (InS2, ' ', 28);
    DX2 := Length (S2) Div 2;
  End Else
    i := 0;

  If DX2 > DX1 Then DX := DX2 Else DX := DX1;

  R. Assign (35 - DX, Round (Hi (WindMax)/2)-14+i+7, 45 + DX, Round (Hi (WindMax)/2)+5-i);
  D := New (PDialog, Init (R, 'Warning!'));

  With D^ Do
  Begin
    R. Assign (5+DX-DX1, 2, 6+DX+DX1, 3);
    Insert (New (PStaticText, Init (R, S1)));

    If i = 1 Then
    Begin
      R. Assign (5 + DX - DX2, 3, 6 + DX + DX2, 4);
      Insert (New (PStaticText, Init (R, S2)));
    End;

    R. Assign (DX + 5 - 13, 5 + i, DX + 5 - 3, 7 + i);
    Insert (New (PButton, Init (R, 'O~k~', cmOk, bfDefault)));
    R. Assign (DX + 5 + 3, 5 + i, DX + 5 + 13, 7 + i);
    Insert (New (PButton, Init (R, 'Cancel', cmCancel, bfNormal)));
    SelectNext (False);
  End;

  ConfirmDelete := (DeskTop^. ExecView (D) = cmOk);
  Dispose (D, Done);
End;

Function SortByName (Item1, Item2 : Pointer): ShortInt;
Var
  i      : ShortInt;
  S1, S2 : String;

Begin
  S1 := UpString(PUser (Item1)^. Name);
  S2 := UpString(PUser (Item2)^. Name);
  If S1 < S2 Then i := -1 else
  If S1 > S2 Then i :=  1 else
                  i :=  0;
  SortByName := i;
End;

Function SortByBirthDate (Item1, Item2 : Pointer): ShortInt;
Var
  i : ShortInt;

Begin
  If PUser (Item1)^. BirthDate = PUser (Item2)^. BirthDate Then i := SortByName (Item1, Item2) else
  If PUser (Item1)^. BirthDate > PUser (Item2)^. BirthDate Then i :=  1                        else
                                                                i := -1;
  SortByBirthDate := i;
End;

Function SortByLastDate (Item1, Item2 : Pointer): ShortInt;
Var
  i : ShortInt;

Begin
  If PUser(Item1)^. LastDate = PUser (Item2)^. LastDate Then i := SortByName (Item1, Item2) else
  If PUser(Item1)^. LastDate > PUser (Item2)^. LastDate Then i :=  1                        else
                                                             i := -1;
  SortByLastDate := i;
End;

Function SortByNoCalls (Item1, Item2 : Pointer): ShortInt;
Var
  i : ShortInt;

Begin
  if PUser (Item1)^. NoCalls = PUser (Item2)^. NoCalls Then i := SortByName (Item1, Item2) else
  if PUser (Item1)^. NoCalls < PUser (Item2)^. NoCalls Then i :=  1                        else
                                                            i := -1;
  SortByNoCalls := i;
End;

Function SortBySecurity (Item1, Item2: Pointer): ShortInt;
Var
  i : ShortInt;

Begin
  If PUser (Item1)^. Security = PUser (Item2)^. Security Then i := SortByName (Item1, Item2) else
  If PUser (Item1)^. Security < PUser (Item2)^. Security Then i :=  1                        else
                                                              i := -1;
  SortBySecurity := i;
End;

Function SortByDownloadsK(Item1, Item2 : Pointer): ShortInt;
Var
  i : ShortInt;

Begin
  If PUser (Item1)^. DownloadsK = PUser (Item2)^. DownloadsK Then i := SortByName (Item1, Item2) else
  If PUser (Item1)^. DownloadsK < PUser (Item2)^. DownloadsK Then i :=  1                        else
                                                                  i := -1;
  SortByDownloadsK := i;
End;

Function SortByUploadsK  (Item1, Item2 : Pointer): ShortInt;
Var
  i : ShortInt;

Begin
  If PUser (Item1)^. UploadsK = PUser (Item2)^. UploadsK Then i := SortByName (Item1, Item2) else
  If PUser (Item1)^. UploadsK < PUser (Item2)^. UploadsK Then i :=  1                        else
                                                              i := -1;
  SortByUploadsK := i;
End;

Function SortByNone (Item1, Item2 : Pointer): ShortInt;
Begin
  SortByNone := -1;
End;

Procedure StartResorting;
Begin
  L^. FreeAll;
  InitUserIndex;
End;

Function ChangeSortCriteria: Boolean;
Type
  TDialogData = Record
    Sorting : {$IFDEF VIRTUALPASCAL} LongInt {$ELSE} Word {$ENDIF};
  End;

Var
  Result  : Word;
  I       : Byte;
  R       : TRect;
  D       : PDialog;
  Cluster : PCluster;
  Data    : TDialogData;

Begin
  R. Assign (25, Round (Hi (WindMax)/2)-7, 56, Round (Hi (WindMax)/2)+7);
  D := New (PDialog, Init (R, 'Sorting method'));
  ChangeSortCriteria := False;

  With D^ Do
  Begin
    R. Assign (5, 2, 26, 10);
    Cluster := New (PRadioButtons, Init (R,
               NewSItem (SortTypeStr [None],
               NewSItem (SortTypeStr [Name],
               NewSItem (SortTypeStr [BirthDate],
               NewSItem (SortTypeStr [LastDate],
               NewSItem (SortTypeStr [NoCalls],
               NewSItem (SortTypeStr [Security],
               NewSItem (SortTypeStr [DownloadsK],
               NewSItem (SortTypeStr [UploadsK],
               nil))))))))));

    Insert (Cluster);
    Data. Sorting := Ord (L^. SortMethod);
    i := Data. Sorting;
    R. Assign (3, 11, 13, 13);
    Insert (New (PButton, Init (R, '~R~esort', cmOk, bfDefault)));
    R. Assign (17, 11, 27, 13);
    Insert (New (PButton, Init (R, '~C~ancel', cmCancel, bfNormal)));
    SelectNext (False);
    SetData (Data);
  End;

  Result := Desktop^. ExecView (D);
  D^. GetData (Data);
  Dispose (D, Done);

  If (Result = cmOk) And (i <> Data. Sorting) Then
  Begin
    L^. SortMethod := SortType (Data. Sorting);

    Case Data.Sorting of
      0 : L^.CompareAddr := SortByNone;
      1 : L^.CompareAddr := SortByName;
      2 : L^.CompareAddr := SortByBirthDate;
      3 : L^.CompareAddr := SortByLastDate;
      4 : L^.CompareAddr := SortByNoCalls;
      5 : L^.CompareAddr := SortBySecurity;
      6 : L^.CompareAddr := SortByDownloadsK;
      7 : L^.CompareAddr := SortByUploadsK;
    End;

    StartResorting;
    ChangeSortCriteria := True;
  End;
End;

Function EditUserTVAdvanced: Boolean;
Type
  TDialogData = Record
    MsgsPosted, TimeUsedToday,
    TotalTime     : String [10];
  End;

Var
  R      : TRect;
  Dialog : PDialog;
  IL     : PInputLine;
  View   : PView;
  Data   : TDialogData;
  C      : {$IFDEF VIRTUALPASCAL} LongInt {$ELSE} Integer {$ENDIF};

Begin
  FillChar (Data, SizeOf (Data), #0);

  R. Assign (20, Round (Hi (WindMax)/2)-6, 59, Round (Hi (WindMax)/2)+5);
  Dialog := New (PEditUserDialog, Init (R, 'Advanced'));
  With Dialog^ Do
  Begin
    R. Assign (20, 2, 32, 3);
    IL := New (PInputLine, Init (R, 10));
    IL^.SetValidator(New(PRangeValidator, Init (0, 2147483647)));
    IL^. State := IL^. State + sfDefault;
    Insert (IL);
    R. Assign (2, 2, 18, 3);
    View := New (PLabel, Init (R, '~M~essages Posted', IL) );
    Insert (View);
    Str (FUser. MsgsPosted, Data. MsgsPosted);

    R. Assign (20, 3, 32, 4);
    IL := New (PInputLine, Init (R, 10));
    IL^. SetValidator(New(PRangeValidator, Init (0, 2147483647)));
    Insert (IL);
    R. Assign (2, 3, 18, 4);
    Insert (New (PLabel, Init (R, '~T~ime used today', IL)));
    Str (FUser. TimeUsedToday, Data. TimeUsedToday);

    R. Assign (20, 4, 32, 5);
    IL := New (PInputLine, Init (R, 10));
    IL^. SetValidator(New(PRangeValidator, Init (0, 2147483647)));
    Insert (IL);
    R. Assign (2, 4, 18, 5);
    Insert (New (PLabel, Init (R, 'Time l~e~ft', IL)));
    Str (FUser. TotalTime, Data. TotalTime);

    R. Assign (20, 6, 27, 7);
    View := New (PDisStaticText, Init (R, ' '+Long2Str(FUser. AvgCPS)));
    Insert (View);
    R. Assign (2, 6, 14, 7);
    View := New (PLabel, Init (R, 'Average CPS', View));
    Insert (View);

    R. Assign (4, 8, 18, 10);
    View := New (PButton, Init (R, 'O~k~', cmOk, bfDefault));
    Insert (View);
    R. Assign (20, 8, 34, 10);
    View := New (PButton, Init (R, 'Cancel', cmCancel, bfNormal));
    Insert (View);

    SelectNext (False);
    SetData (Data);
  End;

  If Desktop^. ExecView (Dialog) = cmOk Then
  Begin
    Dialog^. GetData (Data);
    Val (Data. MsgsPosted, FUser. MsgsPosted, C);
    Val (Data. TimeUsedToday, FUser. TimeUsedToday, C);
    Val (Data. TotalTime, FUser. TotalTime, C);
    EditUserTVAdvanced := True;
  End Else
    EditUserTVAdvanced := False;

  Dispose (Dialog, Done);
End;

Function EditUserTVBox: Boolean;
Var
  R            : TRect;
  D            : PDialog;
  FileName, S  : String;
  F            : FilesRecord;
  DirInfo      : SearchRec;
  i            : Word;

Label
  SkipSave;

Begin
  FLTag := New(PMyFileCollection, Init(5, 5));
  FLPvt := New(PMyFileCollection, Init(5, 5));
  FileName := AddBackSlash(PathToUserTag) + HexL(Crc32Str(FUser.Name)) + '.LST';
  Assign(FF, FileName);
  if FileExists(FileName) then
  begin
    {$I-}
    Reset(FF);
    {$I+}
    if IOResult<>0 then
    begin
      MessageBox('File '+FileName+'in use or locked by another process',
                 nil, mfError+mfOkButton);
      Dispose(FLTag, Done);
      Exit;
    end;
    while not EOF(FF) do
    begin
      ReadLn(FF, S);
      S := Trim(S);
      If S[1] = ';' Then Continue;

      FillChar(F, SizeOF(F), #0);
      if S[1]='*' then
      begin
        F.Owned := True;
        F.Owner := ExtractWord(1, S, ['*','|']);
        Delete(S, 1, Length(F.Owner)+2);
      end else
        if S[1]='+' then Delete(S, 1, 1);

      F.Name := UpString(ExtractWord(1, S, [#0..#32,#255]));
      FindFirst(F.Name, AnyFile-VolumeId-Directory, DirInfo);
      if DOSError=0 then
      begin
        F.Size := DirInfo.Size;
        F.Time := DirInfo.Time;
      end;
      F.Marked := False;
      if F.Owned then FLPvt^.Insert(NewPFile (F))
                 else FLTag^.Insert(NewPFile (F));
      {$IFDEF VIRTUALPASCAL}
      FindClose(DirInfo);
      {$ENDIF}
    end;
    Close(FF);
  end;

  TagVisible := True;

  R.Assign(2, Round(Hi(WindMax)/2)-9, 77, Round(Hi(WindMax)/2)+8);
  D := New(PDialog, Init(R, TitleArray[TagVisible]+Title+FUser.Name));
  With D^ Do
  Begin
    DisableCommands([cmTag]);
    if FLTag^.Count=0 then DisableCommands(FileCommandSet)
                      else EnableCommands (FileCommandSet);

    R.Assign(71, 3, 72, 13);
    BTag := New(PScrollBar, Init(R));
    Insert(BTag);

    R.Assign(151, 3, 152, 13);
    BPvt := New(PScrollBar, Init(R));
    Insert(BPvt);

    R.Assign(3, 2, 72, 3);
    Insert(New(PStaticText, Init(R, Replicate('─', 69))));

    R.Assign(3, 1, 71, 2);
    StTag := New(PStaticText, Init(R, Replicate(' ',  3) +
                         'Filename' + Replicate(' ', 37) +
                         'Date'     + Replicate(' ',  9) +
                         'Size'));
    Insert(StTag);
    R.Assign(3, 3, 71, 13);
    PFLTag := New(PFileListBox, Init(R, 1, BTag));
    PFLTag^.NewList(FLTag);
    Insert(PFLTag);

    R.Assign(83, 1, 151, 2);
    StPvt := New(PStaticText, Init(R, Replicate(' ',  3) +
                         'Uploader' + Replicate(' ', 22) +
                         'Filename' + Replicate(' ',  7) +
                         'Date'     + Replicate(' ',  9) +
                         'Size'));
    Insert(StPvt);
    R.Assign(83, 3, 151, 13);
    PFLPvt := New(PFileListBox, Init(R, 1, BPvt));
    PFLPvt^.NewList(FLPvt);
    Insert(PFLPvt);

    R.Assign( 2, 14, 10, 16);
    Insert(New(PButton, Init(R, '~<~Tag',   cmTag,     bfNormal )));
    R.Assign(13, 14, 23, 16);
    Insert(New(PButton, Init(R, '~I~nsert', cmAddFile, bfNormal )));
    R.Assign(26, 14, 36, 16);
    Insert(New(PButton, Init(R, '~D~elete', cmDelFile, bfNormal )));
    R.Assign(39, 14, 49, 16);
    Insert(New(PButton, Init(R, '~O~k',     cmOk,      bfDefault)));
    R.Assign(52, 14, 62, 16);
    Insert(New(PButton, Init(R, '~C~ancel', cmCancel,  bfNormal )));
    R.Assign(65, 14, 73, 16);
    Insert(New(PButton, Init(R, '~>~Pvt',   cmPvt,     bfNormal )));

    FocusNext(False);
  End;

  if Desktop^.ExecView(D) = cmOk then
  begin
    {$I-}
    ReWrite(FF);
    {$I+}

    If IOResult = 0 Then
    Begin
      if (FLTag^.Count = 1) and (FLPvt^.Count = 0) then
        if PFile(FLTag^.At(0))^.Name=NoFile then
        Begin
          Close(FF);
          Erase(FF);
          goto SkipSave;
        End;

      If FLTag^.Count > 0 Then
      For i := 0 to FLTag^.Count-1 do
      If FileExists(PFile(FLTag^.At(i))^.Name) Then
        WriteLn(FF, '+' + PFile(FLTag^.At(i))^.Name);

      If FLPvt^.Count > 0 Then
      For i := 0 To FLPvt^.Count-1 Do
      WriteLn(FF, '*' + PFile(FLPvt^.At(i))^.Owner + '|' + PFile(FLPvt^.At(i))^.Name);

      Close(FF);
    End;
  end;

 SkipSave:

  Dispose(D, Done);

  While FLTag^.Count > 0 Do
  Begin
    DisposePFile(FLTag^.At(0));
    FLTag^.AtDelete(0);
  End;

  While FLPvt^.Count > 0 Do
  Begin
    DisposePFile(FLPvt^.At(0));
    FLPvt^.AtDelete(0);
  End;

  Dispose(FLPvt, Done);
  Dispose(FLTag, Done);
End;

(****-------------- begin TVision ---------------------------------****)
procedure TWinBackground.Draw;
const
  Ch: Array[0..9] of Char = '12345 ░▒▓█';
  Ws: Array[-1..13] of System. Word = ($2007, $b071, $b179, $b279, $db79,
    $b209, $b171, $b271, $b109, $db71, $b201, $b009, $b101, $b001, $2000);
var
  B: TDrawBuffer;
  X, Y, YY: Integer;
  W: System. Word;
  Ch1, Ch2: Char;
  B1, B2: Byte;
begin
  for Y := 0 to Size.Y - 1 do
  begin
    W := Ws[Y * 14 div Size.Y];
    Ch1 := Char(W shr 8);
    B1 := Byte(W);
    W := Ws[(Y - 1) * 14 div Size.Y];
    Ch2 := Char(W shr 8);
    B2 := Byte(W);
    MoveChar(B, Ch1, B1, Size.X);
    for X := 0 to Size.X - 1 do if Odd(X) then
      MoveChar(B[X], Ch2, B2, 1);
    WriteLine(0, Y, Size.X, 1, B);
  end;
end;

Procedure TEditUserDialog.HandleEvent(var Event: TEvent);
Begin
  Inherited HandleEvent (Event);

  Case Event. What Of
    evCommand:
     Case Event. Command Of
       cmBox      : If State and sfModal <> 0 Then
                    Begin
                      EditUserTVBox;
                      ClearEvent (Event);
                    End;
       cmAdvanced : If State and sfModal <> 0 Then
                    Begin
                      EditUserTVAdvanced;
                      ClearEvent (Event);
                    End;
     End;
  End;
End;

Function InputNameBox (Prompt: String; Var S: String): System. Word;
Var
  Dialog: PDialog;
  Control: PView;
  R: TRect;
  C: Word;

Begin
  R. Assign (0, 0, 60, 7);
  R. Move ((Desktop^. Size. X - R. B. X) Div 2, (Desktop^. Size. Y - R. B. Y) Div 2);
  Dialog := New (PDialog, Init (R, Prompt) );
  With Dialog^ Do
  Begin
    R. Assign (3, 2, Size. X - 3, 3);
    Control := New (PInputLine, Init (R, 36));
    Insert (Control);
    R. Assign (Size. X - 24, Size. Y - 3, Size. X - 14, Size. Y - 1);
    Insert (New (PButton, Init (R, 'O~k~', cmOk, bfDefault)));
    Inc (R. A. X, 12);
    Inc (R. B. X, 12);
    Insert (New (PButton, Init (R, 'Cancel', cmCancel, bfNormal)));
    Inc (R. A. X, 12);
    Inc (R. B. X, 12);
    SelectNext (False);
    SetData (S);
  End;
  C := DeskTop^. ExecView (Dialog);
  If C <> cmCancel Then Dialog^. GetData (S);
  Dispose (Dialog, Done);
  InputNameBox := C;
End;

Constructor TMyApp. Init;
Begin
  {$IFDEF Colored}
  GetPalette^ [06] := #$70;
  GetPalette^ [33] := #$1E;
  GetPalette^ [34] := #$1A;
  GetPalette^ [37] := #$1F;
  GetPalette^ [38] := #$17;
  GetPalette^ [39] := #$1F;
  GetPalette^ [40] := #$1E;
  GetPalette^ [46] := #$10;
  GetPalette^ [47] := #$17;
  GetPalette^ [48] := #$1F;
  GetPalette^ [50] := #$1E;
  GetPalette^ [51] := #$1E;
  GetPalette^ [55] := #$20;
  GetPalette^ [57] := #$17;
  GetPalette^ [58] := #$7F;
  {$ENDIF}
  Inherited Init;
End;

{$IFNDEF VIRTUALPASCAL}
Procedure TMyApp.Idle;
Begin
  TimeSlice;
  Inherited Idle;
End;
{$ENDIF}

Function EditUserTV (Var FUser : tUser) : Boolean;

Function SpaceAdd (St: String; Num: Byte): String;
Begin
  While Length (St) < Num Do St := St + ' ';
  SpaceAdd := St;
End;

Type
  TDialogData = Record
    Name                              : String [36];
    Security                          : String [5];
    Location, Organization, Address1,
    Address2, Address3                : String [50];
    Comment                           : String [80];
    Password, Alias, HPhone, BPhone   : String [15];
    BirthDate                         : String [10];
    Lang                              : String [8];
    {XLAT                              : String [8];}
    Flags                             : String [26];
    Protocol                          : String [1];
    CBSettings, Emu                   : {$IFNDEF VIRTUALPASCAL}
                                          Word;
                                        {$ELSE}
                                          LongInt;
                                        {$ENDIF}
    Lines                             : String [3];
    Downloads, DownloadsK, TodayK,
    Uploads, UploadsK                 : String [10];
  End;

{some from APABSPCL.PAS by TurboPower}
Const
  {почти то же, но (С) мой ;)}
  EmuTypeString : Array [teANSI..teAvatar] Of String [6] =
    ('ANSI  ', 'TTY   ', 'AVATAR');

Var
  i          : Byte;
  R          : TRect;
  Dialog     : PDialog;
  View       : PView;
  TmpUser    : tUser;
  IL         : PInputLine;
  Data       : TDialogData;
  C          : {$IFNDEF VIRTUALPASCAL} Integer; {$ELSE} LongInt; {$ENDIF}
  W          : Word;
  RB         : PMyRadioButtons;
  CB         : PCheckBoxes;
  Flg        : Boolean;

Label
  Restart;

Begin
  Flg := True;
  FillChar (Data, SizeOf (Data), #0);

  Restart:
  R. Assign (3, Round (Hi (WindMax)/2)-11, 76, Round (Hi (WindMax)/2)+11);
  Dialog := New (PEditUserDialog, Init (R, 'Edit User'));
  With Dialog^ Do
  Begin
    R. Assign (16, 1, 54, 2);
    IL := New (PInputLine, Init (R, 36));
    IL^.Options := $1005;
    IL^.SetValidator(New(PMyValidator, Init (CheckNamePic, True)));
    IL^. State := IL^. State + sfDefault;
    Insert (IL);
    R. Assign (2, 1, 7, 2);
    View := New (PLabel, Init (R, 'N~a~me', IL) );
    Insert (View);
    if Flg then Data. Name := FUser. Name;

    R. Assign (63, 1, 70, 2);
    IL := New (PInputLine, Init (R, 5));
    IL^. SetValidator (New (PRangeValidator, Init (0, 65535)));
    Insert (IL);
    R. Assign (56, 1, 63, 2);
    Insert (New (PLabel, Init (R, 'S~e~c.', IL)));
    if Flg then Str (FUser. Security, Data. Security);

    R. Assign (16, 2, 70, 3);
    View := New (PInputLine, Init (R, 50));
    Insert (View);
    R. Assign (2, 2, 11, 3);
    View := New (PLabel, Init (R, 'Locatio~n~', View));
    Insert (View);
    if Flg then Data. Location := FUser. Location;

    R. Assign (16, 3, 70, 4);
    View := New (PInputLine, Init (R, 50));
    Insert (View);
    R. Assign (2, 3, 15, 4);
    View := New (PLabel, Init (R, 'Organi~z~ation', View));
    Insert (View);
    if Flg then Data. Organization := FUser. Organization;

    R. Assign (16, 4, 70, 5);
    View := New (PInputLine, Init (R, 50));
    Insert (View);
    R. Assign (2, 4, 14, 5);
    View := New (PLabel, Init (R, '~1~st address', View));
    Insert (View);
    if Flg then Data. Address1 := FUser. Address1;

    R. Assign (16, 5, 70, 6);
    View := New (PInputLine, Init (R, 50));
    Insert (View);
    R. Assign (2, 5, 14, 6);
    View := New (PLabel, Init (R, '~2~nd address', View));
    Insert (View);
    if Flg then Data. Address2 := FUser. Address2;

    R. Assign (16, 6, 70, 7);
    View := New (PInputLine, Init (R, 50));
    Insert (View);
    R. Assign (2, 6, 14, 7);
    View := New (PLabel, Init (R, '~3~rd address', View));
    Insert (View);
    if Flg then Data. Address3 := FUser. Address3;

    R. Assign (16, 7, 70, 8);
    View := New (PInputLine, Init (R, 80));
    Insert (View);
    R. Assign (2, 7, 10, 8);
    View := New (PLabel, Init (R, 'Co~m~ment', View));
    Insert (View);
    if Flg then Data. Comment := FUser. Comment;

    R. Assign (16, 8, 33, 9);
    View := New (PInputLine, Init (R, 15));
    Insert (View);
    R. Assign (2, 8, 11, 9);
    View := New (PLabel, Init (R, 'Passwo~r~d', View));
    Insert (View);
    if Flg then Data. Password := FUser. Password;

    R. Assign (16, 9, 27, 10);
    IL := New (PInputLine, Init (R, 15));
    IL^.SetValidator(New(PMyValidator, Init (CheckNamePic, True)));
    Insert (IL);
    R. Assign (2,  9, 11, 10);
    View := New (PLabel, Init (R, 'Alias', View));
    Insert (View);
    if Flg then Data. Alias := FUser. Alias;

    R. Assign (16, 10, 26, 11);
    View := New (PInputLine, Init (R, 15));
    Insert (View);
    R. Assign ( 2, 10, 13, 11);
    View := New (PLabel, Init (R, 'Home ~p~hone', View));
    Insert (View);
    if Flg then Data. HPhone := FUser. HPhone;

    R. Assign (16, 11, 26, 12);
    View := New (PInputLine, Init (R, 15));
    Insert (View);
    R. Assign ( 2, 11, 13, 12);
    View := New (PLabel, Init (R, 'Data p~h~one', View));
    Insert (View);
    if Flg then Data. BPhone := FUser. BPhone;

    R. Assign (16, 12, 28, 13);
    IL := New (PInputLine, Init (R, 10));
    IL^. SetValidator (New (PMyValidator, Init (DateOnlyPic, True)));
    Insert (IL);
    R. Assign ( 2, 12, 12, 13);
    Insert (New (PLabel, Init (R, '~B~irthdate', IL)));
    if Flg then Data. BirthDate := Long2Date (FUser. BirthDate, 'DD-MM-YYYY');

    R. Assign (16, 13, 26, 14);
    View := New (PInputLine, Init (R, 8));
    Insert (View);
    R. Assign ( 2, 13, 11, 14);
    View := New (PLabel, Init (R, 'Lan~g~uage', View));
    Insert (View);
    if Flg then Data. Lang := FUser. Lang;

    R. Assign (16, 15, 40, 16);
    IL := New (PInputLine, Init (R, 26));
    IL^.SetValidator (New (PMyValidator, Init (AllUpperPic, False)));
    Insert (IL);
    R. Assign (2, 15, 8, 16);
    View := New (PLabel, Init (R, '~F~lags', IL));
    Insert (View);
    if Flg then Data. Flags := UpString (FUser. Flags);

    R. Assign (16, 16, 19, 17);
    IL := New (PInputLine, Init (R, 1));
    Insert (IL);
    R. Assign (2, 16, 11, 17);
    Insert (New (PLabel, Init (R, 'Proto~c~ol', IL)));
    if Flg then Data. Protocol := FUser. Protocol;

    R. Assign (28, 10, 42, 15);
    CB := New (PCheckBoxes, Init (R, NewSItem ('More',
                                     NewSItem ('Hotkeys',
                                     NewSItem ('Frames',
                                     NewSItem ('Guest',
                                     NewSItem ('FS editor',
                                     Nil)))))));
    Insert (CB);
    R. Assign (28, 9, 38, 10);
    View := New (PLabel, Init (R, 'Se~t~tings', CB));
    Insert (View);
    I := 0;
    If FUser. More Then Inc (I);
    If FUser. HotKeys Then Inc (I, 2);
    If FUser. Frames Then Inc (I, 4);
    If FUser. Guest Then Inc (I, 8);
    If FUser. FSeditor Then Inc (I, 16);
    if Flg then Data. CBSettings := I;

    R. Assign (28, 17, 41, 21);
    RB := New (PMyRadioButtons, Init (R, NewSItem (EmuTypeString [teANSI],
                                         NewSItem (EmuTypeString [teTTY],
                                         NewSItem (EmuTypeString [teAvatar],
                                         Nil)))));
    Insert (RB);
    R. Assign (28, 16, 38, 17);
    Insert (New (PLabel, Init (R, 'Em~u~lation', RB)));
    If Flg Then
    Begin
      Data. Emu := Ord (FUser. Emu);
      If Data. Emu > 2 Then Data. Emu := 0;
    End;

    R. Assign (58, 10, 70, 11);
    IL := New (PInputLine, Init (R, 3));
    IL^. SetValidator (New (PRangeValidator, Init (0, 255)));
    Insert (IL);
    R. Assign (42, 10, 52, 11);
    View := New (PLabel, Init (R, 'L~i~nes', IL));
    Insert (View);
    if Flg then Str (FUser. Lines, Data. Lines);

    R. Assign (58, 11, 70, 12);
    IL := New (PInputLine, Init (R, 10));
    IL^. SetValidator (New (PMyValidator, Init (OnlyNumPic, True)));
    Insert (IL);
    R. Assign (42, 11, 52, 12);
    View := New (PLabel, Init (R, 'Do~w~nloads', IL));
    Insert (View);
    if Flg then Str (FUser. Downloads, Data. Downloads);

    R. Assign (58, 12, 70, 13);
    IL := New (PInputLine, Init (R, 10));
    IL^. SetValidator (New (PMyValidator, Init (OnlyNumPic, True)));
    Insert (IL);
    R. Assign (42, 12, 57, 13);
    View := New (PLabel, Init (R, '~D~ownloads (Kb)', IL));
    Insert (View);
    if Flg then Str (FUser. DownloadsK, Data. DownloadsK);

    R. Assign (58, 13, 70, 14);
    IL := New (PInputLine, Init (R, 10));
    IL^. SetValidator (New (PMyValidator, Init (OnlyNumPic, True)));
    Insert (IL);
    R. Assign (42, 13, 53, 14);
    View := New (PLabel, Init (R, 'Toda~y~ (Kb)', IL));
    Insert (View);
    if Flg then Str (FUser. TodayK, Data. TodayK);

    R. Assign (58, 14, 70, 15);
    IL := New (PInputLine, Init (R, 10));
    IL^. SetValidator (New (PMyValidator, Init (OnlyNumPic, True)));
    Insert (IL);
    R. Assign (42, 14, 53, 15);
    View := New (PLabel, Init (R, 'Up~l~oads', IL));
    Insert (View);
    if Flg then Str (FUser. Uploads, Data. Uploads);

    R. Assign (58, 15, 70, 16);
    IL := New (PInputLine, Init (R, 10));
    IL^. SetValidator (New (PMyValidator, Init (OnlyNumPic, True)));
    Insert (IL);
    R. Assign (42, 15, 55, 16);
    View := New (PLabel, Init (R, 'Upload~s~ (Kb)', IL));
    Insert (View);
    if Flg then Str (FUser. UploadsK, Data. UploadsK);

    R. Assign (16, 17, 28, 18);
    View := New (PDisStaticText, Init (R, ' ' + Long2Date (FUser. LastDate, 'DD-MM-YYYY')));
    Insert (View);
    R. Assign (2, 17, 12, 18);
    View := New (PLabel, Init (R, 'Last date', View));
    Insert (View);

    R. Assign (16, 18, 28, 19);
    View := New (PDisStaticText, Init (R, ' ' + Long2Date (FUser. FirstDate, 'DD-MM-YYYY')));
    Insert (View);
    R. Assign (2, 18, 13, 19);
    View := New (PLabel, Init (R, 'First date', View));
    Insert (View);

    R. Assign (16, 19, 28, 20);
    View := New (PDisStaticText, Init (R, ' '+Long2Str(FUser. NoCalls)));
    Insert (View);
    R. Assign (2, 19, 7, 20);
    View := New (PLabel, Init (R, 'Calls', View));
    Insert (View);

    R. Assign (42, 17, 56, 19);
    View := New (PButton, Init (R, 'FileB~o~x', cmBox, bfNormal));
    Insert (View);

    R. Assign (42, 19, 56, 21);
    View := New (PButton, Init (R, 'Ad~v~anced', cmAdvanced, bfNormal));
    Insert (View);

    R. Assign (56, 17, 70, 19);
    View := New (PButton, Init (R, 'O~k~', cmOk, bfDefault));
    Insert (View);

    R. Assign (56, 19, 70, 21);
    View := New (PButton, Init (R, 'Cancel', cmCancel, bfNormal));
    Insert (View);

    SelectNext (False);
    SetData (Data);
  End;

  If Desktop^. ExecView (Dialog) = cmOk Then
  Begin
    Dialog^. GetData (Data);

    If Trim (Data. Name) = '' Then
    Begin
      Flg := False;
      Dispose (Dialog, Done);
      GoTo Restart;
    End;

    Begin
      FUser. Name := Data. Name;
      Val (Data. Security, W, C);
      FUser. ReReadLimit := FUser. ReReadLimit or (W<>FUser.Security);
      FUser. Location := Data. Location;
      FUser. Organization := Data. Organization;
      FUser. Address1 := Data. Address1;
      FUser. Address2 := Data. Address2;
      FUser. Address3 := Data. Address3;
      FUser. Comment := Data. Comment;
      FUser. Password := Data. Password;
      FUser. Alias := Data. Alias;
      FUser. Flags := Data. Flags;
      FUser. HPhone := Data. HPhone;
      FUser. BPhone := Data. BPhone;
      FUser. BirthDate := Date2Long (ReFormatDate (Data. BirthDate, 'DD-MM-YYYY', DefaultDateMask));
      Val (Data. Security, FUser. Security, C);
      Val (Data. Lines, FUser. Lines, C);
      FUser. Lang := Data. Lang;
      case Data. Emu of
            0 : FUser. Emu := teANSI;
            1 : FUser. Emu := teTTY;
            2 : FUser. Emu := teAvatar;
           end;
      FUser. More := (Data. CBSettings And $01 = $01);
      FUser. HotKeys := (Data. CBSettings And $02 = $02);
      FUser. Frames := (Data. CBSettings And $04 = $04);
      FUser. Guest := (Data. CBSettings And $08 = $08);
      FUser. FSeditor := (Data. CBSettings And 16 = 16);
      FUser. Protocol := Data. Protocol [1];
      Val (Data. Downloads, FUser. Downloads, C);
      Val (Data. DownloadsK, FUser. DownloadsK, C);
      Val (Data. TodayK, FUser. TodayK, C);
      Val (Data. Uploads, FUser. Uploads, C);
      Val (Data. UploadsK, FUser. UploadsK, C);
      EditUserTV := True;
    End;
  End Else
    EditUserTV := False;

  Dispose (Dialog, Done);
End;

Function TMyUserCollection. GetUserName (Item: Pointer): Pointer;
Begin
  GetUserName := NewStr (PUser (Item)^. Name);
End;

Function TMyUserCollection. GetUserAlias (Item: Pointer): Pointer;
Begin
  GetUserAlias := NewStr (PUser (Item)^. Alias);
End;

Function TMyUserCollection. IsUser (Key: Pointer; Var Index:
         {$IFDEF VIRTUALPASCAL} LongInt {$ELSE} Integer {$ENDIF}): Boolean;
Var
  i      : Integer;
  PName,
  PAlias : PString;
  SName,
  SAlias,
  SKey   : String;

Begin
  IsUser := False;
  If Key = Nil Then Exit;

  If Count = 0 Then
    Exit;

  SKey := UpString(PString(Key)^);
  If Length(SKey) = 0 Then Exit;

  For i := 0 To Count-1 do
  begin
    PName := GetUserName (Items^[i]);
    SName := UpString(PName^);
    DisposeStr(PName);
    PAlias := GetUserAlias (Items^[i]);
    SAlias := UpString(PAlias^);
    DisposeStr(PAlias);
    if (SKey = SName) Or (SKey = SAlias) then
      if i <> CurrentUser then
      begin
        IsUser := True;
        Index := i;
        break;
      end;
  end;
end;

(*
Function TMyUserCollection. Search (Key: Pointer; Var Index:
         {$IFDEF VIRTUALPASCAL} LongInt {$ELSE} Integer {$ENDIF}): Boolean;
Var
  I, C      : Integer;
  LastInher : Boolean;
  PS        : PString;

Begin
  Search := False;
  If Key = Nil Then Exit;

  If Count = 0 Then
  Begin
    Inherited Search (Key, Index);
    Exit;
  End;

  For i := 0 To Count-1 do
  begin
    LastInher := False;
    PS := GetUserName (Items^[i]);
    C := Compare(PS, Key);
    DisposeStr(PS);

    if C = 0 then
    Begin
      if I <> CurrentUser then
      begin
        Search := True;
        Break;
      end;
    End Else
    Begin
      Inherited Search (Key, Index);
      LastInher := True;
    End;
  end;
  If Not LastInher Then Index := i;
end;
*)

Function TMyUserCollection. Compare (Key1, Key2 : Pointer):
  {$IFNDEF VIRTUALPASCAL} Integer; {$ELSE} LongInt; {$ENDIF}
Begin
  If Assigned (CompareAddr)
  Then Compare := CompareAddr (Key1, Key2)
  Else Compare := -1;
End;

Function TMyFileCollection. Compare (Key1, Key2 : Pointer):
  {$IFNDEF VIRTUALPASCAL} Integer; {$ELSE} LongInt; {$ENDIF}
Var
  i : ShortInt;

Begin
  I := -1;
  If PFile (Key1)^. Name = PFile (Key2)^. Name Then i := 0;
  If PFile (Key1)^. Name > PFile (Key2)^. Name Then i := 1;
  Compare := i;
End;

Procedure TMyUserCollection. AtPut (
  Index: {$IFNDEF VIRTUALPASCAL} Integer; {$ELSE} LongInt; {$ENDIF} Item: Pointer);
Begin
  DisposePUser (At (Index));
  Inherited AtPut (Index, Item);
End;

Procedure TMyUserCollection. FreeItem (Item: Pointer);
Begin
  DisposePUser(Item);
End;

Procedure TMyApp. InitMenuBar;
Var
  R: TRect;

Begin
  MenuBar := Nil;
End;

Procedure TMyApp. InitStatusLine;
Var
  R: TRect;

Begin
  R. Assign (0, Hi (WindMax), 80, Hi (WindMax)+1);
  StatusLine := New (PStatusLine, Init (R, NewStatusDef (0, 6,
                NewStatusKey ('~Alt-X~ Exit',     kbAltX, cmQuit,
                NewStatusKey ('~F3~ Insert user', kbF3,   cmInsertUser,
                NewStatusKey ('~F7~ Search user', kbF7,   cmSearchUser,
                NewStatusKey ('~F8~ Delete user', kbF8,   cmDeleteUser,
                NewStatusKey ('~F9~ Sort',        kbF9,   cmChangeSorting,
             Nil))))), Nil)));
End;

Procedure TMyApp. InitDeskTop;
Var
  R: TRect;
begin
  GetExtent(R);
  Dec (R.B.Y);
  Desktop := New (PDesktop, Init (R));
  DeskTop^. BackGround^. Free;
  DeskTop^. Background := New (PWinBackground, Init (R, ' '));
  DeskTop^. Insert (DeskTop^.BackGround);
end;

Procedure TMyApp. Run;
Var
  V : String;
  i : Byte;

Begin
  R. Assign (0, 0, 80, 1);

  (*
  i := Pos ('/', Copy (NameVer, 11, 255));
  If i > 0 Then V := Copy (NameVer, 1, 9+i) Else V := NameVer;

  DeskTop^. Insert (New (PStaticText, Init
    (R, ' User Editor for ' +
     V + ' by Vlad Bakaev and Konstantin Klyagin')));
  *)

  DeskTop^. Insert (New (PStaticText, Init
    (R, ' User Editor for Tornado BBS by Vlad Bakaev and Konstantin Klyagin')));

  R. Assign (15, 2, 65, Hi (WindMax)-2);
  W := New (PDialog, Init (R, ''));

  With W^ Do
  Begin
    R. Assign (2, 0, 27, 1);
    Index := New (PMyStaticText, Init (R, 'User list'));
    Insert (Index);
    R. Assign (3, 1, 7, 2);
    Insert (New (PStaticText, Init (R, 'Name')));
    R. Assign (41, 1, 45, 2);
    Insert (New (PStaticText, Init (R, 'Sec.')));
    R. Assign (1, 2, 49, 3);
    Insert (New (PStaticText, Init (R, Replicate('─', 48))));
    R. Assign (49, 3, 50, Hi (WindMax)-5);
    B := New (PScrollBar, Init (R));
    Insert (B);
    R. Assign (2, 3, 49, Hi (WindMax)-5);
    PL := New (PUserListBox, Init (R, 1, B));
    PL^. NewList (L);
    Insert (PL);
  End;
  Desktop^. Insert (W);
  if L^.Count=0 then DisableCommands(UserCommandSet);
  TApplication. Run;
End;

Destructor TMyApp. Done;

Procedure ChangeParam (Var Str: String; NewValue: String);
Var
  Comment : String;
  ComPos  : Byte;
Begin
  ComPos  := Pos (';', Str);
  Comment := Copy (Str, ComPos, 255);
  Str := ExtractWord (1, Str, [' ']) + ' ' + NewValue;
  if ComPos<=Length(Str) then ComPos := Length(Str)+1;
  Str := PadCh (Str, ' ', ComPos-1) + Comment;
End;

Var TFF : Text;

Begin
  if L^.CTLSorting<>L^.SortMethod then
  begin
    Assign(FF, CTLFileName);
    {$I-}
    Reset(FF);
    {$I+}
    if IOResult<>0 then
    begin
      Rewrite(FF);
      WriteLn(FF, ';');
      WriteLn(FF, '; Файл конфигурации программы редактирования пользователей.');
      WriteLn(FF, ';');
      WriteLn(FF, 'Sort None         ; Критерий сортировки списка пользователей в редакторе.');
      WriteLn(FF, '                  ; Допустимые значения этого параметра:');
      WriteLn(FF, '                  ; None,');
      WriteLn(FF, '                  ; Name,');
      WriteLn(FF, '                  ; Security,');
      WriteLn(FF, '                  ; BirthDate,');
      WriteLn(FF, '                  ; UploadsK,');
      WriteLn(FF, '                  ; DownloadsK');
      Close(FF);
      Reset(FF);
    end;

    Assign(TFF, CTLTmpFileName);
    Rewrite(TFF);
    while not EOF(FF) do
    begin
      ReadLn(FF, TempS);
      if UpString(Copy(Trim(TempS), 1, 4))='SORT' then
       ChangeParam(TempS, SortTypeStr[L^.SortMethod]);
      WriteLn(TFF, TempS);
    end;
    Close(TFF);
    Close(FF);
    Erase(FF);
    Rename(TFF, CTLFileName);
  end;
  Inherited Done;
End;


Procedure TUserListBox. HandleEvent (Var Event: TEvent);
Var
  I, J: Word;
  TempS: String;

Function GetName (TUser: TrimUsersRecord): String;
Begin
  GetName := UpString (TUser. Name)
End;

Begin
  If Event. What = evCommand Then
   If Event. Command = cmQuit Then
    If L^.Count = 0            Then
    begin
      L^.Insert(NewPUser (FUser, 0));
    end;

  if L^.Count <> 0 then
   If ((Event. What And evMouseDown = evMouseDown) and Event. Double)
    or ((Event. What And evKeyDown = evKeyDown)
     and (CtrlToArrow (Event. KeyCode)= kbEnter)) Then
      Begin
        I := PUser(L^. At(Focused))^.Index;
        ReadUser(I);
        L^. CurrentUser := Focused;
        If EditUserTV (FUser) Then
        Begin
          L^. AtPut (Focused, NewPUser (FUser, I) );
          DrawView;
          SaveUser (I, smNormal);
        End;
        ClearEvent(Event);
      End;

  Inherited HandleEvent (Event);

  if Focused<0 then Focused := 0;
  Count := Range;
  CurPos := Focused;
  If Index <> Nil Then Index^. Draw;

  Case Event. What Of
    evKeyDown :
                Begin
                  If Event. KeyCode = kbEsc Then
                  begin
                    Event.What := evCommand;
                    Event.Command := cmQuit;
                    Event.InfoPtr := nil;
                    PutEvent(Event);
                    ClearEvent(Event);
                  end
                  Else Exit;
                End;
    evCommand :
                Case Event. Command Of
                     cmInsertUser :
                                     Begin
                                       FillChar (FUser, SizeOf (tUser), #0);

                                       FUser. FirstDate := DateL;
                                       FUser. LastDate := FUser. FirstDate;
                                       FUser. BirthDate := Date2Long (ReFormatDate ('01-01-1970', 'DD-MM-YYYY',
                                         DefaultDateMask));
                                       FUser. TimeUsedToday := 0;
                                       FUser. Security := 1;
                                       FUser. Lines := 24;
                                       FUser. Hotkeys := True;
                                       FUser. Frames := False;
                                       FUser. FSEditor := True;
                                       FUser. Protocol := '7';
                                       FUser. More := True;
                                       FUser. Emu := teAnsi;
                                       FUser. ReReadLimit := True;
                                       L^. CurrentUser := -1;
                                       If EditUserTV (FUser) Then
                                       Begin
                                         if L^.Count=0 then EnableCommands(UserCommandSet);
                                         SaveUser(L^.Count, smInsert);
                                         L^.Insert(NewPUser(FUser, L^.Count));
                                         SetRange (L^.Count);
                                         Inc(Count);
                                         I := 0;
                                         While (I < L^.Count) And
                                           (UpString(FUser.Name) <> GetName(TrimUsersRecord
                                           (L^.At(I)^)))
                                         Do
                                           Inc (I);
                                         FocusItem(I);
                                         Index^.Draw;
                                       End;
                                       ClearEvent(Event);
                                       DrawView;
                                     End;
                     cmDeleteUser :
                                     if L^.Count <> 0 then
                                     Begin
                                       If ConfirmDelete('Delete user',
                                         PUser(L^.At(Focused))^.Name + '?') Then
                                       Begin
                                         J := PUser(L^.At(Focused))^.Index;
                                         SaveUser(J, smDelete);
                                         L^.AtFree(Focused);
                                         SetRange(L^.Count);
                                         Dec (Count);

                                         If L^.Count > 0 Then
                                         for I := 0 to L^.Count-1 do
                                           if PUser(L^.At(I))^.Index>J then
                                             Dec(PUser(L^.At(I))^.Index);

                                         if L^.Count=0 then DisableCommands(UserCommandSet);
                                         if Focused=Range then Dec(Focused);
                                         Index^.Draw;
                                       End;
                                       ClearEvent(Event);
                                       DrawView;
                                     End;
                     cmSearchUser :
                                     Begin
                                       If InputNameBox('Input user name or part of it',
                                                        SS) = cmOk Then
                                       Begin
                                         if LSN <> SS then I := 0
                                                      else I := Focused+1;
                                         While (I < L^.Count) And
                                           (Pos(UpString(SS), GetName(TrimUsersRecord(L^.At(I)^)))=0)
                                         Do
                                           Inc(I);
                                         If I = L^.Count Then
                                         Begin
                                           I := MessageBox(Replicate(' ',5)+'Search string not found',
                                           nil, mfInformation+mfOKButton);
                                           Exit;
                                         End;
                                         CurPos := I;
                                         FocusItem(I);
                                         Index^.Draw;
                                         LSN := SS;
                                       End;
                                       ClearEvent(Event);
                                       DrawView;
                                     End;
                     cmChangeSorting:
                                     Begin
                                       ChangeSortCriteria;
                                       ClearEvent(Event);
                                       DrawView;
                                     End;

                   End; { case Event.Command of ... }
  End; { case Event.What of ... }
End;

Procedure TFileListBox.HandleEvent (Var Event: TEvent);
Var
  I, J    : Word;
  TS      : String;
  PF      : PFile;
  F       : FilesRecord;
  DirInfo : SearchRec;
  PFD     : PFileDialog;
  PD      : PDialog;
  R       : TRect;
  FL      : PMyFileCollection;
  PFL     : PFileListBox;

Begin
  Case Event.What Of
    evKeyDown :
                Case Event.KeyCode Of
           {Space}   14624   : if TagVisible then
                               Begin
                                 if FLTag^.Count>0 then
                                  Begin
                                   PFile(FLTag^. At(PFLTag^.Focused))^.Marked :=
                                   not PFile(FLTag^. At(PFLTag^.Focused))^.Marked;
                                   F := PFile(FLTag^. At(PFLTag^.Focused))^;
                                   DrawView;
                                   ClearEvent(Event);
                                 End;
                               End
                               else
                               Begin
                                 if FLPvt^.Count>0 then
                                  Begin
                                   PFile(FLPvt^. At(PFLPvt^.Focused))^.Marked :=
                                   not PFile(FLPvt^. At(PFLPvt^.Focused))^.Marked;
                                   F := PFile(FLPvt^. At(PFLPvt^.Focused))^;
                                   DrawView;
                                   ClearEvent(Event);
                                 End;
                               End;
                     kbShiftTab,
                     kbTab   : Begin
                                 Event.What := evCommand;
                                 if TagVisible then Event.Command := cmPvt
                                               else Event.Command := cmTag;
                                 Event.InfoPtr := nil;
                                 PutEvent(Event);
                                 ClearEvent(Event);
                               End;
                     kbRight : if TagVisible then
                               Begin
                                 Event.What := evCommand;
                                 Event.Command := cmPvt;
                                 Event.InfoPtr := nil;
                                 PutEvent(Event);
                                 ClearEvent(Event);
                               End else ClearEvent(Event);
                     kbLeft  : if not TagVisible then
                               Begin
                                 Event.What := evCommand;
                                 Event.Command := cmTag;
                                 Event.InfoPtr := nil;
                                 PutEvent(Event);
                                 ClearEvent(Event);
                               End else ClearEvent(Event);
                     kbEnter : Begin
                                 Event.What := evCommand;
                                 Event.Command := cmOk;
                                 Event.InfoPtr := nil;
                                 PutEvent(Event);
                                 ClearEvent(Event);
                                 Exit;
                               End;
                     kbIns   : Begin
                                 Event.What := evCommand;
                                 Event.Command := cmAddFile;
                                 Event.InfoPtr := nil;
                                 PutEvent(Event);
                                 ClearEvent(Event);
                               End;
                     kbDel   : Begin
                                 Event.What := evCommand;
                                 Event.Command := cmDelFile;
                                 Event.InfoPtr := nil;
                                 PutEvent(Event);
                                 ClearEvent(Event);
                               End;

                End;
    evBroadcast :              if Event.Command = 60 then ClearEvent(Event);
    evCommand   :
                Case Event.Command Of
                     cmOk      : if FLTag^.Count=0 then
                                 Begin
                                   F.Name := NoFile;
                                   F.Time := 0;
                                   F.Size := 0;
                                   FLTag^.Insert(NewPFile(F));
                                   PFLTag^.SetRange(FLTag^.Count);
                                   DrawView;
                                 End;
                     cmPvt     : if TagVisible then
                                 begin
                                   R.Assign(151, 3, 152, 13); BTag^.ChangeBounds(R);
                                   R.Assign( 71, 3,  72, 13); BPvt^.ChangeBounds(R);
                                   R.Assign( 83, 3, 151, 13); PFLTag^.ChangeBounds(R);
                                   R.Assign(  3, 3,  71, 13); PFLPvt^.ChangeBounds(R);
                                   R.Assign( 83, 1, 151,  2); StTag^.ChangeBounds(R);
                                   R.Assign(  3, 1,  71,  2); StPvt^.ChangeBounds(R);
                                   if FLPvt^.Count=0 then DisableCommands(FileCommandSet)
                                                     else EnableCommands (FileCommandSet);
                                   PDialog(Owner)^.FocusNext(False);
                                   DisposeStr(PDialog(Owner)^.Title);
                                   TagVisible := not TagVisible;
                                   PDialog(Owner)^.Title :=
                                     NewStr(TitleArray[TagVisible]+Title+FUser.Name);
                                   PDialog(Owner)^.ReDraw;
                                   DisableCommands([cmPvt]);
                                   EnableCommands ([cmTag]);
                                   DrawView;
                                   ClearEvent(Event);
                                 end;
                     cmTag     : if not TagVisible then
                                 begin
                                   R.Assign( 71, 3,  72, 13); BTag^.ChangeBounds(R);
                                   R.Assign(151, 3, 152, 13); BPvt^.ChangeBounds(R);
                                   R.Assign(  3, 3,  71, 13); PFLTag^.ChangeBounds(R);
                                   R.Assign( 83, 3, 151, 13); PFLPvt^.ChangeBounds(R);
                                   R.Assign(  3, 1,  71,  2); StTag^.ChangeBounds(R);
                                   R.Assign( 83, 1, 151,  2); StPvt^.ChangeBounds(R);
                                   if FLTag^.Count=0 then DisableCommands(FileCommandSet)
                                                     else EnableCommands (FileCommandSet);
                                   PDialog(Owner)^.FocusNext(True);
                                   DisposeStr(PDialog(Owner)^.Title);
                                   TagVisible := not TagVisible;
                                   PDialog(Owner)^.Title :=
                                     NewStr(TitleArray[TagVisible]+Title+FUser.Name);
                                   PDialog(Owner)^.ReDraw;
                                   DisableCommands([cmTag]);
                                   EnableCommands ([cmPvt]);
                                   DrawView;
                                   ClearEvent(Event);
                                 end;
                     cmAddFile :
                                 Begin
                                   F.Name := '';
                                   PFD := New(PFileDialog, Init(AllFilesMask, 'Select file:',
                                              'Filename:', fdOKButton, 0));

                                   I := Desktop^.ExecView (PFD);
                                   If (I = cmOk) or (I=800) Then
                                   Begin
                                     FillChar(F, SizeOF(F), #0);
                                     F.Name := PFD^.Directory^ + PFD^.FileName^.Data^;
                                     Dispose (PFD, Done);
                                     FindFirst(F.Name, AnyFile-VolumeId-Directory, DirInfo);
                                     if DOSError=0 then
                                     begin
                                       F.Size := DirInfo.Size;
                                       F.Time := DirInfo.Time;
                                     end;
                                     F.Marked := False;
                                     {$IFDEF VIRTUALPASCAL}
                                     FindClose (DirInfo);
                                     {$ENDIF}
                                     if not TagVisible then
                                     begin
                                       TS := '';
                                       If InputNameBox('Input user name', TS) = cmOk Then
                                       begin
                                         F.Owned := True;
                                         F.Owner := TS;
                                       end else
                                         Exit;
                                     end;
                                     if TagVisible then
                                     begin
                                       if FLTag^.Count=0 then
                                       begin
                                         EnableCommands(FileCommandSet);
                                         PFLTag^.Focused := 0;
                                       end;
                                       FLTag^.AtInsert(PFLTag^.Focused, NewPFile(F));
                                       PFLTag^.SetRange(FLTag^.Count);
                                     end else
                                     begin { not TagVisible }
                                       if FLPvt^.Count=0 then
                                       begin
                                         EnableCommands(FileCommandSet);
                                         PFLPvt^.Focused := 0;
                                       end;
                                       FLPvt^.AtInsert(PFLPvt^.Focused, NewPFile(F));
                                       PFLPvt^.SetRange(FLPvt^.Count);
                                     end;
                                     DrawView;
                                   End;
                                   ClearEvent(Event);
                                 End;
                     cmDelFile : begin
                                   if TagVisible then begin FL := FLTag; PFL := PFLTag; end
                                                 else begin FL := FLPvt; PFL := PFLPvt; end;
                                   if FL^.Count <> 0 then
                                   Begin
                                     J := 0;
                                     for I:= 0 to FL^.Count-1 do
                                       if PFile(FL^.At(I))^.Marked then
                                         Inc(J);
                                     I := 0;
                                     if J>0 then
                                     begin
                                       If ConfirmDelete('Delete selected file(s) from list?', ' ') Then
                                         while I<=FL^.Count-1 do
                                           if PFile(FL^. At(I))^.Marked then
                                           begin
                                             DisposePFile(FL^.At(I));
                                             FL^.AtDelete(I);
                                             PFL^.SetRange(FL^.Count);
                                             if (PFL^.Focused=PFL^.Range) And (PFL^.Focused>0) then
                                               Dec(PFL^.Focused);
                                             if FL^.Count=0 then DisableCommands(FileCommandSet);
                                           end else
                                             Inc(I);
                                     end else
                                     If ConfirmDelete('Delete this file from list',
                                                       PFile(FL^.At(PFL^.Focused))^.
                                                       Name+'?') Then
                                     Begin
                                       DisposePFile(FL^.At(PFL^.Focused));
                                       FL^.AtDelete(PFL^.Focused);
                                       PFL^.SetRange(FL^.Count);
                                       if (PFL^.Focused=PFL^.Range) And (PFL^.Focused>0) then
                                         Dec (PFL^.Focused);
                                       if FL^.Count=0 then DisableCommands(FileCommandSet);
                                     End;
                                     ClearEvent(Event);
                                     DrawView;
                                   End;
                                 end;
                   End; { case Event.Command of ... }
  End; { case Event.What of ... }

  Inherited HandleEvent (Event);

End;



Function TUserListBox. GetText (
  Item: {$IFNDEF VIRTUALPASCAL} Integer; {$ELSE} LongInt; {$ENDIF}
  MaxLen: {$IFNDEF VIRTUALPASCAL} Integer {$ELSE} LongInt {$ENDIF}): String;

Var
  U : TrimUsersRecord;
  S : String;

Begin
  S := '';
  If List <> Nil Then
  Begin
    U := PUser (List^. At (Item) )^;
    Str (U. Security, TempS);
    S := PadCh (U. Name, ' ', 36) + ' ' + LeftPadCh (TempS, ' ', 6);
  End;
  GetText := S;
End;

Function TFileListBox. GetText (
  Item: {$IFNDEF VIRTUALPASCAL} Integer; {$ELSE} LongInt; {$ENDIF}
  MaxLen: {$IFNDEF VIRTUALPASCAL} Integer {$ELSE} LongInt {$ENDIF}): String;

Var
  F : FilesRecord;
  S,
  S1: String;
  DT: DateTime;
Begin
  S := '';
  if List <> Nil Then
  Begin
    FillChar(F, SizeOf(F), #0);
    F := PFile (List^. At (Item) )^;
    if F.Time+F.Size<>0 then
    begin
      UnpackTime(F.Time, DT);
      Str(DT.Day,   S);   S := LeftPadCh(S, '0', 2);
      Str(DT.Month, S1);  S := S + '.' + LeftPadCh(S1, '0', 2);
      Str(DT.Year,  S1);  S := S + '.' + S1;
      Str (F. Size, TempS);
      if F.Marked then S1 := '√ ' else S1 := '  ';
      if F.Owned then
      begin
        S := S1 + PadCh(PrString(F. Owner), ' ', 29) +
             PadCh(JustFileName(F. Name), ' ', 13) + S +
             ' ' + LeftPadCh (TempS, ' ', 9);
      end
      else
      begin
        S := S1 + PadCh(NiceFileName(F. Name, 41), ' ', 42) + S +
             ' ' + LeftPadCh (TempS, ' ', 9);
      end;
    end
    else
    begin
      S := '  ' + PadCh(NiceFileName(F. Name, 37),' ',38) + ' ' + LeftPadCh (NotFoundStr, ' ', 24);
    end;
  End;
  GetText := S;
End;

Function TUserListBox. Valid;
Begin
  Foc := PString (L^. At (Focused) )^;
  Valid := True;
End;

Function TFileListBox. Valid;
Begin
  Foc := PString (FLTag^. At (Focused) )^;
  Valid := True;
End;

Procedure TMyStaticText. Draw;
Var
  Color: Byte;
  Center: Boolean;
  I, J, L, P, Y: Integer;
  B, B1: TDrawBuffer;
  S: String;

Begin
  Color := GetColor (1);
  GetText (S);
  S := ' ' + S;
  L := Length (S);
  P := 1;
  Y := 0;
  Center := False;
  While Y < Size. Y Do
  Begin
    MoveChar (B, ' ', Color, Size. X);
    If P <= L Then
    Begin
      If S [P] = #3 Then
      Begin
        Center := True;
        Inc (P);
      End;
      I := P;
      Repeat
        J := P;
        While (P <= L) And (S [P] = ' ') Do Inc (P);
        While (P <= L) And (S [P] <> ' ') And (S [P] <> #13) Do Inc (P);
      Until (P > L) Or (P >= I + Size. X) Or (S [P] = #13);
      If P > I + Size. X Then
        If J > I Then P := J Else P := I + Size. X;
      If Center Then J := (Size. X - P + I) Div 2 Else J := 0;
      MoveBuf (B [J], S [I], Color, P - I);
      While (P <= L) And (S [P] = ' ') Do Inc (P);
      If (P <= L) And (S [P] = #13) Then
      Begin
        Center := False;
        Inc (P);
        If (P <= L) And (S [P] = #10) Then Inc (P);
      End;
    End;

    if Count=0 then S := '0'
               else Str (CurPos + 1, S);
    S := '[' + S + '/';
    J := Length (S);
    For I := 1 To J Do
      MoveChar (B [L + I], S [I], Color, P - I);
    Inc (L, J); Inc (P, J);
    Str (Count, S);
    S := S + '] ';
    Size. X := Length (S)+L+1;
    J := Length (S);
    For I := 1 To J Do
      MoveChar (B [L + I], S [I], Color, P - I);
    Inc (L, J); Inc (P, J);
    W^.Frame^.Draw;
    WriteLine (0, Y, Size. X, 1, B);
    Inc (Y);
  End;
End;

Function TMyStaticText. GetPalette: PPalette;
Const
  P: String [Length (CStaticText) ] = CStaticText;
Begin
  P [1] := #$02;
  GetPalette := @P;
End;

Function TDisStaticText. GetPalette: PPalette;
Const
  P: String [Length (CStaticText) ] = CStaticText;
Begin
  P [1] := #$04;
  GetPalette := @P;
End;

Function TMyValidator. IsValid(const S: string): Boolean;
var
  Index: {$IFDEF VIRTUALPASCAL} LongInt {$ELSE} Integer {$ENDIF};
  Str: PString;
begin
  if Pic^<>CheckNamePic then IsValid := inherited IsValid(S)
  else
  begin
    Str := NewStr (S);
    IsValid := Not L^. IsUser (Str, Index);
    DisposeStr (Str);
  end;
end;

Procedure TMyValidator. Error;
var S:String;
begin
  if Pic^= OnlyNumPic     then
   S := 'Enter numeric only data in this field'
                          else
  if Pic^= CheckNamePic then
   S := '  This name is already used.'#13#10'  Try another one...'
                          else
  if Pic^= DateOnlyPic    then
   S := 'Enter date only in this field'
                          else
  if Pic^= AllUpperPic    then
   S := 'Only upper-case chars allowed in this field'
                          else
   S := 'Input does not conform to picture:'#13#10' %s';
  MessageBox(S, @Pic, mfError + mfOKButton);
end;

procedure TMyRadioButtons.Draw;
const
  Button = ' ( ) ';
begin
  DrawMultiBox(Button, #32#7);
end;

function TMyRadioButtons.Column(Item: Integer): Integer;
var
  I, Col, Width, L: Integer;
begin
  if Item < Size.Y then Column := 0
  else
  begin
    Width := 0;
    Col := -6;
    for I := 0 to Item do
    begin
      if I mod Size.Y = 0 then
      begin
        Inc(Col, Width + 6);
        Width := 0;
      end;
      if I < Strings.Count then
        L := CStrLen(PString(Strings.At(I))^);
      if L > Width then Width := L;
    end;
    Column := Col;
  end;
end;

function TMyRadioButtons.Row(Item: Integer): Integer;
begin
  Row := Item mod Size.Y;
end;

procedure TMyRadioButtons.DrawMultiBox(const Icon, Marker: String);
var
  I,J,Cur,Col: Integer;
  CNorm, CSel, CDis, Color: Word;
  B: TDrawBuffer;
  SCOff: Byte;
begin
  CNorm := GetColor($0301);
  CSel := GetColor($0402);
  CDis := GetColor($0505);
  for I := 0 to Size.Y do
  begin
    MoveChar(B, ' ', Byte(CNorm), Size.X);
    for J := 0 to (Strings.Count - 1) div Size.Y + 1 do
    begin
      Cur := J*Size.Y + I;
      if Cur < Strings.Count then
      begin
        Col := Column(Cur);
        if Col<>0 then Dec (Col);
        if (Col + CStrLen(PString(Strings.At(Cur))^) + 5 <
          Sizeof(TDrawBuffer) div SizeOf(Word)) and (Col < Size.X) then
        begin
          if not ButtonState(Cur) then
            Color := CDis
          else if (Cur = Sel) and (State and sfFocused <> 0) then
            Color := CSel
          else
            Color := CNorm;
          MoveChar(B[Col], ' ', Byte(Color), Size.X - Col);
          MoveStr(B[Col], Icon, Byte(Color));
          WordRec(B[Col+2]).Lo := Byte(Marker[MultiMark(Cur) + 1]);
          MoveCStr(B[Col+5], PString(Strings.At(Cur))^, Color);
          if ShowMarkers and (State and sfFocused <> 0) and (Cur = Sel) then
          begin
            WordRec(B[Col]).Lo := Byte(SpecialChars[0]);
            WordRec(B[Column(Cur+Size.Y)-1]).Lo := Byte(SpecialChars[1]);
          end;
        end;
      end;
    end;
    WriteBuf(0, I, Size.X, 1, B);
  end;
  SetCursor(Column(Sel)+2,Row(Sel));
end;
(****--------------- end TVision ----------------------------------****)

Procedure InitUserIndex;
Var I: Word;
Begin
  {$I-}
  Assign (F,  UsersBaseFileName);
  Reset (F);

  If IOResult <> 0 Then
  Begin
    ReWrite(F);
    Close(F);
    Reset(F);
  End;

  I := 0;
  While Not EoF (F) And (IOResult = 0) Do
  Begin
    Read (F, FUser);
    L^. Insert (NewPUser (FUser, I));
    Inc(I);
  End;
  Close (F);
  {$I+}
End;

Var
  i: Byte;

Begin
  FileMode := $40;

  UsersBaseFileName := DefaultName (UsersBaseFileName, 'tor', JustPathName (ParamStr (0)));
  UsersBaseTmpFileName := DefaultName (UsersBaseTmpFileName, 'tmp', JustPathName (ParamStr (0)));

  If ParamCount > 0 Then
  Begin
    For i := 1 To ParamCount Do
    Begin
      SS := UpString (Copy (ParamStr (i), 1, 2));

      If (SS = '/?') or (SS = '/H') or (SS = '-H') Then
      Begin
        WriteLn;
        WriteLn ('User editor program for Tornado');
        WriteLn ('Usage: USEREDIT.EXE [-t<UserTagPath>] [-b<Users.Tor_Path>]');
        WriteLn;
        Halt (1);
      End;

      If (SS='-T') or (SS='/T') then PathToUserTag := AddBackSlash (Copy(ParamStr(I), 3, 255));

      If (SS='-B') or (SS='/B') then
      Begin
        UsersBaseFileName := AddBackSlash (Copy(ParamStr(I), 3, 255)) + 'users.tor';
        UsersBaseTmpFileName := AddBackSlash (Copy(ParamStr(I), 3, 255)) + 'users.tmp';
      End;
    End;
  End;

  {$IFNDEF VIRTUALPASCAL}
  InitMulti;

  WX := WhereX;
  WY := WhereY;
  SaveWindow(1, 1, Lo(WindMax)+1, Hi(WindMax)+1, True, SaveScr);
  {$ENDIF}

  Index := Nil;
  LSN := '';
  {PlaySound(1000, 50);}
  New(L, Init(10, 10));
  L^.Duplicates := True;
  SS := '';

  if FileExists(CTLFileName) then
  begin
    Assign(FF, CTLFileName);
    {$I-}
    Reset(FF);
    {$I+}
    if IOResult=0 then
    begin
      while not EOF(FF) do
      begin
        ReadLn(FF, TempS);
        if UpString(Copy(Trim(TempS), 1, 4)) = 'SORT' then
        begin
          I := Pos(';', TempS);
          if I=0 then I := 255;
          TempS := Trim(Copy(UpString(TempS), 5, I-5));
          for I := Ord(None) to Ord(UploadsK) do
            if TempS = UpString(SortTypeStr[SortType(I)]) then
              case I of
                Ord(None)        : begin L^.CompareAddr := SortByNone;       L^.SortMethod := None;       end;
                Ord(Name)        : begin L^.CompareAddr := SortByName;       L^.SortMethod := Name;       end;
                Ord(BirthDate)   : begin L^.CompareAddr := SortByBirthDate;  L^.SortMethod := BirthDate;  end;
                Ord(LastDate)    : begin L^.CompareAddr := SortByLastDate;   L^.SortMethod := LastDate;   end;
                Ord(NoCalls)     : begin L^.CompareAddr := SortByNoCalls;    L^.SortMethod := NoCalls     end;
                Ord(Security)    : begin L^.CompareAddr := SortBySecurity;   L^.SortMethod := Security;   end;
                Ord(DownloadsK)  : begin L^.CompareAddr := SortByDownloadsK; L^.SortMethod := DownloadsK; end;
                Ord(UploadsK)    : begin L^.CompareAddr := SortByUploadsK;   L^.SortMethod := UploadsK;   end;
              end;
          L^.CTLSorting := L^.SortMethod;
          break;
        end;
      end;
      Close(FF);
    end;
  end;

  InitUserIndex;

  PS.Init;
  PS.Run;
  PS.Done;

  Dispose(L, Done);

  TextAttr := $07;
  WriteLn;

  {$IFNDEF VIRTUALPASCAL}
  RestoreWindow(1, 1, Lo(WindMax)+1, Hi(WindMax)+1, True, SaveScr);
  GoToXY(WX, WY);
  {$ENDIF}
End.
