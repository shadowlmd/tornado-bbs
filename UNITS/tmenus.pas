{$IFDEF MSDOS}
  {$IFNDEF DPMI32}
    {$F+}
  {$IFNDEF DPMI}
    {$O+}
  {$ENDIF}
  {$ENDIF}
{$ENDIF}
{$I-}
{&Delphi+}

{*********************************************************}
{*                     TMENUS.PAS                        *}
{*                                                       *}
{*  Copyright (c) Vlad Bakaev, 1995-98,                  *}
{*  Copyright (c) Konstantin Klyagin, 1995-98,           *}
{*                exspecially for Tornado BBS System     *}
{*                                                       *}
{*********************************************************}

Unit tMenus;

Interface

Uses
{$IFDEF OS2}
  Os2Base,
{$ENDIF}
  DOS,
  Objects,
  tMisc,
  Log,
  tGlob,
  SysMsgs;

Const
  ActionsNum = 55;

  Actions : Array [1..ActionsNum] Of String [15] = (
  'Gosub_Menu',                { Name of menu       }
  'DownLoad',                  { Filename(s)        }
  'UpLoad',                    { Directory          }
  'Return',                    {                    }
  'LogOff',         {  5 }     {                    }
  'Change_FArea',              { Number of area     }
  'User_Info',                 { UserName           }
  'Msg_Post',                  { Number of area     }
  'FileList',                  { Number of area     }
  'ArcView',        { 10 }     { Name of archive    }
  'Display_File',              { Filename           }
  'Page_SysOp',                { Theme              }
  'Shell',                     {                    }
  'Change_Msg_Area',           { Number of area     }
  'Msg_Read',       { 15 }     { Number of area     }
  'Msg_List',                  { Number of area     }
  'Display_Text',              {                    }
  'Re_Login',                  { User name          }
  'System_Info',               {                    }
  'Goto_Menu',      { 20 }     { Name Of Menu       }
  'Exec',                      { Program Name       }
  'Scan_NewFiles',             {                    }
  'Scan_PrivMail',             {                    }
  'Send_Msg',                  {                    }
  'Set_Msg_Off',    { 25 }     {                    }
  'Set_Msg_On',                {                    }
  'Line_List',                 {                    }
  'Change_Param',              {                    }
  'Todays_Callers',            {                    }
  'News',           { 30 }     {                    }
  'Show_Raw_Dir',              {                    }
  'Type_File',                 { File Name          }
  'Global_Search',             { WildCard           }
  'Conference',                {                    }
  'Exec_Script',    { 35 }     { File Name          }
  'Next_MsgArea',              {                    }
  'Prev_MsgArea',              {                    }
  'Next_FileArea',             {                    }
  'Prev_FileArea',             {                    }
  'Change_FGroup',  { 40 }     { Group number       }
  'Change_MGroup',             { Group number       }
  'Next_FileGroup',            {                    }
  'Prev_FileGroup',            {                    }
  'Next_MsgGroup',             {                    }
  'Prev_MsgGroup',  { 45 }     {                    }
  'UpLoad_Priv',               { Username           }
  'HTML',                      { Filename           }
  'DownLoad_QWK',              {                    }
  'UpLoad_QWK',                {                    }
  'Select_QWK',     { 50 }     {                    }
  'Exec_Rexx',                 {                    }
  'Telnet',                    {                    }
  'Finger',                    {                    }
  'Search_By_Desc',            { WildCard           }
  'Msg_Search'                 { Masks              }
  );

  {Menu Commands}
  cGoTo = 20;
  cGosub = 1;
  cReturn = 4;

  {File Areas Commands}
  cChangeFileGroup = 40;
  cChangeFileArea = 6;
  cDownLoad = 2;
  cUpLoad = 3;
  cFileList = 9;
  cArcView = 10;
  cShowRawDir = 31;
  cTypeFile = 32;
  cGlobalSearch = 33;
  cSearchByDesc = 54;
  cNextFileArea = 38;
  cPrevFileArea = 39;
  cNextFileGroup = 42;
  cPrevFileGroup = 43;
  cUploadPriv = 46;

  {Message Areas Commands}
  cChangeMsgGroup = 41;
  cChangeMsgArea = 14;
  cReadMsgs = 15;
  cListMsgs = 16;
  cPostMsg = 8;
  cSearchMsgs = 55;
  cNextMsgArea = 36;
  cPrevMsgArea = 37;
  cNextMsgGroup = 44;
  cPrevMsgGroup = 45;
  cDownLoadQWK = 48;
  cUpLoadQWK = 49;
  cSelectQWK = 50;

  {File Displaying Commands}
  cDisplayFile = 11;

  {General System Commands}
  cUserInfo = 7;
  cPageSysOp = 12;
  cLogOff = 5;
  cShell = 13;
  cDisplayOnly = 17;
  cReLogin = 18;
  cVerInfo = 19;
  cExec = 21;
  cScan_NewFiles = 22;
  cScan_PrivMail = 23;
  cChangeParam = 28;
  cTodaysCallers = 29;
  cNews = 30;
  cExecScript = 35;
  cHTML = 47;
  cExecRexx = 51;
  cTelnet = 52;
  cFinger = 53;

  {MultiLine Commands}
  cSend_Msg = 24;
  cSet_Msg_Off = 25;
  cSet_Msg_On = 26;
  cLine_List = 27;
  cConference = 34;

Var
  MenuItems : PMenuItemsCollection;
  mHeader   : tMenuHeader;

Function UseMenu (Const MenuName: PathStr): Boolean;

Implementation

Uses
  RCache;

Function UseMenu (Const MenuName: PathStr): Boolean;
Var
  LineNumber   : Word;
  MenuFile     : Text;
  MenuFileName : PathStr;
  BufStr       : String;
  FileBuf      : FileBufArr;

  Function InitMenuParser (Const MenuName: PathStr): Boolean;
  Begin
    LineNumber := 0;

    Assign (MenuFile, MenuName);
    SetTextBuf (MenuFile, FileBuf, FileBufSize);
    ReSet (MenuFile);

    InitMenuParser := IOResult = 0;
  End;

  Function mReadLn (Var S : String): Boolean;
  Var
    P       : Byte;
    LineDef : String [20];

  Begin
    While Not EoF (MenuFile) Do
    Begin
      Inc (LineNumber);
      ReadLn (MenuFile, S);
      S := Trim (S);
      If S = '' Then
        Continue;

      Case S [1] Of

        ';' : Continue;

        '{' : Begin
                P := Pos ('}', S);
                If P < 3 Then
                  Continue;

                LineDef := DelChars ([#9, ' '], Copy (S, 2, P - 2));
                If Not InRange (BbsLine, LineDef) Then
                  Continue;

                S := TrimLead (Copy (S, P + 1, 255));
              End;
      End;

      mReadLn := True;
      Exit;
    End;

    S := '';
    mReadLn := False;
  End;

  Procedure GetNearestSection (NameSection: String);
  Begin
    NameSection := '[' + UpString (NameSection) + ']';

    Repeat
      If UpString (Copy (BufStr, 1, Length (NameSection))) = NameSection Then
        Exit;
    Until Not mReadLn (BufStr);
  End;

  Function Kill2SpaceInString (InStr: String): String;
  Var
    i, WC  : Integer;
  {$IFNDEF VirtualPascal}
    Result : String;
  {$ENDIF}

  Begin
    InStr := Trim (InStr);
    If InStr <> '' Then
    Begin
      WC := AsciiCount (InStr, SpaceOnly, '"');
      Result := ExtractAscii (1, InStr, SpaceOnly, '"');

      For i := 2 To WC Do
        Result := Result + ' ' + ExtractAscii (i, InStr, SpaceOnly, '"');

    {$IFNDEF VirtualPascal}
      Kill2SpaceInString := Result;
    {$ENDIF}
    End
    Else
      Kill2SpaceInString := '';
  End;

  Procedure ReadMenuHeader (Var MenuHeader: tMenuHeader);
  Const
    ArrowsDelims = [':', ',', ' '];

  Var
    P : Byte;
    S : String;

  Begin
    FillChar (MenuHeader, SizeOf (MenuHeader), 0);
    GetNearestSection ('Header');

    With MenuHeader Do
      While mReadLn (BufStr) Do
      Begin
        P := AsciiPosCh (BufStr, ';', '"');
        If P > 0 Then
          SetLength (BufStr, P - 1);
        BufStr := Kill2SpaceInString (PlaceSubStr (BufStr, #9,
          ReplaceTabSpaces));
        If BufStr = '' Then
          Continue;

        If BufStr [1] = '[' Then
          Break;

        S := UpString (ExtractWord (1, BufStr, SpaceOnly));
        Delete (BufStr, 1, Length (S) + 1);

        If S = 'DISPLAYFILE' Then DisplayFile := BufStr Else
        If S = 'ARROWS_SETUP' Then
        Begin
          ArrowHor := Str2Long (ExtractWord (1, BufStr, ArrowsDelims));
          ArrowVer := Str2Long (ExtractWord (2, BufStr, ArrowsDelims));
          StartY := Str2Long (ExtractWord (3, BufStr, ArrowsDelims));
          StartX := Str2Long (ExtractWord (4, BufStr, ArrowsDelims));
          SelectedColor := Color2Byte ('Gray/' + ExtractWord (5, BufStr,
            ArrowsDelims));
        End Else
        If S = 'PROMPT' Then
        Begin
          If BufStr [1] = '"' Then
          Begin
            Delete (BufStr, 1, 1);
            Prompt := Copy (BufStr, 1, Pos ('"', BufStr) - 1);
          End;
        End Else
        If S = 'WRITEHOTKEYS' Then WriteHotKeys := UpString (BufStr) = 'YES'
        Else
        If S = 'STARTITEM' Then StartItem := Str2Long (BufStr) - 1;
      End;

    GetNearestSection ('Menu');
  End;

  Procedure ReadMenuItem (Var MenuItem: tMenuItem);

    Procedure LogError;
    Begin
      LogWrite ('!', 'Line ' + Long2Str (LineNumber) + ' of file ' +
        NiceFileName (CompletePath (JustPathName (MenuFileName)) +
        JustFileName (MenuFileName), 50) + ' is invalid');
      FillChar (MenuItem, SizeOf (MenuItem), 0);
    End;

  Var
    Err : SysInt;
    P   : Byte;
    S   : String;

  Begin
    FillChar (MenuItem, SizeOf (MenuItem), 0);

    While mReadLn (BufStr) Do
    Begin
      P := AsciiPosCh (BufStr, ';', '"');
      If P > 0 Then
        SetLength (BufStr, P - 1);
      BufStr := Kill2SpaceInString (PlaceSubStr (BufStr, #9, ReplaceTabSpaces));
      If BufStr = '' Then
        Continue;

      S := UpString (ExtractWord (1, BufStr, SpaceOnly));
      Delete (BufStr, 1, Length (S) + 1);

      If S [Length (S)] = '+' Then
      Begin
        MenuItem. AutoExec := True;
        SetLength (S, Length (S) - 1);
      End;

      MenuItem. Action := 1;
      While (MenuItem. Action <= ActionsNum) And
            (UpString (Actions [MenuItem. Action]) <> S) Do
        Inc (MenuItem. Action);

      If MenuItem. Action > ActionsNum Then
      Begin
        LogError;
        Continue;
      End;

      If BufStr [1] = '"' Then
      Begin
        Delete (BufStr, 1, 1);
        P := Pos ('"', BufStr);
        If P = 0 Then
        Begin
          LogError;
          Continue;
        End;

        MenuItem. OptData := NewStr (Copy (BufStr, 1, P - 1));
        Delete (BufStr, 1, P + 1);
      End
      Else
        MenuItem. OptData := NewStr ('');

      P := Pos (' ', BufStr);
      S := Copy (BufStr, 1, P - 1);
      If ConsistsOf (S, ['A'..'Z', 'a'..'z']) Then
      Begin
        MenuItem. Flags := NewStr (S);
        Delete (BufStr, 1, P);
        P := Pos (' ', BufStr);
        S := Copy (BufStr, 1, P - 1);
      End
      Else
        MenuItem. Flags := NewStr ('');

      Val (S, MenuItem. Security, Err);
      Delete (BufStr, 1, P);

      If BufStr [1] = '"' Then
      Begin
        Delete (BufStr, 1, 1);
        P := Pos ('"', BufStr);
        If P = 0 Then
        Begin
          LogError;
          Continue;
        End;

        MenuItem. Display := NewStr (Copy (BufStr, 1, P - 1));
        Delete (BufStr, 1, P + 1);
      End
      Else
        MenuItem. Display := NewStr ('');

      If (BufStr <> '') And (MenuItem. Action <> cDisplayOnly) Then
      Begin
        MenuItem. HotKey := UpCase (BufStr [1]);
        If Length (BufStr) > 1 Then
        Begin
          S := UpString (BufStr);
          If S = 'KEYB_UP' Then MenuItem. HotKey := kbUp Else
          If S = 'KEYB_DOWN' Then MenuItem. HotKey := kbDown Else
          If S = 'KEYB_RIGHT' Then MenuItem. HotKey := kbRight Else
          If S = 'KEYB_LEFT' Then MenuItem. HotKey := kbLeft Else
          If S = 'KEYB_HOME' Then MenuItem. HotKey := kbHome Else
          If S = 'KEYB_END' Then MenuItem. HotKey := kbEnd;
        End;
      End;

      Exit;
    End;
  End;

  Procedure DoneMenuParser;
  Begin
    Close (MenuFile);
  End;

Type
  PItemRec = ^ItemRec;
  ItemRec = Record
    Action   : System. Integer;
    Security : System. Word;
    HotKey   : Char;
    AutoExec : Boolean;
  End;

Var
  P      : ^Byte;
  Block  : Pointer;
  RS     : PMemoryStream;
  i, Len : LongInt;
  Item   : tMenuItem;
  R_Item : ItemRec;

Begin
  MenuItems^. FreeAll;
  MenuFileName := AddBackSlash (lang (laMenus)) + MenuName + '.mnu';

  Block := GetBlockResource (MenuFileName, Len);
  If Block <> Nil Then
  Begin
    P := Block;
    Move (P^, mHeader, SizeOf (mHeader));
    Inc (P, SizeOf (mHeader));
    i := PLongInt (P)^;
    Inc (P, SizeOf (i));

    While i > 0 Do
    Begin
      With PItemRec (P)^ Do
      Begin
        Item. Action := Action;
        Item. Security := Security;
        Item. HotKey := HotKey;
        Item. AutoExec := AutoExec;
      End;
      Inc (P, SizeOf (ItemRec));
      Item. OptData := NewStr (PString (P)^);
      Inc (P, P^ + 1);
      Item. Flags := NewStr (PString (P)^);
      Inc (P, P^ + 1);
      Item. Display := NewStr (PString (P)^);
      Inc (P, P^ + 1);
      MenuItems^. Insert (NewMenuItem (Item));
      Dec (i);
    End;

    FreeMem (Block, Len);
  End Else
  Begin
    If Not InitMenuParser (MenuFileName) Then
      If MenuName <> 'main' Then
      Begin
        LogWrite ('!', sm (smErrorAccessToMenu) + MenuFileName);
        UseMenu := False;
        Exit;
      End Else
      Begin
        LogWrite ('!', sm (smErrorAccessToMain) + MenuFileName);
        Halt (204);
      End;

    RS := New (PMemoryStream, Init (0, 0));
    ReadMenuHeader (mHeader);
    RS^. Write (mHeader, SizeOf (mHeader));
    i := 0;
    RS^. Write (i, SizeOf (i));

    Repeat
      ReadMenuItem (Item);
      If Item. Action = 0 Then
        Break;

      MenuItems^. Insert (NewMenuItem (Item));
      With Item Do
      Begin
        R_Item. Action := Action;
        R_Item. Security := Security;
        R_Item. HotKey := HotKey;
        R_Item. AutoExec := AutoExec;
        RS^. Write (R_Item, SizeOf (R_Item));
        RS^. Write (OptData^, Length (OptData^) + 1);
        RS^. Write (Flags^, Length (Flags^) + 1);
        RS^. Write (Display^, Length (Display^) + 1);
      End;

      Inc (i);
    Until False;

    RS^. Seek (SizeOf (mHeader));
    RS^. Write (i, SizeOf (i));
    If RS^. Status = stOk Then
      PutStreamResource (MenuFileName, RS);
    Dispose (RS, Done);
    DoneMenuParser;
  End;

  UseMenu := True;
End;

End.
