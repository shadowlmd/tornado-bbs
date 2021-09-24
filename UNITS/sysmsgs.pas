{$F+}
Unit SysMsgs;

Interface

Uses
  TGlob,
  Objects,
  OpCrt,
  tMsgLib,
  DOS,
  tMisc;

{$I INC\language.inc}

Const
  MsgFileName : PathStr = 'tornado.msg';
  BoolMsg     : Array [Boolean] Of String [10] = ('', '');
  Letters     : Array ['A'..'P'] Of Byte =
                (pLocation, pOrganization, pAddress, pPassword, pHPhone,
                 pBPhone, pBirthDate, pLines, pLang, pProtocol, pEmu, pMore,
                 pHotKeys, pFrames, pAlias, pFSEditor);

Procedure ResRead (Const FileName: String);
Function sm (Index: Integer): String;

Var
  SysMsg : PBigCollection;

Implementation

Procedure ResRead (Const FileName: String);
Begin
  MsgFileName := FileName;

  If Not FileExists (MsgFileName) Then
  Begin
    TextAttr := 12;
    WriteLn ('! Tornado messages file open error');
    TextAttr := 7;
    WriteLn;
    ExitProc := SavedExitProc;
    Halt (210);
  End;

  SysMsg := New (PBigCollection, Init (Max_SM_Number, 16));
  If Not ReadCollection (MsgFileName, SysMsg^, 'Messages', NameVer, False) Then
  Begin
    TextAttr := 12;
    WriteLn ('! Tornado messages file read error or version mismatch');
    TextAttr := 7;
    WriteLn;
    ExitProc := SavedExitProc;
    Halt (210);
  End;

  ParamNames := New (PBigCollection, Init (MaxParamNamesNumber, 4));
  ReadCollection (MsgFileName, ParamNames^, 'Params', NameVer, False);

  BoolMsg [True] := sm (smYes);
  BoolMsg [False] := sm (smNo);

  sMonths [1] := sm (smJan);
  sMonths [2] := sm (smFeb);
  sMonths [3] := sm (smMar);
  sMonths [4] := sm (smApr);
  sMonths [5] := sm (smMay);
  sMonths [6] := sm (smJun);
  sMonths [7] := sm (smJul);
  sMonths [8] := sm (smAug);
  sMonths [9] := sm (smSep);
  sMonths [10] := sm (smOct);
  sMonths [11] := sm (smNov);
  sMonths [12] := sm (smDec);
End;

Function sm (Index: Integer): String;
Begin
  If Index <= SysMsg^. Count Then sm := PString (SysMsg^. At (Index-1))^
                             Else sm := '';
End;

End.
