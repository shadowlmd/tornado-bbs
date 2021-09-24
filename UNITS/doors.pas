{$IFDEF MSDOS}
  {$IFNDEF DPMI32}
    {$F+}
  {$IFNDEF DPMI}
    {$O+}
  {$ENDIF}
  {$ENDIF}
{$ENDIF}
{$I-}

Unit
  Doors;

Interface

Uses
  TGlob,
  tMisc,
  Crc,
  Users,
  MainComm;

Procedure DorinfoGen;
Procedure DoorSysGen;
Procedure ExitInfoGen;

Implementation

{$I INC\exitinfo.inc}

Procedure ExitInfoGen;
Const
  DateFormat = 'MM-DD-YY';
Var
  ei : ExitInfoRecord;
  F  : File Of ExitInfoRecord;

Begin
  FillChar (ei, SizeOf (ei), #0);
  ei. Baud := GetConnectSpeed;
  ei. SysInfo. TotalCalls := Sys. TotalCalls;
  ei. SysInfo. LastCaller := LC. Name;
  ei. TimeLogInfo. StartDate := '27-04-81';

  ei. UserInfo. Name := R. Name;
  ei. UserInfo. Location := R. Location;
  ei. UserInfo. Organisation := R. Organization;
  ei. UserInfo. Address1 := R. Address1;
  ei. UserInfo. Address2 := R. Address2;
  ei. UserInfo. Address3 := R. Address3;
  ei. UserInfo. Handle := R. Alias;
  ei. UserInfo. Comment := R. Comment;
  ei. UserInfo. PasswordCRC := Crc32Str (R. Password);
  ei. UserInfo. DataPhone := R. BPhone;
  ei. UserInfo. VoicePhone := R. HPhone;
  ei. UserInfo. LastTime := Word2Time (R. LastTime);
  ei. UserInfo. LastDate := Long2Date (R. LastDate, DateFormat);
  ei. UserInfo. Attribute := $02;
  If R. More Then
    Inc (ei. UserInfo. Attribute, $04);
  If R. Emu = teAnsi Then
    Inc (ei. UserInfo. Attribute, $08);
  If R. HotKeys Then
    Inc (ei. UserInfo. Attribute2, $02);
  If R. Guest Then
    Inc (ei. UserInfo. Attribute2, $64);
  ei. UserInfo. MsgsPosted := R. MsgsPosted;
  ei. UserInfo. Security := R. Security;
  ei. UserInfo. LastRead := R. LastRead;
  ei. UserInfo. NoCalls := R. NoCalls;
  ei. UserInfo. Uploads := R. Uploads;
  ei. UserInfo. Downloads := R. Downloads;
  ei. UserInfo. UploadsK := R. UploadsK;
  ei. UserInfo. DownloadsK := R. DownloadsK;
  ei. UserInfo. TodayK := R. TodayK;
  ei. UserInfo. Elapsed := Round (R. TimeUsedToday / 60);
  ei. UserInfo. ScreenLength := R. Lines;
  ei. UserInfo. FirstDate := Long2Date (R. FirstDate, DateFormat);
  ei. UserInfo. BirthDate := Long2Date (R. BirthDate, DateFormat);
  ei. UserInfo. SubDate := Long2Date (R. FirstDate, DateFormat); {?}
  ei. UserInfo. ScreenWidth := 80;
  ei. UserInfo. DateFormat := 0;
  ei. UserInfo. MsgArea := R. MsgArea;
  ei. UserInfo. FileArea := R. FileArea;
  ei. UserInfo. DefaultProtocol := 'Z';

  ei. EventInfo. Status := 2;
  ei. NetMailEntered := NetMailEntered;
  ei. EchoMailEntered := EchoMailEntered;

  ei. LoginTime := Copy (HowTime (EnterTime), 1, 5);
  ei. LoginDate := FormattedCurrDT (DateFormat);
  ei. TimeLimit := Lim. Time;
  ei. LoginSec := 0; {?}
  ei. UserRecord := 0;
  ei. DownLoadLimit := Lim. KBLimit;
  ei. TimeOfCreation := ShortStrTime;
  ei. LogonPasswordCRC := Crc32Str (R. Password);
  ei. WantChat := WantsChat;

  Assign (F, Cnf. DoorInfoDir + 'exitinfo.bbs');
  ReWrite (F);
  Write (F, ei);
  Close (F);
End;

Procedure DorinfoGen;
Var
  F       : Text;
  k       : Byte;
  FileBuf : FileBufArr;

Begin
  k := BBSline;
  If k = 0 Then
    k := 1;
  Assign (F, Cnf. DoorInfoDir + 'dorinfo' + Long2Str (k) + '.def');
  SetTextBuf (F, FileBuf, FileBufSize);
  ReWrite (F);

  WriteLn (F, UpString (Cnf. BBSName));
  WriteLn (F, UpString (ExtractWord (1, Cnf. SysOp, SpaceOnly)));
  WriteLn (F, UpString (ExtractWord (2, Cnf. SysOp, SpaceOnly)));

  If Local Then
    WriteLn (F, 'COM0')
  Else
    WriteLn (F, {$IFNDEF OS2} 'COM' + Long2Str (Cnf. ComPort)
                {$ELSE} Cnf. ComPort {$ENDIF});

  WriteLn (F, Long2Str (GetConnectSpeed) + ' BAUD,' + {$IFNDEF OS2}
    Cnf. Parity [1] + ',' + Long2Str (Cnf. DataBits) + ',' +
    Long2Str (Cnf. StopBits) {$ELSE} 'N,8,1' {$ENDIF});

  WriteLn (F, '0');
  WriteLn (F, UpString (ExtractWord (1, R. Name, SpaceOnly)));
  WriteLn (F, UpString (ExtractWord (2, R. Name, SpaceOnly)));
  WriteLn (F, UpString (R. Location));

  Case R. Emu Of
       teTty : WriteLn (F, '0');
      teAnsi : WriteLn (F, '1');
    teAvatar : WriteLn (F, '2');
  End;

  WriteLn (F, Long2Str (R. Security));
  WriteLn (F, Long2Str (Round ((R. TotalTime - TimeDiff (EnterTime, MidSec)) / 60)));
  Close (F);
End;

Procedure DoorSysGen;
Const
  DateFormat: String [10] = 'MM/DD/YYYY';

Var
  F       : Text;
  c       : Char;
  FileBuf : FileBufArr;

Begin
  Assign (F, Cnf. DoorInfoDir + 'door.sys');
  SetTextBuf (F, FileBuf, FileBufSize);
  ReWrite (F);

  If Local Then
    WriteLn (F, 'COM0:')
  Else
    WriteLn (F, {$IFNDEF OS2} 'COM' + Long2Str (Cnf. ComPort)
                {$ELSE} Cnf. ComPort {$ENDIF} + ':');

  WriteLn (F, Long2Str (GetConnectSpeed));
{$IFNDEF OS2}
  WriteLn (F, Cnf. Parity [1]);
{$ELSE}
  WriteLn (F, 'N');
{$ENDIF}

  If BbsLine = 0 Then WriteLn (F, '1')
                 Else WriteLn (F, Long2Str (BbsLine));
  WriteLn (F, Long2Str (Cnf. BaudRate));
  WriteLn (F, 'Y');
  WriteLn (F, 'N');
  If Cnf. Sound Then c := 'Y'
                Else c := 'N';
  WriteLn (F, c);
  WriteLn (F, c);
  WriteLn (F, UpString (R. Name));
  WriteLn (F, UpString (R. Location));
  WriteLn (F, R. HPhone);
  WriteLn (F, R. BPhone);
  WriteLn (F, R. Password);
  WriteLn (F, Long2Str (R. Security));
  WriteLn (F, Long2Str (R. NoCalls));
  WriteLn (F, Long2Date (R. LastDate, DateFormat));
  WriteLn (F, Long2Str (R. TotalTime - TimeDiff (EnterTime, MidSec)));
  WriteLn (F, Long2Str (Round ((R. TotalTime - TimeDiff (EnterTime, MidSec)) / 60)));
  If R. Emu = teTty Then WriteLn (F, 'NG')
                    Else WriteLn (F, 'GR');
  WriteLn (F, Long2Str (R. Lines));
  WriteLn (F, 'N');
  WriteLn (F);
  WriteLn (F);
  WriteLn (F);
  WriteLn (F, Long2Str (GetUserRecNum (R. Name, False)));
  WriteLn (F, ProtocolDef. Selection);
  WriteLn (F, Long2Str (R. Uploads));
  WriteLn (F, Long2Str (R. Downloads));
  WriteLn (F, '10');
  WriteLn (F, Long2Str (R. TodayK));
  WriteLn (F, Long2Date (R. BirthDate, DateFormat));
  WriteLn (F, Cnf. Path);
  WriteLn (F, Cnf. Path);
  WriteLn (F, Cnf. SysOp);
  WriteLn (F, R. Alias);
  WriteLn (F, '00:00');
  WriteLn (F, 'Y');
  WriteLn (F, 'N');
  WriteLn (F, 'N');
  WriteLn (F, '3');
  WriteLn (F, '0');
  WriteLn (F, Long2Date (R. LastDate, DateFormat));
  WriteLn (F, Copy (HowTime (EnterTime), 1, 5));
  WriteLn (F, Word2Time (R. LastTime));
  WriteLn (F, '999');
  WriteLn (F, Long2Str (R. DownLoads));
  WriteLn (F, Long2Str (R. UpLoadsK));
  WriteLn (F, Long2Str (R. DownLoadsK));
  WriteLn (F, R. Comment);
  WriteLn (F, '0');
  WriteLn (F, '0');
  Close (F);
End;

End.
