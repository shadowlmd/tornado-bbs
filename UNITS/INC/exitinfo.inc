Type

  {exitinfo.bbs}

  raTime           = String [5];
  raDate           = String [8];
  raMSGTOIDXrecord = String [35];
  raFlagType       = array [1..4] of Byte;
  raCOMBINEDrecord = array [1..200] of System. Word;

  raSYSINFOrecord  = record
                     TotalCalls     : LongInt;
                     LastCaller     : raMSGTOIDXrecord;
                     ExtraSpace     : Array [1..128] of Byte;
                   end;

  raTIMELOGrecord  = record
                     StartDate      : raDate;
                     BusyPerHour    : array[0..23] of System. Word;
                     BusyPerDay     : array[0..6] of System. Word;
                   end;

  raUSERSrecord    = record
                     Name           : raMSGTOIDXrecord;
                     Location       : String[25];
                     Organisation,
                     Address1,
                     Address2,
                     Address3       : String[50];
                     Handle         : String[35];
                     Comment        : String[80];
                     PasswordCRC    : LongInt;
                     DataPhone,
                     VoicePhone     : String[15];
                     LastTime       : raTime;
                     LastDate       : raDate;

                     Attribute,

                      { Bit 0 : Deleted
                            1 : Clear screen
                            2 : More prompt
                            3 : ANSI
                            4 : No-kill
                            5 : Xfer priority
                            6 : Full screen msg editor
                            7 : Quiet mode }

                     Attribute2     : Byte;

                      { Bit 0 : Hot-keys
                            1 : AVT/0
                            2 : Full screen message viewer
                            3 : Hidden from userlist
                            4 : Page priority
                            5 : No echomail in mailbox scan
                            6 : Guest account
                            7 : Post bill enabled }

                     Flags          : raFlagType;
                     Credit,
                     Pending        : LongInt;
                     MsgsPosted,
                     Security       : System. Word;
                     LastRead,
                     NoCalls,
                     Uploads,
                     Downloads,
                     UploadsK,
                     DownloadsK,
                     TodayK         : LongInt;
                     Elapsed        : System. Integer;
                     ScreenLength   : System. Word;
                     LastPwdChange  : Byte;
                     Group          : System. Word;
                     CombinedInfo   : raCOMBINEDrecord;
                     FirstDate,
                     BirthDate,
                     SubDate        : raDate;
                     ScreenWidth,
                     Language,
                     DateFormat     : Byte;
                     ForwardTo      : String[35];
                     MsgArea,
                     FileArea       : System. Word;
                     DefaultProtocol: Char;
                     FileGroup      : System. Word;
                     LastDOBCheck   : Byte;
                     Sex            : Byte;
                     XIrecord       : LongInt;
                     MsgGroup       : System. Word;
                     FreeSpace      : Array[1..48] of Byte;
                   end;

  raEVENTrecord    = record
                     Status         : Byte; { 0=Deleted 1=Enabled 2=Disabled }
                     StartTime      : raTime;
                     ErrorLevel     : Byte;
                     Days           : Byte;
                     Forced         : Boolean;
                     LastTimeRun    : raDate;
                   end;

  raUSERSXIrecord  = record
                     FreeSpace      : Array [1..200] of Byte;
                   end;

  EXITINFOrecord = record
                     Baud             : System. Word;
                     SysInfo          : raSYSINFOrecord;
                     TimeLogInfo      : raTIMELOGrecord;
                     UserInfo         : raUSERSrecord;
                     EventInfo        : raEVENTrecord;
                     NetMailEntered,
                     EchoMailEntered  : Boolean;
                     LoginTime        : raTime;
                     LoginDate        : raDate;
                     TimeLimit        : System. Word;
                     LoginSec         : LongInt;
                     UserRecord       : System. Integer;
                     ReadThru,
                     NumberPages,
                     DownloadLimit    : System. Word;
                     TimeOfCreation   : raTime;
                     LogonPasswordCRC : LongInt;
                     WantChat         : Boolean;

                     DeductedTime     : System. Integer;
                     MenuStack        : Array [1..50] of String [8];
                     MenuStackPointer : Byte;
                     UserXIinfo       : raUSERSXIrecord;
                     ErrorFreeConnect,
                     SysopNext        : Boolean;

                     EMSI_Session     : Boolean;        { These fields hold  }
                     EMSI_Crtdef,                       { data related to an }
                     EMSI_Protocols,                    { EMSI session       }
                     EMSI_Capabilities,
                     EMSI_Requests,
                     EMSI_Software    : String[40];
                     Hold_Attr1,
                     Hold_Attr2,
                     Hold_Len         : Byte;

                     PageReason       : String [80];
                     StatusLine       : Byte;
                     LastCostMenu     : String [8];
                     MenuCostPerMin   : System. Word;

                     DoesAVT,
                     RIPmode          : Boolean;

                     ExtraSpace       : Array [1..86] of Byte;
                 end;
