{ users.tor }

Type
  tEmulation  = (teAnsi, teTty, teAvatar);

  tUser = Record
    Name                                     : String [36];
    Location, Organization, Address1,
    Address2, Address3                       : String [50];
    Comment                                  : String [80];
    Password, HPhone, BPhone                 : String [15];
    Lang                                     : String [ 8];
    LastRead, NoCalls, Uploads, Downloads,
    UploadsK, DownloadsK, TodayK, MsgsPosted,
    LastDate, FirstDate, BirthDate,
    TimeUsedToday, TotalTime                 : LongInt;
    MsgArea, FileArea                        : System. Word;
    TimeCount, More, HotKeys                 : Boolean;
    Protocol                                 : Char;
    Lines                                    : Byte;
    Cargo1                                   : Byte;
    FSEditor                                 : Boolean;
    Security                                 : System. Word;
    Emu                                      : tEmulation;
    ReReadLimit, Frames                      : Boolean;
    Flags                                    : String [35];
    Alias                                    : String [15];
    Guest                                    : Boolean;
    FileGroup, MsgGroup                      : System. Word;
    AvgCPS                                   : LongInt;
    LastTime                                 : System. Word;
    DailySize                                : LongInt;
    XLAT                                     : String [30];
    Cargo2                                   : Array [1..28] Of Byte;
  End;
