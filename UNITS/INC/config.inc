Type
  tColorSet = (
             cmFrame,       {main tornado screen frame}
             cmMsgs,        {main tornado screen messages}
             cmStatusLine,  {main tornado screen status line}
             cmStatusFlags, {main tornado screen status line flags}
             cmHighlight,   {status line modem lamps hightlighed}
             cmLamps,       {status line modem lamps normal}
             cmOther,       {main tornado screen other elements color}
             cmUsers,       {main tornado screen last users color}
             cmClock,       {clock color}
             cpFrame,       {protocol window frame}
             cpHeader,      {protocol window header}
             cpFieldNames,  {protocol window fields}
             cpHeadText,    {protocol window notice text}
             cpFieldValues, {protocol window fields values}
             cpScrollBar,   {protocol window progress indicator}
             cpMessages,    {protocol window messages}
             cdFrame,       {dialogs frames}
             cdTitle,       {dialogs titles}
             cdInput,       {dialogs input lines}
             cdScroller,    {dialogs scroller bar}
             cdButton,      {dialogs buttons}
             cdText,        {dialogs text}
             caFrame,       {alerts frames}
             caText,        {alerts text}
             mnHidden,      {menus hidden items}
             umNumber,      {user menus items numbers}
             umDot,         {user menus dots}
             umItem,        {user menus items}
             umSeparator,   {user lists separators}
             mlNumber,      {msglist numbers}
             mlFrom,        {msglist From}
             mlTo,          {msglist To}
             mlSubj,        {msglist Subjs}
             mlLightTo,     {msglist light To}
             mlLightFrom,   {msglist light From}
             ulNumbers,     {userlist numbers}
             ulName,        {userlist names}
             ulLocation,    {userlist location}
             ulBirthDate,   {userlist birthdates}
             ulLastDate,    {userlist last dates}
             tdLine,        {todays callers line}
             tdName,        {todays callers names}
             tdEnterTime,   {todays callers enter times}
             tdOnLine,      {todays callers online times}
             tdTransfers,   {todays callers transfer averages}
             avFileName,    {arcview filenames}
             avDate,        {arcview dates}
             avSize,        {arcview sizes}
             avCompressed,  {arcview compressed sizes}
             flFileName,    {filelist: filenames}
             flTagNum,      {filelist: numbers for tag}
             flSize,        {filelist: sizes}
             flDate,        {filelist: dates}
             flDesc,        {filelist: descriptions}
             flComments,    {filelist: comments}
             flDLC,         {filelist: DLCs}
             mrNormal,      {message reader: normal text}
             mrQuote,       {message reader: quoting}
             mrQuote2,      {message reader: quoting 2}
             mrOrigin,      {message reader: origin and tearline}
             scFrames,      {input lines: frames}
             nwFrames,      {news frames}
             nwDate,        {news dates}
             edText,        {editor text}
             edQuote,       {editor quotes}
             chtText        {chat text}
            );

  tColorScheme = Array [tColorSet] Of Byte;
  tShowSecured = (ssShow, ssHidden, ssHide);
  tShowNews    = (tnYes, tnLast, tnNo);

  ConfigRecord = Record
    ComPort                                             : {$IFNDEF OS2} Byte
                                                          {$ELSE} String [20]
                                                          {$ENDIF};
    {$IFNDEF OS2}
    ComVector, IRQ,
    {$ENDIF}

    DataBits, StopBits, Rings, PassTryes, PageDuration,
    InitTryes, PassLength, CDLostDelay, UploadTimePlus,
    DisplayLines, LongDescPos, PageLimit,
    ChatSysX1,  ChatSysX2,  ChatSysY1,  ChatSysY2,
    ChatUserX1, ChatUserX2, ChatUserY1, ChatUserY2      : Byte;

    SysOp, BBSName, HangupString, LoginString, Location,
    ChoiceLanguageStr1, ChoiceLanguageStr2, InitString,
    AnswerString, NoOneWordNames, OffHookString,
    FileGroupsFile, MsgGroupsFile, Path, LogFile,
    TempDir, VIPList, FileAreasFile, MsgAreasFile,
    LimitsFile, DefLangFile, ChatLog, SaveTagPath, Logo,
    DoorWayCTL, UpgraderCTL, ProtocolCTL, LngPath,
    GoodUsersList, BadUsersList, SuxxUsersList, FlagsDir,
    DoorInfoDir, PrivUploadsDir, ConnectStr, RingStr,
    NoCarrierStr, OkStr, TRCLog, ExtMailEd, QWKpack,
    QWKunpack, QWKadd, EchoLog, BadPasswordsList,
    ExternalChat                                        : String [80];

    {$IFNDEF OS2}
    Parity,
    {$ENDIF}
    MainMenu, LogEvents, DateMask                       : String [10];

    DLCountMask, DLFreeKey                              : String [20];
    ToSysOpArea                                         : String [ 5];
    Flags, GoodFlags, UserInfoMinFlags                  : String [26];
    BaudRate, CDTime, MinSpeed                          : LongInt;
    ANSI, More, HotKeys, DLDescs, Frames, ScanPrivMail,
    ScanNewFiles, FSEditor, PostQuote, PostUpload       : AskType;

    {$IFNDEF OS2}
    {$IFDEF VER70}
    Overlay_XMS, Overlay_EMS,
    {$ENDIF}
    HWFlow, SWFlow,
    {$ENDIF}

    CRInMenu, OneWordNames, DataPhone, VoicePhone,
    Birthdate, Address, Organization, AskChatTopic,
    HangUpCmd, DLCount, FastLogon, SaveTagList, Effects,
    IEMSI_Enable, Private, Blinking, ChatTimeSuspend,
    TxtMoreDisable, LocalOffHook, Aliases, HideSysOp,
    FAShowDate, StatusBar, FAShowSize, FAShowMissing,
    FAShowDLC,  RegResume, NewsPauseAsk, eaAnsi, eaAvatar,
    eaTty, Clock, OneColMenus, Sound, CapitalizeNames,
    CutLongDescs, CutDescs, AbortUpLoad, AbortDownLoad,
    DescUnderDLC, LogicalSize, LogoffAsk, LogoffMail,
    DebugFiles, ULOverWrite, AjustULDate, AskLocation,
    DispAsk, AskAlias, AcceptReg, QuotePrefix,
    ChatStyle, YesNoStyle, ShowVer, DlOnStart,
    CheckDlTime                                         : Boolean;

    ShowSecured                                         : tShowSecured;
    AbortKey                                            : tAbortKey;
    DetectEmu                                           : YesNoAuto;

    {$IFDEF MSDOS}
    Layer                                               : LayerType;
    {$ENDIF}

    PageTime, Answer                                    : TimeArray;
    PortAddress, UploadSpace, Security, InactiveTime,
    GoodSecurity, RegisterTime, DoRegExpire,
    IgnoreOldFlags, UserInfoMinSec, OptimiseUserBase    : System. Word;
    LongDescChar                                        : Char;
    ColorScheme                                         : tColorScheme;
    ShowNews                                            : tShowNews;

  End;
