(*                                                    skMHL: tested by mab =)

 !!! не забывайте включать far calls - {$F+} и extended syntax - {$X+},
 !!! иначе skmhl попpосту не собеpется..

 sk's message handling library [started: 27/12/1998]
 version 0.1 beta 24.

 dedicated to tatyana medvedeva, 2:6033/28@fidonet, "ice-lock npd2000".

 (q) by sergey korowkin [sk], 1998-2001.

 some fixes (q) by michail a. baikov [mab], 1999-2000.
 some stuff (q) by Andrey Novikoff, 2000.

 2:6033/27@fidonet

 history

 0.1b24
   - незначительные изменения в getstringpchar [14/01]
   - afterlastkludge тепеpь не возвpащает gettextsize, если клуджев
     нет пеpед текстом (JT/dupechk) [01/02]
   - delphi 5 compatible [16/02]
   - нет более skComnTV, skComnD3 и тд и тп - есть mhldi.inc, mhldc.inc
     (для delphi) и mhltvi.inc, mhltvc.inc для VP/BP. инклудятся
     автоматически, но для delphi нужно пpописать в conditional defines:
     DELPHI [16/02]
   - также не нужно более вызывать InstallMHLcommon [16/02]
   - немного испpавлена дока [17/02]
   - тепеpь jam все-таки откpывается, если jlr не было [17/02]

 0.1b23
   - setfrom/setto/setsubj в msg: тепеpь обpезаем стpоки на всякий
     случай, чтобы gpf не было [09/11 - JT/elkin's_txt2pkt/Tossing]
   - JamSubBufferSize now $4000 [09/11 - JT/elkin's_txt2pkt/Tossing]
   - setfrom/setto/setsubj в sq: та же фигня, что и в msg [09/11 - JT/elkin's_txt2pkt/Tossing]
   - StrToAddress HЕ паpсил вообще констpукции вида '2:6033'
   - os2/w32 dll собиpаются тепеpь vp 2.1.230

 0.1b22
   - GetRead/SetRead pаботал непpавильно в sq [03/11 - JT/OnFire]

 0.1b21
   - AddressEqu added [13/09]
   - tmb.IsRead, tmb.SetRead [26/09 - JT/OnFire]
   - полный бpед в tfidomsgbase.getattribute [11/10 - JT/Scanning]

 0.1b20
   - GetHighWater, SetHighWater, SeekHighWater в сквише (JT) [27/07]
   - packed records (delphi3 делал grunged sq базы),
     thanx to Anton Kochetkov (2:5011/42.79) [21/08]

 0.1b19
   - killmessage в jam'е не всегда возвpащал true :) [26/06]
   - как-то все было плохо с delphi.. тепеpь, вpоде, получше.
     стpанное поведение FileExists'а в skcommon'е - пpога, скомпиленая
     d3 падала, когда тот создавал TMBF... пpишлось сделать небольшой
     workaround, тепеpь все хоpошо ;) [26/06]
   - если кто-нибудь скажет мне, почему skmhlw32.dll падает, вызываясь
     из дельфей, но не всегда (skmhlapifulltitle пpоходит ноpмально),
     а в момент откpытия базы, то я буду ему очень сильно благодаpен.. [26/06]
   - ClearAddress improved :) [29/06]
   - ClearMessageBaseDateTime (DBeer! ;) [29/06]
   - автоматическая пpостановка 0x01flags в msg отоpвана нахpен [07/07]
   - что-то там с julian'ами, word'ами и integer'ами [13/07]
   - стpанное pаздувание sq-баз должно быть попpавлено. тепеpь мы
     кладем на end_frame в заголовке базы и каждый pаз его попpавляем
     (см. skmhlsq.pas) [14/07]

 0.1b18
   - глючил killmessage в jam'е =) (thanx2mab) [26/06]

 0.1b17:
   - skmhlapiDistinguishBaseFormat [11/05]
   - DoneNoDestroy V\skmhl.pas (специально для thunk'ов ;) [14/05]
   - баги с удалением в сквише [10/06]
   - только попpобуйте не делать seek[next|prev] после killmessage [10/06]
   - дока вводила в заблуждение насчет killmessage & continue :( [10/06]
   - дикие опечатки в доке (pазультат, полня etc =)))))) [10/06]
   - SeekNext нужен после TJamMessageBase.KillMessage :) [10/06]
   - паpа лишних flush'ей в tjammessagebase.resetall =) [10/06]
   - с атpибутами в jam'е должно быть все пучком (thanx2mab) [10/06]

 0.1b16:
   - IsValidMessageBaseDateTime в skCommon [12/04]
   - pаботы по выносу skMHL в dll [22-24/04]
   - все пеpеменные TMessageBase, кpоме двух - в пpивате [25/04]
   - tmessagebase.GetStatus, .setmessagetextstream [25/04]
   - больше абстpагиpования, товаpищи! ;-) [25/04]
   - испpавлен TMessageBaseMemoryStream в skComnTV. должен стать
     значительно быстpее (virtualsize, блоки всегда по 8kb, etc ;) [25/04]
   - skMHLversion/title/fulltitle -> функции, дабы не было коллизий
     с vskMHL [01/05]
   - pаботы по подготовке skMHL к pаботе в dll подходят к концу [03/05]
   - basepath абстpагиpовался тоже :) [04/05]
   - pаботы по выносу skMHL в dll закончены. skMHL*.dll,
     VskMHL в комплекте [09/05]
   - закончено написание доки (если "это" можно так назвать =) [10/05]

 0.1b15:
   - окончательно фиксед баги со сквишем (nummsg/highmsg/sqidx.number) [03/04]
     (во всяком случае аганичевский golded тепеpь уже не падает ;)

 0.1b14:
   - fixed злой баг в extractpathname ;) [29/03]
   - fexpand thunk added (delphi comp.) [29/03]
   - глюки в skcomntv пpи откpытии (кpиво откpывали с шаpингом) [31/03]
   - бага в tsquishmessagebase.exist, из-за нее ломались sq-базы =( [31/03]
   - OpenMessageBaseEx (skopen), ombLockedAttemptsExpired (skcommon) [31/03]
   - Delay в skCommon [31/03]

 0.1b13:
   - не всегда _создавались_ .msg-базы (createdirectory и все такое =)
     [04/01]
   - StringCRC32 в skcommon (нужно мне в JT =) [21/01]
   - bug в skcommon.DirectoryExists, thanx to Diman Petrov
   - две пpоцедуpы для pаботы с msgbinarytimestamp by Andrey Novikoff,
     345:817/10. спасибо ему =) [04/02]
   - пpи pаботе с msg тепеpь юзаются binary timestamps'ы. но пpиоpитетнее
     остается текстовой datewritten =) [datearrived тепеpь начал
     пpоставляться. pуль.. ;)] [04/02]
   - тепеpь вpоде не должны обламывать с getfrom/toaddress пpи наличии
     пеpвой стpоки вида AREA:... без 01x0 в начале (Vad Skakov, 2:5065/16)
     [04/02]
   - StrToAddressR - тепеpь можно использовать что-то вpоде
     base^.SetFromAddress(StrToAddressR('2:6033/27.0')^, ...); [18/02]
   - SetFromAndToAddress params changed (freshmsgid) [18/02]
   - SetBaseType - аналог кучи вызовов SetFlag(). можно пользовать
     как стаpый, так и новый метод [18/02]
   - PutOrigin - useful stuff [18/02] =)
   - GetKludgePChar improved (inspirated by Vad Skakov, 2:5065/16) [21/02]
   - SetKludgePChar improved (inspirated by Vad Skakov, 2:5065/16) [21/02]
   - DeleteKludgePChar improved (inspirated by Vad Skakov, 2:5065/16) [21/02]
   - AfterLastKludge [21/02]
   - Dec(MaxLen) в GetKludgePChar [22/02]
   - TVirtualMessageBaseMHL (mhl streams -> base linker), skMHLvmb [24/02]
   - OpenMessageHeader (используется для "пpосмотpа" заголовков сообщений:
     полный аналог OpenMessage, только текст сообщения не гpузит (только
     клуджи). не забывайте делать closemessage!;) [24/02]
   - пеpелопачены пpактически все модули skmhl, тепеpь стиль исходников
     соответствует моему нынешнему стилю =) [24/02]
   - базы начали шаpиться.. [24/02]
   - поддеpжка ^AFLAGS в skMHLmsg: SetAFLAG, GetAFLAG, обновление в
     SetAttribute [24/02]
   - skcommon.ExplainStatus, все константы ошибок тепеpь в skcommon [24/02]
   - WriteMessageHeader. ATTENTION! writemessageheader пишет только
     БИHАРHЫЙ заголовок! вызов его на свежескpейченном сообщении может
     пpивести к чему угодно: напpимеp, к кpаху базы! должен использоваться
     ТОЛЬКО для обновления какой-либо инфы, хpанящейся в бинаpном хидеpе
     (мне оно нужно для обновления reply'ев ;) [28/02]
   - Get/SetReply* в jam/msg/sq. у каждой базы свои методы ;) [28/02]
   - непонятно, как writemessage в tjammessagebase вообще pаботал после
     openmessage.. 8-[ ] [28/02]
   - в sq тепеpь realnumbering + ускоpение pаботы [05/03]
   - IsRealMessageBaseDateTime в skcommon [08/03]
   - RenameFile в skcommon - нужен мне в skmhlpck (упаковка баз) [08/03]
   - repackindex в jam дико тоpмозил. [13/03]
   - паpа идей по оптимизации некотоpых участков skmhl, активно pаботающих
     с memorystream'ами.. может пpивести к дикому ускоpению. думаю.. [13/03]
   - константы skMHLversion/skMHLtitle/skMHLfulltitle [15/03]
   - куча мелких фиксов

 0.1b12:
   - непpавильно pаботали с ластpидами в msg (почему я написал
     longint вместо word?!) [10/12]
   - испpавлен небольшой баг в skmhlmsg в .GetToAddress [13/12]
   - испpавлено опpеделение адpеса в нетмыле, когда FMPT/TOPT были
     пеpед INTL (идея Diman Petrov, 2:5023/27.18) [13/12]

 0.1b11:
   - непpавильно паpсились msgid вида 'zia.cityline.ru' - bug в
     strtoaddress. тепеpь пеpед обpаботкой адpес пpовеpяется на
     валидность всех символов - если хотя бы один символ не входит
     в диапазон ['0'..'9', ':', '/', '.'], то стpока не паpсится... [21/11]
   - еще несколько Y2k fixes (thanx to mab) [06/12]

 0.1b10:
   - TAddress тепеpь не четыpе longint'а, а четыpе integer'а =)
   - Y2k compatibility, две новых функции в skCommon'е.. может где
     забыл пофиксить, напомните мылом ;) [18/08]
   - ASCIIZFIX [FromASCIIZ] в skCommon'е. связан с багом (фичей?)
     strings'овского StrPas'а [18/08]
   - GetAttributesLine -- maScanned
   - наконец-то вкpучена pабота с lastread'ами [06/10]
   - баг в TMessageBaseFileFind.StopSearch в сбоpке под Win32 =) [07/10]
   - тепеpь библиотека собиpается с помощью Delphi.
     тестиpовалось _только_ на Delphi 3, да и то повеpхностно..
     так что баги будут навеpняка.
     для сбоpки под D3 нужно вместо skComnTV подключить skComnD3.

 0.1b9:
   - мелкие фиксы
   - сообщение, состоящее из одного клуджа, кpиво писалось в squish
     (клудж дублиpовался, skMHLsq.pas, TSquishMessageBase.WriteMessage)
   - вылеты по RTE#217, ибо не был занаследован
     skComnTV.TMessageBaseRamStream.Flush
   - считались существующими сообщения в jam'е с hdrloc=-1
   - после KillMessage в jam'е могли не учитываться некотоpые
     последние сообщения

*)
{$IFDEF MSDOS}
  {$IFNDEF DPMI32}
    {$F+}
  {$IFNDEF DPMI}
    {$O+}
  {$ENDIF}
  {$ENDIF}
{$ENDIF}
{&Use32-}
unit skMHL;

interface
uses
 tGlob,
 skCommon;

type
 PMessageBase = ^TMessageBase;
 TMessageBase = object
  constructor Init;
  destructor Done; virtual;

  function Open(const Path: String): Boolean; virtual;
  function Create(const Path: String): Boolean; virtual;
  function Exist(const Path: String): Boolean; virtual;
  procedure Close; virtual;
  function Exists(Message: Longint): Boolean; virtual;
  function Current: Longint; virtual;
  procedure Seek(Message: Longint); virtual;
  procedure SeekNext; virtual;
  procedure SeekPrev; virtual;
  function SeekFound: Boolean; virtual;
  function GetLocation: Longint; virtual;
  procedure SetLocation(Location: Longint); virtual;
  function OpenMessage: Boolean; virtual;
  function OpenMessageHeader: Boolean; virtual;
  function CloseMessage: Boolean; virtual;
  function GetHighest: Longint; virtual;
  function GetCount: Longint; virtual;
  function GetFrom: String; virtual;
  function GetTo: String; virtual;
  function GetSubject: String; virtual;
  procedure GetFromAddress(var Address: TAddress); virtual;
  procedure GetToAddress(var Address: TAddress); virtual;
  procedure GetFromAndToAddress(var FromAddress, ToAddress: TAddress); virtual;
  procedure SetFrom(const S: String); virtual;
  procedure SetTo(const S: String); virtual;
  procedure SetSubject(const S: String); virtual;
  procedure SetFromAddress(var Address: TAddress; const FreshMSGID: Boolean); virtual;
  procedure SetToAddress(var Address: TAddress); virtual;
  procedure SetFromAndToAddress(var FromAddress, ToAddress: TAddress; const FreshMSGID: Boolean); virtual;
  procedure GetStringPChar(Line: PChar; MaxLen: Longint); virtual;
  procedure GetString(var Line: String); virtual;
  procedure PutStringPChar(Line: PChar); virtual;
  procedure PutString(const Line: String); virtual;
  function EndOfMessage: Boolean; virtual;
  procedure SetTextPos(Position: Longint); virtual;
  function GetTextPos: Longint; virtual;
  function GetTextSize: Longint; virtual;
  procedure TruncateText; virtual;
  procedure ReadText(var Buf; Count: Word); virtual;
  procedure WriteText(var Buf; Count: Word); virtual;
  function GetAttribute(Attribute: Longint): Boolean; virtual;
  procedure SetAttribute(Attribute: Longint; Enable: Boolean); virtual;
  procedure GetWrittenDateTime(var DateTime: TMessageBaseDateTime); virtual;
  procedure GetArrivedDateTime(var DateTime: TMessageBaseDateTime); virtual;
  procedure SetWrittenDateTime(var DateTime: TMessageBaseDateTime); virtual;
  procedure SetArrivedDateTime(var DateTime: TMessageBaseDateTime); virtual;
  function WriteMessage: Boolean; virtual;
  function WriteMessageHeader: Boolean; virtual;
  function GetKludgePChar(const Name, Destination: PChar; const MaxLen: Longint): Boolean; virtual;
  function GetKludge(const Name: String; var Destination: String): Boolean; virtual;
  procedure SetKludgePChar(const Name, Value: PChar); virtual;
  procedure SetKludge(const Name, Value: String); virtual;
  procedure DeleteKludgePChar(const Name: PChar); virtual;
  procedure DeleteKludge(const Name: String); virtual;
  procedure AddKludgePChar(const Value: PChar); virtual;
  procedure AddKludge(const Value: String); virtual;
  function GetMessageTextStream: PMessageBaseStream; virtual;
  procedure SetFlag(const Flag: Longint; const Enable: Boolean); virtual;
  function GetFlag(const Flag: Longint): Boolean; virtual;
  procedure CheckFromAddress(const S: String; var Address: TAddress); virtual;
  procedure CheckToAddress(const S: String; var Address: TAddress); virtual;
  function CreateNewMessage: Boolean; virtual;
  function KillMessage: Boolean; virtual;
  function GetLastRead(const UserNumber: Longint): Longint; virtual;
  procedure SetLastRead(const UserNumber: Longint; const Value: Longint); virtual;
  function GetReplyTo: Longint; virtual;
  procedure SetReplyTo(const Value: Longint); virtual;
  procedure ResetDateTime;
  procedure SetBaseType(const BaseType: TBaseType); virtual;
  procedure PutOrigin(Address: TAddress; const Text: String); virtual;
  function AfterLastKludge: Longint; virtual;
  procedure SetMessageTextStream(const AMessageText: PMessageBaseStream); virtual;
  procedure SetStatus(const AStatus: Longint); virtual;
  function GetStatus: Longint; virtual;
  procedure SetOpened(const AOpened: Boolean); virtual;
  function GetOpened: Boolean; virtual;
  procedure SetCurrent(const ACurrentMessage: Longint); virtual;
  procedure SetBasePath(const ABasePath: String); virtual;
  function GetBasePath: String; virtual;
  function GetRead: Boolean; virtual;
  procedure SetRead(const Value: Boolean); virtual;
  function AbsoluteToRelative(Message: Longint): Longint; virtual;
  function RelativeToAbsolute(Message: Longint): Longint; virtual;
  function GetFirstReply: Longint; virtual;
  procedure SetFirstReply(const Value: Longint); virtual;
 public
  KludgeStart, KludgeEnd: Longint;
  RelativeTable: PSortedLongintCollection;
 private
  Status: Longint;
  Opened: Boolean;
  Flags: Longint;
  CurrentMessage: Longint;
  MessageText: PMessageBaseStream;
  BasePath: String;
 end;

procedure GetAttributesLine(B: PMessageBase; var S: String);
procedure GetDateTimeLine(var DT: TMessageBaseDateTime; var S: String);

implementation

constructor TMessageBase.Init;
 begin
  Flags:=0;
  Status:=0;

  Opened:=False;

  MessageText:=nil;
 end;

destructor TMessageBase.Done;
 begin
 end;

function TMessageBase.Open(const Path: String): Boolean;
 begin
  Abstract;
 end;

function TMessageBase.Create(const Path: String): Boolean;
 begin
  Abstract;
 end;

function TMessageBase.Exist(const Path: String): Boolean;
 begin
  Abstract;
 end;

procedure TMessageBase.Close;
 begin
  Abstract;
 end;

function TMessageBase.Exists(Message: Longint): Boolean;
 begin
  Exists:=(Message > 0) and (Message <= GetCount);
 end;

function TMessageBase.Current: Longint;
 begin
  Current:=CurrentMessage;
 end;

procedure TMessageBase.Seek(Message: Longint);
 begin
  CurrentMessage:=Message - 1;

  SeekNext;
 end;

procedure TMessageBase.SeekNext;
 begin
  repeat
   Inc(CurrentMessage);

   if CurrentMessage > GetCount then
    begin
     CurrentMessage:=0;

     Break;
    end;

   if Exists(CurrentMessage) then
    Break;
  until False;
 end;

procedure TMessageBase.SeekPrev;
 begin
  repeat
   Dec(CurrentMessage);

   if CurrentMessage < 1 then
    begin
     CurrentMessage:=0;

     Break;
    end;

   if Exists(CurrentMessage) then
    Break;
  until False;
 end;

function TMessageBase.SeekFound: Boolean;
 begin
  SeekFound:=CurrentMessage <> 0;
 end;

function TMessageBase.GetLocation: Longint;
 begin
  GetLocation:=Current;
 end;

procedure TMessageBase.SetLocation(Location: Longint);
 begin
  SetCurrent(Location);
 end;

function TMessageBase.OpenMessage: Boolean;
 begin
  Abstract;
 end;

function TMessageBase.OpenMessageHeader: Boolean;
 begin
  Abstract;
 end;

function TMessageBase.CloseMessage: Boolean;
 begin
  Abstract;
 end;

function TMessageBase.GetHighest: Longint;
 begin
  GetHighest:=RelativeToAbsolute(GetCount);
 end;

function TMessageBase.GetCount: Longint;
 begin
  GetCount:=RelativeTable^.Count;
 end;

function TMessageBase.GetFrom: String;
 begin
  Abstract;
 end;

function TMessageBase.GetTo: String;
 begin
  Abstract;
 end;

function TMessageBase.GetSubject: String;
 begin
  Abstract;
 end;

procedure TMessageBase.GetFromAddress(var Address: TAddress);
 var
  S: String;
  First: Boolean;
 begin
  SetTextPos(0);

  First:=True;

  while not EndOfMessage do
   begin
    GetString(S);

    if ((Length(S) = 0) or ((S[1] <> #1) and (S[2] <> '*'))) and not First then
     Continue;

    First:=False;

    CheckFromAddress(S, Address);
   end;
 end;

procedure TMessageBase.GetToAddress(var Address: TAddress);
 var
  S: String;
  First: Boolean;
 begin
  SetTextPos(0);

  First:=True;

  while not EndOfMessage do
   begin
    GetString(S);

    if ((Length(S) = 0) or (S[1] <> #1)) and not First then
     Continue;

    First:=False;

    CheckToAddress(S, Address);
   end;
 end;

procedure TMessageBase.GetFromAndToAddress(var FromAddress, ToAddress: TAddress);
 var
  S: String;
  First: Boolean;
 begin
  SetTextPos(0);

  First:=True;

  while not EndOfMessage do
   begin
    GetString(S);

    if ((Length(S) = 0) or ((S[1] <> #1) and (S[2] <> '*'))) and not First then
     Continue;

    First:=False;

    CheckFromAddress(S, FromAddress);
    CheckToAddress(S, ToAddress);
   end;
 end;

procedure TMessageBase.SetFrom(const S: String);
 begin
  Abstract;
 end;

procedure TMessageBase.SetTo(const S: String);
 begin
  Abstract;
 end;

procedure TMessageBase.SetSubject(const S: String);
 begin
  Abstract;
 end;

procedure TMessageBase.SetFromAddress(var Address: TAddress; const FreshMSGID: Boolean);
 var
  ToAddress: TAddress;
  S: String;
 begin
  if (not FreshMSGID) and GetKludge(#1'MSGID:', S) then
   SetKludge(#1'MSGID:', #1'MSGID: ' + AddressToStrEx(Address) + ' ' + ExtractWord(3, S, [' ']))
  else
   SetKludge(#1'MSGID:', #1'MSGID: ' + AddressToStrEx(Address) + ' ' + GenerateMSGID);

  if GetFlag(afNetmail) then
   begin
    GetToAddress(ToAddress);

    SetKludge(#1'INTL', #1'INTL ' + AddressToStrPointless(ToAddress) + ' ' + AddressToStrPointless(Address));

    if Address.Point = 0 then
     DeleteKludge(#1'FMPT')
    else
     SetKludge(#1'FMPT', #1'FMPT ' + LongToStr(Address.Point));
   end;
 end;

procedure TMessageBase.SetToAddress(var Address: TAddress);
 var
  FromAddress: TAddress;
 begin
  if GetFlag(afNetmail) then
   begin
    GetFromAddress(FromAddress);

    SetKludge(#1'INTL', #1'INTL ' + AddressToStrPointless(Address) + ' ' + AddressToStrPointless(FromAddress));

    if Address.Point = 0 then
     DeleteKludge(#1'TOPT')
    else
     SetKludge(#1'TOPT', #1'TOPT ' + LongToStr(Address.Point));
   end;
 end;

procedure TMessageBase.SetFromAndToAddress(var FromAddress, ToAddress: TAddress; const FreshMSGID: Boolean);
 begin
  SetFromAddress(FromAddress, FreshMSGID);

  SetToAddress(ToAddress);
 end;

procedure TMessageBase.GetStringPChar(Line: PChar; MaxLen: Longint);
 const
  BufferSize = 256;
 var
  Buffer: Array[1..BufferSize + 2] Of Char;
  SavePos, Size, StreamSize, K: Longint;
  LineSize: Longint;
 procedure PutBuffer;
  begin
   {s0t}
   if LineSize + Size > MaxLen then
    Size:=MaxLen - LineSize;
   {/s0t}

   if Size < 0 then
    Exit;

   Buffer[Size + 1]:=#0;

   ConcatASCIIZ(Line, @Buffer);

   Inc(LineSize, Size);
  end;
 begin
  Line[0]:=#0;

  LineSize:=0;

  StreamSize:=MessageText^.GetSize;

  SavePos:=MessageText^.GetPos;

  Dec(MaxLen);

  repeat
   Size:=StreamSize - SavePos;

   if Size <= 0 then
    Exit;

   if Size > BufferSize then
    Size:=BufferSize;

   MessageText^.Read(Buffer, Size);

   for K:=1 to Size do
    if Buffer[K] in [#13, #10] then
     begin
      if (Buffer[K] = #13) and (Buffer[K + 1] = #10) and (SavePos + K < StreamSize) then
        MessageText^.Seek(SavePos + K + 1)
      else
        MessageText^.Seek(SavePos + K);

      Buffer[K]:=#0;

      PutBuffer;

      Exit;
     end;

   Buffer[Size + 1]:=#0;

   Inc(SavePos, Size);

   PutBuffer;
  until False;
 end;

procedure TMessageBase.GetString(var Line: String);
 var
  LinePChar: PChar;
 begin
  GetMem(LinePChar, SizeOf(String));

  GetStringPChar(LinePChar, SizeOf(String));

  Line:=FromASCIIZ(LinePChar);

  FreeMem(LinePChar, SizeOf(String));
 end;

procedure TMessageBase.PutStringPChar(Line: PChar);
 const
  B: System.Byte = 13;
 begin
  MessageText^.Write(Line^, LenASCIIZ(Line));
  MessageText^.Write(B, SizeOf(B));
 end;

procedure TMessageBase.PutString(const Line: String);
 var
  LinePChar: PChar;
 begin
  GetMem(LinePChar, Length(Line) + 1);

  ToASCIIZ(Line, LinePChar);

  PutStringPChar(LinePChar);

  FreeMem(LinePChar, Length(Line) + 1);
 end;

function TMessageBase.EndOfMessage: Boolean;
 begin
  EndOfMessage:=MessageText^.GetSize = MessageText^.GetPos;
 end;

procedure TMessageBase.SetTextPos(Position: Longint);
 begin
  MessageText^.Seek(Position);
 end;

function TMessageBase.GetTextPos: Longint;
 begin
  GetTextPos:=MessageText^.GetPos;
 end;

function TMessageBase.GetTextSize: Longint;
 begin
  GetTextSize:=MessageText^.GetSize;
 end;

procedure TMessageBase.TruncateText;
 begin
  MessageText^.Truncate;
 end;

procedure TMessageBase.ReadText(var Buf; Count: Word);
 begin
  MessageText^.Read(Buf, Count);
 end;

procedure TMessageBase.WriteText(var Buf; Count: Word);
 begin
  MessageText^.Write(Buf, Count);
 end;

function TMessageBase.GetAttribute(Attribute: Longint): Boolean;
 begin
  Abstract;
 end;

procedure TMessageBase.SetAttribute(Attribute: Longint; Enable: Boolean);
 begin
  Abstract;
 end;

procedure TMessageBase.GetWrittenDateTime(var DateTime: TMessageBaseDateTime);
 begin
  Abstract;
 end;

procedure TMessageBase.GetArrivedDateTime(var DateTime: TMessageBaseDateTime);
 begin
  Abstract;
 end;

procedure TMessageBase.SetWrittenDateTime(var DateTime: TMessageBaseDateTime);
 begin
  Abstract;
 end;

procedure TMessageBase.SetArrivedDateTime(var DateTime: TMessageBaseDateTime);
 begin
  Abstract;
 end;

function TMessageBase.WriteMessage: Boolean;
 begin
  Abstract;
 end;

function TMessageBase.WriteMessageHeader: Boolean;
 begin
  Abstract;
 end;

function TMessageBase.GetKludgePChar(const Name, Destination: PChar; const MaxLen: Longint): Boolean;
 var
  NameL: Integer;
 begin
  NameL:=LenASCIIZ(Name);

  SetTextPos(0);

  while not EndOfMessage do
   begin
    KludgeStart:=GetTextPos;

    GetStringPChar(Destination, MaxLen);

    if CompLASCIIZ(Name, Destination, NameL) = 0 then
     begin
      KludgeEnd:=GetTextPos;

      GetKludgePChar:=True;

      Exit;
     end;
   end;

  GetKludgePChar:=False;
 end;

function TMessageBase.GetKludge(const Name: String; var Destination: String): Boolean;
 const
  DestinationSize = SizeOf(String);
 var
  AName, ADestination: PChar;
 begin
  GetMem(ADestination, DestinationSize);
  GetMem(AName, Length(Name) + 1);

  ToASCIIZ(Name, AName);

  GetKludge:=GetKludgePChar(AName, ADestination, DestinationSize - 1);

  Destination:=FromASCIIZ(ADestination);

  FreeMem(AName, Length(Name) + 1);
  FreeMem(ADestination, DestinationSize);
 end;

procedure TMessageBase.SetKludgePChar(const Name, Value: PChar);
 var
  Stream: PMessageBaseStream;
  Temp: PChar;
 begin
  GetMem(Temp, MaxLineSize);

  if GetKludgePChar(Name, Temp, MaxLineSize) then
   begin
    Stream:=CreateMessageBaseMemoryStream(MaxMessageSize);

    SetTextPos(KludgeEnd);

    Stream^.CopyFrom(MessageText^, GetTextSize - KludgeEnd);

    SetTextPos(KludgeStart);

    PutStringPChar(Value);

    Stream^.Seek(0);

    MessageText^.CopyFrom(Stream^, Stream^.GetSize);

    MessageText^.Truncate;

    Dispose(Stream, Done);
   end
  else
   AddKludgePChar(Value);

  FreeMem(Temp, MaxLineSize);
 end;

procedure TMessageBase.SetKludge(const Name, Value: String);
 var
  AName, AValue: PChar;
 begin
  GetMem(AName, Length(Name) + 1);
  GetMem(AValue, Length(Value) + 1);

  ToASCIIZ(Name, AName);
  ToASCIIZ(Value, AValue);

  SetKludgePChar(AName, AValue);

  FreeMem(AValue, Length(Value) + 1);
  FreeMem(AName, Length(Name) + 1);
 end;

procedure TMessageBase.DeleteKludgePChar(const Name: PChar);
 var
  Stream: PMessageBaseStream;
  Temp: PChar;
 begin
  GetMem(Temp, MaxLineSize);

  if GetKludgePChar(Name, Temp, MaxLineSize) then
   begin
    Stream:=CreateMessageBaseMemoryStream(MaxMessageSize);

    SetTextPos(KludgeEnd);

    Stream^.CopyFrom(MessageText^, GetTextSize - KludgeEnd);

    SetTextPos(KludgeStart);

    Stream^.Seek(0);

    MessageText^.CopyFrom(Stream^, Stream^.GetSize);

    MessageText^.Truncate;

    Dispose(Stream, Done);
   end;

  FreeMem(Temp, MaxLineSize);
 end;

procedure TMessageBase.DeleteKludge(const Name: String);
 var
  AName: PChar;
 begin
  GetMem(AName, Length(Name) + 1);

  ToASCIIZ(Name, AName);

  DeleteKludgePChar(AName);

  FreeMem(AName, Length(Name) + 1);
 end;

procedure TMessageBase.AddKludgePChar(const Value: PChar);
 var
  P: Longint;
  Stream: PMessageBaseStream;
 begin
  P:=AfterLastKludge;

  if P = GetTextSize then
   begin
    SetTextPos(P);

    PutStringPChar(Value);
   end
  else
   begin
    Stream:=CreateMessageBaseMemoryStream(MaxMessageSize);

    SetTextPos(P);

    Stream^.CopyFrom(MessageText^, GetTextSize - P);

    SetTextPos(P);

    PutStringPChar(Value);

    Stream^.Seek(0);

    MessageText^.CopyFrom(Stream^, Stream^.GetSize);

    MessageText^.Truncate;

    Dispose(Stream, Done);
   end;
 end;

procedure TMessageBase.AddKludge(const Value: String);
 var
  AValue: PChar;
 begin
  GetMem(AValue, Length(Value) + 1);

  ToASCIIZ(Value, AValue);

  AddKludgePChar(AValue);

  FreeMem(AValue, Length(Value) + 1);
 end;

function TMessageBase.GetMessageTextStream: PMessageBaseStream;
 begin
  GetMessageTextStream:=MessageText;
 end;

procedure TMessageBase.SetFlag(const Flag: Longint; const Enable: Boolean);
 begin
  if Enable then
   Flags:=Flags or Flag
  else
   Flags:=Flags and not Flag;
 end;

function TMessageBase.GetFlag(const Flag: Longint): Boolean;
 begin
  GetFlag:=Flags and Flag = Flag;
 end;

procedure TMessageBase.CheckFromAddress(const S: String; var Address: TAddress);
 var
  A: TAddress;
  B1, B2: Byte;
  Valid: Boolean;
 begin
  Valid:=False;

  if Copy(S, 1, 7) = #1'MSGID:' then
   Valid:=StrToAddress(ExtractWord(2, S, [' ']), A) else
  if Copy(S, 1, 5) = #1'INTL' then
   begin
    Valid:=StrToAddress(ExtractWord(3, S, [' ']), A);
    A.Point:=Address.Point;
   end else
  if Copy(S, 1, 5) = #1'FMPT' then
   StrToInteger(ExtractWord(2, S, [' ']), Address.Point) else
  if Copy(S, 1, 10) = ' * Origin:' then
   begin
    B1:=CharRPos('(', S);
    B2:=CharRPos(')', S);

    if (B1 <> 0) and (B2 > B1) then
     Valid:=StrToAddress(Copy(S, B1 + 1, B2 - B1 - 1), A);
   end;

  if Valid and (A.Zone <> 0) then
   Address:=A;
 end;

procedure TMessageBase.CheckToAddress(const S: String; var Address: TAddress);
 var
  A: TAddress;
 begin
  if Copy(S, 1, 5) = #1'INTL' then
   begin
    if StrToAddress(ExtractWord(2, S, [' ']), A) and (A.Zone <> 0) then
     begin
      A.Point:=Address.Point;
      Address:=A;
     end;
   end else
  if Copy(S, 1, 5) = #1'TOPT' then
   StrToInteger(ExtractWord(2, S, [' ']), Address.Point);
 end;

function TMessageBase.CreateNewMessage: Boolean;
 begin
  Abstract;
 end;

function TMessageBase.KillMessage: Boolean;
 begin
  Abstract;
 end;

function TMessageBase.GetLastRead(const UserNumber: Longint): Longint;
 begin
  Abstract;
 end;

procedure TMessageBase.SetLastRead(const UserNumber: Longint; const Value: Longint);
 begin
  Abstract;
 end;

function TMessageBase.GetReplyTo: Longint;
 begin
  Abstract;
 end;

procedure TMessageBase.SetReplyTo(const Value: Longint);
 begin
  Abstract;
 end;

procedure TMessageBase.ResetDateTime;
 var
  DateTime: TMessageBaseDateTime;
 begin
  GetCurrentMessageBaseDateTime(DateTime);

  SetWrittenDateTime(DateTime);
  SetArrivedDateTime(DateTime);
 end;

procedure TMessageBase.SetBaseType(const BaseType: TBaseType);
 begin
  SetFlag(afNetmail, BaseType = btNetmail);
  SetFlag(afEchomail, BaseType = btEchomail);
  SetFlag(afLocal, BaseType = btLocal);
 end;

procedure TMessageBase.PutOrigin(Address: TAddress; const Text: String);
 var
  S: String;
 begin
  S:=Concat(' * Origin:  (', AddressToStrEx(Address), ')');

  System.Insert(Copy(Text, 1, 79 - Length(S)), S, 12);

  PutString(S);
 end;

function TMessageBase.AfterLastKludge: Longint;
 var
  Line: PChar;
  Mode: (mKludge, mText);
  P, Lines: Longint;
 begin
  GetMem(Line, MaxLineSize);

  Mode:=mText;

  AfterLastKludge:=GetTextSize;

  SetTextPos(0);

  Lines:=0;

  while not EndOfMessage do
   begin
    P:=GetTextPos;

    GetStringPChar(Line, MaxLineSize);

    if Line[0] = #1 then Mode:=mKludge else
    if Mode = mKludge then
     begin
      AfterLastKludge:=P;

      Break;
     end
    else
     begin
      Inc(Lines);

      if Lines > 1 then
       Break;
     end;
   end;

  if Mode <> mKludge then
   AfterLastKludge:=0;

  FreeMem(Line, MaxLineSize);
 end;

procedure TMessageBase.SetMessageTextStream(const AMessageText: PMessageBaseStream);
 begin
  if MessageText <> nil then
   Dispose(MessageText, Done);

  MessageText:=AMessageText;
 end;

procedure TMessageBase.SetStatus(const AStatus: Longint);
 begin
  Status:=AStatus;
 end;

function TMessageBase.GetStatus: Longint;
 begin
  GetStatus:=Status;
 end;

procedure TMessageBase.SetOpened(const AOpened: Boolean);
 begin
  Opened:=AOpened;
 end;

function TMessageBase.GetOpened: Boolean;
 begin
  GetOpened:=Opened;
 end;

procedure TMessageBase.SetCurrent(const ACurrentMessage: Longint);
 begin
  CurrentMessage:=ACurrentMessage;
 end;

procedure TMessageBase.SetBasePath(const ABasePath: String);
 begin
  BasePath:=ABasePath;
 end;

function TMessageBase.GetBasePath: String;
 begin
  GetBasePath:=BasePath;
 end;

function TMessageBase.GetRead: Boolean;
 begin
  GetRead:=False;
 end;

procedure TMessageBase.SetRead(const Value: Boolean);
 begin
 end;

function TMessageBase.AbsoluteToRelative(Message: Longint): Longint;
 begin
  AbsoluteToRelative:=RelativeTable^.IndexOf(Pointer(Message)) + 1;
 end;

function TMessageBase.RelativeToAbsolute(Message: Longint): Longint;
 begin
  if not Exists(Message) then
   begin
    RelativeToAbsolute:=0;

    Exit;
   end;

  RelativeToAbsolute:=Longint(RelativeTable^.At(Message - 1));
 end;

function TMessageBase.GetFirstReply: Longint;
 begin
  Abstract;
 end;

procedure TMessageBase.SetFirstReply(const Value: Longint);
 begin
  Abstract;
 end;

{* Stuff *}

procedure GetAttributesLine(B: PMessageBase; var S: String);
 begin
  S:='';

  if B^.GetAttribute(maPrivate) then S:=S + 'Pvt ';
  if B^.GetAttribute(maCrash) then S:=S + 'Cra ';
  if B^.GetAttribute(maReceived) then S:=S + 'Rcv ';
  if B^.GetAttribute(maSent) then S:=S + 'Snt ';
  if B^.GetAttribute(maAttach) then S:=S + 'Att ';
  if B^.GetAttribute(maTransit) then S:=S + 'Trs ';
  if B^.GetAttribute(maOrphan) then S:=S + 'Orp ';
  if B^.GetAttribute(maKill) then S:=S + 'K/s ';
  if B^.GetAttribute(maLocal) then S:=S + 'Loc ';
  if B^.GetAttribute(maHold) then S:=S + 'Hld ';
  if B^.GetAttribute(maFRq) then S:=S + 'FRq ';
  if B^.GetAttribute(maRRq) then S:=S + 'RRq ';
  if B^.GetAttribute(maRRc) then S:=S + 'RRc ';
  if B^.GetAttribute(maARq) then S:=S + 'ARq ';
  if B^.GetAttribute(maURq) then S:=S + 'URq ';
  if B^.GetAttribute(maScanned) then S:=S + 'Scn ';

  if Length(S) <> 0 then
   {$IFDEF DELPHI}
   SetLength(S, Length(S) - 1);
   {$ELSE}
   Dec(S[0]);
   {$ENDIF}
 end;

procedure GetDateTimeLine(var DT: TMessageBaseDateTime; var S: String);
 function LeadingZero(Number, Count: Longint): String;
  var
   S: String;
  begin
   S:=LongToStr(Number);

   while Length(S) < Count do
    S:='0' + S;

   LeadingZero:=S;
  end;
 begin
  S:=LeadingZero(DT.Year, 4) + '/' +
     LeadingZero(DT.Month, 2) + '/' +
     LeadingZero(DT.Day, 2) + ' ' +
     LeadingZero(DT.Hour, 2) + ':' +
     LeadingZero(DT.Min, 2) + ':' +
     LeadingZero(DT.Sec, 2) + '.' +
     LeadingZero(DT.Sec100, 2);
 end;

end.
