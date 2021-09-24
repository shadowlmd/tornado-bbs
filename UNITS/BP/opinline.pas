{$S-,R-,V-,I-,B-,F-,O-,A-}

{*********************************************************}
{*                  OPINLINE.PAS 1.30                    *}
{*     Copyright (c) TurboPower Software 1987, 1992.     *}
{* Portions Copyright (c) Sunny Hill Software 1985, 1986 *}
{*     and used under license to TurboPower Software     *}
{*                 All rights reserved.                  *}
{*********************************************************}
{*   Compatibility with Virtual Pascal for OS/2 v1.0:    *}
{*             Copyright (c) fPrint UK Ltd 1995          *}
{*                 All rights reserved.                  *}
{*********************************************************}

unit OpInline;
  {-Assorted inline macros}

interface

{$IFDEF DPMI}
Uses
  Dpmi;
{$ENDIF}

type
  JumpRecord =
    record
{$IFDEF VIRTUALPASCAL}
      _edi, _esi, _ebx : Word;
{$ENDIF}
      SpReg, BpReg : Word;
      JmpPt : Pointer;
    end;

{$IFDEF VIRTUALPASCAL}
  OS =
    record
      O : Word;
    end;
{$ELSE}
  OS =
    record
      O, S : Word;
    end;

  LH =
    record
      L, H : Word;
    end;
{$ENDIF}

function SetJump(var JumpDest : JumpRecord) : Word;
  {-Save current SP, BP, and a jump destination}
  {$IFNDEF VIRTUALPASCAL}
  inline(
    $5F/                     {pop di    ;di = Ofs(JmpDest)}
    $07/                     {pop es    ;es = Seg(JmpDest)}
    $FC/                     {cld}
    $89/$E0/                 {mov ax,sp ;save sp}
    $AB/                     {stosw}
    $89/$E8/                 {mov ax,bp ;save bp}
    $AB/                     {stosw}
    $E8/$00/$00/             {call null ;push IP onto stack}
                             {null:}
    $58/                     {pop ax    ;pop into ax}
    $05/$0A/$00/             {add ax,10 ;point to "next:"}
    $AB/                     {stosw     ;save jump offset}
    $8C/$C8/                 {mov ax,cs ;save jump segment}
    $AB/                     {stosw}
    $31/$C0);                {xor ax,ax ;return ax = 0}
                             {next:}
  {$ENDIF VIRTUALPASCAL}

procedure LongJump(var JumpDest : JumpRecord; Result : Word);
  {-Restore SP, BP, and jump to JumpDest.JmpPt}
  {$IFNDEF VIRTUALPASCAL}
  inline(
    $58/                     {pop ax            ;ax = Result}
    $5F/                     {pop di            ;di = Ofs(JumpDest)}
    $07/                     {pop es            ;es = Seg(JumpDest)}
    $26/$8B/$25/             {mov sp,es:[di]    ;restore sp}
    $26/$8B/$6D/$02/         {mov bp,es:[di+2]  ;restore bp}
    $26/$FF/$6D/$04);        {jmp far es:[di+4] ;jump far to JumpDest.JmpPt}
  {$ENDIF VIRTUALPASCAL}

procedure FarCall(ProcAddr : Pointer);
  {-ProcAddr is the address of a routine to be called far. Can be used to
    implement jump tables if procedures take no parameters.}
  {$IFNDEF VIRTUALPASCAL}
  inline(
    $89/$E3/                 {mov bx,sp}
    $36/$FF/$1F/             {call far dword ptr ss:[bx]}
    $81/$C4/$04/$00);        {add sp,4}
  {$ENDIF VIRTUALPASCAL}

procedure NearCall(ProcOfs : Word);
  {-ProcOfs is the offset of a routine to be called near.}
  {$IFNDEF VIRTUALPASCAL}
  inline(
    $5B/                     {pop bx}
    $FF/$D3);                {call bx}
  {$ENDIF VIRTUALPASCAL}

{$IFNDEF VIRTUALPASCAL}
procedure JumpToOldIsr(OldIsr : Pointer);
  {-Jump to previous ISR from an interrupt procedure.}
  inline(
    $5B/                     {pop bx          ;BX = Ofs(OldIsr)}
    $58/                     {pop ax          ;AX = Seg(OldIsr)}
    $87/$5E/$0E/             {xchg bx,[bp+14] ;Switch old BX and Ofs(OldIsr)}
    $87/$46/$10/             {xchg ax,[bp+16] ;Switch old AX and Seg(OldIsr)}
    $89/$EC/                 {mov sp,bp       ;Restore SP}
    $5D/                     {pop bp          ;Restore BP}
    $07/                     {pop es          ;Restore ES}
    $1F/                     {pop ds          ;Restore DS}
    $5F/                     {pop di          ;Restore DI}
    $5E/                     {pop si          ;Restore SI}
    $5A/                     {pop dx          ;Restore DX}
    $59/                     {pop cx          ;Restore CX}
                             {;BX and AX restored earlier; their places on stack}
                             {;now have OldIsr, which is where return will go}
    $CB);                    {retf            ;Chain to OldIsr}
{$ENDIF VIRTUALPASCAL}

function MakeLongInt(H, L : Word) : LongInt;
  {-Constructs a LongInt from two Words}
  {$IFNDEF VIRTUALPASCAL}
  inline(
    $58/                     {pop ax  ;low word into AX}
    $5A);                    {pop dx  ;high word into DX}
  {$ENDIF VIRTUALPASCAL}

function MakeInteger(H, L : Byte): Integer;
  {-Constructs an integer from two bytes}
  {$IFNDEF VIRTUALPASCAL}
  inline(
    $58/                     {pop ax    ;low byte into AL}
    $5B/                     {pop bx    ;high byte into BL}
    $88/$DC);                {mov ah,bl ;high byte into AH}
  {$ENDIF VIRTUALPASCAL}

{$IFDEF StonyBrook}                                                    {!!.13}
function MakeWord(H, L : Byte) : Word [ALTERS(AX, BX)];                {!!.13}
{$ELSE}                                                                {!!.13}
function MakeWord(H, L : Byte) : Word;
{$ENDIF}                                                               {!!.13}
  {-Constructs a word from two bytes}
  {$IFNDEF VIRTUALPASCAL}
  inline(
    $58/                     {pop ax    ;low byte into AL}
    $5B/                     {pop bx    ;high byte into BL}
    $88/$DC);                {mov ah,bl ;high byte into AH}
  {$ENDIF VIRTUALPASCAL}

function Array2Str(var A; Len : Byte) : string;
  {-Convert an array of char to a string}
  {$IFNDEF VIRTUALPASCAL}
  inline(
    $8C/$DB/                 {mov bx,ds ;save DS}
    $59/                     {pop cx    ;CX = Len}
    $30/$ED/                 {xor ch,ch}
    $5E/                     {pop si    ;ds:si => A}
    $1F/                     {pop ds}
    $5F/                     {pop di    ;es:di => result}
    $07/                     {pop es}
    $06/                     {push es   ;put the pointer back on the stack}
    $57/                     {push di}
    $FC/                     {cld       ;go forward}
    $88/$C8/                 {mov al,cl ;set the length byte}
    $AA/                     {stosb}
    $F2/$A4/                 {rep movsb ;move data into string}
    $8E/$DB);                {mov ds,bx ;restore DS}
  {$ENDIF VIRTUALPASCAL}

{$IFNDEF VIRTUALPASCAL}
procedure CallOldIsr(OldIsr : Pointer);
  {-Call previous ISR from an interrupt procedure. Destroys BX.}
  inline(
    $89/$E3/                 {mov bx,sp        ;set up stack frame}
    $9C/                     {pushf            ;push flags to simulate int}
    $36/$FF/$1F/             {call far ss:[bx] ;call OldIsr}
    $81/$C4/$04/$00);        {add sp,4         ;get rid of OldIsr}
{$ENDIF VIRTUALPASCAL}

procedure Reboot;
  {-Reboot the machine}
  {$IFNDEF VIRTUALPASCAL}
{$IFNDEF Dpmi} {!!.21 - for real mode only; see implementation for pmode vers}
  inline(
    $B8/$40/$00/             {mov ax,$40}
    $8E/$D8/                 {mov ds,ax}
    $C7/$06/$72/$00/$34/$12/ {mov word ptr [$0072],$1234}
    $EA/$00/$00/$FF/$FF);    {jmp far $FFFF:$0000}
{$ENDIF}
   {$ENDIF VIRTUALPASCAL}

{$IFDEF StonyBrook}                                                    {!!.13}
function HiWord(L : LongInt) : Word [INLINE];                          {!!.13}
type                                                                   {!!.13}
  Words = Array [0..1] of WORD;                                        {!!.13}
begin                                                                  {!!.13}
  HiWord := Words(L)[1];                                               {!!.13}
end;                                                                   {!!.13}
{$ELSE}                                                                {!!.13}
function HiWord(L : LongInt) : Word;
  {-Return high-order word of L}
  {$IFNDEF VIRTUALPASCAL}
  inline(
    $58/                     {pop ax ;ignore low word}
    $58);                    {pop ax ;pop high word into AX}
  {$ENDIF VIRTUALPASCAL}
{$ENDIF}                                                               {!!.13}

{$IFDEF StonyBrook}                                                    {!!.13}
function LoWord(L : LongInt) : Word [INLINE];                          {!!.03}
begin                                                                  {!!.13}
  LoWord := Word(L);                                                   {!!.13}
end;                                                                   {!!.13}
{$ELSE}                                                                {!!.13}
function LoWord(L : LongInt) : Word;
  {-Return low-order word of L}
  {$IFNDEF VIRTUALPASCAL}
  inline(
    $58/                     {pop ax ;pop low word into AX}
    $5A);                    {pop dx ;ignore high word}
  {$ENDIF VIRTUALPASCAL}
{$ENDIF}                                                               {!!.13}

{$IFDEF StonyBrook}                                                    {!!.13}
function SwapNibble(B : Byte) : Byte [ALTERS(AX, CX)];                 {!!.13}
{$ELSE}                                                                {!!.13}
function SwapNibble(B : Byte) : Byte;
{$ENDIF}                                                               {!!.13}
  {-Swap the high and low nibbles of B: SwapNibble($F0) returns $0F.}
  {$IFNDEF VIRTUALPASCAL}
  inline(
    $58/                     {pop ax}
    $B1/$04/                 {mov cl,4}
    $D2/$C8);                {ror al,cl}
  {$ENDIF VIRTUALPASCAL}

{$IFDEF StonyBrook}                                                    {!!.13}
function SwapWord(L : LongInt) : LongInt [ALTERS(AX, DX)];             {!!.13}
{$ELSE}                                                                {!!.13}
function SwapWord(L : LongInt) : LongInt;
{$ENDIF}                                                               {!!.13}
  {-Swap low- and high-order words of L}
  {$IFNDEF VIRTUALPASCAL}
  inline(
    $5A/                     {pop dx ;pop low word into DX}
    $58);                    {pop ax ;pop high word into AX}
  {$ENDIF VIRTUALPASCAL}

{$IFDEF StonyBrook}                                                    {!!.13}
function Normalized(P : Pointer) : Pointer [ALTERS(AX,BX,CX,DX)];      {!!.13}
{$ELSE}                                                                {!!.13}
function Normalized(P : Pointer) : Pointer;
{$ENDIF}                                                               {!!.13}
  {-Return P as a normalized pointer}
  {$IFNDEF VIRTUALPASCAL}
  inline(
    $58/                     {pop ax    ;pop offset into AX}
    $5A/                     {pop dx    ;pop segment into DX}
    $89/$C3/                 {mov bx,ax ;BX = Ofs(P^)}
    $B1/$04/                 {mov cl,4  ;CL = 4}
    $D3/$EB/                 {shr bx,cl ;BX = Ofs(P^) div 16}
    $01/$DA/                 {add dx,bx ;add BX to segment}
    $25/$0F/$00);            {and ax,$F ;mask out unwanted bits in offset}
  {$ENDIF VIRTUALPASCAL}

{$IFDEF StonyBrook}                                                    {!!.13}
procedure SetFlag(var Flags : Word; FlagMask : Word) [inline];         {!!.13}
begin                                                                  {!!.13}
  Flags := Flags or FlagMask;                                          {!!.13}
end;                                                                   {!!.13}
{$ELSE}                                                                {!!.13}
procedure SetFlag(var Flags : Word; FlagMask : Word);
  {-Set the bit(s) specified by FlagMask in Flags}
  {$IFDEF VIRTUALPASCAL}
procedure SetFlag16(var Flags : SmallWord; FlagMask : SmallWord);
  {$ELSE}
  inline(
    $58/                     {pop ax        ;FlagMask into AX}
    $5F/                     {pop di}
    $07/                     {pop es        ;ES:DI => Flags}
    $26/$09/$05);            {or es:[di],ax ;Flags := Flags or FlagMask}{!!.03}
  {$ENDIF VIRTUALPASCAL}
{$ENDIF}

{$IFDEF StonyBrook}                                                    {!!.13}
procedure ClearFlag(var Flags : Word; FlagMask : Word) [inline];       {!!.13}
begin                                                                  {!!.13}
  Flags := Flags and not FlagMask;                                     {!!.13}
end;                                                                   {!!.13}
{$ELSE}                                                                {!!.13}
procedure ClearFlag(var Flags : Word; FlagMask : Word);
  {-Clear the bit(s) specified by FlagMask in Flags}
  {$IFDEF VIRTUALPASCAL}
procedure ClearFlag16(var Flags : SmallWord; FlagMask : SmallWord);
  {$ELSE}
  inline(
    $58/                     {pop ax         ;FlagMask into AX}
    $5F/                     {pop di}
    $07/                     {pop es         ;ES:DI => Flags}
    $F7/$D0/                 {not ax         ;FlagMask := not FlagMask}
    $26/$21/$05);            {and es:[di],ax ;Flags := Flags and not FlagMask}
  {$ENDIF VIRTUALPASCAL}
{$ENDIF}                                                               {!!.13}

{$IFDEF StonyBrook}                                                    {!!.13}
function FlagIsSet(Flags : Word; FlagMask : Word) : Boolean [inline];  {!!.13}
begin                                                                  {!!.13}
  FlagIsSet := (Flags and FlagMask) <> 0;                              {!!.13}
end;                                                                   {!!.13}
{$ELSE}                                                                {!!.13}
function FlagIsSet(Flags, FlagMask : Word) : Boolean;
  {-Returns True if the bit specified by FlagMask is set in Flags}
  {$IFNDEF VIRTUALPASCAL}
  inline(
    $5A/                     {pop dx    ;FlagMask into DX}
    $58/                     {pop ax    ;Flags into AX}
    $21/$D0/                 {and ax,dx ;Mask out everything not in FlagMask}
    $74/$02/                 {jz  Exit}
    $B0/$01);                {mov al,1  ;AL = Ord(True)}
                             {Exit:}
  {$ENDIF VIRTUALPASCAL}
{$ENDIF}                                                               {!!.13}

{$IFDEF StonyBrook}                                                    {!!.13}
procedure SetByteFlag(var Flags : Byte; FlagMask : Byte) [inline];     {!!.13}
begin                                                                  {!!.13}
  Flags := Flags or FlagMask;                                          {!!.13}
end;                                                                   {!!.13}
{$ELSE}                                                                {!!.13}
procedure SetByteFlag(var Flags : Byte; FlagMask : Byte);
  {-Set the bit(s) specified by FlagMask in Flags}
  {$IFNDEF VIRTUALPASCAL}
  inline(
    $58/                     {pop ax        ;FlagMask into AL}
    $5F/                     {pop di}
    $07/                     {pop es        ;ES:DI => Flags}
    $26/$08/$05);            {or es:[di],al ;Flags := Flags or FlagMask}
  {$ENDIF VIRTUALPASCAL}
{$ENDIF}                                                               {!!.13}

{$IFDEF StonyBrook}                                                    {!!.13}
procedure ClearByteFlag(var Flags : Byte; FlagMask : Byte) [inline];   {!!.13}
begin                                                                  {!!.13}
  Flags := Flags and not FlagMask;                                     {!!.13}
end;                                                                   {!!.13}
{$ELSE}                                                                {!!.13}
procedure ClearByteFlag(var Flags : Byte; FlagMask : Byte);
  {-Clear the bit(s) specified by FlagMask in Flags}
  {$IFNDEF VIRTUALPASCAL}
  inline(
    $58/                     {pop ax         ;FlagMask into AL}
    $5F/                     {pop di}
    $07/                     {pop es         ;ES:DI => Flags}
    $F6/$D0/                 {not al         ;AL := not AL}
    $26/$20/$05);            {and es:[di],al ;Flags := Flags and not FlagMask}
  {$ENDIF VIRTUALPASCAL}
{$ENDIF}                                                               {!!.13}

{$IFDEF StonyBrook}                                                    {!!.13}
function ByteFlagIsSet(Flags : Byte; FlagMask : Byte) : Boolean [inline]; {!!.13}
begin                                                                  {!!.13}
  ByteFlagIsSet := (Flags and FlagMask) <> 0;                          {!!.13}
end;                                                                   {!!.13}
{$ELSE}                                                                {!!.13}
function ByteFlagIsSet(Flags, FlagMask : Byte) : Boolean;
  {-Returns True if the bit specified by FlagMask is set in Flags}
  {$IFNDEF VIRTUALPASCAL}
  inline(
    $5A/                     {pop dx    ;FlagMask into DL}
    $58/                     {pop ax    ;Flags into AL}
    $20/$D0/                 {and al,dl ;Mask out everything not in FlagMask}
    $74/$02/                 {jz  Exit}
    $B0/$01);                {mov al,1  ;AL = Ord(True)}
                             {Exit:}
  {$ENDIF VIRTUALPASCAL}
{$ENDIF}                                                               {!!.13}

{$IFDEF StonyBrook}                                                    {!!.13}
procedure SetLongFlag(var Flags : LongInt; FlagMask : LongInt) [inline]; {!!.13}
begin                                                                  {!!.13}
  Flags := Flags or FlagMask;                                          {!!.13}
end;                                                                   {!!.13}
{$ELSE}                                                                {!!.13}
procedure SetLongFlag(var Flags : LongInt; FlagMask : LongInt);
  {-Set the bit(s) specified by FlagMask in Flags}
  {$IFNDEF VIRTUALPASCAL}
  inline(
    $58/                     {pop ax        ;FlagMask into DX:AX}
    $5A/                     {pop dx}
    $5F/                     {pop di}
    $07/                     {pop es        ;ES:DI => Flags}
    $26/$09/$05/             {or es:[di],ax ;Flags := Flags or FlagMask}
    $26/$09/$55/$02);        {or es:[di+2],dx}
  {$ENDIF VIRTUALPASCAL}
{$ENDIF}                                                               {!!.13}

{$IFDEF StonyBrook}                                                    {!!.13}
procedure ClearLongFlag(var Flags : LongInt; FlagMask : LongInt) [inline]; {!!.13}
begin                                                                  {!!.13}
  Flags := Flags and not FlagMask;                                     {!!.13}
end;                                                                   {!!.13}
{$ELSE}                                                                {!!.13}
procedure ClearLongFlag(var Flags : LongInt; FlagMask : LongInt);
  {-Clear the bit(s) specified by FlagMask in Flags}
  {$IFNDEF VIRTUALPASCAL}
  inline(
    $58/                     {pop ax         ;FlagMask into DX:AX}
    $5A/                     {pop dx}
    $5F/                     {pop di}
    $07/                     {pop es         ;ES:DI => Flags}
    $F7/$D0/                 {not ax         ;FlagMask := not FlagMask}
    $F7/$D2/                 {not dx}
    $26/$21/$05/             {and es:[di],ax ;Flags := Flags and not FlagMask}
    $26/$21/$55/$02);        {and es:[di+2],dx}
  {$ENDIF VIRTUALPASCAL}
{$ENDIF}                                                               {!!.13}

{$IFDEF StonyBrook}                                                    {!!.13}
function LongFlagIsSet(Flags : LongInt; FlagMask : LongInt) : Boolean [inline];{!!.13}
begin                                                                  {!!.13}
  LongFlagIsSet := (Flags and FlagMask) <> 0;                          {!!.13}
end;                                                                   {!!.13}
{$ELSE}                                                                {!!.13}
function LongFlagIsSet(Flags, FlagMask : LongInt) : Boolean;
  {-Returns True if the bit specified by FlagMask is set in Flags}
  {$IFNDEF VIRTUALPASCAL}
  inline(
    $5B/                     {pop bx    ;FlagMask into CX:BX}
    $59/                     {pop cx}
    $58/                     {pop ax    ;Flags into DX:AX}
    $5A/                     {pop dx}
    $21/$D8/                 {and ax,bx ;Mask out everything not in FlagMask}
    $21/$CA/                 {and dx,cx}
    $09/$D0/                 {or  ax,dx}
    $74/$02/                 {jz  Exit}
    $B0/$01);                {mov al,1  ;AL = Ord(True)}
                             {Exit:}
  {$ENDIF VIRTUALPASCAL}
{$ENDIF}                                                               {!!.13}

procedure ExchangeBytes(var I, J : Byte);
  {-Exchange bytes I and J}
  {$IFNDEF VIRTUALPASCAL}
  inline(
    $8C/$DB/                 {mov bx,ds       ;save DS}
    $5E/                     {pop si}
    $1F/                     {pop ds          ;DS:SI => J}
    $5F/                     {pop di}
    $07/                     {pop es          ;ES:DI => I}
    $8A/$04/                 {mov al,[si]     ;AL = J}
    $26/$86/$05/             {xchg al,es:[di] ;I = J, AL = I}
    $88/$04/                 {mov [si],al     ;J = I}
    $8E/$DB);                {mov ds,bx       ;restore DS}
  {$ENDIF VIRTUALPASCAL}

procedure ExchangeWords(var I, J : Word);
  {-Exchange words I and J}
  {$IFDEF VIRTUALPASCAL}
procedure ExchangeWord16s(var I, J : SmallWord);
  {$ELSE}
  inline(
    $8C/$DB/                 {mov bx,ds       ;save DS}
    $5E/                     {pop si}
    $1F/                     {pop ds          ;DS:SI => J}
    $5F/                     {pop di}
    $07/                     {pop es          ;ES:DI => I}
    $8B/$04/                 {mov ax,[si]     ;AX = J}
    $26/$87/$05/             {xchg ax,es:[di] ;I = J, AX = I}
    $89/$04/                 {mov [si],ax     ;J = I}
    $8E/$DB);                {mov ds,bx       ;restore DS}
  {$ENDIF VIRTUALPASCAL}

procedure ExchangeLongInts(var I, J : LongInt);
  {-Exchange LongInts I and J}
  {$IFNDEF VIRTUALPASCAL}
  inline(
    $8C/$DB/               {mov bx,ds       ;save DS}
    $5E/                   {pop si}
    $1F/                   {pop ds          ;DS:SI => J}
    $5F/                   {pop di}
    $07/                   {pop es          ;ES:DI => I}
    $FC/                   {cld}
    $26/$8B/$05/           {mov ax,es:[di]}
    $A5/                   {movsw}
    $89/$44/$FE/           {mov [si-2],ax}
    $8B/$04/               {mov ax,[si]}
    $26/$87/$05/           {xchg ax,es:[di]}
    $89/$04/               {mov [si],ax}
    $8E/$DB);              {mov ds,bx       ;restore DS}
  {$ENDIF VIRTUALPASCAL}

procedure ExchangeStructs(var I, J; Size : Word);
  {-Exchange structures I and J. Useful in sorts}
  {$IFNDEF VIRTUALPASCAL}
  inline(
    $FC/                     {cld       ;go forward}
    $8C/$DA/                 {mov dx,ds       ;save DS}
    $59/                     {pop cx          ;CX = Size}
    $5E/                     {pop si}
    $1F/                     {pop ds          ;DS:SI => J}
    $5F/                     {pop di}
    $07/                     {pop es          ;ES:DI => I}
    $D1/$E9/                 {shr cx,1        ;move by words}
    $E3/$0C/                 {jcxz odd}
    $9C/                     {pushf}
                             {start:}
    $89/$F3/                 {mov bx,si}
    $26/$8B/$05/             {mov ax,es:[di]  ;exchange words}
    $A5/                     {movsw}
    $89/$07/                 {mov [bx],ax}
    $E2/$F6/                 {loop start      ;again?}
    $9D/                     {popf}
                             {odd:}
    $73/$07/                 {jnc exit}
    $8A/$04/                 {mov al,[si]     ;exchange the odd bytes}
    $26/$86/$05/             {xchg al,es:[di]}
    $88/$04/                 {mov [si],al}
                             {exit:}
    $8E/$DA);                {mov ds,dx       ;restore DS}
  {$ENDIF VIRTUALPASCAL}

{$IFDEF StonyBrook}                                                    {!!.13}
function MinWord(A, B : Word) : Word [ALTERS(AX, BX)];                 {!!.13}
{$ELSE}                                                                {!!.13}
function MinWord(A, B : Word) : Word;
{$ENDIF}                                                               {!!.13}
  {-Returns the smaller of A and B}
  {$IFNDEF VIRTUALPASCAL}
  inline(
    $58/                     {pop ax}
    $5B/                     {pop bx}
    $39/$C3/                 {cmp bx,ax}
    $73/$02/                 {jae done}
    $89/$D8);                {mov ax,bx}
                             {done:}
  {$ENDIF VIRTUALPASCAL}

{$IFDEF StonyBrook}                                                    {!!.13}
function MaxWord(A, B : Word) : Word [ALTERS(AX, BX)];                 {!!.13}
{$ELSE}                                                                {!!.13}
function MaxWord(A, B : Word) : Word;
{$ENDIF}                                                               {!!.13}
  {-Returns the greater of A and B}
  {$IFNDEF VIRTUALPASCAL}
  inline(
    $58/                     {pop ax}
    $5B/                     {pop bx}
    $39/$C3/                 {cmp bx,ax}
    $76/$02/                 {jbe done}
    $89/$D8);                {mov ax,bx}
                             {done:}
  {$ENDIF VIRTUALPASCAL}

{$IFDEF StonyBrook}                                                    {!!.13}
function MinLong(A, B : LongInt) : LongInt [ALTERS(AX, BX, CX, DX)];   {!!.13}
{$ELSE}                                                                {!!.13}
function MinLong(A, B : LongInt) : LongInt;
{$ENDIF}                                                               {!!.13}
  {-Returns the smaller of A and B}
  {$IFNDEF VIRTUALPASCAL}
  inline(
    $5B/                     {pop bx       ;CX:BX = B}
    $59/                     {pop cx}
    $58/                     {pop ax       ;DX:AX = A}
    $5A/                     {pop dx}
    $39/$CA/                 {cmp dx,cx    ;compare high byte}
    $7C/$0A/                 {jl  done1    ;A < B?}
    $7F/$04/                 {jg  greater  ;A > B?}
    $39/$D8/                 {cmp ax,bx    ;compare low byte}
    $76/$04/                 {jbe done1    ;A <= B?}
                             {greater:}
    $89/$CA/                 {mov dx,cx    ;A is greater}
    $89/$D8);                {mov ax,bx}
                             {done1:}
  {$ENDIF VIRTUALPASCAL}

{$IFDEF StonyBrook}                                                    {!!.13}
function MaxLong(A, B : LongInt) : LongInt [ALTERS(AX, BX, CX, DX)];   {!!.13}
{$ELSE}                                                                {!!.13}
function MaxLong(A, B : LongInt) : LongInt;
{$ENDIF}                                                               {!!.13}
  {-Returns the greater of A and B}
  {$IFNDEF VIRTUALPASCAL}
  inline(
    $5B/                     {pop bx       ;CX:BX = B}
    $59/                     {pop cx}
    $58/                     {pop ax       ;DX:AX = A}
    $5A/                     {pop dx}
    $39/$CA/                 {cmp dx,cx    ;compare high byte}
    $7F/$0A/                 {jg  done2    ;A > B?}
    $7C/$04/                 {jl  less     ;A < B?}
    $39/$D8/                 {cmp ax,bx    ;compare low byte}
    $73/$04/                 {jae done2    ;A >= B?}
                             {less:}
    $89/$CA/                 {mov dx,cx    ;B is greater}
    $89/$D8);                {mov ax,bx}
                             {done2:}
  {$ENDIF VIRTUALPASCAL}

procedure FillWord(var Dest; Count : Word; Filler : {$IFNDEF VirtualPascal} Word {$ELSE} SmallWord {$ENDIF});
  {-Fill memory starting at Dest with Count instances of Filler}
  {$IFNDEF VIRTUALPASCAL}
  inline(
    $58/                     {pop ax    ;AX = Filler}
    $59/                     {pop cx    ;CX = Count}
    $5F/                     {pop di    ;ES:DI => Dest}
    $07/                     {pop es}
    $FC/                     {cld       ;go forward}
    $F2/$AB);                {rep stosw ;fill memory}
  {$ENDIF VIRTUALPASCAL}

procedure FillStruct(var Dest; Count : Word; var Filler; FillerSize : Word);
  {-Fill memory starting at Dest with Count instances of Filler}
  {$IFNDEF VIRTUALPASCAL}
  inline(
    $58/                     {pop ax     ;AX = FillerSize}
    $5B/                     {pop bx     ;DX:BX => Filler}
    $5A/                     {pop dx}
    $59/                     {pop cx     ;CX = Count}
    $5F/                     {pop di     ;ES:DI => Dest}
    $07/                     {pop es}
    $E3/$11/                 {jcxz done  ;done if Count = 0}
    $FC/                     {cld        ;go forward}
    $1E/                     {push ds    ;save DS}
    $8E/$DA/                 {mov ds,dx  ;DS:BX => Filler}
                             {again:}
    $89/$CA/                 {mov dx,cx  ;save loop count}
    $89/$DE/                 {mov si,bx  ;DS:SI => Filler}
    $89/$C1/                 {mov cx,ax  ;CX = FillerSize}
    $F2/$A4/                 {rep movsb  ;fill}
    $89/$D1/                 {mov cx,dx  ;restore loop count}
    $E2/$F4/                 {loop again ;repeat}
    $1F);                    {pop ds     ;restore DS}
                             {done:}
  {$ENDIF VIRTUALPASCAL}

{$IFDEF StonyBrook}                                                    {!!.13}
function AddWordToPtr(P : Pointer; W : Word) : Pointer [ALTERS(AX, BX, DX)];{!!.13}
{$ELSE}                                                                {!!.13}
function AddWordToPtr(P : Pointer; W : Word) : Pointer;
{$ENDIF}                                                               {!!.13}
  {-Add a Word to a pointer. No normalization or wrap checking performed}
  {$IFNDEF VIRTUALPASCAL}
  inline(
    $5B/                     {pop bx     ;bx = W}
    $58/                     {pop ax     ;ax = Ofs(P^)}
    $5A/                     {pop dx     ;dx = Seg(P^)}
    $01/$D8);                {add ax,bx  ;ax = Ofs(P^)+W}
  {$ENDIF VIRTUALPASCAL}

function LoNibble(N : Byte) : Byte;    {!!.21}
  {-Return the low nibble of a byte value}
{$IFNDEF VIRTUALPASCAL}
inline(
  $58/      {pop ax;    load value}
  $24/$0F); {and al,$F; clear bits 4-7}
  {$ENDIF VIRTUALPASCAL}

function HiNibble(N : Byte) : Byte;    {!!.21}
  {-Return the high nibble of a byte value}
{$IFNDEF VIRTUALPASCAL}
inline(
  $58/      {pop ax; load value}
  $B1/$04/  {mov cl,$4; load shift count}
  $D2/$E8); {shr al,cl; move bits 4-7 to 0-3}
  {$ENDIF VIRTUALPASCAL}

function MakeByte(H, L : Byte) : Byte; {!!.21}
  {-Construct a byte value from two nibble values}
{$IFNDEF VIRTUALPASCAL}
inline(
  $5B/          {pop bx; load low nibble}
  $58/          {pop ax; load hi  nibble}
  $B1/$04/      {mov cl,$4; load shift count}
  $D2/$E0/      {shl al,cl; move bits 0-3 to 4-7}
  $80/$E3/$0F/  {and bl,$f; clear bits 4-7}
  $08/$D8);     {or  al,bl; set bits 0-3 from low nibble}
  {$ENDIF VIRTUALPASCAL}

function PtrToLong(P : Pointer) : LongInt;
  {-Convert pointer, in range $0:$0 to $FFFF:$000F, to LongInt}

function LongToPtr(L : LongInt) : Pointer;
  {-Return LongInt L as a normalized pointer}

function PtrDiff(P1, P2 : Pointer) : LongInt;
  {-Return the number of bytes between P1^ and P2^}

function AddLongToPtr(P : Pointer; L : LongInt) : Pointer;
  {-Add a LongInt to a pointer, returning a normalized pointer}

  {==========================================================================}

implementation

{$IFDEF Dpmi}  {!!.21 - Added for pmode}
procedure Reboot;
var
  Regs : DPMIRegisters;
begin
  Word(Ptr(Seg0040, $72)^) := $1234;
  FillChar(Regs, SizeOf(Regs), 0);
  Regs.CS := $FFFF;
  Regs.IP := $0000;
  if CallFarRealModeProc(0, nil, Regs) <> 0 then ;
end;
{$ENDIF}

function PtrToLong(P : Pointer) : LongInt;
  {-Convert pointer, in range $0:$0 to $FFFF:$000F, to LongInt}
begin
  {$IFDEF VIRTUALPASCAL}
  PtrToLong := LongInt( P );
  {$ELSE}
  PtrToLong := (LongInt(OS(P).S) shl 4)+OS(P).O;
  {$ENDIF VIRTUALPASCAL}
end;

function LongToPtr(L : LongInt) : Pointer;
  {-Return LongInt L as a normalized pointer}
begin
  {$IFDEF VIRTUALPASCAL}
  LongToPtr := Ptr( L );
  {$ELSE}
  LongToPtr := Ptr(Word(L shr 4), Word(L and $F));
  {$ENDIF VIRTUALPASCAL}
end;

function PtrDiff(P1, P2 : Pointer) : LongInt;
  {-Return the number of bytes between P1^ and P2^}
begin
  PtrDiff := Abs(PtrToLong(P1)-PtrToLong(P2));
end;

function AddLongToPtr(P : Pointer; L : LongInt) : Pointer;
  {-Add a LongInt to a pointer, returning a normalized pointer}
begin
  {$IFDEF VIRTUALPASCAL}
  AddLongToPtr := Ptr(L+LongInt(P));
  {$ELSE}
  AddLongToPtr := LongToPtr(L+PtrToLong(P));
  {$ENDIF VIRTUALPASCAL}
end;

{$IFDEF VIRTUALPASCAL}  { Functions not Inline under Virtual Pascal }

{$S-} {$FRAME-} {$USES edi}
function SetJump(var JumpDest : JumpRecord) : Word; assembler;
  {-Save current ESP, EBP, and a jump destination}
  asm
    mov   eax,edi          { Save value of edi }
    mov   edi,JumpDest     { Address of record }
    stosd                  { Edi }
    mov   eax,esi
    stosd                  { esi }
    mov   eax,ebx
    stosd                  { ebx }
    mov   eax,esp          { Stack pointer }
    add   eax,24
    stosd
    mov   eax,ebp          { ebp has not changed }
    stosd
    mov   eax,[esp+4]      { jump point }
    stosd
    xor   eax,eax          { return 0 }
  end;

{$FRAME-} {$USES None}
procedure LongJump(var JumpDest : JumpRecord; Result : Word); assembler;
  {-Restore ESP, EBP, and jump to JumpDest.JmpPt}
  asm
    pop   eax              { Return address - discard }
    pop   eax              { Result value }
    pop   ecx              { JumpDest record }

    mov   edi,[ecx]
    mov   esi,[ecx+4]
    mov   ebx,[ecx+8]
    mov   esp,[ecx+12]
    mov   ebp,[ecx+16]
    jmp   dword ptr [ecx+20]
  end;

{$FRAME-} {$USES None}
procedure FarCall(ProcAddr : Pointer); assembler;
  {-ProcAddr is the address of a routine to be called far. Can be used to
    implement jump tables if procedures take no parameters.}
  asm
    call dword ptr [ ProcAddr ]
  end;

{$FRAME-} {$USES None}
procedure NearCall(ProcOfs : Word); assembler;
  {-ProcOfs is the offset of a routine to be called near.}
  asm
    call  dword ptr [ ProcOfs ]
  end;

{$FRAME-} {$USES None}
function MakeLongInt(H, L : Word) : LongInt; assembler;
  {-Constructs a LongInt from the lower 16 bits of two 32-bit Words}
  asm
    mov  edx,h
    shl  edx,16
    mov  eax,l
    and  eax,$FFFF
    add  eax,edx
  end;

{$FRAME-} {$USES None}
function MakeInteger(H, L : Byte): Integer; assembler;
  {-Constructs a 32-bit integer from two bytes}
  asm
    xor  eax,eax
    mov  al,l
    mov  ah,h
  end;

{$FRAME-} {$USES None}
function MakeWord(H, L : Byte) : Word; assembler;
  {-Constructs a 32-bit word from two bytes}
  asm
    xor  eax,eax
    mov  al,l
    mov  ah,h
  end;

{$FRAME-} {$USES esi,edi}
function Array2Str(var A; Len : Byte) : string; assembler;
  {-Convert an array of char to a string}
  asm
  { xor   ecx,ecx }
    movzx ecx,byte ptr len    { String Length }
    mov   esi,A               { Source address }
    mov   edi,A+4             { Destination string }
    cld
    mov   al,cl               { Store Length Byte }
    stosb
    rep   movsb               { Store Rest of string }
  end;

procedure Reboot;
  {-Not allowed: Run-time error }
  begin
    RunError( 1 );
  end;

{$FRAME-} {$USES None}
function HiWord(L : LongInt) : Word; assembler;
  {-Return high-order 16-bit word of L}
  asm
    xor  eax,eax
    mov  ax,word ptr l+2
  end;

{$FRAME-} {$USES None}
function LoWord(L : LongInt) : Word; assembler;
  {-Return low-order 16-bit word of L}
  asm
    xor  eax,eax
    mov  ax,word ptr l
  end;

{$FRAME-} {$USES None}
function SwapNibble(B : Byte) : Byte; assembler;
  {-Swap the high and low nibbles of B: SwapNibble($F0) returns $0F.}
  asm
    mov  al,b
    ror  al,4
  end;

{$FRAME-} {$USES None}
function SwapWord(L : LongInt) : LongInt; assembler;
  {-Swap low- and high-order words of L}
  asm
    mov  ax,word ptr l
    shl  eax,16
    mov  ax,word ptr l+2
  end;

{$FRAME-} {$USES None}
function Normalized(P : Pointer) : Pointer; assembler;
  {-Return p unchanged}
  asm
    mov  eax,p
  end;

{$FRAME-} {$USES edi}
procedure SetFlag(var Flags : Word; FlagMask : Word); assembler;
  asm
    mov  eax,FlagMask
    mov  edi,Flags
    or   [edi],eax
  end;

{$FRAME-} {$USES edi}
procedure SetFlag16(var Flags : SmallWord; FlagMask : SmallWord); assembler;
  asm
    mov  ax,FlagMask
    mov  edi,Flags
    or   [edi],ax
  end;

{$FRAME-} {$USES edi}
procedure ClearFlag(var Flags : Word; FlagMask : Word); assembler;
  {-Clear the bit(s) specified by FlagMask in Flags}
  asm
    mov  eax,FlagMask
    mov  edi,Flags
    not  eax
    and  [edi],eax
  end;

{$FRAME-} {$USES edi}
procedure ClearFlag16(var Flags : SmallWord; FlagMask : SmallWord); assembler;
  {-Clear the bit(s) specified by FlagMask in Flags}
  asm
    mov  ax,FlagMask
    mov  edi,Flags
    not  ax
    and  [edi],ax
  end;

{$FRAME-} {$USES None}
function FlagIsSet(Flags, FlagMask : Word) : Boolean; assembler;
  {-Returns True if the bit specified by FlagMask is set in Flags}
  asm
    mov  edx,FlagMask
    mov  eax,Flags
    and  eax,edx
    jz   @Exit
    mov  eax,1
  @Exit:
  end;

{$FRAME-} {$USES edi}
procedure SetByteFlag(var Flags : Byte; FlagMask : Byte); assembler;
  {-Set the bit(s) specified by FlagMask in Flags}
  asm
    mov  al,FlagMask
    mov  edi,Flags
    or   [edi],al
  end;

{$FRAME-} {$USES edi}
procedure ClearByteFlag(var Flags : Byte; FlagMask : Byte); assembler;
  {-Clear the bit(s) specified by FlagMask in Flags}
  asm
    mov  al,FlagMask
    mov  edi,Flags
    not  al
    and  [edi],al
  end;

{$FRAME-} {$USES None}
function ByteFlagIsSet(Flags, FlagMask : Byte) : Boolean; assembler;
  {-Returns True if the bit specified by FlagMask is set in Flags}
  asm
    mov  dl,FlagMask
    mov  al,Flags
    and  al,dl
    jz   @Exit
    mov  eax,1
  @Exit:
  end;

{$FRAME-} {$USES edi}
procedure SetLongFlag(var Flags : LongInt; FlagMask : LongInt); assembler;
  asm
    mov  eax,FlagMask
    mov  edi,Flags
    or   [edi],eax
  end;

{$FRAME-} {$USES edi}
procedure ClearLongFlag(var Flags : LongInt; FlagMask : LongInt); assembler;
  {-Clear the bit(s) specified by FlagMask in Flags}
  asm
    mov  eax,FlagMask
    mov  edi,Flags
    not  eax
    and  [edi],eax
  end;

{$FRAME-} {$USES None}
function LongFlagIsSet(Flags, FlagMask : LongInt) : Boolean; assembler;
  {-Returns True if the bit specified by FlagMask is set in Flags}
  asm
    mov  edx,FlagMask
    mov  eax,Flags
    and  eax,edx
    jz   @Exit
    mov  eax,1
  @Exit:
  end;

{$FRAME-} {$USES esi,edi}
procedure ExchangeBytes(var I, J : Byte); assembler;
  {-Exchange bytes I and J}
  asm
    mov  esi,j
    mov  edi,i
    mov  al,[esi]
    xchg al,[edi]
    mov  [esi],al
  end;

{$FRAME-} {$USES esi,edi}
procedure ExchangeWords(var I, J : Word); assembler;
  {-Exchange words I and J}
  asm
    mov  esi,j
    mov  edi,i
    mov  eax,[esi]
    xchg eax,[edi]
    mov  [esi],eax
  end;

{$FRAME-} {$USES esi,edi}
procedure ExchangeWord16s(var I, J : SmallWord); assembler;
  {-Exchange words I and J}
  asm
    mov  esi,j
    mov  edi,i
    mov  ax,[esi]
    xchg ax,[edi]
    mov  [esi],ax
  end;

{$FRAME-} {$USES esi,edi}
procedure ExchangeLongInts(var I, J : Word); assembler;
  {-Exchange words I and J}
  asm
    mov  esi,j
    mov  edi,i
    mov  eax,[esi]
    xchg eax,[edi]
    mov  [esi],eax
  end;

{$FRAME-} {$USES ebx,esi,edi}
procedure ExchangeStructs(var I, J; Size : Word); assembler;
  {-Exchange structures I and J. Useful in sorts}
  asm
    cld                      {forwards}
    mov  esi,j               {Addresses of arguments}
    mov  edi,i
    mov  ecx,size
    mov  edx,ecx
    and  edx,$3              {Save size mod 4 for byte move}
    shr  ecx,2               {Divide by 4 for moving in dwords}
    jcxz @moverest

    pushf
  @mainloop:
    mov  ebx,esi
    mov  eax,[edi]           {exchange dwords}
    movsd
    mov  [ebx],eax
    loop @mainloop
    popf

  @moverest:
    mov  ecx,edx
    jcxz @exit
  @restloop:
    mov  ebx,esi
    mov  al,[edi]
    movsb
    mov  [ebx],al
    loop @restloop

  @exit:
  end;

{$FRAME-} {$USES ebx}
function MinWord(A, B : Word) : Word; assembler;
  {-Returns the smaller of A and B}
  asm
    mov  eax,a
    mov  ebx,b
    cmp  ebx,eax
    jae  @done
    mov  eax,ebx
  @done:
  end;

{$FRAME-} {$USES ebx}
function MaxWord(A, B : Word) : Word; assembler;
  {-Returns the greater of A and B}
  asm
    mov  eax,a
    mov  ebx,b
    cmp  ebx,eax
    jbe  @done
    mov  eax,ebx
  @done:
  end;

{$FRAME-} {$USES ebx}
function MinLong(A, B : LongInt) : LongInt; assembler;
  {-Returns the smaller of A and B}
  asm
    mov  eax,a
    mov  ebx,b
    cmp  ebx,eax
    jae  @done
    mov  eax,ebx
  @done:
  end;

{$FRAME-} {$USES ebx}
function MaxLong(A, B : LongInt) : LongInt; assembler;
  {-Returns the greater of A and B}
  asm
    mov  eax,a
    mov  ebx,b
    cmp  ebx,eax
    jbe  @done
    mov  eax,ebx
  @done:
  end;

{$FRAME-} {$USES edi}
procedure FillWord(var Dest; Count : Word; Filler : SmallWord); assembler;
  {-Fill memory starting at Dest with Count instances of Filler}
  asm
    mov  ax,Filler
    mov  ecx,Count
    mov  edi,Dest
    cld
    rep  stosw
  end;

{$FRAME-} {$USES ebx,esi,edi}
procedure FillStruct(var Dest; Count : Word; var Filler; FillerSize : Word); assembler;
  {-Fill memory starting at Dest with Count instances of Filler}
  asm
    mov  eax,FillerSize
    mov  ebx,Filler
    mov  ecx,Count
    mov  edi,Dest
    jcxz @done

    cld
  @fillloop:
    mov  edx,ecx             {Save loop count}
    mov  esi,ebx             {esi -> Filler}
    mov  ecx,eax             {ecx -> Sizeof filler}
    rep  movsb               {Fill byte by byte}
    mov  ecx,edx             {Get loop count}
    loop @fillloop

  @done:
  end;

{$FRAME-} {$USES None}
function AddWordToPtr(P : Pointer; W : Word) : Pointer; assembler;
  {-Add a Word to a pointer. No normalization or wrap checking performed}
  asm
    mov  eax,p
    add  eax,w
  end;

{$FRAME-} {$USES None}
function LoNibble(N : Byte) : Byte;    {!!.21} assembler;
  {-Return the low nibble of a byte value}
  asm
    mov  al,n
    and  al,$f
  end;

{$FRAME-} {$USES None}
function HiNibble(N : Byte) : Byte;    {!!.21} assembler;
  {-Return the high nibble of a byte value}
  asm
    mov  al,n
    shr  al,4
  end;

{$FRAME-} {$USES ebx}
function MakeByte(H, L : Byte) : Byte; {!!.21} assembler;
  {-Construct a byte value from two nibble values}
  asm
    mov  al,h
    shl  al,4
    mov  bl,l
    and  bl,$f
    or   al,bl
  end;
{$ENDIF VIRTUALPASCAL}

end.
