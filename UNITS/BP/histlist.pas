
{*******************************************************}
{                                                       }
{       Turbo Pascal Version 7.0                        }
{       Turbo Vision Unit                               }
{                                                       }
{       Copyright (c) 1992 Borland International        }
{                                                       }
{*******************************************************}

unit HistList;

{$O+,F+,X+,I-,S-}

{****************************************************************************
   History buffer structure:

    Byte Byte String          Byte Byte String
    +-------------------------+-------------------------+--...--+
    | 0 | Id | History string | 0 | Id | History string |       |
    +-------------------------+-------------------------+--...--+

 ***************************************************************************}

interface

uses Objects;

const
  HistoryBlock: Pointer = nil;
  HistorySize: Word = 1024;
  HistoryUsed: Word = 0;

procedure HistoryAdd(Id: Byte; const Str: String);
function HistoryCount(Id: Byte): Word;
function HistoryStr(Id: Byte; Index: Integer): String;
procedure ClearHistory;

procedure InitHistory;
procedure DoneHistory;

procedure StoreHistory(var S: TStream);
procedure LoadHistory(var S: TStream);

implementation

var
  CurId: Byte;
  CurString: PString;

{ Advance CurString to next string with an ID of CurId }

procedure AdvanceStringPointer; near; assembler;
asm
	PUSH	DS
        MOV     CX,HistoryUsed
        MOV     BL,CurId
	LDS	SI,CurString
        MOV     DX,DS
        MOV     AX,DS
        OR      AX,SI
        JZ      @@3
        CLD
        JMP	@@2
@@1:	LODSW
	CMP	AH,BL { BL = CurId }
        JE	@@3
@@2:    LODSB
        XOR	AH,AH
        ADD	SI,AX
	CMP	SI,CX { CX = HistoryUsed }
        JB	@@1
        XOR	SI,SI
        MOV	DX,SI
@@3:    POP	DS
	MOV	CurString.Word[0],SI
	MOV	CurString.Word[2],DX
end;

{ Deletes the current string from the table }

procedure DeleteString; near; assembler;
asm
	PUSH	DS
        MOV	CX,HistoryUsed
        CLD
        LES	DI,CurString
        MOV     SI,DI
        DEC	DI
        DEC	DI
        PUSH	ES
        POP	DS
        MOV	AL,BYTE PTR [SI]
        XOR	AH,AH
        INC     AX
	ADD	SI,AX
        SUB	CX,SI
        REP	MOVSB
        POP	DS
	MOV	HistoryUsed,DI
end;

{ Insert a string into the table }

procedure InsertString(Id: Byte; const Str: String); near; assembler;
asm
	PUSH	DS
        STD

        { Position ES:DI to the end the buffer  }
        {          ES:DX to beginning of buffer }
        LES	DX,HistoryBlock
        MOV	DI,HistoryUsed
	LDS	SI,Str
	MOV	BL,[SI]
        XOR	BH,BH
        INC	BX
        INC	BX
        INC     BX
        POP     DS
        PUSH    DS
@@1:    MOV     AX,DI
        ADD	AX,BX
	SUB	AX,DX { DX = HistoryBlock.Word[0] }
        CMP	AX,HistorySize
        JB	@@2

        { Drop the last string off the end of the list }
        DEC 	DI
        XOR	AL,AL
        MOV	CX,0FFFFH
        REPNE	SCASB
        INC	DI
        JMP     @@1

        { Move the table down the size of the string }
@@2:	MOV	SI,DI
	ADD	DI,BX
        MOV     HistoryUsed,DI
        PUSH	ES
	POP	DS
        MOV	CX,SI
        SUB	CX,DX { DX = HistoryBlock.Word[0] }
	REP	MOVSB

        { Copy the string into the position }
        CLD
        MOV     DI,DX { DX = HistoryBlock.Word[0] }
        INC     DI
        MOV	AH,Id
        XOR	AL,AL
	STOSW
        LDS	SI,Str
        LODSB
        STOSB
        MOV	CL,AL
        XOR	CH,CH
        REP	MOVSB

        POP	DS
end;

procedure StartId(Id: Byte); near;
begin
  CurId := Id;
  CurString := HistoryBlock;
end;

function HistoryCount(Id: Byte): Word;
var
  Count: Word;
begin
  StartId(Id);
  Count := 0;
  AdvanceStringPointer;
  while CurString <> nil do
  begin
    Inc(Count);
    AdvanceStringPointer;
  end;
  HistoryCount := Count;
end;

procedure HistoryAdd(Id: Byte; const Str: String);
begin
  if Str = '' then Exit;

  StartId(Id);

  { Delete duplicates }
  AdvanceStringPointer;
  while CurString <> nil do
  begin
    if Str = CurString^ then DeleteString;
    AdvanceStringPointer;
  end;

  InsertString(Id, Str);
end;

function HistoryStr(Id: Byte; Index: Integer): String;
var
  I: Integer;
begin
  StartId(Id);
  for I := 0 to Index do AdvanceStringPointer;
  if CurString <> nil then
    HistoryStr := CurString^ else
    HistoryStr := '';
end;

procedure ClearHistory;
begin
  PChar(HistoryBlock)^ := #0;
  HistoryUsed := PtrRec(HistoryBlock).Ofs + 1;
end;

procedure StoreHistory(var S: TStream);
var
  Size: Word;
begin
  Size := HistoryUsed - PtrRec(HistoryBlock).Ofs;
  S.Write(Size, SizeOf(Word));
  S.Write(HistoryBlock^, Size);
end;

procedure LoadHistory(var S: TStream);
var
  Size: Word;
begin
  S.Read(Size, SizeOf(Word));
  S.Read(HistoryBlock^, Size);
  HistoryUsed := PtrRec(HistoryBlock).Ofs + Size;
end;

procedure InitHistory;
begin
  GetMem(HistoryBlock, HistorySize);
  ClearHistory;
end;

procedure DoneHistory;
begin
  FreeMem(HistoryBlock, HistorySize);
end;

end.
