
{*******************************************************}
{                                                       }
{       Turbo Pascal Runtime Library                    }
{       String Handling Unit                            }
{                                                       }
{       Copyright (C) 1990,92 Borland International     }
{                                                       }
{*******************************************************}

Unit Strings;

{$S-}

{$IFNDEF OS2}
{$F+,O+}

Interface

{ StrLen returns the number of characters in Str, not counting  }
{ the null terminator.                                          }

Function StrLen (Str: PChar): Word;

{ StrEnd returns a pointer to the null character that           }
{ terminates Str.                                               }

Function StrEnd (Str: PChar): PChar;

{ StrMove copies exactly Count characters from Source to Dest   }
{ and returns Dest. Source and Dest may overlap.                }

Function StrMove (Dest, Source: PChar; Count: Word): PChar;

{ StrCopy copies Source to Dest and returns Dest.               }

Function StrCopy (Dest, Source: PChar): PChar;

{ StrECopy copies Source to Dest and returns StrEnd(Dest).      }

Function StrECopy (Dest, Source: PChar): PChar;

{ StrLCopy copies at most MaxLen characters from Source to Dest }
{ and returns Dest.                                             }

Function StrLCopy (Dest, Source: PChar; MaxLen: Word): PChar;

{ StrPCopy copies the Pascal style string Source into Dest and  }
{ returns Dest.                                                 }

Function StrPCopy (Dest: PChar; Source: String): PChar;

{ StrCat appends a copy of Source to the end of Dest and        }
{ returns Dest.                                                 }

Function StrCat (Dest, Source: PChar): PChar;

{ StrLCat appends at most MaxLen - StrLen(Dest) characters from }
{ Source to the end of Dest, and returns Dest.                  }

Function StrLCat (Dest, Source: PChar; MaxLen: Word): PChar;

{ StrComp compares Str1 to Str2. The return value is less than  }
{ 0 if Str1 < Str2, 0 if Str1 = Str2, or greater than 0 if      }
{ Str1 > Str2.                                                  }

Function StrComp (Str1, Str2: PChar): Integer;

{ StrIComp compares Str1 to Str2, without case sensitivity. The }
{ return value is the same as StrComp.                          }

Function StrIComp (Str1, Str2: PChar): Integer;

{ StrLComp compares Str1 to Str2, for a maximum length of       }
{ MaxLen characters. The return value is the same as StrComp.   }

Function StrLComp (Str1, Str2: PChar; MaxLen: Word): Integer;

{ StrLIComp compares Str1 to Str2, for a maximum length of      }
{ MaxLen characters, without case sensitivity. The return value }
{ is the same as StrComp.                                       }

Function StrLIComp (Str1, Str2: PChar; MaxLen: Word): Integer;

{ StrScan returns a pointer to the first occurrence of Chr in   }
{ Str. If Chr does not occur in Str, StrScan returns NIL. The   }
{ null terminator is considered to be part of the string.       }

Function StrScan (Str: PChar; Chr: Char): PChar;

{ StrRScan returns a pointer to the last occurrence of Chr in   }
{ Str. If Chr does not occur in Str, StrRScan returns NIL. The  }
{ null terminator is considered to be part of the string.       }

Function StrRScan (Str: PChar; Chr: Char): PChar;

{ StrPos returns a pointer to the first occurrence of Str2 in   }
{ Str1. If Str2 does not occur in Str1, StrPos returns NIL.     }

Function StrPos (Str1, Str2: PChar): PChar;

{ StrUpper converts Str to upper case and returns Str.          }

Function StrUpper (Str: PChar): PChar;

{ StrLower converts Str to lower case and returns Str.          }

Function StrLower (Str: PChar): PChar;

{ StrPas converts Str to a Pascal style string.                 }

Function StrPas (Str: PChar): String;

{ StrNew allocates a copy of Str on the heap. If Str is NIL or  }
{ points to an empty string, StrNew returns NIL and doesn't     }
{ allocate any heap space. Otherwise, StrNew makes a duplicate  }
{ of Str, obtaining space with a call to the GetMem standard    }
{ procedure, and returns a pointer to the duplicated string.    }
{ The allocated space is StrLen(Str) + 1 bytes long.            }

Function StrNew (Str: PChar): PChar;

{ StrDispose disposes a string that was previously allocated    }
{ with StrNew. If Str is NIL, StrDispose does nothing.          }

Procedure StrDispose (Str: PChar);

Implementation

{$W-}

Function StrLen (Str: PChar): Word; Assembler;
Asm
  CLD
  LES   DI, Str
  MOV   CX, 0FFFFH
  XOr   AL, AL
  REPNE SCASB
  MOV   AX, 0FFFEH
  SUB   AX, CX
End;

Function StrEnd (Str: PChar): PChar; Assembler;
Asm
  CLD
  LES   DI, Str
  MOV   CX, 0FFFFH
  XOr   AL, AL
  REPNE SCASB
  MOV   AX, DI
  MOV   DX, ES
  Dec   AX
End;

Function StrMove (Dest, Source: PChar; Count: Word): PChar; Assembler;
Asm
  PUSH  DS
  CLD
  LDS   SI, Source
  LES   DI, Dest
  MOV   AX, DI
  MOV   DX, ES
  MOV   CX, Count
  CMP   SI, DI
  JAE   @@1
  STD
  ADD   SI, CX
  ADD   DI, CX
  Dec   SI
  Dec   DI
  @@1:  REP     MOVSB
  CLD
  POP   DS
End;

Function StrCopy (Dest, Source: PChar): PChar; Assembler;
Asm
  PUSH  DS
  CLD
  LES   DI, Source
  MOV   CX, 0FFFFH
  XOr   AL, AL
  REPNE SCASB
  Not   CX
  LDS   SI, Source
  LES   DI, Dest
  MOV   AX, DI
  MOV   DX, ES
  REP   MOVSB
  POP   DS
End;

Function StrECopy (Dest, Source: PChar): PChar; Assembler;
Asm
  PUSH  DS
  CLD
  LES   DI, Source
  MOV   CX, 0FFFFH
  XOr   AL, AL
  REPNE SCASB
  Not   CX
  LDS   SI, Source
  LES   DI, Dest
  REP   MOVSB
  MOV   AX, DI
  MOV   DX, ES
  Dec   AX
  POP   DS
End;

Function StrLCopy (Dest, Source: PChar; MaxLen: Word): PChar; Assembler;
Asm
  PUSH  DS
  CLD
  LES   DI, Source
  MOV   CX, MaxLen
  MOV   BX, CX
  XOr   AL, AL
  REPNE SCASB
  SUB   BX, CX
  MOV   CX, BX
  LDS   SI, Source
  LES   DI, Dest
  MOV   BX, DI
  MOV   DX, ES
  REP   MOVSB
  STOSB
  XCHG  AX, BX
  POP   DS
End;

Function StrPCopy (Dest: PChar; Source: String): PChar; Assembler;
Asm
  PUSH  DS
  CLD
  LDS   SI, Source
  LES   DI, Dest
  MOV   BX, DI
  MOV   DX, ES
  LODSB
  XOr   AH, AH
  XCHG  AX, CX
  REP   MOVSB
  XOr   AL, AL
  STOSB
  XCHG  AX, BX
  POP   DS
End;

Function StrCat (Dest, Source: PChar): PChar; Assembler;
Asm
  PUSH  Dest. Word [2]
  PUSH  Dest. Word [0]
  PUSH  CS
  Call  Near Ptr StrEnd
  PUSH  DX
  PUSH  AX
  PUSH  Source. Word [2]
  PUSH  Source. Word [0]
  PUSH  CS
  Call  Near Ptr StrCopy
  MOV   AX, Dest. Word [0]
  MOV   DX, Dest. Word [2]
End;

Function StrLCat (Dest, Source: PChar; MaxLen: Word): PChar; Assembler;
Asm
  PUSH  Dest. Word [2]
  PUSH  Dest. Word [0]
  PUSH  CS
  Call  Near Ptr StrEnd
  MOV   CX, Dest. Word [0]
  ADD   CX, MaxLen
  SUB   CX, AX
  JBE   @@1
  PUSH  DX
  PUSH  AX
  PUSH  Source. Word [2]
  PUSH  Source. Word [0]
  PUSH  CX
  PUSH  CS
  Call  Near Ptr StrLCopy
  @@1:  MOV     AX, Dest. Word [0]
  MOV   DX, Dest. Word [2]
End;

Function StrComp (Str1, Str2: PChar): Integer; Assembler;
Asm
  PUSH  DS
  CLD
  LES   DI, Str2
  MOV   SI, DI
  MOV   CX, 0FFFFH
  XOr   AX, AX
  CWD
  REPNE SCASB
  Not   CX
  MOV   DI, SI
  LDS   SI, Str1
  REPE  CMPSB
  MOV   AL, DS: [SI - 1]
  MOV   DL, ES: [DI - 1]
  SUB   AX, DX
  POP   DS
End;

Function StrIComp (Str1, Str2: PChar): Integer; Assembler;
Asm
  PUSH  DS
  CLD
  LES   DI, Str2
  MOV   SI, DI
  MOV   CX, 0FFFFH
  XOr   AX, AX
  CWD
  REPNE SCASB
  Not   CX
  MOV   DI, SI
  LDS   SI, Str1
  @@1:  REPE    CMPSB
  JE    @@4
  MOV   AL, DS: [SI - 1]
  CMP   AL, 'a'
  JB    @@2
  CMP   AL, 'z'
  JA    @@2
  SUB   AL, 20H
  @@2:  MOV     DL, ES: [DI - 1]
  CMP   DL, 'a'
  JB    @@3
  CMP   DL, 'z'
  JA    @@3
  SUB   DL, 20H
  @@3:  SUB     AX, DX
  JE    @@1
  @@4:  POP     DS
End;

Function StrLComp (Str1, Str2: PChar; MaxLen: Word): Integer; Assembler;
Asm
  PUSH  DS
  CLD
  LES   DI, Str2
  MOV   SI, DI
  MOV   AX, MaxLen
  MOV   CX, AX
  JCXZ  @@1
  XCHG  AX, BX
  XOr   AX, AX
  CWD
  REPNE SCASB
  SUB   BX, CX
  MOV   CX, BX
  MOV   DI, SI
  LDS   SI, Str1
  REPE  CMPSB
  MOV   AL, DS: [SI - 1]
  MOV   DL, ES: [DI - 1]
  SUB   AX, DX
  @@1:  POP     DS
End;

Function StrLIComp (Str1, Str2: PChar; MaxLen: Word): Integer; Assembler;
Asm
  PUSH  DS
  CLD
  LES   DI, Str2
  MOV   SI, DI
  MOV   AX, MaxLen
  MOV   CX, AX
  JCXZ  @@4
  XCHG  AX, BX
  XOr   AX, AX
  CWD
  REPNE SCASB
  SUB   BX, CX
  MOV   CX, BX
  MOV   DI, SI
  LDS   SI, Str1
  @@1:  REPE    CMPSB
  JE    @@4
  MOV   AL, DS: [SI - 1]
  CMP   AL, 'a'
  JB    @@2
  CMP   AL, 'z'
  JA    @@2
  SUB   AL, 20H
  @@2:  MOV     DL, ES: [DI - 1]
  CMP   DL, 'a'
  JB    @@3
  CMP   DL, 'z'
  JA    @@3
  SUB   DL, 20H
  @@3:  SUB     AX, DX
  JE    @@1
  @@4:  POP     DS
End;

Function StrScan (Str: PChar; Chr: Char): PChar; Assembler;
Asm
  CLD
  LES   DI, Str
  MOV   SI, DI
  MOV   CX, 0FFFFH
  XOr   AL, AL
  REPNE SCASB
  Not   CX
  MOV   DI, SI
  MOV   AL, Chr
  REPNE SCASB
  MOV   AX, 0
  CWD
  JNE   @@1
  MOV   AX, DI
  MOV   DX, ES
  Dec   AX
  @@1:
End;

Function StrRScan (Str: PChar; Chr: Char): PChar; Assembler;
Asm
  CLD
  LES   DI, Str
  MOV   CX, 0FFFFH
  XOr   AL, AL
  REPNE SCASB
  Not   CX
  STD
  Dec   DI
  MOV   AL, Chr
  REPNE SCASB
  MOV   AX, 0
  CWD
  JNE   @@1
  MOV   AX, DI
  MOV   DX, ES
  Inc   AX
  @@1:  CLD
End;

Function StrPos (Str1, Str2: PChar): PChar; Assembler;
Asm
  PUSH  DS
  CLD
  XOr   AL, AL
  LES   DI, Str2
  MOV   CX, 0FFFFH
  REPNE SCASB
  Not   CX
  Dec   CX
  JE    @@2
  MOV   DX, CX
  MOV   BX, ES
  MOV   DS, BX
  LES   DI, Str1
  MOV   BX, DI
  MOV   CX, 0FFFFH
  REPNE SCASB
  Not   CX
  SUB   CX, DX
  JBE   @@2
  MOV   DI, BX
  @@1:  MOV     SI, Str2.Word [0]
  LODSB
  REPNE SCASB
  JNE   @@2
  MOV   AX, CX
  MOV   BX, DI
  MOV   CX, DX
  Dec   CX
  REPE  CMPSB
  MOV   CX, AX
  MOV   DI, BX
  JNE   @@1
  MOV   AX, DI
  MOV   DX, ES
  Dec   AX
  JMP   @@3
  @@2:  XOr     AX, AX
  MOV   DX, AX
  @@3:  POP     DS
End;

Function StrUpper (Str: PChar): PChar; Assembler;
Asm
  PUSH  DS
  CLD
  LDS   SI, Str
  MOV   BX, SI
  MOV   DX, DS
  @@1:  LODSB
  Or    AL, AL
  JE    @@2
  CMP   AL, 'a'
  JB    @@1
  CMP   AL, 'z'
  JA    @@1
  SUB   AL, 20H
  MOV    [SI - 1], AL
  JMP   @@1
  @@2:  XCHG    AX, BX
  POP   DS
End;

Function StrLower (Str: PChar): PChar; Assembler;
Asm
  PUSH  DS
  CLD
  LDS   SI, Str
  MOV   BX, SI
  MOV   DX, DS
  @@1:  LODSB
  Or    AL, AL
  JE    @@2
  CMP   AL, 'A'
  JB    @@1
  CMP   AL, 'Z'
  JA    @@1
  ADD   AL, 20H
  MOV    [SI - 1], AL
  JMP   @@1
  @@2:  XCHG    AX, BX
  POP   DS
End;

Function StrPas (Str: PChar): String; Assembler;
Asm
  PUSH  DS
  CLD
  LES   DI, Str
  MOV   CX, 0FFFFH
  XOr   AL, AL
  REPNE SCASB
  Not   CX
  Dec   CX
  LDS   SI, Str
  LES   DI, @Result
  MOV   AL, CL
  STOSB
  REP   MOVSB
  POP   DS
End;

{$W+}

Function StrNew (Str: PChar): PChar;
Var
  L: Word;
  P: PChar;
Begin
  StrNew := Nil;
  If (Str <> Nil) And (Str^ <> #0) Then
  Begin
    L := StrLen (Str) + 1;
    GetMem (P, L);
    If P <> Nil Then StrNew := StrMove (P, Str, L);
  End;
End;

Procedure StrDispose (Str: PChar);
Begin
  If Str <> Nil Then FreeMem (Str, StrLen (Str) + 1);
End;

{$ELSE}

{$S-,R-,Q-,I-,Cdecl-,OrgName-,AlignRec-}

Interface

uses Use32;

function StrLen(Str: PChar): Word;
function StrEnd(Str: PChar): PChar;
function StrMove(Dest, Source: PChar; Count: Word): PChar;
function StrCopy(Dest, Source: PChar): PChar;
function StrECopy(Dest, Source: PChar): PChar;
function StrLCopy(Dest, Source: PChar; MaxLen: Word): PChar;
function StrPCopy(Dest: PChar; Source: String): PChar;
function StrCat(Dest, Source: PChar): PChar;
function StrLCat(Dest, Source: PChar; MaxLen: Word): PChar;
function StrComp(Str1, Str2: PChar): Integer;
function StrIComp(Str1, Str2: PChar): Integer;
function StrLComp(Str1, Str2: PChar; MaxLen: Word): Integer;
function StrLIComp(Str1, Str2: PChar; MaxLen: Word): Integer;
function StrScan(Str: PChar; Chr: Char): PChar;
function StrRScan(Str: PChar; Chr: Char): PChar;
function StrPos(Str1, Str2: PChar): PChar;
function StrUpper(Str: PChar): PChar;
function StrLower(Str: PChar): PChar;
function StrPas(Str: PChar): String;
function StrNew(Str: PChar): PChar;
procedure StrDispose(Str: PChar);

implementation

{ Returns the number of characters in Str, not counting the null        }
{ terminator.                                                           }

function StrLen(Str: PChar): Word; assembler; {$USES edi} {$FRAME-}
asm
                cld
                mov     edi,Str
                or      ecx,-1
                xor     eax,eax
                repne   scasb
                sub     eax,ecx
                sub     eax,2
end;

{ Returns a pointer to the null character that terminates Str.          }

function StrEnd(Str: PChar): PChar; assembler; {$USES edi} {$FRAME-}
asm
                cld
                mov     edi,Str
                or      ecx,-1
                xor     al,al
                repne   scasb
                lea     eax,[edi-1]
end;

{ Copies exactly Count characters from Source to Dest and returns Dest. }
{ Source and Dest may overlap.                                          }

function StrMove(Dest, Source: PChar; Count: Word): PChar; assembler; {$USES esi,edi} {$FRAME-}
asm
                mov     esi,Source
                mov     edi,Dest
                mov     edx,edi
                mov     ecx,Count
                cmp     esi,edi
                jae     @@1
                std
                add     esi,ecx
                add     edi,ecx
                mov     eax,ecx
                and     ecx,11b
                shr     eax,2
                dec     esi
                dec     edi
                rep     movsb
                mov     ecx,eax
                sub     esi,3
                sub     edi,3
                rep     movsd
                cld
                jmp     @@2
              @@1:
                cld
                mov     eax,ecx
                shr     ecx,2
                and     al,11b
                rep     movsd
                mov     cl,al
                rep     movsb
              @@2:
                mov     eax,edx
end;

{ Copies Source to Dest and returns Dest.                               }

function StrCopy(Dest, Source: PChar): PChar; assembler; {$USES esi,edi} {$FRAME-}
asm
                cld
                mov     edi,Source
                mov     esi,edi
                xor     al,al
                or      ecx,-1
                repne   scasb
                not     ecx
                mov     dl,cl
                mov     edi,Dest
                mov     eax,edi
                shr     ecx,2
                and     dl,11b
                rep     movsd
                mov     cl,dl
                rep     movsb
end;

{ Copies Source to Dest and returns StrEnd(Dest).                       }

function StrECopy(Dest, Source: PChar): PChar; assembler; {$USES esi,edi} {$FRAME-}
asm
                cld
                mov     edi,Source
                mov     esi,edi
                xor     al,al
                or      ecx,-1
                repne   scasb
                not     ecx
                mov     al,cl
                mov     edi,Dest
                shr     ecx,2
                and     al,11b
                rep     movsd
                mov     cl,al
                rep     movsb
                lea     eax,[edi-1]
end;

{ Copies at most MaxLen characters from Source to Dest and returns Dest.}

function StrLCopy(Dest, Source: PChar; MaxLen: Word): PChar; assembler; {$USES esi,edi} {$FRAME-}
asm
                cld
                mov     edi,Source
                mov     esi,edi
                mov     ecx,MaxLen
                mov     edx,ecx
                xor     al,al
                repne   scasb
                sub     edx,ecx
                mov     ecx,edx
                mov     edi,Dest
                mov     eax,edi
                shr     ecx,2
                and     dl,11b
                rep     movsd
                mov     cl,dl
                rep     movsb
                mov     [edi].Byte,0
end;

{ Copies the Pascal style string Source into Dest and returns Dest.     }

function StrPCopy(Dest: PChar; Source: String): PChar; assembler; {$USES esi,edi} {$FRAME-}
asm
                cld
                mov     esi,Source
                mov     edi,Dest
                mov     eax,edi
                xor     ecx,ecx
                mov     cl,[esi]
                inc     esi
                mov     dl,cl
                shr     ecx,2
                and     dl,11b
                rep     movsd
                mov     cl,dl
                rep     movsb
                mov     [edi].Byte,0
end;

{ Appends a copy of Source to the end of Dest and returns Dest.         }

function StrCat(Dest, Source: PChar): PChar; assembler; {$USES None} {$FRAME+}
asm
                push    Dest
                Call    StrEnd
                push    eax
                push    Source
                Call    StrCopy
                mov     eax,Dest
end;

{ Appends at most MaxLen - StrLen(Dest) characters from Source to the   }
{ end of Dest, and returns Dest.                                        }

function StrLCat(Dest, Source: PChar; MaxLen: Word): PChar; assembler; {$USES None} {$FRAME+}
asm
                push    Dest
                Call    StrEnd
                mov     ecx,Dest
                add     ecx,MaxLen
                sub     ecx,eax
                jbe     @@1
                push    eax
                push    Source
                push    ecx
                Call    StrLCopy
              @@1:
                mov     eax,Dest
end;

{ Compares Str1 to Str2. The return value is less than 0 if Str1 < Str2,}
{ 0 if Str1 = Str2, or greater than 0 if Str1 > Str2.                   }

function StrComp(Str1, Str2: PChar): Integer; assembler; {$USES esi,edi} {$FRAME-}
asm
                cld
                mov     edi,Str2
                mov     esi,edi
                or      ecx,-1
                xor     eax,eax
                xor     edx,edx
                repne   scasb
                not     ecx
                mov     edi,esi
                mov     esi,Str1
                repe    cmpsb
                mov     al,[esi-1]
                mov     dl,[edi-1]
                sub     eax,edx
end;

{ Compares Str1 to Str2, without case sensitivity. The return value is  }
{ the same as StrComp.                                                  }

function StrIComp(Str1, Str2: PChar): Integer; assembler; {$USES esi,edi} {$FRAME-}
asm
                cld
                mov     edi,Str2
                mov     esi,edi
                or      ecx,-1
                xor     eax,eax
                xor     edx,edx
                repne   scasb
                not     ecx
                mov     edi,esi
                mov     esi,Str1
              @@1:
                repe    cmpsb
                je      @@4
                mov     al,[esi-1]
                cmp     al,'a'
                jb      @@2
                cmp     al,'z'
                ja      @@2
                sub     al,'a'-'A'
              @@2:
                mov     dl,[edi-1]
                cmp     dl,'a'
                jb      @@3
                cmp     dl,'z'
                ja      @@3
                sub     dl,'a'-'A'
              @@3:
                sub     eax,edx
                je      @@1
              @@4:
end;

{ Compares Str1 to Str2, for a maximum length of MaxLen characters. The }
{ return value is the same as StrComp.                                  }

function StrLComp(Str1, Str2: PChar; MaxLen: Word): Integer; assembler; {$USES esi,edi} {$FRAME-}
asm
                cld
                mov     edi,Str2
                mov     esi,edi
                mov     eax,MaxLen
                mov     ecx,eax
                jecxz   @@1
                mov     edx,eax
                xor     eax,eax
                repne   scasb
                sub     edx,ecx
                mov     ecx,edx
                mov     edi,esi
                mov     esi,Str1
                repe    cmpsb
                xor     edx,edx
                mov     al,[esi-1]
                mov     dl,[edi-1]
                sub     eax,edx
              @@1:
end;

{ Compares Str1 to Str2, for a maximum length of MaxLen characters,     }
{ without case sensitivity. The return value is the same as StrComp.    }

function StrLIComp(Str1, Str2: PChar; MaxLen: Word): Integer; assembler; {$USES esi,edi} {$FRAME-}
asm
                mov     edi,Str2
                mov     esi,edi
                mov     eax,MaxLen
                mov     ecx,eax
                jecxz   @@4
                cld
                mov     edx,eax
                xor     eax,eax
                repne   scasb
                sub     edx,ecx
                mov     ecx,edx
                mov     edi,esi
                mov     esi,Str1
                xor     edx,edx
              @@1:
                repe    cmpsb
                je      @@4
                mov     al,[esi-1]
                cmp     al,'a'
                jb      @@2
                cmp     al,'z'
                ja      @@2
                sub     al,'a'-'A'
              @@2:
                mov     dl,[edi-1]
                cmp     dl,'a'
                jb      @@3
                cmp     dl,'z'
                ja      @@3
                sub     dl,'a'-'A'
              @@3:
                sub     eax,edx
                je      @@1
              @@4:
end;

{ Returns a pointer to the first occurrence of Chr in Str. If Chr does  }
{ not occur in Str, StrScan returns NIL. The null terminator is         }
{ considered to be part of the string.                                  }

function StrScan(Str: PChar; Chr: Char): PChar; assembler; {$USES edi} {$FRAME-}
asm
                cld
                mov     edi,Str
                mov     edx,edi
                or      ecx,-1
                xor     eax,eax
                repne   scasb
                not     ecx
                mov     edi,edx
                mov     al,Chr
                repne   scasb
                mov     al,0
                jne     @@1
                lea     eax,[edi-1]
              @@1:
end;

{ Returns a pointer to the last occurrence of Chr in Str. If Chr does   }
{ not occur in Str, StrRScan returns NIL. The null terminator is        }
{ considered to be part of the string.                                  }

function StrRScan(Str: PChar; Chr: Char): PChar; assembler; {$USES edi} {$FRAME-}
asm
                cld
                mov     edi,Str
                or      ecx,-1
                xor     eax,eax
                repne   scasb
                not     ecx
                std
                dec     edi
                mov     al,Chr
                repne   scasb
                mov     al,0
                jne     @@1
                lea     eax,[edi+1]
              @@1:
                cld
end;

{ Returns a pointer to the first occurrence of Str2 in Str1. If Str2    }
{ does not occur in Str1, StrPos returns NIL.                           }

function StrPos(Str1, Str2: PChar): PChar; assembler; {$USES ebx,esi,edi} {$FRAME-}
asm
                cld
                xor     al,al
                mov     edi,Str2
                or      ecx,-1
                repne   scasb
                not     ecx
                dec     ecx
                je      @@2
                mov     edx,ecx
                mov     edi,Str1
                mov     ebx,edi
                or      ecx,-1
                repne   scasb
                not     ecx
                sub     ecx,edx
                jbe     @@2
                mov     edi,ebx
              @@1:
                mov     esi,Str2
                lodsb
                repne   scasb
                jne     @@2
                mov     eax,ecx
                mov     ebx,edi
                mov     ecx,edx
                dec     ecx
                repe    cmpsb
                mov     ecx,eax
                mov     edi,ebx
                jne     @@1
                lea     eax,[edi-1]
                jmp     @@3
              @@2:
                xor     eax,eax
              @@3:
end;

{ Converts Str to upper case and returns Str.                           }

function StrUpper(Str: PChar): PChar; assembler; {$USES esi} {$FRAME-}
asm
                cld
                mov     esi,Str
                mov     eax,esi
              @@1:
                mov     dl,[esi]
                test    dl,dl
                jz      @@2
                inc     esi
                cmp     dl,'a'
                jb      @@1
                cmp     dl,'z'
                ja      @@1
                sub     dl,'a'-'A'
                mov     [esi-1],dl
                jmp     @@1
              @@2:
end;

{ Converts Str to lower case and returns Str.                           }

function StrLower(Str: PChar): PChar; assembler; {$USES esi} {$FRAME-}
asm
                cld
                mov     esi,Str
                mov     eax,esi
              @@1:
                mov     dl,[esi]
                test    dl,dl
                jz      @@2
                inc     esi
                cmp     dl,'A'
                jb      @@1
                cmp     dl,'Z'
                ja      @@1
                add     dl,'a'-'A'
                mov     [esi-1],dl
                jmp     @@1
              @@2:
end;

{ StrPas converts Str to a Pascal style string.                         }

function StrPas(Str: PChar): String; assembler; {$USES esi,edi} {$FRAME-}
asm
                cld
                mov     edi,Str
                or      ecx,-1
                xor     al,al
                repne   scasb
                not     ecx
                dec     ecx
                cmp     ecx,255
                jbe     @@1
                mov     ecx,255
              @@1:
                mov     esi,Str
                mov     edi,@Result
                mov     al,cl
                stosb
                shr     ecx,2
                and     al,11b
                rep     movsd
                mov     cl,al
                rep     movsb
end;

{ Allocates a copy of Str on the heap. If Str is NIL or points to an    }
{ empty string, StrNew returns NIL and doesn't allocate any heap space. }
{ Otherwise, StrNew makes a duplicate of Str, obtaining space with a    }
{ call to the GetMem standard procedure, and returns a pointer to the   }
{ duplicated string. The allocated space is StrLen(Str) + 1 bytes long. }

function StrNew(Str: PChar): PChar;
var
  L: Word;
  P: PChar;
begin
  StrNew := nil;
  if (Str <> nil) and (Str^ <> #0) then
  begin
    L := StrLen(Str) + 1;
    GetMem(P, L);
    if P <> nil then StrNew := StrMove(P, Str, L);
  end;
end;

{ Disposes a string that was previously allocated with StrNew. If Str   }
{ is NIL, StrDispose does nothing.                                      }

procedure StrDispose(Str: PChar);
begin
  if Str <> nil then FreeMem(Str, StrLen(Str) + 1);
end;

{$ENDIF}

End.
