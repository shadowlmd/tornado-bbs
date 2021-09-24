{$O+,F+}
{ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป}
{บ      -- THIS FILE IS PART OF THE LIVESYSTEMS COMPRESSOR TOOLBOX. --      บ}
{บ          ALL RIGHTS RESERVED  (C) COPYRIGHTED G. HOOGTERP 1994           บ}
{บ                                                                          บ}
{บ             See the documentation for details on the license.            บ}
{บ                                                                          บ}
{ศออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ}
Unit CompSys;     { Compressor System Main Unit }
Interface
Uses
{$IFDEF VirtualPascal}
     Use16,
{$ENDIF}
     DOS,
     BSC,         { Basic compressed object     Always first! }
     IBM_PKZ,     { Pkzip                       }
     IBM_LHA,     { LHA/LZARC/LA                }
     IBM_ARJ,     { Arj                         }
     IBM_SQZ,     { SQZ                         }
     IBM_ARC,     { ARC/PAK/ARC7                }
     IBM_HYP,     { Hyper                       }
     IBM_ZOO,     { ZOO                         }
     IBM_RAR;     { RAR                         }

Type CompressorType = ^BasicCompressorObject;

Procedure InitCompSys;
Function DetectCompressor (_Filename: ComStr; Var _CO: CompressorType): Boolean;

Implementation

Function DetectCompressor (_Filename: ComStr; Var _CO: CompressorType): Boolean;
Const
  BufferSize = 25*1024;  { Make sure there is enough heap! }

Type
  CheckBuffer = Array[1..BufferSize] of Byte;

Var
  Check   : ^CheckBuffer;
  F       : File;
  RR      : RR_Type;
  ThisOne : Byte;
  Found   : Boolean;

Begin
  DetectCompressor := False;
  New (Check);
  If Check = nil Then
    Exit;

  FillChar (Check^, SizeOf (Check^), #00);

  System. FileMode := ReadOnly+ShareCompatible;
  Assign (F, _FileName);
  Reset (F, 1);
  BlockRead (F, Check^, BufferSize, RR);
  Close (F);
  System. FileMode := ReadWrite+ShareCompatible;

  If (IOResult <> 0) Or (RR = 0) Then
  Begin
    Dispose (Check);
    Exit;
  End;

  ThisOne := 1;
  Found := False;

  While Not Found And (ThisOne <= OPtr) Do
  Begin
    OList [ThisOne]^. FileName := _FileName;
    Found := OList [ThisOne]^. IsThisTypeFile (Check^, RR);
    If Not Found Then Inc (ThisOne);
  End;

  If Found Then
  Begin
    _CO := OList [ThisOne];
    _CO^. Filename := _FileName;
  End
  Else
    _CO := NIL;

  Dispose (Check);
  DetectCompressor := Found;
End;

Procedure InitCompSys;
Begin
  New (Ibm_Rar. CO, RarInit); AddToList (Ibm_Rar. CO);
  New (Ibm_Pkz. CO, ZipInit); AddToList (Ibm_Pkz. CO);
  New (Ibm_Arj. CO, ArjInit); AddToList (Ibm_Arj. CO);
  New (Ibm_Lha. CO, LhaInit); AddToList (Ibm_Lha. CO);
  New (Ibm_Arc. CO, ArcInit); AddToList (Ibm_Arc. CO);
  New (Ibm_Hyp. CO, HypInit); AddToList (Ibm_Hyp. CO);
  New (Ibm_Sqz. CO, SqzInit); AddToList (Ibm_Sqz. CO);
  New (Ibm_Zoo. CO, ZooInit); AddToList (Ibm_Zoo. CO);
End;

End.
