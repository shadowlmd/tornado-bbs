{$IFDEF MSDOS}
  {$IFNDEF DPMI32}
    {$F+}
  {$ENDIF}
{$ENDIF}

Unit
  Crc;

Interface

Type
  PChecksum = ^tChecksum;
  tChecksum = Record
    CRC : LongInt;
    Sum : System. Word;
  End;

Const
  CrcTable: Array [0..255] Of System. Word = (
    $0000, $1021, $2042, $3063, $4084, $50a5, $60c6, $70e7,
    $8108, $9129, $a14a, $b16b, $c18c, $D1ad, $e1ce, $f1ef,
    $1231, $0210, $3273, $2252, $52b5, $4294, $72f7, $62D6,
    $9339, $8318, $b37b, $a35a, $D3bd, $c39c, $f3ff, $e3de,
    $2462, $3443, $0420, $1401, $64e6, $74c7, $44a4, $5485,
    $a56a, $b54b, $8528, $9509, $e5ee, $f5cf, $c5ac, $D58D,
    $3653, $2672, $1611, $0630, $76D7, $66f6, $5695, $46b4,
    $b75b, $a77a, $9719, $8738, $f7df, $e7fe, $D79D, $c7bc,
    $48c4, $58e5, $6886, $78a7, $0840, $1861, $2802, $3823,
    $c9cc, $D9ed, $e98e, $f9af, $8948, $9969, $a90a, $b92b,
    $5af5, $4ad4, $7ab7, $6a96, $1a71, $0a50, $3a33, $2a12,
    $dbfd, $cbdc, $fbbf, $eb9e, $9b79, $8b58, $bb3b, $ab1a,
    $6ca6, $7c87, $4ce4, $5cc5, $2c22, $3c03, $0c60, $1c41,
    $edae, $fd8f, $cdec, $ddcd, $ad2a, $bd0b, $8D68, $9D49,
    $7e97, $6eb6, $5ed5, $4ef4, $3e13, $2e32, $1e51, $0e70,
    $ff9f, $efbe, $dfdd, $cffc, $bf1b, $af3a, $9f59, $8f78,
    $9188, $81a9, $b1ca, $a1eb, $D10c, $c12D, $f14e, $e16f,
    $1080, $00a1, $30c2, $20e3, $5004, $4025, $7046, $6067,
    $83b9, $9398, $a3fb, $b3da, $c33D, $D31c, $e37f, $f35e,
    $02b1, $1290, $22f3, $32D2, $4235, $5214, $6277, $7256,
    $b5ea, $a5cb, $95a8, $8589, $f56e, $e54f, $D52c, $c50D,
    $34e2, $24c3, $14a0, $0481, $7466, $6447, $5424, $4405,
    $a7db, $b7fa, $8799, $97b8, $e75f, $f77e, $c71D, $D73c,
    $26D3, $36f2, $0691, $16b0, $6657, $7676, $4615, $5634,
    $D94c, $c96D, $f90e, $e92f, $99c8, $89e9, $b98a, $a9ab,
    $5844, $4865, $7806, $6827, $18c0, $08e1, $3882, $28a3,
    $cb7D, $db5c, $eb3f, $fb1e, $8bf9, $9bd8, $abbb, $bb9a,
    $4a75, $5a54, $6a37, $7a16, $0af1, $1ad0, $2ab3, $3a92,
    $fd2e, $ed0f, $dd6c, $cd4D, $bdaa, $ad8b, $9de8, $8dc9,
    $7c26, $6c07, $5c64, $4c45, $3ca2, $2c83, $1ce0, $0cc1,
    $ef1f, $ff3e, $cf5D, $df7c, $af9b, $bfba, $8fd9, $9ff8,
    $6e17, $7e36, $4e55, $5e74, $2e93, $3eb2, $0ed1, $1ef0);

  Crc32Table : Array [0..255] Of LongInt = (
    $00000000, $77073096, $ee0e612c, $990951ba, $076dc419, $706af48f, $e963a535,
    $9e6495a3, $0edb8832, $79dcb8a4, $e0D5e91e, $97D2D988, $09b64c2b, $7eb17cbd,
    $e7b82D07, $90bf1D91, $1db71064, $6ab020f2, $f3b97148, $84be41de, $1adad47D,
    $6ddde4eb, $f4D4b551, $83D385c7, $136c9856, $646ba8c0, $fd62f97a, $8a65c9ec,
    $14015c4f, $63066cd9, $fa0f3D63, $8D080df5, $3b6e20c8, $4c69105e, $D56041e4,
    $a2677172, $3c03e4D1, $4b04D447, $D20D85fd, $a50ab56b, $35b5a8fa, $42b2986c,
    $dbbbc9D6, $acbcf940, $32D86ce3, $45df5c75, $dcd60dcf, $abd13D59, $26D930ac,
    $51de003a, $c8D75180, $bfd06116, $21b4f4b5, $56b3c423, $cfba9599, $b8bda50f,
    $2802b89e, $5f058808, $c60cd9b2, $b10be924, $2f6f7c87, $58684c11, $c1611dab,
    $b6662D3D, $76dc4190, $01db7106, $98D220bc, $efd5102a, $71b18589, $06b6b51f,
    $9fbfe4a5, $e8b8D433, $7807c9a2, $0f00f934, $9609a88e, $e10e9818, $7f6a0dbb,
    $086D3D2D, $91646c97, $e6635c01, $6b6b51f4, $1c6c6162, $856530D8, $f262004e,
    $6c0695ed, $1b01a57b, $8208f4c1, $f50fc457, $65b0D9c6, $12b7e950, $8bbeb8ea,
    $fcb9887c, $62dd1ddf, $15da2D49, $8cd37cf3, $fbd44c65, $4db26158, $3ab551ce,
    $a3bc0074, $D4bb30e2, $4adfa541, $3dd895D7, $a4D1c46D, $D3D6f4fb, $4369e96a,
    $346ed9fc, $ad678846, $da60b8D0, $44042D73, $33031de5, $aa0a4c5f, $dd0D7cc9,
    $5005713c, $270241aa, $be0b1010, $c90c2086, $5768b525, $206f85b3, $b966D409,
    $ce61e49f, $5edef90e, $29D9c998, $b0D09822, $c7D7a8b4, $59b33D17, $2eb40D81,
    $b7bd5c3b, $c0ba6cad, $edb88320, $9abfb3b6, $03b6e20c, $74b1D29a, $ead54739,
    $9dd277af, $04db2615, $73dc1683, $e3630b12, $94643b84, $0D6D6a3e, $7a6a5aa8,
    $e40ecf0b, $9309ff9D, $0a00ae27, $7D079eb1, $f00f9344, $8708a3D2, $1e01f268,
    $6906c2fe, $f762575D, $806567cb, $196c3671, $6e6b06e7, $fed41b76, $89D32be0,
    $10da7a5a, $67dd4acc, $f9b9df6f, $8ebeeff9, $17b7be43, $60b08ed5, $D6D6a3e8,
    $a1D1937e, $38D8c2c4, $4fdff252, $D1bb67f1, $a6bc5767, $3fb506dd, $48b2364b,
    $D80D2bda, $af0a1b4c, $36034af6, $41047a60, $df60efc3, $a867df55, $316e8eef,
    $4669be79, $cb61b38c, $bc66831a, $256fd2a0, $5268e236, $cc0c7795, $bb0b4703,
    $220216b9, $5505262f, $c5ba3bbe, $b2bd0b28, $2bb45a92, $5cb36a04, $c2D7ffa7,
    $b5D0cf31, $2cd99e8b, $5bdeae1D, $9b64c2b0, $ec63f226, $756aa39c, $026D930a,
    $9c0906a9, $eb0e363f, $72076785, $05005713, $95bf4a82, $e2b87a14, $7bb12bae,
    $0cb61b38, $92D28e9b, $e5D5be0D, $7cdcefb7, $0bdbdf21, $86D3D2D4, $f1D4e242,
    $68ddb3f8, $1fda836e, $81be16cd, $f6b9265b, $6fb077e1, $18b74777, $88085ae6,
    $ff0f6a70, $66063bca, $11010b5c, $8f659eff, $f862ae69, $616bffd3, $166ccf45,
    $a00ae278, $D70dd2ee, $4e048354, $3903b3c2, $a7672661, $D06016f7, $4969474D,
    $3e6e77db, $aed16a4a, $D9D65adc, $40df0b66, $37D83bf0, $a9bcae53, $debb9ec5,
    $47b2cf7f, $30b5ffe9, $bdbdf21c, $cabac28a, $53b39330, $24b4a3a6, $bad03605,
    $cdd70693, $54de5729, $23D967bf, $b3667a2e, $c4614ab8, $5D681b02, $2a6f2b94,
    $b40bbe37, $c30c8ea1, $5a05df1b, $2D02ef8D);

Function UpdateCrc32 (CurByte: Byte; CurCrc: LongInt): LongInt;
Function UpdateCrc (CurByte: Byte; CurCrc: System. Word): System. Word;
Function UpdateCrc32Buf (Var Buf; Len: Word; StartCrc: LongInt): LongInt;
Function UpdateCrcBuf (Var Buf; Len: Word; StartCrc: System. Word): System. Word;
Function Crc32Str (Const S: String): LongInt;
Procedure GetChecksum (Const S: String; Var C: tChecksum);

Implementation

{$IFNDEF VirtualPascal}
  {$L crc_.obj}
{$ELSE}
  {$L crc32_.obj}
{$ENDIF}
  Function UpdateCrc32 (CurByte: Byte; CurCrc: LongInt): LongInt; External;
  Function UpdateCrc (CurByte: Byte; CurCrc: System. Word): System. Word; External;
  Function Crc32Str (Const S: String): LongInt; External;
  Function UpdateCrc32Buf (Var Buf; Len: Word; StartCrc: LongInt): LongInt; External;
  Function UpdateCrcBuf (Var Buf; Len: Word; StartCrc: System. Word): System. Word; External;
  Procedure GetChecksum (Const S: String; Var C: tChecksum); External;

End.
