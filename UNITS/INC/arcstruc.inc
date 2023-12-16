Type
  MagicTypes =
  (
    None, ARC_Type, ARJ_Type, DWC_Type, HYP_Type,
    LHA_Type, MDCD_Type, ZIP_Type, SQZ_Type, ZOO_Type, RAR_Type,
    RAR2_type, SIT_Type
  );

  PlatformID = (
    ID_IBM,      { IBM and compatibles        }
    ID_MAC,      { MacIntosh                  }
    ID_MULTI     { Platform independend       }
  );

  TimeString = String [20];
  CRCString = String [8];
  NameString = String [20];
  CompressorID = String [4];
  InfoBlock = Array [1..255] Of Byte;

  IBM = Record         { Typecasting record for IBM }
    FileName : ComStr; { platform                   }
    OriginalSize, CompressedSize : LongInt;
    CompressionName : NameString;
    FileCRC : CRCString;
    FileDate : TimeString;
    SaveID : String [3];
    Extra : String [63];
  End;

  InfoArray   = Array [0..3] Of Char;
  MacName     = String [63];

  MAC = Record                    { Typecasting record for MAC }
    ResName : MacName;            { platform                   }
    ResCompSize, ResRealSize : LongInt;
    ResourceType : Byte;
    DataCompSize, DataRealSize : LongInt;
    DataType : Byte;
    FileTyp, FileCreator : InfoArray;
    Filler : Array [1..165] Of Byte;
  End;
