
{ lastcall.tor }

 tLastCaller = Record
   Name                                : String [36];
   Line                                : Byte;
   LoginTime                           : String [5];
   Date, TimeOnLine, ULkb, DLkb        : LongInt;
 End;

{ system.tor   }

 SystemType = Record
   TotalCalls, MsgsPosted, UpLoads,
   DownLoads                        : LongInt;
 End;