{$I-}
Program tMsgC;

Uses
  Objects,
  tMisc,
  tGlob,
  tMsgLib;

Const
  ResName : Array [1..3] Of String [8] = ('Params', 'Messages', 'Help');
  OutFile : String = 'tornado.msg';

{$I ver.inc}

Var
  C       : PBigCollection;
  i       : Byte;
  LineNum : LongInt;
  S       : String;
  F       : Text;

Begin
  WriteLn;
  WriteLn ('Tornado messages resource files compiler.');
  WriteLn ('(C) by Konstantin Klyagin, 1996-97');
  WriteLn;

  If ParamCount < 3 Then
  Begin
    WriteLn ('Command line usage:');
    WriteLn (' tmsgc.exe <param_names> <messages_list> <help_text> [output]');
    WriteLn;
    Exit;
  End;

  If ParamCount >= 4 Then OutFile := ParamStr (4);
  If FileExists (OutFile) Then tDeleteFile (OutFile);

  C := New (PBigCollection, Init (512, 16));

  For i := 1 To 3 Do
  Begin
    WriteLn ('Compiling ', ResName [i], ': ', UpString (DefaultName
            (ParamStr (i), 'tmf', '')));

    Assign (F, DefaultName (ParamStr (i), 'tmf', ''));
    ReSet (F);

    If IOResult <> 0 Then
    Begin
      WriteLn ('Unable to open file ' + UpString (DefaultName
              (ParamStr (i), 'tmf', '')) + '!');
      Halt;
    End;

    LineNum := 0;

    While Not Eof (F) Do
    Begin
      ReadLn (F, S);
      Inc (LineNum);
      If S = '' Then Continue;

      If (S [1] <> S [Length (S)]) And (S [1] <> '[') And (S [Length (S)] <> ']') Then
      Begin
        WriteLn ('Error line #', LineNum, ' in file ', UpString
                (DefaultName (ParamStr (i), 'tmf', '')));
        WriteLn;
        Halt;
      End;

      S := Copy (S, 2, Length (S)-2);
      C^. InsLine (S);
    End;

    Close (F);
    SaveCollection (OutFile, C, ResName [i], tMsgC. NameVer);
    C^. DeleteAll;
  End;

  WriteLn;
  WriteLn ('Done.');
  Dispose (C, Done);
End.
