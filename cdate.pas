{$I+}
Uses
  tMisc,
  DOS;

Var
  F           : Text;
  DayOfWeek   : Word;
  DT          : DateTime;

Begin
  If ParamCount = 0 Then
  Begin
    WriteLn ('Usage: ' + ParamStr (0) + '[[drive:\]path\to\]cdate.inc');
    Halt (1);
  End;

  With DT Do
    GetDate (Year, Month, Day, DayOfWeek);

  Assign (F, ParamStr (1));
  ReWrite (F);
  WriteLn (F, FormattedDate (DT, '''DD-NNN-YYYY'''));
  Close (F);
End.
