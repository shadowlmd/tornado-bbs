{$I+}
Uses
  DOS;

Var
  F                             : Text;
  Year, Month, Day, DayOfWeek   : Word;
  tS, S                         : String [16];

Const
  Months : Array [1..12] Of String [3] =
           ('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun',
            'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec');

Begin
  If ParamCount = 0 Then
  Begin
    WriteLn ('Usage: ' + ParamStr (0) + '[[drive:\]path\to\]cdate.inc');
    Halt (1);
  End;
  GetDate (Year, Month, Day, DayOfWeek);
  Str (Day, tS);
  S := '''' + tS + '-' + Months [Month] + '-';
  Str (Year, tS);
  S := S + tS + '''';
  Assign (F, ParamStr (1));
  ReWrite (F);
  WriteLn (F, S);
  Close (F);
End.
