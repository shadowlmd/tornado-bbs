{$IFNDEF WIN32}
{$F+}
{$ENDIF}
{$I-}
{&Use32-}

Unit
  InstGlob;

Interface

Uses
{$IFDEF WIN32}
  OpCrt,
{$ELSE}
  DOS,
  tWin,
{$ENDIF}
  tMisc;

Type
  tExtrProc     = Procedure (FileName: PathStr);
  tSimpleProc   = Procedure;
  tInstLanguage = (lRussian, lEnglish, lGerman);
  tUserLangs    = Set Of Char;
  tState        = (Menus, TxtFiles, Base, Logos, Util, Docs, CTLs);

{$IFNDEF WIN32}
  ColorSchemeItem =
    (BoxText, Fields, InputLines, Welcome, StatusLine, MenuItems,
     Headers, Frames, Buttons, HighLight, Separator, Progress,
     EmptyLine, Inp, Gray);

  ColorSchemeArray = Array [ColorSchemeItem] Of Byte;
{$ENDIF}

  LanguageItem =
    (Action, StartInst, Quit, Documentation, Interfce, InstallFrom,
     InstallTo, Previous, Upgr, InstallNew, DiskSpace, BytesNeed,
     InstNotFound, TorInst, Archive, mFile, UpdCTL, UpgrCompl1,
     UpgrCompl2, StartUpVal1, StartUpVal2, SysOpName, BBS, Locat,
     Port, Baud, InitStr, AnswerStr, Complete, InvalidPath, LNGfiles);

  LanguageArray = Array [LanguageItem] Of String [80];

  tStVal = Record
    SysOp, Location, BBSName,
    AnswerString, InitString    : String [36];
    ComPort                     : String [1];
    BaudRate                    : LongInt;
  End;

Var
  iPath, InstDir                                : PathStr;
  DocLang, IntfLang                             : tInstLanguage;
  StVal                                         : tStVal;
  VerInstalled                                  : String;
{$IFNDEF WIN32}
  ColorScheme                                   : ColorSchemeArray;
{$ENDIF}

Const
  LangChar : Array [lRussian..lEnglish] Of Char = ('R', 'E');
  LangIndex    : Array [tInstLanguage] Of Char = ('0', '1', '2');
  LastAnswer : Byte = 1;
{$IFDEF WIN32}
  SpaceNeed = 3199552;
{$ELSE}
  SpaceNeed = 2199552;
{$ENDIF}

{$IFNDEF WIN32}
  ColorScheme1 : ColorSchemeArray = ($83, $8F, $83, $8B, $30, $30, $8A, $8F, $2E, $8E, $87, $99, $FF, $30, $87);
  ColorScheme2 : ColorSchemeArray = ($07, $07, $07, $07, $70, $70, $07, $07, $70, $07, $07, $07, $70, $70, $00);
{$ENDIF}

  RussianLang  : LanguageArray    =
    ('Действие', 'Начать инсталляцию', 'Выход', 'Язык документации',
     'Язык программы', 'Директория инсталлятора', 'Инсталлировать в',
     'Пред. версия в каталоге', 'Обновить (не работает) ', 'Установить ',
     'Не хватает места на диске для установки программы.||Необходимо как минимум ',
     ' байт.', 'Невозможно отыскать инсталляционные файлы.|', ' Инсталляция ',
     'Архив:', 'Файл:', 'Обновление .CTL файлов ...',
     'Обновление вашей версии Tornado в версию ',
     '|завершено успешно.', 'Установка стартовых параметров вашей BBS.',
     'Нажмите <Esc> для завершения.', 'Имя СисОпа :', 'Назв. BBS  :',
     'Местонахожд:', 'Порт       :', 'Скорость   :', 'Инит-строка:',
     'Стр. ответа:', ' был установлен|на ваш компьютер успешно.|',
     'Каталог для инсталляции указан неверно.|', 'Пользовательские языки');
  EnglishLang  : LanguageArray    =
    ('Action', 'Start installation', 'Quit', 'Documentation language',
     'Program language', 'Install from', 'Install to',
     'Previous version dir', 'Upgrade (not working)', 'Install new',
     'There is not enought disk space for|installation on this drive.||At least ',
     ' bytes is need.', 'Can''t locate installation files.|', ' Tornado install ',
     'Archive:', 'File:', 'Now updating .CTL files ...', 'Upgrade your Tornado to ',
     ' version|has been completed succesfully.',
     'Now we are going to set up some startup',
     'values. Press <Esc> when ready.', 'SysOp name :', 'BBS name   :',
     'Location   :', 'Comm. port :', 'Baud rate  :', 'Init str.  :',
     'Answer str.:', ' was installed|on your computer successfully.|',
     'Destination path is invalid.|', 'User languages');

  Language     : LanguageArray    = ('', '', '', '', '', '', '', '', '',
                                     '', '', '', '', '', '', '', '', '',
                                     '', '', '', '', '', '', '', '', '',
                                     '', '', '', '');
  lngEnglish   : Boolean = True;
  lngRussian   : Boolean = True;
  lngGerman    : Boolean = True;
  Upgrade      : Boolean = False;
  State        : tState = Base;
  CurrentL     : tInstLanguage = lEnglish;

Procedure UnPackFiles (Extract: tExtrProc);
Procedure UpdConfigs (Status: tSimpleProc);
Function OkFile (Var FName: PathStr): Boolean;

Implementation

Procedure ChangeParam (Var Str: String; NewValue: String);
Var
  Comment         : String;
  ComPos, Spaces  : Byte;

Begin
  ComPos  := Pos (';', Str);

  If ComPos <> 0
  Then
    Comment := Copy (Str, ComPos, 255)
  Else
  Begin
    Comment := '';
    Spaces := 0;
  End;

  Str := ExtractWord (1, Str, [' ']) + ' ' + NewValue;

  If ComPos <> 0 Then
  If ComPos < Length (Str)
  Then
    Spaces := 1
  Else
    Spaces := ComPos-Length (Str)-1;

  Str := Str + Replicate (' ', Spaces) + Comment;
End;

Procedure ReplaceMsgFile;
Begin
  If IntfLang = lRussian Then
  Begin
    tDeleteFile (iPath + '\tornado.msg');
    tCopyFile (iPath + '\tornado.rus', iPath + '\tornado.msg', True);
  End Else
  If IntfLang = lGerman Then
  Begin
    tDeleteFile (iPath + '\tornado.msg');
    tCopyFile (iPath + '\tornado.ger', iPath + '\tornado.msg', True);
  End;
End;

Procedure UnPackFiles (Extract: tExtrProc);
Begin
  If Not DirExists (iPath + '\MENUS') Then MkDir (iPath + '\MENUS');
  If Not DirExists (iPath + '\TXTFILE') Then MkDir (iPath + '\TXTFILE');
  If Not DirExists (iPath + '\UTIL') Then MkDir (iPath + '\UTIL');
  If Not DirExists (iPath + '\DOC') Then MkDir (iPath + '\DOC');
  If Not DirExists (iPath + '\USERTAG') Then MkDir (iPath + '\USERTAG');

  If lngEnglish Then
  Begin
    If Not DirExists (iPath + '\TXTFILE\ENGLISH') Then MkDir (iPath + '\TXTFILE\ENGLISH');
    If Not DirExists (iPath + '\MENUS\ENGLISH') Then MkDir (iPath + '\MENUS\ENGLISH');
  End;

  If lngRussian Then
  Begin
    If Not DirExists (iPath + '\TXTFILE\RUSSIAN') Then MkDir (iPath + '\TXTFILE\RUSSIAN');
    If Not DirExists (iPath + '\MENUS\RUSSIAN') Then MkDir (iPath + '\MENUS\RUSSIAN');
  End;

  If lngGerman Then
  Begin
    If Not DirExists (iPath + '\TXTFILE\GERMAN') Then MkDir (iPath + '\TXTFILE\GERMAN');
    If Not DirExists (iPath + '\MENUS\GERMAN') Then MkDir (iPath + '\MENUS\GERMAN');
  End;

  If IOResult <> 0 Then;

  ChDir (iPath);
  Extract ('TBASE.ZIP');

  State := Menus;

  If lngRussian Then
  Begin
    CurrentL := lRussian;
    ChDir (iPath + '\MENUS\RUSSIAN');
    Extract ('TMISC.ZIP');
  End;

  If lngEnglish Then
  Begin
    CurrentL := lEnglish;
    ChDir (iPath + '\MENUS\ENGLISH');
    Extract ('TMISC.ZIP');
  End;

  If lngGerman Then
  Begin
    CurrentL := lGerman;
    ChDir (iPath + '\MENUS\GERMAN');
    Extract ('TMISC.ZIP');
  End;

  State := Logos;
  ChDir (iPath + '\TXTFILE');
  Extract ('TMISC.ZIP');

  State := TxtFiles;

  If lngRussian Then
  Begin
    CurrentL := lRussian;
    ChDir (iPath + '\TXTFILE\RUSSIAN');
    Extract ('TMISC.ZIP');
  End;

  If lngEnglish Then
  Begin
    CurrentL := lEnglish;
    ChDir (iPath + '\TXTFILE\ENGLISH');
    Extract ('TMISC.ZIP');
  End;

  If lngGerman Then
  Begin
    CurrentL := lGerman;
    ChDir (iPath + '\TXTFILE\GERMAN');
    Extract ('TMISC.ZIP');
  End;

  State := Util;
  ChDir (iPath + '\UTIL');
  Extract ('TUTIL.ZIP');

  State := Docs;
  CurrentL := DocLang;
  ChDir (iPath + '\DOC');
  Extract ('TDOC.ZIP');

  State := CTLs;
  ChDir (iPath);
  Extract ('TCTL.ZIP');

  ReplaceMsgFile;
  ChDir (InstDir);
End;

Procedure UpdConfigs (Status: tSimpleProc);
Var
  F, F1           : Text;
  Str, S, Section : String;

Begin
  Assign (F, AddBackSlash (iPath) + 'tornado.ctl');
  Assign (F1, AddBackSlash (iPath) + 'torctl.$$$');

  ReSet (F);
  ReWrite (F1);

  WriteLn (F1, '; == This config file was modified by Tornado install program ' + StrDate + ' ==');
  WriteLn (F1, ';');

  With StVal Do
  While Not EoF (F) Do
  Begin
    ReadLn (F, Str);
    S := Str;
    If Copy (Trim (S), 1, 1) = '[' Then Section := UpString (ExtractWord (1, Str, ['[', ']', ' ']));

    If Section = 'SYSTEM' Then
    Begin
      If UpString (ExtractWord (1, Str, [' '])) = 'SYSOP' Then ChangeParam (Str, SysOp);
      If UpString (ExtractWord (1, Str, [' '])) = 'BBSNAME' Then ChangeParam (Str, BBSName);
      If UpString (ExtractWord (1, Str, [' '])) = 'LOCATION' Then ChangeParam (Str, Location);
      If UpString (ExtractWord (1, Str, [' '])) = 'PATH' Then ChangeParam (Str, iPath);
    End Else
    If Section = 'MODEM' Then
    Begin
      If UpString (ExtractWord (1, Str, [' '])) = 'INITSTRING' Then ChangeParam (Str, InitString);
      If UpString (ExtractWord (1, Str, [' '])) = 'ANSWERSTRING' Then ChangeParam (Str, AnswerString);
      If UpString (ExtractWord (1, Str, [' '])) = 'PORT' Then ChangeParam (Str, ComPort);
      If UpString (ExtractWord (1, Str, [' '])) = 'BAUDRATE' Then ChangeParam (Str, Long2Str (BaudRate));
    End Else
    If Section = 'FILES&DIRS' Then
    Begin
      If UpString (ExtractWord (1, Str, [' '])) = 'TEMPDIR' Then ChangeParam (Str, 'C:\');
      If UpString (ExtractWord (1, Str, [' '])) = 'LOGFILE' Then ChangeParam (Str, 'tornado.log');
    End;

    WriteLn (F1, Str);
    If (S <> Str) And Assigned (Status) Then Status;
  End;

  Close (F);  Erase (F);
  Close (F1); Rename (F1, AddBackSlash (iPath) + 'tornado.ctl');
End;

Function OkFile (Var FName: PathStr): Boolean;
Var
  Ext : String [3];
  Res : Boolean;
  oFN : PathStr;

Label
  EoP;

Begin
  Res := True;
  Ext := UpString (JustExtension (FName));
  oFN := FName;

  If State = Base Then
  If (Not lngEnglish and (FName = 'ENGLISH.LNG')) Or
     (Not lngGerman and (FName = 'GERMAN.LNG')) Or
     (Not lngRussian and (FName = 'RUSSIAN.LNG')) Then
  Begin
    OkFile := False;
    Exit;
  End Else
  If (Ext = 'TOR') and not FileExists (FName) and Upgrade Then
  Begin
    OkFile := True;
    Exit;
  End;

  If (State = Logos) And Not Upgrade Then
  Begin
    OkFile := (FName = 'LOGO.ANS') Or (FName = 'LOGO.ASC') Or (FName = 'LOGO.AVT');
    Exit;
  End;

  If State in [Menus, TxtFiles, Docs, CTLs] Then
  If Ext [Length (Ext)] = LangIndex [CurrentL] Then
  Case State Of
    Menus    : If Copy (Ext, 1, 2) = 'MN' Then FName := Copy (FName, 1, Length (FName)-1) + 'U';

    TxtFiles : Begin
                 If Copy (Ext, 1, 2) = 'AN' Then FName := Copy (FName, 1, Length (FName)-1) + 'S';
                 If Copy (Ext, 1, 2) = 'AS' Then FName := Copy (FName, 1, Length (FName)-1) + 'C';
                 If Copy (Ext, 1, 2) = 'AV' Then FName := Copy (FName, 1, Length (FName)-1) + 'T';
                 If Copy (Ext, 1, 2) = 'TR' Then FName := Copy (FName, 1, Length (FName)-1) + 'S';
               End;

    Docs     : Begin
                 If Copy (Ext, 1, 2) = 'DO' Then FName := Copy (FName, 1, Length (FName)-1) + 'C';
                 If Copy (Ext, 1, 2) = 'FA' Then FName := Copy (FName, 1, Length (FName)-1) + 'Q';
                 If Copy (Ext, 1, 2) = 'RE' Then FName := Copy (FName, 1, Length (FName)-1) + 'G';
               End;

    CTLs     : Begin
                 If Copy (Ext, 1, 2) = 'CT' Then FName := Copy (FName, 1, Length (FName)-1) + 'L';
                 If Copy (Ext, 1, 2) = 'UP' Then FName := Copy (FName, 1, Length (FName)-1) + 'D';
               End;

  End Else
  Begin
    OkFile := False;
    Exit;
  End;

  Ext := UpString (JustExtension (FName));

  If Not Upgrade Then
  Begin
    If Ext = 'UPD' Then
    Begin
      OkFile := False;
      Exit;
    End;

  {$IFNDEF WIN32}
    If FileExists (FName) Then
    Begin
      If LastAnswer = 3 Then
      Begin
        OkFile := True;
        Exit;
      End;

      LastAnswer := SelectBox ('Yes|No|All', ColorScheme [Fields], ColorScheme [MenuItems],
      ColorScheme [Headers], 5, 'Owerwrite file ' + Trim (NiceFileName (FName, 20)) + ' ?');

      OkFile := LastAnswer in [3, 1];
      Exit;
    End;
  {$ENDIF}

  End Else
  Begin
    Res := (Ext = 'EXE') Or
           (Ext = 'DOC') Or
           (Ext = 'OVR') Or
           (Ext = 'FAQ') Or
           (Ext = 'MSG') Or
           (Ext = 'RES') Or
           (Ext = 'RUS') Or
           (Ext = 'GER') Or
           (Ext = 'ENG');

    Res := Res Or (Ext = 'UPD') Or ((Ext = 'CTL') And
    Not FileExists (AddBackSlash (iPath) + FName));
              {распаковываем всякий не существующий CTL}

    OkFile := Res;
    Exit;
  End;

  EoP:
  OkFile := Not ((FName = oFN) And (State in [Menus, TxtFiles, DOCs, CTLs]));
End;

End.