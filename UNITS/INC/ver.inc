Const
  NameVer : String [40] = 'Tornado'
                          {$IFDEF OS2}
                          + '/OS2'
                          {$ENDIF}

                          {$IFDEF WIN32}
                          + '/W32'
                          {$ENDIF}

                          {$IFDEF MSDOS}
                          {$IFDEF DPMI}
                          + '/DPMI'
                          {$ELSE}
                          {$IFDEF DPMI32}
                          + '/386'
                          {$ELSE}
                          + '/DOS'
                          {$ENDIF}
                          {$ENDIF}
                          {$ENDIF}

                          + ' 1.71.4' +
                          {$IFDEF DEBUG}
                          '/WIP/' +
                          {$I cdate.inc}
                          {$ELSE}
                          '/Release'
                          {$ENDIF};
