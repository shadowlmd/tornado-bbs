@echo off

if not exist out\* mkdir OUT
if not exist bin\* mkdir BIN

set BPC=d:\prog\pascal\bp\bin\bpc.exe
set RELEASE=%2

if not .%RELEASE%==.release set BPC=%BPC% -DDEBUG

if .%1==. goto help
goto %1

:help
echo Usage: %0 target
echo Target may be one of those:
echo   RES - Tornado resources
echo   TD  - Tornado/DOS executables
echo   TP  - Tornado/DPMI executables
echo   UD  - Useredit/DOS
goto end

:r
:res
rem cdate.inc
if exist out\cdate.exe goto cdate
%BPC% -EOUT -M cdate.pas
if errorlevel 1 goto error
:cdate
out\cdate.exe UNITS\INC\cdate.inc

rem resources
if exist out\tmsgc.exe goto tmsgc
%BPC% -EOUT -M tmsgc.pas
if errorlevel 1 goto error
:tmsgc
out\tmsgc.exe resource\prms_rus resource\text_rus resource\help_rus bin\tornado.rus
out\tmsgc.exe resource\prms_eng resource\text_eng resource\help_eng bin\tornado.msg
rem out\tmsgc.exe resource\prms_ger resource\text_ger resource\help_ger bin\tornado.ger
rem out\tmsgc.exe resource\prms_fre resource\text_fre resource\help_fre bin\tornado.fre
goto end

:d
:td
rem cdate.inc
if exist out\cdate.exe goto cdate
%BPC% -EOUT -M cdate.pas
if errorlevel 1 goto error
:cdate
out\cdate.exe UNITS\INC\cdate.inc

rem tornado/dos
%BPC% -CD -$F+ -EOUT -B tornado.pas
if errorlevel 1 goto error
move out\tornado.ovr bin\
move out\tornado.exe bin\
echo.
echo Don't forget to compile resources ;)
goto end

:p
:tp
rem cdate.inc
%BPC% -EOUT -M cdate.pas
if errorlevel 1 goto error
out\cdate.exe UNITS\INC\cdate.inc

rem tornado/dpmi
%BPC% -CP -DMSDOS -EOUT -B tornado.pas
if errorlevel 1 goto error
move out\tornado.exe bin\
echo.
echo Don't forget to compile resources ;)
goto end

:u
:ud
rem useredit
%BPC% -CD -EOUT -B useredit.pas
if errorlevel 1 goto error
move out\useredit.exe bin\
goto end

:error
echo Error occured!

:end
