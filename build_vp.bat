@echo off

if not exist out\* mkdir OUT
if not exist bin\* mkdir BIN

set VPC=vpc.exe
set RELEASE=%2

if not .%RELEASE%==.release set VPC=%VPC% -DDEBUG

if .%1==. goto help
goto %1

:help
echo Usage: %0 target
echo Target may be one of those:
echo   RES - Tornado resources
echo   TW  - Tornado/W32 executables
echo   TF  - Tornado/386 executables
echo   UW  - Useredit/W32
echo  DLL  - NTVDMCLP.DLL
goto end

:r
:res
rem cdate.inc
if exist out\cdate.exe goto cdate
%VPC% -Vvp.vpo -M -CW cdate.pas
if errorlevel 1 goto error
:cdate
out\cdate.exe UNITS\INC\cdate.inc

rem resources
if exist out\tmsgc.exe goto tmsgc
%VPC% -Vvp.vpo -M -CW tmsgc.pas
if errorlevel 1 goto error
:tmsgc
out\tmsgc.exe resource\prms_rus resource\text_rus resource\help_rus bin\tornado.rus
out\tmsgc.exe resource\prms_eng resource\text_eng resource\help_eng bin\tornado.msg
rem out\tmsgc.exe resource\prms_ger resource\text_ger resource\help_ger bin\tornado.ger
rem out\tmsgc.exe resource\prms_fre resource\text_fre resource\help_fre bin\tornado.fre
goto end

:w
:tw
rem cdate.inc
if exist out\cdate.exe goto cdate
%VPC% -Vvp.vpo -M -CW cdate.pas
if errorlevel 1 goto error
:cdate
out\cdate.exe UNITS\INC\cdate.inc

rem tornado
%VPC% -Vvp.vpo -B -CW -EBIN tornado.pas
if errorlevel 1 goto error
echo.
echo Don't forget to compile resources ;)
goto end

:f
:tf
rem cdate.inc
if exist out\cdate.exe goto cdate
%VPC% -Vvp.vpo -M -CW cdate.pas
if errorlevel 1 goto error
:cdate
out\cdate.exe UNITS\INC\cdate.inc

rem tornado
%VPC% -Vvp_d32.vpo -B -CW:D32:DPMI32 -DMSDOS tornado.pas
if errorlevel 1 goto error
d:\prog\pascal\vp_d32\bin.w32\pe2le.exe OUT\tornado.exe BIN\tornado.exe /S:D:\tools\dos32a\binw\stub32a.exe /Q
echo.
echo Don't forget to compile resources ;)
goto end

:u
:uw
rem useredit
%VPC% -Vvp.vpo -B -CW -EBIN useredit.pas
if errorlevel 1 goto error
goto end

:l
:dll
rem ntvdmclp.dll
%VPC% -Vvp_clean.vpo -B -CW -EBIN ntvdmclp.pas
if errorlevel 1 goto error
goto end

:error
echo Error occured!

:end
