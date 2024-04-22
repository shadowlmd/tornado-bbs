@echo off

set VERSION=1.71.4
for /F %%i in ('date /T') do set DATE=%%i

if not exist RELEASE\* mkdir RELEASE

if .%1 == . goto help
goto %1

:help
echo Usage: %0 package
echo Package may be one of those:
echo   TW  - Tornado/W32 binary package
echo   TF  - Tornado/386 binary package
echo   TD  - Tornado/DOS binary package
echo   TP  - Tornado/DPMI binary package
echo   UW  - Useredit/W32 binary package
echo   UD  - Useredit/DOS binary package
echo   SRC - Source code package
echo.
echo Example: %0 TW
goto end

:tw
call clean.bat
call build_vp.bat tw
call build_vp.bat res
echo Tornado/W32 %VERSION%/WIP/%DATE%>out\file_id.diz
echo Executables and resources only>>out\file_id.diz
rar a -cl -k -ep -agymmdd{w} Release\tn out\file_id.diz bin\* doc\tornado\*
goto end

:tf
call clean.bat
call build_vp.bat tf
call build_vp.bat res
echo Tornado/386 %VERSION%/WIP/%DATE%>out\file_id.diz
echo Executables and resources only>>out\file_id.diz
rar a -cl -k -ep -agymmdd{f} Release\tn out\file_id.diz bin\* doc\tornado\* d:\tools\dos32a\binw\dos32a.exe
goto end

:td
call clean.bat
call build_bp.bat td
call build_bp.bat res
call build_vp.bat dll
echo Tornado/DOS %VERSION%/WIP/%DATE%>out\file_id.diz
echo Executables and resources only>>out\file_id.diz
rar a -cl -k -ep -agymmdd{d} Release\tn out\file_id.diz bin\* doc\tornado\*
goto end

:tp
call clean.bat
call build_bp.bat tp
call build_bp.bat res
call build_vp.bat dll
echo Tornado/DPMI %VERSION%/WIP/%DATE%>out\file_id.diz
echo Executables and resources only>>out\file_id.diz
rar a -cl -k -ep -agymmdd{p} Release\tn out\file_id.diz bin\* doc\tornado\* d:\dos\rtm.exe d:\dos\dpmi16bi.ovl
goto end

:uw
call clean.bat
call build_vp.bat uw
echo User editor for Tornado BBS [W32]>out\file_id.diz
echo Build date: %DATE%>>out\file_id.diz
rar a -cl -k -ep -agymmdd{w} Release\ue out\file_id.diz bin\useredit.exe doc\useredit\*
goto end

:ud
call clean.bat
call build_bp.bat ud
echo User editor for Tornado BBS [DOS]>out\file_id.diz
echo Build date: %DATE%>>out\file_id.diz
rar a -cl -k -ep -agymmdd{d} Release\ue out\file_id.diz bin\useredit.exe doc\useredit\*
goto end

:src
:ts
call clean.bat
echo Tornado %VERSION%/WIP/%DATE% source package>file_id.diz
rar a -k -agymmdd{s} Release\tn @src.lst
del /q file_id.diz

:end
