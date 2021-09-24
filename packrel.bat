@echo off

set VERSION=1.71.2

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
echo   SRC - Source code package
echo.
echo Example: %0 TW
goto end

:tw
call clean.bat
call build_vp.bat tw release
call build_vp.bat res release
call build_vp.bat uw release
echo Tornado/W32 %VERSION%/Release>out\file_id.diz
echo Executables and resources only>>out\file_id.diz
rar a -cl -k -ep Release\tornado-%VERSION%-win32.rar out\file_id.diz bin\* bin\useredit.exe doc\tornado\*
goto end

:tf
call clean.bat
call build_vp.bat tf release
call build_vp.bat res release
call build_bp.bat ud release
echo Tornado/386 %VERSION%/Release>out\file_id.diz
echo Executables and resources only>>out\file_id.diz
rar a -cl -k -ep Release\tornado-%VERSION%-386.rar out\file_id.diz bin\* bin\useredit.exe doc\tornado\* d:\dos\dos32a\binw\dos32a.exe
goto end

:td
call clean.bat
call build_bp.bat td release
call build_bp.bat res release
call build_bp.bat ud release
echo Tornado/DOS %VERSION%/Release>out\file_id.diz
echo Executables and resources only>>out\file_id.diz
rar a -cl -k -ep Release\tornado-%VERSION%-dos.rar out\file_id.diz bin\* bin\useredit.exe doc\tornado\*
goto end

:tp
call clean.bat
call build_bp.bat tp release
call build_bp.bat res release
call build_bp.bat ud release
echo Tornado/DPMI %VERSION%/Release>out\file_id.diz
echo Executables and resources only>>out\file_id.diz
rar a -cl -k -ep Release\tornado-%VERSION%-dpmi.rar out\file_id.diz bin\* bin\useredit.exe doc\tornado\*
goto end

:src
:ts
call clean.bat
echo Tornado %VERSION%/Release source package>file_id.diz
rar a -k Release\tornado-%VERSION%-src.rar @src.lst
del /q file_id.diz

:end
