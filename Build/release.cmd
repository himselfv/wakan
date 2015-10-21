@echo off
rem Uploads current version to the release folder on Google Drive,
rem ups the version number and commits it as release.
call setupvars.cmd
if errorlevel 1 goto setupvars_fail

filever.py ..\Release\wakan.exe --major > setupver.txt
if errorlevel 1 goto end
set /P VERSION= < setupver.txt
del /Q setupver.txt
set SETUPFILENAME=Output\wakan-%VERSION%-setup.exe
echo Press any key to upload %SETUPFILENAME%
pause


upload.py %SETUPFILENAME% --folderid %WAKANFOLDERID%
if errorlevel 1 goto end

echo.
echo Upload completed. Release link:
echo.
echo   https://googledrive.com/host/%WAKANFOLDERID%/%SETUPFILENAME%
echo.
echo Please commit the code to the repository now and up the version number.
rem TODO: Automaticaly up the version number, commit the code.
echo.
goto end


:setupvars_fail
echo.
echo To build Wakan, please copy setupvars.cmd.example as setupvars.cmd and customize paths.
echo.

:end
pause