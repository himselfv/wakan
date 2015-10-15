@echo off
rem Builds and packages Wakan completely.
call setupvars.cmd
if errorlevel 1 goto setupvars_fail
pushd ..

set BINOUT=%~dp0..\Release
set UNITTEST_DEFINES=CONSOLE_TESTRUNNER


echo Building jp-tools...
msbuild %JPTOOLS%\Tests\Tests.dproj /t:build /p:config=Release
if errorlevel 1 goto end

echo Testing jp-tools
"%JPTOOLS%\Release\Tests.exe" /nospeed /halt
if errorlevel 1 goto end


rem Build configuration:
rem   /p:OutputPath      build everything into a clean folder.
rem   /p:DCC_ExeOutput   same (Delphi needs this)
rem   /p:DCC_BplOutput   can't rebuild BPLs if Delphi is running, but Wakan is static linked so we only want fresh DCUs -- put BPLs wherever.
msbuild Wakan.groupproj /t:build /p:config=Release /p:OutputPath="%BINOUT%" /p:DCC_ExeOutput="%BINOUT%" /p:DCC_BplOutput="%BINOUT%"
if errorlevel 1 goto end

rem Running tests...
"%BINOUT%\WakanTests.exe" /nospeed /halt
if errorlevel 1 goto end


rem TODO: Update standard dicts? "wakan /updatedics ..."
rem TODO: Update + rebuild character info? "wakan /updatechars ..."


echo.
echo Rebuilding RAD/SOD from sources...
cd "%BINOUT%"
"%BINOUT%\Wakan.exe" makerad
if errorlevel 1 goto end
"%BINOUT%\Wakan.exe" makesod
if errorlevel 1 goto end


echo.
echo Packaging installation...
iscc "%~dp0wakan-setup.iss" /O+
echo.
echo.
rem TODO: Include: history.txt, licence.txt?


rem TODO: Automaticaly up the version number, commit the code.

:success
echo.
echo Build completed.
echo Please commit the code to the repository now and up the version number.
echo.
goto end


:setupvars_fail
echo.
echo To build Wakan, please copy setupvars.cmd.example as setupvars.cmd and customize paths.
echo.

:end
popd
pause