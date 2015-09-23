@echo off
rem Builds and packages Wakan completely.
call setupvars.cmd
if errorlevel 1 goto setupvars_fail
pushd ..

rem TODO: Rebuild dependencies?
rem TODO: Run dependencies' tests?

rem TODO: Update version number.


rem Build
set BINOUT=%~dp0..\Release
set JALETTEST_DEFINES=CONSOLE_TESTRUNNER

rem Build configuration:
rem   /p:OutputPath      build everything into a clean folder.
rem   /p:DCC_ExeOutput   same (Delphi needs this)
rem   /p:DCC_BplOutput   can't rebuild BPLs if Delphi is running, but Wakan is static linked so we only want fresh DCUs -- put BPLs wherever.
set BUILDCONF=/t:build /p:config=Release /p:OutputPath="%BINOUT%" /p:DCC_ExeOutput="%BINOUT%" /p:DCC_BplOutput="%BINOUT%"

msbuild Wakan.groupproj %BUILDCONF%
if errorlevel 1 goto end


rem Run tests
"%BINOUT%\JaletTests.exe"
if errorlevel 1 goto end


rem TODO: Update standard dicts? "wakan /updatedics ..."
rem TODO: Update + rebuild character info? "wakan /updatechars ..."
rem TODO: Rebuild rad/sod? (maybe even in other dir)


rem Package setup
iscc "%~dp0wakan-setup.iss" /O+
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