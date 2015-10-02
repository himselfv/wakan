program JaletTests;
{

  Delphi DUnit Test Project
  -------------------------
  This project contains the DUnit test framework and the GUI/Console test runners.
  Add "CONSOLE_TESTRUNNER" to the conditional defines entry in the project options
  to use the console test runner.  Otherwise the GUI test runner will be used by
  default.

  Global conditionals:
  HARDCORE
    Adds hardcore versions of some tests. Principally they're the same, but with
    more load and therefore slower.
    May help to catch errors which only occur in heavy load conditions. Set when
    running final checks before releasing.

}

{$IFDEF CONSOLE_TESTRUNNER}
{$APPTYPE CONSOLE}
{$ENDIF}

uses
  SysUtils,
  Forms,
  TestFramework,
  GUITestRunner,
  TextTestRunner,
  TestingCommon,
  TextTableTests in 'Modules\Package\TextTableTests.pas',
  JWBWakanTextTests in 'Modules\Editor\JWBWakanTextTests.pas',
  JWBComponentsTests in 'Modules\Components\JWBComponentsTests.pas',
  RaineRadicalsTests in 'Modules\KanjiList\RaineRadicalsTests.pas';

{$R *.RES}

var
  ExitBehavior: TRunnerExitBehavior;

begin
  Application.Initialize;
  if IsConsole then begin
    if FindCmdLineSwitch('halt') then
      ExitBehavior := rxbHaltOnFailures
    else
      ExitBehavior := rxbContinue;
    with TextTestRunner.RunRegisteredTests(ExitBehavior) do
      Free;
    if FindCmdLineSwitch('pause') then
      readln;
  end else
    GuiTestRunner.RunRegisteredTests;
end.

