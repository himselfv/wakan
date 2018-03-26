program WakanTests;
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
  JWBCore in 'Modules\JWBCore.pas',
  JWBSettings in 'Modules\Settings\JWBSettings.pas' {fSettings},
  JWBLegacyMarkup in 'Modules\Vocab\JWBLegacyMarkup.pas',
  JWBRomaSort in 'Modules\Vocab\JWBRomaSort.pas',
  WakanCfg in 'Modules\WakanCfg.pas',
  JWBDic,
  JWBDictionaries,
  TextTableTests in 'Modules\Package\TextTableTests.pas',
  JWBWakanTextTests in 'Modules\Editor\JWBWakanTextTests.pas',
  JWBComponentsTests in 'Modules\Components\JWBComponentsTests.pas',
  RaineRadicalsTests in 'Modules\KanjiList\RaineRadicalsTests.pas',
  UpgradeFilesTests in 'Modules\LocalData\UpgradeFilesTests.pas',
  JWBDicTests in 'Modules\Dictionary\JWBDicTests.pas',
  JWBDicSearchTests in 'Modules\Dictionary\JWBDicSearchTests.pas';

{$R *.RES}

var
  ExitBehavior: TRunnerExitBehavior;

begin
  Application.Initialize;

  //Initialize wakan global environment
  //We prefer to load things locally but it's not always possible.
  LoadWakanCfg(AppFolder + '\wakan.cfg');
  fSettings := TfSettings.Create(nil);
  //Load exactly one standard dictionary
  dicts.Add(dicts.NewDict(AppFolder+'\edict2.dic'));
  dicts.PutInGroup(dicts[0], 1, true);
  dicts[0].Load;

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

