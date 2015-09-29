unit PortabilitySettings;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls,
  SimpleControls, JWBSettings;

type
  TPortabilitySettingsPage = class(TForm)
    lblWakanMode: TLabel;
    Label49: TLabel;
    Label55: TLabel;
    Label56: TLabel;
    lblSettingsPath: TUrlLabel;
    lblDictionariesPath: TUrlLabel;
    lblUserDataPath: TUrlLabel;
    lblUpgradeToStandalone: TLabel;
    lblBackupPath: TUrlLabel;
    Label59: TLabel;
    btnUpgradeToStandalone: TButton;
    procedure lblSettingsPathClick(Sender: TObject);
    procedure btnUpgradeToStandaloneClick(Sender: TObject);
  protected
    procedure UpdatePage;
    procedure WmLoadSettings(var Msg); message WM_LOADSETTINGS;
  end;

var
  PortabilitySettingsPage: TPortabilitySettingsPage;

procedure Register;

implementation
uses IniFiles, JWBStrings, AppData, JWBCore, JWBLanguage;

{$R *.dfm}

procedure Register;
var sheet: TPortabilitySettingsPage;
begin
  sheet := TPortabilitySettingsPage.Create(fSettings);
  fSettings.AddSettingsPage(sheet, 'General');
end;

procedure TPortabilitySettingsPage.WmLoadSettings(var Msg);
begin
  UpdatePage;
end;

procedure TPortabilitySettingsPage.UpdatePage;
begin
  case PortabilityMode of
    pmStandalone: lblWakanMode.Caption := _l('#01026^eWakan is running in standalone mode');
    pmCompatible: lblWakanMode.Caption := _l('#01027^eWakan is running in compatible mode');
    pmPortable: lblWakanMode.Caption := _l('#01028^eWakan is running in portable mode');
  else
    lblWakanMode.Caption := '[error]';
  end;

  if PortabilityMode=pmPortable then begin
    lblSettingsPath.Caption := AppFolder + '\wakan.ini';
    lblSettingsPath.URL := 'file://'+repl(lblSettingsPath.Caption,'\','/');
  end else begin
    lblSettingsPath.Caption := 'HKEY_CURRENT_USER\'+WakanRegKey;
    lblSettingsPath.URL := '';
  end;

  lblDictionariesPath.Caption := DictionaryDir;
  lblDictionariesPath.URL := 'file://'+repl(DictionaryDir,'\','/');
  lblUserDataPath.Caption := UserDataDir;
  lblUserDataPath.URL := 'file://'+repl(UserDataDir,'\','/');
  lblBackupPath.Caption := BackupDir;
  lblBackupPath.URL := 'file://'+repl(BackupDir,'\','/');

  btnUpgradeToStandalone.Visible := PortabilityMode=pmCompatible;
  lblUpgradeToStandalone.Visible := btnUpgradeToStandalone.Visible;
end;

procedure TPortabilitySettingsPage.lblSettingsPathClick(Sender: TObject);
begin
  if PortabilityMode<>pmPortable then
    RegeditAtKey(lblSettingsPath.Caption);
end;

procedure TPortabilitySettingsPage.btnUpgradeToStandaloneClick(Sender: TObject);
var ini: TCustomIniFile;
begin
  if PortabilityMode<>pmCompatible then exit;

  if Application.MessageBox(
    PChar(_l('#01034^eDo you really want to move all your files to Application Data and convert this installation to standalone?')),
    PChar(_l('#01035^eConfirmation')),
    MB_YESNO or MB_ICONQUESTION
  ) <> ID_YES then exit;

  ini := GetWakanIni;
  try
    ini.WriteString('General','Install','Upgrade');
    ini.UpdateFile;
  finally
    FreeSettings;
  end;

  //Localize
  Application.MessageBox(
    PChar(_l('#01036^eWakan has to be restarted for the upgrade to be completed. The application will now exit.')),
    PChar(_l('#01037^eRestart needed')),
    MB_OK
  );
  Application.Terminate;
end;

end.
