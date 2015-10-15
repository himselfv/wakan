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
    lblUpgradeLocalData: TLabel;
    lblBackupPath: TUrlLabel;
    Label59: TLabel;
    btnUpgradeLocalData: TButton;
    procedure lblSettingsPathClick(Sender: TObject);
    procedure btnUpgradeLocalDataClick(Sender: TObject);
  protected
    procedure UpdatePage;
    procedure WmLoadSettings(var Msg); message WM_LOADSETTINGS;
  end;

var
  PortabilitySettingsPage: TPortabilitySettingsPage;

procedure Register;

implementation
uses IniFiles, JWBStrings, AppData, JWBCore, JWBLanguage, UpgradeFiles;

{$R *.dfm}

procedure Register;
begin
  PortabilitySettingsPage := TPortabilitySettingsPage.Create(fSettings);
  fSettings.AddSettingsPage(PortabilitySettingsPage, 'General');
end;

procedure TPortabilitySettingsPage.WmLoadSettings(var Msg);
begin
  UpdatePage;
end;

procedure TPortabilitySettingsPage.UpdatePage;
begin
  case PortabilityMode of
    pmStandalone: lblWakanMode.Caption := _l('#01026^eWakan is running in standalone mode');
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
end;

procedure TPortabilitySettingsPage.lblSettingsPathClick(Sender: TObject);
begin
  if PortabilityMode<>pmPortable then
    RegeditAtKey(lblSettingsPath.Caption);
end;

procedure TPortabilitySettingsPage.btnUpgradeLocalDataClick(Sender: TObject);
begin
  if UpgradeLocalData() then begin
    Application.MessageBox(
      PChar(_l('#01036^eWakan has to be restarted for the upgrade to be completed. The application will now exit.')),
      PChar(_l('#01037^eRestart needed')),
      MB_OK
    );
    Application.Terminate;
  end else
    Application.MessageBox(
      PChar(_l('#01229^There was nothing to upgrade.')),
      PChar(Self.Caption),
      MB_YESNO or MB_ICONQUESTION
    );
end;

end.
