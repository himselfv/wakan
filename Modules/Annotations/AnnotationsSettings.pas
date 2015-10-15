unit AnnotationsSettings;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, JWBSettings, IniFiles;

type
  TAnnotationsSettingsPage = class(TForm)
    cbEnableAnnotations: TCheckBox;
    Bevel1: TBevel;
    cbRebuildAnnotations: TCheckBox;
    cbAnnotateWithPictures: TCheckBox;
    cbAnnotateWithWebPages: TCheckBox;
    cbAnnotateWithColors: TCheckBox;
    cbAnnotateWithSound: TCheckBox;
    btnShowHelp: TButton;
    procedure btnShowHelpClick(Sender: TObject);
  protected
    procedure WmLoadSettings(var Msg: TMessage); message WM_LOADSETTINGS;
    procedure WmSaveSettings(var Msg: TMessage); message WM_SAVESETTINGS;
    procedure LoadSettings(ini: TCustomIniFile);
    procedure SaveSettings(ini: TCustomIniFile);
  end;

var
  AnnotationsSettingsPage: TAnnotationsSettingsPage;

procedure Register;

implementation
uses JWBCore;

{$R *.dfm}

procedure Register;
begin
  AnnotationsSettingsPage := TAnnotationsSettingsPage.Create(fSettings);
  fSettings.AddSettingsPage(AnnotationsSettingsPage, '');
end;

procedure TAnnotationsSettingsPage.WmLoadSettings(var Msg: TMessage);
begin
  LoadSettings(TCustomIniFile(msg.WParam));
end;

procedure TAnnotationsSettingsPage.WmSaveSettings(var Msg: TMessage);
begin
  SaveSettings(TCustomIniFile(msg.WParam));
end;

procedure TAnnotationsSettingsPage.LoadSettings(ini: TCustomIniFile);
begin
  cbEnableAnnotations.Checked := ini.ReadBool('Annotate', 'Enabled', true);
  cbRebuildAnnotations.Checked := ini.ReadBool('Annotate', 'Rebuild', true);
  cbAnnotateWithSound.Checked := ini.ReadBool('Annotate', 'Sound', true);
  cbAnnotateWithPictures.Checked := ini.ReadBool('Annotate', 'Pictures', true);
  cbAnnotateWithWebPages.Checked := ini.ReadBool('Annotate', 'WebPages', true);
  cbAnnotateWithColors.Checked := ini.ReadBool('Annotate', 'Colors', true);
end;

procedure TAnnotationsSettingsPage.SaveSettings(ini: TCustomIniFile);
begin
  ini.WriteBool('Annotate', 'Enabled', cbEnableAnnotations.Checked);
  ini.WriteBool('Annotate', 'Rebuild', cbRebuildAnnotations.Checked);
  ini.WriteBool('Annotate', 'Sound', cbAnnotateWithSound.Checked);
  ini.WriteBool('Annotate', 'Pictures', cbAnnotateWithPictures.Checked);
  ini.WriteBool('Annotate', 'WebPages', cbAnnotateWithWebPages.Checked);
  ini.WriteBool('Annotate', 'Colors', cbAnnotateWithColors.Checked);
end;

procedure TAnnotationsSettingsPage.btnShowHelpClick(Sender: TObject);
begin
  ShellOpen(WikiUrl('Annotations'));
end;

end.
