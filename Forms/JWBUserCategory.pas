unit JWBUserCategory;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, CheckLst, Tabs, Buttons, ExtCtrls;

type
  TfUserCategory = class(TForm)
    procedure ListBox1Click(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure SpeedButton3Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  fUserCategory: TfUserCategory;

implementation

uses JWBWords, JWBMenu, JWBUnit;

{$R *.DFM}

procedure TfUserCategory.ListBox1Click(Sender: TObject);
begin
  fWords.ShowIt(false);
end;

procedure TfUserCategory.SpeedButton2Click(Sender: TObject);
begin
  fWords.UserCategory_SpeedButton2Click(sender);
end;

procedure TfUserCategory.SpeedButton3Click(Sender: TObject);
begin
  fWords.UserCategory_SpeedButton3Click(sender);
end;

procedure TfUserCategory.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  fWords.SpeedButton3.Down:=false;
  fMenu.aUserCategory.Checked:=false;
end;

end.
