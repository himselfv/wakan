unit JWBWordCategory;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls;

type
  TfWordCategory = class(TForm)
    Label30: TLabel;
    Label32: TLabel;
    Label34: TLabel;
    Label36: TLabel;
    Label37: TLabel;
    Label54: TLabel;
    RxLabel9: TLabel;
    Label55: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Bevel1: TBevel;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  end;

var
  fWordCategory: TfWordCategory;

implementation

uses JWBUser, JWBMenu;

{$R *.DFM}

procedure TfWordCategory.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  fMenu.aDictCategories.Checked:=false;
end;

end.
