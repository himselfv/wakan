unit JWBClipboard;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, Buttons, StdCtrls;

type
  TfClipboard = class(TForm)
    procedure FormCreate(Sender: TObject);
  end;

var
  fClipboard: TfClipboard;

implementation

uses JWBMenu;

{$R *.DFM}

procedure TfClipboard.FormCreate(Sender: TObject);
begin
  fMenu.ResFixVal:=76-ClientHeight;
end;

end.
