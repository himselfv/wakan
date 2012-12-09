unit JWBWordDetails;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Buttons, ExtCtrls;

type
  TfWordDetails = class(TForm)
    Shape2: TShape;
    PaintBox1: TPaintBox;
    Shape3: TShape;
    PaintBox2: TPaintBox;
    SpeedButton23: TSpeedButton;
    Shape5: TShape;
    PaintBox5: TPaintBox;
    Bevel1: TBevel;
    procedure PaintBox1Paint(Sender: TObject);
    procedure PaintBox2Paint(Sender: TObject);
    procedure PaintBox5Paint(Sender: TObject);
    procedure SpeedButton23Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  end;

var
  fWordDetails: TfWordDetails;

implementation

uses JWBUser, JWBMenu;

{$R *.DFM}

procedure TfWordDetails.PaintBox1Paint(Sender: TObject);
begin
  fUser.WordDetails_PaintBox1Paint(sender);
end;

procedure TfWordDetails.PaintBox2Paint(Sender: TObject);
begin
  fUser.WordDetails_PaintBox2Paint(sender);
end;

procedure TfWordDetails.PaintBox5Paint(Sender: TObject);
begin
  fUser.WordDetails_PaintBox5Paint(sender);
end;

procedure TfWordDetails.SpeedButton23Click(Sender: TObject);
begin
  fUser.WordDetails_SpeedButton23Click(sender);
end;

procedure TfWordDetails.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  fUser.SpeedButton5.Down:=false;
  fMenu.aDictDetails.Checked:=false;
end;

end.
