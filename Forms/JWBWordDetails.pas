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
    btnAddToClipboard: TSpeedButton;
    Shape5: TShape;
    PaintBox5: TPaintBox;
    Bevel1: TBevel;
    procedure PaintBox1Paint(Sender: TObject);
    procedure PaintBox2Paint(Sender: TObject);
    procedure PaintBox5Paint(Sender: TObject);
    procedure btnAddToClipboardClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  end;

var
  fWordDetails: TfWordDetails;

implementation

uses JWBUser, JWBMenu, JWBStrings, JWBUnit;

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
  PaintBox5.Canvas.Brush.Color:=clWindow;
  DrawUnicode(PaintBox5.Canvas,1,1,22,fstr(fUser.curmeaning),FontEnglish);
end;

procedure TfWordDetails.btnAddToClipboardClick(Sender: TObject);
begin
  fUser.btnCopyToClipboardClick(Sender);
end;

procedure TfWordDetails.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  fUser.SpeedButton5.Down:=false;
  fMenu.aDictDetails.Checked:=false;
end;

end.
