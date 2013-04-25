unit JWBWordDetails;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Buttons, ExtCtrls, WakanPaintbox;

type
  TfWordDetails = class(TForm)
    Bevel: TPanel;
    Paintbox5: TWakanPaintbox;
    Paintbox2: TWakanPaintbox;
    btnAddToClipboard: TSpeedButton;
    Paintbox1: TWakanPaintbox;
    procedure btnAddToClipboardClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Paintbox1Paint(Sender: TObject; Canvas: TCanvas);
    procedure Paintbox2Paint(Sender: TObject; Canvas: TCanvas);
    procedure Paintbox5Paint(Sender: TObject; Canvas: TCanvas);
  end;

var
  fWordDetails: TfWordDetails;

implementation

uses JWBUser, JWBMenu, JWBStrings, JWBUnit;

{$R *.DFM}

procedure TfWordDetails.btnAddToClipboardClick(Sender: TObject);
begin
  fUser.btnCopyToClipboardClick(Sender);
end;

procedure TfWordDetails.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  fMenu.aDictDetails.Checked:=false;
end;

procedure TfWordDetails.Paintbox1Paint(Sender: TObject; Canvas: TCanvas);
begin
  fUser.WordDetails_PaintBox1Paint(Sender, Canvas);
end;

procedure TfWordDetails.Paintbox2Paint(Sender: TObject; Canvas: TCanvas);
begin
  fUser.WordDetails_PaintBox2Paint(Sender, Canvas);
end;

procedure TfWordDetails.Paintbox5Paint(Sender: TObject; Canvas: TCanvas);
begin
  Canvas.Brush.Color:=clWindow;
  DrawUnicode(Canvas,2,2,22,fstr(fUser.curmeaning),FontEnglish);
end;

end.
