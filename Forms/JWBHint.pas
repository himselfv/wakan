unit JWBHint;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, WakanPaintbox;

type
  TfHint = class(TForm)
    PaintBox1: TWakanPaintbox;
    procedure PaintBox1Paint(Sender: TObject; Canvas: TCanvas);
  end;

var
  fHint: TfHint;

implementation

uses JWBUser;

{$R *.DFM}

procedure TfHint.PaintBox1Paint(Sender: TObject; Canvas: TCanvas);
begin
  fUser.PaintHint;
end;

end.
