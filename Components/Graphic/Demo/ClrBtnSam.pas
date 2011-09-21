unit ClrBtnSam;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ColorBtns;

type
  TForm1 = class(TForm)
    Color95Button1: TColor95Button;
    Color95Button2: TColor95Button;
    Color95Button3: TColor95Button;
    Color95Button4: TColor95Button;
    Color95Button5: TColor95Button;
    Color95Button6: TColor95Button;
    Color95Button9: TColor95Button;
    Color95Button10: TColor95Button;
    Color95Button11: TColor95Button;
    Color95Button7: TColor95Button;
    Color95Button8: TColor95Button;
    Color95Button12: TColor95Button;
    Color95Button13: TColor95Button;
    procedure Color95Button5Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.Color95Button5Click(Sender: TObject);
var
	i: Integer;
begin
 	for i := 0 to ControlCount - 1 do
   	if Controls[i] is TColor95Button then
    	TColor95Button(Controls[i]).Color := TColor95Button(Sender).Glyph.Canvas.Pixels[8,8];
end;

end.
