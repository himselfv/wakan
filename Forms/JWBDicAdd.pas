unit JWBDicAdd;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, Buttons, WakanPaintbox;

type
  TfDicAdd = class(TForm)
    Label6: TLabel;
    Label4: TLabel;
    ComboBox1: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    edtMeaning: TMemo;
    Paintbox1: TWakanPaintbox;
    PaintBox2: TWakanPaintbox;
    procedure Paintbox1Paint(Sender: TObject; Canvas: TCanvas);
    procedure PaintBox2Paint(Sender: TObject; Canvas: TCanvas);
  end;

var
  fDicAdd: TfDicAdd;

implementation

uses JWBUser;

{$R *.DFM}

procedure TfDicAdd.Paintbox1Paint(Sender: TObject; Canvas: TCanvas);
begin
  fUser.WordDetails_PaintBox1Paint(Sender, Canvas);
end;

procedure TfDicAdd.PaintBox2Paint(Sender: TObject; Canvas: TCanvas);
begin
  fUser.WordDetails_PaintBox2Paint(Sender, Canvas);
end;

end.
