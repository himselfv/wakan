unit JWBDicAdd;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, Buttons;

type
  TfDicAdd = class(TForm)
    Shape2: TShape;
    PaintBox1: TPaintBox;
    Shape3: TShape;
    PaintBox2: TPaintBox;
    Label6: TLabel;
    Label4: TLabel;
    Edit3: TEdit;
    ComboBox1: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    procedure PaintBox1Paint(Sender: TObject);
    procedure PaintBox2Paint(Sender: TObject);
  end;

var
  fDicAdd: TfDicAdd;

implementation

uses JWBUser;

{$R *.DFM}

procedure TfDicAdd.PaintBox1Paint(Sender: TObject);
begin
  fUser.WordDetails_PaintBox1Paint(sender);
end;

procedure TfDicAdd.PaintBox2Paint(Sender: TObject);
begin
  fUser.WordDetails_PaintBox2Paint(sender);
end;

end.
