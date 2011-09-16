unit JWBHint;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls;

type
  TfHint = class(TForm)
    Shape1: TShape;
    PaintBox1: TPaintBox;
    procedure PaintBox1Paint(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  fHint: TfHint;

implementation

uses JWBUser;

{$R *.DFM}

procedure TfHint.PaintBox1Paint(Sender: TObject);
begin
  fUser.PaintHint;
end;

end.
