unit JWBScreenTip;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, Buttons;

type
  TfScreenTip = class(TForm)
    pb: TPaintBox;
    procedure pbPaint(Sender: TObject);
    procedure pbMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure pbMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  fScreenTip: TfScreenTip;

implementation

uses JWBMenu, JWBUnit;

{$R *.DFM}

procedure TfScreenTip.pbPaint(Sender: TObject);
begin
  fMenu.PaintScreenTip;
end;

procedure TfScreenTip.pbMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  fMenu.PopupMouseMove(x,y);
end;

procedure TfScreenTip.pbMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  fMenu.PopupMouseUp(button,shift,x,y);
end;

end.
