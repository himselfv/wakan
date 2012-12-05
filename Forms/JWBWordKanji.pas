unit JWBWordKanji;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, Buttons;

type
  TfWordKanji = class(TForm)
    Shape8: TShape;
    Shape6: TShape;
    Shape4: TShape;
    Label2: TLabel;
    Label3: TLabel;
    PaintBox7: TPaintBox;
    PaintBox8: TPaintBox;
    PaintBox9: TPaintBox;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    Bevel1: TBevel;
    Shape1: TShape;
    Shape2: TShape;
    Shape3: TShape;
    PaintBoxK4: TPaintBox;
    PaintBoxK5: TPaintBox;
    PaintBoxK6: TPaintBox;
    Label1: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Shape5: TShape;
    Shape7: TShape;
    Shape9: TShape;
    PaintBoxK7: TPaintBox;
    PaintBoxK8: TPaintBox;
    PaintBoxK9: TPaintBox;
    Label6: TLabel;
    Label7: TLabel;
    Label11: TLabel;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure PaintBox7Paint(Sender: TObject);
    procedure PaintBox8Paint(Sender: TObject);
    procedure PaintBox9Paint(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure PaintBoxK4Paint(Sender: TObject);
    procedure PaintBoxK5Paint(Sender: TObject);
    procedure PaintBoxK6Paint(Sender: TObject);
    procedure PaintBoxK7Paint(Sender: TObject);
    procedure PaintBoxK8Paint(Sender: TObject);
    procedure PaintBoxK9Paint(Sender: TObject);
    procedure PaintBox7MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure PaintBox8MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure PaintBox9MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure PaintBoxK4MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure PaintBoxK5MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure PaintBoxK6MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure PaintBoxK7MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure PaintBoxK8MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure PaintBoxK9MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure PaintBox7Click(Sender: TObject);
    procedure PaintBox8Click(Sender: TObject);
    procedure PaintBox9Click(Sender: TObject);
    procedure PaintBoxK4Click(Sender: TObject);
    procedure PaintBoxK5Click(Sender: TObject);
    procedure PaintBoxK6Click(Sender: TObject);
    procedure PaintBoxK7Click(Sender: TObject);
    procedure PaintBoxK8Click(Sender: TObject);
    procedure PaintBoxK9Click(Sender: TObject);
    procedure PaintBox7MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PaintBox8MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PaintBox9MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PaintBoxK4MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PaintBoxK5MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PaintBoxK6MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PaintBoxK7MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PaintBoxK8MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PaintBoxK9MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);

  end;

var
  fWordKanji: TfWordKanji;

implementation

uses JWBUser, JWBMenu;

{$R *.DFM}

procedure TfWordKanji.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  fUser.SpeedButton6.Down:=false;
  fMenu.aDictKanji.Checked:=false;
end;

procedure TfWordKanji.PaintBox7Paint(Sender: TObject);
begin
  fUser.WordKanji_PaintBox7Paint(sender);
end;

procedure TfWordKanji.PaintBox8Paint(Sender: TObject);
begin
  fUser.WordKanji_PaintBox8Paint(sender);
end;

procedure TfWordKanji.PaintBox9Paint(Sender: TObject);
begin
  fUser.WordKanji_PaintBox9Paint(sender);
end;

procedure TfWordKanji.SpeedButton1Click(Sender: TObject);
begin
  fUser.WordDetails_SpeedButton23Click(sender);
end;

procedure TfWordKanji.PaintBoxK4Paint(Sender: TObject);
begin
  fUser.WordKanji_PaintBoxK4Paint(sender);
end;

procedure TfWordKanji.PaintBoxK5Paint(Sender: TObject);
begin
  fUser.WordKanji_PaintBoxK5Paint(sender);
end;

procedure TfWordKanji.PaintBoxK6Paint(Sender: TObject);
begin
  fUser.WordKanji_PaintBoxK6Paint(sender);
end;

procedure TfWordKanji.PaintBoxK7Paint(Sender: TObject);
begin
  fUser.WordKanji_PaintBoxK7Paint(sender);
end;

procedure TfWordKanji.PaintBoxK8Paint(Sender: TObject);
begin
  fUser.WordKanji_PaintBoxK8Paint(sender);
end;

procedure TfWordKanji.PaintBoxK9Paint(Sender: TObject);
begin
  fUser.WordKanji_PaintBoxK9Paint(sender);
end;

procedure TfWordKanji.PaintBox7MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  fMenu.IntTipPaintOver(PaintBox7,x,y,ssLeft in Shift);
end;

procedure TfWordKanji.PaintBox8MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  fMenu.IntTipPaintOver(PaintBox8,x,y,ssLeft in Shift);
end;

procedure TfWordKanji.PaintBox9MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  fMenu.IntTipPaintOver(PaintBox9,x,y,ssLeft in Shift);
end;

procedure TfWordKanji.PaintBoxK4MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  fMenu.IntTipPaintOver(PaintBoxK4,x,y,ssLeft in Shift);
end;

procedure TfWordKanji.PaintBoxK5MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  fMenu.IntTipPaintOver(PaintBoxK5,x,y,ssLeft in Shift);
end;

procedure TfWordKanji.PaintBoxK6MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  fMenu.IntTipPaintOver(PaintBoxK6,x,y,ssLeft in Shift);
end;

procedure TfWordKanji.PaintBoxK7MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  fMenu.IntTipPaintOver(PaintBoxK7,x,y,ssLeft in Shift);
end;

procedure TfWordKanji.PaintBoxK8MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  fMenu.IntTipPaintOver(PaintBoxK8,x,y,ssLeft in Shift);
end;

procedure TfWordKanji.PaintBoxK9MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  fMenu.IntTipPaintOver(PaintBoxK9,x,y,ssLeft in Shift);
end;

procedure TfWordKanji.PaintBox7Click(Sender: TObject);
begin
  fUser.DetailsForKanji(1);
end;

procedure TfWordKanji.PaintBox8Click(Sender: TObject);
begin
  fUser.DetailsForKanji(2);
end;

procedure TfWordKanji.PaintBox9Click(Sender: TObject);
begin
  fUser.DetailsForKanji(3);
end;

procedure TfWordKanji.PaintBoxK4Click(Sender: TObject);
begin
  fUser.DetailsForKanji(4);
end;

procedure TfWordKanji.PaintBoxK5Click(Sender: TObject);
begin
  fUser.DetailsForKanji(5);
end;

procedure TfWordKanji.PaintBoxK6Click(Sender: TObject);
begin
  fUser.DetailsForKanji(6);
end;

procedure TfWordKanji.PaintBoxK7Click(Sender: TObject);
begin
  fUser.DetailsForKanji(7);
end;

procedure TfWordKanji.PaintBoxK8Click(Sender: TObject);
begin
  fUser.DetailsForKanji(8);
end;

procedure TfWordKanji.PaintBoxK9Click(Sender: TObject);
begin
  fUser.DetailsForKanji(9);
end;

procedure TfWordKanji.PaintBox7MouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if mbLeft=Button then fMenu.PopupImmediate(true);
end;

procedure TfWordKanji.PaintBox8MouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if mbLeft=Button then fMenu.PopupImmediate(true);
end;

procedure TfWordKanji.PaintBox9MouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if mbLeft=Button then fMenu.PopupImmediate(true);
end;

procedure TfWordKanji.PaintBoxK4MouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if mbLeft=Button then fMenu.PopupImmediate(true);
end;

procedure TfWordKanji.PaintBoxK5MouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if mbLeft=Button then fMenu.PopupImmediate(true);
end;

procedure TfWordKanji.PaintBoxK6MouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if mbLeft=Button then fMenu.PopupImmediate(true);
end;

procedure TfWordKanji.PaintBoxK7MouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if mbLeft=Button then fMenu.PopupImmediate(true);
end;

procedure TfWordKanji.PaintBoxK8MouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if mbLeft=Button then fMenu.PopupImmediate(true);
end;

procedure TfWordKanji.PaintBoxK9MouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if mbLeft=Button then fMenu.PopupImmediate(true);
end;

end.
