unit JWBWordAdd;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Buttons;

type
  TfWordAdd = class(TForm)
    Panel1: TPanel;
    Notebook1: TNotebook;
    Label1: TLabel;
    Shape9: TShape;
    PaintBox3: TPaintBox;
    Label2: TLabel;
    SpeedButton4: TSpeedButton;
    SpeedButton5: TSpeedButton;
    SpeedButton6: TSpeedButton;
    SpeedButton7: TSpeedButton;
    SpeedButton8: TSpeedButton;
    Bevel2: TBevel;
    Label3: TLabel;
    SpeedButton9: TSpeedButton;
    SpeedButton10: TSpeedButton;
    Bevel3: TBevel;
    SpeedButton11: TSpeedButton;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    SpeedButton1: TSpeedButton;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure SpeedButton1Click(Sender: TObject);
    procedure PaintBox3Paint(Sender: TObject);
    procedure PaintBox3MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure SpeedButton7Click(Sender: TObject);
    procedure SpeedButton8Click(Sender: TObject);
    procedure SpeedButton4Click(Sender: TObject);
    procedure SpeedButton11Click(Sender: TObject);
    procedure SpeedButton9Click(Sender: TObject);
    procedure SpeedButton10Click(Sender: TObject);
    procedure PaintBox3MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PaintBox3MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  fWordAdd: TfWordAdd;

implementation

uses JWBUser, JWBMenu, JWBSettings, JWBWords;

{$R *.DFM}

procedure TfWordAdd.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  fUser.SpeedButton9.Down:=false;
  fMenu.aDictAdd.Checked:=false;
end;

procedure TfWordAdd.SpeedButton1Click(Sender: TObject);
var n:string;
begin
  n:=InputBox(_l('#00892^eGo to example^cPøejít na pøíklad'),_l('#00893^eEnter the number of the example:^cZadejte èíslo pøíkladu:'),'');
  if n<>'' then
  try
    fUser.GotoExample(strtoint(n));
  except end;
end;

procedure TfWordAdd.PaintBox3Paint(Sender: TObject);
begin
  fUser.PaintExample;
end;

procedure TfWordAdd.PaintBox3MouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  fMenu.IntTipPaintOver(PaintBox3,x,y,ssLeft in Shift);
end;

procedure TfWordAdd.SpeedButton7Click(Sender: TObject);
begin
  fUser.MoveExample(false);
end;

procedure TfWordAdd.SpeedButton8Click(Sender: TObject);
begin
  fUser.MoveExample(true);
end;

procedure TfWordAdd.SpeedButton4Click(Sender: TObject);
begin
  PaintBox3.Invalidate;
end;

procedure TfWordAdd.SpeedButton11Click(Sender: TObject);
var cansel:boolean;
begin
  if fUser.Visible then fUser.ShowWord else
  if fWords.Visible then fWords.StringGrid1SelectCell(sender,fWords.StringGrid1.Col,fWords.StringGrid1.Row,cansel);
end;

procedure TfWordAdd.SpeedButton9Click(Sender: TObject);
begin
  fUser.ExampleClipboard(false);
end;

procedure TfWordAdd.SpeedButton10Click(Sender: TObject);
begin
  fUser.ExampleClipboard(true);
end;

procedure TfWordAdd.PaintBox3MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if mbRight=Button then fMenu.PopupImmediate(false);
end;

procedure TfWordAdd.PaintBox3MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if mbLeft=Button then fMenu.PopupImmediate(true);
end;

end.
