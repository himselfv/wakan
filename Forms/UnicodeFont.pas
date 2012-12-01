unit UnicodeFont;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, ExtCtrls, Grids;

type
  TfSelectFont = class(TForm)
    StringGrid1: TStringGrid;
    Shape9: TShape;
    PaintBox3: TPaintBox;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    procedure StringGrid1SelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
    procedure FormShow(Sender: TObject);
    procedure PaintBox3Paint(Sender: TObject);
  private
    { Private declarations }
  public
    teststring:string;
    deffont,selfont:string;
    selcoding:string;
    { Public declarations }
  end;

var
  fSelectFont: TfSelectFont;

implementation

uses JWBUnit;

{$R *.DFM}

procedure TfSelectFont.StringGrid1SelectCell(Sender: TObject; ACol,
  ARow: Integer; var CanSelect: Boolean);
var s:string;
begin
  selfont:=StringGrid1.Cells[0,ARow];
  selcoding:=StringGrid1.Cells[1,ARow];
  PaintBox3.Invalidate;
end;

procedure TfSelectFont.FormShow(Sender: TObject);
var b:boolean;
    i:integer;
begin
  for i:=1 to StringGrid1.RowCount-1 do
    if uppercase(StringGrid1.Cells[0,i])=uppercase(deffont) then
    begin
      StringGrid1SelectCell(self,0,i,b);
      StringGrid1.Row:=i;
    end;  
end;

procedure TfSelectFont.PaintBox3Paint(Sender: TObject);
begin
  PaintBox3.Canvas.Brush.Color:=clWindow;
  PaintBox3.Canvas.Pen.Color:=clWindow;
  PaintBox3.Canvas.Rectangle(0,0,PaintBox3.Width,PaintBox3.Height);
  PaintBox3.Canvas.Pen.Color:=clWindowText;
  DrawUnicode(PaintBox3.Canvas,1,1,48,teststring,selfont);
end;

end.
