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
    PaintBoxK1: TPaintBox;
    PaintBoxK2: TPaintBox;
    PaintBoxK3: TPaintBox;
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
    procedure PaintBoxK1Paint(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure PaintBoxK1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure PaintBoxK1Click(Sender: TObject);
    procedure PaintBoxK1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  protected
    procedure WordKanji_PaintBoxKNPaint(pb: TPaintBox; KN: integer);

  end;

var
  fWordKanji: TfWordKanji;

implementation

uses JWBUnit, JWBUser, JWBMenu;

{$R *.DFM}

procedure TfWordKanji.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  fUser.SpeedButton6.Down:=false;
  fMenu.aDictKanji.Checked:=false;
end;

procedure TfWordKanji.SpeedButton1Click(Sender: TObject);
begin
  fUser.WordDetails_SpeedButton23Click(sender);
end;

procedure TfWordKanji.PaintBoxK1Click(Sender: TObject);
begin
  fUser.DetailsForKanji(TPaintBox(Sender).Tag);
end;

procedure TfWordKanji.PaintBoxK1Paint(Sender: TObject);
begin
  WordKanji_PaintBoxKNPaint(TPaintBox(Sender), TPaintBox(Sender).Tag);
end;

//Paints fWordKanji.PaintBoxK1...PaintBoxK9 contents.
//KN: 1..9
procedure TfWordKanji.WordKanji_PaintBoxKNPaint(pb: TPaintBox; KN: integer);
begin
  Assert((KN>=1) and (KN<=9));
  if length(fUser.curkanjid)<KN then exit;
  BeginDrawReg(pb);
  pb.Canvas.Brush.Color:=Col('Kanji_Back');
  DrawUnicode(pb.Canvas,44,4,16,fUser.curkanjid[KN-1].rad,FontJapaneseGrid);
  case fUser.curkanjid[KN-1].tp of
    'K':pb.Canvas.Font.Color:=Col('Kanji_Learned');
    'C':pb.Canvas.Font.Color:=Col('Kanji_Common');
    'U':pb.Canvas.Font.Color:=Col('Kanji_Rare');
    'N':pb.Canvas.Font.Color:=Col('Kanji_Names');
  end;
  DrawUnicode(pb.Canvas,4,2,36,fUser.curkanjid[KN-1].char,FontJapaneseGrid);
  EndDrawReg;
end;

procedure TfWordKanji.PaintBoxK1MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  fMenu.IntTipPaintOver(TPaintBox(Sender),x,y,ssLeft in Shift);
end;

procedure TfWordKanji.PaintBoxK1MouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if mbLeft=Button then fMenu.PopupImmediate(true);
end;

end.
