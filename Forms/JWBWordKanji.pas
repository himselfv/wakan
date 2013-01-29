unit JWBWordKanji;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, Buttons;

type
  TKanjiBox = record
    lbl: TLabel;
    sh: TShape;
    pb: TPaintBox;
  end;
  PKanjiBox = ^TKanjiBox;

  TfWordKanji = class(TForm)
    Label2: TLabel;
    Label3: TLabel;
    Bevel1: TBevel;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure PaintBoxK1Paint(Sender: TObject);
    procedure PaintBoxK1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure PaintBoxK1Click(Sender: TObject);
    procedure PaintBoxK1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormDestroy(Sender: TObject);

  protected
    procedure WordKanji_PaintBoxKNPaint(pb: TPaintBox; KN: integer);

  protected
    FBoxes: array of TKanjiBox;

  public
    procedure Clear;
    procedure AddBox(const meaning: string);
    procedure InvalidateBoxes;

  end;

var
  fWordKanji: TfWordKanji;

implementation

uses JWBUnit, JWBUser, JWBMenu;

{$R *.DFM}

procedure TfWordKanji.FormDestroy(Sender: TObject);
begin
  Clear;
end;

procedure TfWordKanji.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  fUser.SpeedButton6.Down:=false;
  fMenu.aDictKanji.Checked:=false;
end;

//Hides all kanji boxes
procedure TfWordKanji.Clear;
var i: integer;
begin
  for i := Length(FBoxes) - 1 downto 0 do begin
    FreeAndNil(FBoxes[i].lbl);
    FreeAndNil(FBoxes[i].sh);
    FreeAndNil(FBoxes[i].pb);
  end;
  SetLength(FBoxes, 0);
end;

procedure TfWordKanji.AddBox(const meaning: string);
var idx: integer;
  box: PKanjiBox;
begin
  idx := Length(FBoxes);
  SetLength(FBoxes, idx+1);
  box := @FBoxes[idx];

  box.sh := TShape.Create(Self);
  box.sh.Height := 43;
  box.sh.Width := 202;
  box.sh.Left := 8;
  box.sh.Top := 26 + 48*idx;
  box.sh.Tag := idx+1;
  box.sh.Parent := Self;

  box.pb := TPaintBox.Create(Self);
  box.pb.Height := 41;
  box.pb.Width := 89;
  box.pb.Left := 9;
  box.pb.Top := box.sh.Top + 1;
  box.pb.Tag := idx+1;
  box.pb.OnClick := PaintBoxK1Click;
  box.pb.OnMouseMove := PaintBoxK1MouseMove;
  box.pb.OnMouseUp := PaintBoxK1MouseUp;
  box.pb.OnPaint := PaintBoxK1Paint;
  box.pb.Parent := Self;

  box.lbl := TLabel.Create(Self);
  box.lbl.AutoSize := false;
  box.lbl.Caption := meaning;
  box.lbl.Height := 41;
  box.lbl.Width := 125;
  box.lbl.Left := 85;
  box.lbl.Top := box.sh.Top + 2;
  box.lbl.Tag := idx+1;
  box.lbl.Transparent := true;
  box.lbl.WordWrap := true;
  box.lbl.Parent := Self;
end;

procedure TfWordKanji.InvalidateBoxes;
var i: integer;
begin
  for i := 0 to Length(FBoxes) - 1 do
    FBoxes[i].pb.Invalidate;
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
