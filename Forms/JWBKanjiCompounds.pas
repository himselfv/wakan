unit JWBKanjiCompounds;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Grids, StdCtrls, ExtCtrls, Buttons;

type
  TfKanjiCompounds = class(TForm)
    SpeedButton8: TSpeedButton;
    SpeedButton9: TSpeedButton;
    Shape7: TShape;
    Label25: TLabel;
    CheckBox1: TCheckBox;
    StringGrid1: TStringGrid;
    Bevel1: TBevel;
    CheckBox2: TCheckBox;
    SpeedButton23: TSpeedButton;
    SpeedButton17: TSpeedButton;
    CheckBox3: TCheckBox;
    procedure StringGrid1DrawCell(Sender: TObject; ACol, ARow: Integer;
      Rect: TRect; State: TGridDrawState);
    procedure SpeedButton11Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure StringGrid1DblClick(Sender: TObject);
    procedure StringGrid1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure StringGrid1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure StringGrid1SelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
    procedure SpeedButton23Click(Sender: TObject);
    procedure SpeedButton17Click(Sender: TObject);
    procedure StringGrid1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  end;

var
  fKanjiCompounds: TfKanjiCompounds;

implementation

uses JWBKanji, JWBStrings, JWBUnit, JWBMenu, JWBDicAdd, JWBUser, JWBWords;

var curcphonetic,curckanji,curcmeaning:string;

{$R *.DFM}

procedure TfKanjiCompounds.StringGrid1DrawCell(Sender: TObject; ACol,
  ARow: Integer; Rect: TRect; State: TGridDrawState);
begin
  DrawWordCell(StringGrid1,ACol,ARow,Rect,State);
end;

procedure TfKanjiCompounds.SpeedButton11Click(Sender: TObject);
begin
  fKanji.KanjiCompounds_CheckBox1Click(sender);
end;

procedure TfKanjiCompounds.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  fKanji.btnCompounds.Down:=false;
  fMenu.aKanjiCompounds.Checked:=false;
end;

procedure TfKanjiCompounds.FormResize(Sender: TObject);
begin
  StringGrid1.ColWidths[2]:=StringGrid1.Width-StringGrid1.ColWidths[1]-StringGrid1.ColWidths[0]-24;
end;

procedure TfKanjiCompounds.FormShow(Sender: TObject);
begin
  fKanji.SetCharCompounds;
end;

procedure TfKanjiCompounds.StringGrid1DblClick(Sender: TObject);
begin
  if StringGrid1.Row>-1 then showmessage(StringGrid1.Cells[2,StringGrid1.Row]);
end;

procedure TfKanjiCompounds.StringGrid1MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  fMenu.IntTipGridOver(StringGrid1,x,y,ssLeft in Shift);
end;

procedure TfKanjiCompounds.StringGrid1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if mbRight=Button then fMenu.PopupImmediate(false);
end;

procedure TfKanjiCompounds.StringGrid1SelectCell(Sender: TObject; ACol,
  ARow: Integer; var CanSelect: Boolean);
begin
  curcphonetic:=remexcl(copy(StringGrid1.Cells[0,ARow],2,length(StringGrid1.Cells[0,ARow])-1));
  curckanji:=remexcl(copy(StringGrid1.Cells[1,ARow],2,length(StringGrid1.Cells[1,ARow])-1));
  curcmeaning:=strip_fl(remexcl(StringGrid1.Cells[2,ARow]));
  fDicAdd.Edit3.Text:=remexcl(StringGrid1.Cells[2,ARow]);
end;

procedure TfKanjiCompounds.SpeedButton23Click(Sender: TObject);
begin
  clip:=clip+curckanji;
  fMenu.ChangeClipboard;
end;

procedure TfKanjiCompounds.SpeedButton17Click(Sender: TObject);
begin
  fUser.curkanji:=curckanji;
  fUser.curphonetic:=curcphonetic;
  if fDicAdd.ShowModal=mrOK then
  begin
    if not fWords.AddWord(curckanji,curcphonetic,fDicAdd.edit3.text,fDicAdd.ComboBox1.Text,'?',false,1) then exit;
  end;
end;

procedure TfKanjiCompounds.StringGrid1MouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if mbLeft=Button then fMenu.PopupImmediate(true);
end;

end.
