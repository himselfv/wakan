unit JWBVocabAdd;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Buttons, WakanPaintbox;

type
  TfVocabAdd = class(TForm)
    Label1: TLabel;
    Label7: TLabel;
    Label2: TLabel;
    Label4: TLabel;
    Edit1: TEdit;
    Edit3: TEdit;
    ComboBox1: TComboBox;
    btnOk: TBitBtn;
    btnCancel: TBitBtn;
    Paintbox2: TWakanPaintbox;
    PaintBox3: TWakanPaintbox;
    procedure Edit1Change(Sender: TObject);
    procedure PaintBox3Paint(Sender: TObject; Canvas: TCanvas);
    procedure PaintBox2Paint(Sender: TObject; Canvas: TCanvas);
    procedure Button1Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  end;

var
  fVocabAdd: TfVocabAdd;

implementation

uses JWBVocab, JWBStrings, JWBUnit, JWBMenu;

{$R *.DFM}

procedure TfVocabAdd.Edit1Change(Sender: TObject);
begin
  PaintBox3.Invalidate;
end;

procedure TfVocabAdd.PaintBox3Paint(Sender: TObject; Canvas: TCanvas);
begin
  Canvas.Brush.Color:=clBtnFace;
  DrawUnicode(Canvas,2,2,16,RomajiToKana(Edit1.Text,romasys,curlang,[]),FontSmall);
end;

procedure TfVocabAdd.PaintBox2Paint(Sender: TObject; Canvas: TCanvas);
begin
  Canvas.Brush.Color:=clWindow;
  DrawUnicode(Canvas,2,2,22,clip,FontJapanese);
end;

procedure TfVocabAdd.Button1Click(Sender: TObject);
begin
  fVocab.UserAdd_Button1Click(sender);
end;

procedure TfVocabAdd.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  fVocab.SpeedButton1.Down:=false;
  fMenu.aUserAdd.Checked:=false;
end;

end.
