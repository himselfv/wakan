unit JWBUserAdd;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Buttons;

type
  TfUserAdd = class(TForm)
    Label1: TLabel;
    Shape9: TShape;
    PaintBox3: TPaintBox;
    Label7: TLabel;
    Shape3: TShape;
    PaintBox2: TPaintBox;
    Label2: TLabel;
    Label4: TLabel;
    Edit1: TEdit;
    Edit3: TEdit;
    ComboBox1: TComboBox;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    procedure Edit1Change(Sender: TObject);
    procedure PaintBox3Paint(Sender: TObject);
    procedure PaintBox2Paint(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  fUserAdd: TfUserAdd;

implementation

uses JWBWords, JWBCore, JWBUnit, JWBMenu;

{$R *.DFM}

procedure TfUserAdd.Edit1Change(Sender: TObject);
begin
  PaintBox3.Invalidate;
end;

procedure TfUserAdd.PaintBox3Paint(Sender: TObject);
begin
  PaintBox3.Canvas.Brush.Color:=clBtnFace;
  DrawUnicode(PaintBox3.Canvas,1,1,16,RomajiToKana(Edit1.Text,romasys,false,curlang),FontSmall);
end;

procedure TfUserAdd.PaintBox2Paint(Sender: TObject);
begin
  fWords.UserAdd_PaintBox2Paint(sender);
end;

procedure TfUserAdd.Button1Click(Sender: TObject);
begin
  fWords.UserAdd_Button1Click(sender);
end;

procedure TfUserAdd.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  fWords.SpeedButton1.Down:=false;
  fMenu.aUserAdd.Checked:=false;
end;

end.
