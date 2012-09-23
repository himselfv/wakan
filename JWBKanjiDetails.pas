unit JWBKanjiDetails;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, Buttons, StdCtrls, RXCtrls, rxPlacemnt, ProCtrls, ProUrl;

type
  TfKanjiDetails = class(TForm)
    Shape2: TShape;
    Label1: TLabel;
    RxLabel21: TRxLabel;
    Shape8: TShape;
    PaintBox1: TPaintBox;
    PaintBox2: TPaintBox;
    Shape10: TShape;
    PaintBox4: TPaintBox;
    RxLabel10: TRxLabel;
    RxLabel35: TRxLabel;
    RxLabel38: TRxLabel;
    Label9: TLabel;
    SpeedButton21: TSpeedButton;
    RxLabel39: TRxLabel;
    ScrollBox1: TScrollBox;
    PaintBox3: TPaintBox;
    Shape1: TShape;
    Button1: TButton;
    FormPlacement1: TFormPlacement;
    ProUrlLabel1: TProUrlLabel;
    ProUrlLabel2: TProUrlLabel;
    ProUrlLabel3: TProUrlLabel;
    ProUrlLabel4: TProUrlLabel;
    ProUrlLabel5: TProUrlLabel;
    Label2: TLabel;
    Button2: TButton;
    SpeedButton1: TSpeedButton;
    ComboBox1: TComboBox;
    RxLabel1: TRxLabel;
    Label3: TLabel;
    procedure PaintBox1Paint(Sender: TObject);
    procedure PaintBox2Paint(Sender: TObject);
    procedure PaintBox4Paint(Sender: TObject);
    procedure PaintBox5Paint(Sender: TObject);
    procedure PaintBox6Paint(Sender: TObject);
    procedure PaintBox7Paint(Sender: TObject);
    procedure SpeedButton10Click(Sender: TObject);
    procedure SpeedButton21Click(Sender: TObject);
    procedure SpeedButton28Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure SpeedButton23Click(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure PaintBox3Paint(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure PaintBox2DblClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure Button1KeyPress(Sender: TObject; var Key: Char);
    procedure Button2Click(Sender: TObject);
    procedure PaintBox2MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure PaintBox3MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure PaintBox4MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure PaintBox2MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PaintBox4MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PaintBox3MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ComboBox1Change(Sender: TObject);
    procedure PaintBox2MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PaintBox4MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PaintBox3MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  fKanjiDetails: TfKanjiDetails;

implementation

uses JWBKanji, JWBMenu, JWBUnit, JWBTranslate;

{$R *.DFM}

procedure TfKanjiDetails.PaintBox1Paint(Sender: TObject);
begin
  fKanji.KanjiDetails_PaintBox1Paint(sender);
end;

procedure TfKanjiDetails.PaintBox2Paint(Sender: TObject);
begin
  fKanji.KanjiDetails_PaintBox2Paint(sender);
end;

procedure TfKanjiDetails.PaintBox4Paint(Sender: TObject);
begin
  fKanji.KanjiDetails_PaintBox4Paint(sender);
end;

procedure TfKanjiDetails.PaintBox5Paint(Sender: TObject);
begin
  fKanji.KanjiDetails_PaintBox5Paint(sender);
end;

procedure TfKanjiDetails.PaintBox6Paint(Sender: TObject);
begin
  fKanji.KanjiDetails_PaintBox6Paint(sender);
end;

procedure TfKanjiDetails.PaintBox7Paint(Sender: TObject);
begin
  fKanji.KanjiDetails_PaintBox7Paint(sender);
end;

procedure TfKanjiDetails.SpeedButton10Click(Sender: TObject);
begin
  fKanji.KanjiDetails_SpeedButton10Click(sender);
end;

procedure TfKanjiDetails.SpeedButton21Click(Sender: TObject);
begin
  fKanji.KanjiDetails_SpeedButton21Click(sender);
end;

procedure TfKanjiDetails.SpeedButton28Click(Sender: TObject);
begin
  fKanji.KanjiDetails_SpeedButton28Click(sender);
end;

procedure TfKanjiDetails.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  fKanji.SpeedButton2.Down:=false;
  fTranslate.SpeedButton20.Down:=false;
  fMenu.aKanjiDetails.Checked:=false;
  if not CharDetDocked then FormPlacement1.SaveFormPlacement;
end;

procedure TfKanjiDetails.SpeedButton23Click(Sender: TObject);
begin
  fKanji.SpeedButton23Click(sender);
end;

procedure TfKanjiDetails.SpeedButton1Click(Sender: TObject);
begin
  PaintBox1.Invalidate;
end;

procedure TfKanjiDetails.PaintBox3Paint(Sender: TObject);
begin
  BeginDrawReg(PaintBox3);
  fKanji.InfoPaint(PaintBox3.Canvas,PaintBox3.Width,false);
  EndDrawReg;
end;

procedure TfKanjiDetails.FormShow(Sender: TObject);
begin
  if Visible then fMenu.aKanjiDetails.Checked:=true;
  if CharDetDocked then Button2.Caption:=_l('#00172^eUndock^cOddokovat') else
    Button2.Caption:=_l('#00173^eDock^cZadokovat');
  Button2.Enabled:=CharDetDocked or (curdisplaymode=1) or (curdisplaymode=3) or
    (curdisplaymode=4);
  Button1.Default:=not CharDetDocked;
end;

procedure TfKanjiDetails.FormHide(Sender: TObject);
begin
  fKanji.SpeedButton2.Down:=false;
  fMenu.aKanjiDetails.Checked:=false;
end;

procedure TfKanjiDetails.PaintBox2DblClick(Sender: TObject);
begin
  fKanji.SelRadical;
end;

procedure TfKanjiDetails.Button1Click(Sender: TObject);
begin
  if CharDetDocked then fMenu.aKanjiDetails.Execute else Close;
end;

procedure TfKanjiDetails.FormKeyPress(Sender: TObject; var Key: Char);
begin
  if key=#27 then CLose;
end;

procedure TfKanjiDetails.Button1KeyPress(Sender: TObject; var Key: Char);
begin
  if key=#27 then CLose;
end;

procedure TfKanjiDetails.Button2Click(Sender: TObject);
begin
  if CharDetDocked then
  begin
    CharDetDocked:=false;
    fMenu.ChangeDisplay;
    fMenu.aKanjiDetails.Execute;
  end else
  begin
    FormPlacement1.SaveFormPlacement;
    CharDetDocked:=true;
    CharDetDockedVis1:=true;
    CharDetDockedVis2:=true;
    fMenu.ChangeDisplay;
  end;
end;

procedure TfKanjiDetails.PaintBox2MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  fMenu.IntTipPaintOver(PaintBox2,x,y,ssLeft in Shift);
end;

procedure TfKanjiDetails.PaintBox3MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  fMenu.IntTipPaintOver(PaintBox3,x,y,ssLeft in Shift);
end;

procedure TfKanjiDetails.PaintBox4MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  fMenu.IntTipPaintOver(PaintBox4,x,y,ssLeft in Shift);
end;

procedure TfKanjiDetails.PaintBox2MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if mbRight=Button then fMenu.PopupImmediate(false);
end;

procedure TfKanjiDetails.PaintBox4MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if mbRight=Button then fMenu.PopupImmediate(false);
end;

procedure TfKanjiDetails.PaintBox3MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if mbRight=Button then fMenu.PopupImmediate(false);
end;

procedure TfKanjiDetails.ComboBox1Change(Sender: TObject);
begin
  fKanji.RefreshDetails;
end;

procedure TfKanjiDetails.PaintBox2MouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if mbLeft=Button then fMenu.PopupImmediate(true);
end;

procedure TfKanjiDetails.PaintBox4MouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if mbLeft=Button then fMenu.PopupImmediate(true);
end;

procedure TfKanjiDetails.PaintBox3MouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if mbLeft=Button then fMenu.PopupImmediate(true);
end;

end.
