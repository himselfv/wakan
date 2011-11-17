unit JWBExamples;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Buttons;

type
  TfExamplesPanel = class(TForm)
    Panel1: TPanel;
    Notebook1: TNotebook;
    Label1: TLabel;
    Shape9: TShape;
    PaintBox3: TPaintBox;
    Label2: TLabel;
    btnExamplesShowTranslation: TSpeedButton;
    btnExamplesFontSmall: TSpeedButton;
    btnExamplesFontBig: TSpeedButton;
    btnExamplesPrevious: TSpeedButton;
    btnExamplesNext: TSpeedButton;
    Bevel2: TBevel;
    Label3: TLabel;
    btnExampleCopyToClipboard: TSpeedButton;
    btnExamplesCopyAllToClipboard: TSpeedButton;
    Bevel3: TBevel;
    btnExamplesRandomOrder: TSpeedButton;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    btnExamplesNum: TSpeedButton;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure btnExamplesNumClick(Sender: TObject);
    procedure PaintBox3Paint(Sender: TObject);
    procedure PaintBox3MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure btnExamplesPreviousClick(Sender: TObject);
    procedure btnExamplesNextClick(Sender: TObject);
    procedure btnExamplesShowTranslationClick(Sender: TObject);
    procedure btnExamplesRandomOrderClick(Sender: TObject);
    procedure btnExampleCopyToClipboardClick(Sender: TObject);
    procedure btnExamplesCopyAllToClipboardClick(Sender: TObject);
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
  fExamplesPanel: TfExamplesPanel;

implementation

uses JWBUser, JWBMenu, JWBSettings, JWBWords;

{$R *.DFM}

procedure TfExamplesPanel.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  fUser.btnShowExamples.Down:=false;
  fMenu.aDictAdd.Checked:=false;
end;

procedure TfExamplesPanel.btnExamplesNumClick(Sender: TObject);
var n:string;
begin
  n:=InputBox(_l('#00892^eGo to example^cPøejít na pøíklad'),_l('#00893^eEnter the number of the example:^cZadejte èíslo pøíkladu:'),'');
  if n<>'' then
  try
    fUser.GotoExample(strtoint(n));
  except end;
end;

procedure TfExamplesPanel.PaintBox3Paint(Sender: TObject);
begin
  fUser.PaintExample;
end;

procedure TfExamplesPanel.PaintBox3MouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  fMenu.IntTipPaintOver(PaintBox3,x,y,ssLeft in Shift);
end;

procedure TfExamplesPanel.btnExamplesPreviousClick(Sender: TObject);
begin
  fUser.MoveExample(false);
end;

procedure TfExamplesPanel.btnExamplesNextClick(Sender: TObject);
begin
  fUser.MoveExample(true);
end;

procedure TfExamplesPanel.btnExamplesShowTranslationClick(Sender: TObject);
begin
  PaintBox3.Invalidate;
end;

procedure TfExamplesPanel.btnExamplesRandomOrderClick(Sender: TObject);
var cansel:boolean;
begin
  if fUser.Visible then fUser.ShowWord else
  if fWords.Visible then fWords.StringGrid1SelectCell(sender,fWords.StringGrid1.Col,fWords.StringGrid1.Row,cansel);
end;

procedure TfExamplesPanel.btnExampleCopyToClipboardClick(Sender: TObject);
begin
  fUser.ExampleClipboard(false);
end;

procedure TfExamplesPanel.btnExamplesCopyAllToClipboardClick(Sender: TObject);
begin
  fUser.ExampleClipboard(true);
end;

procedure TfExamplesPanel.PaintBox3MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if mbRight=Button then fMenu.PopupImmediate(false);
end;

procedure TfExamplesPanel.PaintBox3MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if mbLeft=Button then fMenu.PopupImmediate(true);
end;

end.
