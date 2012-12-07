unit JWBWordList;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, RXCtrls, StdCtrls, ExtCtrls, Buttons, Gauges;

type
  TfWordList = class(TForm)
    Notebook1: TNotebook;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    BitBtn3: TBitBtn;
    RxLabel3: TRxLabel;
    Label1: TLabel;
    Label14: TLabel;
    RxLabel4: TRxLabel;
    RadioGroup1: TRadioGroup;
    RadioGroup2: TRadioGroup;
    Label15: TLabel;
    Edit3: TEdit;
    Button1: TButton;
    CheckBox1: TCheckBox;
    RxLabel5: TRxLabel;
    RadioGroup3: TRadioGroup;
    Label16: TLabel;
    RadioGroup4: TRadioGroup;
    RadioGroup5: TRadioGroup;
    RadioGroup6: TRadioGroup;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    Button2: TButton;
    Button3: TButton;
    Label18: TLabel;
    Label19: TLabel;
    Label20: TLabel;
    Label21: TLabel;
    Label26: TLabel;
    Label27: TLabel;
    Label28: TLabel;
    Label29: TLabel;
    Label33: TLabel;
    Label35: TLabel;
    Label42: TLabel;
    Label44: TLabel;
    RxLabel6: TRxLabel;
    Label17: TLabel;
    Button4: TButton;
    CheckBox4: TCheckBox;
    RadioGroup7: TRadioGroup;
    RxLabel7: TRxLabel;
    RadioGroup8: TRadioGroup;
    RxLabel8: TRxLabel;
    Gauge1: TGauge;
    Label22: TLabel;
    Label23: TLabel;
    Label24: TLabel;
    Shape2: TShape;
    PaintBox1: TPaintBox;
    Shape5: TShape;
    Label25: TLabel;
    PaintBox6: TPaintBox;
    RxLabel9: TRxLabel;
    Label30: TLabel;
    Label31: TLabel;
    Label32: TLabel;
    Label34: TLabel;
    Label36: TLabel;
    Label37: TLabel;
    Label38: TLabel;
    Label39: TLabel;
    Label40: TLabel;
    Label41: TLabel;
    GroupBox2: TGroupBox;
    Button7: TButton;
    Button8: TButton;
    Button9: TButton;
    Button10: TButton;
    Shape1: TShape;
    PaintBox2: TPaintBox;
    RxLabel10: TRxLabel;
    Label43: TLabel;
    Label45: TLabel;
    RxLabel11: TRxLabel;
    RxLabel12: TRxLabel;
    Bevel1: TBevel;
    RxLabel1: TRxLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    RadioGroup9: TRadioGroup;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label46: TLabel;
    Label47: TLabel;
    Label48: TLabel;
    Label49: TLabel;
    Label50: TLabel;
    RxLabel2: TRxLabel;
    RadioGroup10: TRadioGroup;
    Button5: TButton;
    CheckBox5: TCheckBox;
    Label51: TLabel;
    Edit1: TEdit;
    BitBtn4: TBitBtn;
    Label52: TLabel;
    Label53: TLabel;
    Label54: TLabel;
    Label55: TLabel;
    CheckBox6: TCheckBox;
    procedure Button2Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
    procedure BitBtn3Click(Sender: TObject);
    procedure RadioGroup2Click(Sender: TObject);
    procedure RadioGroup1Click(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure BitBtn1Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure CheckBox5Click(Sender: TObject);
    procedure PaintBox1Paint(Sender: TObject);
    procedure PaintBox6Paint(Sender: TObject);
    procedure PaintBox2Paint(Sender: TObject);
    procedure Button10Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    step:integer;
    procedure Upd;
  end;

var
  fWordList: TfWordList;

implementation

uses JWBSettings, JWBPrint, Printers, JWBWords, JWBMenu, JWBStrings, JWBUnit;

{$R *.DFM}

procedure TfWordList.Upd;
begin
  Notebook1.PageIndex:=step-1;
  BitBtn3.Enabled:=step>1;
  BitBtn2.Enabled:=(step<>6) and (step<8);
  BitBtn4.Enabled:=step=8;
  BitBtn1.Enabled:=true;
  if (step=6) and (Button10.Visible) then Button10.SetFocus;
end;

procedure TfWordList.Button2Click(Sender: TObject);
begin
  RadioGroup3.ItemIndex:=0;
  RadioGroup4.ItemIndex:=4;
  RadioGroup5.ItemIndex:=1;
  RadioGroup6.ItemIndex:=1;
end;

procedure TfWordList.Button1Click(Sender: TObject);
var ps:string;
    c:integer;
    ph,pr:integer;
begin
  ps:=fSettings.Edit16.Text;
  c:=1;
  if (ps[2]='2') or (ps[4]='2') or (ps[6]='2') or (ps[8]='2') then c:=2;
  if (ps[2]='3') or (ps[4]='3') or (ps[6]='3') or (ps[8]='3') then c:=3;
  if (ps[2]='4') or (ps[4]='4') or (ps[6]='4') or (ps[8]='4') then c:=4;
  try
    GetPrintLine(Printer.PageWidth,Printer.PageHeight,Printer.PageWidth,Printer.PageHeight,strtoint(fSettings.Edit10.Text),ph,pr);
  except
    pr:=100;
  end;
  Edit3.Text:=inttostr(pr*c);
end;

procedure TfWordList.FormShow(Sender: TObject);
begin
  step:=1;
  Upd;
  Button1Click(self);
  RadioGroup1Click(self);
end;

procedure TfWordList.Button3Click(Sender: TObject);
begin
  RadioGroup3.ItemIndex:=3;
  RadioGroup4.ItemIndex:=1;
  RadioGroup5.ItemIndex:=4;
  RadioGroup6.ItemIndex:=0;
end;

procedure TfWordList.BitBtn2Click(Sender: TObject);
begin
  step:=fWords.LLGoNext(step);
  if step<9 then Upd else Close;
end;

procedure TfWordList.BitBtn3Click(Sender: TObject);
begin
  step:=fWords.LLGoPrev(step);
  Upd;
end;

procedure TfWordList.RadioGroup2Click(Sender: TObject);
begin
  Label15.Enabled:=RadioGroup2.ItemIndex<>2;
  Edit3.Enabled:=RadioGroup2.ItemIndex<>2;
  Button1.Enabled:=RadioGroup2.ItemIndex<>2;
end;

procedure TfWordList.RadioGroup1Click(Sender: TObject);
begin
  if RadioGroup1.ItemIndex=0 then Label23.Caption:=inttostr(fWords.WordListCount) else
    Label23.Caption:=inttostr(TUser.RecordCount);
end;

procedure TfWordList.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
var i,j:integer;
begin
  i:=step;
  while i>0 do
  begin
    j:=fWords.LLGoPrev(i);
    if j=i then
    begin
      CanClose:=false;
      Upd;
      exit;
    end;
    i:=j;
  end;
  CanClose:=true;
end;

procedure TfWordList.BitBtn1Click(Sender: TObject);
begin
  Close;
end;

procedure TfWordList.Button4Click(Sender: TObject);
begin
  fWords.BuildWordList;
end;

procedure TfWordList.Button7Click(Sender: TObject);
var res:boolean;
begin
  res:=fWords.FinishTestWord((sender as TButton).Tag);
  if not res then
  begin
    step:=fWords.LLGoNext(step);
    Upd;
    exit;
  end;
  fWords.PrepareTestWord;
  Button10.SetFocus;
end;

procedure TfWordList.Button5Click(Sender: TObject);
begin
  fWords.PrintWordList;
end;

procedure TfWordList.CheckBox5Click(Sender: TObject);
begin
  Label51.Enabled:=CheckBox5.Checked;
  Edit1.Enabled:=CheckBox5.Checked;
end;

procedure TfWordList.PaintBox1Paint(Sender: TObject);
begin
  PaintBox1.Canvas.Brush.Color:=clWindow;
  DrawKana(PaintBox1.Canvas,1,1,22,fWords.twphonetic,FontJapanese,showroma,romasys,curlang);
end;

procedure TfWordList.PaintBox6Paint(Sender: TObject);
var x:string;
begin
  PaintBox6.Canvas.Brush.Color:=clWindow;
  x:=fWords.twkanji;
  PaintBox6.Canvas.Font.Color:=clWindowText;
  if x[1]=UH_UNKNOWN_KANJI then
  begin
    delete(x,1,1);
    PaintBox6.Canvas.Font.Color:=Col('Dict_UnknownChar');
  end;
  DrawUnicode(PaintBox6.Canvas,1,1,22,x,FontJapanese);
end;

procedure TfWordList.PaintBox2Paint(Sender: TObject);
begin
  PaintBox2.Canvas.Brush.Color:=clWindow;
  DrawUnicode(PaintBox2.Canvas,1,1,22,UnicodeToHex(fWords.twmeaning),FontEnglish);
end;

procedure TfWordList.Button10Click(Sender: TObject);
begin
  fWords.ShowTestWord;
  Button7.SetFocus;
end;

end.
