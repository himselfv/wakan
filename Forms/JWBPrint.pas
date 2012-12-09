unit JWBPrint;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, Buttons, Printers;

type
  TPrintGetPageNum=function (canvas:TCanvas; width,height:integer; userdata:pointer):integer;
  TPrintDrawPage=procedure (canvas:TCanvas; pagenum:integer; width,height,origwidth,origheight:integer; userdata:pointer);
  TPrintConfigure=procedure (userdata:pointer);

  TfPrint = class(TForm)
    Panel1: TPanel;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    SpeedButton3: TSpeedButton;
    SpeedButton4: TSpeedButton;
    SpeedButton5: TSpeedButton;
    SpeedButton6: TSpeedButton;
    SpeedButton7: TSpeedButton;
    SpeedButton8: TSpeedButton;
    SpeedButton9: TSpeedButton;
    Shape1: TShape;
    Label1: TLabel;
    ScrollBox1: TScrollBox;
    PaintBox1: TPaintBox;
    PrinterSetupDialog1: TPrinterSetupDialog;
    SpeedButton10: TSpeedButton;
    SpeedButton11: TSpeedButton;
    procedure PaintBox1Paint(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure SpeedButton3Click(Sender: TObject);
    procedure SpeedButton4Click(Sender: TObject);
    procedure SpeedButton7Click(Sender: TObject);
    procedure SpeedButton9Click(Sender: TObject);
    procedure SpeedButton8Click(Sender: TObject);
    procedure SpeedButton5Click(Sender: TObject);
    procedure SpeedButton6Click(Sender: TObject);
    procedure SpeedButton10Click(Sender: TObject);
    procedure SpeedButton11Click(Sender: TObject);
  public
    GetPageNum:TPrintGetPageNum;
    DrawPage:TPrintDrawPage;
    PrintConfigure:TPrintConfigure;
    PageNum:integer;
    UserData:pointer;
    CurPage:integer;
    Description:string;
    ZoomWidth:integer;
    PageRatio:double;
    procedure Preview(gpn:TPrintGetPageNum; dp:TPrintDrawPage; pc:TPrintConfigure; user:pointer; desc:string);
    procedure Prepare;
    procedure PreviewPage;
  end;

procedure GetPrintLine(width,height,origwidth,origheight,nolines:integer;var lineheight,linecount:integer);

var
  fPrint: TfPrint;

implementation

uses JWBBitmap, JWBMenu;

{$R *.DFM}

procedure TfPrint.Preview(gpn:TPrintGetPageNum; dp:TPrintDrawPage; pc:TPrintConfigure; user:pointer; desc:string);
begin
  GetPageNum:=gpn;
  DrawPage:=dp;
  UserData:=user;
  PrintConfigure:=pc;
  Description:=desc;
  Prepare;
  ShowModal;
end;

function Printer_PageWidth:integer;
begin
  try result:=Printer.PageWidth; except result:=Screen.Width; end;
end;

function Printer_PageHeight:integer;
begin
  try result:=Printer.PageHeight; except result:=Screen.Height; end;
end;

procedure TfPrint.Prepare;
begin
  PageNum:=GetPageNum(PaintBox1.Canvas, Printer_PageWidth, Printer_PageHeight, UserData);
  PageRatio:=Printer_PageHeight/Printer_PageWidth;
  CurPage:=1;
  ZoomWidth:=ClientWidth-50;
  SpeedButton8.Down:=true;
  PreviewPage;
end;

procedure TfPrint.PreviewPage;
var w,h:integer;
begin
  ScrollBox1.HorzScrollBar.Position:=0;
  ScrollBox1.VertScrollBar.Position:=0;
  if SpeedButton8.Down then ZoomWidth:=ClientWidth-50;
  SpeedButton1.Enabled:=CurPage<>1;
  SpeedButton2.Enabled:=CurPage>1;
  SpeedButton3.Enabled:=CurPage<PageNum;
  SpeedButton4.Enabled:=CurPage<>PageNum;
  SpeedButton9.Enabled:=ZoomWidth>100;
  SpeedButton8.Enabled:=true;
  Label1.Caption:=Description+' ('+inttostr(CurPage)+'/'+inttostr(PageNum)+')';
  w:=ZoomWidth-6;
  h:=round(w*PageRatio);
  PaintBox1.Left:=10;
  if w<ClientWidth-42 then PaintBox1.Left:=10+(ClientWidth-42-w) div 2;
  PaintBox1.Top:=10;
  PaintBox1.Width:=w;
  PaintBox1.Height:=h;
  ScrollBox1.HorzScrollBar.Range:=w+20;
  ScrollBox1.VertScrollBar.Range:=h+20;
  PaintBox1.Invalidate;
end;

procedure TfPrint.PaintBox1Paint(Sender: TObject);
begin
  PaintBox1.Canvas.Pen.Color:=clBlack;
  PaintBox1.Canvas.Brush.Color:=clWhite;
  PaintBox1.Canvas.Rectangle(0,0,PaintBox1.Width-1,PaintBox1.Height-1);
  DrawPage(PaintBox1.Canvas,CurPage,PaintBox1.Width,PaintBox1.Height,Printer_PageWidth,Printer_PageHeight,UserData);
end;

procedure TfPrint.FormResize(Sender: TObject);
begin
  PreviewPage;
end;

procedure TfPrint.SpeedButton1Click(Sender: TObject);
begin
  CurPage:=1;
  PreviewPage;
end;

procedure TfPrint.SpeedButton2Click(Sender: TObject);
begin
  dec(CurPage);
  PreviewPage;
end;

procedure TfPrint.SpeedButton3Click(Sender: TObject);
begin
  inc(CurPage);
  PreviewPage;
end;

procedure TfPrint.SpeedButton4Click(Sender: TObject);
begin
  CurPage:=PageNum;
  PreviewPage;
end;

procedure TfPrint.SpeedButton7Click(Sender: TObject);
begin
  inc(ZoomWidth,100);
  SpeedButton8.Down:=false;
  PreviewPage;
end;

procedure TfPrint.SpeedButton9Click(Sender: TObject);
begin
  dec(ZoomWidth,100);
  SpeedButton8.Down:=false;
  PreviewPage;
end;

procedure TfPrint.SpeedButton8Click(Sender: TObject);
begin
  ZoomWidth:=ClientWidth-50;
  PreviewPage;
end;

procedure TfPrint.SpeedButton5Click(Sender: TObject);
begin
  PrintConfigure(UserData);
  Prepare;
end;

procedure TfPrint.SpeedButton6Click(Sender: TObject);
var i:integer;
begin
  try
    Printer.Title:=Description;
  except
    Application.MessageBox(
      pchar(_l('#00897^ePrinter is not ready.')),
      pchar(_l('#00898^eError')),
      MB_ICONERROR or MB_OK);
    exit;
  end;
  Printer.BeginDoc;
  for i:=1 to PageNum do
  begin
    if i>1 then Printer.NewPage;
    Printer.Canvas.Pen.Color:=clWhite;
    Printer.Canvas.Brush.Color:=clWhite;
    Printer.Canvas.Rectangle(0,0,PaintBox1.Width-1,PaintBox1.Height-1);
    Printer.Canvas.Pen.Color:=clBlack;
    DrawPage(Printer.Canvas,i,Printer.PageWidth,Printer.PageHeight,Printer.PageWidth,Printer.PageHeight,UserData);
  end;
  Printer.EndDoc;
end;

procedure TfPrint.SpeedButton10Click(Sender: TObject);
begin
  PrinterSetupDialog1.Execute;
  Prepare;
end;

procedure GetPrintLine(width,height,origwidth,origheight,nolines:integer;var lineheight,linecount:integer);
var a,b:double;
begin
  a:=origwidth*origheight;
  b:=sqrt(a);
  linecount:=trunc(nolines/b*origheight);
  lineheight:=height div linecount;
end;

procedure TfPrint.SpeedButton11Click(Sender: TObject);
var bmp:TBitmap;
    i:integer;
    pn:integer;
begin
  if fBitmap.ShowModal=mrOK then
  begin
    bmp:=TBitmap.Create;
    bmp.PixelFormat:=pf24bit;
    try
      bmp.Width:=strtoint(fBitmap.Edit1.Text);
      bmp.Height:=strtoint(fBitmap.Edit2.Text);
    except end;
    pn:=GetPageNum(bmp.Canvas,bmp.Width,bmp.Height,UserData);
    Screen.Cursor:=crHourGlass;
    for i:=1 to pn do
    begin
      bmp.Canvas.Pen.Color:=clWhite;
      bmp.Canvas.Brush.Color:=clWhite;
      bmp.Canvas.Rectangle(0,0,bmp.Width-1,bmp.Height-1);
      bmp.Canvas.Pen.Color:=clBlack;
      DrawPage(bmp.Canvas,i,bmp.Width,bmp.Height,bmp.Width,bmp.Height,UserData);
      bmp.SaveToFile(fBitmap.Edit3.Text+inttostr(i)+'.bmp');
    end;
    Screen.Cursor:=crDefault;
    Prepare;
  end;
end;

end.
