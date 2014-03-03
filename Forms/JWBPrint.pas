unit JWBPrint;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, Buttons, Printers, JwbForms;

type
  TPrintGetPageNum=function (canvas:TCanvas; width,height:integer; userdata:pointer):integer;
  TPrintDrawPage=procedure (canvas:TCanvas; pagenum:integer; width,height,origwidth,origheight:integer; userdata:pointer);
  TPrintConfigure=procedure (userdata:pointer);

  TfPrint = class(TJwbForm)
    Panel1: TPanel;
    btnGotoFirst: TSpeedButton;
    btnGotoPrev: TSpeedButton;
    btnGotoNext: TSpeedButton;
    btnGotoLast: TSpeedButton;
    btnPrintConfigure: TSpeedButton;
    btnPrint: TSpeedButton;
    btnZoomIn: TSpeedButton;
    btnZoomFit: TSpeedButton;
    btnZoomOut: TSpeedButton;
    Shape1: TShape;
    Label1: TLabel;
    ScrollBox1: TScrollBox;
    pbPrintPreview: TPaintBox;
    PrinterSetupDialog1: TPrinterSetupDialog;
    btnPageSettings: TSpeedButton;
    btnPrintToFile: TSpeedButton;
    procedure pbPrintPreviewPaint(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure btnGotoFirstClick(Sender: TObject);
    procedure btnGotoPrevClick(Sender: TObject);
    procedure btnGotoNextClick(Sender: TObject);
    procedure btnGotoLastClick(Sender: TObject);
    procedure btnZoomInClick(Sender: TObject);
    procedure btnZoomOutClick(Sender: TObject);
    procedure btnZoomFitClick(Sender: TObject);
    procedure btnPrintConfigureClick(Sender: TObject);
    procedure btnPrintClick(Sender: TObject);
    procedure btnPageSettingsClick(Sender: TObject);
    procedure btnPrintToFileClick(Sender: TObject);
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
procedure PrintPreview(gpn:TPrintGetPageNum; dp:TPrintDrawPage; pc:TPrintConfigure; user:pointer; desc:string);

implementation
uses JWBBitmap, JWBLanguage;

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
  PageNum:=GetPageNum(pbPrintPreview.Canvas, Printer_PageWidth, Printer_PageHeight, UserData);
  PageRatio:=Printer_PageHeight/Printer_PageWidth;
  CurPage:=1;
  ZoomWidth:=ClientWidth-50;
  btnZoomFit.Down:=true;
  PreviewPage;
end;

procedure TfPrint.PreviewPage;
var w,h:integer;
begin
  ScrollBox1.HorzScrollBar.Position:=0;
  ScrollBox1.VertScrollBar.Position:=0;
  if btnZoomFit.Down then ZoomWidth:=ClientWidth-50;
  btnGotoFirst.Enabled:=CurPage<>1;
  btnGotoPrev.Enabled:=CurPage>1;
  btnGotoNext.Enabled:=CurPage<PageNum;
  btnGotoLast.Enabled:=CurPage<>PageNum;
  btnZoomOut.Enabled:=ZoomWidth>100;
  btnZoomFit.Enabled:=true;
  Label1.Caption:=Description+' ('+inttostr(CurPage)+'/'+inttostr(PageNum)+')';
  w:=ZoomWidth-6;
  h:=round(w*PageRatio);
  pbPrintPreview.Left:=10;
  if w<ClientWidth-42 then pbPrintPreview.Left:=10+(ClientWidth-42-w) div 2;
  pbPrintPreview.Top:=10;
  pbPrintPreview.Width:=w;
  pbPrintPreview.Height:=h;
  ScrollBox1.HorzScrollBar.Range:=w+20;
  ScrollBox1.VertScrollBar.Range:=h+20;
  pbPrintPreview.Invalidate;
end;

procedure TfPrint.pbPrintPreviewPaint(Sender: TObject);
begin
  pbPrintPreview.Canvas.Pen.Color:=clBlack;
  pbPrintPreview.Canvas.Brush.Color:=clWhite;
  pbPrintPreview.Canvas.Rectangle(0,0,pbPrintPreview.Width-1,pbPrintPreview.Height-1);
  DrawPage(pbPrintPreview.Canvas,CurPage,pbPrintPreview.Width,pbPrintPreview.Height,Printer_PageWidth,Printer_PageHeight,UserData);
end;

procedure TfPrint.FormResize(Sender: TObject);
begin
  PreviewPage;
end;

procedure TfPrint.btnGotoFirstClick(Sender: TObject);
begin
  CurPage:=1;
  PreviewPage;
end;

procedure TfPrint.btnGotoPrevClick(Sender: TObject);
begin
  dec(CurPage);
  PreviewPage;
end;

procedure TfPrint.btnGotoNextClick(Sender: TObject);
begin
  inc(CurPage);
  PreviewPage;
end;

procedure TfPrint.btnGotoLastClick(Sender: TObject);
begin
  CurPage:=PageNum;
  PreviewPage;
end;

procedure TfPrint.btnZoomInClick(Sender: TObject);
begin
  inc(ZoomWidth,100);
  btnZoomFit.Down:=false;
  PreviewPage;
end;

procedure TfPrint.btnZoomOutClick(Sender: TObject);
begin
  dec(ZoomWidth,100);
  btnZoomFit.Down:=false;
  PreviewPage;
end;

procedure TfPrint.btnZoomFitClick(Sender: TObject);
begin
  ZoomWidth:=ClientWidth-50;
  PreviewPage;
end;

procedure TfPrint.btnPrintConfigureClick(Sender: TObject);
begin
  PrintConfigure(UserData);
  Prepare;
end;

procedure TfPrint.btnPrintClick(Sender: TObject);
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
    Printer.Canvas.Rectangle(0,0,pbPrintPreview.Width-1,pbPrintPreview.Height-1);
    Printer.Canvas.Pen.Color:=clBlack;
    DrawPage(Printer.Canvas,i,Printer.PageWidth,Printer.PageHeight,Printer.PageWidth,Printer.PageHeight,UserData);
  end;
  Printer.EndDoc;
end;

procedure TfPrint.btnPageSettingsClick(Sender: TObject);
begin
  PrinterSetupDialog1.Execute;
  Prepare;
end;

procedure TfPrint.btnPrintToFileClick(Sender: TObject);
var AParams: TBitmapParams;
  bmp:TBitmap;
  i:integer;
  pn:integer;
begin
  if not QuerySaveToBitmap(AParams) then
    exit;

  bmp:=TBitmap.Create;
  bmp.PixelFormat:=pf24bit;
  bmp.Width:=AParams.Width;
  bmp.Height:=AParams.Height;
  pn:=GetPageNum(bmp.Canvas,bmp.Width,bmp.Height,UserData);
  Screen.Cursor:=crHourGlass;
  for i:=1 to pn do
  begin
    bmp.Canvas.Pen.Color:=clWhite;
    bmp.Canvas.Brush.Color:=clWhite;
    bmp.Canvas.Rectangle(0,0,bmp.Width-1,bmp.Height-1);
    bmp.Canvas.Pen.Color:=clBlack;
    DrawPage(bmp.Canvas,i,bmp.Width,bmp.Height,bmp.Width,bmp.Height,UserData);
    bmp.SaveToFile(AParams.Filename+inttostr(i)+'.bmp');
  end;
  Screen.Cursor:=crDefault;
  Prepare;
end;

procedure GetPrintLine(width,height,origwidth,origheight,nolines:integer;var lineheight,linecount:integer);
var a,btnGotoLast:double;
begin
  a:=origwidth*origheight;
  btnGotoLast:=sqrt(a);
  linecount:=trunc(nolines/btnGotoLast*origheight);
  lineheight:=height div linecount;
end;

procedure PrintPreview(gpn:TPrintGetPageNum; dp:TPrintDrawPage; pc:TPrintConfigure; user:pointer; desc:string);
var fPrint: TfPrint;
begin
  fPrint := TfPrint.Create(Application);
  try
    fPrint.Preview(gpn, dp, pc, user, desc);
  finally
    FreeAndNil(fPrint);
  end;
end;

end.
