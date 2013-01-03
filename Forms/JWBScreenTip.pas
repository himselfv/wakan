unit JWBScreenTip;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, Buttons,JWBDicSearch;

type
  TfScreenTip = class(TForm)
    pb: TPaintBox;
    procedure pbPaint(Sender: TObject);
    procedure pbMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure pbMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  public
    screenTipText:string;
    screenTipWt:integer;
    screenTipList:TSearchResults;
    screenTipWords:integer;
    screenTipWidth:integer;
    screenTipButton:integer;
    procedure PaintScreenTip;
    procedure DrawPopupButtons(sel:integer);
    procedure PopupMouseMove(x,y:integer);
  end;

var
  fScreenTip: TfScreenTip;

const
  PopupButtonNum=4;
  PopupButtonWidth=23;
  PopupButtonSep=2;

implementation

uses JWBStrings, JWBUnit, JWBKanjiCard, JWBMenu, JWBSettings;

{$R *.DFM}

procedure TfScreenTip.FormCreate(Sender: TObject);
begin
  screenTipList:=TSearchResults.Create;
end;

procedure TfScreenTip.FormDestroy(Sender: TObject);
begin
  FreeAndNil(screenTipList);
end;

procedure TfScreenTip.pbPaint(Sender: TObject);
begin
  PaintScreenTip;
end;

procedure TfScreenTip.pbMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  PopupMouseMove(x,y);
end;

procedure TfScreenTip.pbMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  fMenu.PopupMouseUp(button,shift,x,y);
end;

procedure TfScreenTip.PaintScreenTip;
var sl:TSearchResults;
    maxwords:integer;
    ss:string;
    ch,kch:integer;
    rect:TRect;
    kkch,kkcw:integer;
    vsiz,hsiz,vfsiz,hfsiz:integer;
    i:integer;
    s:string;
    wt:integer;
    sep:integer;
    tpp:integer;
    cl:TColor;
    FontJpCh:string;
begin
  s:=screenTipText;
  wt:=screenTipWt;
  sl:=screenTipList;
  cl:=Col('Popup_Back');
  maxwords:=strtoint(fSettings.Edit24.Text);
  if maxwords>sl.Count then maxwords:=sl.Count;
  maxwords:=screenTipWords;
  tpp:=20;
  ch:=GridFontSize+3;
  kch:=strtoint(fSettings.Edit26.Text);
  vsiz:=5;
  hsiz:=screenTipWidth;
  sep:=4;
  vfsiz:=vsiz+5;
  hfsiz:=hsiz+vsiz+4;
  kkcw:=hfsiz*kch;
  kkch:=vfsiz*kch;
  fScreenTip.Width:=kkcw+sep*2;
  fScreenTip.pb.Canvas.Brush.Color:=cl;
  fScreenTip.pb.Canvas.Pen.Color:=Col('Popup_Text');
  fScreenTip.pb.Canvas.Rectangle(0,0,fScreenTip.Width,fScreenTip.Height);
//  fScreenTip.pb.Canvas.Brush.Color:=cl;
//  fScreenTip.pb.Canvas.Pen.Color:=clBlack;
//  fScreenTip.pb.Canvas.Rectangle(sep,sep,fScreenTip.Width-sep,tpp+1+sep);
  DrawPopupButtons(screenTipButton);
  fScreenTip.pb.Canvas.Brush.Color:=cl;
  fScreenTip.pb.Canvas.Pen.Color:=Col('Popup_Text');
  DrawUnicode(fScreenTip.pb.Canvas,sep+2+PopupButtonNum*PopupButtonWidth+PopupButtonNum*PopupButtonSep+sep,sep,tpp-4,s,FontSmall);
  fScreenTip.pb.Canvas.Rectangle(sep,sep+tpp,fScreenTip.Width-sep,sep+maxwords*ch+tpp);
  if curlang='c'then FontJpCh:=FontChinese else FontJpCh:=FontJapanese;
  for i:=0 to maxwords-1 do
  begin
    ss:=sl[i].ArticlesToString;
    rect.left:=sep+1;
    rect.right:=fScreenTip.Width-sep-2;
    rect.top:=sep+ch*i+1+tpp;
    rect.bottom:=sep+ch*i+ch+tpp;
    DrawPackedWordInfo(fScreenTip.pb.Canvas,rect,ss,ch,true);
    fScreenTip.pb.Canvas.MoveTo(sep+1,sep+ch*i+ch+tpp);
    fScreenTip.pb.Canvas.LineTo(fScreenTip.Width-sep-1,sep+ch*i+ch+tpp);
  end;
  fScreenTip.pb.canvas.Font.Style:=[];
  fScreenTip.pb.canvas.Font.Color:=Col('Popup_Text');
  if (wt=1) and (fSettings.CheckBox48.Checked) then
  begin
    fScreenTip.pb.Canvas.Brush.Color:=Col('Popup_Card');
    fScreenTip.pb.Canvas.Pen.Color:=Col('Popup_Text');
    fScreenTip.pb.Canvas.Rectangle(sep,sep*2+maxwords*ch+tpp,fScreenTip.Width-sep,sep*2+maxwords*ch+kkch+tpp);
    fScreenTip.pb.Canvas.Pen.Color:=Col('Popup_Text');
    DrawKanjiCard(fScreenTip.pb.Canvas,fcopy(s,1,1),sep,sep*2+maxwords*ch+tpp,
      kch,false,false,true,true,true,true,true,true,false,true,true,hsiz,vsiz,2,FontJpCh);
    fScreenTip.pb.Canvas.Brush.Color:=Col('Popup_Text');
    fScreenTip.pb.Canvas.Pen.Color:=Col('Popup_Text');
    rect.left:=sep;
    rect.top:=tpp+sep*2+maxwords*ch;
    rect.right:=fScreenTip.Width-sep;
    rect.bottom:=fScreenTip.Height-sep;
    fScreenTip.pb.Canvas.FrameRect(rect);
  end;
  fScreenTip.pb.Canvas.Brush.Color:=Col('Popup_Text');
  fScreenTip.pb.Canvas.Pen.Color:=Col('Popup_Text');
  rect.left:=sep;
  rect.top:=tpp+sep;
  rect.right:=fScreenTip.Width-sep;
  rect.bottom:=tpp+sep+maxwords*ch+1;
  fScreenTip.pb.Canvas.FrameRect(rect);
  fScreenTip.pb.Canvas.Font.Height:=12;
  fScreenTip.pb.Canvas.Font.Name:='Arial';
  fScreenTip.pb.Canvas.Brush.Color:=clWhite;
  fScreenTip.pb.Canvas.Pen.Color:=clBlack;
//  DrawUnicode(fScreenTip.pb.Canvas,7,fScreenTip.Height-20,12,screenTipDebug,FontSmall);
end;

procedure TfScreenTip.DrawPopupButtons(sel:integer);
var i:integer;
    rect:TRect;
    s1,s2:string;
begin
  for i:=0 to PopupButtonNum-1 do
  begin
    fScreenTip.pb.Canvas.Pen.Color:=clBlack;
    if sel=i+1 then fScreenTip.pb.Canvas.Brush.Color:=clBlack else fScreenTip.pb.Canvas.Brush.Color:=clWhite;
    rect.left:=2+PopupButtonWidth*i+PopupButtonSep*(i+1);
    rect.right:=rect.left+PopupButtonWidth;
    rect.top:=3;
    rect.bottom:=22;
    fScreenTip.pb.Canvas.Rectangle(rect);
    case i of
      0:s1:='#00869^eClip';
      1:s1:='#00869^eClip';
      2:s1:='#00870^eShow';
      3:s1:='#00870^eShow';
    end;
    case i of
      0:s2:='#00871^eAdd';
      1:s2:='#00872^eRepl';
      2:s2:='#00873^eDict';
      3:s2:='#00874^eChar';
    end;
    rect.left:=rect.left+1;
    rect.top:=rect.top+1;
    rect.right:=rect.right-1;
    rect.bottom:=rect.bottom-1;
    if sel=i+1 then fScreenTip.pb.Canvas.Font.Color:=clWhite else fScreenTip.pb.Canvas.Font.Color:=clBlack;
    fScreenTip.pb.Canvas.Font.Name:='Arial';
    fScreenTip.pb.Canvas.Font.Height:=9;
    fScreenTip.pb.Canvas.Font.Style:=[];
    fScreenTip.pb.Canvas.TextRect(rect,rect.left+1,rect.top,_l(s1));
    rect.top:=rect.top+9;
    fScreenTip.pb.Canvas.TextRect(rect,rect.left+1,rect.top,_l(s2));
  end;
end;

procedure TfScreenTip.PopupMouseMove(x,y:integer);
var sb:integer;
begin
  x:=x-2-PopupButtonSep;
  sb:=(x div (PopupButtonWidth+PopupButtonSep))+1;
  if (sb>PopupButtonNum) or (y<3) or (y>22) or (x mod (PopupButtonWidth+PopupButtonSep)>PopupButtonWidth) then
    sb:=0;
  if sb<>screenTipButton then
  begin
    screenTipButton:=sb;
    DrawPopupButtons(screenTipButton);
  end;
end;

end.
