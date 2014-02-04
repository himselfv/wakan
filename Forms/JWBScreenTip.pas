unit JWBScreenTip;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, Buttons, JWBStrings, JWBDicSearch;

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
    screenTipWt:TEvalCharType;
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

procedure ShowScreenTip(x,y:integer;s:FString;wt:TEvalCharType;immediate:boolean);
procedure HideScreenTip;

implementation

uses JWBUnit, JWBKanjiCard, JWBDic, JWBMenu, JWBSettings, JWBLegacyMarkup;

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
    wt:TEvalCharType;
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
    ss:=sl[i].ToLegacyString;
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
  if (wt=EC_IDG_CHAR) and (fSettings.CheckBox48.Checked) then
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

//Create screen tip form and show it
procedure ShowScreenTip(x,y:integer;s:FString;wt:TEvalCharType;immediate:boolean);
var maxwords,maxwordss:integer;
    wasfull:boolean;
    s1,s2:FString; //kinda fstring, has control chars
    s3,s4:string;
    ss:string;
    ch,kch:integer;
    rect:TRect;
    kkch,kkcw:integer;
    vsiz,hsiz,vfsiz,hfsiz:integer;
    i:integer;
    sep:integer;
    tpp:integer;
    optwidth,cw:integer;
    proposeds:string;
    maxslen,slen:integer;
begin
  if fScreenTip<>nil then HideScreenTip;
  if ((wt=EC_LATIN_HW) and (not fSettings.CheckBox47.Checked)) then exit;
  if ((wt<>EC_LATIN_HW) and (not fSettings.CheckBox28.Checked)) then exit;
  fScreenTip:=TfScreenTip.Create(nil);
  maxwords:=strtoint(fSettings.Edit24.Text);
  if maxwordss<10 then maxwordss:=10;
  if wt=EC_LATIN_HW then
  begin
    //Try to look for a latin word
    //DicSearch expects latin text to be raw, contrary to every other case when it's in FChars.
    DicSearch(fstrtouni(s),stEnglish,mtExactMatch,false,wt,maxwordss,fScreenTip.screenTipList,5,wasfull);
    if (fScreenTip.screenTipList.Count=0) then
    begin
      ss:=fstrtouni(s);
     //What the hell are we doing here?! "If nothing matches, try deleting
     //some letters, but only if those are 'ed' or 's'"?
     //I think this calls for a proper english deflexion function.
      if (length(ss)>2) and (copy(ss,length(ss)-1,2)='ed') then delete(ss,length(ss)-1,2) else
        if (length(ss)>1) and (ss[length(ss)]='s') then delete(ss,length(ss),1);
      DicSearch(ss,stEnglish,mtExactMatch,false,wt,maxwordss,fScreenTip.screenTipList,5,wasfull);
    end;
  end;
  if wt<>EC_LATIN_HW then
    DicSearch(s,stJapanese,mtExactMatch,false,wt,maxwordss,fScreenTip.screenTipList,5,wasfull);
  if maxwords>fScreenTip.screenTipList.Count then
    maxwords:=fScreenTip.screenTipList.Count;
  fScreenTip.screenTipWords:=maxwords;
  tpp:=20;
  ch:=GridFontSize+3;
  kch:=strtoint(fSettings.Edit26.Text);
  optwidth:=0;
  proposeds:='';
  maxslen:=0;
  for i:=0 to maxwords-1 do
  begin
    slen:=fScreenTip.screenTipList[i].slen;
    if slen>maxslen then maxslen:=slen;
    ss:=fScreenTip.screenTipList[i].ToLegacyString;
    SplitWord(ss,s1,s2,s3,s4);
    rect.left:=0;
    rect.right:=Screen.Width;
    rect.top:=0;
    rect.bottom:=100;
    s1 := remexcl(s1);
    s2 := remexcl(s2);
    cw:=DrawWordInfo(fScreenTip.pb.Canvas,rect,false,false,2,s3,false,true,GridFontSize,true)+GridFontSize*(3+flength(s1+s2));
    if cw>optwidth then optwidth:=cw;
  end;
  if maxslen>0 then proposeds:=fcopy(s,1,maxslen);
  vsiz:=5;
  hsiz:=20;
  optwidth:=optwidth-5*kch;
  optwidth:=optwidth div kch;
  if optwidth<strtoint(fSettings.Edit27.Text) then optwidth:=strtoint(fSettings.Edit27.Text);
  if optwidth>strtoint(fSettings.Edit28.Text) then optwidth:=strtoint(fSettings.Edit28.Text);
  fScreenTip.ScreenTipWidth:=optwidth;
  hsiz:=optwidth;
  sep:=4;
  vfsiz:=vsiz+5;
  hfsiz:=hsiz+vsiz+4;
  kkcw:=hfsiz*kch;
  kkch:=vfsiz*kch;
  fScreenTip.screenTipText:=s;
  if proposeds<>'' then
    fScreenTip.screenTipText:=proposeds;
  fScreenTip.screenTipWt:=wt;
  fScreenTip.Left:=x;
  fScreenTip.Top:=y;
  fScreenTip.Width:=kkcw+sep*2+1;
  if (wt=EC_IDG_CHAR) and (fSettings.CheckBox48.Checked) then
    fScreenTip.Height:=maxwords*ch+sep*3+kkch+tpp else fScreenTip.Height:=maxwords*ch+sep*2+1+tpp;
  if not immediate then
  begin
    if y+fScreenTip.Height>Screen.Height then fScreenTip.Top:=y-20-fScreenTip.Height;
    if x+fScreenTip.Width>Screen.Width then fScreenTip.Left:=Screen.Width-fScreenTip.Width;
  end;
  SetWindowPos(fScreenTip.handle,HWND_TOPMOST,0,0,0,0,SWP_NOMOVE or SWP_NOSIZE or SWP_NOACTIVATE or SWP_SHOWWINDOW);
  fScreenTip.screenTipButton:=0;
  fScreenTip.PopupMouseMove(Mouse.CursorPos.x-fScreenTip.left,Mouse.CursorPos.y-fScreenTip.Top);
end;

procedure HideScreenTip;
begin
  fScreenTip.Hide;
  fScreenTip.Free;
  fScreenTip:=nil;
end;

end.
