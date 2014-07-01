unit JWBScreenTip;
{ Wakan and system-wide popup hint }

{$IFDEF DEBUG}
  {$DEFINE HOOKDEBUG}
  //Enables hook debug messages to be stored in ScreenTip.FHookDebugMessages

{$ENDIF}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, Buttons, JWBStrings, JWBDicSearch;

type
  //Called when one of ScreenTip buttons has been pressed
  //ASender is TScreenTipForm which has additional info (target text)
  TButtonClickEvent = procedure(ASender: TObject; AButtonId: integer) of object;

  TfScreenTipForm = class(TForm)
    pb: TPaintBox;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure pbPaint(Sender: TObject);
    procedure pbMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure pbMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  protected
    FOnButtonClick: TButtonClickEvent;
    FScreenTipButton:integer;
    FScreenTipList: TSearchResults;
    FScreenTipWidth:integer;
  public
    screenTipText:string;
    screenTipWt:TEvalCharType;
    procedure DrawPopupButtons(sel:integer);
    property OnButtonClick: TButtonClickEvent read FOnButtonClick
      write FOnButtonClick;
  end;

  TScreenTip = class
  protected
    FEnabledInWakan: boolean;
    FEnabledSystemWide: boolean;
    FScreenTipForm: TfScreenTipForm; //<>nil ==> popup active
    FOnTipButtonClick: TButtonClickEvent;
    procedure SetEnabledSystemWide(const AValue: boolean);
    procedure SetOnTipButtonClick(const AValue: TButtonClickEvent);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Show(x,y: integer; s: FString; wt: TEvalCharType; immediate: boolean);
    procedure Hide;
    procedure PopupImmediate(left:boolean);
    procedure MaybePopup;
    property EnabledInWakan: boolean read FEnabledInWakan write FEnabledInWakan;
    property EnabledSystemWide: boolean read FEnabledSystemWide
      write SetEnabledSystemWide;
    property OnTipButtonClick: TButtonClickEvent read FOnTipButtonClick
      write SetOnTipButtonClick;

  //Screen tip hook
  protected
    ctlFileMap: cardinal;
    ptrFileMap: pointer;
   {$IFDEF HOOKDEBUG}
    FHookDebugMessages: string; //collects debug messages for the last scan
   {$ENDIF}
    procedure LoadScreenTipHook;
    procedure UnloadScreenTipHook;
    function HookGetTextUnderMouse(const pt: TPoint): string;
    procedure HookDebug(const msg: string; const ACrlf: boolean = true); inline;

  private //Text under mouse
    HandlingPopup:boolean; //set while we're doing popup handling -- TODO: Do we really need this?
    LastMousePt:TPoint; //used to check whether the mouse stays still
    LastMouseMove:cardinal; //tick count
    procedure HandlePopup(ShowImmediate:boolean=false);
    procedure IntTipMouseUp(Sender: TObject);

  end;

var
  ScreenTip: TScreenTip;


type
  TTextInfo = record
    hwnd:HWND;
    hdc:HDC;
    x,y,w,h:integer;
    slen:byte;
    str:array[0..255] of word;
    len:array[0..255] of byte;
    dcinfo:integer;
  end;
  TBitInfo = record
    hwnd:HWND;
    destdc,srcdc:HDC;
    xofs,yofs:integer;
  end;

const
  PopupButtonNum=4;
  PopupButtonWidth=23;
  PopupButtonSep=2;

implementation
uses JWBUnit, JWBKanjiCard, JWBDic, JWBLanguage, JWBSettings, JWBLegacyMarkup,
  JWBWordGrid, JWBIntTip;

{$R *.DFM}

procedure TfScreenTipForm.FormCreate(Sender: TObject);
begin
  FScreenTipList := TSearchResults.Create;
end;

procedure TfScreenTipForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FScreenTipList);
end;

procedure TfScreenTipForm.pbMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var sb:integer;
begin
  x:=x-2-PopupButtonSep;
  sb:=(x div (PopupButtonWidth+PopupButtonSep))+1;
  if (sb>PopupButtonNum) or (y<3) or (y>22) or (x mod (PopupButtonWidth+PopupButtonSep)>PopupButtonWidth) then
    sb:=0;
  if sb<>FScreenTipButton then begin
    FScreenTipButton:=sb;
    DrawPopupButtons(FScreenTipButton);
  end;
end;

procedure TfScreenTipForm.pbMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  Self.pbMouseMove(Sender, [], x, y);
  if Assigned(FOnButtonClick) then
    FOnButtonClick(Self, FScreenTipButton);
end;

procedure TfScreenTipForm.pbPaint(Sender: TObject);
var ss:string;
    ch,SizeFactor:integer;
    rect:TRect;
    kkch,kkcw:integer;
    vsiz,hsiz,vfsiz,hfsiz:integer;
    i:integer;
    s:string;
    wt:TEvalCharType;
    sep:integer;
    tpp:integer;
    FontJpCh:string;
begin
  s:=screenTipText;
  wt:=screenTipWt;
  tpp:=20;
  ch:=GridFontSize+3;
  SizeFactor:=StrToInt(fSettings.edtScreenTipSizeFactor.Text);
  vsiz:=5;
  hsiz:=FScreenTipWidth;
  sep:=4;
  vfsiz:=vsiz+5;
  hfsiz:=hsiz+vsiz+4;
  kkcw:=hfsiz*SizeFactor;
  kkch:=vfsiz*SizeFactor;
  Self.Width:=kkcw+sep*2;

 //Background color
  Self.pb.Canvas.Brush.Color:=Col('Popup_Back');
  Self.pb.Canvas.Pen.Color:=Col('Popup_Text');
  Self.pb.Canvas.Rectangle(0,0,Self.Width,Self.Height);

 //Popup buttons
  DrawPopupButtons(FScreenTipButton);

 //Text itself
  Self.pb.Canvas.Brush.Color:=Col('Popup_Back');
  Self.pb.Canvas.Pen.Color:=Col('Popup_Text');
  DrawUnicode(Self.pb.Canvas,sep+2+PopupButtonNum*PopupButtonWidth
    +PopupButtonNum*PopupButtonSep+sep,sep,tpp-4,s,FontSmall);

 //Words
  Self.pb.Canvas.Rectangle(sep,sep+tpp,Self.Width-sep,sep+FScreenTipList.Count*ch+tpp);
  if curlang='c'then FontJpCh:=FontChinese else FontJpCh:=FontJapanese;
  for i:=0 to FScreenTipList.Count-1 do
  begin
    ss:=FScreenTipList[i].ToLegacyString;
    rect.left:=sep+1;
    rect.right:=Self.Width-sep-2;
    rect.top:=sep+ch*i+1+tpp;
    rect.bottom:=sep+ch*i+ch+tpp;
    DrawPackedWordInfo(Self.pb.Canvas,rect,ss,ch,true);
    Self.pb.Canvas.MoveTo(sep+1,sep+ch*i+ch+tpp);
    Self.pb.Canvas.LineTo(Self.Width-sep-1,sep+ch*i+ch+tpp);
  end;

 //Kanji card
  Self.pb.canvas.Font.Style:=[];
  Self.pb.canvas.Font.Color:=Col('Popup_Text');
  if (wt=EC_IDG_CHAR) and fSettings.cbScreenTipForKanji.Checked then
  begin
    Self.pb.Canvas.Brush.Color:=Col('Popup_Card');
    Self.pb.Canvas.Pen.Color:=Col('Popup_Text');
    Self.pb.Canvas.Rectangle(sep,sep*2+FScreenTipList.Count*ch+tpp,Self.Width-sep,sep*2+FScreenTipList.Count*ch+kkch+tpp);
    Self.pb.Canvas.Pen.Color:=Col('Popup_Text');
    DrawKanjiCard(Self.pb.Canvas,fcopy(s,1,1),sep,sep*2+FScreenTipList.Count*ch+tpp,SizeFactor,
      [koPrintRadical, koPrintAlternateForm, koPrintInnerLines,
      koPrintVocabularyCompounds, koPrintReadings, koPrintDefinition,
      koPrintFullCompounds, koSortCompoundsByFrequency],
      hsiz,vsiz,2,FontJpCh);
    Self.pb.Canvas.Brush.Color:=Col('Popup_Text');
    Self.pb.Canvas.Pen.Color:=Col('Popup_Text');
    rect.left:=sep;
    rect.top:=tpp+sep*2+FScreenTipList.Count*ch;
    rect.right:=Self.Width-sep;
    rect.bottom:=Self.Height-sep;
    Self.pb.Canvas.FrameRect(rect);
  end;

 //Frame
  Self.pb.Canvas.Brush.Color:=Col('Popup_Text');
  Self.pb.Canvas.Pen.Color:=Col('Popup_Text');
  rect.left:=sep;
  rect.top:=tpp+sep;
  rect.right:=Self.Width-sep;
  rect.bottom:=tpp+sep+FScreenTipList.Count*ch+1;
  Self.pb.Canvas.FrameRect(rect);
  Self.pb.Canvas.Font.Height:=12;
  Self.pb.Canvas.Font.Name:='Arial';
  Self.pb.Canvas.Brush.Color:=clWhite;
  Self.pb.Canvas.Pen.Color:=clBlack;
end;

procedure TfScreenTipForm.DrawPopupButtons(sel:integer);
var i:integer;
    rect:TRect;
    s1,s2:string;
begin
  for i:=0 to PopupButtonNum-1 do
  begin
    Self.pb.Canvas.Pen.Color:=clBlack;
    if sel=i+1 then Self.pb.Canvas.Brush.Color:=clBlack else Self.pb.Canvas.Brush.Color:=clWhite;
    rect.left:=2+PopupButtonWidth*i+PopupButtonSep*(i+1);
    rect.right:=rect.left+PopupButtonWidth;
    rect.top:=3;
    rect.bottom:=22;
    Self.pb.Canvas.Rectangle(rect);
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
    if sel=i+1 then Self.pb.Canvas.Font.Color:=clWhite else Self.pb.Canvas.Font.Color:=clBlack;
    Self.pb.Canvas.Font.Name:='Arial';
    Self.pb.Canvas.Font.Height:=9;
    Self.pb.Canvas.Font.Style:=[];
    Self.pb.Canvas.TextRect(rect,rect.left+1,rect.top,_l(s1));
    rect.top:=rect.top+9;
    Self.pb.Canvas.TextRect(rect,rect.left+1,rect.top,_l(s2));
  end;
end;



constructor TScreenTip.Create;
begin
  inherited;
  LastMouseMove:=GetTickCount;
end;

destructor TScreenTip.Destroy;
begin
  UnloadScreenTipHook;
  inherited;
end;

procedure TScreenTip.SetEnabledSystemWide(const AValue: boolean);
begin
  if AValue=FEnabledSystemWide then exit;
  if FEnabledSystemWide then
    LoadScreenTipHook()
  else
    UnloadScreenTipHook();
  FEnabledSystemWide := AValue;
end;

procedure TScreenTip.SetOnTipButtonClick(const AValue: TButtonClickEvent);
begin
  FOnTipButtonClick := AValue;
  if FScreenTipForm<>nil then
    FScreenTipForm.OnButtonClick := AValue;
end;



procedure TScreenTip.LoadScreenTipHook;
begin
  if not FileExists('wakanh.dll') then
  begin
    Application.MessageBox(
      pchar(_l('#00367^eCannot find file WAKANH.DLL.')),
      pchar(_l('#00020^eError')),
      MB_ICONERROR or MB_OK);
    exit;
  end;
  Screen.Cursor:=crHourGlass;
  try
    ctlFileMap:=CreateFileMapping($FFFFFFFF,nil,PAGE_READWRITE,0,1,'wakanh_ctl_sharemem');
    if ctlFileMap=0 then
    begin
      showmessage('Win32 API error: CreateFileMap() failed.');
      exit;
    end;
    ptrFileMap:=MapViewOfFile(ctlFileMap,FILE_MAP_WRITE,0,0,1);
    if ptrFileMap=nil then
    begin
      showmessage('Win32 API error: MapViewOfFile() failed.');
      exit;
    end;
    byte(ptrFileMap^):=0;
{MCH    InjectLibrary(ALL_SESSIONS or SYSTEM_PROCESSES, 'wakanh.dll');}
  finally
    Screen.Cursor:=crDefault;
  end;
end;

procedure TScreenTip.UnloadScreenTipHook;
begin
  Screen.Cursor:=crHourGlass;
  try
    if ptrFileMap<>nil then begin
      UnMapViewOfFile(ptrFileMap);
      ptrFileMap := nil;
    end;
    if (ctlFileMap<>0) and (ctlFileMap<>INVALID_HANDLE_VALUE) then begin
      CloseHandle(ctlFileMap);
      ctlFileMap := INVALID_HANDLE_VALUE;
    end;
{MCH    UninjectLibrary(ALL_SESSIONS or SYSTEM_PROCESSES, 'wakanh.dll');}
  finally
    Screen.Cursor:=crDefault;
  end;
end;

procedure TScreenTip.HookDebug(const msg: string; const ACrlf: boolean);
begin
 {$IFDEF HOOKDEBUG}
  if ACrlf then
    FHookDebugMessages := FHookDebugMessages + msg + #13
  else
    FHookDebugMessages := FHookDebugMessages + msg;
 {$ENDIF}
end;


var
 { IPC stuff -- unfortunately has to be global for now }
  rdcnt: integer = 0;
  bitcnt: integer = 0;
  curtext:array[1..100] of TTextInfo;
  curbit:array[1..100] of TBitInfo;

procedure IPCCallback(name: pchar; messageBuf : pointer; messageLen : dword;
                      answerBuf  : pointer; answerLen  : dword); stdcall;
var mycnt:integer;
begin
  if messageLen<>sizeof(TTextInfo) then exit;
  inc(rdcnt);
  if rdcnt>90 then exit;
  mycnt:=rdcnt;
  move(messageBuf^,curtext[mycnt],sizeof(TTextInfo));
end;

procedure BitCallback(name: pchar; messageBuf : pointer; messageLen : dword;
                      answerBuf  : pointer; answerLen  : dword); stdcall;
var mycnt:integer;
begin
  if messageLen<>sizeof(TBitInfo) then exit;
  inc(bitcnt);
  if bitcnt>90 then exit;
  mycnt:=bitcnt;
  move(messageBuf^,curbit[mycnt],sizeof(TBitInfo));
end;

function TScreenTip.HookGetTextUnderMouse(const pt: TPoint): string;
var
  i, j, k: integer;
  fInvalidator: TForm; //very stupid
  tleft,tright:integer; //additional covered space to the left and right of the mouse
  ftext:array[0..255] of word;
  ftextbeg:array[0..255] of integer;
  ftextend:array[0..255] of integer;
  ftextpos:integer;
  savedx:array[1..100] of integer;
  savedy:array[1..100] of integer;
  ct:TTextInfo;
  ev,cev:TEvalCharType;
  wnd:HWnd;
  wt:shortstring;
  wr:TRect;
  lp:TPoint;
  wbg,wen:integer;
  cx:integer;
  gbg:array[0..255] of integer;
  gen:array[0..255] of integer;
  wtp:integer;
  last:integer;

  procedure wwadd(bg,en:integer;w:word);
  var b:integer;
      i,j:integer;
      ass:boolean;
  begin
    b:=0;
    for i:=0 to ftextpos-1 do if (ftextend[i]=en) and (ftextbeg[i]=bg) then
    begin end else
    begin
      ftext[b]:=ftext[i];
      ftextbeg[b]:=ftextbeg[i];
      ftextend[b]:=ftextend[i];
      inc(b);
    end;
    ftextpos:=b;
    ass:=false;
    for i:=0 to ftextpos-1 do if (not ass) and (ftextbeg[i]>bg) then
    begin
      for j:=ftextpos-1 downto i do
      begin
        ftext[j+1]:=ftext[j];
        ftextbeg[j+1]:=ftextbeg[j];
        ftextend[j+1]:=ftextend[j];
      end;
      ftext[i]:=w;
      ftextbeg[i]:=bg;
      ftextend[i]:=en;
      inc(ftextpos);
      ass:=true;
    end;
    if not ass then
    begin
      ftext[ftextpos]:=w;
      ftextbeg[ftextpos]:=bg;
      ftextend[ftextpos]:=en;
      inc(ftextpos);
    end;
  end;

begin
 {$IFDEF HOOKDEBUG}
  FHookDebugMessages := '';
 {$ENDIF}

  rdcnt:=0;
  bitcnt:=0;
  for i:=1 to 100 do curtext[i].slen:=0;

  if not TryStrToInt(fSettings.Edit22.Text, tleft) then tleft:=10;
  if not TryStrToInt(fSettings.Edit23.Text, tright) then tright:=100;

  //Cover the interesting area with a form, then remove it, forcing everyone
  //to redraw.
  //Meanwhile, tell hook dll to track redrawing.
  fInvalidator:=TForm.Create(nil);
  fInvalidator.Width:=tleft+tright;
  fInvalidator.Height:=1;
  fInvalidator.Top:=pt.y;
  fInvalidator.Left:=pt.x-tleft;
{MCH    CreateIPCQueue('texthook_data',IPCCallback);
    CreateIPCQueue('texthook_bit',BitCallback);}
  byte(ptrFileMap^):=1;
  SetWindowPos(fInvalidator.handle,HWND_TOPMOST,0,0,0,0,SWP_NOMOVE or SWP_NOSIZE or SWP_NOACTIVATE or SWP_SHOWWINDOW);
  fInvalidator.Hide;
  fInvalidator.Free;
  sleep(100);
  byte(ptrFileMap^):=0;
  {MCH    DestroyIPCQueue('texthook_data');
      DestroyIPCQueue('texthook_bit');}

  Result:='';
  if rdcnt>90 then rdcnt:=90;
  if bitcnt>90 then bitcnt:=90;

  // create debug
  for i:=1 to rdcnt do begin
    ct:=curtext[i];
    savedx[i]:=ct.x;
    savedy[i]:=ct.y;
    if (ct.dcinfo and (TA_TOP or TA_BOTTOM or TA_BASELINE))=TA_BOTTOM then ct.y:=ct.y-ct.h;
    if (ct.dcinfo and (TA_TOP or TA_BOTTOM or TA_BASELINE))=TA_BASELINE then ct.y:=ct.y-ct.h;
    if (ct.dcinfo and (TA_RIGHT or TA_LEFT or TA_CENTER))=TA_CENTER then ct.x:=ct.x-ct.w div 2;
    if (ct.dcinfo and (TA_LEFT or TA_RIGHT or TA_CENTER))=TA_RIGHT then ct.x:=ct.x-ct.w;
    curtext[i]:=ct;
  end;

  //Dump window information under cursor
  wnd:=WindowFromPoint(pt);
  wt[0]:=AnsiChar(chr(Windows.GetWindowText(wnd,@(wt[1]),255)));
  HookDebug('Window name: '+string(wt));
  Windows.GetWindowRect(wnd, wr);
  HookDebug('Window rect: ['+inttostr(wr.Left)+':'+
    inttostr(wr.Top)+']-['+inttostr(wr.Right)+':'+inttostr(wr.Bottom)+']');
  Windows.GetClientRect(wnd,wr);
  HookDebug('Client area: '+inttostr(wr.Right)+':'+inttostr(wr.Bottom));
  HookDebug('Cursor pos: '+inttostr(pt.x)+':'+inttostr(pt.y));

  HookDebug('BitBlts:');
  for i:=1 to bitcnt do begin
    HookDebug(inttostr(i)+'# Mod:', false);
    for j:=1 to rdcnt do
      if curbit[i].srcdc=curtext[j].hdc then begin
        HookDebug(inttostr(j)+';', false);
        curtext[j].hwnd:=curbit[i].hwnd;
        curtext[j].x:=curtext[j].x+curbit[i].xofs;
        curtext[j].y:=curtext[j].y+curbit[i].yofs;
        //Result:=Result+'T+'+inttostr(curbit[i].xofs)+'='+inttostr(curbit[i].yofs)+'+';
      end;
    HookDebug(' Ofs:'+inttostr(curbit[i].xofs)+':'+inttostr(curbit[i].yofs));
  end;

  HookDebug('TextOuts:');
  Windows.GetWindowRect(wnd,wr);
  for i:=1 to rdcnt do begin
    lp.x:=curtext[i].x;
    lp.y:=curtext[i].y;
    lp.x:=lp.x+wr.Left;
    lp.y:=lp.y+wr.Top;
    HookDebug(inttostr(i)+'# "', false);
    for j:=0 to curtext[i].slen-1 do if curtext[i].str[j]>=32 then
      HookDebug(widechar(curtext[i].str[j]), false);
    HookDebug('"'+
      ' Org:'+inttostr(savedx[i])+':'+inttostr(savedy[i])+
      ' Align:'+inttostr(curtext[i].x)+':'+inttostr(curtext[i].y)+
      ' Trans:'+inttostr(lp.x)+':'+inttostr(lp.y)+
      ' Size:'+inttostr(curtext[i].w)+':'+inttostr(curtext[i].h)+' ');
    if curtext[i].hwnd=wnd then
      HookDebug('OK')
    else
      HookDebug('BAD WND');
  end;

  ftextpos:=0;
  for i:=1 to rdcnt do
    if (curtext[i].slen>0) and (curtext[i].hwnd=wnd) then begin
      wbg:=0;
      lp.x:=curtext[i].x;
      lp.y:=curtext[i].y;
      Windows.GetWindowRect(wnd,wr);
      lp.x:=lp.x+wr.Left;
      lp.y:=lp.y+wr.Top;
      if (lp.y<=pt.y) and (lp.x+curtext[i].w>=pt.x) and (lp.y+curtext[i].h>=pt.y) then
      begin
        cx:=lp.x;
        wen:=-1;
        for j:=0 to curtext[i].slen-1 do
        begin
          gbg[j]:=cx;
          gen[j]:=cx+curtext[i].len[j];
          //HookDebug(curtext[i].str[j] mod 256, false);
          if wen=-1 then
          begin
            if (curtext[i].str[j]<ord('A')) or ((curtext[i].str[j]>ord('Z')) and
                (curtext[i].str[j]<ord('a'))) or (curtext[i].str[j]>ord('z')) then
                begin wbg:=j+1; wtp:=2; end else wtp:=1;
          end;
          if (cx+curtext[i].len[j]>pt.x) and (wen=-1) then
          begin
            if wbg>j then wbg:=j;
            if wbg=-1 then wbg:=0;
            wen:=wbg;
            if wtp=1 then while (wen+1<curtext[i].slen) and
              (((curtext[i].str[wen+1]>=ord('a')) and (curtext[i].str[wen+1]<=ord('z'))) or
               ((curtext[i].str[wen+1]>=ord('A')) and (curtext[i].str[wen+1]<=ord('Z')))) do inc(wen);
            if wtp=2 then wen:=wbg+10;
            if wen>=curtext[i].slen then wen:=curtext[i].slen-1;
          end;
          cx:=cx+curtext[i].len[j];
        end;
        if wen<>-1 then for k:=wbg to wen do
          wwadd(gbg[k],gen[k],curtext[i].str[k]);
        if wtp=1 then wwadd(gen[wen],gen[wen],32);
      end;
    end;

  cev:=EC_UNKNOWN;
  cx:=-100;
  last:=0;
  if (ftextpos>0) and (ftextbeg[0]<=pt.x+2) then
  for i:=0 to ftextpos-1 do begin
    ev:=EvalChar(WideChar(ftext[i]));
    if cev=EC_UNKNOWN then cev:=ev;
    if (cev=ev) or ((cev=EC_IDG_CHAR) and (ev=EC_HIRAGANA)) then
    begin
      if (ev<>EC_UNKNOWN) and ((ftext[i]<>last) or (ftextbeg[i]>cx+2)) then
        Result:=Result+fstr(widechar(ftext[i]))
    end else break;
    cx:=ftextbeg[i];
    last:=ftext[i];
    cev:=EvalChar(WideChar(ftext[i]));
  end;
end;



//Show screen tip for the given word at the specified point
procedure TScreenTip.Show(x,y: integer; s: FString; wt: TEvalCharType;
  immediate: boolean);
var wasfull:boolean;
    s1,s2:FString; //kinda fstring, has control chars
    s3:string;
    ss:string;
    ch,SizeFactor:integer;
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
  if FScreenTipForm<>nil then Self.Hide;
  if ((wt=EC_LATIN_HW) and not fSettings.cbScreenTipForEnglish.Checked) then exit;
  if ((wt<>EC_LATIN_HW) and not fSettings.cbScreenTipForJapanese.Checked) then exit;
  FScreenTipForm:=TfScreenTipForm.Create(nil);
  FScreenTipForm.OnButtonClick := Self.OnTipButtonClick;

 //Find matches
  if wt=EC_LATIN_HW then begin
    //Try to look for a latin word
    //DicSearch expects latin text to be raw, contrary to every other case when it's in FChars.
    DicSearch(fstrtouni(s),stEnglish,mtExactMatch,false,wt,-1,FScreenTipForm.FScreenTipList,5,wasfull);
    if FScreenTipForm.FScreenTipList.Count=0 then begin
      ss:=fstrtouni(s);
     //What the hell are we doing here?! "If nothing matches, try deleting
     //some letters, but only if those are 'ed' or 's'"?
     //I think this calls for a proper english deflexion function.
      if (length(ss)>2) and (copy(ss,length(ss)-1,2)='ed') then delete(ss,length(ss)-1,2) else
        if (length(ss)>1) and (ss[length(ss)]='s') then delete(ss,length(ss),1);
      DicSearch(ss,stEnglish,mtExactMatch,false,wt,-1,FScreenTipForm.FScreenTipList,5,wasfull);
    end;
  end else
    DicSearch(s,stJapanese,mtExactMatch,false,wt,-1,FScreenTipForm.FScreenTipList,5,wasfull);
 //We might want to pass MaxMathes to DicSearch before, but it would return us
 //FIRST N words ordered by frequency, not MOST FREQUENT N words.
 //So no choice but to match all, sort and trim here.
  FScreenTipForm.FScreenTipList.Trim(StrToInt(
    fSettings.edtScreenTipMaxDictEntries.Text));

  tpp:=20;
  ch:=GridFontSize+3;
  SizeFactor:=strtoint(fSettings.edtScreenTipSizeFactor.Text);
  optwidth:=0;
  proposeds:='';
  maxslen:=0;
  for i:=0 to FScreenTipForm.FScreenTipList.Count-1 do
  begin
    slen:=FScreenTipForm.FScreenTipList[i].slen;
    if slen>maxslen then maxslen:=slen;
    FScreenTipForm.FScreenTipList[i].ToLegacyParts(s1, s2, s3);
    rect.left:=0;
    rect.right:=Screen.Width;
    rect.top:=0;
    rect.bottom:=100;
    s1 := remexcl(s1);
    s2 := remexcl(s2);
    cw:=DrawWordInfo(FScreenTipForm.pb.Canvas,rect,false,false,2,s3,false,true,GridFontSize,true)+GridFontSize*(3+flength(s1+s2));
    if cw>optwidth then optwidth:=cw;
  end;
  if maxslen>0 then proposeds:=fcopy(s,1,maxslen);

  optwidth:=optwidth-5*SizeFactor;
  optwidth:=optwidth div SizeFactor;
  if optwidth<strtoint(fSettings.edtScreenTipMinCompounds.Text) then
    optwidth:=strtoint(fSettings.edtScreenTipMinCompounds.Text);
  if optwidth>strtoint(fSettings.edtScreenTipMaxCompounds.Text) then
    optwidth:=strtoint(fSettings.edtScreenTipMaxCompounds.Text);
  FScreenTipForm.FScreenTipWidth:=optwidth;

  vsiz:=5;
  hsiz:=optwidth;
  sep:=4;
  vfsiz:=vsiz+5;
  hfsiz:=hsiz+vsiz+4;
  kkcw:=hfsiz*SizeFactor;
  kkch:=vfsiz*SizeFactor;
  FScreenTipForm.screenTipText:=s;
  if proposeds<>'' then
    FScreenTipForm.screenTipText:=proposeds;
  FScreenTipForm.screenTipWt:=wt;
  FScreenTipForm.Left:=x;
  FScreenTipForm.Top:=y;
  FScreenTipForm.Width:=kkcw+sep*2+1;
  if (wt=EC_IDG_CHAR) and fSettings.cbScreenTipForKanji.Checked then
    FScreenTipForm.Height:=FScreenTipForm.FScreenTipList.Count*ch+sep*3+kkch+tpp
  else
    FScreenTipForm.Height:=FScreenTipForm.FScreenTipList.Count*ch+sep*2+1+tpp;
  if not immediate then
  begin
    if y+FScreenTipForm.Height>Screen.Height then FScreenTipForm.Top:=y-20-FScreenTipForm.Height;
    if x+FScreenTipForm.Width>Screen.Width then FScreenTipForm.Left:=Screen.Width-FScreenTipForm.Width;
  end;
  SetWindowPos(FScreenTipForm.handle,HWND_TOPMOST,0,0,0,0,SWP_NOMOVE or SWP_NOSIZE or SWP_NOACTIVATE or SWP_SHOWWINDOW);
  FScreenTipForm.FScreenTipButton:=0;
  FScreenTipForm.pbMouseMove(FScreenTipForm, [],
    Mouse.CursorPos.x-FScreenTipForm.left,Mouse.CursorPos.y-FScreenTipForm.Top);
end;

//Hide whatever screen tip might be showing right now
procedure TScreenTip.Hide;
begin
  FScreenTipForm.Hide;
  FScreenTipForm.Free;
  FScreenTipForm:=nil;
end;

//Immediately display a tip for the word under mouse (without waiting)
procedure TScreenTip.PopupImmediate(left:boolean);
begin
  HandlePopup({ShowImmediate=}true);
end;

//Check if the mouse has not been moving for a while and display a tip
//if neccessary
procedure TScreenTip.MaybePopup;
begin
  HandlePopup({ShowImmediate=}false);
end;

{ Shows or hides or updates popup, reacting to mouse movements.
 Call on timer, or possibly OnMouseMove, or manually with ShowImmediate=true. }
procedure TScreenTip.HandlePopup(ShowImmediate:boolean);
var pt:TPoint;
  s:string;
  intmosc:TPoint;
  ttim:integer;
  evc:TEvalCharType;
begin
  if not FEnabledInWakan and not FEnabledSystemWide and not ShowImmediate
    and (FScreenTipForm=nil) then exit;

  try
    pt:=Mouse.CursorPos;
  except
   //Mouse.CursorPos can raise EOSError on some versions of Delphi,
   //as underlying WINAPI GetCursorPos returns false if this is not the active desktop.
    on E: EOSError do exit;
  end;

  if HandlingPopup then exit;
  HandlingPopup:=true;
  try
    if not TryStrToInt(fSettings.Edit21.Text, ttim) then ttim:=10;

   //Popup active + mouse inside popup => exit
    if (FScreenTipForm<>nil)
    and (pt.x>=FScreenTipForm.Left-10)
    and (pt.y>=FScreenTipForm.Top-10)
    and (pt.x<=FScreenTipForm.Left+FScreenTipForm.Width+10)
    and (pt.y<=FScreenTipForm.Top+FScreenTipForm.Height+10) then
      exit;

   //Mouse moved => hide popup, reset popup timer
    if (not ShowImmediate) and ((pt.x<>LastMousePt.x) or (pt.y<>LastMousePt.y)) then
    begin
      if FScreenTipForm<>nil then
        Self.Hide;
      LastMousePt:=pt;
      LastMouseMove:=GetTickCount;
      exit;
    end;

   //Do not show popup if we're doing drag-selection
    if (not ShowImmediate) and (FScreenTipForm=nil) and IntTip.IsDragging then
      exit;

   //Wait for popup delay
    if (not ShowImmediate) and (FScreenTipForm=nil)
    and (GetTickCount()-LastMouseMove<cardinal(ttim)*100) then
      exit;

    s:='';
    if FEnabledInWakan or ShowImmediate then begin
     //Popup delay might expire while we're over some unrelated place
      if IntTip.HoverCtl<>nil then begin
        intmosc:=IntTip.HoverCtl.ClientToScreen(IntTip.HoverPt);
        if (pt.x=intmosc.x) and (pt.y=intmosc.y) then
          s:=IntTip.StringUnderMouse;
      end;
    end;

    if (s='') and FEnabledSystemWide then
      s := Self.HookGetTextUnderMouse(pt);

    if s<>'' then begin
      evc:=EvalChar(fgetch(s,1));
      IntTip.ResetHighlight;
      if ShowImmediate then
        Self.Show(pt.x-10,pt.y-10,s,evc,true)
      else
        Self.Show(pt.x+10,pt.y+10,s,evc,false);
    end;

  finally
    HandlingPopup:=false;
  end;
end;

procedure TScreenTip.IntTipMouseUp(Sender: TObject);
begin
  Self.PopupImmediate(true);
end;

initialization
  ScreenTip := TScreenTip.Create;
  IntTip.OnMouseUp := ScreenTip.IntTipMouseUp;

finalization
  FreeAndNil(ScreenTip);

end.
