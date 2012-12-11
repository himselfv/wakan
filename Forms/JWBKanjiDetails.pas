unit JWBKanjiDetails;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, Buttons, StdCtrls, RXCtrls, rxPlacemnt, UrlLabel, JWBStrings;

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
    ProUrlLabel1: TUrlLabel;
    ProUrlLabel2: TUrlLabel;
    ProUrlLabel3: TUrlLabel;
    ProUrlLabel4: TUrlLabel;
    ProUrlLabel5: TUrlLabel;
    Label2: TLabel;
    Button2: TButton;
    SpeedButton1: TSpeedButton;
    ComboBox1: TComboBox;
    RxLabel1: TRxLabel;
    Label3: TLabel;
    procedure PaintBox1Paint(Sender: TObject);
    procedure PaintBox2Paint(Sender: TObject);
    procedure PaintBox4Paint(Sender: TObject);
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
  protected
    function InfoPaint(canvas:TCanvas;w:integer;onlycount:boolean):integer;
  public
    procedure SetCharDetails(unicode: FString);
  end;

var
  fKanjiDetails: TfKanjiDetails;

  curradno: string;
  cursimple: FString;

implementation

uses ShellApi, MemSource, JWBDicSearch, JWBKanji, JWBMenu, JWBTranslate,
  JWBSettings, JWBStrokeOrder, JWBUnit;

{$R *.DFM}

var
  curon,curkun: FString; //not used anywhere outside of SetCharDetails! Why the hell are these unit-global...
  curnanori: string; //same
  curindex: integer; //same
  curpinyin: string; //same

procedure TfKanjiDetails.PaintBox1Paint(Sender: TObject);
var f:string;
begin
  if chin then
  case fSettings.RadioGroup5.ItemIndex of
    0:f:=FontChinese;
    1:f:=FontChineseGB;
    2:f:=FontRadical;
  end else f:=FontJapanese;
  if (fKanjiDetails.SpeedButton1.Down) then
    if chin then f:=FontChinese else f:=FontStrokeOrder;
  fKanjiDetails.PaintBox1.Canvas.Brush.Color:=clWindow;
  fKanjiDetails.PaintBox1.Canvas.Font.Style:=[];
  DrawUnicode(fKanjiDetails.PaintBox1.Canvas,1,1,137,JWBKanji.curkanji,f);
  if fKanjiDetails.SpeedButton1.Down then DrawStrokeOrder(fKanjiDetails.PaintBox1.Canvas,1,1,137,137,curkanji,12,clBlue);
end;

procedure TfKanjiDetails.PaintBox2Paint(Sender: TObject);
var f:string;
begin
  f:=FontRadical;
  fKanjiDetails.PaintBox2.Canvas.Brush.Color:=clWindow;
  fKanjiDetails.PaintBox2.Canvas.Font.Style:=[];
  BeginDrawReg(fKanjiDetails.PaintBox2);
  DrawUnicode(fKanjiDetails.PaintBox2.Canvas,1,1,48,curradical,f);
  EndDrawReg;
end;

procedure TfKanjiDetails.PaintBox4Paint(Sender: TObject);
begin
  fKanjiDetails.PaintBox4.Canvas.Brush.Color:=clWindow;
  fKanjiDetails.PaintBox4.Canvas.Font.Style:=[];
  BeginDrawReg(fKanjiDetails.PaintBox4);
  DrawUnicode(fKanjiDetails.PaintBox4.Canvas,1,1,48,cursimple,FontRadical);
  EndDrawReg;
end;


procedure TfKanjiDetails.SpeedButton21Click(Sender: TObject);
begin
  if curkanji<>'' then
    SetKnown(strtoint(kanjicatuniqs[fKanjiDetails.ComboBox1.ItemIndex]),curkanji,
      not IsKnown(strtoint(kanjicatuniqs[fKanjiDetails.ComboBox1.ItemIndex]),curkanji));
  fMenu.ChangeUserData;
  fKanjiDetails.SetCharDetails(curkanji);
end;

procedure TfKanjiDetails.SpeedButton28Click(Sender: TObject);
var s:string;
begin
  s:='StrokeOrder\'+curkanji+'.gif';
  ShellExecute(handle,nil,pchar(s),nil,nil,SW_SHOW);
end;

procedure TfKanjiDetails.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  fKanji.SpeedButton2.Down:=false;
  fTranslate.sbDockKanjiDetails.Down:=false;
  fMenu.aKanjiDetails.Checked:=false;
  if not CharDetDocked then FormPlacement1.SaveFormPlacement;
end;

procedure TfKanjiDetails.SpeedButton23Click(Sender: TObject);
begin
  clip:=clip+curkanji;
  fMenu.ChangeClipboard;
end;

procedure TfKanjiDetails.SpeedButton1Click(Sender: TObject);
begin
  PaintBox1.Invalidate;
end;

procedure TfKanjiDetails.PaintBox3Paint(Sender: TObject);
begin
  BeginDrawReg(PaintBox3);
  InfoPaint(PaintBox3.Canvas,PaintBox3.Width,false);
  EndDrawReg;
end;

procedure TfKanjiDetails.FormShow(Sender: TObject);
begin
  if Visible then fMenu.aKanjiDetails.Checked:=true;
  if CharDetDocked then Button2.Caption:=_l('#00172^eUndock') else
    Button2.Caption:=_l('#00173^eDock');
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

procedure TfKanjiDetails.SetCharDetails(unicode:FString);
var piny,engy,kory,s,cany,chiny:string;
    ony,kuny,nany:widestring;
    ws:widestring;
    sl:TStringList;
    radf:integer;
    i,j,k:integer;
    kix:FString;
    kig:string;
    stp:string;
    mf:TMemoryFile;
    ms:TMemoryStream;
    ld:boolean;
    dic:TJaletDic;
    h:integer;
    adddot:integer;
    s2:string;
    cv:string;
    scat:string;
    cv_i1, cv_i2: integer;
  charval: string; //GetCharValue
begin
  curkanji:=UH_NOCHAR;
  curradical:='';
  curon:='';
  curkun:='';
  curnanori:='';
  cursimple:='';
  PaintBox1.Invalidate;
  PaintBox2.Invalidate;
  PaintBox3.Invalidate;
  RxLabel35.Hide;
  PaintBox4.Hide;
  Shape10.Hide;
  Label1.Caption:='-';
  kix:=unicode;
  if not TChar.Locate('Unicode',kix,false) then exit;
  Screen.Cursor:=crHourGlass;
  curkanji:=TChar.FCh(TCharUnicode);
  curindex:=TChar.Int(TCharIndex);
  ld:=false;
  cv:=fMenu.GetCharValue(curindex,51);
  ProURLLabel1.Enabled:=cv<>'';
  ProURLLabel1.URL:='http://www.zhongwen.com/cgi-bin/zipux2.cgi?b5=%'+copy(cv,1,2)+'%'+copy(cv,3,2);
  ProURLLabel2.Enabled:=TChar.Int(TCharChinese)=0;
  ProURLLabel2.URL:='http://www.csse.monash.edu.au/cgi-bin/cgiwrap/jwb/wwwjdic?1MKU'+lowercase(curKanji);
  ProURLLabel3.Enabled:=true;
  ProURLLabel3.URL:='http://charts.unicode.org/unihan/unihan.acgi$0x'+lowercase(curKanji);
  cv:=fMenu.GetCharValue(curindex,54);
  ProURLLabel4.Enabled:=(cv<>'')
    and TryStrToInt(copy(cv,1,2), cv_i1)
    and TryStrToInt(copy(cv,3,2), cv_i2);
  if ProURLLabel4.Enabled then
    ProURLLabel4.URL:='www.ocrat.com/chargif/GB/horiz/'+lowercase(Format('%2x%2x',[cv_i1+160,cv_i2+160]))+'.html';
  cv:=fMenu.GetCharValue(curindex,57);
  ProURLLabel5.Enabled:=cv<>'';
  ProURLLabel5.URL:='http://web.mit.edu/jpnet/ji/data/'+cv+'.html';
  fStrokeOrder.TrackBar1.Max:=0;
  if (fMenu.StrokeOrderPackage<>nil) and (fMenu.GetCharValueInt(TChar.Int(TCharIndex),101)<65535) then
  begin
    try
      s:=fMenu.GetCharValue(TChar.Int(TCharIndex),101);
      while length(s)<4 do s:='0'+s;
      mf:=fMenu.StrokeOrderPackage.Files['so'+s+'.gif'];
      if mf<>nil then
      begin
        ms:=mf.Lock;
        fStrokeOrder.RxGIFAnimator1.Image.LoadFromStream(ms);
        ld:=true;
        fStrokeOrder.strokenum:=0;
        i:=0;
        fStrokeOrder.RxGIFAnimator1.FrameIndex:=1;
        while fStrokeOrder.RxGIFAnimator1.FrameIndex>i do
        begin
          inc(i);
          fStrokeOrder.RxGIFAnimator1.FrameIndex:=i+1;
        end;
        fStrokeOrder.strokenum:=i;
        fStrokeOrder.TrackBar1.Max:=i;
        fStrokeOrder.TrackBar1.Position:=i;
        fStrokeOrder.RxGIFAnimator1.FrameIndex:=i;
      end;
    except
    end;
  end;
  fStrokeOrder.RxGIFAnimator1.Visible:=ld;
  fStrokeOrder.Label1.Visible:=not ld;
  radf:=fSettings.ComboBox1.ItemIndex+12;

  charval := fMenu.GetCharValue(TChar.Int(TCharIndex),43);
  if charval<>'' then
  begin
    cursimple:=hextofstr(charval);
    RxLabel35.Caption:=_l('#00135^eSimplified:');
    RxLabel35.Show;
    PaintBox4.Show;
    Shape10.Show;
  end else begin
    charval := fMenu.GetCharValue(TChar.Int(TCharIndex),44);
    if charval<>'' then
    begin
      cursimple:=hextofstr(charval);
      RxLabel35.Caption:=_l('#00136^eTraditional:');
      RxLabel35.Show;
      PaintBox4.Show;
      Shape10.Show;
    end else
    begin
      cursimple:='';
      RxLabel35.Hide;
      PaintBox4.Hide;
      Shape10.Hide;
    end;
  end;

  i:=fMenu.GetCharValueRad(TChar.Int(TCharIndex),radf);
  curradno:=inttostr(i);
  Label2.Caption:=curradno;
  if i=255 then curradical:='' else
  begin
    if TRadicals.Locate('Number',inttostr(i),true) then
      curradical:=TRadicals.Str(TRadicalsUnicode) else curradical:='';
  end;
{  if chin then
    RxLabel21.Caption:=_l('#00137^eChar #')+inttostr(DrawGrid1.ColCount*Arow+Acol+1) else
    RxLabel21.Caption:='Kanji #'+inttostr(DrawGrid1.ColCount*Arow+Acol+1);}
  if not chin then
  begin
    if TChar.Int(TCharJouyouGrade)<9 then kig:='C'else
    if TChar.Int(TCharJouyouGrade)<10 then kig:='N'else
    kig:='U';
  end else
    if TChar.Int(TCharChFrequency)<=5 then kig:='C'else kig:='U';
  if IsKnown(KnownLearned,kix) then kig:='K';
  if not IsKnown(strtoint(kanjicatuniqs[ComboBox1.ItemIndex]),kix) then SpeedButton21.Caption:='+'
  else SpeedButton21.Caption:='-';
  if chin then if TChar.Int(TCharStrokeCount)<255 then Label9.Caption:=TChar.Str(TCharStrokeCount) else Label9.Caption:='-';
  if not chin then if TChar.Int(TCharJpStrokeCount)<255 then Label9.Caption:=TChar.Str(TCharJpStrokeCount) else Label9.Caption:='-';
  scat:='';
  for i:=0 to kanjicatuniqs.Count-1 do
  begin
    if IsKnown(strtoint(kanjicatuniqs[i]),kix) then
      if scat='' then scat:=ComboBox1.Items[i] else
        scat:=scat+', '+ComboBox1.Items[i];
  end;
  if scat='' then scat:='-';
  Label3.Caption:=scat;
  case kig[1] of
    'K':RxLabel38.Font.Color:=Col('Kanji_Learned');
    'C':RxLabel38.Font.Color:=Col('Kanji_Common');
    'U':RxLabel38.Font.Color:=Col('Kanji_Rare');
    'N':RxLabel38.Font.Color:=Col('Kanji_Names');
  end;
  case kig[1] of
    'K':RxLabel38.Caption:=_l('#00140^eLearned');
    'C':RxLabel38.Caption:=_l('#00141^eCommon');
    'U':RxLabel38.Caption:=_l('#00142^eRare');
    'N':RxLabel38.Caption:=_l('#00143^eUsed in names');
    'A':RxLabel38.Caption:=_l('#00144^eJapanese and chinese');
    'J':RxLabel38.Caption:=_l('#00145^eJapanese only');
  end;
  piny:='';
  engy:='';
  kory:='';
  ony:='';
  kuny:='';
  chiny:='';
  nany:='';
  cany:='';
  TCharRead.SetOrder('');
  TCharRead.Locate('Kanji',TChar.Str(TCharIndex),true);
  while (not TCharRead.EOF) and (TCharRead.Int(TCharReadKanji)=TChar.Int(TCharIndex)) do
  begin
   //Readings are stored as STRINGS which contain HEX.
   //So there's nothing we can do to avoid HexToUnicode conversion.
    s:=TCharRead.Str(TCharReadReading);
    if (TCharRead.Int(TCharReadType)>3) and (TCharRead.Int(TCharReadType)<7) then
    begin
      ws:='';
      adddot:=0;
      if s[1]='+'then
      begin
        ws:=#$FF0B;
        delete(s,1,1);
        adddot:=1;
      end;
      if s[1]='-'then
      begin
        ws:=ws+#$FF0D;
        delete(s,1,1);
        adddot:=1;
      end;
      if TCharRead.Int(TCharReadReadDot)>0 then
      begin
        ws:=ws+HexToUnicode(copy(s,1,TCharRead.Int(TCharReadReadDot)-1-adddot));
        ws:=ws+#$FF0E;
        delete(s,1,TCharRead.Int(TCharReadReadDot)-1-adddot);
      end;
      if s[length(s)]='-'then
        ws:=ws+HexToUnicode(copy(s,1,length(s)-1))+#$FF0D
      else
        ws:=ws+HexToUnicode(s);
    end;
    case TCharRead.Int(TCharReadType) of
      1:if kory='' then kory:=s else kory:=kory+', '+s;
      2:if piny='' then piny:=s else piny:=piny+','+s;
      4:if ony='' then ony:=ws else ony:=ony+#$FF0C+ws;
      5:if kuny='' then kuny:=ws else kuny:=kuny+#$FF0C+ws;
      6:if nany='' then nany:=ws else nany:=nany+#$FF0C+ws;
      7:if chiny='' then chiny:=s else chiny:=chiny+', '+s;
      3:if engy='' then engy:=s else engy:=engy+', '+s;
      8:if cany='' then cany:=s else cany:=cany+', '+s;
    end;
    TCharRead.Next;
  end;
  if chin then
    Label1.Caption:=chiny else
    Label1.Caption:=engy;
  curon:=fstr(ony);
  curkun:=fstr(kuny);
  curnanori:=fstr(nany);
  curpinyin:=piny;
  kval.Clear;
  for i:=0 to chardetl.Count-1 do
  begin
    for j:=0 to chartypel.Count-1 do if fMenu.GetCharType(j,0)=fMenu.GetCharDet(i,0) then
    begin
      k:=strtoint(fMenu.GetCharType(j,0));
      s:='';
      case k of
        0:s:='---';
        1:s:=LowerCase(kory);
        2:s:=ConvertPinYin(piny);
        3:s:=engy;
        4:s:=fstr(ony);
        5:s:=fstr(kuny);
        6:s:=fstr(nany);
        7:s:=chiny;
        8:s:=LowerCase(cany);
        100:s:=curkanji;
        else begin
          TCharRead.SetOrder('');
          TCharRead.Locate('Kanji',TChar.Str(TCharIndex),true);
          while (not TCharRead.EOF) and (TCharRead.Int(TCharReadKanji)=TChar.Int(TCharIndex)) do
          begin
            if TCharRead.Int(TCharReadType)=k then
            begin
              if (fMenu.GetCharType(j,3)='R') then
              begin
                s2:=TCharRead.Str(TCharReadReading);
                if (length(s2)>0) and (s2[1]='''') then delete(s2,1,1);
                if (length(s2)>0) and (s2[length(s2)]='''') then delete(s2,length(s2),1);
                TRadicals.Locate('Number',s2,true);
                s:=s+TRadicals.Str(TRadicalsUnicode);
              end else
              begin
                if (fMenu.GetCharType(j,3)<>'U') and (s<>'') then s:=s+', ';
                s:=s+TCharRead.Str(TCharReadReading);
              end;
            end;
            TCharRead.Next;
          end;
        end;
      end;
      if fMenu.GetCharDet(i,6)<>'' then
        kval.Add(fMenu.GetCharType(j,3)+';'+chardetl[i]) else
        kval.Add(fMenu.GetCharType(j,3)+';'+chardetl[i]+_l('^e'+fMenu.GetCharType(j,4)));
      kval.Add(s);
    end;
  end;
  h:=InfoPaint(PaintBox3.Canvas,PaintBox3.Width,true);
  PaintBox3.Height:=h;
  sl.Free;
  curcali:=curkanji;
  Screen.Cursor:=crDefault;
end;

function TfKanjiDetails.InfoPaint(canvas:TCanvas;w:integer;onlycount:boolean):integer;
var i:integer;
    x,y,rh:integer;
begin
  x:=6;
  y:=3;
  rh:=0;
  for i:=0 to (kval.Count div 2)-1 do
    fKanji.DrawItem(canvas,kval[i*2],kval[i*2+1],6,w-2,x,y,rh,onlycount);
  result:=y;
end;

initialization
  curon:='';
  curkun:='';
  curnanori:='';
  curindex:=0;

end.
