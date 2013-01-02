unit JWBKanji;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, Buttons, ExtCtrls, JWBStrings,
  RXCtrls, Grids, DB, ShellAPI;

//{$DEFINE UPDATE_WITH_DELAY}
// If set, DoItTimer() will really use timer and not just update instanteneously.

type
  TfKanji = class(TForm)
    Panel1: TPanel;
    RxLabel15: TRxLabel;
    Shape6: TShape;
    Label24: TLabel;
    btnSearchSort: TSpeedButton;
    btnKanjiDetails: TSpeedButton;
    btnCompounds: TSpeedButton;
    btnStrokeOrder: TSpeedButton;
    DrawGrid1: TDrawGrid;
    btnPrintCards: TButton;
    Panel2: TPanel;
    Panel3: TPanel;
    btnReadingChart: TButton;
    SaveDialog1: TSaveDialog;
    UpdateTimer: TTimer;
    procedure FormShow(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure btnPrintCardsClick(Sender: TObject);
    procedure RadioGroup1Click(Sender: TObject);
    procedure DrawGrid1SelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
    procedure DrawGrid1DrawCell(Sender: TObject; ACol, ARow: Integer;
      Rect: TRect; State: TGridDrawState);
    procedure CheckBox2Click(Sender: TObject);
    procedure SpeedButton15Click(Sender: TObject);
    procedure SpeedButton17Click(Sender: TObject);
    procedure DrawGrid1KeyPress(Sender: TObject; var Key: Char);
    procedure UpdateTimerTimer(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
    procedure btnStrokeOrderClick(Sender: TObject);
    procedure SpeedButton25Click(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure btnSearchSortClick(Sender: TObject);
    procedure btnKanjiDetailsClick(Sender: TObject);
    procedure btnCompoundsClick(Sender: TObject);
    procedure btnReadingChartClick(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure DrawGrid1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure DrawGrid1DblClick(Sender: TObject);
    procedure DrawGrid1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
  public
    procedure KanjiSearch_SpeedButton20Click(Sender: TObject);
    procedure KanjiCompounds_CheckBox1Click(Sender: TObject);
    procedure DoIt;
    procedure DoItTimer;
    procedure SaveChars;
    procedure RefreshDetails;
    procedure SetCharCompounds;
    procedure SelRadical;
    function GetKanji(cx,cy:integer):string;

  protected
    procedure ReadFilter(flt:TStringList;const tx:string;typ:integer;partial,space,number,takedot:boolean);
    procedure ReadRaineFilter(fltradical:TStringList;const tx:string);

  end;

var
  fKanji: TfKanji;
  chin:boolean;
  testkanji:string;

var
  curkanji: FChar;

implementation

uses JWBMenu, JWBRadical, JWBSettings, JWBPrint,
  JWBKanjiSearch, JWBKanjiCompounds, JWBKanjiDetails,
  JWBStrokeOrder, MemSource, JWBTranslate, JWBConvert, JWBWords,
  JWBDicSearch, JWBKanjiCard, JWBUnit, JWBUtils, JWBCategories,
  JWBAnnotations, TextTable, JWBDic, JWBEdictMarkers;

var ki:TStringList;
    calfonts:TStringList;
    radtype:string;
    caltype:integer;

{$R *.DFM}

procedure TfKanji.FormShow(Sender: TObject);
begin
  fKanjiSearch.ReloadOtherTypes;
//  fMenu.ShowForm(sbYomi,fMenu.aKanjiSearch,fKanjiSearch);
//  fMenu.ShowForm(SpeedButton2,fMenu.aKanjiDetails,fKanjiDetails);
//  fMenu.ShowForm(SpeedButton3,fMenu.aKanjiCompounds,fKanjiCompounds);
//  fMenu.ShowForm(sbPinYin,nil,fStrokeOrder);
  fMenu.aKanji.Checked:=true;
//  fKanjiSearch.SpeedButton20.Enabled:=ChinesePresent;
  chin:=false;
  if curlang='c'then chin:=true;
//  if fKanjiSearch.SpeedButton20.Down then chin:=true;
  DoIt;
  caltype:=0;
end;

procedure TfKanji.FormHide(Sender: TObject);
begin
  fMenu.aKanji.Checked:=false;
//  fMenu.HideForm(sbYomi,fMenu.aKanjiSearch,fKanjiSearch);
//  fMenu.HideForm(SpeedButton2,fMenu.aKanjiDetails,fKanjiDetails);
//  fMenu.HideForm(SpeedButton3,fMenu.aKanjiCompounds,fKanjiCompounds);
//  fMenu.HideForm(sbPinYin,nil,fStrokeOrder);
end;

//split value string into string list. values are separated by comma or ;
//if numeric, values can have format n1-n2 (edtPinYin.g. 3-5 is expanded into 3,4,5)
procedure MakeList(tx:string;number:boolean;sl:TStringList);
var fnd:boolean;
    curtx,txleft:string;
    min,max:integer;
    i:integer;
begin
  if sl.Count>0 then exit;
  txleft:=tx;
  sl.Sorted:=true;
  fnd:=false;
  while (not fnd) and (txleft<>'') do
  begin
    curtx:='';
    while (length(txleft)>0) and (txleft[1]<>';') and (txleft[1]<>',') do
    begin
      curtx:=curtx+txleft[1];
      delete(txleft,1,1);
    end;
    if (length(txleft)>0) and ((txleft[1]=';') or (txleft[1]=',')) then delete(txleft,1,1);
    if not number then
      sl.Add(uppercase(curtx))
    else
      if pos('-',curtx)=0 then
        sl.Add(curtx)
      else
      begin
        if not TryStrToInt(copy(curtx,1,pos('-',curtx)-1), min) then
          min := 1;
        delete(curtx,1,pos('-',curtx));
        if not TryStrToInt(curtx, max) then
          max := min;
        for i:=min to max do sl.Add(inttostr(i));
      end;
  end;
  if sl.Count=0 then sl.Add('$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$');
end;

procedure TfKanji.ReadFilter(flt:TStringList;const tx:string;typ:integer;partial,space,number,takedot:boolean);
var CCharRead: TTextTableCursor;
  sl:TStringList;
  s_fltval:string;
  s_val:string;
  i:integer;
  dot:integer;
  propId: integer;
  propType: string;
begin
  sl:=TStringList.Create;
  CCharRead := TCharRead.NewCursor;
  try

   //Convert filter value into a list of exact values to match
    MakeList(tx,number,sl);
  
   {$IFDEF UNICODE}
   //Figure out this property's data format and convert values into it
    propId := FindCharPropType(IntToStr(typ));
    if propId<0 then raise Exception.Create('Property not found: '+IntToStr(typ));
    propType := GetCharPropType(propId,3);
    if (propType='P') or (propType='U') then begin
      sl.Sorted := false; //don't really care if it's sorted
      for i := 0 to sl.Count - 1 do
        sl[i] := UnicodeToHex(sl[i]);
    end; //else it's AnsiString
   {$ELSE}
    //On Ansi anything Unicode will already be in FChars
   {$ENDIF}

    for i:=0 to sl.Count-1 do
    begin
      s_fltval:=uppercase(sl[i]); //Locate is case-insensitive anyway
      CCharRead.SetOrder('Reading_Ind');
      CCharRead.Locate('Reading',s_fltval,false);
      s_val:=uppercase(CCharRead.Str(TCharReadReading));
      while (not CCharRead.EOF) and (
        (s_val=s_fltval)
        or ((partial and not space) and (pos(s_fltval,s_val)=1))
        or ((partial and space) and (pos(s_fltval+' ',s_val)=1))) do
      begin
        if takedot then dot:=CCharRead.Int(TCharReadReadDot);
        if CCharRead.Int(TCharReadType)=typ then
          if (not takedot) or (s_val=s_fltval) or ((dot>0) and (s_fltval=copy(s_val,dot-1))) then
            flt.Add(CCharRead.Str(TCharReadKanji));
        CCharRead.Next;
        s_val:=uppercase(CCharRead.Str(TCharReadReading));
      end;
    end;

  finally
    FreeAndNil(CCharRead);
    FreeAndNil(sl);
  end;
  flt.Sort;
  flt.Sorted:=true;
end;

procedure TfKanji.ReadRaineFilter(fltradical:TStringList;const tx:string);
var sltemp:TStringList;
  s1,s2:string;
  rrind:integer;
  rrfrom,rrlen:integer;
  rrus:boolean;
  p:PWideChar;
  i:integer;
begin
  sltemp:=TStringList.Create;
  sltemp.Sorted:=true;
  fltradical.Sorted:=true;
  s1:=tx;
  rrus:=false;
  while s1<>'' do
  begin
    rrind:=StrToInt(strqpop(s1,';'));
    rrlen:=strtoint(copy(raineradicals[rrind-1],9,4));
    rrfrom:=strtoint(copy(raineradicals[rrind-1],14,5));
    p:=PWideChar(integer(rainesearch)+rrfrom*sizeof(WideChar));
    for i:=1 to rrlen do
    begin
      s2:=fstr(p^);
      p:=p+1;
      if not rrus or (sltemp.IndexOf(s2)<>-1) then fltradical.Add(s2);
    end;
    if s1<>'' then
    begin
      sltemp.Assign(fltradical);
      fltradical.Clear;
      rrus:=true;
    end;
  end;
  sltemp.Free;
end;

//---------------------------------------
//filter kanji and show them in the grid
//---------------------------------------
procedure TfKanji.DoIt;

function InRange(tx,fld:string;number:boolean;sl:TStringList):boolean;
begin
  MakeList(tx,number,sl);
  result:=sl.IndexOf(uppercase(fld))>-1;
end;

var fltclip,fltpinyin,fltyomi,fltmean:TStringList;
    accept:boolean;
    radf:integer;
    i,j,k,grs:integer;
    s1,s2,s3:string;
    sbJouyou:string;
    x:integer;
    mr:TGridRect;
    b:boolean;
    sl4,sl10:TStringList;
    fltradical,fltskip,fltother:TStringList;
    sltemp:TStringList;
    p:pchar;
    w:word;
    onecheck:boolean;
    clipsort:boolean;
    clipind:integer;
begin
  if not Visible then exit;
  DrawGrid1.Hide;
  Screen.Cursor:=crHourGlass;
  fMenu.aKanjiLearned.Checked:=fKanjiSearch.SpeedButton1.Down;
  fMenu.aKanjiCommon.Checked:=fKanjiSearch.SpeedButton2.Down;
  fMenu.aKanjiClipboard.Checked:=fKanjiSearch.SpeedButton3.Down;
  fltclip:=TStringList.Create;
  fltpinyin:=TStringList.Create;
  fltyomi:=TStringList.Create;
  fltmean:=TStringList.Create;
  fltradical:=TStringList.Create;
  fltskip:=TStringList.Create;
  fltother:=TStringList.Create;
  sl4:=TStringList.Create;
  sl10:=TStringList.Create;
  if fKanjiSearch.SpeedButton3.Down then
    for i:=0 to length(clip) div 4-1 do
      if clip[i*4+1]>='4'then if fltclip.IndexOf(uppercase(copy(clip,i*4+1,4)))=-1 then
        fltclip.Add(uppercase(copy(clip,i*4+1,4)));
  if fKanjiSearch.sbPinYin.Down then begin
    ReadFilter(fltpinyin,fKanjiSearch.edtPinYin.text,2,true,false,false,false); //Mandarin
    ReadFilter(fltpinyin,fKanjiSearch.edtPinYin.text,8,true,false,false,false); //Canton
  end;
  if fKanjiSearch.sbYomi.Down then begin
   //ON and KUN
    ReadFilter(fltyomi,RomajiToKana('H'+fKanjiSearch.edtYomi.Text,romasys,true,'j'),4,fSettings.CheckBox57.Checked,false,false,fSettings.CheckBox57.Checked);
    ReadFilter(fltyomi,RomajiToKana('H'+fKanjiSearch.edtYomi.Text,romasys,true,'j'),5,fSettings.CheckBox57.Checked,false,false,fSettings.CheckBox57.Checked);
    ReadFilter(fltyomi,RomajiToKana('Q'+fKanjiSearch.edtYomi.Text,romasys,true,'j'),4,fSettings.CheckBox57.Checked,false,false,fSettings.CheckBox57.Checked);
    ReadFilter(fltyomi,RomajiToKana('Q'+fKanjiSearch.edtYomi.Text,romasys,true,'j'),5,fSettings.CheckBox57.Checked,false,false,fSettings.CheckBox57.Checked);
  end;
  if fKanjiSearch.sbSKIP.Down then
    ReadFilter(fltskip,fKanjiSearch.edtSKIP.Text,22,true,false,false,false); //SKIP
  if fKanjiSearch.sbRadicals.Down then
    case fKanjiSearch.curRadSearchType of
      stClassic: ReadFilter(fltradical,fKanjiSearch.edtRadicals.Text,10,false,false,true,false); //Radicals
      stRaine: ReadRaineFilter(fltradical,fKanjiSearch.edtRadicals.Text);
    end;
  if fKanjiSearch.sbOther.Down then
  begin
    if fKanjiSearch.cbOtherType.ItemIndex=0 then
      fltother.Add(fKanjiSearch.edtOther.Text)
    else
      j:=0;
    for i:=0 to CharPropTypes.Count-1 do
      if strtoint(GetCharPropType(i,0))>20 then
      begin
        inc(j);
        if j=fKanjiSearch.cbOtherType.ItemIndex then
          ReadFilter(fltother,fKanjiSearch.edtOther.Text,strtoint(GetCharPropType(i,0)),false,false,GetCharPropType(i,3)='N',false);
      end;
  end;
  if fKanjiSearch.sbDefinition.Down then if chin then
    ReadFilter(fltmean,fKanjiSearch.edtDefinition.text,7,true,true,false,false) //Chinese definition
  else
    ReadFilter(fltmean,fKanjiSearch.edtDefinition.text,3,true,true,false,false); //Japanese definition
  case fSettings.RadioGroup3.ItemIndex of
    0: grs:=30;
    1: grs:=45;
    2: grs:=60;
  end;
  if not chin then
    case fKanjiSearch.rgSortBy.ItemIndex of
      0,4:TChar.SetOrder('JpUnicode_Ind');
      1:TChar.SetOrder('JpStrokeCount_Ind');
      2:TChar.SetOrder('JpFrequency_Ind');
      3:TChar.SetOrder('JpUnicode_Ind');
    end;
  if chin then
    case fKanjiSearch.rgSortBy.ItemIndex of
      0,4:TChar.SetOrder('ChUnicode_Ind');
      1:TChar.SetOrder('ChStrokeCount_Ind');
      2:TChar.SetOrder('ChFrequency_Ind');
      3:TChar.SetOrder('ChUnicode_Ind');
    end;
  ki.Clear;
  radf:=fSettings.ComboBox1.ItemIndex+12;
  clipsort:=(fKanjiSearch.SpeedButton3.Down) and (fKanjiSearch.rgSortBy.ItemIndex=4);
  clipind:=0;
//  if not clipsort then fltclip.Sort;
  while ((not clipsort) and ((not TChar.EOF) and ((chin) or (TChar.Int(TCharChinese)=0)))) or
        ((clipsort) and (clipind<fltclip.Count)) do
  begin
    accept:=true;
    if clipsort then accept:=TChar.Locate('Unicode',fltclip[clipind],false);
    if accept and chin and (fSettings.RadioGroup5.ItemIndex=0) and (TChar.Str(TCharType)='S') then accept:=false;
    if accept and chin and (fSettings.RadioGroup5.ItemIndex=1) and (TChar.Str(TCharType)='T') then accept:=false;
    if accept and (fKanjiSearch.SpeedButton2.Down) and chin and (TChar.Int(TCharChFrequency)>=255) then accept:=false;
    if accept and (fKanjiSearch.SpeedButton2.Down) and not chin and (TChar.Int(TCharJouyouGrade)>=10) then accept:=false;
    if accept and (not clipsort) and (fKanjiSearch.SpeedButton3.Down) and (fltclip.IndexOf(uppercase(TChar.Str(TCharUnicode)))=-1) then accept:=false;
    if accept and (fKanjiSearch.sbPinYin.Down) and (fltpinyin.IndexOf(TChar.Str(TCharIndex))=-1) then accept:=false;
    if accept and (fKanjiSearch.sbYomi.Down) and (fltyomi.IndexOf(TChar.Str(TCharIndex))=-1) then accept:=false;
    if accept and (fKanjiSearch.sbDefinition.Down) and (fltmean.IndexOf(TChar.Str(TCharIndex))=-1) then accept:=false;
    if accept and (fKanjiSearch.sbSKIP.Down) and (fltskip.IndexOf(TChar.Str(TCharIndex))=-1) then accept:=false;
    if accept and (fKanjiSearch.sbRadicals.Down) then
      case fKanjiSearch.curRadSearchType of
        stClassic: if fltradical.IndexOf(TChar.Str(TCharIndex))=-1 then accept:=false;
        stRaine: if fltradical.IndexOf(TChar.Str(TCharUnicode))=-1 then accept:=false;
      end;
    if accept and (fKanjiSearch.sbOther.Down) and (fKanjiSearch.cbOtherType.ItemIndex=0) and (fltother.IndexOf(TChar.Str(TCharUnicode))=-1) then accept:=false;
    if accept and (fKanjiSearch.sbOther.Down) and (fKanjiSearch.cbOtherType.ItemIndex>0) and (fltOther.IndexOf(TChar.Str(TCharIndex))=-1) then accept:=false;
//    if accept and (fKanjiSearch.sbOther.Down) and (fKanjiSearch.SpeedButton25.Down) and not InRange(fKanjiSearch.edtOther.text,TChar.Str(TCharUnicode),false,sl1) then accept:=false;
//    if accept and (fKanjiSearch.sbOther.Down) and (fKanjiSearch.SpeedButton26.Down) and not InRange(fKanjiSearch.edtOther.text,TChar.Str(TCharUnicode),true,sl2) then accept:=false;
//    if accept and (fKanjiSearch.sbOther.Down) and (fKanjiSearch.SpeedButton27.Down) and not InRange(fKanjiSearch.edtOther.text,TChar.Str(TCharUnicode),true,sl3) then accept:=false;
    if chin and accept and (fKanjiSearch.sbStrokeCount.Down) and not InRange(fKanjiSearch.edtStrokeCount.Text,TChar.Str(TCharStrokeCount),true,sl4) then accept:=false;
    if (not chin) and accept and (fKanjiSearch.sbStrokeCount.Down) and not InRange(fKanjiSearch.edtStrokeCount.Text,TChar.Str(TCharJpStrokeCount),true,sl4) then accept:=false;
    if accept and (fKanjiSearch.sbSKIP.Down) then
    begin
      s1:=fKanjiSearch.edtSKIP.Text;
      s2:='0';
      s3:='0';
      if pos('-',s1)>0 then
      begin
        s2:=s1;
        s1:=copy(s2,1,pos('-',s2)-1);
        delete(s2,1,pos('-',s2));
      end;
      if pos('-',s2)>0 then
      begin
        s3:=s2;
        s2:=copy(s3,1,pos('-',s3)-1);
        delete(s3,1,pos('-',s3));
      end;
//      if accept then accept:=InRange(s1,TChar.Str(TCharSKIP1),false,sl6);
//      if accept then accept:=InRange(s2,TChar.Str(TCharSKIP2),false,sl7);
//      if accept then accept:=InRange(s3,TChar.Str(TCharSKIP3),false,sl8);
    end;
    if accept then
    begin
      if fKanjiSearch.rgOrAnd.ItemIndex=0 then accept:=false;
      onecheck:=false;
      for k:=0 to fKanjiSearch.lbCategories.Items.Count-1 do
        if fKanjiSearch.lbCategories.Checked[k] then
      begin
        onecheck:=true;
        if IsKnown(GetSelCatIdx(fKanjiSearch.lbCategories), TChar.FCh(TCharUnicode)) then
        begin
          if fKanjiSearch.rgOrAnd.ItemIndex=0 then accept:=true;
        end else
        begin
          if fKanjiSearch.rgOrAnd.ItemIndex=1 then accept:=false;
        end;
      end;
      if not onecheck then accept:=true;
      if fKanjiSearch.cbNot.Checked and onecheck then accept:=not accept;
    end;
        
{    if accept and (fKanjiSearch.SpeedButton16.Down) then
    begin
      s1:=fKanjiSearch.Edit5.Text;
      if pos('.',s1)>0 then delete(s1,pos('.',s1),1);
      if accept then accept:=InRange(s1,TChar.Str(TCharFourCornerCode),false,sl9);
    end;
}    if accept and (fKanjiSearch.sbJouyou.Down) and not InRange(fKanjiSearch.edtJouyou.Text,TChar.Str(TCharJouyouGrade),true,sl10) then accept:=false;
    if accept then
    begin
      if not chin then
      begin
        if TChar.Int(TCharJouyouGrade)<9 then sbJouyou:='C'else
        if TChar.Int(TCharJouyouGrade)<10 then sbJouyou:='N'else
        sbJouyou:='U';
      end else
//        if TChar.Str(TCharType)[1]='A'then sbJouyou:='A'else
//        if TChar.Str(TCharType)[1]='J'then sbJouyou:='J'else
        if TChar.Int(TCharChFrequency)<=5 then sbJouyou:='C'else sbJouyou:='U';
      if ((not chin) and (fKanjiSearch.rgSortBy.ItemIndex=3)) or
         ((chin) and (fKanjiSearch.rgSortBy.ItemIndex=3)) then
        ki.Insert(random(ki.Count),sbJouyou+TChar.Str(TCharUnicode)) else
          ki.Add(sbJouyou+TChar.Str(TCharUnicode));
    end;
    if clipsort then inc(clipind) else TChar.Next;
  end;
  fltclip.Free;
  fltpinyin.Free;
  fltyomi.Free;
  fltmean.Free;
  fltradical.Free;
  fltskip.Free;
  fltother.Free;
  sl4.Free;
  sl10.Free;

 //First try new translation strings  
  if chin then begin
    case fSettings.RadioGroup5.ItemIndex of
      0: sbJouyou:=_l('#00958^eFound traditional characters (%d):', [ki.Count]);
      1: sbJouyou:=_l('#00959^eFound simplified characters (%d):', [ki.Count]);
    else sbJouyou:=_l('#00960^eFound characters (%d):', [ki.Count]);
    end;
  end else
    sbJouyou:=_l('#00961^eFound kanji (%d):',[ki.Count]);

  RxLabel15.Caption:=sbJouyou;
  DrawGrid1.ColCount:=(DrawGrid1.Width-32) div grs;
  x:=DrawGrid1.ColCount;
  if ki.Count=0 then DrawGrid1.RowCount:=1 else
    DrawGrid1.RowCount:=((ki.Count-1) div x)+1;
  DrawGrid1.DefaultRowHeight:=grs;
  DrawGrid1.DefaultColWidth:=grs;
  testkanji:='';
  for i:=0 to 14 do if i<ki.Count then testkanji:=testkanji+copy(ki[i],2,4);
  mr.Left:=0;
  mr.Right:=0;
  mr.Bottom:=0;
  mr.Top:=0;
  for i:=0 to ki.Count-1 do if curkanji=copy(ki[i],2,4) then
  begin
    mr.Left:=i mod x;
    mr.Top:=i div x;
    mr.Right:=i mod x;
    mr.Bottom:=i div x;
  end;
  TChar.SetOrder('ChUnicode_Ind');
  if ki.Count>0 then DrawGrid1.Show;
  DrawGrid1.Selection:=mr;
  if (mr.Top>1) and (DrawGrid1.RowCount>DrawGrid1.VisibleRowCount) then DrawGrid1.TopRow:=mr.Top-1 else
  DrawGrid1.TopRow:=0;
  curkanji:=UH_NOCHAR;
  DrawGrid1SelectCell(self,mr.Left,mr.Top,b);
  Screen.Cursor:=crDefault;
  DrawGrid1.Invalidate;
end;

function GetPageNum(canvas:TCanvas; width,height:integer; userdata:pointer):integer;
var lh,lc:integer;
    ncv,nch:integer;
    ch:double;
    numh:integer;
    chi:integer;
begin
  GetPrintLine(width,height,width,height,strtoint(fSettings.Edit11.Text),lh,lc);
  ncv:=strtoint(fSettings.edit13.text);
  if fSettings.CheckBox22.Checked then inc(ncv,3);
  if fSettings.CheckBox44.Checked then inc(ncv,2);
  if fSettings.CheckBox62.Checked then inc(ncv,1+strtoint(fSettings.Edit35.Text));
  ch:=lh/ncv;
  chi:=trunc(ch);
  nch:=strtoint(fSettings.edit13.text);
  if (fSettings.CheckBox19.Checked) or (fSettings.CheckBox20.Checked) then inc(nch,(strtoint(fSettings.Edit13.Text) div 2)+1);
  if fSettings.CheckBox18.Checked then nch:=nch+1+strtoint(fSettings.edit12.text);
  numh:=trunc(width-ch*2) div trunc(nch*ch);
  lc:=trunc(height-ch*2) div trunc(ncv*ch);
  result:=((ki.Count-1) div (numh*lc))+1;
end;

procedure DrawPage(canvas:TCanvas; pagenum:integer; width,height,origwidth,origheight:integer; userdata:pointer);
var lh,lc:integer;
    ncv,nch:integer;
    ch:double;
    numh,i:integer;
    u:string;
    x,xp,y,yp:integer;
    chi:integer;
    rel:integer;
begin
  GetPrintLine(origwidth,origheight,origwidth,origheight,strtoint(fSettings.Edit11.Text),lh,lc);
//  lh:=round(0.98*lh);
  ncv:=strtoint(fSettings.edit13.text);
  if fSettings.CheckBox22.Checked then inc(ncv,3);
  if fSettings.CheckBox44.Checked then inc(ncv,2);
  if fSettings.CheckBox62.Checked then inc(ncv,1+strtoint(fSettings.Edit35.Text));
  ch:=lh/ncv;
  chi:=trunc(ch);
  nch:=strtoint(fSettings.edit13.text);
  if (fSettings.CheckBox19.Checked) or (fSettings.CheckBox20.Checked) then inc(nch,(strtoint(fSettings.Edit13.Text) div 2)+1);
  if fSettings.CheckBox18.Checked then nch:=nch+1+strtoint(fSettings.edit12.text);
  numh:=trunc(origwidth-ch*2) div trunc(nch*ch);
  lc:=trunc(origheight-ch*2) div trunc(ncv*ch);
  ch:=ch/origwidth*width;
  for i:=0 to numh*lc-1 do
  begin
    if ((pagenum-1)*numh*lc+i)<ki.Count then
    begin
      u:=ki[((pagenum-1)*numh*lc)+i];
      delete(u,1,1);
      if fSettings.CheckBox24.Checked then
      begin
        yp:=i mod lc;
        xp:=numh-(i div lc)-1;
      end else
      begin
        xp:=i mod numh;
        yp:=i div numh;
      end;
      y:=trunc((height-lc*ncv*ch)/2+(yp*ncv*ch));
      if (numh>1) and (fSettings.CheckBox25.Checked) then
      x:=trunc(ch+((width-(numh*nch*ch+2*ch))/(numh-1))*xp+xp*nch*ch) else x:=trunc(ch+xp*nch*ch);
      try
        DrawKanjiCard(canvas,u,x,y,ch,fSettings.CheckBox45.Checked,fSettings.CheckBox21.Checked,
          fSettings.CheckBox19.Checked,fSettings.CheckBox20.Checked,fSettings.CheckBox23.Checked,
          fSettings.CheckBox18.Checked,fSettings.CheckBox22.Checked,fSettings.CheckBox44.Checked,fSettings.CheckBox52.Checked,
          fSettings.CheckBox62.Checked,fSettings.CheckBox63.Checked,
          strtoint(fSettings.Edit12.Text),strtoint(fSettings.Edit13.Text),strtoint(fSettings.Edit35.Text),fSettings.Edit14.Text);
      except end;
    end;
  end;
end;

procedure PrintConfigure(userdata:pointer);
begin
  fSettings.pcPages.ActivePage:=fSettings.tsCharacterCardPrinting;
  fSettings.ShowModal;
end;

procedure TfKanji.btnPrintCardsClick(Sender: TObject);
begin
  ClearKanjiCardCache;
  fPrint.Preview(GetPageNum,DrawPage,PrintConfigure,nil,_l('#00134^eKanji cards'));
end;

procedure TfKanji.RadioGroup1Click(Sender: TObject);
begin
  DoIt;
end;

//It's not an event handler, actually. fMenu calls this on language reload.
procedure TfKanji.KanjiSearch_SpeedButton20Click(Sender: TObject);
begin
  chin:=curlang='c';
  fKanjiSearch.rgSortBy.Items.Clear;
  if chin then
  begin
    fKanjiSearch.rgSortBy.Items.Add(_l('#00146^eRadical'));
    fKanjiSearch.rgSortBy.Items.Add(_l('#00147^eStroke count'));
    fKanjiSearch.rgSortBy.Items.Add(_l('#00148^eFrequency'));
    fKanjiSearch.rgSortBy.Items.Add(_l('#00149^eRandom'));
    fKanjiSearch.rgSortBy.Items.Add(_l('#00877^eUnsorted'));
  end else
  begin
    fKanjiSearch.rgSortBy.Items.Add(_l('#00146^eRadical'));
    fKanjiSearch.rgSortBy.Items.Add(_l('#00147^eStroke count'));
    fKanjiSearch.rgSortBy.Items.Add(_l('#00148^eFrequency'));
    fKanjiSearch.rgSortBy.Items.Add(_l('#00149^eRandom'));
    fKanjiSearch.rgSortBy.Items.Add(_l('#00877^eUnsorted'));
  end;
  fKanjiSearch.rgSortBy.ItemIndex:=0;
  DoIt;
end;

procedure TfKanji.KanjiCompounds_CheckBox1Click(Sender: TObject);
var sel:TGridRect;
    b:boolean;
begin
  sel:=DrawGrid1.Selection;
  DrawGrid1SelectCell(sender,sel.left,sel.top,b);
end;

procedure TfKanji.SetCharCompounds;
var sl,sl2:TStringList;
    pass:boolean;
    i,j,k,l:integer;
    dic:TJaletDic;
    freq,mark:string;
    stp:string;
    kj:string;
begin
  if not fKanjiCompounds.Visible then exit;
  fKanjiCompounds.StringGrid1.Visible:=false;
  fKanjiCompounds.CheckBox3.Enabled:=(not fKanjiCompounds.SpeedButton8.Down) and (curlang='j');
  kj:=ChinFrom(TChar.Str(TCharUnicode));
  sl:=TStringList.Create;
  sl2:=TStringList.Create;
  if fKanjiCompounds.SpeedButton9.Down then
  begin
    if (curlang='j') and (fKanjiCompounds.CheckBox3.Checked) then
    begin
      for i:=0 to dicts.Count-1 do
        if dicts[i].loaded and dicts.IsInGroup(dicts[i], 4)
          and (dicts[i].TDictFrequency<>-1) then
      begin
        dic:=dicts[i];
        dic.Demand;
        dic.FindIndexString(false,kj);
        k:=0;
        j:=dic.ReadIndex;
        while (j>0) do
        begin
          dic.TDict.Locate('Index',inttostr(j),true);
          inc(k);
          if pos(kj,dic.TDict.Str(dic.TDictKanji))=0 then
            showmessage('Dictionary has corrupted index: '+TChar.Str(TCharUnicode)+'-'+inttostr(k)+'-'+Format('%4.4X',[j])+'-'+dic.TDict.Str(dic.TDictEnglish));
          if (not fKanjiCompounds.CheckBox1.Checked) or (pos(kj,dic.TDict.Str(dic.TDictKanji))=1) then
          begin
            if dic.TDictMarkers<>-1 then mark:=dic.TDict.Str(dic.TDictMarkers) else mark:='';
            freq:=inttostr(9999999-dic.TDict.Int(dic.TDictFrequency));
            while length(freq)<7 do freq:='0'+freq;
            if freq<>'9999999'then
            if ((not fKanjiCompounds.CheckBox2.Checked) or (pos('<spop>',EnrichDictEntry(dic.TDict.Str(dic.TDictEnglish),mark))>0)) then
              sl.Add(freq+#9+CheckKnownKanji(ChinTo(dic.TDict.Str(dic.TDictKanji)))+' ['+dic.TDict.Str(dic.TDictPhonetic)+'] {'+EnrichDictEntry(dic.TDict.Str(dic.TDictEnglish),mark)+'}{');
          end;
          j:=dic.ReadIndex;
        end;
      end;
{        sl.Sort;
      for i:=0 to sl.Count-1 do
      begin
        kj:=sl[i];
        delete(kj,1,7);
        sl[i]:=kj;
      end;}
    end else
    begin
      for i:=0 to dicts.Count-1 do
        if dicts[i].loaded and dicts.IsInGroup(dicts[i], 4) then
      begin
        dic:=dicts[i];
        dic.Demand;
        dic.FindIndexString(false,kj);
        k:=0;
        j:=dic.ReadIndex;
        while (j>0) do
        begin
          dic.TDict.Locate('Index',inttostr(j),true);
          inc(k);
          if pos(kj,dic.TDict.Str(dic.TDictKanji))=0 then
            showmessage('Dictionary has corrupted index: '+TChar.Str(TCharUnicode)+'-'+inttostr(k)+'-'+Format('%4.4X',[j])+'-'+dic.TDict.Str(dic.TDictEnglish));
          if (not fKanjiCompounds.CheckBox1.Checked) or (pos(kj,dic.TDict.Str(dic.TDictKanji))=1) then
          begin
            if dic.TDictMarkers<>-1 then mark:=dic.TDict.Str(dic.TDictMarkers) else mark:='';
            if ((not fKanjiCompounds.CheckBox2.Checked) or (pos('<spop>',EnrichDictEntry(dic.TDict.Str(dic.TDictEnglish),mark))>0)) then
              sl.Add(dic.TDict.Str(dic.TDictKanji)+#9+CheckKnownKanji(ChinTo(dic.TDict.Str(dic.TDictKanji)))+' ['+dic.TDict.Str(dic.TDictPhonetic)+'] {'+EnrichDictEntry(dic.TDict.Str(dic.TDictEnglish),mark)+'}{');
          end;
          j:=dic.ReadIndex;
        end;
      end;
    end;
    sl.Sort;
  end else if (fKanjiCompounds.SpeedButton8.Down) then
  begin
    TUserIdx.SetOrder('Kanji_Ind');
    TUserIdx.Locate('Kanji',kj,false);
    while (not TUserIdx.EOF) and (TUserIdx.Str(TUserIdxKanji)=kj) do
    begin
      if (not fKanjiCompounds.CheckBox1.Checked) or (TUserIdx.Bool(TUserIdxBegin)) then
      begin
        sl2.Clear;
        ListWordCategories(TUserIdx.Int(TUserIdxWord),sl2);
        pass:=false;
        for l:=0 to sl2.Count-1 do if (pos(curlang+'~',sl2[l])=1) or (length(sl2[l])<2) or (copy(sl2[l],2,1)<>'~') then pass:=true;
        if (pass) and (TUser.Locate('Index',TUserIdx.Str(TUserIdxWord),true)) then
        begin
          stp:=TUser.Str(TUserScore);
          sl.Add(TUser.Str(TUserKanji)+#9+'!'+stp+CheckKnownKanji(ChinTo(TUser.Str(TUserKanji)))+' ['+'!'+stp+TUser.Str(TUserPhonetic)+'] {'+'!'+stp+TUser.Str(TUserEnglish)+'}');
        end;
      end;
      TUserIdx.Next;
    end;
  end;
  sl.Sort;
  if (fKanjiCompounds.SpeedButton9.Down) and (fKanjiCompounds.CheckBox3.Checked) and (strtoint(fSettings.Edit34.Text)<>0) then
    while sl.Count>strtoint(fSettings.Edit34.Text) do sl.Delete(strtoint(fSettings.Edit34.Text));
  for i:=0 to sl.Count-1 do
    sl[i]:=copy(sl[i],pos(#9,sl[i])+1,length(sl[i])-pos(#9,sl[i]));
  FillWordGrid(fKanjiCompounds.StringGrid1,sl,false,false);
  sl.Free;
  sl2.Free;
end;

procedure TfKanji.DrawGrid1SelectCell(Sender: TObject; ACol, ARow: Integer;
  var CanSelect: Boolean);
var kix:string;
begin
  if DrawGrid1.ColCount*ARow+Acol>=ki.Count then
  begin
    CanSelect:=false;
    exit;
  end;
  CanSelect:=true;
  kix:=ki[DrawGrid1.ColCount*ARow+Acol];
  delete(kix,1,1);
  fKanjiDetails.SetCharDetails(kix);
  AnnotShowMedia(kix,'');
  if fKanjiCompounds.Visible then SetCharCompounds;
end;

procedure TfKanji.DrawGrid1DrawCell(Sender: TObject; ACol, ARow: Integer;
  Rect: TRect; State: TGridDrawState);
var w:widechar;
  kix:FString;
  kig:string;
  sbJouyou:string;
begin
  if (ARow*DrawGrid1.ColCOunt+ACol>=ki.Count) then
  begin
    DrawGrid1.Canvas.Pen.Color:=clWindow;
    DrawGrid1.Canvas.Brush.Color:=clWindow;
    DrawGrid1.Canvas.FillRect(Rect);
    exit;
  end;
  kix:=ki[DrawGrid1.ColCount*ARow+Acol];
  delete(kix,1,1);
 {$IFDEF UNICODE}
  w := kix[1];
 {$ELSE}
  w:=HexToUnicode(kix)[1];
 {$ENDIF}
  if gdSelected in State then DrawGrid1.Canvas.Brush.Color:=clHighlight else
  DrawGrid1.Canvas.Brush.Color:=clWindow;
  if gdSelected in state then DrawGrid1.Canvas.Font.Color:=clHighlightText else
  DrawGrid1.Canvas.Font.Color:=clWindowText;
  if (not fSettings.CheckBox3.Checked) and not (gdSelected in State) then
  begin
    DrawGrid1.Canvas.Brush.Color:=Col('Kanji_Back');
    TChar.Locate('Unicode',kix,false);
    kig:=ki[DrawGrid1.ColCount*ARow+Acol];
    if IsKnown(KnownLearned,TChar.Str(TCharUnicode)) then kig:='K';
    case kig[1] of
      'K':DrawGrid1.Canvas.Font.Color:=Col('Kanji_Learned');
      'C':DrawGrid1.Canvas.Font.Color:=Col('Kanji_Common');
      'U':DrawGrid1.Canvas.Font.Color:=Col('Kanji_Rare');
      'N':DrawGrid1.Canvas.Font.Color:=Col('Kanji_Names');
    end;
  end;
  if fSettings.CheckBox69.Checked and HaveAnnotations then
  begin
    Annot.SeekK(TChar.Str(TCharUnicode),'');
    sbJouyou:=Annot.GetOne('c');
    if sbJouyou<>'' then try
      DrawGrid1.Canvas.Font.Color:=strtoint('0x'+copy(sbJouyou,5,2)+copy(sbJouyou,3,2)+copy(sbJouyou,1,2));
    except end;
  end;
  DrawGrid1.Canvas.FillRect(Rect);
  if not chin then DrawGrid1.Canvas.Font.Name:=FontJapaneseGrid else
  case fSettings.RadioGroup5.ItemIndex of
    0:DrawGrid1.Canvas.Font.Name:=FontChineseGrid;
    1:DrawGrid1.Canvas.Font.Name:=FontChineseGridGB;
    2:DrawGrid1.Canvas.Font.Name:=FontRadical;
  end;
  case fSettings.RadioGroup3.ItemIndex of
    0:DrawGrid1.Canvas.Font.Height:=22;
    1:DrawGrid1.Canvas.Font.Height:=37;
    2:DrawGrid1.Canvas.Font.Height:=52;
  end;
  DrawGrid1.Canvas.Font.Style:=[];
  TextOutW(DrawGrid1.Canvas.Handle,Rect.Left+5,Rect.Top+4,@w,1);
  if fSettings.CheckBox1.Checked then
  begin
    TChar.Locate('Unicode',kix,false);
    DrawGrid1.Canvas.Font.Name:=FontEnglish;
    DrawGrid1.Canvas.Font.Height:=8+4*fSettings.RadioGroup3.ItemIndex;
    DrawGrid1.Canvas.Font.Color:=clWindowText;
    if chin then DrawGrid1.Canvas.TextOut(Rect.Left+1,Rect.Top+1,TChar.Str(TCharStrokeCount));
    if not chin then DrawGrid1.Canvas.TextOut(Rect.Left+1,Rect.Top+1,TChar.Str(TCharJpStrokeCount));
  end;
  fKanjiDetails.PaintBox1.Invalidate;
  fKanjiDetails.PaintBox2.Invalidate;
  fKanjiDetails.PaintBox4.Invalidate;
end;

procedure TfKanji.CheckBox2Click(Sender: TObject);
begin
  DrawGrid1.Invalidate;
end;

procedure TfKanji.SpeedButton15Click(Sender: TObject);
begin
  showmessage(_l('#00150^eFeature not implemented yet.'));
end;

procedure TfKanji.SpeedButton17Click(Sender: TObject);
begin
  showmessage(_l('#00150^eFeature not implemented yet.'));
end;

procedure TfKanji.DrawGrid1KeyPress(Sender: TObject; var Key: Char);
begin
  if key=' 'then
  begin
    clip:=clip+curkanji;
    fMenu.ChangeClipboard;
  end;
  if key=#13 then
  begin
    if not fMenu.aKanjiDetails.Checked then
      fMenu.aKanjiDetails.Execute
    else
      if fKanjiDetails.Visible then
        fKanjiDetails.SetFocus;
  end;
  if key=#8 then
  begin
    if length(clip)>0 then delete(clip,length(clip)-3,4);
    fMenu.ChangeClipboard;
  end;
end;

procedure TfKanji.DoItTimer;
begin
{$IFDEF UPDATE_WITH_DELAY}
  UpdateTimer.Interval:=1000;
  UpdateTimer.Enabled:=true;
{$ELSE}
  DoIt;
{$ENDIF}
end;

procedure TfKanji.UpdateTimerTimer(Sender: TObject);
begin
  UpdateTimer.Enabled:=false;
  DoIt;
end;

procedure TfKanji.BitBtn1Click(Sender: TObject);
begin
  DrawGrid1.SetFocus;
end;

procedure TfKanji.btnStrokeOrderClick(Sender: TObject);
begin
  fMenu.ToggleForm(fStrokeOrder,btnStrokeOrder,nil);
end;

procedure TfKanji.SpeedButton25Click(Sender: TObject);
begin
  DoIt;
end;

procedure TfKanji.FormResize(Sender: TObject);
var grs:integer;
begin
  case fSettings.RadioGroup3.ItemIndex of
    0: grs:=30;
    1: grs:=45;
    2: grs:=60;
  end;
  if DrawGrid1.ColCount<>(DrawGrid1.ClientWidth-32) div grs then DoIt;
end;

procedure TfKanji.btnSearchSortClick(Sender: TObject);
begin
  fMenu.ToggleForm(fKanjiSearch,btnSearchSort,fMenu.aKanjiSearch);
end;

procedure TfKanji.btnKanjiDetailsClick(Sender: TObject);
begin
  fMenu.aKanjiDetails.Execute;
end;

procedure TfKanji.btnCompoundsClick(Sender: TObject);
var CanSelect:boolean;
begin
  fMenu.ToggleForm(fKanjiCompounds,btnCompounds,fMenu.aKanjiCompounds);
  DrawGrid1SelectCell(Sender, DrawGrid1.Col, DrawGrid1.Row, CanSelect);
end;

procedure TfKanji.btnReadingChartClick(Sender: TObject);
var di:integer;
    chars:array[1..7] of string;
    i,j,k,l,m:integer;
    sbJouyou,s2:string;
    wd:integer;
    o:string;
    add:TStringList;
procedure wrid;
var sbJouyou:TCharacterLineProps;
    x:integer;
begin
  fTranslate.doc.Add(o);
  sbJouyou.AddChars(flength(o));
  for x:=1 to flength(o) do
    sbJouyou.chars[x-1].SetChar('-', 9, 0, 1);
  fTranslate.doctr.AddLine(sbJouyou);
  o:='';
end;
begin
  fTranslate.doc.Clear;
  fTranslate.doctr.Clear;
  wd:=6;
  o:='';
  add:=TStringList.Create;
  o:=UH_SPACE+UH_SPACE+UH_SPACE;
  for i:=0 to readchl.Count-1 do
  begin
    sbJouyou:=readchl[i];
    if sbJouyou[1]='e'then
    begin
      delete(sbJouyou,1,1);
      if sbJouyou='' then sbJouyou:=UH_SPACE;
      o:=o+sbJouyou;
      for j:=1 to wd do o:=o+UH_SPACE;
    end;
  end;
  wrid;
  wrid;
  for i:=0 to readchl.Count-1 do
  begin
    sbJouyou:=readchl[i];
    if sbJouyou[1]='b'then
    begin
      delete(sbJouyou,1,1);
      o:=o+sbJouyou+UH_SPACE;
      if length(sbJouyou)<=4 then o:=o+UH_SPACE;
      k:=0;
      for j:=0 to readchl.Count-1 do
      begin
        s2:=readchl[j];
        if s2[1]='e'then
        begin
          delete(s2,1,1);
          inc(k);
          chars[k]:='';
          TCharRead.First;
          while not TCharRead.EOF do
          begin
            if (TCharRead.Int(TCharReadType)=4) and (TCharRead.Str(TCharReadReading)=sbJouyou+s2) and (TCharRead.Int(TCharReadPosition)=1) then
            begin
              TChar.Locate('Index',TCharRead.Str(TCharReadKanji),true);
              if IsKnown(KnownLearned,TChar.Str(TCharUnicode)) then begin
                chars[k]:=chars[k]+TChar.Str(TCharUnicode);
                add.Add(TCharRead.Str(TCharReadKanji));
              end;
            end;
            TCharRead.Next;
          end;
//          showmessage(KanaToRomaji(sbJouyou+s2,1,'j')+' : '+chars[k]);
        end;
      end;
      l:=0;
      for j:=1 to 7 do if length(chars[j]) div 4>l then l:=length(chars[j]) div 4;
      l:=((l-1) div wd)+1;
      for j:=1 to l do
      begin
        if j>1 then o:=o+UH_SPACE+UH_SPACE+UH_SPACE;
        for k:=1 to 7 do
        begin
          for m:=1 to wd do if length(chars[k])<((j-1)*wd+m)*4 then o:=o+UH_SPACE else o:=o+copy(chars[k],((j-1)*wd+m-1)*4+1,4);
          o:=o+UH_SPACE;
        end;
        wrid;
      end;
      wrid;
    end;
  end;
  TCharRead.First;
  while not TCharRead.EOF do
  begin
    if (TCharRead.Int(TCharReadType)=4) and (add.IndexOf(TCharRead.Str(TCharReadKanji))=-1) and (TCharRead.Int(TCharReadPosition)=1) then
    begin
      TChar.Locate('Index',TCharRead.Str(TCharReadKanji),true);
      if IsKnown(KnownLearned,TChar.Str(TCharUnicode)) then o:=o+TChar.Str(TCharUnicode)+TCharRead.Str(TCharReadReading);
    end;
    TCharRead.Next;
  end;
  wrid;
  clip:=o;
  fMenu.ChangeClipboard;
end;

procedure TfKanji.Button3Click(Sender: TObject);
var i:integer;
    t,t2:textfile;
    sbJouyou,s2:string;
begin
  i:=7;
  assignfile(t,'ren.bat');
  rewrite(t);
  while fileexists(inttostr(i)+'.mdw') do
  begin
    assignfile(t2,inttostr(i)+'.mdw');
    reset(t2);
    readln(t2,sbJouyou);
    readln(t2,sbJouyou);
    delete(sbJouyou,1,length(sbJouyou)-4);
    readln(t2,s2);
    delete(s2,1,5);
    closefile(t2);
    writeln(t,'COPY "'+s2+'" so'+sbJouyou+'.gif');
    inc(i);
  end;
  closefile(t);
end;

procedure TfKanji.SelRadical;
begin
  fKanjiSearch.curRadSearchType:=stClassic;
  fKanjiSearch.curRadSearch:=JWBKanjiDetails.curradical;
  fKanjiSearch.edtRadicals.Text:=JWBKanjiDetails.curradno;
  fKanjiSearch.pbRadicals.Invalidate;
  fKanji.DoIt;
end;

procedure TfKanji.DrawGrid1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
begin
  if mbRight=Button then
  begin
{    clip:=clip+curkanji;
    fMenu.ChangeClipboard;}
    fMenu.PopupImmediate(false);
  end;
end;

procedure TfKanji.DrawGrid1DblClick(Sender: TObject);
begin
  if not fMenu.aKanjiDetails.Checked then fMenu.aKanjiDetails.Execute else if fKanjiDetails.Visible then fKanjiDetails.SetFocus;
end;

procedure TfKanji.DrawGrid1MouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  fMenu.IntTipGridOver(DrawGrid1,x,y,false);
end;

function TfKanji.GetKanji(cx,cy:integer):string;
begin
  if (cy*DrawGrid1.ColCOunt+cx>=ki.Count) or (cx<0) or (cy<0) then
  begin
    result:='';
    exit;
  end;
  result:=ki[DrawGrid1.ColCount*cy+cx];
  delete(result,1,1);
end;

procedure TfKanji.RefreshDetails;
begin
  fKanjiDetails.SetCharDetails(curkanji);
end;

procedure TfKanji.SaveChars;
var i:integer;
begin
  if SaveDialog1.Execute then
  begin
    Conv_Create(SaveDialog1.FileName,Conv_ChooseType(false,1));
    for i:=0 to ki.Count-1 do
      Conv_Write(copy(ki[i],2,4));
    Conv_Flush;
    Conv_Close;
  end;
end;

initialization
  calfonts:=TStringList.Create;
  ki:=TStringList.Create;
  curkanji:=UH_NOCHAR;

finalization
  ki.Free;
  calfonts.Free;

end.
