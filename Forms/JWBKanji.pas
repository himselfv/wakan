unit JWBKanji;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ArtLabel, StdCtrls, ComCtrls, Buttons, ExtCtrls,
  RXCtrls, Grids, DB, ShellAPI;

type
  TfKanji = class(TForm)
    Panel1: TPanel;
    RxLabel15: TRxLabel;
    Shape6: TShape;
    Label18: TLabel;
    Label24: TLabel;
    SpeedButton5: TSpeedButton;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    SpeedButton3: TSpeedButton;
    SpeedButton4: TSpeedButton;
    DrawGrid1: TDrawGrid;
    Button1: TButton;
    Timer1: TTimer;
    Panel2: TPanel;
    Panel3: TPanel;
    Button2: TButton;
    SaveDialog1: TSaveDialog;
    procedure FormShow(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure RadioGroup1Click(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure DrawGrid1SelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
    procedure DrawGrid1DrawCell(Sender: TObject; ACol, ARow: Integer;
      Rect: TRect; State: TGridDrawState);
    procedure CheckBox2Click(Sender: TObject);
    procedure SpeedButton15Click(Sender: TObject);
    procedure SpeedButton17Click(Sender: TObject);
    procedure SpeedButton23Click(Sender: TObject);
    procedure DrawGrid1KeyPress(Sender: TObject; var Key: Char);
    procedure Timer1Timer(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
    procedure SpeedButton4Click(Sender: TObject);
    procedure SpeedButton25Click(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure SpeedButton5Click(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure SpeedButton3Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure DrawGrid1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure DrawGrid1DblClick(Sender: TObject);
    procedure DrawGrid1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
  private
    { Private declarations }
  public
    procedure KanjiSearch_SpeedButton20Click(Sender: TObject);
    procedure KanjiCompounds_CheckBox1Click(Sender: TObject);
    procedure KanjiDetails_PaintBox1Paint(Sender: TObject);
    procedure KanjiDetails_PaintBox2Paint(Sender: TObject);
    procedure KanjiDetails_PaintBox3Paint(Sender: TObject);
    procedure KanjiDetails_PaintBox4Paint(Sender: TObject);
    procedure KanjiDetails_PaintBox5Paint(Sender: TObject);
    procedure KanjiDetails_PaintBox6Paint(Sender: TObject);
    procedure KanjiDetails_PaintBox7Paint(Sender: TObject);
    procedure KanjiDetails_SpeedButton21Click(Sender: TObject);
    procedure KanjiDetails_SpeedButton28Click(Sender: TObject);
    procedure KanjiDetails_SpeedButton10Click(Sender: TObject);
    function OffsetRange(tx:string;min,max:integer):string;
    procedure DoIt;
    procedure DoItTimer;
    procedure SetCharDetails(unicode:string);
    procedure SaveChars;
    procedure RefreshDetails;
    procedure SetCharCompounds;
    procedure SelRadical;
    function InfoPaint(canvas:TCanvas;w:integer;onlycount:boolean):integer;
    procedure DrawItem(canvas:TCanvas;its,txs:string;l,r:integer;var x,y,rh:integer;onlycount:boolean);
    procedure DrawSingleText(canvas:TCanvas;tp:char;l,t,r,fh:integer;s:string);
    function FitText(canvas:TCanvas;tp:char;wrap:boolean;w,fh:integer;fname:string;var l:integer;var s:string):string;
    function GetKanji(cx,cy:integer):string;
    { Public declarations }
  end;

var
  fKanji: TfKanji;
  chin:boolean;
  clip2:string;
  testkanji:string;
  curradsearch:string;
  raineradsearch:boolean;

implementation

uses JWBMenu, JWBRadical, JWBUnit, JWBSettings, JWBPrint,
  JWBKanjiSearch, JWBKanjiSort, JWBKanjiCompounds, JWBKanjiDetails,
  JWBStrokeOrder, MemSource, JWBTranslate, JWBConvert, JWBWords,
  JWBDicSearch;

var ki:TStringList;
    calfonts,kval:TStringList;
    curcali,curkanji,curradical,cursimple,curon,curkun,curnanori,curpinyin:string;
    curradno:string;
    curindex:integer;
    radtype:string;
    caltype:integer;

{$R *.DFM}

procedure TfKanji.FormShow(Sender: TObject);
var i:integer;
    bk:integer;
begin
  bk:=fKanjiSearch.ComboBox1.ItemIndex;
  if bk=-1 then bk:=kanji_othersearch;
  fKanjiSearch.ComboBox1.Items.Clear;
  fKanjiSearch.ComboBox1.Items.Add('Unicode');
  for i:=0 to chartypel.Count-1 do
    if strtoint(fMenu.GetCharType(i,0))>20 then
      fKanjiSearch.ComboBox1.Items.Add(_l('^e'+fMenu.GetCharType(i,4)+'^c'+fMenu.GetCharType(i,5)));
  fKanjiSearch.ComboBox1.ItemIndex:=0;
  if bk<fKanjiSearch.ComboBox1.Items.Count-1 then fKanjiSearch.ComboBox1.ItemIndex:=bk;
//  fMenu.ShowForm(SpeedButton5,fMenu.aKanjiSearch,fKanjiSearch);
//  fMenu.ShowForm(SpeedButton1,fMenu.aKanjiSort,fKanjiSort);
//  fMenu.ShowForm(SpeedButton2,fMenu.aKanjiDetails,fKanjiDetails);
//  fMenu.ShowForm(SpeedButton3,fMenu.aKanjiCompounds,fKanjiCompounds);
//  fMenu.ShowForm(SpeedButton4,nil,fStrokeOrder);
  fMenu.aKanji.Checked:=true;
//  fKanjiSearch.SpeedButton20.Enabled:=ChinesePresent;
  chin:=false;
  if curlang='c'then chin:=true;
//  if fKanjiSearch.SpeedButton20.Down then chin:=true;
  DoIt;
  cursimple:='';
  curcali:='';
  caltype:=0;
end;

procedure TfKanji.FormHide(Sender: TObject);
begin
  fMenu.aKanji.Checked:=false;
//  fMenu.HideForm(SpeedButton5,fMenu.aKanjiSearch,fKanjiSearch);
//  fMenu.HideForm(SpeedButton1,fMenu.aKanjiSort,fKanjiSort);
//  fMenu.HideForm(SpeedButton2,fMenu.aKanjiDetails,fKanjiDetails);
//  fMenu.HideForm(SpeedButton3,fMenu.aKanjiCompounds,fKanjiCompounds);
//  fMenu.HideForm(SpeedButton4,nil,fStrokeOrder);
end;

function InClipboard(s:string):boolean;
var s2:string;
begin
  s2:=clip;
  result:=false;
  if pos(','+uppercase(s),clip2)>0 then result:=true;
end;

function TfKanji.OffsetRange(tx:string;min,max:integer):string;
var fnd:boolean;
    curtx,txleft,otx:string;
    nn1,nn2:integer;
    i:integer;
begin
  txleft:=tx;
  otx:='';
  while txleft<>'' do
  begin
    curtx:='';
    while (length(txleft)>0) and (txleft[1]<>';') and (txleft[1]<>',') do
    begin
      curtx:=curtx+txleft[1];
      delete(txleft,1,1);
    end;
    if (length(txleft)>0) and ((txleft[1]=';') or (txleft[1]=',')) then delete(txleft,1,1);
    nn1:=0;
    nn2:=0;
    if pos('-',curtx)=0 then begin try nn1:=strtoint(curtx); nn2:=strtoint(curtx); except end; end
    else
      begin
        try
          nn1:=strtoint(copy(curtx,1,pos('-',curtx)-1));
          delete(curtx,1,pos('-',curtx));
          nn2:=strtoint(curtx);
        except end;
      end;
    nn1:=nn1+min; nn2:=nn2+max;
    if nn1<0 then nn1:=0; if nn2<0 then nn2:=0;
    if nn1>nn2 then nn1:=nn2;
    if nn1=nn2 then if otx='' then otx:=inttostr(nn1) else otx:=otx+';'+inttostr(nn1);
    if nn1<>nn2 then if otx='' then otx:=inttostr(nn1)+'-'+inttostr(nn2) else otx:=otx+';'+inttostr(nn1)+'-'+inttostr(nn2);
  end;
  result:=otx;
end;

procedure TfKanji.DoIt;
procedure MakeList(tx:string;number:boolean;sl:TStringList);
var fnd:boolean;
    curtx,txleft:string;
    min,max:integer;
    i:integer;
begin
  if sl.Count>0 then exit;
  txleft:=tx;
  sl.Sorted:=true;
  while (not fnd) and (txleft<>'') do
  begin
    curtx:='';
    while (length(txleft)>0) and (txleft[1]<>';') and (txleft[1]<>',') do
    begin
      curtx:=curtx+txleft[1];
      delete(txleft,1,1);
    end;
    if (length(txleft)>0) and ((txleft[1]=';') or (txleft[1]=',')) then delete(txleft,1,1);
    if not number then sl.Add(uppercase(curtx));
    if number then
    begin
      if pos('-',curtx)=0 then sl.Add(curtx) else
      begin
        try
          try
            min:=strtoint(copy(curtx,1,pos('-',curtx)-1));
          except min:=1; end;
          delete(curtx,1,pos('-',curtx));
          try
            max:=strtoint(curtx);
          except max:=min; end;
          for i:=min to max do sl.Add(inttostr(i));
        except end;
      end;
    end;
  end;
  if sl.Count=0 then sl.Add('$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$');
end;
function InRange(tx,fld:string;number:boolean;sl:TStringList):boolean;
begin
  MakeList(tx,number,sl);
  result:=sl.IndexOf(uppercase(fld))>-1;
end;
procedure ReadFilter(flt:TStringList;tx:string;typ1,typ2:integer;partial,space,number,takedot:boolean);
var s:string;
    sl:TStringList;
    s2:string;
    i:integer;
    dot:integer;
begin
  sl:=TStringList.Create;
  MakeList(tx,number,sl);
  for i:=0 to sl.Count-1 do
  begin
    s2:=sl[i];
    TCharRead.SetOrder('Reading_Ind');
    TCharRead.Locate('Reading',s2,false);
    if takedot then dot:=TCharRead.Int(TCharReadReadDot);
    s:=uppercase(TCharRead.Str(TCharReadReading));
    while (not TCharRead.EOF) and ((s=uppercase(s2)) or ((partial and not space) and (pos(uppercase(s2),s)=1)) or
          ((partial and space) and (pos(uppercase(s2)+' ',s)=1))) do
    begin
  //    showmessage('FOund '+TCharRead.Str(TCharReadReading)+' '+TCharRead.Str(TCharReadType));
      if (TCharRead.Int(TCharReadType)=typ1) or (TCharRead.Int(TCharReadType)=typ2) then
      if (not takedot) or (s=uppercase(s2)) or ((dot>0) and (uppercase(s2)=copy(s,1,dot-1))) then      
//        if flt.IndexOf(TCharRead.Str(TCharReadKanji))=-1 then
//        begin
         flt.Add(TCharRead.Str(TCharReadKanji));
//         end;
      TCharRead.Next;
      s:=uppercase(TCharRead.Str(TCharReadReading));
      if takedot then dot:=TCharRead.Int(TCharReadReadDot);
    end;
  end;
  flt.Sort;
  flt.Sorted:=true;
end;
var fltclip,fltpinyin,fltyomi,fltmean:TStringList;
    accept:boolean;
    radf:integer;
    i,j,k,grs:integer;
    s1,s2,s3:string;
    s:string;
    x:integer;
    mr:TGridRect;
    b:boolean;
    sl4,sl10:TStringList;
    fltradical,fltskip,fltother:TStringList;
    sltemp:TStringList;
    rrfrom,rrlen:integer;
    rrus:boolean;
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
  if fKanjiSearch.SpeedButton4.Down then ReadFilter(fltpinyin,fKanjiSearch.edit1.text,2,8,true,false,false,false);
  if fKanjiSearch.SpeedButton5.Down then ReadFilter(fltyomi,RomajiToKana('H'+fKanjiSearch.Edit6.Text,romasys,true,'j'),4,5,fSettings.CheckBox57.Checked,false,false,fSettings.CheckBox57.Checked);
  if fKanjiSearch.SpeedButton5.Down then ReadFilter(fltyomi,RomajiToKana('Q'+fKanjiSearch.Edit6.Text,romasys,true,'j'),4,5,fSettings.CheckBox57.Checked,false,false,fSettings.CheckBox57.Checked);
  if fKanjiSearch.SpeedButton14.Down then ReadFilter(fltskip,fKanjiSearch.Edit4.Text,22,0,true,false,false,false);
  if fKanjiSearch.SpeedButton6.Down then if not raineradsearch then ReadFilter(fltradical,fKanjiSearch.Edit2.Text,10,0,false,false,true,false) else
  begin
    sltemp:=TStringList.Create;
    fltradical.Sorted:=true;
    sltemp.Sorted:=true;
    s1:=fKanjiSearch.Edit2.Text;
    rrus:=false;
    while s1<>'' do
    begin
      if pos(';',s1)>0 then
      begin
        s2:=copy(s1,1,pos(';',s1)-1);
        delete(s1,1,pos(';',s1));
      end else
      begin
        s2:=s1;
        s1:='';
      end;
      rrlen:=strtoint(copy(raineradicals[strtoint(s2)-1],9,4));
      rrfrom:=strtoint(copy(raineradicals[strtoint(s2)-1],14,5));
      p:=rainesearch;
      p:=p+rrfrom*2;
      for i:=1 to rrlen do
      begin
        move(p^,w,2);
        s2:=UnicodeToHex(widechar(w));
        p:=p+2;
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
  if fKanjiSearch.SpeedButton22.Down then
  begin
    if fKanjiSearch.ComboBox1.ItemIndex=0 then fltother.Add(fKanjiSearch.Edit8.Text) else
    j:=0;
    for i:=0 to chartypel.Count-1 do if strtoint(fMenu.GetCharType(i,0))>20 then
    begin
      inc(j);
      if j=fKanjiSearch.ComboBox1.ItemIndex then ReadFilter(fltother,fKanjiSearch.Edit8.Text,strtoint(fMenu.GetCharType(i,0)),0,false,false,fMenu.GetCharType(i,3)='N',false);
    end;
  end;
  if fKanjiSearch.SpeedButton7.Down then if chin then
    ReadFilter(fltmean,fKanjiSearch.edit3.text,7,0,true,true,false,false) else
    ReadFilter(fltmean,fKanjiSearch.edit3.text,3,0,true,true,false,false);
  case fSettings.RadioGroup3.ItemIndex of
    0: grs:=30;
    1: grs:=45;
    2: grs:=60;
  end;
  if not chin then
    case fKanjiSearch.RadioGroup1.ItemIndex of
      0,4:TChar.SetOrder('JpUnicode_Ind');
      1:TChar.SetOrder('JpStrokeCount_Ind');
      2:TChar.SetOrder('JpFrequency_Ind');
      3:TChar.SetOrder('JpUnicode_Ind');
    end;
  if chin then
    case fKanjiSearch.RadioGroup1.ItemIndex of
      0,4:TChar.SetOrder('ChUnicode_Ind');
      1:TChar.SetOrder('ChStrokeCount_Ind');
      2:TChar.SetOrder('ChFrequency_Ind');
      3:TChar.SetOrder('ChUnicode_Ind');
    end;
  ki.Clear;
  radf:=fSettings.ComboBox1.ItemIndex+12;
  clipsort:=(fKanjiSearch.SpeedButton3.Down) and (fKanjiSearch.RadioGroup1.ItemIndex=4);
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
    if accept and (fKanjiSearch.SpeedButton4.Down) and (fltpinyin.IndexOf(TChar.Str(TCharIndex))=-1) then accept:=false;
    if accept and (fKanjiSearch.SpeedButton5.Down) and (fltyomi.IndexOf(TChar.Str(TCharIndex))=-1) then accept:=false;
    if accept and (fKanjiSearch.SpeedButton7.Down) and (fltmean.IndexOf(TChar.Str(TCharIndex))=-1) then accept:=false;
    if accept and (fKanjiSearch.SpeedButton14.Down) and (fltskip.IndexOf(TChar.Str(TCharIndex))=-1) then accept:=false;
    if accept and (fKanjiSearch.SpeedButton6.Down) and (not raineradsearch) and (fltradical.IndexOf(TChar.Str(TCharIndex))=-1) then accept:=false;
    if accept and (fKanjiSearch.SpeedButton6.Down) and (raineradsearch) and (fltradical.IndexOf(TChar.Str(TCharUnicode))=-1) then accept:=false;
    if accept and (fKanjiSearch.SpeedButton22.Down) and (fKanjiSearch.ComboBox1.ItemIndex=0) and (fltother.IndexOf(TChar.Str(TCharUnicode))=-1) then accept:=false;
    if accept and (fKanjiSearch.SpeedButton22.Down) and (fKanjiSearch.ComboBox1.ItemIndex>0) and (fltOther.IndexOf(TChar.Str(TCharIndex))=-1) then accept:=false;
//    if accept and (fKanjiSearch.SpeedButton22.Down) and (fKanjiSearch.SpeedButton25.Down) and not InRange(fKanjiSearch.Edit8.text,TChar.Str(TCharUnicode),false,sl1) then accept:=false;
//    if accept and (fKanjiSearch.SpeedButton22.Down) and (fKanjiSearch.SpeedButton26.Down) and not InRange(fKanjiSearch.Edit8.text,TChar.Str(TCharUnicode),true,sl2) then accept:=false;
//    if accept and (fKanjiSearch.SpeedButton22.Down) and (fKanjiSearch.SpeedButton27.Down) and not InRange(fKanjiSearch.Edit8.text,TChar.Str(TCharUnicode),true,sl3) then accept:=false;
    if chin and accept and (fKanjiSearch.SpeedButton12.Down) and not InRange(fKanjiSearch.Edit7.Text,TChar.Str(TCharStrokeCount),true,sl4) then accept:=false;
    if (not chin) and accept and (fKanjiSearch.SpeedButton12.Down) and not InRange(fKanjiSearch.Edit7.Text,TChar.Str(TCharJpStrokeCount),true,sl4) then accept:=false;
    if accept and (fKanjiSearch.SpeedButton14.Down) then
    begin
      s1:=fKanjiSearch.Edit4.Text;
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
      if fKanjiSearch.RadioGroup2.ItemIndex=0 then accept:=false;
      onecheck:=false;
      for k:=0 to kanjicatuniqs.Count-1 do
        if fKanjiSearch.ListBox1.Checked[k] then
      begin
        onecheck:=true;
        if IsKnown(strtoint(kanjicatuniqs[k]),TChar.Str(TCharUnicode)) then
        begin
          if fKanjiSearch.RadioGroup2.ItemIndex=0 then accept:=true;
        end else
        begin
          if fKanjiSearch.RadioGroup2.ItemIndex=1 then accept:=false;
        end;
      end;
      if not onecheck then accept:=true;
      if fKanjiSearch.CheckBox1.Checked and onecheck then accept:=not accept;
    end;
        
{    if accept and (fKanjiSearch.SpeedButton16.Down) then
    begin
      s1:=fKanjiSearch.Edit5.Text;
      if pos('.',s1)>0 then delete(s1,pos('.',s1),1);
      if accept then accept:=InRange(s1,TChar.Str(TCharFourCornerCode),false,sl9);
    end;
}    if accept and (fKanjiSearch.SpeedButton24.Down) and not InRange(fKanjiSearch.Edit9.Text,TChar.Str(TCharJouyouGrade),true,sl10) then accept:=false;
    if accept then
    begin
      if not chin then
      begin
        if TChar.Int(TCharJouyouGrade)<9 then s:='C'else
        if TChar.Int(TCharJouyouGrade)<10 then s:='N'else
        s:='U';
      end else
//        if TChar.Str(TCharType)[1]='A'then s:='A'else
//        if TChar.Str(TCharType)[1]='J'then s:='J'else
        if TChar.Int(TCharChFrequency)<=5 then s:='C'else s:='U';
      if ((not chin) and (fKanjiSearch.RadioGroup1.ItemIndex=3)) or
         ((chin) and (fKanjiSearch.RadioGroup1.ItemIndex=3)) then
        ki.Insert(random(ki.Count),s+TChar.Str(TCharUnicode)) else
          ki.Add(s+TChar.Str(TCharUnicode));
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
  if chin then
    s:=_l('#00129^eFound^cNalezené') else s:=_l('#00130^eFound^cNalezená');
  if (chin) and (fSettings.RadioGroup5.ItemIndex=0) then s:=s+_l('#00131^e traditional^c tradièní');
  if (chin) and (fSettings.RadioGroup5.ItemIndex=1) then s:=s+_l('#00132^e simplified^c zjednodušené');
  if chin then s:=s+_l('#00133^e characters^c znaky') else s:=s+' Kanji';
  s:=s+' ('+inttostr(ki.Count)+_l(')');
  RxLabel15.Caption:=s;
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
  curkanji:='';
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
  fSettings.PageControl1.ActivePage:=fSettings.TabSheet6;
  fSettings.ShowModal;
end;

procedure TfKanji.Button1Click(Sender: TObject);
begin
  ClearKanjiCardCache;
  fPrint.Preview(GetPageNum,DrawPage,PrintConfigure,nil,_l('#00134^eKanji cards^cKarty znakù'));
end;

procedure TfKanji.RadioGroup1Click(Sender: TObject);
begin
  DoIt;
end;

procedure TfKanji.SpeedButton1Click(Sender: TObject);
begin
  fMenu.ToggleForm(fKanjiSort,SpeedButton1,fMenu.aKanjiSort);
end;

procedure TfKanji.KanjiCompounds_CheckBox1Click(Sender: TObject);
var sel:TGridRect;
    b:boolean;
begin
  sel:=DrawGrid1.Selection;
  DrawGrid1SelectCell(sender,sel.left,sel.top,b);
end;

procedure TfKanji.SetCharDetails(unicode:string);
var piny,engy,kory,s,cany,chiny:string;
    ony,kuny,nany:widestring;
    ws:widestring;
    sl:TStringList;
    radf:integer;
    i,j,k:integer;
    kix:string;
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
begin
  curkanji:='';
  curradical:='';
  curon:='';
  curkun:='';
  curnanori:='';
  cursimple:='';
  fKanjiDetails.PaintBox1.Invalidate;
  fKanjiDetails.PaintBox2.Invalidate;
  fKanjiDetails.PaintBox3.Invalidate;
  fKanjiDetails.RxLabel35.Hide;
  fKanjiDetails.PaintBox4.Hide;
  fKanjiDetails.Shape10.Hide;
  fKanjiDetails.Label1.Caption:='-';
  kix:=unicode;
  if not TChar.Locate('Unicode',kix,false) then exit;
  Screen.Cursor:=crHourGlass;
  curkanji:=TChar.Str(TCharUnicode);
  curindex:=TChar.Int(TCharIndex);
  ld:=false;
  cv:=fMenu.GetCharValue(curindex,51);
  fKanjiDetails.ProURLLabel1.Enabled:=cv<>'';
  fKanjiDetails.ProURLLabel1.URL:='http://www.zhongwen.com/cgi-bin/zipux2.cgi?b5=%'+copy(cv,1,2)+'%'+copy(cv,3,2);
  fKanjiDetails.ProURLLabel2.Enabled:=TChar.Int(TCharChinese)=0;
  fKanjiDetails.ProURLLabel2.URL:='http://www.csse.monash.edu.au/cgi-bin/cgiwrap/jwb/wwwjdic?1MKU'+lowercase(curKanji);
  fKanjiDetails.ProURLLabel3.Enabled:=true;
  fKanjiDetails.ProURLLabel3.URL:='http://charts.unicode.org/unihan/unihan.acgi$0x'+lowercase(curKanji);
  cv:=fMenu.GetCharValue(curindex,54);
  fKanjiDetails.ProURLLabel4.Enabled:=(cv<>'')
    and TryStrToInt(copy(cv,1,2), cv_i1)
    and TryStrToInt(copy(cv,3,2), cv_i2);
  if fKanjiDetails.ProURLLabel4.Enabled then
    fKanjiDetails.ProURLLabel4.URL:='www.ocrat.com/chargif/GB/horiz/'+lowercase(Format('%2x%2x',[cv_i1+160,cv_i2+160]))+'.html';
  cv:=fMenu.GetCharValue(curindex,57);
  fKanjiDetails.ProURLLabel5.Enabled:=cv<>'';
  fKanjiDetails.ProURLLabel5.URL:='http://web.mit.edu/jpnet/ji/data/'+cv+'.html';
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
  if fMenu.GetCharValue(TChar.Int(TCharIndex),43)<>'' then
  begin
    cursimple:=fMenu.GetCharValue(TChar.Int(TCharIndex),43);
    fKanjiDetails.RxLabel35.Caption:=_l('#00135^cZjednoduš.:^eSimplified:');
    fKanjiDetails.RxLabel35.Show;
    fKanjiDetails.PaintBox4.Show;
    fKanjiDetails.Shape10.Show;
  end else
  if fMenu.GetCharValue(TChar.Int(TCharIndex),44)<>'' then
  begin
    cursimple:=fMenu.GetCharValue(TChar.Int(TCharIndex),44);
    fKanjiDetails.RxLabel35.Caption:=_l('#00136^cTradièní:^eTraditional:');
    fKanjiDetails.RxLabel35.Show;
    fKanjiDetails.PaintBox4.Show;
    fKanjiDetails.Shape10.Show;
  end else
  begin
    cursimple:='';
    fKanjiDetails.RxLabel35.Hide;
    fKanjiDetails.PaintBox4.Hide;
    fKanjiDetails.Shape10.Hide;
  end;
  i:=fMenu.GetCharValueRad(TChar.Int(TCharIndex),radf);
  curradno:=inttostr(i);
  fKanjiDetails.Label2.Caption:=curradno;
  if i=255 then curradical:='' else
  begin
    if TRadicals.Locate('Number',inttostr(i),true) then
      curradical:=TRadicals.Str(TRadicalsUnicode) else curradical:='';
  end;
{  if chin then
    fKanjiDetails.RxLabel21.Caption:=_l('#00137^eChar #^cZnak #')+inttostr(DrawGrid1.ColCount*Arow+Acol+1) else
    fKanjiDetails.RxLabel21.Caption:='Kanji #'+inttostr(DrawGrid1.ColCount*Arow+Acol+1);}
  if not chin then
  begin
    if TChar.Int(TCharJouyouGrade)<9 then kig:='C'else
    if TChar.Int(TCharJouyouGrade)<10 then kig:='N'else
    kig:='U';
  end else
    if TChar.Int(TCharChFrequency)<=5 then kig:='C'else kig:='U';
  if IsKnown(KnownLearned,kix) then kig:='K';
  if not IsKnown(strtoint(kanjicatuniqs[fKanjiDetails.ComboBox1.ItemIndex]),kix) then fKanjiDetails.SpeedButton21.Caption:='+'
  else fKanjiDetails.SpeedButton21.Caption:='-';
  if chin then if TChar.Int(TCharStrokeCount)<255 then fKanjiDetails.Label9.Caption:=TChar.Str(TCharStrokeCount) else fKanjiDetails.Label9.Caption:='-';
  if not chin then if TChar.Int(TCharJpStrokeCount)<255 then fKanjiDetails.Label9.Caption:=TChar.Str(TCharJpStrokeCount) else fKanjiDetails.Label9.Caption:='-';
  scat:='';
  for i:=0 to kanjicatuniqs.Count-1 do
  begin
    if IsKnown(strtoint(kanjicatuniqs[i]),kix) then
      if scat='' then scat:=fKanjiDetails.ComboBox1.Items[i] else
        scat:=scat+', '+fKanjiDetails.ComboBox1.Items[i];
  end;
  if scat='' then scat:='-';
  fKanjiDetails.Label3.Caption:=scat;
  case kig[1] of
    'K':fKanjiDetails.RxLabel38.Font.Color:=Col('Kanji_Learned');
    'C':fKanjiDetails.RxLabel38.Font.Color:=Col('Kanji_Common');
    'U':fKanjiDetails.RxLabel38.Font.Color:=Col('Kanji_Rare');
    'N':fKanjiDetails.RxLabel38.Font.Color:=Col('Kanji_Names');
  end;
  case kig[1] of
    'K':fKanjiDetails.RxLabel38.Caption:=_l('#00140^cNauèený^eLearned');
    'C':fKanjiDetails.RxLabel38.Caption:=_l('#00141^cBìžný^eCommon');
    'U':fKanjiDetails.RxLabel38.Caption:=_l('#00142^cVzácný^eRare');
    'N':fKanjiDetails.RxLabel38.Caption:=_l('#00143^cPoužívaný ve jménech^eUsed in names');
    'A':fKanjiDetails.RxLabel38.Caption:=_l('#00144^cJaponský i èínský^eJapanese and chinese');
    'J':fKanjiDetails.RxLabel38.Caption:=_l('#00145^cPouze japonský^eJapanese only');
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
    s:=TCharRead.Str(TCharReadReading);
    if (TCharRead.Int(TCharReadType)>3) and (TCharRead.Int(TCharReadType)<7) then
    begin
          ws:='';
          adddot:=0;
          if s[1]='+'then
          begin
            ws:=HexToUnicode('FF0B');
            delete(s,1,1);
            adddot:=1;
          end;
          if s[1]='-'then
          begin
            ws:=ws+HexToUnicode('FF0D');
            delete(s,1,1);
            adddot:=1;
          end;
          if TCharRead.Int(TCharReadReadDot)>0 then
          begin
            ws:=ws+HexToUnicode(copy(s,1,TCharRead.Int(TCharReadReadDot)-1-adddot));
            ws:=ws+HexToUnicode('FF0E');
            delete(s,1,TCharRead.Int(TCharReadReadDot)-1-adddot);
          end;
          if s[length(s)]='-'then ws:=ws+HexToUnicode(copy(s,1,length(s)-1))+HexToUnicode('FF0D')
            else ws:=ws+HexToUnicode(s);
    end;
    case TCharRead.Int(TCharReadType) of
      1:if kory='' then kory:=s else kory:=kory+', '+s;
      2:if piny='' then piny:=s else piny:=piny+','+s;
      4:if ony='' then ony:=ws else ony:=ony+HexToUnicode('FF0C')+ws;
      5:if kuny='' then kuny:=ws else kuny:=kuny+HexToUnicode('FF0C')+ws;
      6:if nany='' then nany:=ws else nany:=nany+HexToUnicode('FF0C')+ws;
      7:if chiny='' then chiny:=s else chiny:=chiny+', '+s;
      3:if engy='' then engy:=s else engy:=engy+', '+s;
      8:if cany='' then cany:=s else cany:=cany+', '+s;
    end;
    TCharRead.Next;
  end;
  if chin then
    fKanjiDetails.Label1.Caption:=chiny else
    fKanjiDetails.Label1.Caption:=engy;
  curon:=UnicodeToHex(ony);
  curkun:=UnicodeToHex(kuny);
  curnanori:=UnicodeToHex(nany);
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
        4:s:=UnicodeToHex(ony);
        5:s:=UnicodeToHex(kuny);
        6:s:=UnicodeToHex(nany);
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
        kval.Add(fMenu.GetCharType(j,3)+';'+chardetl[i]+_l('^e'+fMenu.GetCharType(j,4)+'^c'+fMenu.GetCharType(j,5)));
      kval.Add(s);
    end;
  end;
  h:=InfoPaint(fKanjiDetails.PaintBox3.Canvas,fKanjiDetails.PaintBox3.Width,true);
  fKanjiDetails.PaintBox3.Height:=h;
  sl.Free;
  curcali:=curkanji;
  Screen.Cursor:=crDefault;
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
    if (fKanjiCompounds.SpeedButton9.Down) then
    begin
      if (curlang='j') and (fKanjiCompounds.CheckBox3.Checked) then
      begin
        for i:=0 to dicts.Count-1 do if ((dicts.Objects[i] as TJaletDic).loaded) and (pos(','+(dicts.Objects[i] as TJaletDic).name,NotGroupDicts[4])=0)
          and ((dicts.Objects[i] as TJaletDic).TDictFrequency<>-1) then
        begin
          dic:=dicts.Objects[i] as TJaletDic;
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
        for i:=0 to dicts.Count-1 do if ((dicts.Objects[i] as TJaletDic).loaded) and (pos(','+(dicts.Objects[i] as TJaletDic).name,NotGroupDicts[4])=0) then
        begin
          dic:=dicts.Objects[i] as TJaletDic;
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
          fWords.ListWordCategories(TUserIdx.Int(TUserIdxWord),sl2,'',false);
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
  SetCharDetails(kix);
  fMenu.AnnotShowMedia(kix,'');
  if fKanjiCompounds.Visible then SetCharCompounds;
end;

procedure TfKanji.DrawGrid1DrawCell(Sender: TObject; ACol, ARow: Integer;
  Rect: TRect; State: TGridDrawState);
var w:widechar;
    kix:string;
    kig:string;
    s:string;
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
  w:=HexToUnicode(kix)[1];
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
  if fSettings.CheckBox69.Checked and fMenu.IsAnnot then
  begin
    fMenu.AnnotSeekK(TChar.Str(TCharUnicode),'');
    s:=fMenu.AnnotGetOne('c');
    if s<>'' then try
      DrawGrid1.Canvas.Font.Color:=strtoint('0x'+copy(s,5,2)+copy(s,3,2)+copy(s,1,2));
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

procedure TfKanji.KanjiDetails_PaintBox1Paint(Sender: TObject);
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
  DrawUnicode(fKanjiDetails.PaintBox1.Canvas,1,1,137,curkanji,f);
  if fKanjiDetails.SpeedButton1.Down then DrawStrokeOrder(fKanjiDetails.PaintBox1.Canvas,1,1,137,137,curkanji,12,clBlue);
end;

procedure TfKanji.KanjiDetails_PaintBox2Paint(Sender: TObject);
var f:string;
begin
  f:=FontRadical;
  fKanjiDetails.PaintBox2.Canvas.Brush.Color:=clWindow;
  fKanjiDetails.PaintBox2.Canvas.Font.Style:=[];
  BeginDrawReg(fKanjiDetails.PaintBox2);
  DrawUnicode(fKanjiDetails.PaintBox2.Canvas,1,1,48,curradical,f);
  EndDrawReg;
end;

procedure TfKanji.KanjiDetails_PaintBox3Paint(Sender: TObject);
begin
end;

procedure TfKanji.KanjiDetails_PaintBox4Paint(Sender: TObject);
begin
  fKanjiDetails.PaintBox4.Canvas.Brush.Color:=clWindow;
  fKanjiDetails.PaintBox4.Canvas.Font.Style:=[];
  BeginDrawReg(fKanjiDetails.PaintBox4);
  DrawUnicode(fKanjiDetails.PaintBox4.Canvas,1,1,48,cursimple,FontRadical);
  EndDrawReg;
end;

procedure TfKanji.KanjiDetails_PaintBox5Paint(Sender: TObject);
begin
end;

procedure TfKanji.KanjiDetails_PaintBox6Paint(Sender: TObject);
begin
end;

procedure TfKanji.KanjiDetails_PaintBox7Paint(Sender: TObject);
begin
end;

procedure TfKanji.KanjiSearch_SpeedButton20Click(Sender: TObject);
begin
  chin:=curlang='c';
  fKanjiSearch.RadioGroup1.Items.Clear;
  if chin then
  begin
    fKanjiSearch.RadioGroup1.Items.Add(_l('#00146^eRadical^cRadikálu'));
    fKanjiSearch.RadioGroup1.Items.Add(_l('#00147^eStroke count^cPoètu tahù'));
    fKanjiSearch.RadioGroup1.Items.Add(_l('#00148^eFrequency^cFrekvence'));
    fKanjiSearch.RadioGroup1.Items.Add(_l('#00149^eRandom^cNáhodnì'));
    fKanjiSearch.RadioGroup1.Items.Add(_l('#00877^eUnsorted^cNetøídit'));
  end else
  begin
    fKanjiSearch.RadioGroup1.Items.Add(_l('#00146^eRadical^cRadikálu'));
    fKanjiSearch.RadioGroup1.Items.Add(_l('#00147^eStroke count^cPoètu tahù'));
    fKanjiSearch.RadioGroup1.Items.Add(_l('#00148^eFrequency^cFrekvence'));
    fKanjiSearch.RadioGroup1.Items.Add(_l('#00149^eRandom^cNáhodnì'));
    fKanjiSearch.RadioGroup1.Items.Add(_l('#00877^eUnsorted^cNetøídit'));
  end;
  fKanjiSearch.RadioGroup1.ItemIndex:=0;
  DoIt;
end;

procedure TfKanji.SpeedButton15Click(Sender: TObject);
begin
  showmessage(_l('#00150^cFunkce není doposud implementována.^eFeature not implemented yet.'));
end;

procedure TfKanji.SpeedButton17Click(Sender: TObject);
begin
  showmessage(_l('#00150^cFunkce není doposud implementována.^eFeature not implemented yet.'));
end;

procedure TfKanji.KanjiDetails_SpeedButton21Click(Sender: TObject);
var b:boolean;
begin
  if curkanji<>'' then
    SetKnown(strtoint(kanjicatuniqs[fKanjiDetails.ComboBox1.ItemIndex]),curkanji,
      not IsKnown(strtoint(kanjicatuniqs[fKanjiDetails.ComboBox1.ItemIndex]),curkanji));
  fMenu.ChangeUserData;
  SetCharDetails(curkanji);
end;

procedure TfKanji.SpeedButton23Click(Sender: TObject);
begin
  clip:=clip+curkanji;
  fMenu.ChangeClipboard;
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
    if not fMenu.aKanjiDetails.Checked then fMenu.aKanjiDetails.Execute else if fKanjiDetails.Visible then fKanjiDetails.SetFocus;
  end;
  if key=#8 then
  begin
    if length(clip)>0 then delete(clip,length(clip)-3,4);
    fMenu.ChangeClipboard;
  end;
end;

procedure TfKanji.DoItTimer;
begin
//  Timer1.Interval:=1000;
//  Timer1.Enabled:=true;
  DoIt;
end;

procedure TfKanji.Timer1Timer(Sender: TObject);
begin
  Timer1.Enabled:=false;
  DoIt;
end;

procedure TfKanji.BitBtn1Click(Sender: TObject);
begin
  DrawGrid1.SetFocus;
end;

procedure TfKanji.SpeedButton4Click(Sender: TObject);
begin
  fMenu.ToggleForm(fStrokeOrder,SpeedButton4,nil);
end;

procedure TfKanji.SpeedButton25Click(Sender: TObject);
begin
  DoIt;
end;

procedure TfKanji.KanjiDetails_SpeedButton28Click(Sender: TObject);
var s:string;
begin
  s:='StrokeOrder\'+curkanji+'.gif';
  ShellExecute(handle,nil,pchar(s),nil,nil,SW_SHOW);
end;

procedure TfKanji.KanjiDetails_SpeedButton10Click(Sender: TObject);
begin
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

procedure TfKanji.SpeedButton5Click(Sender: TObject);
begin
  fMenu.ToggleForm(fKanjiSearch,SpeedButton5,fMenu.aKanjiSearch);
end;

procedure TfKanji.SpeedButton2Click(Sender: TObject);
begin
  fMenu.aKanjiDetails.Execute;
end;

procedure TfKanji.SpeedButton3Click(Sender: TObject);
var CanSelect:boolean;
begin
  fMenu.ToggleForm(fKanjiCompounds,SpeedButton3,fMenu.aKanjiCompounds);
  DrawGrid1SelectCell(Sender, DrawGrid1.Col, DrawGrid1.Row, CanSelect);
end;

procedure TfKanji.Button2Click(Sender: TObject);
var di:integer;
    chars:array[1..7] of string;
    i,j,k,l,m:integer;
    s,s2:string;
    wd:integer;
    o:string;
    add:TStringList;
procedure wrid;
var s:string;
    x:integer;
begin
  doc.Add(o);
  s:='';
  for x:=1 to length(o) div 4 do s:=s+'-90000001';
  doctr.Add(s);
  o:='';
end;
begin
  doc.Clear;
  doctr.Clear;
  wd:=6;
  o:='';
  add:=TStringList.Create;
  o:='002000200020';
  for i:=0 to readchl.Count-1 do
  begin
    s:=readchl[i];
    if s[1]='e'then
    begin
      delete(s,1,1);
      if s='' then s:='0020';
      o:=o+s;
      for j:=1 to wd do o:=o+'0020';
    end;
  end;
  wrid;
  wrid;
  for i:=0 to readchl.Count-1 do
  begin
    s:=readchl[i];
    if s[1]='b'then
    begin
      delete(s,1,1);
      o:=o+s+'0020';
      if length(s)<=4 then o:=o+'0020';
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
            if (TCharRead.Int(TCharReadType)=4) and (TCharRead.Str(TCharReadReading)=s+s2) and (TCharRead.Int(TCharReadPosition)=1) then
            begin
              TChar.Locate('Index',TCharRead.Str(TCharReadKanji),true);
              if IsKnown(KnownLearned,TChar.Str(TCharUnicode)) then begin
                chars[k]:=chars[k]+TChar.Str(TCharUnicode);
                add.Add(TCharRead.Str(TCharReadKanji));
              end;
            end;
            TCharRead.Next;
          end;
//          showmessage(KanaToRomaji(s+s2,1,'j')+' : '+chars[k]);
        end;
      end;
      l:=0;
      for j:=1 to 7 do if length(chars[j]) div 4>l then l:=length(chars[j]) div 4;
      l:=((l-1) div wd)+1;
      for j:=1 to l do
      begin
        if j>1 then o:=o+'002000200020';
        for k:=1 to 7 do
        begin
          for m:=1 to wd do if length(chars[k])<((j-1)*wd+m)*4 then o:=o+'0020'else o:=o+copy(chars[k],((j-1)*wd+m-1)*4+1,4);
          o:=o+'0020';
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
    s,s2:string;
begin
  i:=7;
  assignfile(t,'ren.bat');
  rewrite(t);
  while fileexists(inttostr(i)+'.mdw') do
  begin
    assignfile(t2,inttostr(i)+'.mdw');
    reset(t2);
    readln(t2,s);
    readln(t2,s);
    delete(s,1,length(s)-4);
    readln(t2,s2);
    delete(s2,1,5);
    closefile(t2);
    writeln(t,'COPY "'+s2+'" so'+s+'.gif');
    inc(i);
  end;
  closefile(t);
end;

function TfKanji.FitText(canvas:TCanvas;tp:char;wrap:boolean;w,fh:integer;fname:string;var l:integer;var s:string):string;
function countwidth(tp:char;fh:integer;s:string):integer;
var ts:TSize;
begin
  if tp='P'then result:=0 else
  if (tp='U') then result:=(length(s) div 4)*(fh-2)
  else begin
    ts:=canvas.TextExtent(s);
    result:=ts.cx;
  end;
end;
var st,stl:string;
    i,ii,iii:integer;
begin
  canvas.Font.Name:=fname;
  canvas.Font.Height:=fh;
  if countwidth(tp,fh,s)<=w then
  begin
    l:=countwidth(tp,fh,s);
    result:=s;
    s:='';
    exit;
  end;
  i:=0;
  stl:='';
  st:='';
  ii:=0;
  iii:=0;
  repeat
    stl:=st;
    iii:=ii;
    ii:=i;
    if (tp='U') or (tp='P') then st:=copy(s,1,i*4) else st:=copy(s,1,i);
    if not wrap then if (tp='U') or (tp='P') then st:=st+'2026'else st:=st+'...';
    if (tp='U') or (tp='P') or (not wrap) then inc(i) else
    begin
      inc(i);
      while (i<length(s)) and (s[i+1]<>' ') do inc(i);
    end;
  until countwidth(tp,fh,st)>w;
  if stl='' then stl:=st;
  i:=iii;
  if i<0 then i:=0;
  if wrap then
  begin
    result:=stl;
    if (tp='U') or (tp='P') then delete(s,1,i*4) else delete(s,1,i);
    l:=countwidth(tp,fh,stl);
  end else
  begin
    result:=stl;
    s:='';
    l:=countwidth(tp,fh,stl);
  end;
end;

procedure TfKanji.DrawSingleText(canvas:TCanvas;tp:char;l,t,r,fh:integer;s:string);
var font:string;
begin
  if chin then font:=FontRadical else font:=FontSmall;
  if tp='P'then font:=FontEnglish;
  if (tp='U') or (tp='P') then DrawUnicode(canvas,l,t,fh-2,s,font) else
  if (tp='N') or (tp='T') then canvas.TextOut(r-canvas.TextExtent(s).cx,t,s) else
    canvas.TextOut(l,t,s);
end;

procedure TfKanji.DrawItem(canvas:TCanvas;its,txs:string;l,r:integer;var x,y,rh:integer;onlycount:boolean);
var fh:integer;
    fname:string;
    lbl:string;
function DoesFit(s:string):boolean;
var l:integer;
begin
  FitText(canvas,its[1],true,r-x,fh,fname,l,s);
  if s<>'' then result:=false else result:=true;
end;
function GetDet(j:integer):string;
var s:string;
begin
  s:=its;
  while j>0 do
  begin
    delete(s,1,pos(';',s));
    dec(j);
  end;
  delete(s,pos(';',s),length(s)-pos(';',s)+1);
  result:=s;
end;
var lw,rr:integer;
    ws:string;
    s:string;
begin
  if (GetDet(4)='C') and not chin then exit;
  if (GetDet(4)='J') and chin then exit;
  if (GetDet(5)='N') and (txs='') and (its[1]<>'-') then exit;
  if its[1]='R'then its[1]:='U';
  if txs='' then if (its[1]<>'U') and (its[1]<>'R') and (its[1]<>'P') then txs:='-';
  canvas.Font.Style:=[fsBold];
  case GetDet(6)[1] of
    'B':fh:=20;
    'M':fh:=16;
    'S':fh:=12;
  end;
  fname:=FontEnglish;
  if GetDet(3)<>'N'then
  begin
    lbl:=GetDet(7);
    if its[1]<>'-'then lbl:=lbl+':';
    if (GetDet(2)='C') and (x>l) then
    begin
      if not DoesFit(lbl) then
      begin
        x:=l;
        y:=y+rh;
      end;
    end else if x>l then
    begin
      x:=l;
      y:=y+rh;
    end;
    lbl:=FitText(canvas,'S',false,r-x,fh,fname,lw,lbl);
    if not onlycount then if txs<>'---'then DrawSingleText(canvas,'S',x,y,r,fh,lbl);
    lw:=lw+5;
    canvas.Font.Style:=[];
    if (GetDet(3)='W') or ((GetDet(3)='L') and (GetDet(2)='W') and (lw>(r-x) div 2)) then
    begin
      x:=l;
      y:=y+fh+2;
    end else x:=x+lw;
  end;
  canvas.Font.Style:=[];
    if (its[1]='U') or (its[1]='R') then fname:=FontSmall else fname:=FontEnglish;
  rh:=fh+2;
  rr:=r;
  if (GetDet(2)='C') and (x<r div 2) then rr:=(r div 2)-5;
  if (GetDet(2)='W') and (txs<>'---') then
  begin
    s:=txs;
    while s<>'' do
    begin
      ws:=FitText(canvas,its[1],true,r-x,fh,fname,lw,s);
      if not onlycount then if txs<>'---'then DrawSingleText(canvas,its[1],x,y,r,fh,ws);
      x:=l;
      inc(y,rh);
    end;
  end else
  begin
    s:=txs;
    ws:=FitText(canvas,its[1],false,rr-x,fh,fname,lw,s);
    if not onlycount then if txs<>'---'then DrawSingleText(canvas,its[1],x,y,rr,fh,ws);
    if (not onlycount) and (txs='---') then
    begin
      canvas.MoveTo(l,y+7);
      canvas.LineTo(r,y+7);
    end;
    if rr=r then
    begin
      x:=l;
      inc(y,rh);
    end else x:=rr+5;
  end;
end;

function TfKanji.InfoPaint(canvas:TCanvas;w:integer;onlycount:boolean):integer;
var i:integer;
    x,y,rh:integer;
begin
  x:=6;
  y:=3;
  rh:=0;
  for i:=0 to (kval.Count div 2)-1 do
  begin
    DrawItem(canvas,kval[i*2],kval[i*2+1],6,w-2,x,y,rh,onlycount);
  end;
  result:=y;
end;

procedure TfKanji.SelRadical;
begin
  fKanjiSearch.Edit2.Text:=curradno;
  curradsearch:=curradical;
  fKanjiSearch.PaintBox1.Invalidate;
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
  SetCharDetails(curkanji);
end;

procedure TfKanji.SaveChars;
var f:file;
    i:integer;
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
  kval:=TStringList.Create;
  curkanji:='';
  curradical:='';
  curon:='';
  curkun:='';
  curnanori:='';
  curradsearch:='';
  curindex:=0;

finalization
  ki.Free;
  calfonts.Free;
  kval.Free;

end.
