unit JWBWordList;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, StdCtrls, ExtCtrls, Buttons, Gauges, JwbForms;

type
  TfWordList = class(TJwbForm)
    Notebook1: TNotebook;
    btnCancel: TBitBtn;
    btnNext: TBitBtn;
    btnPrev: TBitBtn;
    RxLabel3: TLabel;
    Label1: TLabel;
    Label14: TLabel;
    RxLabel4: TLabel;
    RadioGroup1: TRadioGroup;
    RadioGroup2: TRadioGroup;
    Label15: TLabel;
    Edit3: TEdit;
    Button1: TButton;
    CheckBox1: TCheckBox;
    RxLabel5: TLabel;
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
    RxLabel6: TLabel;
    Label17: TLabel;
    Button4: TButton;
    CheckBox4: TCheckBox;
    RadioGroup7: TRadioGroup;
    RxLabel7: TLabel;
    RadioGroup8: TRadioGroup;
    RxLabel8: TLabel;
    Gauge1: TGauge;
    Label22: TLabel;
    Label23: TLabel;
    Label24: TLabel;
    Shape2: TShape;
    PaintBox1: TPaintBox;
    Shape5: TShape;
    Label25: TLabel;
    PaintBox6: TPaintBox;
    RxLabel9: TLabel;
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
    RxLabel10: TLabel;
    Label43: TLabel;
    Label45: TLabel;
    RxLabel11: TLabel;
    RxLabel12: TLabel;
    Bevel1: TBevel;
    RxLabel1: TLabel;
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
    RxLabel2: TLabel;
    RadioGroup10: TRadioGroup;
    Button5: TButton;
    CheckBox5: TCheckBox;
    Label51: TLabel;
    Edit1: TEdit;
    btnFinish: TBitBtn;
    Label52: TLabel;
    Label53: TLabel;
    Label54: TLabel;
    Label55: TLabel;
    CheckBox6: TCheckBox;
    procedure Button2Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure btnNextClick(Sender: TObject);
    procedure btnPrevClick(Sender: TObject);
    procedure RadioGroup2Click(Sender: TObject);
    procedure RadioGroup1Click(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure btnCancelClick(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure CheckBox5Click(Sender: TObject);
    procedure PaintBox1Paint(Sender: TObject);
    procedure PaintBox6Paint(Sender: TObject);
    procedure PaintBox2Paint(Sender: TObject);
    procedure Button10Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);

  public
    step:integer;
    procedure Upd;

  protected
    twi:integer;
    twchg:array[0..3,0..3] of integer;
    ll,ltl:TStringList;
    twkanji,twphonetic,twmeaning:string;
  public
    function LLGoNext(page:integer):integer;
    function LLGoPrev(page:integer):integer;
    procedure BuildWordList;
    procedure PrepareTestWord;
    procedure ShowTestWord;
    function FinishTestWord(button:integer):boolean;
    procedure SaveTestResults;
    procedure PrintWordList;
    procedure SaveWordList;
    procedure UpdateWordListStats;

  end;

implementation
uses JWBStrings, JWBLanguage, JWBCore, JWBUnit, JWBSettings, JWBPrint, Printers,
  JWBVocab, JWBMenu, JWBUserData, TextTable, StdPrompt, JWBCategories,
  JWBLegacyMarkup;

{$R *.DFM}

procedure TfWordList.FormCreate(Sender: TObject);
begin
  ll:=TStringList.Create;
  ltl:=TStringList.Create;
end;

procedure TfWordList.FormDestroy(Sender: TObject);
begin
  ll.Free;
  ltl.Free;
end;

procedure TfWordList.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
var i,j:integer;
begin
  i:=step;
  while i>0 do
  begin
    j:=Self.LLGoPrev(i);
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

procedure TfWordList.Upd;
begin
  Notebook1.PageIndex:=step-1;
  btnPrev.Enabled:=step>1;
  btnNext.Enabled:=(step<>6) and (step<8);
  btnFinish.Enabled:=step=8;
  btnCancel.Enabled:=true;
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

procedure TfWordList.btnNextClick(Sender: TObject);
begin
  step:=Self.LLGoNext(step);
  if step<9 then Upd else Close;
end;

procedure TfWordList.btnPrevClick(Sender: TObject);
begin
  step:=Self.LLGoPrev(step);
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
  if RadioGroup1.ItemIndex=0 then Label23.Caption:=inttostr(fVocab.WordListCount) else
    Label23.Caption:=inttostr(TUser.RecordCount);
end;

procedure TfWordList.btnCancelClick(Sender: TObject);
begin
  Close;
end;

procedure TfWordList.Button4Click(Sender: TObject);
begin
  Self.BuildWordList;
end;

procedure TfWordList.Button7Click(Sender: TObject);
var res:boolean;
begin
  res:=Self.FinishTestWord((sender as TButton).Tag);
  if not res then
  begin
    step:=Self.LLGoNext(step);
    Upd;
    exit;
  end;
  Self.PrepareTestWord;
  Button10.SetFocus;
end;

procedure TfWordList.Button5Click(Sender: TObject);
begin
  Self.PrintWordList;
end;

procedure TfWordList.CheckBox5Click(Sender: TObject);
begin
  Label51.Enabled:=CheckBox5.Checked;
  Edit1.Enabled:=CheckBox5.Checked;
end;

procedure TfWordList.PaintBox1Paint(Sender: TObject);
begin
  PaintBox1.Canvas.Brush.Color:=clWindow;
  DrawKana(PaintBox1.Canvas,1,1,22,twphonetic,FontJapanese,showroma,curlang);
end;

procedure TfWordList.PaintBox6Paint(Sender: TObject);
var x:string;
begin
  PaintBox6.Canvas.Brush.Color:=clWindow;
  x:=twkanji;
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
  DrawUnicode(PaintBox2.Canvas,1,1,22,fstr(twmeaning),FontEnglish);
end;

procedure TfWordList.Button10Click(Sender: TObject);
begin
  Self.ShowTestWord;
  Button7.SetFocus;
end;

function TfWordList.LLGoNext(page:integer):integer;
var i,j:integer;
begin
  result:=page;
  case page of
    1:result:=2;
    2:begin
        i:=0; try i:=strtoint(Edit3.Text); except end;
        j:=0; try j:=strtoint(Label23.Caption); except end;
        if (RadioGroup2.ItemIndex<2) and ((i<1) or (i>500)) then
        begin
          Application.MessageBox(
            pchar(_l('#00858^eInvalid number of words (must be between 1 and 500).')),
            pchar(_l('#00020^eError')),
            MB_ICONERROR or MB_OK);
          exit;
        end;
        if (RadioGroup2.ItemIndex=0) and (2*i>j) then
        begin
          Application.MessageBox(
            pchar(_l('#00862^eNumber of input words must be at least two times higher than number of selected words.')),
            pchar(_l('#00020^eError')),
            MB_ICONERROR or MB_OK);
          exit;
        end;
        if (RadioGroup2.ItemIndex=1) and (i>j) then
        begin
          Application.MessageBox(
            pchar(_l('#00863^eNumber of input words must not be lower than number of selected words.')),
            pchar(_l('#00020^eError')),
            MB_ICONERROR or MB_OK);
          exit;
        end;
        if j<1 then
        begin
          Application.MessageBox(
            pchar(_l('#00864^eInput list cannot be empty.')),
            pchar(_l('#00020^eError')),
            MB_ICONERROR or MB_OK);
          exit;
        end;
        if RadioGroup2.ItemIndex=0 then result:=3 else result:=4;
        if result=4 then BuildWordList;
      end;
    3:begin
        result:=4;
        BuildWordList;
      end;
    4:if RadioGroup7.ItemIndex=0 then result:=5 else result:=8;
    5:begin
        RadioGroup7.ItemIndex:=1;
        RadioGroup7.Enabled:=false;
        twi:=0;
        ltl.Clear;
        for i:=0 to 3 do for j:=0 to 3 do twchg[i,j]:=0;
        PrepareTestWord;
        result:=6;
      end;
    6:result:=7;
    7:begin
        if RadioGroup9.ItemIndex=0 then SaveTestResults;
        result:=8;
      end;
    8:begin
        if CheckBox4.Checked then UpdateWordListStats;
        if CheckBox5.Checked then SaveWordList;
        result:=9;
      end;
  end;
end;

function TfWordList.LLGoPrev(page:integer):integer;
begin
  result:=page;
  case page of
    1:result:=0;
    2:result:=1;
    3:result:=2;
    4:begin
        if Application.MessageBox(
          pchar(_l('#00865^eGenerated list will be lost. Do you want to continue?')),
          pchar(_l('#00573^eWarning')),
          MB_YESNO or MB_ICONWARNING)=idYes then result:=2;
      end;
    5:result:=4;
    6,7:begin
        if Application.MessageBox(
          pchar(_l('#00866^eCurrent test data will be lost. Do you want to continue?')),
          pchar(_l('#00573^eWarning')),
          MB_YESNO or MB_ICONWARNING)=idYes then result:=5;
      end;
    8:result:=4;
    9:result:=0;
  end;
end;

function DateOld(s:string;threshold:integer):integer;
var d:TDateTime;
begin
  if s='00000000' then
    Result:=threshold
  else begin
    d:=EncodeDate(strtoint(copy(s,1,4)),strtoint(copy(s,5,2)),strtoint(copy(s,7,2)));
    if trunc(now-d)<threshold then
      Result:=trunc(now-d)
    else
      Result:=threshold;
  end;
end;

procedure TfWordList.BuildWordList;
var il,sl:TStringList;
    i,j,k,l,m,n,o:integer;
    ca:array[1..4] of double;
    ba:array[1..4] of double;
    pa:array[1..4] of integer;
    wa:array[1..4] of double;
    da:array[1..4,1..4] of integer;
    qa:array[1..4] of integer;
    ia:array[1..4] of double;
    sa:array[1..4] of double;
    masc,proc:integer;
    wnum:integer;
    best,bestw:integer;
    cur:double;
    curi:integer;
    rand:integer;
    coef:double;
    s:string;

  sp: TSMPromptForm;

  procedure SetPaQa(q:integer;w:integer);
  begin
    case w of
      0:begin pa[q]:=-2; qa[q]:=2; end;
      1:begin pa[q]:=-1; qa[q]:=1; end;
      2:begin pa[q]:=1; qa[q]:=1; end;
      3:begin pa[q]:=2; qa[q]:=2; end;
      4:begin pa[q]:=0; qa[q]:=1; end;
      5:begin pa[q]:=0; qa[q]:=0; end;
    end;
  end;

begin
  sp:=SMProgressDlg(
    _l('#00860^eBuilding learning list...'),
    _l('#00685^ePlease wait...'),
    100); //for now we don't track progress, so set max to 100

  Label52.Caption:='';
  ll.Clear;
  Screen.Cursor:=crHourGlass;
  il:=TStringList.Create;
  sl:=TStringList.Create;
  randomize;
  if RadioGroup1.ItemIndex=0 then
    il.Assign(fVocab.wl) else
  begin
    TUser.SetOrder('Index_Ind');
    TUser.First;
    while not TUser.EOF do
    begin
      il.Add(TUser.Str(TUserIndex));
      TUser.Next;
    end;
  end;
  if RadioGroup2.ItemIndex=1 then
  begin
    for i:=1 to strtoint(Edit3.Text) do
    begin
      j:=random(il.Count);
      sl.Add(il[j]);
      il.Delete(j);
    end;
  end else if RadioGroup2.ItemIndex=2 then
    sl.Assign(il) else
  begin
    SetPaQa(1,RadioGroup3.ItemIndex);
    SetPaQa(2,RadioGroup4.ItemIndex);
    SetPaQa(3,RadioGroup5.ItemIndex);
    SetPaQa(4,RadioGroup6.ItemIndex);
    for i:=1 to 4 do wa[i]:=qa[i]*2;
    for i:=1 to 4 do ia[i]:=0;
    for i:=1 to 4 do sa[i]:=pa[i]*250;
    for i:=1 to 4 do ia[i]:=sa[i];
    masc:=0; proc:=0;
    for i:=1 to 4 do for j:=1 to 4 do da[i,j]:=0;
    wnum:=strtoint(Edit3.Text);
    for i:=1 to wnum do
    begin
      if i mod 10=0 then
      begin
        sp.SetProgress(trunc(i/wnum*100));
        sp.ProcessMessages;
      end;
      bestw:=0;
      best:=100001;
      for j:=0 to il.Count-1 do
      begin
        TUser.Locate('Index',strtoint(il[j]));
        if TUser.Int(TUserScore)<=1 then ca[1]:=-200 else ca[1]:=600;
        ca[1]:=ca[1]-DateOld(TUser.Str(TUserLearned),200)-DateOld(TUser.Str(TUserMastered),200);
        ca[2]:=800-DateOld(TUser.Str(TUserAdded),500)*3;
        if pos('<spop>',TUser.Str(TUserEnglish))>0 then ca[3]:=-500 else ca[3]:=0; //TODO: Note <spop> -- to be fixed when we upgrade dict format
        ca[3]:=ca[3]-length(TUser.Str(TUserPhonetic))*10;
        if FirstUnknownKanjiIndex(TUser.Str(TUserKanji))>=0 then ca[3]:=ca[3]+800;
        if ca[3]>900 then ca[3]:=900;
        if ca[3]<-900 then ca[3]:=-900;
        if TUser.Int(TUserNoPrinted)<=10 then ca[4]:=-200+TUser.Int(TUserNoPrinted)*50 else
          ca[4]:=200+TUser.Int(TUserNoPrinted)*10;
        ca[4]:=ca[4]-DateOld(TUser.Str(TUserPrinted),100)*10;
        if ca[4]>900 then ca[4]:=900;
        if ca[4]<-900 then ca[4]:=-900;
        cur:=0;
        for k:=1 to 4 do cur:=cur+abs(wa[k]*(ca[k]-ia[k]));
        if (TUser.Int(TUserScore)=0) and (proc<wnum div 3) and (CheckBox2.Checked) then cur:=cur-25000;
        if (TUser.Int(TUserScore)=3) and (masc>wnum div 20) and (CheckBox3.Checked) then cur:=cur+25000;
        rand:=0;
        for k:=1 to 100 do rand:=rand+random(500);
        cur:=cur+rand;
        if cur>100000 then cur:=100000;
        if cur<1 then cur:=1;
        curi:=trunc(cur);
        if curi<=best then
        begin
          bestw:=strtoint(il[j]);
          for k:=1 to 4 do ba[k]:=ca[k];
          best:=curi;
        end;
      end;
      if best<=0 then showmessage('error');
      if best>0 then
      begin
        sl.Add(inttostr(bestw));
        il.Delete(il.IndexOf(inttostr(bestw)));
        TUser.Locate('Index',bestw);
        if TUser.Int(TUserScore)=3 then inc(masc);
        if TUser.Int(TUserScore)=0 then inc(proc);
        for k:=1 to 4 do
        begin
          if ba[k]<-500 then inc(da[1,k]) else
          if ba[k]<0 then inc(da[2,k]) else
          if ba[k]<500 then inc(da[3,k]) else inc(da[4,k]);
          case k of
            1:coef:=2*qa[1];
            2:coef:=1*qa[2];
            3:coef:=1*qa[3];
            4:coef:=1.5*qa[4];
          end;
{          m:=pa[k];
          if (ba[k]-ia[k])>0 then m:=-m;
          case m of
            -2:coef:=coef/5;
            -1:coef:=coef/2;
            0:coef:=coef;
            1:coef:=coef*2;
            2:coef:=coef*5;
          end;}
          ia[k]:=ia[k]-((ba[k]-sa[k])/20)*coef;
          if ia[k]<-900 then ia[k]:=-900;
          if ia[k]>900 then ia[k]:=900;
          wa[k]:=(wa[k]+(coef*abs(ba[k]-sa[k])/500))/2;
        end;
      end;
    end;
    s:='DEBUG LEARNING LIST STATS:'#13;
    for i:=1 to 4 do
    begin
      s:=s+#13+inttostr(i)+': -';
      for j:=1 to 4 do
        s:=s+inttostr(da[j,i])+'-';
      s:=s+' I:'+floattostrf(ia[i],ffNumber,7,2)+' W:'+floattostrf(wa[i],ffNumber,7,2);
    end;
    Label52.Caption:=s;
  end;
  if not CheckBox1.Checked then ll.Assign(sl) else
  for i:=0 to sl.Count-1 do
  begin
    j:=random(sl.Count);
    ll.Add(sl[j]);
    sl.Delete(j);
  end;
  il.Free;
  sl.Free;
  i:=0; j:=0; k:=0; l:=0; m:=0; n:=0;
  for o:=0 to ll.Count-1 do
  begin
    TUser.Locate('Index',strtoint(ll[o]));
    if TUser.Int(TUserScore)=0 then inc(i);
    if TUser.Int(TUserScore)>=2 then inc(j);
    if TUser.Int(TUserScore)=3 then inc(k);
    if pos('<spop>',TUser.Str(TUserEnglish))=0 then inc(l);
    if (length(TUser.Str(TUserPhonetic))>1) and (TUser.Str(TUserPhonetic)[3]>='A') then inc(m);
    if FirstUnknownKanjiIndex(TUser.Str(TUserKanji))<0then inc(n);
    TUser.Next;
  end;
  Label26.Caption:=perc(j,ll.Count);
  Label27.Caption:=perc(k,ll.Count);
  Label28.Caption:=perc(i,ll.Count);
  Label35.Caption:=perc(l,ll.Count);
  Label29.Caption:=perc(m,ll.Count);
  Label44.Caption:=perc(n,ll.Count);
  RadioGroup7.ItemIndex:=0;
  RadioGroup7.Enabled:=true;
  Screen.Cursor:=crDefault;
  sp.Free;
end;

//Takes present KnownState (0..3) and flashcard answer (0..2) and returns
//new KnownState after this answer
function StateNew(state, ans:integer):integer;
begin
  case state of
    0:if ans=0 then Result:=0 else if ans=1 then Result:=1 else Result:=2;
    1:if ans=0 then Result:=1 else if ans=1 then Result:=2 else Result:=3;
    2:if ans=0 then Result:=0 else if ans=1 then Result:=2 else Result:=3;
    3:if ans=0 then Result:=0 else if ans=1 then Result:=1 else Result:=3;
  else Result := 0; //should not happen
  end;
end;

procedure TfWordList.PrepareTestWord;
var i:integer;
    s:string;
    sl:TStringList;
begin
  TUser.Locate('Index',strtoint(ll[twi]));
  twkanji:=CheckKnownKanji(TUser.Str(TUserKanji));
  twphonetic:=TUser.Str(TUserPhonetic);
  twmeaning:=remmark(TUser.Str(TUserEnglish));
  Gauge1.MaxValue:=ll.Count;
  Gauge1.Progress:=twi;
  Label38.Caption:=DateForm(TUser.Str(TUserAdded));
  Label39.Caption:=DateForm(TUser.Str(TUserLearned));
  Label40.Caption:=DateForm(TUser.Str(TUserMastered));
  Label41.Caption:=DateForm(TUser.Str(TUserPrinted));
  if Label41.Caption<>'-'then Label41.Caption:=Label41.Caption+' ('+TUser.Str(TUserNoPrinted)+'x)';
  RxLabel9.Caption:=StateStr(TUser.Int(TUserScore));
  RxLabel10.Caption:=StateStr(StateNew(TUser.Int(TUserScore),0));
  RxLabel11.Caption:=StateStr(StateNew(TUser.Int(TUserScore),1));
  RxLabel12.Caption:=StateStr(StateNew(TUser.Int(TUserScore),2));
  sl:=TStringList.Create;
  sl.Clear;
  ListWordCategories(strtoint(ll[twi]),sl);
  for i:=0 to sl.Count-1 do if s='' then s:=sl[i] else s:=s+', '+sl[i];
  Label55.Caption:=s;
  sl.Free;
  Label24.Visible:=false;
  Label25.Visible:=false;
  Label31.Visible:=false;
  Label30.Visible:=false;
  Label32.Visible:=false;
  Label36.Visible:=false;
  Label37.Visible:=false;
  Label34.Visible:=false;
  Label38.Visible:=false;
  Label39.Visible:=false;
  Label40.Visible:=false;
  Label41.Visible:=false;
  Label43.Visible:=false;
  Label45.Visible:=false;
  Label54.Visible:=false;
  Label55.Visible:=false;
  RxLabel9.Visible:=false;
  RxLabel10.Visible:=false;
  RxLabel11.Visible:=false;
  RxLabel12.Visible:=false;
  Button7.Visible:=false;
  Button8.Visible:=false;
  Button9.Visible:=false;
  Shape1.Visible:=false;
  Shape2.Visible:=false;
  Shape5.Visible:=false;
  PaintBox1.Visible:=false;
  PaintBox2.Visible:=false;
  PaintBox6.Visible:=false;
  Button10.Visible:=true;
  i:=0;
  case RadioGroup8.ItemIndex of
    0:i:=1;
    1:i:=2;
    2:i:=3;
    3:i:=4;
    4:i:=5;
    5:i:=6;
    6:if random(100)<50 then i:=1 else i:=2;
    7:if random(100)<66 then i:=1 else i:=2;
    8:if random(100)<33 then i:=1 else if random(66)<33 then i:=2 else i:=3;
    9:if random(100)<25 then i:=1 else if random(75)<25 then i:=2 else i:=3;
    10:if random(100)<50 then i:=1 else if random(50)<25 then i:=2 else i:=3;
  end;
  if (i=2) and (CheckBox6.Checked) and (FirstUnknownKanjiIndex(TUser.Str(TUserKanji))>=0) then i:=1;
  if i=1 then
  begin
    PaintBox1.Visible:=true;
    Shape2.Visible:=true;
    Label24.Visible:=true;
  end;
  if i=2 then
  begin
    PaintBox6.Visible:=true;
    Shape5.Visible:=true;
    Label25.Visible:=true;
  end;
  if i=3 then
  begin
    PaintBox2.Visible:=true;
    Shape1.Visible:=true;
    Label31.Visible:=true;
  end;
  if i=4 then
  begin
    PaintBox1.Visible:=true;
    Shape2.Visible:=true;
    Label24.Visible:=true;
    PaintBox6.Visible:=true;
    Shape5.Visible:=true;
    Label25.Visible:=true;
  end;
  if i=5 then
  begin
    PaintBox1.Visible:=true;
    Shape2.Visible:=true;
    Label24.Visible:=true;
    PaintBox2.Visible:=true;
    Shape1.Visible:=true;
    Label31.Visible:=true;
  end;
  if i=6 then
  begin
    PaintBox6.Visible:=true;
    Shape5.Visible:=true;
    Label25.Visible:=true;
    PaintBox2.Visible:=true;
    Shape1.Visible:=true;
    Label31.Visible:=true;
  end;
end;

procedure TfWordList.ShowTestWord;
begin
  Label24.Visible:=true;
  Label25.Visible:=true;
  Label31.Visible:=true;
  Label30.Visible:=true;
  Label32.Visible:=true;
  Label36.Visible:=true;
  Label37.Visible:=true;
  Label34.Visible:=true;
  Label38.Visible:=true;
  Label39.Visible:=true;
  Label40.Visible:=true;
  Label41.Visible:=true;
  Label43.Visible:=true;
  Label45.Visible:=true;
  Label54.Visible:=true;
  Label55.Visible:=true;
  RxLabel9.Visible:=true;
  RxLabel10.Visible:=true;
  RxLabel11.Visible:=true;
  RxLabel12.Visible:=true;
  Button7.Visible:=true;
  Button8.Visible:=true;
  Button9.Visible:=true;
  Shape1.Visible:=true;
  Shape2.Visible:=true;
  Shape5.Visible:=true;
  PaintBox1.Visible:=true;
  PaintBox2.Visible:=true;
  PaintBox6.Visible:=true;
  Button10.Visible:=false;
end;

function TfWordList.FinishTestWord(button:integer):boolean;
begin
  ltl.Add(inttostr(button));
  inc(twchg[TUser.Int(TUserScore),StateNew(TUser.Int(TUserScore),button)]);
  inc(twi);
  if twi<ll.Count then
  begin
    result:=true;
    exit;
  end;
  result:=false;
  Label53.Caption:=perc(twchg[0,0]+twchg[1,1]+twchg[2,2]+twchg[3,3],ll.Count);
  Label11.Caption:=perc(twchg[2,0],ll.Count);
  Label12.Caption:=perc(twchg[3,0],ll.Count);
  Label13.Caption:=perc(twchg[3,1],ll.Count);
  Label46.Caption:=perc(twchg[0,1],ll.Count);
  Label47.Caption:=perc(twchg[0,2],ll.Count);
  Label48.Caption:=perc(twchg[1,2],ll.Count);
  Label49.Caption:=perc(twchg[1,3],ll.Count);
  Label50.Caption:=perc(twchg[2,3],ll.Count);
end;

procedure TfWordList.SaveTestResults;
var i,n:integer;
    ms:integer;
begin
  Screen.Cursor:=crHourGlass;
  for i:=0 to ll.Count-1 do
  begin
    TUser.Locate('Index',strtoint(ll[i]));
    n:=StateNew(TUser.Int(TUserScore),strtoint(ltl[i]));
    ms:=n;
    if TUser.Int(TUserMaxScore)>ms then ms:=TUser.Int(TUserMaxScore);
    case n of
      0,1:TUser.Edit([TUserScore,TUserMaxScore],[inttostr(n),inttostr(ms)]);
      2:TUser.Edit([TUserScore,TUserMaxScore,TUserLearned],['2',inttostr(ms),FormatDateTime('yyyymmdd',now)]);
      3:TUser.Edit([TUserScore,TUserMaxScore,TUserMastered],['3',inttostr(ms),FormatDateTime('yyyymmdd',now)]);
    end;
  end;
  Screen.Cursor:=crDefault;
  fMenu.ChangeUserData;
end;

procedure TfWordList.PrintWordList;
var bk:TStringList;
  i:integer;
begin
 { As this module is lazy, it prints through fVocab facilities, temporarily
  replacing it with its list. Ugh. }
  bk:=TStringList.Create;
  bk.Assign(fVocab.wl);
  fVocab.wl.Clear;
  for i:=0 to ll.Count-1 do
  begin
    TUser.Locate('Index',strtoint(ll[i]));
    if (RadioGroup10.ItemIndex=0) or
       ((RadioGroup10.ItemIndex=1) and (TUser.Int(TUserScore)<3)) or
       (TUser.Int(TUserScore)<2) then
      fVocab.wl.Add(ll[i]);
  end;
  fVocab.PrintList(_l('#00861^eLearning list'));
  fVocab.wl.Clear;
  fVocab.wl.Assign(bk);
  bk.Free;
end;

procedure TfWordList.SaveWordList;
var i:integer;
  catidx:integer;
begin
  TUserCat.Insert(['0',Edit1.Text,inttostr(ord('W')),FormatDateTime('yyyymmdd',now)]);
  catidx:=TUserCat.TrueInt(TUserCatIndex);
  for i:=0 to ll.Count-1 do
  begin
    TUser.Locate('Index',strtoint(ll[i]));
    if (RadioGroup10.ItemIndex=0) or
       ((RadioGroup10.ItemIndex=1) and (TUser.Int(TUserScore)<3)) or
       (TUser.Int(TUserScore)<2) then
      TUserSheet.Insert([ll[i],IntToStr(catidx),inttostr(i)]);
  end;
  fMenu.ChangeUserData;
  fMenu.RefreshCategory;
  fVocab.SetCategoryFilter(Edit1.Text);
end;

procedure TfWordList.UpdateWordListStats;
var i,j:integer;
begin
  for i:=0 to ll.Count-1 do
  begin
    TUser.Locate('Index',strtoint(ll[i]));
    j:=TUser.Int(TUserNoPrinted);
    TUser.Edit([TUserNoPrinted,TUserPrinted],[inttostr(j+1),FormatDateTime('yyyymmdd',now)]);
  end;
  fMenu.ChangeUserData;
end;


end.
