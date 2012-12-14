unit JWBUser;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  rxPlacemnt, StdCtrls, ExtCtrls, ComCtrls, Grids, RXCtrls, Buttons,
  JWBUtils, JWBDicSearch;

type
  TLookSettings = record
    dgroup:integer;
    full: boolean;
    maxwords: integer;
    clips:string;
    mt: TMatchType;
  end;
  PLookSettings = ^TLookSettings;

  TfUser = class(TForm)
    Panel1: TPanel;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    SpeedButton3: TSpeedButton;
    Shape11: TShape;
    Label16: TLabel;
    SpeedButton5: TSpeedButton;
    SpeedButton6: TSpeedButton;
    SpeedButton7: TSpeedButton;
    SpeedButton8: TSpeedButton;
    SpeedButton9: TSpeedButton;
    SpeedButton10: TSpeedButton;
    SpeedButton11: TSpeedButton;
    SpeedButton12: TSpeedButton;
    SpeedButton23: TSpeedButton;
    Edit1: TEdit;
    StringGrid1: TStringGrid;
    BitBtn1: TBitBtn;
    Panel2: TPanel;
    Panel3: TPanel;
    Label1: TLabel;
    SpeedButton4: TSpeedButton;
    SpeedButton13: TSpeedButton;
    SpeedButton14: TSpeedButton;
    SpeedButton15: TSpeedButton;
    SpeedButton16: TSpeedButton;
    Label2: TLabel;
    SpeedButton17: TSpeedButton;
    Label3: TLabel;
    SpeedButton18: TSpeedButton;
    SpeedButton19: TSpeedButton;
    procedure Edit1Change(Sender: TObject);
    procedure StringGrid1DrawCell(Sender: TObject; ACol, ARow: Integer;
      Rect: TRect; State: TGridDrawState);
    procedure Edit2Change(Sender: TObject);
    procedure Edit2Click(Sender: TObject);
    procedure Edit1Click(Sender: TObject);
    procedure StringGrid1SelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
    procedure WordDetails_PaintBox1Paint(Sender: TObject);
    procedure WordDetails_PaintBox2Paint(Sender: TObject);
    procedure WordDetails_SpeedButton23Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure WordDetails_PaintBox5Paint(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
    procedure SpeedButton8Click(Sender: TObject);
    procedure SpeedButton5Click(Sender: TObject);
    procedure SpeedButton6Click(Sender: TObject);
    procedure SpeedButton7Click(Sender: TObject);
    procedure SpeedButton9Click(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure SpeedButton23Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure StringGrid1DblClick(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure StringGrid1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure SpeedButton17Click(Sender: TObject);
    procedure StringGrid1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure StringGrid1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure SpeedButton10Click(Sender: TObject);
    procedure SpeedButton19Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);

  public
    procedure WordKanji_PaintBoxKNPaint(pb: TPaintBox; KN: integer);

  public
    curkanji,curphonetic,curmeaning,curkanjid:string;
    procedure ShowWord;
    procedure ShowHint;
    procedure HideHint;
    procedure PaintHint;

    procedure DetailsForKanji(n:integer);

  public
    ul:TStringList;
    dicsl:TStringList;
    procedure Look_Setup(a: integer; out st: TLookSettings);
    procedure Look_Run(a: integer; st: PLookSettings; nogriddisplay:boolean; NoScreenUpdates: boolean = false);
    procedure Look(nogriddisplay:boolean; NoScreenUpdates: boolean = false);

  end;

var
  fUser: TfUser;

implementation

uses JWBStrings, JWBUnit, JWBMenu, JWBWords, JWBSettings, JWBStatistics,
  JWBPrint, JWBTranslate, JWBWordDetails, JWBWordKanji, JWBExamples,
  JWBWordCategory, JWBHint, JWBKanjiDetails, JWBKanji, StdPrompt, JWBDicAdd, Math,
  JWBCategories;

var curword:integer;
    hinthide:boolean;
    donotsetbegset:boolean;

{$R *.DFM}


{
Look()
This function translates a chunk of currently selected text either in the editor
or in the word lookup edit field.
When translating from the editor, it's called thousands of times, and therefore
we split the setup part and the work part.
}

procedure TfUser.Look_Setup(a: integer; out st: TLookSettings);
var i: integer;
begin
  donotsetbegset:=true;
  if SpeedButton1.Down then dictmodeset:=0;
  if SpeedButton2.Down then dictmodeset:=1;
  if SpeedButton3.Down then dictmodeset:=2;
  if not((SpeedButton1.Down) or (SpeedButton2.Down)) then
  begin
    Edit1.enabled:=false;
    Edit1.Color:=clMenu;
  end else begin
    Edit1.enabled:=true;
    Edit1.Color:=clWindow;
  end;
  fMenu.aDictExact.Checked:=SpeedButton10.Down;
  fMenu.aDictBeginning.Checked:=SpeedButton11.Down;
  fMenu.aDictEnd.Checked:=SpeedButton12.Down;
  fMenu.aDictMiddle.Checked:=SpeedButton18.Down;
  fMenu.aDictBeginning.Enabled:=SpeedButton11.Enabled;
  fMenu.aDictEnd.Enabled:=SpeedButton12.Enabled;
  fMenu.aDictMiddle.Enabled:=SpeedButton18.Enabled;
  fMenu.aDictInflect.Checked:=SpeedButton4.Down;
  fMenu.aDictAuto.Checked:=SpeedButton13.Down;
  fMenu.aDictGroup1.Checked:=SpeedButton14.Down;
  fMenu.aDictGroup2.Checked:=SpeedButton15.Down;
  fMenu.aDictGroup3.Checked:=SpeedButton16.Down;

 //Dictionary group
  if SpeedButton14.Down then st.dgroup:=1 else
  if SpeedButton15.Down then st.dgroup:=2 else
  if SpeedButton16.Down then st.dgroup:=3 else
    st.dgroup := 1; //we must have some group chosen

  SpeedButton11.Enabled:=true;
  SpeedButton12.Enabled:=true;
  SpeedButton18.Enabled:=true;
  case dictbeginset of
    0:fUser.SpeedButton10.Down:=true;
    1:fUser.SpeedButton11.Down:=true;
    2:fUser.SpeedButton12.Down:=true;
    3:fUser.SpeedButton18.Down:=true;
  end;
  Label1.Caption:='';

  if (not SpeedButton13.Down) or (SpeedButton18.Down) then
    BitBtn1.Caption:=_l('#00669^eSearch')
  else
    BitBtn1.Caption:=_l('#00670^eAll');

  if (a<4) and (BitBtn1.Enabled) and ((not SpeedButton13.Down) or (SpeedButton18.Down)) then
  begin
    BitBtn1.Visible:=true;
    Label2.Visible:=false;
    StringGrid1.Visible:=false;
    Label16.Visible:=(edit1.text<>'') or (a=4);
    curword:=0;
    donotsetbegset:=false;
    ShowWord;
    exit;
  end;
  if ((a=2) or (a=4)) then
  begin
    if SpeedButton12.Down then SpeedButton10.Down:=true;
    if SpeedButton18.Down then SpeedButton10.Down:=true;
    SpeedButton12.Enabled:=false;
    SpeedButton18.Enabled:=false;
  end;
  if a=4 then
  begin
    if SpeedButton11.Down then SpeedButton10.Down:=true;
    SpeedButton11.Enabled:=false;
  end;
  donotsetbegset:=false;

  st.full:=not BitBtn1.Enabled;
//  if SpeedButton10.Down then full:=true;

  StringGrid1.RowCount:=200;
  st.maxwords:=StringGrid1.VisibleRowCount;

  if a=3 then
  begin
    st.clips:='';
    for i:=1 to length(clip) div 4 do
      if copy(clip,i*4-3,2)='00'then break else st.clips:=st.clips+copy(clip,i*4-3,4);
  end;
  if SpeedButton11.Down then st.mt := mtMatchLeft else
  if SpeedButton12.Down then st.mt := mtMatchRight else
  if SpeedButton18.Down then st.mt := mtMatchAnywhere else
    st.mt := mtExactMatch;
  if (a=2) and not (st.mt in [mtExactMatch, mtMatchLeft]) then
    st.mt := mtExactMatch;

  if (not fSettings.CheckBox12.Checked) or (SpeedButton10.Down) then st.full:=true;
end;


{
NoScreenUpdates: Do not update anything on the screen.
  Mainly, don't do fKanji.SetCharDetails, because we're translating a large block of text.
  I don't clearly understand why it's there, but surely it's not needed when translating.
}
procedure TfUser.Look_Run(a: integer; st: PLookSettings; nogriddisplay:boolean; NoScreenUpdates: boolean = false);
var wt:integer;
    i:integer;
    wasfull:boolean;
    s:string;
    b:boolean;
begin
  dicsl.Clear;

  case a of
    1: DicSearch(Edit1.Text,1,st.mt,st.full,-1,st.maxwords,dicsl,st.dgroup,wasfull);
    2: DicSearch(Edit1.Text,2,st.mt,st.full,-1,st.maxwords,dicsl,st.dgroup,wasfull);
    3: DicSearch(st.clips,3,st.mt,st.full,-1,st.maxwords,dicsl,st.dgroup,wasfull);
    4: //In "word insert" mode
       if fTranslate.insx<>-1 then
       begin
         if fTranslate.buffertype='H'then
           DicSearch(fTranslate.GetInsertKana(false),4,mtExactMatch,st.full,-1,st.maxwords,dicsl,5,wasfull) else
           DicSearch(fTranslate.GetInsertKana(false),4,mtExactMatch,st.full,-2,st.maxwords,dicsl,5,wasfull);
       end else
       //In "translate text" mode
       begin
         s:=fTranslate.GetDocWord(fTranslate.rcurx,fTranslate.rcury,wt,nogriddisplay);
         if (not NoScreenUpdates) and (length(s)>=4) then fKanjiDetails.SetCharDetails(copy(s,1,4));
         DicSearch(s,4,mtMatchLeft,st.full,wt,st.maxwords,dicsl,5,wasfull);
       end;
  end;
  
  ul.Clear;
 
  if a<>4 then
    if dicsl.Count=0 then
      Label3.Caption:='-'
    else
      if not wasfull then
        Label3.Caption:=inttostr(dicsl.Count)+'+'
      else
        Label3.Caption:=inttostr(dicsl.Count);

  for i:=0 to dicsl.Count-1 do
    ul.Add(copy(dicsl[i],6,25));
  
  if not nogriddisplay then
  begin
    for i:=0 to dicsl.Count-1 do if (not st.full) and (i>=StringGrid1.VisibleRowCount) then
      dicsl.Delete(StringGrid1.VisibleRowCount)
    else
      dicsl[i]:=copy(dicsl[i],31,length(dicsl[i])-30);
    FillWordGrid(StringGrid1,dicsl,false,false);
    if not wasfull then
      s:=_l('#00671^eSearch results (partial)')
    else
      s:=_l('#00672^eSearch results');
    BitBtn1.Visible:=not wasfull or (st.full and not BitBtn1.Enabled);
    Label2.Visible:=not BitBtn1.Visible;
    s:=s+' ';
    case a of
      1:s:=s+_l('#00673^eby phonetic');
      2:s:=s+_l('#00674^eby meaning');
      3:s:=s+_l('#00675^eby written (clipboard)');
      4:s:=s+_l('#00676^eby written (text)');
    end;
    s:=s+' ('+inttostr(dicsl.Count)+')';
    curword:=0;
    if StringGrid1.Visible then StringGrid1SelectCell(self,0,1,b);
    if StringGrid1.Visible then StringGrid1.Row:=1;
    if StringGrid1.Visible then curword:=1;
    ShowWord;
  end;
end;

{
We keep the complete version for compability.
}

procedure TfUser.Look(nogriddisplay:boolean; NoScreenUpdates: boolean);
var a: integer;
  st: TLookSettings;
begin
  if SpeedButton1.Down then a:=1 else
    if SpeedButton2.Down then a:=2 else
    if SpeedButton3.Down then a:=3 else a:=4;

  Look_Setup(a, st);
  Look_Run(a, @st, nogriddisplay, noscreenupdates);
end;

procedure TfUser.Edit1Change(Sender: TObject);
begin
  BitBtn1.Enabled:=true;
  Look(false);
end;

procedure TfUser.StringGrid1DrawCell(Sender: TObject; ACol, ARow: Integer;
  Rect: TRect; State: TGridDrawState);
begin
  DrawWordCell(StringGrid1,ACol,ARow,Rect,State);
end;

procedure TfUser.Edit2Change(Sender: TObject);
begin
  Look(false);
end;

procedure TfUser.Edit2Click(Sender: TObject);
begin
  Look(false);
end;

procedure TfUser.Edit1Click(Sender: TObject);
begin
  Look(false);
end;

procedure TfUser.StringGrid1SelectCell(Sender: TObject; ACol,
  ARow: Integer; var CanSelect: Boolean);
begin
  curword:=ARow;
  if curword<=ul.Count then ShowWord;
end;

procedure TfUser.WordDetails_PaintBox1Paint(Sender: TObject);
begin
  fDicAdd.PaintBox1.Canvas.Brush.Color:=clWindow;
  if showroma then
    DrawUnicode(fDicAdd.PaintBox1.Canvas,1,1,22,ConvertPinYin(KanaToRomaji(curphonetic,romasys,curlang)),FontEnglish) else
  DrawUnicode(fDicAdd.PaintBox1.Canvas,1,1,22,curphonetic,FontJapanese);
end;

procedure TfUser.WordDetails_PaintBox2Paint(Sender: TObject);
begin
  fDicAdd.PaintBox2.Canvas.Brush.Color:=clWindow;
  DrawUnicode(fDicAdd.PaintBox2.Canvas,1,1,22,curkanji,FontJapanese);
end;

procedure TfUser.WordDetails_SpeedButton23Click(Sender: TObject);
begin
  clip:=clip+curkanji;
  fMenu.ChangeClipboard;
end;

procedure TfUser.FormShow(Sender: TObject);
begin
//  fMenu.ShowForm(SpeedButton5,fMenu.aDictDetails,fWordDetails);
//  fMenu.ShowForm(SpeedButton6,fMenu.aDictKanji,fWordKanji);
//  fMenu.ShowForm(SpeedButton7,fMenu.aDictCategories,fWordCategory);
//  fMenu.ShowForm(SpeedButton9,fMenu.aDictAdd,fExamples);
//  fMenu.ShowForm(SpeedButton8,fMenu.aDictEditor,fTranslate);
  fMenu.aDict.Checked:=true;
  if Edit1.Enabled then Edit1.SetFocus;
  Look(false);
  dic_ignorekana:=false;
end;


procedure TfUser.ShowWord;
var ki:integer;
    s,s2,s3:string;
    radf:integer;
    sl:TStringList;
    i:integer;
begin
  curkanjid:='00000000000000000000000000000000000000000000000000000000000000000000000000000000000000';
  curphonetic:='';
  curkanji:='';
  curmeaning:='';
  fDicAdd.Edit3.Text:='';
  SpeedButton17.Enabled:=false;
  SpeedButton23.Enabled:=false;
  SpeedButton19.Enabled:=false;
  fWordCategory.RxLabel9.Caption:='-';
  fWordCategory.Label55.Caption:='-';
  fWordCategory.Label11.Caption:='-';
  fWordCategory.Label12.Caption:='-';
  fWordCategory.Label13.Caption:='-';
  fWordCategory.Label14.Caption:='-';
  fWordKanji.Label8.Caption:='';
  fWordKanji.Label9.Caption:='';
  fWordKanji.Label10.Caption:='';
  fWordKanji.Label1.Caption:='';
  fWordKanji.Label4.Caption:='';
  fWordKanji.Label5.Caption:='';
  fWordKanji.Label6.Caption:='';
  fWordKanji.Label7.Caption:='';
  fWordKanji.Label11.Caption:='';
  fWordKanji.Shape4.Visible:=false;
  fWordKanji.Shape6.Visible:=false;
  fWordKanji.Shape8.Visible:=false;
  fWordKanji.Shape3.Visible:=false;
  fWordKanji.Shape2.Visible:=false;
  fWordKanji.Shape1.Visible:=false;
  fWordKanji.Shape9.Visible:=false;
  fWordKanji.Shape7.Visible:=false;
  fWordKanji.Shape5.Visible:=false;
  fWordKanji.PaintBoxK1.Visible:=false;
  fWordKanji.PaintBoxK2.Visible:=false;
  fWordKanji.PaintBoxK3.Visible:=false;
  fWordKanji.PaintBoxK4.Visible:=false;
  fWordKanji.PaintBoxK5.Visible:=false;
  fWordKanji.PaintBoxK6.Visible:=false;
  fWordKanji.PaintBoxK7.Visible:=false;
  fWordKanji.PaintBoxK8.Visible:=false;
  fWordKanji.PaintBoxK9.Visible:=false;
  if curword<>0 then
  begin
    curphonetic:=remexcl(copy(StringGrid1.Cells[0,curword],2,length(StringGrid1.Cells[0,curword])-1));
    curkanji:=remexcl(copy(StringGrid1.Cells[1,curword],2,length(StringGrid1.Cells[1,curword])-1));
    curmeaning:=strip_fl(remexcl(StringGrid1.Cells[2,curword]));
    fExamples.SetExamples(curkanji);
    s:=remexcl(StringGrid1.Cells[2,curword]);
    if pos(' >> ',s)>0 then delete(s,1,pos(' >> ',s)+3);
    fDicAdd.Edit3.Text:=s;
    SpeedButton17.Enabled:=true;
    SpeedButton23.Enabled:=true;
    fWordCategory.RxLabel9.Caption:=_l('#00677^eNot in vocabulary');
    ki:=0;
    s:=remexcl(curkanji);
    curkanjid:='';
    while length(s)>3 do
    begin
      s2:=copy(s,1,4);
      delete(s,1,4);
      if TChar.Locate('Unicode',s2,false) then
      begin
        inc(ki);
        radf:=fSettings.ComboBox1.ItemIndex+12;
        if TRadicals.Locate('Number',inttostr(fMenu.GetCharValueRad(TChar.Int(TCharIndex),radf)),true) then
        begin
          curkanjid:=curkanjid+s2+TRadicals.Str(TRadicalsUnicode);
          if TChar.Bool(TCharChinese) then curkanjid:=curkanjid+'J'else
            if IsKnown(KnownLearned,TChar.Fch(TCharUnicode)) then curkanjid:=curkanjid+'K'else
            if TChar.Int(TCharJouyouGrade)<9 then curkanjid:=curkanjid+'C'else
            if TChar.Int(TCharJouyouGrade)<10 then curkanjid:=curkanjid+'N'else
            curkanjid:=curkanjid+'U';
          TCharRead.SetOrder('');
          TCharRead.Locate('Kanji',TChar.Str(TCharIndex),true);
          s3:='';
          while (not TCharRead.EOF) and (TCharRead.Int(TCharReadKanji)=TChar.Int(TCharIndex)) do
          begin
            if (curlang='j') and (TCharRead.Int(TCharReadType)=3) then s3:=s3+', '+TCharRead.Str(TCharReadReading);
            if (curlang='c') and (TCharRead.Int(TCharReadType)=7) then s3:=s3+', '+TCharRead.Str(TCharReadReading);
            TCharRead.Next;
          end;
          delete(s3,1,2);
          case ki of
            1:fWordKanji.Label8.Caption:=s3;
            2:fWordKanji.Label9.Caption:=s3;
            3:fWordKanji.Label10.Caption:=s3;
            4:fWordKanji.Label1.Caption:=s3;
            5:fWordKanji.Label4.Caption:=s3;
            6:fWordKanji.Label5.Caption:=s3;
            7:fWordKanji.Label6.Caption:=s3;
            8:fWordKanji.Label7.Caption:=s3;
            9:fWordKanji.Label11.Caption:=s3;
          end;
        end;
      end;
    end;
    if copy(ul[curword-1],1,6)<>'000000'then
    begin
      SpeedButton19.Enabled:=true;
      TUser.Locate('Index',copy(ul[curword-1],1,6),true);
      fWordCategory.Label11.Caption:=DateForm(TUser.Str(TUserAdded));
      fWordCategory.Label12.Caption:=DateForm(TUser.Str(TUserLearned));
      fWordCategory.Label13.Caption:=DateForm(TUser.Str(TUserMastered));
      fWordCategory.Label14.Caption:=DateForm(TUser.Str(TUserPrinted));
      if fWordCategory.Label13.Caption<>'-'then fWordCategory.Label13.Caption:=fWordCategory.Label13.Caption+' ('+TUser.Str(TUserNoPrinted)+'x)';
      fWordCategory.RxLabel9.Caption:=StateStr(TUser.Int(TUserScore));
      sl:=TStringList.Create;
      sl.Clear;
      ListWordCategories(strtoint(copy(ul[curword-1],1,6)),sl);
      s:='';
      for i:=0 to sl.Count-1 do if s='' then s:=sl[i] else s:=s+', '+sl[i];
      fWordCategory.Label55.Caption:=s;
    end;
    if ki<1 then fWordKanji.Shape4.Visible:=false else fWordKanji.Shape4.Visible:=true;
    if ki<2 then fWordKanji.Shape6.Visible:=false else fWordKanji.Shape6.Visible:=true;
    if ki<3 then fWordKanji.Shape8.Visible:=false else fWordKanji.Shape8.Visible:=true;
    if ki<4 then fWordKanji.Shape3.Visible:=false else fWordKanji.Shape3.Visible:=true;
    if ki<5 then fWordKanji.Shape2.Visible:=false else fWordKanji.Shape2.Visible:=true;
    if ki<6 then fWordKanji.Shape1.Visible:=false else fWordKanji.Shape1.Visible:=true;
    if ki<7 then fWordKanji.Shape9.Visible:=false else fWordKanji.Shape9.Visible:=true;
    if ki<8 then fWordKanji.Shape7.Visible:=false else fWordKanji.Shape7.Visible:=true;
    if ki<9 then fWordKanji.Shape5.Visible:=false else fWordKanji.Shape5.Visible:=true;
    if ki<1 then fWordKanji.PaintBoxK1.Visible:=false else fWordKanji.PaintBoxK1.Visible:=true;
    if ki<2 then fWordKanji.PaintBoxK2.Visible:=false else fWordKanji.PaintBoxK2.Visible:=true;
    if ki<3 then fWordKanji.PaintBoxK3.Visible:=false else fWordKanji.PaintBoxK3.Visible:=true;
    if ki<4 then fWordKanji.PaintBoxK4.Visible:=false else fWordKanji.PaintBoxK4.Visible:=true;
    if ki<5 then fWordKanji.PaintBoxK5.Visible:=false else fWordKanji.PaintBoxK5.Visible:=true;
    if ki<6 then fWordKanji.PaintBoxK6.Visible:=false else fWordKanji.PaintBoxK6.Visible:=true;
    if ki<7 then fWordKanji.PaintBoxK7.Visible:=false else fWordKanji.PaintBoxK7.Visible:=true;
    if ki<8 then fWordKanji.PaintBoxK8.Visible:=false else fWordKanji.PaintBoxK8.Visible:=true;
    if ki<9 then fWordKanji.PaintBoxK9.Visible:=false else fWordKanji.PaintBoxK9.Visible:=true;
  end else fExamples.SetExamples('');
  fWordDetails.PaintBox1.Invalidate;
  fWordDetails.PaintBox2.Invalidate;
  fWordDetails.PaintBox5.Invalidate;
  fWordKanji.PaintBoxK1.Invalidate;
  fWordKanji.PaintBoxK2.Invalidate;
  fWordKanji.PaintBoxK3.Invalidate;
  fWordKanji.PaintBoxK4.Invalidate;
  fWordKanji.PaintBoxK5.Invalidate;
  fWordKanji.PaintBoxK6.Invalidate;
  fWordKanji.PaintBoxK7.Invalidate;
  fWordKanji.PaintBoxK8.Invalidate;
  fWordKanji.PaintBoxK9.Invalidate;
  fMenu.AnnotShowMedia(curkanji,curphonetic);
end;

procedure TfUser.WordDetails_PaintBox5Paint(Sender: TObject);
begin
  fWordDetails.PaintBox5.Canvas.Brush.Color:=clWindow;
  DrawUnicode(fWordDetails.PaintBox5.Canvas,1,1,22,UnicodeToHex(curmeaning),FontEnglish);
end;

//Paints fWordKanji.PaintBoxK1...PaintBoxK9 contents.
//KN: 1..9
procedure TfUser.WordKanji_PaintBoxKNPaint(pb: TPaintBox; KN: integer);
begin
  Assert((KN>=1) and (KN<=9));
  if length(curkanjid)<9*KN then exit;
  BeginDrawReg(pb);
  pb.Canvas.Brush.Color:=Col('Kanji_Back');
  DrawUnicode(pb.Canvas,44,4,16,copy(curkanjid,9*KN-4,4),FontJapaneseGrid);
  case curkanjid[9*KN] of
    'K':pb.Canvas.Font.Color:=Col('Kanji_Learned');
    'C':pb.Canvas.Font.Color:=Col('Kanji_Common');
    'U':pb.Canvas.Font.Color:=Col('Kanji_Rare');
    'N':pb.Canvas.Font.Color:=Col('Kanji_Names');
  end;
  DrawUnicode(pb.Canvas,4,2,36,copy(curkanjid,9*KN-8,4),FontJapaneseGrid);
  EndDrawReg;
end;

procedure TfUser.SpeedButton1Click(Sender: TObject);
begin
  Look(false);
  if Edit1.Enabled then Edit1.SetFocus;
end;

procedure TfUser.BitBtn1Click(Sender: TObject);
begin
  BitBtn1.Enabled:=false;
  Look(false);
end;

procedure TfUser.SpeedButton8Click(Sender: TObject);
begin
  fMenu.ToggleForm(fTranslate,SpeedButton8,fMenu.aDictEditor);
end;

procedure TfUser.SpeedButton5Click(Sender: TObject);
begin
  fMenu.ToggleForm(fWordDetails,SpeedButton5,fMenu.aDictDetails);
end;

procedure TfUser.SpeedButton6Click(Sender: TObject);
begin
  fMenu.ToggleForm(fWordKanji,SpeedButton6,fMenu.aDictKanji);
end;

procedure TfUser.SpeedButton7Click(Sender: TObject);
begin
  fMenu.ToggleForm(fWordCategory,SpeedButton7,fMenu.aDictCategories);
end;

procedure TfUser.SpeedButton9Click(Sender: TObject);
begin
  fMenu.ToggleForm(fExamples,SpeedButton9,fMenu.aDictAdd);
end;

procedure TfUser.FormHide(Sender: TObject);
begin
//  fMenu.HideForm(SpeedButton5,fMenu.aDictDetails,fWordDetails);
//  fMenu.HideForm(SpeedButton6,fMenu.aDictKanji,fWordKanji);
//  fMenu.HideForm(SpeedButton7,fMenu.aDictCategories,fWordCategory);
//  fMenu.HideForm(SpeedButton9,fMenu.aDictAdd,fExamples);
//  fMenu.HideForm(SpeedButton8,fMenu.aDictEditor,fTranslate);
  fMenu.aDict.Checked:=false;
end;

procedure TfUser.SpeedButton23Click(Sender: TObject);
begin
  WordDetails_SpeedButton23Click(sender);
end;

procedure TfUser.FormCreate(Sender: TObject);
begin
  ul:=TStringList.Create;
  dicsl:=TStringList.Create;
end;

procedure TfUser.FormDestroy(Sender: TObject);
begin
  ul.Free;
end;

procedure TfUser.FormResize(Sender: TObject);
begin
  StringGrid1.ColWidths[2]:=StringGrid1.Width-StringGrid1.ColWidths[1]-StringGrid1.ColWidths[0]-20;
end;

procedure TfUser.FormActivate(Sender: TObject);
begin
//  if SpeedButton4.Down then SpeedButton1.Down:=true;
//  SpeedButton1Click(sender);
end;

procedure TfUser.ShowHint;
var l,t:integer;
    p:TPoint;
begin
  if (not fSettings.CheckBox2.Checked) or (not fTranslate.sbKanjiMode.Down) then
  begin
    fHint.Hide;
    exit;
  end;
  hinthide:=true;
  p.x:=0;
  p.y:=4;
  p:=fTranslate.ClientToScreen(p);
  l:=p.x+fTranslate.EditorPaintBox.Left+fTranslate.cursorposcache*fTranslate.lastxsiz;
  t:=p.y+fTranslate.EditorPaintBox.Top+(fTranslate.cursorscreeny+1-fTranslate.view)
    *fTranslate.lastxsiz*fTranslate.lastycnt;
  if l+fHint.Width>Screen.Width then l:=Screen.Width-fHint.Width;
  if t+44>p.y+fTranslate.Height then t:=p.y+fTranslate.EditorPaintBox.Top
    +(fTranslate.Height-fTranslate.CLientHeight)
    +fTranslate.cursorscreeny*fTranslate.lastxsiz*fTranslate.lastycnt-fHint.Height;
  fHint.Left:=l;
  fHint.Top:=t;
  if fSettings.CheckBox13.Checked then fHint.Height:=44 else fHint.Height:=22;
  fHint.Show;
  fHint.Invalidate;
  fTranslate.ListBox1.SetFocus;
  hinthide:=false;
end;

procedure TfUser.HideHint;
begin
  if not hinthide then fHint.Hide;
end;

procedure TfUser.PaintHint;
var kanjis:FString;
  i:integer;
  cw,cwl:integer;
  curk:string;
  fs,fsl:integer;
  rect:TRect;
begin
  fHint.PaintBox1.Canvas.Brush.Color:=Col('Editor_HintBack');
  cw:=-1;
  kanjis:='';
  for i:=1 to StringGrid1.RowCount-1 do
  begin
    if kanjis<>'' then kanjis:=kanjis+UH_IDG_SPACE;
    curk:=remexcl(copy(StringGrid1.Cells[1,i],2,length(StringGrid1.Cells[1,i])-1));
    if StringGrid1.Row=i then
    begin
      cw:=flength(kanjis);
      cwl:=flength(curk);
    end;
    kanjis:=kanjis+curk;
  end;
  fs:=18;
  fsl:=fHint.PaintBox1.Width div fs;
  while flength(kanjis)>fsl do
  begin
    if cw>1 then
    begin
      while fcopy(kanjis,1,1)<>UH_IDG_SPACE do
      begin
        fdelete(kanjis,1,1);
        dec(cw,1);
      end;
      fdelete(kanjis,1,1);
      kanjis:=UH_ELLIPSIS+kanjis;
    end else
    begin
      while fcopy(kanjis,flength(kanjis)-1,1)<>UH_IDG_SPACE do fdelete(kanjis,flength(kanjis)-1,1);
      fdelete(kanjis,flength(kanjis)-1,1);
      kanjis:=kanjis+UH_ELLIPSIS;
    end;
  end;
//  fHint.PaintBox1.Canvas.Font.Style:=[];
  fHint.PaintBox1.Canvas.Font.Color:=Col('Editor_HintText');
  DrawUnicode(fHint.PaintBox1.Canvas,2,2,fs,fcopy(kanjis,1,cw),FontJapaneseGrid);
//  fHint.PaintBox1.Canvas.Font.Style:=[fsBold];
  fHint.PaintBox1.Canvas.Brush.Color:=Col('Editor_HintSelected');
  rect.Left:=2+cw*fs;
  rect.Top:=2;
  rect.Bottom:=fs+2;
  rect.Right:=2+cw*fs+cwl*fs;
  fHint.PaintBox1.Canvas.FillRect(rect);
  DrawUnicode(fHint.PaintBox1.Canvas,2+cw*fs,2,fs,fcopy(kanjis,cw+1,cwl),FontJapaneseGrid);
//  fHint.PaintBox1.Canvas.Font.Style:=[];
  fHint.PaintBox1.Canvas.Brush.Color:=Col('Editor_HintBack');
  DrawUnicode(fHint.PaintBox1.Canvas,2+cw*fs+cwl*fs,2,fs,fcopy(kanjis,cw+cwl+1,flength(kanjis)-cwl-cw),FontJapaneseGrid);
  if fSettings.CheckBox13.Checked then
  begin
    fHint.PaintBox1.Canvas.Font.Name:=FontEnglish;
    fHint.PaintBox1.Canvas.Font.Height:=fs-4;
    rect.top:=fs+2;
    rect.left:=2;
    rect.right:=fHint.PaintBox1.Width-4;
    rect.bottom:=fs*2;
    fHint.PaintBox1.Canvas.TextRect(rect,2,fs+2,strip_fl(remexcl(StringGrid1.Cells[2,curword])));
  end;
end;

procedure TfUser.StringGrid1DblClick(Sender: TObject);
begin
  if SpeedButton17.Enabled then SpeedButton17Click(sender);
end;



procedure TfUser.CheckBox1Click(Sender: TObject);
begin
//  Look(false);
end;

procedure TfUser.StringGrid1MouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  fMenu.IntTipGridOver(StringGrid1,x,y,ssLeft in Shift);
end;

procedure TfUser.DetailsForKanji(n:integer);
begin
  if CharDetDocked then exit;
  fKanjiDetails.SetCharDetails(copy(curkanjid,(n-1)*9+1,4));
  if not fKanjiDetails.Visible then fMenu.aKanjiDetails.Execute else fKanjiDetails.SetFocus;
end;

procedure TfUser.SpeedButton17Click(Sender: TObject);
begin
  if fDicAdd.ShowModal=mrOK then
  begin
    if not fWords.AddWord(curkanji,curphonetic,fDicAdd.edit3.text,fDicAdd.ComboBox1.Text,'?',false,1) then exit;
    Look(false);
    if Edit1.Enabled then Edit1.SetFocus;
  end;
end;

procedure TfUser.StringGrid1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if mbRight=Button then fMenu.PopupImmediate(false);
end;

procedure TfUser.StringGrid1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if mbLeft=Button then fMenu.PopupImmediate(true);
end;

procedure TfUser.SpeedButton10Click(Sender: TObject);
begin
  if not donotsetbegset then
  begin
    if fUser.SpeedButton10.Down then dictbeginset:=0;
    if fUser.SpeedButton11.Down then dictbeginset:=1;
    if fUser.SpeedButton12.Down then dictbeginset:=2;
    if fUser.SpeedButton18.Down then dictbeginset:=3;
  end;
  Look(false);
  if Edit1.Enabled then Edit1.SetFocus;
end;

procedure TfUser.SpeedButton19Click(Sender: TObject);
begin
  fMenu.aMode5Execute(sender);
  if copy(ul[curword-1],1,6)<>'000000' then
    fWords.SearchWord(strtoint(copy(ul[curword-1],1,6)));
end;


end.
