unit JWBKanjiSearch;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Buttons, CheckLst, IniFiles, JWBRadical, WakanPaintbox;

type
  TfKanjiSearch = class(TForm)
    Panel1: TPanel;
    rgSortBy: TRadioGroup;
    btnOnlyCommon: TSpeedButton;
    btnInClipboard: TSpeedButton;
    sbClearFilters: TSpeedButton;
    Panel2: TPanel;
    SpeedButton1: TSpeedButton;
    SpeedButton19: TSpeedButton;
    SpeedButton20: TSpeedButton;
    SpeedButton25: TSpeedButton;
    rgOrAnd: TRadioGroup;
    cbNot: TCheckBox;
    lbCategories: TCheckListBox;
    ScrollBox1: TScrollBox;
    sbDefinition: TSpeedButton;
    sbJouyou: TSpeedButton;
    sbJouyouExpand: TSpeedButton;
    sbJouyouMinus: TSpeedButton;
    sbJouyouPlus: TSpeedButton;
    sbJouyouShrink: TSpeedButton;
    sbListRadicals: TSpeedButton;
    sbOther: TSpeedButton;
    sbPinYin: TSpeedButton;
    sbRadicals: TSpeedButton;
    sbSKIP: TSpeedButton;
    sbStrokeCount: TSpeedButton;
    sbStrokeCountExpand: TSpeedButton;
    sbStrokeCountMinus: TSpeedButton;
    sbStrokeCountPlus: TSpeedButton;
    sbStrokeCountShrink: TSpeedButton;
    sbYomi: TSpeedButton;
    cbOtherType: TComboBox;
    edtDefinition: TEdit;
    edtJouyou: TEdit;
    edtOther: TEdit;
    edtPinYin: TEdit;
    edtSkip: TEdit;
    edtStrokeCount: TEdit;
    edtYomi: TEdit;
    pbRadicals: TWakanPaintbox;
    procedure sbPinYinClick(Sender: TObject);
    procedure sbClearFiltersClick(Sender: TObject);
    procedure edtPinYinChange(Sender: TObject);
    procedure edtYomiChange(Sender: TObject);
    procedure edtDefinitionChange(Sender: TObject);
    procedure edtSkipChange(Sender: TObject);
    procedure edtOtherChange(Sender: TObject);
    procedure edtStrokeCountChange(Sender: TObject);
    procedure edtJouyouChange(Sender: TObject);
    procedure SpeedButton19Click(Sender: TObject);
    procedure sbListRadicalsClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure sbStrokeCountPlusClick(Sender: TObject);
    procedure sbStrokeCountMinusClick(Sender: TObject);
    procedure sbStrokeCountExpandClick(Sender: TObject);
    procedure sbStrokeCountShrinkClick(Sender: TObject);
    procedure sbJouyouPlusClick(Sender: TObject);
    procedure sbJouyouMinusClick(Sender: TObject);
    procedure sbJouyouExpandClick(Sender: TObject);
    procedure sbJouyouShrinkClick(Sender: TObject);
    procedure rgSortByClick(Sender: TObject);
    procedure cbOtherTypeChange(Sender: TObject);
    procedure pbRadicalsPaint(Sender: TObject; Canvas: TCanvas);
    procedure SpeedButton1Click(Sender: TObject);
    procedure lbCategoriesClick(Sender: TObject);
    procedure lbCategoriesClickCheck(Sender: TObject);
    procedure SpeedButton25Click(Sender: TObject);
    procedure SpeedButton20Click(Sender: TObject);
    procedure lbCategoriesDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure rgOrAndClick(Sender: TObject);
    procedure lbCategoriesDblClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);

  public
    procedure SaveSettings(reg: TCustomIniFile);
    procedure LoadSettings(reg: TCustomIniFile);
    procedure LanguageChanged;
    procedure ResetFilters;
    procedure SetCategoryFilter(const ACategories: array of integer;
      AOr, ANot: boolean);

  protected
    FSetOtherTypeIndex: integer; //set when reading settings, applied on next ReloadOtherTypes()
    FSetSortBy: integer;
    procedure SetOtherTypeIndex(const AItemIndex: integer);
    procedure ReloadSortBy;
  public
    procedure ReloadOtherTypes;

  protected
   { Currently selected radical characters }
    FCurRadChars: string;
    procedure SetCurRadChars(const Value: string);
    procedure RadicalSelectionChanged(Sender: TObject);
    function OffsetRange(tx:string;min,max:integer):string;
  public
    CurRadSearchType: TRadSearchType;
    property CurRadChars: string read FCurRadChars write SetCurRadChars;

  end;

var
  fKanjiSearch: TfKanjiSearch;

implementation
uses UITypes, JWBKanji, JWBUnit, JWBCategories, JWBCharData, JWBLanguage;

{$R *.DFM}

procedure TfKanjiSearch.FormCreate(Sender: TObject);
begin
  CurRadSearchType:=stRaine;
  FCurRadChars:='';
end;

procedure TfKanjiSearch.FormShow(Sender: TObject);
begin
  ReloadOtherTypes;
end;

procedure TfKanjiSearch.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  fKanji.btnSearchSort.Down:=false;
  fKanji.aSearch.Checked:=false;
end;

//Resets filters but does not apply it, so that you can chain it with something.
procedure TfKanjiSearch.ResetFilters;
begin
  sbPinYin.Down:=false;
  sbYomi.Down:=false;
  sbDefinition.Down:=false;
  sbOther.Down:=false;
  SpeedButton1.Down:=false;
  btnOnlyCommon.Down:=false;
  btnInClipboard.Down:=false;
  sbStrokeCount.Down:=false;
  sbRadicals.Down:=false;
  sbSKIP.Down:=false;
  sbJouyou.Down:=false;
end;

//Reloads a list of "other" search types
procedure TfKanjiSearch.ReloadOtherTypes;
var i: integer;
  bk: integer;
begin
  bk := fKanjiSearch.cbOtherType.ItemIndex;
  if bk=-1 then begin //this is the first reload
    bk := FSetOtherTypeIndex;
    FSetOtherTypeIndex := -1; //to make it obvious if we mistakengly reuse it
  end;

  fKanjiSearch.cbOtherType.Items.Clear;
  fKanjiSearch.cbOtherType.Items.Add('Unicode');
  for i:=0 to Length(CharPropTypes)-1 do
    if CharPropTypes[i].id>20 then
      fKanjiSearch.cbOtherType.Items.Add(_l('^e'+CharPropTypes[i].englishName));
  fKanjiSearch.cbOtherType.ItemIndex:=0;
  if bk<fKanjiSearch.cbOtherType.Items.Count-1 then fKanjiSearch.cbOtherType.ItemIndex:=bk;
end;

procedure TfKanjiSearch.SetOtherTypeIndex(const AItemIndex: integer);
begin
 //Sometimes this is called when "other search types" are not yet reloaded,
 //so we save the value until then.
  if cbOtherType.Items.Count<=0 then
    FSetOtherTypeIndex := AItemIndex
  else
    fKanjiSearch.cbOtherType.ItemIndex := AItemIndex;
end;

//Called from LanguageChanged because can be different for different target languages
procedure TfKanjiSearch.ReloadSortBy;
begin
  fKanjiSearch.rgSortBy.Items.Clear;
  fKanjiSearch.rgSortBy.Items.Add(_l('#00146^eRadical'));
  fKanjiSearch.rgSortBy.Items.Add(_l('#00147^eStroke count'));
  fKanjiSearch.rgSortBy.Items.Add(_l('#00148^eFrequency'));
  fKanjiSearch.rgSortBy.Items.Add(_l('#00149^eRandom'));
  fKanjiSearch.rgSortBy.Items.Add(_l('#00877^eUnsorted'));
 { There could be additional sorting orders for Japanese, but they're somehow
  disabled in Wakan 1.67+ }
  fKanjiSearch.rgSortBy.ItemIndex := FSetSortBy; //0 by default
  FSetSortBy := 0; //no reuse
end;

//Saves form settings to registry
procedure TfKanjiSearch.SaveSettings(reg: TCustomIniFile);
begin
  reg.WriteBool('KanjiSearch','OnlyCommon',fKanjiSearch.btnOnlyCommon.Down);
//  reg.WriteBool('KanjiSearch','InClipboard',fKanjiSearch.btnInClipboard.Down); //do not save-restore this for now (by design)
  if fKanjiSearch.sbPinYin.Down then
    reg.WriteString('KanjiSearch','PinYin',fKanjiSearch.edtPinYin.Text)
  else
    reg.DeleteKey('KanjiSearch','PinYin');
  if fKanjiSearch.sbYomi.Down then
    reg.WriteString('KanjiSearch','Yomi',fKanjiSearch.edtYomi.Text)
  else
    reg.DeleteKey('KanjiSearch','Yomi');
  if fKanjiSearch.sbDefinition.Down then
    reg.WriteString('KanjiSearch','Definition',fKanjiSearch.edtDefinition.Text)
  else
    reg.DeleteKey('KanjiSearch','Definition');
  if fKanjiSearch.sbStrokeCount.Down then
    reg.WriteString('KanjiSearch','Strokes',fKanjiSearch.edtStrokeCount.Text)
  else
    reg.DeleteKey('KanjiSearch','Strokes');
  if fKanjiSearch.sbRadicals.Down then begin
    reg.WriteInteger('KanjiSearch','RadSearchType',integer(fKanjiSearch.curRadSearchType));
    reg.WriteString('KanjiSearch','RadSearch',fKanjiSearch.curRadChars);
  end else begin
    reg.DeleteKey('KanjiSearch','RadSearchType');
    reg.DeleteKey('KanjiSearch','RadSearch');
    reg.DeleteKey('KanjiSearch','RadIndexes');
  end;
  if fKanjiSearch.sbSKIP.Down then
    reg.WriteString('KanjiSearch','SKIP',fKanjiSearch.edtSKIP.Text)
  else
    reg.DeleteKey('KanjiSearch','SKIP');
  if fKanjiSearch.sbJouyou.Down then
    reg.WriteString('KanjiSearch','Jouyou',fKanjiSearch.edtJouyou.Text)
  else
    reg.DeleteKey('KanjiSearch','Jouyou');
  if fKanjiSearch.sbOther.Down then begin
    reg.WriteInteger('KanjiSearch','OtherCriteriaIndex',fKanjiSearch.cbOtherType.ItemIndex);
    reg.WriteString('KanjiSearch','Other',fKanjiSearch.edtOther.Text);
  end else begin
    reg.DeleteKey('KanjiSearch','OtherCriteriaIndex');
    reg.DeleteKey('KanjiSearch','Other');
  end;
  reg.WriteInteger('Characters','Sort',fKanjiSearch.rgSortBy.ItemIndex);
  reg.WriteInteger('Characters','OtherSearch',fKanjiSearch.cbOtherType.ItemIndex);
end;

//Loads form settings from registry
procedure TfKanjiSearch.LoadSettings(reg: TCustomIniFile);
var AOtherTypeSelected: integer;
begin
  fKanjiSearch.btnOnlyCommon.Down := reg.ReadBool('KanjiSearch','OnlyCommon',false);
//  fKanjiSearch.btnInClipboard.Down := reg.ReadBool('KanjiSearch','InClipboard',false); //do not save-restore this for now (by design)
  fKanjiSearch.edtPinYin.Text := reg.ReadString('KanjiSearch','PinYin','');
  fKanjiSearch.edtYomi.Text := reg.ReadString('KanjiSearch','Yomi','');
  fKanjiSearch.edtDefinition.Text := reg.ReadString('KanjiSearch','Definition','');
  fKanjiSearch.edtStrokeCount.Text := reg.ReadString('KanjiSearch','Strokes','');
  fKanjiSearch.curRadSearchType := TRadSearchType(reg.ReadInteger('KanjiSearch','RadSearchType',0));
  fKanjiSearch.curRadChars := reg.ReadString('KanjiSearch','RadSearch','');
  fKanjiSearch.edtSKIP.Text := reg.ReadString('KanjiSearch','SKIP','');
  fKanjiSearch.edtJouyou.Text := reg.ReadString('KanjiSearch','Jouyou','');
  fKanjiSearch.edtOther.Text := reg.ReadString('KanjiSearch','Other',''); //why did we need this?
  AOtherTypeSelected := reg.ReadInteger('KanjiSearch','OtherCriteriaIndex',-1);
  if AOtherTypeSelected<0 then
    AOtherTypeSelected := reg.ReadInteger('Characters','OtherSearch',0); //backward compability
  SetOtherTypeIndex(AOtherTypeSelected);
  FSetSortBy := reg.ReadInteger('Characters','Sort',0);
end;

//Called by MainForm when selected language (Japanese/Chinese) changes
procedure TfKanjiSearch.LanguageChanged;
begin
  ReloadSortBy;
  fKanji.Reload;
end;

procedure TfKanjiSearch.sbPinYinClick(Sender: TObject);
begin
  fKanji.Reload;
end;

procedure TfKanjiSearch.sbClearFiltersClick(Sender: TObject);
begin
  ResetFilters;
  fKanji.Reload;
end;

procedure TfKanjiSearch.edtPinYinChange(Sender: TObject);
begin
  sbPinYin.Down:=edtPinYin.Text<>'';;
  fKanji.InvalidateList;
end;

procedure TfKanjiSearch.edtYomiChange(Sender: TObject);
begin
  sbYomi.Down:=edtYomi.Text<>'';
  fKanji.InvalidateList;
end;

procedure TfKanjiSearch.edtDefinitionChange(Sender: TObject);
begin
  sbDefinition.Down:=edtDefinition.Text<>'';
  fKanji.InvalidateList;
end;

{ Called when a radical filter changes }
procedure TfKanjiSearch.SetCurRadChars(const Value: string);
begin
  FCurRadChars := Value;
  sbRadicals.Down := FCurRadChars<>'';
  fKanji.InvalidateList;
end;

procedure TfKanjiSearch.edtSkipChange(Sender: TObject);
begin
  sbSKIP.Down:=edtSkip.Text<>'';
  fKanji.InvalidateList;
end;

procedure TfKanjiSearch.edtOtherChange(Sender: TObject);
begin
  sbOther.Down:=edtOther.Text<>'';
  fKanji.InvalidateList;
end;

procedure TfKanjiSearch.edtStrokeCountChange(Sender: TObject);
begin
  sbStrokeCount.Down:=edtStrokeCount.Text<>'';
  fKanji.InvalidateList;
end;

procedure TfKanjiSearch.edtJouyouChange(Sender: TObject);
begin
  sbJouyou.Down:=edtJouyou.Text<>'';
  fKanji.InvalidateList;
end;

procedure TfKanjiSearch.SpeedButton19Click(Sender: TObject);
var i:integer;
begin
  for i:=0 to lbCategories.Items.Count-1 do lbCategories.Checked[i]:=false;
  fKanji.Reload;
end;

procedure TfKanjiSearch.RadicalSelectionChanged(Sender: TObject);
begin
  curRadSearchType := fRadical.SearchType;
  curRadChars := fRadical.SelectedRadicals;
 //SetCurRadChars() will trigger filter update
  pbRadicals.Invalidate;
end;

procedure TfKanjiSearch.sbListRadicalsClick(Sender: TObject);
var
  _radSearchType: TRadSearchType;
  _radChars: string;
begin
 //save current search, it'll be broken by RadicalSelectionChanged
  _radSearchType := curRadSearchType;
  _radChars := FCurRadChars;
 //bring up selection window
  fRadical.SetSelectedRadicals(curRadSearchType, FCurRadChars);
  fRadical.OnSelectionChanged := Self.RadicalSelectionChanged;
  if IsPositiveResult(fRadical.ShowModal) then begin
    _radSearchType := fRadical.SearchType;
    _radChars := fRadical.SelectedRadicals;
  end;
  fRadical.OnSelectionChanged := nil;
 //apply new search, or re-apply old search
  curRadSearchType := _radSearchType;
  curRadChars := _radChars;
 //SetCurRadicals will trigger filter update
  pbRadicals.Invalidate;
end;

//Takes tx in the form of "number" or "number_min-number_max", and adds min
//to number_min and max to number max.
//Returns new value for this filter.
function TfKanjiSearch.OffsetRange(tx:string;min,max:integer):string;
var curtx,txleft,otx:string;
    nn1,nn2:integer;
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

procedure TfKanjiSearch.sbStrokeCountPlusClick(Sender: TObject);
begin
  edtStrokeCount.text:=OffsetRange(edtStrokeCount.text,1,1);
end;

procedure TfKanjiSearch.sbStrokeCountMinusClick(Sender: TObject);
begin
  edtStrokeCount.text:=OffsetRange(edtStrokeCount.text,-1,-1);
end;

procedure TfKanjiSearch.sbStrokeCountExpandClick(Sender: TObject);
begin
  edtStrokeCount.text:=OffsetRange(edtStrokeCount.text,-1,1);
end;

procedure TfKanjiSearch.sbStrokeCountShrinkClick(Sender: TObject);
begin
  edtStrokeCount.text:=OffsetRange(edtStrokeCount.text,1,-1);
end;

procedure TfKanjiSearch.sbJouyouPlusClick(Sender: TObject);
begin
  edtJouyou.text:=OffsetRange(edtJouyou.text,1,1);
end;

procedure TfKanjiSearch.sbJouyouMinusClick(Sender: TObject);
begin
  edtJouyou.text:=OffsetRange(edtJouyou.text,-1,-1);
end;

procedure TfKanjiSearch.sbJouyouExpandClick(Sender: TObject);
begin
  edtJouyou.text:=OffsetRange(edtJouyou.text,-1,1);
end;

procedure TfKanjiSearch.sbJouyouShrinkClick(Sender: TObject);
begin
  edtJouyou.text:=OffsetRange(edtJouyou.text,1,-1);
end;

procedure TfKanjiSearch.rgSortByClick(Sender: TObject);
begin
  fKanji.Reload;
end;

procedure TfKanjiSearch.cbOtherTypeChange(Sender: TObject);
begin
  fKanji.InvalidateList;
end;

procedure TfKanjiSearch.pbRadicalsPaint(Sender: TObject; Canvas: TCanvas);
begin
  Canvas.Brush.Color:=clBtnFace;
  Canvas.Font.Style:=[];
  DrawUnicode(Canvas,1,1,16,FCurRadChars,FontRadical);
end;

procedure TfKanjiSearch.SpeedButton1Click(Sender: TObject);
begin
  NewKanjiCategoryUI();
end;

procedure TfKanjiSearch.lbCategoriesClick(Sender: TObject);
var IsKnownLearned: boolean;
begin
  IsKnownLearned := GetSelCatIdx(lbCategories)=KnownLearned;
  SpeedButton25.Enabled:=not IsKnownLearned;
  SpeedButton20.Enabled:=not IsKnownLearned;
end;

procedure TfKanjiSearch.lbCategoriesClickCheck(Sender: TObject);
begin
  fKanji.Reload;
end;

procedure TfKanjiSearch.SpeedButton25Click(Sender: TObject);
begin
  if lbCategories.ItemIndex=-1 then exit;
  DeleteCategoryUI(GetSelCatIdx(lbCategories));
end;

procedure TfKanjiSearch.SpeedButton20Click(Sender: TObject);
begin
  if lbCategories.ItemIndex=-1 then exit;
  EditCategoryUI(GetSelCatIdx(lbCategories));
end;

procedure TfKanjiSearch.lbCategoriesDrawItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);
var IsKnownLearned: boolean;
begin
  IsKnownLearned := GetCatIdx(lbCategories,index)=KnownLearned;
  if IsKnownLearned then
    lbCategories.Canvas.Font.Style:=[fsBold]
  else
    lbCategories.Canvas.Font.Style:=[];
  lbCategories.Canvas.TextOut(Rect.Left,Rect.Top,lbCategories.Items[index]);
end;

procedure TfKanjiSearch.rgOrAndClick(Sender: TObject);
begin
  fKanji.Reload;
end;

procedure TfKanjiSearch.lbCategoriesDblClick(Sender: TObject);
var i:integer;
begin
  if lbCategories.ItemIndex<>-1 then
  begin
    for i:=0 to lbCategories.Items.Count-1 do
      lbCategories.Checked[i]:=i=lbCategories.ItemIndex;
    fKanji.Reload;
  end;
end;

{ Sets category filter as specified but does not apply it, allowing you to chain
 it with some other changes.
 ACategories is an array of category indexes }
procedure TfKanjiSearch.SetCategoryFilter(const ACategories: array of integer;
  AOr, ANot: boolean);
var i, j, idx: integer;
  found: boolean;
begin
  if AOr then
    rgOrAnd.ItemIndex := 0
  else
    rgOrAnd.ItemIndex := 1;
  cbNot.Checked := ANot;

  for i:=0 to lbCategories.Items.Count-1 do begin
    idx := GetCatIdx(lbCategories, i);
    found := false;
    for j := Low(ACategories) to High(ACategories) do
      if ACategories[j]=idx then begin
        found := true;
        break;
      end;
    lbCategories.Checked[i]:=found;
  end;
end;

end.
