unit JWBKanjiSearch;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Buttons, CheckLst;

type
  TfKanjiSearch = class(TForm)
    sbPinYin: TSpeedButton;
    sbYomi: TSpeedButton;
    sbRadicals: TSpeedButton;
    sbDefinition: TSpeedButton;
    Bevel2: TBevel;
    sbSKIP: TSpeedButton;
    sbClearFilters: TSpeedButton;
    SpeedButton2: TSpeedButton;
    SpeedButton3: TSpeedButton;
    sbStrokeCount: TSpeedButton;
    sbOther: TSpeedButton;
    sbJouyou: TSpeedButton;
    edtPinYin: TEdit;
    edtRadicals: TEdit;
    edtDefinition: TEdit;
    edtSkip: TEdit;
    edtYomi: TEdit;
    edtStrokeCount: TEdit;
    edtOther: TEdit;
    edtJouyou: TEdit;
    sbListRadicals: TSpeedButton;
    Bevel1: TBevel;
    SpeedButton9: TSpeedButton;
    SpeedButton10: TSpeedButton;
    SpeedButton11: TSpeedButton;
    SpeedButton13: TSpeedButton;
    SpeedButton15: TSpeedButton;
    SpeedButton17: TSpeedButton;
    SpeedButton21: TSpeedButton;
    SpeedButton23: TSpeedButton;
    rgSortBy: TRadioGroup;
    Bevel3: TBevel;
    cbOtherType: TComboBox;
    pbRadicals: TPaintBox;
    Shape1: TShape;
    lbCategories: TCheckListBox;
    SpeedButton19: TSpeedButton;
    SpeedButton20: TSpeedButton;
    SpeedButton25: TSpeedButton;
    SpeedButton1: TSpeedButton;
    rgOrAnd: TRadioGroup;
    cbNot: TCheckBox;
    procedure sbPinYinClick(Sender: TObject);
    procedure sbClearFiltersClick(Sender: TObject);
    procedure edtPinYinChange(Sender: TObject);
    procedure edtYomiChange(Sender: TObject);
    procedure edtDefinitionChange(Sender: TObject);
    procedure edtRadicalsChange(Sender: TObject);
    procedure edtSkipChange(Sender: TObject);
    procedure edtOtherChange(Sender: TObject);
    procedure edtStrokeCountChange(Sender: TObject);
    procedure edtJouyouChange(Sender: TObject);
    procedure SpeedButton19Click(Sender: TObject);
    procedure sbListRadicalsClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure SpeedButton10Click(Sender: TObject);
    procedure SpeedButton9Click(Sender: TObject);
    procedure SpeedButton11Click(Sender: TObject);
    procedure SpeedButton13Click(Sender: TObject);
    procedure SpeedButton17Click(Sender: TObject);
    procedure SpeedButton15Click(Sender: TObject);
    procedure SpeedButton21Click(Sender: TObject);
    procedure SpeedButton23Click(Sender: TObject);
    procedure rgSortByClick(Sender: TObject);
    procedure cbOtherTypeChange(Sender: TObject);
    procedure pbRadicalsPaint(Sender: TObject);
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

  protected
    procedure RadicalSelectionChanged(Sender: TObject);
    function OffsetRange(tx:string;min,max:integer):string;

  public
    procedure ReloadOtherTypes;

  end;

var
  fKanjiSearch: TfKanjiSearch;

implementation

uses JWBKanji, JWBSettings, JWBUnit, JWBMenu, JWBRadical, JWBNewCategory,
  JWBKanjiDetails, JWBCategories;

{$R *.DFM}

procedure TfKanjiSearch.FormShow(Sender: TObject);
begin
  ReloadOtherTypes;
end;

procedure TfKanjiSearch.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  fKanji.btnSearchSort.Down:=false;
  fMenu.aKanjiSearch.Checked:=false;
end;

//Reloads a list of "other" search types
procedure TfKanjiSearch.ReloadOtherTypes;
var i:integer;
  bk:integer;
begin
  bk:=fKanjiSearch.cbOtherType.ItemIndex;
  if bk=-1 then bk:=kanji_othersearch;
  fKanjiSearch.cbOtherType.Items.Clear;
  fKanjiSearch.cbOtherType.Items.Add('Unicode');
  for i:=0 to CharPropTypes.Count-1 do
    if strtoint(GetCharPropType(i,0))>20 then
      fKanjiSearch.cbOtherType.Items.Add(_l('^e'+GetCharPropType(i,4)));
  fKanjiSearch.cbOtherType.ItemIndex:=0;
  if bk<fKanjiSearch.cbOtherType.Items.Count-1 then fKanjiSearch.cbOtherType.ItemIndex:=bk;
end;

procedure TfKanjiSearch.sbPinYinClick(Sender: TObject);
begin
  fKanji.DoIt;
end;

procedure TfKanjiSearch.sbClearFiltersClick(Sender: TObject);
begin
  sbPinYin.Down:=false;
  sbYomi.Down:=false;
  sbDefinition.Down:=false;
  sbOther.Down:=false;
  SpeedButton1.Down:=false;
  SpeedButton2.Down:=false;
  SpeedButton3.Down:=false;
  sbStrokeCount.Down:=false;
  sbRadicals.Down:=false;
  sbSKIP.Down:=false;
//  SpeedButton16.Down:=false;
  sbJouyou.Down:=false;
  fKanji.DoIt;
end;

procedure TfKanjiSearch.edtPinYinChange(Sender: TObject);
begin
  sbPinYin.Down:=edtPinYin.Text<>'';;
  fKanji.DoItTimer;
end;

procedure TfKanjiSearch.edtYomiChange(Sender: TObject);
begin
  sbYomi.Down:=edtYomi.Text<>'';
  fKanji.DoItTimer;
end;

procedure TfKanjiSearch.edtDefinitionChange(Sender: TObject);
begin
  sbDefinition.Down:=edtDefinition.Text<>'';
  fKanji.DoItTimer;
end;

procedure TfKanjiSearch.edtRadicalsChange(Sender: TObject);
begin
  sbRadicals.Down:=edtRadicals.Text<>'';
  fKanji.DoItTimer;
end;

procedure TfKanjiSearch.edtSkipChange(Sender: TObject);
begin
  sbSKIP.Down:=edtSkip.Text<>'';
  fKanji.DoItTimer;
end;

procedure TfKanjiSearch.edtOtherChange(Sender: TObject);
begin
  sbOther.Down:=edtOther.Text<>'';
  fKanji.DoItTimer;
end;

procedure TfKanjiSearch.edtStrokeCountChange(Sender: TObject);
begin
  sbStrokeCount.Down:=edtStrokeCount.Text<>'';
  fKanji.DoItTimer;
end;

procedure TfKanjiSearch.edtJouyouChange(Sender: TObject);
begin
  sbJouyou.Down:=edtJouyou.Text<>'';
  fKanji.DoItTimer;
end;

procedure TfKanjiSearch.SpeedButton19Click(Sender: TObject);
var i:integer;
begin
  for i:=0 to lbCategories.Items.Count-1 do lbCategories.Checked[i]:=false;
  fKanji.DoIt;
end;

procedure TfKanjiSearch.RadicalSelectionChanged(Sender: TObject);
begin
  curradsearch := fRadical.SelectedRadicals;
  edtRadicals.Text := fRadical.SelectedRadicals;
 //edtRadicals.OnChange will trigger filter update
  pbRadicals.Invalidate;
end;

procedure TfKanjiSearch.sbListRadicalsClick(Sender: TObject);
var _radsearch: string;
begin
  _radsearch := curradsearch; //save current search, it'll be broken by RadicalSelectionChanged
  fRadical.SelectedRadicals := curradsearch;
  fRadical.OnSelectionChanged := Self.RadicalSelectionChanged;
  if IsPositiveResult(fRadical.ShowModal) then
    _radsearch := fRadical.SelectedRadicals;
  fRadical.OnSelectionChanged := nil;
 //apply new search, or re-apply old search
  curradsearch := _radsearch;
  edtRadicals.Text := _radsearch;
 //edtRadicals.OnChange will trigger filter update
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

procedure TfKanjiSearch.SpeedButton10Click(Sender: TObject);
begin
  edtStrokeCount.text:=OffsetRange(edtStrokeCount.text,1,1);
end;

procedure TfKanjiSearch.SpeedButton9Click(Sender: TObject);
begin
  edtStrokeCount.text:=OffsetRange(edtStrokeCount.text,-1,-1);
end;

procedure TfKanjiSearch.SpeedButton11Click(Sender: TObject);
begin
  edtStrokeCount.text:=OffsetRange(edtStrokeCount.text,-1,1);
end;

procedure TfKanjiSearch.SpeedButton13Click(Sender: TObject);
begin
  edtStrokeCount.text:=OffsetRange(edtStrokeCount.text,1,-1);
end;

procedure TfKanjiSearch.SpeedButton17Click(Sender: TObject);
begin
  edtJouyou.text:=OffsetRange(edtJouyou.text,1,1);
end;

procedure TfKanjiSearch.SpeedButton15Click(Sender: TObject);
begin
  edtJouyou.text:=OffsetRange(edtJouyou.text,-1,-1);
end;

procedure TfKanjiSearch.SpeedButton21Click(Sender: TObject);
begin
  edtJouyou.text:=OffsetRange(edtJouyou.text,-1,1);
end;

procedure TfKanjiSearch.SpeedButton23Click(Sender: TObject);
begin
  edtJouyou.text:=OffsetRange(edtJouyou.text,1,-1);
end;

procedure TfKanjiSearch.rgSortByClick(Sender: TObject);
begin
  fKanji.DoIt;
end;

procedure TfKanjiSearch.cbOtherTypeChange(Sender: TObject);
begin
  fKanji.DoItTimer;
end;

procedure TfKanjiSearch.pbRadicalsPaint(Sender: TObject);
begin
  pbRadicals.Canvas.Brush.Color:=clBtnFace;
  pbRadicals.Canvas.Font.Style:=[];
  DrawUnicode(pbRadicals.Canvas,1,1,16,curradsearch,FontRadical);
end;

procedure TfKanjiSearch.SpeedButton1Click(Sender: TObject);
var category:string;
begin
  fNewCategory.RadioGroup1.Enabled:=false;
  fNewCategory.Edit1.Text:='';
  if fNewCategory.ShowModal<>idOK then
  begin
    fNewCategory.RadioGroup1.Enabled:=true;
    exit;
  end;
  fNewCategory.RadioGroup1.Enabled:=false;
  category:='k~'+fNewCategory.Edit1.Text;
  inc(MaxCategoryIndex);
  TUserCat.Insert([inttostr(MaxCategoryIndex),category,inttostr(ord('K')),FormatDateTime('yyyymmdd',now)]);
  CreateKnownList(MaxCategoryIndex,0);
  fMenu.RefreshKanjiCategory;
  fMenu.ChangeUserData;
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
  fKanji.DoIt;
end;

procedure TfKanjiSearch.SpeedButton25Click(Sender: TObject);
begin
  if lbCategories.ItemIndex=-1 then exit;
  if Application.MessageBox(
    pchar(_l('#00882^eDo you really want to delete the category including all character links to it?')),
    pchar(_l('#00573^eWarning')),
    MB_ICONWARNING or MB_YESNO)=idYes then
  begin
    TUserCat.Locate('Name','k~'+lbCategories.Items[lbCategories.ItemIndex],false);
    TUserCat.Delete;
    fMenu.RefreshKanjiCategory;
    fMenu.ChangeUserData;
  end;
end;

procedure TfKanjiSearch.SpeedButton20Click(Sender: TObject);
var catname: string;
begin
  if lbCategories.ItemIndex=-1 then exit;
  TUserCat.Locate('Name','k~'+fKanjiDetails.ComboBox1.Items[lbCategories.ItemIndex],false);
  catname := StripCatName(TUserCat.Str(TUserCatName));
  if fNewCategory.EditCategory(catname) then begin
    TUserCat.Edit([TUserCatName],['k~'+catname]);
    fMenu.RefreshKanjiCategory;
    fMenu.ChangeUserData;
  end;
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
  fKanji.DoIt;
end;

procedure TfKanjiSearch.lbCategoriesDblClick(Sender: TObject);
var i:integer;
begin
  if lbCategories.ItemIndex<>-1 then
  begin
    for i:=0 to lbCategories.Items.Count-1 do
      lbCategories.Checked[i]:=i=lbCategories.ItemIndex;
    fKanji.DoIt;
  end;
end;

end.
