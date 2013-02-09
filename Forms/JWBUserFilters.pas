unit JWBUserFilters;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, CheckLst, Tabs, Buttons;

type
  TfUserFilters = class(TForm)
    rgSort: TRadioGroup;
    gbFilter: TGroupBox;
    cbFilterUnlearned: TCheckBox;
    cbFilterLearned: TCheckBox;
    cbFilterMastered: TCheckBox;
    cbFilterProblematic: TCheckBox;
    btnCatToggleAll: TSpeedButton;
    btnCatEdit: TSpeedButton;
    btnCatDelete: TSpeedButton;
    tabCatList: TTabSet;
    lbCategories: TCheckListBox;
    Label1: TLabel;
    Bevel1: TBevel;
    procedure cbFilterUnlearnedClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure lbCategoriesClick(Sender: TObject);
    procedure lbCategoriesDblClick(Sender: TObject);
    procedure tabCatListChange(Sender: TObject; NewTab: Integer;
      var AllowChange: Boolean);
    procedure btnCatToggleAllClick(Sender: TObject);
    procedure btnCatEditClick(Sender: TObject);
    procedure btnCatDeleteClick(Sender: TObject);
  public
    function CheckEnabledCategories(catlist: TStringList): boolean;
  end;

var
  fUserFilters: TfUserFilters;

implementation

uses JWBWords, JWBMenu, JWBUserData, JWBCategories, JWBUnit, JWBNewCategory;

{$R *.DFM}

procedure TfUserFilters.cbFilterUnlearnedClick(Sender: TObject);
begin
  fWords.ShowIt(false);
end;

procedure TfUserFilters.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  fWords.SpeedButton2.Down:=false;
  fMenu.aUserSettings.Checked:=false;
end;

procedure TfUserFilters.lbCategoriesClick(Sender: TObject);
begin
  fWords.ShowIt(false);
end;

procedure TfUserFilters.lbCategoriesDblClick(Sender: TObject);
var i:integer;
begin
  if lbCategories.ItemIndex<>-1 then lbCategories.Checked[lbCategories.ItemIndex]:=not
    lbCategories.Checked[lbCategories.ItemIndex];
  exit;
  if lbCategories.ItemIndex<>-1 then
  begin
    for i:=0 to lbCategories.Items.Count-1 do
      lbCategories.Checked[i]:=i=lbCategories.ItemIndex;
    fWords.ShowIt(false);
  end;
end;

procedure TfUserFilters.tabCatListChange(Sender: TObject; NewTab: Integer;
  var AllowChange: Boolean);
var i:integer;
    lc:char;
    s:string;
begin
  lbCategories.Items.Clear;
  TUserCat.SetOrder('Name_Ind');
  TUserCat.First;
  while not TUserCat.EOF do
  begin
    if chr(TUserCat.Int(TUserCatType))=tabCatList.Tabs[NewTab] then
    begin
      lc:='j';
      s:=TUserCat.Str(TUserCatName);
      if (length(s)>1) and (s[2]='~') then lc:=s[1];
      if lc=curlang then
        AddCatItem(lbCategories, StripCatName(TUserCat.Str(TUserCatName)), TUserCat.Int(TUserCatIndex));
    end;
    TUserCat.Next;
  end;
  for i:=0 to lbCategories.Items.Count-1 do lbCategories.Checked[i]:=true;
  lbCategories.ItemIndex:=0;
  fWords.ShowIt(false);
end;

procedure TfUserFilters.btnCatToggleAllClick(Sender: TObject);
var i:integer;
    allchecked:boolean;
begin
  allchecked:=true;
  for i:=0 to lbCategories.Items.Count-1 do if not lbCategories.Checked[i] then allchecked:=false;
  for i:=0 to lbCategories.Items.Count-1 do lbCategories.Checked[i]:=not allchecked;
  fWords.ShowIt(false);
end;

procedure TfUserFilters.btnCatEditClick(Sender: TObject);
begin
  if lbCategories.ItemIndex=-1 then exit;
  EditCategoryUI(GetSelCatIdx(lbCategories));
end;

procedure TfUserFilters.btnCatDeleteClick(Sender: TObject);
begin
  if lbCategories.ItemIndex=-1 then exit;
  DeleteCategoryUI(GetSelCatIdx(lbCategories));
end;

//Returns true, if at least one of those categories is enabled in fUserFilters.
function TfUserFilters.CheckEnabledCategories(catlist: TStringList): boolean;
var i, ind: integer;
begin
  Result := false;
  for i := 0 to catlist.Count - 1 do begin
    ind:=fUserFilters.lbCategories.Items.IndexOf(StripCatName(catlist[i]));
    if (ind<>-1) and (fUserFilters.lbCategories.Checked[i]) and (GetCatPrefix(catlist[i])=curlang) then begin
      Result:=true;
      break; //no point in scanning further
    end;
  end;
end;

end.
