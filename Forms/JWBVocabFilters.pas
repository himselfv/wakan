unit JWBVocabFilters;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, CheckLst, Tabs, Buttons, JWBForms;

type
  TfVocabFilters = class(TForm)
    rgSort: TRadioGroup;
    gbFilter: TGroupBox;
    cbFilterUnlearned: TCheckBox;
    cbFilterLearned: TCheckBox;
    cbFilterMastered: TCheckBox;
    cbFilterProblematic: TCheckBox;
    Bevel1: TBevel;
    pnlCategories: TPanel;
    pnlCategoryControls: TPanel;
    Label1: TLabel;
    lbCategories: TCheckListBox;
    tabCatList: TTabSet;
    btnCatDelete: TSpeedButton;
    btnCatEdit: TSpeedButton;
    btnCatToggleAll: TSpeedButton;
    procedure cbFilterUnlearnedClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure lbCategoriesClick(Sender: TObject);
    procedure lbCategoriesDblClick(Sender: TObject);
    procedure tabCatListChange(Sender: TObject; NewTab: Integer;
      var AllowChange: Boolean);
    procedure btnCatToggleAllClick(Sender: TObject);
    procedure btnCatEditClick(Sender: TObject);
    procedure btnCatDeleteClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);

  protected
    procedure CategoryListChanged(Sender: TObject);
  public
    function CheckEnabledCategories(catlist: TStringList): boolean;

  protected
   //Undocked width and height are used as permanent storage for dock width/height
   //Docker reads from those by default, and we update on Resize.
    FDockMode: TAlign;
    procedure WMSetDockMode(var msg: TMessage); message WM_SET_DOCK_MODE;
    procedure UpdateAlignment;

  end;

var
  fVocabFilters: TfVocabFilters;

implementation

uses TextTable, JWBVocab, JWBMenu, JWBUserData, JWBCategories, JWBUnit;

{$R *.DFM}

procedure TfVocabFilters.FormCreate(Sender: TObject);
begin
  OnCategoryListChanged.Add(Self.CategoryListChanged);
end;

procedure TfVocabFilters.FormDestroy(Sender: TObject);
begin
  OnCategoryListChanged.Remove(Self.CategoryListChanged);
end;

procedure TfVocabFilters.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  fVocab.btnListSettings.Down:=false;
  fMenu.aVocabSettings.Checked:=false;
end;

procedure TfVocabFilters.CategoryListChanged(Sender: TObject);
var b:boolean;
begin
  Self.tabCatListChange(fMenu,fVocabFilters.tabCatList.TabIndex,b);
end;

procedure TfVocabFilters.cbFilterUnlearnedClick(Sender: TObject);
begin
  fVocab.ShowIt(false);
end;

procedure TfVocabFilters.lbCategoriesClick(Sender: TObject);
begin
  fVocab.ShowIt(false);
end;

procedure TfVocabFilters.lbCategoriesDblClick(Sender: TObject);
begin
  if lbCategories.ItemIndex<>-1 then lbCategories.Checked[lbCategories.ItemIndex]:=not
    lbCategories.Checked[lbCategories.ItemIndex];
end;

procedure TfVocabFilters.tabCatListChange(Sender: TObject; NewTab: Integer;
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
  fVocab.ShowIt(false);
end;

procedure TfVocabFilters.btnCatToggleAllClick(Sender: TObject);
var i:integer;
    allchecked:boolean;
begin
  allchecked:=true;
  for i:=0 to lbCategories.Items.Count-1 do if not lbCategories.Checked[i] then allchecked:=false;
  for i:=0 to lbCategories.Items.Count-1 do lbCategories.Checked[i]:=not allchecked;
  fVocab.ShowIt(false);
end;

procedure TfVocabFilters.btnCatEditClick(Sender: TObject);
begin
  if lbCategories.ItemIndex=-1 then exit;
  EditCategoryUI(GetSelCatIdx(lbCategories));
end;

procedure TfVocabFilters.btnCatDeleteClick(Sender: TObject);
begin
  if lbCategories.ItemIndex=-1 then exit;
  DeleteCategoryUI(GetSelCatIdx(lbCategories));
end;

//Returns true, if at least one of those categories is enabled in fUserFilters.
function TfVocabFilters.CheckEnabledCategories(catlist: TStringList): boolean;
var i, ind: integer;
begin
  Result := false;
  for i := 0 to catlist.Count - 1 do begin
    ind:=lbCategories.Items.IndexOf(StripCatName(catlist[i]));
    if (ind<>-1) and (lbCategories.Checked[ind]) and (GetCatPrefix(catlist[i])=curlang) then begin
      Result:=true;
      break; //no point in scanning further
    end;
  end;
end;

function GetMaxItemLength(cb: TCheckListBox): integer;
var i: integer;
begin
  Result := 0;
  for i := 0 to cb.Items.Count - 1 do
    if Length(cb.Items[i])>Result then
      Result := Length(cb.Items[i]);
end;

{ Call on resize, on dock mode change, on lbCategories font change }
procedure TfVocabFilters.UpdateAlignment;
var maxItemWidth: integer;
begin
  if FDockMode in [alLeft,alRight] then
    gbFilter.Width := ClientWidth - 15
  else
    gbFilter.Width := 177;

  rgSort.Width := gbFilter.Width;
  if (FDockMode in [alLeft,alRight]) or (ClientHeight > (gbFilter.Top + gbFilter.Height + 7) + rgSort.Height + 7) then begin
   //Even in Portrait Mode, we try to stack these two modules when possible
    rgSort.Left := gbFilter.Left;
    rgSort.Top := gbFilter.Top + gbFilter.Height + 7;
  end else begin
    rgSort.Left := gbFilter.Left + gbFilter.Width + 8;
    rgSort.Top := gbFilter.Top;
  end;

  if FDockMode in [alLeft,alRight] then begin
    pnlCategories.Left := gbFilter.Left;
    pnlCategories.Top := rgSort.Top + rgSort.Height + 5;
    pnlCategories.Width := gbFilter.Width;
    pnlCategories.Height := Self.ClientHeight - pnlCategories.Top - 6;
    lbCategories.Columns := 0; //don't use columns //TODO: Perhaps use if the max item width allows?
  end else begin
   //Categories are aligned to the right of rgSort, stacked or not
    pnlCategories.Left := rgSort.Left + rgSort.Width + 8;
    pnlCategories.Top := gbFilter.Top-6;
    pnlCategories.Width := ClientWidth - pnlCategories.Left - 8;
    pnlCategories.Height := ClientHeight - 15;
   //Some guessing as to what is max item width
    maxItemWidth := GetMaxItemLength(lbCategories)*lbCategories.Font.Size;
    if maxItemWidth=0 then maxItemWidth := 90;
    if lbCategories.Width < maxItemWidth then
      lbCategories.Columns := 0 //don't use columns
    else
      lbCategories.Columns := lbCategories.Width div maxItemWidth;
  end;
end;

procedure TfVocabFilters.WMSetDockMode(var msg: TMessage);
begin
  if FDockMode=TAlign(msg.WParam) then exit;
  FDockMode := TAlign(msg.WParam);
  UpdateAlignment;
end;

procedure TfVocabFilters.FormResize(Sender: TObject);
begin
  UpdateAlignment;
 //Remember width/height preferences in UndockWidth/Height
  case FDockMode of
    alLeft, alRight: Self.UndockWidth := Self.Width;
    alTop, alBottom: Self.UndockHeight := Self.Height;
  end;
end;

end.

