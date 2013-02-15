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
  public
    function CheckEnabledCategories(catlist: TStringList): boolean;

  protected
    FPortraitMode: boolean;
  public
    procedure UpdateAlignment;
    procedure SetPortraitMode(Value: boolean);


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

function GetMaxItemLength(cb: TCheckListBox): integer;
var i: integer;
begin
  Result := 0;
  for i := 0 to cb.Items.Count - 1 do
    if Length(cb.Items[i])>Result then
      Result := Length(cb.Items[i]);
end;

procedure TfUserFilters.SetPortraitMode(Value: boolean);
begin
  FPortraitMode := Value;
 //One-time set default width/height (can be adjusted later)
 //TODO: Perhaps remember these for both modes and restore when switching between them
  if not FPortraitMode then
    fUserFilters.ClientWidth := 192
  else
    fUserFilters.ClientHeight := 120;
  UpdateAlignment;
end;

// Call on resize, on portrait mode change, on lbCategories font change
//TODO: Call on resize, on lbCategories font change
procedure TfUserFilters.UpdateAlignment;
var maxItemWidth: integer;
begin
  if not FPortraitMode then begin
    rgSort.Left := gbFilter.Left;
    rgSort.Top := gbFilter.Top + gbFilter.Height + 7;
    pnlCategories.Left := gbFilter.Left;
    pnlCategories.Top := rgSort.Top + rgSort.Height + 5;
    pnlCategories.Width := gbFilter.Width;
    pnlCategories.Height := Self.ClientHeight - pnlCategories.Top - 6;
    lbCategories.Columns := 0; //don't use columns //TODO: Perhaps use if the max item width allows?
    gbFilter.Anchors := [akLeft,akTop,akRight];
    rgSort.Anchors := [akLeft,akTop,akRight];
    pnlCategories.Anchors := [akLeft,akTop,akRight,akBottom];
  end else begin
    gbFilter.Width := 177;
    rgSort.Width := 177; //fixed width in horz mode
    rgSort.Left := gbFilter.Left + gbFilter.Width + 8;
    rgSort.Top := gbFilter.Top;
    pnlCategories.Left := rgSort.Left + rgSort.Width + 8;
    pnlCategories.Top := gbFilter.Top;
    pnlCategories.Width := Self.ClientWidth - pnlCategories.Left - 8;
    pnlCategories.Height := rgSort.Height;
   //Some guessing as to what is max item width
    maxItemWidth := GetMaxItemLength(lbCategories)*lbCategories.Font.Size;
    if maxItemWidth=0 then maxItemWidth := 90;
    if lbCategories.Width < maxItemWidth then
      lbCategories.Columns := 0 //don't use columns
    else
      lbCategories.Columns := lbCategories.Width div maxItemWidth;
    gbFilter.Anchors := [akLeft,akTop]; //no point in akBottom
    rgSort.Anchors := [akLeft,akTop]; //no point in akBottom
    pnlCategories.Anchors := [akLeft,akTop,akRight,akBottom];
  end;
end;

procedure TfUserFilters.FormResize(Sender: TObject);
begin
  UpdateAlignment;
end;


end.
