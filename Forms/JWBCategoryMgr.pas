unit JWBCategoryMgr;

interface

//TODO: Localize everything here
//TODO: Also aCategoryManager in JWBMenu

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ComCtrls, ExtCtrls, Generics.Collections,
  JwbForms;

type
  TfCategoryMgr = class(TJwbForm)
    pcPages: TTabControl;
    lbList: TListBox;
    btnClose: TBitBtn;
    Panel1: TPanel;
    btnNew: TBitBtn;
    btnDelete: TBitBtn;
    btnDuplicate: TBitBtn;
    btnMerge: TBitBtn;
    btnEdit: TBitBtn;
    procedure FormShow(Sender: TObject);
    procedure pcPagesChange(Sender: TObject);
    procedure lbListClick(Sender: TObject);
    procedure btnNewClick(Sender: TObject);
    procedure btnEditClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure lbListDrawItem(Control: TWinControl; Index: Integer; Rect: TRect;
      State: TOwnerDrawState);
    procedure btnDuplicateClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnMergeClick(Sender: TObject);
    procedure FormHide(Sender: TObject);
  protected
    type TCategoryData = record
      idx: integer;
      category: string;
    end;
  protected
    FCatList: TList<TCategoryData>; //synchronized with lbList
    function FocusedCategory: integer;
    function FindCategory(catidx: integer): integer;
    procedure TryFocusCategory(catidx: integer);
  public
    procedure ReloadList;
  end;

const //Tab indices
  TI_LESSON = 0;
  TI_GROUP = 1;
  TI_TEMPORARY = 2;
  TI_WORDLIST = 3;
  TI_KANJI = 4;

implementation
uses TextTable, JWBCategories, JWBUserData, JWBMenu, JWBUnit;

{$R *.dfm}

procedure TfCategoryMgr.FormCreate(Sender: TObject);
begin
  FCatList := TList<TCategoryData>.Create;
end;

procedure TfCategoryMgr.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FCatList);
end;

procedure TfCategoryMgr.FormShow(Sender: TObject);
begin
  ReloadList;
  if fsModal in FormState then
    BeginCategoryUpdate; //do not update anyone else while in modal
end;

procedure TfCategoryMgr.FormHide(Sender: TObject);
begin
  if fsModal in FormState then
    EndCategoryUpdate;
end;

procedure TfCategoryMgr.pcPagesChange(Sender: TObject);
begin
  ReloadList;
end;

procedure TfCategoryMgr.ReloadList;
var cd: TCategoryData;
  pref: char;
  catname: string;
  cattype: char;
  skip: boolean;
  CUserCat: TTextTableCursor;
begin
  lbList.Clear;
  lbList.Perform(WM_SETREDRAW,0,0);
  FCatList.Clear;

  CUserCat := TTextTableCursor.Create(TUserCat);
  try
    CUserCat.SetOrder('Name_Ind');
    CUserCat.First;
    while not CUserCat.EOF do
    begin
      cd.idx := CUserCat.Int(TUserCatIndex);
      cd.category:=CUserCat.Str(TUserCatName);
      pref:=GetCatPrefix(cd.category);
      catname:=StripCatName(cd.category);
      cattype:=Chr(CUserCat.Int(TUserCatType));
      skip:=false;
      case pcPages.TabIndex of
        0: if (pref<>curlang) or (cattype<>'L') then skip:=true;
        1: if (pref<>curlang) or (cattype<>'G') then skip:=true;
        2: if (pref<>curlang) or (cattype<>'T') then skip:=true;
        3: if (pref<>curlang) or (cattype<>'W') then skip:=true;
        4: if pref<>'k' then skip:=true;
      else skip:=true;
      end;

      if not skip then begin
        lbList.AddItem(
          _l('#01055^e%s: %d items', [catname, GetCategoryItemCount(cd.idx)]),
          TObject(cd.idx) //Important: same format PasteKanjiCategoriesTo() uses!
        );
        FCatList.Add(cd);
      end;
      CUserCat.Next;
    end;
  finally
    FreeAndNil(CUserCat);
  end;

  if lbList.Items.Count>0 then
    lbList.ItemIndex := 0
  else
    lbList.ItemIndex := -1;
  lbListClick(lbList);
  lbList.Perform(WM_SETREDRAW,1,0);
end;

procedure TfCategoryMgr.lbListClick(Sender: TObject);
var IsAnyKnownLearned: boolean;
begin
  IsAnyKnownLearned := IsAnySelectedKnownLearned(lbList);
  btnNew.Enabled := (pcPages.TabIndex=TI_KANJI); //Manual group addition is allowed only for kanji groups
  btnEdit.Enabled := (lbList.ItemIndex >= 0) and (lbList.SelCount = 1);
  btnDelete.Enabled := (lbList.ItemIndex >= 0) and (lbList.SelCount = 1) and not IsAnyKnownLearned;
  btnDuplicate.Enabled := (lbList.ItemIndex >= 0) and (lbList.SelCount = 1);
  btnMerge.Enabled := (lbList.SelCount>1); //we can merge with KnownLearned, merged category becomes KnownLearned then
end;

function TfCategoryMgr.FocusedCategory: integer;
begin
  if lbList.ItemIndex<0 then
    Result := -1
  else
    Result := FCatList[lbList.ItemIndex].idx;
end;

function TfCategoryMgr.FindCategory(catidx: integer): integer;
var i: integer;
begin
  Result := -1;
  for i := 0 to FCatList.Count - 1 do
    if FCatList.Items[i].idx=catidx then begin
      Result := i;
      exit;
    end;
end;

procedure TfCategoryMgr.TryFocusCategory(catidx: integer);
var i: integer;
begin
  i := FindCategory(catidx);
  if i<0 then exit;
  lbList.ItemIndex := i;
  lbList.SetFocus;
  lbList.Selected[i] := true;
  lbListClick(lbList);
end;

procedure TfCategoryMgr.btnNewClick(Sender: TObject);
var catidx: integer;
begin
  if pcPages.TabIndex <> TI_KANJI then exit;
  catidx := NewKanjiCategoryUI();
  if catidx>=0 then begin
    ReloadList;
    TryFocusCategory(catidx);
  end;
end;

procedure TfCategoryMgr.btnEditClick(Sender: TObject);
var catidx: integer;
begin
  catidx := FocusedCategory;
  if catidx<0 then exit;
  if EditCategoryUI(catidx) then begin
    ReloadList;
    TryFocusCategory(catidx); //might be impossible if it was moved
  end;
end;

procedure TfCategoryMgr.btnDeleteClick(Sender: TObject);
var catidx: integer;
begin
  catidx := FocusedCategory;
  if catidx<0 then exit;
  if DeleteCategoryUI(catidx) then
    ReloadList;
end;

type
  TDirtyListboxHack = class(TListBox)
  end;

procedure TfCategoryMgr.lbListDrawItem(Control: TWinControl; Index: Integer;
  Rect: TRect; State: TOwnerDrawState);
var OldOnDrawItem: TDrawItemEvent;
begin
 //Just for nicety, draw LEARNED category in appropriate color
  if GetCatidx(lbList,Index)=KnownLearned then
    lbList.Canvas.Font.Color := Col('Kanji_Learned')
  else
    lbList.Canvas.Font.Color := clWindowText; //normal
 //Do normal drawing by temporarily disabling OnDrawItem. There's no other way...
  OldOnDrawItem := lbList.OnDrawItem;
  lbList.OnDrawItem := nil;
  TDirtyListboxHack(lbList).DrawItem(Index,Rect,State);
  lbList.OnDrawItem := OldOnDrawItem;
end;

procedure TfCategoryMgr.btnDuplicateClick(Sender: TObject);
var catidx: integer;
begin
  catidx := FocusedCategory;
  if catidx<0 then exit;
  catidx := DuplicateCategoryUI(catidx);
  if catidx>=0 then begin
    ReloadList;
    TryFocusCategory(catidx);
  end;
end;

procedure TfCategoryMgr.btnMergeClick(Sender: TObject);
var sel: TCatIndexList;
  i, j: integer;
  catidx: integer;
begin
  if lbList.SelCount < 2 then exit;

 //Build list of category indexes
  SetLength(sel, lbList.SelCount);
  j := 0;
  for i := 0 to lbList.Items.Count - 1 do
    if lbList.Selected[i] then begin
      sel[j] := FCatList[i].idx;
      Inc(j);
    end;

  catidx := MergeCategoryUI(sel);
  if catidx>=0 then begin
    ReloadList;
    TryFocusCategory(catidx);
  end;
end;

end.
