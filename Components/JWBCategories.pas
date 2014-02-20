unit JWBCategories;
{
Everything category-related.
Except tables. Tables are still in JWBMenu.

Category names are in the form:
  p~Category Name
  p: TCatPrefix

Throughout this file as a convention:
  catname     means bare category name (ex. 'Animals')
  category    means full category id (ex. 'j~Animals')

DO NOT USE CATEGORY NAME AS AN UNIQUE ID.
All such API is deprecated or must be deprecated. There could be several
categories with the same name.
}

interface
uses SysUtils, Classes, StdCtrls, Generics.Collections, JWBStrings,
  MemSource, TextTable;

type
  TCatPrefix = char;
  //c - chinese word category
  //j - japanese word category
  //k - kanji category

  TCatType = char;
  //Q - "LEARNED" - only one category is allowed
  //K - KnownList? (see JWBUnit::CreateKnownList)
  //L - "Lesson"
  //G - "Group"
  //T - "Temporary"
  //W - "WordList"

function GetCatPrefix(const s:string):TCatPrefix;
function StripCatName(const s:string):string;

{ Notifications. }

procedure CategoriesChanged;
procedure BeginCategoryUpdate;
procedure EndCategoryUpdate;


{ Raw Category manipulations.
 Linked KnownLists are kept consistent, but functions do not call CategoriesChanged. }

type
  TCatIndexList = array of integer;

function FindCategory(const category:string): integer;
procedure ListCategoryWords(cat:integer;list:TList<integer>); overload;
procedure ListWordCategories(word:integer;list:TStringList); overload;
procedure ListWordCategories(word:integer;list:TList<integer>); overload;

function RemoveWordFromCategory(word:integer;cat:integer): boolean;
function RemoveAllWordsFromCategory(cat:integer): boolean;
procedure MergeCategories(idxCats: array of integer; idxIntoCat: integer);
procedure CopyCategory(idxCat: integer; idxIntoCat: integer);
function GetWordCategoryItemCount(cat: integer): integer;
function GetCategoryItemCount(cat:integer): integer;
function DeleteUniqueCategoryWords(cat:integer;simulate:boolean): boolean;


{ Category manipulations UI.
 Every function must call CategoriesChanged in the end, if it changed anything }

function NewKanjiCategoryUI(const AOwner: TComponent = nil): integer;
function EditCategoryUI(cat:integer; const AOwner: TComponent = nil): boolean;
function DeleteCategoryUI(catidx:integer): boolean;
function MergeCategoryUI(categories: TCatIndexList): integer;
function DuplicateCategoryUI(catidx:integer; const AOwner: TComponent = nil): integer;
function NeedCategoryUI(category: string; cattype: char; silent: boolean): integer;


{ List of kanji categories (populated by mainform, used by others).
 It's okay to reload the list without reloading the controls which reference it,
 because the controls store category indexes inside them anyway.
 But the main form has methods for reloading all subscribers in one go }

var
  KanjiCats: array of record
    idx: integer;
    name: string;
  end;

procedure ReloadKanjiCategories;
procedure PasteKanjiCategoriesTo(sl: TStrings); //Makes a Category-TStringList


{ Category-TStringList and Category-TCombobox standard handling.
 Add items with AddCatItem(), and then you can call GetCatIdx and other functions. }
procedure AddCatItem(ctl: TCustomListBox; const title: string; idx: integer); overload;
procedure AddCatItem(ctl: TCustomComboBox; const title: string; idx: integer); overload;
function GetSelCatIdx(ctl: TCustomListBox): integer; overload;
function GetSelCatIdx(ctl: TCustomComboBox): integer; overload;
function GetCatIdx(ctl: TCustomListBox; ItemIndex: integer): integer; overload;
function GetCatIdx(ctl: TCustomComboBox; ItemIndex: integer): integer; overload;
function FindCat(ctl: TCustomListBox; ACatIdx: integer): integer; overload;
function FindCat(ctl: TCustomComboBox; ACatIdx: integer): integer; overload;
function IsFocusedKnownLearned(ctl: TCustomListBox): boolean;
function IsAnySelectedKnownLearned(ctl: TCustomListBox): boolean;


{ Known lists.
 Some categories have kanji bit mask buffer linked to it. }

var
  KnownLearned:integer; //index of "LEARNED" category (type 'Q'), set on load

procedure InitKnownLists;
procedure CreateKnownList(listno:integer;charnumber:integer);
procedure CopyKnownList(listfrom, listto: integer);
procedure FreeKnownList(listno: integer);
procedure FreeKnownLists;
procedure SaveKnownList(listno:integer;filename:string);
procedure LoadKnownList(listno:integer;stream:TStream);
procedure MergeKnownList(idxListNo, idxIntoListNo: integer);
function GetKnownListItemCount(listno: integer): integer;

function IsKnown(listno:integer;const char:FChar):boolean; overload;
function IsAnyKnown(listno:integer;const chars:FString):boolean;
function IsAllKnown(listno:integer;const chars:FString):boolean;
procedure SetKnown(listno:integer;const char:FChar;known:boolean); overload;
function FirstUnknownKanjiIndex(const kanji:FString):integer;
function CheckKnownKanji(const kanji:FString): FString;
{$IFDEF UNICODE}
function IsKnown(listno:integer;const char:FString):boolean; overload; inline;
procedure SetKnown(listno:integer;const char:FString;known:boolean); overload; inline;
{$ENDIF}


{
Classification depends on a number of factors, different classes are shown in
different colors.
Supported classes (not mutually exclusive, in the order of priority):
  K: Learned
  C: Common
  U: Rare/Unknown
  N: Names (mostly used in names only)
  A: Japanese and chinese
  J: Japanese only
}
function GetCharClass(const AChar: FString; ACursor: TTextTableCursor = nil): char;


{ Loading }
{ Call these after you've loaded the user data }

procedure LoadCategories(ps:TPackageSource);
procedure SaveCategories(const tempDir:string);
procedure FreeCategories;

procedure AddKnownLearnedCategory(ps:TPackageSource);

{ Bugfixes }

function FixDuplicateCategories(): boolean;


implementation
uses Controls, Forms, Windows, JWBMenu, JWBUserData, JWBVocabFilters,
  JWBNewCategory, JWBUnit, JWBCharData, JWBLegacyMarkup;

const
  eCannotLocateCat: string = 'Cannot locate category.'; //Do not localize

function GetCatPrefix(const s:string):char;
begin
  if Length(s)<1 then
    Result := '?'
  else
    Result := s[1];
end;

function StripCatName(const s:string):string;
begin
  Result:=s;
  if (length(Result)>1) and (Result[2]='~') then delete(Result,1,2);
end;


{
Notifications
}
var
  CategoryUpdateLock: integer;
  CategoryChangesRecorded: integer;

procedure CategoriesChanged;
begin
  if CategoryUpdateLock>0 then begin
    Inc(CategoryChangesRecorded);
    exit;
  end;
  fMenu.RefreshKanjiCategory;
  fMenu.RefreshCategory;
  fMenu.ChangeUserData;
end;

procedure BeginCategoryUpdate;
begin
  Inc(CategoryUpdateLock);
  if CategoryUpdateLock=1 then
    CategoryChangesRecorded := 0;
end;

procedure EndCategoryUpdate;
begin
  Dec(CategoryUpdateLock);
  if CategoryUpdateLock=0 then
    if CategoryChangesRecorded>0 then
      CategoriesChanged;
end;


threadvar
 { This is not very nice, but will do as a fix }
  CTUserSheet: TTextTableCursor;
  CTUserCat: TTextTableCursor;

function GetUserSheet: TTextTableCursor;
begin
  Result:=CTUserSheet;
  if Result=nil then begin
    Result := TTextTableCursor.Create(TUserSheet);
    CTUserSheet := Result;
  end;
end;

function GetUserCat: TTextTableCursor;
begin
  Result:=CTUserCat;
  if Result=nil then begin
    Result := TTextTableCursor.Create(TUserCat);
    CTUserCat := Result;
  end;
end;

//Finds category and returns its index, or -1 if not found.
function FindCategory(const category:string): integer;
var CUserCat: TTextTableCursor;
begin
  CUserCat := GetUserCat;
  if CUserCat.Locate('Name',category) then
    Result := CUserCat.Int(TUserCatIndex)
  else
    Result := -1;
end;

procedure ListCategoryWords(cat:integer;list:TList<integer>);
var CUserSheet: TTextTableCursor;
begin
  CUserSheet := GetUserSheet;
  CUserSheet.SetOrder('Sheet_Ind');
  CUserSheet.Locate('Number',cat);
  list.Clear;
  while (not CUserSheet.EOF) and (CUserSheet.Int(TUserSheetNumber)=cat) do
  begin
    list.Add(CUserSheet.Int(TUserSheetWord));
    CUserSheet.Next;
  end;
end;

//Populates TStringList with full names of all categories that contain this word.
procedure ListWordCategories(word:integer;list:TStringList);
var s:string;
  CUserSheet: TTextTableCursor;
  CUserCat: TTextTableCursor;
begin
  CUserSheet := GetUserSheet;
  CUserCat := GetUserCat;

  CUserSheet.SetOrder('Word_Ind');
  CUserSheet.Locate('Word',word);
  list.Clear;
  while (not CUserSheet.EOF) and (CUserSheet.Int(TUserSheetWord)=word) do
  begin
    CUserCat.Locate('Index',CUserSheet.Int(TUserSheetNumber));
    s:=CUserCat.Str(TUserCatName);
    list.Add(s);
    CUserSheet.Next;
  end;
end;

procedure ListWordCategories(word:integer;list:TList<integer>);
var CUserSheet: TTextTableCursor;
begin
  CUserSheet := GetUserSheet;
  CUserSheet.SetOrder('Word_Ind');
  CUserSheet.Locate('Word',word);
  list.Clear;
  while (not CUserSheet.EOF) and (CUserSheet.Int(TUserSheetWord)=word) do
  begin
    list.Add(CUserSheet.Int(TUserSheetNumber));
    CUserSheet.Next;
  end;
end;

//Removes a word from a category. Returns true if it was in there (false if it wasn't in that category).
function RemoveWordFromCategory(word:integer;cat:integer): boolean;
begin
  Result := false;
  TUserSheet.SetOrder('Word_Ind');
  TUserSheet.Locate('Word',word);
  while (not TUserSheet.EOF) and (TUserSheet.Int(TUserSheetWord)=word) do
  begin
    if TUserSheet.Int(TUserSheetNumber)=cat then begin
      TUserSheet.Delete;
      Result := true;
    end;
    TUserSheet.Next;
  end;
end;

//Deletes all TUserSheet records linking to a category, effectively removing all words from it.
//(But it also can be used AFTER some words have been deleted)
function RemoveAllWordsFromCategory(cat:integer): boolean;
begin
  Result := false;
  TUserSheet.First;
  while not TUserSheet.EOF do
  begin
    if TUserSheet.Int(TUserSheetNumber)=TUserCat.Int(TUserCatIndex) then begin
      TUserSheet.Delete;
      Result := true;
    end;
    TUserSheet.Next;
  end;
end;

//Moves all the words from idxCats to idxIntoCat, and deletes idxCats
procedure MergeCategories(idxCats: array of integer; idxIntoCat: integer);
var sidxIntoCat: string;
  CUserSheet: TTextTableCursor;
  NeedReindex: boolean;
  i: integer;
begin
  sidxIntoCat := IntToStr(idxIntoCat);

  TUserSheet.NoCommitting := true;
  NeedReindex := false;
  CUserSheet := TUserSheet.NewCursor;
  try
    CUserSheet.SetOrder('Sheet_Ind');
    for i := Low(idxCats) to High(idxCats) do begin
      CUserSheet.Locate('Number',idxCats[i]);
      while (not CUserSheet.EOF) and (CUserSheet.Int(TUserSheetNumber)=idxCats[i]) do begin
        TUserSheet.SetField(CUserSheet.tcur, TUserSheetNumber, sidxIntoCat);
        NeedReindex := true;
        CUserSheet.Next;
      end;
    end;
  finally
    CUserSheet.Free;
    TUserSheet.NoCommitting := false;
    if NeedReindex then
      TUserSheet.Reindex;
  end;

  with TUserCat.NewCursor do try
    for i := Low(idxCats) to High(idxCats) do begin
      if not Locate('Index',idxCats[i]) then
        raise Exception.Create(eCannotLocateCat);
      Delete;
      MergeKnownList(idxCats[i], idxIntoCat);
      if KnownLearned=idxCats[i] then KnownLearned:=idxIntoCat; //just in case
    end;
  finally
    Free;
  end;
end;

{ Helper function to build a list of word entries in a category.
 This is not a duplicate of ListCategoryWords because we also need a "pos" here }
type
  TCatWordsArray = array of record
    word: string;
    pos: string;
  end;

function __GetCategoryWords(catidx: integer): TCatWordsArray;
var CUserSheet: TTextTableCursor;
begin
  SetLength(Result,0);
  CUserSheet := GetUserSheet;
  CUserSheet.SetOrder('Sheet_Ind');
  CUserSheet.Locate('Number',catidx);
  while (not CUserSheet.EOF) and (CUserSheet.Int(TUserSheetNumber)=catidx) do
  begin
    SetLength(Result, Length(Result)+1);
    with Result[Length(Result)-1] do begin
      word := CUserSheet.Str(TUserSheetWord);
      pos := CUserSheet.Str(TUserSheetPos);
    end;
    CUserSheet.Next;
  end;
end;

//Copies all category links from idxCat to idxIntoCat.
//Doesn't care for the duplicates, so idxIntoCat better be empty or something.
procedure CopyCategory(idxCat: integer; idxIntoCat: integer);
var sidxIntoCat: string;
  words: TCatWordsArray;
  i: integer;
begin
  sidxIntoCat := IntToStr(idxIntoCat);
  words := __GetCategoryWords(idxCat);
  if Length(words)<=0 then exit; //skip pointless reindex
  TUserSheet.NoCommitting := true;
  try
    for i := 0 to Length(words) - 1 do
      TUserSheet.Insert([
        words[i].word, //they have to be strings for Insert
        sidxIntoCat,
        words[i].pos
      ]);
  finally
    TUserSheet.NoCommitting := false;
    TUserSheet.Reindex;
  end;
end;

function GetWordCategoryItemCount(cat: integer): integer;
var cUserSheet: TTextTableCursor;
begin
  Result := 0;
  CUserSheet := GetUserSheet;
  CUserSheet.SetOrder('Sheet_Ind');
  CUserSheet.Locate('Number',cat);
  while (not cUserSheet.EOF) and (CUserSheet.Int(TUserSheetNumber)=cat) do begin
    Inc(Result);
    CUserSheet.Next;
  end;
end;

function GetCategoryItemCount(cat:integer): integer;
begin
  Result := GetWordCategoryItemCount(cat)
    + GetKnownListItemCount(cat);
end;

{ PART OF A CATEGORY DELETION PROCEDURE.
 Deletes all words which are included only in a specified category.
 Returns true if such words were found.
 NOTE THAT links are not deleted, so the database is left in an inconsistent state. }
{ Simulate: Do not actually delete words, just return true or false. }
function DeleteUniqueCategoryWords(cat:integer;simulate:boolean): boolean;
var CUser: TTextTableCursor;
  words,sl: TList<integer>;
  i: integer;
begin
  Result := false;
  words:=TList<integer>.Create;
  sl:=TList<integer>.Create;
  CUser:=TUser.NewCursor;
  try
   { Big gain over enumerating all words, because for every word we skip
    we spare us one pass over TUserSheet. }
    ListCategoryWords(cat,words); //costs one pass over TUserSheet
    for i := 0 to words.Count - 1 do begin
      CUser.Locate('Index',words[i]);
      ListWordCategories(CUser.Int(TUserIndex),sl);
      if (sl.Count=1) and (sl[0]=cat) then begin
        Result := true;
        if simulate then exit; //It's enough to find one such item to return true
        CUser.Delete;
      end;
    end;
  finally
    sl.Free;
    words.Free;
    CUser.Free;
  end;
end;


{ Lets user create a new kanji category.
 Word categories are created automatically when you add words to them.
 Returns new category index or <0 }
function NewKanjiCategoryUI(const AOwner: TComponent): integer;
var fNewCategory: TfNewCategory;
  catname:string;
begin
  catname := '';
  fNewCategory := TfNewCategory.Create(AOwner);
  try
    fNewCategory.Caption:=_l('#01038^eNew category');
    if not fNewCategory.EditCategory(catname) then begin
      Result := -1;
      exit;
    end;
  finally
    FreeAndNil(fNewCategory);
  end;

  TUserCat.Insert(['0','k~'+catname,inttostr(ord('K')),
    FormatDateTime('yyyymmdd',now)]);
  Result := TUserCat.TrueInt(TUserCatIndex);
  CreateKnownList(Result,0);

  CategoriesChanged;
end;

function EditCategoryUI(cat:integer; const AOwner: TComponent): boolean;
var fNewCategory: TfNewCategory;
  CUserCat: TTextTableCursor;
  category: string;
  catname: string;
  cattype: char;
  pref: TCatPrefix;
begin
  Result := false;
  fNewCategory := nil;
  CUserCat := TUserCat.NewCursor;
  try
    if not CUserCat.Locate('Index',cat) then
      raise Exception.Create(eCannotLocateCat);
    category := CUserCat.Str(TUserCatName);
    catname := StripCatName(category);
    pref := GetCatPrefix(category);
    fNewCategory := TfNewCategory.Create(AOwner);
    fNewCategory.Caption:=_l('#01039^eEdit category');
    if pref='k' then begin
      Result := fNewCategory.EditCategory(catname);
      if Result then
        CUserCat.Edit([TUserCatName],['k~'+catname]);
    end else begin
      cattype := chr(CUserCat.Int(TUserCatType));
      Result := fNewCategory.EditCategory(catname, cattype);
      if Result then
        CUserCat.Edit([TUserCatName,TUserCatType],[pref+'~'+catname,inttostr(ord(cattype))]);
    end;
    if not Result then exit;
  finally
    FreeAndNil(CUserCat);
    FreeAndNil(fNewCategory);
  end;

  CategoriesChanged;
end;

//Deletes a category, handling all required user interaction.
//Returns false if the operation has been cancelled.
function DeleteCategoryUI(catidx:integer): boolean;
var CUserCat: TTextTableCursor;
  category: string;
  pref: TCatPrefix;
  confmsg: string;
begin
  if catidx=KnownLearned then
    raise Exception.Create(_l('#01043^eThis is a protected category, it cannot be deleted.'));
    //but better just don't let user call us like this

  Result := false;
  CUserCat := TUserCat.NewCursor;
  try
    if not CUserCat.Locate('Index',catidx) then
      raise Exception.Create(eCannotLocateCat);
    category := CUserCat.Str(TUserCatName);

    pref := GetCatPrefix(category);
    if pref='k' then
      confmsg := _l('#00882^eDo you really want to delete the category including all character links to it?')
    else
      confmsg := _l('#00857^eDo you really want to delete the category including all word links to it?');
    if Application.MessageBox(
      pchar(confmsg),
      pchar(_l('#00573^eWarning')),
      MB_ICONWARNING or MB_YESNO)<>idYes then
    begin
      Result := false;
      exit;
    end;

    if pref<>'k' then
      if DeleteUniqueCategoryWords(catidx,{simulate=}true) then begin
        if Application.MessageBox(
          pchar(_l('#01042^eSome word(s) are assigned only to this category. Do you want to remove them from vocabulary?')),
          pchar(_l('#00885^eWarning')),
          MB_ICONWARNING or MB_YESNO)=idNo then
        begin
          Application.MessageBox(
            pchar(_l('#00886^eCategory was not deleted.')),
            pchar(_l('#00887^eAborted')),
            MB_ICONERROR or MB_OK);
          Result := false;
          exit;
        end;
        DeleteUniqueCategoryWords(catidx,{simulate=}false); //this time for real
      end;

    RemoveAllWordsFromCategory(catidx);

    CUserCat.Delete;
    FreeKnownList(catidx);
    Result := true;

   //Rebuild indexes and refresh UI.
    if pref<>'k' then
      RebuildUserIndex;
  finally
    FreeAndNil(CUserCat);
  end;

  CategoriesChanged;
end;

function MergeCategoryUI(categories: TCatIndexList): integer;
var i: integer;
  KnownLearnedIndex: integer; //if we have KnownLearned category amidst others, take note of it
  CUserCat: TTextTableCursor;
begin
  if Length(categories)<0 then begin
    Result := -1;
    exit;
  end;

  KnownLearnedIndex := -1;
  for i := Low(categories) to High(categories) do
    if categories[i]=KnownLearned then begin
      KnownLearnedIndex := i;
      break;
    end;

 //If there's a KnownLearned, merge into it, otherwise merge into the first one.
  if KnownLearnedIndex>=0 then begin
    i := KnownLearned;
  end else
    i := Low(categories);
  Result := categories[i];
  while i<High(categories) do begin
    categories[i]:=categories[i+1];
    Inc(i);
  end;
  SetLength(categories, Length(categories)-1);

  CUserCat := TTextTableCursor.Create(TUserCat);
  try
    if not CUserCat.Locate('Index',Result) then
      raise Exception.Create(eCannotLocateCat);

    if Application.MessageBox(
      PChar(_l('#01044^eDo you really want to merge %d categories into one "%s" category?',
        [Length(categories), StripCatName(CUserCat.Str(TUserCatName))])),
      PChar(_l('#01045^eConfirm merge')),
      MB_YESNO + MB_ICONQUESTION
    )<>ID_YES then begin
      Result := -1;
      exit;
    end;

    MergeCategories(categories,Result);
  finally
    FreeAndNil(CUserCat);
  end;

  CategoriesChanged;
end;

{ Creates a copy of the specified category with the same content.
 Returns index of the new category, or < 0 }
function DuplicateCategoryUI(catidx:integer; const AOwner: TComponent): integer;
var fNewCategory: TfNewCategory;
  CUserCat: TTextTableCursor;
  category: string;
  pref: TCatPrefix;
  catname: string;
  cattype: char;
  confirmed: boolean;
begin
  Result := -1;
  fNewCategory := nil;
  CUserCat := TUserCat.NewCursor;
  try
    if not CUserCat.Locate('Index',catidx) then
      raise Exception.Create(eCannotLocateCat);
    category := CUserCat.Str(TUserCatName);

    catname := StripCatName(category);
    catname := _l('#01056^e%s - Copy', [catname]); //do not provoke duplicate names, suggest a different one
    fNewCategory := TfNewCategory.Create(AOwner);
    fNewCategory.Caption:=_l('#01040^eDuplicate category');
    pref := GetCatPrefix(category);
    if pref='k' then begin
      cattype := 'K'; //kanji
      confirmed := fNewCategory.EditCategory(catname);
    end else begin
      cattype := chr(CUserCat.Int(TUserCatType)); //by default same type
      confirmed := fNewCategory.EditCategory(catname, cattype);
    end;
    if not confirmed then begin
      Result := -1;
      exit;
    end;

   //Create category
    CUserCat.Insert(['0',pref+'~'+catname,
      inttostr(ord(cattype)),FormatDateTime('yyyymmdd',now)]);
    Result := CUserCat.TrueInt(TUserCatIndex);
    if pref='k' then
      CreateKnownList(Result,0);

   //Copy contents
    if pref='k' then
      CopyKnownList(catidx, Result)
    else
      CopyCategory(catidx, Result);
  finally
    FreeAndNil(CUserCat);
    FreeAndNil(fNewCategory);
  end;

  CategoriesChanged;
end;

{ Finds a category by name, or creates a new one asking user for details.
 Returns category id. If user cancels the operation, returns -1.
   category: category name
   cattype: category type
   silent: do not update user interface after adding. (Do it manually later!)
 Note that while there could be several categories with the same name,
 this function will only choose the first one. }
function NeedCategoryUI(category: string; cattype: char; silent: boolean): integer;
var fNewCategory: TfNewCategory;
  catname: string;
begin
  if TUserCat.Locate('Name',category) then begin
    Result:=TUserCat.Int(TUserCatIndex);
    exit;
  end;

  if cattype='?' then
  begin
    catname := StripCatName(category);
    fNewCategory := TfNewCategory.Create(Application);
    try
      fNewCategory.Caption:=_l('#01038^eNew category');
      if not fNewCategory.EditCategory(catname, cattype, []) then begin
        Result := -1;
        exit;
      end;
    finally
      FreeAndNil(fNewCategory);
    end;
    category:=curlang+'~'+catname;
  end;

 //Once more because the user could have specified a different name
  if TUserCat.Locate('Name',category) then begin
    Result:=TUserCat.Int(TUserCatIndex);
    exit;
  end;

 //Create new
  TUserCat.Insert(['0',category,inttostr(ord(cattype)),FormatDateTime('yyyymmdd',now)]);
  Result:=TUserCat.TrueInt(TUserCatIndex);
  if not silent then
    CategoriesChanged;
end;



{ KanjiCategories }

procedure ReloadKanjiCategories;
var lc:char;
  s:string;
begin
  SetLength(KanjiCats, 0);

  TUserCat.First;
  while not TUserCat.EOF do
  begin
    s:=TUserCat.Str(TUserCatName);
    lc:=GetCatPrefix(s);
    if lc='?' then begin
      lc := 'j';
      TUserCat.Edit([TUserCatName],['j~'+s])
    end;
    s:=StripCatName(s);
    if lc='k'then
    begin
      setlength(KanjiCats, Length(KanjiCats)+1);
      KanjiCats[Length(KanjiCats)-1].idx := TUserCat.Int(TUserCatIndex);
      KanjiCats[Length(KanjiCats)-1].name := s;
    end;
    TUserCat.Next;
  end;

 //We create at least one when loading user data, and don't let anyone delete it.
  Assert(Length(KanjiCats)>0, 'Internal error: No category!');
end;

{ Populate a string list with kanji categories. Also sets TObject references to category indexes. }
procedure PasteKanjiCategoriesTo(sl: TStrings);
var i: integer;
begin
  sl.Clear;
  for i := 0 to Length(KanjiCats) - 1 do
    sl.AddObject(KanjiCats[i].name, TObject(KanjiCats[i].idx));
end;

procedure AddCatItem(ctl: TCustomListBox; const title: string; idx: integer);
begin
  ctl.Items.AddObject(title, TObject(idx));
end;

procedure AddCatItem(ctl: TCustomComboBox; const title: string; idx: integer);
begin
  ctl.Items.AddObject(title, TObject(idx));
end;

function GetSelCatIdx(ctl: TCustomListBox): integer;
begin
  Result := integer(ctl.Items.Objects[ctl.ItemIndex]);
end;

function GetSelCatIdx(ctl: TCustomComboBox): integer;
begin
  Result := integer(ctl.Items.Objects[ctl.ItemIndex]);
end;

function GetCatIdx(ctl: TCustomListBox; ItemIndex: integer): integer;
begin
  Result := integer(ctl.Items.Objects[ItemIndex]);
end;

function GetCatIdx(ctl: TCustomComboBox; ItemIndex: integer): integer;
begin
  Result := integer(ctl.Items.Objects[ItemIndex]);
end;

function FindCat(ctl: TCustomListBox; ACatIdx: integer): integer;
var i: integer;
begin
  Result := -1;
  for i := 0 to ctl.Items.Count-1 do
    if integer(ctl.Items.Objects[i])=ACatIdx then begin
      Result := i;
      break;
    end;
end;

function FindCat(ctl: TCustomComboBox; ACatIdx: integer): integer;
var i: integer;
begin
  Result := -1;
  for i := 0 to ctl.Items.Count-1 do
    if integer(ctl.Items.Objects[i])=ACatIdx then begin
      Result := i;
      break;
    end;
end;

function IsFocusedKnownLearned(ctl: TCustomListBox): boolean;
begin
  Result := GetSelCatIdx(ctl)=KnownLearned;
end;

function IsAnySelectedKnownLearned(ctl: TCustomListBox): boolean;
var i: integer;
begin
  Result := false;
  for i := 0 to ctl.Items.Count - 1 do
    if ctl.Selected[i] and (GetCatIdx(ctl,i)=KnownLearned) then begin
      Result := true;
      break;
    end;
end;


{ Known lists }

var
  KnownList:array[1..20000] of pointer;
  KnownListSize:integer;

procedure InitKnownLists;
var i: integer;
begin
  for i:=1 to 20000 do KnownList[i]:=nil;
end;

procedure CheckListIndex(listno: integer); inline;
begin
  if listno>20000 then raise Exception.Create('ListNo size exceeded!');
end;

procedure CreateKnownList(listno:integer;charnumber:integer);
begin
  KnownListSize:=65536 div 8;
  CheckListIndex(listno);
  getmem(KnownList[listno],KnownListSize);
  fillchar(KnownList[listno]^,KnownListSize,0);
end;

//Copies all contents of a known list, replacing whatever was there.
//Both lists must be already created.
procedure CopyKnownList(listfrom, listto: integer);
begin
  CheckListIndex(listfrom);
  CheckListIndex(listto);
  if (KnownList[listfrom]=nil) or (KnownList[listto]=nil) then
    raise Exception.Create('Invalid CopyKnownList().');
  Move(KnownList[listfrom]^, KnownList[listto]^, KnownListSize);
end;

procedure FreeKnownList(listno: integer);
begin
  CheckListIndex(listno);
  if KnownList[listno]<>nil then begin
    freemem(KnownList[listno]);
    KnownList[listno] := nil;
  end;
end;

procedure FreeKnownLists;
var i:integer;
begin
  for i:=1 to 20000 do
    if KnownList[i]<>nil then begin
      freemem(KnownList[i]);
      KnownList[i] := nil;
    end;
end;

procedure SaveKnownList(listno:integer;filename:string);
var f:file;
begin
  CheckListIndex(listno);
  assignfile(f,filename);
  rewrite(f,1);
  blockwrite(f,KnownList[listno]^,KnownListSize);
  closefile(f);
end;

procedure LoadKnownList(listno:integer;stream:TStream);
var i,kj:integer;
    b:byte;
    w:integer;
begin
  CheckListIndex(listno);
  if stream.Size<KnownListSize then
  begin
    w:=stream.Size;
    for i:=1 to w do
    begin
      stream.Read(b,1);
      for kj:=0 to 7 do
        if (((b) shr kj) and 1)<>0 then
        begin
          if TChar.Locate('Index',(i-1)*8+1+kj) then
          begin
            SetKnown(listno,TChar.Str(TChar.Field('Unicode')),true);
          end;
        end;
    end;
  end
  else
    stream.Read(KnownList[listno]^,KnownListSize);
end;

procedure MergeKnownList(idxListNo, idxIntoListNo: integer);
var i: integer;
  pList, pIntoList: PInteger;
begin
 //Since they're bit arrays of the same size it's rather simple to merge...
  pList := KnownList[idxListNo];
  pIntoList := KnownList[idxIntoListNo];
  if pList=nil then exit; //nothing to merge
  if pIntoList=nil then begin //we'd have to create one, but there's a simpler solution
    KnownList[idxIntoListNo] := KnownList[idxListNo];
    KnownList[idxListNo] := nil;
    exit;
  end;

  i := KnownListSize;
  while i>=sizeof(integer) do begin
    pIntoList^ := pIntoList^ or pList^;
    Inc(pList); //inc in sizeof(Integer)s
    Inc(pIntoList);
    dec(i, sizeof(integer));
  end;
 //and the remainder in bytes (probably zero but let's write the code just in case)
  while i>0 do begin
    PByte(pIntoList)^ := PByte(pIntoList)^ or PByte(pList)^;
    pList := PInteger(integer(pList)+1);
    pIntoList := PInteger(integer(pIntoList)+1);
    dec(i, 1);
  end;

 //Delete the list we merged
  FreeKnownList(idxListNo);
end;

//You are not meant to understand or maintain this code, just worship the gods that revealed it to mankind
//http://stackoverflow.com/questions/109023/how-to-count-the-number-of-set-bits-in-a-32-bit-integer
function NumberOfBitsSet(i: integer): integer;
begin
  i := i - (i shr 1) and $55555555;
  i := i and $33333333 + (i shr 2) and $33333333;
  Result := (((i + (i shr 4)) and $0F0F0F0F) * $01010101) shr 24;
end;

function GetKnownListItemCount(listno: integer): integer;
var i: integer;
  pList: PInteger;
begin
  Result := 0;
  i := KnownListSize;
  pList := KnownList[listno];
  if pList=nil then exit;
  while i>=sizeof(integer) do begin
    Result := Result + NumberOfBitsSet(pList^);
    Inc(pList); //inc in sizeof(Integer)s
    dec(i, sizeof(integer));
  end;

 //and the remainder in bytes (probably zero but let's write the code just in case)
  while i>0 do begin
    Result := Result + NumberOfBitsSet(PByte(pList)^);
    pList := PInteger(integer(pList)+1);
    dec(i, 1);
  end;
end;

function IsKnown(listno:integer;const char:FChar):boolean;
var w:widechar{$IFDEF UNICODE} absolute char{$ENDIF};
  ki,kj:integer;
begin
 {$IFNDEF UNICODE}
  w:=HexToUnicode(char)[1];
 {$ENDIF}
  ki:=ord(w) div 8;
  kj:=ord(w) mod 8;
  if ki>=KnownListSize then
  begin
    result:=false;
    exit;
  end;
  result:=(((TByteArray(KnownList[listno]^)[ki]) shr kj) and 1)<>0;
end;

function IsAnyKnown(listno:integer;const chars:FString):boolean;
var i: integer;
begin
  Result := false;
  for i := 1 to flength(chars) do
    if IsKnown(listno, fgetch(chars,i)) then begin
      Result := true;
      exit;
    end;
end;

function IsAllKnown(listno:integer;const chars:FString):boolean;
var i: integer;
begin
  Result := true;
  for i := 1 to flength(chars) do
    if not IsKnown(listno, fgetch(chars,i)) then begin
      Result := false;
      exit;
    end;
end;

procedure SetKnown(listno:integer;const char:FChar;known:boolean);
var ki,kj:integer;
  a:byte;
  w:widechar{$IFDEF UNICODE} absolute char{$ENDIF};
begin
 {$IFNDEF UNICODE}
  w:=HexToUnicode(char)[1];
 {$ENDIF}
  ki:=ord(w) div 8;
  kj:=ord(w) mod 8;
  if ki>=KnownListSize then exit;
  a:=TByteArray(KnownList[listno]^)[ki];
  if known then a:=a or (1 shl kj) else a:=a and not (1 shl kj);
  TByteArray(KnownList[listno]^)[ki]:=a;
end;

{
Parses the string, fchar by fchar, checking that all kanji are "Learned".
If it encounters a character you haven't learned, it returns that character's
number, else it just returns -1.
}
function FirstUnknownKanjiIndex(const kanji:FString):integer;
{$IFDEF UNICODE}
var i: integer;
begin
  Result := -1;
  for i := 1 to Length(kanji) - 1 do
    if (Word(kanji[i]) and $F000 > $3000) and not IsKnown(KnownLearned, kanji[i]) then begin
      Result := i;
      break;
    end;
end;
{$ELSE}
var i, ch: integer;
begin
  Result := -1;
 //Original function had similar check so let's keep it
  if Length(kanji) mod 4 <> 0 then
    raise Exception.Create('Invalid FChar string at FirstUnknownKanjiIndex(): '+kanji);

  for i := 1 to Length(kanji) div 4 do begin
    ch := PInteger(@kanji[4*(i-1)+1])^;
    if (PFCharData(@ch)^[1]>'3') and not IsKnown(KnownLearned, fcopy(kanji, i, 1)) then begin
      Result := i;
      break;
    end;
  end;
end;
{$ENDIF}

{
Backward compability.
Prepends 'U' to the string if it contains kanjis not yet "learned".
}
function CheckKnownKanji(const kanji:FString): FString;
var i: integer;
begin
  i := FirstUnknownKanjiIndex(kanji);
  if i<0 then
    Result := kanji
  else
    Result := UH_UNKNOWN_KANJI + kanji;
end;


{$IFDEF UNICODE}
//Variants of the functions for cases where we pass a string
function IsKnown(listno:integer;const char:FString):boolean;
begin
  Result := (pointer(char)<>nil) and IsKnown(listno, PFChar(char)^);
end;
procedure SetKnown(listno:integer;const char:FString;known:boolean);
begin
  if Length(char)>=1 then
    SetKnown(listno, char[1], known);
end;
{$ENDIF}


{ Character classes }

function GetCharClass(const AChar: FString; ACursor: TTextTableCursor): char;
var CChar: TTextTableCursor;
begin
  if ACursor<>nil then
    CChar := ACursor
  else
    CChar := TChar.NewCursor;
  try
    if not CChar.Locate('Unicode', AChar) then begin
      Result := 'U'; //unknown char
      exit;
    end;

    if curLang<>'c' then
    begin
      if CChar.Int(TChar.fJouyouGrade)<9 then
        Result:='C'
      else
      if CChar.Int(TChar.fJouyouGrade)<10 then
        Result:='N'
      else
        Result:='U';
    end else
      Result := CChar.Str(TChar.fType)[1]; //'A'll or 'J'apanese only
      if (Result='A') or (Result='J') then begin
       //Keep it
      end else
      if CChar.Int(TChar.fChFrequency)<=5 then
        Result:='C'
      else
        Result:='U';
    if IsKnown(KnownLearned,AChar) then
      Result:='K';

  finally
    if ACursor=nil then
      FreeAndNil(CChar);
  end;
end;


{ Loading }

{ Call after loading User Data to update category list }
procedure LoadCategories(ps:TPackageSource);
var CatIdx:integer;
  CatName: string;
  CatType: char;
  ms: TMemoryStream;
begin
  FreeCategories(); //safety
  KnownLearned:=-1; //not found
  TUserCat.First;
  while not TUserCat.EOF do
  begin
    CatIdx:=strtoint(TUserCat.Str(TUserCatIndex));
    CatName:=TUserCat.Str(TUserCatName);
    CatType:=chr(TUserCat.Int(TUserCatType));

    if CatType='Q' then
    begin
     //First Q category is selected as LEARNED, rest are bugs and are ignored here.
     //But we still load them, that'd do us no harm and simplify processing later.
      if KnownLearned<0 then
        KnownLearned:=CatIdx;
      ms:=ps['knownchar.bin'].Lock;
      CreateKnownList(CatIdx,0);
      LoadKnownList(CatIdx,ms);
      ps['knownchar.bin'].Unlock;
    end else
    if CatType='K' then
    begin
      ms:=ps['char'+inttostr(CatIdx)+'.bin'].Lock;
      CreateKnownList(CatIdx,0);
      LoadKnownList(CatIdx,ms);
      ps['char'+inttostr(CatIdx)+'.bin'].Unlock;
    end;

    TUserCat.Next;
  end;
end;

procedure SaveCategories(const tempDir:string);
var i: integer;
  un: integer;
begin
  for i:=0 to Length(KanjiCats)-1 do
  begin
    un:=KanjiCats[i].idx;
    if un=KnownLearned then
      SaveKnownList(un,tempDir+'\knownchar.bin')
    else
      SaveKnownList(un,tempDir+'\char'+inttostr(un)+'.bin');
  end;
end;

procedure FreeCategories;
begin
  FreeKnownLists;
end;

{ Adds KnownLearned category to WAKAN.USR if it was missing.
 Tries to use knownchar.bin, when it's present. }
procedure AddKnownLearnedCategory(ps:TPackageSource);
var ms: TMemoryStream;
begin
  Assert(KnownLearned<0); //why are you adding it otherwise
  TUserCat.Insert(['0', 'k~'+_l('LEARNED'), inttostr(ord('Q')), FormatDateTime('yyyymmdd',now)]);
  KnownLearned := TUserCat.TrueInt(TUserCatIndex);
  ms:=ps['knownchar.bin'].Lock;
  CreateKnownList(KnownLearned,0);
  LoadKnownList(KnownLearned,ms);
  ps['knownchar.bin'].Unlock;
end;


{ Bugfixes }

{ Some older wakan.usr's have duplicate categories, perhaps due to a bug in Wakan.
 We merge them on load.
 Returns true if some changes were made. }
function FixDuplicateCategories(): boolean;
var i: integer;
  CatIdx, DupIdx:integer;
  CatName: string;
  CatType: char;
  KnownCategories: array of record
    idx: integer;
    category: string;
    cattype: char;
  end;
  CategoriesToMerge: array of record
    idxCat: integer;
    idxIntoCat: integer;
  end;

  procedure AddKnownCategory(idx: integer; category: string; cattype: char);
  var i: integer;
  begin
    i := Length(KnownCategories);
    SetLength(KnownCategories, i+1);
    KnownCategories[i].idx := idx;
    KnownCategories[i].category := category;
    KnownCategories[i].cattype := cattype;
  end;

  function IsKnownIndex(idx: integer): boolean;
  var i: integer;
  begin
    Result := false;
    for i := 0 to Length(KnownCategories) - 1 do
      if KnownCategories[i].idx=idx then begin
        Result := true;
        break;
      end;
  end;

  function FindKnownCategory(category: string; cattype: char): integer;
  var i: integer;
  begin
    Result := -1;
    for i := 0 to Length(KnownCategories) - 1 do
      if (KnownCategories[i].category=category)
      and (KnownCategories[i].cattype=cattype) then begin
        Result := KnownCategories[i].idx;
        break;
      end;
  end;

begin
 { There are several types of corruption which can occur in category lists.
    - categories can have the same name (=> merge)
    - categories with different names can be of type 'Q' (=> merge)
    - categories can have the same index (=> kill all but one) }

  Result := false;
  SetLength(KnownCategories, 0);
  SetLength(CategoriesToMerge, 0);
  TUserCat.First;
  while not TUserCat.EOF do
  begin
    CatIdx:=strtoint(TUserCat.Str(TUserCatIndex));
    CatName:=TUserCat.Str(TUserCatName);
    CatType:=chr(TUserCat.Int(TUserCatType));

    if IsKnownIndex(CatIdx) then begin
     //Duplicate index: just delete the category (words will be fine! there's
     //another one with the same index!)
      TUserCat.Delete;
      TUserCat.Next;
      Result := true;
      continue;
    end else

    if (KnownLearned<>CatIdx) and (CatType='Q') then begin
     //There can be only one Q category, irrelevant of name.
      SetLength(CategoriesToMerge, Length(CategoriesToMerge)+1);
      with CategoriesToMerge[Length(CategoriesToMerge)-1] do begin
        idxCat := CatIdx;
        idxIntoCat := KnownLearned;
      end;
    end else begin
      DupIdx := FindKnownCategory(CatName, CatType);
      if DupIdx>=0 then begin
        SetLength(CategoriesToMerge, Length(CategoriesToMerge)+1);
        with CategoriesToMerge[Length(CategoriesToMerge)-1] do begin
          idxCat := CatIdx;
          idxIntoCat := DupIdx;
        end;
      end else
        AddKnownCategory(CatIdx, CatName, CatType);
    end;

    TUserCat.Next;
  end;

 //Delete duplicates
  for i := 0 to Length(CategoriesToMerge) - 1 do begin
    MergeCategories([CategoriesToMerge[i].idxCat], CategoriesToMerge[i].idxIntoCat);
   //We'd delete the KnownList file from the package, but there's no need:
   //package will be rebuilt on save, and only KnownLists for existing categories will be added.
    Result := true;
  end;
end;





initialization
  InitKnownLists;

end.
