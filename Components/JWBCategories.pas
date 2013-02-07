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
}

interface
uses SysUtils, Classes, StdCtrls, JWBStrings;

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

{ Notifications }

procedure CategoriesChanged;
procedure BeginCategoryUpdate;
procedure EndCategoryUpdate;

{ Misc }

function FindCategory(const category:string): integer;
procedure ListWordCategories(word:integer;catlist:TStringList);
function CheckEnabledCategories(catlist: TStringList): boolean;

{ Category manipulations }

type
  TCatIndexList = array of integer;

function NewKanjiCategoryUI(): integer;
function EditCategoryUI(const category: string): boolean;
function RemoveWordFromCategory(word:integer;const catname:string): boolean;
function RemoveAllWordsFromCategory(const category:string): boolean;
function DeleteCategoryUI(category: string): boolean;
procedure MergeCategories(idxCats: array of integer; idxIntoCat: integer);
function MergeCategoryUI(categories: TCatIndexList): integer;
procedure CopyCategory(idxCat: integer; idxIntoCat: integer);
function DuplicateCategoryUI(const category: string): integer;
function NeedCategoryUI(category: string; cattype: char; silent: boolean): integer;
function GetWordCategoryItemCount(idxCat: integer): integer;
function GetCategoryItemCount(const category: string): integer;

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
{ Category string lists are used throughout the program }
procedure PasteKanjiCategoriesTo(sl: TStrings);
function GetSelCatIdx(ctl: TCustomListBox): integer; overload;
function GetSelCatIdx(ctl: TCustomComboBox): integer; overload;
function GetCatIdx(ctl: TCustomListBox; ItemIndex: integer): integer; overload;
function GetCatIdx(ctl: TCustomComboBox; ItemIndex: integer): integer; overload;
function IsFocusedKnownLearned(ctl: TCustomListBox): boolean;
function IsAnySelectedKnownLearned(ctl: TCustomListBox): boolean;

{ Known lists }

var
  KnownLearned:integer; //index of "LEARNED" category (type 'Q'), set on load

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
function IsKnown(listno:integer;const char:FString):boolean; overload; {$IFDEF INLINE}inline;{$ENDIF}
procedure SetKnown(listno:integer;const char:FString;known:boolean); overload; {$IFDEF INLINE}inline;{$ENDIF}
{$ENDIF}

{ Bugfixes }

function FixDuplicateCategories(): boolean;


implementation
uses Controls, Forms, Windows, TextTable, JWBMenu, JWBUserData, JWBUserFilters,
  JWBNewCategory, JWBUnit;

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

procedure CategoriesChanged;
begin
  if CategoryUpdateLock>0 then exit;
  fMenu.RefreshKanjiCategory;
  fMenu.RefreshCategory;
  fMenu.ChangeUserData;
end;

procedure BeginCategoryUpdate;
begin
  Inc(CategoryUpdateLock);
end;

procedure EndCategoryUpdate;
begin
  Dec(CategoryUpdateLock);
  if CategoryUpdateLock=0 then
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

//Populates TStringList with full names of all categories that contain this word.
procedure ListWordCategories(word:integer;catlist:TStringList);
var s:string;
  CUserSheet: TTextTableCursor;
  CUserCat: TTextTableCursor;
begin
  CUserSheet := GetUserSheet;
  CUserCat := GetUserCat;

  CUserSheet.SetOrder('Word_Ind');
  CUserSheet.Locate('Word',word);
  catlist.Clear;
  while (not CUserSheet.EOF) and (CUserSheet.Int(TUserSheetWord)=word) do
  begin
    CUserCat.Locate('Index',CUserSheet.Int(TUserSheetNumber));
    s:=CUserCat.Str(TUserCatName);
    catlist.Add(s);
    CUserSheet.Next;
  end;
end;

//Returns true, if at least one of those categories is enabled in fUserFilters.
function CheckEnabledCategories(catlist: TStringList): boolean;
var i, ind: integer;
  s: string;
  CUserCat: TTextTableCursor;
begin
  CUserCat := GetUserCat;
  Result := false;
  for i := 0 to catlist.Count - 1 do begin
    CUserCat.Locate('Index', StrToInt(catlist[i]));
    s:=CUserCat.Str(TUserCatName);
    ind:=fUserFilters.lbCategories.Items.IndexOf(StripCatName(s));
    if (ind<>-1) and (fUserFilters.lbCategories.Checked[i]) and (GetCatPrefix(s)=curlang) then begin
      Result:=true;
      break; //no point in scanning further
    end;
  end;
end;



{ Lets user create a new kanji category.
 Word categories are created automatically when you add words to them.
 Returns new category index or <0 }
function NewKanjiCategoryUI(): integer;
var catname:string;
begin
  catname := '';
  fNewCategory.Caption:=_l('#01038^eNew category');
  if not fNewCategory.EditCategory(catname) then begin
    Result := -1;
    exit;
  end;

  Inc(MaxCategoryIndex);
  TUserCat.Insert([inttostr(MaxCategoryIndex),'k~'+catname,inttostr(ord('K')),
    FormatDateTime('yyyymmdd',now)]);
  CreateKnownList(MaxCategoryIndex,0);
  Result := MaxCategoryIndex;

  CategoriesChanged;
end;

function EditCategoryUI(const category: string): boolean;
var catname: string;
  cattype: char;
  pref: TCatPrefix;
begin
  if not TUserCat.Locate('Name',category) then
    raise Exception.Create(eCannotLocateCat);
  catname := StripCatName(TUserCat.Str(TUserCatName));
  pref := GetCatPrefix(category);
  fNewCategory.Caption:=_l('#01039^eEdit category');
  if pref='k' then begin
    Result := fNewCategory.EditCategory(catname);
    if Result then
      TUserCat.Edit([TUserCatName],['k~'+catname]);
  end else begin
    cattype := chr(TUserCat.Int(TUserCatType));
    Result := fNewCategory.EditCategory(catname, cattype);
    if Result then
      TUserCat.Edit([TUserCatName,TUserCatType],[pref+'~'+catname,inttostr(ord(cattype))]);
  end;
  if not Result then exit;

  CategoriesChanged;
end;

//Removes a word from a category. Returns true if it was in there (false if it wasn't in that category).
function RemoveWordFromCategory(word:integer;const catname:string): boolean;
var s:string;
begin
  Result := false;
  TUserSheet.SetOrder('Word_Ind');
  TUserSheet.Locate('Word',word);
  while (not TUserSheet.EOF) and (TUserSheet.Int(TUserSheetWord)=word) do
  begin
    TUserCat.Locate('Index',TUserSheet.Int(TUserSheetNumber));
    s:=TUserCat.Str(TUserCatName);
    if catname=StripCatName(s) then begin
      TUserSheet.Delete;
      Result := true;
    end;
    TUserSheet.Next;
  end;
end;

//Deletes all TUserSheet records linking to a category, effectively removing all words from it.
//(But it also can be used AFTER some words have been deleted)
function RemoveAllWordsFromCategory(const category:string): boolean;
begin
  Result := false;
  if not TUserCat.Locate('Name',category) then
    raise Exception.Create(eCannotLocateCat);
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

//Deletes a category, handling all required user interaction.
//Returns false if the operation has been cancelled.
function DeleteCategoryUI(category: string): boolean;
var pref: TCatPrefix;
  confmsg: string;
  confirmed:boolean;
  sl:TStringList;
  catidx: integer;
begin
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

  if not TUserCat.Locate('Name',category) then
    raise Exception.Create(eCannotLocateCat);
  catidx := TUserCat.Int(TUserCatIndex);

  if catidx=KnownLearned then
    raise Exception.Create(_l('#01043^eThis is a protected category, it cannot be deleted.')); //but better just don't let user do this

  if pref<>'k' then begin
    //Scan all user words and remove those which were only in this category
    confirmed:=false;
    sl:=TStringList.Create;
    TUser.First;
    while not TUser.EOF do
    begin
      ListWordCategories(TUser.Int(TUserIndex),sl);
      if (sl.Count=1) and (sl[0]=category) then
      begin
        if not confirmed then
          if Application.MessageBox(
            pchar(_l('#01042^eSome word(s) are assigned only to this category. Do you want to remove them from vocabulary?')),
            pchar(_l('#00885^eWarning')),
            MB_ICONWARNING or MB_YESNO)=idNo then
          begin
            Application.MessageBox(
              pchar(_l('#00886^eCategory was not deleted.')),
              pchar(_l('#00887^eAborted')),
              MB_ICONERROR or MB_OK);
            Result := false; //although we did delete some words! bummer!
            exit;
          end;
        confirmed:=true;
        TUser.Delete;
      end;
      TUser.Next;
    end;
    sl.Free;
  end;

  RemoveAllWordsFromCategory(category);

  TUserCat.Delete;
  FreeKnownList(catidx);
  Result := true;

 //Rebuild indexes and refresh UI.
  if pref<>'k' then
    fMenu.RebuildUserIndex;
  CategoriesChanged;
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

{ Helper function to build a list of word entries in a category }
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

{ Creates a copy of the specified category with the same content.
 Returns index of the new category, or < 0 }
function DuplicateCategoryUI(const category: string): integer;
var pref: TCatPrefix;
  catname: string;
  cattype: char;
  catidx: integer;
  confirmed: boolean;
begin
  TUserCat.Locate('Name',category);
  catname := StripCatName(TUserCat.Str(TUserCatName));
  catidx := TUserCat.Int(TUserCatIndex);
  fNewCategory.Caption:=_l('#01040^eDuplicate category');
  pref := GetCatPrefix(category);
  if pref='k' then begin
    cattype := 'K'; //kanji
    confirmed := fNewCategory.EditCategory(catname);
  end else begin
    cattype := chr(TUserCat.Int(TUserCatType)); //by default the same
    confirmed := fNewCategory.EditCategory(catname, cattype);
  end;
  if not confirmed then begin
    Result := -1;
    exit;
  end;

 //Create category
  inc(MaxCategoryIndex);
  TUserCat.Insert([inttostr(MaxCategoryIndex),pref+'~'+catname, inttostr(ord(cattype)),
    FormatDateTime('yyyymmdd',now)]);
  if pref='k' then
    CreateKnownList(MaxCategoryIndex,0);
  Result := MaxCategoryIndex;

 //Copy contents
  if pref='k' then
    CopyKnownList(catidx, MaxCategoryIndex)
  else
    CopyCategory(catidx, MaxCategoryIndex);

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
var catname: string;
begin
  if TUserCat.Locate('Name',category) then begin
    Result:=TUserCat.Int(TUserCatIndex);
    exit;
  end;

  if cattype='?' then
  begin
    catname := StripCatName(category);
    fNewCategory.Caption:=_l('#01038^eNew category');
    if not fNewCategory.EditCategory(catname, cattype, []) then begin
      Result := -1;
      exit;
    end;
    category:=curlang+'~'+catname;
  end;

 //Once more because the user could have specified a different name
  if TUserCat.Locate('Name',category) then begin
    Result:=TUserCat.Int(TUserCatIndex);
    exit;
  end;

 //Create new
  Inc(MaxCategoryIndex);
  TUserCat.Insert([inttostr(MaxCategoryIndex),category,inttostr(ord(cattype)),FormatDateTime('yyyymmdd',now)]);
  Result:=MaxCategoryIndex;
  if not silent then
    CategoriesChanged;
end;

function GetWordCategoryItemCount(idxCat: integer): integer;
var cUserSheet: TTextTableCursor;
begin
  Result := 0;
  CUserSheet := GetUserSheet;
  CUserSheet.SetOrder('Sheet_Ind');
  CUserSheet.Locate('Number',idxCat);
  while not cUserSheet.EOF do begin
    if cUserSheet.Int(TUserSheetNumber)=idxCat then
      Inc(Result);
    cUserSheet.Next;
  end;
end;

function GetCategoryItemCount(const category: string): integer;
var pref: TCatPrefix;
begin
  pref := GetCatPrefix(category);
  if not TUserCat.Locate('Name',category) then
    Result := 0
  else
  if pref='k' then
    Result := GetKnownListItemCount(TUserCat.Int(TUserCatIndex))
  else
    Result := GetWordCategoryItemCount(TUserCat.Int(TUserCatIndex));
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

procedure CheckListIndex(listno: integer); {$IFDEF INLINE}inline;{$ENDIF}
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
  FreeMem(KnownList[idxListNo]);
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


var i: integer;
initialization
  for i:=1 to 20000 do KnownList[i]:=nil;

finalization

end.
