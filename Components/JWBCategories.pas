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
uses SysUtils, Classes, JWBStrings;

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
  //W - WordList?

function GetCatPrefix(s:string):TCatPrefix;
function StripCatName(s:string):string;

{ Misc }

procedure ListWordCategories(word:integer;catlist:TStringList);
function CheckEnabledCategories(catlist: TStringList): boolean;

{ Category manipulations }

function FindCategory(category:string): integer;
function RemoveWordFromCategory(word:integer;catname:string): boolean;
function RemoveAllWordsFromCategory(category:string): boolean;
function DeleteCategoryUI(category: string): boolean;
procedure MergeCategory(idxCat: integer; idxIntoCat: integer);

{ Known lists }

var
  KnownLearned:integer; //index of "LEARNED" category (type 'Q'), set on load

procedure CreateKnownList(listno:integer;charnumber:integer);
procedure FreeKnownLists;
procedure SaveKnownList(listno:integer;filename:string);
procedure LoadKnownList(listno:integer;stream:TStream);
procedure MergeKnownList(idxListNo, idxIntoListNo: integer);

function IsKnown(listno:integer;const char:FChar):boolean; overload;
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
uses Forms, Windows, JWBMenu, JWBUserFilters;

function GetCatPrefix(s:string):char;
begin
  if Length(s)<1 then
    Result := '?'
  else
    Result := s[1];
end;

function StripCatName(s:string):string;
begin
  if (length(s)>1) and (s[2]='~') then delete(s,1,2);
  result:=s;
end;

//Populates TStringList with full names of all categories that contain this word.
procedure ListWordCategories(word:integer;catlist:TStringList);
var s:string;
begin
  TUserSheet.SetOrder('Word_Ind');
  TUserSheet.Locate('Word',inttostr(word),true);
  catlist.Clear;
  while (not TUserSheet.EOF) and (TUserSheet.Int(TUserSheetWord)=word) do
  begin
    TUserCat.Locate('Index',TUserSheet.Str(TUserSheetNumber),true);
    s:=TUserCat.Str(TUserCatName);
    catlist.Add(s);
    TUserSheet.Next;
  end;
end;

//Returns true, if at least one of those categories is enabled in fUserFilters.
function CheckEnabledCategories(catlist: TStringList): boolean;
var i, ind: integer;
  s: string;
begin
  Result := false;
  for i := 0 to catlist.Count - 1 do begin
    s:=TUserCat.Str(TUserCatName);
    ind:=fUserFilters.ListBox1.Items.IndexOf(StripCatName(s));
    if (ind<>-1) and (fUserFilters.ListBox1.Checked[i]) and (GetCatPrefix(s)=curlang) then begin
      Result:=true;
      break; //no point in scanning further
    end;
  end;
end;

//Finds category and returns its index, or -1 if not found.
function FindCategory(category:string): integer;
begin
  if TUserCat.Locate('Name',category,false) then
    Result := TUserCat.Int(TUserCatIndex)
  else
    Result := -1;
end;

//Removes a word from a category. Returns true if it was in there (false if it wasn't in that category).
function RemoveWordFromCategory(word:integer;catname:string): boolean;
var s:string;
begin
  Result := false;
  TUserSheet.SetOrder('Word_Ind');
  TUserSheet.Locate('Word',inttostr(word),true);
  while (not TUserSheet.EOF) and (TUserSheet.Int(TUserSheetWord)=word) do
  begin
    TUserCat.Locate('Index',TUserSheet.Str(TUserSheetNumber),true);
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
function RemoveAllWordsFromCategory(category:string): boolean;
begin
  Result := false;
  TUserCat.Locate('Name',category,false);
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
var sl:TStringList;
    confirmed:boolean;
begin
  confirmed:=false;
  if Application.MessageBox(
    pchar(_l('#00857^eDo you really want to delete the category including all word links to it?')),
    pchar(_l('#00573^eWarning')),
    MB_ICONWARNING or MB_YESNO)<>idYes then
  begin
    Result := false;
    exit;
  end;

  //Scan all user words and remove them from this category
  sl:=TStringList.Create;
  TUser.First;
  while not TUser.EOF do
  begin
    ListWordCategories(TUser.Int(TUserIndex),sl);
    if (sl.Count=1) and (sl[0]=category) then
    begin
      if not confirmed then
        if Application.MessageBox(
          pchar(_l('^eSome word(s) are assigned only to this category. Do you want to remove them from vocabulary?')),
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

  RemoveAllWordsFromCategory(category);

  TUserCat.Locate('Name',category,false);
  TUserCat.Delete;
  Result := true;

 //Don't forget to rebuild indexes and refresh UI.
 //Not doing it here.
end;

//Moves all the words from idxCat to idxIntoCat, and deletes idxCat
procedure MergeCategory(idxCat: integer; idxIntoCat: integer);
var sidxCat: string;
begin
  sidxCat := IntToStr(idxCat);

  TUserSheet.First;
  while not TUserSheet.EOF do
  begin
    if TUserSheet.Int(TUserSheetNumber)=idxCat then begin
      TUserSheet.SetField(TUserSheet.tcur, TUserSheetNumber, sidxCat);
      TUserSheet.Commit(TUserSheet.tcur);
    end;
    TUserSheet.Next;
  end;

  TUserCat.Locate('Index',sidxCat,false);
  TUserCat.Delete;
end;

{ Known lists }

var
  KnownList:array[1..20000] of pointer;
  KnownListSize:integer;

procedure CreateKnownList(listno:integer;charnumber:integer);
begin
  KnownListSize:=65536 div 8;
  if listno>20000 then raise Exception.Create('ListNo size exceeded!');
  getmem(KnownList[listno],KnownListSize);
  fillchar(KnownList[listno]^,KnownListSize,0);
end;

procedure FreeKnownLists;
var i:integer;
begin
  for i:=1 to 20000 do if KnownList[i]<>nil then freemem(KnownList[i],KnownListSize);
end;

procedure SaveKnownList(listno:integer;filename:string);
var f:file;
begin
  if listno>20000 then raise Exception.Create('ListNo size exceeded!');
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
  if listno>20000 then raise Exception.Create('ListNo size exceeded!');
  if stream.Size<KnownListSize then
  begin
    w:=stream.Size;
    for i:=1 to w do
    begin
      stream.Read(b,1);
      for kj:=0 to 7 do
        if (((b) shr kj) and 1)<>0 then
        begin
          if TChar.Locate('Index',inttostr((i-1)*8+1+kj),true) then
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
    MergeCategory(CategoriesToMerge[i].idxCat, CategoriesToMerge[i].idxIntoCat);
    MergeKnownList(CategoriesToMerge[i].idxCat, CategoriesToMerge[i].idxIntoCat);
   //We'd delete the KnownList file from the package, but there's no need:
   //package will be rebuilt on save, and only KnownLists for existing categories will be added.
    if KnownLearned=CategoriesToMerge[i].idxCat then KnownLearned:=CategoriesToMerge[i].idxIntoCat; //just in case, although shouldn't happen
    Result := true;
  end;
end;


var i: integer;
initialization
  for i:=1 to 20000 do KnownList[i]:=nil;

finalization

end.
