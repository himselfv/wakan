unit JWBUserData;

interface
uses TextTable, JWBStrings;

var
  TUser: TTextTable;
  TUserIndex, //i
  TUserEnglish, //s
  TUserPhonetic, //x
  TUserPhoneticSort, //s
  TUserKanji, //x
  TUserAdded, //s
  TUserPrinted, //s
  TUserLearned, //s
  TUserMastered, //s
  TUserNoPrinted, //i
  TUserScore, //b
  TUserMaxScore: integer; //b
  MaxUserIndex:integer;

  TUserIdx: TTextTable;
  TUserIdxWord, //i
  TUserIdxKanji, //x
  TUserIdxBegin, //l
  TUserIdxIndex: integer;

  TUserSheet: TTextTable;
  TUserSheetWord, //i
  TUserSheetNumber, //w
  TUserSheetPos: integer; //w

  TUserCat: TTextTable;
  TUserCatIndex,  //i
  TUserCatName, //s, prefix~Category name. See commens in JWBCategories
  TUserCatType, //b, TCatType, see comments in JWBCategories
  TUserCatCreated: integer; //s, datetime of creation
  MaxCategoryIndex: integer;

  TUserPrior: TTextTable;

function FindMaxUserIndex(): integer;
function FindMaxCategoryIndex(): integer;

function FindUserWord(kanji,phonetic: FString): integer;

function GetPhoneticSortStr(phonetic: FString): string;

implementation
uses JWBUnit, JWBMenu;

//Used in several places when loading
function FindMaxUserIndex(): integer;
begin
  Result:=0;
  TUser.First;
  while not TUser.EOF do
  begin
    if TUser.Int(TUserIndex)>Result then Result:=TUser.Int(TUserIndex);
    TUser.Next;
  end;
end;

function FindMaxCategoryIndex(): integer;
begin
  Result:=0;
  TUserCat.First;
  while not TUserCat.EOF do
  begin
    if TUserCat.Int(TUserCatIndex)>Result then Result:=TUserCat.Int(TUserCatIndex);
    TUserCat.Next;
  end;
end;

//Locates user record for a given kanji+phonetic pair. Returns its TUserIndex or -1.
function FindUserWord(kanji,phonetic: FString): integer;
begin
  Result:=-1;
  TUser.SetOrder('Kanji_Ind');
  TUser.Locate('Kanji',kanji);
  while (not TUser.EOF) and (TUser.Str(TUserKanji)=kanji) do
  begin
    if TUser.Str(TUserPhonetic)=phonetic then begin
      Result:=TUser.Int(TUserIndex);
      break;
    end;
    TUser.Next;
  end;
end;

//Returns a string which will represent this phonetic in sorting.
function GetPhoneticSortStr(phonetic: FString): string;
var s: FString;
  s2: FChar;
  a1,a2: string;
  i, j: integer;
begin
  if curlang='j'then
  begin
    Result:='';
    s:=RomajiToKana('H'+KanaToRomaji(phonetic,1,'j'),1,true,'j');
    for i:=0 to flength(s)-1 do
    begin
      s2:=fgetch(s,i+1);
      a1:='';
      a2:='';
      for j:=0 to Length(romasortl)-1 do
      begin
        if romasortl[j].roma=s2 then a1:=romasortl[j].order;
        if romasortl[j].roma=
         {$IFNDEF UNICODE}
          copy(s2,1,3)+chr(ord(s2[4])+1)
         {$ELSE}
          chr(ord(s2)+1)
         {$ENDIF}
        then a2:=romasortl[j].order;
      end;
      if a1='' then Result:=Result+a2 else Result:=Result+a1;
    end;
  end else
    Result:=KanaToRomaji(phonetic,1,'c');
end;

end.
