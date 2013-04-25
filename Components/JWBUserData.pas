unit JWBUserData;
{
WAKAN.USR hanlding.

User dictionary is quite different from normal dictionaries.
Even search is possible only by kana.
}

interface
uses TextTable, JWBStrings, MemSource;

const
 {
  Current WAKAN.USR format. Version history:
    0  version information not found
    1  ...
    2  current format
  Wakan was trying to do some silent upgrading from 1 to 2 but as I see it,
  it was pretty pointless.
  We might later do upgrades but let's try to do those on load (in-memory data
  always in the latest format).
 }
  CurrentUserDataVersion=2;

var
  UserDataVersion: integer;

  TUser: TTextTable;
  TUserIndex, //i
  TUserEnglish, //s
  TUserPhonetic, //x
  TUserPhoneticSort, //s -- that's not the same as dictionary's "sort", it
    //contains the results of GetPhoneticSortStr which can be string-numeric
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

procedure InitializeUserPackage(const package:string);
function LoadUserPackage(const filename: string): TPackageSource;
procedure SaveUserPackage(const tempDir: string; const filename: string);
procedure FreeUserPackage;

function UserDataAutoRepair(): boolean;

function FindMaxUserIndex(): integer;
function FindMaxCategoryIndex(): integer;

function FindUserWord(kanji,phonetic: FString): integer;

procedure RebuildUserIndex;


{
User dictionary word sort order.
Required only when adding words to the dictionary, in which case:
1. Populate romasortl.
2. Call GetPhoneticSortStr(phonetic,lang) on phonetic for a given word to get
  TUserPhoneticSort value.
}

procedure ClearRomaSortRecords;
procedure AddRomaSortRecord(const s: string);

function GetPhoneticSortStr(const phonetic: FString; const lang: char): string;


implementation
uses SysUtils, JWBUnit, JWBKanaConv, PKGWrite, JWBCategories, Classes;

{ Packs WAKAN.USR data from directory Dir to package Package.
 Do not use directly; there are functions to save and load user data packages. }
procedure WriteUserPackage(const dir:string;const package:string);
var f:file of byte;
    b:byte;
begin
  assignfile(f,dir+'\struct.ver');
  rewrite(f);
  b:=CurrentUserDataVersion;
  write(f,b);
  closefile(f);
  PKGWriteForm.PKGWriteCmd('NotShow');
  PKGWriteForm.PKGWriteCmd('PKGFileName '+package);
  PKGWriteForm.PKGWriteCmd('MemoryLimit 100000000');
  PKGWriteForm.PKGWriteCmd('Name WaKan User Data');
  PKGWriteForm.PKGWriteCmd('TitleName WaKan User Data File');
  PKGWriteForm.PKGWriteCmd('CopyrightName '+WakanCopyright);
  PKGWriteForm.PKGWriteCmd('FormatName Pure Package File');
  PKGWriteForm.PKGWriteCmd('CommentName File is used by '+WakanAppName);
  PKGWriteForm.PKGWriteCmd('VersionName 1.0');
  PKGWriteForm.PKGWriteCmd('HeaderCode 621030');
  PKGWriteForm.PKGWriteCmd('FileSysCode 587135');
  PKGWriteForm.PKGWriteCmd('WriteHeader');
  PKGWriteForm.PKGWriteCmd('TemporaryLoad');
  PKGWriteForm.PKGWriteCmd('CryptMode 0');
  PKGWriteForm.PKGWriteCmd('CRCMode 0');
  PKGWriteForm.PKGWriteCmd('PackMode 0');
  PKGWriteForm.PKGWriteCmd('CryptCode 978312');
  PKGWriteForm.PKGWriteCmd('Include '+dir);
  PKGWriteForm.PKGWriteCmd('Finish');
end;

{ Creates new empty WAKAN.USR in the specified file }
procedure InitializeUserPackage(const package:string);
var tempDir: string;
  t:textfile;
begin
  tempDir := CreateRandomTempDir();
  CreateKnownList(1,0);
  SaveKnownList(1,tempDir+'\knownchar.bin');
  assignfile(t,tempDir+'\User.info');
  rewrite(t);
  writeln(t,'$TEXTTABLE');
  writeln(t,'$PREBUFFER');
  writeln(t,'$FIELDS');
  writeln(t,'iIndex');
  writeln(t,'sEnglish');
  writeln(t,'xPhonetic');
  writeln(t,'sPhoneticSort');
  writeln(t,'xKanji');
  writeln(t,'sAdded');
  writeln(t,'sPrinted');
  writeln(t,'sLearned');
  writeln(t,'sMastered');
  writeln(t,'iNoPrinted');
  writeln(t,'bScore');
  writeln(t,'bMaxScore');
  writeln(t,'$ORDERS');
  writeln(t,'Index_Ind');
  writeln(t,'Kanji_Ind');
  writeln(t,'Phonetic_Ind');
  writeln(t,'PhoneticSeek_Ind');
  writeln(t,'English_Ind');
  writeln(t,'Added_Ind');
  writeln(t,'Printed_Ind');
  writeln(t,'Score_Ind');
  writeln(t,'$SEEKS');
  writeln(t,'0');
  writeln(t,'Index');
  writeln(t,'Kanji+PhoneticSort');
  writeln(t,'PhoneticSort+Phonetic');
  writeln(t,'Phonetic');
  writeln(t,'English');
  writeln(t,'Added+PhoneticSort');
  writeln(t,'Printed+PhoneticSort');
  writeln(t,'Score+PhoneticSort');
  writeln(t,'$CREATE');
  closefile(t);
  assignfile(t,tempDir+'\UserIdx.info');
  rewrite(t);
  writeln(t,'$TEXTTABLE');
  writeln(t,'$PREBUFFER');
  writeln(t,'$FIELDS');
  writeln(t,'iWord');
  writeln(t,'xKanji');
  writeln(t,'lBegin');
  writeln(t,'$ORDERS');
  writeln(t,'Kanji_Ind');
  writeln(t,'$SEEKS');
  writeln(t,'0');
  writeln(t,'Kanji');
  writeln(t,'$CREATE');
  closefile(t);
  assignfile(t,tempDir+'\UserSheet.info');
  rewrite(t);
  writeln(t,'$TEXTTABLE');
  writeln(t,'$PREBUFFER');
  writeln(t,'$FIELDS');
  writeln(t,'iWord');
  writeln(t,'wNumber');
  writeln(t,'wPos');
  writeln(t,'$ORDERS');
  writeln(t,'Word_Ind');
  writeln(t,'Sheet_Ind');
  writeln(t,'$SEEKS');
  writeln(t,'0');
  writeln(t,'Word+Number+Pos');
  writeln(t,'Number+Pos');
  writeln(t,'$CREATE');
  closefile(t);
  assignfile(t,tempDir+'\UserCat.info');
  rewrite(t);
  writeln(t,'$TEXTTABLE');
  writeln(t,'$PREBUFFER');
  writeln(t,'$FIELDS');
  writeln(t,'iIndex');
  writeln(t,'sName');
  writeln(t,'bType');
  writeln(t,'sCreated');
  writeln(t,'$ORDERS');
  writeln(t,'Index_Ind');
  writeln(t,'Type_Ind');
  writeln(t,'Name_Ind');
  writeln(t,'Created_Ind');
  writeln(t,'$SEEKS');
  writeln(t,'0');
  writeln(t,'Index');
  writeln(t,'Type+Name');
  writeln(t,'Name');
  writeln(t,'Created+Name');
  writeln(t,'$CREATE');
  closefile(t);
  assignfile(t,tempDir+'\UserPrior.info');
  rewrite(t);
  writeln(t,'$TEXTTABLE');
  writeln(t,'$PREBUFFER');
  writeln(t,'$FIELDS');
  writeln(t,'xKanji');
  writeln(t,'wCount');
  writeln(t,'$ORDERS');
  writeln(t,'Kanji_Ind');
  writeln(t,'Count_Ind');
  writeln(t,'$SEEKS');
  writeln(t,'0');
  writeln(t,'Kanji');
  writeln(t,'Count');
  writeln(t,'$CREATE');
  closefile(t);
  WriteUserPackage(tempDir, package);
  DeleteDirectory(tempDir);
end;

{ Load user data package from the specified file }
function LoadUserPackage(const filename: string): TPackageSource;
var ps:TPackageSource;
  ms:TMemoryStream;
  tempDir: string;
  t:textfile;
begin
  ps:=TPackageSource.Create(filename,621030,587135,978312);
  TUser:=TTextTable.Create(ps,'User',false,false);
  TUserIdx:=TTextTable.Create(ps,'UserIdx',false,false);
  TUserSheet:=TTextTable.Create(ps,'UserSheet',false,false);
  TUserCat:=TTextTable.Create(ps,'UserCat',false,false);
  if ps['UserPrior.info']<>nil then
    TUserPrior:=TTextTable.Create(ps,'UserPrior',false,false)
  else
  begin
    tempDir := CreateRandomTempDir();
    assignfile(t,tempDir+'\UserPrior.info');
    rewrite(t);
    writeln(t,'$TEXTTABLE');
    writeln(t,'$PREBUFFER');
    writeln(t,'$FIELDS');
    writeln(t,'xKanji');
    writeln(t,'wCount');
    writeln(t,'$ORDERS');
    writeln(t,'Kanji_Ind');
    writeln(t,'Count_Ind');
    writeln(t,'$SEEKS');
    writeln(t,'0');
    writeln(t,'Kanji');
    writeln(t,'Count');
    writeln(t,'$CREATE');
    closefile(t);
    TUserPrior:=TTextTable.Create(nil,tempDir+'\UserPrior',false,false);
    DeleteDirectory(tempDir);
  end;
  TUserIndex:=TUser.Field('Index');
  TUserEnglish:=TUser.Field('English');
  TUserPhonetic:=TUser.Field('Phonetic');
  TUserPhoneticSort:=TUser.Field('PhoneticSort');
  TUserKanji:=TUser.Field('Kanji');
  TUserAdded:=TUser.Field('Added');
  TUserPrinted:=TUser.Field('Printed');
  TUserLearned:=TUser.Field('Learned');
  TUserMastered:=TUser.Field('Mastered');
  TUserNoPrinted:=TUser.Field('NoPrinted');
  TUserScore:=TUser.Field('Score');
  TUserMaxScore:=TUser.Field('MaxScore');
  TUserIdxWord:=TUserIdx.Field('Word');
  TUserIdxKanji:=TUserIdx.Field('Kanji');
  TUserIdxBegin:=TUserIdx.Field('Begin');
  TUserIdxIndex:=TUserIdx.Field('Index');
  TUserSheetWord:=TUserSheet.Field('Word');
  TUserSheetNumber:=TUserSheet.Field('Number');
  TUserSheetPos:=TUserSheet.Field('Pos');
  TUserCatIndex:=TUserCat.Field('Index');
  TUserCatName:=TUserCat.Field('Name');
  TUserCatType:=TUserCat.Field('Type');
  TUserCatCreated:=TUserCat.Field('Created');

  UserDataVersion := 0;
  try
    ms:=ps['struct.ver'].Lock;
    ms.Read(UserDataVersion,1);
    ps['struct.ver'].UnLock;
  except end;

  Result := ps;
end;

{ Saves currently loaded user data package to the specified directory and packs
 all the content of that directory into the package. }
procedure SaveUserPackage(const tempDir: string; const filename: string);
begin
  TUser.WriteTable(tempDir+'\User',false);
  TUserIdx.WriteTable(tempDir+'\UserIdx',false);
  TUserSheet.WriteTable(tempDir+'\UserSheet',false);
  TUserCat.WriteTable(tempDir+'\UserCat',false);
  TUserPrior.WriteTable(tempDir+'\UserPrior',false);
  WriteUserPackage(tempDir, filename);
end;

{ Free currently loaded user data package }
procedure FreeUserPackage;
begin
  FreeAndNil(TUser);
  FreeAndNil(TUserIdx);
  FreeAndNil(TUserSheet);
  FreeAndNil(TUserCat);
end;

function UserDataAutoRepair(): boolean;
begin
  Result:=false;
  if not TUser.CheckIndex then begin
    TUser.Reindex;
    Result:=true;
  end;
  if not TUserIdx.CheckIndex then begin
    TUserIdx.Reindex;
    Result:=true;
  end;
  if not TUserSheet.CheckIndex then begin
    TUserSheet.Reindex;
    Result:=true;
  end;
  if not TUserCat.CheckIndex then begin
    TUserCat.Reindex;
    Result:=true;
  end;
end;

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

procedure RebuildUserIndex;
begin
  TUserIdx.First;
  while not TUserIdx.EOF do
  begin
    if not TUser.Locate('Index',TUserIdx.TrueInt(TUserIdxWord)) then
      TUserIdx.Delete;
    TUserIdx.Next;
  end;
end;


{
Phonetic sort order
}

var
  romasortl: array of record //romaji sort order
    roma: FString;
    order: string; //although it's integer inside
  end;

procedure ClearRomaSortRecords;
begin
  SetLength(romasortl, 0);
end;

procedure AddRomaSortRecord(const s: string);
var parts: TStringArray;
  i: integer;
begin
  parts := SplitStr(s, 2);
  if Length(parts)<=0 then exit;
  i := Length(romasortl);
  SetLength(romasortl, i+1);
  romasortl[i].roma := hextofstr(parts[0]);
  if Length(parts)>=2 then
    romasortl[i].order := parts[1]
  else
    romasortl[i].order := '';
end;

//Returns a string which will represent this phonetic in sorting in User dictionary.
//Normal dictionaries don't use this.
function GetPhoneticSortStr(const phonetic: FString;const lang: char): string;
var s: FString;
  s2: FChar;
  a1,a2: string;
  i, j: integer;
begin
  if lang='j'then
  begin
   //Reconvert to some standard format and then use a preset table of
   //katakana syllable weights
    Result:='';
    s:=RomajiToKana('H'+KanaToRomaji(phonetic,1,'j'),1,'j',[rfDeleteInvalidChars]);
    for i:=1 to flength(s) do
    begin
      s2:=fgetch(s,i);
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



initialization
  UserDataVersion := 0;
  TUser := nil;
  TUserIdx := nil;
  TUserSheet := nil;
  TUserCat := nil;
  ClearRomaSortRecords();

end.
