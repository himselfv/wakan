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

 { Index of kanji usage in vocabulary words. Used by KanjiCompounds search }
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

 { Usage priorities for kanji. The more the user accepts certain kanji suggestions,
  the higher those suggestions appear in the list }
  TUserPrior: TTextTable;

procedure InitializeUserPackage(const package:string);
function LoadUserPackage(const filename: string): TPackageSource;
procedure SaveUserPackage(const tempDir: string; const filename: string);
procedure FreeUserPackage;

function UserDataAutoRepair(): boolean;

function FindUserWord(kanji,phonetic: FString): integer;

procedure RebuildUserIndex;

//Increase character priority a bit (will be sorted before others with less priority)
procedure IncCharPriority(const char: FChar);


implementation
uses SysUtils, JWBCore, JWBUnit, KanaConv, PKGWrite, JWBCategories, Classes;

{ Packs WAKAN.USR data from directory Dir to package Package.
 Do not use directly; there are functions to save and load user data packages. }
procedure WriteUserPackage(const dir:string;const package:string);
var pack: TPackageBuilder;
  f:file of byte;
  b:byte;
begin
  assignfile(f,dir+'\struct.ver');
  rewrite(f);
  b:=CurrentUserDataVersion;
  write(f,b);
  closefile(f);

  pack := TPackageBuilder.Create;
  try
    pack.PackageFile := package;
    pack.MemoryLimit := 100000000;
    pack.Name := 'WaKan User Data';
    pack.TitleName := 'WaKan User Data File';
    pack.CopyrightName := WakanCopyright;
    pack.FormatName := 'Pure Package File';
    pack.CommentName := 'File is used by '+WakanAppName;
    pack.VersionName := '1.0';
    pack.HeaderCode := 621030;
    pack.FilesysCode := 587135;
    pack.WriteHeader;
    pack.LoadMode := lmTemporaryLoad;
    pack.CryptMode := 0;
    pack.CrcMode := 0;
    pack.PackMode := 0;
    pack.CryptCode := 978312;
    pack.Include(dir);
    pack.Finish;
  finally
    FreeAndNil(pack);
  end;

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
begin
  ps:=TPackageSource.Create(filename,621030,587135,978312);

  TUser:=TTextTable.Create(ps,'User',false,false);
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
  TUser.IsAutoIncField[TUserIndex] := true;

  TUserIdx:=TTextTable.Create(ps,'UserIdx',false,false);
  TUserIdxWord:=TUserIdx.Field('Word');
  TUserIdxKanji:=TUserIdx.Field('Kanji');
  TUserIdxBegin:=TUserIdx.Field('Begin');
  TUserIdxIndex:=TUserIdx.Field('Index');

  TUserSheet:=TTextTable.Create(ps,'UserSheet',false,false);
  TUserSheetWord:=TUserSheet.Field('Word');
  TUserSheetNumber:=TUserSheet.Field('Number');
  TUserSheetPos:=TUserSheet.Field('Pos');

  TUserCat:=TTextTable.Create(ps,'UserCat',false,false);
  TUserCatIndex:=TUserCat.Field('Index');
  TUserCatName:=TUserCat.Field('Name');
  TUserCatType:=TUserCat.Field('Type');
  TUserCatCreated:=TUserCat.Field('Created');
  TUserCat.IsAutoIncField[TUserCatIndex] := true;

  if ps['UserPrior.info']<>nil then
    TUserPrior:=TTextTable.Create(ps,'UserPrior',false,false)
  else
    TUserPrior:=TTextTable.Create([
      '$TEXTTABLE',
      '$PREBUFFER',
      '$FIELDS',
      'xKanji',
      'wCount',
      '$ORDERS',
      'Kanji_Ind',
      'Count_Ind',
      '$SEEKS',
      '0',
      'Kanji',
      'Count'
    ]);

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


procedure IncCharPriority(const char: FChar);
begin
  if TUserPrior.Locate('Kanji', char) then
    TUserPrior.Edit([TUserPrior.Field('Count')],[inttostr(TUserPrior.Int(TUserPrior.Field('Count'))+1)])
  else
    TUserPrior.Insert([char,'1']);
end;


initialization
  UserDataVersion := 0;
  TUser := nil;
  TUserIdx := nil;
  TUserSheet := nil;
  TUserCat := nil;

end.
