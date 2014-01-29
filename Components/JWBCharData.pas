unit JWBCharData;
{
Kanji info database handling.
Related files:
  WAKAN.CHR -- contains static character information
  WAKAN.CFG/[CharInfo] -- describes all pieces of information that can be had about kanji
  WAKAN.CDT -- user configuration for which kanji info to display
}

interface
uses SysUtils, Classes, TextTable, JWBStrings;

type
  ECharDataException = class(Exception);

const
 {
  Current WAKAN.CHR format. Version history:
    0  version information not found
    ...
    7  current version
  No attempts at upgrading are being done as of now.
  (The solution is always to download new version of file.)
 }
  CurrentCharDataVersion=7;

var
 { Database props -- populated on load }
  CharDataVersion: integer;
  CharDataProps: record
   { None of these fields are really relevant:
    1. Wakan.chr can be rebuilt/updated at any time.
    2. The concept of "version date" does not apply to KANJIDIC anymore as there
    could be different language versions with different update schedules.
    3. Unihan is problematic too as it's several files instead of one, which one
    we consider correct?
    So version fields are purely decorative. Do not use for any checks. }
    DicBuildDate: integer; //DateOf(TDatetime) when this dictionary was LAST updated
    KanjidicVersion: string; //file write time of LAST kanjidic imported into wakan.chr
    UnihanVersion: string;   //file write time of LAST unihan imported into wakan.chr
    ChinesePresent:boolean;  //true if unihan was imported at least once
  end;

 { Tables and fields }
  TChar: TTextTable;
  TCharIndex,                 //char index
  TCharChinese,               //1 if char is chinese-only (UNIHAN), 0 if both japanese/chinese (KANJIDIC+UNIHAN)
  TCharType,
   { 'A', 'N', 'T', 'S', 'J'
    Set according to whether the character has kBigFive/kGB0 assignments in UNIHAN:
        jp/chin               'J'
        jp/chin + kBigFive    'A'
        jp/chin + kGB0        'A'
        chin                  'N'
        chin + kBigFive       'T'
        chin + kGB0           'S'
    }
  TCharUnicode,               //char itself
  TCharStrokeCount,           //255 if unknown
  TCharJpStrokeCount,         //from kanjidic, 255 if unknown
  TCharJpFrequency,           //from kanjidic, 65535 if unknown
  TCharChFrequency,           //255 if unknown
  TCharJouyouGrade: integer;  //from kanjidic, 255 if unknown

  TCharProp: TTextTable;
  TCharPropIndex,
   { Absolute index through all the records. Not used anywhere but kept
    for compability with older Wakans/chardb-s. }
  TCharPropKanji,
   { Index from TChar table }
  TCharPropTypeId,
   { Property type ID in the offline type table (from wakan.cfg).
    Some property types can have several entries for a kanji. }
  TCharPropValue, {
    Property value. WARNING! This field here is dangerous.
    Database column format is AnsiString, but some properties store 4-char hex
    unicode. Default auto-conversion therefore does not work.
    When reading from, or searching by this field, you have to check its
    property type:
       type := CCharRead.Int(TCharPropTypeId);
       dataType := FindCharPropType(type).dataType;
    And then recode data (see TCharPropType.dataType field comments)
    TCharPropertyCursor below has this implemented, just use Value().
   }
  TCharPropReadDot,   // if there was a dot in the reading, this is it's position (the dot is removed)
  TCharPropPosition   // orders the properties of the same type
    : integer;

  TRadicals: TTextTable;
  TRadicalsNumber,
  TRadicalsVariant,
  TRadicalsUnicode,
  TRadicalsStrokeCount,
  TRadicalsUnicodeCount,
  TRadicalsBushuCount,
  TRadicalsJapaneseCount,
  TRadicalsKangXiCount: integer;

procedure LoadCharData(const filename: string);
procedure SaveCharData(const filename: string);
procedure FreeCharData();
procedure ClearCharDbProps; //when rebuilding

{ Creates new empty wakan.chr in a specified file }
procedure InitializeCharPackage(const package:string);

{ Create empty tables of proper format }
function NewCharTable: TTextTable;
function NewCharPropTable: TTextTable;
function NewRadicalsTable: TTextTable;

{ Initialize TChar* field numbers above with values from current tables.
 Separate procedures because sometimes we build all of this in memory. }
procedure SetupCharTable();
procedure SetupCharPropTable();
procedure SetupRadicalsTable();

{ Fixes for old format pecularities -- see comments to functions  }
procedure FixTCharJpUnicodeIndex(table: TTextTable);
procedure FixTCharJpStrokeOrderIndex(table: TTextTable);

{ Used for .wtt, .jtt header timestamps, for CharDataProps.*Version fields etc.  }
function WakanDatestamp(const dt: TDatetime): string;

{
Character property type information.
}
type
  TCharPropType = record
    id: integer;
    superceded_by: integer; //-1 if not obsolete
    supercedes: integer;
    sourceType: char;
     { Where did we get that info
        'D': KANJIDIC
        'U': UNIHAN }
    sourceField: string;
    dataType: char;
     { Controls how data for this property is stored and handled
        'U': unicode, 4-hex string (sometimes with ansi '+' or '-' before or after it -- see parsing)
        'P': pinyin, AnsiString
        'R': radical, AnsiString
          - in some cases (see parsing), TRadicals.Number field value, possibly
           int or 'int' (int in quotes).
        'N': 'Number' (?), AnsiString
        'T': ?, AnsiString
        'S': 'String', AnsiString }
    englishName: string;
    description: string; // in english
    function Obsolete: boolean;
  end;
  PCharPropType = ^TCharPropType;

var
 { All possible pieces of information to store about Characters and to display
  in KanjiDetails info box. }
  CharPropTypes: array of TCharPropType;

const
 { Some predefined property type IDs. These cannot be changed at a later date,
  only declared obsolete. }
  ptKoreanReading = 1;
  ptMandarinReading = 2;
  ptJapaneseDefinition = 3;
  ptOnReading = 4;
  ptKunReading = 5;
  ptNanoriReading = 6;
  ptChineseDefinition = 7; //usually taken from UNIHAN
  ptCantoneseReading = 8;
  ptRadicals = 10; //virtual property

  ptBushuRadical = 12;
  ptRSUnicode = 13;
  ptRSJapanese = 14;
  ptRSKanWa = 15;
  ptRSKangXi = 16;
  ptRSKorean = 17;

  ptSKIP = 22;

  ptJapaneseDefinitionUnicode = 121;
  ptChineseDefinitionUnicode = 122;
  ptClassicalRadical = 123;

 { ptRadicals internally has a special treatment and so we keep the list of all
  properties which go into it }
  ptRadicalsComposition = [ptBushuRadical,ptRSUnicode,ptRSJapanese,ptRSKanWa,
    ptRSKangXi,ptRSKorean];

procedure AddCharPropType(const str: string);
function FindCharPropType(const propTypeId: integer): PCharPropType; overload;
function FindCharPropTypeIndex(const propTypeId: integer): integer;
function FindCharPropType(const ASource: char; const AField: string): PCharPropType; overload;


{
User configuration for KanjiDetails info box -- stored in WAKAN.CDT
}
var
  chardetl:TStringList;

function GetCharDet(i,j:integer):string;


{
CharPropertyCursor
}

const
  NoRadical = 65535;

type
  TCharPropertyCursor = class(TTextTableCursor)
  protected
    CRadicals: TTextTableCursor;
  public
    constructor Create(ATable: TTextTable);
    destructor Destroy; override;
    function Locate(kanjiIndex,propType: integer): boolean; overload;
    function PropType: PCharPropType;
    function RawValue: string;
    function Value: FString;
    function AsRadicalNumber: integer;
    function AsRadicalCharOnly: FString;

  public
   { Uses Cursor to enumerate over a certain character properties.
    These break the current position. }
    function GetCharValues(kanjiIndex,propType: integer; const sep: FString={$IFDEF UNICODE}', '{$ELSE}$002C$0020{$ENDIF}):FString;
    function GetJapaneseDefinitions(kanjiIndex: integer; const sep: string=', '): FString;
    function GetChineseDefinitions(kanjiIndex: integer; const sep: string=', '): FString;

  end;


{
Shortcuts for getting character property values
}

function GetCharValue(index,propType:integer):string;
function GetCharValueRad(index,propType:integer):integer;

function RadicalUnicode(const radno: integer): FString;
function RadicalIndex(const rad: FChar): integer;

implementation
uses MemSource, PKGWrite, JWBUnit;

{
Database
}

procedure LoadCharData(const filename: string);
var ps: TPackageSource;
  ms: TMemoryStream;
  vi: TStringList;
begin
  FreeCharData; //just in case

  ps:=TPackageSource.Create(filename,791564,978132,978123);
  try
    vi:=TStringList.Create;
    try
      ms:=ps['jalet.ver'].Lock;
      vi.LoadFromStream(ms);
      ps['jalet.ver'].Unlock;

      if (vi[0]<>'JALET.DIC') and (vi[0]<>'JALET.CHR') then
        raise Exception.Create('Unknown DICT.VER header.');
      CharDataVersion := strtoint(vi[1]);
      if CharDataVersion<CurrentCharDataVersion then
        raise ECharDataException.Create('#00354^eWAKAN.CHR has old structure. '
          +'Please download new version.'#13#13'Application will now exit.')
      else
      if CharDataVersion>CurrentCharDataVersion then
        raise ECharDataException.Create('#00355^eWAKAN.CHR has newer structure. '
          +'Please download new WAKAN.EXE.'#13#13'Application will now exit.');

      CharDataProps.DicBuildDate:=strtoint(vi[2]);
      CharDataProps.KanjidicVersion:=vi[4];
      CharDataProps.UnihanVersion:=vi[5];
      CharDataProps.ChinesePresent:=vi[6]='CHINESE';
    finally
      vi.Free;
    end;

    TChar:=TTextTable.Create(ps,'Char',true,false);
    SetupCharTable();

    TCharProp:=TTextTable.Create(ps,'CharRead',true,false); //sic. 'CharRead' for compat. reasons
    SetupCharPropTable();

    TRadicals:=TTextTable.Create(ps,'Radicals',true,false);
    SetupRadicalsTable();
  finally
    FreeAndNil(ps);
  end;
end;

procedure SetupCharTable;
begin
  TCharIndex:=TChar.Field('Index');
  TCharChinese:=TChar.Field('Chinese');
  TCharType:=TChar.Field('Type');
  TCharUnicode:=TChar.Field('Unicode');
  TCharStrokeCount:=TChar.Field('StrokeCount');
  TCharJpStrokeCount:=TChar.Field('JpStrokeCount');
  TCharJpFrequency:=TChar.Field('JpFrequency');
  TCharChFrequency:=TChar.Field('ChFrequency');
  TCharJouyouGrade:=TChar.Field('JouyouGrade');
  TChar.IsAutoIncField[TCharIndex]:=true;
end;

procedure SetupCharPropTable;
begin
  TCharPropIndex:=TCharProp.Field('Index');
  TCharPropKanji:=TCharProp.Field('Kanji');
  TCharPropTypeId:=TCharProp.Field('Type');
  TCharPropValue:=TCharProp.Field('Reading'); //sic. 'Reading'
  TCharPropReadDot:=TCharProp.Field('ReadDot');
  TCharPropPosition:=TCharProp.Field('Position');
  TCharProp.IsAutoIncField[TCharPropIndex]:=true;
end;

procedure SetupRadicalsTable;
begin
  TRadicalsNumber:=TRadicals.Field('Number');
  TRadicalsVariant:=TRadicals.Field('Variant');
  TRadicalsUnicode:=TRadicals.Field('Unicode');
  TRadicalsStrokeCount:=TRadicals.Field('StrokeCount');
  TRadicalsUnicodeCount:=TRadicals.Field('UnicodeCount');
  TRadicalsBushuCount:=TRadicals.Field('BushuCount');
  TRadicalsJapaneseCount:=TRadicals.Field('JapaneseCount');
  TRadicalsKangXiCount:=TRadicals.Field('KangXiCount');
end;

{ Packs WAKAN.CHR data from directory Dir to package Package.
 Do not use directly; there are functions to save and load user data packages. }
procedure WriteCharPackage(const dir:string;const package:string);
var pack: TPackageBuilder;
begin
  pack := TPackageBuilder.Create;
  try
    pack.PackageFile := package;
    pack.MemoryLimit := 100000000;
    pack.Name := 'WaKan User Data';
    pack.TitleName := 'WaKan Character Dictionary';
    pack.CopyrightName := WakanCopyright;
    pack.FormatName := 'Pure Package File';
    pack.CommentName := 'File is used by '+WakanAppName;
    pack.VersionName := '1.0';
    pack.HeaderCode := 791564;
    pack.FilesysCode := 978132;
    pack.WriteHeader;
    pack.LoadMode := lmTemporaryLoad;
    pack.CryptMode := 0;
    pack.CrcMode := 0;
    pack.PackMode := 0;
    pack.CryptCode := 978123;
    pack.Include(dir);
    pack.Finish;
  finally
    FreeAndNil(pack);
  end;
end;

{ Creates new empty WAKAN.CHR in the specified file.
 Still relies on CharDataProps for some header data, might want to remove this
 in the future (init everything to the blank or something) }
procedure InitializeCharPackage(const package:string);
var tempDir: string;
  vi: TStringList;
begin
  tempDir := CreateRandomTempDir();

  vi:=TStringList.Create;
  try
    vi.Add('JALET.CHR');
    vi.Add(IntToStr(CurrentCharDataVersion));
    vi.Add(IntToStr(CharDataProps.DicBuildDate));
    vi.Add('');
    vi.Add(CharDataProps.KanjidicVersion);
    vi.Add(CharDataProps.UnihanVersion);
    if CharDataProps.ChinesePresent then
      vi.Add('CHINESE')
    else
      vi.Add('');
    vi.SaveToFile(tempDir+'\jalet.ver');
  finally
    vi.Free;
  end;

  with NewCharTable() do begin
    WriteTable(tempDir+'\Char.info', false);
    Free;
  end;

  with NewCharPropTable() do begin
    WriteTable(tempDir+'\CharRead.info', false);
    Free;
  end;

  with NewRadicalsTable() do begin
    WriteTable(tempDir+'\Radicals.info', false);
    Free;
  end;

  WriteCharPackage(tempDir, package);
  DeleteDirectory(tempDir);
end;

function NewCharTable: TTextTable;
var TIndex: integer;
begin
  Result := TTextTable.Create([
    '$TEXTTABLE',
    '$PRECOUNTED',
    '$RAWINDEX',
    '$FIELDS',
    'iIndex',
    'bChinese',
    'sType',
    'xUnicode',
    'bStrokeCount',
    'wJpFrequency',
    'wChFrequency',
    'bJouyouGrade',
    'bJpStrokeCount',
    '$ORDERS',
    'ChFrequency_Ind',
    'ChStrokeCount_Ind',
    'ChUnicode_Ind',
    'JpFrequency_Ind',
    'JpStrokeCount_Ind',
    'JpUnicode_Ind',
    '$SEEKS',
    'Index',
    'ChFrequency',
    'StrokeCount',
    'Unicode',
    'JpFrequency',
    'Chinese+JpStrokeCount',
    'Chinese+Unicode'
  ]);
  TIndex := Result.GetFieldIndex('Index');
  Result.IsAutoIncField[TIndex] := true;
end;

function NewCharPropTable: TTextTable;
var TIndex: integer;
begin
  Result := TTextTable.Create([
    '$TEXTTABLE',
    '$PRECOUNTED',
    '$RAWINDEX',
    '$FIELDS',
    'iIndex', //order of addition when rebuilding
    'wKanji',
    'bType',
    'sReading',
    'bReadDot',
    'bPosition',
    '$ORDERS',
    'Kanji',
    'Reading_Ind',
    'Type_Ind',
    '$SEEKS',
    '0',
   { Older tables have Kanji $ORDER but no Kanji $SEEK definition.
    They rely on records being stored in Kanji+Type+Index order internally.
    Therefore TCharProp has to be rebuilt from scratch in the Kanji+Type+Index order
    after making any changes to it. }
    'Kanji+Type+Index',
    'Reading',
    'Type'
  ]);
  TIndex := Result.GetFieldIndex('Index');
  Result.IsAutoIncField[TIndex] := true;
end;

function NewRadicalsTable: TTextTable;
begin
  Result := TTextTable.Create([
    '$TEXTTABLE',
    '$PRECOUNTED',
    '$RAWINDEX',
    '$FIELDS',
    'bNumber',
    'bVariant',
    'xUnicode',
    'bStrokeCount',
    'wBushuCount',
    'wUnicodeCount',
    'wJapaneseCount',
    'wKangXiCount',
    '$ORDERS',
    '$SEEKS',
    'Number'
  ]);
end;

//NOTE: This cannot at this time be used to create a new character database.
//  It only works if we already have the data loaded. All the header info
//  (dic version, build date) is preserved.
procedure SaveCharData(const filename: string);
var tempDir: string;
  vi: TStringList;
begin
  tempDir := CreateRandomTempDir();

  vi:=TStringList.Create;
  try
    vi.Add('JALET.CHR');
    vi.Add(IntToStr(CharDataVersion));
    vi.Add(IntToStr(CharDataProps.DicBuildDate));
    vi.Add('');
    vi.Add(CharDataProps.KanjidicVersion);
    vi.Add(CharDataProps.UnihanVersion);
    if CharDataProps.ChinesePresent then
      vi.Add('CHINESE')
    else
      vi.Add('');
    vi.SaveToFile(tempDir+'\jalet.ver');
  finally
    vi.Free;
  end;

  TChar.WriteTable(tempDir+'\Char',false);
  TCharProp.WriteTable(tempDir+'\CharRead',false);
  TRadicals.WriteTable(tempDir+'\Radicals',false);
  WriteCharPackage(tempDir, filename);
  DeleteDirectory(tempDir)
end;

//Clears database properties after unloading, as a safety measure
procedure ClearCharDbProps;
begin
  CharDataVersion := 0;
  CharDataProps.DicBuildDate := 0;
  CharDataProps.KanjidicVersion := '';
  CharDataProps.UnihanVersion := '';
  CharDataProps.ChinesePresent := false;
end;

procedure FreeCharData();
begin
  FreeAndNil(TChar);
  FreeAndNil(TCharProp);
  FreeAndNil(TRadicals);
  ClearCharDbProps();
end;


{ Older TChar tables don't have matching $SEEK defintion for JpUnicode_Ind $ORDER.
 Definitions are required or you cannot add entries to the index. }
procedure FixTCharJpUnicodeIndex(table: TTextTable);
var ord_i: integer;
begin
  if not table.Orders.Find('JpUnicode_Ind', ord_i) then
    raise Exception.Create('FixTCharJpUnicodeIndex(): JpUnicode_Ind order not found.');
  if ord_i<table.seeks.Count-1 then exit; //has matching $SEEK
  if ord_i<table.seeks.Count-2 then
    raise Exception.Create('FixTCharJpUnicodeIndex(): Other index defintions missing!');
   //We can amend for only one
  table.Seeks.Add('Chinese+Unicode');
end;

{ Older TChar tables have a bug in JpStrokeCount $SEEK definition: it is defined
 simply as 'JpStrokeCount' but in fact built as 'Chinese+JpStrokeCount':
    JapaneseChar       01
    JapaneseChar       04
    ...
    JapaneseChar       33
    ChineseOnlyChar    00
    ChineseOnlyChar    00
 All chinese-only characters have JpStrokeCount of 0 but go AFTER the japanese ones.

 This ordering is crucial so that you can SetOrder('JpStrokeCount') and just
 enumerate from First() to Last() and chinese characters will be at the end.

 But the definition has to be corrected, because simply 'JpStrokeCount' gives
 you different order where Chinese-only characters go first. }
procedure FixTCharJpStrokeOrderIndex(table: TTextTable);
var ord_i: integer;
begin
  if not table.Orders.Find('JpStrokeCount_Ind', ord_i) then
    raise Exception.Create('FixTCharJpStrokeOrderIndex(): JpStrokeCount_Ind order not found.');
  if ord_i>table.Seeks.Count-2 then
    raise Exception.Create('FixTCharJpStrokeOrderIndex(): Seek definition not found');
  table.Seeks.Delete(ord_i+1);
  table.Seeks.Insert(ord_i+1,'Chinese+JpStrokeCount');
end;


//Formats datetime in a Wakan "version" format (14AUG05)
function WakanDatestamp(const dt: TDatetime): string;
var fs: TFormatSettings;
begin
 {$IF CompilerVersion>=22}
  fs := TFormatSettings.Create('en-us');
 {$ELSE}
 //older compilers only have obsolete function
  GetLocaleFormatSettings($0409, fs);
 {$IFEND}
  Result := AnsiUpperCase(FormatDatetime('ddmmmyy',dt,fs));
end;



{
Pieces of information about Kanji
}

function TCharPropType.Obsolete: boolean;
begin
  Result := (superceded_by >= 0);
end;

{ Adds a char property type info from parsing a wakan.cfg style string }
procedure AddCharPropType(const str: string);
var tmp: string;
  parts: TStringArray;
  pt: TCharPropType;
  idx: integer;
begin
  tmp := str;
  parts := SplitStr(str, 7);
  if Length(parts)<5 then
    raise Exception.Create('AddCharPropType: invalid property type info line format');

  if not TryStrToInt(parts[0], pt.id) then
    raise Exception.Create('AddCharPropType: invalid integer ID: '+parts[0]);

  if parts[1]='' then
    pt.superceded_by := -1
  else
  if not TryStrToInt(parts[1], pt.superceded_by) then
    raise Exception.Create('AddCharPropType: invalid integer superceded_by: '+parts[0]);

  if pt.superceded_by>0 then begin //0 is no man's land, The Separator
   //For now we don't support multi-obsolete-superceding
    idx := FindCharPropTypeIndex(pt.superceded_by);
    if idx>=0 then
      CharPropTypes[idx].supercedes := pt.id;
  end;

  pt.supercedes := -1;
  for idx := 0 to Length(CharPropTypes) - 1 do
    if CharPropTypes[idx].superceded_by=pt.id then begin
      pt.supercedes := CharPropTypes[idx].id;
      break;
    end;

  if Length(parts[2])<>1 then
    raise Exception.Create('AddCharPropType: invalid source: '+parts[1]);
  pt.sourceType := parts[2][1];
  pt.sourceField := parts[3];

  if Length(parts[4])<>1 then
    raise Exception.Create('AddCharPropType: invalid data type: '+parts[4]);
  pt.dataType := parts[4][1];

  if Length(parts)>=6 then
    pt.englishName := parts[5];
  if Length(parts)>=7 then
    pt.description := parts[6];

  SetLength(CharPropTypes, Length(CharPropTypes)+1);
  CharPropTypes[Length(CharPropTypes)-1] := pt;
end;

function FindCharPropType(const propTypeId: integer): PCharPropType;
var i: integer;
begin
  Result := nil;
  for i := 0 to Length(CharPropTypes) - 1 do
    if propTypeId=CharPropTypes[i].id then begin
      Result := @CharPropTypes[i];
      break;
    end;
end;

function FindCharPropTypeIndex(const propTypeId: integer): integer;
var i: integer;
begin
  Result := -1;
  for i := 0 to Length(CharPropTypes) - 1 do
    if propTypeId=CharPropTypes[i].id then begin
      Result := i;
      break;
    end;
end;

{ Locates property type which stores data from ASource:AField }
function FindCharPropType(const ASource: char; const AField: string): PCharPropType;
var i: integer;
begin
  Result := nil;
  for i := 0 to Length(CharPropTypes) - 1 do
    if not CharPropTypes[i].Obsolete
    and (ASource=CharPropTypes[i].sourceType)
    and (AField=CharPropTypes[i].sourceField) then begin
      Result := @CharPropTypes[i];
      break;
    end;
end;


{
User configuration for KanjiDetails info box
}

function GetCharDet(i,j:integer):string;
var s:string;
begin
  s:=chardetl[i];
  while j>0 do
  begin
    delete(s,1,pos(';',s));
    dec(j);
  end;
  delete(s,pos(';',s),length(s)-pos(';',s)+1);
  result:=s;
end;


{
TCharPropertyCursor
}

constructor TCharPropertyCursor.Create(ATable: TTextTable);
begin
  inherited;
  CRadicals := TRadicals.NewCursor;
end;

destructor TCharPropertyCursor.Destroy;
begin
  FreeAndNil(CRadicals);
  inherited;
end;

{ Locates first entry of a property for a given kanji.
 Note that there's no telling how properties are ordered so Next() property
 may not be of the same type. }
function TCharPropertyCursor.Locate(kanjiIndex,propType: integer): boolean;
begin
  Result := false;
  Self.SetOrder('');
  if not Self.Locate('Kanji',kanjiIndex) then
    exit;
  while (not Self.EOF) and (Self.Int(TCharPropKanji)=kanjiIndex) do
  begin
    if Self.Int(TCharPropTypeId)=propType then
    begin
      Result := true;
      break;
    end;
    Self.Next;
  end;
end;

function TCharPropertyCursor.PropType: PCharPropType;
var propTypeId: integer;
begin
  propTypeId := Self.Int(TCharPropTypeId);
  Result := FindCharPropType(propTypeid);
end;

{ Raw Ansi data from Value field.
 It's returned as string because DB reads as string at the lowest level and
 there's no need to convert it again. }
function TCharPropertyCursor.RawValue: string;
begin
  Result := Self.Str(TCharPropValue);
end;

{ Property values are stored as AnsiStrings, as far as database is concerned.
For property types which support this:
 - Decodes to FString from explicit hex or encodes from Ansi
 - Add dot at the specified position
 - Replace special markers such as +/i with full-width equivalents  }
function TCharPropertyCursor.Value: FString;
var propDataType: char;
  s_str: string;
  dotPosition: integer;
  adddot:integer;
begin
  propDataType := Self.PropType.dataType;

 { Different property types have data in different formats! }
  if propDataType='R' then
  begin

   { R-type properties contain radical index - maybe in extended Unihan format.
    Nothing to parse here. }
    Result := Self.Str(TCharPropValue);

  end else
 { 'U' has reading in 'a'-type hex }
  if (propDataType='U') then
  begin
    s_str:=Self.Str(TCharPropValue);
    if not (propType.id in [4..6]) then begin //Rest don't need special treatment
      Result:=hextofstr(s_str);
      exit;
    end;

   //'+' and '-' seem to be not used in modern char DBs
    dotPosition := self.Int(TCharPropReadDot);
    adddot:=0;
    Result:='';
    if s_str[1]='+' then
    begin
      Result:={$IFDEF UNICODE}#$FF0B{$ELSE}'FF0B'{$ENDIF};
      System.delete(s_str,1,1);
      adddot:=1;
    end;
    if s_str[1]='-' then
    begin
      Result:=Result+{$IFDEF UNICODE}#$FF0D{$ELSE}'FF0D'{$ENDIF};
      System.delete(s_str,1,1);
      adddot:=1;
    end;
    if dotPosition>0 then
    begin
      Result:=Result+hextofstr(copy(s_str,1,dotPosition-1-adddot));
      Result:=Result+{$IFDEF UNICODE}#$FF0E{$ELSE}'FF0E'{$ENDIF};
      fdelete(s_str,1,dotPosition-1-adddot);
    end;
    if s_str[length(s_str)]='-' then
      Result:=Result+hextofstr(copy(s_str,1,length(s_str)-1))+{$IFDEF UNICODE}#$FF0D{$ELSE}'FF0D'{$ENDIF}
    else
      Result:=Result+hextofstr(s_str);

  end else
 { Rest is read as it is }
  begin
    Result := Self.Str(TCharPropValue);
  end;

end;

{ Returns the value of current property as if it was a radical index.
 In case of extended indexes (radical.stroke_count) returns only the radical. }
function TCharPropertyCursor.AsRadicalNumber: integer;
var propType: PCharPropType;
  s_str: string;
  i: integer;
begin
  propType := Self.PropType;
  if propType.dataType<>'R' then
    raise Exception.Create('AsRadical(): not a radical property');
  s_str := Self.RawValue; //should be ansi string

 { Parse extended radical format -- see JWBUnihanImport }
  i := pos('.',s_str);
  if i>0 then System.Delete(s_str,i,MaxInt);
  if (length(s_str)>0) and (s_str[length(s_str)]='''') then System.Delete(s_str,length(s_str),1);
  if (s_str='') or not TryStrToInt(s_str, Result) then
    raise Exception.Create('AsRadical: invalid radical data ('+Self.RawValue+')');
end;

{ Returns the value of current property as if it was a radical index.
 In case of extended indexes (radical.stroke_count) returns only the radical. }
function TCharPropertyCursor.AsRadicalCharOnly: FString;
var i: integer;
begin
  i := AsRadicalNumber;
  if not CRadicals.Locate('Number',i) then
    raise Exception.Create('Invalid radical number value: '+IntToStr(i));
  Result := CRadicals.Str(TRadicalsUnicode);
end;


{ Returns a list of all property values for the given character and property type.
 This is the preferred function to return "property value for a type" }
function TCharPropertyCursor.GetCharValues(kanjiIndex, propType:integer; const sep: FString):FString;
var Match: boolean;
  curval: FString;
begin
  Result := '';
  Self.SetOrder('');
  if Self.Locate('Kanji',kanjiIndex) then
  while (not Self.EOF) and (Self.Int(TCharPropKanji)=kanjiIndex) do
  begin
    if propType=ptRadicals then
      Match := Self.Int(TCharPropTypeId) in ptRadicalsComposition
    else
      Match := Self.Int(TCharPropTypeId)=propType;
    if Match then begin
      if propType=ptRadicals then
        curval := Self.AsRadicalCharOnly
      else
        curval := Self.Value;
      if (propType<>ptRadicals) or (pos(curval,Result)<=0) then
        if Result<>'' then
          Result := Result + sep + curval
        else
          Result := curval;
    end;
    Self.Next;
  end;
end;

function TCharPropertyCursor.GetJapaneseDefinitions(kanjiIndex: integer; const sep: string): FString;
begin
 //Newer versions sometimes have both Unicode and non-Unicode fields for compability
  Result := GetCharValues(kanjiIndex, ptJapaneseDefinitionUnicode, fstr(sep));
  if Result='' then
    Result := GetCharValues(kanjiIndex, ptJapaneseDefinition, sep);
end;

function TCharPropertyCursor.GetChineseDefinitions(kanjiIndex: integer; const sep: string): FString;
begin
  Result := GetCharValues(kanjiIndex, ptChineseDefinition, sep);
end;


{ Following functions are kept for backward compability and simplicity,
 but if you do a lot of these, using a separate cursor is preferred. }

function GetCharValue(index,propType:integer):string;
var CCharProp: TCharPropertyCursor;
begin
  CCharProp := TCharPropertyCursor.Create(TCharProp);
  try
    if CCharProp.Locate(index,propType) then
      Result := CCharProp.RawValue
    else
      Result := '';
  finally
    FreeAndNil(CCharProp);
  end;
end;

{ May return NoRadical if the radical isn't defined. }
function GetCharValueRad(index,propType:integer):integer;
var CCharProp: TCharPropertyCursor;
begin
  CCharProp := TCharPropertyCursor.Create(TCharProp);
  try
    if CCharProp.Locate(index,propType) then
      Result := CCharProp.AsRadicalNumber
    else
      Result := NoRadical;
  finally
    FreeAndNil(CCharProp);
  end;
end;

{ Returns radical character by it's common radical number }
function RadicalUnicode(const radno: integer): FString;
var CRadical: TTextTableCursor;
begin
  CRadical := TRadicals.NewCursor;
  try
    if not CRadical.Locate('Number', radno) then
      Result := ''
    else
      Result := CRadical.Str(TRadicalsUnicode);
  finally
    FreeAndNil(CRadical);
  end;
end;

{ Returns radical index for a radical character. If the character is not a radical,
 returns -1 }
function RadicalIndex(const rad: FChar): integer;
var CRadical: TTextTableCursor;
begin
  CRadical := TRadicals.NewCursor;
  try
   { Unfortuantely there's no Unicode index in the Radicals table => enumerating }
    Result := -1;
    CRadical.First;
    while not CRadical.EOF do begin
      if CRadical.Str(TRadicalsUnicode)=rad then begin
        Result := CRadical.Int(TRadicalsNumber);
        break;
      end;
      CRadical.Next;
    end;
  finally
    FreeAndNil(CRadical);
  end;
end;


initialization
  TChar := nil;
  TCharProp := nil;
  TRadicals := nil;
  ClearCharDbProps();
  chardetl:=TStringList.Create;

finalization
 {$IFDEF CLEAN_DEINIT}
  FreeAndNil(chardetl);
 {$ENDIF}

end.
