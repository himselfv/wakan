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
    DicBuildDateInt: integer; //original 'dic build date'
    DicBuildDate: string; //string version
    KanjidicVersion: string;
    UnihanVersion: string;
    ChinesePresent:boolean;
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

procedure InitializeCharPackage(const package:string);
function NewCharTable: TTextTable;
function NewCharPropTable: TTextTable;
function NewRadicalsTable: TTextTable;

procedure FixTCharJpUnicodeIndex(table: TTextTable);

function WakanDatestamp(const dt: TDatetime): string;

{
Character property type information.
}
type
  TCharPropType = record
    id: integer;
    superceded_by: integer;
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
    description: string // in english
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
  ptRadicals = 10;
  ptJapaneseDefinitionUnicode = 121;

procedure AddCharPropType(const str: string);
function FindCharPropType(const propTypeId: integer): PCharPropType; overload;
function FindCharPropTypeIndex(const propTypeId: integer): integer;
function FindCharPropType(const ASource: char; const AField: string): PCharPropType; overload;


{
Shortcuts for getting character property values
}

function GetCharValue(index,propType:integer):string;
function GetCharValueInt(index,propType:integer):integer;
function GetCharValueRad(index,propType:integer):integer;


{
User configuration for KanjiDetails info box -- stored in WAKAN.CDT
}
var
  chardetl:TStringList;

function GetCharDet(i,j:integer):string;


{
CharPropertyCursor
}

type
  TCharPropertyCursor = class(TTextTableCursor)
  protected
    CRadicals: TTextTableCursor;
  public
    constructor Create(ATable: TTextTable);
    destructor Destroy; override;
    function PropType: PCharPropType;
    function RawValue: string;
    function Value: FString;

  public
   { Uses Cursor to enumerate over a certain character properties.
    These break the current position. }
    function GetCharValues(kanjiIndex,propType:integer; const sep: FString={$IFDEF UNICODE}', '{$ELSE}$002C$0020{$ENDIF}):FString;
    function GetJapaneseDefinitions(kanjiIndex: integer; const sep: string=', '): FString;
    function GetChineseDefinitions(kanjiIndex: integer; const sep: string=', '): FString;

  end;

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
      if CharDataVersion<CurrentCharDataVersion then
        raise ECharDataException.Create('#00355^eWAKAN.CHR has newer structure. '
          +'Please download new WAKAN.EXE.'#13#13'Application will now exit.');

      CharDataProps.DicBuildDateInt:=strtoint(vi[2]);
      CharDataProps.DicBuildDate:=datetostr(CharDataProps.DicBuildDateInt);
      CharDataProps.KanjidicVersion:=vi[4];
      CharDataProps.UnihanVersion:=vi[5];
      CharDataProps.ChinesePresent:=vi[6]='CHINESE';
    finally
      vi.Free;
    end;

    TChar:=TTextTable.Create(ps,'Char',true,false);
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

    TCharProp:=TTextTable.Create(ps,'CharRead',true,false); //sic. 'CharRead' for compat. reasons
    TCharPropIndex:=TCharProp.Field('Index');
    TCharPropKanji:=TCharProp.Field('Kanji');
    TCharPropTypeId:=TCharProp.Field('Type');
    TCharPropValue:=TCharProp.Field('Reading'); //sic. 'Reading'
    TCharPropReadDot:=TCharProp.Field('ReadDot');
    TCharPropPosition:=TCharProp.Field('Position');
    TCharProp.IsAutoIncField[TCharPropIndex]:=true;

    TRadicals:=TTextTable.Create(ps,'Radicals',true,false);
    TRadicalsNumber:=TRadicals.Field('Number');
    TRadicalsVariant:=TRadicals.Field('Variant');
    TRadicalsUnicode:=TRadicals.Field('Unicode');
    TRadicalsStrokeCount:=TRadicals.Field('StrokeCount');
    TRadicalsUnicodeCount:=TRadicals.Field('UnicodeCount');
    TRadicalsBushuCount:=TRadicals.Field('BushuCount');
    TRadicalsJapaneseCount:=TRadicals.Field('JapaneseCount');
    TRadicalsKangXiCount:=TRadicals.Field('KangXiCount');
  finally
    FreeAndNil(ps);
  end;
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
    vi.Add(IntToStr(CharDataProps.DicBuildDateInt));
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
    'JpStrokeCount',
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
    'iIndex',
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
    'Kanji',
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
    vi.Add(IntToStr(CharDataProps.DicBuildDateInt));
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
  CharDataProps.DicBuildDate := '';
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


{
Pieces of information about Kanji
}

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
    pt.superceded_by := 0
  else
  if not TryStrToInt(parts[0], pt.superceded_by) then
    raise Exception.Create('AddCharPropType: invalid integer superceded_by: '+parts[0]);

  if pt.superceded_by>0 then begin
   //For now we don't support multi-obsolete-superceding
    idx := FindCharPropTypeIndex(pt.superceded_by);
    if idx>=0 then
      CharPropTypes[idx].supercedes := pt.id;
  end;

  for idx := 0 to Length(CharPropTypes) - 1 do
    if CharPropTypes[idx].supercedes=pt.id then begin
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
    if (ASource=CharPropTypes[i].sourceType)
    and (AField=CharPropTypes[i].sourceField) then begin
      Result := @CharPropTypes[i];
      break;
    end;
end;

{ Following functions are inherited and left as they are, although
 they reimplement functionality and are bad. }

{ NOTE: On Unicode properties this will return hex gibberish }
function GetCharValue(index,propType:integer):string;
begin
  TCharProp.SetOrder('');
  if TCharProp.Locate('Kanji',index) then
  while (not TCharProp.EOF) and (TCharProp.Int(TCharPropKanji)=index) do
  begin
    if TCharProp.Int(TCharPropTypeId)=propType then
    begin
      result:=TCharProp.Str(TCharPropValue);
      exit;
    end;
    TCharProp.Next;
  end;
  result:='';
end;

function GetCharValueInt(index,propType:integer):integer;
var s:string;
begin
  s:=GetCharValue(index,propType);
  if (length(s)<>0) and (s[length(s)]='''') then delete(s,length(s),1);
  if (s='') or not TryStrToInt(s, Result) then
    Result:=65535;
end;

function GetCharValueRad(index,propType:integer):integer;
var s:string;
begin
  s:=GetCharValue(index,propType);
  if pos('.',s)>0 then delete(s,pos('.',s),length(s)-pos('.',s)+1);
  if (length(s)<>0) and (s[length(s)]='''') then delete(s,length(s),1);
  if (s='') or not TryStrToInt(s, Result) then
    Result:=65535;
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

   { ptRadicals contains TRadicals index and Wakan expects us to auto-resolve it.
    If anyone needs raw value here (and not from RawValue), maybe it's better
    to just make a new function, RadicalValue. }
    if propType.id=ptRadicals then begin
      s_str:=Self.Str(TCharPropValue); //should be ansi string
     //compat.: sometimes index is in quotes
      if (length(s_str)>0) and (s_str[1]='''') then System.delete(s_str,1,1);
      if (length(s_str)>0) and (s_str[length(s_str)]='''') then System.delete(s_str,length(s_str),1);
      CRadicals.Locate('Number',strtoint(s_str));
      Result := CRadicals.Str(TRadicalsUnicode);
    end else

     { Other R-type properties contain random text }
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

//Returns a list of all property values for the given character and property tipe
function TCharPropertyCursor.GetCharValues(kanjiIndex, propType:integer; const sep: FString):FString;
begin
  Result := '';
  Self.SetOrder('');
  if Self.Locate('Kanji',kanjiIndex) then
  while (not Self.EOF) and (Self.Int(TCharPropKanji)=kanjiIndex) do
  begin
    if Self.Int(TCharPropTypeId)=propType then
    begin
      if Result<>'' then
        Result := Result + sep + Self.Value
      else
        Result := Self.Value;
    end;
    Self.Next;
  end;
end;

function TCharPropertyCursor.GetJapaneseDefinitions(kanjiIndex: integer; const sep: string): FString;
begin
 //Newer versions sometimes have both Unicode and non-Unicode fields for compability
  Result := GetCharValues(kanjiIndex, ptJapaneseDefinitionUnicode, fstr(', '));
  if Result='' then
    Result := GetCharValues(kanjiIndex, ptJapaneseDefinition, ', ');
end;

function TCharPropertyCursor.GetChineseDefinitions(kanjiIndex: integer; const sep: string): FString;
begin
  Result := GetCharValues(kanjiIndex, ptChineseDefinition, ', ');
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
