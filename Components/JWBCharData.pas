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


{
Character property type information.
}
type
  TCharPropType = record
    id: integer;
    sourceType: char;
     { Where did we get that info
        'D': KANJIDIC
        'U': UNIHAN
        '-': Other (special handling)    }
    sourceField: string;
    dataType: char;
     { Controls how data for this property is handled.
        'S': unicode string
        'U': full-width unicode string (ideographic)
        'R': radical
        'N': number
        'P': pinyin
      See parsing code for details. All data is stored as UTF16 strings in DB. }
    englishName: string;
    description: string; // in english
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
  ptTotalStrokes = 11;

  ptBushuRadical = 12;
  ptRSUnicode = 13;
  ptRSJapanese = 14;
  ptRSKanWa = 15;
  ptRSKangXi = 16;
  ptRSKorean = 17;

  ptSKIP = 22;

  ptSimplifiedVariant = 43;
  ptTraditionalVariant = 44;

  ptClassicalRadical = 123;

 { ptRadicals internally has a special treatment and so we keep the list of all
  properties which go into it }
  ptRadicalsComposition = [ptBushuRadical,ptRSUnicode,ptRSJapanese,ptRSKanWa,
    ptRSKangXi,ptRSKorean];

procedure AddCharPropType(const str: string);
function FindCharPropType(const propTypeId: integer): PCharPropType; overload;
function FindCharPropTypeIndex(const propTypeId: integer): integer;
function FindCharPropType(const ASource: char; const AField: string): PCharPropType; overload;


const
 {
  Current WAKAN.CHR format. Version history:
    0  version information not found
    ...
    7  Wakan 1.69 version. Some fields are in Ansi. Not supported.
    8. Current version
  No attempts at upgrading are being done as of now.
  (The solution is always to download new version of file.)
 }
  CurrentCharDataVersion = 8;

var
 { Database props -- populated on load }
  CharDataVersion: integer;
  CharDataProps: record
    DicBuildDate: integer; //DateOf(TDatetime) when this dictionary was last updated
   //Free format strings describing data sources
    KanjidicVersion: string;
    UnihanVersion: string;
    ChinesePresent: boolean; //true if Unihan data is available
  end;

{ Tables and fields }

type
  TCharTableV8 = class(TTextTable)
  public
    fUnicode,               //The char itself, in UTF16
    fChinese,               //1 if char is chinese-only (UNIHAN), 0 if both japanese/chinese (KANJIDIC+UNIHAN)
    fType,
     { 'A', 'N', 'T', 'S', 'J'
      Set according to whether the character has kBigFive/kGB0 assignments in UNIHAN:
          jp/chin               'J'
          jp/chin + kBigFive    'A'
          jp/chin + kGB0        'A'
          chin                  'N'
          chin + kBigFive       'T'
          chin + kGB0           'S'
      }
    fJpStrokeCount,         //from kanjidic, 255 if unknown
    fJpFrequency,           //from kanjidic, 65535 if unknown
    fChStrokeCount,         //255 if unknown
    fChFrequency,           //255 if unknown
    fJouyouGrade: integer;  //from kanjidic, 255 if unknown
    procedure SetupTable; override;

  end;

  TCharPropTableV8 = class(TTextTable)
  protected
    FsKanji: TSeekObject;
  public
    fKanji,     // Char UTF16 value as integer. Since UTF16 is 2 positions max, it fits.
    fTypeId,    { Property type ID in the offline type table (from wakan.cfg).
                  Some property types can have several entries for a kanji. }
    fValue,     // Property value, unicode string. Numbers are stored as strings too.
    fReadDot,   // If there was a dot in the reading, this is it's position (the dot is removed)
    fPosition   // Sequential index in the properties of the same type
      : integer;
    sKanji: PSeekObject;
    procedure SetupTable; override;

  end;

  TRadicalsTableV8 = class(TTextTable)
  public
    fNumber,
    fVariant,
    fUnicode,
    fStrokeCount,
    fUnicodeCount,
    fBushuCount,
    fJapaneseCount,
    fKangXiCount: integer;
    procedure SetupTable; override;

  end;


var
  TChar: TCharTableV8;
  TCharProp: TCharPropTableV8;
  TRadicals: TRadicalsTableV8;

procedure LoadCharData(const filename: string);
procedure SaveCharData(const filename: string);
procedure FreeCharData();
procedure ClearCharDbProps; //when rebuilding

{ Create empty tables of proper format }
function NewCharTable: TCharTableV8;
function NewCharPropTable: TCharPropTableV8;
function NewRadicalsTable: TRadicalsTableV8;


{ Used for .wtt, .jtt header timestamps, for CharDataProps.*Version fields etc.  }
function WakanDatestamp(const dt: TDatetime): string;


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
    function Locate(const AChar: FString): boolean; overload;
    function Locate(const AChar: FString; const APropType: integer): boolean; overload;
    function LocateRawValue(const AValue: string): boolean;
    function Kanji: FString;
    function PropType: PCharPropType;
    function RawValue: FString;
    function AsString: FString;
    function AsRadicalNumber: integer;
    function AsRadical: FString;
    function AsNumber: integer;

  public
   { Uses Cursor to enumerate over a certain character properties.
    These break the current position. }
    function GetCharProps(const AChar: FString; APropType: integer; ASep: FString): FString; overload;
    function GetCharProps(const AChar: FString; APropType: integer): FString; overload;

  end;


{ Converts between actual character and its index used in TCharProp }
function CharUnicode(const ACharIndex: integer): string; inline;
function CharIndex(const AChar: char): integer; overload; inline;
function CharIndex(const AChar: string): integer; overload;

{ Following functions are kept for backward compability and simplicity,
 but if you do a lot of these, using a separate cursor is preferred. }

{ Shortcuts for getting character property values }
function GetCharProp(const AChar: FString; const APropType: integer): string;
function GetCharProps(const AChar: FString; const APropType: integer): string;
function GetCharRadicalNumber(const AChar: FString; const APropType: integer): integer;
function GetCharRadical(const AChar: FString; const APropType: integer): FString;

{ Shortcust for converting radical index to radical char }
function RadicalUnicode(const radno: integer): FString;
function RadicalNumber(const rad: char): integer;


implementation
uses MemSource, PKGWrite, JWBCore;

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

    TChar:=TCharTableV8.Create(ps,'Char',true,false);
    TCharProp:=TCharPropTableV8.Create(ps,'CharProp',true,false);
    TRadicals:=TRadicalsTableV8.Create(ps,'Radicals',true,false);
  finally
    FreeAndNil(ps);
  end;
end;

procedure TCharTableV8.SetupTable;
begin
  fUnicode:=Self.Field('Unicode');
  fChinese:=Self.Field('Chinese');
  fType:=Self.Field('Type');
  fJpStrokeCount:=Self.Field('JpStrokeCount');
  fJpFrequency:=Self.Field('JpFrequency');
  fChStrokeCount:=Self.Field('ChStrokeCount');
  fChFrequency:=Self.Field('ChFrequency');
  fJouyouGrade:=Self.Field('JouyouGrade');
end;

procedure TCharPropTableV8.SetupTable;
begin
  fKanji:=Self.Field('Kanji');
  fTypeId:=Self.Field('Type');
  fValue:=Self.Field('Value');
  fReadDot:=Self.Field('ReadDot');
  fPosition:=Self.Field('Position');
  FsKanji := Self.GetSeekObject('Kanji');
  sKanji := @FsKanji;
end;

procedure TRadicalsTableV8.SetupTable;
begin
  fNumber:=Self.Field('Number');
  fVariant:=Self.Field('Variant');
  fUnicode:=Self.Field('Unicode');
  fStrokeCount:=Self.Field('StrokeCount');
  fUnicodeCount:=Self.Field('UnicodeCount');
  fBushuCount:=Self.Field('BushuCount');
  fJapaneseCount:=Self.Field('JapaneseCount');
  fKangXiCount:=Self.Field('KangXiCount');
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
    pack.Name := 'Wakan User Data';
    pack.TitleName := 'Wakan Character Dictionary';
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


{ Version 8 package structure }

function NewCharTable: TCharTableV8;
begin
  Result := TCharTableV8.Create([
    '$TEXTTABLE',
    '$PRECOUNTED',
    '$RAWINDEX',

    '$FIELDS',
    'xUnicode',
    'bChinese',
    'sType',
    'bJpStrokeCount',
    'wJpFrequency',
    'bChStrokeCount',
    'wChFrequency',
    'bJouyouGrade',

    '$ORDERS',
    'Unicode_Ind',              //All chars together
    'JpUnicode_Ind',            //Chinese+Unicode => Japanese chars go first
    'ChFrequency_Ind',
    'ChStrokeCount_Ind',
    'JpFrequency_Ind',
    'JpStrokeCount_Ind',

    '$SEEKS',
    '0',
    'Unicode',
    'Chinese+Unicode',
    'ChFrequency',
    'ChStrokeCount',
    'JpFrequency',
    'Chinese+JpStrokeCount'
  ]);
end;

function NewCharPropTable: TCharPropTableV8;
begin
  Result := TCharPropTableV8.Create([
    '$TEXTTABLE',
    '$PRECOUNTED',
    '$RAWINDEX',

    '$FIELDS',
    'iKanji',
    'bType',
    'xValue',
    'bReadDot',
    'bPosition',

    '$ORDERS',
    'KanjiTypePosition_Ind',
    'Value_Ind',
    'Type_Ind',

    '$SEEKS',
    '0',
    'Kanji+Type+Position',
    'Value',
    'Type'
  ]);
end;

function NewRadicalsTable: TRadicalsTableV8;
begin
  Result := TRadicalsTableV8.Create([
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

procedure SaveCharData(const filename: string);
var tempDir: string;
  vi: TStringList;
begin
  tempDir := CreateRandomTempDir();

  vi := TStringList.Create;
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

  TChar.WriteTable(tempDir+'\Char', false);
  TCharProp.WriteTable(tempDir+'\CharProp', false);
  TRadicals.WriteTable(tempDir+'\Radicals', false);
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






{
Pieces of information about Kanji
}

{ Adds a char property type info from parsing a wakan.cfg style string }
procedure AddCharPropType(const str: string);
var tmp: string;
  parts: TStringArray;
  pt: TCharPropType;
begin
  tmp := str;
  parts := SplitStr(str, 6);
  if Length(parts)<4 then
    raise Exception.Create('AddCharPropType: invalid property type info line format');

  if not TryStrToInt(parts[0], pt.id) then
    raise Exception.Create('AddCharPropType: invalid integer ID: '+parts[0]);

  if Length(parts[1])<>1 then
    raise Exception.Create('AddCharPropType: invalid source: '+parts[1]);
  pt.sourceType := parts[1][1];
  pt.sourceField := parts[2];

  if Length(parts[3])<>1 then
    raise Exception.Create('AddCharPropType: invalid data type: '+parts[3]);
  pt.dataType := parts[3][1];

  if Length(parts)>=5 then
    pt.englishName := parts[4];
  if Length(parts)>=6 then
    pt.description := parts[5];

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



{
TCharPropertyCursor
}

constructor TCharPropertyCursor.Create(ATable: TTextTable);
begin
  inherited;
  Self.SetOrder('KanjiTypePosition_Ind'); //Default order for this cursor
  CRadicals := TRadicals.NewCursor;
end;

destructor TCharPropertyCursor.Destroy;
begin
  FreeAndNil(CRadicals);
  inherited;
end;

{ Locates first property for a given kanji }
function TCharPropertyCursor.Locate(const AChar: FString): boolean;
begin
  Self.SetOrder('KanjiTypePosition_Ind');
  Result := Self.Locate(TCharPropTableV8(FTable).sKanji, CharIndex(AChar));
end;

{ Locates first entry of a property for a given kanji }
function TCharPropertyCursor.Locate(const AChar: FString; const APropType: integer): boolean;
begin
  Result := false;
  if not Self.Locate(AChar) then
    exit;
  while (not Self.EOF) and (Self.Kanji=AChar) do begin
    if Self.Int(TCharProp.fTypeId)=APropType then
    begin
      Result := true;
      break;
    end;
    Self.Next;
  end;
end;

//This can only locate raw values because they are indexed as such
function TCharPropertyCursor.LocateRawValue(const AValue: string): boolean;
begin
  SetOrder('Value_Ind');
  Result := Locate('Value', AValue);
end;

function TCharPropertyCursor.Kanji: FString;
begin
  Result := CharUnicode(Self.Int(TCharProp.fKanji));
end;

function TCharPropertyCursor.PropType: PCharPropType;
var propTypeId: integer;
begin
  propTypeId := Self.Int(TCharProp.fTypeId);
  Result := FindCharPropType(propTypeid);
end;

{ Raw data from Value field. It's a string but it may be in a specific format.
 You need the property type to know how to deal with it. }
function TCharPropertyCursor.RawValue: FString;
begin
  Result := Self.Str(TCharProp.fValue);
end;

{ Text data from Value field. Different handling for different property types,
 but as a rule of thumb, this is what we will display in KanjiDetails. }
function TCharPropertyCursor.AsString: FString;
var propType: PCharPropType;
  dotPosition: integer;
begin
  propType := Self.PropType;
  case propType.dataType of
    'R': Result := AsRadical; //char
  else
   //Just text data
    Result := RawValue;

   //Mind ReadDot
    dotPosition := self.Int(TCharProp.fReadDot);
    if dotPosition>0 then
      Result := copy(Result, 1, dotPosition-1)
        + #$FF0E //wide dot
        + copy(Result, dotPosition, MaxInt);
  end;
end;

{ Assuming current property is a radical index, returns that index.
 In case of extended indexes (radical.stroke_count) returns only the radical,
 use RawData for full data. }
function TCharPropertyCursor.AsRadicalNumber: integer;
var propType: PCharPropType;
  s_str: string;
  i: integer;
begin
  propType := Self.PropType;
  if propType.dataType<>'R' then
    raise Exception.Create('AsRadicalIndex(): not a radical property');

 { Parse extended radical format -- see UnihanImport }
  s_str := Self.Str(TCharProp.fValue);
  i := pos('.', s_str);
  if i>0 then System.Delete(s_str,i,MaxInt);
  if (length(s_str)>0) and (s_str[length(s_str)]='''') then System.Delete(s_str,length(s_str),1);
  if (s_str='') or not TryStrToInt(s_str, Result) then
    raise Exception.Create('AsRadicalIndex(): invalid radical data ('+Self.RawValue+')');
end;

{ Assuming current property is a radical index, returns that radical
 (the character itself).
 In case of extended indexes (radical.stroke_count) ignores the right part. }
function TCharPropertyCursor.AsRadical: FString;
var radno: integer;
begin
  radno := AsRadicalNumber;
  if not CRadicals.Locate('Number', radno) then
    raise Exception.Create('Invalid radical number value: '+IntToStr(radno));
  Result := CRadicals.Str(TRadicals.fUnicode);
end;

function TCharPropertyCursor.AsNumber: integer;
begin
 //Self.Int would do auto-conversion but let's expose the fact that the data
 //is in the string form
  Result := StrToInt(Self.Str(TCharProp.fValue));
end;

//Returns a list of all property values for the given character and property type.
function TCharPropertyCursor.GetCharProps(const AChar: FString; APropType: integer; ASep: FString): FString;
var Match: boolean;
  curval: FString;
begin
  Result := '';
  if not Self.Locate(AChar) then exit;
  while (not Self.EOF) and (Self.Kanji=AChar) do begin
   //This special property is virtual and consists of several others
    if APropType=ptRadicals then
      Match := Self.Int(TCharProp.fTypeId) in ptRadicalsComposition
    else
      Match := Self.Int(TCharProp.fTypeId)=APropType;
    if Match then begin
      curval := Self.AsString;
      if (APropType<>ptRadicals) or (pos(curval,Result)<=0) then
        if Result<>'' then
          Result := Result + ASep + curval
        else
          Result := curval;
    end;
    Self.Next;
  end;
end;

//Same but uses the default appropriate list separator for a property type.
//This is the preferred function to return "property value for a type"
function TCharPropertyCursor.GetCharProps(const AChar: FString; APropType: integer): FString;
var propType: PCharPropType;
  sep: FString;
begin
  propType := Self.PropType;
  case propType.dataType of
    'U': sep := UH_IDG_COMMA; //There's also fullwidth latin comma: $FF0C
    'R': sep := '';
  else sep := ', '
  end;
  Result := GetCharProps(AChar, APropType, sep);
end;


{ Converts between actual character and its index used in TCharProp }

function CharUnicode(const ACharIndex: integer): string;
begin
  if ACharIndex > $FFFF then
    Result := WideChar(ACharIndex shr 16) + WideChar(ACharIndex and $FFFF)
  else
    Result := WideChar(Word(ACharIndex));
end;

function CharIndex(const AChar: char): integer;
begin
  Result := Word(AChar);
end;

function CharIndex(const AChar: string): integer;
begin
  case Length(AChar) of
    1: Result := Word(AChar[1]);
    2: Result := Word(AChar[1]) + Word(AChar[2]) shl 16;
  else Result := 0;
  end;
end;

function GetCharProp(const AChar: FString; const APropType: integer): string;
var CCharProp: TCharPropertyCursor;
begin
  CCharProp := TCharPropertyCursor.Create(TCharProp);
  try
    if CCharProp.Locate(AChar,APropType) then
      Result := CCharProp.AsString
    else
      Result := '';
  finally
    FreeAndNil(CCharProp);
  end;
end;

function GetCharProps(const AChar: FString; const APropType: integer): string;
var CCharProp: TCharPropertyCursor;
begin
  CCharProp := TCharPropertyCursor.Create(TCharProp);
  try
    Result := CCharProp.GetCharProps(AChar, APropType);
  finally
    FreeAndNil(CCharProp);
  end;
end;

function GetCharRadicalNumber(const AChar: FString; const APropType: integer): integer;
var CCharProp: TCharPropertyCursor;
begin
  CCharProp := TCharPropertyCursor.Create(TCharProp);
  try
    if CCharProp.Locate(AChar,APropType) then
      Result := CCharProp.AsRadicalNumber
    else
      Result := NoRadical;
  finally
    FreeAndNil(CCharProp);
  end;
end;

function GetCharRadical(const AChar: FString; const APropType: integer): FString;
var CCharProp: TCharPropertyCursor;
begin
  CCharProp := TCharPropertyCursor.Create(TCharProp);
  try
    if CCharProp.Locate(AChar,APropType) then
      Result := CCharProp.AsRadical
    else
      Result := UH_NOCHAR;
  finally
    FreeAndNil(CCharProp);
  end;
end;

{ Shortcust for converting radical index to radical char }

{ Returns radical character by it's common radical number }
function RadicalUnicode(const radno: integer): FString;
var CRadical: TTextTableCursor;
begin
  CRadical := TRadicals.NewCursor;
  try
    if not CRadical.Locate('Number', radno) then
      Result := ''
    else
      Result := CRadical.Str(TRadicals.fUnicode);
  finally
    FreeAndNil(CRadical);
  end;
end;

{ Returns radical number for a radical character. If the character is not a radical,
 returns -1 }
function RadicalNumber(const rad: FChar): integer;
var CRadical: TTextTableCursor;
begin
  CRadical := TRadicals.NewCursor;
  try
   { Unfortuantely there's no Unicode index in the Radicals table => enumerating }
    Result := -1;
    CRadical.First;
    while not CRadical.EOF do begin
      if CRadical.Str(TRadicals.fUnicode)=rad then begin
        Result := CRadical.Int(TRadicals.fNumber);
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

end.
