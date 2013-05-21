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
  TCharIndex,
  TCharChinese,
  TCharType,
  TCharUnicode,
  TCharStrokeCount,
  TCharJpStrokeCount,
  TCharJpFrequency,
  TCharChFrequency,
  TCharJouyouGrade: integer;

  TCharProp: TTextTable;
  TCharPropKanji,
  TCharPropTypeId,
   { Property type ID in the Property Type offline table (from wakan.cfg).
    Some property types can have several entries. }
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
  TCharPropIndex,
  TCharPropReadDot,
  TCharPropPosition: integer;

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


{
Character property type information.
}
type
  TCharPropType = record
    id: integer;
    sourceType: char;
     { Where did we get that info
        'D': KANJIDIC
        'U': UNIHAN }
    sourceField: string;
    dataType: char;
     { Controls how data for this property is stored and handled
        'U', 'P': stored as 'a'-type hex string, contains unicode
        'R': TRadicals.Number field value, possibly int or 'int' (int in quotes)
        'N': 'Number' (?), AnsiString
        'T': ?, AnsiString
        'S': 'String', AnsiString }
    englishName: string;
    czechName: string;
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
  ptJapaneseDefinition = 3;
  ptChineseDefinition = 7; //usually taken from UNIHAN
  ptJapaneseDefinitionUnicode = 121;

procedure AddCharPropType(const str: string);
function FindCharPropType(const propTypeId: integer): PCharPropType;
function FindCharPropTypeIndex(const propTypeId: integer): integer;


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

function DecodeCharReading(rt: integer; str: string; dotPosition: integer): FString;

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
    function AnsiValue: AnsiString;
    function Value: FString;

  public
   { Uses Cursor to enumerate over a certain character properties.
    These break the current position. }
    function GetCharAnsiValues(kanjiIndex,propType:integer; const sep: string=', '):string;
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

    TCharProp:=TTextTable.Create(ps,'CharRead',true,false); //sic. 'CharRead' for compat. reasons
    TCharPropKanji:=TCharProp.Field('Kanji');
    TCharPropTypeId:=TCharProp.Field('Type');
    TCharPropValue:=TCharProp.Field('Reading'); //sic. 'Reading'
    TCharPropIndex:=TCharProp.Field('Index');
    TCharPropReadDot:=TCharProp.Field('ReadDot');
    TCharPropPosition:=TCharProp.Field('Position');

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
begin
  PKGWriteForm.PKGWriteCmd('NotShow');
  PKGWriteForm.PKGWriteCmd('PKGFileName '+package);
  PKGWriteForm.PKGWriteCmd('MemoryLimit 100000000');
  PKGWriteForm.PKGWriteCmd('Name WaKan User Data');
  PKGWriteForm.PKGWriteCmd('TitleName WaKan Character Dictionary');
  PKGWriteForm.PKGWriteCmd('CopyrightName '+WakanCopyright);
  PKGWriteForm.PKGWriteCmd('FormatName Pure Package File');
  PKGWriteForm.PKGWriteCmd('CommentName File is used by '+WakanAppName);
  PKGWriteForm.PKGWriteCmd('VersionName 1.0');
  PKGWriteForm.PKGWriteCmd('HeaderCode 791564');
  PKGWriteForm.PKGWriteCmd('FileSysCode 978132');
  PKGWriteForm.PKGWriteCmd('WriteHeader');
  PKGWriteForm.PKGWriteCmd('TemporaryLoad');
  PKGWriteForm.PKGWriteCmd('CryptMode 0');
  PKGWriteForm.PKGWriteCmd('CRCMode 0');
  PKGWriteForm.PKGWriteCmd('PackMode 0');
  PKGWriteForm.PKGWriteCmd('CryptCode 978123');
  PKGWriteForm.PKGWriteCmd('Include '+dir);
  PKGWriteForm.PKGWriteCmd('Finish');
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
  parts := SplitStr(str, 7);
  if Length(parts)<7 then
    raise Exception.Create('AddCharPropType: invalid property type info line format');
  if not TryStrToInt(parts[0], pt.id) then
    raise Exception.Create('AddCharPropType: invalid integer ID: '+parts[0]);
  if Length(parts[1])<>1 then
    raise Exception.Create('AddCharPropType: invalid source: '+parts[1]);
  pt.sourceType := parts[1][1];
  pt.sourceField := parts[2];
  if Length(parts[1])<>1 then
    raise Exception.Create('AddCharPropType: invalid data type: '+parts[3]);
  pt.dataType := parts[3][1];
  pt.englishName := parts[4];
  pt.czechName := parts[5];
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

function TCharPropertyCursor.AnsiValue: AnsiString;
var propDataType: char;
  s_ansi: AnsiString;
begin
  propDataType := Self.PropType.dataType;

 { Different property types have data in different formats! }
  if propDataType='R' then
  begin
    s_ansi:=Self.AnsiStr(TCharPropValue);
    if (length(s_ansi)>0) and (s_ansi[1]='''') then System.delete(s_ansi,1,1);
    if (length(s_ansi)>0) and (s_ansi[length(s_ansi)]='''') then System.delete(s_ansi,length(s_ansi),1);
    CRadicals.Locate('Number',strtoint(string(s_ansi)));
    Result := CRadicals.AnsiStr(TRadicalsUnicode);
  end else
 { 'U' and 'P' have reading in 'a'-type hex.
  This is a problem though because logically they're Unicode strings and we want Ansi. }
  if (propDataType='U') or (propDataType='P') then
  begin
    Result := AnsiString(Self.Dehex(TCharPropValue));
  end else
 { Rest is read as it is }
  begin
    Result := Self.AnsiStr(TCharPropValue);
  end;
end;

function TCharPropertyCursor.Value: FString;
var propDataType: char;
  s_ansi: AnsiString;
begin
  propDataType := Self.PropType.dataType;

 { Different property types have data in different formats! }
  if propDataType='R' then
  begin
    s_ansi:=Self.AnsiStr(TCharPropValue);
    if (length(s_ansi)>0) and (s_ansi[1]='''') then System.delete(s_ansi,1,1);
    if (length(s_ansi)>0) and (s_ansi[length(s_ansi)]='''') then System.delete(s_ansi,length(s_ansi),1);
    CRadicals.Locate('Number',strtoint(string(s_ansi)));
    Result := CRadicals.Str(TRadicalsUnicode);
  end else
 { 'U' and 'P' have reading in 'a'-type hex }
  if (propDataType='U') or (propDataType='P') then
  begin
    Result := Self.Dehex(TCharPropValue);
  end else
 { Rest is read as it is }
  begin
    Result := Self.Str(TCharPropValue);
  end;
end;

//Works for AnsiString type properties
function TCharPropertyCursor.GetCharAnsiValues(kanjiIndex, propType:integer; const sep: string):string;
begin
  Result := '';
  Self.SetOrder('');
  if Self.Locate('Kanji',kanjiIndex) then
  while (not Self.EOF) and (Self.Int(TCharPropKanji)=kanjiIndex) do
  begin
    if Self.Int(TCharPropTypeId)=propType then
    begin
      if Result<>'' then
        Result := Result + sep + Self.Str(TCharPropValue)
      else
        Result := Self.Str(TCharPropValue);
    end;
    Self.Next;
  end;
end;

//Works for Unicode type properties
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
        Result := Result + sep + Self.Dehex(TCharPropValue)
      else
        Result := Self.Dehex(TCharPropValue);
    end;
    Self.Next;
  end;
end;

function TCharPropertyCursor.GetJapaneseDefinitions(kanjiIndex: integer; const sep: string): FString;
var tmp: FString;
begin
  Result := fstr(GetCharAnsiValues(kanjiIndex, ptJapaneseDefinition, ', '));
  tmp := GetCharValues(kanjiIndex, ptJapaneseDefinitionUnicode, fstr(', '));
  if (Result<>'') and (tmp<>'') then
    Result := Result + fstr(sep) + tmp;
end;

function TCharPropertyCursor.GetChineseDefinitions(kanjiIndex: integer; const sep: string): FString;
begin
  Result := fstr(GetCharAnsiValues(kanjiIndex, ptChineseDefinition, ', '));
end;


{ Following function is kinda deprecated and needs checks/rewrites.
 Take comments with a grain of salt. }

{ Readings are stored as STRINGS which for some reading-types contain HEX.
So there's nothing we can do to avoid HexToUnicode conversion.

For char property types which support this:
- Decode to FString from explicit hex or encode from Ansi
- Add dot at the specified position
- Replace special markers such as +/i with full-width equivalents
Call this function for everything you get from TCharProps, it won't hurt. }
function DecodeCharReading(rt: integer; str: string; dotPosition: integer): FString;
var adddot:integer;
begin
  if rt in [2, 8] then begin
   //Chinese ony/kuny
    Result := fstr(str);
    exit;
  end;

  if (rt<=3) or (rt>=7) then begin
   //Rest are Ansi strings
    Result := fstr(str);
    exit;
  end;

  Result:='';
  adddot:=0;
  if str[1]='+'then
  begin
    Result:={$IFNDEF UNICODE}'FF0B'{$ELSE}#$FF0B{$ENDIF};
    delete(str,1,1);
    adddot:=1;
  end;
  if str[1]='-'then
  begin
    Result:=Result+{$IFNDEF UNICODE}'FF0D'{$ELSE}#$FF0D{$ENDIF};
    delete(str,1,1);
    adddot:=1;
  end;
  if dotPosition>0 then
  begin
    Result:=Result+hextofstr(copy(str,1,dotPosition-1-adddot));
    Result:=Result+{$IFNDEF UNICODE}'FF0E'{$ELSE}#$FF0E{$ENDIF};
    delete(str,1,dotPosition-1-adddot);
  end;
  if str[length(str)]='-' then
    Result:=Result+hextofstr(copy(str,1,length(str)-1))+{$IFNDEF UNICODE}'FF0D'{$ELSE}#$FF0D{$ENDIF}
  else
    Result:=Result+hextofstr(str);
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
