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
    DicBuildDate: string;
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

  TCharRead: TTextTable;
  TCharReadKanji,
  TCharReadType,
   { S = AnsiString, U = Unicode (4 char hex), P = ? (4 char hex)
     T = ?, R = TRadicals.Number field value, N = Number (?) }
  TCharReadReading, { WARNING! This field here is dangerous.
    Some properties store normal AnsiStrings here, other 4-char hex AnsiStrings,
    i.e. no auto-conversion to unicode is possible.
    When reading from, or searching by this field, you have to check it's property type:
       type := CCharRead.Int(TCharReadType);
       propType := GetCharPropType(type,3);
    And then recode data:
       R: TRadicals.Number field value, possibly int or 'int' (int in quotes)
       U,P: 4char hex
       other: AnsiString
    TODO: Make a class which reads from these and auto-converts, i.e.
      cursor.FS[]: FString
      cursor.AS[]: AnsiString
      cursor.US[]: UnicodeString
      cursor.I[]: Integer
   }
  TCharReadIndex,
  TCharReadReadDot,
  TCharReadPosition: integer;

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
procedure FreeCharData();

var
  CharPropTypes:TStringList; { All possible pieces of information to display in KanjiDetails info box. }
  chardetl:TStringList; { User configuration for KanjiDetails info box -- stored in WAKAN.CDT }

{
Character property information available:
  0 - charPropId
  1 - sourceType -- where did we get that info
    'D': KANJIDIC
    'U': UNIHAN
  2 - sourceField
  3 - propType -- controls how data for this property is stored and handled
    'U', 'P': stored as 'a'-type hex string, contains unicode
    'R', 'N', 'T'
  4 - english name
  5 - czech name
  6 - description (english)
}
function GetCharPropType(idx:integer;fld:integer):string;
function GetCharDet(i,j:integer):string;
function FindCharPropType(charPropId: string): integer;

function GetCharValueInt(index,vt:integer):integer;
function GetCharValueRad(index,vt:integer):integer;
function GetCharValue(index,vt:integer):string;

implementation
uses MemSource;

procedure LoadCharData(const filename: string);
var ps: TPackageSource;
  ms: TMemoryStream;
  vi: TStringList;
begin
  FreeCharData; //just in case

  ps:=TPackageSource.Create(filename,791564,978132,978123);
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

    CharDataProps.DicBuildDate:=datetostr(strtoint(vi[2]));
    CharDataProps.KanjidicVersion:=vi[4];
    CharDataProps.UnihanVersion:=vi[5];
    CharDataProps.ChinesePresent:=vi[6]='CHINESE';
  finally
    vi.Free;
  end;

  TChar:=TTextTable.Create(ps,'Char',true,false);
  TCharRead:=TTextTable.Create(ps,'CharRead',true,false);
  TRadicals:=TTextTable.Create(ps,'Radicals',true,false);
  TCharIndex:=TChar.Field('Index');
  TCharChinese:=TChar.Field('Chinese');
  TCharType:=TChar.Field('Type');
  TCharUnicode:=TChar.Field('Unicode');
  TCharStrokeCount:=TChar.Field('StrokeCount');
  TCharJpStrokeCount:=TChar.Field('JpStrokeCount');
  TCharJpFrequency:=TChar.Field('JpFrequency');
  TCharChFrequency:=TChar.Field('ChFrequency');
  TCharJouyouGrade:=TChar.Field('JouyouGrade');
  TCharReadKanji:=TCharRead.Field('Kanji');
  TCharReadType:=TCharRead.Field('Type');
  TCharReadReading:=TCharRead.Field('Reading');
  TCharReadIndex:=TCharRead.Field('Index');
  TCharReadReadDot:=TCharRead.Field('ReadDot');
  TCharReadPosition:=TCharRead.Field('Position');
  TRadicalsNumber:=TRadicals.Field('Number');
  TRadicalsVariant:=TRadicals.Field('Variant');
  TRadicalsUnicode:=TRadicals.Field('Unicode');
  TRadicalsStrokeCount:=TRadicals.Field('StrokeCount');
  TRadicalsUnicodeCount:=TRadicals.Field('UnicodeCount');
  TRadicalsBushuCount:=TRadicals.Field('BushuCount');
  TRadicalsJapaneseCount:=TRadicals.Field('JapaneseCount');
  TRadicalsKangXiCount:=TRadicals.Field('KangXiCount');

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
  FreeAndNil(TCharRead);
  FreeAndNil(TRadicals);
  ClearCharDbProps();
end;

{ Pieces of information about Kanji }

function GetCharPropType(idx:integer;fld:integer):string;
var s:string;
begin
  s:=CharPropTypes[idx];
  while fld>0 do
  begin
    delete(s,1,pos(',',s));
    dec(fld);
  end;
  if fld<6 then delete(s,pos(',',s),length(s)-pos(',',s)+1);
  result:=s;
end;

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

//Returns chartypel index for a given char type, or -1
function FindCharPropType(charPropId: string): integer;
var i: integer;
begin
  Result := -1;
  for i := 0 to CharPropTypes.Count - 1 do
    if charPropId=GetCharPropType(i,0) then begin
      Result := i;
      break;
    end;
end;

function GetCharValueInt(index,vt:integer):integer;
var s:string;
begin
  s:=GetCharValue(index,vt);
  if (length(s)<>0) and (s[length(s)]='''') then delete(s,length(s),1);
  if (s='') or not TryStrToInt(s, Result) then
    Result:=65535;
end;

function GetCharValueRad(index,vt:integer):integer;
var s:string;
begin
  s:=GetCharValue(index,vt);
  if pos('.',s)>0 then delete(s,pos('.',s),length(s)-pos('.',s)+1);
  if (length(s)<>0) and (s[length(s)]='''') then delete(s,length(s),1);
  if (s='') or not TryStrToInt(s, Result) then
    Result:=65535;
end;

function GetCharValue(index,vt:integer):string;
begin
  TCharRead.SetOrder('');
  if TCharRead.Locate('Kanji',index) then
  while (not TCharRead.EOF) and (TCharRead.Int(TCharReadKanji)=index) do
  begin
    if TCharRead.Int(TCharReadType)=vt then
    begin
      result:=TCharRead.Str(TCharReadReading);
      exit;
    end;
    TCharRead.Next;
  end;
  result:='';
end;

initialization
  TChar := nil;
  TCharRead := nil;
  TRadicals := nil;
  ClearCharDbProps();
  CharPropTypes:=TStringList.Create;
  chardetl:=TStringList.Create;

finalization
 {$IFDEF CLEAN_DEINIT}
  FreeAndNil(chardetl);
  FreeAndNil(CharPropTypes);
 {$ENDIF}

end.
