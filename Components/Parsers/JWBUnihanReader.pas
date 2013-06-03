unit JWBUnihanReader;
{
Parses Unihan format.
Unihan is organized as a bunch of files:
  Unihan_Readings.txt
  Unihan_Variants.txt
  ...
Each associates some properties to kanji:
  U+53C1             kAccountingNumeric     3
  U+kanjiHex [tab]   kPropertyName [tab]    Value
Some properties can have multiple values for a single kanji.

Simple way to parse this is to parse every file line by line, and look for
the properties you need.
}

interface
uses SysUtils;

{ Do not pass here lines starting with this character.
 Alternatively, use IsUnihanComment() }
const
  UNIHAN_COMMENT: WideChar = '#';

type
  EUnihanParsingException = class(Exception);

  TUnihanPropertyEntry = record
    char: UnicodeString; //it can be multibyte
    propType: string;
    value: string;
    procedure Reset;
  end;
  PUnihanPropertyEntry = ^TUnihanPropertyEntry;

{ Returns true if the line in question is a comment line. }
function IsUnihanComment(const s: UnicodeString): boolean;

{ Parses a valid non-empty non-comment line and populates the record }
procedure ParseUnihanLine(const s: UnicodeString; ed: PUnihanPropertyEntry);

implementation

resourcestring
  eNoCharHexCode = 'Record does not begin with U+hex code';
  eInvalidCharHexCode = 'Invalid char hex code: %s';
  eInvalidHexCharacter = 'Invalid hex character "%s"';
  eNoPropertyName = 'Missing property name';
  eNoPropertyValue = 'Missing property value';

function IsUnihanComment(const s: UnicodeString): boolean;
var pc: PWideChar;
begin
  pc := PWideChar(integer(s)); //do not uniquestr on cast
  if pc=nil then begin
    Result := false;
    exit;
  end;
  while pc^=' ' do Inc(pc);
  Result := pc^=UNIHAN_COMMENT;
end;

procedure TUnihanPropertyEntry.Reset;
begin
  char := '';
  propType := '';
  value := '';
end;

//Returns a value in range 0..15 for a given hex character, or throws an exception
function HexCharCode(const c:char): byte; inline;
begin
  if (ord(c)>=ord('0')) and (ord(c)<=ord('9')) then
    Result := ord(c)-ord('0')
  else
  if (ord(c)>=ord('A')) and (ord(c)<=ord('F')) then
    Result := 10 + ord(c)-ord('A')
  else
  if (ord(c)>=ord('a')) and (ord(c)<=ord('f')) then
    Result := 10 + ord(c)-ord('a')
  else
    raise EUnihanParsingException.CreateFmt(eInvalidHexCharacter,[c]);
end;

{ Do not pass empty or comment lines }
procedure ParseUnihanLine(const s: UnicodeString; ed: PUnihanPropertyEntry);
var i_next, j: integer;
  val, tmp: string;
  uni_idx: integer;
begin
  val := s;
  if val[1]<>'U' then
    raise EUnihanParsingException.Create(eNoCharHexCode);

  ed.Reset;
  i_next := pos(#09, val);
  if i_next<0 then
    raise EUnihanParsingException.Create(eNoPropertyName);
  tmp := copy(val,3,i_next-3);
  if Length(tmp)<4 then
    raise EUnihanParsingException.CreateFmt(eInvalidCharHexCode, [tmp]);
  uni_idx := 0;
  if (Length(tmp)>6) or (Length(tmp)<=0) then
    raise EUnihanParsingException.CreateFmt(eInvalidCharHexCode, [tmp]);
  for j := 1 to Length(tmp) do
    uni_idx := uni_idx shl 4 + HexCharCode(tmp[j]);
  if uni_idx<=$FFFF then
    ed.char := WideChar(Word(uni_idx))
  else
  if uni_idx>$10FFFF then
    raise EUnihanParsingException.CreateFmt(eInvalidCharHexCode, [tmp])
  else begin
    uni_idx := uni_idx - $10000; //max $FFFFF remains
    ed.char :=
        WideChar(Word($D800+(uni_idx and $FFC00) shr 10)) //base + top ten bits = lead surrogate
      + WideChar(Word($DC00+(uni_idx and $003FF))); //base + low ten bits = trail surrogate
  end;

  val := copy(val, i_next+1, MaxInt);
  i_next := pos(#09, val);
  if i_next<0 then
    raise EUnihanParsingException.Create(eNoPropertyValue);
  ed.propType := copy(val, 1, i_next-1);
  ed.value := copy(val, i_next+1, MaxInt);
end;

end.
