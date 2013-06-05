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

{ Parsers for some special property formats }

type
  TVariantValue = record
    char: UnicodeString;
    sources: array of string;
  end;
  TVariantValues = array of TVariantValue;

procedure ParseVariantProperty(const s: UnicodeString; out variants: TVariantValues);

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

{ Decodes stuff like U+20B3A into a character (possibly multibyte) }
function DecodeUplusChar(const ch: UnicodeString): UnicodeString;
var uni_idx, i: integer;
begin
  Result := '';
  if Length(ch)<6 then
    raise EUnihanParsingException.CreateFmt(eInvalidCharHexCode, [ch]);
  if (ch[1]<>'U') or (ch[2]<>'+') then
    raise EUnihanParsingException.CreateFmt(eInvalidCharHexCode, [ch]);
  uni_idx := 0;
  if (Length(ch)>8) or (Length(ch)<=2) then //need at least U+1234 four chars
    raise EUnihanParsingException.CreateFmt(eInvalidCharHexCode, [ch]);
  for i := 3 to Length(ch) do
    uni_idx := uni_idx shl 4 + HexCharCode(ch[i]);
  if uni_idx<=$FFFF then
    Result := WideChar(Word(uni_idx))
  else
  if uni_idx>$10FFFF then
    raise EUnihanParsingException.CreateFmt(eInvalidCharHexCode, [ch])
  else begin
    uni_idx := uni_idx - $10000; //max $FFFFF remains
    Result :=
        WideChar(Word($D800+(uni_idx and $FFC00) shr 10)) //base + top ten bits = lead surrogate
      + WideChar(Word($DC00+(uni_idx and $003FF))); //base + low ten bits = trail surrogate
  end;
end;

{ Do not pass empty or comment lines }
procedure ParseUnihanLine(const s: UnicodeString; ed: PUnihanPropertyEntry);
var i_next: integer;
  val, tmp: string;
begin
  val := s;
  if val[1]<>'U' then
    raise EUnihanParsingException.Create(eNoCharHexCode);

  ed.Reset;
  i_next := pos(#09, val);
  if i_next<0 then
    raise EUnihanParsingException.Create(eNoPropertyName);
  tmp := copy(val,1,i_next-1);
  ed.char := DecodeUplusChar(tmp);
  if Length(ed.char)<=0 then
    raise EUnihanParsingException.CreateFmt(eInvalidCharHexCode, [tmp]);
  delete(val,1,i_next);
  i_next := pos(#09, val);
  if i_next<0 then
    raise EUnihanParsingException.Create(eNoPropertyValue);
  ed.propType := copy(val, 1, i_next-1);
  ed.value := copy(val, i_next+1, MaxInt);
end;

{ Used in most properties in Unihan_Variants.txt
Ex.: U+70AE<kMeyerWempe U+7832<kLau,kMatthews,kMeyerWempe U+791F<kLau,kMatthews }
procedure ParseVariantProperty(const s: UnicodeString; out variants: TVariantValues);
var tmp, part, src: UnicodeString;
  i_sp: integer;
  new_v: TVariantValue;
begin
  SetLength(Variants,0);
  tmp := s;
  i_sp := pos(' ', tmp);
  while (i_sp>0) or (tmp<>'') do begin
    if i_sp>0 then begin
      part := copy(tmp,1,i_sp-1);
      delete(tmp,1,i_sp);
    end else begin //last part
      part := tmp;
      tmp := '';
    end;

    i_sp := pos('<', part);
    if i_sp<=0 then begin
      new_v.char := DecodeUplusChar(part);
      part := '';
    end else begin
      new_v.char := DecodeUplusChar(copy(part,1,i_sp-1));
      delete(part,1,i_sp);
    end;

    SetLength(new_v.sources, 0);
    i_sp := pos(',',part);
    while (i_sp>0) or (part<>'') do begin
      if i_sp>0 then begin
        src := copy(part,1,i_sp-1);
        delete(part,1,i_sp);
      end else begin //last part
        src := part;
        part := '';
      end;

      SetLength(new_v.sources, Length(new_v.sources)+1);
      new_v.sources[Length(new_v.sources)-1] := src;
      i_sp := pos(',',part);
    end;

    SetLength(variants, Length(variants)+1);
    variants[Length(variants)-1] := new_v;
    variants[Length(variants)-1].sources := copy(new_v.sources);
    i_sp := pos(' ', tmp);
  end;
end;

end.
