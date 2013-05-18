unit JWBKanjiDicReader;
{
Reads KANJIDIC format.
  # comment
  亜 3021 U4e9c [fields] [readings] [T1 readings] [T2 ...] [meanings]

See http://www.csse.monash.edu.au/~jwb/kanjidic_doc.html
}

interface
uses SysUtils;

{ Do not pass here lines starting with this character.
 Alternatively, use IsKanjidicComment() }
const
  KANJIDIC_COMMENT: WideChar = '#';

{ Some reasonable values. Increase if it's not enough one day }
const
  MaxValues = 5;
  MaxOns = 10;
  MaxKuns = 10;
  MaxReadingClasses = 3;
  MaxFields = 30;
  MaxMeanings = 16;

type
  EKanjidicParsingException = class(Exception);

 { NOTE: This can be made much faster if we assume all field keys are AnsiStrings
    with at most 4 symbols (VERY reasonable assumption).
    We can just declare key as integer, and even have some sort of table to map
    integer(key_string) to one of sequential pre-allocated cells, i.e.
      integer('U') -> 0
      integer('K') -> 1
    This will all work in constant time.
    We can also have some constant time uppercase functions }

  TFieldEntry = record
    key: string; //always lowercase
    values: array[0..MaxValues-1] of string; //all values so far are ansi
    values_used: integer;
    procedure Reset;
    procedure AddValue(const value: string);
  end;
  PFieldEntry = ^TFieldEntry;

  TReadingClassEntry = record
    key: string;
    ons: array[0..MaxOns] of UnicodeString;
    ons_used: integer;
    kuns: array[0..MaxKuns] of UnicodeString;
    kuns_used: integer;
    procedure Reset;
    procedure AddOn(const value: UnicodeString);
    procedure AddKun(const value: UnicodeString);
  end;

  TKanjidicEntry = record
    kanji: UnicodeString; //it may be wider than a char
    jis: string; //JIS key
    fields: array[0..MaxFields-1] of TFieldEntry;
    fields_used: integer;
   { Kanjidic supports several reading classes: T0 the default, T1 and T2.
    There's no _used because there's a fixed number of them. }
    readings: array[0..MaxReadingClasses-1] of TReadingClassEntry;
    meanings: array[0..MaxMeanings] of UnicodeString; //we support multilingual kanjidics
    meanings_used: integer;
    procedure Reset;
    procedure AddField(const key: string; const value: string);
    procedure AddMeaning(const value: UnicodeString);
   //Shortcuts for ease of access
    function GetField(const key: string): PFieldEntry;
    function GetFieldValue(const key: string): string;
    function GetPinyin: PFieldEntry;
    function GetKoreanReadings: PFieldEntry;
  end;
  PKanjidicEntry = ^TKanjidicEntry;

{ Returns true if the line in question is a Kanjidic comment line.
 You could have done this by yourself, but here goes. }
function IsKanjidicComment(const s: UnicodeString): boolean;

{ Parses a valid non-empty non-comment Kanjidic line and populates the record }
procedure ParseKanjidicLine(const s: UnicodeString; ed: PKanjidicEntry);

implementation

procedure TFieldEntry.Reset;
begin
  key := '';
  values_used := 0;
end;

procedure TFieldEntry.AddValue(const value: string);
begin
  Inc(values_used);
  if values_used>Length(values) then
    raise EKanjidicParsingException.Create('FieldEntry: Cannot add one more value');
  values[values_used-1] := value;
end;

procedure TReadingClassEntry.Reset;
begin
  ons_used := 0;
  kuns_used := 0;
end;

procedure TReadingClassEntry.AddOn(const value: UnicodeString);
begin
  Inc(ons_used);
  if ons_used>Length(ons) then
    raise EKanjidicParsingException.Create('ReadingClassEntry: Cannot add one more ON');
  ons[ons_used-1] := value;
end;

procedure TReadingClassEntry.AddKun(const value: UnicodeString);
begin
  Inc(kuns_used);
  if kuns_used>Length(kuns) then
    raise EKanjidicParsingException.Create('ReadingClassEntry: Cannot add one more KUN');
  kuns[kuns_used-1] := value;
end;

procedure TKanjidicEntry.Reset;
var i: integer;
begin
  kanji := '';
  jis := '';
  fields_used := 0;
  meanings_used := 0;
  for i := 0 to Length(readings) - 1 do
    readings[i].Reset;
end;

procedure TKanjidicEntry.AddField(const key: string; const value: string);
var field: PFieldEntry;
begin
  field := GetField(key);
  if field=nil then begin
    Inc(fields_used);
    if fields_used>Length(fields) then
      raise EKanjidicParsingException.Create('KanjiDicEntry: Cannot add one more field');
    field := @fields[fields_used-1];
    field^.Reset;
    field^.key := AnsiLowerCase(key);
  end;
  field^.AddValue(value);
end;

procedure TKanjidicEntry.AddMeaning(const value: UnicodeString);
begin
  Inc(meanings_used);
  if meanings_used>Length(meanings) then
    raise EKanjidicParsingException.Create('KanjiDicEntry: Cannot add one more meaning');
  meanings[meanings_used-1] := value;
end;

function TKanjidicEntry.GetField(const key: string): PFieldEntry;
var i: integer;
  tmp: string;
begin
  Result := nil;
  tmp := AnsiLowerCase(key);
  for i := 0 to fields_used - 1 do
    if fields[i].key=tmp then begin
      Result := @fields[i];
      break;
    end;
end;

{ Returns first value for the field, or empty string }
function TKanjidicEntry.GetFieldValue(const key: string): string;
var field: PFieldEntry;
begin
  field := GetField(key);
  if (field=nil) or (field.values_used<=0) then
    Result := ''
  else
    Result := field.values[0];
end;

function TKanjidicEntry.GetPinyin: PFieldEntry;
begin
  Result := GetField('Y');
end;

function TKanjidicEntry.GetKoreanReadings: PFieldEntry;
begin
  Result := GetField('W');
end;

function IsKanjidicComment(const s: UnicodeString): boolean;
var pc: PWideChar;
begin
  pc := PWideChar(integer(s)); //do not uniquestr on cast
  if pc=nil then begin
    Result := false;
    exit;
  end;
  while pc^=' ' do Inc(pc);
  Result := pc^=KANJIDIC_COMMENT;
end;

function ReadNextWord(var pc: PWideChar; out word: UnicodeString): boolean;
var stop_char: WideChar;
begin
  while pc^=' ' do Inc(pc);
  if pc^=#00 then begin
   //Nothing to read
    Result := false;
    exit;
  end;
 //Otherwise there is something
  Result := true;

  if pc^='{' then
    stop_char := '}'
  else
    stop_char := ' ';

  while (pc^<>#00) and (pc^<>stop_char) do begin
    word := word + pc^;
    Inc(pc);
  end;
  if pc^<>#00 then begin
   //We eat the stop_char. We must do this with curly bracket, and even if it's space it's okay.
    if stop_char<>' ' then
      word := word + stop_char;
    Inc(pc);
  end;
end;

function IsKatakana(const ch: WideChar): boolean; inline;
begin
  Result := (Word(ch)>=$30A0) and (Word(ch)<=$30FF);
end;

function IsHiragana(const ch: WideChar): boolean; inline;
begin
  Result := (Word(ch)>=$3040) and (Word(ch)<=$309F);
end;

procedure ParseKanjidicLine(const s: UnicodeString; ed: PKanjidicEntry);
var
  rclass: integer;
  pc: PWideChar;
  word: UnicodeString;

begin
  ed.Reset;
  rclass := 0;
  pc := PWideChar(integer(s)); //do not uniquestr on cast
  if pc=nil then exit;

  if not ReadNextWord(pc, ed.kanji) then
    raise EKanjidicParsingException.Create('No kanji field in record');
  if not ReadNextWord(pc, word) then
    raise EKanjidicParsingException.Create('No jis field in record');
  ed.jis := string(word);

 //Rest is dynamic
  while ReadNextWord(pc, word) do begin
    if Length(word)<=0 then continue;

   //Meaning
    if word[1]='{' then begin
      if word[Length(word)]<>'}' then
        raise EKanjidicParsingException.Create('Broken meaning brakets');
      ed.AddMeaning(copy(word, 2, Length(word)-2));
    end else
   //Readings
    if IsKatakana(word[1]) then
      ed.readings[rclass].AddOn(word)
    else
    if IsHiragana(word[1]) then
      ed.readings[rclass].AddKun(word)
    else
   //Reading class, e.g. T0, T1
    if (Length(word)=2) and (word[1]='T') and (word[2]>='0') and (word[2]<='9') then begin
      rclass := Ord(word[2])-Ord('0');
      if rclass>Length(ed.readings) then
        raise EKanjidicParsingException.Create('Unsupported reading class: '+word);
    end else
   //Normal field
    begin
     //Most field keys are 1-char, but these two arent:
      if (Length(word)>=2) and (word[1]='M') and ((word[2]='N') or (word[2]='P')) then
        ed.AddField(word[1]+word[2], copy(word, 3, Length(word)-2))
      else
        ed.AddField(word[1], copy(word, 2, Length(word)-1));
    end;
  end;
end;

end.
