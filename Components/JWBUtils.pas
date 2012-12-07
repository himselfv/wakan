unit JWBUtils;
{
Various high-speed containers used in program.
For now they are grouped together but may be regrouped in the future.
}

interface
uses SysUtils, Classes;

{
Deflection parsing code and deflection list.
For more info see wakan.cfg.
}
type
 //Verb deflection rule --- see comments in wakan.cfg
 //Rules should probably be parsed on loading and stored in that form,
 //but for now we have what we have. 
  TDeflectionRule = record
    vt: char;       {
      Supported verb types:
        1 for godan verbs
        2 for ichidan verbs
        K for kuru verb
        I for Iku verb
        A for adjective
        N for noun
    }
    sufcat: char;   //suffix category
    infl: string;   //inflected suffix
    defl: string;   //deflected suffix
  end;
  PDeflectionRule = ^TDeflectionRule;
  TDeflectionArray = array of TDeflectionRule;
  TDeflectionList = class
  protected
    FList: TDeflectionArray;
    FListUsed: integer;
    procedure Grow(ARequiredFreeLen: integer);
    function GetItemPtr(Index: integer): PDeflectionRule;{$IFDEF INLINE} inline;{$ENDIF}
    function MakeNewItem: PDeflectionRule;
  public
    procedure Add(r: TDeflectionRule); overload;
    procedure Add(s: string); overload;{$IFDEF INLINE} inline;{$ENDIF}
    procedure Clear;
    property Count: integer read FListUsed;
    property Items[Index: integer]: PDeflectionRule read GetItemPtr; default;
  end;

function ParseDeflectionRule(s: string): TDeflectionRule; inline;


{
Romaji translation table.
For more info see wakan.cfg.
}
type
  TRomajiTranslationRule = record
   { Hiragana and katakana: FStrings. Hex is already upcased! Do not upcase. }
    hiragana: string;
    katakana: string;
    japanese: string;
    english: string;
    czech: string;
   //These pointers always point to somewhere.
   //If hiragana or katakana strings are nil, they're pointing to const strings of '000000000'
   //So there's no need to check for nil; they're also guaranteed to have at least two 4-chars available
   //(for real strings which are one 4-char in length, they have #00 as next 4-char's first symbol,
   // so it won't match to anything)
   {$IFNDEF UNICODE}
    hiragana_ptr: PAnsiChar;
    katakana_ptr: PAnsiChar;
   {$ELSE}
   //These point to two UNICODE chars (4 bytes)
    hiragana_ptr: PWideChar;
    katakana_ptr: PWideChar;
   {$ENDIF}
  end;
  PRomajiTranslationRule = ^TRomajiTranslationRule;
  TRomajiTranslationTable = class
  protected
    FList: array of TRomajiTranslationRule;
    FListUsed: integer;
    procedure Grow(ARequiredFreeLen: integer);
    function GetItemPtr(Index: integer): PRomajiTranslationRule;{$IFDEF INLINE} inline;{$ENDIF}
    function MakeNewItem: PRomajiTranslationRule;
    procedure SetupRule(r: PRomajiTranslationRule);
  public
    procedure Add(r: TRomajiTranslationRule); overload;
    procedure Add(const AHiragana, AKatakana, AJapanese, AEnglish, ACzech: string); overload;
    procedure Add(s: string); overload;{$IFDEF INLINE} inline;{$ENDIF}
    procedure Clear;
    property Count: integer read FListUsed;
    property Items[Index: integer]: PRomajiTranslationRule read GetItemPtr; default;
  end;


{
Candidate lookup list for JWBUser's dictionary lookups.
Should be reasonably fast.
Not thread safe.

Example:
指示を飛ばされている turns into this list:
1 1 指示を飛ばす
1 1 指示を飛ばされつ
1 1 指示を飛ぶ
1 1 指示を飛ばさる
1 2 指示を飛ばされる
1 2 指示る
9 F 指示を飛ばされている
8 F 指示を飛ばされている
7 F 指示を飛ばされてい
6 F 指示を飛ばされて
5 F 指示を飛ばされ
4 F 指示を飛ばさ
3 F 指示を飛ば
2 F 指示を飛
1 F 指示を
0 F 指示
0 F 指
}

type
  TCandidateLookup = record
    priority: integer; {0..anything, 0 is the worst}
    len: integer;  {
      I'm not sure why we can't just take length(str),
      but for now I will replicate how it was done in Wakan with strings }
    verbType: char; {
      Supported verb types:
        Same as in TDeflectionRule +
        F for whatever it stands for (probably "unknown")
    }
    str: string;
  end;
  PCandidateLookup = ^TCandidateLookup;
  TCandidateLookupArray = array of TCandidateLookup;
  TCandidateLookupList = class
  protected
    FList: TCandidateLookupArray;
    FListUsed: integer;
    procedure Grow(ARequiredFreeLen: integer);
    function GetItemPtr(Index: integer): PCandidateLookup;{$IFDEF INLINE} inline;{$ENDIF}
    function MakeNewItem: PCandidateLookup;
  public
    procedure Add(priority: integer; len: integer; verbType: char; const str: string); overload;
    procedure Add(ct: TCandidateLookup); overload;{$IFDEF INLINE} inline;{$ENDIF}
    procedure Delete(Index: integer);
    procedure Clear;
    function Find(len: integer; verbType: char; const str: string): integer;
    property Count: integer read FListUsed;
    property Items[Index: integer]: PCandidateLookup read GetItemPtr; default;
  end;


{
Wakan keeps a list of "graphical lines", i.e. lines as displayed on screen.
They refer to a "logical lines", that is, lines from a text document. 
}
type
  TGraphicalLineInfo = record
    xs: integer; //First character index in a logical line
    ys: integer; //Logical line number
    len: integer; //Number of characters in a logical line
  end;
  PGraphicalLineInfo = ^TGraphicalLineInfo;

  TGraphicalLineList = class
  protected
    FList: array of TGraphicalLineInfo;
    FListUsed: integer;
    procedure Grow(ARequiredFreeLen: integer);
    function GetItemPtr(Index: integer): PGraphicalLineInfo;{$IFDEF INLINE} inline;{$ENDIF}
    function MakeNewItem: PGraphicalLineInfo;
    function InsertNewItem(Index: integer): PGraphicalLineInfo;
  public
    procedure Add(xs, ys, len: integer); overload;
    procedure Insert(Index: Integer; xs, ys, len: integer); overload;
    procedure Delete(Index: integer);
    procedure Clear;
    property Count: integer read FListUsed;
    property Items[Index: integer]: PGraphicalLineInfo read GetItemPtr; default;
  end;


{
Character properties for the editor.
Originally they were stored in strings, taking 9 characters for one property set:
  -90000001
}
type
  TCharacterProps = record
    wordstate: char;
    learnstate: byte;
    dicidx: integer;
    docdic: byte; //document dictionary index.
      //I don't know why the hell do we have "local dictionaries", but it's a pain
      //to replace this system since they're saved into wtt files too.
    procedure SetChar(awordstate: char; alearnstate: byte; adicidx: integer; adocdic: byte); {$IFDEF INLINE}inline;{$ENDIF}
  end;
  PCharacterProps = ^TCharacterProps;
  TCharacterPropArray = array of TCharacterProps;

  TCharacterLineProps = record
    chars: TCharacterPropArray;
    charcount: integer;
    procedure Grow(ARequiredFreeLen: integer);
    function AddChar(): PCharacterProps; overload;
    function AddChar(cp: TCharacterProps): PCharacterProps; overload; {$IFDEF INLINE}inline;{$ENDIF}
    procedure AddChar(awordstate: char; alearnstate: byte; adicidx: integer; adocdic: byte); overload; {$IFDEF INLINE}inline;{$ENDIF}
    procedure AddChars(Count: integer); overload;
    procedure AddChars(const AChars: TCharacterPropArray); overload; {$IFDEF INLINE}inline;{$ENDIF}
    procedure AddChars(const AChars: TCharacterLineProps); overload; {$IFDEF INLINE}inline;{$ENDIF}
    function InsertChar(Index: integer): PCharacterProps;
    procedure InsertChars(Index: integer; Count: integer); overload;
    procedure InsertChars(Index: integer; const AChars: TCharacterPropArray); overload;
    procedure DeleteChar(Index: integer);
    procedure DeleteChars(Index: integer; Count: integer); overload;
    procedure DeleteChars(Index: integer); overload; {$IFDEF INLINE}inline;{$ENDIF}
    function CopySubstr(Index: integer; Count: integer): TCharacterLineProps; overload;
    function CopySubstr(Index: integer): TCharacterLineProps; overload; {$IFDEF INLINE}inline;{$ENDIF}
    procedure Clear;
  end;
  PCharacterLineProps = ^TCharacterLineProps;

  TCharacterPropList = class
  protected
    FList: array of TCharacterLineProps;
    FListUsed: integer;
    procedure Grow(ARequiredFreeLen: integer);
    function GetLinePtr(Index: integer): PCharacterLineProps;{$IFDEF INLINE} inline;{$ENDIF}
  public
    function AddNewLine: PCharacterLineProps;
    function InsertNewLine(Index: integer): PCharacterLineProps;
    procedure AddLine(l: TCharacterLineProps); {$IFDEF INLINE}inline;{$ENDIF}
    procedure InsertLine(Index: integer; l: TCharacterLineProps); {$IFDEF INLINE}inline;{$ENDIF}
    procedure DeleteLine(Index: integer);
    procedure Clear;
    property Count: integer read FListUsed;
    property Lines[Index: integer]: PCharacterLineProps read GetLinePtr; default;
  end;

function CharPropArray(a: array of TCharacterProps): TCharacterPropArray;


implementation
uses JWBStrings;

//Parses deflection rule from string form into record
//See comments in wakan.cfg for format details.
function ParseDeflectionRule(s: string): TDeflectionRule; {$IFDEF INLINE}inline;{$ENDIF}
var i: integer;
begin
  Result.vt := s[1];
  Result.sufcat := s[2];
  i := pos('->', s);
 {$IFDEF UNICODE}
  s := copy(s,3,i-3);
  if s='KKKK' then
    Result.infl := 'KKKK'
  else
    Result.infl := HexToUnicode(s);
  Result.defl := HexToUnicode(copy(s,i+2,Length(s)-(i+2)+1));
 {$ELSE}
  Result.infl := copy(s,3,i-3);
  Result.defl := copy(s,i+2,Length(s)-(i+2)+1);
 {$ENDIF}
end;

function TDeflectionList.GetItemPtr(Index: integer): PDeflectionRule;
begin
  Result := @FList[Index]; //valid until next list growth
end;

function TDeflectionList.MakeNewItem: PDeflectionRule;
begin
 //Thread unsafe
  Grow(1);
  Result := @FList[FListUsed];
  Inc(FListUsed);
end;

//Reserves enough memory to store at least ARequiredFreeLen additional items to list.
procedure TDeflectionList.Grow(ARequiredFreeLen: integer);
const MIN_GROW_LEN = 20;
begin
  if Length(FList)-FListUsed>=ARequiredFreeLen then exit; //already have the space
 //else we don't grow in less than a chunk
  if ARequiredFreeLen < MIN_GROW_LEN then
    ARequiredFreeLen := MIN_GROW_LEN;
  SetLength(FList, Length(FList)+ARequiredFreeLen);
end;

procedure TDeflectionList.Add(r: TDeflectionRule);
begin
  MakeNewItem^ := r;
end;

procedure TDeflectionList.Add(s: string);
begin
  Add(ParseDeflectionRule(s));
end;

procedure TDeflectionList.Clear;
begin
  SetLength(FList, 0);
  FListUsed := 0;
end;



function TRomajiTranslationTable.GetItemPtr(Index: integer): PRomajiTranslationRule;
begin
  Result := @FList[Index]; //valid until next list growth
end;

function TRomajiTranslationTable.MakeNewItem: PRomajiTranslationRule;
begin
 //Thread unsafe
  Grow(1);
  Result := @FList[FListUsed];
  Inc(FListUsed);
end;

//Reserves enough memory to store at least ARequiredFreeLen additional items to list.
procedure TRomajiTranslationTable.Grow(ARequiredFreeLen: integer);
const MIN_GROW_LEN = 40;
begin
  if Length(FList)-FListUsed>=ARequiredFreeLen then exit; //already have the space
 //else we don't grow in less than a chunk
  if ARequiredFreeLen < MIN_GROW_LEN then
    ARequiredFreeLen := MIN_GROW_LEN;
  SetLength(FList, Length(FList)+ARequiredFreeLen);
end;

const
 {$IFDEF UNICODE}
  UNICODE_ZERO_CODE: string = #00#00;
 {$ELSE}
  UNICODE_ZERO_CODE: string = '00000000';
 {$ENDIF}

//Makes various safety checks and sets up optimization fields for a rule
procedure TRomajiTranslationTable.SetupRule(r: PRomajiTranslationRule);
begin
 {$IFNDEF UNICODE}
 //Make sure hiragana and katakana have at least one 4-char in length, or are disabled
  if Length(r.hiragana)<4 then SetLength(r.hiragana, 0);
  if Length(r.katakana)<4 then SetLength(r.katakana, 0);
 //Drop symbols till the nearest 4-char
  if Length(r.hiragana) mod 4 <> 0 then SetLength(r.hiragana, Length(r.hiragana) - Length(r.hiragana) mod 4);
  if Length(r.katakana) mod 4 <> 0 then SetLength(r.katakana, Length(r.katakana) - Length(r.katakana) mod 4);
 {$ENDIF}
 //Setup optimization pointers
  r.hiragana_ptr := pointer(r.hiragana);
  if r.hiragana_ptr=nil then r.hiragana_ptr := pointer(UNICODE_ZERO_CODE);
  r.katakana_ptr := pointer(r.katakana);
  if r.katakana_ptr=nil then r.katakana_ptr := pointer(UNICODE_ZERO_CODE);
end;

procedure TRomajiTranslationTable.Add(r: TRomajiTranslationRule);
begin
  SetupRule(@r);
  MakeNewItem^ := r;
end;

//Hiragana and katakana must be in decoded format (unicode on UFCHAR builds)
procedure TRomajiTranslationTable.Add(const AHiragana, AKatakana, AJapanese, AEnglish, ACzech: string);
var r: PRomajiTranslationRule;
begin
  r := MakeNewItem;
  r.hiragana := AHiragana;
  r.katakana := AKatakana;
  r.japanese := AJapanese;
  r.english := AEnglish;
  r.czech := ACzech;
  SetupRule(r);
end;

//Parses romaji translation rule from string form into record
//See comments in wakan.cfg for format details.
function ParseRomajiTranslationRule(s: string): TRomajiTranslationRule; {$IFDEF INLINE}inline;{$ENDIF}
var s_parts: TStringArray;
begin
  s_parts := SplitStr(s, 5);
 {$IFDEF UNICODE}
  Result.hiragana := HexToUnicode(s_parts[0]);
  Result.katakana := HexToUnicode(s_parts[1]);
 {$ELSE}
  Result.hiragana := Uppercase(s_parts[0]);
  Result.katakana := Uppercase(s_parts[1]);
 {$ENDIF}
  Result.japanese := s_parts[2];
  Result.english := s_parts[3];
  Result.czech := s_parts[4];
end;

procedure TRomajiTranslationTable.Add(s: string);
begin
  Add(ParseRomajiTranslationRule(s));
end;

procedure TRomajiTranslationTable.Clear;
begin
  SetLength(FList, 0);
  FListUsed := 0;
end;


function TCandidateLookupList.GetItemPtr(Index: integer): PCandidateLookup;
begin
  Result := @FList[Index]; //valid until next list growth
end;

function TCandidateLookupList.MakeNewItem: PCandidateLookup;
begin
 //Thread unsafe
  Grow(1);
  Result := @FList[FListUsed];
  Inc(FListUsed);
end;

//Reserves enough memory to store at least ARequiredFreeLen additional items to list.
procedure TCandidateLookupList.Grow(ARequiredFreeLen: integer);
const MIN_GROW_LEN = 20;
begin
  if Length(FList)-FListUsed>=ARequiredFreeLen then exit; //already have the space
 //else we don't grow in less than a chunk
  if ARequiredFreeLen < MIN_GROW_LEN then
    ARequiredFreeLen := MIN_GROW_LEN;
  SetLength(FList, Length(FList)+ARequiredFreeLen);
end;

//str must be in decoded format (Unicode on UFCHAR builds)
procedure TCandidateLookupList.Add(priority: integer; len: integer; verbType: char;
  const str: string);
var item: PCandidateLookup;
begin
 //Only priorities >=0 are supported
  if priority<0 then priority := 0;

  item := MakeNewItem;
  item.priority := priority;
  item.len := len;
  item.verbType := verbType;
  item.str := str;
end;

procedure TCandidateLookupList.Add(ct: TCandidateLookup);
begin
  Add(ct.priority, ct.len, ct.verbType, ct.str);
end;

//Slow, so try to not use
procedure TCandidateLookupList.Delete(Index: integer);
begin
 //Properly release the cell's data
  Finalize(FList[Index]);
 //Move everything up one cell
  Move(FList[Index+1], FList[Index], (FListUsed-Index-1)*SizeOf(FList[0]));
  Dec(FListUsed);
 //Zero out last cell
  FillChar(FList[FListUsed], SizeOf(FList[0]), 00); //so that we don't properly release last cell's data, it's been moved to previous cell
end;

procedure TCandidateLookupList.Clear;
begin
  SetLength(FList, 0);
  FListUsed := 0;
end;

function TCandidateLookupList.Find(len: integer; verbType: char; const str: string): integer;
var k: integer;
begin
  Result := -1;
  for k:=0 to Self.Count-1 do
    if (FList[k].len=len)
    and (FList[k].verbType=verbType)
    and (FList[k].str=str) then begin
      Result := k;
      break;
    end;
end;



function TGraphicalLineList.GetItemPtr(Index: integer): PGraphicalLineInfo;
begin
  Assert(Index<FListUsed);
  Result := @FList[Index]; //valid until next list growth
end;

function TGraphicalLineList.MakeNewItem: PGraphicalLineInfo;
begin
 //Thread unsafe
  Grow(1);
  Result := @FList[FListUsed];
  Inc(FListUsed);
end;

function TGraphicalLineList.InsertNewItem(Index: integer): PGraphicalLineInfo;
begin
  Grow(1);
//  Initialize(FList[FListUsed]); //needs no initialize for now
 //Move everything down one cell
  Move(FList[Index], FList[Index+1], (FListUsed-Index)*SizeOf(FList[0]));
  Inc(FListUsed);
 //Zero out the cell so that no reference counting is done
  FillChar(FList[Index], SizeOf(FList[Index]), 00);
  Result := @FList[Index];
end;

//Reserves enough memory to store at least ARequiredFreeLen additional items to list.
procedure TGraphicalLineList.Grow(ARequiredFreeLen: integer);
const MIN_GROW_LEN = 50; //there's usually a lot of lines in the document so grow in large chunks
begin
  if Length(FList)-FListUsed>=ARequiredFreeLen then exit; //already have the space
 //else we don't grow in less than a chunk
  if ARequiredFreeLen < MIN_GROW_LEN then
    ARequiredFreeLen := MIN_GROW_LEN;
  SetLength(FList, Length(FList)+ARequiredFreeLen);
end;

procedure TGraphicalLineList.Add(xs, ys, len: integer);
var item: PGraphicalLineInfo;
begin
  item := MakeNewItem;
  item.xs := xs;
  item.ys := ys;
  item.len := len;
end;

procedure TGraphicalLineList.Insert(Index: Integer; xs, ys, len: integer);
var item: PGraphicalLineInfo;
begin
  item := InsertNewItem(Index);
  item.xs := xs;
  item.ys := ys;
  item.len := len;
end;

//Slow, so try to not use
procedure TGraphicalLineList.Delete(Index: integer);
begin
 //Properly release the cell's data
 // Finalize(FList[Index]); //needs no finalize for now
   //I know it'd be better to keep the call but Delphi emits a warning
 //Move everything up one cell
  Move(FList[Index+1], FList[Index], (FListUsed-Index-1)*SizeOf(FList[0]));
  Dec(FListUsed);
 //Zero out last cell
  FillChar(FList[FListUsed], SizeOf(FList[0]), 00); //so that we don't properly release last cell's data, it's been moved to previous cell
end;

procedure TGraphicalLineList.Clear;
begin
  SetLength(FList, 0);
  FListUsed := 0;
end;


{ Character props }

procedure TCharacterProps.SetChar(awordstate: char; alearnstate: byte;
  adicidx: integer; adocdic: byte);
begin
  wordstate := awordstate;
  learnstate := alearnstate;
  dicidx := adicidx;
  docdic := adocdic;
 //And reset the rest
end;

//Reserves enough memory to store at least ARequiredFreeLen additional lines.
procedure TCharacterLineProps.Grow(ARequiredFreeLen: integer);
const MIN_GROW_LEN = 40;
begin
  if Length(chars)-charcount>=ARequiredFreeLen then exit; //already have the space
 //else we don't grow in less than a chunk
  if ARequiredFreeLen < MIN_GROW_LEN then
    ARequiredFreeLen := MIN_GROW_LEN;
  SetLength(chars, Length(chars)+ARequiredFreeLen);
end;

function TCharacterLineProps.AddChar: PCharacterProps;
begin
  Grow(1);
  Result := @chars[charcount];
  Inc(charcount);
end;

function TCharacterLineProps.AddChar(cp: TCharacterProps): PCharacterProps;
begin
  AddChar^ := cp;
end;

procedure TCharacterLineProps.AddChar(awordstate: char; alearnstate: byte; adicidx: integer; adocdic: byte);
begin
  with AddChar^ do begin
    wordstate := awordstate;
    learnstate := alearnstate;
    dicidx := adicidx;
    docdic := adocdic;
  end;
end;

procedure TCharacterLineProps.AddChars(Count: integer);
begin
  Grow(Count);
  Inc(charcount, Count);
end;

procedure TCharacterLineProps.AddChars(const AChars: TCharacterPropArray);
var i: integer;
begin
  Grow(Length(AChars));
  for i := 0 to Length(AChars) - 1 do
    AddChar(AChars[i]);
end;

procedure TCharacterLineProps.AddChars(const AChars: TCharacterLineProps);
var i: integer;
begin
  Grow(AChars.charcount);
  for i := 0 to AChars.charcount - 1 do
    AddChar(AChars.chars[i]);
end;

function TCharacterLineProps.InsertChar(Index: integer): PCharacterProps;
begin
  Grow(1);
//  Initialize(FList[FListUsed]); //needs no initialize for now
 //Move everything down one cell
  Move(chars[Index], chars[Index+1], (charcount-Index)*SizeOf(chars[0]));
  Inc(charcount);
 //Zero out the cell so that no reference counting is done
  FillChar(chars[Index], SizeOf(chars[Index]), 00);
  Result := @chars[Index];
end;

//Inserts a bunch of empty characters, accessible by chars[Index]...chars[Index+Count-1]
procedure TCharacterLineProps.InsertChars(Index: integer; Count: integer);
begin
  Grow(Count);
 //Move everything down one cell
  Move(chars[Index], chars[Index+Count], (charcount-(Index+Count))*SizeOf(chars[0]));
  Inc(charcount, Count);
 //Zero out the cells so that no reference counting is done
  FillChar(chars[Index], SizeOf(chars[Index])*Count, 00);
end;

procedure TCharacterLineProps.InsertChars(Index: integer; const AChars: TCharacterPropArray);
var i: integer;
begin
  InsertChars(Index, Length(AChars));
  for i := 0 to Length(AChars) - 1 do
    chars[Index+i] := AChars[i];
end;

procedure TCharacterLineProps.DeleteChar(Index: integer);
begin
 //Properly release the cell's data
 // Finalize(chars[Index]); //needs no finalize for now
 //Move everything up one cell
  Move(chars[Index+1], chars[Index], (charcount-Index-1)*SizeOf(chars[0]));
  Dec(charcount);
 //Zero out last cell
  FillChar(chars[charcount], SizeOf(chars[0]), 00); //so that we don't properly release last cell's data, it's been moved to previous cell
end;

//See CopySubstr comment about count
procedure TCharacterLineProps.DeleteChars(Index: integer; Count: integer);
var i: integer;
begin
  if Count<=0 then
    Count := charcount+Count;
 //Move everything up Count cell
  Move(chars[Index+Count], chars[Index], (charcount-(Index+Count)-1)*SizeOf(chars[0]));
  Dec(charcount, Count);
 //Zero out last cells
  FillChar(chars[charcount], SizeOf(chars[0])*Count, 00);
end;

procedure TCharacterLineProps.DeleteChars(Index: integer);
begin
  Assert(Index<charcount);
  DeleteChars(Index, charcount-Index)
end;

//If Count<0 then it means "Till the end of the string minus Count characters"
//  CopySubstr(0,0) == empty string
//  CopySubstr(0)   == copy entire string
//  CopySubStr(0,x) == copy from start to x
//  CopySubstr(x,-1) == copy from x to end-1
//  CopySubStr(x)   == copy from x to end
function TCharacterLineProps.CopySubstr(Index: integer; Count: integer): TCharacterLineProps;
begin
  if Count<=0 then
    Count := charcount+Count;
  Result.chars := copy(Self.chars, Index, Count);
  Result.charcount := Count;
end;

function TCharacterLineProps.CopySubstr(Index: integer): TCharacterLineProps;
begin
  Assert(Index<charcount);
  CopySubstr(Index, charcount-Index);
end;

procedure TCharacterLineProps.Clear;
begin
  charcount := 0;
  //No need to clear the array
end;

//Reserves enough memory to store at least ARequiredFreeLen additional lines.
procedure TCharacterPropList.Grow(ARequiredFreeLen: integer);
const MIN_GROW_LEN = 50; //there's usually a lot of lines in the document so grow in large chunks
begin
  if Length(FList)-FListUsed>=ARequiredFreeLen then exit; //already have the space
 //else we don't grow in less than a chunk
  if ARequiredFreeLen < MIN_GROW_LEN then
    ARequiredFreeLen := MIN_GROW_LEN;
  SetLength(FList, Length(FList)+ARequiredFreeLen);
end;

function TCharacterPropList.GetLinePtr(Index: integer): PCharacterLineProps;
begin
  Assert(Index<FListUsed);
  Result := @FList[Index]; //valid until next list growth
end;

function TCharacterPropList.AddNewLine: PCharacterLineProps;
begin
 //Thread unsafe
  Grow(1);
  Result := @FList[FListUsed];
  Inc(FListUsed);
end;

function TCharacterPropList.InsertNewLine(Index: integer): PCharacterLineProps;
begin
  Grow(1);
  Initialize(FList[FListUsed]);
 //Move everything down one cell
  Move(FList[Index], FList[Index+1], (FListUsed-Index)*SizeOf(FList[0]));
  Inc(FListUsed);
 //Zero out the cell so that no reference counting is done
  FillChar(FList[Index], SizeOf(FList[Index]), 00);
  Result := @FList[Index];
end;

procedure TCharacterPropList.AddLine(l: TCharacterLineProps);
begin
  AddNewLine^ := l;
end;

procedure TCharacterPropList.InsertLine(Index: integer; l: TCharacterLineProps);
begin
  InsertNewLine(Index)^ := l;
end;

procedure TCharacterPropList.DeleteLine(Index: integer);
begin
 //Properly release the cell's data
  Finalize(FList[Index]);
 //Move everything up one cell
  Move(FList[Index+1], FList[Index], (FListUsed-Index-1)*SizeOf(FList[0]));
  Dec(FListUsed);
 //Zero out last cell
  FillChar(FList[FListUsed], SizeOf(FList[0]), 00); //so that we don't properly release last cell's data, it's been moved to previous cell
end;

procedure TCharacterPropList.Clear;
begin
  SetLength(FList, 0);
  FListUsed := 0;
end;

function CharPropArray(a: array of TCharacterProps): TCharacterPropArray;
var i: integer;
begin
  SetLength(Result, Length(a));
  for i := 0 to Length(a) - 1 do
    Result[i] := a[i];
end;


end.
