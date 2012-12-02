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


implementation
uses JWBUnit;

//Parses deflection rule from string form into record
//See comments in wakan.cfg for format details.
function ParseDeflectionRule(s: string): TDeflectionRule; inline;
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


end.
