unit JWBUtils;
{
Various high-speed containers used in program.
For now they are grouped together but may be regrouped in the future.
}

//If defined, inline small functions. Bad for debug.
{$DEFINE INLINE}

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
    hiragana_ptr: PAnsiChar;
    katakana_ptr: PAnsiChar;
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
Candidate translation list for JWBUser's dictionary lookups.
Should be reasonably fast.
Not thread safe.
}

type
  TCandidateTranslation = record
    priority: integer; {1..9}
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
  PCandidateTranslation = ^TCandidateTranslation;
  TCandidateTranslationArray = array of TCandidateTranslation;
  TCandidateTranslationList = class
  protected
    FList: TCandidateTranslationArray;
    FListUsed: integer;
    procedure Grow(ARequiredFreeLen: integer);
    function GetItemPtr(Index: integer): PCandidateTranslation;{$IFDEF INLINE} inline;{$ENDIF}
    function MakeNewItem: PCandidateTranslation;
  public
    procedure Add(priority: integer; len: integer; verbType: char; const str: string); overload;
    procedure Add(ct: TCandidateTranslation); overload;{$IFDEF INLINE} inline;{$ENDIF}
    procedure Delete(Index: integer);
    procedure Clear;
    property Count: integer read FListUsed;
    property Items[Index: integer]: PCandidateTranslation read GetItemPtr; default;
  end;

implementation

//Parses deflection rule from string form into record
//See comments in wakan.cfg for format details.
function ParseDeflectionRule(s: string): TDeflectionRule; inline;
var i: integer;
begin
  Result.vt := s[1];
  Result.sufcat := s[2];
  i := pos('->', s);
  Result.infl := copy(s,3,i-1);
  Result.defl := copy(s,i+2,Length(s)-(i+2)+1);
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
 //else we don't grow in less than, say, 20 items
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
 //else we don't grow in less than a batch
  if ARequiredFreeLen < MIN_GROW_LEN then
    ARequiredFreeLen := MIN_GROW_LEN;
  SetLength(FList, Length(FList)+ARequiredFreeLen);
end;

const
  UNICODE_ZERO_CODE: AnsiString = '00000000';

//Makes various safety checks and sets up optimization fields for a rule
procedure TRomajiTranslationTable.SetupRule(r: PRomajiTranslationRule);
begin
 //Make sure hiragana and katakana have at least one 4-char in length, or are disabled
  if Length(r.hiragana)<4 then SetLength(r.hiragana, 0);
  if Length(r.katakana)<4 then SetLength(r.katakana, 0);
 //Drop symbols till the nearest 4-char
  if Length(r.hiragana) mod 4 <> 0 then SetLength(r.hiragana, Length(r.hiragana) - Length(r.hiragana) mod 4);
  if Length(r.katakana) mod 4 <> 0 then SetLength(r.katakana, Length(r.katakana) - Length(r.katakana) mod 4);
 //Setup optimization pointers
  r.hiragana_ptr := pointer(r.hiragana);
  if r.hiragana_ptr=nil then r.hiragana_ptr := @UNICODE_ZERO_CODE[1];
  r.katakana_ptr := pointer(r.katakana);
  if r.katakana_ptr=nil then r.katakana_ptr := @UNICODE_ZERO_CODE[1];
end;

procedure TRomajiTranslationTable.Add(r: TRomajiTranslationRule);
begin
  SetupRule(@r);
  MakeNewItem^ := r;
end;

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


function TCandidateTranslationList.GetItemPtr(Index: integer): PCandidateTranslation;
begin
  Result := @FList[Index]; //valid until next list growth
end;

function TCandidateTranslationList.MakeNewItem: PCandidateTranslation;
begin
 //Thread unsafe
  Grow(1);
  Result := @FList[FListUsed];
  Inc(FListUsed);
end;

//Reserves enough memory to store at least ARequiredFreeLen additional items to list.
procedure TCandidateTranslationList.Grow(ARequiredFreeLen: integer);
const MIN_GROW_LEN = 20;
begin
  if Length(FList)-FListUsed>=ARequiredFreeLen then exit; //already have the space
 //else we don't grow in less than, say, 20 items
  if ARequiredFreeLen < MIN_GROW_LEN then
    ARequiredFreeLen := MIN_GROW_LEN;
  SetLength(FList, Length(FList)+ARequiredFreeLen);
end;

procedure TCandidateTranslationList.Add(priority: integer; len: integer; verbType: char; const str: string);
var item: PCandidateTranslation;
begin
 //Only priorities 0..9 are supported
  if priority>9 then priority := 9;
  if priority<0 then priority := 0;

  item := MakeNewItem;
  item.priority := priority;
  item.len := len;
  item.verbType := verbType;
  item.str := str;
end;

procedure TCandidateTranslationList.Add(ct: TCandidateTranslation);
begin
  Add(ct.priority, ct.len, ct.verbType, ct.str);
end;

//Slow, so try to not use
procedure TCandidateTranslationList.Delete(Index: integer);
begin
 //Properly release the cell's data
  Finalize(FList[Index]);
 //Move everything up one cell
  Move(FList[Index+1], FList[Index], (FListUsed-Index-1)*SizeOf(FList[0]));
  Dec(FListUsed);
 //Zero out last cell
  FillChar(FList[FListUsed], SizeOf(FList[0]), 00); //so that we don't properly release last cell's data, it's been moved to previous cell
end;

procedure TCandidateTranslationList.Clear;
begin
  SetLength(FList, 0);
  FListUsed := 0;
end;



end.
