unit JWBKanaConv;
{
Converts kana to romaji and back according to a set of rules.
This is a very hot codepath when translating in Wakan. Therefore we use
balanced binary trees and do everything in a very optimal way.

Throughout this file Romaji stands to mean also Polaji, Kiriji, Pinyin etc.

Unicode only.
}

interface
uses Classes, JWBStrings, BalancedTree;

{$DEFINE KATA_AT_SEARCH}
{ Convert katakana to hiragana at search instead of looking for it as is.
 Atm the only supported way. }

type
  PFCharPtr = PWideChar;

 { Common classes for handling roma<->kata translations. }

 { A rule assigning a set of Romaji to one Phonetic syllable.
  First Romaji entry is the one which will be used for Phonetic -> Romaji replacements }
  TTranslationRule = record
    Phonetic: FString;
    Romaji: array of string;
    function FindRomaji(const ARomaji: string): integer;
    procedure AddRomaji(const ARomaji: string);
    procedure Add(const ARule: TTranslationRule);
  end;
  PTranslationRule = ^TTranslationRule;
  TKanaTranslationRule = TTranslationRule; //Phonetic is *always* Hiraganized
  TBopomofoTranslationRule = TTranslationRule; //Phonetic is Bopomofo

 { Binary search node for phonetic syllables. In fact used only for Kana.
  Pinyin has its own lookup. }
  TPhoneticRuleNode = class(TBinTreeItem)
  protected
    FText: FString;
   { This pointer must always point somewhere.
    If the string is nil, it's pointing to the const string of '000000000', so
    there's no need to check for nil.
    It's also guaranteed to have at least two FChars available. For real strings
    which are one FChar in length, it has #00 as next 4-char's first symbol, so
    it won't match to anything }
    FTextPtr: PFCharPtr;
    FRule: PTranslationRule;
  public
    constructor Create(const AText: FString; ARule: PTranslationRule);
    function Compare(a:TBinTreeItem):Integer; override;
    procedure Copy(ToA:TBinTreeItem); override;
  end;
  TPhoneticRuleNode1 = class(TPhoneticRuleNode) //1-char comparison
    function CompareData(const a):Integer; override;
  end;
  TPhoneticRuleNode2 = class(TPhoneticRuleNode) //2-char comparison
    function CompareData(const a):Integer; override;
  end;

 { There's a romaji index entry for every romaji syllable at every priority.
  It links to all the table entries it can potentially mean. }
  TRomajiIndexEntry = class(TBinTreeItem)
  public
    roma: string;
    prior: integer;
    entries: array of PTranslationRule;
    constructor Create(const ARoma: string; APrior: integer);
    function CompareData(const a):Integer; override;
    function Compare(a:TBinTreeItem):Integer; override;
    procedure Copy(ToA:TBinTreeItem); override;
    procedure AddEntry(const AEntry: PTranslationRule);
    procedure Merge(const A: TRomajiIndexEntry);
    procedure SortEntries(const APhoneticList: TStringArray);
  end;
  PRomajiIndexEntry = ^TRomajiIndexEntry;

  TTranslationTable = class
  protected
    FList: array of PTranslationRule;
    FListUsed: integer;
    FRomajiIndex: TBinTree;
    procedure Grow(ARequiredFreeLen: integer);
    function GetItem(Index: integer): PTranslationRule; inline;
    function MakeNewItem: PTranslationRule;
    procedure AddToIndex(r: PTranslationRule; APrior: integer); virtual;
    procedure AddRomajiToIndex(r: PTranslationRule; AFirst: integer; APrior: integer);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear; virtual;
    procedure Add(const r: TTranslationRule; APrior: integer);
    function FindItem(const APhonetic: string): PTranslationRule; virtual;
    function IndexOf(const AItem: PTranslationRule): integer;
    property Count: integer read FListUsed;
    property Items[Index: integer]: PTranslationRule read GetItem; default;
  end;
  TKanaTranslationTable = class(TTranslationTable)
  protected
    FOneCharTree: TBinTree;
    FTwoCharTree: TBinTree;
    procedure AddToIndex(r: PTranslationRule; APrior: integer); override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear; override;
    function FindItem(const APhonetic: string): PTranslationRule; override;
  end;
  TBopomofoTranslationTable = TTranslationTable; //nothing special atm

  TRomajiReplacementRule = record
    s_find: string;
    s_repl: string;
    pref: char; //H, K, #00 means any
  end;
  PRomajiReplacementRule = ^TRomajiReplacementRule;

  TRomajiReplacementTable = class
  protected
    FList: array of TRomajiReplacementRule;
    FListUsed: integer;
    procedure Grow(ARequiredFreeLen: integer);
    function GetItemPtr(Index: integer): PRomajiReplacementRule; inline;
    function MakeNewItem: PRomajiReplacementRule;
  public
    procedure Clear;
    procedure Add(const r: TRomajiReplacementRule);
    function Find(const r: TRomajiReplacementRule): integer; overload;
    property Count: integer read FListUsed;
    property Items[Index: integer]: PRomajiReplacementRule read GetItemPtr; default;
  end;

 {
  Kana and romaji in dictionaries can contain latin letters and punctuation.
  In general, it is safe to mix latin letters with katakana/bopomofo, but not with
  romaji (i.e. is "ding" an english word or a pinyin syllable?)

  Dictionaries address this in different ways: EDICT stores latin as katakana,
  CCEDICT separates letters with spaces ("D N A jian4 ding4").

  Functions here can convert pure romaji/kana both ways, and latin+kana to romaji.
  It is recommended that you use kana+latin as internal format, and only convert
  to romaji on presentation.

  When user enters romaji, either require that to be pure or match it against
  some "romaji signature" (same for ding-syllable and DING-latin).
      Signature   Bopomofo    Translation
      ding        [ding]      Chinese for "person who programs too much"
      ding        DING        English for "ding"
 }

  TResolveFlag = (
    rfConvertLatin,
      //Convert latin letters to target notation instead of treating as invalid chars.
      //This is only reliable when doing Bopomofo->PinYin, not the reverse.
    rfConvertPunctuation,
      //Convert some punctuation to target notation.
    rfDeleteInvalidChars
      //Delete invalid characters instead of replacing with '?'
  );
  TResolveFlags = set of TResolveFlag;

 {
  Instantiate and call descendants:

    conv := TKanaTranslator.Create;
    conv.LoadFromFile('Hepburn.roma');
    roma := conv.KanaToRomaji(kana);
    kana := conv.RomajiToKana(roma);

  You can load several transliterations but they may conflict:

    conv.LoadFromFile('Hepburn.roma');
    conv.LoadFromFile('Kiriji.roma');
    kana := conv.RomajiToKana(kiriji);

  }
  TRomajiTranslator = class
  protected
    FTablesLoaded: integer;
    FTrans: TTranslationTable;
    FReplKtr: TRomajiReplacementTable;
    FReplRtk: TRomajiReplacementTable;
    procedure CreateTranslationTable; virtual;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure LoadFromFile(const filename: string);
    procedure LoadFromStrings(const sl: TStrings);
    function KanaToRomaji(const AString: FString; AFlags: TResolveFlags): string; virtual; abstract;
    function RomajiToKana(const AString: string; AFlags: TResolveFlags): FString; virtual; abstract;
  end;

 { At this moment hiragana and katakana, lowercase and uppercase chars are equal.
  The code converts everything to lowercase, hiragana internally before passing
  to any of the common code.
  Exceptions:
   - entries in kana index are created both for hira and kata, for speed.
   - although all romaji is lowercased in RomajiToKana, romaji rules loaded from
    file are kept as is. This is because romaji tables sometimes use uppercase
    as a temporary form between translation and replacements }
  TKanaTranslator = class(TRomajiTranslator)
  protected
    FTrans: TKanaTranslationTable;
    procedure CreateTranslationTable; override;
    function SingleKanaToRomaji(var ps: PFChar; flags: TResolveFlags): string;
    procedure RomaReplace(var s: string; const r: PRomajiReplacementRule);
  public
   { Always generates lowcase romaji }
    function KanaToRomaji(const AString: FString; AFlags: TResolveFlags): string; override;
   { Supports inline markers:
      K -- from this point it's katakana
      H -- from this point it's hiragana }
    function RomajiToKana(const AString: string; AFlags: TResolveFlags): FString; override;
   { Someone might add an option to treat BIG LETTERS as katakana instead }
  end;

  TPinYinTranslator = class(TRomajiTranslator)
  protected
    function BopomofoBestMatch(const AText: FString): integer;
    function PinyinBestMatch(const AText: string; out ARomaIdx: integer): integer;
    function PinyinPartialMatch(const ASyllable: string): integer;
  public
    function KanaToRomaji(const AString: FString; AFlags: TResolveFlags): string; override;
    function RomajiToKana(const AString: string; AFlags: TResolveFlags): FString; override;
  end;

function IsAllowedPunctuation(c:WideChar): boolean;
function ConvertPunctuation(c:WideChar): char;
function ConvertPunctuationF(c:FChar): char;

{ Converts all hiragana characters in a string to katakana and vice versa }
function ToHiragana(const ch: FChar): FChar; inline; overload;
function ToKatakana(const ch: FChar): FChar; inline; overload;
function ToHiragana(const s: FString): FString; overload;
function ToKatakana(const s: FString): FString; overload;


{
In PinYin tone markers (1-5) follow every syllable.
See http://en.wikipedia.org/wiki/PinYin#Tones

There are 4 different ways to store these:
1. pin3yin4. Raw pinyin. This is what people type on the keyboard.
2. pínín. Pinyin for display.
3. ㄆㄧㄣˇㄧ. Bopomofo text tones.
4. ㄆㄧㄣ[F033]ㄧ. Bopomofo F03*-tones (see below).

In all cases tones can be omitted, and books usually do omit them.

#2 and #3 are weak: not all tones have markers and it's impossible to distinguish
between "syllable with markerless tone" and "syllable with tone omitted".
With #2 it's also a problem if pinyin is merged with valid latin text.
}

const
 { Tone markers are encoded as F030+[0..5] }
  UH_PY_TONE = #$F030;

function fpytone(const i: byte): fchar; inline; //creates a character which encodes PinYin tone
function fpydetone(const ch: fchar): byte; inline; //returns 0-5 if the character encodes tone, or 255
function fpygettone(const s: fstring): byte; inline; //returns 0-5 if the first character encodes tone, or 255
function fpyextrtone(var s: fstring): byte; inline; //same, but deletes the tone from the string

{ pin4yin4<->pínín conversion
 Works only for pure pinyin, although tolerant for some punctuation and limited latin. }
function ConvertPinYin(const str:string):FString;
function DeconvertPinYin(romac: TPinYinTranslator; const str:FString):string;

{ FF0*-enhanced bopomofo -> Tonemark-enhanced bopomofo
 There could be parentless tone marks already in the string, so the result
 is slightly less unambiguous. }
function ConvertBopomofo(const str:FString):FString;

implementation
uses SysUtils;

//True if c is a punctuation mark we allow in kanji and kana fields.
function IsAllowedPunctuation(c:WideChar): boolean;
begin
  Result :=
    (c='·') or (c=',') //in CCEDICT
    or (c='・') or (c='、') or (c='〜'); //in EDICT2
end;

//When we need to store punctuation into pinyin, we have to make it ansi
function ConvertPunctuation(c:WideChar): char;
begin
  case c of
    '·': Result:='-';
    '・': Result:='-';
    '、': Result:=',';
    '〜': Result:='~';
  else
    Result := c;
  end;
end;

function ConvertPunctuationF(c:FChar): char;
begin
  Result := ConvertPunctuation(c);
end;

function ToHiragana(const ch: FChar): FChar;
begin
  if (Ord(ch)>=$30A1) and (Ord(ch)<=$30F4) then
    Result := Chr(Ord(ch)-$60)
  else
    Result := ch;
end;

function ToKatakana(const ch: FChar): FChar;
begin
  if (Ord(ch)>=$3041) and (Ord(ch)<=$3094) then
    Result := Chr(Ord(ch)+$60)
  else
    Result := ch;
end;

//Converts all katakana in the string to hiragana
function ToHiragana(const s: FString): FString;
var i: integer;
begin
  Result := '';
  for i := 1 to flength(s) do
    Result := Result + ToHiragana(fgetch(s,i));
end;

//Converts all hiragana in the string to katakana
function ToKatakana(const s: FString): FString;
var i: integer;
begin
  Result := '';
  for i := 1 to flength(s) do
    Result := Result + ToKatakana(fgetch(s,i));
end;


{ TTranslationRule }

function TTranslationRule.FindRomaji(const ARomaji: string): integer;
var i: integer;
begin
  Result := -1;
  for i := 0 to Length(romaji)-1 do
    if romaji[i]=ARomaji then begin
      Result := i;
      break;
    end;
end;

procedure TTranslationRule.AddRomaji(const ARomaji: string);
begin
  SetLength(romaji, Length(romaji)+1);
  romaji[Length(romaji)-1] := ARomaji;
end;

procedure TTranslationRule.Add(const ARule: TTranslationRule);
var i: integer;
begin
  //Add new parts
  for i := 0 to Length(ARule.romaji)-1 do
    if Self.FindRomaji(ARule.romaji[i])<0 then
      Self.AddRomaji(ARule.romaji[i]);
end;

{ TPhoneticRuleNode }

const
  UNICODE_ZERO_CODE: string = #00#00;

constructor TPhoneticRuleNode.Create(const AText: FString; ARule: PTranslationRule);
begin
  inherited Create;
  Self.FText := AText;
 //Setup optimization pointer
  Self.FTextPtr := pointer(FText);
  if Self.FTextPtr=nil then Self.FTextPtr := pointer(UNICODE_ZERO_CODE);
  Self.FRule := ARule;
end;

{ Returns a pointer to an integer p+#c counting from 0
 This is pretty fast when inlined, basically the same as typing that inplace, so use without fear.
 You can even put c==0 and the code will be almost eliminated at compilation time. Delphi is smart! }
function IntgOff(p: pointer; c: integer): PInteger; inline;
begin
  Result := PInteger(IntPtr(p)+c*4);
end;

function TPhoneticRuleNode1.CompareData(const a):Integer;
begin
  Result := PWord(a)^ - PWord(FTextPtr)^;
end;

//TRomajiRuleNode expects PFChar (i.e. pointer to an FString data) with at least
//two FChars in it.
function TPhoneticRuleNode2.CompareData(const a):Integer;
begin
 //Compare two characters at once
  Result := PInteger(a)^-PInteger(FTextPtr)^;
end;

function TPhoneticRuleNode.Compare(a:TBinTreeItem):Integer;
begin
  Result := CompareData(TPhoneticRuleNode(a).FTextPtr);
end;

procedure TPhoneticRuleNode.Copy(ToA:TBinTreeItem);
begin
  TPhoneticRuleNode(ToA).FText := Self.FText;
  TPhoneticRuleNode(ToA).FTextPtr := Self.FTextPtr;
  TPhoneticRuleNode(ToA).FRule := Self.FRule;
end;

{ TRomajiIndexEntry }

constructor TRomajiIndexEntry.Create(const ARoma: string; APrior: integer);
begin
  inherited Create;
  Self.roma := ARoma;
  Self.prior := APrior;
end;

function TRomajiIndexEntry.CompareData(const a):Integer;
begin
  Result := 0; //not supported
end;

function TRomajiIndexEntry.Compare(a:TBinTreeItem):Integer;
begin
 //First sort by priority, lower is better
  Result := (TRomajiIndexEntry(a).prior-Self.prior);
  if Result<>0 then exit;

 //Then by length, longer is better
  Result := Length(self.roma)-Length(TRomajiIndexEntry(a).roma);
  if Result<>0 then exit;

 //Then by the text itself
  Result := AnsiCompareStr(TRomajiIndexEntry(a).roma, Self.roma);
end;

procedure TRomajiIndexEntry.Copy(ToA:TBinTreeItem);
begin
  TRomajiIndexEntry(ToA).roma := Self.roma;
  TRomajiIndexEntry(ToA).prior := Self.prior;
  TRomajiIndexEntry(ToA).entries := System.Copy(Self.entries);
end;

procedure TRomajiIndexEntry.AddEntry(const AEntry: PTranslationRule);
begin
  SetLength(Self.entries, Length(Self.entries)+1);
  Self.entries[Length(Self.entries)-1] := AEntry;
end;

procedure TRomajiIndexEntry.Merge(const A: TRomajiIndexEntry);
var i, j: integer;
begin
  i := Length(Self.entries);
  SetLength(Self.entries, i+Length(A.entries));
  for j := 0 to Length(A.entries) do
    Self.entries[i+j] := A.entries[J];
end;

//Reorders Entries to match the order of APhoneticList. Entries which are not
//mentioned are moved to the tail.
procedure TRomajiIndexEntry.SortEntries(const APhoneticList: TStringArray);
var i, j, j_pos, j_found: integer;
  ARule: PTranslationRule;
begin
  j_pos := 0;
  for i := 0 to Length(APhoneticList)-1 do begin
    j_found := -1;
    for j := j_pos to Length(entries)-1 do
      if entries[j].Phonetic=APhoneticList[i] then begin
        j_found := j;
        break;
      end;
    if j_found>=0 then begin
      ARule := entries[j_found];
      Move(entries[j_pos], entries[j_pos+1], SizeOf(entries[j_pos])*(j_found-j_pos));
      entries[j_pos] := ARule;
      Inc(j_pos);
    end;
  end;
end;

{ TRomajiTranslationTable }

constructor TTranslationTable.Create;
begin
  inherited;
  FRomajiIndex := TBinTree.Create;
end;

destructor TTranslationTable.Destroy;
begin
  FreeAndNil(FRomajiIndex);
  inherited;
end;

procedure TTranslationTable.Clear;
var i: integer;
begin
  FRomajiIndex.Clear;
  for i := 0 to FListUsed - 1 do
    Dispose(FList[i]);
  SetLength(FList, 0);
  FListUsed := 0;
end;

function TTranslationTable.GetItem(Index: integer): PTranslationRule;
begin
  Result := FList[Index]; //valid until next list growth
end;

function TTranslationTable.MakeNewItem: PTranslationRule;
begin
 //Thread unsafe
  Grow(1);
  New(Result);
  FList[FListUsed] := Result;
  Inc(FListUsed);
end;

//Reserves enough memory to store at least ARequiredFreeLen additional items to list.
procedure TTranslationTable.Grow(ARequiredFreeLen: integer);
const MIN_GROW_LEN = 40;
begin
  if Length(FList)-FListUsed>=ARequiredFreeLen then exit; //already have the space
 //else we don't grow in less than a chunk
  if ARequiredFreeLen < MIN_GROW_LEN then
    ARequiredFreeLen := MIN_GROW_LEN;
  SetLength(FList, Length(FList)+ARequiredFreeLen);
end;

procedure TTranslationTable.AddToIndex(r: PTranslationRule; APrior: integer);
begin
end;

procedure TTranslationTable.AddRomajiToIndex(r: PTranslationRule;
  AFirst: integer; APrior: integer);
var i: integer;
  bi, bi_ex: TRomajiIndexEntry;
begin
  for i := AFirst to Length(r.romaji)-1 do begin
    bi := TRomajiIndexEntry.Create(r.romaji[i], APrior);
    bi_ex := TRomajiIndexEntry(FRomajiIndex.AddOrSearch(bi));
    if bi_ex<>bi then begin //already existed
      FreeAndNil(bi);
      bi := bi_ex;
    end;
    bi.AddEntry(r);
  end;
end;

procedure TTranslationTable.Add(const r: TTranslationRule; APrior: integer);
var pr: PTranslationRule;
  oldcnt: integer;
begin
  pr := FindItem(r.Phonetic);
  if pr=nil then begin
    pr := MakeNewItem;
    pr^ := r;
    AddToIndex(pr, APrior);
    AddRomajiToIndex(pr, 0, APrior);
  end else begin
    oldcnt := Length(pr^.romaji);
    pr^.Add(r);
    AddRomajiToIndex(pr, oldcnt, APrior);
  end;
end;

{ Locates a rule which exactly matches a given phonetic. This default version
 is quite slow, but descendants may keep their own indices and override it.
 Note that if you need partial matches, best matches or best speed, you should
 maybe access indexes directly from wherever you use it.
 Also note that this does not account for any hiragana/katakana adjustments,
 but descendants may. }
function TTranslationTable.FindItem(const APhonetic: string): PTranslationRule;
var i: integer;
begin
  Result := nil;
  for i := 0 to Self.Count-1 do
    if FList[i].Phonetic=APhonetic then begin
      Result := FList[i];
      break;
    end;
end;

function TTranslationTable.IndexOf(const AItem: PTranslationRule): integer;
var i: integer;
begin
  Result := -1;
  for i := 0 to Self.Count-1 do
    if FList[i]=AItem then begin
      Result := i;
      break;
    end;
end;


{ TKanaTranslationTable }

constructor TKanaTranslationTable.Create;
begin
  inherited;
  FOneCharTree := TBinTree.Create;
  FTwoCharTree := TBinTree.Create;
end;

destructor TKanaTranslationTable.Destroy;
begin
  FreeAndNil(FTwoCharTree);
  FreeAndNil(FOneCharTree);
  inherited;
end;

procedure TKanaTranslationTable.Clear;
begin
  FOneCharTree.Clear;
  FTwoCharTree.Clear;
  inherited;
end;

procedure TKanaTranslationTable.AddToIndex(r: PTranslationRule; APrior: integer);
begin
 { Maybe we should also add katakana versions here to speed up search?
  Options are:
  1. Add both hiragana and katakana, do nothing special when searching =>
       x2 items => +1 comparison
  2. Add only hiragana, convert katakana to hiragana on the fly before searching =>
       x1 items
       +1 integer comparison, +1 potential integer op, but in main loop
  2 looks slightly better since comparisons in the main loop, without function
  calls are faster. }
  case flength(r.Phonetic) of
    1: FOneCharTree.Add(TPhoneticRuleNode1.Create(r.Phonetic, r));
    2: FTwoCharTree.Add(TPhoneticRuleNode2.Create(r.Phonetic, r));
  end;
end;

//Kana must be hiraganized
function TKanaTranslationTable.FindItem(const APhonetic: string): PTranslationRule;
var data: PFChar;
  bti: TBinTreeItem;
begin
  case Length(APhonetic) of
   1: begin
     data := @APhonetic[1];
     bti := FOneCharTree.SearchData(data);
     if bti=nil then
       Result := nil
     else
       Result := TPhoneticRuleNode(bti).FRule;
   end;
   2: begin
     data := @APhonetic[1];
     bti := FTwoCharTree.SearchData(data);
     if bti=nil then
       Result := nil
     else
       Result := TPhoneticRuleNode(bti).FRule;
   end;
  else Result := nil;
  end;
end;



{ TRomajiReplacementTable }

function TRomajiReplacementTable.GetItemPtr(Index: integer): PRomajiReplacementRule;
begin
  Result := @FList[Index]; //valid until next list growth
end;

function TRomajiReplacementTable.MakeNewItem: PRomajiReplacementRule;
begin
 //Thread unsafe
  Grow(1);
  Result := @FList[FListUsed];
  Inc(FListUsed);
end;

//Reserves enough memory to store at least ARequiredFreeLen additional items to list.
procedure TRomajiReplacementTable.Grow(ARequiredFreeLen: integer);
const MIN_GROW_LEN = 40;
begin
  if Length(FList)-FListUsed>=ARequiredFreeLen then exit; //already have the space
 //else we don't grow in less than a chunk
  if ARequiredFreeLen < MIN_GROW_LEN then
    ARequiredFreeLen := MIN_GROW_LEN;
  SetLength(FList, Length(FList)+ARequiredFreeLen);
end;

procedure TRomajiReplacementTable.Add(const r: TRomajiReplacementRule);
begin
 { Skip exact duplicates. We still add expanded rules, e.g.
          nn->nm only for hiragana
      ++  nn->nm for all cases
   The reason we don't simply upgrade in place is that rules are prioritized:
   all rules from the preceding files must execute before we can apply this
   expanded rule (maybe someone before us relied on NN not being replaced for
   katakana) }
  if Find(r)<0 then
    MakeNewItem^ := r;
end;

{ Locates the replacement rule which is exactly equal to the given rule. }
function TRomajiReplacementTable.Find(const r: TRomajiReplacementRule): integer;
var i: integer;
begin
  Result := -1;
  for i := 0 to Count-1 do
    if (r.s_find=Items[i].s_find)
    and (r.s_repl=Items[i].s_repl)
    and (r.pref=Items[i].pref) then begin
      Result := i;
      break;
    end;
end;

procedure TRomajiReplacementTable.Clear;
begin
  SetLength(FList, 0);
  FListUsed := 0;
end;


{ TRomajiTranslator }

constructor TRomajiTranslator.Create;
begin
  inherited Create;
  CreateTranslationTable;
  FReplKtr := TRomajiReplacementTable.Create;
  FReplRtk := TRomajiReplacementTable.Create;
  FTablesLoaded := 0;
end;

destructor TRomajiTranslator.Destroy;
begin
  FreeAndNil(FReplRtk);
  FreeAndNil(FReplKtr);
  FreeAndNil(FTrans);
  inherited;
end;

//In case descendants need some specialized, indexed translation table
procedure TRomajiTranslator.CreateTranslationTable;
begin
  FTrans := TTranslationTable.Create;
end;

procedure TRomajiTranslator.Clear;
begin
  FTrans.Clear;
  FReplKtr.Clear;
  FReplRtk.Clear;
  FTablesLoaded := 0;
end;

{
Loads data from file.
The file must contain [Table], [KanaToRomaji] and [RomajiToKana*] sections,
and it can contain other sections, they'll be ignored.
}

procedure TRomajiTranslator.LoadFromFile(const filename: string);
var sl: TStringList;
begin
  sl := TStringList.Create();
  try
    sl.LoadFromFile(filename);
    LoadFromStrings(sl);
  finally
    FreeAndNil(sl);
  end;
end;

function ParseTranslationRule(const s: string): TTranslationRule; forward;
function ParseRomajiReplacementRule(const s: string): TRomajiReplacementRule; inline; forward;

procedure TRomajiTranslator.LoadFromStrings(const sl: TStrings);
const //sections
  LS_NONE = 0;
  LS_TABLE = 1;
  LS_PRIORITY = 2;
  LS_KANATOROMAJI = 3;
  LS_ROMAJITOKANA = 4;
var i, j: integer;
  ln: string;
  sect: integer;
  pref: char;
  statements: TStringArray;
  parts: TStringArray;
  bi, bi_ex: TRomajiIndexEntry;
  r: TRomajiReplacementRule;
begin
  pref := #00;
  sect := LS_NONE;
  for i := 0 to sl.Count - 1 do begin
    ln := Trim(sl[i]);
    if (Length(ln)<=0) or (ln[1]='#') or (ln[1]=';') then
      continue;

    if (ln='[Romaji]')
    or (ln='[PinYin]') then
      sect := LS_TABLE
    else
    if ln='[Priority]' then
      sect := LS_PRIORITY
    else
    if ln='[KanaToRomaji]' then begin
      sect := LS_KANATOROMAJI;
      pref := #00;
    end else
    if ln='[RomajiToKana]' then begin
      sect := LS_ROMAJITOKANA;
      pref := #00;
    end else
    if ln='[RomajiToKatakana]' then begin
      sect := LS_ROMAJITOKANA;
      pref := 'K';
    end else
    if ln='[RomajiToHiragana]' then begin
      sect := LS_ROMAJITOKANA;
      pref := 'H';
    end else
    if (Length(ln)>=2) and (ln[1]='[') then
     //Some unknown section, skip it
      sect := LS_NONE
    else begin

     //Statements are allowed to come on the same string to let you arrange
     //rules into meaningful tables
      statements := SplitStr(ln,';');
      if Length(statements)<=0 then continue;

      for j := 0 to Length(statements)-1 do begin
        ln := UTrim(statements[j],' '+#09);
        if ln='' then continue; //allow empty statements

       { We need hiragana in FTrans.Add, FPriority[].SortEntries.
        It's cheaper to just hiraganize the whole string }
        ln := ToHiragana(ln);

        case sect of
          LS_TABLE: FTrans.Add(ParseTranslationRule(ln), FTablesLoaded);
          LS_PRIORITY: begin
           //roma,kana,kana,kana
            parts := SplitStr(ln,',');
            if Length(parts)<=1 then continue;
            bi := TRomajiIndexEntry.Create(parts[0], FTablesLoaded);
            bi_ex := TRomajiIndexEntry(FTrans.FRomajiIndex.SearchItem(bi));
            FreeAndNil(bi);
            if bi_ex<>nil then begin
              parts := copy(parts,1,MaxInt);
              bi_ex.SortEntries(parts);
            end;
          end;
          LS_ROMAJITOKANA: begin
            r := ParseRomajiReplacementRule(ln);
            r.pref := pref;
            FReplRtk.Add(r);
          end;
          LS_KANATOROMAJI: begin
            r := ParseRomajiReplacementRule(ln);
            r.pref := pref;
            FReplKtr.Add(r);
          end;
        end;
      end; //of statement cycle

    end; //of else clause

  end; //of line enumeration
  Inc(FTablesLoaded);
end;

//Parses romaji translation rule from string form into record
function ParseTranslationRule(const s: string): TTranslationRule;
var s_parts: TStringArray;
  i: integer;
  base: integer;
begin
  s_parts := SplitStr(s,',');
  Result.Phonetic := ToHiragana(autohextofstr(s_parts[0]));
  base := 1;
  if ([EC_HIRAGANA,EC_KATAKANA] * EvalChars(s_parts[1]) <> []) then begin
    if ToHiragana(autohextofstr(s_parts[1]))<>Result.Phonetic then
      raise Exception.Create('Invalid rule: '+s+'. Katakana and hiragana entries differ.');
    Inc(base);
  end;
  SetLength(Result.romaji, Length(s_parts)-base);
  for i := 0 to Length(Result.romaji)-1 do
    Result.romaji[i] := s_parts[base+i];
end;

//Parses romaji translation rule from string form into record
function ParseRomajiReplacementRule(const s: string): TRomajiReplacementRule; inline;
var s_parts: TStringArray;
begin
  s_parts := SplitStr(s, 2);
  Result.s_find := s_parts[0];
  Result.s_repl := s_parts[1];
  Result.pref := #00; //by default
end;


{ TKanaTranslator }

procedure TKanaTranslator.CreateTranslationTable;
begin
  FTrans := TKanaTranslationTable.Create;
  TRomajiTranslator(Self).FTrans := Self.FTrans; //older field with the same name
end;

{
KanaToRomaji().
This function here is a major bottleneck when translating,
so we're going to try and implement it reallly fast.
}

//ps must have at least one 4-char symbol in it
function TKanaTranslator.SingleKanaToRomaji(var ps: PFChar; flags: TResolveFlags): string;
var bn: TBinTreeItem;
 {$IFDEF KATA_AT_SEARCH}
  chira: array[0..1] of FChar;
  pt: PFChar;
 {$ENDIF}
begin
  if ps^=UH_HYPHEN then begin
    Inc(ps);
    Result := '-';
    exit;
  end;

  if ps^=UH_LOWLINE then begin
    Inc(ps);
    Result := '_';
    exit;
  end;

 {$IFDEF KATA_AT_SEARCH}
  chira[0] := ToHiragana(ps^);
  pt := @chira[0];
 {$ENDIF}

 //first try 2 FChars
 //but we have to test that we have at least that much
  if (ps^<>#00) and ((ps+1)^<>#00) then begin
   {$IFDEF KATA_AT_SEARCH}
    chira[1] := ToHiragana((ps+1)^);
    bn := FTrans.FTwoCharTree.SearchData(pt);
   {$ELSE}
    bn := FTrans.FTwoCharTree.SearchData(ps);
   {$ENDIF}
    if bn<>nil then begin
      Result := TPhoneticRuleNode(bn).FRule.romaji[0];
      Inc(ps, 2);
      exit;
    end;
  end;

 //this time 1 FChar only
 {$IFDEF KATA_AT_SEARCH}
  chira[1] := #00;
  bn := FTrans.FOneCharTree.SearchData(pt);
 {$ELSE}
  bn := FTrans.FOneCharTree.SearchData(ps);
 {$ENDIF}
  if bn<>nil then begin
    Result := TPhoneticRuleNode(bn).FRule.romaji[0];
    Inc(ps);
    exit;
  end;

 //Latin symbol
  if IsLatinLetterF(ps^) and (rfConvertLatin in flags) then
    Result := Char(ftoansi(ps^))
  else
  if IsAllowedPunctuation(ps^) and (rfConvertPunctuation in flags) then
    Result := ConvertPunctuationF(ps^)
  else
  if rfDeleteInvalidChars in flags then
    Result := ''
  else
    Result := '?';

  Inc(ps);
end;

function TKanaTranslator.KanaToRomaji(const AString: FString; AFlags: TResolveFlags): string;
var lowcased_s: FString;
  fn:string;
  s2:string;
  ps: PWideChar;
  i: integer;
  r: PRomajiReplacementRule;
begin
  if Length(AString)<=0 then begin
    Result := '';
    exit;
  end;
  lowcased_s := Lowercase(AString);
  s2 := '';
  ps := PWideChar(lowcased_s);

 { Translation }
  while ps^<>#00 do begin
    fn := SingleKanaToRomaji(ps, AFlags); //also eats one or two symbols
    s2:=s2+fn;
  end;

 { Replacements }
  for i := 0 to FReplKtr.Count - 1 do begin
    r := FReplKtr[i];
    repl(s2, r.s_find, r.s_repl);
  end;

  if (length(s2)>0) and (s2[length(s2)]='''') then delete(s2,length(s2),1);
  result:=s2;
end;

function isMatch(pc_sub, pc_s: PChar): boolean; inline;
begin
  while (pc_sub^<>#00) and (pc_s^<>#00) do begin
    if pc_sub^<>pc_s^ then break;
    Inc(pc_sub);
    Inc(pc_s);
  end;
  Result := pc_sub^=#00;
end;

{ Performs a replacement, minding Hiragana/Katakana mode. }
procedure TKanaTranslator.RomaReplace(var s: string; const r: PRomajiReplacementRule);
var mode: char;
  i: integer;
begin
  mode := 'H';

  i := 1;
  while i<Length(s) do begin
    if s[i]='H' then
      mode := 'H'
    else
    if s[i]='K' then
      mode := 'K'
    else
    if (r.pref=#00) or (r.pref=mode) then
      if isMatch(@r.s_find[1], @s[i]) then begin
        s := copy(s,1,i-1)+r.s_repl+copy(s,i+Length(r.s_find),MaxInt);
        Inc(i,Length(r.s_find)-1);
      end;
    Inc(i);
  end;
end;

function TKanaTranslator.RomajiToKana(const AString: string; AFlags: TResolveFlags): string;
var s2,s3,fn:string;
  kata:integer;
  l,i:integer;
  bi: TBinTreeItem;
  bir: TRomajiIndexEntry absolute bi;
begin
  if length(AString)<=0 then begin
    Result := '';
    exit;
  end;

  s2 := AString;

 { Replacements }
  for i := 0 to FReplRtk.Count - 1 do
    RomaReplace(s2, FReplRtk[i]);

 { Translation }
  kata:=0;
  s3:='';
  while length(s2)>0 do
  begin
    fn:='';
    l:=0;
    if s2[1]='_' then begin fn:=fstr('_'); l:=1; end;
    if s2[1]='-' then begin fn:=fstr('-'); l:=1; end;

    for bi in FTrans.FRomajiIndex do
      if StartsStr(bir.roma, s2) then begin
        l := Length(bir.roma);
        if kata=0 then
          fn := bir.entries[0].Phonetic
        else
          fn := ToKatakana(bir.entries[0].Phonetic);
        break;
      end;

    if l=0 then begin
      if not (rfDeleteInvalidChars in AFlags) then
        if s2[1]<>'''' then
         //Latin letter (supposedly)
          fn := s2[1]
        else
          fn:='';
      l:=1;
    end;

    if s2[1]='H' then begin
      kata:=0;
      l:=1;
      fn:='';
    end else
    if (s2[1]='K') then begin
      kata:=1;
      l:=1;
      fn:='';
    end;
    delete(s2,1,l);
    s3:=s3+fn;
  end;
  result:=s3;
end;


{ TPinYinTranslator }

//Finds best matching entry for bopomofo syllable at the start of the text
function TPinYinTranslator.BopomofoBestMatch(const AText: FString): integer;
var i, cl: integer;
begin
  Result := -1;
  cl := 0;
  for i:=0 to FTrans.Count-1 do
    if pos(FTrans[i].Phonetic,AText)=1 then
      if flength(FTrans[i].Phonetic)>cl then
      begin
        cl:=flength(FTrans[i].Phonetic);
        Result:=i;
      end;
end;

//Finds best matching entry for the pinyin syllable at the start of the text
//Text must be lowercased
function TPinYinTranslator.PinyinBestMatch(const AText: string; out ARomaIdx: integer): integer;
var i, j, cl: integer;
  rom: string;
begin
  Result := -1;
  cl := 0;
  for i:=0 to FTrans.Count-1 do
    for j:=0 to Length(FTrans[i].romaji)-1 do begin
      rom := FTrans[i].romaji[j];
      if pos(rom, AText)=1 then
        if flength(rom)>cl then
        begin
          cl:=flength(rom);
          Result:=i;
          ARomaIdx:=j;
        end;
    end;
end;

//Finds first entry which starts with ASyllable
//Text must be lowercased.
function TPinYinTranslator.PinyinPartialMatch(const ASyllable: string): integer;
var i, j: integer;
  rom: string;
begin
  Result := -1;
  for i:=0 to FTrans.Count-1 do begin
    for j:=0 to Length(FTrans[i].romaji)-1 do begin
      rom := FTrans[i].romaji[j];
      if pos(ASyllable,rom)=1 then begin
        Result := i;
        break;
      end;
    end;
    if Result>=0 then
      break;
  end;
end;

function TPinYinTranslator.KanaToRomaji(const AString: FString; AFlags: TResolveFlags): string;
var s2:string;
  cl:integer;
  i:integer;
  ch:WideChar;
  curstr:FString;
begin
  s2:='';
  curstr := AString;
  while curstr<>'' do
  begin
    //Find longest match for character sequence starting at this point
    i := BopomofoBestMatch(curstr);
    if i>=0 then
      cl := Length(FTrans[i].Phonetic)
    else
      cl := 0;

    if i>=0 then begin
      s2:=s2+FTrans[i].romaji[0];
      delete(curstr,1,cl);
    end
    else begin
      ch:=fstrtouni(fgetch(curstr,1))[1];
      fdelete(curstr,1,1);

      if (rfConvertLatin in AFlags) and IsLatinLetterW(ch) then
        s2:=s2+ch
      else
      if (rfConvertPunctuation in AFlags) and IsAllowedPunctuation(ch) then
        s2:=s2+ConvertPunctuation(ch)
      else

      if not (rfDeleteInvalidChars in AFlags) then
        s2 := s2 + '?';
    end;

   //Extract tones always, as they are in special characters
    if fpygettone(curstr) in [0..5] then
      s2 := s2 + Chr(Ord('0') + fpyextrtone(curstr)); //to digit
  end;
  Result:=lowercase(s2);
end;

function TPinYinTranslator.RomajiToKana(const AString: string; AFlags: TResolveFlags): FString;
var s2:string;
  cl:integer;
  i,j:integer;
  ch:WideChar;
  curstr:string;
begin
  curstr := LowerCase(AString);
  repl(curstr,'v','u:');
  s2:='';
  while curstr<>'' do
  begin
    //Find longest match for character sequence starting at this point
    i := PinyinBestMatch(curstr,j);
    if i>=0 then
      cl := Length(FTrans[i].romaji[j])
    else
      cl := 0;

    if i>=0 then begin
      s2:=s2+FTrans[i].Phonetic;
      delete(curstr,1,cl);

     //with ansi pinyin, we only try to extract tone after a syllable match
      if (length(curstr)>0) and (curstr[1]>='0') and (curstr[1]<='5') then
      begin
        s2:=s2+fpytone(Ord(curstr[1])-Ord('0')); //from digit
        delete(curstr,1,1);
      end else
        s2:=s2+UH_PY_TONE;
    end
    else begin
      ch:=curstr[1];
      delete(curstr,1,1);

      if (rfConvertLatin in AFlags) and IsLatinLetterW(ch) then
        s2:=s2+ch
      else
      if (rfConvertPunctuation in AFlags) and IsAllowedPunctuation(ch) then
        s2:=s2+ConvertPunctuation(ch)
      else

      if not (rfDeleteInvalidChars in AFlags) then
        s2 := s2 + '?'; //in unicode both strings are in native form
    end;
  end;
  Result := s2;
end;


{
PinYin tones -- see comment to UH_PY_TONE
}

//creates a character which encodes PinYin tone
function fpytone(const i: byte): fchar;
begin
  Result := Chr(Ord(UH_PY_TONE)+i);
end;

//returns 0-5 if the character encodes tone, or 255
function fpydetone(const ch: fchar): byte;
begin
  if Ord(ch) and $FFF0 = $F030 then
    Result := Ord(ch) and $000F
  else
    Result := 255;
end;

//returns 0-5 if the first character encodes tone, or 255
function fpygettone(const s: fstring): byte;
begin
  if (length(s)>=1) and (Ord(s[1]) and $FFF0 = $F030) then
    Result := Ord(s[1]) and $000F
  else
    Result := 255;
end;

//same, but deletes the tone from the string
function fpyextrtone(var s: fstring): byte;
begin
  Result := fpygettone(s);
  if Result<255 then fdelete(s,1,1);
end;

{
Converts raw database pin4yin4 to enhanced unicode pínín with marks.
Only works for pure pinyin (no latin letters).
}
function ConvertPinYin(const str:string):FString;
const UH_DUMMY_CHAR:FChar = #$F8F0;
 { Used in place of a char, does not go out of this function }
var cnv:string;
  li:integer; //last suitable vowel
  li_dirty:boolean; //there were consonants after last suitable vowel. New vowel will replace it.
  ali:FString;
  cnv2:FString;
  cc:char;
  i:integer;
  iscomma:boolean;
begin
  cnv:=AnsiLowerCase(str); //source string
  cnv2:=''; //building output here
  li:=0;
  ali:='';
  iscomma:=false;
  li_dirty:=false;
  for i:=1 to length(cnv) do
  begin
    if li<=0 then begin
     //No suitable vowel yet => use this one
      if CharInSet(cnv[i], ['a', 'e', 'o', 'u', 'i']) then begin
        li:=i;
        li_dirty:=false;
      end
    end else //li > 0
   //focus second vowel in some syllables
    if CharInSet(cnv[li], ['i', 'u', 'ь']) and CharInSet(cnv[i], ['a', 'e', 'o', 'u', 'i']) then begin
      li:=i;
      li_dirty:=false;
    end else
   //relocate focus to new vowel in cases like "dnajianyang" (from dnA to jIan)
    if li_dirty and CharInSet(cnv[i], ['a', 'e', 'o', 'u', 'i']) then begin
      li:=i;
      li_dirty:=false;
    end else
    if (not li_dirty) and (not CharInSet(cnv[i], ['a', 'e', 'o', 'u', 'i'])) then
      li_dirty:=true;

    if (cnv[i]>='0') and (cnv[i]<='5') and (li>0) then
    begin
      cc:=cnv[li]; //the character to be replaced
      ali:=fcopy(cnv2,length(cnv2)-(i-li-1)+1,i-li-1); //copy the rest of the output
      fdelete(cnv2,length(cnv2)-(i-li)+1,i-li); //delete char + rest from the output
      if iscomma and (cc='u') then cc:='w';
      case cnv[i] of
        '2':case cc of
              'a':cnv2:=cnv2+#$00E1;
              'e':cnv2:=cnv2+#$00E9;
              'i':cnv2:=cnv2+#$00ED;
              'o':cnv2:=cnv2+#$00F3;
              'u':cnv2:=cnv2+#$00FA;
              'w':cnv2:=cnv2+#$01D8;
            end;
        '4':case cc of
              'a':cnv2:=cnv2+#$00E0;
              'e':cnv2:=cnv2+#$00E8;
              'i':cnv2:=cnv2+#$00EC;
              'o':cnv2:=cnv2+#$00F2;
              'u':cnv2:=cnv2+#$00F9;
              'w':cnv2:=cnv2+#$01DC;
            end;
        '1':case cc of
              'a':cnv2:=cnv2+#$0101;
              'e':cnv2:=cnv2+#$0113;
              'i':cnv2:=cnv2+#$012B;
              'o':cnv2:=cnv2+#$014D;
              'u':cnv2:=cnv2+#$016B;
              'w':cnv2:=cnv2+#$01D6;
            end;
        '3':case cc of
              'a':cnv2:=cnv2+#$01CE;
              'e':cnv2:=cnv2+#$011B;
              'i':cnv2:=cnv2+#$01D0;
              'o':cnv2:=cnv2+#$01D2;
              'u':cnv2:=cnv2+#$01D4;
              'w':cnv2:=cnv2+#$01DA;
            end;
      end;
      li:=0;
      if (cnv[i]='0') or (cnv[i]='5') then
        if cc='w'then
          cnv2:=cnv2+#$00FC
        else
          cnv2:=cnv2+fch(cc);
      cnv2:=cnv2+ali;
      iscomma:=false;
    end else
    if cnv[i]=':'then begin
      cnv2:=cnv2+UH_DUMMY_CHAR;
      iscomma:=true
    end else
    if (cnv[i]<'0') or (cnv[i]>'5') then
      cnv2:=cnv2+fch(cnv[i]);
  end;

  //Remove dummy chars
  i := fpos(UH_DUMMY_CHAR,cnv2);
  while i>0 do begin
    fdelete(cnv2,i,1);
    i := fpos(UH_DUMMY_CHAR,cnv2);
  end;
  Result := cnv2;
end;


{
Converts tonemark-enhanced pínín back into ansi pin4yin4.
Only works for pure pinyin (no latin letters).
}
function DeconvertPinYin(romac: TPinYinTranslator; const str:FString):string;
{ Implemented only in Unicode. This is slower on Ansi, but FStrings are deprecated anyway. }
var cnv:UnicodeString; //source string
  curs:UnicodeString;
  nch:string;
  cnv2:string; //building output here
    i,j:integer;
    curcc:string;
    curp,curpx:char;
    mustbegin,mustnotbegin,befmustnotbegin,befbefmustnotbegin:boolean;
    number:boolean;
    putcomma,fnd:boolean;
    cc:char;
begin
  cnv:=fstrtouni(str);

  putcomma:=false;
  cnv2:='';
  for i:=1 to length(cnv) do
  begin
    curs:=cnv[i];
    if putcomma and (curs<>'e') then begin curs:=':'+curs; putcomma:=false; end;
    if curs=#$01D6 then begin putcomma:=true; curs:=#$016B; end;
    if curs=#$01D8 then begin putcomma:=true; curs:=#$00FA; end;
    if curs=#$01DA then begin putcomma:=true; curs:=#$01D4; end;
    if curs=#$01DC then begin putcomma:=true; curs:=#$00F9; end;
    if curs=#$00FC then begin putcomma:=true; curs:=#$0075; end;
    if curs=#$2026 then curs:='_';
    cnv2:=cnv2+curs;
  end;
  if putcomma then cnv2:=cnv2+':';
  cnv:=cnv2;
  cnv2:='';

  mustbegin:=true;
  mustnotbegin:=false;
  befmustnotbegin:=false;
  number:=false;
  for i:=1 to length(cnv) do
  begin
    curs:=cnv[i];
    cc:=curs[1];
    if (cc>='0') and (cc<='9') then number:=true; //WTF? Maybe we should test that ALL chars are digits, not ANY?
  end;
  if number then
  begin
    result:=fstrtouni(str);
    exit;
  end;

  curp:='0';
  curcc:='';
  for i:=1 to length(cnv) do
  begin
    curs:=cnv[i];
    curpx:='0';
    if Ord(curs[1])<$0080 then cc:=upcase(curs[1])
    else if curs=#$00E1 then begin cc:='A'; curpx:='2'; end
    else if curs=#$00E9 then begin cc:='E'; curpx:='2'; end
    else if curs=#$00ED then begin cc:='I'; curpx:='2'; end
    else if curs=#$00F3 then begin cc:='O'; curpx:='2'; end
    else if curs=#$00FA then begin cc:='U'; curpx:='2'; end
    else if curs=#$00E0 then begin cc:='A'; curpx:='4'; end
    else if curs=#$00E8 then begin cc:='E'; curpx:='4'; end
    else if curs=#$00EC then begin cc:='I'; curpx:='4'; end
    else if curs=#$00F2 then begin cc:='O'; curpx:='4'; end
    else if curs=#$00F9 then begin cc:='U'; curpx:='4'; end
    else if curs=#$0101 then begin cc:='A'; curpx:='1'; end
    else if curs=#$0113 then begin cc:='E'; curpx:='1'; end
    else if curs=#$012B then begin cc:='I'; curpx:='1'; end
    else if curs=#$014D then begin cc:='O'; curpx:='1'; end
    else if curs=#$016B then begin cc:='U'; curpx:='1'; end
    else if curs=#$0103 then begin cc:='A'; curpx:='3'; end
    else if curs=#$0115 then begin cc:='E'; curpx:='3'; end
    else if curs=#$012D then begin cc:='I'; curpx:='3'; end
    else if curs=#$014F then begin cc:='O'; curpx:='3'; end
    else if curs=#$016D then begin cc:='U'; curpx:='3'; end
    else if curs=#$01CE then begin cc:='A'; curpx:='3'; end
    else if curs=#$011B then begin cc:='E'; curpx:='3'; end
    else if curs=#$01D0 then begin cc:='I'; curpx:='3'; end
    else if curs=#$01D2 then begin cc:='O'; curpx:='3'; end
    else if curs=#$01D4 then begin cc:='U'; curpx:='3'; end
    else cc:='?';
    if (((cc>='A') and (cc<='Z')) or (cc=':')) and (cc<>'''') then curcc:=curcc+cc;

    fnd:=romac.PinyinPartialMatch(LowerCase(curcc))>=0;
    if ((cc<'A') or (cc>'Z')) and (cc<>':') then
    begin
      if curcc<>'' then cnv2:=cnv2+lowercase(curcc)+curp;
      curcc:='';
      cnv2:=cnv2+cc;
      curp:='0';
    end else
    if ((not fnd) or ((curpx<>'0') and (curp<>'0'))) and
       ((copy(curcc,length(curcc)-1,2)='GU') or
        (copy(curcc,length(curcc)-1,2)='NU') or
        (copy(curcc,length(curcc)-1,2)='NI') or
        (copy(curcc,length(curcc)-1,2)='NO')) then
    begin
      cnv2:=cnv2+lowercase(copy(curcc,1,length(curcc)-2))+curp;
      delete(curcc,1,length(curcc)-2);
      curp:='0';
    end else if (not fnd) or ((curpx<>'0') and (curp<>'0')) then
    begin
      cnv2:=cnv2+lowercase(copy(curcc,1,length(curcc)-1))+curp;
      delete(curcc,1,length(curcc)-1);
      curp:='0';
    end else if (cc='?') or (cc='''') then
    begin
      cnv2:=cnv2+lowercase(curcc)+curp;
      curcc:='';
      curp:='0';
    end;
    if curpx<>'0'then curp:=curpx;
  end;
  Result:=cnv2+lowercase(curcc)+curp;
end;

function ConvertBopomofo(const str:FString):FString;
var i, tone: integer;
  ch: FChar;
begin
  Result := str;
  i := 1;
  while i<=flength(Result) do begin
    ch := fgetch(Result, i);
    tone := fpydetone(ch);
    if (tone<0) or (tone>5) then begin
     //not a tone or unrecognized tone code -- keep as is
      Inc(i);
      continue;
    end;

    if (tone=0) or (tone=1) then begin
     //0 is tone unknown, 1 is not drawn in bopomofo
      fdelete(Result, i, 1);
      continue;
    end;

    case tone of
      2:ch:=#$02CA;
      3:ch:=#$02C7;
      4:ch:=#$02CB;
      5:ch:=#$02D9; //only in bopomofo!
    end;
    fputch(Result,i,ch);
    Inc(i);
  end;
end;



end.
