﻿unit JWBDicSearch;
{
High-level dictionary search, unifies results from dictionaries and vocab.
Has a lot of dark legacy in code form.
Throughout this module:
  Japanese == Japanese, Chinese, any source language.
  Kana     == Kana, Bopomofo etc.
  Romaji   == Romaji, Pinyin etc.
  English  == English, Polish, any target language (even Japanese in Jp->Jp dicts).
}

interface
uses SysUtils, Classes, JWBStrings, TextTable, StdPrompt, JWBDic
  {$IFNDEF AUTOTEST}, JWBVocab1{$ENDIF};

{
Particle list
}
type
  TParticleList = class(TStringList)
  public
    procedure Add(const entry: string); reintroduce; inline;
  end;

{
Deflection parsing code and deflection list.
For more info see wakan.cfg.
}
type
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
    function GetItemPtr(Index: integer): PDeflectionRule; inline;
    function MakeNewItem: PDeflectionRule;
  public
    procedure Add(const r: TDeflectionRule); overload;
    procedure Add(const s: string); overload; inline;
    procedure Clear;
    property Count: integer read FListUsed;
    property Items[Index: integer]: PDeflectionRule read GetItemPtr; default;
  end;

function ParseDeflectionRule(const s: string): TDeflectionRule; inline;

{
Candidate lookup list for JWBUser's dictionary lookups.
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
  TRomaType = (
    rtNormal,   //default lookup type for this search type
    rtRoma      //romaji signature
  );
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
    roma: TRomaType;
    str: string;
  end;
  PCandidateLookup = ^TCandidateLookup;
  TCandidateLookupArray = array of TCandidateLookup;
  TCandidateLookupList = class
  protected
    FList: TCandidateLookupArray;
    FListUsed: integer;
    procedure Grow(ARequiredFreeLen: integer);
    function GetItemPtr(Index: integer): PCandidateLookup; inline;
    function MakeNewItem: PCandidateLookup;
  public
    procedure Add(priority: integer; len: integer; verbType: char; roma: TRomaType;
      const str: string); overload;
    procedure Add(const ct: TCandidateLookup); overload; inline;
    procedure Delete(Index: integer);
    procedure Clear;
    function Find(len: integer; verbType: char; const str: string): integer;
    procedure Sort;
    property Count: integer read FListUsed;
    property Items[Index: integer]: PCandidateLookup read GetItemPtr; default;
  end;


{
Search result list.
Populated by TDicSearchRequest.Search().
}
type
 {
  If several results are really similar, Wakan merges those into one. Header then
  contains the best of everything (lowest sort score, first availabe userindex etc)
  and article is a merger of all articles.
 }

  TSearchResArticle = record
    score: integer; //lower is better
    dicname: string;
    dicindex: integer;
    freq: integer; //word frequency according to the dictionary, -1 if unavailable
    entries: TEntries;
    procedure Reset;
    function ToLegacyString: string;
    function ToEdictXml: string;
  end;
  PSearchResArticle = ^TSearchResArticle;

  TSearchResult = record
   //At this time there has to be a "primary" match which is duplicated here:
    score: integer;
    dicname: string;
    dicindex: integer;
    freq: integer;
   //If the word is in the user vocabulary, that's added as an article + indicated here
    userindex: integer; //0 means not in a user dict
    userscore: integer;
   //There's currently only one match class for all grouped entries
    sdef: char; //match class -- see TCandidateLookup.verbType
    inflen: integer; {
      Length of the inflected expression as it appeared in the search request.
      Different search results are different guesses at deflexion and may assume
      original expression was of different length.

      Note this returns the length of the original expression, not it's "kanji form".
      For instance:
         たべた -> たべる   returns 3
         食た -> 食る       returns 2
      If you need the length of the kanji replacement, calculate it yourself
      by adjusting by the kanji/kana length difference.

      For stRomaji, this returns the length of the inflected match, IN KANA.
      Matches may be different interpretations of how to translate romaji to
      kana/latin sequences, but each match has a kana field from which you can
      figure this out.
    }
    kanji: string; //already simplified if needed, but otherwise no special marks
    kana: string;
    articles: array of TSearchResArticle;
    procedure Reset;
    function AddArticle: PSearchResArticle; overload;
    procedure AddArticle(const art: TSearchResArticle); overload;
    procedure InsertArticle(const AIndex: integer; const art: TSearchResArticle);
    procedure DeleteArticle(const AIndex: integer);
    function FindArticle(const dicname: string; const dicindex: integer): integer;
    function ToLegacyString: string;
    procedure ToLegacyParts(out AKanji, AKana, ABody: string);
    function ToEdictXml: string;
  end;
  PSearchResult = ^TSearchResult;

  TSearchResults = class;
  TSearchResultsCompare = function(List: TSearchResults; Index1, Index2: Integer): Integer;

  TSearchResults = class
  protected
    FList: array of PSearchResult;
    FListUsed: integer;
    procedure Grow(ARequiredFreeLen: integer);
    function GetItemPtr(Index: integer): PSearchResult; inline;
    procedure ExchangeItems(I,J: integer);
    procedure QuickSort(L, R: Integer; SCompare: TSearchResultsCompare);
  public
    destructor Destroy; override;
    procedure Clear;
    function AddResult: PSearchResult;
    function Add(const sr: TSearchResult): integer;
    procedure CustomSort(SCompare: TSearchResultsCompare);
    procedure SortByFrequency;
    procedure SortByKanji;
    procedure Trim(const AMaxItems: integer);
    property Count: integer read FListUsed;
    property Items[Index: integer]: PSearchResult read GetItemPtr; default;
  end;

{$IFNDEF AUTOTEST}
//Converts Vocab1 parsed article to common search result.
//Why do we even need a separate Vocab1-parsed structure? Because our common
//TSearchResArticle can change, while Vocab1 format will always stay the same.
function Vocab1ToSearchRes(const ASource: TVocab1Article): TSearchResArticle;
{$ENDIF}

{
Search itself.
}
type
  TSearchType = (
    stJapanese,     //japanese text in Unicode
    stRomaji,       //romaji input (in user preferred romaji system)
    stEnglish       //english text
  );

 {
   How to match words.
   This is not the same as TDicMatchType as we can do more here.
  }
  TMatchType = (
    mtExactMatch,
      //Expression text must be matched exactly, except maybe for exact deflexion
    mtMatchLeft,
      //Match words that start with this full expression
    mtMatchRight,
      //Match words that end with this full expression
    mtMatchAnywhere,
      //Match words that contain this full expression
    mtBestGuessLeft
      //Try to match the longest leftmost part of the expression text
      //This is the mode for when you're translating text and not sure where the
      //word ends.
    );

  TDicSetup = record
    cursor: TDicLookupCursor;
    PriorityClass: integer; //Reflects how high is this dict in the priority list. Lower is better.
  end;

 { Dictionary search class. Reusable.
  Create, fill params, Prepare(), then do multiple Search()es. }
  TDicSearchRequest = class
  public
    constructor Create;
    destructor Destroy; override;

  public //Settings
    st: TSearchType;
    MatchType: TMatchType;
    MaxWords: integer;
   { Maximum number of matches to return, <=0 = all.
     Tries to return the top matches but cuts corners so if you really need the best,
     pass <=0 and sort the results. }
    DictGroup: TDictGroup;
    dic_ignorekana: boolean; //Do not include kana-only words in results. Half-baked.
    AutoDeflex: boolean; //Search for inflected words/conjugated verbs
    MindUserPrior: boolean; //Mind kanji usage priorities for this user. See TUserPrior.
    procedure Prepare; //Call after changing settings

  protected
    dics: array of TDicSetup; //empty field => dic was not loaded, skip
   {$IFNDEF AUTOTEST}
    //Cached cursors and seeks for User tables
    CUser: TTextTableCursor; //Order is always 'Kanji_ind' for now, do not change
    stUserKanji: TSeekObject;
    stUserIndex: TSeekObject;
    CUserPrior: TTextTableCursor;
    stUserPriorKanji: TSeekObject;
    fldUserPriorCount: integer;
   {$ENDIF}
  public
    procedure Search(search: string; sl: TSearchResults; wt: TEvalCharType = EC_UNKNOWN);

  public //Output
    WasFull: boolean;

  protected
    procedure MakeLookupList(se: TCandidateLookupList; search: string; wt: TEvalCharType);

  protected
    nowt:TDateTime; //Search start time
    mess: TSMPromptForm; //"Please wait" form, created if the search takes too long
    se: TCandidateLookupList; //Lookup candidates -- see comments where this type is declared
    presentl:TStringList;
    procedure TestLookupCandidate(ds: TDicSetup; lc: PCandidateLookup;
      sl: TSearchResults);

  protected
    kanaonly:boolean; //request is kana only, so are all the lookup candidates
    procedure FinalizeResults(sl: TSearchResults);

  end;

var
 //Created here but must be populated manually from outside (perhaps config)
  defll: TDeflectionList; //verb deflections
  suffixl: TStringList; //suffixes
  partl: TParticleList; //particles such as NO, NI, etc

function GuessWord(const AString: string; APos: integer; out AWordType: TEvalCharType): string;

//Compability
procedure DicSearch(search:string;st:TSearchType; MatchType: TMatchType;
  wt:TEvalCharType;MaxWords:integer;sl:TSearchResults; DictGroup:integer;
  var wasfull:boolean);

implementation
uses Forms, Windows, Math, KanaConv, JWBDictionaries, JWBUnit, JWBSettings,
 {$IFNDEF AUTOTEST} JWBCategories, JWBUserData, {$ENDIF}
  JWBLegacyMarkup, JWBLanguage, JWBEdictMarkers;

procedure Deflex(const w:string;sl:TCandidateLookupList;prior,priordfl:byte;mustsufokay:boolean); forward;

{
Particle list.
}

{ Adds a particle from configuration list. Supports both FHex and direct Unicode }
procedure TParticleList.Add(const entry: string);
begin
  inherited Add(autohextofstr(entry))
end;

{
Deflexion rules
}

{ Parses deflection rule from string form into record. Supports both FHex and
 Unicode.
 See comments in wakan.cfg for format details. }
function ParseDeflectionRule(const s: string): TDeflectionRule;
var i: integer;
begin
  Result.vt := s[1];
  Result.sufcat := s[2];
  i := pos('->', s);
  Result.infl := copy(s,3,i-3);
  if Result.infl='KKKK' then
    //KKKK are special inflected ending markers which mean "no inflected ending",
    //like when inflecting "neru -> ne+masu"
    Result.infl := ''
  else
    Result.infl := autohextofstr(Result.infl);
  Result.defl := autohextofstr(copy(s,i+2,Length(s)-(i+2)+1));
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

procedure TDeflectionList.Add(const r: TDeflectionRule);
begin
  MakeNewItem^ := r;
end;

procedure TDeflectionList.Add(const s: string);
begin
  Add(ParseDeflectionRule(s));
end;

procedure TDeflectionList.Clear;
begin
  SetLength(FList, 0);
  FListUsed := 0;
end;


{
Candidate lookup list
}

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
  roma: TRomaType; const str: string);
var item: PCandidateLookup;
begin
 //Only priorities >=0 are supported
  if priority<0 then priority := 0;

  item := MakeNewItem;
  item.priority := priority;
  item.len := len;
  item.verbType := verbType;
  item.roma := roma;
  item.str := str;
end;

procedure TCandidateLookupList.Add(const ct: TCandidateLookup);
begin
  Add(ct.priority, ct.len, ct.verbType, ct.roma, ct.str);
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

//Sort the candidate lookups by priority (higher first)
procedure TCandidateLookupList.Sort;
var i, j: integer;
  tmp: TCandidateLookup;
begin
  //Try to keep equal-priority candidates in the original order, as we sometimes
  //may hint at their sub-priority by the order in which we add them.
  for i := 1 to Length(Self.FList)-1 do begin
    j := i-1;
    if Self.FList[j].priority >= Self.FList[i].priority then
      continue; //fast case
    tmp := Self.FList[i];
    repeat
      Self.FList[i] := Self.FList[j];
      Dec(j);
    until (j < 0) or (Self.FList[j].priority >= tmp.priority);
    Inc(j);
    Self.FList[j] := tmp;
  end;
end;


{
Search Results
}

destructor TSearchResults.Destroy;
begin
  Clear;
  inherited;
end;

//Reserves enough memory to store at least ARequiredFreeLen additional items to list.
procedure TSearchResults.Grow(ARequiredFreeLen: integer);
const MIN_GROW_LEN = 20;
begin
  if Length(FList)-FListUsed>=ARequiredFreeLen then exit; //already have the space
 //else we don't grow in less than a chunk
  if ARequiredFreeLen < MIN_GROW_LEN then
    ARequiredFreeLen := MIN_GROW_LEN;
  SetLength(FList, Length(FList)+ARequiredFreeLen);
end;

function TSearchResults.GetItemPtr(Index: integer): PSearchResult;
begin
  Result := FList[Index];
end;

function TSearchResults.AddResult: PSearchResult;
begin
 //Thread unsafe
  Grow(1);
  New(Result);
  FList[FListUsed] := Result;
  Inc(FListUsed);
end;

function TSearchResults.Add(const sr: TSearchResult): integer;
begin
  AddResult^ := sr;
  Result := FListUsed-1;
end;

procedure TSearchResults.Clear;
var i: integer;
begin
  for i := 0 to FListUsed - 1 do
    Dispose(FList[i]);
  SetLength(FList, 0);
  FListUsed := 0;
end;

procedure TSearchResults.ExchangeItems(I,J: integer);
var tmp: PSearchResult;
begin
  tmp := FList[I];
  FList[I] := FList[J];
  FList[J] := tmp;
end;

procedure TSearchResults.QuickSort(L, R: Integer; SCompare: TSearchResultsCompare);
var
  I, J, P: Integer;
begin
  if R<=L then exit;
  repeat
    I := L;
    J := R;
    P := (L + R) shr 1;
    repeat
      while SCompare(Self, I, P) < 0 do Inc(I);
      while SCompare(Self, J, P) > 0 do Dec(J);
      if I <= J then
      begin
        if I <> J then
          ExchangeItems(I, J);
        if P = I then
          P := J
        else if P = J then
          P := I;
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then QuickSort(L, J, SCompare);
    L := I;
  until I >= R;
end;

procedure TSearchResults.CustomSort(SCompare: TSearchResultsCompare);
begin
  QuickSort(0, Count-1, SCompare);
end;

function SearchResultCompareScore(List: TSearchResults; Index1, Index2: Integer): Integer;
var pi, pj: PSearchResult;
begin
  pi := List.FList[Index1];
  pj := List.FList[Index2];
  Result := (pi^.score-pj^.score);
end;

function SearchResultCompareKanji(List: TSearchResults; Index1, Index2: Integer): Integer;
var pi, pj: PSearchResult;
begin
  pi := List.FList[Index1];
  pj := List.FList[Index2];
  Result := AnsiCompareText(pi^.kanji, pj^.kanji);
end;

procedure TSearchResults.SortByFrequency;
begin
  CustomSort(@SearchResultCompareScore);
end;

procedure TSearchResults.SortByKanji;
begin
  CustomSort(@SearchResultCompareKanji);
end;

{ Leaves no more than AMaxItems entries }
procedure TSearchResults.Trim(const AMaxItems: integer);
begin
  if FListUsed<AMaxItems then exit;
  while FListUsed>AMaxItems do begin
    Dec(FListUsed);
    Dispose(FList[FListUsed]);
    FList[FListUsed] := nil;
  end;
end;

procedure TSearchResult.Reset;
begin
  score := 0;
  userIndex := 0;
  userScore := -1;
  sdef := 'F'; //maybe something else?
  inflen := 0;
  kanji := '';
  kana := '';
  SetLength(articles, 0);
end;

function TSearchResult.AddArticle: PSearchResArticle;
begin
  SetLength(articles, Length(articles)+1);
  Result := @articles[Length(articles)-1];
  Result.Reset;
end;

procedure TSearchResult.AddArticle(const art: TSearchResArticle);
begin
  AddArticle^ := art;
end;

procedure TSearchResult.InsertArticle(const AIndex: integer; const art: TSearchResArticle);
begin
  SetLength(articles, Length(articles)+1);
  Move(articles[AIndex], articles[AIndex+1], (Length(articles)-AIndex-1)*SizeOf(articles[AIndex]));
  ZeroMemory(@articles[AIndex], SizeOf(articles[AIndex]));
  articles[AIndex] := art;
end;

procedure TSearchResult.DeleteArticle(const AIndex: integer);
begin
  articles[AIndex].Reset;
  Move(articles[AIndex+1], articles[AIndex], (Length(articles)-AIndex-1)*SizeOf(articles[AIndex]));
  ZeroMemory(@articles[Length(articles)-1], SizeOf(articles[Length(articles)-1]));
  SetLength(articles, Length(articles)-1);
end;

{ Locates a reference to this exact article or returns -1 }
function TSearchResult.FindArticle(const dicname: string; const dicindex: integer): integer;
var i: integer;
begin
  Result := -1;
  for i := 0 to Length(articles)-1 do
    if (articles[i].dicname=dicname) and (dicindex=dicindex) then begin
      Result := i;
      break;
    end;
end;

{ Returns the result in a legacy format. Do not use in new code. }
function TSearchResult.ToLegacyString: string;
var AKanji, AKana, ABody: string;
begin
  ToLegacyParts(AKanji, AKana, ABody);
  Result := AKanji + ' [' + AKana + '] {' + ABody + '}';
end;

{ Returns the result in a legacy formatted parts. Do not use in new code }
procedure TSearchResult.ToLegacyParts(out AKanji, AKana, ABody: string);
var statpref: string;
  i: integer;
begin
  if UserScore>=0 then
    statpref:=ALTCH_EXCL+inttostr(UserScore)
  else
    statpref:='';

 //Store match type in the string. The only place where this is used is
 //DrawWordInfo(), and it should not be used anywhere else.
  if sdef<>'F' then
    ABody := UH_WORDTYPE+'I'
  else
    ABody := UH_WORDTYPE+'F';
 //TODO: Wtf? This is overriden just the next line.

  ABody := '';
  for i := 0 to Length(articles)-1 do begin
    if ABody<>'' then ABody := ABody + ' / ';
    ABody := ABody + articles[i].ToLegacyString();
  end;

 //wakan uses {} as special chars, unfortunately
  ABody := repl(ABody, '{', '(');
  ABody := repl(ABody, '}', ')');

{$IFNDEF AUTOTEST}
  AKanji := statpref + CheckKnownKanji(kanji);
{$ENDIF}
  AKana := statpref + kana;
  ABody := statpref + ABody;
end;

{ Generates EDICT-compatible XML entry for this result. It roughly equates
 to <entry>, but includes additional grouping of <senses> by <articles> }
function TSearchResult.ToEdictXml: string;
var i: integer;
begin
  Result := '<entry>'
    +'<k_ele><keb>'+Self.kanji+'</keb></k_ele>';
  if (Self.kana<>'') and (Self.kana<>Self.kanji) then
    Result := Result
      +'<r_ele><reb>'+Self.kana+'</reb></r_ele>';
  for i := 0 to Length(Self.articles)-1 do
    Result := Result + Self.articles[i].ToEdictXml;
  Result := Result + '</entry>';
end;

procedure TSearchResArticle.Reset;
begin
  dicindex := 0;
  dicname := '';
  entries.Reset;
end;

{ Returns backward-compatible entry string. Eventually do be deleted. }
function TSearchResArticle.ToLegacyString: string;
begin
  Result := entries.ToEnrichedString;
  if Self.dicname<>'' then
    Result := Result  +' '+UH_LBEG+'d'+Self.dicname+UH_LEND;
end;

{ Generates JMDICT-compatible entry for this result. Although compatible,
 it uses a custom <article> tag = a middle ground between <entry> and <sense>.
 It consists of <sense>s, grouped and with <dict> source but without kana/kanji. }
function TSearchResArticle.ToEdictXml: string;
var i: integer;
begin
  Result := '<article>';
  for i := 0 to Self.entries.Count-1 do
    Result := Result + Self.entries.items[i].ToEdictXml;
  if dicname<>'' then
    Result := Result + '<dict>' + HtmlEscape(dicname) + '</dict>';
  Result := Result + '</article>';
end;

{$IFNDEF AUTOTEST}
function Vocab1ToSearchRes(const ASource: TVocab1Article): TSearchResArticle;
begin
  Result.Reset;
  Result.dicname := ASource.dicname;
  Result.entries := ASource.entries;
end;
{$ENDIF}


{
Deflex()
Generates a list of possible deflected versions of the word.

Japanese version:
Looks for the stem of the word, tries to deflex it to a base state with a deflexion table,
then it must end with one of the few possible verb suffixes.

There's a number of weak places in this algorithm:

1. It doesn't handle secondary verbs (itteOKU).
  This is okay since verbs don't get joined randomly. Common combinations are
  covered in the dictionaries.
  If the combination is not covered, our goal here is to match it as 2 separate
  properly recognized words:
    itte + oku

2. Verb conjugations can be chained into rather long chains (itte rare nakereba).
  The algorithm doesn't really parse the full chain of conjugations,
  it just looks for the furthest recognizable stem:
    itterarenaKERE -> itterarenaKU
  which ends in a valid suffix:
    kere + BA is valid
  It'll also generate all midway versions:
    itterarenai
    itterareru
    itteru
    iu
  And look for suffixes after them. Most versions will be thrown away as there's
  no valid suffix.

  So the algorithm relies on being given a correctly cut word. If it's given
  too much:
    itterarenakerebanarimasen
  It'll only suggest
    itterarenakerebanarimaSU
  Which won't match anything from the dictionary. Meanwhile
    itterarenaKERE
  Won't match since what it continues with (banarimasen) is not a valid suffix.

3. This works for words we type in the dictionary search, as it's not expected
  to handle multiple words anyway.

  But when we type in the editor, and especially when we translate blocks of text,
  we DO NOT know where the word ends at all. We can guess the upper limit, but
  it's the job of the search/deflex to find matches.

  For these cases we have to relax the suffix requirement. The guesses do not have
  to end in suffix at all.

  The algorithm then has conflicting goals:
  - Find the longest match ending with a known inflexion, without understanding
    the inflexions intbetween
      itterarenaKERE  -- "tterarena" is not even parsed
  - And also somehow keep it the shortest, otherwise we'll end with eating two
    words instead of one:
      itterarenakereba narimaSEN -- we ate too much because the middle is not parsed

4. There are some tricks we could do with kanji. Most kanji words are either
    KANJI[+KANJI]+kana tail
  Or
    KANJI+kana+KANJI+kana tail
  We could scan for up to 2 consecutive kanji, or separated by at most 1 kana,
  and consider any kanji after that to be a sign of the next word.

  But we do not only get tasked to translate kanji:
  - Some words are usually fully in kana, as with "narimasen"
  - Sometimes we parse typing buffers. There's only kana as the user types.

5. So what it should really do IN THIS CASE is something like:
  - find all possible breakage points where the left part ends in something like
    inflected stem
  - BONUS POINTS if there's a matching suffix after that.
  - REJECT certain cases outright (e.g. empty root; 3+ kanji to the left, long kana span between 2 kanji)
  - BONUS POINTS if the match is just the right length (not too short, but definitely not too long)
  - produce all these lookup guesses

5. Additionally, for lookup guesses we need DEFLECTED VERSIONS OF THE ROOT.
  We probably won't be able to find "itterarenaku" in the dictionary. "itteru" or "iu" are
  our best chances.
  On the other hand, if we just match "itte" and leave "rarenaku", that would definitely
  parse weird.
  So what we would ideally want is to parse the word down to the root, then construct
  various inflected verb stages and look for each. The best one would be used.
  But we don't have this. Sorry.

mustsufokay:
  If true, the tail after the inflected stem MUST be one of the allowed suffixes exactly.
  If false, we don't even check. (currently)
  In other words, currently no option to "prioritize those with suffixes" etc.
}

procedure Deflex(const w:string;sl:TCandidateLookupList;prior,priordfl:byte;mustsufokay:boolean);
var ws: integer; //length of w in symbols. Not sure if needed but let's keep it for a while
    i,j,k:integer;
    roma:string;
    core:string;
    pass:boolean;
    lastkanji:integer;
    ad:string;
    suf:string;
    sufokay:boolean;
    dr: PDeflectionRule;
    ct: TCandidateLookup;
begin
  ws:=flength(w);

  if curlang='j'then
  begin
    lastkanji:=0;
    for i:=1 to flength(w) do
      if EvalChar(fgetch(w,i))<>EC_HIRAGANA then
        lastkanji:=i
      else
        break;
    core:=fcopy(w,1,lastkanji);
    roma:=fcopy(w,lastkanji+1,flength(w)-lastkanji);
    for i:=0 to defll.Count-1 do
    begin
      dr := defll[i];

      //Check inflected ending at every position to the right of the core.
      //This is probably done to allow for unaccounted middle-of-the word variations.
      for j:=0 to flength(roma)-flength(dr.infl) do begin

        if dr.infl = '' then begin
          //A special inflected ending of "nothing", for iru/eru verbs like "miru -> mi+masu" (no inflected ending)

          if (j<>0) or (core='') then continue; //skip empty guesses

          {
          Original Wakan had this restriction in place:
            if (j<>0) or (core='') then continue
          In other words, only allow empty inflection guesses with kanji root + only straight after the root.
          I don't see why it's needed. Maybe for speedup? But what about typing suggestions, we don't yet have kanji root.

          Perhaps this was to deter the case of 0-core 0-position 0-inflex = 0-word which matches everything?
          But we can just require EITHER core [+perhaps 0-position] OR non-0-position.
          }

        end else begin
          if fcopy(roma,j+1,flength(dr.infl)) <> dr.infl then
            continue; //inflection doesn't match
        end;

        //Check kuru/iku kanji match
        case dr.vt of
         'K': if (j<>0) or ((core<>'') and (core<>{$IFNDEF UNICODE}'6765'{$ELSE}#$6765{$ENDIF})) then continue;
         'I': if (j<>0) or ((core<>'') and (core<>{$IFNDEF UNICODE}'884C'{$ELSE}#$884C{$ENDIF})) then continue;
        //else no restriction
        end;

        if flength(dr.defl)+j>6 then
          continue;

        if core+fcopy(roma,1,j)+dr.defl = w then
          continue; //already have this guess

       //Calculate inflected length for this guess
        ws:=flength(core)+j+flength(dr.infl);
        if ws = 0 then continue; //do not consider 0-length guesses

        ad:=core+fcopy(roma,1,j)+dr.defl;
        if sl.Find(ws, dr.vt, ad)>=0 then
          continue; //already added

        //Check if the rest of the string is a suffix valid for this guessed verb type/conjugation
        suf:=fcopy(roma,j+1+flength(dr.infl),flength(roma)-j-flength(dr.infl));
        sufokay:=(suf='');
        for k:=0 to suffixl.Count-1 do
          if (dr.sufcat+suf=suffixl[k]) or ((dr.sufcat='*') and (suffixl[k][1]+suf=suffixl[k])) then
            sufokay:=true;
        if sufokay then //include suffix in the inflected length if it is recognized
          ws := ws + flength(suf);
        if sufokay or not mustSufokay then
        begin
          if sufokay and (dr.infl<>'') then
            sl.Add(priordfl, ws, dr.vt, rtNormal, ad)
          else
            sl.Add(1, ws, dr.vt, rtNormal, ad);
        end;

      end; //of j iteration
    end; //of defll[] enumeration
  end;

  if curlang='c'then
  begin
    sl.Add(prior, ws, 'F', rtNormal, w);
    if pos('?',KanaToRomaji(w,'c'))>0 then exit;
   //For every lookup candidate check if there were "unknown" tone markers,
   //and generate all possible resolutions for those.
    repeat
     //At each pass we only support one "unknown" marker at most,
     //but we make passes until there are no unresolved entries.
      pass:=true;
      i:=0;
      while i<sl.Count do
      begin
        ct := sl[i]^;
        j := fpos({$IFNDEF UNICODE}'F030'{$ELSE}#$F030{$ENDIF},ct.str);
        if j>0 then begin
          pass:=false;
         //First version is modified in-place to avoid slow deletions
         {$IFNDEF UNICODE}sl[i]^.str[j*4+3] := '1';{$ELSE}sl[i]^.str[j] := #$F031;{$ENDIF}
         //Next versions are made into copies
         {$IFNDEF UNICODE}ct.str[j*4+3]:='2';{$ELSE}ct.str[j] := #$F032;{$ENDIF}
          sl.Add(ct);
         {$IFNDEF UNICODE}ct.str[j*4+3]:='3';{$ELSE}ct.str[j] := #$F033;{$ENDIF}
          sl.Add(ct);
         {$IFNDEF UNICODE}ct.str[j*4+3]:='4';{$ELSE}ct.str[j] := #$F034;{$ENDIF}
          sl.Add(ct);
         {$IFNDEF UNICODE}ct.str[j*4+3]:='5';{$ELSE}ct.str[j] := #$F035;{$ENDIF}
          sl.Add(ct);
        end else inc(i);
      end;
    until pass;
  end;
end;

{
MakeLookupList()
Given a search string, search type, match type and word type,
builds a list of all possible matches to look for in the dictionary.
wt: word type (can be EC_UNKNOWN, EC_IDG_CHAR, EC_HIRAGANA, EC_KATAKANA)
}
procedure TDicSearchRequest.MakeLookupList(se: TCandidateLookupList; search: string;
  wt: TEvalCharType);
var _s: string;
  i: integer;
  searchKana: string;
  addGuess: boolean;
  prior: integer;
begin
  case st of
    stRomaji: begin
     //Convert user-romaji to kana as an intermediate,
     //since the request could be in a different romaji system (ex. mujun instead of mudjun)
      if curlang='j'then
        searchKana:=RomajiToKana('H'+search,'j',[rfReplaceInvalidChars])
      else
        searchKana:=RomajiToKana(search,'c',[rfReplaceInvalidChars]);

     //If the conversion was less than perfect use lower priority
      if pos('?', searchKana) > 0 then begin
        prior := 6;
        searchKana := repl(searchKana, '?', '');
      end  else
        prior := 9;
      if searchKana <> '' then begin //even after repl('?','')
        if AutoDeflex then
          Deflex(searchKana, se, prior, prior-1, true);
       //In any case add non-deflexed kana translation
        se.Add(prior, flength(searchKana), 'F', rtNormal, searchKana);
      end;

     //Add exact original romaji (it's user-roma and not db-roma, but who knows?)
      search := SignatureFrom(search);
      if search <> '' then
        se.Add(9,length(search),'F',rtRoma,search);

     //Add shortened roma guesses if requested - no deflexion though
      if MatchType = mtBestGuessLeft then
        for i:=flength(search)-1 downto 2 do begin //no less than 2 chars because 1 char roma lookups are pointless (I think)
          //The longer the cut, the lower the priority
          prior := 8-((flength(search)-i) div 2);
          if prior < 0 then
            prior := 0;
          se.Add(prior, length(search), 'F', rtRoma, fcopy(search,1,i));
        end;
    end;

    stEnglish:
      se.Add(9, 1, 'F', rtNormal, search);

    stJapanese: begin
      //Ignore all weird word types - the auto-translation speed may depend on
      //not looking at the clearly wrong candidates
      if not (wt in [EC_UNKNOWN, EC_IDG_CHAR, EC_HIRAGANA, EC_KATAKANA]) then
        exit;

      _s := ChinTraditional(search);

      //If we're unsure of the type, also preserve the word exactly as typed
      if (wt=EC_UNKNOWN) and (_s<>search) then
        se.Add(9, flength(search), 'F', rtNormal, search);

      //Standard (traditionalized) version
      se.Add(9, flength(_s), 'F', rtNormal, _s);

      if wt=EC_KATAKANA then exit; //nothing more to be tested for katakana

      {
      This is a bit weird. There are two ways to drop the right part of the word.
      Deflex accepts "mustsufokay = false" which means it can look for inflected
      endings in the middle of the word and drop the rest.
      And we have a tail-drop mechanics below.
      }

      //If this is a potentially deflexable word, deflex
      //But only use tail-drop deflex options if we accept left matches
      if (wt in [EC_UNKNOWN, EC_IDG_CHAR]) and AutoDeflex then
        Deflex(_s, se, 9, 8, MatchType <> mtBestGuessLeft);

     {
     Generate partial left guesses
     Here's a list of what this does:
     1.
        (PART-DROP)
     3. Add every shortened version where next character is a kanji:
           KKaKKaa
           KKaK
           KKa
           K
        Deflex() covers this too if it runs, but it might be disabled.
     }
      if (MatchType in [mtBestGuessLeft, mtExactMatch]) and (wt <> EC_KATAKANA) then
        for i:=flength(_s)-1 downto 1 do
        begin
          addGuess := false;

          //In BestGuess, add every single shortened version with non-zero root
          if (MatchType = mtBestGuessLeft) and (i>=1) then
            addGuess := true;

          //If a sequence ends in 1 or 2 hiragana chars which are together a particle,
          //add the sequence also without them. (At most 2, or it'll be slow)
          if not addGuess
          and (i>=flength(_s)-2)
          and (partl.IndexOf(fcopy(_s,i+1,flength(_s)-i))>-1) then
            addGuess := true;

          //Allow some cutting in MatchLeft. This is the way we
          //inherited it, for now it'll stay.
          if (MatchType = mtMatchLeft)
          and (EvalChar(fgetch(_s,i))=EC_IDG_CHAR) then
            addGuess := true;

          if addGuess then
            se.Add(i, i, 'F', rtNormal, fcopy(_s,1,i));
        end;

    end;
  end;
end;


{ Makes upper bound guess on the length of the word starting at the specified
 position. Actual word may be shorter (mi ni iku -> MI) or longer (rarely).
 A guess at the word type is also given:
   EC_UNKNOWN: We are not sure.
   EC_IDG_CHAR: Ideographic word (kanji/kana/pinyin mixed).
   EC_KATAKANA: Katakana-only word.
   EC_HIRAGANA: Hiragana-only word.
   EC_BOPOMOFO: Bopomofo-only word.
   EC_PUNCTUATION: Punctuation only.
   EC_LATIN_FW,
   EC_LATIN_HW: Latin characters only.
 This has to work both on Japanese and Chinese text, no matter what mode
 we're in. }
function GuessWord(const AString: string; APos: integer; out AWordType: TEvalCharType): string;
var wt2:TEvalCharType;
  i:integer;
  tc: string; //"this character"
  HasHonorific: boolean;
  HiraCount,
  KanjiCount: integer;
begin
  if (APos <= 0) or (APos > Length(AString)) then begin
    AWordType := EC_UNKNOWN;
    Result := '';
    exit;
  end;

 //Determine initial word type from the first symbol
  tc := AString[APos];

 //Skip initial japanese honorific, if present
  HasHonorific := (tc='お') or (tc='ご');
  if HasHonorific and (Length(AString) >= APos+1)
  and (EvalChar(AString[APos+1]) in [EC_UNKNOWN, EC_IDG_CHAR, EC_HIRAGANA]) then
    AWordType := EvalChar(AString[APos+1])
  else
    AWordType := EvalChar(tc);
  if not (AWordType in [EC_UNKNOWN, EC_IDG_CHAR, EC_HIRAGANA, EC_KATAKANA,
    EC_IDG_PUNCTUATION]) then
    AWordType := EC_IDG_PUNCTUATION;

 {
  EC_IDG_CHAR for Japanese:
  Allowed syllable sequences (captured part in brackets):
    (***) A
    (C H H) C
    (C H C H) C
    (C H C H H) C
    (C H C H H H) C
  Where C is IDG_CHAR, H is HIRAGANA and A is anything else.
  I.e. up to one "middle" hiragana and any number of "tail" hiragana,
  anything else breaks word.
 }
  HiraCount := 0; //total number of hiragana syllables in an IDG_CHAR word
  KanjiCount := 0;
  Result := AString[APos];
  repeat
    Inc(APos);
    if APos>Length(AString) then
      break;

      tc := AString[APos];
      wt2:=EvalChar(tc);
      if not (wt2 in [EC_UNKNOWN, EC_IDG_CHAR, EC_HIRAGANA, EC_KATAKANA,
        EC_IDG_PUNCTUATION]) then
        wt2:=EC_IDG_PUNCTUATION;

      case AWordType of
      EC_IDG_CHAR: begin
        if wt2=EC_HIRAGANA then begin
          if tc<>'っ' then //doesn't count towards characters
            Inc(HiraCount)
        end else
        if wt2=EC_IDG_CHAR then begin
          if HiraCount>=2 then break;
         //There could be very long kanji chains (especially in chinese),
         //we have to have some limit
          Inc(KanjiCount);
          if KanjiCount>=6 then
            break;
        end else
          break;
      end;
      else
        if wt2<>AWordType then break;
      end;

    Result := Result + tc;
  until false;
end;


{
DicSearch()
Don't use if you're doing a lot of searches, use TDicSearchRequest instead.
}
procedure DicSearch(search:string;st:TSearchType; MatchType: TMatchType;
  wt:TEvalCharType;MaxWords:integer;sl:TSearchResults; DictGroup:integer;
  var wasfull:boolean);
var req: TDicSearchRequest;
begin
  req := TDicSearchRequest.Create;
  try
    req.st := st;
    req.MatchType := MatchType;
    req.MaxWords := MaxWords;
    req.DictGroup := DictGroup;
    req.AutoDeflex := true; //those who use this expect this
    req.Prepare;
    req.Search(search, sl, wt);
    wasfull := req.WasFull;
  finally
    req.Free;
  end;
end;


{
Dictionary search
}

constructor TDicSearchRequest.Create;
begin
  inherited;
  se:=TCandidateLookupList.Create;
  presentl:=TStringList.Create;
  presentl.Sorted := true; //faster searching
  SetLength(dics, 0);
  //Default settings
  MaxWords := -1; //no limit
end;

destructor TDicSearchRequest.Destroy;
var di: integer;
begin
  //Destroy cursors
  for di := 0 to Length(dics) - 1 do
    FreeAndNil(dics[di].cursor);
  se.Destroy;
  presentl.Destroy;
  inherited;
end;

//Search request params can't be changed after it has been Prepare()d.
procedure TDicSearchRequest.Prepare;
var di, dj: integer;
  dic: TJaletDic;
  dicSetup: TDicSetup;
  dicMatchType: TDicMatchType;
  prior: integer;
begin
  if (MaxWords > 0) and (MaxWords < 10) then MaxWords := 10;

  //Destroy existing cursors
  for di := 0 to Length(dics) - 1 do
    FreeAndNil(dics[di].cursor);
{$IFNDEF AUTOTEST}
  FreeAndNil(CUser);
  FreeAndNil(CUserPrior);
{$ENDIF}

  //Verify some configuration? Stuff we can't do.
  if (st=stEnglish) and not (Self.MatchType in [mtExactMatch, mtMatchLeft]) then
    Self.MatchType := mtExactMatch;

 //Create dictionary cursors
  SetLength(dics, dicts.Count);
  for di := 0 to dicts.Count-1 do begin
    dics[di].cursor := nil;
    dic:=dicts[di];
    if not dic.loaded or not dicts.IsInGroup(dic,DictGroup) then
      continue;

    case MatchType of
    mtMatchLeft: dicMatchType := dmtMatchLeft;
    mtMatchRight: dicMatchType := dmtMatchRight;
    mtMatchAnywhere: dicMatchType := dmtMatchAnywhere;
    else //mtExactMatch, mtBestGuessLeft
      dicMatchType := dmtExactMatch;
    end;

    dic.Demand;
    dics[di].cursor := dic.NewLookup(dicMatchType);

   //Calculate priority class. Lowest priority gets 20000, highest gets 0
    prior := dicts.Priority.IndexOf(dic.name);
    if prior<0 then
      prior := 20000
    else
      prior := Trunc(20000*(prior/dicts.Priority.Count));
    dics[di].PriorityClass := prior;
  end; //of dict enum

 //Sort by dict priority, glosses are gonna be listed in that order in merged entries (for now anyway)
  for di := 0 to Length(dics)-2 do begin
    dj := di + 1;
    while (dj>0) and (dics[dj].PriorityClass<dics[dj-1].PriorityClass) do begin
      dicSetup := dics[dj-1];
      dics[dj-1] := dics[dj];
      dics[dj] := dicSetup;
      Dec(dj);
    end;
  end;

{$IFNDEF AUTOTEST}
  //Create cached table cursors
  CUser := TTextTableCursor.Create(TUser);
  stUserKanji := TUser.GetSeekObject('Kanji');
  stUserIndex := TUser.GetSeekObject('Index');
  CUser.SetOrder('Kanji_Ind');
  CUserPrior := TTextTableCursor.Create(TUserPrior);
  stUserPriorKanji := TUserPrior.GetSeekObject('Kanji');
  fldUserPriorCount := TUserPrior.Field('Count');
{$ENDIF}
end;

{
Search()
Searches the dictionary for all candidate translations to the line.
search
  string to look for:
    stJp: romaji (in user preferred romaji system)
    stEn: english/other "translated" language
    stEditor/stClipboard: kanji/kana/whatever
wt
  Word type hint. Only used for stJapanese mode.
    EC_HIRAGANA: A hiragana-only or hiragana-leading word, perhaps a particle.
    EC_KATAKANA: A katakana-only or katakana+hiragana tail word.
    EC_IDG_CHAR: A word starting with a kanji (after perhaps o- or go-) and maybe a tail of mixed kanji/hiragana.
    EC_UNKNOWN: No word type hint.

  Why pass word type hints? Because the auto-translator already uses these rules
  to choose where to cut the next word. So it might as well pass them.

  EC_UNKNOWN means guess the type / try everything.

sl
  Match results to return.
wasfull
  True if we have retrieved all of the available results.
}

procedure TDicSearchRequest.Search(search: string; sl: TSearchResults; wt: TEvalCharType);
var i,di:integer;
begin
  wasfull:=true;
  if search='' then exit;
  mess := nil;
  se.Clear;
  presentl.Clear;

  nowt:=now;

  MakeLookupList(se, search, wt);

  if MaxWords > 0 then begin
    //Sort the lookup candidates by priority if we may only have to look at the first few
    //Don't waste CPU if we have to look at all
    se.Sort;
  end;

 //Particles are built into the program.
 //Eventually should be moved out to own dictionaries and get common treatment.
  if st=stJapanese then //for now only this mode, although we can convert stRomaji to kana too
    if wt in [EC_UNKNOWN, EC_HIRAGANA] then //assume that otherwise we know what we're doing
      if partl.IndexOf(search)>-1 then
        with sl.AddResult^ do begin
          Reset();
          sdef := 'P';
          kana := ChinTraditional(search);
          kanji := kana;
          with AddArticle^ do begin
            sdef := 'P';
            entries.Add(_l('#01142^%s particle', [KanaToRomaji(search,'j')]), '');
          end;
          inflen := Length(kanji);
        end;

 { kanaonly:
  If this is set, we're going to convert kanji+kana to ??+romaji and search for that.
  Makes sense only if we're sure our word is all kana, but gives us better kana
  coverage (e.g. hiragana/katakana).
  This is expected, for instance, when handling user input. While we store original
  input as a roma lookup, deflexed lookups are in kana (deflexion happens in kana),
  and without this flag we'd miss words like KATAKANA ROOT + hiragana verb ending.

  Note that if lookup candidates could possibly differ in this regard, we'd have
  to re-check for this property for every candidate. }
  kanaonly := (st=stJapanese) and (
    (wt in [EC_HIRAGANA, EC_KATAKANA, EC_BOPOMOFO])
    or (
      (wt=EC_UNKNOWN) and TestCharsAre(search, [EC_HIRAGANA, EC_KATAKANA, EC_BOPOMOFO])
    )
  );

  if MaxWords <= 0 then begin
   //Loop over dictionaries first as that gives better locality.
    for di:=0 to Length(dics)-1 do begin
      if dics[di].cursor=nil then continue;
      for i:=0 to se.Count-1 do
        TestLookupCandidate(dics[di], se[i], sl);
    end;
  end else begin
   //If we need N best matches we have to loop over candidates first (in the priority order)
   {
    We're doing some voodoo to get best matches from all dictionaries.
    TestLookupCandidate() will return up to MaxWords matches from EACH dictionary,
    and then we'll sort and cut them.
    Otherwise the first dic may eat all the slots on vague requests.
   }
    for i:=0 to se.Count-1 do begin
      for di:=0 to Length(dics)-1 do begin
        if dics[di].cursor=nil then continue;
        TestLookupCandidate(dics[di], se[i], sl);
      end;
      //Only test for count after all dics are queried
      if sl.Count > MaxWords then begin
        WasFull := false;
        break;
      end;
    end;
  end;

  FinalizeResults(sl);

  mess.Free;
end;

resourcestring
  sDicSearchTitle='#00932^eDic.search';
  sDicSearchText='#00933^ePlease wait. Searching dictionary...';

function IsAppropriateVerbType(const sdef: string; const mk:TMarkers): boolean;
begin
  case sdef[1] of
   'F': Result := true;
   '2': Result := TestMarkers(mk,#66);
   'S': Result := TestMarkers(mk,#83);
   'K': Result := TestMarkers(mk,#84);
   'I': Result := TestMarkers(mk,#77);
   '1': Result := TestMarkers(mk,#67#68#69#70#71#72#73#74#75#76#77#78#79#80#81#82#83#84#85
          +#110#111#112#113#114#115#116#117#118);
   'A': Result := TestMarkers(mk,#43#46#47#48#49#99#100);
   'N': Result := TestMarkers(mk,#45);
  else
    Result := false;
  end;
end;

//Returns the base popularity class for a record (the lower the better)
function GetPopClass(const mk:TMarkers): integer;
begin
  Result := 40;
  if fSettings.cbPreferNounsAndVerbs.Checked then begin
    if TestMarkers(mk,#46#46#58#59#60#61#62#63#106) then dec(Result,5);
    if TestMarkers(mk,#66#67#68#69#70#71#72#73#74#75#76#77#78#79#80#81#82#83#84#85
          +#110#111#112#113#114#115#116#117#118) then dec(Result,5);
  end;
  if fSettings.cbPreferPolite.Checked then begin
    if TestMarkers(mk,#86) then dec(Result,1); //honor
    if TestMarkers(mk,#87) then dec(Result,2); //humor
    if TestMarkers(mk,#88) then dec(Result,3); //humble
  end;
  if TestMarkers(mk,#91) then inc(Result,20); //obsolete
  if TestMarkers(mk,#92) then inc(Result,20); //obscure
  if TestMarkers(mk,#93) then inc(Result,20); //outd-kanji
  if TestMarkers(mk,#94) then inc(Result,20); //outd-kana
  if TestMarkers(mk,#135) then inc(Result,20); //rare

  if fSettings.cbPreferPopular.Checked and TestMarkers(mk,MarkPop) then dec(Result,150); //pop
end;

procedure TDicSearchRequest.TestLookupCandidate(ds: TDicSetup; lc: PCandidateLookup;
  sl: TSearchResults);
var
  dic: TDicLookupCursor;
  matchCount: integer;

  sxx: string;  //==lc.str
  sxxr: string; //same in romaji
  sp:integer;   //==lc.priority
  sdef:char;    //==lc.verbType

  raw_entries: TEntries;
  markers,kmarkers:TMarkers;
  popclas:integer;
  UserScore:integer;
  UserIndex:integer;
  sort:integer;
  freq:integer;
  ssig:string; //translation signature (reading x kanji) to merge duplicates
  scomp:PSearchResult;
  scur:TSearchResult;
  sart:PSearchResArticle;

  existingIdx: integer;

{$IFNDEF AUTOTEST}
 //Used several times with different kanji_vals
  procedure TryGetUserScore(kanji_val: string);
  begin
   //CUser.Order is 'Kanji_ind'
    CUser.Locate(@stUserKanji,kanji_val);
    while (not CUser.EOF) and (kanji_val=CUser.Str(TUserKanji)) do
    begin
      if dic.GetPhonetic=CUser.Str(TUserPhonetic) then
      begin
        UserScore:=CUser.Int(TUserScore);
        UserIndex:=CUser.Int(TUserIndex);
      end;
      CUser.Next;
    end;
  end;
{$ENDIF}

begin
  dic := ds.cursor;
  sxx:=lc.str;
  if sxx='' then exit;
  sp:=lc.priority;
  sdef:=lc.verbType;

 //Initial lookup
 { KanaToRomaji is VERY expensive so let's only call it when really needed }
  case st of
    stRomaji: begin
      if lc.roma=rtRoma then
        sxxr:=sxx
      else begin
        sxxr:=DbKanaToRomaji(sxx,curlang);
        if sxxr='' then exit;
      end;
      dic.LookupRomaji(sxxr);
    end;
    stEnglish: dic.LookupMeaning(sxx);
    stJapanese:
      if lc.roma=rtRoma then
        dic.LookupRomaji(lc.str)
      else
      if kanaonly then begin
        sxxr:=DbKanaToRomaji(sxx,curlang);
        if sxxr='' then exit;
        dic.LookupRomaji(sxxr);
      end else
        dic.LookupKanji(sxx);
  end;

  matchCount := 0;

  while dic.HaveMatch do begin
    if (mess=nil) and (now-nowt>1/24/60/60) then
      mess:=SMMessageDlg(_l(sDicSearchTitle), _l(sDicSearchText));

    raw_entries:=dic.GetEntries;
    markers:=raw_entries.MergeMarkers;
    kmarkers:=dic.GetKanjiKanaMarkers;

    if IsAppropriateVerbType(sdef, markers) then
    if (not dic_ignorekana) or (st<>stJapanese)
      or (not kanaonly) or raw_entries.HasMarker(MarkUsuallyKana) then
    begin

     //Calculate popularity class
      popclas := GetPopClass(markers)+GetPopClass(kmarkers);

{$IFNDEF AUTOTEST}
      if MindUserPrior
      and CUserPrior.Locate(@stUserPriorKanji,dic.GetKanji) then
        dec(popclas,10*CUserPrior.Int(fldUserPriorCount));
{$ENDIF}

      UserScore:=-1;
      UserIndex:=0;
{$IFNDEF AUTOTEST}
      if raw_entries.HasMarker(MarkUsuallyKana) then
        TryGetUserScore(dic.GetPhonetic);
      TryGetUserScore(dic.GetKanji);
      if (UserScore=-1) and (dic.GetKanji<>ChinSimplified(dic.GetKanji)) then
        TryGetUserScore(ChinSimplified(dic.GetKanji));
{$ENDIF}

     //Calculate sorting order -- the bigger the worse (will apear later in list)
      case st of
        stEnglish: begin
          if pos(trim(uppercase(sxx)),trim(uppercase(dic.GetArticleBody)))=1 then sort:=10000 else sort:=11000;
          sort:=sort+popclas*100;
        end;
        stRomaji: sort:=(10000*(9-min(sp,9)))+length(dic.GetPhonetic)*1000+popclas*10;
        stJapanese:
          sort:=(10000*(9-min(sp,9)))-length(dic.GetPhonetic)+popclas*10;
         //in auto-translation mode longer matches are better (those are longer *exact* matches after all)
      else sort:=0;
      end;
      sort:=sort+10000;

      if (fSettings.cbPreferUserWords.Checked) and (UserScore>-1) then dec(sort,1000);
      if (fSettings.cbPreferUserWords.Checked) and (UserScore>1) then dec(sort,1000);
      if IsKanaCharKatakana(dic.GetPhonetic, 1) then inc(sort,1000);
      sort:=sort+dic.dic.priority*20000;
      sort:=sort+ds.PriorityClass;

      if fSettings.cbShowFreq.Checked or fSettings.cbOrderFreq.Checked then
        freq := dic.GetFrequency //may also return -1
      else
        freq := -1;

      if fSettings.cbOrderFreq.Checked and (freq>=0) then begin
        if freq>=500000 then
          sort := sort + 0 //top score
        else
        if freq>=10000 then
          sort := sort + 8000 - (freq-10000) div 100
        else
        if freq>=1000 then
          sort := sort + 9000 - (freq-1000) div 10
        else
          sort := sort + 10000 - freq;
      end;

     //Fill in current result
      scur.Reset;
      scur.userindex := UserIndex;
      if fSettings.cbStatusColors.Checked then
        scur.userscore := UserScore
      else
        scur.userscore := -1;

      scur.kana := dic.GetPhonetic;
     { TODO: Not good. UsuallyKana is set for only some results, not all of them.
        The replacement is applied for all. }
      if fSettings.cbReplaceKanji.Checked and raw_entries.HasMarker(MarkUsuallyKana) then
        scur.kanji := scur.kana
      else
        scur.kanji := ChinSimplified(dic.GetKanji);


      if st in [stJapanese, stRomaji] then
        scur.inflen := lc.len
      else
       //with stEnglish lc.len doesn't help, but there's no deflexion anyway
        scur.inflen := flength(scur.kanji);

      scur.sdef := sdef;
     {
      TODO: There were two other ways sdef could be set.
      First one is similar to what entries.EntryText does:
        if sdef<>'F' then scur.sdef:='I' else scur.sdef:='F';

      Second one is like this:
        if (pos('-v'+UH_LEND,entry)>0) then
          scur.sdef:='I'
        else
          scur.sdef:='F';

      Second one was ultimately the one active. -v refers to markers which have it,
      which turns out to be ichidan and all godan subtypes.
      I stands for "iku verb" so I don't understand what's going on, so I'm
      disabling it for now.
      When/if I find out who expects sdef==I and why, I may reenable it.
     }

      sart := scur.AddArticle;
      sart.score := sort;
      sart.dicname := dic.dic.name;
      sart.dicindex := dic.GetIndex;
      sart.freq := freq;
      sart.entries := raw_entries;

     //Copy to header (in case we're going to store this)
      scur.score := sart.score;
      scur.dicname := sart.dicname;
      scur.dicindex := sart.dicindex;
      scur.freq := sart.freq;

    { Result grouping.
      This intends to group results which have the same reading x kanji into
      a single entry.
      In the future, grouping should be smarter. Different kanji/kana for the
      same meaning should be grouped together like it's done in EDICT.
      Sometimes only some of the entries have to be returned (if others are
      inappropriate due to verb type or something). }

     //result signature (reading x kanji)
      ssig:=dic.GetPhonetic+'x'+dic.GetKanji;
     //if we already have that result, only upgrade it (lower its sorting order, add translations)
      existingIdx := presentl.IndexOf(ssig);
      if existingIdx>=0 then begin
        existingIdx := integer(presentl.Objects[existingIdx]); //presentl is sorted, real indexes are kept this way
       //Update existing one
        scomp:=sl[existingIdx];
        //lower sorting order
        if scomp.score > scur.score then
          scomp.score := scur.score;
        //add user index if missing
        if scomp.userindex < 0 then begin
          scomp.userindex := scur.userindex;
          scomp.userscore := scur.userscore;
        end;
        //sometimes we have two identical deflexions, one with longer source match than another
        if scomp.inflen < scur.inflen then begin
          scomp.inflen := scur.inflen;
          scomp.sdef := scur.sdef;
        end;

       //already present? multiple matches are totally possible
        if scomp.FindArticle(sart.dicname, sart.dicindex)<0 then
         //add tl
          scomp.AddArticle(sart^);
      end else begin
       //Store group
        existingIdx := sl.Add(scur);
        presentl.AddObject(ssig, TObject(existingIdx));
      end;

      Inc(matchCount);
    end;

   //If we only need N matches, get out.
   //Note we're still counting to N matches from THIS PARTICULAR DICT. See comments in Search().
    if (MaxWords > 0) and (matchCount >= MaxWords) then begin
      WasFull := false;
      break;
    end;

    dic.NextMatch;
  end;
end;

procedure TDicSearchRequest.FinalizeResults(sl: TSearchResults);
{$IFNDEF AUTOTEST}
var i, j: integer;
  scomp: PSearchResult;
  voc_entry: string;
  sl2: TStringList;
  sl2i: integer;
  sart: TSearchResArticle;
{$ENDIF}
begin
  if sl.Count > 1 then //saves a bit when translating
    sl.SortByFrequency;

 //We might have more results than requested and could do sl.Trim(MaxWords) here,
 //but we have those results anyway -- why delete.

{$IFNDEF AUTOTEST}
 //Add user entries to the beginning
  for i:=0 to sl.Count-1 do
    if sl[i].userindex<>0 then
    begin
      scomp:=sl[i];

      CUser.Locate(@stUserIndex,sl[i].userindex);
      voc_entry := FixVocabEntry(CUser.Str(TUserEnglish));

     //Often user entries are copies of dictionary entries, remove such entries.
     //Fat chance, but whatever.
      for j := 0 to Length(scomp.articles)-1 do
       { We will be forgiving and compare without marks, because otherwise scomp
         may never match - it contains <dDictName>
         It's also possible that vocabulary entry contains several dict entries,
         so we pos() for them instead. }
        if pos(remmark(scomp.articles[j].ToLegacyString), remmark(voc_entry))>=0 then begin
          scomp.DeleteArticle(j);
         //Do not break, per above
        end;
     { All of this is pretty shitty since we can't reorder results inside vocab
      entry. Common deduplication should be written instead. }

     //Delete ~F/~I word type if it's present at the beginning of the vocab entry -
     //it will be added dynamically
      if (length(voc_entry)>0) and (voc_entry[1]=UH_WORDTYPE) then
        delete(voc_entry,1,2);

     //Enhance with word categories
      sl2:=TStringList.Create;
      try
        ListWordCategories(CUser.Int(TUserIndex),sl2);
        for sl2i:=0 to sl2.Count-1 do
          voc_entry:=voc_entry+' '+UH_LBEG+'l'+copy(sl2[sl2i],3,length(sl2[sl2i])-2)+UH_LEND;
      finally
        sl2.Free;
      end;

      sart.Reset;
      sart := Vocab1ToSearchRes(ParseVocab1Article(voc_entry));
      sart.score := 0; //ultimate
      sart.dicname := '';
      sart.dicindex := scomp.userindex;
      scomp.InsertArticle(0, sart);
    end;
{$ENDIF}
end;

initialization
  defll := TDeflectionList.Create;
  suffixl := TStringList.Create;
  partl := TParticleList.Create;

finalization
  FreeAndNil(partl);
  FreeAndNil(suffixl);
  FreeAndNil(defll);

end.
