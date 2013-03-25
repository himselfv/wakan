unit JWBDicSearch;
{
Dictionary engine.
Has a lot of dark legacy in code form.
}

interface
uses SysUtils, Classes, JWBStrings, TextTable, MemSource, StdPrompt, JWBDic;

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
    function GetItemPtr(Index: integer): PDeflectionRule;{$IFDEF INLINE} inline;{$ENDIF}
    function MakeNewItem: PDeflectionRule;
  public
    procedure Add(const r: TDeflectionRule); overload;
    procedure Add(const s: string); overload;{$IFDEF INLINE} inline;{$ENDIF}
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
    function GetItemPtr(Index: integer): PCandidateLookup;{$IFDEF INLINE} inline;{$ENDIF}
    function MakeNewItem: PCandidateLookup;
  public
    procedure Add(priority: integer; len: integer; verbType: char; roma: TRomaType;
      const str: string); overload;
    procedure Add(const ct: TCandidateLookup); overload;{$IFDEF INLINE} inline;{$ENDIF}
    procedure Delete(Index: integer);
    procedure Clear;
    function Find(len: integer; verbType: char; const str: string): integer;
    property Count: integer read FListUsed;
    property Items[Index: integer]: PCandidateLookup read GetItemPtr; default;
  end;


{
Search result list.
Populated by TDicSearchRequest.Search().
}
type
  {
   If DicIndex/DicName are set, article contents MUST contain "dDicname" too
   because Wakan relies on it. (I'll fix that later)

   If several results are really similar, Wakan merges those into one. Header then
   contains the best of everything (lowest sort score, first availabe userindex etc)
   and article is a merger of all articles.

   Article which information (dicindex+dicname) went to the header MUST go first
   in this merger.
  }

  TSearchResult = record
    score: integer; //lower is better
    userindex: integer; //0 means not in a user dict
    userscore: integer;
    dicindex: integer;  //only one dictionary reference is supported for the result
    dicname: string;
    slen: integer; //wtf
    sdef: char; //match class -- see TCandidateLookup.verbType
    kanji: string;
    kana: string;
    entry: string;
    procedure Reset;
    function ArticlesToString: string;
  end;
  PSearchResult = ^TSearchResult;

  TSearchResults = class;
  TSearchResultsCompare = function(List: TSearchResults; Index1, Index2: Integer): Integer;

  TSearchResults = class
  protected
    FList: array of PSearchResult;
    FListUsed: integer;
    procedure Grow(ARequiredFreeLen: integer);
    function GetItemPtr(Index: integer): PSearchResult;{$IFDEF INLINE} inline;{$ENDIF}
    procedure ExchangeItems(I,J: integer);
    procedure QuickSort(L, R: Integer; SCompare: TSearchResultsCompare);
  public
    destructor Destroy; override;
    procedure Clear;
    function AddResult: PSearchResult;
    function Add(const sr: TSearchResult): integer;
    procedure CustomSort(SCompare: TSearchResultsCompare);
    procedure Sort();
    property Count: integer read FListUsed;
    property Items[Index: integer]: PSearchResult read GetItemPtr; default;
  end;


{
Search itself.
}
type
 { Translation type }
  TSearchType = (
    stJp,           //jp->en
    stEn,           //en->jp
    stClipboard,    //clipboard translation
    stEditorInsert, //editor input translation
    stEditorAuto    //text translation (no UI)
  );

 { SatanString is a string which is FString except for when you search for English,
  then it contains raw english. Happy debugging. }
  SatanString = string;

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
    a: TSearchType;
    MatchType: TMatchType;
   { Maximum number of words to look for [ignored if "full" is set?] }
    maxwords: integer;
   { Find all matches instead of only most relevant ones (slower) }
    full: boolean;
    dictgroup: TDictGroup;
    dic_ignorekana: boolean; //Do not include kana-only words in results. Half-baked.
    AutoDeflex: boolean; //Search for inflected words/conjugated verbs
    procedure Prepare; //Call after changing settings

  protected
    dics: array of TDicSetup; //empty field => dic was not loaded, skip
    //Cached cursors and seeks for User tables
    CUser: TTextTableCursor;
    stUserKanji: TSeekObject;
    stUserIndex: TSeekObject;
    CUserPrior: TTextTableCursor;
    stUserPriorKanji: TSeekObject;
    fldUserPriorCount: integer;
  public
    procedure Search(search:SatanString; wt: integer; sl: TSearchResults);

  public //Output
    WasFull: boolean;

  protected
    procedure MakeLookupList(se: TCandidateLookupList; search: string; wt: integer);

  protected
    nowt:TDateTime; //Search start time
    mess: TSMPromptForm; //"Please wait" form, created if the search takes too long
    se: TCandidateLookupList; //Lookup candidates -- see comments where this type is declared
    presentl:TStringList;
    p4reading:boolean;
    procedure TestLookupCandidate(ds: TDicSetup; lc: PCandidateLookup; wt: integer;
      sl: TSearchResults);

  protected
    kanaonly:boolean; //request is kana only, so are all the lookup candidates
    procedure FinalizeResults(sl: TSearchResults);

  end;


//Compability
procedure DicSearch(search:string;a:TSearchType; MatchType: TMatchType;
  full:boolean;wt,maxwords:integer;sl:TSearchResults;dictgroup:integer;var wasfull:boolean);

implementation
uses Forms, Windows, JWBMenu, JWBKanaConv, JWBUnit, JWBUser, JWBSettings, JWBWords, Math,
  JWBCategories, JWBEdictMarkers, JWBUserData;

procedure Deflex(const w:string;sl:TCandidateLookupList;prior,priordfl:byte;mustsufokay:boolean); forward;


{
Deflexion rules
}

//Parses deflection rule from string form into record
//See comments in wakan.cfg for format details.
function ParseDeflectionRule(const s: string): TDeflectionRule; {$IFDEF INLINE}inline;{$ENDIF}
var i: integer;
begin
  Result.vt := s[1];
  Result.sufcat := s[2];
  i := pos('->', s);
 {$IFDEF UNICODE}
  Result.infl := copy(s,3,i-3);
  if Result.infl<>'KKKK' then
    Result.infl := HexToUnicode(Result.infl);
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

function DefaultSearchResultCompare(List: TSearchResults; Index1, Index2: Integer): Integer;
var pi, pj: PSearchResult;
begin
  pi := List.FList[Index1];
  pj := List.FList[Index2];
  Result := (pi^.score-pj^.score);
end;

procedure TSearchResults.Sort();
begin
  CustomSort(@DefaultSearchResultCompare);
end;

procedure TSearchResult.Reset;
begin
  score := 0;
  userIndex := 0;
  userScore := -1;
  dicindex := 0;
  dicname := '';
  slen := 0;
  sdef := 'F'; //maybe something else?
  kanji := '';
  kana := '';
  entry := '';
end;

function TSearchResult.ArticlesToString: string;
var statpref: string;
  tmp: string;
begin
  if UserScore>=0 then
    statpref:=ALTCH_EXCL+inttostr(UserScore)
  else
    statpref:='';
 //wakan uses {} as special chars, unfortunately
  tmp := entry;
  repl(tmp, '{', '(');
  repl(tmp, '}', ')');
  Result := statpref + kanji + ' [' + statpref + kana + '] {' + statpref + tmp + '}';
end;


{
Deflex()
Generates a list of possible deflected versions of the word.
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

      for j:=0 to flength(roma)-flength(dr.infl) do
        if (fcopy(roma,j+1,flength(dr.infl))=dr.infl) or
           ((dr.infl='KKKK') and (j=0) and (core<>'')) then
        if ((dr.vt<>'K') and (dr.vt<>'I')) or ((dr.vt='K') and (j=0) and ((core='') or (core='6765')))
           or ((dr.vt='I') and (j=0) and ((core='') or (core='884C'))) then
        begin
          if (flength(dr.defl)+j<=6) and (core+fcopy(roma,1,j)+dr.defl<>w) then
          begin
            //ws:=flength(core+fcopy(roma,1,j)+dr.infl);
            //if dr.infl='KKKK'then ws:=flength(core);
            //if ws<=flength(core) then ws:=flength(core)+1;
            //if ws<=1 then ws:=2;
            ws:=flength(w);
            ad:=core+fcopy(roma,1,j)+dr.defl;
            suf:=fcopy(roma,j+1+flength(dr.infl),flength(roma)-j-flength(dr.infl));
            if sl.Find(ws, dr.vt, ad)>=0 then
              continue; //already added
            sufokay:=(suf='');
            for k:=0 to suffixl.Count-1 do
              if (dr.sufcat+suf=suffixl[k]) or ((dr.sufcat='*') and (suffixl[k][1]+suf=suffixl[k])) then
                sufokay:=true;
            if (sufokay or not mustSufokay) then
            begin
              if (sufokay) and (dr.infl<>'KKKK') then
                sl.Add(priordfl, ws, dr.vt, rtNormal, ad)
              else
                sl.Add(1, ws, dr.vt, rtNormal, ad);
            end;
          end;
        end;
    end;
    ws:=flength(w);
    sl.Add(prior, ws, 'F', rtNormal, w);
  end;
  if curlang='c'then
  begin
    sl.Add(prior, ws, 'F', rtNormal, w);
    if pos('?',KanaToRomaji(w,romasys,'c'))>0 then exit;
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
}
procedure TDicSearchRequest.MakeLookupList(se: TCandidateLookupList; search: string; wt: integer);
var _s: string;
  i: integer;
  partfound:boolean;
  every:boolean;
  tmpkana:string;
begin
  case a of
    stJp:
      if not AutoDeflex then begin
       //No autodeflex => exact roma lookup
        search := SignatureFrom(search);
        se.Add(9,length(search),'F',rtRoma,search)
      end else begin
        if curlang='j'then
          tmpkana:=RomajiToKana('H'+search,romasys,'j',[])
        else
          tmpkana:=RomajiToKana(search,romasys,'c',[]);
        if pos('?',tmpkana)>0 then begin //not fully converted => add exact roma first
          search := SignatureFrom(search);
          se.Add(9,length(search),'F',rtRoma,search);
          repl(tmpkana,'?','');
          Deflex(tmpkana,se,6,5,true); //deflex with lower priority since this is probably wrong decoding of roma
        end else
          Deflex(tmpkana,se,9,8,true);
      end;
    stEn:
      se.Add(9, 1, 'F', rtNormal, search);
    stClipboard:
      if AutoDeflex then
        Deflex(ChinFrom(search),se,9,8,true)
      else
        se.Add(9,flength(search),'F',rtNormal,search);
    stEditorInsert,
    stEditorAuto:
      if wt<0 then
      begin
        _s:=ChinFrom(search);
        Deflex(_s,se,9,8,false); //ignores AutoDeflex
      end else
      begin
        if (wt=1) or (wt=2) then
        begin
          _s:=ChinFrom(search);
          if wt=1 then Deflex(_s,se,9,8,false); //ignores AutoDeflex
          for i:=flength(_s) downto 1 do
          begin
            partfound:=false;
            every:=false;
            if EvalChar(fgetch(_s,i))=1 then every:=true;
            if (wt=2) and (i<flength(_s)) and (i>=flength(_s)-1)
            and (partl.IndexOf(fcopy(_s,i+1,flength(_s)-i))>-1) then
              partfound:=true;
            //if ((i<flength(_s)) and every) or (wt=2) then
            if (every) or ((i>1) and (MatchType=mtMatchLeft)) or (i=flength(_s)) or (partfound) then
              se.Add(i, i, 'F', rtNormal, fcopy(_s,1,i));
          end;
        end;
        if (wt=3) then
        begin
          _s:=ChinFrom(search);
          se.Add(9, fLength(_s), 'F', rtNormal, _s);
        end;
      end;
  end;
end;



{
DicSearch()
Don't use if you're doing a lot of searches, use TDicSearchRequest instead.
}
procedure DicSearch(search:string;a:TSearchType; MatchType: TMatchType;
  full:boolean;wt,maxwords:integer;sl:TSearchResults;dictgroup:integer;var wasfull:boolean);
var req: TDicSearchRequest;
begin
  req := TDicSearchRequest.Create;
  try
    req.a := a;
    req.MatchType := MatchType;
    req.full := full;
    req.maxwords := maxwords;
    req.dictgroup := dictgroup;
    req.Prepare;
    req.Search(search, wt, sl);
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

procedure TDicSearchRequest.Prepare;
var di: integer;
  dic: TJaletDic;
  prior: integer;
begin
  if maxwords<10 then maxwords:=10;

  //Destroy existing cursors
  for di := 0 to Length(dics) - 1 do
    FreeAndNil(dics[di].cursor);
  FreeAndNil(CUser);
  FreeAndNil(CUserPrior);

  //Create dictionary cursors
  SetLength(dics, dicts.Count);
  for di:=0 to dicts.Count-1 do begin
    dics[di].cursor := nil;
    dic:=dicts[di];
    if not dic.loaded or not dicts.IsInGroup(dic,dictgroup) then
      continue;

    dic.Demand;
    dics[di].cursor := dic.NewLookup(MatchType);

   //Calculate priority class. Lowest priority gets 20000, highest gets 0
    prior := dicts.Priority.IndexOf(dic.name);
    if prior<0 then
      prior := 20000
    else
      prior := Trunc(20000*(prior/dicts.Priority.Count));
    dics[di].PriorityClass := prior;
  end; //of dict enum

  //Create cached table cursors
  CUser := TTextTableCursor.Create(TUser);
  stUserKanji := TUser.GetSeekObject('Kanji');
  stUserIndex := TUser.GetSeekObject('Index');
  CUserPrior := TTextTableCursor.Create(TUserPrior);
  stUserPriorKanji := TUserPrior.GetSeekObject('Kanji');
  fldUserPriorCount := TUserPrior.Field('Count');
end;

{
Search()
Searches the dictionary for all candidate translations to the line.
search
  string to look for
wt
  WordType
  ???
  -2, -1, [0..4] from EvalChar
sl
  Match results to return.
wasfull
  True if we have retrieved all of the available results.
Note:
- "search" has to be in 4-char-per-symbol hex encoding (=> length divisible by 4)
- Except for mode=2, then "search" must be raw english
}

procedure TDicSearchRequest.Search(search:SatanString; wt: integer; sl:TSearchResults);
var i,di:integer;
begin
  if search='' then exit;
  mess := nil;
  se.Clear;
  presentl.Clear;

  p4reading:=false;
  wasfull:=true;
  nowt:=now;

  MakeLookupList(se, search, wt);

  case a of
    stEditorInsert,
    stEditorAuto: begin
      p4reading:=wt=-1;
      if wt=-1 then if partl.IndexOf(search)>-1 then
        with sl.AddResult^ do begin
          Reset();
          sdef := 'P';
          kana := ChinFrom(search);
          kanji := kana;
          entry := KanaToRomaji(search,1,'j')+' particle';
          slen := Length(kanji);
        end;
    end;
  end;

 { kanaonly:
  If this is set, we're going to convert kanji+kana to ??+romaji and search for that.
  Makes sense only if we're sure our word is all kana, but gives us better kana
  coverage (i.e. hiragana/katakana).
  This is expected, for instance, when handling user input. While we store original
  input as a roma lookup, deflexed lookups are in kana (deflexion happens in kana),
  and without this flag we'd miss words like KATAKANA ROOT + hiragana verb ending.

  Note that if lookup candidates could possibly differ in this regard, we'd have
  to re-check for this property for every candidate. }
  case a of
    stClipboard: begin
      kanaonly:=true;
      for i:=1 to flength(search) do
        if not (EvalChar(fgetch(search,i)) in [EC_HIRAGANA, EC_KATAKANA]) then begin
          kanaonly := false;
          break;
        end;
    end;
    stEditorInsert,
    stEditorAuto:
      kanaonly := p4reading or (wt=2);
  else
    kanaonly := false;
  end;

  if full or (fUser.SpeedButton13.Down and (MatchType<>mtMatchAnywhere)) then //TODO: Move this line out of here
  for di:=0 to Length(dics)-1 do begin
    if dics[di].cursor=nil then continue;
    for i:=0 to se.Count-1 do
      TestLookupCandidate(dics[di], se[i], wt, sl);
  end; //of dict enum

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
  if fSettings.CheckBox5.Checked then begin
    if TestMarkers(mk,#46#46#58#59#60#61#62#63#106) then dec(Result,5);
    if TestMarkers(mk,#66#67#68#69#70#71#72#73#74#75#76#77#78#79#80#81#82#83#84#85
          +#110#111#112#113#114#115#116#117#118) then dec(Result,5);
  end;
  if fSettings.CheckBox6.Checked then begin
    if TestMarkers(mk,#86) then dec(Result,1); //honor
    if TestMarkers(mk,#87) then dec(Result,2); //humor
    if TestMarkers(mk,#88) then dec(Result,3); //humble
  end;
  if TestMarkers(mk,#91) then inc(Result,20); //obsolete
  if TestMarkers(mk,#92) then inc(Result,20); //obscure
  if TestMarkers(mk,#93) then inc(Result,20); //outd-kanji
  if TestMarkers(mk,#94) then inc(Result,20); //outd-kana
  if TestMarkers(mk,#135) then inc(Result,20); //rare

  if fSettings.CheckBox7.Checked and TestMarkers(mk,MarkPop) then dec(Result,150); //pop
end;

procedure TDicSearchRequest.TestLookupCandidate(ds: TDicSetup; lc: PCandidateLookup;
  wt: integer; sl: TSearchResults);
var
  dic: TDicLookupCursor;
  i: integer;

  sxx: string;  //==lc.str
  sxxr: string; //same in romaji
  sp:integer;   //==lc.priority
  sdef:char;    //==lc.verbType
  slen:integer; //==lc.len

  raw_entries: TEntries;
  entry:string; //translation entry text
  converted:string;
  markers,kmarkers:TMarkers;
  popclas:integer;
  UserScore:integer;
  UserIndex:integer;
  sort:integer;
  freq:integer;
  sorts:string;
  statpref:string;
  ssig:string; //translation signature (reading x kanji) to merge duplicates
  scomp:PSearchResult;
  scur:TSearchResult;

  existingIdx: integer;

 //Used several times with different kanji_vals
  procedure TryGetUserScore(kanji_val: string);
  begin
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

begin
  dic := ds.cursor;
  sxx:=lc.str;
  if sxx='' then exit;
  sp:=lc.priority;
  sdef:=lc.verbType;
  slen:=lc.len;

  if a in [stJp,stEn] then slen:=1;

 //Initial lookup
 { KanaToRomaji is VERY expensive so let's only call it when really needed }
  case a of
    stJp: begin
      if lc.roma=rtRoma then
        sxxr:=sxx
      else
        sxxr:=KanaToRomaji(sxx,1,curlang);
      dic.LookupRomaji(sxxr);
    end;
    stEn: dic.LookupMeaning(sxx);
    stClipboard,
    stEditorInsert,
    stEditorAuto:
      if lc.roma=rtRoma then
        dic.LookupRomaji(lc.str)
      else
      if kanaonly then begin
        sxxr:=KanaToRomaji(sxx,1,curlang);
        dic.LookupRomaji(sxxr);
      end else
        dic.LookupKanji(sxx);
  end;

  i:=0;

  while dic.HaveMatch do begin
    if (mess=nil) and (now-nowt>1/24/60/60) then
      mess:=SMMessageDlg(_l(sDicSearchTitle), _l(sDicSearchText));
    if sdef<>'F' then entry:=ALTCH_TILDE+'I'else entry:=ALTCH_TILDE+'F';

    raw_entries:=dic.GetEntries;
    markers:=raw_entries.MergeMarkers;
    kmarkers:=dic.GetKanjiKanaMarkers;
    entry:=entry+raw_entries.ToEnrichedString;

    if IsAppropriateVerbType(sdef, markers) then
    if (not dic_ignorekana) or (not (a in [stEditorInsert,stEditorAuto]))
      or (not kanaonly) or (pos(UH_LBEG+'skana'+UH_LEND,entry)>0) then
    begin

     //Calculate popularity class
      popclas := GetPopClass(markers)+GetPopClass(kmarkers);
      //if ((a=1) or ((a=4) and (p4reading))) and (dic.Field('Priority')<>-1) then
      //  if dic.Int(dic.Field('Priority'))>9 then
      //    inc(popclas,90)
      //  else
      //    inc(popclas,dic.Int(dic.Field('Priority'))*10);
      if (a in [stEditorInsert, stEditorAuto]) and (p4reading)
      and CUserPrior.Locate(@stUserPriorKanji,dic.GetKanji) then
        dec(popclas,10*CUserPrior.Int(fldUserPriorCount));

      //CUser.SetOrder('Kanji_Ind');

      UserScore:=-1;
      UserIndex:=0;
      if pos(UH_LBEG+'skana'+UH_LEND,entry)>0 then
        TryGetUserScore(dic.GetPhonetic);
      TryGetUserScore(dic.GetKanji);
      if (UserScore=-1) and (dic.GetKanji<>ChinTo(dic.GetKanji)) then
        TryGetUserScore(ChinTo(dic.GetKanji));

      if (fSettings.CheckBox58.Checked) and (dic.GetFrequency>-1) and (dic.GetFrequency>0) then
        entry:=entry+' '+UH_LBEG+'pwc'+IntToStr(dic.GetFrequency)+UH_LEND;
      entry:=entry+' '+UH_LBEG+'d'+dic.dic.name+UH_LEND;

     //Calculate sorting order
      sort:=0; //the bigger the worse (will apear later in list)
      if a=stEn then
      begin
        if pos(trim(uppercase(sxx)),trim(uppercase(dic.GetArticleBody)))=1 then sort:=10000 else sort:=11000;
        sort:=sort+popclas*100;
      end;
      if a=stJp then sort:=(10000*(9-min(sp,9)))+length(dic.GetPhonetic)*1000+popclas*10;
      if a in [stClipboard, stEditorInsert, stEditorAuto] then sort:=(10000*(9-min(sp,9)))+popclas*10;
      sort:=sort+10000;
      //if (a in [stEditorInsert, stEditorAuto]) and (p4reading) then sort:=10000+popclas*10;
      if (fSettings.CheckBox4.Checked) and (UserScore>-1) then dec(sort,1000);
      if (fSettings.CheckBox4.Checked) and (UserScore>1) then dec(sort,1000);
      if IsKanaCharKatakana(dic.GetPhonetic, 1) then inc(sort,1000);
      sort:=sort+dic.dic.priority*20000;
      sort:=sort+ds.PriorityClass;
      if (fSettings.CheckBox59.Checked) then
      begin
        if dic.GetFrequency>-1 then
        begin
          freq:=dic.GetFrequency;
          if freq>=500000 then freq:=10000 else
          if freq>=10000 then freq:=2000+(freq-10000) div 100 else
          if freq>=1000 then freq:=1000+(freq-1000) div 10;
        end else
          freq:=0;
        sort:=sort+10000-freq;
      end;

      if (a in [stEditorInsert, stEditorAuto]) and (p4reading) then
        entry:=copy(entry,1,2)+UH_LBEG+'pp'+inttostr(sort div 100)+UH_LEND+' '+copy(entry,3,length(entry)-2);

     //Fill in current result
      scur.Reset;
      scur.score := sort;
      scur.userindex := UserIndex;
      if fSettings.CheckBox11.Checked then
        scur.userscore := UserScore
      else
        scur.userscore := -1;
      scur.dicindex := dic.GetIndex;
      scur.dicname := dic.dic.name;
      scur.slen := slen;
     // if sdef<>'F'then scur.sdef:='I' else scur.sdef:='F';
      if (pos('-v'+UH_LEND,entry)>0) then
        scur.sdef:='I'
      else
        scur.sdef:='F';
      scur.kana := dic.GetPhonetic;
      if (fSettings.CheckBox8.Checked) and (pos(UH_LBEG+'skana'+UH_LEND,entry)<>0) then
        scur.kanji := scur.kana
      else
        scur.kanji := CheckKnownKanji(ChinTo(dic.GetKanji));
      scur.entry := entry;

     //result signature (reading x kanji)
      ssig:=dic.GetPhonetic+'x'+dic.GetKanji;
     //if we already have that result, only upgrade it (lower it's sorting order, add translations)
      existingIdx := presentl.IndexOf(ssig);
      if existingIdx>=0 then
      begin
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
        //add tl
        if pos(copy(entry,3,length(entry)-2),scomp.entry)=0 then begin
         //for now we add everything to the end, ignoring what result had higher score
         //if we ever care about that, we also have to replace DicName and WordIndex in the header
          if (length(entry)>0) and (entry[1]=ALTCH_TILDE) then delete(entry,1,2);
          scomp.entry:=scomp.entry+' / '+entry;
        end;
      end else
      begin
        existingIdx := sl.Add(scur);
        presentl.AddObject(ssig, TObject(existingIdx));
      end;
      inc(i);
    end;

    if (not full) and (i>=maxwords) then
    begin
      wasfull:=false;
      break;
    end;

    dic.NextMatch;
  end;
end;

procedure TDicSearchRequest.FinalizeResults(sl: TSearchResults);
var i: integer;
  scomp: PSearchResult;
  ex_pos: integer;
  s2: string;
  sl2: TStringList;
  sl2i: integer;
begin
  if sl.Count<=1 then exit; //apparently that's the most common case when translating

  sl.Sort;

 //Add user entries to the beginning
  for i:=0 to sl.Count-1 do
    if sl[i].userindex<>0 then
    begin
      CUser.Locate(@stUserIndex,sl[i].userindex);
      scomp:=sl[i];
      s2:=FixVocabEntry(CUser.Str(TUserEnglish));
      ex_pos := pos(s2,scomp.entry);

     //If there's already this article in the list, cut it
      if ex_pos>0 then
        scomp.entry := copy(scomp.entry,1,ex_pos-1) + copy(scomp.entry,ex_pos+Length(s2),length(scomp.entry)-length(s2)-ex_pos+1);

     //Delete ~F/~I word type since it would be ignored by painting anyway as it stands
     //And it would be visible.
      if (length(scomp.entry)>0) and (scomp.entry[1]=ALTCH_TILDE) then delete(scomp.entry,1,2);

      sl2:=TStringList.Create;
      ListWordCategories(CUser.Int(TUserIndex),sl2);
      for sl2i:=0 to sl2.Count-1 do s2:=s2+' '+UH_LBEG+'l'+copy(sl2[sl2i],3,length(sl2[sl2i])-2)+UH_LEND;
      sl2.Free;

      scomp.entry := s2+' / '+scomp.entry;
    end;
end;

end.
