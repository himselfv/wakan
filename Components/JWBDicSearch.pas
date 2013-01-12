unit JWBDicSearch;
{
Dictionary engine.
Has a lot of dark legacy in code form.
}

interface
uses SysUtils, Classes, JWBStrings, JWBUtils, TextTable, MemSource, StdPrompt,
  JWBDic;

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
    dic_ignorekana: boolean;
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
    a3kana:boolean;   //a==3 and only kana in string
    a4limitkana:boolean; //a in [4,5]
    procedure FinalizeResults(sl: TSearchResults);

  end;

{
I don't know where to put this so I'm placing it here.
This is the code that attaches annotations for every search result.
It clearly doesn't belong here in dictionary search unit. It should be moved
to wherever the results are handled.

In TDicSearchRequest:

    CAnnot: TAnnotationCursor;

In TDicSearchRequest.Prepare:
  if HaveAnnotations() then
    CAnnot := TAnnotationCursor.Create(TAnnots)
  else
    CAnnot := nil;

On preparing another result:

        if CAnnot<>nil then
        begin
          CAnnot.SeekK(dic.Str(dic.TDictKanji),dic.Str(dic.TDictPhonetic));
          s2:=CAnnot.GetAll('t',', ');
          if CAnnot.GetOne('c')<>'' then s2:=UH_SETCOLOR+CAnnot.GetOne('c')+s2;
          ii:=pos('{'+statpref,scur.entry)+length(statpref)+1;
          if scur[ii]=ALTCH_EXCL then inc(ii,2);
          if scur[ii]=ALTCH_TILDE then inc(ii,2);
          if scur[ii]=ALTCH_EXCL then inc(ii,2);
          if s2<>'' then
            scur:=copy(scur,1,ii-1)+
              s2+' >> '+
              copy(scur,ii,length(scur)-ii+1);
        end;
}


//Compability
procedure DicSearch(search:string;a:TSearchType; MatchType: TMatchType;
  full:boolean;wt,maxwords:integer;sl:TSearchResults;dictgroup:integer;var wasfull:boolean);

procedure Deflex(const w:string;sl:TCandidateLookupList;prior,priordfl:byte;mustsufokay,alwaysdeflect:boolean);

implementation
uses Forms, Windows, JWBMenu, JWBUnit, JWBUser, JWBSettings, JWBWords, Math,
  JWBCategories, JWBEdictMarkers;


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
begin
  if UserScore>=0 then
    statpref:=ALTCH_EXCL+inttostr(UserScore)
  else
    statpref:='';
  Result := statpref + kanji + ' [' + statpref + kana + '] {' + statpref + entry + '}';
end;


{
Deflection and lookup lists
}

procedure Deflex(const w:string;sl:TCandidateLookupList;prior,priordfl:byte;mustsufokay,alwaysdeflect:boolean);
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
  if prior>9 then prior:=9;
  if priordfl>9 then priordfl:=9;
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
    if (fUser.SpeedButton4.Down) or (alwaysdeflect) then
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
//              ws:=flength(core+fcopy(roma,1,j)+dr.infl);
//              if dr.infl='KKKK'then ws:=flength(core);
//              if ws<=flength(core) then ws:=flength(core)+1;
//              if ws<=1 then ws:=2;
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
                if (sufokay) and (dr.infl<>'KKKK') then sl.Add(priordfl, ws, dr.vt, ad) else sl.Add(1, ws, dr.vt, ad);
              end;
            end;
          end;
      end;
    ws:=flength(w);
    sl.Add(prior, ws, 'F', w);
  end;
  if curlang='c'then
  begin
   //TODO: Convert to unicode
    sl.Add(prior, ws, 'F', w);
    if pos('?',KanaToRomaji(w,romasys,'c'))>0 then exit;
    repeat
      pass:=true;
      i:=0;
      while i<sl.Count do
      begin
        ct := sl[i]^;
        j := pos('F030',ct.str);
        if j>0 then begin
          pass:=false;
         //First version is modified in-place to avoid slow deletions
          sl[i]^.str[j] := '1';
         //Next versions are made into copies
          ct.str[j]:='2';
          sl.Add(ct);
          ct.str[j]:='3';
          sl.Add(ct);
          ct.str[j]:='4';
          sl.Add(ct);
          ct.str[j]:='5';
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
begin
  case a of
    stJp:
      if curlang='j'then
        Deflex(RomajiToKana('H'+search,romasys,true,'j'),se,9,8,true,false)
      else
        Deflex(RomajiToKana(search,romasys,true,'c'),se,9,8,true,false);
    stEn:
      se.Add(9, 1, 'F', search);
    stClipboard:
      Deflex(ChinFrom(search),se,9,8,true,false);
    stEditorInsert,
    stEditorAuto:
      if wt<0 then
      begin
        _s:=ChinFrom(search);
        Deflex(_s,se,9,8,false,true);
      end else
      begin
        if (wt=1) or (wt=2) then
        begin
          _s:=ChinFrom(search);
          if wt=1 then Deflex(_s,se,9,8,false,true);
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
              se.Add(i, i, 'F', fcopy(_s,1,i));
          end;
        end;
        if (wt=3) then
        begin
          _s:=ChinFrom(search);
          se.Add(9, fLength(_s), 'F', _s);
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

 //I don't know WHY the following line is needed,
 //but it does exactly what it did in old Wakan. Whatever that is.  
  if full or (fUser.SpeedButton13.Down and (MatchType<>mtMatchAnywhere)) then
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
  if a = stClipboard then begin
    a3kana:=true;
    for i:=1 to flength(sxx) do
      if not (EvalChar(fgetch(sxx,i)) in [EC_HIRAGANA, EC_KATAKANA]) then begin
        a3kana := false;
        break;
      end;
  end;

 //stEditorInsert/stEditorAuto:
 //If this is set, we're going to convert kanji+kana to ??+romaji and search for that.
 //Makes sense only if we're sure our word is all kana.
  a4limitkana:=(a in [stEditorInsert, stEditorAuto]) and (p4reading or (wt=2));

 //Initial lookup
 { KanaToRomaji is VERY expensive so let's only call it when really needed }
  case a of
    stJp: begin
      sxxr:=KanaToRomaji(sxx,1,curlang);
      dic.LookupRomaji(sxxr);
    end;
    stEn: dic.LookupMeaning(sxx);
    stClipboard:
      if a3kana then begin
        sxxr:=KanaToRomaji(sxx,1,curlang);
        dic.LookupRomaji(sxxr)
      end else
        dic.LookupKanji(sxx);
    stEditorInsert,
    stEditorAuto:
      if a4limitkana then begin
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
    if (not dic_ignorekana) or (not a4limitkana) or (pos(UH_LBEG+'skana'+UH_LEND,entry)>0) then
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
      s2:=CUser.Str(TUserEnglish);
      ex_pos := pos(s2,scomp.entry);

     //If there's already this article in the list, cut it
      if ex_pos>0 then
        scomp.entry := copy(scomp.entry,1,ex_pos-1) + copy(scomp.entry,ex_pos+Length(s2),length(scomp.entry)-length(s2)-ex_pos+1);

      s2:=FixVocabEntry(s2);

     //Delete ~F/~I word type since it would be ignored by painting anyway as it stands
     //And it would be visible.
      if (length(scomp.entry)>0) and (scomp.entry[1]=ALTCH_TILDE) then delete(scomp.entry,1,2);

      sl2:=TStringList.Create;
      ListWordCategories(CUser.Int(TUserIndex),sl2);
      for sl2i:=0 to sl2.Count-1 do s2:=s2+' '+UH_LBEG+'l'+copy(sl2[sl2i],3,length(sl2[sl2i])-2)+UH_LEND;
      sl2.Free;

      scomp.entry := s2+' // '+scomp.entry;
    end;
end;

end.
