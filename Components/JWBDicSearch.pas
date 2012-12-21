unit JWBDicSearch;
{
Dictionary engine.
Has a lot of dark legacy in code form.
}

interface
uses SysUtils, Classes, JWBStrings, JWBUtils, TextTable, MemSource, StdPrompt,
  JWBAnnotations;

//See comments to TextTable.CURSOR_IN_TABLE
{$DEFINE DIC_CURSOR_IN_TABLE}

type
 { Dictionary group. Presently 5 are supported:
    1..3 - user dictionary groups
    4 - use for compounds
    5 - use for popup/editor }
  TDictGroup = integer;

  TDicCursor = class;

  TJaletDic = class
  public
    TDict:TTextTable;
    TDictIndex,
    TDictEnglish,
    TDictKanji,
    TDictPhonetic,
    TDictSort,
    TDictMarkers,
    TDictFrequency:integer;
    charidx,wordidx:pointer;
    charno,wordno:integer;
    charfsiz,wordfsiz:integer;
    package:TPackageSource;
    loaded:boolean;
    pname:string;
    version:string;
    dicver:integer;
    priority:integer;
    builddate:TDateTime;
    name,description:string;
    language:char;
    tested:boolean;
    entries:integer;
    copyright:string;
    demandloaded:boolean;
    vocmode:integer;
   {$IFDEF DIC_CURSOR_IN_TABLE}
    _intcur: TDicCursor;
   {$ENDIF}
    constructor Create;
    destructor Destroy; override;
    procedure FillInfo(packagefile:string); virtual;
    procedure Build(edictfile,packagefile:string); virtual;
    procedure Load; virtual;
    procedure Unload; virtual;
    procedure Demand;
    function ReadIndexString(word:boolean;loc:integer):string;
    function ReadIndexInfo(word:boolean;loc:integer):integer;
   {$IFDEF DIC_CURSOR_IN_TABLE}
    procedure FindIndexString(word:boolean;locator:string); {$IFDEF INLINE}inline;{$ENDIF}
    function ReadIndex:integer; {$IFDEF INLINE}inline;{$ENDIF}
   {$ENDIF}

  protected
   { To speed up access to certain seek tables we're going to keep pre-calculated
    references here. }
    FstSort: TSeekObject;
    FstSortReverse: TSeekObject;
    FstKanji: TSeekObject;
    FstKanjiReverse: TSeekObject;
    FstIndex: TSeekObject;
   { In case you need a seek table not specified here, you can get it from
    TDict.GetSeekObject('seek name') }
    procedure SetupSeekObjects;
  public
   { Public pointers. Pass these to Locate and have no care in life. }
    stSort: PSeekObject;
    stSortReverse: PSeekObject;
    stKanji: PSeekObject;
    stKanjiReverse: PSeekObject;
    stIndex: PSeekObject;

  end;

  TDicCursor = class(TTextTableCursor)
  public
    dic: TJaletDic;
   { Copied from TJaletDic on Create() for code readability }
    TDictIndex,
    TDictEnglish,
    TDictKanji,
    TDictPhonetic,
    TDictSort,
    TDictMarkers,
    TDictFrequency:integer;
    stSort: PSeekObject;
    stSortReverse: PSeekObject;
    stKanji: PSeekObject;
    stKanjiReverse: PSeekObject;
    stIndex: PSeekObject;
    constructor Create(ADic: TJaletDic);

  protected
   { JaletDic cursor -- independent from basic cursor functionality }
    indexword:boolean;
    indexfrom,indexto:integer;
  public
    procedure FindIndexString(word:boolean;locator:string);
    function ReadIndex:integer;

  end;


type
 { How to match words (exact, match left, right or anywhere) }
  TMatchType = (
    mtExactMatch,
    mtMatchLeft,
    mtMatchRight,
    mtMatchAnywhere);

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
    dics: array of TDicCursor; //empty field => dic was not loaded, skip
    //Cached cursors and seeks for User tables
    CUser: TTextTableCursor;
    stUserKanji: TSeekObject;
    stUserIndex: TSeekObject;
    CUserPrior: TTextTableCursor;
    stUserPriorKanji: TSeekObject;
    fldUserPriorCount: integer;
    CAnnot: TAnnotationCursor;
  public
    procedure Search(search:SatanString; wt: integer; sl: TStringList);

  public //Output
    WasFull: boolean;

  protected
    procedure MakeLookupList(se: TCandidateLookupList; search: string; wt: integer);

  protected
    nowt:TDateTime; //Search start time
    mess: TSMPromptForm; //"Please wait" form, created if the search takes too long
    se: TCandidateLookupList; //Lookup candidates -- see comments where this type is declared
    presentl,presindl:TStringList;
    p4reading:boolean;
    procedure TestLookupCandidate(dic: TDicCursor; lc: PCandidateLookup; wt: integer;
      sl: TStringList);

  protected
    a3kana:boolean;   //a==3 and only kana in string
    a4limitkana:boolean; //a in [4,5]
    procedure DicInitialLookup(dic: TDicCursor; wt: integer; sxxr: string; sxx: string);
    procedure SortResults(sl: TStringList);

  end;

//Compability
procedure DicSearch(search:string;a:TSearchType; MatchType: TMatchType;
  full:boolean;wt,maxwords:integer;sl:TStringList;dictgroup:integer;var wasfull:boolean);

procedure Deflex(const w:string;sl:TCandidateLookupList;prior,priordfl:byte;mustsufokay,alwaysdeflect:boolean);

implementation
uses Forms, Windows, JWBMenu, JWBUnit, JWBUser, JWBSettings, JWBWords, Math,
  JWBCategories;



{
Utility functions
}

function CompareUnicode(sub,str:FString):boolean;
var i:integer;
begin
  result:=false;
  for i:=0 to flength(str)-flength(sub) do
    if fcopy(str,i+1,flength(sub))=sub then
    begin
      result:=true;
      exit;
    end;
end;


{
Dictionary
}
constructor TJaletDic.Create;
begin
 {$IFDEF DIC_CURSOR_IN_TABLE}
  _intcur := nil;
  //We don't create cursor here because the table needs to be already loaded for that
 {$ENDIF}
  tested:=false;
  loaded:=false;
end;

destructor TJaletDic.Destroy;
begin
  if loaded then Unload;
 {$IFDEF DIC_CURSOR_IN_TABLE}
  FreeAndNil(_intcur);
 {$ENDIF}
end;

procedure TJaletDic.FillInfo(packagefile:string);
var ps:TPackageSource;
    err:string;
    vs:TStringList;
begin
  tested:=false;
  pname:=packagefile;
  err:='';
  vs:=TStringList.Create;
  try
    ps:=TPackageSource.Create(pname,791564,978132,978123);
    if ps.GetFileList.IndexOf('dict.ver')<>-1 then
    begin
      vs.LoadFromStream(ps.Files['dict.ver'].Lock);
      ps.Files['dict.ver'].Unlock;
      if vs[0]<>'DICT'then err:='Invalid DIC header'else
      begin
        dicver:=strtoint(vs[1]);
        if dicver>CurDicVer then err:='Unsupported DIC version'else
        if (dicver<CurDicVer) and ((CurDicVer<>4) or (dicver<>3)) then err:='Outdated DIC structure - please download new DIC file'else
        if (dicver<CurDicVer) then err:='DIC indexes corrupted - bug in 1.47 DIC, you have to download this DIC anew (for 1.50+), I''m sorry for inconvenience'else
        begin
          builddate:=strtoint(vs[2]);
          version:=vs[3];
          name:=vs[4];
          language:=vs[5][1];
          description:=vs[6];
          priority:=strtoint(vs[7]);
          entries:=strtoint(vs[8]);
          copyright:=vs[9];
          tested:=true;
        end;
      end;
    end else err:='Unknown file structure';
    ps.Free;
  except
    err:=(ExceptObject as Exception).Message;
  end;
  if err<>'' then
    Application.MessageBox(
      pchar(_l('#00321^eCannot register dictionary ')+pname+#13#13+err),
      pchar(_l('#00020^eError')),
      MB_ICONERROR or MB_OK);
end;

procedure TJaletDic.Build(edictfile,packagefile:string);
var err:string;
begin
  pname:=packagefile;
  err:='Building not supported yet.';
  if err<>'' then
    Application.MessageBox(
      pchar(_l('#00322^eCannot build dictionary ')+pname+#13#13+err),
      pchar(_l('#00020^eError')),
      MB_ICONERROR or MB_OK);
end;

procedure TJaletDic.Load;
begin
  if loaded then Unload;
  demandloaded:=false;
  loaded:=true;
  if not fSettings.CheckBox49.Checked then Demand;
end;

procedure TJaletDic.Demand;
var pd:TSMPromptForm;
    mf:TMemoryFile;
begin
  if demandloaded then exit;
  pd:=SMMessageDlg(
    _l('#00323^eDictionary loading'),
    _l('#00324^eLoading dictionary ')+name+'...');
  try
    package:=TPackageSource.Create(pname,791564,978132,978123);
    TDict:=TTextTable.Create(package,'Dict',true,pos(','+name,OfflineDicts)<>0);
    TDictIndex:=TDict.Field('Index');
    TDictEnglish:=TDict.Field('English');
    TDictKanji:=TDict.Field('Kanji');
    TDictPhonetic:=TDict.Field('Phonetic');
    TDictEnglish:=TDict.Field('English');
    TDictSort:=TDict.Field('Sort');
    TDictMarkers:=TDict.Field('Markers');
    TDictFrequency:=TDict.Field('Frequency');
    WordIdx:=nil;
    CharIdx:=nil;
    mf:=package['WordIdx.bin'];
    if mf<>nil then
    begin
      GetMem(WordIdx,mf.Size-4);
      package.ReadRawData(WordNo,integer(mf.Position),4);
      package.ReadRawData(WordIdx^,integer(mf.Position)+4,mf.Size-4);
      wordfsiz:=(mf.Size div 4)-1;
    end;
    mf:=package['CharIdx.bin'];
    if mf<>nil then
    begin
      GetMem(CharIdx,mf.Size-4);
      package.ReadRawData(CharNo,integer(mf.Position),4);
      package.ReadRawData(CharIdx^,integer(mf.Position)+4,mf.Size-4);
      charfsiz:=(mf.Size div 4)-1;
    end;
    SetupSeekObjects;
  except
    Application.MessageBox(
      pchar(_l('#00325^eCannot load dictionary ')+pname+#13#13+(ExceptObject as Exception).Message),
      pchar(_l('#00020^eError')),
      MB_ICONERROR or MB_OK);
  end;
 {$IFDEF DIC_CURSOR_IN_TABLE}
  _intcur:=TDicCursor.Create(Self);
 {$ENDIF}
  demandloaded:=true;
  pd.Free;
end;

procedure TJaletDic.Unload;
begin
  if not demandloaded then exit;
  TDict.Free;
  package.Free;
  if WordIdx<>nil then FreeMem(WordIdx);
  if CharIdx<>nil then FreeMem(CharIdx);
  loaded:=false;
end;

procedure TJaletDic.SetupSeekObjects;
begin
  FstSort := TDict.GetSeekObject('Sort');
  FstSortReverse := TDict.GetSeekObject('<Sort');
  FstKanji := TDict.GetSeekObject('Kanji');
  FstKanjiReverse := TDict.GetSeekObject('<Kanji');
  FstIndex := TDict.GetSeekObject('Index');

  stSort := @FstSort;
  stSortReverse := @FstSortReverse;
  stKanji := @FstKanji;
  stKanjiReverse := @FstKanjiReverse;
  stIndex := @FstIndex;
end;

function TJaletDic.ReadIndexInfo(word:boolean;loc:integer):integer;
begin
  if word then
    Result := PInteger(integer(wordidx)+loc*4)^
  else
    Result := PInteger(integer(charidx)+loc*4)^;
end;

function TJaletDic.ReadIndexString(word:boolean;loc:integer):string;
var l:array[0..3] of AnsiChar;
  p:PAnsiChar;
begin
  if word then p:=wordidx else p:=charidx;
  p:=p+loc*4;
  move(p^,l,4);
  result:=l;
end;

{$IFDEF DIC_CURSOR_IN_TABLE}
procedure TJaletDic.FindIndexString(word:boolean;locator:string);
begin
  _intcur.FindIndexString(word,locator);
end;

function TJaletDic.ReadIndex:integer;
begin
  Result := _intcur.ReadIndex;
end;
{$ENDIF}


{
Cursor
}
constructor TDicCursor.Create(ADic: TJaletDic);
begin
  inherited Create(ADic.TDict);
  self.dic := ADic;
  self.TDictIndex := ADic.TDictIndex;
  self.TDictEnglish := ADic.TDictEnglish;
  self.TDictKanji := ADic.TDictKanji;
  self.TDictPhonetic := ADic.TDictPhonetic;
  self.TDictSort := ADic.TDictSort;
  self.TDictMarkers := ADic.TDictMarkers;
  self.TDictFrequency := ADic.TDictFrequency;
  self.stSort := ADic.stSort;
  self.stSortReverse := ADic.stSortReverse;
  self.stKanji := ADic.stKanji;
  self.stKanjiReverse := ADic.stKanjiReverse;
  self.stIndex := ADic.stIndex;
end;

procedure TDicCursor.FindIndexString(word:boolean;locator:string);
var l,r,m,max:integer;
    s,s2:string;
begin
  l:=0;
  if word then max:=dic.wordno-1 else max:=dic.charno-1;
  r:=max;
  s:=locator;
  s2:='';
  while (pos(' ',s)>0) and (s2='') do
  begin
    s2:=System.copy(s,1,pos(' ',s)-1);
    System.delete(s,1,pos(' ',s));
    if ignorel.IndexOf(s2)<>-1 then s2:='';
  end;
  if s2='' then s2:=s;
  locator:=s;
  if length(locator)>4 then locator:=copy(locator,1,4);
  while length(locator)<4 do locator:=locator+' ';
  while l<=r do
  begin
    m:=l+(r-l) div 2;
    s2:=dic.ReadIndexString(word,m*2);
    if s2=locator then break;
    if s2<locator then l:=m+1 else r:=m-1;
  end;
  if l>r then indexfrom:=0 else
  begin
    indexfrom:=dic.ReadIndexInfo(word,m*2+1)+(max+1)*2;
    if m<max then
      indexto:=dic.ReadIndexInfo(word,m*2+3)+(max+1)*2
    else
      if word then indexto:=dic.wordfsiz else indexto:=dic.charfsiz;
    indexword:=word;
  end;
end;

function TDicCursor.ReadIndex:integer;
begin
  if (indexfrom=0) or (indexfrom>=indexto) then
  begin
    result:=0;
    exit;
  end;
  result:=dic.ReadIndexInfo(indexword,indexfrom);
  inc(indexfrom);
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


//Returns the base popularity class for a record (the lower the better)
function GetPopClass(s2: string): integer;
begin
  Result := 40;
  if (fSettings.CheckBox5.Checked) and (pos(UH_LBEG+'gn',s2)>0) then dec(Result,5);
  if (fSettings.CheckBox5.Checked) and (pos('-v'+UH_LEND,s2)>0) then dec(Result,5);
  if (fSettings.CheckBox5.Checked) and (pos(UH_LBEG+'g',s2)=0) then dec(Result,5);
  if (fSettings.CheckBox6.Checked) and (pos(UH_LBEG+'shonor'+UH_LEND,s2)>0) then dec(Result,1);
  if (fSettings.CheckBox6.Checked) and (pos(UH_LBEG+'shum'+UH_LEND,s2)>0) then dec(Result,2);
  if (fSettings.CheckBox6.Checked) and (pos(UH_LBEG+'spolite'+UH_LEND,s2)>0) then dec(Result,3);
  if pos(UH_LBEG+'sobsolete'+UH_LEND,s2)>0 then inc(Result,20);
  if pos(UH_LBEG+'sobscure'+UH_LEND,s2)>0 then inc(Result,20);
  if (fSettings.CheckBox7.Checked) and (pos(UH_LBEG+'spop'+UH_LEND,s2)>0) then dec(Result,150);
end;



{
DicSearch()
Don't use if you're doing a lot of searches, use TDicSearchRequest instead.
}
procedure DicSearch(search:string;a:TSearchType; MatchType: TMatchType;
  full:boolean;wt,maxwords:integer;sl:TStringList;dictgroup:integer;var wasfull:boolean);
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
  presindl:=TStringList.Create;
  SetLength(dics, 0);
end;

destructor TDicSearchRequest.Destroy;
var di: integer;
begin
  //Destroy cursors
  for di := 0 to Length(dics) - 1 do
    FreeAndNil(dics[di]);
  se.Destroy;
  presentl.Destroy;
  presindl.Destroy;
  inherited;
end;

procedure TDicSearchRequest.Prepare;
var di: integer;
  dic: TJaletDic;
begin
  if maxwords<10 then maxwords:=10;

  //Destroy existing cursors
  for di := 0 to Length(dics) - 1 do
    FreeAndNil(dics[di]);
  FreeAndNil(CUser);
  FreeAndNil(CUserPrior);

  //Create dictionary cursors
  SetLength(dics, dicts.Count);
  for di:=0 to dicts.Count-1 do begin
    dics[di] := nil;
    if not (dicts.Objects[di] as TJaletDic).loaded then continue;

    dic:=dicts.Objects[di] as TJaletDic;
    if pos(','+dic.name,NotGroupDicts[dictgroup])<>0 then
      continue; //dic excluded from this dictgroup

    dic.Demand;
    dics[di] := TDicCursor.Create(dic);
    dics[di].dic := dic;
  end; //of dict enum

  //Create cached table cursors
  CUser := TTextTableCursor.Create(TUser);
  stUserKanji := TUser.GetSeekObject('Kanji');
  stUserIndex := TUser.GetSeekObject('Index');
  CUserPrior := TTextTableCursor.Create(TUserPrior);
  stUserPriorKanji := TUserPrior.GetSeekObject('Kanji');
  fldUserPriorCount := TUserPrior.Field('Count');
  if HaveAnnotations() then
    CAnnot := TAnnotationCursor.Create(TAnnots)
  else
    CAnnot := nil;
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

procedure TDicSearchRequest.Search(search:SatanString; wt: integer; sl:TStringList);
var i:integer;
    di:integer;
    _s:string;
begin
  if search='' then exit;
  mess := nil;
  se.Clear;
  presentl.Clear;
  presindl.Clear;

  p4reading:=false;
  wasfull:=true;
  nowt:=now;
  presentl.Sorted:=true;

  MakeLookupList(se, search, wt);

  case a of
    stEditorInsert,
    stEditorAuto: begin
      p4reading:=wt=-1;
      if wt=-1 then if partl.IndexOf(search)>-1 then begin
        _s := ChinFrom(search);
        sl.Add('000000000000000000'+inttostr(flength(search))+'P          '
          +_s+' ['+_s+'] {'+KanaToRomaji(search,1,'j')+' particle}');
      end;
    end;
  end;

 //I don't know WHY the following line is needed,
 //but it does exactly what it did in old Wakan. Whatever that is.  
  if full or (fUser.SpeedButton13.Down and (MatchType<>mtMatchAnywhere)) then
  for di:=0 to Length(dics)-1 do begin
    if dics[di]=nil then continue;

    case a of
      stJp:
        if MatchType=mtMatchRight then dics[di].SetOrder('<Phonetic_Ind') else dics[di].SetOrder('Phonetic_Ind');
      stClipboard:
        if MatchType=mtMatchRight then dics[di].SetOrder('<Kanji_Ind') else dics[di].SetOrder('Kanji_Ind');
      stEditorInsert,
      stEditorAuto:
        dics[di].SetOrder('Kanji_Ind');
    end;

    for i:=0 to se.Count-1 do
      TestLookupCandidate(dics[di], se[i], wt, sl);

  end; //of dict enum

  SortResults(sl);

  mess.Free;
end;

resourcestring
  sDicSearchTitle='#00932^eDic.search';
  sDicSearchText='#00933^ePlease wait. Searching dictionary...';

procedure TDicSearchRequest.DicInitialLookup(dic: TDicCursor; wt: integer; sxxr: string; sxx: string);
begin
  case a of
  stJp:
    case MatchType of
      mtMatchAnywhere: begin end; //nothing
      mtMatchRight: dic.Locate(dic.stSortReverse,sxxr,false);
    else
      dic.Locate(dic.stSort,sxxr,false);
    end;
  stEn:
    dic.FindIndexString(true,lowercase(sxx));
  stClipboard:
    if a3kana then
      case MatchType of
        mtMatchAnywhere: begin end;
        mtMatchRight: dic.Locate(dic.stSortReverse,sxxr,false);
      else
        dic.Locate(dic.stSort,sxxr,false);
      end
    else
      case MatchType of
        mtMatchAnywhere: begin end;
        mtMatchRight: dic.Locate(dic.stKanjiReverse,sxx,false);
      else
        dic.Locate(dic.stKanji,sxx,false);
      end;
  stEditorInsert,
  stEditorAuto:
    if a4limitkana then begin
      dic.SetOrder('Phonetic_Ind');
      dic.Locate(dic.stSort,sxxr,false);
    end else
    begin
      dic.SetOrder('Kanji_Ind');
      dic.Locate(dic.stKanji,sxx,false);
    end;
  end;
end;

function IsAppropriateVerbType(sdef: string; s2: string): boolean;
begin
  Result :=
       ((sdef='F') or
       ((sdef='2') and (pos(UH_LBEG+'gru-v'+UH_LEND,s2)>0)) or
       ((sdef='S') and (pos(UH_LBEG+'gsuru-v'+UH_LEND,s2)>0)) or
       ((sdef='K') and (pos(UH_LBEG+'gkuru-v'+UH_LEND,s2)>0)) or
       ((sdef='I') and (pos(UH_LBEG+'gIku-v'+UH_LEND,s2)>0)) or
       ((sdef='1') and (pos('-v'+UH_LEND,s2)>0) and (pos(UH_LBEG+'gru-v'+UH_LEND,s2)=0)) or
       ((sdef='A') and (pos('adj'+UH_LEND,s2)>0) and (pos(UH_LBEG+'gna-adj'+UH_LEND,s2)=0)) or
       ((sdef='N') and (pos('gna-adj',s2)>0)));
end;

procedure TDicSearchRequest.TestLookupCandidate(dic: TDicCursor; lc: PCandidateLookup;
  wt: integer; sl: TStringList);
var
  i, ii: integer;

  sxx: string;  //==lc.str
  sxxr: string; //same in romaji
  sp:integer;   //==lc.priority
  sdef:char;    //==lc.verbType
  slen:integer; //==lc.len

  wif:integer;
  entry:string; //translation entry text
  s2:string;
  converted, markers:string;
  ts:string;
  popclas:integer;
  UserScore:integer;
  UserIndex:string;
  sort:integer;
  freq:integer;
  sorts:string;
  statpref:string;
  ssig:string; //translation signature (reading x kanji) to merge duplicates
  scomp,scur:string;

  existingIdx: integer;

 //Used several times with different kanji_vals
  procedure TryGetUserScore(kanji_val: string);
  begin
    CUser.Locate(@stUserKanji,kanji_val,false);
    while (not CUser.EOF) and (kanji_val=CUser.Str(TUserKanji)) do
    begin
      if dic.Str(dic.TDictPhonetic)=CUser.Str(TUserPhonetic) then
      begin
        UserScore:=CUser.Int(TUserScore);
        UserIndex:=CUser.Str(TUserIndex);
      end;
      CUser.Next;
    end;
  end;

begin
  if (a in [stJp, stClipboard]) and (MatchType=mtMatchAnywhere) then begin
    dic.SetOrder('Index_Ind');
    dic.First;
  end;

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

 { KanaToRomaji is VERY expensive so let's only call it when really needed }
  case a of
    stJp:
      sxxr:=KanaToRomaji(sxx,1,curlang);
    stClipboard:
      if a3kana then sxxr:=KanaToRomaji(sxx,1,curlang); //only with a3kana
    stEditorInsert,
    stEditorAuto:
      if a4limitkana then
        sxxr:=KanaToRomaji(sxx,1,curlang);
  end;

  DicInitialLookup(dic, wt, sxxr, sxx);

  i:=0;
  if a=stEn then wif:=dic.ReadIndex;

  while
    ((a=stEn) or (not dic.EOF)) and
    (((a=stJp) and (MatchType=mtMatchLeft) and (pos(sxxr,dic.Str(dic.TDictSort))=1)) or
    ((a=stJp) and (MatchType=mtMatchRight) and (pos(sxxr,dic.Str(dic.TDictSort))>0)) or
    ((a=stJp) and (sxxr=dic.Str(dic.TDictSort))) or
    ((a=stClipboard) and (a3kana) and (MatchType=mtMatchLeft) and (pos(sxxr,dic.Str(dic.TDictSort))=1)) or
    ((a=stClipboard) and (a3kana) and (MatchType=mtMatchRight) and (pos(sxxr,dic.Str(dic.TDictSort))>0)) or
    ((a=stClipboard) and (a3kana) and (sxxr=dic.Str(dic.TDictSort))) or
    ((a=stEn) and (wif<>0)) or
    ((a=stClipboard) and (not a3kana) and (MatchType=mtMatchLeft) and (pos(sxx,dic.Str(dic.TDictKanji))=1)) or
    ((a=stClipboard) and (not a3kana) and (MatchType=mtMatchRight) and (pos(sxx,dic.Str(dic.TDictKanji))>0)) or
    ((a=stClipboard) and (not a3kana) and (sxx=dic.Str(dic.TDictKanji))) or
    ((a in [stEditorInsert, stEditorAuto]) and (not a4limitkana) and (sxx=dic.Str(dic.TDictKanji))) or
    ((a in [stEditorInsert, stEditorAuto]) and (a4limitkana) and (sxxr=dic.Str(dic.TDictSort))) or
    (((a=stJp) or (a=stClipboard)) and (MatchType=mtMatchAnywhere)))
  do
  begin
    if (mess=nil) and (now-nowt>1/24/60/60) then
      mess:=SMMessageDlg(_l(sDicSearchTitle), _l(sDicSearchText));
    if a=stEn then dic.Locate(dic.stIndex,inttostr(wif),true);
    //if a=stEn then showmessage(Format('%4.4X',[wif])+'-'+dic.Str(dic.TDictEnglish));
    if sdef<>'F' then entry:=ALTCH_TILDE+'I'else entry:=ALTCH_TILDE+'F';
    if dic.TDictMarkers<>-1 then
      entry:=entry+EnrichDictEntry(dic.Str(dic.TDictEnglish),dic.Str(dic.TDictMarkers))
    else begin
      converted := ConvertEdictEntry(dic.Str(dic.TDictEnglish), markers);
      entry:=entry+EnrichDictEntry(converted, markers);
    end;
    if a=stEn then ts:=lowercase(dic.Str(dic.TDictEnglish)+' ');
    if IsAppropriateVerbType(sdef, entry) then
    if (a<>stEn) or ((MatchType=mtMatchLeft) and (pos(lowercase(sxx),ts)>0)) or
    (((pos(lowercase(sxx)+' ',ts)>0) or (pos(lowercase(sxx)+',',ts)>0) or (lowercase(sxx)=ts))) then
    if (not dic_ignorekana) or (not a4limitkana) or (pos(UH_LBEG+'skana'+UH_LEND,entry)>0) then
    if (MatchType<>mtMatchAnywhere) or (CompareUnicode(sxx,dic.Str(dic.TDictPhonetic)))
    or ((a=stClipboard) and (CompareUnicode(sxx,dic.Str(dic.TDictKanji)))) then
    begin

     //Calculate popularity class
      popclas := GetPopClass(entry);
      //if ((a=1) or ((a=4) and (p4reading))) and (dic.Field('Priority')<>-1) then
      //  if dic.Int(dic.Field('Priority'))>9 then
      //    inc(popclas,90)
      //  else
      //    inc(popclas,dic.Int(dic.Field('Priority'))*10);
      if (a in [stEditorInsert, stEditorAuto]) and (p4reading)
      and CUserPrior.Locate(@stUserPriorKanji,dic.Str(dic.TDictKanji),false) then
        dec(popclas,10*CUserPrior.Int(fldUserPriorCount));

      //CUser.SetOrder('Kanji_Ind');

      UserScore:=-1;
      UserIndex:='';
      if pos(UH_LBEG+'skana'+UH_LEND,entry)>0 then
        TryGetUserScore(dic.Str(dic.TDictPhonetic));
      TryGetUserScore(dic.Str(dic.TDictKanji));
      if (UserScore=-1) and (dic.Str(dic.TDictKanji)<>ChinTo(dic.Str(dic.TDictKanji))) then
        TryGetUserScore(ChinTo(dic.Str(dic.TDictKanji)));

      if (fSettings.CheckBox58.Checked) and (dic.TDictFrequency>-1) and (dic.Int(dic.TDictFrequency)>0) then
        entry:=entry+' '+UH_LBEG+'pwc'+dic.Str(dic.TDictFrequency)+UH_LEND;
      entry:=entry+' '+UH_LBEG+'d'+dic.dic.name+UH_LEND;

     //Calculate sorting order
      sort:=0; //the bigger the worse (will apear later in list)
      if a=stEn then
      begin
        if (pos(trim(uppercase(sxx)),trim(uppercase(dic.Str(dic.TDictEnglish))))=1) then sort:=10000 else sort:=11000;
        sort:=sort+popclas*100;
      end;
      if a=stJp then sort:=(10000*(9-min(sp,9)))+length(dic.Str(dic.TDictPhonetic))*1000+popclas*10;
      if a in [stClipboard, stEditorInsert, stEditorAuto] then sort:=(10000*(9-min(sp,9)))+popclas*10;
      sort:=sort+10000;
      //if (a in [stEditorInsert, stEditorAuto]) and (p4reading) then sort:=10000+popclas*10;
      if (fSettings.CheckBox4.Checked) and (UserScore>-1) then dec(sort,1000);
      if (fSettings.CheckBox4.Checked) and (UserScore>1) then dec(sort,1000);
      if IsKanaCharKatakana(dic.Str(dic.TDictPhonetic), 1) then inc(sort,1000);
      sort:=sort+dic.dic.priority*20000;
      if (fSettings.CheckBox59.Checked) then
      begin
        if dic.TDictFrequency>-1 then
        begin
          freq:=dic.Int(dic.TDictFrequency);
          if freq>=500000 then freq:=10000 else
          if freq>=10000 then freq:=2000+(freq-10000) div 100 else
          if freq>=1000 then freq:=1000+(freq-1000) div 10;
        end;
        sort:=sort+10000-freq;
      end;
      if sort>99999 then sort:=99999;
      if sort<0 then sort:=0;

      if (a in [stEditorInsert, stEditorAuto]) and (p4reading) then
        entry:=copy(entry,1,2)+UH_LBEG+'pp'+inttostr(sort div 100)+UH_LEND+' '+copy(entry,3,length(entry)-2);

      sorts:=dic.Str(dic.TDictIndex);
      while length(sorts)<6 do
        sorts:='0'+sorts;
      while length(UserIndex)<6 do
        UserIndex:='0'+UserIndex;
      UserIndex:=UserIndex+sorts;
      sorts:=inttostr(sort);
      if length(sorts)>5 then
        sorts:='99999';
      while length(sorts)<5 do
        sorts:='0'+sorts;
      sorts:=sorts+UserIndex+Format('%.2d',[slen]);
      // if sdef<>'F'then sorts:=sorts+'I'else sorts:=sorts+'F';
      if (pos('-v'+UH_LEND,entry)>0) then
        sorts:=sorts+'I'
      else
        sorts:=sorts+'F';
      // 5 (Sort), 6 (Userindex), 6 (Dicindex), 2 (slen), 1 (sdef), 10 (dicname)
      sorts:=sorts+dic.dic.name;
      while length(sorts)<30 do
        sorts:=sorts+' ';

      statpref:='';
      if (fSettings.CheckBox11.Checked) and (UserScore>-1) then
        statpref:='!'+inttostr(UserScore);
      if (fSettings.CheckBox8.Checked) and (pos(UH_LBEG+'skana'+UH_LEND,entry)<>0) then
        scur:=sorts+statpref+dic.Str(dic.TDictPhonetic)
          +' ['+statpref+dic.Str(dic.TDictPhonetic)+'] {'+statpref+entry+'}'
      else
        scur:=sorts+statpref+CheckKnownKanji(ChinTo(dic.Str(dic.TDictKanji)))
          +' ['+statpref+dic.Str(dic.TDictPhonetic)+'] {'+statpref+entry+'}';

     //result signature (reading x kanji)
      ssig:=dic.Str(dic.TDictPhonetic)+'x'+dic.Str(dic.TDictKanji);
     //if we already have that result, only upgrade it (lower it's sorting order, add translations)
      existingIdx := presentl.IndexOf(ssig);
      if existingIdx>=0 then
      begin
        scomp:=sl[existingIdx];
        //lower sorting order
        if copy(scomp,1,5)>copy(sorts,1,5) then
          scomp := copy(sorts,1,5) + copy(scomp,6,Length(scomp)-5);
        //add user index if missing
        if copy(scomp,6,6)='000000' then
          scomp := copy(scomp,1,5) + copy(scur,6,6) + copy(scomp,12,Length(scomp)-11);
        //add tl
        if pos(copy(entry,3,length(entry)-2),scomp)=0 then begin
         //for now we add everything to the end, ignoring what result had higher score
          delete(scomp,length(scomp),1); //delete '}'
          if (length(entry)>0) and (entry[1]=ALTCH_TILDE) then delete(entry,1,2);
          scomp:=scomp+' / '+entry+'}';
        end;
        sl[existingIdx]:=scomp;
      end else
      begin
        if CAnnot<>nil then
        begin
          CAnnot.SeekK(dic.Str(dic.TDictKanji),dic.Str(dic.TDictPhonetic));
          s2:=CAnnot.GetAll('t',', ');
          if CAnnot.GetOne('c')<>'' then s2:=UH_SETCOLOR+CAnnot.GetOne('c')+s2;
          ii:=pos('{'+statpref,scur)+length(statpref)+1;
          if scur[ii]=ALTCH_EXCL then inc(ii,2);
          if scur[ii]=ALTCH_TILDE then inc(ii,2);
          if scur[ii]=ALTCH_EXCL then inc(ii,2);
          if s2<>'' then
            scur:=copy(scur,1,ii-1)+
              s2+' >> '+
              copy(scur,ii,length(scur)-ii+1);
        end;
        sl.Add(scur);
        presentl.Add(ssig);
        presindl.Add(ssig);
      end;
      inc(i);
    end;

    if (not full) and (i>=maxwords) then
    begin
      wasfull:=false;
      break;
    end;

    if a<>stEn then dic.Next else wif:=dic.ReadIndex;
  end;
end;

procedure TDicSearchRequest.SortResults(sl: TStringList);
var i: integer;
  scomp, scur:string;
  s2: string;
  sl2: TStringList;
  sl2i: integer;
begin
  if sl.Count<=1 then exit; //apparently that's the most common case when translating
  sl.Sort;
  for i:=0 to sl.Count-1 do
    if copy(sl[i],6,6)<>'000000'then
    begin
      CUser.Locate(@stUserIndex,inttostr(strtoint(copy(sl[i],6,6))),true);
      scomp:=sl[i];
      scur:=sl[i];
      delete(scomp,1,pos(' {',scomp));
      delete(scomp,1,1);
      if (length(scomp)>0) and (scomp[1]=ALTCH_EXCL) then delete(scomp,1,2);
      if (length(scomp)>0) and (scomp[1]=ALTCH_TILDE) then delete(scomp,1,2);
      scur:=copy(scur,1,length(sl[i])-length(scomp));
      s2:=CUser.Str(TUserEnglish);
      if pos(s2,scomp)>0 then
        scomp:=copy(scomp,1,pos(s2,scomp)-1)+copy(scomp,pos(s2,scomp)+length(s2),length(scomp)-length(s2)-pos(s2,scomp)+1)
      else
        scomp:=' // '+scomp;
      sl2:=TStringList.Create;
      ListWordCategories(CUser.Int(TUserIndex),sl2);
      for sl2i:=0 to sl2.Count-1 do s2:=s2+' '+UH_LBEG+'l'+copy(sl2[sl2i],3,length(sl2[sl2i])-2)+UH_LEND;
      sl2.Free;
      sl[i]:=scur+s2+scomp;
    end;
end;

end.
