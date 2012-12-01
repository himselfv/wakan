unit JWBDicSearch;
{
Dictionary search engine.
Split into a proper object from a bunch of functions which existed in JWBUser.
Has a lot of dark legacy in code form.
}

interface
uses SysUtils, Classes, JWBUtils, TextTable, MemSource, StdPrompt;

type
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
    indexword:boolean;
    indexfrom,indexto:integer;
    demandloaded:boolean;
    vocmode:integer;
    constructor Create;
    destructor Destroy; override;
    procedure FillInfo(packagefile:string); virtual;
    procedure Build(edictfile,packagefile:string); virtual;
    procedure Load; virtual;
    procedure Unload; virtual;
    procedure Demand;
    function ReadIndexString(word:boolean;loc:integer):string;
    function ReadIndexInfo(word:boolean;loc:integer):integer;
    procedure FindIndexString(word:boolean;locator:string);
    function ReadIndex:integer;

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
   { Public references in form of pointers. Pass these to Locate and have no care in life. }
    stSort: PSeekObject;
    stSortReverse: PSeekObject;
    stKanji: PSeekObject;
    stKanjiReverse: PSeekObject;
    stIndex: PSeekObject;

  end;

type
  TMatchType = (mtExactMatch, mtMatchLeft, mtMatchRight, mtMatchAnywhere);

 //Dictionary search class
 //Reuse it if you want, it'll be faster than re-creating it and filling the
 //settings again.
  TDicSearchRequest = class
  public
    constructor Create;
    destructor Destroy; override;

  public //Settings
   { search mode
    1 = jp->en
    2 = en->jp
    3 = clipboard translation
    4 = ??? }
    a:integer;
   { How to match words (exact, match left, right or anywhere) }
    MatchType: TMatchType;
   { Maximum number of words to look for [ignored if "full" is set?] }
    maxwords: integer;
   { Find all matches instead of only most relevant ones (slower) }
    full: boolean;
   { Dictionary group. Presently 5 are supported:
      1..3 - user dictionary groups
      4 - use for compounds
      5 - use for popup/editor
    No call to this function uses 4 though. }
    dictgroup: integer;
    procedure Prepare; //Call after changing settings

  public
    procedure Search(search: string; wt: integer; sl: TStringList);

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
    procedure TestLookupCandidate(dic: TJaletDic; lc: PCandidateLookup; wt: integer;
      sl: TStringList);

  protected
    a3kana:boolean;   //a==3 and only kana in string
    limitkana: boolean;
    procedure DicInitialLookup(dic: TJaletDic; wt: integer; sxxr: string; sxx: string);

  end;

//Compability
procedure DicSearch(search:string;a:integer; MatchType: TMatchType;
  full:boolean;wt,maxwords:integer;sl:TStringList;dictgroup:integer;var wasfull:boolean);

var
  dic_ignorekana:boolean;

procedure Deflex(const w:string;sl:TCandidateLookupList;prior,priordfl:byte;mustsufokay,alwaysdeflect:boolean);

implementation
uses Forms, Windows, JWBMenu, JWBUnit, JWBUser, JWBSettings, JWBWords, Math;


procedure SortResults(sl: TStringList); forward;


{
Utility functions
}

function CompareUnicode(sub,str:string):boolean;
var i:integer;
begin
  result:=false;
  for i:=0 to ((length(str)-length(sub)) div 4) do
    if copy(str,i*4+1,length(sub))=sub then
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
  tested:=false;
  loaded:=false;
end;

destructor TJaletDic.Destroy;
begin
  if loaded then Unload;
end;

function TJaletDic.ReadIndexInfo(word:boolean;loc:integer):integer;
var l:integer;
    p:PAnsiChar;
begin
  if word then p:=wordidx else p:=charidx;
  p:=p+loc*4;
  move(p^,l,4);
  result:=l;
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

procedure TJaletDic.FindIndexString(word:boolean;locator:string);
var l,r,m,max:integer;
    s,s2:string;
begin
  l:=0;
  if word then max:=wordno-1 else max:=charno-1;
  r:=max;
  s:=locator;
  s2:='';
  while (pos(' ',s)>0) and (s2='') do
  begin
    s2:=copy(s,1,pos(' ',s)-1);
    delete(s,1,pos(' ',s));
    if ignorel.IndexOf(s2)<>-1 then s2:='';
  end;
  if s2='' then s2:=s;
  locator:=s;
  if length(locator)>4 then locator:=copy(locator,1,4);
  while length(locator)<4 do locator:=locator+' ';
  while l<=r do
  begin
    m:=l+(r-l) div 2;
    s2:=ReadIndexString(word,m*2);
    if s2=locator then break;
    if s2<locator then l:=m+1 else r:=m-1;
  end;
  if l>r then indexfrom:=0 else
  begin
    indexfrom:=ReadIndexInfo(word,m*2+1)+(max+1)*2;
    if m<max then indexto:=ReadIndexInfo(word,m*2+3)+(max+1)*2 else if word then indexto:=wordfsiz else indexto:=charfsiz;
    indexword:=word;
  end;
end;

function TJaletDic.ReadIndex:integer;
begin
  if (indexfrom=0) or (indexfrom>=indexto) then
  begin
    result:=0;
    exit;
  end;
  result:=ReadIndexInfo(indexword,indexfrom);
//  showmessage(Format('%4.4X-%4.4X',[indexfrom*4,result]));
  inc(indexfrom);
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
  if err<>'' then Application.MessageBox(pchar(_l('#00321^eCannot register dictionary ^cNepodaøilo se zaregistrovat slovník ')+pname+#13#13+err),pchar(_l('#00020^eError^cChyba')),MB_ICONERROR or MB_OK);
end;

procedure TJaletDic.Build(edictfile,packagefile:string);
var err:string;
begin
  pname:=packagefile;
  err:='Building not supported yet.';
  if err<>'' then Application.MessageBox(pchar(_l('#00322^eCannot build dictionary ^cNepodaøilo se vytvoøit slovník ')+pname+#13#13+err),pchar(_l('#00020^eError^cChyba')),MB_ICONERROR or MB_OK);
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
  pd:=SMMessageDlg(_l('#00323^eDictionary loading^cNahrávání slovníku'),_l('#00324^eLoading dictionary ^cNahrávám slovník ')+name+'...');
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
    Application.MessageBox(pchar(_l('#00325^eCannot load dictionary ^cNepodaøilo se nahrát slovník ')+pname+#13#13+(ExceptObject as Exception).Message),pchar(_l('#00020^eError^cChyba')),MB_ICONERROR or MB_OK);
  end;
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


{
Deflection and lookup lists
}

procedure Deflex(const w:string;sl:TCandidateLookupList;prior,priordfl:byte;mustsufokay,alwaysdeflect:boolean);
var ws: integer; //length of w in symbols. Not sure if needed but let's keep it for a while
    i,j,k:integer;
    s,s2:string;
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
  ws:=length(w) div 4;
  if curlang='j'then
  begin
    lastkanji:=0;
    for i:=1 to length(w) div 4 do
      if EvalChar(copy(w,i*4-3,4))<>EC_HIRAGANA then
        lastkanji:=i
      else
        break;
    core:=copy(w,1,lastkanji*4);
    roma:=copy(w,lastkanji*4+1,length(w)-lastkanji*4);
    if (fUser.SpeedButton4.Down) or (alwaysdeflect) then
      for i:=0 to defll.Count-1 do
      begin
        dr := defll[i];

        for j:=0 to (length(roma)-length(dr.infl)) div 4 do
          if (copy(roma,j*4+1,length(dr.infl))=dr.infl) or
             ((dr.infl='KKKK') and (j=0) and (core<>'')) then
          if ((dr.vt<>'K') and (dr.vt<>'I')) or ((dr.vt='K') and (j=0) and ((core='') or (core='6765')))
             or ((dr.vt='I') and (j=0) and ((core='') or (core='884C'))) then
          begin
            if (length(dr.defl)+j*4<=24) and (core+copy(roma,1,j*4)+dr.defl<>w) then
            begin
//              ws:=length(core+copy(roma,1,j*4)+dr.infl) div 4;
//              if dr.infl='KKKK'then ws:=length(core) div 4;
//              if ws<=length(core) div 4 then ws:=(length(core)+4) div 4;
//              if ws<=1 then ws:=2;
              ws:=length(w) div 4;
              ad:=core+copy(roma,1,j*4)+dr.defl;
              suf:=copy(roma,j*4+1+length(dr.infl),length(roma)-j*4-length(dr.infl));
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
    ws:=length(w) div 4;
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
    1:if curlang='j'then
        Deflex(RomajiToKana('H'+search,romasys,true,'j'),se,9,8,true,false)
      else
        Deflex(RomajiToKana(search,romasys,true,'c'),se,9,8,true,false);
    2:se.Add(9, 1, 'F', search);
    3:Deflex(ChinFrom(search),se,9,8,true,false);
    4:begin
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
            for i:=(length(_s) div 4) downto 1 do
            begin
              partfound:=false;
              every:=false;
              if EvalChar(copy(_s,i*4-3,4))=1 then every:=true;
              if (wt=2) and (i<(length(_s) div 4)) and (i>=(length(_s) div 4)-4) and (partl.IndexOf(copy(_s,i*4+1,length(_s)-i*4))>-1) then
                partfound:=true;
//              if ((i<(length(_s) div 4)) and every) or (wt=2) then
              if (every) or ((i>1) and (MatchType=mtMatchLeft)) or (i=length(_s) div 4) or (partfound) then
                se.Add(i, i, 'F', copy(_s,1,i*4));
            end;
          end;
          if (wt=3) then
          begin
            _s:=ChinFrom(search);
            se.Add(9, Length(_s) div 4, 'F', _s);
          end;
        end;
      end;
  end;
end;


//Returns the base popularity class for a record (the lower the better)
function GetPopClass(s2: string): integer;
begin
  Result := 40;
  if (fSettings.CheckBox5.Checked) and (pos('<gn',s2)>0) then dec(Result,5);
  if (fSettings.CheckBox5.Checked) and (pos('-v>',s2)>0) then dec(Result,5);
  if (fSettings.CheckBox5.Checked) and (pos('<g',s2)=0) then dec(Result,5);
  if (fSettings.CheckBox6.Checked) and (pos('<shonor>',s2)>0) then dec(Result,1);
  if (fSettings.CheckBox6.Checked) and (pos('<shum>',s2)>0) then dec(Result,2);
  if (fSettings.CheckBox6.Checked) and (pos('<spolite>',s2)>0) then dec(Result,3);
  if pos('<sobsolete>',s2)>0 then inc(Result,20);
  if pos('<sobscure>',s2)>0 then inc(Result,20);
  if (fSettings.CheckBox7.Checked) and (pos('<spop>',s2)>0) then dec(Result,150);
end;



{
DicSearch()
Don't use if you're doing a lot of searches, use TDicSearchRequest instead.
}
procedure DicSearch(search:string;a:integer; MatchType: TMatchType;
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
end;

destructor TDicSearchRequest.Destroy;
begin
  se.Destroy;
  presentl.Destroy;
  presindl.Destroy;
  inherited;
end;

procedure TDicSearchRequest.Prepare;
begin
  if maxwords<10 then maxwords:=10;
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
}

procedure TDicSearchRequest.Search(search:string; wt: integer; sl:TStringList);
var i:integer;
    di:integer;
    _s:string;
    dic:TJaletDic;

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
    4: begin
      p4reading:=wt=-1;
      if wt=-1 then if partl.IndexOf(search)>-1 then begin
        _s := ChinFrom(search);
        sl.Add('000000000000000000'+inttostr(length(search) div 4)+'P          '
          +_s+' ['+_s+'] {'+KanaToRomaji(search,1,'j')+' particle}');
      end;
    end;
  end;

 //I don't know WHY the following line is needed,
 //but it does exactly what it did in old Wakan. Whatever that is.  
  if full or (fUser.SpeedButton13.Down and (MatchType<>mtMatchAnywhere)) then
  for di:=0 to dicts.Count-1 do begin
    if not (dicts.Objects[di] as TJaletDic).loaded then continue;

    dic:=dicts.Objects[di] as TJaletDic;
    if pos(','+dic.name,NotGroupDicts[dictgroup])<>0 then
      continue; //dic excluded from this dictgroup

    dic.Demand;

    case a of
      1: if MatchType=mtMatchRight then dic.TDict.SetOrder('<Phonetic_Ind') else dic.TDict.SetOrder('Phonetic_Ind');
      3: if MatchType=mtMatchRight then dic.TDict.SetOrder('<Kanji_Ind') else dic.TDict.SetOrder('Kanji_Ind');
      4: dic.TDict.SetOrder('Kanji_Ind');
    end;

    for i:=0 to se.Count-1 do
      TestLookupCandidate(dic, se[i], wt, sl);

  end; //of dict enum

  SortResults(sl);

  mess.Free;
end;

resourcestring
  sDicSearchTitle='#00932^eDic.search^cSlovník';
  sDicSearchText='#00933^ePlease wait. Searching dictionary...^cProsím èekejte. Prohledávám slovník...';

procedure TDicSearchRequest.DicInitialLookup(dic: TJaletDic; wt: integer; sxxr: string; sxx: string);
begin
  case a of
  1:
    case MatchType of
      mtMatchAnywhere: begin end; //nothing
      mtMatchRight: dic.TDict.Locate(dic.stSortReverse,sxxr,false);
    else
      dic.TDict.Locate(dic.stSort,sxxr,false);
    end;
  2: dic.FindIndexString(true,lowercase(sxx));
  3:
    if a3kana then
      case MatchType of
        mtMatchAnywhere: begin end;
        mtMatchRight: dic.TDict.Locate(dic.stSortReverse,sxxr,false);
      else
        dic.TDict.Locate(dic.stSort,sxxr,false);
      end
    else
      case MatchType of
        mtMatchAnywhere: begin end;
        mtMatchRight: dic.TDict.Locate(dic.stKanjiReverse,sxx,false);
      else
        dic.TDict.Locate(dic.stKanji,sxx,false);
      end;
  4:
    if p4reading then begin
      dic.TDict.SetOrder('Phonetic_Ind');
      dic.TDict.Locate(dic.stSort,sxxr,false);
      limitkana:=true;
    end else
    begin
      if wt<>2 then
      begin
        dic.TDict.SetOrder('Kanji_Ind');
        dic.TDict.Locate(dic.stKanji,sxx,false);
      end else
      begin
        limitkana:=true;
        dic.TDict.SetOrder('Phonetic_Ind');
        dic.TDict.Locate(dic.stSort,sxxr,false);
      end;
    end;
  end;
end;

function IsAppropriateVerbType(sdef: string; s2: string): boolean;
begin
  Result :=
       ((sdef='F') or
       ((sdef='2') and (pos('<gru-v>',s2)>0)) or
       ((sdef='S') and (pos('<gsuru-v>',s2)>0)) or
       ((sdef='K') and (pos('<gkuru-v>',s2)>0)) or
       ((sdef='I') and (pos('<gIku-v>',s2)>0)) or
       ((sdef='1') and (pos('-v>',s2)>0) and (pos('<gru-v>',s2)=0)) or
       ((sdef='A') and (pos('adj>',s2)>0) and (pos('<gna-adj>',s2)=0)) or
       ((sdef='N') and (pos('gna-adj',s2)>0)));
end;

procedure TDicSearchRequest.TestLookupCandidate(dic: TJaletDic; lc: PCandidateLookup;
  wt: integer; sl: TStringList);
var
  i, ii: integer;

  sxx: string;  //==lc.str
  sxxr: string; //same in romaji
  sp:integer;   //==lc.priority
  sdef:char;    //==lc.verbType
  slen:integer; //==lc.len

  wif:integer;
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
  ssig,scomp,scur:string;

 //Used several times with different kanji_vals
  procedure TryGetUserScore(kanji_val: string);
  begin
    TUser.Locate('Kanji',kanji_val,false);
    while (not TUser.EOF) and (kanji_val=TUser.Str(TUserKanji)) do
    begin
      if dic.TDict.Str(dic.TDictPhonetic)=TUser.Str(TUserPhonetic) then
      begin
        UserScore:=TUser.Int(TUserScore);
        UserIndex:=TUser.Str(TUserIndex);
      end;
      TUser.Next;
    end;
  end;

begin
  if ((a=1) or (a=3)) and (MatchType=mtMatchAnywhere) then begin
    dic.TDict.SetOrder('Index_Ind');
    dic.TDict.First;
  end;

  sxx:=lc.str;
  if sxx='' then exit;
  sp:=lc.priority;
  sdef:=lc.verbType;
  slen:=lc.len;

  limitkana:=false;
  if a<3 then slen:=1;
  if a=3 then begin
    a3kana:=true;
    for i:=1 to length(sxx) div 4 do
      if not (EvalChar(copy(sxx,(i-1)*4+1,4)) in [EC_HIRAGANA, EC_KATAKANA]) then begin
        a3kana := false;
        break;
      end;
  end;

  case a of
    1, 4: sxxr:=KanaToRomaji(sxx,1,curlang);
    3: if a3kana then sxxr:=KanaToRomaji(sxx,1,curlang);
  end;

  DicInitialLookup(dic, wt, sxxr, sxx);

  i:=0;
  if a=2 then wif:=dic.ReadIndex;


  while
    ((a=2) or (not dic.TDict.EOF)) and
    (((a=1) and (MatchType=mtMatchLeft) and (pos(sxxr,dic.TDict.Str(dic.TDictSort))=1)) or
    ((a=1) and (MatchType=mtMatchRight) and (pos(sxxr,dic.TDict.Str(dic.TDictSort))>0)) or
    ((a=1) and (sxxr=dic.TDict.Str(dic.TDictSort))) or
    ((a=3) and (a3kana) and (MatchType=mtMatchLeft) and (pos(sxxr,dic.TDict.Str(dic.TDictSort))=1)) or
    ((a=3) and (a3kana) and (MatchType=mtMatchRight) and (pos(sxxr,dic.TDict.Str(dic.TDictSort))>0)) or
    ((a=3) and (a3kana) and (sxxr=dic.TDict.Str(dic.TDictSort))) or
    ((a=2) and (wif<>0)) or
    ((a=3) and (not a3kana) and (MatchType=mtMatchLeft) and (pos(sxx,dic.TDict.Str(dic.TDictKanji))=1)) or
    ((a=3) and (not a3kana) and (MatchType=mtMatchRight) and (pos(sxx,dic.TDict.Str(dic.TDictKanji))>0)) or
    ((a=3) and (not a3kana) and (sxx=dic.TDict.Str(dic.TDictKanji))) or
    ((a=4) and (not limitkana) and (sxx=dic.TDict.Str(dic.TDictKanji))) or
    ((a=4) and (limitkana) and (sxxr=dic.TDict.Str(dic.TDictSort))) or
    (((a=1) or (a=3)) and (MatchType=mtMatchAnywhere)))
  do
  begin
    if (mess=nil) and (now-nowt>1/24/60/60) then
      mess:=SMMessageDlg(_l(sDicSearchTitle), _l(sDicSearchText));
    if a=2 then dic.TDict.Locate(dic.stIndex,inttostr(wif),true);
    //if a=2 then showmessage(Format('%4.4X',[wif])+'-'+dic.TDict.Str(dic.TDictEnglish));
    if sdef<>'F'then s2:='~I'else s2:='~F';
    if dic.TDictMarkers<>-1 then
      s2:=s2+EnrichDictEntry(dic.TDict.Str(dic.TDictEnglish),dic.TDict.Str(dic.TDictMarkers))
    else begin
      converted := ConvertEdictEntry(dic.TDict.Str(dic.TDictEnglish), markers);
      s2:=s2+EnrichDictEntry(converted, markers);
    end;
    if a=2 then ts:=lowercase(dic.TDict.Str(dic.TDictEnglish)+' ');
    //showmessage(sdef+KanaToRomaji(sxx,1,'j')+','+s2);
    if IsAppropriateVerbType(sdef, s2) then
    if (a<>2) or ((MatchType=mtMatchLeft) and (pos(lowercase(sxx),ts)>0)) or
    (((pos(lowercase(sxx)+' ',ts)>0) or (pos(lowercase(sxx)+',',ts)>0) or (lowercase(sxx)=ts))) then
    if (not dic_ignorekana) or (not limitkana) or (pos('<skana>',s2)>0) then
    if (MatchType<>mtMatchAnywhere) or (CompareUnicode(sxx,dic.TDict.Str(dic.TDictPhonetic)))
    or ((a=3) and (CompareUnicode(sxx,dic.TDict.Str(dic.TDictKanji)))) then
    begin

     //Calculate popularity class
      popclas := GetPopClass(s2);
      //if ((a=1) or ((a=4) and (p4reading))) and (dic.TDict.Field('Priority')<>-1) then
      //  if dic.TDict.Int(dic.TDict.Field('Priority'))>9 then
      //    inc(popclas,90)
      //  else
      //    inc(popclas,dic.TDict.Int(dic.TDict.Field('Priority'))*10);
      if (a=4) and (p4reading)
      and TUserPrior.Locate('Kanji',dic.TDict.Str(dic.TDictKanji),false) then
        dec(popclas,10*TUserPrior.Int(TUserPrior.Field('Count')));

      //TUser.SetOrder('Kanji_Ind');

      UserScore:=-1;
      UserIndex:='';
      if pos('<skana>',s2)>0 then
        TryGetUserScore(dic.TDict.Str(dic.TDictPhonetic));
      TryGetUserScore(dic.TDict.Str(dic.TDictKanji));
      if (UserScore=-1) and (dic.TDict.Str(dic.TDictKanji)<>ChinTo(dic.TDict.Str(dic.TDictKanji))) then
        TryGetUserScore(ChinTo(dic.TDict.Str(dic.TDictKanji)));

      if (fSettings.CheckBox58.Checked) and (dic.TDictFrequency>-1) and (dic.TDict.Int(dic.TDictFrequency)>0) then
        s2:=s2+' <pwc'+dic.TDict.Str(dic.TDictFrequency)+'>';
      s2:=s2+' <d'+dic.name+'>';

     //Calculate sorting order
      sort:=0; //the bigger the worse (will apear later in list)
      if a=2 then
      begin
        if (pos(trim(uppercase(sxx)),trim(uppercase(dic.TDict.Str(dic.TDictEnglish))))=1) then sort:=10000 else sort:=11000;
        sort:=sort+popclas*100;
      end;
      if a=1 then sort:=(10000*(9-min(sp,9)))+length(dic.TDict.Str(dic.TDictPhonetic))*1000+popclas*10;
      if (a=3) or (a=4) then sort:=(10000*(9-min(sp,9)))+popclas*10;
      sort:=sort+10000;
      //if (a=4) and (p4reading) then sort:=10000+popclas*10;
      if (fSettings.CheckBox4.Checked) and (UserScore>-1) then dec(sort,1000);
      if (fSettings.CheckBox4.Checked) and (UserScore>1) then dec(sort,1000);
      if dic.TDict.Str(dic.TDictPhonetic)[3]>='A' then inc(sort,1000);
      sort:=sort+dic.priority*20000;
      if (fSettings.CheckBox59.Checked) then
      begin
        if dic.TDictFrequency>-1 then
        begin
          freq:=dic.TDict.Int(dic.TDictFrequency);
          if freq>=500000 then freq:=10000 else
          if freq>=10000 then freq:=2000+(freq-10000) div 100 else
          if freq>=1000 then freq:=1000+(freq-1000) div 10;
        end;
        sort:=sort+10000-freq;
      end;
      if sort>99999 then sort:=99999;
      if sort<0 then sort:=0;

      if (a=4) and (p4reading) then
        s2:=copy(s2,1,2)+'<pp'+inttostr(sort div 100)+'> '+copy(s2,3,length(s2)-2);

      sorts:=dic.TDict.Str(dic.TDictIndex);
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
      if (pos('-v>',s2)>0) then
        sorts:=sorts+'I'
      else
        sorts:=sorts+'F';
      // 5 (Sort), 6 (Userindex), 6 (Dicindex), 2 (slen), 1 (sdef), 10 (dicname)
      sorts:=sorts+dic.name;
      while length(sorts)<30 do
        sorts:=sorts+' ';

      statpref:='';
      if (fSettings.CheckBox11.Checked) and (UserScore>-1) then
        statpref:='!'+inttostr(UserScore);
      ssig:=dic.TDict.Str(dic.TDictPhonetic)+'x'+dic.TDict.Str(dic.TDictKanji);
      if (fSettings.CheckBox8.Checked) and (pos('<skana>',s2)<>0) then
        scur:=sorts+statpref+dic.TDict.Str(dic.TDictPhonetic)
          +' ['+statpref+dic.TDict.Str(dic.TDictPhonetic)+'] {'+statpref+s2+'}'
      else
        scur:=sorts+statpref+CheckKnownKanji(ChinTo(dic.TDict.Str(dic.TDictKanji)))
          +' ['+statpref+dic.TDict.Str(dic.TDictPhonetic)+'] {'+statpref+s2+'}';

      if presentl.IndexOf(ssig)<>-1 then
      begin
        scomp:=sl[presindl.IndexOf(ssig)];
        if pos(copy(s2,3,length(s2)-2),scomp)=0 then
        begin
          if copy(scomp,1,5)>copy(sorts,1,5) then
          begin
            delete(scomp,1,pos(' {',scomp));
            delete(scomp,1,1);
            if (length(scomp)>0) and (scomp[1]='!') then delete(scomp,1,2);
            if (length(scomp)>0) and (scomp[1]='~') then delete(scomp,1,2);
            delete(scur,length(scur),1);
            scur:=scur+' / '+scomp;
            sl[presindl.IndexOf(ssig)]:=scur;
          end else
          begin
            delete(scomp,length(scomp),1);
            if (length(s2)>0) and (s2[1]='~') then delete(s2,1,2);
            scomp:=scomp+' / '+s2+'}';
            sl[presindl.IndexOf(ssig)]:=scomp;
          end;
        end;
      end else
      begin
        if fMenu.IsAnnot then
        begin
          fMenu.AnnotSeekK(dic.TDict.Str(dic.TDictKanji),dic.TDict.Str(dic.TDictPhonetic));
          s2:=fMenu.AnnotGetAll('t',', ');
          if fMenu.AnnotGetOne('c')<>'' then s2:='%'+fMenu.AnnotGetOne('c')+s2;
          ii:=pos('{'+statpref,scur)+length(statpref)+1;
          if scur[ii]='!' then inc(ii,2);
          if scur[ii]='~' then inc(ii,2);
          if scur[ii]='!' then inc(ii,2);
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

    if a<>2 then dic.TDict.Next else wif:=dic.ReadIndex;
  end;

end;

procedure SortResults(sl: TStringList);
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
      TUser.Locate('Index',inttostr(strtoint(copy(sl[i],6,6))),true);
      scomp:=sl[i];
      scur:=sl[i];
      delete(scomp,1,pos(' {',scomp));
      delete(scomp,1,1);
      if (length(scomp)>0) and (scomp[1]='!') then delete(scomp,1,2);
      if (length(scomp)>0) and (scomp[1]='~') then delete(scomp,1,2);
      scur:=copy(scur,1,length(sl[i])-length(scomp));
      s2:=TUser.Str(TUserEnglish);
      if pos(s2,scomp)>0 then
        scomp:=copy(scomp,1,pos(s2,scomp)-1)+copy(scomp,pos(s2,scomp)+length(s2),length(scomp)-length(s2)-pos(s2,scomp)+1)
      else
        scomp:=' // '+scomp;
      sl2:=TStringList.Create;
      fWords.ListWordCategories(TUser.Int(TUserIndex),sl2,'',false);
      for sl2i:=0 to sl2.Count-1 do s2:=s2+' <l'+copy(sl2[sl2i],3,length(sl2[sl2i])-2)+'>';
      sl2.Free;
      sl[i]:=scur+s2+scomp;
    end;
end;

initialization
finalization

end.
