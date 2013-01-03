unit JWBDic;

interface
uses SysUtils, Classes, MemSource, TextTable;

//See comments to TextTable.CURSOR_IN_TABLE
{$DEFINE DIC_CURSOR_IN_TABLE}

const
  CurDicVer=4;

type
 { Dictionary group. Presently 5 are supported:
    1..3 - user dictionary groups
    4 - use for compounds
    5 - use for popup/editor }
  TDictGroup = integer;

  TDicCursor = class;

  TNotifyExceptionEvent = procedure(Sender: TObject; E: Exception) of object;

  TJaletDic = class
  protected
    FOffline: boolean;
    FLoadOnDemand: boolean;
    FOnLoadStart: TNotifyEvent;
    FOnLoadEnd: TNotifyEvent;
    FOnLoadException: TNotifyExceptionEvent;

  public //Extracted from package on FillInfo
    builddate:TDateTime;
    version:string;
    dicver:integer;
    name:string;
    description:string;
    language:char;
    priority:integer;
    entries:integer;
    copyright:string;
    tested:boolean;
    hasWordIndex:boolean;
    hasCharIndex:boolean;

  public //Fields, populated on load
    TDict:TTextTable;
    TDictIndex,
    TDictEnglish,
    TDictKanji,
    TDictPhonetic,
    TDictSort,
    TDictMarkers,
    TDictFrequency:integer;

  public
    charidx,wordidx:pointer;
    charno,wordno:integer;
    charfsiz,wordfsiz:integer;
    package:TPackageSource;
    loaded:boolean;
    pname:string;
    demandloaded:boolean;
    vocmode:integer;
   {$IFDEF DIC_CURSOR_IN_TABLE}
    _intcur: TDicCursor;
   {$ENDIF}
    constructor Create;
    destructor Destroy; override;
    procedure FillInfo(packagefile:string); virtual;
    procedure Load; virtual;
    procedure Unload; virtual;
    procedure Demand;
    function ReadIndexString(word:boolean;loc:integer):string;
    function ReadIndexInfo(word:boolean;loc:integer):integer;
   {$IFDEF DIC_CURSOR_IN_TABLE}
    procedure FindIndexString(word:boolean;locator:string); {$IFDEF INLINE}inline;{$ENDIF}
    function ReadIndex:integer; {$IFDEF INLINE}inline;{$ENDIF}
   {$ENDIF}
    property Offline: boolean read FOffline write FOffline;
    property LoadOnDemand: boolean read FLoadOnDemand write FLoadOnDemand; //if set, load the dictionary only on demand
    property OnLoadStart: TNotifyEvent read FOnLoadStart write FOnLoadStart;
    property OnLoadEnd: TNotifyEvent read FOnLoadEnd write FOnLoadEnd;
    property OnLoadException: TNotifyExceptionEvent read FOnLoadException write FOnLoadException;

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

  EDictionaryException = class(Exception);

const
  GROUP_NOTUSED = -1;
  GROUP_OFFLINE = 0;

type
  TDictionaryList = class(TList)
  protected
    function Get(Index: Integer): TJaletDic; reintroduce;
    procedure Put(Index: Integer; Item: TJaletDic); reintroduce;
  public
    Priority: TStringList;
    NotUsedDicts:string;
    NotGroupDicts:array[1..5] of string;
    OfflineDicts:string;
    constructor Create;
    destructor Destroy; override;
    function FindIndex(AName: string): integer;
    function Find(AName: string): TJaletDic;
    procedure PutInGroup(dic: TJaletDic; group: TDictGroup; inGroup: boolean); overload; inline;
    procedure PutInGroup(dicname: string; group: TDictGroup; inGroup: boolean); overload;
    function IsInGroup(dic: TJaletDic; group: TDictGroup): boolean; overload; inline;
    function IsInGroup(dicname: string; group: TDictGroup): boolean; overload;
    property Items[Index: Integer]: TJaletDic read Get write Put; default;
  end;

var
  ignorel: TStringList; //words to ignore when indexing dictionaries

implementation
uses Forms;

{
Dictionary list
}

constructor TDictionaryList.Create;
begin
  inherited;
  Priority := TStringList.Create;
end;

destructor TDictionaryList.Destroy;
begin
  FreeAndNil(Priority);
  inherited;
end;

function TDictionaryList.Get(Index: Integer): TJaletDic;
begin
  Result := TJaletDic(inherited Get(Index));
end;

procedure TDictionaryList.Put(Index: Integer; Item: TJaletDic);
begin
  inherited Put(Index, Item);
end;

function TDictionaryList.IsInGroup(dic: TJaletDic; group: TDictGroup): boolean;
begin
  Result := IsInGroup(dic.name,group);
end;

function TDictionaryList.IsInGroup(dicname: string; group: TDictGroup): boolean;
begin
  case group of
    GROUP_NOTUSED: Result := pos(','+dicname,NotUsedDicts)<>0;
    GROUP_OFFLINE: Result := pos(','+dicname,OfflineDicts)<>0;
  else
    Result := pos(','+dicname,NotGroupDicts[group])=0;
  end;
end;

procedure TDictionaryList.PutInGroup(dic: TJaletDic; group: TDictGroup; inGroup: boolean);
begin
  PutInGroup(dic.name, group, inGroup);
end;

procedure AddToList(const name: string; var list: string; add: boolean);
begin
  if add then begin
    if pos(','+name,list)=0 then list:=list+','+name;
  end else begin
    if pos(','+name,list)>0 then delete(list,pos(','+name,list),length(name)+1);
  end;
end;

procedure TDictionaryList.PutInGroup(dicname: string; group: TDictGroup; inGroup: boolean);
begin
  case group of
    GROUP_NOTUSED: AddToList(dicname, NotUsedDicts, inGroup);
    GROUP_OFFLINE: AddToList(dicname, OfflineDicts, inGroup);
  else
    AddToList(dicname, NotGroupDicts[group], not inGroup);
  end;
end;

function TDictionaryList.FindIndex(AName: string): integer;
var i: integer;
begin
  Result := -1;
  for i := 0 to Self.Count - 1 do
    if Items[i].name=AName then begin
      Result := i;
      break;
    end;
end;

function TDictionaryList.Find(AName: string): TJaletDic;
var i: integer;
begin
  i := FindIndex(AName);
  if i<0 then Result := nil else Result := Items[i];
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
  vs:TStringList;
begin
  tested:=false;
  pname:=packagefile;
  vs:=TStringList.Create;
  ps:=TPackageSource.Create(pname,791564,978132,978123);
  try
    if ps.GetFileList.IndexOf('dict.ver')<0 then
      raise EDictionaryException.Create('Unknown file structure');

    vs.LoadFromStream(ps.Files['dict.ver'].Lock);
    ps.Files['dict.ver'].Unlock;
    if vs[0]<>'DICT' then
      raise EDictionaryException.Create('Invalid DIC header')
    else
    begin
      dicver:=strtoint(vs[1]);
      if dicver>CurDicVer then
        raise EDictionaryException.Create('Unsupported DIC version')
      else
      if (dicver<CurDicVer) and ((CurDicVer<>4) or (dicver<>3)) then
        raise EDictionaryException.Create('Outdated DIC structure - please download new DIC file')
      else
      if (dicver<CurDicVer) then
        raise EDictionaryException.Create('DIC indexes corrupted - bug in '
          +'1.47 DIC, you have to download this DIC anew (for 1.50+), I''m '
          +'sorry for inconvenience')
      else
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

    hasWordIndex := ps.GetFileList.IndexOf('WordIdx.bin')>=0;
    hasCharIndex := ps.GetFileList.IndexOf('CharIdx.bin')>=0;
  finally
    ps.Free;
    vs.Free;
  end;
end;

procedure TJaletDic.Load;
begin
  if loaded then Unload;
  demandloaded:=false;
  loaded:=true;
  if not LoadOnDemand then Demand;
end;

procedure TJaletDic.Demand;
var mf:TMemoryFile;
begin
  if demandloaded then exit;
  if Assigned(FOnLoadStart) then
    FOnLoadStart(Self);
  try
    package:=TPackageSource.Create(pname,791564,978132,978123);
    TDict:=TTextTable.Create(package,'Dict',true,Self.Offline);
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
    on E: Exception do begin
      if Assigned(FOnLoadException) then
        FOnLoadException(Self, E)
      else raise;
    end;
  end;
 {$IFDEF DIC_CURSOR_IN_TABLE}
  _intcur:=TDicCursor.Create(Self);
 {$ENDIF}
  demandloaded:=true;
  if Assigned(FOnLoadEnd) then
    FOnLoadEnd(Self);
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

initialization
  ignorel:=TStringList.Create;

finalization
  ignorel.Free;

end.
