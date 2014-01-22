unit JWBDic;

interface
uses SysUtils, Classes, MemSource, TextTable, JWBStrings, JWBIndex,
  JWBEdictMarkers;

//See comments to TextTable.CURSOR_IN_TABLE
{$DEFINE DIC_CURSOR_IN_TABLE}

type
  TDicVersion = integer;
 {
  Two versions are currently supported.
  v4:
    one table
    no multilingual support
    EDICT1 compatible

  v5:
    two tables, (kanji-kana) and (entries)
    multilingual support
    EDICT2 compatible
 }

type
 { Dictionary group. 5 are supported at this time:
    1..3 - user dictionary groups
    4 - use for compounds
    5 - use for popup/editor }
  TDictGroup = integer;

  TDicCursor = class;
  TDicIndexReader = class;
  TDicLookupCursor = class;

 { How to match words (exact, match left, right or anywhere) }
  TMatchType = (
    mtExactMatch,
    mtMatchLeft,
    mtMatchRight,
    mtMatchAnywhere);

  TIndexType = (
    itWord,
    itChar
  );

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
    hasWordIndex:boolean;
    hasCharIndex:boolean;
    sources:TStringList; //nil if lacks this info
    tested:boolean; //this information has been loaded

  protected //Fields, populated on load
    TDict:TTextTable;
    TDictIndex, //i in v4, missing in v5
    TDictEnglish, //s in v4, missing in v5
    TDictKanji, //x
    TDictPhonetic, //x
    TDictSort, //s
    TDictMarkers, //s
    TDictFrequency, //i
    TDictArticle //i on v5, missing on v4
      :integer;

    TEntries:TTextTable;
    TEntriesIndex,  //i in v5
    TEntriesEntry,  //x in v5
    TEntriesMarkers //s in v5
      :integer;

   { To speed up access to certain seek tables we're going to keep pre-calculated
    references here. }
    FstSort: TSeekObject;
    FstSortReverse: TSeekObject;
    FstKanji: TSeekObject;
    FstKanjiReverse: TSeekObject;
    FstIndex: TSeekObject;
    FstDictArticle: TSeekObject;
    FstEntriesIndex: TSeekObject;
   { In case you need a seek table not specified here, you can get it from
    TDict.GetSeekObject('seek name') }
    procedure SetupSeekObjects;
  public
    function SupportsFrequency: boolean;
    function SupportsMarkers: boolean;
    property TTDict:TTextTable read TDict; //don't access unless you're doing something low-level!
    property TTEntries:TTextTable read TEntries; 

  public
   { Public pointers. Pass these to Locate and have no care in life. }
    stSort: PSeekObject;
    stSortReverse: PSeekObject;
    stKanji: PSeekObject;
    stKanjiReverse: PSeekObject;
    stIndex: PSeekObject;
    stDictArticle: PSeekObject;
    stEntriesIndex: PSeekObject;

  protected
   {
   Word and character indexes.
   On v4 both are indexed by 4-byte sequences,
     char: 4-byte FChar of that char
     word: first 4 ansi characters of english meaning
   On v5 both are indexed by 8-byte sequences,
     char: first two bytes are that WideChar, rest is unused
     word: first 4 wide characters of meaning
   }
    charidx: TIndex;
    wordidx: TIndex;

  public
    package:TPackageSource;
    loaded:boolean;
    pname:string;
    demandloaded:boolean;
   {$IFDEF DIC_CURSOR_IN_TABLE}
    _intcur: TDicIndexReader;
   {$ENDIF}
    constructor Create;
    destructor Destroy; override;
    procedure FillInfo(const packagefile:string); overload;
    procedure FillInfo(ps:TPackageSource); overload;
    procedure Load; virtual;
    procedure Unload; virtual;
    procedure Demand;
   {$IFDEF DIC_CURSOR_IN_TABLE}
    procedure FindIndexString(t:TIndexType;const locator:UnicodeString); inline;
    function ReadIndex:integer; inline;
   {$ENDIF}
    function NewCursor: TDicCursor;
    function NewLookup(AMatchType: TMatchType): TDicLookupCursor;
    function GetRecord(Index: integer): TDicCursor;
    property Offline: boolean read FOffline write FOffline;
    property LoadOnDemand: boolean read FLoadOnDemand write FLoadOnDemand; //if set, load the dictionary only on demand
    property OnLoadStart: TNotifyEvent read FOnLoadStart write FOnLoadStart;
    property OnLoadEnd: TNotifyEvent read FOnLoadEnd write FOnLoadEnd;
    property OnLoadException: TNotifyExceptionEvent read FOnLoadException write FOnLoadException;

  end;

  TEntry = record
    text: FString;
    markers: TMarkers;
  end;
  PEntry = ^TEntry;

  TEntries = record
    items: array of TEntry;
    procedure Reset;
    procedure Add(const AText: FString; AMarkers: TMarkers);
    function GetCount: integer; inline;
    function ToString: FString;
    function ToEnrichedString: FString;
    function MergeMarkers: TMarkers;
    function HasMarker(const AMarker: TMarker): boolean;
    property Count: integer read GetCount;
  end;
  PEntries = ^TEntries;

  TDicCursor = class
  public
    dic: TJaletDic;

  protected
   { Copied from TJaletDic on Create() for code readability }
    CDict: TTextTableCursor;
    TDictIndex,
    TDictEnglish,
    TDictKanji,
    TDictPhonetic,
    TDictSort, //actually it's not "sort" but "romaji signature"
    TDictMarkers,
    TDictFrequency,
    TDictArticle
      :integer;

    CEntries: TTextTableCursor;
    TEntriesIndex,  //i in v5
    TEntriesEntry,  //x in v5
    TEntriesMarkers //s in v5
      :integer;

    stSort: PSeekObject;
    stSortReverse: PSeekObject;
    stKanji: PSeekObject;
    stKanjiReverse: PSeekObject;
    stIndex: PSeekObject;
    stDictArticle: PSeekObject;
    stEntriesIndex: PSeekObject;

  public
    constructor Create(ADic: TJaletDic);
    procedure SeekIndex(Value: integer);
    procedure SeekEntry(Value: integer);
    function GetIndex: integer;
    function GetKanji: FString;
    function GetPhonetic: FString;
    function GetSort: string;
    function GetFrequency: integer;
    function GetKanjiKanaMarkers: TMarkers;
    function GetArticle: integer;
    function GetArticleBody: FString; deprecated;
    function GetArticleMarkers: TMarkers; deprecated;
    function GetEntries: TEntries;

  end;

 { JaletDic cursor -- independent from basic cursor functionality }
  TDicIndexReader = class
  protected
    dic:TJaletDic;
    idx:TIndex;
    indexfrom,indexto:integer;
    procedure FindIndexStringV4(t:TIndexType;const locator:UnicodeString);
    procedure FindIndexStringV5(t:TIndexType;const locator:UnicodeString);
  public
    constructor Create(ADic: TJaletDic);
    procedure FindIndexString(t:TIndexType;const locator:UnicodeString);
    function ReadIndex:integer;
  end;

  TDicIndexCursorType = (
    ctDictIndex,  //index stores TDict references
    ctEntryIndex  //index stores TEntry references
  );

  TDicIndexCursor = class(TDicCursor)
  protected
    FType: TDicIndexCursorType;
    FReader: TDicIndexReader;
  public
    constructor Create(ADic: TJaletDic);
    destructor Destroy; override;
    procedure Find(t:TIndexType;const locator:UnicodeString);
    function Next: boolean;
  end;

  TDicLookupCursor = class(TDicCursor)
  public
    procedure LookupKanji(const val: FString); virtual; abstract;
    procedure LookupRomaji(const val: string); virtual; abstract;
    procedure LookupMeaning(const val: FString); virtual; abstract;
    function HaveMatch: boolean; virtual; abstract;
    procedure NextMatch; virtual; abstract;
  end;

 { Internal lookup state of the dictionary cursor }
  TDicLookupType = (ltNone,ltKanji,ltRomaji,ltMeaning);
  
 { Cursor + some cached info about the dictionary }
  TDicLookupCursorV4 = class(TDicLookupCursor)
  protected
    FIndexReader: TDicIndexReader;
    FLookupType: TDicLookupType;
    FMatchType: TMatchType;
    FValue: string; //can be FString
    function NextMeaningMatch: boolean;
    function NextAnywhereMatch: boolean;
  public
    constructor Create(ADic: TJaletDic; AMatchType: TMatchType);
    destructor Destroy; override;
    procedure LookupKanji(const val: FString); override;
    procedure LookupRomaji(const val: string); override;
    procedure LookupMeaning(const val: FString); override;
    function HaveMatch: boolean; override;
    procedure NextMatch; override;
  end;

 { V4 is mostly compatible, thanks to TDicCursor abstracting stuff away.
  This is for any fixes }
  TDicLookupCursorV5 = class(TDicLookupCursorV4)
  end;
  

  EDictionaryException = class(Exception);

const
  GROUP_NOTUSED = -1;
  GROUP_OFFLINE = 0;

type
  TDictionaryList = class(TList)
  protected
   //Lists in internal faster format
    FNotUsedDicts:string;
    FNotGroupDicts:array[1..5] of string;
    FOfflineDicts:string;
    function Get(Index: Integer): TJaletDic; reintroduce;
    procedure Put(Index: Integer; Item: TJaletDic); reintroduce;
    function GetDicts(Index: integer): string;
    procedure SetDicts(Index: integer; const Value: string);
  public
    Priority: TStringList;
    constructor Create;
    destructor Destroy; override;
    function FindIndex(AName: string): integer;
    function Find(AName: string): TJaletDic;
    procedure PutInGroup(dic: TJaletDic; group: TDictGroup; inGroup: boolean); overload; inline;
    procedure PutInGroup(dicname: string; group: TDictGroup; inGroup: boolean); overload;
    function IsInGroup(dic: TJaletDic; group: TDictGroup): boolean; overload; inline;
    function IsInGroup(dicname: string; group: TDictGroup): boolean; overload;
    property Items[Index: Integer]: TJaletDic read Get write Put; default;
   //Lists in external compatible format of older Wakans -- do not use except when reading/saving settings
    property NotGroupDicts[Index:integer]: string read GetDicts write SetDicts;
    property NotUsedDicts:string index GROUP_NOTUSED read GetDicts write SetDicts;
    property OfflineDicts:string index GROUP_OFFLINE read GetDicts write SetDicts;
  end;

var
  ignorel: TStringList; //words to ignore when indexing dictionaries
  DictFormatSettings: TFormatSettings; //to use in locate-neutral information

implementation
uses Forms, JWBLegacyMarkup;

{
Dictionary list
}

constructor TDictionaryList.Create;
begin
  inherited;
  Priority := TStringList.Create;
  Priority.CaseSensitive := false;
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

{ Proxy functions to get and set dictionary lists in old string format.
 Internally we can keep them as optimized as we want.
 Although currently we don't do much except add a comma in SetDicts. }
function TDictionaryList.GetDicts(Index: integer): string;
begin
  case Index of
    GROUP_NOTUSED: Result := FNotUsedDicts;
    GROUP_OFFLINE: Result := FOfflineDicts;
  else
    Result := FNotGroupDicts[Index];
  end;
 //Delete final ',' for compability with older Wakans (this is to be saved to registry)
  if (Length(Result)>0) and (Result[Length(Result)]=',') then
    SetLength(Result,Length(Result)-1);
end;

procedure TDictionaryList.SetDicts(Index: integer; const Value: string);
var val: string;
begin
  val := Value;
 { By convention, each entry starts with ',' and the list must always end with ','
  so that we can freely check against ',dictname,' }
  if (Length(val)<=0) or (val[Length(val)]<>',') then
    val := val+',';
  if (Length(val)>0) and (val[1]<>',') then
    val := ','+val;
  case Index of
    GROUP_NOTUSED: FNotUsedDicts := val;
    GROUP_OFFLINE: FOfflineDicts := val;
  else
    FNotGroupDicts[Index] := val;
  end;
end;

function TDictionaryList.IsInGroup(dic: TJaletDic; group: TDictGroup): boolean;
begin
  Result := IsInGroup(dic.name,group);
end;

function TDictionaryList.IsInGroup(dicname: string; group: TDictGroup): boolean;
begin
 //To properly match "dicname" in list, we check for commas around it
  case group of
    GROUP_NOTUSED: Result := pos(','+lowercase(dicname)+',',FNotUsedDicts)<>0;
    GROUP_OFFLINE: Result := pos(','+lowercase(dicname)+',',FOfflineDicts)<>0;
  else
    Result := pos(','+lowercase(dicname)+',',FNotGroupDicts[group])=0;
  end;
end;

procedure TDictionaryList.PutInGroup(dic: TJaletDic; group: TDictGroup; inGroup: boolean);
begin
  PutInGroup(lowercase(dic.name), group, inGroup);
end;

procedure AddToList(const name: string; var list: string; add: boolean);
var l_name: string;
begin
  l_name := lowercase(name);
  if add then begin
    if pos(','+l_name+',',list)=0 then list:=list+l_name+','; //list ends in comma
  end else begin
    if pos(','+l_name+',',list)>0 then delete(list,pos(','+l_name+',',list)+1,length(l_name)+1); //list ends in comma
  end;
end;

procedure TDictionaryList.PutInGroup(dicname: string; group: TDictGroup; inGroup: boolean);
begin
  case group of
    GROUP_NOTUSED: AddToList(dicname, FNotUsedDicts, inGroup);
    GROUP_OFFLINE: AddToList(dicname, FOfflineDicts, inGroup);
  else
    AddToList(dicname, FNotGroupDicts[group], not inGroup);
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
  sources:=nil;
  tested:=false;
  loaded:=false;
end;

destructor TJaletDic.Destroy;
begin
  if loaded then Unload;
  FreeAndNil(sources);
 {$IFDEF DIC_CURSOR_IN_TABLE}
  FreeAndNil(_intcur);
 {$ENDIF}
end;

{ JaletDic cannot be loaded without calling this function at least once,
 because it sets pname.
 But just in case, we auto-FillInfo on load too, if it was not yet filled. }
procedure TJaletDic.FillInfo(const packagefile:string);
var ps:TPackageSource;
begin
  pname:=packagefile;
  ps:=TPackageSource.Create(pname,791564,978132,978123);
  try
    FillInfo(ps);
  finally
    ps.Free;
  end;
end;

procedure TJaletDic.FillInfo(ps:TPackageSource);
var vs:TStringList;
  mf:TMemoryFile;
begin
  tested:=false;
  vs:=TStringList.Create;
  try
    if ps.GetFileList.IndexOf('dict.ver')<0 then
      raise EDictionaryException.Create('Unknown file structure');

    vs.LoadFromStream(ps.Files['dict.ver'].Lock);
    ps.Files['dict.ver'].Unlock;
    if vs[0]<>'DICT' then
      raise EDictionaryException.Create('Invalid DIC header');

    dicver:=strtoint(vs[1]);
    if dicver>5 then
      raise EDictionaryException.Create('Unsupported DIC version');
    if dicver<4 then
      raise EDictionaryException.Create('Outdated DIC structure - please download new DIC file');

    builddate:=strtoint(vs[2]);
    version:=vs[3];
    name:=vs[4];
    language:=vs[5][1];
    description:=vs[6];
    priority:=strtoint(vs[7]);
    entries:=strtoint(vs[8]);
    copyright:=vs[9];

    mf := ps['sources.lst'];
    if mf=nil then
      FreeAndNil(sources) //if it was created in earlier FillInfo
    else begin
      sources := TStringList.Create;
      sources.LoadFromStream(mf.Lock);
      mf.Unlock;
    end;

    hasWordIndex := ps.GetFileList.IndexOf('WordIdx.bin')>=0;
    hasCharIndex := ps.GetFileList.IndexOf('CharIdx.bin')>=0;

    tested:=true;
  finally
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
    if not tested then FillInfo(package); //although this shouldn't happen
    if not tested then raise Exception.Create('Cannot load dictionary info for "'+pname+'"');
    TDict:=TTextTable.Create(package,'Dict',true,Self.Offline);
    TDictIndex:=TDict.Field('Index');
    if dicver=4 then
      TDictEnglish:=TDict.Field('English')
    else
      TDictEnglish:=-1;
    TDictKanji:=TDict.Field('Kanji');
    TDictPhonetic:=TDict.Field('Phonetic');
    TDictSort:=TDict.Field('Sort');
    TDictMarkers:=TDict.Field('Markers');
    if dicver=5 then
      TDictArticle:=TDict.Field('Article')
    else
      TDictArticle:=-1;
    TDictFrequency:=TDict.Field('Frequency');

    if dicver=5 then begin
      TEntries:=TTextTable.Create(package,'Entries',true,Self.Offline);
      TEntriesIndex:=TEntries.Field('Index');
      TEntriesEntry:=TEntries.Field('Entry');
      TEntriesMarkers:=TEntries.Field('Markers');
    end else begin
      TEntries:=nil;
      TEntriesIndex:=-1;
      TEntriesEntry:=-1;
      TEntriesMarkers:=-1;
    end;
    
    mf:=package['WordIdx.bin'];
    if mf=nil then
      WordIdx := nil
    else begin
      if dicver=5 then
        WordIdx := TIndexV5.Create(mf.Lock)
      else
        WordIdx := TIndexV4.Create(mf.Lock);
      mf.Unlock;
    end;

    mf:=package['CharIdx.bin'];
    if mf=nil then
      CharIdx := nil
    else begin
      if dicver=5 then
        CharIdx := TIndexV5.Create(mf.Lock)
      else
        CharIdx := TIndexV4.Create(mf.Lock);
      mf.Unlock;
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
  _intcur:=TDicIndexReader.Create(Self);
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
  FreeAndNil(WordIdx);
  FreeAndNil(CharIdx);
  loaded:=false;
end;

procedure TJaletDic.SetupSeekObjects;
begin
  FstSort := TDict.GetSeekObject('Sort');
  FstSortReverse := TDict.GetSeekObject('<Sort');
  FstKanji := TDict.GetSeekObject('Kanji');
  FstKanjiReverse := TDict.GetSeekObject('<Kanji');
  if dicver=5 then begin
    FstIndex.ind_i := -1;
    FstDictArticle := TDict.GetSeekObject('Article');
    FstEntriesIndex := TEntries.GetSeekObject('Index');
  end else begin
    FstIndex := TDict.GetSeekObject('Index');
    FstDictArticle.ind_i := -1;
    FstEntriesIndex.ind_i := -1;
  end;

  stSort := @FstSort;
  stSortReverse := @FstSortReverse;
  stKanji := @FstKanji;
  stKanjiReverse := @FstKanjiReverse;
  if dicver=5 then begin
    stIndex := nil;
    stDictArticle := @FstDictArticle;
    stEntriesIndex := @FstEntriesIndex;
  end else begin
    stIndex := @FstIndex;
    stDictArticle := nil;
    stEntriesIndex := nil;
  end;
end;

function TJaletDic.SupportsFrequency: boolean;
begin
  Result := TDictFrequency<>-1;
end;

function TJaletDic.SupportsMarkers: boolean;
begin
  Result := (TDictMarkers<>-1) //older dicts apparently can't into markers
    or (dicver=5); //all v5 dicts can into markers :)
end;

{$IFDEF DIC_CURSOR_IN_TABLE}
procedure TJaletDic.FindIndexString(t:TIndexType;const locator:UnicodeString);
begin
  _intcur.FindIndexString(t,locator);
end;

function TJaletDic.ReadIndex:integer;
begin
  Result := _intcur.ReadIndex;
end;
{$ENDIF}

function TJaletDic.NewCursor: TDicCursor;
begin
  Self.Demand;
  Result := TDicCursor.Create(Self);
end;

function TJaletDic.NewLookup(AMatchType: TMatchType): TDicLookupCursor;
begin
  Self.Demand;
  case dicver of
    4: Result := TDicLookupCursorV4.Create(Self, AMatchType);
    5: Result := TDicLookupCursorV5.Create(Self, AMatchType);
  else
    raise Exception.Create('Invalid dictionary version');
  end;
end;

function TJaletDic.GetRecord(Index: integer): TDicCursor;
begin
  Result := NewCursor;
  Result.SeekIndex(Index);
end;


{
Cursor
}
constructor TDicCursor.Create(ADic: TJaletDic);
begin
  inherited Create();
  self.dic := ADic;
  self.dic.Demand; //can't delay this further or TTextTableCursor will get nil table

  self.CDict := TTextTableCursor.Create(dic.TDict);
  self.TDictIndex := ADic.TDictIndex;
  self.TDictEnglish := ADic.TDictEnglish;
  self.TDictKanji := ADic.TDictKanji;
  self.TDictPhonetic := ADic.TDictPhonetic;
  self.TDictSort := ADic.TDictSort;
  self.TDictMarkers := ADic.TDictMarkers;
  self.TDictFrequency := ADic.TDictFrequency;
  self.TDictArticle := ADic.TDictArticle;

  if dic.dicver=5 then
    self.CEntries := TTextTableCursor.Create(dic.TEntries)
  else
    self.CEntries := nil;
  self.TEntriesIndex := ADic.TEntriesIndex;
  self.TEntriesEntry := ADic.TEntriesEntry;
  self.TEntriesMarkers := ADic.TEntriesMarkers;
  
  self.stSort := ADic.stSort;
  self.stSortReverse := ADic.stSortReverse;
  self.stKanji := ADic.stKanji;
  self.stKanjiReverse := ADic.stKanjiReverse;
  self.stIndex := ADic.stIndex;
  self.stDictArticle := ADic.stDictArticle;
  self.stEntriesIndex := ADic.stEntriesIndex;
end;

procedure TDicCursor.SeekIndex(Value: integer);
begin
  if dic.dicver=5 then
    CDict.tcur := Value
  else
    CDict.Locate(stIndex, Value);
end;

procedure TDicCursor.SeekEntry(Value: integer);
begin
  if dic.dicver=5 then begin
    CEntries.tcur := Value;
    CDict.Locate(stDictArticle, CEntries.Int(TEntriesIndex));
  end else
    CDict.Locate(stIndex, Value); //on older dicts they're the same
end;

{ Returns unique index for current kanji-kana entry }
function TDicCursor.GetIndex: integer;
begin
  if dic.dicver=5 then
    Result := CDict.tcur
  else
    Result := CDict.Int(TDictIndex);
end;

function TDicCursor.GetKanji: FString;
begin
  Result := CDict.Str(TDictKanji);
end;

{ Returns kana }
function TDicCursor.GetPhonetic: FString;
begin
  Result := CDict.Str(TDictPhonetic);
end;

{ Returns romaji }
function TDicCursor.GetSort: string;
begin
  Result := CDict.Str(TDictSort);
end;

function TDicCursor.GetFrequency: integer;
begin
  if dic.TDictFrequency<>-1 then
    Result := CDict.Int(TDictFrequency)
  else
    Result := 1;
end;

{ Returns markers for current kanji-kana entry }
function TDicCursor.GetKanjiKanaMarkers: TMarkers;
begin
  case dic.dicver of
    4: Result := ''; //everything goes under entry markers
    5: Result := CDict.AnsiStr(TDictMarkers);
  end;
end;

{ Returns article index for current kanji-kana entry. }
function TDicCursor.GetArticle: integer;
begin
  case dic.dicver of
    4: Result := GetIndex;
    5: Result := CDict.Int(TDictArticle);
  else
    raise Exception.Create('Invalid dictionary version.');  
  end;
end;

{ Returns merged article body; deprecated }
function TDicCursor.GetArticleBody: FString;
begin
  Result := GetEntries.ToString;
end;

{ Returns merged markers for kanji-kana and all entries; deprecated }
function TDicCursor.GetArticleMarkers: TMarkers;
var art: integer;
begin
  case dic.dicver of
    4:
      if TDictMarkers<>-1 then
        Result := CDict.AnsiStr(TDictMarkers)
      else begin
        FConvertEdictEntry(GetArticleBody(), Result);
      end;
    5: begin
     //lump everything together as in old version. This is not correct though...
      Result := CDict.AnsiStr(TDictMarkers); //kana-kanji markers
      art := GetArticle;
      CEntries.Locate(stEntriesIndex, art); //every entry's markers
      while (not CEntries.EOF) and (CEntries.Int(TEntriesIndex)=art) do begin
        Result := Result + CEntries.AnsiStr(TEntriesMarkers);
        CEntries.Next;
      end;
    end;
  else
     raise Exception.Create('Invalid dictionary version.');  
  end;
end;

{ Returns all article entries with markers }
function TDicCursor.GetEntries: TEntries;
var art: integer;
begin
  case dic.dicver of
    4: begin
      SetLength(Result.items, 1);
      if TDictMarkers<>-1 then begin
        Result.items[0].text := fstr(CDict.Str(TDictEnglish));
        Result.items[0].markers := CDict.AnsiStr(TDictMarkers);
      end else begin
        Result.items[0].text := FConvertEdictEntry(fstr(CDict.Str(TDictEnglish)), Result.items[0].markers);
      end;
    end;
    5: begin
      SetLength(Result.items, 0);
      art := GetArticle;
      CEntries.Locate(stEntriesIndex, art); //every entry's markers
      while (not CEntries.EOF) and (CEntries.Int(TEntriesIndex)=art) do begin
        SetLength(Result.items, Length(Result.items)+1);
        with Result.items[Length(Result.items)-1] do begin
          text := CEntries.Str(TEntriesEntry);
          markers := CEntries.AnsiStr(TEntriesMarkers);
        end;
        CEntries.Next;
      end;
    end;
  else
     raise Exception.Create('Invalid dictionary version.');
  end;
end;

procedure TEntries.Reset;
begin
  SetLength(Self.items, 0);
end;

procedure TEntries.Add(const AText: FString; AMarkers: TMarkers);
begin
  SetLength(Self.items, Length(Self.items)+1);
  with Self.items[Length(Self.items)-1] do begin
    Text := AText;
    Markers := AMarkers;
  end;
end;

function TEntries.GetCount: integer;
begin
  Result := Length(items);
end;

function TEntries.ToString: FString;
var i: integer;
  ent: FString;
begin
  Result := '';
  for i := 0 to Length(items) - 1 do begin
    ent := items[i].text;
    if Result='' then
      Result := ent
    else
    if i=1 then
     //convert to multi-entry article
      Result := fstr('(1) ')+Result+fstr('; (2) ')+ent
    else
      Result := Result+fstr('; ('+IntToStr(i+1)+') ')+ent;
  end;
end;

function TEntries.ToEnrichedString: FString;
var i: integer;
  ent: FString;
begin
  Result := '';
  for i := 0 to Length(items) - 1 do begin
    ent := EnrichDictEntry(items[i].text, items[i].markers);
    if Result='' then
      Result := ent
    else
    if i=1 then
     //convert to multi-entry article
      Result := fstr('(1) ')+Result+fstr('; (2) ')+ent
    else
      Result := Result+fstr('; ('+IntToStr(i+1)+') ')+ent;
  end;
end;

function TEntries.MergeMarkers: TMarkers;
var i: integer;
begin
  Result := '';
  for i := 0 to Length(items) - 1 do
    Result := Result + items[i].markers;
end;

{ True if any of the entries has the specified marker. Entry markers do not apply
 to the whole entry set, but sometimes this is useful (e.g. if any of the meanings
 has written-in-kana flag, then we should do some additional lookups just by kana) }
function TEntries.HasMarker(const AMarker: TMarker): boolean;
var i: integer;
begin
  Result := false;
  for i := 0 to Length(items)-1 do
    if TestMarkers(AMarker, items[i].markers) then begin
      Result := true;
      break;
    end;
end;


{ Index Cursor }

constructor TDicIndexReader.Create(ADic: TJaletDic);
begin
  inherited Create;
  dic := ADic;
end;

procedure TDicIndexReader.FindIndexStringV4(t:TIndexType;const locator:UnicodeString);
var m: integer;
begin
  if t=itChar then begin
    idx := dic.CharIdx;
    m := idx.FindEntry(UnicodeToHex(PWideChar(locator),1));
  end else
  begin //4 AnsiChars
    idx := dic.WordIdx;
    m := idx.FindEntry(locator);
  end;

  if m<0 then
    indexfrom:=0
  else
    idx.ReadIndexEntryFromTo(m,indexfrom,indexto);
end;

procedure TDicIndexReader.FindIndexStringV5(t:TIndexType;const locator:UnicodeString);
var m: integer;
  u_str: UnicodeString;
begin
  u_str := locator;
  if t=itChar then begin
    idx := dic.CharIdx;
    MakeFixedLen(u_str, 4, WideChar(#00));
  end else begin
    idx := dic.WordIdx;
    MakeFixedLen(u_str, 4, WideChar(' '));
  end;
  m := idx.FindEntry(u_str);

  if m<0 then
    indexfrom:=0
  else
    idx.ReadIndexEntryFromTo(m,indexfrom,indexto);
end;

{ Always convert to Wide chars before calling this function. }
procedure TDicIndexReader.FindIndexString(t:TIndexType;const locator:UnicodeString);
begin
  if dic.dicver=5 then
    FindIndexStringV5(t,locator)
  else
    FindIndexStringV4(t,locator);
end;

function TDicIndexReader.ReadIndex:integer;
begin
  if (indexfrom<=0) or (indexfrom>=indexto) then
  begin
    result:=-1;
    exit;
  end;
  result:=idx.ReadIndexInfo(indexfrom);
  inc(indexfrom);
end;

constructor TDicIndexCursor.Create(ADic: TJaletDic);
begin
  inherited Create(ADic);
  FReader := TDicIndexReader.Create(ADic);
end;

destructor TDicIndexCursor.Destroy;
begin
  FreeAndNil(FReader);
  inherited;
end;

procedure TDicIndexCursor.Find(t:TIndexType;const locator:UnicodeString);
begin
  case t of
    itWord: FType:=ctEntryIndex;
    itChar: FType:=ctDictIndex;
  end;
  FReader.FindIndexString(t,locator);
end;

function TDicIndexCursor.Next: boolean;
var wif: integer;
begin
  wif := FReader.ReadIndex;
  Result := (wif>=0);
  if Result then
    if FType=ctDictIndex then
      Self.SeekIndex(wif)
    else
      Self.SeekEntry(wif);
end;


{
Lookup cursor.
These let you browse through a set of results.
}

constructor TDicLookupCursorV4.Create(ADic: TJaletDic; AMatchType: TMatchType);
begin
  inherited Create(ADic);
  FMatchType := AMatchType;
  FIndexReader := TDicIndexReader.Create(ADic);
end;

destructor TDicLookupCursorV4.Destroy;
begin
  FreeAndNil(CDict);
  FreeAndNil(FIndexReader);
  inherited Destroy;
end;

procedure TDicLookupCursorV4.LookupKanji(const val: FString);
begin
  FLookupType := ltKanji;
  FValue := val;  
  case FMatchType of
    mtMatchRight: begin
      CDict.SetOrder('<Kanji_Ind');
      CDict.Locate(stKanjiReverse,val);
    end;
    mtMatchAnywhere: begin
      CDict.SetOrder('Index_Ind');
      CDict.First;
      NextAnywhereMatch;
    end;
  else //left and exact
    CDict.SetOrder('Kanji_Ind');
    CDict.Locate(stKanji,val);
  end;
end;

procedure TDicLookupCursorV4.LookupRomaji(const val: string);
begin
  FLookupType := ltRomaji;
  FValue := val;  
  case FMatchType of
    mtMatchRight: begin
      CDict.SetOrder('<Phonetic_Ind');
      CDict.Locate(stSortReverse,val);
    end;
    mtMatchAnywhere: begin
      CDict.SetOrder('Index_Ind');
      CDict.First;
      NextAnywhereMatch;
    end;
  else //left and exact
    CDict.SetOrder('Phonetic_Ind');
    CDict.Locate(stSort,val);
  end;
end;

procedure TDicLookupCursorV4.LookupMeaning(const val: FString);
begin
  FLookupType := ltMeaning;
  FValue := lowercase(val);
  if not (FMatchType in [mtExactMatch, mtMatchLeft]) then
    FMatchType := mtMatchLeft; //other match types are not supported
  FIndexReader.FindIndexString(itWord,fstrtouni(FValue));
 //This mode requires auto-NextMatch at the start
  NextMatch();
 //Which may also terminate the search instantly if there are no matches -- see NextMatch()
end;

function TDicLookupCursorV4.HaveMatch: boolean;
var s_val: string;
begin
  if FLookupType in [ltKanji,ltRomaji] then
    if CDict.EOF then begin
      Result := false;
      exit;
    end;

  case FLookupType of
    ltKanji:
      case FMatchType of
        mtMatchLeft: Result := StartsStr(FValue, CDict.Str(TDictKanji));
        mtMatchRight: Result := EndsStr(FValue, CDict.Str(TDictKanji));
        mtExactMatch: Result := FValue=CDict.Str(TDictKanji);
      else //anywhere
        Result := pos(FValue,CDict.Str(TDictKanji))>0;
      end;

    ltRomaji:
      case FMatchType of
        mtMatchLeft: Result := StartsStr(FValue, CDict.Str(TDictSort));
        mtMatchRight: Result := EndsStr(FValue, CDict.Str(TDictSort));
        mtExactMatch: Result := FValue=CDict.Str(TDictSort);
      else //anywhere
        Result := pos(FValue,CDict.Str(TDictSort))>0;
      end;

    ltMeaning:
      Result := true; //we always have a match in Meaning -- see NextMatch()
  else
    Result := false; //not looking for anything
  end;
end;

procedure TDicLookupCursorV4.NextMatch;
begin
  case FLookupType of
    ltMeaning: begin
      if not NextMeaningMatch then
        FLookupType := ltNone; //lookup is over
    end
  else
    if (FLookupType in [ltKanji,ltRomaji]) and (FMatchType=mtMatchAnywhere) then
      NextAnywhereMatch
    else
      CDict.Next;
  end;
end;

function TDicLookupCursorV4.NextMeaningMatch: boolean;
var wif: integer;
  ts: string;
begin
  wif:=FIndexReader.ReadIndex;
  while wif>=0 do begin
    self.SeekEntry(wif);
   { Word index only works on first 4 bytes of the word, so we have to manually check after it. }
    ts:=lowercase(Self.GetArticleBody)+' ';
    case FMatchType of
      mtMatchLeft: if pos(FValue,ts)>0 then break;
      mtExactMatch: begin
        ts := ts + ' ';
       //TODO: support other word breaks in addition to ' ' and ','
        if (pos(lowercase(FValue)+' ',ts)>0) or (pos(lowercase(FValue)+',',ts)>0) then break;
      end;
    end;
    wif:=FIndexReader.ReadIndex;
  end;
  Result := (wif>=0);
end;

function TDicLookupCursorV4.NextAnywhereMatch: boolean;
begin
 { mtAnywhere searches can't use any index, so we scan through all the records -- very slow }
  if CDict.EOF then begin
    Result := false;
    exit;
  end;

  repeat
    CDict.Next;
  until CDict.EOF or HaveMatch;
  Result := not CDict.EOF;
end;


initialization
  ignorel:=TStringList.Create;
 {$IF CompilerVersion>=22}
 //use EN-US for locale neutral data
  DictFormatSettings := TFormatSettings.Create('en-us');
 {$ELSE}
 //older compilers only have obsolete function
  GetLocaleFormatSettings($0409, DictFormatSettings);
 {$IFEND}

finalization
  ignorel.Free;

end.
