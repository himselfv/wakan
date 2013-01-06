unit JWBDic;

interface
uses SysUtils, Classes, MemSource, TextTable, JWBStrings;

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
    tested:boolean;
    hasWordIndex:boolean;
    hasCharIndex:boolean;

  protected //Fields, populated on load
    TDict:TTextTable;
    TDictIndex, //i
    TDictEnglish, //s in v4, missing in v5
    TDictKanji, //x
    TDictPhonetic, //x
    TDictSort, //s
    TDictMarkers, //s in v4, missing in v5
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
    charidx,wordidx:pointer;
    charno,wordno:integer;
    charfsiz,wordfsiz:integer;

  public
    package:TPackageSource;
    loaded:boolean;
    pname:string;
    demandloaded:boolean;
    vocmode:integer;
   {$IFDEF DIC_CURSOR_IN_TABLE}
    _intcur: TDicIndexReader;
   {$ENDIF}
    constructor Create;
    destructor Destroy; override;
    procedure FillInfo(packagefile:string); virtual;
    procedure Load; virtual;
    procedure Unload; virtual;
    procedure Demand;
    function ReadIndexEntryV4(t:TIndexType;loc:integer):AnsiString;
    function ReadIndexEntryV5(t:TIndexType;loc:integer):int64;
    function ReadIndexInfo(t:TIndexType;loc:integer):integer;
   {$IFDEF DIC_CURSOR_IN_TABLE}
    procedure FindIndexString(t:TIndexType;const locator:UnicodeString); {$IFDEF INLINE}inline;{$ENDIF}
    function ReadIndex:integer; {$IFDEF INLINE}inline;{$ENDIF}
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
    TDictSort,
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
    stEntriesIndex: PSeekObject;

  public
    constructor Create(ADic: TJaletDic);
    procedure SeekIndex(Value: integer);
    function GetIndex: integer;
    function GetKanji: FString;
    function GetPhonetic: FString;
    function GetSort: string;
    function GetFrequency: integer;
    function GetArticle: integer;
    function GetArticleBody: FString;
    function GetArticleMarkers: string;

  end;

 { JaletDic cursor -- independent from basic cursor functionality }
  TDicIndexReader = class
  protected
    dic: TJaletDic;
    indextype:TIndexType;
    indexfrom,indexto:integer;
    procedure FindIndexStringV4(t:TIndexType;const locator:UnicodeString);
    procedure FindIndexStringV5(t:TIndexType;const locator:UnicodeString);
  public
    constructor Create(ADic: TJaletDic);
    procedure FindIndexString(t:TIndexType;const locator:UnicodeString);
    function ReadIndex:integer;
  end;

  TDicIndexCursor = class(TDicCursor)
  protected
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

  TDicLookupCursorV5 = TDicLookupCursorV4; //TODO!
  

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
    tested:=true;

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
    if dicver=4 then
      TDictEnglish:=TDict.Field('English')
    else
      TDictEnglish:=-1;
    TDictKanji:=TDict.Field('Kanji');
    TDictPhonetic:=TDict.Field('Phonetic');
    TDictSort:=TDict.Field('Sort');
    if dicver=4 then
      TDictMarkers:=TDict.Field('Markers')
    else
      TDictMarkers:=-1;
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
    
    WordIdx:=nil;
    mf:=package['WordIdx.bin'];
    if mf<>nil then
    begin
      GetMem(WordIdx,mf.Size-4);
      package.ReadRawData(WordNo,integer(mf.Position),4);
      package.ReadRawData(WordIdx^,integer(mf.Position)+4,mf.Size-4);
      wordfsiz:=(mf.Size div 4)-1;
    end;

    CharIdx:=nil;
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
  if dicver=5 then
    FstEntriesIndex := TEntries.GetSeekObject('Index')
  else
    FstEntriesIndex.ind_i := -1;

  stSort := @FstSort;
  stSortReverse := @FstSortReverse;
  stKanji := @FstKanji;
  stKanjiReverse := @FstKanjiReverse;
  stIndex := @FstIndex;
  if dicver=5 then
    stEntriesIndex := @FstEntriesIndex
  else
    stEntriesIndex := nil;
end;

function TJaletDic.SupportsFrequency: boolean;
begin
  Result := TDictFrequency<>-1;
end;

function TJaletDic.SupportsMarkers: boolean;
begin
  Result := TDictMarkers<>-1; //older dicts apparently can't into markers
end;

//4 bytes compared as AnsiString on v4
function TJaletDic.ReadIndexEntryV4(t:TIndexType;loc:integer):AnsiString;
var p:PAnsiChar;
begin
  SetLength(Result, 4);
  if t=itWord then p:=wordidx else p:=charidx;
  move(PAnsiChar(p+loc*4)^,Result[1],4);
end;

//8 bytes compared as int64 in v5
function TJaletDic.ReadIndexEntryV5(t:TIndexType;loc:integer):int64;
var p: PByte;
begin
  if t=itWord then p:=wordidx else p:=charidx;
  if dicver=5 then
    Result := PInt64(integer(p)+loc*8)^
  else
    Result := PInteger(integer(p)+loc*4)^;
end;

function TJaletDic.ReadIndexInfo(t:TIndexType;loc:integer):integer;
begin
  if t=itWord then
    Result := PInteger(integer(wordidx)+loc*4)^
  else
    Result := PInteger(integer(charidx)+loc*4)^;
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
  self.stEntriesIndex := ADic.stEntriesIndex;
end;

procedure TDicCursor.SeekIndex(Value: integer);
begin
  CDict.Locate(stIndex, Value);
end;

{ Returns unique index for current kanji-kana entry }
function TDicCursor.GetIndex: integer;
begin
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

function TDicCursor.GetArticleBody: FString;
var art: integer;
  entrycnt: integer;
  ent: FString;
begin
  case dic.dicver of
    4: Result := fstr(CDict.Str(TDictEnglish));
    5: begin
      Result := '';
      art := GetArticle;
      entrycnt := 0;
      CEntries.Locate(stEntriesIndex, art);
      while (not CEntries.EOF) and (CEntries.Int(TEntriesIndex)=art) do begin
        ent := CEntries.Str(TEntriesEntry);
        if Result='' then
          Result := ent
        else
        if entrycnt=1 then
         //convert to multi-entry article
          Result := fstr('(1) ')+Result+fstr('; (2) ')+ent
        else
          Result := Result + fstr('; ('+IntToStr(entrycnt+1)+') ')+ent;
        Inc(entrycnt);
        CEntries.Next;
      end;
    end;
  else
     raise Exception.Create('Invalid dictionary version.');  
  end;
end;

function TDicCursor.GetArticleMarkers: string;
var art: integer;
begin
  case dic.dicver of
    4:
      if TDictMarkers<>-1 then 
        Result := CDict.Str(TDictMarkers)
      else
        Result := '';
    5: begin
      Result := '';
      art := GetArticle;
      CEntries.Locate(stEntriesIndex, art);
      while (not CEntries.EOF) and (CEntries.Int(TEntriesIndex)=art) do begin
        Result := Result + CEntries.Str(TEntriesMarkers); //lump together as on old version. This is not correct though...
        CEntries.Next;
      end;
    end;
  else
     raise Exception.Create('Invalid dictionary version.');  
  end;
end;


{ Index Cursor }

constructor TDicIndexReader.Create(ADic: TJaletDic);
begin
  inherited Create;
  dic := ADic;
end;

procedure TDicIndexReader.FindIndexStringV4(t:TIndexType;const locator:UnicodeString);
var l,r,m,max:integer;
  a_str: AnsiString;
  val: AnsiString;
begin
  if t=itChar then begin
    a_str := AnsiString(UnicodeToHex(PWideChar(locator),1));
  end else
  begin //4 AnsiChars
    a_str := AnsiString(locator); //This calls WideCharToMultiByte so a good conversion
    MakeFixedLen(a_str, 4, ' ');
  end;

  l:=0;
  if t=itWord then max:=dic.wordno-1 else max:=dic.charno-1;
  r:=max;
  m:=l;
  while l<=r do
  begin
    m:=l+(r-l) div 2;
    val:=dic.ReadIndexEntryV4(t,m*2);
    if val=a_str then break;
    if val<a_str then l:=m+1 else r:=m-1;
  end;
  if l>r then
    indexfrom:=0
  else
  begin
    indexfrom:=dic.ReadIndexInfo(t,m*2+1)+(max+1)*2;
    if m<max then
      indexto:=dic.ReadIndexInfo(t,m*2+3)+(max+1)*2
    else
      if t=itWord then indexto:=dic.wordfsiz else indexto:=dic.charfsiz;
    indextype:=t;
  end;
end;

procedure TDicIndexReader.FindIndexStringV5(t:TIndexType;const locator:UnicodeString);
var l,r,m,max:integer;
  u_str: UnicodeString;
  loc_val: int64;
  val: int64;
begin
  if t=itChar then begin
    loc_val := PWord(locator)^ //1 char
  end else begin
    u_str := locator;
    MakeFixedLen(u_str, 4, ' ');
    loc_val := PInt64(u_str)^;
  end;

  l:=0;
  if t=itWord then max:=dic.wordno-1 else max:=dic.charno-1;
  r:=max;
  m:=l;
  while l<=r do
  begin
    m:=l+(r-l) div 2;
    val:=dic.ReadIndexEntryV5(t,m*2);
    if val=loc_val then break;
    if val<loc_val then l:=m+1 else r:=m-1;
  end;
  if l>r then
    indexfrom:=0 //not found
  else
  begin
    indexfrom:=dic.ReadIndexInfo(t,m*2+1)+(max+1)*2;
    if m<max then
      indexto:=dic.ReadIndexInfo(t,m*2+3)+(max+1)*2
    else
      if t=itWord then indexto:=dic.wordfsiz else indexto:=dic.charfsiz;
    indextype:=t;
  end;
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
  if (indexfrom=0) or (indexfrom>=indexto) then
  begin
    result:=0;
    exit;
  end;
  result:=dic.ReadIndexInfo(indextype,indexfrom);
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
  FReader.FindIndexString(t,locator);
end;

function TDicIndexCursor.Next: boolean;
var wif: integer;
begin
  wif := FReader.ReadIndex;
  Result := (wif<>0);
  if Result then
    Self.SeekIndex(wif);
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
var i_pos: integer;
  s_val: string;
begin
  if FLookupType in [ltKanji,ltRomaji] then
    if CDict.EOF then begin
      Result := false;
      exit;
    end;

  case FLookupType of
    ltKanji:
      case FMatchType of
        mtMatchLeft: Result := pos(FValue,CDict.Str(TDictKanji))=1;
        mtMatchRight: begin
          s_val := CDict.Str(TDictKanji);
          i_pos := pos(FValue, s_val);
          Result := (i_pos>0) and (i_pos=Length(s_val)-Length(FValue));
        end;
        mtExactMatch: Result := FValue=CDict.Str(TDictKanji);
      else //anywhere
        Result := pos(FValue,CDict.Str(TDictKanji))>0;
      end;

    ltRomaji:
      case FMatchType of
        mtMatchLeft: Result := pos(FValue,CDict.Str(TDictSort))=1;
        mtMatchRight: begin
          s_val := CDict.Str(TDictSort);
          i_pos := pos(FValue, s_val);
          Result := (i_pos>0) and (i_pos=Length(s_val)-Length(FValue)+1);
        end;
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
  while wif<>0 do begin
    CDict.Locate(stIndex,wif);
   { Word index only works on first 4 bytes of the word, so we have to manually check after it. }
    ts:=lowercase(CDict.Str(dic.TDictEnglish))+' ';
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
  Result := (wif<>0);
end;

function TDicLookupCursorV4.NextAnywhereMatch: boolean;
begin
 { mtAnywhere searches can't use any index, so we scan through all the records -- very slow }
  if EOF then begin
    Result := false;
    exit;
  end;

  repeat
    CDict.Next;
  until CDict.EOF or HaveMatch;
  Result := not EOF;
end;


initialization
  ignorel:=TStringList.Create;

finalization
  ignorel.Free;

end.
