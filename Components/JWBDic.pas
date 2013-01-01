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
//    procedure Build(edictfile,packagefile:string); virtual;
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

var
  ignorel: TStringList; //words to ignore when indexing dictionaries

implementation
uses Forms;

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
    if ps.GetFileList.IndexOf('dict.ver')<>-1 then
    begin
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
    end else
      raise EDictionaryException.Create('Unknown file structure');
  finally
    ps.Free;
    vs.Free;
  end;
end;

{
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
}

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
