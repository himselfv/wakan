unit TextTable;

interface
uses MemSource,Classes,SysUtils,Dialogs,Windows,JWBStrings,JWBIO;

{
Unicode status:
- Table formats aren't going to change and some fields in some tables are
 kept in Ansi hex.
- Strings are returned as FStrings (although not typed as such):
    Ansi field @ Ansi compiler     =>  Ansi contents
    Ansi field @ Unicode compiler  =>  Ansi contents in UnicodeString
    Wide field @ Ansi compiler     =>  Ansi hex contents
    Wide field @ Unicode compiler  =>  Unicode contents
 So basically, you get unchanged text from Ansi fields (and it might be hex)
 and FString from Unicode fields.
}

{$DEFINE CURSOR_IN_TABLE}
{ To navigate TTextTable you must use TTextTableCursor.
 However, old code used to call TTextTable.First/Next and so we keep a copy
 of a cursor code inside of a TTextTable. }

{$DEFINE TTSTATS}
{ Keep some potentially slow statistics about table and it's usage.
 Maybe this should only be limited to debug builds. }

const
  AllocDataBufferSz=65536;
  AllocOrderBufferSz=1024;
  AllocStructBufferSz=1024;

type
  TTextTable = class;
  TTextTableCursor = class;

  TTextTableIndex = record
    Data: pointer;
    DataSize: integer;
    RecCnt: integer;
    procedure Reset;
    procedure Free;
    procedure NeedAtLeast(const sz: integer);
    function Read(const pos: integer): integer; inline;
    procedure Write(const pos: integer; const val: integer); inline;
    procedure AddRecs(const ACount: integer); inline;
    procedure ShiftLeft(const pos, len: integer; const shift: integer); inline;
    procedure ShiftRight(const pos, len: integer; const shift: integer); inline;
    function FindEntry(const val: integer): integer;
  end;
  PTextTableIndex = ^TTextTableIndex;

 { Seek table reference to speed up Locate()
  Retrieve it once, use instead of a textual name.
  For common tables and common seeks, global references are kept. }
  TSeekObject = record
    ind_i: integer; //seek index
     { Seek table number is one lower than seek index.
      First seek is always the "default one". }
    fld_i: integer; //field index
    reverse: boolean;
  end;
  PSeekObject = ^TSeekObject;

 { Seek table descriptions, parsed to this binary format on load. }
  TSeekFieldDescription = record
    index: integer; //field index
    reverse: boolean;
    strdata: boolean //treat field data as string
  end;
  PSeekFieldDescription = ^TSeekFieldDescription;
  TSeekFieldDescriptions = array of TSeekFieldDescription;

  TSeekDescription = record
    Name: string;
    Declaration: string;
    fields: TSeekFieldDescriptions;
  end;
  PSeekDescription = ^TSeekDescription;

  TFieldDescription = record
    Name: string;
    Size: ShortInt;
     { >=0: fixed size field
        <0: (-1) + offset in size data for a record }
    DataType: char;
    AutoInc: integer;
     { -1: no autoinc
        0: determine autoinc on next request
       >0: next autoinc value }
  end;
  PFieldDescription = ^TFieldDescription;

  TFieldList = array of TFieldDescription;
  TSeekList = class
  private
    FTable: TTextTable;
    FItems: array of TSeekDescription;
    function GetItem(const Index: integer): PSeekDescription; inline;
    function GetCount: integer; inline;
    function ParseSeekDescription(const AFormula: string): TSeekDescription;
  public
    constructor Create(ATable: TTextTable);
    destructor Destroy; override;
    procedure Clear;
    procedure Add(const AItem: TSeekDescription); overload;
    procedure Add(const AFormula: string); overload;
    procedure Delete(const AIndex: integer);
    procedure Insert(const APos: integer; const AItem: TSeekDescription); overload;
    procedure Insert(const APos: integer; const AFormula: string); overload;
    function Find(const AName: string): integer;
    property Items[const Index: integer]: PSeekDescription read GetItem; default;
    property Count: integer read GetCount;
  end;

  TTextTable=class
  protected
   { Table can be loaded/created in-memory or it can be disk-backed.
    When loading from disk, these must be set. }
    Source:TPackageSource;
    Offline:boolean; //do not load all data into memory, only table headers

   { Create-time flags }
    NeedCreateData:boolean; //set if info-schema asked for CreateData
    Precounted:boolean;
     { Set if info-schema does not specify that the table structure is in new format.
      See comments to UpgradeToPrecounted() }

   { Table settings -- loaded from table configuration }
    rawindex:boolean;
    prebuffer:boolean;
    wordfsize:boolean; //Table stores variable field sizes in Words, not Bytes

   //Internal functions, call public ones instead
    constructor Create; overload;
    procedure LoadInfo(AInfo:TStream);
    procedure CreateData();
    procedure LoadData(const AFilename:string;ARawRead:boolean); //only from Source
    procedure UpgradeToPrecounted();
    procedure PrintDataStats();
    procedure SetupTable; virtual;

  public
    constructor Create(ASource:TPackageSource;const AFilename:string;ARawread,AOffline:boolean); overload;
    constructor Create(AInfo:TStream); overload;
    constructor Create(AInfoLines:array of string); overload;
    destructor Destroy; override;
    procedure WriteTable(const filename:string;nodelete:boolean);

  protected
   {
    Data: Table data. Raw data, indexed by struct.
    Struct: Record headers.
      Each headers is of size struct_recsz == 5+#_of_variable_fields*[1_or_2]:
        Deleted: boolean, 1 byte
        DataOffset: integer, 4 byte
        For every variable-length field:
          DataSize: 1 byte OR
          DataSize: 2 bytes IF wordfsize=true
    Index: sorting orders
      array [0..IndexCount-1] x [0..IndexRecCount-1] of RecordNo: integer;
    Note that IndexRecCount may be < than RecCount if not all records are commited.
   }
    data,struct:pointer;
    databuffer,structbuffer:integer; //free memory available in data and struct ptrs
    datalen:integer; //data length (used)
    struct_recsz:integer; //struct record size
    datafpos:integer; //start of data in file, when using offline mode
   //Buffer expansion
    procedure ReserveData(const sz: integer);
    procedure ReserveStruct(const sz: integer);

  protected
   { Fields }
    FFields: TFieldList;
    varfields_sz:integer; //cached size of variable-length fields part in a struct record

    function GetIsAutoIncField(const fieldIdx: integer): boolean;
    procedure SetIsAutoIncField(const fieldIdx: integer; const Value: boolean);
    function GetAutoIncValue(const fieldIdx: integer): integer;
    function FindMaxFieldValue(const fieldIdx: integer): integer;
  public
    function GetFieldIndex(const field:string):integer;
    function Field(const field:string):integer; inline; //shortcut; use FieldIndex for clarity
    property Fields: TFieldList read FFields;
    property IsAutoIncField[const fieldIdx: integer]: boolean read GetIsAutoIncField
      write SetIsAutoIncField;

  protected
   {
    Index tables.
    The point of an index table is to keep records sorted in a particular order.
    This allows both for Seeking and Ordering of records.
   }
    FIndexes: array of TTextTableIndex;

   {
    Index table names and descriptions are loaded from $SEEKS section, they are
    in the form "field1+field2+field3" -- see ParseSeekDescription()
    First seek description is always for NO_INDEX, however it's named.
      seeks[0] = NO_INDEX = DEFAULT_ORDER
      seeks[1] = index[0]
      seeks[2] = index[1]
      etc.
    You may use '0' as a $SEEK formula for seeks[0] if you don't need it.
   }
    FSeeks: TSeekList;

   { Orders refer to the same index data, only they have different names:
      orders[0] = index[0] == seeks[1]
      orders[1] = index[1] == seeks[2]
      etc.
    There's no way to restore index description just from an order, we need
    to have matching $SEEKS entry. Therefore Order without matching Seek is
    read-only. }
    FOrders:TStringList;

   //Generates a signature for a record in sort order (i.e. FIELD1_FIELD2_FIELD3)
    function SortRec(r:integer;const fields:TSeekFieldDescriptions):string; overload; //newer cooler version
    function SortRec(r:integer;seekIndex:integer):string; overload; inline;

   { Comparison functions. TextTable has two comparison modes:
     - Field comparison (string or integer, used by LocateRecord)
     - Signature comparison for a set of fields (used by TrueLocateRecord, CheckIndex, Reindex)
     The way strings are compared depends on rawindex variable. }
    function ttCompareStr(const s1, s2: string): integer; inline;
    function ttCompareSign(const s1, s2: string): integer; inline;

    function TrueLocateRecord(order: integer; const sign:string; out pos: integer):boolean;

  public
    function GetSeekObject(seek: string): TSeekObject;
   { Even if the index is multi-field, LocateRecord() only seeks by first field.
    This works, but you have to move Next() and check other fields manually }
    function LocateRecord(seek: PSeekObject; value:string; out idx: integer):boolean; overload;
    function LocateRecord(seek: PSeekObject; value:integer; out idx: integer):boolean; overload;
    function TransOrder(rec,order:integer):integer; inline;
    procedure Reindex;
    function CheckIndex(const index: integer): boolean; overload;
    function CheckIndices: integer; overload;
    function CheckIndex: boolean; overload;
    function HasIndex(const index:string):boolean;
    property Seeks: TSeekList read FSeeks;
    property Orders: TStringList read FOrders;

  public //Import/export
    procedure ExportToText(t:TStreamEncoder;ord:string);
    procedure ImportFromText(t:TStreamDecoder);

  protected
    reccount:integer;
    numberdeleted:integer;
    nocommit:boolean; //do not update indexes on Add/Edit/Delete. You'll have to Reindex later.
    function TestIdxPos(const order: integer; const pos: integer): boolean;
  public
    function AddRecord(values:array of string): integer;
    procedure DeleteRecord(RecNo: integer);
    procedure EditRecord(RecNo: integer; const AFields:array of byte;
      const AValues:array of string; JustInserted:boolean=false);
    procedure Commit(RecNo:integer; JustInserted: boolean = false);
    property NoCommitting:boolean read nocommit write nocommit;
    function FilterPass(rec:integer;fil:string):boolean;
    function IsDeleted(rec:integer):boolean;
    function NewCursor: TTextTableCursor;
    property RecordCount:integer read reccount;

  protected //Field reading
    function GetDataOffset(rec:integer;field:integer):integer;
  public
    function GetFieldSize(recno,field:integer):word;
    function GetField(rec:integer;field:integer):string;
    function GetIntField(rec:integer;field:integer;out value:integer):boolean;
    function GetAnsiField(rec:integer;field:integer):AnsiString;
    procedure SetField(rec:integer;field:integer;const value:string);
    procedure SetAnsiField(rec:integer;field:integer;const value:AnsiString);

 {$IFDEF CURSOR_IN_TABLE}
  protected
    _intcur: TTextTableCursor;
  public
    procedure First;
    procedure Next;
    function EOF:boolean;
    procedure SetOrder(const index:string);
    procedure SetFilter(const filtr:string);
    function Test(const fil:string):boolean;
  public
    procedure Insert(values:array of string);
    procedure Delete; inline;
    procedure Edit(const fields:array of byte;const values:array of string);
    function Locate(seek: PSeekObject; const value:string):boolean; overload; inline;
    function Locate(const seek,value:string):boolean; overload; inline;
    function Locate(seek:PSeekObject; const value:integer):boolean; overload; inline;
    function Locate(const seek:string; const value:integer):boolean; overload; inline;
  public
    function Str(field:integer):string; inline;
    function Int(field:integer):integer; inline;
    function TrueInt(field:integer):integer; inline;
    function Bool(field:integer):boolean; inline;
   {$IFDEF UNICODE}
    function Fch(field:integer):WideChar; inline;
   {$ELSE}
    function Fch(field:integer):string; inline;
   {$ENDIF}
 {$ENDIF}

  end;

 { Implements navigation through TextTable records. }
  TTextTableCursor = class
  protected
    FTable: TTextTable;
    cur: integer; //current position in index
    curorder:integer;
    filter:string;
  public
    tcur: integer; //current RecdNo (for this index position)
    constructor Create(ATable: TTextTable);
    procedure First;
    procedure Next;
    function EOF:boolean;
    procedure SetOrder(const index:string);
    procedure SetFilter(const filtr:string);
    function Test(const fil:string):boolean;
    property Table: TTextTable read FTable;

  public
   { Write access to TTextTable MUST BE EXCLUSIVE. ALWAYS. NO EXCEPTIONS. }
    procedure Insert(values:array of string);
    procedure Delete; inline;
    procedure Edit(const fields:array of byte; const values:array of string);

  public
    function Locate(seek:PSeekObject; const value:string):boolean; overload; inline;
    function Locate(const seek,value:string):boolean; overload; inline;
    function Locate(seek:PSeekObject; const value:integer):boolean; overload; inline;
    function Locate(const seek:string; const value:integer):boolean; overload; inline;

  public
    function Str(field:integer):string; inline;
    function AnsiStr(field:integer):AnsiString; inline;
    function Int(field:integer):integer; inline;
    function TrueInt(field:integer):integer; inline;
    function Bool(field:integer):boolean; inline;
   {$IFDEF UNICODE}
    function Fch(field:integer):WideChar; inline;
   {$ELSE}
    function Fch(field:integer):string; inline;
   {$ENDIF}
    function Dehex(field:integer):FString; inline;

  end;

procedure ShowAllocStats;

function BoolToStr(b: boolean): string;

implementation

resourcestring
  eReadOnlyIndexes = 'This table is missing definitions for some of its indexes. '
      +'Changes are impossible with such tables.';

{$IFDEF TTSTATS}
var
  dataalloc,structalloc,indexalloc:integer;
  statlist:TStringList;
{$ENDIF}

procedure ShowAllocStats;
begin
 {$IFDEF TTSTATS}
  statlist.SaveToFile('tablestats.txt');
  winexec('notepad.exe tablestats.txt',SW_SHOW);
 {$ELSE}
  raise Exception.Create('Table level statistics not available in this build.');
 {$ENDIF}
end;

function OffsetPtr(source: pointer; ofs: integer): PByte; inline;
begin
  Result := PByte(IntPtr(source)+ofs);
end;

procedure MoveOfs(source,dest:pointer;ofssource,ofsdest:integer;size:integer);
var sp,dp:pbyte;
begin
  sp:=source;
  sp:=PByte(IntPtr(sp)+ofssource); //older Delphi versions can't add pointers
  dp:=dest;
  dp:=PByte(IntPtr(dp)+ofsdest);
  move(sp^,dp^,size);
end;

//That's how TextTable stores bools
function BoolToStr(b:boolean): string;
begin
  if b then Result:='T' else Result:='F';
end;


procedure TTextTableIndex.Reset;
begin
  Self.Data := nil;
  Self.DataSize := 0;
  Self.RecCnt := 0;
end;

procedure TTextTableIndex.Free;
begin
  FreeMem(Self.Data);
  Self.DataSize := 0;
  Self.RecCnt := 0;
end;

procedure TTextTableIndex.NeedAtLeast(const sz: integer);
begin
  if DataSize>=sz then exit;
  DataSize := Trunc(DataSize*1.6)+10; //standard growth
  if sz>DataSize then //not enough
    DataSize := sz;
  ReallocMem(Self.Data, Self.DataSize);
end;

function TTextTableIndex.Read(const pos: integer): integer;
begin
  Result := PInteger(OffsetPtr(Self.Data,pos*4))^;
end;

procedure TTextTableIndex.Write(const pos: integer; const val: integer);
begin
  PInteger(OffsetPtr(Self.Data,pos*4))^ := val;
end;

procedure TTextTableIndex.AddRecs(const ACount: integer);
begin
  RecCnt := RecCnt + ACount;
  NeedAtLeast(RecCnt*4);
end;

procedure TTextTableIndex.ShiftLeft(const pos, len: integer; const shift: integer);
begin
  Move(
    PByte(IntPtr(Self.Data)+pos*4)^,
    PByte(IntPtr(Self.Data)+(pos-shift)*4)^,
    len*4);
end;

procedure TTextTableIndex.ShiftRight(const pos, len: integer; const shift: integer);
begin
  Move(
    PByte(IntPtr(Self.Data)+pos*4)^,
    PByte(IntPtr(Self.Data)+(pos+shift)*4)^,
    len*4);
end;

{ Locates first occurence of a record reference in an index.
 Normally, it would be the only occurence. }
function TTextTableIndex.FindEntry(const val: integer): integer;
var i: integer;
begin
  Result := -1;
  for i:=0 to RecCnt-1 do
    if Read(i)=val then begin
      Result := i;
      break;
    end;
end;


constructor TSeekList.Create(ATable: TTextTable);
begin
  inherited Create;
  FTable := ATable;
end;

destructor TSeekList.Destroy;
begin
  inherited Destroy;
end;

procedure TSeekList.Clear;
begin
  SetLength(FItems, 0);
end;

procedure TSeekList.Add(const AItem: TSeekDescription);
begin
  SetLength(FItems, Length(FItems)+1);
  FItems[Length(FItems)-1] := AItem;
end;

function TSeekList.ParseSeekDescription(const AFormula: string): TSeekDescription;
var i:integer;
  fx:string;
  reverse:boolean;
  desc: PSeekFieldDescription;
  fld: string;
begin
  Result.Name := AFormula;
  if pos('+',Result.Name)>0 then
    system.delete(Result.Name,pos('+',Result.Name),MaxInt);
  Result.Declaration := AFormula;

  fld := AFormula;
  SetLength(Result.fields, 0);
  while length(fld)>0 do
  begin
    if pos('+',fld)>0 then
    begin
      fx:=copy(fld,1,pos('+',fld)-1);
      system.delete(fld,1,length(fx)+1);
    end else
    begin
      fx:=fld;
      fld:='';
    end;
    reverse:=false;
    if fx[1]='<' then
    begin
      system.delete(fx,1,1);
      reverse:=true;
    end;
    i:=FTable.GetFieldIndex(fx);
    if i=-1 then raise Exception.Create('Unknown seek field '+fx+' (TTextTable.Commit).');

    SetLength(Result.fields, Length(Result.fields)+1);
    desc := @Result.fields[Length(Result.fields)-1];
    desc.index := i;
    desc.reverse := reverse;
    desc.strdata := (FTable.Fields[i].DataType='s') or (FTable.Fields[i].DataType='x')
  end;
end;

{ Parses seek formula taken from .info file and adds seek definition }
procedure TSeekList.Add(const AFormula: string);
var item: TSeekDescription;
begin
 //First seekbuild is sometimes '0' and we don't actually seek by it
  if (Self.Count=0) and (AFormula='0') then begin
    item.Name := AFormula;
    item.Declaration := AFormula;
    SetLength(item.fields, 0);
  end else
    item := ParseSeekDescription(AFormula);
  Add(item);
end;

procedure TSeekList.Delete(const AIndex: integer);
begin
  Move(FItems[AIndex+1], FItems[AIndex], SizeOf(FItems[AIndex])*(Length(FItems)-AIndex-1));
  ZeroMemory(@FItems[Length(FItems)-1], SizeOf(FItems[AIndex]));
  SetLength(FItems, Length(FItems)-1);
end;

procedure TSeekList.Insert(const APos: integer; const AItem: TSeekDescription);
begin
  SetLength(FItems, Length(FItems)+1);
  Move(FItems[APos], FItems[APos+1], SizeOf(FItems[APos])*(Length(FItems)-APos-1));
  ZeroMemory(@FItems[APos], SizeOf(FItems[APos]));
  FItems[APos] := AItem;
end;

procedure TSeekList.Insert(const APos: integer; const AFormula: string);
var item: TSeekDescription;
begin
  item := ParseSeekDescription(AFormula);
  Insert(APos, item);
end;

function TSeekList.GetItem(const Index: integer): PSeekDescription;
begin
  Result := @FItems[index];
end;

function TSeekList.GetCount: integer;
begin
  Result := Length(FItems);
end;

function TSeekList.Find(const AName: string): integer;
var i: integer;
begin
  Result := -1;
  for i := 0 to Length(FItems) - 1 do
    if SameText(FItems[i].Name, AName) then begin
      Result := i;
      break;
    end;
end;


//Basic Create() to be called by all customized versions
constructor TTextTable.Create;
begin
  inherited Create;
  SetLength(FFields, 0);
  FSeeks:=TSeekList.Create(Self);
  FOrders:=TStringList.Create;
  data:=TStringList.Create;
 {$IFDEF CURSOR_IN_TABLE}
  _intcur := TTextTableCursor.Create(Self);
 {$ENDIF}
  nocommit:=false;
  numberdeleted:=0;
  rawindex:=false;
  self.offline:=false;
end;

{ Loads the table schema and, where possible, the data from disk.
 When ASource is set, AFilename is a file path inside it. }
constructor TTextTable.Create(ASource:TPackageSource;const AFileName:string;ARawread,AOffline:boolean);
var mf: TMemoryFile;
  ms: TStream;
begin
  Self.Create();
  Self.Source:=ASource;
  Self.Offline:=AOffline;

 {$IFDEF TTSTATS}
  if Source<>nil then
    statlist.Add(source.FileName+'->'+AFilename)
  else
    statlist.Add('->'+AFilename);
 {$ENDIF}

 { When Source is not set, the file is assumed to be from native FS. }
  if Source<>nil then
  begin
    mf:=Source[AFilename+'.info'];
    if mf=nil then raise Exception.Create('TextTable: Important file missing.');
    ms:=mf.Lock;
  end else begin
    mf:=nil;
    ms:=TFileStream.Create(AFilename+'.info',fmOpenRead);
  end;

  LoadInfo(ms);

  if mf<>nil then begin
    mf.Unlock;
    ms := nil;
  end else
    FreeAndNil(ms);

  if NeedCreateData then begin
    CreateData();
    NeedCreateData := false;
  end else
    LoadData(AFilename, ARawRead);

  PrintDataStats();
  SetupTable();
end;

{ Creates a new empty table according to a info-schema taken from a stream }
constructor TTextTable.Create(AInfo:TStream);
begin
  Self.Create();
  Self.Offline:=false; //can only create data in memory at this time
  LoadInfo(AInfo);
  CreateData();
 {$IFDEF TTSTATS}
  statlist.Add('New table created.');
 {$ENDIF}
  SetupTable();
end;

{ Sometimes in code it's handier to specify schema line by line }
constructor TTextTable.Create(AInfoLines:array of string);
var ms: TMemoryStream;
  a_line: AnsiString;
  i: integer;
begin
  ms := TMemoryStream.Create;
  try
    for i := Low(AInfoLines) to High(AInfoLines) do begin
      a_line := AnsiString(AInfoLines[i]+#13#10);
      ms.Write(a_line[1], Length(a_line)*SizeOf(AnsiChar));
    end;
    ms.Seek(0, soFromBeginning);
    Self.Create(ms);
  finally
    FreeAndNil(ms);
  end;
end;

{ Reads the database schema from an .info file.  }
procedure TTextTable.LoadInfo(AInfo:TStream);
var sl:TStringList;
  section:char; //current section when parsing info
  i,j:integer;
  tmpSeeks:TStringList;
begin
  sl:=TStringList.Create;
  sl.LoadFromStream(AInfo);
  NeedCreateData:=false;

  tmpSeeks := TStringList.Create;

  section:=' ';
  prebuffer:=false;
  Precounted:=false;
  wordfsize:=false;
  varfields_sz:=0;
  for i:=0 to sl.Count-1 do
  begin
    if sl[i]='$FIELDS' then section:='f';
    if sl[i]='$ORDERS' then section:='o';
    if sl[i]='$SEEKS' then section:='s';
    if sl[i]='$PREBUFFER' then prebuffer:=true;
    if sl[i]='$PRECOUNTED' then Precounted:=true;
    if sl[i]='$CREATE' then NeedCreateData:=true;
    if sl[i]='$RAWINDEX' then rawindex:=true;
    if sl[i]='$WORDFSIZE' then wordfsize:=true;
    if (length(sl[i])>0) and (sl[i][1]<>'$') then
    case section of
      'f': begin
       //Field declaration
        j := Length(FFields);
        SetLength(FFields,j+1);
        FFields[j].DataType := sl[i][1];
        FFields[j].Name := copy(sl[i],2,MaxInt);
        FFields[j].AutoInc := -1; //disabled by default
        case FFields[j].DataType of
         //Dynamic size fields
          's', 'x': begin
            FFields[j].Size := (-1) + (-1)*varfields_sz;
            if wordfsize then
              inc(varfields_sz,2)
            else
              inc(varfields_sz,1);
          end;
         //Static size fields
          'b': FFields[j].Size := 1;
          'w': FFields[j].Size := 2;
          'i': FFields[j].Size := 4;
          'l': FFields[j].Size := 1;
        else
          FFields[j].Size := 0;
        end;
      end;
      's': tmpSeeks.Add(sl[i]);
       { Will parse fields later when all schema is loaded -
        at this point some fields may not yet be available }
      'o':orders.Add(sl[i]);
    end;
  end;
  struct_recsz := 5+varfields_sz;
  sl.Free;

 //Parse seek fields -- after schema has been loaded
  for i:=0 to tmpSeeks.Count-1 do
    FSeeks.Add(tmpSeeks[i]);
  FreeAndNil(tmpSeeks);
end;

{ Initialized empty table indexes and data in memory.
 Only info-schema is required to be parsed by this point. }
procedure TTextTable.CreateData();
var IndexCount, i: integer;
begin
  reccount:=0;
  if prebuffer then
    databuffer := AllocDataBufferSz
  else
    databuffer := 0;
  GetMem(data,databuffer);
  IndexCount := Max(FOrders.Count, FSeeks.Count-1);
  SetLength(FIndexes, IndexCount);
  for i := 0 to IndexCount - 1 do
    FIndexes[i].Reset;
  if prebuffer then
    structbuffer := AllocStructBufferSz
  else
    structbuffer := 0;
  GetMem(struct,structbuffer);
  datalen:=0;
  Precounted:=true; //new structure format
end;

{ Loads table indexes and data from Source.
 Info-schema is required to be parsed by this point. When Offline==true, only
 loads parts of data. }
procedure TTextTable.LoadData(const AFilename:string;ARawRead:boolean);
var bufsize,dsize:integer;
  mf:TMemoryFile;
  ms:TStream;
  index:pointer;
  IndexCount,IndexSize,i:integer;
 {$IFDEF TTSTATS}
  totalloc:integer;
 {$ENDIF}
begin
  if prebuffer then
    bufsize := AllocDataBufferSz
  else
    bufsize := 0;
  if Source=nil then
    ARawRead:=false; //can't have raw read from package without package

  if Source<>nil then
  begin
    mf:=Source[AFilename+'.data'];
    if mf=nil then raise Exception.Create('TextTable: Important file missing.');
    if not ARawRead then
      ms:=mf.Lock;
  end else begin
    mf:=nil;
    ms:=TFileStream.Create(AFilename+'.data',fmOpenRead);
  end;

  if not ARawread then
  begin
    databuffer:=bufsize;
    GetMem(data,ms.Size+bufsize);
    datalen:=ms.Size-4;
    ms.Read(data^,4);
    moveofs(data,@reccount,0,0,4);
    ms.Read(data^,datalen);

    if mf<>nil then begin
      mf.Unlock;
      ms := nil;
    end else
      FreeAndNil(ms);
  end else
  begin
    if offline then GetMem(data,4) else GetMem(data,mf.Size+bufsize);
    datalen:=mf.Size-4;
    source.ReadRawData(data^,integer(mf.Position),4);
    datafpos:=IntPtr(mf.Position)+4;
    moveofs(data,@reccount,0,0,4);
    if not offline then
    begin
      source.ReadRawData(data^,integer(mf.Position)+4,datalen);
    end;
  end;

  {$IFDEF TTSTATS}
  dataalloc:=dataalloc+(datalen+bufsize) div 1024;
  statlist.Add('  Records: '+inttostr(reccount));
  statlist.Add('  Indexes: '+inttostr(orders.Count));
  statlist.Add('  Data size: '+inttostr((datalen+bufsize) div 1000)+'k');
  totalloc:=datalen+bufsize;
  {$ENDIF}

  if Source<>nil then
  begin
    mf:=Source[AFilename+'.index'];
    if mf=nil then raise Exception.Create('TextTable: Important file missing.');
    if not ARawRead then
      ms:=mf.Lock;
  end else begin
    mf:=nil;
    ms:=TFileStream.Create(AFilename+'.index',fmOpenRead);
  end;

  if not ARawread then
  begin
    GetMem(index,ms.Size);
    ms.Read(index^,ms.Size);
    dsize := ms.Size;

    if mf<>nil then begin
      mf.Unlock;
      ms := nil;
    end else
      FreeAndNil(ms);
  end else
  begin
    GetMem(index,mf.Size);
    source.ReadRawData(index^,integer(mf.Position),mf.Size);
    dsize := mf.Size;
  end;

 { Split indices }
  try
    IndexCount := Max(FOrders.Count, FSeeks.Count-1);
    IndexSize := RecCount * 4;
    if dsize<>IndexCount*IndexSize then
      raise Exception.Create('Invalid index size');
    SetLength(FIndexes, IndexCount);
    for i := 0 to IndexCount - 1 do begin
      FIndexes[i].Reset;
      FIndexes[i].RecCnt := RecCount;
      FIndexes[i].NeedAtLeast(IndexSize);
      MoveOfs(index, FIndexes[i].Data, i*IndexSize, 0, IndexSize);
    end;
  finally
    FreeMem(index);
  end;

  {$IFDEF TTSTATS}
  indexalloc:=indexalloc+dsize div 1024;
  statlist.Add('  Indexes size: '+inttostr(dsize div 1000)+'k');
  totalloc:=totalloc+dsize;
  {$ENDIF}

  if prebuffer then
    bufsize := AllocStructBufferSz
  else
    bufsize := 0;
  structbuffer:=bufsize;

  if Source<>nil then
  begin
    mf:=Source[AFilename+'.struct'];
    if mf=nil then raise Exception.Create('TextTable: Important file missing.');
    if not ARawRead then
      ms:=mf.Lock;
  end else begin
    mf:=nil;
    ms:=TFileStream.Create(AFilename+'.struct',fmOpenRead);
  end;

  if not ARawread then
  begin
    GetMem(struct,ms.Size+bufsize);
    ms.Read(struct^,ms.Size);
    dsize := ms.Size;

    if mf<>nil then begin
      mf.Unlock;
      ms := nil;
    end else
      FreeAndNil(ms);
  end else
  begin
    GetMem(struct,mf.Size+bufsize);
    source.ReadRawData(struct^,integer(mf.Position),mf.Size);
    dsize := mf.Size;
  end;

  {$IFDEF TTSTATS}
  structalloc:=structalloc+(dsize+bufsize) div 1024;
  if not precounted then
    statlist.Add('  Structure size: '+inttostr((dsize+bufsize+reccount*5) div 1000)+'k')
  else
    statlist.Add('  Structure size: '+inttostr((dsize+bufsize) div 1000)+'k');
  totalloc:=totalloc+dsize+bufsize;
  statlist.Add('  Total size: '+inttostr(totalloc div 1000)+'k');
  {$ENDIF}

  if not Precounted then
    UpgradeToPrecounted;
end;

{ There apparently was an older format of table structure which did not include
 a pointer to record data with each record header.
 This function upgrades older structure to modern format by re-scanning all data. }
procedure TTextTable.UpgradeToPrecounted;
var
  bufsize:integer;
  oldstruct:pointer; //to free at the end
  oldstructptr:PByte; //current position
  structptr_rec:PByte; //start of curret record header
  structptr:PByte; //current position in header
  curfield:integer;
  fsize:word; //field size
  dataptr:integer;
begin
  if Precounted then exit;
  if offline then
    raise Exception.Create('OFFLINE table must be PRECOUNTED!');

 {$IFDEF TTSTATS}
  structalloc:=structalloc+(reccount*5) div 1024;
 {$ENDIF}
  oldstruct:=struct;
  oldstructptr:=oldstruct;

  struct:=nil;
  if prebuffer then
    bufsize := AllocStructBufferSz
  else
    bufsize := 0;
  GetMem(struct,reccount*struct_recsz+bufsize);
  structptr:=struct;

  dataptr:=0;
  curfield:=0;
  structptr_rec:=struct;
  while dataptr<datalen do
  begin
    if curfield=0 then begin
      structptr := structptr_rec;
      PByte(structptr)^ := 0; //1 byte
      Inc(structptr);
      PInteger(structptr)^ := dataptr; //4 bytes
      Inc(structptr, 4);
    end;

   //Field type -> size formula might change someday, but this deals with old
   //format where it'll always stay like this:
    case fields[curfield].DataType of
      'b':fsize:=1;
      'w':fsize:=2;
      'i':fsize:=4;
      'l':fsize:=1;
      'x','s':begin
        fsize:=PByte(oldstructptr)^;
        Inc(oldstructptr);
        if wordfsize then begin
          PWord(structptr)^ := fsize;
          Inc(structptr,2);
        end else begin
          PByte(structptr)^ := fsize;
          Inc(structptr,1);
        end;
      end;
    else
      fsize := 0;
    end;
    Inc(dataptr,fsize);

    Inc(curfield);
    if curfield>=Length(fields) then begin
      curfield := 0;
      Inc(structptr_rec, struct_recsz);
    end;
  end;
  FreeMem(oldstruct);

  if Integer(structptr_rec)-Integer(struct)>reccount*struct_recsz then
    raise Exception.Create('Table is corrupt.');
  if dataptr<>datalen then
    raise Exception.Create('Table is corrupt.');

  Precounted := true;
end;

{ Called after successful load or create to let descendants analyze field
 structure and cache all required field indexes, seeks etc. }
procedure TTextTable.SetupTable;
begin
end;

{ Outputs total size of all data in every column into stats log.
 Maybe we should make this run only on debug? I bet it's slow. }
procedure TTextTable.PrintDataStats();
{$IFDEF TTSTATS}
var i, j, cn: integer;
{$ENDIF}
begin
 {$IFDEF TTSTATS}
  for i:=0 to Length(fields)-1 do
  begin
    cn:=0;
    for j:=0 to reccount-1 do inc(cn,GetFieldSize(j,i));
    statlist.Add('  Field '+fields[i].Name+': '+inttostr(cn div 1000)+'k');
  end;
  statlist.Add('');
 {$ENDIF}
end;

procedure TTextTable.WriteTable(const filename:string;nodelete:boolean);
procedure wfbuf(var f:file;buf:pointer;var bufpos:integer;buflimit:integer;source:pointer;sourcepos,sourcelength:integer);
begin
  if bufpos+sourcelength<buflimit then
  begin
    moveofs(source,buf,sourcepos,bufpos,sourcelength);
    inc(bufpos,sourcelength);
  end else
  begin
    blockwrite(f,buf^,bufpos);
    bufpos:=0;
    moveofs(source,buf,sourcepos,bufpos,sourcelength);
    inc(bufpos,sourcelength);
  end;
end;
var t:textfile;
    i,j,k:integer;
    il:TStringList;
    b:byte;
    fsz:word;
    f1,f2:file;
    bufdata,bufstruct,buforder:pointer;
    bufdatapos,bufstructpos,buforderpos:integer;
    c:char;
    l:integer;
    w:word;
    mypos:integer;
begin
  if offline then
    raise Exception.Create('Cannot save to "'+filename+'": cannot write offline table.');
  assignfile(t,filename+'.info');
  rewrite(t);
  writeln(t,'$TEXTTABLE');
  if prebuffer then writeln(t,'$PREBUFFER');
  writeln(t,'$PRECOUNTED');
  if rawindex then writeln(t,'$RAWINDEX');
  if wordfsize then writeln(t,'$WORDFSIZE');
  writeln(t,'$FIELDS');
  for i:=0 to Length(fields)-1 do writeln(t,fields[i].DataType+fields[i].Name);
  writeln(t,'$ORDERS');
  for i:=0 to orders.Count-1 do writeln(t,orders[i]);
  writeln(t,'$SEEKS');
  for i:=0 to Seeks.Count-1 do writeln(t,seeks[i].Declaration);
  closefile(t);
  il:=TStringList.Create;
  j:=0;
  assignfile(f1,filename+'.data');
  rewrite(f1,1);
  assignfile(f2,filename+'.struct');
  rewrite(f2,1);
  getmem(bufdata,64000);
  getmem(bufstruct,64000);
  getmem(buforder,64000);
  if nodelete then
  begin
    i:=recordcount;
    blockwrite(f1,i,4);
    blockwrite(f1,data^,datalen);
    blockwrite(f2,struct^,reccount*struct_recsz);
    closefile(f1);
    closefile(f2);
    assignfile(f1,filename+'.index');
    rewrite(f1,1);
    for i := 0 to Length(FIndexes) - 1 do
      blockwrite(f1,FIndexes[i].Data^,FIndexes[i].RecCnt*4);
    closefile(f1);
  end else
  begin
    bufdatapos:=0; bufstructpos:=0; buforderpos:=0;
    i:=recordcount-numberdeleted;
    wfbuf(f1,bufdata,bufdatapos,64000,@i,0,4);
    mypos:=0;
    for i:=0 to recordcount-1 do
    begin
      moveofs(struct,@b,i*struct_recsz,0,1);
      if b=1 then
        il.Add('DEAD')
      else begin
        wfbuf(f2,bufstruct,bufstructpos,64000,@b,0,1);
        wfbuf(f2,bufstruct,bufstructpos,64000,@mypos,0,4);
        il.Add(inttostr(j));
        inc(j);
        w:=0;
        moveofs(struct,@l,i*struct_recsz+1,0,4);
        for k:=0 to Length(fields)-1 do
        begin
          fsz:=GetFieldSize(i,k);
          c:=fields[k].DataType;
          if (c='x') or (c='s') then
            if wordfsize then
              wfbuf(f2,bufstruct,bufstructpos,64000,@fsz,0,2)
            else
              wfbuf(f2,bufstruct,bufstructpos,64000,@fsz,0,1);
          wfbuf(f1,bufdata,bufdatapos,64000,data,l+w,fsz);
          w:=w+fsz;
          mypos:=mypos+fsz;
        end;
      end;
    end;
    blockwrite(f1,bufdata^,bufdatapos);
    blockwrite(f2,bufstruct^,bufstructpos);
    closefile(f1);
    closefile(f2);

    assignfile(f1,filename+'.index');
    rewrite(f1,1);
    for i:=0 to orders.Count-1 do for j:=0 to recordcount-1 do
    begin
      l:=TransOrder(j,i);
      if il[l]<>'DEAD' then
      begin
        l:=strtoint(il[l]);
        wfbuf(f1,buforder,buforderpos,64000,@l,0,4);
      end;
    end;
    blockwrite(f1,buforder^,buforderpos);
    closefile(f1);
  end;
  freemem(bufdata,64000);
  freemem(bufstruct,64000);
  freemem(buforder,64000);
end;

destructor TTextTable.Destroy;
var i: integer;
begin
  FreeAndNil(FOrders);
  freemem(data);
  for i := 0 to Length(FIndexes) - 1 do
    FIndexes[i].Free;
  SetLength(FIndexes, 0);
  freemem(struct);
  FreeAndNil(FSeeks);
end;

function TTextTable.NewCursor: TTextTableCursor;
begin
  Result := TTextTableCursor.Create(Self);
end;


{
GetFieldSize()
Heavily used function. Should be very optimized.
}
function TTextTable.GetFieldSize(recno,field:integer):word;
var sz:ShortInt;
begin
  sz:=fields[field].Size;
  if sz>=0 then
    Result:=word(sz)
  else
    if wordfsize then
      Result := PWord(OffsetPtr(struct, recno*struct_recsz+(-sz-1)+5))^
    else
      Result := PByte(OffsetPtr(struct, recno*struct_recsz+(-sz-1)+5))^
end;

function TTextTable.GetDataOffset(rec:integer;field:integer):integer;
var i,ii:integer;
begin
  ii:=rec*struct_recsz;
  Result:=PInteger(OffsetPtr(struct, ii+1))^;
  for i:=0 to field-1 do Result:=Result+GetFieldSize(rec,i);
end;

//Swaps bytes in every word
//sz is the number of bytes
procedure SwapBytes(pb: PByte; sz: integer); inline;
var b: byte;
  i: integer;
begin
  //Swap bytes
  for i := 0 to sz div 2 - 1 do begin
    b := PByteArray(pb)[2*i+0];
    PByteArray(pb)[2*i+0] := PByteArray(pb)[2*i+1];
    PByteArray(pb)[2*i+1] := b;
  end;
end;

{
Read the field value for a record.
This is a HEAVILY used function, so let's be as fast as we can.
Everything which is already in memory is read from memory directly. If we're
working in offline mode, it's read from the disc in one go.
}
function TTextTable.GetField(rec:integer;field:integer):string;
{$IFNDEF UNICODE}
const HexChars: AnsiString = '0123456789ABCDEF';
{$ENDIF}
var ofs:integer;
  sz:word;
  tp:char; //field type. Char because FieldTypes is string.
  b:byte;
  w:word;
  l:integer;
  c: AnsiChar;
 {$IFDEF UNICODE}
  ansi_s: AnsiString;
 {$ELSE}
  pb: PByte;
  pc: PChar;
 {$ENDIF}
begin
  if rec>=reccount then
    raise Exception.Create('Read beyond!');
  ofs:=GetDataOffset(rec,field);
  tp:=fields[field].DataType;
  sz:=GetFieldSize(rec,field);
  case tp of
  'b': begin
     if offline then
       source.ReadRawData(b,datafpos+ofs,1)
     else
       b := OffsetPtr(data, ofs)^;
     result:=inttostr(b);
   end;
  'w':begin
     if offline then
       source.ReadRawData(w,datafpos+ofs,2)
     else
       w := PWord(OffsetPtr(data, ofs))^;
     result:=inttostr(w);
   end;
  'i':begin
     if offline then
       source.ReadRawData(l,datafpos+ofs,4)
     else
       l := PInteger(OffsetPtr(data, ofs))^;
     result:=inttostr(l);
   end;
  'l':begin
     if offline then
       source.ReadRawData(c,datafpos+ofs,1)
     else
       c := PAnsiChar(OffsetPtr(data, ofs))^;
     if (c>#0) and (c<>'F') and (c<>'f') then c:='T' else c:='F';
     result:=Char(c);
   end;
  //AnsiString.
  's':begin
    {$IFNDEF UNICODE}
     SetLength(Result, sz);
     if offline then
       source.ReadRawData(PAnsiChar(Result)^,datafpos+ofs,sz)
     else
       move(OffsetPtr(data, ofs)^, PAnsiChar(Result)^, sz);
    {$ELSE}
     SetLength(ansi_s, sz);
     if offline then
       source.ReadRawData(PAnsiChar(ansi_s)^,datafpos+ofs,sz)
     else
       move(OffsetPtr(data, ofs)^, PAnsiChar(ansi_s)^, sz);
     Result := string(ansi_s);
    {$ENDIF}
   end;
  //UnicodeString. On non-unicode compilers reads as hex, thus 'x'.
  'x': begin
    {$IFDEF UNICODE}
     SetLength(Result, sz div 2 + sz mod 2);
     if offline then
       source.ReadRawData(PWideChar(Result)^,datafpos+ofs,sz)
     else
       move(OffsetPtr(data, ofs)^, PWideChar(Result)^, sz);
     SwapBytes(PByte(Result), sz);
    {$ELSE}
     SetLength(Result, 2*sz);
     if offline then begin
      //If we're offline we beter read everything in one go!
      //Position it in the second half of the buffer so that it won't be
      //overwritten too early when we rewrite the string.
       source.ReadRawData(Result[sz+1],datafpos+ofs,sz);
       pb := PByte(@Result[sz+1]);
     end else
      //If we have data in memory, let's just use it from there.
       pb := OffsetPtr(data, ofs);
     pc := PChar(@Result[1]);
     for l:=0 to sz-1 do begin
       pc^ := char(HexChars[1 + pb^ shr 4]);
       Inc(pc);
       pc^ := char(HexChars[1 + pb^ and $0F]);
       Inc(pc);
       Inc(pb);
     end;
    {$ENDIF}
   end;
  end;
end;

//If the field is numeric, reads it without conversions,
//else does the usual "read string + StrToInt" routine.
function TTextTable.GetIntField(rec:integer;field:integer;out value: integer):boolean;
var tp: char;
  ofs: integer;
  b: byte;
  w: word;
  l: integer;
  s: string;
begin
  tp:=fields[field].DataType;
  case tp of
  'b': begin
     ofs:=GetDataOffset(rec,field);
     if offline then
       source.ReadRawData(b,datafpos+ofs,1)
     else
       b := OffsetPtr(data, ofs)^;
     value:=b;
     Result:=true;
   end;
  'w':begin
     ofs:=GetDataOffset(rec,field);
     if offline then
       source.ReadRawData(w,datafpos+ofs,2)
     else
       w := PWord(OffsetPtr(data, ofs))^;
     value:=w;
     Result:=true;
   end;
  'i':begin
     ofs:=GetDataOffset(rec,field);
     if offline then
       source.ReadRawData(l,datafpos+ofs,4)
     else
       l := PInteger(OffsetPtr(data, ofs))^;
     value:=l;
     Result:=true;
   end;
  else
    s := GetField(rec,field);
    if (length(s)>0) and (s[length(s)]='''') then system.delete(s,length(s),1);
    if (length(s)>0) and (s[1]='''') then system.delete(s,1,1);
    Result := TryStrToInt(s, value);
  end;
end;

{ Useful when you don't want the Unicode->Ansi conversion to interfere, such as for RawByteStrings }
function TTextTable.GetAnsiField(rec:integer;field:integer):AnsiString;
var ofs:integer;
  sz:word;
  tp:char; //field type. Char because FieldTypes is string.
begin
  if rec>=reccount then
    raise Exception.Create('Read beyond!');
  ofs:=GetDataOffset(rec,field);
  tp:=fields[field].DataType;
  sz:=GetFieldSize(rec,field);
  case tp of
  //AnsiString.
  's':begin
     SetLength(Result, sz);
     if offline then
       source.ReadRawData(PAnsiChar(Result)^,datafpos+ofs,sz)
     else
       move(OffsetPtr(data, ofs)^, PAnsiChar(Result)^, sz);
   end;
  else
    raise Exception.Create('Unsupported field type in GetAnsiField');
  end;
end;

//Setting fields is not supported for offline dictionaries.
//It's only used for user data anyway, which is never offline.
procedure TTextTable.SetField(rec:integer;field:integer;const value:string);
var ofs:integer;
  sz:word;
  tp:char;
  b:byte;
  w:word;
  l:integer;
 {$IFDEF UNICODE}
  ansi_s: AnsiString;
  value_c: UnicodeString;
 {$ELSE}
  i: integer;
  pb: PByte;
  pc: PChar;
 {$ENDIF}
begin
  if rec>=reccount then
    raise Exception.Create('Write beyond!');
  ofs:=GetDataOffset(rec,field);
  tp:=fields[field].DataType;
  sz:=GetFieldSize(rec,field);

  case tp of
  'b': begin
     if TryStrToInt(value, l) then
       b := l
     else
       b := 0;
     PByte(OffsetPtr(data, ofs))^ := b;
   end;
  'w': begin
     if TryStrToInt(value, l) then
       w := l
     else
       w := 0;
     PWord(OffsetPtr(data, ofs))^ := w;
   end;
  'i': begin
     if not TryStrToInt(value, l) then
       l := 0;
     PInteger(OffsetPtr(data, ofs))^ := l;
   end;
  'l': begin
     if upcase(value[1])='T' then b:=1 else b:=0;
     PByte(OffsetPtr(data, ofs))^ := b;
   end;
  's': begin
    {$IFNDEF UNICODE}
     if sz>0 then
       move(PAnsiChar(value)^, OffsetPtr(data, ofs)^, sz);
    {$ELSE}
     if sz>0 then begin
       ansi_s := AnsiString(value);
       move(PAnsiChar(ansi_s)^, OffsetPtr(data, ofs)^, sz);
     end;
    {$ENDIF}
   end;
  'x': begin
    {$IFNDEF UNICODE}
     pb := OffsetPtr(data, ofs);
     pc := PChar(value);
     for i := 0 to sz - 1 do begin
       pb^ := HexCharCode(pc^) shl 4;
       Inc(pc);
       pb^ := pb^ + HexCharCode(pc^);
       Inc(pc);
       Inc(pb);
     end;
    {$ELSE}
     if sz>0 then begin
       value_c := value; //can't mess with someone else's string
       UniqueString(value_c);
       SwapBytes(PByte(value_c), sz);
       move(PWideChar(value_c)^, OffsetPtr(data, ofs)^, sz);
     end;
    {$ENDIF}
   end;
  end;
end;

{ See comments for GetAnsiField }
procedure TTextTable.SetAnsiField(rec:integer;field:integer;const value:AnsiString);
var ofs:integer;
  sz:word;
  tp:char;
begin
  if rec>=reccount then
    raise Exception.Create('Write beyond!');
  ofs:=GetDataOffset(rec,field);
  tp:=fields[field].DataType;
  sz:=GetFieldSize(rec,field);

  case tp of
  's': begin
     if sz>0 then
       move(PAnsiChar(value)^, OffsetPtr(data, ofs)^, sz);
   end;
  else
    raise Exception.Create('Unsupported field type in SetAnsiField');
  end;
end;


{$IFDEF CURSOR_IN_TABLE}
function TTextTable.Str(field:integer):string;
begin
  Result := _intcur.Str(field);
end;

function TTextTable.Int(field:integer):integer;
begin
  Result := _intcur.Int(field);
end;

function TTextTable.TrueInt(field:integer):integer;
begin
  Result := _intcur.TrueInt(field);
end;

function TTextTable.Bool(field:integer):boolean;
begin
  Result := _intcur.Bool(field);
end;

//Returns one character (in older versions nothing is guaranteed but it was always this way)
{$IFDEF UNICODE}
function TTextTable.Fch(field:integer):WideChar;
begin
  Result := _intcur.Fch(field);
end;
{$ELSE}
function TTextTable.Fch(field:integer):string;
begin
  Result := _intcur.Fch(field);
end;
{$ENDIF}
{$ENDIF}



function TTextTable.FilterPass(rec:integer;fil:string):boolean;
var c,i:integer;
    p1,p2,el:string;
    cx:char;
begin
  try
    if fil='' then
    begin
      result:=true;
      exit;
    end;
    if fil[1]='(' then
    begin
      c:=0;
      i:=1;
      repeat
        if fil[i]='(' then inc(c);
        if fil[i]=')' then dec(c);
        inc(i);
      until c=0;
      p1:=copy(fil,2,i-3);
      p2:='';
      el:='';
      system.delete(fil,1,length(p1)+2);
      if i<length(fil) then
      begin
        el:=copy(fil,1,pos('(',fil)-1);
        system.delete(fil,1,length(el));
        p2:=fil;
      end;
      if el='' then result:=FilterPass(rec,p1) else
      if UpperCase(el)=' AND ' then result:=FilterPass(rec,p1) and FilterPass(rec,p2)
      else if UpperCase(el)=' OR ' then result:=FilterPass(rec,p1) or FilterPass(rec,p2)
      else result:=false;
    end else
    begin
      if pos('=',fil)<>0 then
      begin
        p1:=copy(fil,1,pos('=',fil)-1);
        cx:='=';
      end else if pos('<',fil)<>0 then
      begin
        p1:=copy(fil,1,pos('<',fil)-1);
        cx:='<';
      end else
      begin
        p1:=copy(fil,1,pos('>',fil)-1);
        cx:='>';
      end;
      system.delete(fil,1,length(p1)+1);
      if fil[1]='''' then system.delete(fil,1,1);
      if fil[length(fil)]='''' then system.delete(fil,length(fil),1);
      result:=false;
      if (cx='=') and (UpperCase(GetField(rec,strtoint(p1)))=UpperCase(fil)) then result:=true;
      if (cx='<') and (strtoint(GetField(rec,strtoint(p1)))<strtoint(fil)) then result:=true;
      if (cx='>') and (strtoint(GetField(rec,strtoint(p1)))>strtoint(fil)) then result:=true;
    end;
  except result:=false; end;
end;

{ Retrieves a record number by a seek table id (order) and a seek table record number (rec).
  The table has to be loaded. }
function TTextTable.TransOrder(rec,order:integer):integer;
begin
  if order=-1 then
    Result:=rec
  else
  if rec>=FIndexes[order].RecCnt then
    Result:=rec //not yet indexed record
  else
    Result:=FIndexes[order].Read(rec);
end;

{$IFDEF CURSOR_IN_TABLE}
procedure TTextTable.First;
begin
  _intcur.First;
end;

function TTextTable.Test(const fil:string):boolean;
begin
  Result := _intcur.Test(fil);
end;

procedure TTextTable.Next;
begin
  _intcur.Next;
end;

function TTextTable.EOF:boolean;
begin
  Result := _intcur.EOF;
end;

procedure TTextTable.SetOrder(const index:string);
begin
  _intcur.SetOrder(index);
end;

procedure TTextTable.SetFilter(const filtr:string);
begin
  _intcur.SetFilter(filtr);
end;
{$ENDIF}


function ReverseString(s:string):string;
var i:integer;
begin
  result:='';
  for i:=length(s) downto 1 do result:=result+s[i];
end;

function TTextTable.GetSeekObject(seek: string): TSeekObject;
begin
  Result.ind_i:=Seeks.Find(seek);
  if Result.ind_i<0 then raise Exception.Create('Cannot find seek object "'+seek+'"');
  Result.reverse:=false;
  if (seek[1]='<') then
  begin
    system.delete(seek,1,1);
    Result.reverse:=true;
  end;
  Result.fld_i:=Self.GetFieldIndex(seek);
end;

{ LocateRecord():
 Locates first matching record by using a given seek table.
 Returns true and record index in seek table if there are matches,
 or false and record index for record just after where the match would have occured.
 To convert seek index to record index call TransOrder(idx, seek.ind_i) }
{ NOTE that LocateRecord only uses values for first field in a formula.
 For full formula location (required when changing index, for ex.) use TrueLocateRecord }
function TTextTable.LocateRecord(seek: PSeekObject; value:string; out idx: integer):boolean;
var sn:integer;       //seek table number
  fn:integer;         //field number
  reverse:boolean;
  l,r,c:integer;
  s:string;
  RecNo:integer;
  IndexRecCount:integer;
begin
 { Seek table number is one lower than seek index. First seek is always the "default one" }
  sn := seek.ind_i-1;
  fn := seek.fld_i;
  reverse := seek.reverse;

  idx := -1;
  Result := false;
  if (sn<-1) or (fn<0) then
    exit;

  if reverse then value:=ReverseString(value);
  value := uppercase(value);

  idx := RecCount+1; //if there are no records this'll be used

  if sn<0 then
    IndexRecCount := RecCount
  else
    IndexRecCount := FIndexes[sn].RecCnt;

 //Initiate binary search
  l:=0;
  r:=IndexRecCount-1;
  if l<=r then repeat
    c:=((r-l) div 2)+l;

    s:=GetField(TransOrder(c,sn),fn);
    if reverse then
      s:=ReverseString(s);
    if ttCompareStr(value,uppercase(s))<=0 then
      r:=c
    else
      l:=c+1;

    if l>=r then
    begin
      if (l=r) and (l=IndexRecCount-1) then begin //the only case when R was never checked
        s:=GetField(TransOrder(l,sn),fn);
        if reverse then
          s:=ReverseString(s);
        if ttCompareStr(value,uppercase(s))>0 then
          Inc(l);
      end;
      idx:=l;
      RecNo:=TransOrder(idx,sn);
      while (idx<IndexRecCount) and IsDeleted(RecNo) do
      begin
        inc(idx);
        RecNo:=TransOrder(idx,sn);
      end;
      Result := idx<IndexRecCount;
      if Result then begin
        s:=GetField(RecNo,fn);
        Result:=(ttCompareStr(value,uppercase(s))=0);
      end;
      exit;
    end;
  until false;
end;

{ Same but for numbers. May at times be faster (if the field is actually numberic in the DB and not string) }
function TTextTable.LocateRecord(seek: PSeekObject; value:integer; out idx: integer):boolean;
var sn:integer;       //seek table number
  fn:integer;         //field number
  l,r,c:integer;
  i_s: integer;      //integer value for "s", when number==true
  RecNo: integer;
  IndexRecCount: integer;
begin
 { Seek table number is one lower than seek index. First seek is always the "default one" }
  sn := seek.ind_i-1;
  fn := seek.fld_i;
 //"reverse" is ignored for numeric lookups

  idx := -1;
  Result := false;
  if (sn<-1) or (fn<0) then
    exit;

  idx := RecCount+1; //if there are no records this'll be used
  if sn<0 then
    IndexRecCount := RecCount
  else
    IndexRecCount := FIndexes[sn].RecCnt;

 //Initiate binary search
  l:=0;
  r:=IndexRecCount-1;
  if l<=r then repeat
    c:=((r-l) div 2)+l;

    if not GetIntField(TransOrder(c,sn),fn,i_s) then
      r := c
    else
      if value<=i_s then r:=c else l:=c+1;

    if l>=r then
    begin
      if (l=r) and (l=IndexRecCount-1) then begin //the only case when R was never checked
        if GetIntField(TransOrder(l,sn),fn,i_s) and (value>i_s) then
          Inc(l);
      end;
      idx:=l;
      RecNo:=TransOrder(idx,sn);
      while (idx<IndexRecCount) and (IsDeleted(RecNo)) do
      begin
        inc(idx);
        RecNo:=TransOrder(idx,sn);
      end;
      Result := (idx<IndexRecCount) and GetIntField(RecNo,fn,i_s) and (value=i_s);
      exit;
    end;
  until false;
end;

function TTextTable.TrueLocateRecord(order: integer; const sign:string; out pos: integer):boolean;
var idx: PTextTableIndex;
  l,r,c:integer;
  lastCmp:integer;
begin
  idx := @FIndexes[order];
  pos := idx.RecCnt; //if there are no records this'll be used
  Result := false;

  l:=0;
  r:=idx.RecCnt-1;
  if l<=r then repeat
    c:=((r-l) div 2)+l;

    lastCmp:=ttCompareSign(sign,SortRec(TransOrder(c,order),order+1));
    if lastCmp<=0 then
      r:=c
    else
      l:=c+1;

    if l>=r then
    begin
      if (l=r) and (l=idx.RecCnt-1) then begin //the only case when R was never checked
        lastCmp:=ttCompareSign(sign,SortRec(TransOrder(l,order),order+1));
        if lastCmp>0 then Inc(l);
      end;
      pos:=l;
      Result := (pos<idx.RecCnt) and (lastCmp=0);
      exit;
    end;
  until false;
end;

{$IFDEF CURSOR_IN_TABLE}
//Faster version, by seek object
function TTextTable.Locate(seek: PSeekObject; const value:string):boolean;
begin
  Result := _intcur.Locate(seek, value);
end;

//Slower version, specifying seek by a string name
function TTextTable.Locate(const seek,value:string):boolean;
begin
  Result := _intcur.Locate(seek,value);
end;

function TTextTable.Locate(seek:PSeekObject; const value:integer):boolean;
begin
  Result := _intcur.Locate(seek, value);
end;

function TTextTable.Locate(const seek:string; const value:integer):boolean;
begin
  Result := _intcur.Locate(seek,value);
end;
{$ENDIF}

function TTextTable.GetFieldIndex(const field:string):integer;
var i: integer;
begin
  Result := -1;
  for i := 0 to Length(fields) - 1 do
    if SameText(fields[i].Name, field) then begin
      Result := i;
      break;
    end;
end;

function TTextTable.Field(const field:string):integer;
begin
  Result := GetFieldIndex(field);
end;

{ For integer fields, TTextTable can do Auto-Increments.
 Enable it after table load:
   table.IsAutoInc[fieldIdx] := true;
 Set field to 0 to assign it automatically incremented value. }

function TTextTable.GetIsAutoIncField(const fieldIdx: integer): boolean;
begin
  Result := (fields[fieldIdx].AutoInc>=0);
end;

{ Enables auto-increment on a field. }
procedure TTextTable.SetIsAutoIncField(const fieldIdx: integer; const Value: boolean);
begin
  if Value then
    fields[fieldIdx].AutoInc := 0 //Find max autoinc value next time it's needed
  else
    fields[fieldIdx].AutoInc := -1;
end;

{ Retrieves next available auto-increment value for a field. }
function TTextTable.GetAutoIncValue(const fieldIdx: integer): integer;
var oldval: integer;
begin
  repeat
    oldval := fields[fieldIdx].AutoInc;
    if oldval<0 then begin
      Result := -1; //no auto-inc
      exit;
    end;

    if oldval=0 then begin
      Result:=FindMaxFieldValue(fieldIdx)+1;
      if Result<0 then
        Result:=0;
     { With current implementation, we can't have AutoInc values < 0.
      It's fine, it's our choice. }
    end else
      Result:=oldval;

    if InterlockedCompareExchange(fields[fieldIdx].AutoInc, Result+1, oldval)=oldval then
      break;
   { Otherwise someone changed it while we were considering the change,
    so we need to try again }
  until false;
end;

function TTextTable.FindMaxFieldValue(const fieldIdx: integer): integer;
var cursor: TTextTableCursor;
  newval: integer;
begin
  cursor := TTextTableCursor.Create(Self);
  try
    cursor.First;
    if cursor.EOF then
      Result := 0
    else
      Result := cursor.TrueInt(fieldIdx);
    while not cursor.EOF do begin
      newval := cursor.TrueInt(fieldIdx);
      if newval>Result then Result:=newval;
      cursor.Next;
    end;
  finally
    FreeAndNil(cursor);
  end;
end;

function TTextTable.IsDeleted(rec:integer):boolean;
begin
  Result := PBoolean(IntPtr(struct)+rec*struct_recsz)^;
end;

function GetFieldValueSize(ftype: char; const fval: string): integer;
begin
  case ftype of
    'b':Result:=1;
    'w':Result:=2;
    'i':Result:=4;
    'l':Result:=1;
    's':Result:=length(fval);
   {$IFDEF UNICODE}
    'x':Result:=length(fval) * 2;
   {$ELSE}
    'x':Result:=length(fval) div 2;
   {$ENDIF}
  else
    Result := 0;
  end;
end;

{ Ensures that there's at least sz free bytes available in data buffer }
procedure TTextTable.ReserveData(const sz: integer);
var p:pointer;
begin
  if databuffer>=sz then exit;
  databuffer:=Trunc(datalen*0.4)+AllocDataBufferSz; //incremental growth
  if sz>=databuffer then //still not enough
    databuffer := sz*2;
  GetMem(p,datalen+databuffer);
  if data<>nil then
    move(data^,p^,datalen);
  freemem(data);
  data:=p;
end;

procedure TTextTable.ReserveStruct(const sz: integer);
var p:pointer;
begin
  if structbuffer>=sz then exit;
  structbuffer:=Trunc((reccount*struct_recsz)*0.4)+AllocStructBufferSz;
  if sz>=structbuffer then //still not enough
    structbuffer := sz*2;
  GetMem(p,(reccount*struct_recsz)+structbuffer);
  if struct<>nil then
    move(struct^,p^,reccount*struct_recsz);
  freemem(struct);
  struct:=p;
end;

{ Appends record to the table, returns its RecNo }
function TTextTable.AddRecord(values:array of string): integer;
var totsize:integer;
  i:integer;
  c:char;
  a:array of byte;
  k:integer;
  fsz:word; //field size
  autoval:integer;
begin
  if offline then
    raise Exception.Create('Cannot insert into offline table.');
  if High(values)+1<>Length(fields) then
    raise Exception.Create('Invalid values array count (TTextTable.Insert).');

 { Fill record header, allocate memory then call Edit() to copy data }
  ReserveStruct(struct_recsz);
  dec(structbuffer,struct_recsz);
  PByte(IntPtr(struct)+reccount*struct_recsz)^ := 0;
  PInteger(IntPtr(struct)+reccount*struct_recsz+1)^ := datalen;
  totsize:=0;
  k:=0;
  for i:=0 to Length(fields)-1 do
  begin
    c:=fields[i].DataType;

   { Auto-increment fields }
    if (fields[i].AutoInc>=0) and (values[i]='0') then begin
      autoval := GetAutoIncValue(i);
      if autoval>=0 then
        values[i] := IntToStr(autoval);
    end;

    fsz:=GetFieldValueSize(c, values[i]);
    if (c='x') or (c='s') then begin
      if wordfsize then begin
        if fsz>65530 then fsz := 65530; { Only this much bytes will be stored }
        PWord(IntPtr(struct)+reccount*struct_recsz+5+k)^ := fsz;
        inc(k,2);
      end else begin
        if fsz>250 then fsz := 250;
        PByte(IntPtr(struct)+reccount*struct_recsz+5+k)^ := fsz;
        inc(k,1);
      end;
    end;
    Inc(totsize,fsz);
  end;

  ReserveData(totsize);
  dec(databuffer,totsize);
  Inc(datalen,totsize);

  SetLength(a,Length(fields));
  for i:=0 to Length(fields)-1 do a[i]:=i;
  Result := reccount;
  inc(reccount);
  EditRecord(Result, a, values, {JustInserted=}true);
end;

procedure TTextTable.DeleteRecord(RecNo: integer);
begin
  PBoolean(IntPtr(struct)+RecNo*struct_recsz)^ := true;
  inc(numberdeleted);
end;

procedure TTextTable.EditRecord(RecNo: integer; const AFields:array of byte;
  const AValues:array of string; JustInserted:boolean=false);
var i,j:integer;
    sz:byte;
    tp:char;
    willinsert:boolean;
    a:array of string;
    fnd:boolean;
begin
  if offline then
    raise Exception.Create('Cannot edit offline table');
  willinsert:=false;
  for i:=0 to High(AValues) do
  begin
    sz:=GetFieldSize(RecNo,AFields[i]);
    tp:=fields[AFields[i]].DataType;
    if ((tp='s') and (sz<>length(AValues[i])))
   {$IFDEF UNICODE}
    or ((tp='x') and (sz<>length(AValues[i]) * 2))
   {$ELSE}
    or ((tp='x') and (sz<>length(AValues[i]) div 2))
   {$ENDIF}
    then willinsert:=true;
  end;
  if not justinserted and willinsert then
  begin
    SetLength(a,Length(fields));
    for i:=0 to Length(fields)-1 do
    begin
      fnd:=false;
      for j:=0 to High(AValues) do
        if AFields[j]=i then
        begin
          fnd:=true;
          a[i]:=AValues[j];
        end;
      if not fnd then a[i]:=Self.GetField(RecNo,i);
    end;
    DeleteRecord(RecNo);
    AddRecord(a);
    exit;
  end;
  for i:=0 to High(AValues) do
    SetField(RecNo, AFields[i], AValues[i]);
  if not nocommit then Commit(RecNo, JustInserted);
end;

{$IFDEF CURSOR_IN_TABLE}
procedure TTextTable.Insert(values:array of string);
begin
  _intcur.Insert(values);
end;

procedure TTextTable.Delete;
begin
  _intcur.Delete();
end;

procedure TTextTable.Edit(const fields:array of byte; const values:array of string);
begin
  _intcur.Edit(fields,values);
end;
{$ENDIF}

{ Generates a signature for a set of fields to user in search/sort.
 Signature must be in such a format that it's sorted exactly like those fields
 would be sorted as strings and integers (depending on their types).
 To achieve this, fields are separated with #9 (assumed to not appear in text),
 and integers are padded with zeroes (this gives us proper integer-like sorting). }
function TTextTable.SortRec(r:integer;const fields:TSeekFieldDescriptions):string;
var j, idx: integer;
  s3: string;
begin
  Result := '';
  for j := 0 to Length(fields) - 1 do begin
    idx := fields[j].index;
    if fields[j].strdata then
    begin
      if not fields[j].reverse then
        Result:=Result+GetField(r,idx)+#9
      else
        Result:=Result+ReverseString(GetField(r,idx))+#9;
    end else
    begin
      s3:=GetField(r,idx);
      while length(s3)<6 do s3:='0'+s3;
      Result:=Result+s3+#9;
    end;
  end;
  while (length(Result)>0) and (Result[length(Result)]=#9) do system.delete(Result,length(Result),1);
  Result:=uppercase(Result);
end;

function TTextTable.SortRec(r:integer;seekIndex:integer):string;
begin
  Result := SortRec(r, seeks[seekIndex].fields);
end;

{ String comparison for a single field }
function TTextTable.ttCompareStr(const s1, s2: string): integer;
begin
  if rawindex then
    Result := CompareStr(s1, s2)
  else
    Result := AnsiCompareStr(s1, s2);
end;

{ String comparison for a signature }
function TTextTable.ttCompareSign(const s1, s2: string): integer;
begin
 { Same as for a field, at this time }
  if rawindex then
    Result := CompareStr(s1, s2)
  else
    Result := AnsiCompareStr(s1, s2);
end;

{ Returns false if the index is broken at that point, otherwise true }
function TTextTable.TestIdxPos(const order: integer; const pos: integer): boolean;
var idx: PTextTableIndex;
  sign, sign2: string;
  RecNo, RecNo2: integer;
begin
  idx := @FIndexes[order];
  RecNo := idx.Read(pos);
  sign := SortRec(RecNo,order+1);
  if pos>0 then begin
    RecNo2 := idx.Read(pos-1);
    sign2 := SortRec(RecNo2,order+1);
    if ttCompareSign(sign2,sign)>0 then begin
      Result := false;
      exit;
    end;
  end;
  if pos<idx.RecCnt-1 then begin
    RecNo2 := idx.Read(pos+1);
    sign2 := SortRec(RecNo2,order+1);
    if ttCompareSign(sign,sign2)>0 then begin
      Result := false;
      exit;
    end;
  end;
  Result:=true;
end;

{ Edit-Commit, as opposed to Insert-Commit, can be optimized with linear indexes.
 Instead of 1. extracting the entry and 2. inserting at new location, we can
 determine old and new location and move only the cells between them in one go.
 Unfortunately there are complications (see below) so I have disabled the code
 path, but the function is written so that it's possible. }
{$DEFINE COMMIT_EXPENSIVE_MOVE}

procedure TTextTable.Commit(RecNo:integer; JustInserted: boolean);
var pos_prev, pos_next: integer;
  idx: PTextTableIndex;
  i: integer;
begin
  if orders.Count>=seeks.Count then
    raise Exception.Create(eReadOnlyIndexes);
  for i := 0 to Length(FIndexes) - 1 do begin
    idx := @FIndexes[i];

   { Locate existing record }
    if JustInserted then
      pos_prev := -1 //we're guaranteed it's not in index
    else
      pos_prev := idx.FindEntry(RecNo);

    if pos_prev>=0 then begin
    {$IFDEF COMMIT_EXPENSIVE_MOVE}
     //If it's still okay where it is, do nothing
      if TestIdxPos(i, pos_prev) then
        continue;
      idx.ShiftLeft(pos_prev+1, idx.RecCnt-pos_prev-1, 1);
      Dec(idx.RecCnt);
      pos_prev:=-1; //assume there was none
    {$ELSE}
     {
     We would like to remove old value from index without shifting cells,
     just "zero it out", so that we can Locate() new position.
     That's not so simple. We can't just zero it (record #0 may not belong
     at this place in the index).
     I tried copying value to the left/to the right but that was broken. Maybe
     one day someone tries again and figures why.
     For now I disabled the codepath.
     }
    {$ENDIF}
    end;

   { Find where the record should be }
    TrueLocateRecord(i, SortRec(RecNo,i+1), pos_next);

    if pos_prev<0 then begin
     { Entry was not found, either we're adding the record or editing not yet indexed
      record with indexing enabled (invalid, but we'll tolerate it here) }
      idx.AddRecs(1);
      pos_prev:=idx.RecCnt-1;
    end;

   { Add entry }
    if pos_prev=pos_next then begin
     //do nothing
    end else
    if pos_prev<pos_next then
      idx.ShiftLeft(pos_prev+1, pos_next-pos_prev, 1)
    else
      idx.ShiftRight(pos_next, pos_prev-pos_next, 1);
    idx.Write(pos_next, RecNo);
  end;
end;

{ A special TStringList to sort strings exactly how TextTable sorts those.
 We probably should just move to our own BinarySort someday. }
type
  TIndexStringList = class(TStringList)
  public
    FTable: TTextTable;
    function CompareStrings(const S1, S2: string): Integer; override;
  end;

function TIndexStringList.CompareStrings(const S1, S2: string): Integer;
begin
  Result := FTable.ttCompareSign(uppercase(S1),uppercase(S2));
end;

procedure TTextTable.Reindex;
var i,j:integer;
  sl:TIndexStringList;
  idx: PTextTableIndex;
begin
  if orders.Count>=seeks.Count then
    raise Exception.Create(eReadOnlyIndexes);
  for i := 0 to Length(FIndexes) - 1 do begin
    FIndexes[i].RecCnt := RecCount;
    FIndexes[i].NeedAtLeast(RecCount*4);
  end;
  sl:=TIndexStringList.Create;
  sl.FTable := Self;
  for i:=0 to orders.Count-1 do
  begin
    idx := @FIndexes[i];
    sl.Clear;
    for j:=0 to RecCount-1 do
      sl.AddObject(SortRec(j,i+1),pointer(j));
    sl.Sort;
    for j:=0 to RecCount-1 do
      idx.Write(j, integer(sl.Objects[j])); //there's really only integer stored in TObject, even on 64bit platforms
  end;
  sl.Free;
end;

{ Checks the specified index. It needs to have a $SEEK definition }
function TTextTable.CheckIndex(const index: integer):boolean;
var j:integer;
  s1,s2:string;
  IndexRecCount: integer;
begin
  s1:='';
  IndexRecCount := FIndexes[index].RecCnt;
  for j:=0 to IndexRecCount-1 do
  begin
    s2:=sortrec(transorder(j,index),index+1);
    if ttCompareSign(s1,s2)>0 then
    begin
      Result:=false;
      exit;
    end;
    s1:=s2;
  end;
  Result := true;
end;

{ Checks all those indexes for which there's a $SEEK definition }
function TTextTable.CheckIndices: integer;
var i: integer;
begin
  Result := -1;
  for i:=0 to orders.Count-1 do
    if seeks.Count>i+1 then
      if not CheckIndex(i) then begin
        Result := i;
        exit;
      end;
end;

function TTextTable.CheckIndex:boolean;
begin
  Result := CheckIndices<0;
end;

function TTextTable.HasIndex(const index:string):boolean;
begin
  Result := orders.IndexOf(index)>=0;
end;


{ Import/export }
{ This will only work properly on Ansi }

procedure TTextTable.ExportToText(t:TStreamEncoder;ord:string);
var i,j:integer;
    s,s2:string;
    ordn:integer;
begin
  s:='';
  for i:=0 to Length(fields)-1 do s:=s+';'+fields[i].Name;
  ordn:=orders.IndexOf(ord);
  s[1]:='>';
  t.Writeln(s);
  for i:=0 to reccount-1 do
  begin
    s:='';
    for j:=0 to Length(fields)-1 do
    begin
      s2:=GetField(TransOrder(i,ordn),j);
      while pos(';',s2)>0 do s2:=copy(s2,1,pos(';',s2)-1)+','+copy(s2,pos(';',s2)+1,length(s2)-pos(';',s2));
      s:=s+';'+s2;
    end;
    s[1]:='+';
    t.Writeln(s);
  end;
  t.Writeln('.');
end;

procedure TTextTable.ImportFromText(t:TStreamDecoder);
var s:string;
    i,j:integer;
    fld:TStringList;
    a:array of string;
    s2:string;
begin
  nocommit:=true;
  fld:=TStringList.Create;  

 //Header
  while true do begin
    s := Trim(t.ReadLn());
    if s='' then continue;
    if s[1]=';' then continue; //header comment
    if s[1]='>' then begin
     //Field definition
      system.delete(s,1,1);
      while s<>'' do
      begin
        if s[1]=';' then system.delete(s,1,1);
        if pos(';',s)>0 then s2:=copy(s,1,pos(';',s)-1) else s2:=s;
        system.delete(s,1,length(s2));
        fld.Add(s2);
      end;
      break;
    end else
      raise Exception.Create('Invalid header line: "'+s+'"');
  end;
  
 //Check that all fields are present -- as a safety
  for i := 0 to fld.Count - 1 do
    if Self.GetFieldIndex(fld[i])<0 then
      raise Exception.Create('Field not found: "'+fld[i]+'"');

 //Import
  while true do begin
    s := Trim(t.ReadLn());
    if (s='') or (s='.') then break;
    if s[1]<>'+' then
      raise Exception.Create('Value set starts with invalid symbol: "'+s+'"');
    system.delete(s,1,1);
    SetLength(a,Length(fields));
    for i:=0 to Length(fields)-1 do a[i]:='0';
    i:=0;
    while s<>'' do
    begin
      if s[1]=';' then system.delete(s,1,1);
      if (pos(';',s)>0) and (i<Length(fields)-1) then s2:=copy(s,1,pos(';',s)-1) else s2:=s;
      system.delete(s,1,length(s2));
      j:=GetFieldIndex(fld[i]);
      if j<>-1 then a[j]:=s2;
      inc(i);
    end;
    Insert(a);
  end;
  fld.Free;
  nocommit:=false;
  Reindex;
end;



{ TTextTableCursor }

constructor TTextTableCursor.Create(ATable: TTextTable);
begin
  inherited Create;
  FTable := ATable;
  cur:=-1;
  tcur:=0;
  curorder:=-1;
  filter:='';
end;

procedure TTextTableCursor.First;
begin
  cur:=-1;
  tcur:=0;
  Next;
end;

procedure TTextTableCursor.Next;
var stop:boolean;
begin
  repeat
    inc(cur);
    if cur<Table.RecCount then
    begin
      tcur:=Table.TransOrder(cur,curorder);
      if tcur>=Table.RecCount then
        raise Exception.Create('Index problem');
      if filter='' then
        stop:=not Table.IsDeleted(tcur)
      else
        stop:=not Table.IsDeleted(tcur) and Table.FilterPass(tcur,filter);
    end else
    begin
      tcur:=cur;
      stop:=true;
    end;
  until stop;
end;

function TTextTableCursor.EOF:boolean;
begin
  result:=(cur>=Table.RecCount);
end;

procedure TTextTableCursor.SetOrder(const index:string);
begin
  curorder:=Table.orders.IndexOf(index);
  First;
end;

procedure TTextTableCursor.SetFilter(const filtr:string);
begin
  filter:=filtr;
end;

function TTextTableCursor.Test(const fil:string):boolean;
begin
  Result := Table.FilterPass(tcur,fil);
end;


procedure TTextTableCursor.Insert(values:array of string);
begin
  tcur := Table.AddRecord(values);
end;

procedure TTextTableCursor.Delete;
begin
  Table.DeleteRecord(tcur);
end;

procedure TTextTableCursor.Edit(const fields:array of byte; const values:array of string);
begin
  Table.EditRecord(tcur, fields, values);
end;


//Slower version, specifying seek by a string name
function TTextTableCursor.Locate(const seek,value:string):boolean;
var so: TSeekObject;
begin
  so := Table.GetSeekObject(seek);
  Result := Locate(@so, value);
end;

//Faster version, by seek object
function TTextTableCursor.Locate(seek: PSeekObject; const value:string):boolean;
var idx: integer;
begin
  Result := Table.LocateRecord(seek, value, idx);
  cur := idx;
  if (idx>=0) and (idx<Table.RecordCount) then
    tcur := Table.TransOrder(idx, seek.ind_i-1)
  else
    tcur := Table.RecordCount; //-1 is not detected as EOF
end;

function TTextTableCursor.Locate(const seek:string; const value:integer):boolean;
var so: TSeekObject;
begin
  so := Table.GetSeekObject(seek);
  Result := Locate(@so, value);
end;

function TTextTableCursor.Locate(seek:PSeekObject; const value:integer):boolean;
var idx: integer;
begin
  Result := Table.LocateRecord(seek, value, idx);
  cur := idx;
  if (idx>=0) and (idx<Table.RecordCount) then
    tcur := Table.TransOrder(idx, seek.ind_i-1)
  else
    tcur := Table.RecordCount; //-1 is not detected as EOF
end;


function TTextTableCursor.Str(field:integer):string;
begin
  Result:=Table.GetField(tcur,field);
end;

function TTextTableCursor.AnsiStr(field:integer):AnsiString;
begin
  Result:=Table.GetAnsiField(tcur,field);
end;

function TTextTableCursor.Int(field:integer):integer;
begin
  if not Table.GetIntField(tcur,field,Result) then
    Result := 0;
end;

function TTextTableCursor.TrueInt(field:integer):integer;
begin
  if not Table.GetIntField(tcur,field,Result) then
    raise Exception.Create('Cannot convert this to integer: '+Table.GetField(tcur,field));
end;

function TTextTableCursor.Bool(field:integer):boolean;
var s: string;
begin
  s := Table.GetField(tcur,field);
  Result :=(Length(s)>0) and (UpCase(s[1])='T');
end;

{
For single characters stored as 'x'-type strings.
Returns one unicode character in FChar.
}
{$IFDEF UNICODE}
function TTextTableCursor.Fch(field:integer):WideChar;
var s: string;
begin
  s := Str(field);
  if Length(s)<=0 then
    Result := #0000
  else
    Result := s[1];
end;
{$ELSE}
//On older versions character is not guaranteed, but it was always this way.
function TTextTableCursor.Fch(field:integer):string;
begin
  Result := Str(field);
end;
{$ENDIF}

{
For unicode strings stored as hex in 'a'-type strings.
Returns contents in FString.
}
function TTextTableCursor.Dehex(field:integer):FString;
begin
 {$IFDEF UNICODE}
  Result := HexToUnicode(Str(field));
 {$ELSE}
  Result := Str(field);
 {$ENDIF}
end;


initialization
 {$IFDEF TTSTATS}
  dataalloc:=0;
  structalloc:=0;
  indexalloc:=0;
  statlist:=TStringList.Create;
 {$ENDIF}

finalization
 {$IFDEF TTSTATS}
  statlist.Free;
 {$ENDIF}

end.
