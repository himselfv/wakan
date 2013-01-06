unit JWBIndex;
{
Contains index-related functionality.
Currently there are two indexes, both used for dictionaries: word and char index.

All indexes are in the following format
  <entry_count>
  <entries>
  <references>

Entries are 4 (v4) or 8 (v5) bytes long, each followed by an index of a first
reference for that entry.
  <entry> <first # of ref>

References are always 4 bytes long, and are indexed in 4-byte positions,
from the start of the file.

Entries are ordered in the same way reference blocks are, so <first # of ref>
for the next entry also signifies <last # of ref> for current entry.

Last # of ref for the last entry is the ref at end of the file
}

interface
uses Classes, JWBStrings;

type
  TIndex = class
  protected
    FEntryLen: integer; //entry length in positions: 1 on v4, 2 on v5
    FData:pointer;
    FEntryCount:integer; //number of entries in this index
    FPosCount:integer; //number of available "positions" in the data
   //Cached
    FTotalEntrySz: integer; //total size of each entry record in bytes == sizeof(entry + first_ref)
    FHeaderLen: integer; //total length of header in positions
  public
    constructor Create(AEntryLen: integer; mf: TStream);
    destructor Destroy; override;
    procedure ReadIndexEntryFromTo(loc:integer;out indexfrom,indexto:integer); virtual;
    function ReadIndexInfo(loc:integer):integer; virtual;
    function FindEntry(const locator:UnicodeString):integer; virtual; abstract;
  end;

  TIndexV4 = class(TIndex)
  public
    constructor Create(mf: TStream);
    function ReadIndexEntry(loc:integer):AnsiString;
    function FindEntry(const locator:UnicodeString):integer; override;
  end;

  TIndexV5 = class(TIndex)
    constructor Create(mf: TStream);
    function ReadIndexEntry(loc:integer):int64;
    function FindEntry(const locator:UnicodeString):integer; override;
  end;

 {
  Dictionary index builder.
  1. Create.
  2. AddToIndex(Key, Article)
  3. Pack.
  4. Write and Destroy
 }

  TIndexEntryPrep = record
    entry: int64;
    refs: string;
    procedure Reset;
  end;
  PIndexEntryPrep = ^TIndexEntryPrep;

  TIndexBuilder = class;
  TIndexBuilderCompare = function(List: TIndexBuilder; Index1, Index2: Integer): Integer;

  TIndexBuilder = class
  protected
    FList: array of PIndexEntryPrep;
    FListUsed: integer;
    procedure Grow(ARequiredFreeLen: integer);
    function GetItemPtr(Index: integer): PIndexEntryPrep;{$IFDEF INLINE} inline;{$ENDIF}
    function InsertItem(Index: integer): PIndexEntryPrep;
    function Find(const entry: int64; var Index: Integer): Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure AddToIndex(key: int64; article: integer);
    procedure Write(filename: string);
    property Count: integer read FListUsed;
    property Items[Index: integer]: PIndexEntryPrep read GetItemPtr; default;
  end;

  TWordIndexBuilder = class(TIndexBuilder)
  public
    procedure AddToIndex(const str: FString; article: integer);
  end;

implementation
uses SysUtils;

{
Index
}

constructor TIndex.Create(AEntryLen: integer; mf: TStream);
begin
  inherited Create;
  FEntryLen := AEntryLen;
  FTotalEntrySz := FEntryLen*4 + 4; //in bytes
  mf.Seek(0, soFromBeginning);
  mf.Read(FEntryCount, 4);
  GetMem(FData,mf.Size-4);
  mf.Read(FData^,mf.Size-4);
  FPosCount:=(mf.Size div 4)-1;
  FHeaderLen := FEntryCount*(FEntryLen+1); //in positions
end;

destructor TIndex.Destroy;
begin
  FreeMem(FData);
  inherited;
end;

procedure TIndex.ReadIndexEntryFromTo(loc:integer;out indexfrom,indexto:integer);
begin
  indexfrom:=FHeaderLen+PInteger(integer(FData)+loc*FTotalEntrySz+4)^;
  if loc<FEntryCount then
    indexto:=FHeaderLen+PInteger(integer(FData)+(loc+1)*FTotalEntrySz+4)^
  else
    indexto:=FPosCount;
end;

function TIndex.ReadIndexInfo(loc:integer):integer;
begin
  Result := PInteger(integer(FData)+loc*4)^
end;

//4 bytes compared as AnsiString on v4
constructor TIndexV4.Create(mf: TStream);
begin
  inherited Create(1,mf);
end;

function TIndexV4.ReadIndexEntry(loc:integer):AnsiString;
begin
  SetLength(Result, 4);
  move(PAnsiChar(integer(FData)+loc*8)^,Result[1],4);
end;

function TIndexV4.FindEntry(const locator:UnicodeString):integer;
var l,r,m:integer;
  a_str: AnsiString;
  val: AnsiString;
begin
  a_str := AnsiString(locator); //This calls WideCharToMultiByte so a good conversion
  MakeFixedLen(a_str, 4, ' ');

  l:=0; r:=FEntryCount-1; m:=l;
  while l<=r do
  begin
    m:=l+(r-l) div 2;
    val:=ReadIndexEntry(m);
    if val=a_str then break;
    if val<a_str then l:=m+1 else r:=m-1;
  end;
  if l>r then
    Result:=-1 //not found
  else
    Result:=m;
end;

//8 bytes compared as int64 in v5
constructor TIndexV5.Create(mf: TStream);
begin
  inherited Create(2,mf);
end;

function TIndexV5.ReadIndexEntry(loc:integer):int64;
begin
  Result := PInt64(integer(FData)+loc*12)^
end;

function TIndexV5.FindEntry(const locator:UnicodeString):integer;
var l,r,m:integer;
  loc_val: int64;
  val: int64;
begin
  loc_val := PInt64(@locator[1])^;

  l:=0; r:=FEntryCount-1; m:=l;
  while l<=r do
  begin
    m:=l+(r-l) div 2;
    val:=ReadIndexEntry(m);
    if val=loc_val then break;
    if val<loc_val then l:=m+1 else r:=m-1;
  end;
  if l>r then
    Result:=-1 //not found
  else
    Result:=m;
end;


{
Index Builder
}

procedure TIndexEntryPrep.Reset;
begin
  entry := 0;
  refs := '';
end;

constructor TIndexBuilder.Create;
begin
  inherited;
end;

destructor TIndexBuilder.Destroy;
begin
  Clear;
  inherited;
end;

//Reserves enough memory to store at least ARequiredFreeLen additional items to list.
procedure TIndexBuilder.Grow(ARequiredFreeLen: integer);
const MIN_GROW_LEN = 20;
begin
  if Length(FList)-FListUsed>=ARequiredFreeLen then exit; //already have the space
 //else we don't grow in less than a chunk
  if ARequiredFreeLen < MIN_GROW_LEN then
    ARequiredFreeLen := MIN_GROW_LEN;
  SetLength(FList, Length(FList)+ARequiredFreeLen);
end;

function TIndexBuilder.GetItemPtr(Index: integer): PIndexEntryPrep;
begin
  Result := FList[Index];
end;

function TIndexBuilder.InsertItem(Index: integer): PIndexEntryPrep;
var i: integer;
begin
 //Thread unsafe
  Grow(1);
  New(Result);
  for i := FListUsed downto Index+1 do
    FList[i] := FList[i-1];
  FList[Index] := Result;
  Inc(FListUsed);
end;

procedure TIndexBuilder.Clear;
var i: integer;
begin
  for i := 0 to FListUsed - 1 do
    Dispose(FList[i]);
  SetLength(FList, 0);
  FListUsed := 0;
end;

function DefaultIndexEntryCompare(List: TIndexBuilder; Index1, Index2: Integer): Integer;
var pi, pj: PIndexEntryPrep;
begin
  pi := List.FList[Index1];
  pj := List.FList[Index2];
  Result := (pi^.entry-pj^.entry);
end;

function TIndexBuilder.Find(const entry: int64; var Index: Integer): Boolean;
var
  L, H, I, C: Integer;
begin
  Result := False;
  L := 0;
  H := Count - 1;
  while L <= H do
  begin
    I := (L + H) shr 1;
    C := FList[i].entry-entry;
    if C < 0 then L := I + 1 else
    begin
      H := I - 1;
      if C = 0 then
      begin
        Result := True;
        L := I;
      end;
    end;
  end;
  Index := L;
end;

{ Key: Index key.
Article: Article index to link to this key. }
procedure TIndexBuilder.AddToIndex(key: int64; article: integer);
var i:integer;
  pe: PIndexEntryPrep;
  ind:string;
begin
 //Article IDs are stored as 8 byte hex blocks one after another
  ind:=ByteToHex(@article, sizeof(article));

  if Find(key, i) then begin
    pe := FList[i];
    if copy(pe.refs,length(pe.refs)-7,8)<>ind then
      pe.refs := pe.refs + ind;
  end else begin
    pe := InsertItem(i);
    pe.entry := key;
    pe.refs := ind;
  end;
end;

procedure TIndexBuilder.Write(filename: string);
var
  fb: file;
  buf: array[0..3999] of byte;
  bufp: integer;
  i, j: integer;

  procedure PutToBuf(pb:PByte;sz:integer);
  begin
    if Length(buf)-bufp<sz then begin
      blockwrite(fb,buf,4000);
      bufp:=0;
    end;
    move(pb^,buf[bufp],sz);
    Inc(bufp,sz);
  end;

  procedure PutToBufL(l:integer);
  begin
    PutToBuf(@l, sizeof(l));
  end;

  procedure PutToBufLL(l:int64);
  begin
    PutToBuf(@l, sizeof(l));
  end;

begin
  assignfile(fb,filename);
  bufp:=0;
  rewrite(fb,1);
  PutToBufL(Self.Count);
  j:=0;
  for i:=0 to Self.Count-1 do
  begin
    PutToBufLL(FList[i].entry);
    PutToBufL(j);
    inc(j,length(FList[i].refs) div 8);
  end;
  for i:=0 to Self.Count-1 do
    for j:=0 to (length(FList[i].refs) div 8)-1 do
    begin
      PutToBufL(StrToInt('0x'
        +copy(FList[i].refs,j*8+7,2)
        +copy(FList[i].refs,j*8+5,2)
        +copy(FList[i].refs,j*8+3,2)
        +copy(FList[i].refs,j*8+1,2)
      ));
    end;
  blockwrite(fb,buf,bufp);
  closefile(fb);
end;

procedure TWordIndexBuilder.AddToIndex(const str: FString; article: integer);
var tmp: UnicodeString;
begin
  tmp := fstrtouni(str);
  MakeFixedLen(tmp, 4, ' ');
  inherited AddToIndex(PInt64(@tmp[1])^, article);
end;

end.
