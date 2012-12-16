unit TextTable;

interface
uses MemSource,Classes,SysUtils,Dialogs,StdPrompt,Windows;

{
Unicode status:
- Dictionary format is not going to change so everything inside the dictionary
is kept as AnsiStrings, encoded in old 4-char format.
We convert it as we read it.
- We return stuff in strings (UnicodeStrings).
}

{
TextTable relies heavily on in-memory structures.
Table header is always kept in memory, and if TextTable.offline is true then
the whole table is, too.
Otherwise the data is read from the disk on demand. 
}

{$DEFINE CURSOR_IN_TABLE}
{ To navigate TTextTable you must use TTextTableCursor.
 However, old code used to call TTextTable.First/Next and so we keep a copy
 of a cursor code inside of a TTextTable. }

const
  AllocDataBuffer=65536;
  AllocOrderBuffer=1024;
  AllocStructBuffer=1024;

type
 //Seek table reference to speed up Locate()
 //You retrieve it once, you use it instead of a textual name.
 //For common tables and common seeks, global references are kept.
  TSeekObject = record
    ind_i: integer; //seek table index
    fld_i: integer; //field index
    reverse: boolean;
  end;
  PSeekObject = ^TSeekObject;

  TSeekFieldDescription = record
    index: integer; //field index
    reverse: boolean;
    strdata: boolean //treat field data as string
  end;
  PSeekFieldDescription = ^TSeekFieldDescription;
  TSeekFieldDescriptions = array of TSeekFieldDescription;

  TSeekDescription = record
    fields: TSeekFieldDescriptions;
  end;
  PSeekDescription = ^TSeekDescription;
  TSeekDescriptions = array of TSeekDescription;

  TTextTableCursor = class;

  TTextTable=class
    fieldlist:TStringList;
   { Seek table names.
    The point of seek table is to keep records sorted in a particular order.
    The way it's done now, you just have to KNOW what order was that,
    if you're going to use the index. }
    seeks:TStringList;
   { Seek table descriptions in the form "field1+field2+field3".
    Loaded from the table file, used to rebuild indexes if needed.
    First field name ("field1") becomes seek table name }
    seekbuild:TStringList;
   { Seek table descriptions, parsed. }
    seekbuilddesc:TSeekDescriptions;
    fieldbuild:TStringList;
   { Orders are the same as seekbuilds, only they have different names
     and there's 1 less of them:
                  == seekbuild[0]
        orders[0] == seekbuild[1]
        orders[1] == seekbuild[2]
        ...etc
     Don't ask me why it's like that. }
    orders:TStringList;
    data,struct,index:pointer;
    fieldtypes:string;
    fieldsizes:string;
    varfields:integer;
    fieldcount:byte;
    reccount:integer;
    rawindex:boolean;
    databuffer,structbuffer:integer;
    prebuffer:boolean;
    datalen:integer;
    numberdeleted:integer;
    nocommit:boolean;
    tablename:string;
    load_source:TPackageSource;
    load_filename:string;
    load_rawread:boolean;
    loaded:boolean;
    offline:boolean;
    datafpos:integer;
    source:TPackageSource;
    function FilterPass(rec:integer;fil:string):boolean;
    function TransOrder(rec,order:integer):integer; inline;
    function IsDeleted(rec:integer):boolean;

   //Record sorting
    function ParseSeekDescription(fld:string): TSeekFieldDescriptions;
    function SortRec(r:integer;const fields:TSeekFieldDescriptions):string; overload; //newer cooler version
    function SortRec(r:integer;seekIndex:integer):string; overload; {$IFDEF INLINE}inline;{$ENDIF}
    function SortRecByStr(r:integer; fld:string):string; deprecated;
  public
    constructor Create(source:TPackageSource;filename:string;rawread,offline:boolean);
    destructor Destroy; override;
    function Field(const field:string):integer;
    function HasIndex(const index:string):boolean;
    procedure WriteTable(const filename:string;nodelete:boolean);
    property RecordCount:integer read reccount;
    function NewCursor: TTextTableCursor;

    procedure ExportToText(var t:textfile;ord:string);
    procedure ImportFromText(var t:textfile;smf:TSMPromptForm;mess:string);
    procedure Reindex;
    procedure Load;
    function CheckIndex:boolean;
    property NoCommitting:boolean read nocommit write nocommit;

  public
    function AddRecord(values:array of string): integer;
    procedure DeleteRecord(RecNo: integer);
    procedure EditRecord(RecNo: integer; const fields:array of byte;
      const values:array of string; JustInserted:boolean=false);
    procedure Commit(RecNo:integer; JustInserted: boolean = false);

  public //Field reading
    function GetFieldSize(recno,field:integer):byte;
    function GetField(rec:integer;field:integer):string;
    procedure SetField(rec:integer;field:integer;const value:string);

  public
    function GetSeekObject(seek: string): TSeekObject;
    function LocateRecord(seek: PSeekObject; value:string; number:boolean; out idx: integer):boolean;

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
    procedure Delete; {$IFDEF INLINE}inline;{$ENDIF}
    procedure Edit(const fields:array of byte;const values:array of string);
    function Locate(seek: PSeekObject; const value:string; number:boolean):boolean; overload; {$IFDEF INLINE}inline;{$ENDIF}
    function Locate(seek,value:string; number:boolean):boolean; overload; {$IFDEF INLINE}inline;{$ENDIF}
  public
    function Str(field:integer):string; {$IFDEF INLINE}inline;{$ENDIF}
    function Int(field:integer):integer; {$IFDEF INLINE}inline;{$ENDIF}
    function Bool(field:integer):boolean; {$IFDEF INLINE}inline;{$ENDIF}
   {$IFDEF UNICODE}
    function Fch(field:integer):WideChar; {$IFDEF INLINE}inline;{$ENDIF}
   {$ELSE}
    function Fch(field:integer):string; {$IFDEF INLINE}inline;{$ENDIF}
   {$ENDIF}
 {$ENDIF}

  end;
  TDataCache=array[0..30,0..1] of word;

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
    procedure Delete; {$IFDEF INLINE}inline;{$ENDIF}
    procedure Edit(const fields:array of byte; const values:array of string);

  public
    function Locate(seek: PSeekObject; const value:string; number:boolean):boolean; overload; {$IFDEF INLINE}inline;{$ENDIF}
    function Locate(seek,value:string; number:boolean):boolean; overload; {$IFDEF INLINE}inline;{$ENDIF}

  public
    function Str(field:integer):string; {$IFDEF INLINE}inline;{$ENDIF}
    function Int(field:integer):integer; {$IFDEF INLINE}inline;{$ENDIF}
    function Bool(field:integer):boolean; {$IFDEF INLINE}inline;{$ENDIF}
   {$IFDEF UNICODE}
    function Fch(field:integer):WideChar; {$IFDEF INLINE}inline;{$ENDIF}
   {$ELSE}
    function Fch(field:integer):string; {$IFDEF INLINE}inline;{$ENDIF}
   {$ENDIF}

  end;

procedure LogAlloc(s:string;len:integer);
procedure ShowAllocStats;

implementation
uses JWBStrings;

var dataalloc,structalloc,indexalloc:integer;
    tottim,datatim,othtim,proctim:double;
    statlist:TStringList;

procedure ShowAllocStats;
begin
  statlist.SaveToFile('tablestats.txt');
  winexec('notepad.exe tablestats.txt',SW_SHOW);
end;

function OffsetPtr(source: pointer; ofs: integer): PByte; inline;
begin
  Result := PByte(integer(source)+ofs); 
end;

procedure MoveOfs(source,dest:pointer;ofssource,ofsdest:integer;size:integer);
var sp,dp:pbyte;
begin
  sp:=source;
  sp:=PByte(integer(sp)+ofssource); //older Delphi versions can't add pointers
  dp:=dest;
  dp:=PByte(integer(dp)+ofsdest);
  move(sp^,dp^,size);
end;

constructor TTextTable.Create(source:TPackageSource;filename:string;rawread,offline:boolean);
begin
  load_source:=source;
  load_filename:=filename;
  load_rawread:=rawread;
  self.offline:=offline;
  loaded:=false;
  Load;
end;

procedure TTextTable.Load;
var ms:TMemoryStream;
    mf:TMemoryFile;
    i,j,k,vk,ii:integer;
    p:pointer;
    dt,s:string;
    stop:boolean;
    dc:^TDataCache;
    dcp:TDataCache;
    posx:integer;
    b:byte;
    c:char;
    datasize:integer;
    sl:TStringList;
    tcreate:boolean;
    bufsize:integer;
    w:word;
    stim,stim2:TDateTime;
    strubuf:pointer;
    struptr: PAnsiChar;
    precounted:boolean;
    ww:array[0..255] of byte;
    fs:string;
    filename:string;
    rawread:boolean;
    t:textfile;
    cn:integer;
    totalloc:integer;
begin
  source:=load_source;
  filename:=load_filename;
  rawread:=load_rawread;
//  rawread:=false;
  stim:=now;
  tablename:=filename;
  statlist.Add(source.FileName+'->'+filename);
  nocommit:=false;
  fieldlist:=TStringList.Create;
  seeks:=TStringList.Create;
  seekbuild:=TStringList.Create;
  orders:=TStringList.Create;
  data:=TStringList.Create;
  fieldbuild:=TStringList.Create;
  numberdeleted:=0;
  rawindex:=false;
 {$IFDEF CURSOR_IN_TABLE}
  _intcur := TTextTableCursor.Create(Self);
 {$ENDIF}
//  cachedrec:=-1;
  sl:=TStringList.Create;
  if source<>nil then
  begin
    mf:=source[filename+'.info'];
    if mf=nil then raise Exception.Create('TextTable: Important file missing.');
    ms:=mf.Lock;
    sl.LoadFromStream(ms);
  end else sl.LoadFromFile(filename+'.info');
  tcreate:=false;
  c:=' ';
  prebuffer:=false;
  precounted:=false;
  for i:=0 to sl.Count-1 do
  begin
    if sl[i]='$FIELDS' then c:='f';
    if sl[i]='$ORDERS' then c:='o';
    if sl[i]='$SEEKS' then c:='s';
    if sl[i]='$PREBUFFER' then prebuffer:=true;
    if sl[i]='$PRECOUNTED' then precounted:=true;
    if sl[i]='$CREATE' then tcreate:=true;
    if sl[i]='$RAWINDEX' then rawindex:=true;
    if (length(sl[i])>0) and (sl[i][1]<>'$') then
    case c of
      'f':fieldlist.Add(sl[i]);
      'o':orders.Add(sl[i]);
      's':seeks.Add(sl[i]);
    end;
  end;
  sl.Free;
  fieldcount:=fieldlist.Count;
  fieldtypes:='';
  for i:=0 to fieldcount-1 do fieldtypes:=fieldtypes+fieldlist[i][1];
  varfields:=0;
  fieldsizes:='';
  for i:=0 to fieldcount-1 do
  begin
    if (fieldtypes[i+1]='s') or (fieldtypes[i+1]='x') then begin
      fieldsizes:=fieldsizes+chr(ord('a')+varfields);
      inc(varfields);
    end else case fieldtypes[i+1] of
      'b':fieldsizes:=fieldsizes+'1';
      'w':fieldsizes:=fieldsizes+'2';
      'i':fieldsizes:=fieldsizes+'4';
      'l':fieldsizes:=fieldsizes+'1';
    end;
  end;
  for i:=0 to seeks.Count-1 do
  begin
   //Add to seekbuild
    seekbuild.Add(seeks[i]);
   //Will parse to seekbuilddesc later when all schema is loaded
   //Add to seek
    s:=seeks[i];
    if pos('+',seeks[i])>0 then system.delete(s,pos('+',seeks[i]),length(seeks[i])-pos('+',seeks[i])+1);
    seeks[i]:=s;
  end;
  if source<>nil then mf.Unlock;
  if not tcreate then
  begin
    stim2:=now;
    bufsize:=AllocDataBuffer; if not prebuffer then bufsize:=0;
    mf:=source[filename+'.data'];
    if mf=nil then raise Exception.Create('TextTable: Important file missing.');
    if not rawread then
    begin
      ms:=mf.Lock;
      databuffer:=bufsize;
      GetMem(data,ms.Size+bufsize);
      datalen:=ms.Size-4;
      datasize:=ms.Size;
      ms.Read(data^,4);
      moveofs(data,@reccount,0,0,4);
      ms.Read(data^,datalen);
      mf.Unlock;
    end else
    begin
      if offline then GetMem(data,4) else GetMem(data,mf.Size+bufsize);
      datalen:=mf.Size-4;
      datasize:=mf.Size;
      source.ReadRawData(data^,integer(mf.Position),4);
      datafpos:=integer(mf.Position)+4;
      moveofs(data,@reccount,0,0,4);
      if not offline then
      begin
        source.ReadRawData(data^,integer(mf.Position)+4,datalen);
      end;
    end;
    dataalloc:=dataalloc+(mf.Size+bufsize) div 1024;
    totalloc:=mf.Size+bufsize;
    statlist.Add('  Records: '+inttostr(reccount));
    statlist.Add('  Indexes: '+inttostr(orders.Count));
    statlist.Add('  Data size: '+inttostr((mf.Size+bufsize) div 1000)+'k');
    datatim:=datatim+(now-stim2);
    stim2:=now;
    mf:=source[filename+'.index'];
    if mf=nil then raise Exception.Create('TextTable: Important file missing.');
    if not rawread then
    begin
      ms:=mf.Lock;
      GetMem(index,ms.Size);
      ms.Read(index^,ms.Size);
      mf.Unlock;
    end else
    begin
      GetMem(index,mf.Size);
      source.ReadRawData(index^,integer(mf.Position),mf.Size);
    end;
    indexalloc:=indexalloc+mf.Size div 1024;
    statlist.Add('  Indexes size: '+inttostr(mf.Size div 1000)+'k');
    totalloc:=totalloc+mf.Size;
    mf:=source[filename+'.struct'];
    if mf=nil then raise Exception.Create('TextTable: Important file missing.');
    bufsize:=AllocStructBuffer; if not prebuffer then bufsize:=0;
    structbuffer:=bufsize;
    if not rawread then
    begin
      ms:=mf.Lock;
      GetMem(struct,ms.Size+bufsize);
      ms.Read(struct^,ms.Size);
      mf.Unlock;
     { if mf.Size>4 then
      begin
        showmessage(inttostr(mf.Size)+'.');
        moveofs(strubuf,@i,mf.Size-3,0,4);
        showmessage(inttostr(i));
      end; }
    end else
    begin
      GetMem(struct,mf.Size+bufsize);
      source.ReadRawData(struct^,integer(mf.Position),mf.Size);
     { if mf.Size>4 then
      begin
        showmessage(inttostr(mf.Size)+'.');
        moveofs(strubuf,@i,mf.Size-3,0,4);
        showmessage(inttostr(i));
      end; }
    end;
    structalloc:=structalloc+(mf.Size+bufsize) div 1024;
    if not precounted then
      statlist.Add('  Structure size: '+inttostr((mf.Size+bufsize+reccount*5) div 1000)+'k')
      else statlist.Add('  Structure size: '+inttostr((mf.Size+bufsize) div 1000)+'k');
    totalloc:=totalloc+mf.Size+bufsize;
    statlist.Add('  Total size: '+inttostr(totalloc div 1000)+'k');
    if not precounted and offline then showmessage('OFFLINE table must be PRECOUNTED!');
    if not precounted then
    begin
      structalloc:=structalloc+(reccount*5) div 1024;
      dec(datasize,4);
      strubuf:=struct;
      struptr:=strubuf;
      struct:=nil;
      GetMem(struct,reccount*5+varfields*reccount+bufsize);
      fs:='';
      for i:=0 to fieldcount-1 do fs:=fs+fieldlist[i][1];
      i:=0;
      w:=0;
      j:=0;
      k:=0;
      vk:=0;
      othtim:=othtim+(now-stim2);
      stim2:=now;
      while i<datasize do
      begin
        case fs[k+1] of
          'b':b:=1;
          'w':b:=2;
          'i':b:=4;
          'l':b:=1;
          'x','s':begin
                    b:=ord(struptr^);
                    inc(struptr);
                    ww[vk]:=b;
                    inc(vk);
                  end;
        end;
        inc(w,b);
        inc(k);
        if k=fieldcount then
        begin
          k:=0;
          b:=0;
          moveofs(@b,struct,0,j,1);
          moveofs(@i,struct,0,j+1,4);
          moveofs(@ww,struct,0,j+5,varfields);
          inc(j,5+varfields);
          inc(i,w);
          w:=0;
          vk:=0;
        end;
      end;
      FreeMem(strubuf);
      if j>(reccount*varfields+reccount*5+bufsize) then raise Exception.Create('Table "'+filename+'" is corrupt.');
      dec(datasize,i);
      if datasize<>0 then raise Exception.Create('Table "'+filename+'" is corrupt.');
    end;
    proctim:=proctim+(now-stim2);
    stim2:=now;
  end else
  begin
    reccount:=0;
    bufsize:=AllocDataBuffer; if not prebuffer then bufsize:=0;
    databuffer:=bufsize;
    GetMem(data,bufsize);
    GetMem(index,0);
    bufsize:=AllocStructBuffer; if not prebuffer then bufsize:=0;
    structbuffer:=bufsize;
    GetMem(struct,bufsize);
    datalen:=0;
  end;
  for i:=0 to fieldlist.Count-1 do
  begin
    fieldbuild.Add(fieldlist[i]);
    fieldlist[i]:=copy(fieldlist[i],2,length(fieldlist[i])-1);
  end;

 //Parse seekbuilds -- after schema has been loaded
  SetLength(seekbuilddesc, seekbuild.Count);
  for i := 0 to seekbuild.Count - 1 do begin
   //First seekbuild is sometimes '0' and we don't actually seek by it
    if (i=0) and (seekbuild[0]='0') then continue;
    seekbuilddesc[i].fields := ParseSeekDescription(seekbuild[i]);
  end;

  tottim:=tottim+(now-stim);
{  if filename='Words' then showmessage(filename+#13#13+'Tot:'+formatdatetime('hh:nn:ss.zzz',tottim)+#13+
                              'Data:'+formatdatetime('hh:nn:ss.zzz',datatim)+#13+
                              'Oth:'+formatdatetime('hh:nn:ss.zzz',othtim)+#13+
                              'Proc:'+formatdatetime('hh:nn:ss.zzz',proctim)+#13);}
  loaded:=true;
  for i:=0 to fieldcount-1 do
  begin
    cn:=0;
    for j:=0 to reccount-1 do inc(cn,GetFieldSize(j,i));
    statlist.Add('  Field '+fieldlist[i]+': '+inttostr(cn div 1000)+'k');
  end;
  statlist.Add('');
//  sl:=TStringList.Create;
//  assignfile(t,tablename+'.dump');
//  rewrite(t);
//  ExportToText(t,'Traa');
//  closefile(t);
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
    f1,f2:file;
    bufdata,bufstruct,buforder:pointer;
    bufdatapos,bufstructpos,buforderpos:integer;
    c:char;
    l:integer;
    w,wn:word;
    strubuf:pointer;
    mypos:integer;
begin
  if not loaded then load;
  if offline then
  begin
    showmessage('Cannot write OFFLINE table!');
    exit;
  end;
  assignfile(t,filename+'.info');
  rewrite(t);
  writeln(t,'$TEXTTABLE');
  if prebuffer then writeln(t,'$PREBUFFER');
  writeln(t,'$PRECOUNTED');
  if rawindex then writeln(t,'$RAWINDEX');
  writeln(t,'$FIELDS');
  for i:=0 to fieldbuild.Count-1 do writeln(t,fieldbuild[i]);
  writeln(t,'$ORDERS');
  for i:=0 to orders.Count-1 do writeln(t,orders[i]);
  writeln(t,'$SEEKS');
  for i:=0 to seekbuild.Count-1 do writeln(t,seekbuild[i]);
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
    blockwrite(f2,struct^,reccount*varfields+reccount*5);
    closefile(f1);
    closefile(f2);
    assignfile(f1,filename+'.index');
    rewrite(f1,1);
    blockwrite(f1,index^,orders.Count*reccount*4);
    closefile(f1);
  end else
  begin
    bufdatapos:=0; bufstructpos:=0; buforderpos:=0;
    i:=recordcount-numberdeleted;
    wfbuf(f1,bufdata,bufdatapos,64000,@i,0,4);
    mypos:=0;
    for i:=0 to recordcount-1 do
    begin
      moveofs(struct,@b,i*(varfields+5),0,1);
      if b=1 then il.Add('DEAD') else
      begin
        wfbuf(f2,bufstruct,bufstructpos,64000,@b,0,1);
        wfbuf(f2,bufstruct,bufstructpos,64000,@mypos,0,4);
        il.Add(inttostr(j));
        inc(j);
        w:=0;
        moveofs(struct,@l,i*(varfields+5)+1,0,4);
        for k:=0 to fieldcount-1 do
        begin
          b:=GetFieldSize(i,k);
          c:=fieldtypes[k+1];
          if (c='x') or (c='s') then
            wfbuf(f2,bufstruct,bufstructpos,64000,@b,0,1);
          wfbuf(f1,bufdata,bufdatapos,64000,data,l+w,b);
          w:=w+b;
          mypos:=mypos+b;
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
begin
  seeks.Free;
  orders.Free;
  fieldlist.Free;
  seekbuild.Free;
  fieldbuild.Free;
  freemem(data);
  freemem(index);
  freemem(struct);
end;

function TTextTable.NewCursor: TTextTableCursor;
begin
  Result := TTextTableCursor.Create(Self);
end;


{
GetFieldSize()
Heavily used function. Should be very optimized.
}
function TTextTable.GetFieldSize(recno,field:integer):byte;
var c:char;
begin
  c:=fieldsizes[field+1];
  if (c>='1') and (c<='9') then
    Result := ord(c)-ord('0')
  else
    Result := OffsetPtr(struct, recno*(varfields+5)+(ord(c)-ord('a'))+5)^;
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
var i,ii:integer;
  ofs:integer;
  sz:byte;
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
  if not loaded then load;
  if rec>=reccount then
    raise Exception.Create('Read beyond!');
  ii:=rec*5+varfields*rec;
  ofs:=PInteger(OffsetPtr(struct, ii+1))^;
  for i:=0 to field-1 do ofs:=ofs+GetFieldSize(rec,i);
  tp:=fieldtypes[field+1];
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

//Setting fields is not supported for offline dictionaries.
//It's only used for user data anyway, which is never offline.
procedure TTextTable.SetField(rec:integer;field:integer;const value:string);
var i,ii:integer;
  ofs:integer;
  sz:byte;
  tp:char;
  b:byte;
  w:word;
  l:integer;
 {$IFDEF UNICODE}
  ansi_s: AnsiString;
  value_c: UnicodeString;
 {$ELSE}
  pb: PByte;
  pc: PChar;
 {$ENDIF}
begin
  if rec>=reccount then
    raise Exception.Create('Write beyond!');
  ii:=rec*5+varfields*rec;
  ofs:=PInteger(OffsetPtr(struct, ii+1))^;
  for i:=0 to field-1 do ofs:=ofs+GetFieldSize(rec,i);
  tp:=fieldtypes[field+1];
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
    //TODO: Test for non-unicode
     pb := OffsetPtr(data, ofs);
     pc := PChar(value);
     for i := 0 to sz div 2 - 1 do begin
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

{$IFDEF CURSOR_IN_TABLE}
function TTextTable.Str(field:integer):string;
begin
  Result := _intcur.Str(field);
end;

function TTextTable.Int(field:integer):integer;
begin
  Result := _intcur.Int(field);
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
    Result:=PInteger(OffsetPtr(index, (order*reccount+rec)*4))^;
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
  if not loaded then load;
  Result.ind_i:=seeks.IndexOf(seek)-1;
  Result.reverse:=false;
  if (seek[1]='<') then
  begin
    system.delete(seek,1,1);
    Result.reverse:=true;
  end;
  Result.fld_i:=fieldlist.IndexOf(seek);
end;

{ LocateRecord():
 Locates first matching record by using a given seek table.
 Returns true and record index in seek table if there are matches,
 or false and record index for record just after where the match would have occured.
 To convert seek index to record index call TransOrder(idx, seek.ind_i) }

function TTextTable.LocateRecord(seek: PSeekObject; value:string; number:boolean; out idx: integer):boolean;
var sn:integer;       //seek table number
  fn:integer;         //field number
  reverse:boolean;
  l,r,c:integer;
  s:string;
  i_val: integer;    //integer value for "value", when number==true
  i_s: integer;      //integer value for "s", when number==true
  RecNo: integer;
begin
  if not loaded then load;
  sn := seek.ind_i;
  fn := seek.fld_i;
  reverse := seek.reverse;

  idx := -1;
  Result := false;
  if (sn<-1) or (fn<0) then
    exit;

  if number then begin
    if not TryStrToInt(value, i_val) then
      exit;
  end else begin
    if reverse then value:=ReverseString(value);
    value := uppercase(value);
  end;

  idx := RecCount+1; //if there are no records this'll be used

 //Initiate binary search
  l:=0;
  r:=reccount-1;
  if l<=r then repeat
    c:=((r-l) div 2)+l;
    s:=GetField(TransOrder(c,sn),fn);
    if reverse then
      s:=ReverseString(s);
    if number then
    begin
      if (length(s)>0) and (s[length(s)]='''') then system.delete(s,length(s),1);
      if (length(s)>0) and (s[1]='''') then system.delete(s,1,1);
      if not TryStrToInt(s, i_s) then
        r := c
      else
        if i_val<=i_s then r:=c else l:=c+1;
    end else begin
      if rawindex then
        if value<=uppercase(s) then r:=c else l:=c+1
      else
        if AnsiCompareStr(value,uppercase(s))<=0 then r:=c else l:=c+1;
    end;
    if l>=r then
    begin
      idx:=l;
      RecNo:=TransOrder(idx,sn);
      while (idx<reccount) and (IsDeleted(RecNo)) do
      begin
        inc(idx);
        RecNo:=TransOrder(idx,sn);
      end;
      if idx<reccount then
        s:=GetField(TransOrder(idx,sn),fn);
      Result := (idx<reccount);
      if Result then
        if number then begin
          if (not TryStrToInt(s, i_s)) or (i_val<>i_s) then
            Result := false;
        end else begin
          if value<>uppercase(s) then
            Result := false;
        end;
      exit;
    end;
  until false;
end;

{$IFDEF CURSOR_IN_TABLE}
//Faster version, by seek object
function TTextTable.Locate(seek: PSeekObject; const value:string; number:boolean):boolean;
begin
  Result := _intcur.Locate(seek, value, number);
end;

//Slower version, specifying seek by a string name
function TTextTable.Locate(seek,value:string;number:boolean):boolean;
begin
  Result := _intcur.Locate(seek,value,number);
end;
{$ENDIF}

function TTextTable.Field(const field:string):integer;
begin
  result:=fieldlist.IndexOf(field);
end;

function TTextTable.IsDeleted(rec:integer):boolean;
var b:boolean;
begin
  moveofs(struct,@b,rec*(varfields+5),0,1);
  result:=b;
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

{ Appends record to the table, returns its RecNo }
function TTextTable.AddRecord(values:array of string): integer;
var totsize:integer;
    i:integer;
    p:pointer;
    c:char;
    b:byte;
    a:array of byte;
    w:word;
    k:integer;
begin
  if not loaded then load;
  if offline then
    raise Exception.Create('Cannot insert into offline table.');
  totsize:=0;
  if High(values)+1<>fieldcount then raise Exception.Create('Invalid values array count (TTextTable.Insert).');
  for i:=0 to fieldcount-1 do
    if length(values[i])>250 then
      values[i]:=copy(values[i],1,250);
  for i:=0 to fieldcount-1 do
    Inc(totsize, GetFieldValueSize(fieldbuild[i][1], values[i]));
  if databuffer<totsize then
  begin
    GetMem(p,datalen+AllocDataBuffer);
    move(data^,p^,datalen);
    freemem(data);
    databuffer:=AllocDataBuffer;
    data:=p;
  end;
  dec(databuffer,totsize);
  if structbuffer<varfields+5 then
  begin
    GetMem(p,reccount*varfields+reccount*5+AllocStructBuffer);
    move(struct^,p^,reccount*varfields+reccount*5);
    freemem(struct);
    structbuffer:=AllocStructBuffer;
    struct:=p;
  end;
  dec(structbuffer,5+varfields);
  b:=0;
  moveofs(@b,struct,0,reccount*(varfields+5),1);
  moveofs(@datalen,struct,0,reccount*(varfields+5)+1,4);
  w:=0;
  k:=0;
  for i:=0 to fieldcount-1 do
  begin
    c:=fieldbuild[i][1];
    b := GetFieldValueSize(c, values[i]);
    if (c='x') or (c='s') then
    begin
      moveofs(@b,struct,0,reccount*(varfields+5)+5+k,1);
      inc(k);
    end;
    inc(datalen,b);
  end;
  SetLength(a,fieldcount);
  for i:=0 to fieldcount-1 do a[i]:=i;
  inc(reccount);
  Result := reccount-1;
  EditRecord(RecordCount-1, a, values, {JustInserted=}true);
end;

procedure TTextTable.DeleteRecord(RecNo: integer);
begin
  if not loaded then load;
  PBoolean(integer(struct)+RecNo*(varfields+5))^ := true;
  inc(numberdeleted);
end;

procedure TTextTable.EditRecord(RecNo: integer; const fields:array of byte;
  const values:array of string; JustInserted:boolean=false);
var i,j:integer;
    sz:byte;
    tp:char;
    willinsert:boolean;
    a:array of string;
    fnd:boolean;
begin
  if not loaded then load;
  if offline then
  begin
    showmessage('Cannot edit OFFLINE table!');
    exit;
  end;
  willinsert:=false;
  for i:=0 to High(values) do
  begin
    sz:=GetFieldSize(RecNo,fields[i]);
    tp:=fieldtypes[fields[i]+1];
    if ((tp='s') and (sz<>length(values[i])))
   {$IFDEF UNICODE}
    or ((tp='x') and (sz<>length(values[i]) * 2))
   {$ELSE}
    or ((tp='x') and (sz<>length(values[i]) div 2))
   {$ENDIF}
    then willinsert:=true;
  end;
  if not justinserted and willinsert then
  begin
    SetLength(a,fieldcount);
    for i:=0 to fieldcount-1 do
    begin
      fnd:=false;
      for j:=0 to High(values) do
        if fields[j]=i then
        begin
          fnd:=true;
          a[i]:=values[j];
        end;
      if not fnd then a[i]:=Str(i);
    end;
    Delete;
    Insert(a);
    exit;
  end;
  for i:=0 to High(values) do
    SetField(RecNo, fields[i], values[i]);
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



function TTextTable.ParseSeekDescription(fld:string): TSeekFieldDescriptions;
var i:integer;
  fx:string;
  reverse:boolean;
  desc: PSeekFieldDescription;
begin
  SetLength(Result, 0);
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
    i:=Field(fx);
    if i=-1 then raise Exception.Create('Unknown seek field '+fx+' (TTextTable.Commit).');

    SetLength(Result, Length(Result)+1);
    desc := @Result[Length(Result)-1];
    desc.index := i;
    desc.reverse := reverse;
    desc.strdata := (fieldbuild[i][1]='s') or (fieldbuild[i][1]='x')
  end;
end;

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
  Result := SortRec(r, seekbuilddesc[seekIndex].fields);
end;

{ This is the functionality older SortRec had. I'm keeping it just in case
 there really is a case when we SortRec by something other than the contents of
 a built-in index. }
function TTextTable.SortRecByStr(r:integer; fld:string):string;
var desc: TSeekFieldDescriptions;
begin
  desc := ParseSeekDescription(fld);
  Result := SortRec(r, desc);
end;

procedure TTextTable.Commit(RecNo:integer; JustInserted: boolean);
var p:pointer;
    i:integer;
    j,k:integer;
    s:string;
    fnd:boolean;
begin
  if not loaded then load;
  if justinserted then
  begin
    getmem(p,reccount*orders.Count*4);
    for i:=0 to orders.Count-1 do moveofs(index,p,i*(reccount-1)*4,i*reccount*4,(reccount-1)*4);
    freemem(index);
    index:=p;
  end else
  begin
    for i:=0 to orders.Count-1 do
      for j:=0 to reccount-1 do if TransOrder(j,i)=RecNo then
        moveofs(index,index,i*reccount*4+j*4+4,i*reccount*4+j*4,(reccount-j-1)*4);
  end;
  k:=RecNo;
  for i:=0 to orders.Count-1 do
  begin
    s:=sortrec(RecNo,i+1);
    fnd:=false;
    for j:=0 to reccount-2 do if AnsiCompareStr(sortrec(TransOrder(j,i),i+1),s)>=0 then
    begin
      moveofs(index,index,i*reccount*4+j*4,i*reccount*4+j*4+4,(reccount-j-1)*4);
      moveofs(@k,index,0,i*reccount*4+j*4,4);
      fnd:=true;
      break;
    end;
    if not fnd then moveofs(@k,index,0,i*reccount*4+(reccount-1)*4,4);
  end;
end;

{procedure TTextTable.SortIndex(ino:integer;a,b:integer);
var c:integer;
    li,ri:TStringList;
    i,k:integer;
    sc,sa:string;
begin
  if not loaded then load;
  if a=b then exit;
  // choose cue point
  c:=a+(b-a) div 2;
  sc:=SortRec(TransOrder(c,ino),seekbuild[ino+1]);
  li:=TStringList.Create;
  ri:=TStringList.Create;
  for i:=a to b do
  if i<>c then begin
    sa:=SortRec(TransOrder(i,ino),seekbuild[ino+1]);
    if sa>sc then ri.Add(inttostr(TransOrder(i,ino))) else li.Add(inttostr(TransOrder(i,ino)));
  end;
  for i:=0 to li.Count-1 do
  begin
    k:=strtoint(li[i]);
    moveofs(@k,index,0,ino*reccount*4+a*4+i*4,4);
  end;
  k:=c;
  moveofs(@k,index,0,ino*reccount*4+a*4+li.Count*4,4);
  for i:=0 to ri.Count-1 do
  begin
    k:=strtoint(ri[i]);
    moveofs(@k,index,0,ino*reccount*4+a*4+i*4+li.Count*4+4,4);
  end;
  if li.Count>1 then SortIndex(ino,a,a+li.Count-1);
  if ri.Count>1 then SortIndex(ino,b-ri.Count+1,b);
  li.Free;
  ri.Free;
end;}

function CustomSortCompare(list:TStringList;index1,index2:integer):integer;
begin
  result:=CompareStr(uppercase(list[index1]),uppercase(list[index2]));
end;

procedure TTextTable.Reindex;
var i,j:integer;
  sl:TStringList;
  p:pointer;
begin
  if not loaded then load;
  getmem(p,reccount*orders.Count*4);
  freemem(index);
  index:=p;
  sl:=TStringList.Create;
  for i:=0 to orders.Count-1 do
  begin
    sl.Clear;
    for j:=0 to reccount-1 do
      sl.AddObject(SortRec(j,i+1),pointer(j));
    if rawindex then
      sl.CustomSort(CustomSortCompare)
    else
      sl.Sort;
    for j:=0 to reccount-1 do
      PInteger(integer(index)+i*reccount*4+j*4)^ := integer(sl.Objects[j]);
  end;
  sl.Free;
end;

function TTextTable.CheckIndex:boolean;
var i,j:integer;
    s1,s2:string;
begin
  if not loaded then load;
  for i:=0 to orders.Count-1 do if seekbuild.Count>i+1 then
  begin
    s1:='';
    for j:=0 to reccount-1 do
    begin
      s2:=sortrec(transorder(j,i),i+1);
      if ((not rawindex) and (AnsiCompareStr(s2,s1)<0)) or
         ((rawindex) and (s2<s1)) then
      begin
        result:=false;
        exit;
      end;
      s1:=s2;
    end;
  end;
  result:=true;
end;

procedure TTextTable.ExportToText(var t:textfile;ord:string);
var i,j:integer;
    s,s2:string;
    ordn:integer;
begin
  if not loaded then load;
  s:='';
  for i:=0 to fieldlist.Count-1 do s:=s+';'+fieldlist[i];
  ordn:=orders.IndexOf(ord);
  s[1]:='>';
  writeln(t,s);
  for i:=0 to reccount-1 do
  begin
    s:='';
    for j:=0 to fieldlist.Count-1 do
    begin
      s2:=GetField(TransOrder(i,ordn),j);
      while pos(';',s2)>0 do s2:=copy(s2,1,pos(';',s2)-1)+','+copy(s2,pos(';',s2)+1,length(s2)-pos(';',s2));
      s:=s+';'+s2;
    end;
    s[1]:='+';
    writeln(t,s);
  end;
  writeln(t,'.');
end;

procedure TTextTable.ImportFromText(var t:textfile;smf:TSMPromptForm;mess:string);
var s:string;
    i,j:integer;
    fld:TStringList;
    a:array of string;
    s2:string;
    cnt:integer;
begin
  nocommit:=true;
  readln(t,s);
  i:=0;
  system.delete(s,1,1);
  fld:=TStringList.Create;
  while s<>'' do
  begin
    if s[1]=';' then system.delete(s,1,1);
    if pos(';',s)>0 then s2:=copy(s,1,pos(';',s)-1) else s2:=s;
    system.delete(s,1,length(s2));
    fld.Add(s2);
    inc(i);
  end;
  readln(t,s);
  cnt:=0;
  while s[1]<>'.' do
  begin
    inc(cnt);
    if cnt mod 100=0 then if smf<>nil then smf.SetMessage(mess+' ('+inttostr(cnt)+')...');
    system.delete(s,1,1);
    SetLength(a,fieldcount);
    for i:=0 to fieldcount-1 do a[i]:='0';
    i:=0;
    while s<>'' do
    begin
      if s[1]=';' then system.delete(s,1,1);
      if (pos(';',s)>0) and (i<fieldcount-1) then s2:=copy(s,1,pos(';',s)-1) else s2:=s;
      system.delete(s,1,length(s2));
      j:=fieldlist.IndexOf(fld[i]);
      if j<>-1 then a[j]:=s2;
      inc(i);
    end;
    Insert(a);
    readln(t,s);
  end;
  fld.Free;
  nocommit:=false;
  if smf<>nil then smf.SetMessage(mess+' (index)...');
  Reindex;
  if smf<>nil then smf.SetMessage(mess+' (finish)...');
end;

procedure LogAlloc(s:string;len:integer);
//var t:textfile;
begin
{  assignfile(t,'alloc.log');
  append(t);
  inc(totalalloc,len);
  writeln(t,s,' - '+inttostr(len div 1024),' > ',inttostr(totalalloc div 1024));
  closefile(t);}
end;

function TTextTable.HasIndex(const index:string):boolean;
begin
  Result := orders.IndexOf(index)>=0;
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
  if not Table.Loaded then Table.Load;
  cur:=-1;
  tcur:=0;
  Next;
end;

procedure TTextTableCursor.Next;
var stop:boolean;
begin
  if not Table.Loaded then Table.Load;
  repeat
    stop:=false;
    inc(cur);
    if cur<Table.RecCount then
    begin
      tcur:=Table.TransOrder(cur,curorder);
      if tcur>=Table.RecCount then showmessage('index problem!');
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
  if not Table.Loaded then Table.Load;
  result:=(cur>=Table.RecCount);
end;

procedure TTextTableCursor.SetOrder(const index:string);
begin
  if not Table.Loaded then Table.Load;
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
function TTextTableCursor.Locate(seek,value:string; number:boolean):boolean;
var so: TSeekObject;
begin
  so := Table.GetSeekObject(seek);
  Result := Locate(@so, value, number);
end;

//Faster version, by seek object
function TTextTableCursor.Locate(seek: PSeekObject; const value:string; number:boolean):boolean;
var idx: integer;
begin
  Result := Table.LocateRecord(seek, value, number, idx);
  cur := idx;
  if (idx>=0) and (idx<Table.RecordCount) then
    tcur := Table.TransOrder(idx, seek.ind_i)
  else
    tcur := Table.RecordCount; //-1 is not detected as EOF
end;



function TTextTableCursor.Str(field:integer):string;
begin
  Result:=Table.GetField(tcur,field);
end;

function TTextTableCursor.Int(field:integer):integer;
begin
  if not TryStrToInt(Table.GetField(tcur,field), Result) then
    Result := 0;
end;

function TTextTableCursor.Bool(field:integer):boolean;
var s: string;
begin
  s := Table.GetField(tcur,field);
  Result :=(Length(s)>0) and (UpCase(s[1])='T');
end;

//Returns one character (in older versions nothing is guaranteed but it was always this way)
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
function TTextTableCursor.Fch(field:integer):string;
begin
  Result := Str(field);
end;
{$ENDIF}


initialization
  dataalloc:=0;
  structalloc:=0;
  indexalloc:=0;
  tottim:=0;
  datatim:=0;
  othtim:=0;
  proctim:=0;
  statlist:=TStringList.Create;

finalization
  statlist.Free;
end.
