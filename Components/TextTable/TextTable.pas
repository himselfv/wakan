unit TextTable;

interface

uses MemSource,Classes,SysUtils,Dialogs,StdPrompt,Windows;

{
TextTable relies heavily on in-memory structures.
Table header is always kept in memory, and if TextTable.offline is true then
the whole table is, too.
Otherwise the data is read from the disk on demand. 
}

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

  TTextTable=class
    fieldlist:TStringList;
   { Seek tables.
    The point of seek table is to keep records sorted in a particular order.
    The way it's done now, you just have to KNOW what order was that,
    if you're going to use the index. }
    seeks:TStringList;
    seekbuild:TStringList;
    fieldbuild:TStringList;
    orders:TStringList;
    data,struct,index:pointer;
    fieldtypes:string;
    fieldsizes:string;
    varfields:integer;
    cur,tcur:integer;
    curorder:integer;
    filter:string;
    fieldcount:byte;
    reccount:integer;
    rawindex:boolean;
    databuffer,structbuffer:integer;
    prebuffer:boolean;
    datalen:integer;
    justinserted:boolean;
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
    function GetField(rec:integer;field:integer):string;
    function FilterPass(rec:integer;fil:string):boolean;
    function TransOrder(rec,order:integer):integer; inline;
    function IsDeleted(rec:integer):boolean;
    procedure Commit(rec:integer);
    function SortRec(r:integer;fld:string):string;
  public
    constructor Create(source:TPackageSource;filename:string;rawread,offline:boolean);
    destructor Destroy; override;
    procedure First;
    procedure Next;
    function EOF:boolean;
    function GetFieldSize(recno,field:integer):byte;
    function Field(field:string):integer;
    function HasIndex(index:string):boolean;
    function Str(field:integer):string;
    function Int(field:integer):integer;
    function Bool(field:integer):boolean;
    procedure SetOrder(index:string);
    procedure SetFilter(filtr:string);
    function Test(fil:string):boolean;
    procedure WriteTable(filename:string;nodelete:boolean);
    property RecordCount:integer read reccount;
    procedure Insert(values:array of string);
    procedure Delete;
    procedure Edit(fields:array of byte;values:array of string);
    procedure ExportToText(var t:textfile;ord:string);
    procedure ImportFromText(var t:textfile;smf:TSMPromptForm;mess:string);
    procedure Reindex;
    procedure Load;
    function CheckIndex:boolean;
    property NoCommitting:boolean read nocommit write nocommit;

  public
    function GetSeekObject(seek: string): TSeekObject;
    function Locate(seek: PSeekObject; value:string; number:boolean):boolean; overload;
    function Locate(seek,value:string;number:boolean):boolean; overload;

  end;
  TDataCache=array[0..30,0..1] of word;

procedure LogAlloc(s:string;len:integer);
procedure ShowAllocStats;

implementation

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
var sp,dp:pchar;
begin
  sp:=source;
  sp:=sp+ofssource;
  dp:=dest;
  dp:=dp+ofsdest;
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
    struptr:pchar;
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
  cur:=0;
  curorder:=-1;
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
    seekbuild.Add(seeks[i]);
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
      if mf.Size>4 then
      begin
//        showmessage(inttostr(mf.Size)+'.');
//        moveofs(strubuf,@i,mf.Size-3,0,4);
//        showmessage(inttostr(i));
      end;
    end else
    begin
      GetMem(struct,mf.Size+bufsize);
      source.ReadRawData(struct^,integer(mf.Position),mf.Size);
      if mf.Size>4 then
      begin
//        showmessage(inttostr(mf.Size)+'.');
//        moveofs(strubuf,@i,mf.Size-3,0,4);
//        showmessage(inttostr(i));
      end;
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
  justinserted:=false;
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

{
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

procedure TTextTable.WriteTable(filename:string;nodelete:boolean);
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
var i:integer;
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

{
Read the field value for a record.
This is a HEAVILY used function, so let's be as fast as we can.
Everything which is already in memory is read from memory directly. If we're
working in offline mode, it's read from the disc in one go.
}
function TTextTable.GetField(rec:integer;field:integer):string;
const HexChars: AnsiString = '0123456789ABCDEF';
var i,ii:integer;
    ofs:integer;
    sz:byte;
    tp:char;
    b:byte;
    w:word;
    l:integer;
    c:char;
    pb: PByte;
    pc: PChar;
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
       c := PChar(OffsetPtr(data, ofs))^;
     if (c>#0) and (c<>'F') and (c<>'f') then c:='T' else c:='F';
     result:=c;
   end;
  's':begin
     SetLength(Result, sz);
     if offline then
       source.ReadRawData(Result[1],datafpos+ofs,sz)
     else
       move(OffsetPtr(data, ofs)^, Result[1], sz);
   end;
  'x': begin
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
       pc^ := HexChars[1 + pb^ shr 4];
       Inc(pc);
       pc^ := HexChars[1 + pb^ and $0F];
       Inc(pc);
       Inc(pb);
     end;
   end;
   end;
end;

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
    Result := PInteger(OffsetPtr(index, (order*reccount+rec)*4))^;
end;

procedure TTextTable.Next;
var stop:boolean;
begin
  if not loaded then load;
  repeat
    stop:=false;
    inc(cur);
    if cur<reccount then
    begin
      tcur:=TransOrder(cur,curorder);
      if tcur>=reccount then showmessage('index problem!');
      if filter='' then stop:=not IsDeleted(tcur) else stop:=not IsDeleted(tcur) and FilterPass(tcur,filter);
    end else
    begin
      tcur:=cur;
      stop:=true;
    end;
  until stop;
end;

function TTextTable.EOF:boolean;
begin
  if not loaded then load;
  result:=cur>=reccount;
end;

function TTextTable.Str(field:integer):string;
begin
  result:=GetField(tcur,field);
end;

function TTextTable.Int(field:integer):integer;
begin
  result:=0;
  try
    result:=strtoint(GetField(tcur,field));
  except end;
end;

function TTextTable.Bool(field:integer):boolean;
begin
  result:=false;
  try
    if UpCase(GetField(tcur,field)[1])='T' then result:=true;
  except end;
end;


procedure TTextTable.SetOrder(index:string);
begin
  if not loaded then load;
  curorder:=orders.IndexOf(index);
  First;
end;

procedure TTextTable.SetFilter(filtr:string);
var i,j:integer;
    b:boolean;
    s:string;
begin
  filter:=filtr;
end;

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

//Slower version, specifying seek by a string name
function TTextTable.Locate(seek,value:string;number:boolean):boolean;
var so: TSeekObject;
begin
  so := GetSeekObject(seek);
  Result := Locate(@so, value, number);
end;

//Faster version, by seek object
function TTextTable.Locate(seek: PSeekObject; value:string; number:boolean):boolean;
var sn:integer;       //seek table number
  fn:integer;         //field number
  reverse:boolean;
  l,r,c:integer;
  s:string;
  i_val: integer;    //integer value for "value", when number==true
  i_s: integer;      //integer value for "s", when number==true
begin
  if not loaded then load;
  sn := seek.ind_i;
  fn := seek.fld_i;
  reverse := seek.reverse;

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
      result:=true;
      cur:=l;
      tcur:=TransOrder(l,sn);
      while (cur<reccount) and (IsDeleted(tcur)) do
      begin
        inc(cur);
        tcur:=TransOrder(cur,sn);
      end;
      if cur<reccount then s:=GetField(TransOrder(cur,sn),fn);
      if (cur>=reccount) or (value<>uppercase(s)) then result:=false;
      exit;
    end;
  until false;
end;

procedure TTextTable.First;
var a:boolean;
begin
  if not loaded then load;
  cur:=-1;
  tcur:=0;
  Next;
end;

function TTextTable.Test(fil:string):boolean;
begin
  result:=FilterPass(tcur,fil);
end;

function TTextTable.Field(field:string):integer;
begin
  result:=fieldlist.IndexOf(field);
end;

function TTextTable.IsDeleted(rec:integer):boolean;
var b:boolean;
begin
  moveofs(struct,@b,rec*(varfields+5),0,1);
  result:=b;
end;

procedure TTextTable.Insert(values:array of string);
var totsize:integer;
    i,j:integer;
    p:pointer;
    c:char;
    b:byte;
    a:array of byte;
    w:word;
    k:integer;
begin
  if not loaded then load;
  if offline then
  begin
    showmessage('Cannot insert into OFFLINE table!');
    exit;
  end;
  totsize:=0;
  if High(values)+1<>fieldcount then raise Exception.Create('Invalid values array count (TTextTable.Insert).');
  for i:=0 to fieldcount-1 do if length(values[i])>250 then
  begin
    values[i]:=copy(values[i],1,250);
  end;
  for i:=0 to fieldcount-1 do
    case fieldbuild[i][1] of
      'b':inc(totsize,1);
      'w':inc(totsize,2);
      'i':inc(totsize,4);
      'l':inc(totsize,1);
      'x':inc(totsize,length(values[i]) div 2);
      's':inc(totsize,length(values[i]));
    end;
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
    case c of
      'b':b:=1;
      'w':b:=2;
      'i':b:=4;
      'l':b:=1;
      'x':b:=length(values[i]) div 2;
      's':b:=length(values[i]);
    end;
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
  tcur:=reccount-1;
  justinserted:=true;
  Edit(a,values);
  justinserted:=false;
end;

procedure TTextTable.Delete;
var b:boolean;
begin
  if not loaded then load;
  b:=true;
  moveofs(@b,struct,0,tcur*(varfields+5),1);
  inc(numberdeleted);
end;

function HexToUnicode(s:string):widestring;
var s2:widestring;
    d:word;
    c:widechar;
    i:integer;
begin
  s2:='';
  for i:=1 to length(s) div 4 do
  begin
    d:=StrToInt('0x'+copy(s,(i-1)*4+1,4));
    c:=widechar(d);
    s2:=s2+c;
  end;
  result:=s2;
end;

procedure TTextTable.Edit(fields:array of byte;values:array of string);
var i,j,k,l:integer;
    b:byte;
    wo,w:word;
    sz:byte;
    tp:char;
    willinsert:boolean;
    a:array of string;
    fnd:boolean;
    s:widestring;
    m:integer;
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
    sz:=GetFieldSize(tcur,fields[i]);
    tp:=fieldtypes[fields[i]+1];
    if ((tp='s') and (sz<>length(values[i]))) or
       ((tp='x') and (sz<>length(values[i]) div 2)) then willinsert:=true;
  end;
  if not justinserted and willinsert then
  begin
    SetLength(a,fieldcount);
    for i:=0 to fieldcount-1 do
    begin
      fnd:=false;
      for j:=0 to High(values) do if fields[j]=i then
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
  begin
    sz:=GetFieldSize(tcur,fields[i]);
    moveofs(struct,@l,(varfields*tcur)+tcur*5+1,0,4);
    for j:=0 to fields[i]-1 do l:=l+GetFieldSize(tcur,j);
    tp:=fieldtypes[fields[i]+1];
    case tp of
      'b':begin
            b:=0; try b:=strtoint(values[i]); except end;
            moveofs(@b,data,0,l,1);
          end;
      'w':begin
            w:=0; try w:=strtoint(values[i]); except end;
            moveofs(@w,data,0,l,2);
          end;
      'i':begin
            k:=0; try k:=strtoint(values[i]); except end;
            moveofs(@k,data,0,l,4);
          end;
      'l':begin
            if upcase(values[i][1])='T' then b:=1 else b:=0;
            moveofs(@b,data,0,l,1);
          end;
      's':begin
            moveofs(@(values[i][1]),data,0,l,sz);
          end;
      'x':begin
            for m:=1 to sz div 2 do
            begin
              w:=strtoint('0x'+copy(values[i],(m-1)*4+3,2)+copy(values[i],(m-1)*4+1,2));
              moveofs(@w,data,0,l+(m-1)*2,2);
            end;
          end;
    end;
  end;
  if not nocommit then Commit(tcur);
end;

function TTextTable.SortRec(r:integer;fld:string):string;
var s2,s3:string;
    i:integer;
    fx:string;
    reverse:boolean;
begin
  s2:='';
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
    if (fieldbuild[i][1]='s') or (fieldbuild[i][1]='x') then
    begin
      if not reverse then s2:=s2+GetField(r,i)+#9 else s2:=s2+ReverseString(GetField(r,i))+#9;
    end else
    begin
      s3:=GetField(r,i);
      while length(s3)<6 do s3:='0'+s3;
      s2:=s2+s3+#9;
    end;
  end;
  while (length(s2)>0) and (s2[length(s2)]=#9) do system.delete(s2,length(s2),1);
  result:=uppercase(s2);
end;

procedure TTextTable.Commit(rec:integer);
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
      for j:=0 to reccount-1 do if TransOrder(j,i)=rec then
        moveofs(index,index,i*reccount*4+j*4+4,i*reccount*4+j*4,(reccount-j-1)*4);
  end;
  k:=rec;
  for i:=0 to orders.Count-1 do
  begin
    s:=sortrec(rec,seekbuild[i+1]);
    fnd:=false;
    for j:=0 to reccount-2 do if AnsiCompareStr(sortrec(TransOrder(j,i),seekbuild[i+1]),s)>=0 then
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
var i,j,k,l:integer;
    sl:TStringList;
    s:string;
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
    begin
      sl.AddObject(SortRec(j,seekbuild[i+1]),pointer(j));
    end;
    if rawindex then sl.CustomSort(CustomSortCompare) else sl.Sort;
    for j:=0 to reccount-1 do
    begin
      k:=integer(sl.Objects[j]);
      moveofs(@k,index,0,i*reccount*4+j*4,4);
    end;
  end;
  sl.Free;
  if not CheckIndex then showmessage('INTERNAL ERROR: Reindex failed.');
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
      s2:=sortrec(transorder(j,i),seekbuild[i+1]);
      if ((not rawindex) and (AnsiCompareStr(s2,s1)<0)) or
         ((rawindex) and (s2<s1)) then
      begin
//        showmessage('CheckIndex fails:'#13#13+tablename+#13+orders[i]+';'+seekbuild[i+1]+#13+s1+#13+s2+#13+inttostr(j));
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
    i:=0;
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
var t:textfile;
begin
{  assignfile(t,'alloc.log');
  append(t);
  inc(totalalloc,len);
  writeln(t,s,' - '+inttostr(len div 1024),' > ',inttostr(totalalloc div 1024));
  closefile(t);}
end;

function TTextTable.HasIndex(index:string):boolean;
begin
  Result := orders.IndexOf(index)>=0;
end;

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
