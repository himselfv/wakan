program JaletSubConv;

uses SysUtils;

{$R *.RES}

function UnicodeToHex(s:widestring):string;
var i:integer;
    c:widechar;
    d:word;
    s2,s3:string;
begin
  s2:='';
  for i:=1 to length(s) do
  begin
    c:=s[i];                     
    d:=word(c);
    s3:=format('%4.4X',[d]);
    s2:=s2+s3;
  end;
  result:=s2;
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

procedure ConvUniToMixUni(inpf,outf:string);
var f:file of byte;
    t:textfile;
    bl,bh:byte;
    s:string;
    inuni:boolean;
begin
  assignfile(f,inpf);
  reset(f);
  assignfile(t,outf);
  rewrite(t);
  s:='';
  inuni:=false;
  while not eof(f) do
  begin
    read(f,bl);
    read(f,bh);
    if (bh=0) and (bl=10) then
    begin
      if inuni then s:=s+']';
      writeln(t,s);
      s:='';
      inuni:=false;
    end;
    if (bh=0) and (bl<>10) and (bl<>13) then
    begin
      if inuni then s:=s+']';
      inuni:=false;
      s:=s+chr(bl);
    end;
    if (bh<>0) and ((bh<>254) or (bl<>255)) then
    begin
      if not inuni then s:=s+'[';
      inuni:=true;
      s:=s+Format('%2.2X%2.2X',[bh,bl]);
    end;
  end;
  if s<>'' then writeln(t,s);
  closefile(f);
  closefile(t);
end;

procedure WritelnMixUni(var f:file;s:string;raw:boolean);
var bl,bh:byte;
    inuni:boolean;
    s2:string;
begin
  inuni:=false;
  while length(s)>0 do
  begin
    if inuni and (s[1]=']') then
    begin
      inuni:=false;
      delete(s,1,1);
    end else if not inuni and (s[1]='[') and not raw then
    begin
      inuni:=true;
      delete(s,1,1);
    end else if inuni then
    begin
      s2:=copy(s,1,2);
      delete(s,1,2);
      bh:=strtoint('0x'+s2);
      s2:=copy(s,1,2);
      delete(s,1,2);
      bl:=strtoint('0x'+s2);
      blockwrite(f,bl,1);
      blockwrite(f,bh,1);
    end else
    begin
      bh:=0;
      bl:=ord(s[1]);
      delete(s,1,1);
      blockwrite(f,bl,1);
      blockwrite(f,bh,1);
    end;
  end;
  bh:=0;
  bl:=13;
  blockwrite(f,bl,1);
  blockwrite(f,bh,1);
  bh:=0;
  bl:=10;
  blockwrite(f,bl,1);
  blockwrite(f,bh,1);
end;

var t,t2:textfile;
    s,s2:string;
    k:integer;
    f:file;
    b:byte;
    cbeg,cend,cnam,cst8,cst3,cst2,cst1,cst0:string;
    s3:string;
    spl:array[1..10] of string;
    dbeg,dend,ddur,dlast:TDateTime;
    aftertrans:boolean;
procedure split;
var i:integer;
begin
  i:=1;
  while pos(';',s2)>0 do
  begin
    spl[i]:=copy(s2,1,pos(';',s2)-1);
    delete(s2,1,pos(';',s2));
    inc(i);
  end;
  spl[i]:=s2;
end;
begin
  try
  writeln('JALET SUBTITLE CONVERTER (C) LABYRINTH 2003');
  writeln;
  if paramcount<2 then
  begin
    writeln('Usage: JALETSUBCONV <input SSA file> <annotation file> [<output SSA file>]');
    exit;
  end;
  writeln('Converting '+paramstr(1)+'...');
  dlast:=0;
  ConvUniToMixUni(paramstr(1),'_TEMP_SSA.CNV');
  if paramcount=2 then
  begin
    writeln('Writing ',paramstr(2),'...');
    assignfile(t,'_TEMP_SSA.CNV');
    reset(t);
    assignfile(f,paramstr(2));
    rewrite(f,1);
    b:=255;
    blockwrite(f,b,1);
    b:=254;
    blockwrite(f,b,1);
    aftertrans:=false;
    while (not eof(t)) and (not aftertrans) do
    begin
      readln(t,s);
      if s=';trans' then aftertrans:=true;
      if pos('Dialogue: ',s)=1 then
      begin
        k:=9;
        while (pos(',',s)>0) and (k>0) do
        begin
          dec(k);
          delete(s,1,pos(',',s));
        end;
        writelnmixuni(f,s,false);
      end;
    end;
    closefile(t);
    closefile(f);
  end else
  begin
    writeln('Converting ',paramstr(2),'...');
    ConvUniToMixUni(paramstr(2),'_TEMP_ANN.CNV');
    writeln('Writing ',paramstr(3),'...');
    assignfile(t,'_TEMP_SSA.CNV');
    reset(t);
    assignfile(t2,'_TEMP_ANN.CNV');
    reset(t2);
    assignfile(f,paramstr(3));
    rewrite(f,1);
    b:=255;
    blockwrite(f,b,1);
    b:=254;
    blockwrite(f,b,1);
    readln(t2,s2);
    while (length(s2)>0) and (s2[1]<>'>') do readln(t2,s2);
    delete(s2,1,1);
    aftertrans:=false;
    while not eof(t) do
    begin
      readln(t,s);
      if s=';trans' then begin aftertrans:=true; dlast:=0; end;
      if pos('Dialogue: ',s)=1 then
      begin
        k:=9;
        while (pos(',',s)>0) and (k>0) do
        begin
          dec(k);
          if k=8 then cst8:=copy(s,1,pos(',',s)-1);
          if k=7 then cbeg:=copy(s,1,pos(',',s)-1);
          if k=6 then cend:=copy(s,1,pos(',',s)-1);
          if k=4 then cnam:=copy(s,1,pos(',',s)-1);
          if k=3 then cst3:=copy(s,1,pos(',',s)-1);
          if k=2 then cst2:=copy(s,1,pos(',',s)-1);
          if k=1 then cst1:=copy(s,1,pos(',',s)-1);
          if k=0 then cst0:=copy(s,1,pos(',',s)-1);
          delete(s,1,pos(',',s));
        end;
        dbeg:=encodetime(strtoint(cbeg[1]),strtoint(cbeg[3]+cbeg[4]),strtoint(cbeg[6]+cbeg[7]),
          strtoint(cbeg[9]+cbeg[10])*10);
        dend:=encodetime(strtoint(cend[1]),strtoint(cend[3]+cend[4]),strtoint(cend[6]+cend[7]),
          strtoint(cend[9]+cend[10])*10);
        ddur:=dend-dbeg;
        ddur:=ddur+encodetime(0,0,2,0);
        dend:=dbeg+ddur;
        if dbeg<dlast then dbeg:=dlast+encodetime(0,0,0,100);
        dlast:=dend;
        cbeg:=formatdatetime('h:nn:ss.zzz',dbeg);
        delete(cbeg,length(cbeg),1);
        cend:=formatdatetime('h:nn:ss.zzz',dend);
        delete(cend,length(cend),1);
        if aftertrans then
        begin
          writelnmixuni(f,cst8+','+cbeg+','+cend+',Translation,'+cnam+','+cst3+','+cst2+','+cst1+','+cst0+','+s,false);
        end else
        begin
          if s2<>s then
          begin
            writeln('Annotation and input files do not match. Aborting!');
            exit;
          end;
          readln(t2,s2);
          if s2='' then s2:='$';
          while s2[1]<>'f' do readln(t2,s2);
          delete(s2,1,1);
          writelnmixuni(f,cst8+','+cbeg+','+cend+',Japanese,'+cnam+','+cst3+','+cst2+','+cst1+','+cst0+','+s2,false);
          readln(t2,s2);
          while s2[1]<>'F' do readln(t2,s2);
          delete(s2,1,1);
          writelnmixuni(f,cst8+','+cbeg+','+cend+',Furigana,'+cnam+','+cst3+','+cst2+','+cst1+','+cst0+','+s2,false);
          readln(t2,s2);
          s3:=cst8+','+cbeg+','+cend+',Dictionary,'+cnam+','+cst3+','+cst2+','+cst1+','+cst0+',';
          while (not eof(t2)) and (length(s2)>0) and (s2[1]<>':') and (s2[1]<>'%') and (s2[1]<>'>') do readln(t2,s2);
          if s2='' then s2:='$';
          while (not eof(t2)) and (s2[1]=':') do
          begin
            delete(s2,1,1);
            split;
            s3:=s3+'{\fnMS Gothic\c&HFFFFFF&}'+spl[1]+'{\c&H00FFFF&}[3016]'+spl[2]+'[3017]{\fnArial\c&HFFAADD&}'+spl[3]+'\N';
            readln(t2,s2);
            if s2='' then s2:='$';
          end;
          s3:=s3+'\N';
          while (not eof(t2)) and (s2[1]='%') do
          begin
            delete(s2,1,1);
            split;
            s3:=s3+'{\fnMS Gothic\c&HFFFFFF&}'+spl[1]+' {\c&HAAAAAA&}'+spl[2]+' {\c&H00AA00&}[322'+chr(ord(spl[3][1])-1)+
              '] {\c&H00FFFF&}'+spl[4]+'{\c&H0088FF&}'+spl[5]+' {\fnArial\c&HFFAADD&}'+spl[6];
              if spl[7]<>'' then s3:=s3+'; '+spl[7];
              s3:=s3+'\N';
            readln(t2,s2);
            if s2='' then s2:='$';
          end;
          writelnmixuni(f,s3,false);
          delete(s2,1,1);
        end;
      end else writelnmixuni(f,s,true);
    end;
    closefile(t);
    closefile(f);
  end;
  writeln;
  writeln('Conversion successfully done.');
  except
    writeln('Exception '+(ExceptObject as Exception).ClassName+':');
    writeln((ExceptObject as Exception).Message);
  end;
end.
