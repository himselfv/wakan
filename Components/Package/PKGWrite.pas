unit PKGWrite;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, PackageCommon;

type
  TPKGWriteForm = class(TForm)
    Memo1: TMemo;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    procedure FormCreate(Sender: TObject);
  private
    pkgf:file;
    pkgfilename:string;
    companyname,copyrightname,titlename,commentname,name,versionname,formatname:AnsiString;
    loadmode,packmode,cryptmode,crcmode:byte;
    memorylimit:longint;
    filesyscode,headercode,cryptcode:longint;
    headertextname:AnsiString;
    totfsize:longint;
    filenamespecified,fileopened:boolean;
    inclfname:AnsiString;
    inclmask,inclrmask:AnsiString;
    notshow:boolean;
    cryptheader:AnsiString;
    LastDirectory:longint;
    pkghs:PKGCryptStarter;
    curcmd:string;
    HuffLeft:integer;
    Huff:PKGHuffArray;
    IntBuf:array[1..2048] of byte;
    IntBufPos:integer;
    IntBufLen:integer;
    RotBuf:array[1..2048] of byte;
    RotBufSkip,RotBufLen,RotBufStart,RotBufEnd,RotBufAct:integer;
    RotBufInit,RotBufDone:boolean;
    BitStack:array[0..256] of boolean;
    BitStackLen:byte;
    TempCount:longint;
    procedure IncludePath(path:string;mask:string;dirno:longint;recursive:boolean);
    function PrepareHuffman(var inf:file):longint;
    function ProcessHuffman(var inf:file;var buf;buflen:word;LZ77:boolean):integer;

  public
    function PKGWriteCmd(cmd:string):boolean;

  end;

var
  PKGWriteForm: TPKGWriteForm;

implementation

{$R *.DFM}

procedure pkgerr(errs:string;cmd:string);
begin
  filemode:=2;
  MessageDlg('An error occured while building package file:'+#13+errs+#13+
  'Command: '+cmd+#13+'Package file was not built.',mtError,[mbAbort],0);
  if pkgwriteform.fileopened then
  begin
    closefile(pkgwriteform.pkgf);
    erase(pkgwriteform.pkgf);
    pkgwriteform.fileopened:=false;
  end;
  if pkgwriteform.visible then pkgwriteform.hide;
end;

procedure CalculateHuffmanCode(var ha:PKGHuffArray);
var actcnt:word;
    lasthuff:word;
    max1n,max2n:integer;
    max1,max2:longint;
    i:integer;
begin
  actcnt:=256;
  lasthuff:=255;
  for i:=0 to 511 do ha[i].active:=false;
  for i:=0 to 255 do
  begin
    ha[i].parent:=-1; ha[i].bit1:=-1; ha[i].bit0:=-1;
    ha[i].active:=true;
  end;
  while actcnt>1 do
  begin
    max1:=-1;
    max2:=-1;
    max1n:=0;
    max2n:=0;
    for i:=0 to lasthuff do
    begin
      if ha[i].active then
      begin
        if (max1=-1) or (ha[i].count<max1) then
        begin
          max2:=max1;
          max2n:=max1n;
          max1:=ha[i].count;
          max1n:=i;
        end else if (max2=-1) or (ha[i].count<max2) then
        begin
          max2:=ha[i].count;
          max2n:=i;
        end;
      end;
    end;
    inc(lasthuff);
    ha[max1n].parent:=lasthuff;
    ha[max1n].active:=false;
    ha[max2n].parent:=lasthuff;
    ha[max2n].active:=false;
    ha[lasthuff].count:=ha[max1n].count+ha[max2n].count;
    ha[lasthuff].active:=true;
    ha[lasthuff].bit0:=max1n;
    ha[lasthuff].bit1:=max2n;
    ha[lasthuff].parent:=-1;
    dec(actcnt);
  end;
end;

function TPKGWriteForm.PrepareHuffman(var inf:file):longint;
var buf:array[1..2000] of byte;
    i:integer;
    reat:integer;
    fsize:longint;
begin
  seek(inf,0);
  for i:=0 to 511 do Huff[i].count:=0;
  while not eof(inf) do
  begin
    blockread(inf,buf,2000,reat);
    for i:=1 to reat do inc(Huff[buf[i]].count);
  end;
  CalculateHuffmanCode(Huff);
  fsize:=0;
  for i:=0 to 511 do if Huff[i].bit0<>-1 then inc(fsize,Huff[i].count);
  seek(inf,0);
  HuffLeft:=256;
  result:=fsize div 8;
  if fsize mod 8>0 then inc(result);
  for i:=0 to 255 do if Huff[i].count>0 then inc(result,6);
  inc(result);
  IntBufPos:=1;
  IntBufLen:=0;
  BitStackLen:=0;
  TempCount:=0;
  RotBufInit:=false;
  RotBufDone:=false;
  RotBufLen:=2048;
  RotBufSkip:=0;
end;

function TPKGWriteForm.ProcessHuffman(var inf:file;var buf;buflen:word;LZ77:boolean):integer;
var pos:word;
    bitbuf:byte;
    bitcnt:byte;
    bitmul:byte;
    go:word;
    encodechar:boolean;
    bufferdone:boolean;
    curstart,curspos,curapos:integer;
    fndcnt:integer;
    fndhlt:boolean;
    bestdiff,bestlen:integer;
    bitmul2:word;
    tocode:word;
    i:integer;
    rdiff:integer;
    prep:integer;
begin
  if buflen<256*6+1 then pkgerr('Internal error#1.','');
  pos:=0;
  while HuffLeft>0 do
  begin
    dec(HuffLeft);
    if Huff[HuffLeft].count>0 then
    begin
      TByteArray(buf)[pos]:=1;
      inc(pos);
      TByteArray(buf)[pos]:=HuffLeft;
      inc(pos);
      move(Huff[HuffLeft].count,TByteArray(buf)[pos],4);
      inc(pos,4);
    end;
    if pos=buflen then begin result:=buflen; exit; end;
    if HuffLeft=0 then
    begin
      TByteArray(buf)[pos]:=0;
      inc(pos);
    end;
  end;
  if not RotBufInit then
  begin
    blockread(inf,RotBuf,RotBufLen,RotBufEnd);
    if RotBufEnd=0 then
    begin
      result:=pos;
      exit;
    end;
    RotBufStart:=1;
    RotBufAct:=1;
    RotBufInit:=true;
  end;
  while pos<buflen do
  begin
    bitbuf:=0;
    bitcnt:=0;
    bitmul:=1;
    while bitcnt<8 do
    begin
      if (BitStackLen=0) and (RotBufDone) then
      begin
        if bitcnt>0 then
        begin
          TByteArray(buf)[pos]:=bitbuf;
          inc(pos);
        end;
        result:=pos;
        exit;
      end;
      if BitStackLen=0 then
      begin
        if RotBufSkip>0 then dec(RotBufSkip) else
        begin
          encodechar:=true;
          if lz77 then
          begin
            bestlen:=0;
            curstart:=RotBufAct;
            prep:=0;
            while (curstart<>RotBufStart) and (prep<256) do
            begin
              inc(prep);
              dec(curstart);
              if curstart=0 then curstart:=RotBufLen;
              curspos:=curstart;
              curapos:=RotBufAct;
              fndcnt:=0;
              fndhlt:=false;
              repeat
                if RotBuf[curapos]=RotBuf[curspos] then inc(fndcnt) else fndhlt:=true;
                if curapos=RotBufEnd then fndhlt:=true;
                inc(curapos);
                if curapos>RotBufLen then curapos:=1;
                inc(curspos);
                if curspos>RotBufLen then curspos:=1;
              until fndhlt;
              if fndcnt>bestlen then
              begin
                if curstart<RotBufAct then bestdiff:=RotBufAct-curstart else bestdiff:=RotBufLen+RotBufAct-curstart;
                bestlen:=fndcnt;
              end;
            end;
//            if bestlen>3 then writeln(logf,'Repeat: Dif:',bestdiff,' Len:',bestlen);
            if bestlen>3 then
            begin
//              write(logf,'>');
              encodechar:=false;
              if bestlen>259 then bestlen:=259;
              tocode:=bestlen-4;
              bitmul2:=1;
              while bitmul2<256 do
              begin
                 BitStack[BitStackLen]:=(tocode and bitmul2)>0;
                 inc(BitStackLen);
                 bitmul2:=bitmul2*2;
              end;
              tocode:=bestdiff mod 128;
              bitmul2:=1;
              while bitmul2<128 do
              begin
                 BitStack[BitStackLen]:=(tocode and bitmul2)>0;
                 inc(BitStackLen);
                 bitmul2:=bitmul2*2;
              end;
              BitStack[BitStackLen]:=false;
              inc(BitStackLen);
              for i:=1 to bestdiff div 128 do
              begin
                BitStack[BitStackLen]:=true;
                inc(BitStackLen);
              end;
              RotBufSkip:=bestlen-1;
              BitStack[BitStackLen]:=true;
              inc(BitStackLen);
            end;
          end;
          if encodechar then
          begin
            go:=RotBuf[RotBufAct];
            while not Huff[go].active do
            begin
              if (Huff[Huff[go].parent].bit1=go) then BitStack[BitStackLen]:=true
              else if (Huff[Huff[go].parent].bit0=go) then BitStack[BitStackLen]:=false
              else pkgerr('Internal error#2.','');
              inc(BitStackLen);
              inc(TempCount);
              go:=Huff[go].parent;
            end;
            if lz77 then
            begin
//              write(logf,'.');
              BitStack[BitStackLen]:=false;
              inc(BitStackLen);
            end;
          end;
        end;
        bufferdone:=false;
        if IntBufPos>IntBufLen then
        begin
          if eof(inf) then bufferdone:=true else
          begin
            blockread(inf,IntBuf,2000,IntBufLen);
            IntBufPos:=1;
          end;
          if IntBufLen=0 then bufferdone:=true;
        end;
        if (RotBufAct=RotBufEnd) and bufferdone then RotBufDone:=true;
        inc(RotBufAct);
        if RotBufEnd>RotBufAct then rdiff:=RotBufEnd-RotBufAct else rdiff:=RotBufLen+RotBufEnd-RotBufAct;
        if (not bufferdone) and (rdiff<256) then
        begin
          inc(RotBufEnd);
          if RotBufEnd>RotBufLen then RotBufEnd:=1;
          if RotBufEnd=RotBufStart then inc(RotBufStart);
          if RotBufStart>RotBufLen then RotBufStart:=1;
          RotBuf[RotBufEnd]:=IntBuf[IntBufPos];
          inc(IntBufPos);
        end;
        if RotBufAct>RotBufLen then RotBufAct:=1;
      end;
      if BitStackLen>0 then
      begin
        inc(bitcnt);
        dec(BitStackLen);
        if BitStack[BitStackLen] then inc(bitbuf,bitmul);
        bitmul:=bitmul*2;
      end;
    end;
    TByteArray(buf)[pos]:=bitbuf;
    inc(pos);
  end;
  result:=pos;
end;

procedure TPKGWriteForm.IncludePath(path:string;mask:string;dirno:longint;recursive:boolean);
var sr:tsearchrec;
    buf:array[1..2000] of byte;
    pkghf:pkgfile;
    tmpf,hdrf:file;
    ab,ax:boolean;
    b:byte;
    pkghfarr:array[1..sizeof(pkgfile)*2] of byte;
    crc:cardinal;
    crch:byte;
    i:integer;
    reat:integer;
    testlen:integer;
    usedcoding:integer;
    s:string;
    hufflen:integer;
function corr(s:string):string;
var j:integer;
begin
  randseed:=FileSysCode+totfsize;
  result:='';
  for j:=1 to length(s) do
    case s[j] of
      'A'..'Z':result:=result+chr(((26+(ord(s[j])-ord('A'))-random(26)) mod 26)+ord('A'));
      'a'..'z':result:=result+chr(((26+(ord(s[j])-ord('a'))-random(26)) mod 26)+ord('a'));
    else result:=result+s[j];
  end;
end;
begin
  if ((recursive) and (findfirst(path+mask,faAnyFile,sr)=0)) or
     ((not recursive) and (findfirst(path+mask,faAnyFile-faDirectory,sr)=0)) then
  repeat
    if ((sr.attr and faVolumeID=0) and (sr.name<>'.') and (sr.name<>'..')) then
    begin
    if (sr.attr and faDirectory=0) then
    begin
      assignfile(hdrf,path+sr.name);
      try
      ab:=false;
      filemode:=0;
      reset(hdrf,1);
      except
      Memo1.Lines.Add('Unable to open include file ('+sr.name+').'); ab:=true; end;
      if not ab then
      begin
      filemode:=2;
      if pos('.',sr.name)>0 then
      begin
        pkghf.FileName:=copy(sr.name,1,pos('.',sr.name)-1);
        pkghf.FileExt:=copy(sr.name,pos('.',sr.name)+1,length(sr.name)-pos('.',sr.name));
      end else
      begin
        pkghf.FileName:=sr.name;
        pkghf.FileExt:='';
      end;
      if inclfname<>'' then pkghf.FileName:=inclfname;
      inclfname:='';
      end;
    end else begin
      pkghf.FileName:=sr.name;
      pkghf.FileExt:='*DIR*';
    end;
    ax:=true;
    for i:=1 to length(pkghf.Filename) do if pkghf.FileName[i] in ['a'..'z'] then ax:=false;
    if (ax) and (length(pkghf.FileName)<=8) then
    begin
      pkghf.FileName:=LowerCase(pkghf.FileName);
      if pkghf.FileExt='*DIR*' then pkghf.FileName[1]:=Upcase(pkghf.FileName[1]);
    end;
    if pkghf.FileExt<>'*DIR*' then pkghf.FileExt:=lowercase(pkghf.FileExt);
    if not ab then
    begin
    pkghf.FileName:=corr(pkghf.FileName);
    if pkghf.FileExt<>'*DIR*' then pkghf.FileExt:=corr(pkghf.FileExt);
    pkghf.pkgtag:='PKGF';
    b:=125;
    pkghf.cryptmode:=cryptmode;
    pkghf.crcmode:=crcmode;
    if (sr.attr and faDirectory>0) then
    pkghf.loadmode:=3 else pkghf.loadmode:=loadmode;
    if (sr.attr and faDirectory=0) then
    pkghf.filelength:=filesize(hdrf) else pkghf.filelength:=0;
    if (sr.attr and faDirectory=0) then
    case packmode of
      0:begin
          pkghf.packedlength:=pkghf.filelength;
          usedcoding:=0;
        end;
      1:begin
          hufflen:=PrepareHuffman(hdrf);
          if hufflen>pkghf.filelength then
          begin
            pkghf.packedlength:=pkghf.filelength;
            usedcoding:=0;
          end else
          begin
            pkghf.packedlength:=hufflen;
            usedcoding:=1;
          end;
        end;
      2:begin
          hufflen:=PrepareHuffman(hdrf);
          assignfile(tmpf,'LZ77.TMP');
//          assignfile(logf,'LZ77.LOG');
          rewrite(tmpf,1);
//          rewrite(logf);
          reat:=2000;
          while reat=2000 do
          begin
            reat:=ProcessHuffman(hdrf,buf,2000,true);
            blockwrite(tmpf,buf,reat);
          end;
          seek(hdrf,0);
          if (filesize(tmpf)<hufflen) and (filesize(tmpf)<pkghf.filelength) then
          begin
            pkghf.packedlength:=filesize(tmpf);
            usedcoding:=2;
          end else
          if hufflen>pkghf.filelength then
          begin
            pkghf.packedlength:=pkghf.filelength;
            usedcoding:=0;
          end else
          begin
            pkghf.packedlength:=hufflen;
            usedcoding:=1;
            IntBufPos:=1;
            IntBufLen:=0;
            BitStackLen:=0;
            TempCount:=0;
            RotBufInit:=false;
            RotBufDone:=false;
            RotBufLen:=2048;
            HuffLeft:=256;
            RotBufSkip:=0;
          end;
          closefile(tmpf);
//          closefile(logf);
        end;
      else pkgerr('Unknown pack mode.',curcmd);
    end else pkghf.packedlength:=0;
    pkghf.packmode:=usedcoding;
    if (sr.attr and faDirectory=0) and (pkghf.filelength<>0) then
    if not notshow then Memo1.Lines.Add('Including file '+sr.name+' ('+inttostr(round(pkghf.packedlength/pkghf.filelength*100))+'%)...');
    s:='   ';
    case usedcoding of
      0:s:=s+'Compression:NONE ';
      1:s:=s+'Compression:HUFFMAN ';
      2:s:=s+'Compression:LZ77 ';
    end;
    case cryptmode of
      0:s:=s+'Crypting:NONE ';
      1:s:=s+'Crypting:COMPLEX ';
      2:s:=s+'Crypting:SIMPLE ';
    end;
    case crcmode of
      0:s:=s+'Safety:NONE ';
      1:s:=s+'Safety:CRC ';
    end;
    Memo1.Lines.Add(s);
    pkghf.directory:=dirno;
    pkghf.headercrc:=0;
    move(pkghf,pkghfarr,sizeof(pkghf));
    crch:=0;
    for i:=1 to sizeof(pkghf) do crch:=crch+pkghfarr[i];
    pkghf.headercrc:=crch;
    randseed:=totfsize+headercode;
    move(pkghf,pkghfarr,sizeof(pkghf));
    for i:=sizeof(pkghf) downto 1 do pkghfarr[i*2]:=pkghfarr[i];
    for i:=1 to sizeof(pkghfarr) do pkghfarr[i]:=pkghfarr[i] xor random(256);
    blockwrite(pkgf,b,1);
    blockwrite(pkgf,pkghfarr,sizeof(pkghfarr));
    totfsize:=totfsize+1+sizeof(pkghfarr);
    if (sr.attr and faDirectory=0) then
    begin
      if usedcoding=2 then
      begin
        assignfile(tmpf,'LZ77.TMP');
        reset(tmpf,1);
      end;
      testlen:=0;
      randseed:=cryptcode+totfsize;
      reat:=2000;
      while reat=2000 do
      begin
        case usedcoding of
          0:blockread(hdrf,buf,2000,reat);
          1:reat:=ProcessHuffman(hdrf,buf,2000,false);
          2:blockread(tmpf,buf,2000,reat);
          else pkgerr('Unknown pack mode.',curcmd);
        end;
        if reat>0 then
        begin
          inc(testlen,reat);
          case cryptmode of
            0:begin end;
            1:begin
                randseed:=cryptcode+random(1000);
                for i:=1 to 2000 do buf[i]:=buf[i] xor random(256);
              end;
            2:for i:=1 to 2000 do buf[i]:=buf[i] xor ((cryptcode*i) mod 256);
            else pkgerr('Unknown crypt mode.',curcmd);
          end;
          blockwrite(pkgf,buf,reat);
          case crcmode of
            0:begin end;
            1:if filesize(hdrf)>0 then begin
                crc:=0;
                for i:=1 to reat do crc:=(crc+(buf[i] xor i)) mod 12345678;
                blockwrite(pkgf,crc,sizeof(crc));
                totfsize:=totfsize+sizeof(crc);
              end;
            else pkgerr('Unknown CRC mode.',curcmd);
          end;
        end;
      end;
      totfsize:=totfsize+pkghf.packedlength;
//      for i:=256 to 511 do if Huff[i].count>0 then pkgerr('Count err-'+inttostr(i)+'-'+inttostr(Huff[i].count),curcmd);
      if testlen<>pkghf.packedlength then pkgerr('Internal error#3.'+inttostr(testlen)+'/'+inttostr(pkghf.packedlength),curcmd);
      closefile(hdrf);
      if not notshow then Memo1.Lines.Add('Included file '+sr.name+'.');
      if usedcoding=2 then
      begin
        closefile(tmpf);
        erase(tmpf);
      end;
    end else
    begin
      Memo1.Lines.Add('Directory: '+sr.name+':');
      inc(LastDirectory);
      IncludePath(path+sr.name+'\',inclrmask,LastDirectory,true);
    end;
    end;
    end;
  until findnext(sr)<>0;
  findclose(sr);
end;

function TPKGWriteForm.PKGWriteCmd(cmd:string):boolean;
var cmds,cmdp:string;
    proc:boolean;
    tmpf,hdrf:file;
    i:integer;
    reat:integer;
    tmpt:textfile;
    pkgh:pkgheader;
    pkghc:pkgcryptheader;
    pkgft:array[0..5] of AnsiChar;
    b:byte;
    buf:array[1..2000] of byte;
    w:word;
    rnx:word;
begin
  curcmd:=cmd;
  cmdp:='';
  cmds:=copy(cmd,1,pos(' ',cmd));
  if cmds='' then cmds:=cmd else
  begin
    delete(cmds,length(cmds),1);
    cmdp:=copy(cmd,pos(' ',cmd)+1,length(cmd)-pos(' ',cmd));
  end;
  proc:=false;
  cmds:=UpperCase(cmds);
  if cmds='PKGFILENAME' then
  begin
    if cmdp='' then begin pkgerr('PKGFileName needs a parameter.',cmd); result:=false; exit; end;
    filenamespecified:=true;
    fileopened:=false;
    pkgfilename:=cmdp;
    proc:=true;
    companyname:='Unknown';
    copyrightname:='Unknown';
    name:='No';
    titlename:='Unknown';
    formatname:='Unknown';
    versionname:='Unknown';
    commentname:='None';
    headertextname:='No';
    cryptheader:='$';
    inclfname:='';
    memorylimit:=1024*1024;
    if not notshow then Show;
    if not notshow then Memo1.Lines.Clear;
    if not notshow then Memo1.Lines.Add('PKG File Name:'+pkgfilename);
    Invalidate;
    Update;
  end;
  if cmds='CRYPTHEADER' then
  begin if not filenamespecified then begin pkgerr('Filename was not specified.',cmd); result:=false; exit; end;
  proc:=true; cryptheader:=cmdp; end;
  if cmds='COMPANYNAME' then
  begin if not filenamespecified then begin pkgerr('Filename was not specified.',cmd); result:=false; exit; end;
  proc:=true; companyname:=cmdp; end;
  if cmds='COPYRIGHTNAME' then
  begin if not filenamespecified then begin pkgerr('Filename was not specified.',cmd); result:=false; exit; end;
  proc:=true; copyrightname:=cmdp; end;
  if cmds='FORMATNAME' then
  begin if not filenamespecified then begin pkgerr('Filename was not specified.',cmd); result:=false; exit; end;
  proc:=true; formatname:=cmdp; end;
  if cmds='TITLENAME' then
  begin if not filenamespecified then begin pkgerr('Filename was not specified.',cmd); result:=false; exit; end;
  proc:=true; titlename:=cmdp; end;
  if cmds='VERSIONNAME' then
  begin if not filenamespecified then begin pkgerr('Filename was not specified.',cmd); result:=false; exit; end;
  proc:=true; versionname:=cmdp; end;
  if cmds='COMMENTNAME' then
  begin if not filenamespecified then begin pkgerr('Filename was not specified.',cmd); result:=false; exit; end;
  proc:=true; commentname:=cmdp; end;
  if cmds='NAME' then
  begin if not filenamespecified then begin pkgerr('Filename was not specified.',cmd); result:=false; exit; end;
  proc:=true; name:=cmdp; end;
  if cmds='HEADERTEXT' then
  begin if not filenamespecified then begin pkgerr('Filename was not specified.',cmd); result:=false; exit; end;
  proc:=true; headertextname:=cmdp; end;
  if cmds='MEMORYLIMIT' then
  begin if not filenamespecified then begin pkgerr('Filename was not specified.',cmd); result:=false; exit; end;
  proc:=true; try memorylimit:=strtoint(cmdp);
  except pkgerr('Invalid number.',cmd); result:=false; exit; end; end;
  if cmds='FILESYSCODE' then
  begin if not filenamespecified then begin pkgerr('Filename was not specified.',cmd); result:=false; exit; end;
  proc:=true; try filesyscode:=strtoint(cmdp);
  except pkgerr('Invalid number.',cmd); result:=false; exit; end; end;
  if cmds='HEADERCODE' then
  begin if not filenamespecified then begin pkgerr('Filename was not specified.',cmd); result:=false; exit; end;
  proc:=true; try headercode:=strtoint(cmdp);
  except pkgerr('Invalid number.',cmd); result:=false; exit; end; end;
  if cmds='PACKMODE' then
  begin if not fileopened then begin pkgerr('File is not open.',cmd); result:=false; exit; end;
  proc:=true; try packmode:=strtoint(cmdp);
  except pkgerr('Invalid number.',cmd); result:=false; exit; end; end;
  if cmds='CRYPTMODE' then
  begin if not fileopened then begin pkgerr('File is not open.',cmd); result:=false; exit; end;
  proc:=true; try cryptmode:=strtoint(cmdp);
  except pkgerr('Invalid number.',cmd); result:=false; exit; end; end;
  if cmds='CRCMODE' then
  begin if not fileopened then begin pkgerr('File is not open.',cmd); result:=false; exit; end;
  proc:=true; try crcmode:=strtoint(cmdp);
  except pkgerr('Invalid number.',cmd); result:=false; exit; end; end;
  if cmds='CRYPTCODE' then
  begin if not fileopened then begin pkgerr('File is not open.',cmd); result:=false; exit; end;
  proc:=true; try cryptcode:=strtoint(cmdp);
  except pkgerr('Invalid number.',cmd); result:=false; exit; end; end;
  if cmds='PERMANENTLOAD' then
  begin if not fileopened then begin pkgerr('File is not open.',cmd); result:=false; exit; end;
  proc:=true; try loadmode:=0;
  except pkgerr('Invalid number.',cmd); result:=false; exit; end; end;
  if cmds='AUTOLOAD' then
  begin if not fileopened then begin pkgerr('File is not open.',cmd); result:=false; exit; end;
  proc:=true; try loadmode:=1;
  except pkgerr('Invalid number.',cmd); result:=false; exit; end; end;
  if cmds='DEMANDLOAD' then
  begin if not fileopened then begin pkgerr('File is not open.',cmd); result:=false; exit; end;
  proc:=true; try loadmode:=2;
  except pkgerr('Invalid number.',cmd); result:=false; exit; end; end;
  if cmds='TEMPORARYLOAD' then
  begin if not fileopened then begin pkgerr('File is not open.',cmd); result:=false; exit; end;
  proc:=true; try loadmode:=3;
  except pkgerr('Invalid number.',cmd); result:=false; exit; end; end;
  if cmds='NOTSHOW' then begin proc:=true; notshow:=true; end;
  if cmds='WRITEHEADER' then
  begin
    if not filenamespecified then begin pkgerr('Filename was not specified.',cmd); result:=false; exit; end;
    if fileopened then begin pkgerr('File is already open.',cmd); result:=false; exit; end;
    if name='No' then begin pkgerr('Name was not specified.',cmd); result:=false; exit; end;
    assignfile(pkgf,pkgfilename);
    try
      rewrite(pkgf,1);
    except
      pkgerr('Unable to create file.',cmd); result:=false; exit; end;
    assignfile(tmpt,'header.tmp');
    try
      rewrite(tmpt);
    except
      pkgerr('Unable to create temporary file.',cmd); result:=false; exit; end;
    if cryptheader<>'$' then
    begin
      assignfile(hdrf,cryptheader);
      try
        reset(hdrf,1);
      except
        closefile(tmpt); pkgerr('Unable to open crypt header file.',cmd); result:=false; exit; end;
      fillchar(pkghs,sizeof(pkghs),0);
      // fix rnx:=random(16384);
      rnx:=0;
      pkghs.actualstart:=filesize(hdrf)+rnx*2;
      pkghs.pkgtag:=65279;
      while not eof(hdrf) do
      begin
        blockread(hdrf,buf,sizeof(buf),reat);
        blockwrite(pkgf,buf,reat);
      end;
      for i:=1 to rnx do
      begin
        w:=random(65536);
        blockwrite(pkgf,w,2);
      end;
      closefile(hdrf);
    end;
    if headertextname<>'No' then
    begin
      assignfile(hdrf,headertextname);
      try
        reset(hdrf,1);
      except
        closefile(tmpt); pkgerr('Unable to open header text file.',cmd); result:=false; exit; end;
    end;
    if titlename<>'$' then
    begin
      writeln(tmpt,'PKGBOF');
      writeln(tmpt,'Labyrinth Package File');
      writeln(tmpt,'');
      writeln(tmpt,'Title:           ',titlename);
      writeln(tmpt,'Filename:        ',pkgfilename);
      writeln(tmpt,'Format:          ',formatname);
      writeln(tmpt,'Company:         ',companyname);
      writeln(tmpt,'Copyright:       ',copyrightname);
      writeln(tmpt,'Version:         ',versionname);
      writeln(tmpt,'Comments:        ',commentname);
      writeln(tmpt,'');
      writeln(tmpt,'Builder: PKG Builder (C) LABYRINTH 1999-2001');
      writeln(tmpt,'Package file technology (C) LABYRINTH 1999-2001');
      writeln(tmpt,'');
      writeln(tmpt,'If distributing this file, it must be distributed as it is');
      writeln(tmpt,'without any modifications. Note that distributing this file may not');
      writeln(tmpt,'be allowed by the creator of this file and will be lawfully punished.');
      writeln(tmpt,'Ask the creator of this file whether you can distribute it.');
      writeln(tmpt,'');
      writeln(tmpt,'LABYRINTH is not responsible for any damage caused by misuse of this');
      writeln(tmpt,'file and is not responsible for the contents.');
      writeln(tmpt,'');
      writeln(tmpt,'< end of header >');
    end;
    closefile(tmpt);
    assignfile(tmpf,'header.tmp');
    try
      reset(tmpf,1);
    except
      pkgerr('Unable to open temporary file.',cmd); result:=false; exit; end;
    fileopened:=true;
    fillchar(pkgh,sizeof(pkgh),0);
    pkgh.pkgtag:='PKGv2.1>';
    pkgh.pkgname:=name;
    pkgh.headerlength:=filesize(tmpf);
    pkgh.memorylimit:=memorylimit;
    if headertextname<>'No' then
      pkgh.headerlength:=pkgh.headerlength+filesize(hdrf);
    fillchar(pkghc,sizeof(pkghc),0);
    pkghc.pkgtag:='g21';
    pkghc.headerlength:=filesize(tmpf);
    pkghc.memorylimit:=memorylimit;
    if headertextname<>'No' then
      pkghc.headerlength:=pkghc.headerlength+filesize(hdrf);
    if cryptheader='$' then
      blockwrite(pkgf,pkgh,sizeof(pkgh))
      else blockwrite(pkgf,pkghc,sizeof(pkghc));
    while not eof(tmpf) do
    begin
      blockread(tmpf,buf,sizeof(buf),reat);
      blockwrite(pkgf,buf,reat);
    end;
    closefile(tmpf);
    if headertextname<>'No' then
    begin
      while not eof(hdrf) do
      begin
        blockread(hdrf,buf,sizeof(buf),reat);
        blockwrite(pkgf,buf,reat);
      end;
      closefile(hdrf);
    end;
    if not notshow then Memo1.Lines.Add('PKG header was written.');
    loadmode:=0;
    packmode:=0;
    cryptmode:=0;
    crcmode:=1;
    cryptcode:=1;
    proc:=true;
    totfsize:=123;
    lastdirectory:=0;
    inclmask:='*.*';
    inclrmask:='*.*';
    DeleteFile('header.tmp');
  end;
  if cmds='INCLUDEFILENAME' then
  begin
    if not fileopened then begin pkgerr('File is not open.',cmd); result:=false; exit; end;
    if cmdp='' then begin pkgerr('IncludeFileName needs a parameter.',cmd); result:=false; exit; end;
    inclfname:=cmdp;
    proc:=true;
  end;
  if cmds='INCLUDEMASK' then
  begin
    if not fileopened then begin pkgerr('File is not open.',cmd); result:=false; exit; end;
    if cmdp='' then begin pkgerr('IncludeMask needs a parameter.',cmd); result:=false; exit; end;
    inclmask:=cmdp;
    proc:=true;
  end;
  if cmds='INCLUDERECURSIVEMASK' then
  begin
    if not fileopened then begin pkgerr('File is not open.',cmd); result:=false; exit; end;
    if cmdp='' then begin pkgerr('IncludeRecursiveMask needs a parameter.',cmd); result:=false; exit; end;
    inclrmask:=cmdp;
    proc:=true;
  end;
  if cmds='INCLUDE' then
  begin
    if not fileopened then begin pkgerr('File is not open.',cmd); result:=false; exit; end;
    if cmdp='' then begin pkgerr('Include needs a parameter.',cmd); result:=false; exit; end;
    if cmdp[length(cmdp)]<>'\' then cmdp:=cmdp+'\';
    IncludePath(cmdp,inclmask,0,false);
    proc:=true;
  end;
  if cmds='INCLUDETREE' then
  begin
    if not fileopened then begin pkgerr('File is not open.',cmd); result:=false; exit; end;
    if cmdp='' then begin pkgerr('Include needs a parameter.',cmd); result:=false; exit; end;
    if cmdp[length(cmdp)]<>'\' then cmdp:=cmdp+'\';
    IncludePath(cmdp,inclmask,0,true);
    proc:=true;
  end;
  if cmds='FINISH' then
  begin
    if not fileopened then begin pkgerr('File is not open.',cmd); result:=false; exit; end;
    b:=233;
    pkgft:='PKGEOF';
    blockwrite(pkgf,b,1);
    if cryptheader='$' then
      blockwrite(pkgf,pkgft,sizeof(pkgft)) else
    begin
      blockwrite(pkgf,pkghs,sizeof(pkghs));
      //fix rnx:=random(16384);
      rnx:=0;
      for i:=1 to rnx do
      begin
        w:=random(65536);
        if w=65279 then w:=0;
        blockwrite(pkgf,w,2);
      end;
    end;
    closefile(pkgf);
    if not notshow then memo1.lines.add('PKG file was written.');
    if not notshow then hide;
    proc:=true;
  end;
  if not proc then begin pkgerr('Unknown command.',cmd); result:=false; exit; end;
  result:=true;
end;

procedure TPKGWriteForm.FormCreate(Sender: TObject);
begin
  filenamespecified:=false;
  notshow:=false;
  randomize;
end;

end.
