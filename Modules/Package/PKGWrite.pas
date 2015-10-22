unit PKGWrite;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, PackageCommon;

const
 //LoadMode
  lmPermanentLoad = 0;
  lmAutoLoad = 1;
  lmDemandLoad = 2;
  lmTemporaryLoad = 3;

type
  TPKGWriteForm = class(TForm)
    Memo1: TMemo;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;

  end;

  EPackageBuildException = class(Exception);

  TLogMessageEvent = procedure(Sender: TObject; const msg: string) of object;

  TPackageBuilder = class
  private
    pkgf:file;
    pkgfilename:string;
    FName,
    FCompanyName,
    FCopyrightName,
    FTitleName,
    FCommentName,
    FVersionName,
    FFormatName: AnsiString;
    FBuilder: AnsiString;
    FLoadMode: byte;
    FPackMode,
    FCryptMode,
    FCrcMode:byte;
    FMemoryLimit:longint;
    FHeaderCode,
    FFilesysCode,
    FCryptCode:longint;
    totfsize:longint;
    filenamespecified,fileopened:boolean;
    inclfname:AnsiString;
    inclmask,inclrmask:string;
    FCryptHeader:string; //if set, some cryptographic header is taken from this file //TODO: clarify
    FHeaderTextName:string; //if set, some header data is taken from this file //TODO: clarify
    LastDirectory:longint;
    pkghs:PKGCryptStarter;
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

  protected
    FOnLogMessage: TLogMessageEvent;
    procedure SetPackageFile(const Value: string);
    procedure Crash(const msg: string);

  public
    constructor Create;
    destructor Destroy; override;
    procedure Log(const msg: string);
    procedure WriteHeader;
    procedure Include(dir: string);
    procedure IncludeTree(dir: string);
    procedure Finish;

    property OnLogMessage: TLogMessageEvent read FOnLogMessage write FOnLogMessage;
    property PackageFile: string read pkgfilename write SetPackageFile;
    property MemoryLimit: longint read FMemoryLimit write FMemoryLimit;
    property Name: AnsiString read FName write FName;
    property CompanyName: AnsiString read FCompanyName write FCompanyName;
    property CopyrightName: AnsiString read FCopyrightName write FCopyrightName;
    property TitleName: AnsiString read FTitleName write FTitleName;
    property CommentName: AnsiString read FCommentName write FCommentName;
    property VersionName: AnsiString read FVersionName write FVersionName;
    property FormatName: AnsiString read FFormatName write FFormatName;
    property Builder: AnsiString read FBuilder write FBuilder;
    property HeaderCode: longint read FHeaderCode write FHeaderCode;
    property FilesysCode: longint read FFilesysCode write FFilesysCode;
    property CryptCode: longint read FCryptCode write FCryptCode;
    property LoadMode: byte read FLoadMode write FLoadMode;
    property PackMode: byte read FPackMode write FPackMode;
    property CryptMode: byte read FCryptMode write FCryptMode;
    property CrcMode: byte read FCrcMode write FCrcMode;
    property CryptHeader: string read FCryptHeader write FCryptHeader;
    property HeaderTextName: string read FHeaderTextName write FHeaderTextName;
    property IncludeFilename: AnsiString read inclfname write inclfname;
    property IncludeMask: string read inclmask write inclmask;
    property IncludeRecursiveMask: string read inclrmask write inclrmask;

  end;

var
  PKGWriteForm: TPKGWriteForm;

implementation

{$R *.DFM}

constructor TPackageBuilder.Create;
begin
  inherited;
  filenamespecified:=false;
  fileopened:=false;
  name:='No';
  companyname:='Unknown';
  copyrightname:='Unknown';
  titlename:='Unknown';
  formatname:='Unknown';
  versionname:='Unknown';
  commentname:='None';
  cryptheader:='$';
  headertextname:='$';
  inclfname:='';
  memorylimit:=1024*1024;
  loadmode:=lmPermanentLoad;
  packmode:=0;
  cryptmode:=0;
  crcmode:=1;
  cryptcode:=1;
  lastdirectory:=0;
  inclmask:='*.*';
  inclrmask:='*.*';
end;

destructor TPackageBuilder.Destroy;
begin
  if fileopened then begin
    closefile(pkgf);
    erase(pkgf);
    fileopened:=false;
  end;
  inherited;
end;

procedure TPackageBuilder.Log(const msg: string);
begin
  if Assigned(FOnLogMessage) then
    FOnLogMessage(Self, msg);
end;

procedure TPackageBuilder.Crash(const msg: string);
begin
  Log(msg);
  raise EPackageBuildException.Create(msg);
 { 'An error occured while building package file:'+#13+errs+#13+
  'Command: '+cmd+#13+'Package file was not built.',mtError,[mbAbort],0); }
end;

procedure TPackageBuilder.SetPackageFile(const Value: string);
begin
  pkgfilename:=Value;
  filenamespecified:=true;
  Log('PKG File Name:'+pkgfilename)
end;

{
var cmds,cmdp:string;
    proc:boolean;
    tmpt:textfile;
}

procedure TPackageBuilder.WriteHeader;
var tmpt:textfile;
  tmpf:file;
  hdrf:file;
  pkgh:pkgheader;
  pkghc:pkgcryptheader;
  rnx:word;
  buf:array[1..2000] of byte;
  reat:integer;
  i:integer;
  w:word;
begin
  if not filenamespecified then Crash('Filename was not specified.');
  if fileopened then Crash('File is already open.');
  if name='No' then Crash('Name was not specified.');

  assignfile(pkgf,pkgfilename);
  try
    rewrite(pkgf,1);
  except
    on E: Exception do
      Crash('Unable to create file: '+E.ClassName+': '+E.Message);
  end;

  assignfile(tmpt,'header.tmp');
  try
    rewrite(tmpt);
  except
    on E: Exception do
      Crash('Unable to create temporary file: '+E.ClassName+': '+E.Message);
  end;

  if cryptheader<>'$' then
  begin
    assignfile(hdrf,cryptheader);
    try
      reset(hdrf,1);
    except
      on E: Exception do begin
        closefile(tmpt);
        Crash('Unable to open crypt header file: '+E.ClassName+': '+E.Message);
      end;
    end;
    fillchar(pkghs,sizeof(pkghs),0);
    // fix rnx:=encmask(16384);
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
      w:=encmask(65536);
      blockwrite(pkgf,w,2);
    end;
    closefile(hdrf);
  end;

  if headertextname<>'$' then
  begin
    assignfile(hdrf,headertextname);
    try
      reset(hdrf,1);
    except
      on E: Exception do begin
        closefile(tmpt);
        Crash('Unable to open header text file: '+E.ClassName+': '+E.Message);
      end;
    end;
  end;

  if titlename<>'$' then
  begin
    if FBuilder='' then
      FBuilder:='PKG Builder (C) LABYRINTH 1999-2001';
    writeln(tmpt,'PKGBOF');
    writeln(tmpt,'Labyrinth Package File');
    writeln(tmpt,'');
    writeln(tmpt,'Title:           ',titlename);
    writeln(tmpt,'Filename:        ',ExtractFilename(pkgfilename));
    writeln(tmpt,'Format:          ',formatname);
    writeln(tmpt,'Company:         ',companyname);
    writeln(tmpt,'Copyright:       ',copyrightname);
    writeln(tmpt,'Version:         ',versionname);
    writeln(tmpt,'Comments:        ',commentname);
    writeln(tmpt,'');
    writeln(tmpt,'Builder: '+FBuilder);
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
    on E: Exception do
      Crash('Unable to open temporary file: '+E.ClassName+': '+E.Message);
  end;
  fileopened:=true;

  fillchar(pkgh,sizeof(pkgh),0);
  pkgh.pkgtag:='PKGv2.1>';
  pkgh.pkgname:=name;
  pkgh.headerlength:=filesize(tmpf);
  pkgh.memorylimit:=memorylimit;
  if headertextname<>'$' then
    pkgh.headerlength:=pkgh.headerlength+filesize(hdrf);

  fillchar(pkghc,sizeof(pkghc),0);
  pkghc.pkgtag:='g21';
  pkghc.headerlength:=filesize(tmpf);
  pkghc.memorylimit:=memorylimit;
  if headertextname<>'$' then
    pkghc.headerlength:=pkghc.headerlength+filesize(hdrf);

  if cryptheader='$' then
    blockwrite(pkgf,pkgh,sizeof(pkgh))
  else
    blockwrite(pkgf,pkghc,sizeof(pkghc));

  while not eof(tmpf) do
  begin
    blockread(tmpf,buf,sizeof(buf),reat);
    blockwrite(pkgf,buf,reat);
  end;
  closefile(tmpf);

  if headertextname<>'$' then
  begin
    while not eof(hdrf) do
    begin
      blockread(hdrf,buf,sizeof(buf),reat);
      blockwrite(pkgf,buf,reat);
    end;
    closefile(hdrf);
  end;

  Log('PKG header was written.');
  DeleteFile('header.tmp');
  totfsize:=123;
end;

function TPackageBuilder.PrepareHuffman(var inf:file):longint;
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

function TPackageBuilder.ProcessHuffman(var inf:file;var buf;buflen:word;LZ77:boolean):integer;
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
  if buflen<256*6+1 then Crash('Internal error#1.');
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
              else Crash('Internal error#2.');
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

procedure TPackageBuilder.IncludePath(path:string;mask:string;dirno:longint;recursive:boolean);
var
  res: integer;
  sr: TSearchRec;
  pkghf: pkgfile;
  AllUppercase: boolean;
  buf:array[1..2000] of byte;
  tmpf,hdrf:file;
  b:byte;
  pkghfarr:array[1..sizeof(pkgfile)*2] of byte;
  crc:cardinal;
  crch:byte;
  i:integer;
  reat:integer;
  testlen:integer;
  usedcoding:integer;
  logmsg:string;
  hufflen:integer;

begin
  if recursive then
    res := findfirst(path+mask,faAnyFile,sr)
  else
    res := findfirst(path+mask,faAnyFile-faDirectory,sr);
  if res <> 0 then exit;

  repeat
    if (sr.Attr and faVolumeID <> 0) or (sr.Name = '.') or (sr.Name = '..') then
      continue;

    if (sr.attr and faDirectory=0) then begin
      assignfile(hdrf,path+sr.name);
      try
        FileMode := fmOpenRead;
        Reset(hdrf,1);
      except
        Log('Unable to open include file ('+sr.name+').');
        FileMode := fmOpenWrite;
        continue;
      end;
      FileMode := fmOpenWrite;

      if pos('.',sr.name) > 0 then begin
        pkghf.FileName := ShortString(copy(sr.name,1,pos('.',sr.name)-1));
        pkghf.FileExt := ShortString(copy(sr.name,pos('.',sr.name)+1,length(sr.name)-pos('.',sr.name)));
      end else begin
        pkghf.FileName := ShortString(sr.name);
        pkghf.FileExt := '';
      end;

      if inclfname<>'' then pkghf.FileName:=inclfname;
      inclfname:='';
    end else begin
      pkghf.FileName:=ShortString(sr.name);
      pkghf.FileExt:='*DIR*';
    end;


    AllUppercase := true;
    for i:=1 to length(pkghf.Filename) do
      if pkghf.FileName[i] in ['a'..'z'] then AllUppercase := false;

    if AllUppercase and (length(pkghf.FileName)<=8) then begin
      pkghf.FileName:=ShortString(LowerCase(string(pkghf.FileName)));
      if pkghf.FileExt='*DIR*' then
        pkghf.FileName[1]:=Upcase(pkghf.FileName[1]);
    end;

    if pkghf.FileExt<>'*DIR*' then
      pkghf.FileExt:=ShortString(lowercase(string(pkghf.FileExt)));


    ObfuscateFilename(FileSysCode, totfsize, @pkghf.FileName[1], Length(pkghf.FileName));
    if pkghf.FileExt<>'*DIR*' then
      ObfuscateFilename(FileSysCode, totfsize, @pkghf.FileExt[1], Length(pkghf.FileExt));
    pkghf.pkgtag:='PKGF';
    b:=125;
    pkghf.cryptmode:=cryptmode;
    pkghf.crcmode:=crcmode;

    if sr.attr and faDirectory = 0 then begin
      pkghf.loadmode:=loadmode;
      pkghf.filelength:=filesize(hdrf);
    end else begin
      pkghf.loadmode:=3;
      pkghf.filelength:=0;
    end;

    usedcoding := 0;
    if (sr.attr and faDirectory <> 0) then
      pkghf.packedlength:=0
    else
      case packmode of
      0:begin
          pkghf.packedlength:=pkghf.filelength;
          usedcoding:=0;
        end;
      1:begin
          hufflen:=PrepareHuffman(hdrf);
          if hufflen>pkghf.filelength then begin
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
        rewrite(tmpf,1);
        reat:=2000;
        while reat=2000 do begin
          reat:=ProcessHuffman(hdrf,buf,2000,true);
          blockwrite(tmpf,buf,reat);
        end;
        seek(hdrf,0);
        if (filesize(tmpf)<hufflen) and (filesize(tmpf)<pkghf.filelength) then begin
          pkghf.packedlength:=filesize(tmpf);
          usedcoding:=2;
        end else
        if hufflen>pkghf.filelength then begin
          pkghf.packedlength:=pkghf.filelength;
          usedcoding:=0;
        end else begin
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
      end; //of case 2
      else
        Crash('Unknown pack mode.');
      end;

    pkghf.packmode := usedcoding;

    if (sr.attr and faDirectory=0) and (pkghf.filelength<>0) then
      Log('Including file '+sr.name+' ('+inttostr(round(pkghf.packedlength/pkghf.filelength*100))+'%)...');
    logmsg:='   ';
    case usedcoding of
      0:logmsg:=logmsg+'Compression:NONE ';
      1:logmsg:=logmsg+'Compression:HUFFMAN ';
      2:logmsg:=logmsg+'Compression:LZ77 ';
    end;
    case cryptmode of
      0:logmsg:=logmsg+'Crypting:NONE ';
      1:logmsg:=logmsg+'Crypting:COMPLEX ';
      2:logmsg:=logmsg+'Crypting:SIMPLE ';
    end;
    case crcmode of
      0:logmsg:=logmsg+'Safety:NONE ';
      1:logmsg:=logmsg+'Safety:CRC ';
    end;
    Log(logmsg);

    pkghf.directory:=dirno;
    pkghf.headercrc:=0;
    move(pkghf,pkghfarr,sizeof(pkghf));
    crch:=0;
    for i:=1 to sizeof(pkghf) do crch:=crch+pkghfarr[i];
    pkghf.headercrc:=crch;
    encseed:=totfsize+headercode;
    move(pkghf,pkghfarr,sizeof(pkghf));
    for i:=sizeof(pkghf) downto 1 do pkghfarr[i*2]:=pkghfarr[i];
    for i:=1 to sizeof(pkghfarr) do pkghfarr[i]:=pkghfarr[i] xor encmask(256);
    blockwrite(pkgf,b,1);
    blockwrite(pkgf,pkghfarr,sizeof(pkghfarr));
    totfsize:=totfsize+1+sizeof(pkghfarr);

    if (sr.attr and faDirectory=0) then begin
      if usedcoding=2 then begin
        assignfile(tmpf,'LZ77.TMP');
        reset(tmpf,1);
      end;
      testlen:=0;
      encseed:=cryptcode+totfsize;
      reat:=2000;
      while reat=2000 do begin
        case usedcoding of
          0:blockread(hdrf,buf,2000,reat);
          1:reat:=ProcessHuffman(hdrf,buf,2000,false);
          2:blockread(tmpf,buf,2000,reat);
          else Crash('Unknown pack mode.');
        end;
        if reat>0 then begin
          inc(testlen,reat);
          case cryptmode of
            0:begin end;
            1:begin
                encseed:=cryptcode+encmask(1000);
                for i:=1 to 2000 do buf[i]:=buf[i] xor encmask(256);
              end;
            2:for i:=1 to 2000 do buf[i]:=buf[i] xor ((cryptcode*i) mod 256);
            else Crash('Unknown crypt mode.');
          end;
          blockwrite(pkgf,buf,reat);
          case crcmode of
            0:begin end;
            1:if filesize(hdrf)>0 then begin
                crc := CalcCRC(@buf[1], reat);
                blockwrite(pkgf,crc,sizeof(crc));
                totfsize:=totfsize+sizeof(crc);
              end;
            else Crash('Unknown CRC mode.');
          end;
        end;
      end; //of while

      totfsize:=totfsize+pkghf.packedlength;
//        for i:=256 to 511 do if Huff[i].count>0 then pkgerr('Count err-'+inttostr(i)+'-'+inttostr(Huff[i].count),curcmd);
      if testlen<>pkghf.packedlength then
        Crash('Internal error#3.'+inttostr(testlen)+'/'+inttostr(pkghf.packedlength));
      closefile(hdrf);
      Log('Included file '+sr.name+'.');

      if usedcoding=2 then begin
        closefile(tmpf);
        erase(tmpf);
      end;

    end else begin
      Log('Directory: '+sr.name+':');
      inc(LastDirectory);
      IncludePath(path+sr.name+'\',inclrmask,LastDirectory,true);
    end;

  until findnext(sr)<>0;
  findclose(sr);
end;

procedure TPackageBuilder.Include(dir: string);
begin
  if dir='' then exit;
  if dir[length(dir)]<>'\' then dir:=dir+'\';
  IncludePath(dir,inclmask,0,false);
end;

procedure TPackageBuilder.IncludeTree(dir: string);
begin
  if dir='' then exit;
  if dir[length(dir)]<>'\' then dir:=dir+'\';
  IncludePath(dir,inclmask,0,true);
end;

procedure TPackageBuilder.Finish;
var pkgft:array[0..5] of AnsiChar;
  b:byte;
  rnx:word;
  i:integer;
  w:word;
begin
  if not fileopened then Crash('File is not open.');
  b:=233;
  pkgft:='PKGEOF';
  blockwrite(pkgf,b,1);
  if cryptheader='$' then
    blockwrite(pkgf,pkgft,sizeof(pkgft)) else
  begin
    blockwrite(pkgf,pkghs,sizeof(pkghs));
    //fix rnx:=encmask(16384);
    rnx:=0;
    for i:=1 to rnx do
    begin
      w:=encmask(65536);
      if w=65279 then w:=0;
      blockwrite(pkgf,w,2);
    end;
  end;
  closefile(pkgf);
  fileopened := false;
  Log('PKG file was written.');
end;

end.
