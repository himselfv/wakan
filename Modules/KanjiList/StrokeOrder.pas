unit StrokeOrder;
{ Wakan stroke order package. Built from strokes.csv and stored in a PKG-type package.
 Must be loaded with LoadStrokeOrder }

interface
uses Classes, Graphics;

var
  sodir: TStringList = nil;
  sobin: pointer = nil;

procedure LoadStrokeOrder(const AFilename: string);
procedure FreeStrokeOrder;
procedure DrawStrokeOrder(canvas:TCanvas;x,y,w,h:integer;char:string;fontsize:integer;color:TColor);

//Use to rebuild stroke order package from strokes.csv
procedure BuildStrokeOrderPackage(const sourceCsv, targetSod: string);

implementation
uses SysUtils, Dialogs, Windows, MemSource, JWBStrings, JWBCore, JWBUnit, PKGWrite;


procedure LoadStrokeOrder(const AFilename: string);
var ps: TPackageSource;
  ms: TMemoryStream;
begin
  ps:=TPackageSource.Create(AFilename,791564,978132,978123);
  ms:=ps['strokes.bin'].Lock;
  GetMem(sobin,ms.Size);
  ms.Read(sobin^,ms.Size);
  ps['strokes.bin'].Unlock;
  ms:=ps['dir.txt'].Lock;
  sodir:=TStringList.Create;
  sodir.LoadFromStream(ms);
  ps['dir.txt'].Unlock;
  ps.Free;
end;

procedure FreeStrokeOrder;
begin
  FreeAndNil(sodir);
  if sobin<>nil then begin
    FreeMem(sobin);
    sobin := nil;
  end;
end;

procedure DrawStrokeOrder(canvas:TCanvas;x,y,w,h:integer;char:string;fontsize:integer;color:TColor);
var i,l,r,m:integer;
  xx,yy:byte;
  p:PByte;
  fc:FChar;
begin
  if sobin=nil then exit;
  l:=0;
  m:=0;//not really used but shut up delphi
  r:=sodir.Count-1;
  while l<=r do
  begin
    m:=l+(r-l) div 2;
   //sodir contains hex chars
    fc := fgetch(hextofstr(copy(sodir[m],1,4)), 1);
    if fc<char then
      l:=m+1
    else
    if fc>char then
      r:=m-1
    else
      break;
  end;
  if l>r then exit;
  i:=strtoint('0x'+copy(sodir[m],5,4));
  p:=sobin;
  p:=p+i*2;
  xx:=255;
  yy:=255;
  i:=0;
  SetBkMode(canvas.Handle,TRANSPARENT);
  canvas.Font.Color:=color;
  canvas.Font.Style:=[fsBold];
  while (xx<>0) or (yy<>0) do
  begin
    xx:=byte(p^);
    p:=p+1;
    yy:=byte(p^);
    p:=p+1;
    inc(i);
    if (xx<>0) and (yy<>0) then
    begin
      canvas.Font.Color:=clWindow;
      DrawUnicode(canvas,round(x+w*(xx/256))+1,round(y+h*(yy/256)),fontsize,fstr(inttostr(i)),FontEnglish);
      DrawUnicode(canvas,round(x+w*(xx/256))-1,round(y+h*(yy/256)),fontsize,fstr(inttostr(i)),FontEnglish);
      DrawUnicode(canvas,round(x+w*(xx/256)),round(y+h*(yy/256))+1,fontsize,fstr(inttostr(i)),FontEnglish);
      DrawUnicode(canvas,round(x+w*(xx/256)),round(y+h*(yy/256))-1,fontsize,fstr(inttostr(i)),FontEnglish);
      canvas.Font.Color:=color;
      DrawUnicode(canvas,round(x+w*(xx/256)),round(y+h*(yy/256)),fontsize,fstr(inttostr(i)),FontEnglish);
    end;
  end;
  canvas.Font.Color:=clWindowText;
  canvas.Font.Style:=[];
end;



{ Rebuilds wakan.sod from strokes.csv. Mostly used by a devs, but users can do this too. }
procedure BuildStrokeOrderPackage(const sourceCsv, targetSod: string);
var
  tempDir: string;
  pack: TPackageBuilder;
  sl,sl2:TStringList;
  t:textfile;
  f:file of byte;
  s,s2,s3,s4,uni:string;
  i,j,k:integer;
  b:byte;
begin
  sl:=TStringList.Create;
  assignfile(t,sourceCsv);
  System.reset(t);
  while not eof(t) do
  begin
    readln(t,s);
    s2:='';
    uni:=format('%4.4X',[strtoint(copy(s,4,5))]);
    while (pos('";"',s)>0) do delete(s,1,pos('";"',s)+2);
    delete(s,length(s),1);
    while s<>'' do
    begin
      s3:=copy(s,2,pos(']',s)-2);
      delete(s,1,pos(']',s));
      s4:=copy(s3,1,pos(',',s3)-1);
      delete(s3,1,pos(',',s3));
      if (strtoint(s3)>255) or (strtoint(s4)>255) then showmessage('error');
      s2:=s2+format('%2.2X%2.2X',[strtoint(s4),strtoint(s3)]);
    end;
    sl.Add(uni+s2);
  end;
  closefile(t);
  sl2:=TStringList.Create;
  sl.Sort;

  tempDir := CreateRandomTempDirName();
  ForceDirectories(tempDir);
  assignfile(f,tempDir+'\strokes.bin');
  rewrite(f);
  k:=0;
  for i:=0 to sl.Count-1 do
  begin
    sl2.Add(copy(sl[i],1,4)+Format('%4.4X',[k]));
    s:=sl[i];
    k:=k+(length(s) div 4);
    for j:=2 to (length(s) div 2)-1 do
    begin
      b:=strtoint('0x'+copy(s,j*2+1,2));
      write(f,b);
    end;
    b:=0;
    write(f,b); write(f,b);
  end;
  closefile(f);
  sl2.SaveToFile(tempDir+'\dir.txt');
  sl.Free;
  sl2.Free;

  pack := TPackageBuilder.Create;
  try
    pack.PackageFile := targetSod;
    pack.MemoryLimit := 100000000;
    pack.Name := 'Stroke order';
    pack.TitleName := 'Japanese stroke order charts';
    pack.CompanyName := 'LABYRINTH';
    pack.CopyrightName := '(C) Jim Breen, Yasuhito Tanaka';
    pack.FormatName := 'Pure Package File';
    pack.CommentName := 'File is used by '+WakanAppName;
    pack.VersionName := '1.0';
    pack.HeaderCode := 791564;
    pack.FilesysCode := 978132;
    pack.WriteHeader;
    pack.LoadMode := lmTemporaryLoad;
    pack.CryptMode := 0;
    pack.CrcMode := 0;
    pack.PackMode := 0;
    pack.CryptCode := 978123;
    pack.Include(tempDir);
    pack.Finish;
  finally
    FreeAndNil(pack);
  end;

  DeleteDirectory(tempDir);
end;

end.
