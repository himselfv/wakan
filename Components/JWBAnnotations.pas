unit JWBAnnotations;

interface
uses SysUtils, Classes, Forms, Windows, TextTable, JWBStrings;

type
  TAnnotationCursor = class(TTextTableCursor)
  protected
    curAnnot:string;
    curAnnotPos:integer;
  public
    procedure Seek(const des:array of string);
    procedure SeekK(const kanji,kana:string);
    procedure First;
    function Get(cmd:char):string;
    function GetOne(cmd:char):string;
    function GetAll(cmd:char; const delimit:string):string;
  end;

var
  TAnnots: TTextTable;
  Annot: TAnnotationCursor; //default cursor, try to avoid using it! create independent ones
  //Fields
  fldAnnotsTag: integer;
  fldAnnotsData: integer;

procedure RebuildAnnotations;
procedure LoadAnnotations;
function HaveAnnotations:boolean; {$IFDEF INLINE}inline;{$ENDIF}
procedure AnnotShowMedia(kanji,kana:string);

implementation
uses MemSource, JWBMedia, PKGWrite, StdPrompt, JWBUnit, JWBMenu, JWBIO;

procedure WriteAnnotPackage(const tempDir: string; pkg: string);
var pack: TPackageBuilder;
begin
  pack := TPackageBuilder.Create;
  try
    pack.PackageFile := pkg;
    pack.MemoryLimit := 100000000;
    pack.Name := 'WaKan Compiled Annotations';
    pack.TitleName := 'WaKan Compiled Annotations';
    pack.CopyrightName := WakanCopyright;
    pack.FormatName := 'Pure Package File';
    pack.CommentName := 'File is used by '+WakanAppName;
    pack.VersionName := '1.0';
    pack.HeaderCode := 621030;
    pack.FilesysCode := 587135;
    pack.WriteHeader;
    pack.LoadMode := lmTemporaryLoad;
    pack.CryptMode := 0;
    pack.CrcMode := 0;
    pack.PackMode := 0;
    pack.CryptCode := 453267;
    pack.Include(tempDir);
    pack.Finish;
  finally
    FreeAndNil(pack);
  end;
end;

{ Rebuilds the annotation package if it's missing, or if any of the *.ANO files
have been changed. }
//TODO: Convert to Unicode
procedure RebuildAnnotations;
const
  UH_COMMENT:FChar = {$IFDEF UNICODE}'#'{$ELSE}'0023'{$ENDIF};
  UH_SEP_SEMICOLON:FChar = {$IFDEF UNICODE}';'{$ELSE}'003B'{$ENDIF};
  UH_SEP_COLON:FChar = {$IFDEF UNICODE}':'{$ELSE}'003A'{$ENDIF};
var
  tempDir,tempPkgDir: string;
  t:textfile;
  chk,nchk:TStringList;
  ch:FChar;
  ss: string;
  sr:TSearchRec;
  bld:boolean; //true if need to rebuild
  pd:TSMPromptForm;
  ps:TPackageSource;
  tt:TTextTable;
  AEncoding: CEncoding;
  conv: TStreamDecoder;
  moded:boolean;
  curt,curd:FString;
  dd:string;
begin
  chk:=TStringList.Create;
  nchk:=TStringList.Create;
  bld:=false;
  if FileExists('ANNOTATE.CHK') then chk.LoadFromFile('ANNOTATE.CHK');
  if FindFirst('*.ANO',faAnyFile,sr)=0 then
  repeat
    ss:=sr.name+';'+inttostr(sr.size)+';'+inttostr(sr.time);
    if chk.IndexOf(ss)=-1 then bld:=true;
    nchk.Add(ss);
  until FindNext(sr)<>0;
  SysUtils.FindClose(sr);
  nchk.SaveToFile('ANNOTATE.CHK');
  chk.Free;
  nchk.Free;

  if FileExists('ANNOTATE.PKG') and not bld then exit;

  ps:=nil;
  tt:=nil;
  pd:=SMMessageDlg(
    _l('^eAnnotations'),
    _l('^eRebuilding annotations...'));
  try
    tempDir:=CreateRandomTempDirName(); //for package files
    tempPkgDir:=CreateRandomTempDirName(); //another one for compiled package.
        //We don't want to put it into Wakan folder until done.

   //Create empty package first
    ForceDirectories(tempDir);
    assignfile(t,tempDir+'\Annot.info');
    rewrite(t);
    writeln(t,'$TEXTTABLE');
    writeln(t,'$PREBUFFER');
    writeln(t,'$FIELDS');
    writeln(t,'xTag');
    writeln(t,'bUser');
    writeln(t,'sData');
    writeln(t,'$ORDERS');
    writeln(t,'Tag_Ind');
    writeln(t,'$SEEKS');
    writeln(t,'0');
    writeln(t,'Tag');
    writeln(t,'$CREATE');
    closefile(t);
    ForceDirectories(tempPkgDir);
    WriteAnnotPackage(tempDir,tempPkgDir+'\annotate.pkg');
    DeleteDirectory(tempDir);

    //Populate with data
    ps:=TPackageSource.Create(tempPkgDir+'\annotate.pkg',621030,587135,453267);
    tt:=TTextTable.Create(ps,'annot',false,false);
    tt.Nocommitting:=true;
    if FindFirst('*.ANO',faAnyFile,sr)=0 then
    repeat
      AEncoding:=Conv_DetectType(sr.name);
      if AEncoding=nil then AEncoding:=TAsciiEncoding;
      conv := TStreamDecoder.Open(sr.name, AEncoding.Create);
      if Uppercase(sr.name)='_USER.ANO' then dd:='1' else dd:='0';
      moded:=false;
      curd:=''; curt:='';
      while conv.ReadChar(ch) do
      begin
        if ch=UH_LF then
        begin
          if (curd<>'') and (curt<>'') and (fgetch(curt,1)<>UH_COMMENT) then
            tt.Insert([curt,dd,fstrtohex(curd)]);
          curd:='';
          curt:='';
          moded:=false;
        end else
          if (not moded) and ((ch=UH_SEP_SEMICOLON) or (ch=UH_SEP_COLON)) then
            moded:=true
          else
          if ch<>UH_CR then
            if moded then curd:=curd+ch else curt:=curt+ch;
        conv.ReadChar(ch);
      end;
      FreeAndNil(conv);
    until FindNext(sr)<>0;
    tt.Nocommitting:=false;
    tt.ReIndex;

    //Write down
    ForceDirectories(tempDir);
    tt.WriteTable(tempDir+'\Annot',true);

    //Release
    FreeAndNil(tt);
    FreeAndNil(ps); //so that they don't hold the package open

    //And build into final form
    WriteAnnotPackage(tempDir,'annotate.pkg');
    DeleteDirectory(tempDir);
    DeleteDirectory(tempPkgDir);

  finally
    pd.Free;
    FreeAndNil(tt);
    FreeAndNil(ps);
  end;
end;

procedure LoadAnnotations;
var ps:TPackageSource;
begin
  try
    if not fileexists('annotate.pkg') then RebuildAnnotations;
    if not fileexists('annotate.pkg') then exit;
    ps:=TPackageSource.Create('annotate.pkg',621030,587135,453267);
    TAnnots:=TTextTable.Create(ps,'annot',false,false);
    fldAnnotsTag:=TAnnots.Field('Tag');
    fldAnnotsData:=TAnnots.Field('Data');
    Annot := TAnnotationCursor.Create(TAnnots);
    ps.Free;
  except
    Application.MessageBox(
      pchar(_l('^eAnnotations file ANNOTATE.PKG is corrupt and wasn''t loaded.'#13
        +'If you delete it, it will be recreated.')),
      pchar(_l('^eError')),
      MB_ICONERROR or MB_OK);
  end;
end;

function HaveAnnotations: boolean;
begin
  result:=TAnnots<>nil;
end;

procedure AnnotShowMedia(kanji, kana: string);
var s:string;
    b:boolean;
begin
  if not HaveAnnotations then exit;
  Annot.SeekK(kanji,kana);
  fMedia.media.Clear;
  fMedia.TabSet1.Tabs.Clear;
 // images
  Annot.First;
  s:=Annot.Get('I');
  while s<>'' do
  begin
    fMedia.media.Add(s);
    fMedia.TabSet1.Tabs.Add(inttostr(fMedia.TabSet1.Tabs.Count+1)+': '+copy(s,1,pos('.',s)-1));
    s:=Annot.Get('I');
  end;
  // pages
  Annot.First;
  s:=Annot.Get('W');
  while s<>'' do
  begin
    fMedia.media.Add('http://'+s);
    fMedia.TabSet1.Tabs.Add(inttostr(fMedia.TabSet1.Tabs.Count+1)+': '+s);
    s:=Annot.Get('W');
  end;
  if fMedia.media.Count>0 then
  begin
    fMedia.Show;
    fMedia.TabSet1.Visible:=fMedia.media.Count>1;
    fMedia.TabSet1.TabIndex:=0;
    fMedia.TabSet1Change(fMenu,0,b);
    fMenu.SetFocus;
  end else fMedia.Hide;
end;

procedure TAnnotationCursor.Seek(const des: array of string);
var i:integer;
begin
  if Self=nil then exit; //yep we can be called like that. Code legacy!
  curAnnot:='';
  for i:=0 to High(des) do
  begin
    SetOrder('Tag_Ind');
    Locate('Tag',des[i]);
    while (not EOF) and (Str(fldAnnotsTag)=des[i]) do
    begin
      if curAnnot<>'' then curAnnot:=curAnnot+',';
      curAnnot:=curAnnot+Str(fldAnnotsData);
      Next;
    end;
  end;
  First;
end;

procedure TAnnotationCursor.First;
begin
  curAnnotPos:=1;
end;

function TAnnotationCursor.Get(cmd: char): string;
begin
  result:='';
  if curAnnotPos+1>length(curAnnot) then exit;
  while (curAnnotPos<length(curAnnot)) and
    ((upcase(curAnnot[curAnnotPos])<>upcase(cmd)) or (curAnnot[curAnnotPos+1]<>':')) do
    begin
      while (curAnnotPos<length(curAnnot)) and (curAnnot[curAnnotPos]<>',') do inc(curAnnotPos);
      inc(curAnnotPos);
    end;
  inc(curAnnotPos,2);
  if curAnnotPos>length(curAnnot) then exit;
  while (curAnnotPos<=length(curAnnot)) and (curAnnot[curAnnotPos]<>',') do
  begin
    result:=result+curAnnot[curAnnotPos];
    inc(curAnnotPos);
  end;
  inc(curAnnotPos);
end;

function TAnnotationCursor.GetAll(cmd: char; const delimit: string): string;
var s:string;
begin
  First;
  result:='';
  s:=Get(cmd);
  while s<>'' do
  begin
    if result<>'' then result:=result+delimit;
    result:=result+s;
    s:=Get(cmd);
  end;
end;

function TAnnotationCursor.GetOne(cmd: char): string;
begin
  First;
  result:=Get(cmd);
end;

//TODO: Unicode!
procedure TAnnotationCursor.SeekK(const kanji, kana: string);
begin
  if (curlang<>'j') or (kana='') then
  begin
    if length(kanji)>4 then Seek([kanji]) else Seek([kanji,UnicodeToHex(UH_UNKNOWN_KANJI+kanji)]);
  end else
    Seek([kanji,kana,UnicodeToHex(KanaToRomaji(kana,1,'j')),kanji+UnicodeToHex('+')+kana]);
end;


initialization
  TAnnots:=nil;
  Annot:=nil;

end.
