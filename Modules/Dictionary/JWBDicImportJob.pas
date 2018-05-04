unit JWBDicImportJob;
{ Core dictionary import routines. }

interface
uses SysUtils, Classes, JWBIO, JWBJobs, JWBDic, JWBIndex, EdictReader;

type
  EDictImportException = class(Exception);

  TDicInfo = record
    Description: string;
  end;
  PDicInfo = ^TDicInfo;

  TEdictRoma = array[0..MaxKana-1] of string;

  TDicImportJob = class(TJob)
  protected //Input params
    FDicFilename: string;
    FDicDescription: string;
    FDicLanguage: char;
    FFiles: array of record
      Filename: string;
      Encoding: CEncoding; //can be nil for autodetect
    end;
    procedure Initialize;
    procedure Cleanup;
    function GetTotalLineCount: integer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure ProcessChunk; override;
    procedure AddSourceFile(const AFilename: string; AEncoding: CEncoding=nil);
    property DicFilename: string read FDicFilename write FDicFilename;
    property DicDescription: string read FDicDescription write FDicDescription;
    property DicLanguage: char read FDicLanguage write FDicLanguage;

  protected
    dic: TJaletDic;
    wordidx: TWordIndexBuilder;
    charidx: TIndexBuilder;
    freql: TStringList; //frequency list, if used
    roma_prob: TStreamEncoder; //error log
    tempPackageDir: string; //a temporary dir where initial package file is stored
    procedure CreateDictTables(dicFilename: string; info: TDicInfo; diclang:char; entries: integer);
    procedure WriteDictPackage(dicFilename: string; tempDir: string; info: TDicInfo;
      diclang:char; entries:integer);
    procedure WriteSources(const fname: string);

  protected
    procedure UpdateProgress; inline;
    procedure AddArticle(const ed: PEdictArticle; const roma: TEdictRoma);
    function ImportCCEdict(fuin: TStreamDecoder): integer;
    function ImportEdict(fuin: TStreamDecoder): integer;
  public
    LineCount: integer;
    LineNo: integer;
    LastArticle: integer;
    ProblemRecords: integer; //# of records which weren't imported

  end;

//Word frequency data
function SupportsFrequencyList: boolean;
function GetFrequencyList: TStringList;

function GetLastWriteTime(const filename: string; out dt: TDatetime): boolean;

implementation
uses Windows, PKGWrite, JWBStrings, KanaConv, AppData, JWBCore, JWBLanguage, JWBUnit,
  JWBEdictMarkers;

const
  eCannotImportDict = '#01194^Cannot import dictionary. It''s probably in the '
    +'different encoding, unsupported format or damaged.';

{
Returns true if importer thinks it could load WORDFREQ_CK and add frequency info,
if asked to.
}
function SupportsFrequencyList: boolean;
begin
  Result:= FileExists(ProgramDataDir+'\wordfreq_ck.uni')
    or FileExists(ProgramDataDir+'\wordfreq_ck.euc')
    or FileExists(ProgramDataDir+'\wordfreq_ck');
end;

{
Creates and populates TStringList with frequency information taken from wordfreq_ck.
Caches it and returns. Do not destroy it.
}
var
  _CachedFrequencyList: TStringList;

function GetFrequencyList: TStringList;
const
 //Format markers
  UH_FREQ_COMMENT = {$IFDEF UNICODE}'#'{$ELSE}'0023'{$ENDIF};
  UH_TAB = {$IFDEF UNICODE}#$0009{$ELSE}'0009'{$ENDIF};
var fc: FChar;
  newline,nownum,comment:boolean;
  addkan,addnum:string;
  conv: TStreamDecoder;
  enctype: CEncoding;
begin
  if _CachedFrequencyList<>nil then begin
    Result := _CachedFrequencyList;
    exit;
  end;

  Result:=TStringList.Create;
  if FileExists(ProgramDataDir+'\wordfreq_ck.uni') then
    conv := OpenTextFile(ProgramDataDir+'\wordfreq_ck.uni', TUTF16LEEncoding)
  else
  if FileExists(ProgramDataDir+'\wordfreq_ck.euc') then
    conv := OpenTextFile(ProgramDataDir+'\wordfreq_ck.euc', TEUCEncoding)
  else
  if FileExists(ProgramDataDir+'\wordfreq_ck') then begin
   //best guess
    enctype := Conv_DetectType(ProgramDataDir+'\wordfreq_ck');
    conv := OpenTextFile(ProgramDataDir+'\wordfreq_ck', enctype);
  end else
    raise EDictImportException.Create(_l('#00915^eCannot find WORDFREQ_CK file.'));

  try
    newline:=true;
    nownum:=false;
    addkan:='';
    addnum:='';
    comment:=false;
    while conv.ReadChar(fc) do
    begin
      if (newline) and (fc=UH_FREQ_COMMENT) then comment:=true;
      newline:=false;
      if fc=UH_LF then
      begin
        if not comment and (addkan<>'') and (addnum<>'') then
        begin
          Result.AddObject(addkan,TObject(strtoint(addnum)));
        end;
        addkan:='';
        addnum:='';
        newline:=true;
        comment:=false;
        nownum:=false;
      end else
      if not comment and (fc=UH_TAB) then
        nownum:=true
      else
      if not comment then if nownum then
      begin
        if IsLatinDigitF(fc) then addnum:=addnum+fstrtouni(fc);
      end else
        addkan:=addkan+fc;
    end;
  finally
    FreeAndNil(conv);
  end;

  Result.Sorted:=true;
  Result.Sort;
  if InterlockedCompareExchangePointer(pointer(_CachedFrequencyList), nil, Result)<>nil then begin
    FreeAndNil(Result);
    Result := _CachedFrequencyList;
  end;
end;

constructor TDicImportJob.Create;
begin
  inherited;
end;

destructor TDicImportJob.Destroy;
begin
  Cleanup;
  inherited;
end;

procedure TDicImportJob.AddSourceFile(const AFilename: string; AEncoding: CEncoding=nil);
var i: integer;
begin
  i := Length(FFiles);
  SetLength(FFiles, i+1);
  FFiles[i].Filename := AFilename;
  FFiles[i].Encoding := AEncoding;
end;

procedure TDicImportJob.Initialize;
begin
  wordidx := nil;
  charidx := nil;
  freql := nil;
  ProblemRecords := 0;
  dic := nil;
  LastArticle := 0;
end;

procedure TDicImportJob.Cleanup;
begin
  FreeAndNil(dic);
  FreeAndNil(wordidx);
  FreeAndNil(charidx);
  FreeAndNil(roma_prob); //or else it'll remain open until the form is closed
  freql := nil;
  if tempPackageDir<>'' then begin
    DeleteDirectory(tempPackageDir);
    tempPackageDir := '';
  end;
end;

procedure TDicImportJob.UpdateProgress;
begin
  if (linecount>0) and (lineno mod 100=0) then
    SetProgress(round(lineno/linecount*100));
end;

procedure TDicImportJob.ProcessChunk;
var fuin: TStreamDecoder;
  fi: integer;
  info: TDicInfo;
  tempDir: string;
begin
  FState := jsWorking;
  StartOperation(_l('#01165>Importing'), 0); //indeterminate state

 //Create frequency list
  try
    SetOperation(_l('#00917^eCreating frequency chart...'));
    freql := GetFrequencyList;
  except
    on E: EDictImportException do begin
      E.Message := _l('#01195^Frequency list creation failed: %s', [E.Message]);
      raise;
    end;
  end;

 //Create indexes
  charidx := TIndexBuilder.Create;
  wordidx := TWordIndexBuilder.Create;

  LineCount := GetTotalLineCount;

 //Create temporary package dir
 //Since dic holds the file open, we can't delete it until later
  tempPackageDir := CreateRandomTempDirName();
  ForceDirectories(tempPackageDir);

 //Create empty dictionary tables
  info.Description := Self.DicDescription;
  CreateDictTables(tempPackageDir+'\DICT.TMP', info, Self.DicLanguage, 0);
  dic := TJaletDic.Create(tempPackageDir+'\DICT.TMP');
  dic.Load;
  dic.Demand;
  dic.TTDict.NoCommitting := true;
  dic.TTEntries.NoCommitting := true;

 //Create roma_problems (can be delayed until needed)
  if roma_prob=nil then begin
    roma_prob := CreateTextFile(UserDataDir+'\roma_problems.txt', TUTF16LEEncoding);
    roma_prob.WriteChar(#$FEFF); //BOM
  end;

  SetMaxProgress(100);
  LineNo:=0;

  for fi:=0 to Length(FFiles)-1 do begin
    SetOperation(_l('#00086^eReading && parsing ')+ExtractFilename(FFiles[fi].Filename)+'...');

    if FFiles[fi].Encoding=nil then begin
     { Try to detect encoding, but don't be too smart about it.
      This is not the place to ask the user, if anyone wants to implement it,
      do it before passing the file to us.
      Also, with auto import we are able to specify the exact encoding in config,
      and with manual import just convert the dictionary manually. }
      FFiles[fi].Encoding := Conv_DetectType(FFiles[fi].Filename);
      if FFiles[fi].Encoding=nil then
        FFiles[fi].Encoding := TUTF8Encoding;
    end;

   //We don't run checks to verify if the encoding is correct because there's no
   //common header mark for all EDICT files out there.
   //Instead EDICT parser must fail on some obviously wrong lines.

    fuin := OpenTextFile(FFiles[fi].Filename, FFiles[fi].Encoding);
    try
      if Self.FDicLanguage='c' then
        ImportCCEdict(fuin)
      else
        ImportEdict(fuin);
    finally
      FreeAndNil(fuin);
    end;
  end; //for every file

  roma_prob.Flush; //just in case someone goes looking at it straight away

  SetOperation(_l('^Rebuilding index...'));
  dic.TTDict.NoCommitting := false;
  dic.TTDict.Reindex;
  dic.TTEntries.NoCommitting := true;
  dic.TTEntries.Reindex;

 //This time it's for our package
  tempDir := CreateRandomTempDirName();
  ForceDirectories(tempDir);
  try
    if charidx<>nil then begin
      SetOperation(_l('^Writing character index...'));
      charidx.Write(tempDir+'\CharIdx.bin');
    end;

    if wordidx<>nil then begin
      SetOperation(_l('^Writing word index...'));
      wordidx.Write(tempDir+'\WordIdx.bin');
    end;

    SetOperation(_l('^Writing dictionary table...'));
    dic.TTDict.WriteTable(tempDir+'\Dict',true);
    dic.TTEntries.WriteTable(tempDir+'\Entries',true);
    FreeAndNil(dic);

    WriteSources(tempDir+'\sources.lst');
    WriteDictPackage(dicFilename, tempDir, info, Self.DicLanguage, LastArticle);
  finally
    if tempDir<>'' then begin
      DeleteDirectory(tempDir);
      tempDir := '';
    end;
  end;

  Cleanup;
  FState := jsFinished;
end;

function TDicImportJob.GetTotalLineCount: integer;
var fuin: TStreamDecoder;
  fi: integer;
begin
  Result := 0;
  for fi:=0 to Length(FFiles)-1 do begin
   //Count number of lines in the converted file and add to total
    fuin := OpenTextFile(FFiles[fi].Filename, FFiles[fi].Encoding);
    Result := Result + GetLineCount(fuin);
    FreeAndNil(fuin);
  end;
end;


procedure TDicImportJob.CreateDictTables(dicFilename: string; info: TDicInfo;
  diclang:char; entries: integer);
var tempDir: string;
  t:textfile;
begin
  tempDir := CreateRandomTempDirName();
  ForceDirectories(tempDir);
  try
    assignfile(t,tempDir+'\Dict.info');
    rewrite(t);
    writeln(t,'$TEXTTABLE');
    writeln(t,'$PREBUFFER');
    writeln(t,'$RAWINDEX');
    writeln(t,'$WORDFSIZE');
    writeln(t,'$FIELDS');
    writeln(t,'xPhonetic');
    writeln(t,'xKanji');
    writeln(t,'sSort');
    writeln(t,'sMarkers');
    writeln(t,'iFrequency');
    writeln(t,'iArticle');
    writeln(t,'$ORDERS');
    writeln(t,'Phonetic_Ind');
    writeln(t,'Kanji_Ind');
    writeln(t,'<Phonetic_Ind');
    writeln(t,'<Kanji_Ind');
    writeln(t,'Article');
    writeln(t,'$SEEKS');
    writeln(t,'0');
    writeln(t,'Sort');
    writeln(t,'Kanji');
    writeln(t,'<Sort');
    writeln(t,'<Kanji');
    writeln(t,'Article');
    writeln(t,'$CREATE');
    closefile(t);

    assignfile(t,tempDir+'\Entries.info');
    rewrite(t);
    writeln(t,'$TEXTTABLE');
    writeln(t,'$PREBUFFER');
    writeln(t,'$RAWINDEX');
    writeln(t,'$WORDFSIZE');
    writeln(t,'$FIELDS');
    writeln(t,'iIndex');
    writeln(t,'xEntry');
    writeln(t,'sMarkers');
    writeln(t,'$ORDERS');
    writeln(t,'$SEEKS');
    writeln(t,'Index');
    writeln(t,'$CREATE');
    closefile(t);

    WriteDictPackage(dicFilename, tempDir, info, diclang, entries);
  finally
    DeleteDirectory(tempDir);
  end;
end;

procedure TDicImportJob.WriteDictPackage(dicFilename: string; tempDir: string;
  info: TDicInfo; diclang:char; entries:integer);
var f:textfile;
  path:string;
  pack: TPackageBuilder;
  dicName: string;
begin
  path := ExtractFilePath(dicFilename);
  if path<>'' then ForceDirectories(path);

  dicName := ChangeFileExt(dicFilename,''); //Explicit name -- for compability

  assignfile(f,tempDir+'\dict.ver');
  rewrite(f);
  writeln(f,'DICT');
  writeln(f,'5');
  writeln(f,inttostr(Trunc(now)));
  writeln(f,''); //Dictionary version -- deprecated
  writeln(f,dicName);
  writeln(f,diclang);
  writeln(f,EncodeInfoField(info.Description));
  writeln(f,'0'); //Priority -- deprecated
  writeln(f,IntToStr(entries));
  writeln(f,''); //Copyright -- deprecated
  closefile(f);

  pack := TPackageBuilder.Create;
  try
    pack.PackageFile := dicFilename;
    pack.MemoryLimit := 100000000;
    pack.Name := AnsiString(dicName);
    pack.TitleName := AnsiString(dicName)+' Dictionary';
    pack.CopyrightName := ''; //Copyright -- deprecated
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

end;



const
  UH_EDICT_SEMICOL = {$IFDEF UNICODE}';'{$ELSE}'003B'{$ENDIF};
  UH_EDICT_COMMA = {$IFDEF UNICODE}','{$ELSE}'002C'{$ENDIF};
  UH_EDICT_ALTERN = {$IFDEF UNICODE}'/'{$ELSE}'002F'{$ENDIF};

{
Adds an article to the dictionary we're building.
Also adds it to all indexes.
}
procedure TDicImportJob.AddArticle(const ed: PEdictArticle; const roma: TEdictRoma);
var freqi: integer;
  prior: array[0..MaxKanji-1] of integer;
  prior_this: integer;
  i, j, k: integer;
  s_art: string;
  kanji_local_idx: integer;
  kanji_matches_found: integer;
  add_mark: TMarkers;
  rec: integer;

 //Adds given dict record to a character index for a given character
  procedure IndexChars(rec:integer;const kanji:UnicodeString);
  var u_str: UnicodeString;
    uc: WideChar;
  begin
    u_str:=kanji;
    while u_str<>'' do
    begin
      uc:=u_str[1];
      delete(u_str,1,1);
      if EvalChar(uc)=EC_IDG_CHAR then
        charidx.AddToIndex(word(uc), rec);
    end;
  end;

 //Adds given entry record to a word index for a given string of words
  procedure IndexWords(rec:integer;const sense:UnicodeString);
  var u_str, u_part, u_word: UnicodeString;
  begin
    u_str:=sense;
    while u_str<>'' do
    begin
     //Pop next translation alternative
      u_part:=ULowerCase(ustrqpop(u_str,WideChar('/')));
      if u_part='' then continue;
      while u_part<>'' do
      begin
       //Remove all the leading (flags) (and) (markers)
        while (length(u_part)>0) and (u_part[1]='(') do
        begin
          ustrqpop(u_part,WideChar(')'));
          u_part := UTrimLeft(u_part);
        end;
       //Pop next word until space
        u_word:=ustrqpop(u_part,WideChar(' '));
        u_part:=repl(u_part,';',',');
        if (ignorel.IndexOf(u_word)=-1) and (u_word<>'')
        and (fgetch(u_word, 1)>{$IFDEF UNICODE}#$0040{$ELSE}'0040'{$ENDIF}) //it's all punctuation and digits before
        then
          wordidx.AddToIndex(u_word, rec);
      end;
    end;
  end;

begin
  Inc(LastArticle);
  s_art := IntToStr(LastArticle);

  //Base priority is either 0 or expression frequency (higher = better).
  for i := 0 to ed.kanji_used - 1 do
    prior[i] := 0;
  if freql<>nil then
    for i := 0 to ed.kanji_used - 1 do begin
      freqi:=freql.IndexOf(ed.kanji[i].kanji);
      if freqi<>-1 then
        prior[i]:=integer(freql.Objects[freqi]);
    end;

 //Write out kanji-kana entries
  for i := 0 to ed.kana_used - 1 do begin
    kanji_matches_found := 0;
    for j := 0 to ed.kanji_used - 1 do begin

      kanji_local_idx := -1; //kanji index in kana's personal kanji list
     //kana matches only selected kanjis
      if ed.kana[i].kanji_used <> 0 then begin
        for k := 0 to ed.kana[i].kanji_used - 1 do
          if ed.kanji[j].kanji=ed.kana[i].kanji[k] then begin
            Inc(kanji_matches_found);
            kanji_local_idx := k;
            break;
          end;
        if kanji_local_idx < 0 then
          continue;
      end;

      //Dictionaries usually put kana and kanji in their usage order, so give
      //slightly better priority to earlier entries:
      //   First, sort by kana order,
      //   For the same kana, sort by kanji order
      prior_this := prior[j] + (ed.kana_used - i) * ed.kanji_used;
      if kanji_local_idx >= 0 then
        prior_this := prior_this - kanji_local_idx //if this kana has a personal kanji list, take position from there
      else
        prior_this := prior_this - j;

      rec := dic.TTDict.AddRecord([ed.kana[i].kana, ed.kanji[j].kanji, roma[i],
        string(ToWakanMarkers(ed.kanji[j].markers+ed.kana[i].markers)), IntToStr(prior_this), s_art]);
     { Markers are converted to (Unicode)String in the previous line. This can potentially
      lead to marker corruption since markers include illegal Ansi characters.
      So we set markers again, directly as Ansi (exactly as they are stored) }
      dic.TTDict.SetAnsiField(rec,3,ToWakanMarkers(ed.kanji[j].markers+ed.kana[i].markers));
      if charidx<>nil then
        IndexChars(rec, ed.kanji[j].kanji);

    end;

    //If explicit kanji were set for this kana, and not all of them were found
    if ed.kana[i].kanji_used <> kanji_matches_found then begin
      roma_prob.Writeln(fstr('Kana ')+ed.kana[i].kana+fstr(': some of explicit kanji matches not found.'));
      Inc(ProblemRecords);
    end;
  end;

 //Additional markers to every entry
  if ed.pop then
    add_mark := MarkPop
  else
    add_mark := '';

 //Write out senses
  for i := 0 to ed.senses_used - 1 do begin
    rec := dic.TTEntries.AddRecord([s_art, ed.senses[i].text, string(add_mark+ToWakanMarkers(ed.senses[i].pos+ed.senses[i].markers))]);
    dic.TTEntries.SetAnsiField(rec,2,add_mark+ToWakanMarkers(ed.senses[i].pos+ed.senses[i].markers)); //see above
    if wordidx<>nil then
      IndexWords(rec, ed.senses[i].text);
  end;

end;

//Decodes a string in form "D N A jian4 ding4" to local pinyin/bopomofo,
//preserving latin characters.
//Also allows for some punctuation.
procedure DecodeRomajiCC(const s:UnicodeString;lang:char;out PinYin:string;out Bopomofo:FString);
var syl:string;
  i: integer;

  procedure Commit;
  var tmp: string;
  begin
    if (length(syl)=1) and (
      IsLatinLetterW(syl[1])        //latin chars are allowed
      or IsAllowedPunctuation(syl[1]) //some punctuation is allowed
    ) then begin
      Bopomofo := Bopomofo + syl[1];
      if IsLatinLetterW(syl[1]) then //punctuation does not make it into pinyin
        PinYin := PinYin + syl[1];
    end else begin
      tmp := DbRomajiToKana(syl,lang,[]);
      Bopomofo := Bopomofo + tmp;
      PinYin := PinYin + DbKanaToRomaji(SanitizeKana(tmp),lang); //this way we make sure no unsupported stuff gets into pinyin
    end;
  end;

begin
  PinYin := '';
  Bopomofo := '';
  syl := '';
  for i := 1 to length(s) do
    if s[i]=' ' then begin
      Commit;
      syl := '';
    end else
      syl := syl + s[i];
  Commit;
end;

{ This also works for older CEDICTs }
function TDicImportJob.ImportCCEdict(fuin: TStreamDecoder): integer;
const
 //Format markers
  UH_CEDICT_COMMENT = {$IFDEF UNICODE}'#'{$ELSE}'0023'{$ENDIF};
var
  ustr: string;
  i: integer;
  ed: TEdictArticle;
  roma: TEdictRoma;
  loclineno: integer;
  pphon: UnicodeString;
begin
  loclineno := 0;
  //Read another line
  while fuin.ReadLn(ustr) do begin
    UpdateProgress();
    Inc(loclineno);
    inc(lineno);

    if (pos({$IFDEF UNICODE}#$FF1F#$FF1F{$ELSE}'FF1FFF1F'{$ENDIF},ustr)<>0)  //EDICT header line -- CEDICT shouldn't have it but whatever
    or (pos(UH_CEDICT_COMMENT,ustr)<>0) then
      continue;

    try
      ParseCCEdictLine(ustr, @ed);

     //Unlike with EDICT we can't just assume kanji contained pinyin and copy it to kana.
     //For now I fail such records (haven't seen them in the wild anyway).
      if ed.kana_used<=0 then begin
        roma_prob.Writeln('Line '+IntToStr(loclineno)+': no reading.');
        Inc(ProblemRecords);
        continue;
      end;

     {
      CC-EDICT has kana in form of "D N A jian4 ding4".
      See http://code.google.com/p/wakan/issues/detail?id=121 for details on
      how we convert it and why.
     }

      //Generate romaji
      for i := 0 to ed.kana_used - 1 do begin
        pphon:=ed.kana[i].kana; //copy original pin yin before conversions
        DecodeRomajiCC(pphon,dic.language,roma[i],ed.kana[i].kana);
        roma[i]:=SignatureFrom(roma[i]); //lowercase + safety for invalid chars
        if pos('?',roma[i])>0 then
        begin
          if dic.language='c' then
            roma_prob.Writeln('Line '+IntToStr(loclineno)+': '+pphon+' ('+ed.kana[i].kana+') ---> '+roma[i])
          else
            roma_prob.Writeln('Line '+IntToStr(loclineno)+': '+pphon+' ---> '+roma[i]);
          Inc(ProblemRecords);
        end;
        roma[i]:=repl(roma[i],'?','');
        if roma[i]='' then roma[i]:='XXX';
      end;

      //Convert entries
      for i := 0 to ed.senses_used - 1 do begin
        ed.senses[i].text:=repl(ed.senses[i].text,UH_EDICT_SEMICOL,','); //replace ; with ,
        ed.senses[i].text:=repl(ed.senses[i].text,UH_EDICT_ALTERN,'; '); //replace / with ;
      end;

      AddArticle(@ed, roma);

    except
      on E: EEdictParsingException do begin
        roma_prob.Writeln('Line '+IntToStr(loclineno)+': '+E.Message);
        Inc(ProblemRecords);
        if (ProblemRecords>400) and (ProblemRecords>Trunc(0.75*LineCount)) then
          raise EDictImportException.Create(_l(eCannotImportDict));
      end;
    end;

  end; //for every line

  Result := loclineno;
end;

function TDicImportJob.ImportEdict(fuin: TStreamDecoder): integer;
var
  ustr: string;
  i: integer;
  ed: TEdictArticle;
  san_kana: string;
  roma: TEdictRoma;
  loclineno: integer;
begin
  loclineno := 0;
  //Read another line
  while fuin.ReadLn(ustr) do begin
    UpdateProgress();
    Inc(loclineno);
    inc(lineno);

    if (pos({$IFDEF UNICODE}#$FF1F#$FF1F{$ELSE}'FF1FFF1F'{$ENDIF},ustr)<>0) then //EDICT header line
      continue;
    if ustr='' then continue; //sometimes last line

    try
      ParseEdict2Line(ustr, @ed);

     //Sometimes we only have kana and then it's written as kanji
      if ed.kana_used=0 then begin
        ed.kana_used := ed.kanji_used;
        for i := 0 to ed.kanji_used - 1 do begin
          ed.kana[i].Reset;
          ed.kana[i].kana := ed.kanji[i].kanji;
        end;
      end;

      //Generate romaji
      for i := 0 to ed.kana_used - 1 do begin
       //First check for completely invalid characters
        san_kana := SanitizeKana(ed.kana[i].kana, [sfKeepLatin, sfKeepAllowedPunctuation], '?');
        if pos('?', san_kana)>0 then begin
          roma_prob.Writeln('Line '+IntToStr(loclineno)+': '+ed.kana[i].kana+' -> '+san_kana);
          Inc(ProblemRecords);
        end;

       //Now keep latin, clean the rest (punctuation+invalid)
        roma[i] := SignatureFrom(DbKanaToRomaji(san_kana, dic.language, []));
        if roma[i]='' then roma[i]:='XXX';
      end;

      //Convert entries
      for i := 0 to ed.senses_used - 1 do begin
        ed.senses[i].text:=repl(ed.senses[i].text,UH_EDICT_SEMICOL,','); //replace ; with ,
        ed.senses[i].text:=repl(ed.senses[i].text,UH_EDICT_ALTERN,'; '); //replace / with ;
      end;

      AddArticle(@ed, roma);

    except
      on E: EEdictParsingException do begin
        roma_prob.Writeln('Line '+IntToStr(loclineno)+': '+E.Message);
        Inc(ProblemRecords);
        if (ProblemRecords>400) and (ProblemRecords>Trunc(0.75*LineCount)) then
          raise EDictImportException.Create(_l(eCannotImportDict));
      end;
    end;

  end; //for every line

  Result := loclineno;
end;

function GetLastWriteTime(const filename: string; out dt: TDatetime): boolean;
var h: THandle;
  ft: TFileTime;
  st: TSystemTime;
begin
  h := CreateFile(PChar(filename), GENERIC_READ, FILE_SHARE_READ or FILE_SHARE_WRITE,
    nil, OPEN_EXISTING, 0, 0);
  if h=INVALID_HANDLE_VALUE then begin
    Result := false;
    exit;
  end;
  try
    if not GetFileTime(h,nil,nil,@ft) then begin
      Result := false;
      exit;
    end;
  finally
    CloseHandle(h);
  end;

  FileTimeToSystemTime(ft,st);
  dt := SystemTimeToDatetime(st); //TODO: We want UTC!
  Result := true;
end;

{ Compiles "sources.lst" -- a file listing dictionary sources and their last write times }
procedure TDicImportJob.WriteSources(const fname: string);
var fwr: TStreamEncoder;
  fi: integer;
  dt: TDatetime;
begin
  fwr := CreateTextFile(fname, TUTF16LEEncoding);
  try
    fwr.WriteChar(#$FEFF); //BOM
    for fi:=0 to Length(FFiles)-1 do begin
      if not GetLastWriteTime(FFiles[fi].Filename, dt) then
        dt := 0; //unknown -- will be skipped, unless it becomes known later
      fwr.Writeln(EncodeSourceFilename(ExtractFilename(FFiles[fi].Filename))+','
        +DatetimeToStr(dt, DictFormatSettings));
    end;
  finally
    FreeAndNil(fwr);
  end;
end;

initialization
  _CachedFrequencyList := nil;

finalization
 {$IFDEF CLEAN_DEINIT}
  FreeAndNil(_CachedFrequencyList);
 {$ENDIF}

end.
