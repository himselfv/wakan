unit JWBDictImport;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, ExtCtrls, StdPrompt, JWBStrings, JWBIO, JWBDic, JWBIndex,
  JWBEdictMarkers, JWBEdictReader;

type
  TImportDictFlag = (
    ifAddWordIndex,       //add word index (words from translation, for reverse-lookups)
    ifAddCharacterIndex,  //kanji index for all used kanjis
    ifAddFrequencyInfo,
    ifSilent              //don't display any UI
  );
  TImportDictFlags = set of TImportDictFlag;

  TDictInfo = record
    Name: string;
    Description: string;
    Copyright: string;
    Version: string;
    Priority: integer;
  end;
  PDictInfo = ^TDictInfo;

  TEdictRoma = array[0..MaxKana-1] of string;

  TfDictImport = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    edtDictFilename: TEdit;
    edtDictName: TEdit;
    lbFiles: TListBox;
    edtVersion: TEdit;
    rgPriority: TRadioGroup;
    edtDescription: TEdit;
    edtCopyright: TEdit;
    btnBuild: TBitBtn;
    btnCancel: TBitBtn;
    btnAddFile: TButton;
    btnRemoveFile: TButton;
    AddFileDialog: TOpenDialog;
    rgLanguage: TRadioGroup;
    cbAddWordIndex: TCheckBox;
    cbAddCharacterIndex: TCheckBox;
    cbAddFrequencyInfo: TCheckBox;
    procedure FormShow(Sender: TObject);
    procedure btnAddFileClick(Sender: TObject);
    procedure btnRemoveFileClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure btnBuildClick(Sender: TObject);

  public
    Silent: boolean;
    destructor Destroy; override;

  protected
    procedure CreateDictTables(dicFilename: string; info: TDictInfo; diclang:char; entries: integer);
    procedure WriteDictPackage(dicFilename: string; tempDir: string; info: TDictInfo;
      diclang:char; entries:integer);
    procedure RunUniConv(srcFile, outpFile: string; srcEnc: string);

  private
   //Shared between ImportDictionary calls
    cfreql:TStringList; //cached frequency list, call GetFrequencyList
    roma_prob: TUnicodeFileWriter; //error log
    roma_prob_cnt: integer; //increased on every record error
    ProblemRecords: integer; //# of records which weren't imported
    ProblemDictionaries: string; //>5% of each wasn't imported

  protected //Dictionary building
    dic:TJaletDic;
    linecount,lineno:integer;
    prog: TSMPromptForm;
    freql:TStringList; //frequency list, if used
    wordidx:TWordIndexBuilder;
    charidx:TIndexBuilder;
    LastDictEntry: integer;
    LastArticle: integer;
    function GetFrequencyList: TStringList;
    procedure AddArticle(const ed: PEdictArticle; const roma: TEdictRoma);
    function ImportCCEdict(fuin: TUnicodeFileReader): integer;
    function ImportEdict(fuin: TUnicodeFileReader): integer;
    procedure SetProgress(perc: integer);

  public
    function SupportsFrequencyList: boolean;
    function ImportDictionary(dicFilename: string; info: TDictInfo;
      files: TFileList; diclang:char; flags: TImportDictFlags): boolean;

  end;

  EDictImportException = class(Exception);

var
  fDictImport: TfDictImport;

function GetLastWriteTime(const filename: string; out dt: TDatetime): boolean;

implementation

uses StrUtils, WideStrUtils, JWBDictCoding, JWBKanaConv, JWBUnit, JWBMenu,
  PKGWrite, JWBConvert, JWBDicSearch;

{$R *.DFM}

procedure TfDictImport.FormShow(Sender: TObject);
begin
  edtDictFilename.text:='noname';
  edtDictName.text:='NONAME';
  lbFiles.items.clear;
  btnRemoveFile.enabled:=false;
  btnBuild.enabled:=false;
  edtVersion.text:='N/A';
  rgPriority.ItemIndex:=0;
  edtDescription.text:='';
  edtCopyright.text:='';
end;

destructor TfDictImport.Destroy;
begin
  FreeAndNil(roma_prob);
  FreeAndNil(cfreql);
  inherited;
end;

procedure TfDictImport.btnAddFileClick(Sender: TObject);
begin
  if AddFileDialog.execute then
  begin
    lbFiles.Items.Add(AddFileDialog.FileName);
    btnRemoveFile.enabled:=true;
    btnBuild.enabled:=true;
    if lbFiles.ItemIndex=-1 then lbFiles.ItemIndex:=0;
  end;
end;

procedure TfDictImport.btnRemoveFileClick(Sender: TObject);
begin
  lbFiles.Items.Delete(lbFiles.ItemIndex);
  if lbFiles.Items.Count>0 then lbFiles.ItemIndex:=0 else
  begin
    btnRemoveFile.Enabled:=false;
    btnBuild.enabled:=false;
  end;
end;

procedure TfDictImport.btnCancelClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TfDictImport.SetProgress(perc: Integer);
begin
  prog.SetProgress(perc);
  prog.ProcessMessages;
  if prog.ModalResult=mrCancel then begin
    prog.SetProgressPaused(true);
    if Application.MessageBox(
      PChar(_l('#01003^eThe dictionary has not been yet completely imported. Do you '
        +'really want to abort the operation?')),
      PChar(_l('#01004^eConfirm abort')),
      MB_ICONQUESTION+MB_YESNO
    )=idYes then
      raise EAbort.Create('Aborted by user'); //no need to localize
    prog.ModalResult := 0;
    prog.SetProgressPaused(false);
    prog.Show; //ModalResult hides it
  end;
end;

procedure TfDictImport.CreateDictTables(dicFilename: string; info: TDictInfo;
  diclang:char; entries: integer);
var tempDir: string;
  t:textfile;
begin
  tempDir := CreateRandomTempDirName();
  ForceDirectories(tempDir);

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
  DeleteDirectory(tempDir);
end;

procedure TfDictImport.WriteDictPackage(dicFilename: string; tempDir: string;
  info: TDictInfo; diclang:char; entries:integer);
var f:textfile;
  path:string;
begin
  path := ExtractFilePath(dicFilename);
  if path<>'' then ForceDirectories(path);

  assignfile(f,tempDir+'\dict.ver');
  rewrite(f);
  writeln(f,'DICT');
  writeln(f,'5');
  writeln(f,inttostr(trunc(now)));
  writeln(f,info.Version);
  writeln(f,info.Name);
  writeln(f,diclang);
  writeln(f,info.Description);
  writeln(f,IntToStr(info.Priority));
  writeln(f,inttostr(entries));
  writeln(f,info.Copyright);
  closefile(f);

  PKGWriteForm.PKGWriteCmd('NotShow');
  PKGWriteForm.PKGWriteCmd('PKGFileName '+dicFilename);
  PKGWriteForm.PKGWriteCmd('MemoryLimit 100000000');
  PKGWriteForm.PKGWriteCmd('Name '+info.Name);
  PKGWriteForm.PKGWriteCmd('TitleName '+info.Name+' Dictionary');
  PKGWriteForm.PKGWriteCmd('CopyrightName '+info.Copyright);
  PKGWriteForm.PKGWriteCmd('FormatName Pure Package File');
  PKGWriteForm.PKGWriteCmd('CommentName File is used by '+WakanAppName);
  PKGWriteForm.PKGWriteCmd('VersionName 1.0');
  PKGWriteForm.PKGWriteCmd('HeaderCode 791564');
  PKGWriteForm.PKGWriteCmd('FileSysCode 978132');
  PKGWriteForm.PKGWriteCmd('WriteHeader');
  PKGWriteForm.PKGWriteCmd('TemporaryLoad');
  PKGWriteForm.PKGWriteCmd('CryptMode 0');
  PKGWriteForm.PKGWriteCmd('CRCMode 0');
  PKGWriteForm.PKGWriteCmd('PackMode 0');
  PKGWriteForm.PKGWriteCmd('CryptCode 978123');
  PKGWriteForm.PKGWriteCmd('Include '+tempDir);
  PKGWriteForm.PKGWriteCmd('Finish');
end;

procedure TfDictImport.btnBuildClick(Sender: TObject);
var i: integer;
  files: TFileList;
  diclang:char;
  flags: TImportDictFlags;
  info: TDictInfo;
  fname: string;
begin
  SetLength(files, lbFiles.Items.Count);
  for i := 0 to lbFiles.Items.Count - 1 do
    files[i] := lbFiles.Items[i];

  case rgLanguage.ItemIndex of
    0:diclang:='j';
    1:diclang:='c';
  else diclang:='j';
  end;

  flags := [];
  if cbAddWordIndex.Checked then flags := flags + [ifAddWordIndex];
  if cbAddCharacterIndex.Checked then flags := flags + [ifAddCharacterIndex];
  if cbAddFrequencyInfo.Checked then flags := flags + [ifAddFrequencyInfo];
  if Silent then flags := flags + [ifSilent];


  fname := edtDictFilename.text;
  if not EndsStr('.dic', LowerCase(fname, loUserLocale)) then
    fname := fname + '.dic';

  info.name := edtDictName.text;
  if info.name='' then info.name := ExtractFilename(edtDictFilename.text);
  info.version := edtVersion.Text;
  info.description := edtDescription.text;
  info.copyright := edtCopyright.text;
  info.priority := rgPriority.itemindex;

  if ImportDictionary(fname, info, files, diclang, flags) then
  begin
    ModalResult := mrOk;
    if not Silent then
      Application.MessageBox(
        pchar(_l('#00093^eDictionary was built.')),
        pchar(_l('#00094^eSuccess')),
        MB_ICONINFORMATION or MB_OK);
  end;
end;

const
  UH_EDICT_SEMICOL = {$IFDEF UNICODE}';'{$ELSE}'003B'{$ENDIF};
  UH_EDICT_COMMA = {$IFDEF UNICODE}','{$ELSE}'002C'{$ENDIF};
  UH_EDICT_ALTERN = {$IFDEF UNICODE}'/'{$ELSE}'002F'{$ENDIF};

{
Adds an article to the dictionary we're building.
Also adds it to all indixes.
}
procedure TfDictImport.AddArticle(const ed: PEdictArticle; const roma: TEdictRoma);
var freqi: integer;
  prior: array[0..MaxKanji-1] of string; //strings because we need to convert to string when adding anyway
  i, j, k: integer;
  s_art: string;
  kanji_found: boolean;
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
        urepl(u_part,';',',');
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

 //Priority
  for i := 0 to ed.kanji_used - 1 do
    prior[i] := '0';
  if freql<>nil then
    for i := 0 to ed.kanji_used - 1 do begin
      freqi:=freql.IndexOf(ed.kanji[i].kanji);
      if freqi<>-1 then
        prior[i]:=IntToStr(integer(freql.Objects[freqi]));
    end;

 //Write out kanji-kana entries
  for i := 0 to ed.kana_used - 1 do
    if ed.kana[i].kanji_used=0 then begin
     //kana matches any kanji
      for j := 0 to ed.kanji_used - 1 do begin
        rec := dic.TTDict.AddRecord([ed.kana[i].kana, ed.kanji[j].kanji, roma[i],
          string(ed.kanji[j].markers+ed.kana[i].markers), prior[j], s_art]);
       { Markers are converted to (Unicode)String in the previous line. This can potentially
        lead to marker corruption since markers include illegal Ansi characters.
        So we set markers again, directly as Ansi (exactly as they are stored) }
        dic.TTDict.SetAnsiField(rec,3,ed.kanji[j].markers+ed.kana[i].markers);
        if charidx<>nil then
          IndexChars(rec, ed.kanji[j].kanji);
      end;
    end else begin
     //kana matches only selected kanjis
      for j := 0 to ed.kana[i].kanji_used - 1 do begin
        kanji_found := false;
        for k := 0 to ed.kanji_used - 1 do
          if ed.kanji[k].kanji=ed.kana[i].kanji[j] then begin
            rec := dic.TTDict.AddRecord([ed.kana[i].kana, ed.kanji[k].kanji, roma[i],
              string(ed.kanji[k].markers+ed.kana[i].markers), prior[k], s_art]);
            dic.TTDict.SetAnsiField(rec,3,ed.kanji[k].markers+ed.kana[i].markers); //see above
            if charidx<>nil then
              IndexChars(rec, ed.kanji[k].kanji);
            kanji_found := true;
            break;
          end;
        if not kanji_found then begin
          roma_prob.WritelnUnicode(fstr('Kana ')+ed.kana[i].kana+fstr(': some of explicit kanji matches not found.'));
          Inc(roma_prob_cnt);
        end;
      end;
    end;

 //Additional markers to every entry
  if ed.pop then
    add_mark := MarkPop
  else
    add_mark := '';

 //Write out senses
  for i := 0 to ed.senses_used - 1 do begin
    rec := dic.TTEntries.AddRecord([s_art, ed.senses[i].text, string(add_mark+ed.senses[i].pos+ed.senses[i].markers)]);
    dic.TTEntries.SetAnsiField(rec,2,add_mark+ed.senses[i].pos+ed.senses[i].markers); //see above
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
      tmp := RomajiToKana(syl,1,lang,[]);
      Bopomofo := Bopomofo + tmp;
      PinYin := PinYin + KanaToRomaji(tmp,1,lang); //this way we make sure no unsupported stuff gets into pinyin
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
function TfDictImport.ImportCCEdict(fuin: TUnicodeFileReader): integer;
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
    Inc(loclineno);

    if (linecount>0) and (lineno mod 100=0) then
      SetProgress(round(lineno/linecount*100));
    inc(lineno);

    if (pos({$IFDEF UNICODE}#$FF1F#$FF1F{$ELSE}'FF1FFF1F'{$ENDIF},ustr)<>0)  //EDICT header line -- CEDICT shouldn't have it but whatever
    or (pos(UH_CEDICT_COMMENT,ustr)<>0) then
      continue;

    try
      ParseCCEdictLine(ustr, @ed);

     //Unlike with EDICT we can't just assume kanji contained pinyin and copy it to kana.
     //For now I fail such records (haven't seen them in the wild anyway).
      if ed.kana_used<=0 then begin
        roma_prob.WritelnUnicode('Line '+IntToStr(loclineno)+': no reading.');
        Inc(roma_prob_cnt);
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
            roma_prob.WritelnUnicode('Line '+IntToStr(loclineno)+': '+pphon+' ('+ed.kana[i].kana+') ---> '+roma[i])
          else
            roma_prob.WritelnUnicode('Line '+IntToStr(loclineno)+': '+pphon+' ---> '+roma[i]);
          Inc(roma_prob_cnt);
        end;
        repl(roma[i],'?','');
        if roma[i]='' then roma[i]:='XXX';
      end;

      //Convert entries
      for i := 0 to ed.senses_used - 1 do begin
        urepl(ed.senses[i].text,UH_EDICT_SEMICOL,','); //replace ; with ,
        urepl(ed.senses[i].text,UH_EDICT_ALTERN,'; '); //replace / with ;
      end;

      AddArticle(@ed, roma);

    except
      on E: EEdictParsingException do begin
        roma_prob.WritelnUnicode('Line '+IntToStr(loclineno)+': '+E.Message);
        Inc(roma_prob_cnt);
      end;
    end;

  end; //for every line

  Result := loclineno;
end;

function TfDictImport.ImportEdict(fuin: TUnicodeFileReader): integer;
var
  ustr: string;
  i: integer;
  ed: TEdictArticle;
  roma: TEdictRoma;
  loclineno: integer;
begin
  loclineno := 0;
  //Read another line
  while fuin.ReadLn(ustr) do begin
    Inc(loclineno);

    if (linecount>0) and (lineno mod 100=0) then
      SetProgress(round(lineno/linecount*100));
    inc(lineno);

    if (pos({$IFDEF UNICODE}#$FF1F#$FF1F{$ELSE}'FF1FFF1F'{$ENDIF},ustr)<>0) then //EDICT header line
      continue;

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
        roma[i]:=KanaToRomaji(ed.kana[i].kana,1,dic.language,[rfConvertLatin,rfConvertPunctuation]);
        if pos('?',roma[i])>0 then begin
          roma_prob.WritelnUnicode('Line '+IntToStr(loclineno)+': '+ed.kana[i].kana+' -> '+roma[i]);
          Inc(roma_prob_cnt);
        end;

       //Now keep latin, clean the rest (punctuation+invalid)
        roma[i]:=SignatureFrom(KanaToRomaji(ed.kana[i].kana,1,dic.language,[rfConvertLatin,rfDeleteInvalidChars]));
        if roma[i]='' then roma[i]:='XXX';
      end;

      //Convert entries
      for i := 0 to ed.senses_used - 1 do begin
        urepl(ed.senses[i].text,UH_EDICT_SEMICOL,','); //replace ; with ,
        urepl(ed.senses[i].text,UH_EDICT_ALTERN,'; '); //replace / with ;
      end;

      AddArticle(@ed, roma);

    except
      on E: EEdictParsingException do begin
        roma_prob.WritelnUnicode('Line '+IntToStr(loclineno)+': '+E.Message);
        Inc(roma_prob_cnt);
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
procedure WriteSources(const fname: string; const files:TFileList);
var fwr:TUnicodeFileWriter;
  fi: integer;
  dt: TDatetime;
begin
  fwr := TUnicodeFileWriter.Rewrite(fname);
  try
    fwr.WriteWideChar(#$FEFF); //BOM
    for fi:=0 to Length(files)-1 do begin
      if not GetLastWriteTime(files[fi],dt) then
        dt := 0; //unknown -- will be skipped, unless it becomes known later
      fwr.WritelnUnicode(ExtractFilename(files[fi])+','
        +DatetimeToStr(dt, DictFormatSettings));
    end;
  finally
    FreeAndNil(fwr);
  end;
end;


{
ImportDictionary()
Builds Wakan package from one or more dictionaries.
Returns true if the package was successfully built, false if aborted.
}
function TfDictImport.ImportDictionary(dicFilename: string; info: TDictInfo;
  files: TFileList; diclang:char; flags: TImportDictFlags): boolean;
var
  tempDir, tempDir2: string;
  fi:integer;
  fname:string;
  mes:string;
  cd:string; //stores chosen encoding when converting
  fuin:TUnicodeFileReader;
  uc: WideChar;
  dicrecno: integer;

begin
  prog:=SMProgressDlgCreate(_l('#00071^eDictionary import'),_l('^eImporting...'),100,{CanCancel=}true);
  if not self.Visible then //auto mode
    prog.Position := poScreenCenter;
  prog.Width := 500; //we're going to have long file names
  prog.AppearModal;
  wordidx := nil;
  charidx := nil;
  freql := nil;
  ProblemRecords := 0;
  ProblemDictionaries := '';
  dic := nil;
  tempDir := '';
  tempDir2 := '';
  linecount:=0;
  LastArticle := 0;
  LastDictEntry := 0;
  try
    prog.SetMaxProgress(0); //indeterminate state

   //Create indexes
    if ifAddCharacterIndex in flags then
      charidx := TIndexBuilder.Create;
    if ifAddWordIndex in flags then
      wordidx := TWordIndexBuilder.Create;

   //Create frequency list
    if ifAddFrequencyInfo in flags then
    try
      prog.SetMessage(_l('#00917^eCreating frequency chart...'));
      freql := GetFrequencyList;
    except
      on E: EDictImportException do begin
        E.Message := 'Frequency list creation failed: '+E.Message;
        raise;
      end;
    end;

   { Create temporary dir and dictionary tables }
    tempDir := CreateRandomTempDirName();
    ForceDirectories(tempDir);

    CreateDictTables(tempDir+'\DICT.TMP', info, diclang, 0);
    dic := fMenu.NewDict(tempDir+'\DICT.TMP');
    if not dic.tested then
      raise EDictImportException.Create('Cannot load the newly created dictionary.');
    dic.Load;
    if not dic.loaded then
      raise EDictImportException.Create('Cannot load the target dictionary');
    dic.Demand;
    dic.TTDict.NoCommitting := true;
    dic.TTEntries.NoCommitting := true;

   { Convert all dictionaries to UTF16-LE }
    for fi:=0 to Length(files)-1 do
    begin
      fname:=tempDir+'\DICT_'+inttostr(fi)+'.TMP';
      mes:=_l('#00085^eConverting '); //used later too
      prog.SetMessage(mes+ExtractFilename(files[fi])+'...');

      if not FileExists(files[fi]) then
        raise EDictImportException.CreateFmt(_l('File not found: %s'), [files[fi]]);

      fDictCoding.Label2.Caption:=_l('#00087^eInput file: ')+ExtractFilename(files[fi]);
      if ifSilent in flags then begin
       //Choose default encoding
        case diclang of
          'j': fDictCoding.RadioGroup1.ItemIndex:=1;
          'c': fDictCoding.RadioGroup1.ItemIndex:=2;
        else fDictCoding.RadioGroup1.ItemIndex:=0;
        end;
      end else
      begin
       //Ask user
        fDictCoding.ShowModal;
        if not fDictCoding.succeeded then
          raise EAbort.Create('File conversion aborted ('+files[fi]+').');
      end;

      if fDictCoding.RadioGroup1.ItemIndex=0 then
        CopyFile(pchar(files[fi]),pchar(fname),false)
      else
      begin
        case fDictCoding.RadioGroup1.ItemIndex of
          1:cd:='JapaneseAutoDetect';
          2:cd:='ChineseAutoDetect';
          3:cd:='KoreanAutoDetect';
        end;
        try
          RunUniConv(files[fi], fname, cd);
        except
          on E: EDictImportException do begin
            E.Message := _l('While converting ')+files[fi]+': '+E.Message;
            raise;
          end;
        end;
      end;

      if not FileExists(fname) then
        raise EDictImportException.CreateFmt(_l('File conversion failed (%s)'), [files[fi]]);

     //Count number of lines in the converted file and add to total
      fuin := TUnicodeFileReader.Create(fname);
      while fuin.ReadWideChar(uc) do
        if uc=#$000A then
          Inc(linecount);
      FreeAndNil(fuin);
    end;


   { Import }
    if roma_prob=nil then begin
      roma_prob := TUnicodeFileWriter.Rewrite(UserDataDir+'\roma_problems.txt');
      roma_prob.WriteWideChar(#$FEFF); //BOM
    end;

    prog.SetMaxProgress(100); //progress bar
    lineno:=0;
    for fi:=0 to Length(files)-1 do
    begin
      mes:=_l('#00086^eReading && parsing ');
      prog.SetMessage(mes+ExtractFilename(files[fi])+'...');

      fuin := TUnicodeFileReader.Create(tempDir+'\DICT_'+inttostr(fi)+'.TMP');
      try
        if not fuin.ReadWideChar(uc) or (uc<>#$FEFF) then
          raise EDictImportException.Create(_l('#00088^eUnsupported file encoding')+' ('+files[fi]+')');

        roma_prob_cnt := 0;

        if diclang='c' then
          dicrecno:=ImportCCEdict(fuin)
        else
          dicrecno:=ImportEdict(fuin);

        Inc(ProblemRecords,roma_prob_cnt);
        if roma_prob_cnt>dicrecno/20 then //>5% errors
          ProblemDictionaries := ProblemDictionaries + ','+ExtractFilename(files[fi]);

      finally
        FreeAndNil(fuin);
      end;
    end; //for every file

    roma_prob.Flush; //just in case someone goes looking at it straight away
    prog.Invalidate;
    prog.Repaint;
    prog.ProcessMessages;

    prog.SetMessage(_l('^eRebuilding index...'));
    dic.TTDict.NoCommitting := false;
    dic.TTDict.Reindex;
    dic.TTEntries.NoCommitting := true;
    dic.TTEntries.Reindex;

   //This time it's for our package
    tempDir2 := CreateRandomTempDirName();
    ForceDirectories(tempDir2);

    if charidx<>nil then begin
      prog.SetMessage(_l('^eWriting character index...'));
      charidx.Write(tempDir2+'\CharIdx.bin');
    end;

    if wordidx<>nil then begin
      prog.SetMessage(_l('^eWriting word index...'));
      wordidx.Write(tempDir2+'\WordIdx.bin');
    end;

    prog.SetMessage(_l('^eWriting dictionary table...'));
    dic.TTDict.WriteTable(tempDir2+'\Dict',true);
    dic.TTEntries.WriteTable(tempDir2+'\Entries',true);
    FreeAndNil(dic);

    DeleteDirectory(tempDir); //empty dictionary
    tempDir := '';

    WriteSources(tempDir2+'\sources.lst',files);
    WriteDictPackage(dicFilename, tempDir2, info, diclang, LastArticle);
    DeleteDirectory(tempDir2);
    tempDir2 := '';

  finally
    if tempDir<>'' then
      DeleteDirectory(tempDir);
    if tempDir2<>'' then
      DeleteDirectory(tempDir);
    FreeAndNil(dic);
    FreeAndNil(wordidx);
    FreeAndNil(charidx);
    FreeAndNil(prog);
    freql := nil;
  end;

  if (ProblemDictionaries<>'') or (ProblemRecords > 300) then
    Application.MessageBox(
      PChar('There were some problems during the conversion. '
      +IntToStr(ProblemRecords)+' records could not have been imported.'#13
      +'Please study the roma_problems.txt found in the application directory.'),
      'Had problems',
      MB_ICONEXCLAMATION)
  else
  if (ProblemRecords > 0) then
    Application.MessageBox(
      PChar('The dictionary has been created but '+IntToStr(ProblemRecords)
      +' records had some problems.'#13
      +'This is not much so it''s probably fine, but if you want details, '
      +'study the roma_problems.txt found in the application directory.'),
      'Notice',
      MB_ICONINFORMATION
    );

  Result := true;
end;

{
Returns true if importer things it could load WORDFREQ_CK and add frequency info,
if asked to.
}
function TfDictImport.SupportsFrequencyList: boolean;
begin
  Result:= FileExists('wordfreq_ck.uni')
    or FileExists('wordfreq_ck.euc')
    or FileExists('wordfreq_ck');
end;

{
Creates and populates TStringList with frequency information taken from wordfreq_ck.
Caches it and returns. Do not destroy it.
}
function TfDictImport.GetFrequencyList: TStringList;
const
 //Format markers
  UH_FREQ_COMMENT = {$IFDEF UNICODE}'#'{$ELSE}'0023'{$ENDIF};
  UH_TAB = {$IFDEF UNICODE}#$0009{$ELSE}'0009'{$ENDIF};
var fc: FChar;
  newline,nownum,comment:boolean;
  addkan,addnum:string;
  tp: byte;
begin
  if cfreql<>nil then begin
    Result := cfreql;
    exit;
  end;

  Result:=TStringList.Create;
  if FileExists('wordfreq_ck.uni') then
    Conv_Open('wordfreq_ck.uni', FILETYPE_UTF16LE)
  else
  if FileExists('wordfreq_ck.euc') then
    Conv_Open('wordfreq_ck.euc', FILETYPE_EUC)
  else
  if FileExists('wordfreq_ck') then begin
   //best guess
    tp := Conv_DetectType('wordfreq_ck');
    Conv_Open('wordfreq_ck', tp);
  end else
    raise EDictImportException.Create(_l('#00915^eCannot find WORDFREQ_CK file.'));

  newline:=true;
  nownum:=false;
  addkan:='';
  addnum:='';
  comment:=false;
  while Conv_ReadChar(fc) do
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
  Result.Sorted:=true;
  Result.Sort;
  cfreql := Result;
end;

{
Executes UNICONV.exe to convert srcFile to outpFile.
srcFile is assumed to be in srcEnc encoding (see UCONV docs). outpFile is in UCS2.
Raise exceptions on any errors.
}
procedure TfDictImport.RunUniConv(srcFile, outpFile: string; srcEnc: string);
var lpi:PROCESS_INFORMATION;
  si:STARTUPINFO;
  fail: boolean;
  err: integer;
begin
  if not FileExists(AppFolder+'\UNICONV.exe') then
    raise EDictImportException.Create(_l('#00070^eUNICONV.EXE was not found. It is required for encoding conversion.'));

  FillChar(lpi, sizeof(lpi), 0);
  FillChar(si, sizeof(si), 0);
  si.dwFlags:=STARTF_USESHOWWINDOW;
  si.wShowWindow:=SW_HIDE;
  if not CreateProcess(nil,pchar(AppFolder+'\UNICONV.EXE '+srcEnc+' "'+srcFile+'" UCS2 "'+outpFile+'"'),nil,nil,false,0,nil,nil,si,lpi) then
    RaiseLastOSError();

  fail := (WaitForSingleObject(lpi.hProcess,30000)<>WAIT_OBJECT_0);
  if fail then err := GetLastError() else err := 0; //shut up delphi

 { We don't try to terminate the process in case of failure for two reasons:
   1. It might be impossible on Windows 7 or XP with stricter security.
   2. It's better to leave it be, for user to see what's the problem. }

  CloseHandle(lpi.hProcess);
  CloseHandle(lpi.hThread);
  if fail then
    RaiseLastOsError(err);
end;

end.
