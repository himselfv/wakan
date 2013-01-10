unit JWBDictImport;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, ExtCtrls, StdPrompt, JWBIO, JWBDic, JWBIndex,
  JWBEdictMarkers, JWBEdictReader;

type
  TFileList = array of string;

  TImportDictFlag = (
    ifAddWordIndex,       //add word index (words from translation, for reverse-lookups)
    ifAddCharacterIndex,  //kanji index for all used kanjis
    ifAddFrequencyInfo,
    ifSilent,             //don't display any UI
    ifUnicode             //unicode format dictionary (bigger in size but supports other languages)
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

  TPrepArticle = record
    ed: TEdictArticle;
    roma: array[0..MaxKana-1] of string;
   //Packed markers have to go into their own fields to avoid Ansi<->Unicode conversions
    kanji_mark: array[0..MaxKanji-1] of TMarkers;
    kana_mark: array[0..MaxKana-1] of TMarkers;
    m_mark: array[0..MaxMeaning-1] of TMarkers;
  end;
  PPrepArticle = ^TPrepArticle;

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

  protected
    procedure CreateDictTables(dicFilename: string; info: TDictInfo; diclang:char; entries: integer);
    procedure WriteDictPackage(dicFilename: string; tempDir: string; info: TDictInfo;
      diclang:char; entries:integer);
    procedure RunUniConv(srcFile, outpFile: string; srcEnc: string);

  protected //Dictionary building
    dic:TJaletDic;
    linecount,lineno:integer;
    prog: TSMPromptForm;
    roma_prob: TUnicodeFileWriter;
    freql:TStringList; //frequency list, if used
    wordidx:TWordIndexBuilder;
    charidx:TIndexBuilder;
    had_problems: boolean;
    LastDictEntry: integer;
    LastArticle: integer;
    function CreateFrequencyList: TStringList;
    procedure AddArticle(const art: PPrepArticle);
    procedure ImportCEdict(fuin: TUnicodeFileReader);
    procedure ImportEdict(fuin: TUnicodeFileReader);

  public
    function ImportDictionary(dicFilename: string; info: TDictInfo;
      files: TFileList; diclang:char; flags: TImportDictFlags): boolean;

  end;


var
  fDictImport: TfDictImport;

implementation

uses StrUtils, WideStrUtils, JWBDictCoding, JWBUnit, JWBMenu, PKGWrite, JWBConvert,
  JWBStrings, JWBDicSearch;

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
  PKGWriteForm.PKGWriteCmd('CompanyName LABYRINTH');
  PKGWriteForm.PKGWriteCmd('CopyrightName '+info.Copyright);
  PKGWriteForm.PKGWriteCmd('FormatName Pure Package File');
  PKGWriteForm.PKGWriteCmd('CommentName File is used by WaKan - Japanese & Chinese Learning Tool');
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
procedure TfDictImport.AddArticle(const art:PPrepArticle);
var ed: PEdictArticle;
  freqi: integer;
  prior: array[0..MaxKanji-1] of string; //strings because we need to convert to string when adding anyway
  i, j, k: integer;
  s_art: string;
  kanji_found: boolean;
  u_str, u_part, u_word: UnicodeString;
  uc: WideChar;
  add_mark: TMarkers;
  rec: integer;

 //Adds given dict record to a character index for a given character
  procedure IndexChars(rec:integer;const kanji:UnicodeString);
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
  procedure IndexWords(rec:integer;const meaning:UnicodeString);
  begin
    u_str:=meaning;
    while u_str<>'' do
    begin
     //Pop next translation alternative
      u_part:=ULowerCase(ustrqpop(u_str,'/'));
      if u_part='' then continue;
      while u_part<>'' do
      begin
       //Remove all the leading (flags) (and) (markers)
        while (length(u_part)>0) and (u_part[1]='(') do
        begin
          ustrqpop(u_part,')');
          u_part := UTrimLeft(u_part);
        end;
       //Pop next word until space
        u_word:=ustrqpop(u_part,' ');
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
  ed := @art.ed;

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
        rec := dic.TTDict.AddRecord([ed.kana[i].kana, ed.kanji[j].kanji, art.roma[i],
          string(art.kanji_mark[j]+art.kana_mark[i]), prior[j], s_art]);
       { Markers are converted to (Unicode)String in the previous line. This can potentially
        lead to marker corruption since markers include illegal Ansi characters.
        So we set markers again, directly as Ansi (exactly as they are stored) }
        dic.TTDict.SetAnsiField(rec,3,art.kanji_mark[j]+art.kana_mark[i]);
        if charidx<>nil then
          IndexChars(rec, ed.kanji[j].kanji);
      end;
    end else begin
     //kana matches only selected kanjis
      for j := 0 to ed.kana[i].kanji_used - 1 do begin
        kanji_found := false;
        for k := 0 to ed.kanji_used - 1 do
          if ed.kanji[k].kanji=ed.kana[i].kanji[j] then begin
            rec := dic.TTDict.AddRecord([ed.kana[i].kana, ed.kanji[k].kanji, art.roma[i],
              string(art.kanji_mark[k]+art.kana_mark[i]), prior[k], s_art]);
            dic.TTDict.SetAnsiField(rec,3,art.kanji_mark[k]+art.kana_mark[i]); //see above
            if charidx<>nil then
              IndexChars(rec, ed.kanji[k].kanji);
            kanji_found := true;
            break;
          end;
        if not kanji_found then begin
          roma_prob.WritelnUnicode(fstr('Kana ')+ed.kana[i].kana+fstr(': some of explicit kanji matches not found.'));
          had_problems := true;
        end;
      end;
    end;

 //Additional markers to every entry
  if ed.pop then
    add_mark := MarkPop
  else
    add_mark := '';

 //Write out meanings
  for i := 0 to ed.meanings_used - 1 do begin
    rec := dic.TTEntries.AddRecord([s_art, ed.meanings[i].text, string(add_mark+art.m_mark[i])]);
    dic.TTEntries.SetAnsiField(rec,2,add_mark+art.m_mark[i]); //see above
    if wordidx<>nil then
      IndexWords(rec, ed.meanings[i].text);
  end;

end;

{
CEDICT is in a similar but different format so its importing have been left
more or less as it were.
Now's the magic!
This should also work for EDICT1. So you can use EDICT1 to test this routine.
}
procedure TfDictImport.ImportCEdict(fuin: TUnicodeFileReader);
const
 //Format markers
  UH_CEDICT_COMMENT = {$IFDEF UNICODE}'#'{$ELSE}'0023'{$ENDIF};
var
  uc:WideChar;

  ppp:integer;
  kanji:FString;
  phon:FString;
  pphon:FString;
  writ:FString;
  s_roma: string;
  s_entry: FString;
  s_mark: TMarkers;

  prep: TPrepArticle;

begin

  //Read another line
  while not fuin.Eof do
  begin
    kanji:='';
    phon:='';
    writ:='';
    ppp:=0;

    while fuin.ReadWideChar(uc) and (uc<>#$000A) do
      if (uc<>#$000A) and (uc<>#$000D) then
        case ppp of
          0: if uc=#$0020 then ppp:=1 else kanji:=kanji+fstr(uc);
          1: if uc=#$005B then ppp:=2 else ppp:=3;
          2: if uc=#$002F then ppp:=3 else
               if (uc<>#$005D) and (uc<>#$0020) then phon:=phon+fstr(uc);
        else
          writ:=writ+fstr(uc);
        end;
    if (flength(writ)>0) and (fgetch(writ, flength(writ))=UH_EDICT_ALTERN) then
      fdelete(writ,length(writ),1);
    if phon='' then phon:=kanji;

    if (linecount>0) and (lineno mod 100=0) then
      prog.SetProgress(round(lineno/linecount*100));
    inc(lineno);

    if (pos({$IFDEF UNICODE}#$FF1F#$FF1F{$ELSE}'FF1FFF1F'{$ENDIF},kanji)<>0)  //EDICT header line -- CEDICT shouldn't have it but whatever
    or (pos(UH_CEDICT_COMMENT,kanji)<>0) then
      continue;

    //Generate romaji
    pphon:=phon;
    if dic.language='c'then
    begin
      repl(phon,' ','');
      if phon<>kanji then phon:=RomajiToKana(fstrtouni(phon),1,false,dic.language);
    end;
    s_roma:=KanaToRomaji(phon,1,dic.language);
    if pos('?',s_roma)>0 then
    begin
      roma_prob.WritelnUnicode(s_roma);
      roma_prob.WritelnUnicode(fstrtouni(pphon));
      had_problems := true;
    end;
    repl(s_roma,'?','');
    if s_roma='' then s_roma:='XXX';

    s_entry:=FConvertEdictEntry(writ,s_mark);
    repl(s_entry,UH_EDICT_SEMICOL,UH_EDICT_COMMA);

    prep.ed.Reset;
    prep.ed.ref := '';
    prep.ed.AddKanji;
    prep.ed.kanji[0].kanji := kanji;
    prep.ed.AddKana;
    prep.ed.kana[0].kana := phon;
    prep.ed.AddMeaning;
    prep.ed.meanings[0].text := s_entry;
    prep.roma[0] := s_roma;
    prep.kanji_mark[0] := '';
    prep.kana_mark[0] := '';
    prep.m_mark[0] := s_mark;

    AddArticle(@prep);
  end; //for every character in the file
end;

procedure TfDictImport.ImportEdict(fuin: TUnicodeFileReader);
var
  ustr: string;
  prep: TPrepArticle;
  i: integer;
  mark_left: string;
  ed: PEdictArticle;
begin
  ed := @prep.ed;
  //Read another line
  while fuin.ReadLn(ustr) do begin

    if (linecount>0) and (lineno mod 100=0) then
      prog.SetProgress(round(lineno/linecount*100));
    inc(lineno);

    if (pos({$IFDEF UNICODE}#$FF1F#$FF1F{$ELSE}'FF1FFF1F'{$ENDIF},ustr)<>0) then //EDICT header line
      continue;

    ParseEdict2Line(ustr, @prep.ed);

   //Sometimes we only have kana and the it's written as kanji
    if ed.kana_used=0 then begin
      ed.kana_used := ed.kanji_used;
      for i := 0 to ed.kanji_used - 1 do begin
        ed.kana[i].Reset;
        ed.kana[i].kana := ed.kanji[i].kanji;
      end;
    end;

    //Convert markers
    for i := 0 to ed.kanji_used - 1 do begin
      prep.kanji_mark[i] := ConvertMarkers(ed.kanji[i].markers, mark_left);
     //With kanji and kana there's nothing we can do with unrecognized markers
    end;
    for i := 0 to ed.kana_used - 1 do begin
      prep.kana_mark[i] := ConvertMarkers(ed.kana[i].markers, mark_left);
     //With kanji and kana there's nothing we can do with unrecognized markers
    end;
    for i := 0 to ed.meanings_used - 1 do begin
      prep.m_mark[i] := ConvertMarkers(ed.meanings[i].markers, mark_left);
      if mark_left<>'' then //put them back into the text, but to the end of it
        ed.meanings[i].text := ed.meanings[i].text + fstr(' ('+mark_left+')');
    end;

    //Generate romaji
    for i := 0 to ed.kana_used - 1 do begin
      prep.roma[i]:=KanaToRomaji(ed.kana[i].kana,1,dic.language);
      if pos('?',prep.roma[i])>0 then
      begin
       //roma_problems
        roma_prob.WritelnUnicode(prep.roma[i]);
        roma_prob.WritelnUnicode(fstrtouni(ed.kana[i].kana));
        had_problems := true;
      end;
      repl(prep.roma[i],'?','');
      if prep.roma[i]='' then prep.roma[i]:='XXX';
    end;

    //Convert entries
    for i := 0 to ed.meanings_used - 1 do
      repl(ed.meanings[i].text,UH_EDICT_SEMICOL,UH_EDICT_COMMA);

    AddArticle(@prep);
  end; //for every line
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

begin
  prog:=SMProgressDlgCreate(_l('#00071^eDictionary import'),_l('^eImporting...'),100);
  prog.Width := 500; //we're going to have long file names
  prog.Appear;
  wordidx := nil;
  charidx := nil;
  freql := nil;
  had_problems := false;
  linecount:=0;
  LastArticle := 0;
  LastDictEntry := 0;
  try

   //Create indexes
    if ifAddCharacterIndex in flags then
      charidx := TIndexBuilder.Create;
    if ifAddWordIndex in flags then
      wordidx := TWordIndexBuilder.Create;

   //Create frequency list
    if ifAddFrequencyInfo in flags then
    try
      prog.SetMessage(_l('#00917^eCreating frequency chart...'));
      freql := CreateFrequencyList;
    except
      on E: Exception do begin
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
      raise Exception.Create('Cannot load the newly created dictionary.');
    dic.Load;
    if not dic.loaded then
      raise Exception.Create('Cannot load the target dictionary');
    dic.Demand;
    dic.TTDict.NoCommitting := true;
    dic.TTEntries.NoCommitting := true;

   { Convert all dictionaries to UTF16-LE }
    for fi:=0 to Length(files)-1 do
    begin
      fname:=tempDir+'\DICT_'+inttostr(fi)+'.TMP';
      mes:=_l('#00085^eConverting '); //used later too
      prog.SetMessage(mes+ExtractFilename(files[fi])+'...');

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
          on E: Exception do begin
            E.Message := _l('While converting ')+files[fi]+': '+E.Message;
            raise;
          end;
        end;
      end;

      if not FileExists(fname) then
        raise Exception.CreateFmt(_l('File conversion failed (%s)'), [files[fi]]);

     //Count number of lines in the converted file and add to total
      fuin := TUnicodeFileReader.Create(fname);
      while fuin.ReadWideChar(uc) do
        if uc=#$000A then
          Inc(linecount);
      FreeAndNil(fuin);
    end;


   { Import }
    roma_prob := TUnicodeFileWriter.Rewrite('roma_problems.txt'); //TODO: This one should go into UserDir when we have one
    roma_prob.WriteWideChar(#$FEFF); //BOM

    lineno:=0;
    for fi:=0 to Length(files)-1 do
    begin
      mes:=_l('#00086^eReading && parsing ');
      prog.SetMessage(mes+ExtractFilename(files[fi])+'...');

      fuin := TUnicodeFileReader.Create(tempDir+'\DICT_'+inttostr(fi)+'.TMP');
      try
        if not fuin.ReadWideChar(uc) or (uc<>#$FEFF) then
          raise Exception.Create(_l('#00088^eUnsupported file encoding')+' ('+files[fi]+')');

        if diclang='c' then
          ImportCEdict(fuin)
        else
          ImportEdict(fuin);

      finally
        FreeAndNil(fuin);
      end;
    end; //for every file

    FreeAndNil(roma_prob);
    prog.Invalidate;
    prog.Repaint;

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
    dic.Free;

    DeleteDirectory(tempDir); //empty dictionary

    WriteDictPackage(dicFilename, tempDir2, info, diclang, LastArticle);
    DeleteDirectory(tempDir2);

  finally
    FreeAndNil(wordidx);
    FreeAndNil(charidx);
    FreeAndNil(freql);
    FreeAndNil(prog);
  end;

  if had_problems then
    Application.MessageBox('There were some problems during the conversion. '
      +'Please study the roma_problems.txt found in the application directory.',
      'Had problems', MB_ICONEXCLAMATION);

  Result := true;
end;

{
Creates and populates TStringList with frequency information taken from wordfreq_ck
}
function TfDictImport.CreateFrequencyList: TStringList;
const
 //Format markers
  UH_FREQ_COMMENT = {$IFDEF UNICODE}'#'{$ELSE}'0023'{$ENDIF};
  UH_TAB = {$IFDEF UNICODE}#$0009{$ELSE}'0009'{$ENDIF};
var fc: FChar;
  newline,nownum,comment:boolean;
  addkan,addnum:string;
  tp: byte;
begin
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
    raise Exception.Create(_l('#00915^eCannot find WORDFREQ_CK file.'));

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
      if IsLatinDigit(fc) then addnum:=addnum+fstrtouni(fc);
    end else
      addkan:=addkan+fc;
  end;
  Result.Sorted:=true;
  Result.Sort;
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
