unit JWBDictImport;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, ExtCtrls, StdPrompt;

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

  protected
    prog: TSMPromptForm;
    function CreateFrequencyList: TStringList;

  public
    function ImportDictionary(dicFilename: string; info: TDictInfo;
      files: TFileList; diclang:char; flags: TImportDictFlags): boolean;

  end;

 {
  Dictionary index builder. Instances are not reusable.
  1. Create.
  2. AddToIndex(Key, Article)
  3. Pack.
  4. Write and Destroy
 }
  TIndexBuilder = class
  protected
    sl_names: TStringList;
    sl_values: TStringList;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddToIndex(key: string; article: integer);
    procedure Pack;
    procedure Write(filename: string);

  end;

var
  fDictImport: TfDictImport;

implementation

uses StrUtils, JWBDictCoding, JWBUnit, JWBMenu, PKGWrite, JWBConvert,
  JWBStrings, JWBIO, JWBDic, JWBDicSearch, JWBEdictMarkers;

{$R *.DFM}

{
Currently we keep data in two StringLists, one for names, another for article
indices for a given name (8 bytes hex == integer each).
}

constructor TIndexBuilder.Create;
begin
  inherited;
  sl_names := TStringList.Create;
  sl_values := TStringList.Create;
  sl_names.Sorted:=true;
end;

destructor TIndexBuilder.Destroy;
begin
  FreeAndNil(sl_values);
  FreeAndNil(sl_names);
  inherited;
end;

{ Key: Index key.
Article: Article index to link to this key. }
procedure TIndexBuilder.AddToIndex(key: string; article: integer);
var i:integer;
  s:string;
  ind:string;
begin
 //Currently this only supports indexing by first 4 chars of the key (it's fine,
 //since indexes are not ultimate by definition)
  if length(key)>4 then SetLength(key, 4);

 //Article IDs are stored as 8 byte hex blocks one after another
  ind:=ByteToHex(@article, sizeof(article));

  i:=sl_names.IndexOf(key);
  if i<>-1 then s:=sl_values[i] else s:='';
  if copy(s,length(s)-7,8)<>ind then
  begin
    s:=s+ind;
    if i=-1 then
    begin
      i:=sl_names.Add(key);
      sl_values.Insert(i,s);
    end else
      sl_values[i]:=s;
  end;
end;

function CustomSortCompare(list:TStringList;index1,index2:integer):integer;
begin
  result:=CompareStr(uppercase(list[index1]),uppercase(list[index2]));
end;

{ Call to finalize index building and pack/sort the data. }
procedure TIndexBuilder.Pack;
var i: integer;
begin
  sl_names.Sorted:=false;
  for i:=0 to sl_names.Count-1 do while length(sl_names[i])<4 do sl_names[i]:=sl_names[i]+' ';
  for i:=0 to sl_names.Count-1 do sl_names[i]:=sl_names[i]+'    '+sl_values[i];
  sl_values.Clear(); //free memory
  sl_names.CustomSort(CustomSortCompare);
end;

procedure TIndexBuilder.Write(filename: string);
var
  fb: file;
  buf: array[0..3999] of byte;
  bufp: integer;
  i, j: integer;

  //TODO: Rewrite both these functions and perhaps change the method TIndexBuilder uses to keep sl_names at all
  procedure PutToBuf(b1,b2,b3,b4:byte);
  begin
    buf[bufp]:=b1;
    buf[bufp+1]:=b2;
    buf[bufp+2]:=b3;
    buf[bufp+3]:=b4;
    inc(bufp,4);
    if bufp=4000 then begin
      blockwrite(fb,buf,4000);
      bufp:=0;
    end;
  end;

  procedure PutToBufL(l:integer);
  var b:array[0..3] of byte;
  begin
    move(l,b,4);
    PutToBuf(b[0],b[1],b[2],b[3]);
  end;

begin
  assignfile(fb,filename);
  bufp:=0;
  rewrite(fb,1);
  PutToBufL(sl_names.Count);
  j:=0;
  for i:=0 to sl_names.Count-1 do
  begin
    PutToBuf(ord(sl_names[i][1]),ord(sl_names[i][2]),ord(sl_names[i][3]),ord(sl_names[i][4]));
    PutToBufL(j);
    inc(j,(length(sl_names[i]) div 8)-1);
  end;
  for i:=0 to sl_names.Count-1 do
    for j:=1 to (length(sl_names[i]) div 8)-1 do
    begin
      PutToBuf(strtoint('0x'+copy(sl_names[i],j*8+1,2)),strtoint('0x'+copy(sl_names[i],j*8+3,2)),
               strtoint('0x'+copy(sl_names[i],j*8+5,2)),strtoint('0x'+copy(sl_names[i],j*8+7,2)));
    end;
  blockwrite(fb,buf,bufp);
  closefile(fb);
end;


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
  writeln(t,'iIndex');
  writeln(t,'xPhonetic');
  writeln(t,'xKanji');
  writeln(t,'sSort');
  writeln(t,'iFrequency');
  writeln(t,'iArticle');
  writeln(t,'$ORDERS');
  writeln(t,'Phonetic_Ind');
  writeln(t,'Kanji_Ind');
  writeln(t,'<Phonetic_Ind');
  writeln(t,'<Kanji_Ind');
  writeln(t,'Article');
  writeln(t,'$SEEKS');
  writeln(t,'Index');
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

{
ImportDictionary()
Builds Wakan package from one or more dictionaries.
Returns true if the package was successfully built, false if aborted.
}
function TfDictImport.ImportDictionary(dicFilename: string; info: TDictInfo;
  files: TFileList; diclang:char; flags: TImportDictFlags): boolean;
const
 //Format markers
  UH_EDICT_COMMENT = {$IFDEF UNICODE}'#'{$ELSE}'0023'{$ENDIF};
var fi:integer;
    fname:string;
    s:string;
    fin:TFileReader;
    fout:TFileWriter;
    fuin: TUnicodeFileReader;
    fb:file;
    cd:string;
    buf:array[0..3999] of byte;
    abuf:array[0..1999+1] of AnsiChar; //2000 + one ansichar for #0000
    bufc:integer;
    bufp:integer;
    buff:file;
    cnt:integer;
    kanji:FString;
    phon:FString;
    writ:AnsiString;
    feof:boolean;
    lreat:boolean;
    asc:string;
    ppp:integer;
    mes:string;
    s2,s3:string;
    s_roma:string; //romaji
    s_entry:string; //edict entry (converted)
    s_mark:string; //edict markers returned by ConvertEdictEntry()
    bl,bh:byte;
    beg:boolean;
    cnt2:integer;
    dic:TJaletDic;
    romap:textfile;
    pphon:string;
    prior:integer;
    skk,skk2:string;
    skki,skkj:integer;
    wordidx,charidx:TIndexBuilder;
    linecount,lineno:integer;
    i,j:integer;
    freql:TStringList;
    freqf:file;
    freqi:integer;

    tempDir: string;
    fc: FChar;
    uc: WideChar;
    ac: AnsiChar;

    { Converts block to ansi. Len is the length of buf in Wide characters.
     abuf must be the same length in Ansi characters. }
    procedure blockAnsi(buf: PWideChar; abuf: PAnsiChar; len: integer);
    const defaultChar: AnsiChar = ' ';
    var usedDefaultChar: longbool;
    begin
      if (WideCharToMultiByte(CP_ACP, 0, buf, len, abuf, len, @defaultChar, @usedDefaultChar) <> len) then
       //we need the translated characters to be exactly at the same places
        raise Exception.Create('Not a direct 2->1 translation when converting UTF-16 to Ansi (wtf?)');
    end;

begin
  Result := false;
  prog:=SMProgressDlgCreate(_l('#00071^eDictionary import'),_l('^eImporting...'),100);
  prog.Width := 500; //we're going to have long file names
  prog.Appear;
  wordidx := TIndexBuilder.Create;
  charidx := TIndexBuilder.Create;
  freql := nil;
  linecount:=0;
  try

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

    tempDir := CreateRandomTempDirName();
    ForceDirectories(tempDir);

    CreateDictTables(tempDir+'\DICT.TMP', info, diclang, cnt);
    dic := fMenu.NewDict(tempDir+'\DICT.TMP');
    if not dic.tested then
      raise Exception.Create('Cannot load the newly created dictionary.');
    dic.Load;
    if not dic.loaded then
      raise Exception.Create('Cannot load the target dictionary');
    dic.Demand;

    assignfile(romap,'roma_problems.txt'); //TODO: This one should go into UserDir when we have one
    rewrite(romap);

   { Phase 0 }
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

   { Phase 1 }
    lineno:=0;
    cnt:=0;
   //On Ansi we're writing and reading from Ansi file, on Unicode from Unicode
    for fi:=0 to Length(files)-1 do
    begin
      fname:=tempDir+'\DICT_'+inttostr(fi)+'.TMP';
      mes:=_l('#00086^eReading && parsing ');
      prog.SetMessage(mes+ExtractFilename(files[fi])+'...');

      assignfile(buff,fname);
      reset(buff,1);
      bufc:=0;
      bufp:=0;
      cnt2:=0;
      blockread(buff,buf,2);
      if (buf[0]<>255) or (buf[1]<>254) then
        raise Exception.Create(_l('#00088^eUnsupported file encoding')+' ('+files[fi]+')');

      //Read another line
      feof:=false;
      while (not feof) or (bufp<bufc) do
      begin
        lreat:=false;
        kanji:='';
        phon:='';
        writ:='';
        ppp:=0;
        while not lreat do
        begin
          if (bufp>=bufc) and (not feof) then
          begin
            blockread(buff,buf,4000,bufc);
            if bufc<4000 then feof:=true;
            bufp:=0;
            blockansi(PWideChar(@buf[0]),@abuf[0],2000);
          end;
          if bufp>=bufc then lreat:=true else
          begin
            uc:=PWideChar(@buf[bufp])^;
            ac := abuf[bufp div 2];
            inc(bufp,2);
//            if Ord(uc)>255 then ac:=' ' else ac:=AnsiChar(uc);
            if uc=#$000A then lreat:=true;
            if (uc<>#$000A) and (uc<>#$000D) then
              case ppp of
                0: if uc=#$0020 then ppp:=1 else kanji:=kanji+fstr(uc);
                1: if uc=#$005B then ppp:=2 else ppp:=3;
                2: if uc=#$002F then ppp:=3 else
                     if (uc<>#$005D) and (uc<>#$0020) then phon:=phon+fstr(uc);
              else
                writ:=writ+ac;
              end;
          end;
        end;
        if (length(writ)>0) and (writ[length(writ)]='/') then
          delete(writ,length(writ),1);
        if phon='' then phon:=kanji;

        if (linecount>0) and (lineno mod 100=0) then
          prog.SetProgress(round(lineno/linecount*100));
        inc(lineno);

        inc(cnt);
        if (pos({$IFDEF UNICODE}#$FF1F#$FF1F{$ELSE}'FF1FFF1F'{$ENDIF},kanji)<>0)  //EDICT header line
        or (pos(UH_EDICT_COMMENT,kanji)<>0) then
          continue;

        //Generate romaji
        pphon:=phon;
        if diclang='c'then
        begin
          repl(phon,' ','');
          if phon<>kanji then phon:=RomajiToKana(fstrtouni(phon),1,false,diclang);
        end;
        s_roma:=KanaToRomaji(phon,1,diclang);
        if pos('?',s_roma)>0 then
        begin
         //roma_problems
          writeln(romap,writ);
          writeln(romap,s_roma);
          writeln(romap,string(fstrtouni(pphon))); //Data loss! But whatever.
        end;
        repl(s_roma,'?','');
        if s_roma='' then s_roma:='XXX';

        s_entry:=ConvertEdictEntry(string(writ),s_mark);
        repl(s_entry,';',',');

       //Priority
        prior:=0;
        if ifAddFrequencyInfo in flags then
        begin
          freqi:=freql.IndexOf(kanji);
          if freqi<>-1 then prior:=integer(freql.Objects[freqi]);
        end;

       //Write out
        if s_roma<>'' then begin
          dic.TTDict.Insert([inttostr(cnt), phon, kanji, s_roma, inttostr(prior)]);
          dic.TTEntries.Insert([inttostr(cnt), s_entry, s_mark]);
        end;

       //Indexes
        s:=kanji;
        if ifAddCharacterIndex in flags then
          while s<>'' do
          begin
            fc:=fgetch(s,1);
            fdelete(s,1,1);
            if TChar.Locate('Unicode',fc) then
              charidx.AddToIndex(fstrtohex(fc), cnt); //char index has to have fc in FHex -- that's the format
          end; //of AddCharacterIndex while clause

        s:=string(writ);
        if ifAddWordIndex in flags then
          while s<>'' do
          begin
           //Pop next translation alternative
            s2:=lowercase(strqpop(s,'/'));
            if s2='' then continue;
            while s2<>'' do
            begin
             //Remove all the leading (flags) (and) (markers)
              while (length(s2)>0) and (s2[1]='(') do
              begin
                strqpop(s2,')');
                s2 := TrimLeft(s2);
              end;
             //Pop next word until space
              s3:=strqpop(s2,' ');
              repl(s2,';',',');
              if (ignorel.IndexOf(s3)=-1) and (s3<>'')
              and (s3[1]<>' ') and (s3[1]<>'"') and (s3[1]<>'-')
              and (s3[1]<>'.') and ((s3[1]<'0') or (s3[1]>'9')) then
                wordidx.AddToIndex(s3, cnt);
            end;
          end; //of AddWordIndex while clause

      end; //for every character in the file

      closefile(buff);
    end; //for every file
    fout.Writeln('.');
    FreeAndNil(fout);

    closefile(romap);
    prog.Invalidate;
    prog.Repaint;


    DeleteDirectory(tempDir);


   //This time it's for our package
    tempDir := CreateRandomTempDirName();
    ForceDirectories(tempDir);

    prog.SetMessage(_l('#00092^eSorting indexes...'));
    charidx.Pack;
    wordidx.Pack;

    prog.SetMessage(_l('^eWriting character index...'));
    charidx.Write(tempDir+'\CharIdx.bin');

    prog.SetMessage(_l('^eWriting word index...'));
    wordidx.Write(tempDir+'\WordIdx.bin');

    prog.SetMessage(_l('^eWriting dictionary table...'));
    dic.TTDict.WriteTable(tempDir+'\Dict',true);
    dic.TTEntries.WriteTable(tempDir+'\Entries',true);
    dic.Free;

    WriteDictPackage(dicFilename, tempDir, info, diclang, cnt);
    DeleteDirectory(tempDir);

  finally
    wordidx.Free;
    charidx.Free;
    freql.Free;
    prog.Free;
  end;

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
