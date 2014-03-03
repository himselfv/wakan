unit JWBCharDataImport;
{
Creates character database from sources.
Only full replacement of all relevant properties is supported (impossible to
add translations while keeping existing ones at this point).
Sources:
  KANJIDIC
  Unihan
  radicals.txt (existing TRadicals table can be kept)
}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, UrlLabel, TextTable, JwbForms, JWBCharData;

type
  TFlagList = array of boolean;

  TfCharDataImport = class(TJwbForm)
    Label1: TLabel;
    edtKanjidicFilename: TEdit;
    btnKanjidicBrowse: TButton;
    Label2: TLabel;
    btnUpdate: TButton;
    OpenKanjidicDialog: TOpenDialog;
    Label4: TLabel;
    lblBackupPath: TUrlLabel;
    Label3: TLabel;
    btnCancel: TButton;
    Label7: TLabel;
    edtUnihanFolder: TEdit;
    btnUnihanBrowse: TButton;
    procedure btnKanjidicBrowseClick(Sender: TObject);
    procedure btnUpdateClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btnUnihanBrowseClick(Sender: TObject);

  protected
    procedure ImportKanjidic(const KanjidicFilename: string; out CharsCovered: TFlagList);
    procedure ImportUnihan(const UnihanFolder: string);
    function SortByTChar(TChar: TCharTableV8; TCharProp: TCharPropTableV8): TCharPropTableV8;

  public
    procedure Import;

  end;

implementation
uses FileCtrl, StdPrompt, JWBStrings, KanjidicReader, UnihanReader, JWBCore,
  JWBIO, JWBFileType, FastArray, JWBLanguage;

{$R *.dfm}

procedure TfCharDataImport.FormShow(Sender: TObject);
begin
  lblBackupPath.Caption := BackupDir;
  lblBackupPath.URL := 'file://'+repl(BackupDir,'\','/');
end;

procedure TfCharDataImport.btnKanjidicBrowseClick(Sender: TObject);
begin
  with OpenKanjidicDialog do
    if Execute then
      edtKanjidicFilename.Text := Filename;
end;

procedure TfCharDataImport.btnUnihanBrowseClick(Sender: TObject);
var newDir: string;
begin
  if SelectDirectory(_l('#01090^eSelect UNIHAN directory:'), edtUnihanFolder.Text, newDir) then
    edtUnihanFolder.Text := newDir;
end;

procedure TfCharDataImport.btnUpdateClick(Sender: TObject);
begin
 //Not really needed but let's check beforehand
  if (edtKanjidicFilename.Text<>'') and not FileExists(edtKanjidicFilename.Text) then
    raise Exception.CreateFmt(_l('#01075^eFile %s does not exist!'), [edtKanjidicFilename.Text]);
  if (edtUnihanFolder.Text<>'') and not SysUtils.DirectoryExists(edtUnihanFolder.Text) then
    raise Exception.CreateFmt(_l('#01089^eFolder %s does not exist!'), [edtUnihanFolder.Text]);

  if Application.MessageBox(
    PChar(_l('#01072^Character data in this copy of Wakan will be replaced with '
      +'the new one provided here. Application will then restart and you will '
      +'lose all the unsaved data. Do you want to continue?')),
    PChar(_l('#01071^eConfirm import')),
    MB_YESNO or MB_ICONQUESTION)<>ID_YES then exit;

  try
    try
      Self.Import;
      Application.MessageBox(
        PChar(_l('#01074^eCharacter data has been imported. Application will now terminate.')),
        PChar(_l('#01073^eImport completed')),
        MB_OK or MB_ICONINFORMATION
      );
    except
      on E: Exception do
        Application.MessageBox(
          PChar(_l('#01091^eCannot import character data. Application will now terminate.')+#13
            +E.Message),
          PChar(_l('#00020^eError')),
          MB_OK or MB_ICONERROR
        );
    end;
  finally
   //Terminate anyway because TChar tables has been messed up with.
   //If the import failed or was cancelled, old tables will be loaded next time.
    Application.Terminate;
    Close;
  end;
end;

//Returns a Wakan date stamp which represents file last modification time
function FileAgeStr(const filename: string): string;
var dt: TDatetime;
begin
  if not FileAge(filename, dt) then
    dt := now();
  Result := WakanDatestamp(dt);
end;


type
  TCharPropBuilder = class
  protected
    FTable: TTextTable;
    FChar: FString;
    FCharIdxStr: string;
  public
    constructor Create(ATable: TTextTable);
    destructor Destroy; override;
    procedure OpenKanji(const AChar: FString);
    procedure AddCharPropRaw(TypeId: integer; Value: string; ReadDot: integer;
      Position: integer); overload;
    procedure AddCharProp(propType: PCharPropType; Value: string; ReadDot: integer;
      Position: integer); overload;
    procedure AddCharProp(TypeId: integer; Value: string; ReadDot: integer;
      Position: integer); overload;
    function AddProperties(propType: PCharPropType; field: PFieldEntry;
      pre_idx: integer=0): integer; overload;
    procedure AddProperties(propType: PCharPropType; ed: PKanjidicEntry); overload;
    function AddDefinitions(propType: PCharPropType; ed: PKanjidicEntry;
      pre_idx: integer=0): integer;
    function AddOns(propType: PCharPropType; cla: PReadingClassEntry;
      pre_idx: integer=0): integer;
    function AddKuns(propType: PCharPropType; cla: PReadingClassEntry;
      pre_idx: integer=0): integer;
    procedure CloseKanji;
  end;

constructor TCharPropBuilder.Create(ATable: TTextTable);
begin
  inherited Create();
  FTable := ATable;
  FChar := '';
end;

destructor TCharPropBuilder.Destroy;
begin
  inherited;
end;

procedure TCharPropBuilder.OpenKanji(const AChar: FString);
begin
  Assert(FChar='', 'CharPropEditor: One kanji is already selected for editing');
  FChar := AChar;
  FCharIdxStr := IntToStr(CharIndex(FChar)); //char index as string
end;

//Does not auto-convert Value according to the type preferences
procedure TCharPropBuilder.AddCharPropRaw(TypeId: integer; Value: string; ReadDot: integer;
  Position: integer);
begin
  Assert(FChar<>'', 'CharPropEditor: No kanji selected for editing');
  FTable.AddRecord([
    FCharIdxStr,
    IntToStr(TypeId),
    FString(Value),
    IntToStr(ReadDot),
    IntToStr(Position)
  ]);
end;

//Auto-converts Value to appropriate string storage according to type preferences
procedure TCharPropBuilder.AddCharProp(propType: PCharPropType; Value: string;
  ReadDot: integer; Position: integer);
begin
  Assert(FChar<>'', 'CharPropEditor: No kanji selected for editing');
  AddCharPropRaw(propType.id, Value, ReadDot, Position);
end;

procedure TCharPropBuilder.AddCharProp(TypeId: integer; Value: string;
  ReadDot: integer; Position: integer);
begin
  AddCharProp(FindCharPropType(TypeId), Value, ReadDot, Position);
end;

{ Allows passing nil as a second param in which case adds nothing }
function TCharPropBuilder.AddProperties(propType: PCharPropType; field: PFieldEntry;
  pre_idx: integer=0): integer;
var i: integer;
begin
  Result := 0;
  if field=nil then exit;
  if propType=nil then exit;
  for i := 0 to field.values.Length - 1 do
    AddCharProp(propType, field.values[i], 0, pre_idx+i);
  Result := field.values.Length;
end;

procedure TCharPropBuilder.AddProperties(propType: PCharPropType; ed: PKanjidicEntry);
var i: integer;
  sourceFields: TStringArray;
  field: PFieldEntry;
  pre_idx: integer;
begin
  if propType=nil then exit;
  pre_idx := 0;
  sourceFields := SplitStr(propType.sourceField,'+');
  for i := 0 to Length(sourceFields)-1 do begin
    field := ed.GetField(sourceFields[i]);
    pre_idx := pre_idx + AddProperties(propType, field, pre_idx);
  end;
end;

function TCharPropBuilder.AddDefinitions(propType: PCharPropType; ed: PKanjidicEntry;
  pre_idx: integer=0): integer;
var i: integer;
begin
  if propType=nil then begin
    Result := 0;
    exit;
  end;
  for i := 0 to ed.meanings.Length - 1 do
    AddCharProp(propType, ed.meanings[i], 0, pre_idx+i);
  Result := ed.meanings.Length;
end;

function TCharPropBuilder.AddOns(propType: PCharPropType; cla: PReadingClassEntry;
  pre_idx: integer=0): integer;
var i: integer;
begin
  if propType=nil then begin
    Result := 0;
    exit;
  end;
  for i := 0 to cla.ons.Length - 1 do
    AddCharProp(propType.id, cla.ons[i], 0, pre_idx+i);
  Result := cla.ons.Length;
end;

function TCharPropBuilder.AddKuns(propType: PCharPropType; cla: PReadingClassEntry;
  pre_idx: integer=0): integer;
var i, dot_pos: integer;
  val: string;
begin
  if propType=nil then begin
    Result := 0;
    exit;
  end;
  for i := 0 to cla.kuns.Length - 1 do begin
    val := cla.kuns[i];
    dot_pos := pos(val, '.');
    if dot_pos>0 then
      delete(val, dot_pos, 1);
    AddCharProp(propType.id, val, dot_pos, pre_idx+i);
  end;
  Result := cla.kuns.Length;
end;

procedure TCharPropBuilder.CloseKanji;
begin
  Assert(FChar<>'', 'CharPropEditor: No kanji selected for editing');
  FChar := '';
end;

{ Imports all supported data from KANJIDIC/UNIHAN into wakan.chr.
 If some of TChar/TCharProp/TRadicals tables are present, copies data from those. }
procedure TfCharDataImport.Import;
var prog: TSMPromptForm;
  tempDir: string;
  backupFile: string;
  i: integer;

 { If we're updating DB, we need to port TChar and property entries which were
  not covered by the KANJIDIC/UNIHAN provided. So we keep track of it. }
  KanjidicCovered: TFlagList;
 { Unihan is handled differently, it's either present or not. It can't cover
  less than it covered on previous import. }

  procedure ImportRadicals(const AFilename: string);
  var re: TStreamDecoder;
  begin
    re := UnicodeFileReader(AFilename);
    try
      re.TrySkipBom;
      TRadicals.ImportFromText(re);
    finally
      FreeAndNil(re);
    end;
  end;

begin
  prog:=SMProgressDlgCreate(
    _l('#01076^eCharacter data import'),
    _l('#01077^eImporting...'),
    0,{CanCancel=}true);
  try
    if not self.Visible then //auto mode
      prog.Position := poScreenCenter;
    prog.Show;
    prog.Update;

   { STAGE I. Clear/reset everything }
    prog.SetMessage(_l('^eInitializing...'));

   //Free existing tables
    FreeAndNil(TChar);
    FreeAndNil(TCharProp);
   { Do not free radicals because most of the time, people importing from GUI
    would not have radicals.txt }
   // FreeAndNil(TRadicals);

    ClearCharDbProps();

   //Create new tables
    TCharProp := NewCharPropTable();
    TCharProp.NoCommitting := true;
    TChar := NewCharTable();

   //Create and import TRadicals table if we don't have one
    if TRadicals=nil then begin
      TRadicals := NewRadicalsTable();
      prog.SetMessage(_l('^eImporting radicals...'));
      ImportRadicals(AppFolder+'\radicals.txt');
    end;

   {$IFDEF DEBUG}
    Assert(TChar.CheckIndex,'TChar index broken before any import (?!)');
   {$ENDIF}

   { STAGE II. Import KANJIDIC }
    prog.SetMessage(_l('^eImporting KANJIDIC...'));
    if edtKanjidicFilename.Text<>'' then
      ImportKanjidic(edtKanjidicFilename.Text, KanjidicCovered)
    else
      SetLength(KanjidicCovered, 0);

   {$IFDEF DEBUG}
    for i := 0 to Min(TChar.Seeks.Count-1,TChar.Orders.Count)-1 do
      Assert(TChar.CheckIndex(i),'TChar index '+TChar.Orders[i]+' broken after importing KANJIDIC');
   {$ENDIF}

   { STAGE III. Import UNIHAN }
    prog.SetMessage(_l('^eImporting UNIHAN...'));
    if edtUnihanFolder.Text<>'' then
      ImportUnihan(edtUnihanFolder.Text);

   {$IFDEF DEBUG}
    for i := 0 to Min(TChar.Seeks.Count-1,TChar.Orders.Count)-1 do
      Assert(TChar.CheckIndex(i),'TChar index '+TChar.Orders[i]+' broken after importing UNIHAN');
   {$ENDIF}

   { STAGE V. Sort, reindex and finalize }
    prog.SetMessage(_l('#01078^eReindexing...'));

   //Reindex
    TCharProp.NoCommitting := false;
    TCharProp.Reindex;

    prog.SetMessage(_l('Sorting...'));

   //Re-arrange TChar-wise
   //This is not required but may improve speed. I will decide later if this
   //is of any help.
    TCharProp := SortByTChar(TChar, TCharProp);

   { Update CharDataProps. See comments there about the relevance of fields }
    CharDataVersion := CurrentCharDataVersion;
    CharDataProps.DicBuildDate := Trunc(now); //todays' date
    if edtKanjidicFilename.Text<>'' then
      CharDataProps.KanjidicVersion := FileAgeStr(edtKanjidicFilename.Text);
    if edtUnihanFolder.Text<>'' then begin
      CharDataProps.UnihanVersion := FileAgeStr(edtUnihanFolder.Text+'\Unihan_Readings.txt');
      CharDataProps.ChinesePresent := true;
    end;

   //Save
    if FileExists(AppFolder+'\wakan.chr') then begin
      backupFile := Backup(AppFolder+'\wakan.chr');
      if backupFile='' then
        raise Exception.Create('Cannot backup WAKAN.CHR, will not continue');
    end else
      backupFile := ''; //the only case we continue without backup

    tempDir := CreateRandomTempDir();
    SaveCharData(tempDir+'\wakan.chr');
    if (backupFile<>'') and not DeleteFile(AppFolder+'\wakan.chr') then
      raise Exception.Create('Cannot replace current wakan.chr. Maybe the file '
        +'is in use or you do not have permissions to delete it.'#13
        +'Make sure only one copy of Wakan is running and that you have '
        +'permissions to delete files in the folder where character data is '
        +'stored.');
    if not MoveFile(PChar(tempDir+'\wakan.chr'), PChar(AppFolder+'\wakan.chr')) then begin
      if backupFile<>'' then
        CopyFile(PChar(backupFile), PChar(AppFolder+'\wakan.chr'), true);
      raise Exception.Create('Cannot move newly created wakan.chr. Old wakan.chr restored.');
    end;
    DeleteDirectory(tempDir);

  finally
    FreeAndNil(prog);
  end;
end;

{ Imports all possible data from KANJIDIC into TChar/TCharProp }
procedure TfCharDataImport.ImportKanjidic(const KanjidicFilename: string;
  out CharsCovered: TFlagList);
var CChar: TTextTableCursor;
  CNewProp: TCharPropBuilder;
  fuin: TStreamDecoder;
  conv_type: CEncoding;
  ed: TKanjidicEntry;
  line: string;
  OldCharCount: integer;
  propType: PCharPropType;
  pre_idx: integer;
  i: integer;
  JpStrokeCount: integer;
  JpFrequency: integer;
  JouyouGrade: integer;

  function Ed_GetIntField(const key: string; out value: integer): boolean;
  var field: PFieldEntry;
  begin
    field := ed.GetField(key);
    if (field=nil) or (field.values.Length <1) then begin
      Result := false;
      exit;
    end;
   //There can be several entries (i.e. for stroke count less popular variants come
   //after the main one), but we only take the first one.
    value := StrToInt(field.values[0]); //crash and burn if not int
    Result := true;
  end;

begin
 { Preallocate CharsCovered and mark all existing chars as not yet covered }
  SetLength(CharsCovered, TChar.RecordCount);
  for i := 0 to Length(CharsCovered) - 1 do
    CharsCovered[i] := false;

 { Parse KANJIDIC and add new properties }
  CChar := nil;
  CNewProp := nil;
  fuin := nil;
  try
    CChar := TChar.NewCursor;
    CNewProp := TCharPropBuilder.Create(TCharProp);

    ed.Reset;
    conv_type := Conv_DetectType(KanjidicFilename);
    if conv_type=nil then
      conv_type := Conv_ChooseType({chinese=}false,TEUCEncoding); //kanjidic is by default EUC
    if conv_type=nil then
      raise EAbort.Create('');
    fuin := OpenTextFile(KanjidicFilename, conv_type);
    while not fuin.EOF do begin
      line := fuin.ReadLn;
      if line='' then continue;
      if IsKanjidicComment(line) then continue;
      ParseKanjidicLine(line, @ed);

     //Find kanji
      if not CChar.Locate('Unicode', ed.kanji) then begin
        if not ed_GetIntField('S', JpStrokeCount) then
          JpStrokeCount := 255;
        if not ed_GetIntField('F', JpFrequency) then
          JpFrequency := 65535;
        if not ed_GetIntField('G', JouyouGrade) then
          JouyouGrade := 255;
        CChar.Insert([
         ed.kanji, //Unicode,
         '0', //Not chinese-only
         'J', //See comments for TCharType. For now, 'J'. Unihan parsing might alter this.
         IntToStr(JpStrokeCount),
         IntToStr(JpFrequency),
         '255', //ChStrokeCount
         '255', //ChFrequence
         IntToStr(JouyouGrade)
        ]);
      end else
       { If there are duplicate records in KANJIDIC we may get CChar.tcur for a
        newly-added record, outside of CharsCovered.
        New records have no properies to migrate so we need not to extend
        CharsCovered to them }
        if CChar.tcur < Length(CharsCovered) then
          CharsCovered[CChar.tcur] := true; //mark as migrated

      CNewProp.OpenKanji(ed.kanji);

     //Add standard properties
     { AddProperties allows passing Nil as a second param, so we don't check
      if the optional fields are present }
      CNewProp.AddProperties(FindCharPropType(ptKoreanReading), ed.GetField('W'));
      CNewProp.AddProperties(FindCharPropType(ptMandarinReading), ed.GetField('Y'));
      CNewProp.AddDefinitions(FindCharPropType(ptJapaneseDefinition), @ed);
      CNewProp.AddOns(FindCharPropType(ptOnReading), @ed.readings[0]);
      CNewProp.AddKuns(FindCharPropType(ptKunReading), @ed.readings[0]);
      pre_idx := CNewProp.AddOns(FindCharPropType(ptNanoriReading), @ed.readings[1]);
      CNewProp.AddKuns(FindCharPropType(ptNanoriReading), @ed.readings[1], pre_idx);

     //Add automated properties
      for i := 0 to Length(CharPropTypes) - 1 do begin
        propType := @CharPropTypes[i];
        if propType.id<=10 then continue; //not automated
        if propType.id=ptJapaneseDefinition then continue; //not automated either
        if propType.sourceType<>'D' then continue; //not from kanjidic
        CNewProp.AddProperties(propType, PKanjidicEntry(@ed));
      end;

      CNewProp.CloseKanji;
    end;

  finally
    FreeAndNil(fuin);
    FreeAndNil(CNewProp);
    FreeAndNil(CChar);
  end;

 { Expand CharsCovered to include newly added chars which WERE covered }
  OldCharCount := Length(CharsCovered);
  SetLength(CharsCovered,TChar.RecordCount);
  for i := OldCharCount to Length(CharsCovered) - 1 do
    CharsCovered[i] := true;
end;

procedure TfCharDataImport.ImportUnihan(const UnihanFolder: string);
var CChar: TTextTableCursor;
  CNewProp: TCharPropBuilder;
  parts: TStringArray;
  soUnicode: TSeekObject;
  LastChar: UnicodeString;

  procedure NeedChar(const kanji: UnicodeString);
  begin
   //Simple optimization: If it's the same char as before, nothing needs to be done!
   // (Unihan really stores chars mostly in batches, not between diff. files though)
    if (LastChar<>'') and (LastChar=kanji) then
      exit;

    if CChar.Locate(@soUnicode, kanji) then
      exit;

    CChar.Insert([
     kanji, //Unicode,
     '1', //Chinese-only
     'N', //See comments for TCharType
     '255', //JpStroke count
     '65535', //JpFrequency
     '255', //ChStroke count,
     '255', //ChFrequency
     '255' //Jouyou grade
    ]);
  end;

  procedure ImportFile(const UnihanFile: string);
  var fuin: TStreamDecoder;
    line: string;
    ed: TUnihanPropertyEntry;
    propType: PCharPropType;
    i: integer;
    entry_no: integer;
    varpropvals: TVariantValues;
  begin
    fuin := OpenTextFile(UnihanFile, TUTF8Encoding);
    try
      while not fuin.Eof do begin
        line := Trim(fuin.ReadLn);
        if (line='') or IsUnihanComment(line) then continue;
        ParseUnihanLine(line, @ed);

        if Length(ed.char)>1 then begin
          line := '';
          continue; //multibyte chars not supported
        end;

        propType := FindCharPropType('U', ed.propType);
        if ed.propType='kFrequency' then begin
          NeedChar(ed.char);
          CChar.Edit([TChar.fChFrequency],[IntToStr(StrToInt(ed.value))]);
        end else
        if ed.propType='kTotalStrokes' then begin
          NeedChar(ed.char);
          parts := SplitStr(ed.value,' '); //there could be several: zh-Hans (CN) and zh-Hant (TW)
          if Length(parts)>1 then //take Chinese one
            CChar.Edit([TChar.fChStrokeCount],[IntToStr(StrToInt(parts[0]))]);
        end else
        if (ed.propType='kCantonese') or (ed.propType='kMandarin')
        or (ed.propType='kDefinition') or (ed.propType='kKorean') then begin
         //Split + add
          Assert(propType<>nil); //should still have proptype
          if ed.propType='kDefinition' then
           { Definitions are stored in a somewhat strange format of:
                defAA; defAB; defAC, defBA; defBB, defC
             Note that ; separates close meanings and , separates loosely related ones. }
            parts := SplitStr(ed.value,',')
          else
            parts := SplitStr(ed.value,' '); //cantonese, mandarin and korean are space-separated
          NeedChar(ed.char);
          CNewProp.OpenKanji(ed.char);
          entry_no:=0; //we don't use 'i' because some parts[i] can be empty: Split('a  b') => ('a', '', 'b')
          for i := 0 to Length(parts) - 1 do begin
            parts[i] := Trim(parts[i]);
            if parts[i]='' then continue;
            CNewProp.AddCharProp(propType, parts[i], 0, entry_no);
            Inc(entry_no);
          end;
          CNewProp.CloseKanji;
        end else
        if propType<>nil then begin
         //Normal addition

         //Variant properties need reparsing
          if (propType.sourceField='kCompatibilityVariant')
          or (propType.sourceField='kSemanticVariant')
          or (propType.sourceField='kSimplifiedVariant')
          or (propType.sourceField='kTraditionalVariant')
          or (propType.sourceField='kZVariant') then begin
            ParseVariantProperty(ed.value, varpropvals);
            ed.value := '';
            for i := 0 to Length(varpropvals)-1 do
              ed.value := ed.value + varpropvals[i].char;
          end else

         //These have special handling but also need normal handling
          if ed.propType='kBigFive' then begin
            NeedChar(ed.char);
            if CChar.Str(TChar.fType)='N' then
              CChar.Edit([TChar.fType],['T'])
            else;
              CChar.Edit([TChar.fType],['A']);
          end else
          if ed.propType='kGB0' then begin
            NeedChar(ed.char);
            if CChar.Str(TChar.fType)='N' then
              CChar.Edit([TChar.fType],['S'])
            else;
              CChar.Edit([TChar.fType],['A']);
          end;

          NeedChar(ed.char);
          CNewProp.OpenKanji(ed.char);
          CNewProp.AddCharProp(propType, ed.value, 0, 0);
          CNewProp.CloseKanji;
        end;

      end;
    finally
      FreeAndNil(fuin);
    end;
  end;

begin
  soUnicode := TChar.GetSeekObject('Unicode');
  LastChar := '';

  CChar := nil;
  CNewProp := nil;
  try
    CChar := TChar.NewCursor;
    CNewProp := TCharPropBuilder.Create(TCharProp);

    ImportFile(UnihanFolder+'\Unihan_RadicalStrokeCounts.txt');
    ImportFile(UnihanFolder+'\Unihan_Readings.txt');
    ImportFile(UnihanFolder+'\Unihan_DictionaryLikeData.txt');
    ImportFile(UnihanFolder+'\Unihan_Variants.txt');
    ImportFile(UnihanFolder+'\Unihan_OtherMappings.txt');
    ImportFile(UnihanFolder+'\Unihan_DictionaryIndices.txt');
   //A more radical solution would be to import every .txt file in a folder...
   //But that's risky. Who knows what else is there.

  finally
    FreeAndNil(CNewProp);
    FreeAndNil(CChar);
  end;
end;

{ Rebuilds TCharProp table so that records are stored in a specific order --
 Kanji+Type+Index -- see NewCharPropTable() comments.
 Returns new table (old one is freed) }
function TfCharDataImport.SortByTChar(TChar: TCharTableV8; TCharProp: TCharPropTableV8): TCharPropTableV8;
var CChar: TTextTableCursor;
  CCharProp: TCharPropertyCursor;
  Builder: TCharPropBuilder;
  ch: FString;
begin
  Result := NewCharPropTable();
  Result.NoCommitting := true;

  CChar := nil;
  CCharProp := nil;
  Builder := nil;
  try
    CChar := TTextTableCursor.Create(TChar);
    CCharProp := TCharPropertyCursor.Create(TCharProp);
    Builder := TCharPropBuilder.Create(Result);

    CCharProp.SetOrder('Kanji');
    CChar.First;
    while not CChar.EOF do begin
      ch := CChar.Str(TChar.fUnicode);
      Assert(
        CCharProp.Locate(ch),
        'Cannot locate any properties for character '+ch
      );
      Builder.OpenKanji(ch);
      while CCharProp.Kanji=ch do begin
        Builder.AddCharPropRaw(
         CCharProp.TrueInt(TCharProp.fTypeId),
         CCharProp.Str(TCharProp.fValue),
         CCharProp.Int(TCharProp.fReadDot),
         CCharProp.Int(TCharProp.fPosition)
        );

        CCharProp.Next;
      end;
      Builder.CloseKanji;

      CChar.Next;
    end;

  finally
    FreeAndNil(Builder);
    FreeAndNil(CCharProp);
    FreeAndNil(CChar);
  end;

  FreeAndNil(TCharProp);
  Result.NoCommitting := false;
  Result.Reindex;
end;

end.
