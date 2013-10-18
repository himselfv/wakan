unit JWBCharDataImport;
{
Updates or re-imports character database from sources.
Currently only KANJIDIC is supported (and as a consequence no full reimport is
possible).
Only full replacement of all relevant properties is supported (impossible to
add translations while keeping existing ones at this point).
}

//TODO: Localize some messages

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, UrlLabel, TextTable, JwbForms;

type
  TFlagList = array of boolean;

{ Updates or recreates from the scratch WAKAN.CHR.
 If any of KanjidicFilename, UnihanFolder is not specified, that part is skipped.
 If ResetDb is set, existing information is discarded, otherwise preserved where
 no new info is available. }

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
    cbResetDb: TCheckBox;
    Label5: TLabel;
    Label6: TLabel;
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
    procedure CopyProperties(OldCharProp: TTextTable; const KanjidicCovered: TFlagList;
      const UnihanPresent: boolean);
    function SortByTChar(TChar: TTextTable; TCharProp: TTextTable): TTextTable;

  public
    procedure Import;

  end;

implementation
uses FileCtrl, StdPrompt, JWBStrings, JWBCharData, JWBKanjidicReader, JWBUnihanReader,
  JWBUnit, JWBIO;

{$R *.dfm}

procedure TfCharDataImport.FormShow(Sender: TObject);
begin
  lblBackupPath.Caption := BackupDir;
  lblBackupPath.URL := 'file://'+replc(BackupDir,'\','/');
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

  if cbResetDb.Checked then begin
    FreeAndNil(TChar);
    FreeAndNil(TCharProp);
   { Do not free radicals because most of the time, people importing from GUI
    would not have radicals.txt }
   // FreeAndNil(TRadicals);
    ClearCharDbProps();
  end;

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

{
Unfortunately, the CharProp table is made in such a way that records for the
same kanji must go in one batch:
  Kanji=5, Prop=7
  Kanji=6, Prop=1
  Kanji=5, Prop=8  <--- BAD, this property will be ignored
This order is not even controlled by any field; it's the internal record order.
In other words, you can not add properties, but only rebuild table from scratch.

There's also the Index field which is supposed to have unique values, but it's
not used anywhere in Wakan (there's no Order or Seek for this field).

Wakan 1.80 enhances the format but remains compatible for now. We promote
'Kanji' Seek (which already exists) to an Order by shifting it to a second
position (by adding seek '0'):
  $ORDERS
  Kanji
  [others]
  $SEEKS
  '0'
  Kanji [already present]
All code which previously did SetOrder(''); Seek('Kanji',kanji) now uses
SetOrder('kanji').

This is a non-breaking change and we are still rebuilding table from the scratch
and even filling Index field even though nobody (even Wakan 1.67) uses it.
We're just being a bit more future proof.
}

type
  TCharPropBuilder = class
  protected
    FTable: TTextTable;
//    FRecIndex: integer;
    FCharIdx: integer;
  public
    constructor Create(ATable: TTextTable);
    destructor Destroy; override;
    procedure OpenKanji(ACharIdx: integer);
    procedure AddCharPropRaw(TypeId: integer; Value: AnsiString; ReadDot: integer;
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
  FCharIdx := -1;
///  FRecIndex := 0;
end;

destructor TCharPropBuilder.Destroy;
begin
  inherited;
end;

procedure TCharPropBuilder.OpenKanji(ACharIdx: integer);
begin
  Assert(FCharIdx<0, 'CharPropEditor: One kanji is already selected for editing');
  FCharIdx := ACharIdx;
end;

//Does not auto-convert Value according to the type preferences
procedure TCharPropBuilder.AddCharPropRaw(TypeId: integer; Value: AnsiString; ReadDot: integer;
  Position: integer);
begin
  Assert(FCharIdx>=0, 'CharPropEditor: No kanji selected for editing');
//  Inc(FRecIndex);
  FTable.AddRecord(['0', IntToStr(FCharIdx), IntToStr(TypeId),
    string(Value), IntToStr(ReadDot), IntToStr(Position)]);
end;

//Auto-converts Value to appropriate string storage according to type preferences
procedure TCharPropBuilder.AddCharProp(propType: PCharPropType; Value: string;
  ReadDot: integer; Position: integer);
begin
  Assert(FCharIdx>=0, 'CharPropEditor: No kanji selected for editing');
  if (propType.dataType='U') then
    Value := UnicodeToHex(UnicodeString(Value));
  AddCharPropRaw(propType.id, AnsiString(Value), ReadDot, Position);
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
  for i := 0 to field.values_used - 1 do
    AddCharProp(propType, field.values[i], 0, pre_idx+i);
  Result := field.values_used;
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
  for i := 0 to ed.meanings_used - 1 do
    AddCharProp(propType, ed.meanings[i], 0, pre_idx+i);
  Result := ed.meanings_used;
end;

function TCharPropBuilder.AddOns(propType: PCharPropType; cla: PReadingClassEntry;
  pre_idx: integer=0): integer;
var i: integer;
begin
  if propType=nil then begin
    Result := 0;
    exit;
  end;
  for i := 0 to cla.ons_used - 1 do
    AddCharProp(propType.id, cla.ons[i], 0, pre_idx+i);
  Result := cla.ons_used;
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
  for i := 0 to cla.kuns_used - 1 do begin
    val := cla.kuns[i];
    dot_pos := pos(val, '.');
    if dot_pos>0 then
      delete(val, dot_pos, 1);
    AddCharProp(propType.id, val, dot_pos, pre_idx+i);
  end;
  Result := cla.kuns_used;
end;

procedure TCharPropBuilder.CloseKanji;
begin
  Assert(FCharIdx>=0, 'CharPropEditor: No kanji selected for editing');
  FCharIdx := -1;
end;

{ Imports all supported data from KANJIDIC/UNIHAN into wakan.chr.
 If some of TChar/TCharProp/TRadicals tables are present, copies data from those. }
procedure TfCharDataImport.Import;
var prog: TSMPromptForm;
  tempDir: string;
  backupFile: string;
  OldCharProp: TTextTable;
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

   //Preserve current CharData table and create a new one
    OldCharProp := TCharProp;
    TCharProp := NewCharPropTable();
    TCharProp.NoCommitting := true;
    SetupCharPropTable();

   //Create TChar table if we don't have one
    if TChar=nil then begin
      TChar := NewCharTable();
      SetupCharTable();
    end else begin
    { Fix older DBs where a seek formula was not defined for JpUnicode_Int order.
     We won't be able to add records without this. }
      FixTCharJpUnicodeIndex(TChar);
      FixTCharJpStrokeOrderIndex(TChar);
    end;

   //Create and import TRadicals table if we don't have one
    if TRadicals=nil then begin
      TRadicals := NewRadicalsTable();
      SetupRadicalsTable();
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

   { STAGE IV. Copy missing stuff from old tables }
    prog.SetMessage(_l('^eCopying old data...'));
    if OldCharProp<>nil then
      CopyProperties(OldCharProp, KanjidicCovered, edtUnihanFolder.Text<>'');

   { STAGE V. Sort, reindex and finalize }
    prog.SetMessage(_l('#01078^eReindexing...'));

   //Reindex
    TCharProp.NoCommitting := false;
    TCharProp.Reindex;

   //Re-arrange TChar-wise
    TCharProp := SortByTChar(TChar, TCharProp);

   //Free old char data
    FreeAndNil(OldCharProp);

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
  CharIdx: integer;
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
    if (field=nil) or (field.values_used<1) then begin
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
         '0', //Index
         '0', //Not unicode-only
         'J', //See comments for TCharType. For now, 'J'. Unihan parsing might alter this.
         ed.kanji, //Unicode,
         '255', //Stroke count (chinese),
         IntToStr(JpFrequency),
         '255', //ChFrequence
         IntToStr(JouyouGrade),
         IntToStr(JpStrokeCount)
        ]);
      end else
       { If there are duplicate records in KANJIDIC we may get CChar.tcur for a
        newly-added record, outside of CharsCovered.
        New records have no properies to migrate so we need not to extend
        CharsCovered to them }
        if CChar.tcur < Length(CharsCovered) then
          CharsCovered[CChar.tcur] := true; //mark as migrated

      CharIdx := CChar.TrueInt(TCharIndex);
      CNewProp.OpenKanji(CharIdx);

     //Add standard properties
     { AddProperties allows passing Nil as a second param, so we don't check
      if the optional fields are present }
      CNewProp.AddProperties(FindCharPropType(ptKoreanReading), ed.GetField('W'));
      CNewProp.AddProperties(FindCharPropType(ptMandarinReading), ed.GetField('Y'));
      CNewProp.AddDefinitions(FindCharPropType(ptJapaneseDefinitionUnicode), @ed);
      CNewProp.AddOns(FindCharPropType(ptOnReading), @ed.readings[0]);
      CNewProp.AddKuns(FindCharPropType(ptKunReading), @ed.readings[0]);
      pre_idx := CNewProp.AddOns(FindCharPropType(ptNanoriReading), @ed.readings[1]);
      CNewProp.AddKuns(FindCharPropType(ptNanoriReading), @ed.readings[1], pre_idx);

     //Add automated properties
      for i := 0 to Length(CharPropTypes) - 1 do begin
        propType := @CharPropTypes[i];
        if propType.id<=10 then continue; //not automated
        if propType.id=ptJapaneseDefinitionUnicode then continue; //not automated either
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
  CharIdx: integer;
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
     '0', //Index
     '1', //Unicode-only
     'N', //See comments for TCharType
     kanji, //Unicode,
     '255', //Stroke count,
     '255', //Jp stroke count
     '65535', //Jp frequency
     '255', //Frequency
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
          CChar.Edit([TCharChFrequency],[IntToStr(StrToInt(ed.value))]);
        end else
        if ed.propType='kTotalStrokes' then begin
          NeedChar(ed.char);
          CChar.Edit([TCharStrokeCount],[IntToStr(StrToInt(ed.value))]);
        end else
        if ed.propType='kBigFive' then begin
          NeedChar(ed.char);
          if CChar.Str(TCharType)='N' then
            CChar.Edit([TCharType],['T'])
          else;
            CChar.Edit([TCharType],['A']);
        end else
        if ed.propType='kGB0' then begin
          NeedChar(ed.char);
          if CChar.Str(TCharType)='N' then
            CChar.Edit([TCharType],['S'])
          else;
            CChar.Edit([TCharType],['A']);
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
          CharIdx := CChar.TrueInt(TCharIndex);
          CNewProp.OpenKanji(CharIdx);
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
         //Just add
         //May other properties need splitting too?

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
          end;

          NeedChar(ed.char);
          CharIdx := CChar.TrueInt(TCharIndex);
          CNewProp.OpenKanji(CharIdx);
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

const
  MainKanjidicPropTypes = [ptKoreanReading, ptMandarinReading,
    ptJapaneseDefinition, ptOnReading, ptKunReading, ptNanoriReading];
  MainUnihanPropTypes = [ptChineseDefinition, ptCantoneseReading];

{ For every char from TChar, checks which sources covered it in this update,
 and copies old data for any sources which did not cover it this time. }
procedure TfCharDataImport.CopyProperties(OldCharProp: TTextTable;
  const KanjidicCovered: TFlagList; const UnihanPresent: boolean);
var CChar: TTextTableCursor;
  CCharProp: TCharPropertyCursor;
  CNewProp: TCharPropBuilder;
  SCharIndex: TSeekObject;
  CharIdx: integer;
  propType: PCharPropType;
  src: char;
  skip: boolean;
begin
  SCharIndex := TChar.GetSeekObject('Index');

  CChar := nil;
  CCharProp := nil;
  CNewProp := nil;
  try
    CChar := TTextTableCursor.Create(TChar);
    CCharProp := TCharPropertyCursor.Create(OldCharProp);
    CNewProp := TCharPropBuilder.Create(TCharProp);

    CCharProp.First;
    while not CCharProp.EOF do begin
      CharIdx := CCharProp.TrueInt(TCharPropKanji);
      if not CChar.Locate(@SCharIndex, CharIdx) then
        raise Exception.Create('Somehow TChar for a property wasn''t found.'
          +'Perhaps you have a broken WAKAN.CHR? Rebuild from the scratch.');

     { Properties with id <= 10 are special ones and are crafted by hand,
      others are more or less automated. }
      propType := CCharProp.PropType;
      if propType.id>10 then
        src := propType.sourceType
      else
      if propType.id in MainKanjidicPropTypes then
        src := 'D'
      else
      if propType.id in MainUnihanPropTypes then
        src := 'U'
      else
        src := '?'; //god knows what it is, really.

      if src='D' then
        skip := (CChar.tcur < Length(KanjidicCovered)) and KanjidicCovered[CChar.tcur]
      else
      if src='U' then
        skip := UnihanPresent
      else
        skip := false; //just copy the abomination

      if not skip then begin
        CNewProp.OpenKanji(CharIdx);
        CNewProp.AddCharPropRaw(propType.id, CCharProp.AnsiStr(TCharPropValue),
          CCharProp.Int(TCharPropReadDot), CCharProp.Int(TCharPropPosition));
        CNewProp.CloseKanji;
      end;

      CCharProp.Next;
    end;

  finally
    FreeAndNil(CNewProp);
    FreeAndNil(CCharProp);
    FreeAndNil(CChar);
  end;
end;

{ Rebuilds TCharProp table so that records are stored in a specific order --
 Kanji+Type+Index -- see NewCharPropTable() comments.
 Returns new table (old one is freed) }
function TfCharDataImport.SortByTChar(TChar: TTextTable; TCharProp: TTextTable): TTextTable;
var CChar: TTextTableCursor;
  CCharProp: TTextTableCursor;
  CharIdx: integer;
  SCharPropKanji: TSeekObject;
  Builder: TCharPropBuilder;
begin
  Result := NewCharPropTable();
  Result.NoCommitting := true;

  CChar := nil;
  CCharProp := nil;
  Builder := nil;
  try
    CChar := TTextTableCursor.Create(TChar);
    CCharProp := TTextTableCursor.Create(TCharProp);
    Builder := TCharPropBuilder.Create(Result);

    CCharProp.SetOrder('Kanji');
    SCharPropKanji := TCharProp.GetSeekObject('Kanji');

    CChar.First;
    while not CChar.EOF do begin
      CharIdx := CChar.TrueInt(TCharIndex);
      CCharProp.Locate(@SCharPropKanji, CharIdx);
      Builder.OpenKanji(CharIdx);
      while CCharProp.Int(TCharPropKanji)=CharIdx do begin
        Builder.AddCharPropRaw(
         CCharProp.TrueInt(TCharPropTypeId),
         CCharProp.AnsiStr(TCharPropValue),
         CCharProp.Int(TCharPropReadDot),
         CCharProp.Int(TCharPropPosition)
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
