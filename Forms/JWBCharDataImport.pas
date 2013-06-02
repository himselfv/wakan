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
  Dialogs, StdCtrls, UrlLabel, TextTable;

type
  TFlagList = array of boolean;

  TfCharDataImport = class(TForm)
    Label1: TLabel;
    edtKanjidicFile: TEdit;
    btnKanjidicBrowse: TButton;
    Label2: TLabel;
    btnUpdate: TButton;
    OpenKanjidicDialog: TOpenDialog;
    Label4: TLabel;
    lblBackupPath: TUrlLabel;
    Label3: TLabel;
    btnCancel: TButton;
    procedure btnKanjidicBrowseClick(Sender: TObject);
    procedure btnUpdateClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  protected
    procedure ImportKanjidic(const KanjidicFilename: string; out CharsCovered: TFlagList);
    procedure ImportUnihan(const UnihanFolder: string);
    procedure CopyProperties(OldCharProp: TTextTable; const KanjidicCovered: TFlagList;
      const UnihanPresent: boolean);
    function SortByTChar(TChar: TTextTable; TCharProp: TTextTable): TTextTable;
  public
    procedure UpdateCharDb(ResetDb: boolean; const KanjidicFilename: string;
      const UnihanFolder: string);
  end;

var
  fCharDataImport: TfCharDataImport;

implementation
uses StdPrompt, JWBStrings, JWBCharData, JWBKanjidicReader, JWBUnihanReader,
  JWBConvert, JWBIO, JWBUnit;

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
      edtKanjidicFile.Text := Filename;
end;

procedure TfCharDataImport.btnUpdateClick(Sender: TObject);
begin
 //Not really needed but let's check beforehand
  if not FileExists(edtKanjidicFile.Text) then
    raise Exception.CreateFmt(_l('#01075^eFile %s does not exist!'), [edtKanjidicFile.Text]);
  if Application.MessageBox(
    PChar(_l('#01072^Character data in this copy of Wakan will be replaced with '
      +'the new one provided here. Application will then restart and you will '
      +'lose all the unsaved data. Do you want to continue?')),
    PChar(_l('#01071^eConfirm import')),
    MB_YESNO or MB_ICONQUESTION)<>ID_YES then exit;

  UpdateCharDb({ResetDB=}false, edtKanjidicFile.Text, {UNIHAN=}'');
  Application.MessageBox(
    PChar(_l('#01074^eCharacter data has been imported. Application will now terminate.')),
    PChar(_l('#01073^eImport completed')),
    MB_OK or MB_ICONINFORMATION
  );
  Application.Terminate;
  Close;
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

//{$DEFINE UNIHAN_NOINDEX}

{ Updates or recreates from the scratch WAKAN.CHR.
 If any of KanjidicFilename, UnihanFolder is not specified, that part is skipped.
 If ResetDb is set, existing information is discarded, otherwise preserved where
 no new info is available. }
procedure TfCharDataImport.UpdateCharDb(ResetDb: boolean;
  const KanjidicFilename: string; const UnihanFolder: string);
var prog: TSMPromptForm;
  tempDir: string;
  backupFile: string;
  OldCharProp: TTextTable;

 { If we're updating DB, we need to port TChar and property entries which were
  not covered by the KANJIDIC/UNIHAN provided. So we keep track of it. }
  KanjidicCovered: TFlagList;
 { Unihan is handled differently, it's either present or not. It can't cover
  less than it covered on previous import. }

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
    prog.SetMessage(_l('^eClearing up...'));

   //Preserve current CharData table and create a new one
    OldCharProp := TCharProp;
    TCharProp := NewCharPropTable();
    TCharProp.NoCommitting := true;

   //Kill OldCharProp if we don't need it -- why keep it?
    if ResetDb then
      FreeAndNil(OldCharProp);

   //Clear current Char table if needed
    if ResetDb then begin
      FreeAndNil(TChar);
      TChar := NewCharTable();
    end else
    { Fix older DBs where a seek formula was not defined for JpUnicode_Int order.
     We won't be able to add records without this. }
      FixTCharJpUnicodeIndex(TChar);

    TChar.NoCommitting := true;

   { STAGE II. Import KANJIDIC }
    prog.SetMessage(_l('^eImporting KANJIDIC...'));
    if KanjidicFilename<>'' then
      ImportKanjidic(KanjidicFilename, KanjidicCovered)
    else
      SetLength(KanjidicCovered, 0);
{$IFNDEF UNIHAN_NOINDEX}
    TChar.Reindex; //next stage will need to locate() in additions
    TChar.NoCommitting := false;
   //We cannot do NoCommiting with UNIHAN since the same (newly-added) kanji
   //will appear several times in it.
   //First time we added, second time we need to locate it, which requires it
   //to be indexed (or we'll add it again)
{$ENDIF}

   { STAGE III. Import UNIHAN }
    prog.SetMessage(_l('^eImporting UNIHAN...'));
    if UnihanFolder<>'' then
      ImportUnihan(UnihanFolder);

{$IFDEF UNIHAN_NOINDEX}
    TChar.Reindex;
    TChar.NoCommitting := false;
{$ENDIF}

   { STAGE IV. Copy missing stuff from old tables }
    prog.SetMessage(_l('^eCopying old data...'));
    if not ResetDb then
      CopyProperties(OldCharProp, KanjidicCovered, UnihanFolder<>'');

   { STAGE V. Sort, reindex and finalize }
    prog.SetMessage(_l('#01078^eReindexing...'));

   //Reindex
    TCharProp.NoCommitting := false;
    TCharProp.Reindex;

   //Re-arrange TChar-wise
    TCharProp := SortByTChar(TChar, TCharProp);

   //Free old char data
    FreeAndNil(OldCharProp);

   //We can't exactly say what is this KANJIDIC's "version",
   //but we'll at least mark the file last write time.
    CharDataProps.KanjidicVersion := FileAgeStr(KanjidicFilename);

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
      raise Exception.Create('Cannot replace current wakan.chr');
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

{ Imports all possible data from KANJIDIC source into current TChar/TCharProp }
procedure TfCharDataImport.ImportKanjidic(const KanjidicFilename: string;
  out CharsCovered: TFlagList);
var CChar: TTextTableCursor;
  CNewProp: TCharPropBuilder;
  fuin: TJwbConvert;
  conv_type: integer;
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
    fuin := TJwbConvert.Open(KanjidicFilename, FILETYPE_UNKNOWN);

    conv_type := fuin.DetectType;
    if conv_type=FILETYPE_UNKNOWN then
      conv_type := Conv_ChooseType({chinese=}false,FILETYPE_EUC); //kanjidic is by default EUC
    if conv_type=FILETYPE_UNKNOWN then
      raise EAbort.Create('');
    fuin.RewindAsType(conv_type);
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
         '255', //Stroke count,
         IntToStr(JpStrokeCount),
         IntToStr(JpFrequency),
         '255', //Frequence
         IntToStr(JouyouGrade)
        ]);
      end else
        CharsCovered[CChar.tcur] := true; //migrated

      CharIdx := CChar.TrueInt(TCharIndex);
      CNewProp.OpenKanji(CharIdx);

     //Add standard properties
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

  procedure NeedChar(const kanji: UnicodeString);
  begin
   //TODO: Problem problem problem! The chars added during this pass
   // are not being located (they're not yet in indexes).
   //Solution: Move away from DisableIndexes/Reindex scheme. Commits should not
   // be disableable.
   //I have to write a fast indexing scheme, binary tree with logarithm addition
   //and fixed positions for all elements probably.
   //Convert to this on load, convert from this on write (with traversal).

   //TODO: Simple optimization: If it's the same char as before, nothing needs to be done!
   // (Unihan really stores chars mostly in batches, not between diff. files though)

   //TODO: Replace 'Unicode' with ptr index reference

    if CChar.Locate('Unicode', kanji) then
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
  var ffile: TCustomFileReader;
    line: string;
    ed: TUnihanPropertyEntry;
    propType: PCharPropType;
    i: integer;
    entry_no: integer;
  begin
    ffile := TAnsiFileReader.Create(UnihanFile);
    try
      while ffile.Readln(line) do begin
        line := Trim(line);
        if (line='') or IsUnihanComment(line) then continue;
        ParseUnihanLine(line, @ed);

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
            CNewProp.AddCharProp(propType, ed.value, 0, entry_no);
            Inc(entry_no);
          end;
          CNewProp.CloseKanji;
        end else
        if propType<>nil then begin
         //Just add
         //May other properties need splitting too?
          NeedChar(ed.char);
          CharIdx := CChar.TrueInt(TCharIndex);
          CNewProp.OpenKanji(CharIdx);
          CNewProp.AddCharProp(propType, ed.value, 0, 0);
          CNewProp.CloseKanji;
        end;

      end;
    finally
      FreeAndNil(ffile);
    end;
  end;

begin
 { Parse KANJIDIC and add new properties }
  CChar := nil;
  CNewProp := nil;
  try
    CChar := TChar.NewCursor;
    CNewProp := TCharPropBuilder.Create(TCharProp);

    ImportFile(UnihanFolder+'\Unihan_Readings.txt');
    ImportFile(UnihanFolder+'\Unihan_RadicalStrokeCounts.txt');
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
  MainUnihanPropTypes = [ptChineseDefinition, ptCantoneseReading, ptRadicals];

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

{ Rebuilds TCharProp table so that records for a single character go in a batch.
 This a hack but it's needed by older Wakans.
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
