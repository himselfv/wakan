unit JWBCharDataImport;
{
Updates or re-imports character database from sources.
Currently only KANJIDIC is supported (and as a consequence no full reimport is
possible).
Only full replacement of all relevant properties is supported (impossible to
add translations while keeping existing ones at this point).
}

//TODO: Localize everything in the unit.

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TfCharDataImport = class(TForm)
    Label1: TLabel;
    edtKanjidicFile: TEdit;
    btnKanjidicBrowse: TButton;
    Label2: TLabel;
    btnUpdate: TButton;
    OpenKanjidicDialog: TOpenDialog;
    procedure btnKanjidicBrowseClick(Sender: TObject);
    procedure btnUpdateClick(Sender: TObject);
  public
    procedure UpdateKanjidicData(const KanjidicFilename: string);
  end;

var
  fCharDataImport: TfCharDataImport;

implementation
uses TextTable, StdPrompt, JWBStrings, JWBCharData, JWBKanjidicReader,
  JWBConvert, JWBUnit;

{$R *.dfm}

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

  UpdateKanjidicData(edtKanjidicFile.Text);
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
    FRecIndex: integer;
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
  FRecIndex := 0;
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
  Inc(FRecIndex);
  FTable.AddRecord([IntToStr(FRecIndex), IntToStr(FCharIdx), IntToStr(TypeId),
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

procedure TfCharDataImport.UpdateKanjidicData(const KanjidicFilename: string);
var prog: TSMPromptForm;
  fuin: TJwbConvert;
  conv_type: integer;
  ed: TKanjidicEntry;
  line: string;
  tempDir: string;
  backupFile: string;
  CharIdx: integer;
  propType: PCharPropType;
  i: integer;
  RecIndex: integer;
  pre_idx: integer;

  OldChar: TTextTable;
  OldCharProp: TTextTable;
  OldRadicals: TTextTable;

  CChar: TTextTableCursor;
  CCharProp: TCharPropertyCursor;
  CNewProp: TCharPropBuilder;

  SCharPropKanji: TSeekObject;

  CharsCovered: array of boolean;

  { Copies non-KANJIDIC-related properties into new table }
  procedure CopyProperties(CCharProp: TCharPropertyCursor; CNewProp: TCharPropBuilder;
    CharIdx: integer);
  var propType: PCharPropType;
  begin
    CCharProp.Locate(@SCharPropKanji, CharIdx);
    while CCharProp.Int(TCharPropKanji)=CharIdx do begin
      propType := CCharProp.PropType;
     { Properties with id <= 10 are special ones and are crafted by hand,
      others are more or less automated.  }
      if ((propType.id>10) and (propType.sourceType<>'D'))
      or ((propType.id<10) and not (propType.id in [ptKoreanReading, ptMandarinReading,
        ptJapaneseDefinition, ptOnReading, ptKunReading, ptNanoriReading])) then
      begin
        CNewProp.AddCharPropRaw(propType.id, CCharProp.AnsiStr(TCharPropValue),
          CCharProp.Int(TCharPropReadDot), CCharProp.Int(TCharPropPosition));
      end;
      CCharProp.Next;
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

   //Create new empty CharData package
    tempDir := CreateRandomTempDir();
    InitializeCharPackage(tempDir+'\wakan.chr'); //copies current package header

   //Preserve current CharData tables
    OldChar := TChar;
    OldCharProp := TCharProp;
    OldRadicals := TRadicals;
    TChar := nil;
    TCharProp := nil;
    TRadicals := nil;

   //Release whatever is not preserved
    FreeCharData;

   //Load empty CharData package
    LoadCharData(tempDir+'\wakan.chr');
    DeleteDirectory(tempDir); //it's in memory now

   { All of this was needed just so we create an empty TCharProp table
    according to the latest schema. }

   //From the newly loaded empty package we only need one table, rest are dropped.
    FreeAndNil(TChar);
    FreeAndNil(TRadicals);
    TChar := OldChar;
    TRadicals := OldRadicals;

   //Populate new, up to date TCharProp with data from OldCharProp + kanjidic
   //OldCharProp might not have latest indexes
    TCharProp.NoCommitting := true;


    CChar := TChar.NewCursor;
    CCharProp := TCharPropertyCursor.Create(OldCharProp);
    CNewProp := TCharPropBuilder.Create(TCharProp);
    try
     //Old CharProp table might not have 'Kanji' Order, in which case use default one,
     //it was sufficient then.
      if OldCharProp.orders.IndexOf('Kanji')>=0 then
        CCharProp.SetOrder('Kanji')
      else
        CCharProp.SetOrder('');

      SCharPropKanji := OldCharProp.GetSeekObject('Kanji');

     //Mark all chars as not migrated
      SetLength(CharsCovered, TChar.RecordCount);
      for i := 0 to Length(CharsCovered) - 1 do
        CharsCovered[i] := false;

     //Parse KANJIDIC and add new properties
      ed.Reset;
      fuin := TJwbConvert.Open(KanjidicFilename, FILETYPE_UNKNOWN);
      try
        conv_type := fuin.DetectType;
        if conv_type=FILETYPE_UNKNOWN then
          conv_type := Conv_ChooseType({chinese=}false,FILETYPE_EUC); //kanjidic is by default EUC
        fuin.RewindAsType(conv_type);
        while not fuin.EOF do begin
          line := fuin.ReadLn;
          if line='' then continue;
          if IsKanjidicComment(line) then continue;
          ParseKanjidicLine(line, @ed);

         //Find kanji
          if not CChar.Locate('Unicode', ed.kanji) then
            continue; //char not in db
          CharsCovered[CChar.tcur] := true; //migrated

          CharIdx := CChar.TrueInt(TCharIndex);
          CNewProp.OpenKanji(CharIdx);

         //For each character, first copy all properties which are to be preserved
          CopyProperties(CCharProp, CNewProp, CharIdx);

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
            if propType.sourceType<>'D' then continue; //not from kanjidic
            CNewProp.AddProperties(propType, PKanjidicEntry(@ed));
          end;

          CNewProp.CloseKanji;
        end;
      finally
        FreeAndNil(fuin);
      end;

     //For all characters which weren't found in the kanjidic, migrate their data
      for i := 0 to Length(CharsCovered) - 1 do
        if not CharsCovered[i] then begin
          CChar.tcur := i;
          CharIdx := CChar.Int(TCharIndex);
          CNewProp.OpenKanji(CharIdx);
          CopyProperties(CCharProp, CNewProp, CharIdx);
          CNewProp.CloseKanji;
        end;

    finally
      FreeAndNil(CNewProp);
      FreeAndNil(CCharProp);
      FreeAndNil(CChar);
    end;


   //Reindex
    prog.SetMessage(_l('#01078^eReindexing...'));
    TCharProp.NoCommitting := false;
    TCharProp.Reindex;

   //Free old char data
    FreeAndNil(OldCharProp);

   //We can't exactly say what is this KANJIDIC's "version",
   //but we'll at least mark the file last write time.
    CharDataProps.KanjidicVersion := FileAgeStr(KanjidicFilename);

   //Save
    backupFile := Backup(AppFolder+'\wakan.chr');
    if backupFile='' then
      raise Exception.Create('Cannot backup WAKAN.CHR, will not continue');

    tempDir := CreateRandomTempDir();
    SaveCharData(tempDir+'\wakan.chr');
    if not DeleteFile(AppFolder+'\wakan.chr') then
      raise Exception.Create('Cannot replace current wakan.chr');
    if not MoveFile(PChar(tempDir+'\wakan.chr'), PChar(AppFolder+'\wakan.chr')) then begin
      CopyFile(PChar(backupFile), PChar(AppFolder+'\wakan.chr'), true);
      raise Exception.Create('Cannot move newly created wakan.chr. Old wakan.chr restored.');
    end;
    DeleteDirectory(tempDir);

  finally
    FreeAndNil(prog);
  end;
end;

end.
