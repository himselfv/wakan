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
uses StdPrompt, JWBStrings, JWBCharData, JWBKanjidicReader, JWBConvert, JWBUnit;

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
    raise Exception.Create('File '+edtKanjidicFile.Text+' does not exist!');
  UpdateKanjidicData(edtKanjidicFile.Text);
end;

//Formats datetime in a Wakan "version" format (14AUG05)
function WakanDatestamp(const dt: TDatetime): string;
var fs: TFormatSettings;
begin
  GetLocaleFormatSettings($0409, fs);
  Result := AnsiUpperCase(FormatDatetime('ddmmmyy',dt,fs));
end;

//Returns a Wakan date stamp which represents file last modification time
function FileAgeStr(const filename: string): string;
var dt: TDatetime;
begin
  if not FileAge(filename, dt) then
    dt := now();
  Result := WakanDatestamp(dt);
end;

procedure TfCharDataImport.UpdateKanjidicData(const KanjidicFilename: string);
var prog: TSMPromptForm;
  fuin: TJwbConvert;
  conv_type: integer;
  ed: TKanjidicEntry;
  line: string;
  tempDir: string;
  backupFile: string;
begin
  prog:=SMProgressDlgCreate(_l('^eCharacter data import'),_l('^eUpdating...'),0,{CanCancel=}true);
  try
    if not self.Visible then //auto mode
      prog.Position := poScreenCenter;
    prog.Show;

   //Delete existing properties taken from KANJIDIC

   //Parse KANJIDIC and add new properties
    ed.Reset;
    fuin := TJwbConvert.Open(KanjidicFilename, FILETYPE_UNKNOWN);
    try
      conv_type := fuin.DetectType;
      if conv_type=FILETYPE_UNKNOWN then
        conv_type := FILETYPE_EUC; //kanjidic is by default EUC
      fuin.RewindAsType(conv_type);
      while not fuin.EOF do begin
        line := fuin.ReadLn;
        if line='' then continue;
        if IsKanjidicComment(line) then continue;
        ParseKanjidicLine(line, @ed);
      end;
    finally
      FreeAndNil(fuin);
    end;

    prog.Update;

   //Reindex
    prog.SetMessage('Reindexing...');
    TCharProp.Reindex;

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
