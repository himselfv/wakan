unit JWBDictImport;
{ Dictionary import UI }

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, ExtCtrls, StdPrompt, JWBStrings, JWBIO, JWBDic, JWBIndex,
  JWBEdictMarkers, JwbForms, JWBJobs, JWBDicImportJob;

type
  TfDictImport = class(TJwbForm)
    Label2: TLabel;
    Label5: TLabel;
    edtDictName: TEdit;
    btnBuild: TBitBtn;
    btnCancel: TBitBtn;
    AddFileDialog: TOpenDialog;
    rgLanguage: TRadioGroup;
    cbAddFrequencyInfo: TCheckBox;
    mmDescription: TMemo;
    Label1: TLabel;
    edtFilename: TEdit;
    btnChooseFile: TButton;
    procedure FormShow(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure btnBuildClick(Sender: TObject);
    procedure btnChooseFileClick(Sender: TObject);
    procedure edtFilenameChange(Sender: TObject);
    procedure edtDictNameChange(Sender: TObject);

  public
    destructor Destroy; override;
    procedure UpdateBuildButton;
    procedure ImportDictionary(const AFilename: string; ADescription: string;
      ASourceFiles: TFileList; ALang: char; AAddFrequencyInfo: boolean;
      ASilent: boolean);

  end;

implementation

uses StrUtils, WideStrUtils, JWBDictCoding, JWBKanaConv, JWBCore, JWBUnit,
  JWBLanguage, PKGWrite, JWBMenu;

{$R *.DFM}

procedure TfDictImport.FormShow(Sender: TObject);
begin
  edtFilename.Text := '';
  edtDictName.Text := '';
  mmDescription.Text:='';
  UpdateBuildButton;
end;

destructor TfDictImport.Destroy;
begin
  inherited;
end;

procedure TfDictImport.btnCancelClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TfDictImport.btnChooseFileClick(Sender: TObject);
begin
  AddFileDialog.FileName := edtFilename.Text;
  if not AddFileDialog.Execute then
    exit;
  edtFilename.Text := AddFileDialog.FileName;
  edtFilenameChange(nil);
 //Automatically suggest first source as a dictionary name
  if edtDictName.Text='' then begin
    edtDictName.Text := ChangeFileExt(ExtractFilename(AddFileDialog.FileName), '');
    edtDictNameChange(nil);
  end;
end;

procedure TfDictImport.edtFilenameChange(Sender: TObject);
begin
  UpdateBuildButton;
end;

procedure TfDictImport.edtDictNameChange(Sender: TObject);
begin
  UpdateBuildButton;
end;

procedure TfDictImport.UpdateBuildButton;
begin
  btnBuild.Enabled := (edtFilename.Text<>'') and (edtDictName.Text<>'');
end;

procedure TfDictImport.btnBuildClick(Sender: TObject);
var job: TDicImportJob;
  files: TFileList;
  fname: string;
  fDictCoding: TfDictCoding;
  fi: integer;
  ALang: char;
begin
 //We only support one file in package here, but the code is a bit more generic
  SetLength(files, 1);
  files[0] := edtFilename.Text;

  case rgLanguage.ItemIndex of
    0: ALang := 'j';
    1: ALang := 'c';
  else ALang := 'j';
  end;

  ImportDictionary(
    edtDictName.Text,
    mmDescription.Text,
    files,
    ALang,
    cbAddFrequencyInfo.Checked,
    {Silent=}false
  );

  ModalResult := mrOk;
end;

procedure TfDictImport.ImportDictionary(const AFilename: string;
  ADescription: string; ASourceFiles: TFileList; ALang: char;
  AAddFrequencyInfo: boolean; ASilent: boolean);
var job: TDicImportJob;
  prog: TSMPromptForm;
  fname: string;
  fDictCoding: TfDictCoding;
  fi: integer;
begin
 //Name == Filename - Extension
 //We accept it both with or without extension and adjust
  fname := SanitizeFilename(AFilename);
  if fname<>AFilename then
    raise Exception.Create('Invalid dictionary name, please use only supported symbols'); //TODO: Localize

  fname := MakeDicFilename(fname);

  if FileExists(fname) and (
    MessageBox(Self.Handle, PChar(Format('Dictionary %s already exists. Do you want to replace it?', [fname])), //TODO: Localize
      PChar(Self.Caption), MB_YESNO) <> ID_YES
  ) then
    raise EAbort.Create('');
 //This doesn't guarantee it won't be present later, but we catch most of the
 //common cases.

  prog:=SMProgressDlgCreate(_l('#00071^eDictionary import'),_l('^eImporting...'),100,{CanCancel=}true); //TODO: Localize
  if not self.Visible then //auto mode
    prog.Position := poScreenCenter;
  prog.Width := 500; //we're going to have long file names
  prog.AppearModal;

  job := TDicImportJob.Create;
  try
    job.DicFilename := fname;
    job.DicDescription := ADescription;
    job.DicLanguage := ALang;

    if AAddFrequencyInfo then
      job.Flags := [ifAddFrequencyInfo]
    else
      job.Flags := [];

    fDictCoding := nil;
    try
      prog.SetMaxProgress(0);

      fDictCoding := TfDictCoding.Create(Application);

     //Choose re-encoding actions for files
      for fi:=0 to Length(ASourceFiles)-1 do begin
        if not FileExists(ASourceFiles[fi]) then
          raise EDictImportException.CreateFmt(_l('File not found: %s'), [ASourceFiles[fi]]);

        if ASilent then begin
          job.AddSourceFile(ASourceFiles[fi], ALang); //same as dic language
          continue;
        end;

        fDictCoding.Label2.Caption:=_l('#00087^eInput file: ')+ExtractFilename(ASourceFiles[fi]);
       //Ask user
        fDictCoding.ShowModal;
        if not fDictCoding.succeeded then
          raise EAbort.Create('File conversion aborted ('+ASourceFiles[fi]+').');

        case fDictCoding.RadioGroup1.ItemIndex of
         1: job.AddSourceFile(ASourceFiles[fi], 'j');
         2: job.AddSourceFile(ASourceFiles[fi], 'c');
         3: job.AddSourceFile(ASourceFiles[fi], 'k');
        else job.AddSourceFile(ASourceFiles[fi], #00);
        end;
      end;

      repeat
        job.ProcessChunk;
        prog.SetMessage(job.Operation);
        prog.SetMaxProgress(job.MaxProgress);
        prog.SetProgress(job.Progress);
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
      until (job.State=jsCompleted);

    finally
      FreeAndNil(fDictCoding);
      FreeAndNil(prog);
    end;

    if job.ProblemRecords > 300 then
      Application.MessageBox(
        PChar('There were some problems during the conversion. '
        +IntToStr(job.ProblemRecords)+' records could not have been imported.'#13
        +'Please study the roma_problems.txt found in the application directory.'),
        'Had problems',
        MB_ICONEXCLAMATION)
    else
    if job.ProblemRecords > 0 then
      Application.MessageBox(
        PChar('The dictionary has been created but '+IntToStr(job.ProblemRecords)
        +' records had some problems.'#13
        +'This is not much so it''s probably fine, but if you want details, '
        +'study the roma_problems.txt found in the application directory.'),
        'Notice',
        MB_ICONINFORMATION
      )
    else
    if not ASilent then
      Application.MessageBox(
        pchar(_l('#00093^eDictionary was built.')),
        pchar(_l('#00094^eSuccess')),
        MB_ICONINFORMATION or MB_OK);

  finally
    FreeAndNil(job);
  end;
end;



end.
