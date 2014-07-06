unit JWBDictImport;
{ Dictionary import UI }

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, ExtCtrls, JWBStrings, JWBIO, JwbForms;

type
  TfDictImport = class(TJwbForm)
    Label2: TLabel;
    Label5: TLabel;
    edtDictName: TEdit;
    btnBuild: TBitBtn;
    btnCancel: TBitBtn;
    AddFileDialog: TOpenDialog;
    rgLanguage: TRadioGroup;
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

  protected
    procedure Reset;
    function ChooseFileQuery: boolean;
    function TryFillComponentDetails: boolean;
    function TryFillExistingDetails: boolean;
    procedure ImportCancelQuery(Sender: TObject; var DoAbort: boolean);
  public
    destructor Destroy; override;
    function ShowModal: integer; override;
    procedure UpdateBuildButton;
    procedure ImportDictionary(AFilename: string; ADescription: string;
      ASourceFiles: TFileList; ALang: char; ASilent: boolean);

  end;

implementation
uses StrUtils, WideStrUtils, JWBKanaConv, JWBCore, JWBUnit, JWBLanguage,
  JWBComponents, JWBFileType, StdPrompt, JWBDic, JWBDicImportJob;

{$R *.DFM}

procedure TfDictImport.Reset;
begin
  edtFilename.Text := '';
  edtDictName.Text := '';
  mmDescription.Text := '';
  UpdateBuildButton;
end;

function TfDictImport.ShowModal: integer;
begin
  Reset;
 //Ask to choose the source file before showing the dialog
  if ChooseFileQuery then
    Result := inherited ShowModal
  else
    Result := mrCancel;
end;

procedure TfDictImport.FormShow(Sender: TObject);
begin
 //Set focus to description because we don't show the form until the file is
 //chosen
  mmDescription.SetFocus;
end;

destructor TfDictImport.Destroy;
begin
  inherited;
end;

procedure TfDictImport.btnCancelClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

//Let the user choose the source dictionary file and accept it. False if cancelled.
function TfDictImport.ChooseFileQuery: boolean;
begin
  AddFileDialog.Title := _l('#01138^Choose dictionary file');
  AddFileDialog.FileName := edtFilename.Text;
  Result := AddFileDialog.Execute;
  if not Result then
    exit;
  edtFilename.Text := AddFileDialog.FileName;
  edtFilenameChange(nil);

 //Try to pre-fill details from existing package / known component
  if not TryFillExistingDetails
  and not TryFillComponentDetails then begin
   //Suggest file name as a dictionary name
    if edtDictName.Text='' then begin
      edtDictName.Text := ChangeFileExt(ExtractFilename(AddFileDialog.FileName), '');
      edtDictNameChange(nil);
    end;
  end;
end;

//If this looks like a known dictionary source, pre-populate the default name
//and description
function TfDictImport.TryFillComponentDetails: boolean;
var AComponent: PAppComponent;
begin
  AComponent := AppComponents.FindByFile(ExtractFilename(edtFilename.Text));
  if (AComponent=nil) or (AComponent.Category<>'dic') or (AComponent.Format=sfWakan) then begin
    Result := false;
    exit;
  end;

  if edtDictName.Text='' then begin
    edtDictName.Text := AComponent.Name;
    edtDictNameChange(nil);
  end;

  if mmDescription.Text='' then
    mmDescription.Text := AComponent.Description;

  if AComponent.BaseLanguage='c' then
    rgLanguage.ItemIndex := 1
  else
    rgLanguage.ItemIndex := 0;
  Result := true;
end;

function TfDictImport.TryFillExistingDetails: boolean;
var dic: TJaletDic;
begin
  dic := dicts.FindBySourceFilename(ExtractFilename(edtFilename.Text));
  if dic=nil then begin
    Result := false;
    exit;
  end;

  if edtDictName.Text='' then begin
    edtDictName.Text := dic.Name;
    edtDictNameChange(nil);
  end;

  if mmDescription.Text='' then
    mmDescription.Text := dic.Description;

  if dic.language='c' then
    rgLanguage.ItemIndex := 1
  else
    rgLanguage.ItemIndex := 0;
  Result := true;
end;

procedure TfDictImport.btnChooseFileClick(Sender: TObject);
begin
  ChooseFileQuery;
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
var files: TFileList;
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
    {Silent=}false
  );

  ModalResult := mrOk;
end;

//AFilename can be either of: path+filename, filename only or dict name (without
//extension)
procedure TfDictImport.ImportDictionary(AFilename: string; ADescription: string;
  ASourceFiles: TFileList; ALang: char; ASilent: boolean);
var job: TDicImportJob;
  prog: TSMPromptForm;
  fi: integer;
  AComponent: PAppComponent;
  AEncoding: CEncoding;
begin
  if ExtractFilePath(AFilename)='' then begin
   //Name == Filename - Extension
   //We accept it both with or without extension and adjust
    if ExtractFileExt(AFilename)<>'.dic' then
      AFilename := AFilename + '.dic';
    if SanitizeDicFilename(AFilename)<>AFilename then
      raise Exception.Create(_l('#01145^Invalid dictionary name, please do not use special symbols (\/:*? and so on)'));
    AFilename := ProgramDataDir+'\'+MakeDicFilename(AFilename);
  end;

  if FileExists(AFilename) and (
    MessageBox(Self.Handle, PChar(_l('#01146^Dictionary %s already exists. Do you want to replace it?', [AFilename])),
      PChar(Self.Caption), MB_YESNO) <> ID_YES
  ) then
    raise EAbort.Create('');
 //This doesn't guarantee it won't be present later, but we catch most of the
 //common cases.

  dicts.UnloadAll; //may hold some files open

  job := TDicImportJob.Create;
  try
    job.DicFilename := AFilename;
    job.DicDescription := ADescription;
    job.DicLanguage := ALang;

    prog:=SMProgressDlgCreate(_l('#00071^Dictionary import'),_l('#01147^Importing...'),100,{CanCancel=}true);
    if not self.Visible then //auto mode
      prog.Position := poScreenCenter;
    prog.Width := 500; //we're going to have long file names
    prog.AppearModal;
    try
      prog.SetMaxProgress(0);

      for fi:=0 to Length(ASourceFiles)-1 do begin
        if not FileExists(ASourceFiles[fi]) then
          raise EDictImportException.CreateFmt(_l('#01148^File not found: %s'), [ASourceFiles[fi]]);

       //If this is one of known files, use predefined encoding
        AComponent := AppComponents.FindByFile(ExtractFilename(ASourceFiles[fi]));
        if (AComponent<>nil) and (AComponent.Encoding<>'') then
          AEncoding := FindEncodingByName(AComponent.Encoding)
        else
          AEncoding := nil;
       //Else try to guess it
        if AEncoding=nil then
          AEncoding := Conv_DetectType(ASourceFiles[fi]);
       //Let the user confirm/change it (unless silent + not needed):
        if (AEncoding=nil) or (not ASilent) then
          AEncoding := Conv_ChooseType({Chinese=}ALang='c', AEncoding);

        job.AddSourceFile(ASourceFiles[fi], AEncoding);
      end;

      prog.OnCancelQuery := ImportCancelQuery;
      prog.ExecuteJob(job);
    finally
      FreeAndNil(prog);
    end;

    if job.ProblemRecords > 300 then
      Application.MessageBox(
        PChar(_l(
          '#01149^There were some problems building %s. %d records could not have been imported.'#13
          +'Please study %s found in the dictionary directory.',
          [AFilename, job.ProblemRecords, 'roma_problems.txt']
        )),
        PChar(_l('#01150^Had problems')),
        MB_ICONEXCLAMATION)
    else
    if job.ProblemRecords > 0 then
      Application.MessageBox(
        PChar(_l(
          '#01151^The dictionary %s has been created but %d records had some problems.'#13
          +'This is not much so it''s probably fine, but if you want details, '
          +'study %s found in the dictionary directory.',
          [AFilename, job.ProblemRecords, 'roma_problems.txt']
        )),
        PChar(_l('#01152^Notice')),
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

procedure TfDictImport.ImportCancelQuery(Sender: TObject; var DoAbort: boolean);
begin
  if Application.MessageBox(
    PChar(_l('#01003^eThe dictionary has not been yet completely imported. Do you '
      +'really want to abort the operation?')),
    PChar(_l('#01004^eConfirm abort')),
    MB_ICONQUESTION+MB_YESNO
  )<>idYes then
    DoAbort := false;
end;




end.
