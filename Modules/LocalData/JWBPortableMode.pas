unit JWBPortableMode;
{
Wakan uses "wakan.ini" to determine whether it's running in portable mode:
  wakan.ini present + Install=portable    =>  force portable or die if not writeable folder
  wakan.ini present + Install=standalone  =>  force standalone
  wakan.ini absent                        =>  ask user, initialize wakan.ini, move files if needed

If the application needs to be installed in a specific mode, just put pre-configured
wakan.ini in the same directory.

Standalone mode:
- User data in AppData\Roaming\Wakan
- Settings in registry

Portable mode:
- User data in Wakan folder
- Settings in wakan.ini

Unsupported scenarios:
1. Configured as portable and non-writeable dir     =>  error!
2. Configured as standalone and data in folder      =>  ignore!
3. No wakan.ini + non-writeable dir                 =>  error!
  Trying to be smart here leads to all sorts of problems.

See also:
  fSettings.LoadSettings/SaveSettings
}

interface
uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, ExtCtrls, ComCtrls, IniFiles, JWBStrings, JwbForms;

type
  TfPortableMode = class(TJwbForm)
    pcPages: TPageControl;
    tsSelectMode: TTabSheet;
    lblQuestion: TLabel;
    lblPortableDescription: TLabel;
    lblStandaloneDescription: TLabel;
    btnPortable: TButton;
    btnStandalone: TButton;
    btnCompatible: TButton;
    lblCompatibleDescription: TLabel;
    tsMoveFiles: TTabSheet;
    lblMoveQuestion: TLabel;
    Label1: TLabel;
    lbFiles: TListBox;
    Label2: TLabel;
    btnMoveFiles: TButton;
    btnIgnoreFiles: TButton;
    procedure btnStandaloneClick(Sender: TObject);
    procedure btnCompatibleClick(Sender: TObject);
    procedure btnPortableClick(Sender: TObject);
    procedure btnMoveFilesClick(Sender: TObject);
    procedure btnIgnoreFilesClick(Sender: TObject);
  protected
    function BuildFileList: TFileList;
  public
    function Initialize(ini: TCustomIniFile): string;
    function ContinueUpgrade(ini: TCustomIniFile): string;
  end;

const
  MR_STANDALONE = 1001;
  MR_UPGRADE = 1002;
  MR_COMPATIBLE = 1003;
  MR_PORTABLE = 1004;

implementation
uses AppData, JWBLanguage, StdPrompt;

{$R *.DFM}

procedure AddFile(var list: TFileList; const filename: string);
begin
  if FileExists(AppFolder+'\'+filename) then
    JWBStrings.AddFilename(list, filename);
end;

procedure AddByMask(var list: TFileList; const path, mask: string; recursive: boolean);
var sr: TSearchRec;
  attr: integer;
  res: integer;
  path2: string;
begin
  attr := faAnyFile;
  if not recursive then
    attr := attr and not faDirectory;
  path2 := path;
  if (path2<>'') and (path2[Length(path2)]<>'\') then
    path2 := path2 + '\'; //don't want duplicate slashes in AddFilename
  res := FindFirst(AppFolder+'\'+path2+mask, attr, sr);
  while res=0 do begin
    if recursive and (sr.Attr and faDirectory <> 0) and (sr.Name<>'.') and (sr.Name<>'..') then
      AddByMask(list, path2+sr.Name, mask, recursive)
    else
    if (sr.Attr and faDirectory = 0) then
      JWBStrings.AddFilename(list, path2+sr.Name);
    res := FindNext(sr);
  end;
  SysUtils.FindClose(sr);
end;

procedure AddDirectory(var list: TFileList; const dirname: string; recursive: boolean);
begin
  AddByMask(list, dirname, '*.*', recursive);
end;

function TfPortableMode.BuildFileList: TFileList;
begin
  AddFile(Result, 'wakan.usr');
  AddFile(Result, 'wakan.bak');
  AddFile(Result, 'wakan.cdt');
  AddDirectory(Result, 'backup', true);
//Do not move dictionaries, they are currently always in Wakan folder
{  AddByMask(Result, '', '*.dic', false);
  AddFile(Result, 'edict');
  AddFile(Result, 'edict2');
  AddFile(Result, 'cedict'); }
end;

procedure TfPortableMode.btnStandaloneClick(Sender: TObject);
var files: TFileList;
  i: integer;
begin
  files := BuildFileList;
  if Length(files)<=0 then begin
    ModalResult := MR_STANDALONE;
    exit;
  end;

  lbFiles.Clear;
  for i := 0 to Length(files) - 1 do
    lbFiles.Items.Add(files[i]);
  pcPages.ActivePage := tsMoveFiles;
end;

procedure TfPortableMode.btnCompatibleClick(Sender: TObject);
begin
  ModalResult := MR_COMPATIBLE;
end;

procedure TfPortableMode.btnPortableClick(Sender: TObject);
begin
  ModalResult := MR_PORTABLE;
end;

procedure TfPortableMode.btnIgnoreFilesClick(Sender: TObject);
begin
  ModalResult := MR_STANDALONE;
end;

procedure TfPortableMode.btnMoveFilesClick(Sender: TObject);
begin
  ModalResult := MR_UPGRADE;
end;


//Call if the portability mode is not configured.
//We'll ask user and possibly copy some files.
function TfPortableMode.Initialize(ini: TCustomIniFile): string;
var mr: integer;
begin
  if not Application.MainForm.Visible then
    Self.Position := poScreenCenter
  else
    Self.Position := poOwnerFormCenter;
  Self.pcPages.ActivePage := tsSelectMode;
  mr := Self.ShowModal;

  if mr=MR_UPGRADE then begin
    ini.WriteString('General','Install','Upgrade');
    ini.UpdateFile;
    ContinueUpgrade(ini);
  end;

 //Write the result
  case mr of
    MR_STANDALONE,
    MR_UPGRADE: Result := 'standalone';
    MR_COMPATIBLE: Result := 'compatible';
    MR_PORTABLE: Result := 'portable';
  else
    raise EAbort.Create('Loading aborted');
  end;

  ini.WriteString('General','Install',Result);
  ini.UpdateFile;
end;

function CopyProgress(
  TotalFileSize: int64;
  TotalBytesTransferred: int64;
  StreamSize: int64;
  StreamBytesTransferred: int64;
  dwStreamNumber: dword;
  dwCallbackReason: dword;
  hSourceFile: THandle;
  hDestinationFile: THandle;
  lpData: pointer
  ): dword; stdcall;
var sp: TSMPromptForm absolute lpData;
begin
  if dwCallbackReason<>CALLBACK_CHUNK_FINISHED then begin
    Result := PROGRESS_CONTINUE;
    exit;
  end;
  if not sp.Visible then begin
    sp.AppearModal;
    sp.SetMaxProgress(TotalFileSize);
  end;
  sp.SetProgress(TotalBytesTransferred);
  sp.ProcessMessages;
  if sp.ModalResult=mrCancel then begin
    sp.Hide;
    sp.SetProgressPaused(true); //although not important when hidden
    if Application.MessageBox(
      PChar(_l('#01022^eNot all files have been moved. This operation will continue if you restart Wakan.'#13
        +'Do you want to abort the operation?')),
      PChar(_l('#01023^eConfirm abort')),
      MB_ICONQUESTION+MB_YESNO
    )=idYes then begin
      Result := PROGRESS_STOP; //Or PROGRESS_ABORT?
      exit;
    end;
    sp.ModalResult := 0;
    sp.SetProgressPaused(false);
    sp.Show;
  end;
  Result := PROGRESS_CONTINUE;
end;

procedure MoveToAppData(const filename: string);
var sp: TSMPromptForm;
begin
  if not FileExists(AppFolder+'\'+filename) then
    exit;
  sp := nil;
  try
   //Bring up the progress window
    sp:=SMProgressDlgCreate(
      _l('#01024^eMove in progress'),
      _l('#01025^eMoving file %s...', [ExtractFilename(filename)]),
      100, //for starters
      {canCancel=}true);

    ForceDirectories(ExtractFilePath(GetAppDataFolder+'\'+filename)); //filename might contain subdirs
    if not MoveFileWithProgress(
      PChar(AppFolder+'\'+filename),
      PChar(GetAppDataFolder+'\'+filename),
      @CopyProgress,
      pointer(sp),
      MOVEFILE_COPY_ALLOWED) then
      RaiseLastOsError();
  finally
    FreeAndNil(sp);
  end;
end;

//Call if an unfinished upgrade process is found ("Install=upgrade")
function TfPortableMode.ContinueUpgrade(ini: TCustomIniFile): string;
var files: TFileList;
  i: integer;
begin
  files := BuildFileList;
  for i := 0 to Length(files) - 1 do
    MoveToAppData(files[i]);
  Result := 'standalone';
  ini.WriteString('General','Install',Result);
  ini.UpdateFile;
end;

end.