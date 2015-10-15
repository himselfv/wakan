unit UpgradeFiles;
// Upgrades local data and moves data from obsolete to relevant locations.

interface

//TODO: Handle "cannot delete the file from source", run as Admin

//TODO: Auto-elevate if needed.


uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, JwbForms,
  StdCtrls, Generics.Collections, JWBStrings;

type
 //Upgrade is performed through UpgradeActions. The simplest is "MoveToAppData" which moves file
 //to the AppData/Roaming/Wakan folder.

  TProgressHandler = class
    function HandleProgress(const Total, Done: int64): boolean; virtual; abstract;
  end;

  TUpgradeAction = class
  public
    ProgressHandler: TProgressHandler;
    function Description: string; virtual; abstract;
    procedure Run; virtual; abstract;
  end;

  TMoveFileFlag = (
    mfAutoRename    // if the target file exists, automatically rename the new one
  );
  TMoveFileFlags = set of TMoveFileFlag;

  TMoveFile = class(TUpgradeAction)
  protected
    FBasePath, FFilename, FTargetPath: string;
    FFlags: TMoveFileFlags;
    function DoMove(const TargetFilename: string): integer;
  public
    constructor Create(const ABasePath, AFilename, ATargetPath: string; AFlags: TMoveFileFlags = []);
    function Description: string; override;
    procedure Run; override;
  end;

  TUpgradeActionList = class(TObjectList<TUpgradeAction>)
  public
    procedure AddMoveFile(const ABasePath, AFilename, ATargetPath: string);
  end;

  TfUpgradeFiles = class(TJwbForm)
    lblMoveQuestion: TLabel;
    Label1: TLabel;
    lbFiles: TListBox;
    Label2: TLabel;
    btnMoveFiles: TButton;
    btnIgnoreFiles: TButton;
  public
    class function ConfirmUpgrade(AOwner: TComponent; ActionList: TUpgradeActionList): boolean;
  end;

{ Performs all checks and data upgrades. Silent if nothing to upgrade. May talk to user if needed.
 Returns true if anything was upgraded.
 Standalone/portable mode must be set before calling. }
function UpgradeLocalData: boolean;

function BuildUpgradeActionList: TUpgradeActionList;

implementation
uses UITypes, ShlObj, AppData, JWBCore, JWBLanguage, StdPrompt;

{$R *.dfm}

{
BasePath: root folder where AFilename is located.
Filename: target subfolder and file
TargetPath: where to recreate subfolders and move file
}
constructor TMoveFile.Create(const ABasePath, AFilename, ATargetPath: string; AFlags: TMoveFileFlags = []);
begin
  inherited Create;
  FBasePath := ABasePath;
  FFilename := AFilename;
  FTargetPath := ATargetPath;
  FFlags := AFlags;
end;

function TMoveFile.Description: string;
begin
  Result := _l('#01232^Move %s', [FFilename]);
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
var Action: TMoveFile absolute lpData;
begin
  Result := PROGRESS_CONTINUE;
  if dwCallbackReason<>CALLBACK_CHUNK_FINISHED then
    exit;
  if Action.ProgressHandler <> nil then
    if not Action.ProgressHandler.HandleProgress(TotalFileSize, TotalBytesTransferred) then
      Result := PROGRESS_STOP; //Or PROGRESS_ABORT?
end;

//Moves the file to the target path, using the specified target filename
function TMoveFile.DoMove(const TargetFilename: string): integer;
begin
  if not MoveFileWithProgress(
    PChar(FBasePath+'\'+FFilename),
    PChar(FTargetPath+'\'+ExtractFilePath(FFilename)+'\'+TargetFilename),
    @CopyProgress,
    pointer(Self),
    MOVEFILE_COPY_ALLOWED) then
    Result := GetLastError
  else
    Result := 0;
end;

//Adds (Index) to the end of filename (before the extension)
function CreateIndexedFilename(const ABaseFilename: string; AIndex: integer): string;
begin
  Result := ChangeFileExt(ABaseFilename, '') + ' (' + IntToStr(AIndex) + ')' + ExtractFileExt(ABaseFilename);
end;

procedure TMoveFile.Run;
var err: integer;
  RenameIndex: integer;
  TargetFilename: string;
begin
  if not FileExists(FBasePath+'\'+FFilename) then
    exit;
  SysUtils.ForceDirectories(ExtractFilePath(FTargetPath+'\'+FFilename)); //FFilename might contain subdirs

  RenameIndex := 1;
  TargetFilename := ExtractFilename(FFilename);
  repeat
    err := DoMove(TargetFilename);
    case err of
      ERROR_SUCCESS: break;
      ERROR_FILE_EXISTS,
      ERROR_ALREADY_EXISTS:
        if mfAutoRename in FFlags then begin
          TargetFilename := CreateIndexedFilename(ExtractFilename(FFilename), RenameIndex);
          Inc(RenameIndex);
          continue; // retry
        end else
          exit; //for now just skip
    end;
  until false;

  if err <> 0 then
    RaiseLastOsError(err);
end;

procedure TUpgradeActionList.AddMoveFile(const ABasePath, AFilename, ATargetPath: string);
begin
  if FileExists(ABasePath+'\'+AFilename) then
    Self.Add(TMoveFile.Create(ABasePath, AFilename, ATargetPath, [mfAutoRename]));
end;

//ABasePath and APath must not have slashes at the end.
//APath must have slash at the beginning if it has at least one component.
procedure EnumByMask(var List: TFileList; const ABasePath, APath, AMask: string; Recursive: boolean); overload;
var sr: TSearchRec;
  attr: integer;
  res: integer;
begin
  attr := faAnyFile;
  if not Recursive then
    attr := attr and not faDirectory;
  res := FindFirst(ABasePath+APath+'\'+AMask, attr, sr);
  while res=0 do begin
    if recursive and (sr.Attr and faDirectory <> 0) and (sr.Name<>'.') and (sr.Name<>'..') then
      EnumByMask(List, ABasePath, APath+'\'+sr.Name, AMask, Recursive)
    else
    if (sr.Attr and faDirectory = 0) then
      AddFilename(List, APath+sr.Name);
    res := FindNext(sr);
  end;
  SysUtils.FindClose(sr);
end;

//Enumerates all files in APath, returns a list of their relative paths and names.
function EnumByMask(const APath, AMask: string; Recursive: boolean): TFileList; overload;
var ABasePath: string;
begin
  SetLength(Result, 0);
  ABasePath := APath;
  if (ABasePath <> '') and (ABasePath[Length(ABasePath)] = '\') then
    delete(ABasePath, Length(ABasePath), 1);
  EnumByMask(Result, ABasePath, '', AMask, Recursive);
end;

//Builds a list of all upgrade actions in the order of their execution.
//Standalone/portable mode must be set before calling. Make sure to only add actions relevant to
//current mode.
function BuildUpgradeActionList: TUpgradeActionList;
var Filename: string;
  AppFolder: string;
  AppFolderDrive: string;
  UserDataDir: string;
  VirtualStore: string;

begin
  Result := TUpgradeActionList.Create;

  AppFolder := JWBStrings.AppFolder;
  UserDataDir := AppData.UserDataDir;
  //On Vista+, some files are virtualized. They cannot be enumerated so we need to go
  //where they are actually stored.
  AppFolderDrive := ExtractFileDrive(AppFolder);
  VirtualStore := GetSpecialFolderPath(CSIDL_LOCAL_APPDATA) + '\VirtualStore'
    + Copy(AppFolder, Length(AppFolderDrive), MaxInt); //AppFolder without Drive component

  //Older Wakans stored these in the application folder
  Result.AddMoveFile(VirtualStore, 'wakan.usr', UserDataDir);
  Result.AddMoveFile(VirtualStore, 'wakan.bak', UserDataDir);
  Result.AddMoveFile(VirtualStore, 'wakan.cdt', UserDataDir);
  Result.AddMoveFile(VirtualStore, 'wakan.lay', UserDataDir);
  Result.AddMoveFile(VirtualStore, 'roma_problems.txt', UserDataDir);
  for Filename in EnumByMask(VirtualStore+'\backup\', '*.*', true) do
    Result.Add(TMoveFile.Create(VirtualStore, 'backup\'+Filename, UserDataDir, [mfAutoRename]));

  Result.AddMoveFile(AppFolder, 'wakan.usr', UserDataDir);
  Result.AddMoveFile(AppFolder, 'wakan.bak', UserDataDir);
  Result.AddMoveFile(AppFolder, 'wakan.cdt', UserDataDir);
  Result.AddMoveFile(AppFolder, 'wakan.lay', UserDataDir);
  Result.AddMoveFile(AppFolder, 'roma_problems.txt', UserDataDir);
  for Filename in EnumByMask(AppFolder+'\backup\', '*.*', true) do
    Result.Add(TMoveFile.Create(AppFolder, 'backup\'+Filename, UserDataDir, [mfAutoRename]));


//Do not move dictionaries, they are currently always in Wakan folder
{  Result.AddMoveByMask(AppFolder, '*.dic', DictionaryDir, false);
  Result.AddMoveFile(AppFolder, 'edict', DictionaryDir);
  Result.AddMoveFile(AppFolder, 'edict2', DictionaryDir);
  Result.AddMoveFile(AppFolder, 'cedict', DictionaryDir); }
end;

class function TfUpgradeFiles.ConfirmUpgrade(AOwner: TComponent; ActionList: TUpgradeActionList): boolean;
var instance: TfUpgradeFiles;
  i: integer;
begin
  instance := TfUpgradeFiles.Create(nil);
  try
    for i := 0 to ActionList.Count-1 do
      instance.lbFiles.Items.Add(ActionList[i].Description);

    if AOwner = nil then
      AOwner := Application.MainForm;
    if (AOwner = nil) or not (AOwner is TForm) or not TForm(AOwner).Visible then
      instance.Position := poScreenCenter
    else
      instance.Position := poOwnerFormCenter;

    Result := IsPositiveResult(instance.ShowModal);
  finally
    FreeAndNil(instance);
  end;
end;


type
  TVisibleProgressHandler = class(TProgressHandler)
  protected
    LastMaxProgress: int64;
  public
    sp: TSMPromptForm;
    procedure Start(Action: TUpgradeAction);
    function HandleProgress(const Total, Done: int64): boolean; override;
  end;

procedure TVisibleProgressHandler.Start(Action: TUpgradeAction);
begin
  Action.ProgressHandler := self;
  if sp.Visible then
    sp.SetProgress(0);
  sp.Caption :=  _l('#01236^Upgrading...');
  sp.SetMessage(Action.Description);
end;

function TVisibleProgressHandler.HandleProgress(const Total, Done: int64): boolean;
begin
  if not sp.Visible then begin
    sp.AppearModal;
    sp.SetMaxProgress(Total);
    LastMaxProgress := Total;
  end else
  if LastMaxProgress <> Total then begin
    sp.SetMaxProgress(Total);
    LastMaxProgress := Total;
  end;
  sp.SetProgress(Done);
  sp.ProcessMessages;
  if sp.ModalResult=mrCancel then begin
    sp.Hide;
    sp.SetProgressPaused(true); //although not important when hidden
    if Application.MessageBox(
      PChar(_l('#01237^Not all files have been upgraded. You will need to restart the upgrade manually.'#13
        +'Do you want to abort the operation?')),
      PChar(_l('#01023^eConfirm abort')),
      MB_ICONQUESTION+MB_YESNO
    )=idYes then begin
      Result := false;
      exit;
    end;
    sp.ModalResult := 0;
    sp.SetProgressPaused(false);
    sp.Show;
  end;
  Result := true;
end;


function UpgradeLocalData: boolean;
var Actions: TUpgradeActionList;
  progress: TVisibleProgressHandler;
  sp: TSMPromptForm;
  i: integer;
begin
  Actions := BuildUpgradeActionList;
  try
    Result := (Actions <> nil) and (Actions.Count > 0);
    if not Result then exit;

    if not IsElevated() then begin
      RunElevatedWorker('upgradelocaldata');
      Result := true; //we can't really tell
      exit;
    end;

    if not TfUpgradeFiles.ConfirmUpgrade(nil, Actions) then
      raise EAbort.Create('User cancelled');

    sp := nil;
    progress := nil;
    try
     //Bring up the progress window
      sp:=SMProgressDlgCreate(
        '', '', 100, //for starters
        {canCancel=}true);
      progress := TVisibleProgressHandler.Create;
      progress.sp := sp;

      for i := 0 to Actions.Count - 1 do begin
        progress.Start(Actions[i]);
        Actions[i].Run;
      end;

    finally
      FreeAndNil(progress);
      FreeAndNil(sp);
    end;

  finally
    FreeAndNil(Actions);
  end;
end;


end.
