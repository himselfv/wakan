unit UpgradeFiles;
// Upgrades local data and moves data from obsolete to relevant locations.

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, JwbForms,
  StdCtrls, Generics.Collections, JWBStrings;

type
 //Upgrade is performed through UpgradeActions. The simplest is "MoveFile" which
 //moves file or folder where specified

  TProgressHandler = class
    function HandleProgress(const Total, Done: int64): boolean; virtual; abstract;
    procedure ReportError(ErrorText: string); virtual; abstract;
  end;

  TUpgradeAction = class
  public
    ProgressHandler: TProgressHandler;
    function Description: string; virtual; abstract;
    procedure Run; virtual; abstract;
  end;


 {
  Moves a file or directory from one place into another.
   BasePath: source dir
   Filename: relative path and name of file or folder to move
   TargetPath: target dir where to reproduce the relative path and put the file

  Directories will automatically be moved with all contents.
  It's better to MoveFile(dirname) than to move it file by file. In easy cases
  this'll move the directory in one action, and hard cases are covered.

  Simple errors are reported through ProgressHandler, unpexpected ones are raised.
 }

  TMoveFileFlag = (
    mfAutoMerge,    // if the target is a folder, merge the contents
    mfAutoRename    // if the target file exists, give this one a new name
  );
  TMoveFileFlags = set of TMoveFileFlag;

  TMoveFile = class(TUpgradeAction)
  protected
    FSourceBase, FFilename: string;
    FTargetBase: string;
    FFlags: TMoveFileFlags;
    function DoMove(const SourceFile, TargetPath, TargetName: string): integer;
    procedure MoveFile(const SourceFile, TargetPath: string);
  public
    constructor Create(const ABasePath, AFilename, ATargetPath: string; AFlags: TMoveFileFlags = []);
    function Description: string; override;
    procedure Run; override;
  end;

  TUpgradeActionList = class(TObjectList<TUpgradeAction>)
  public
    procedure AddMoveFile(const ABasePath, AFilename, ATargetPath: string; AFlags: TMoveFileFlags = [mfAutoRename]);
    procedure AddMoveByMask(const ABasePath, AMask, ATargetPath: string; AFlags: TMoveFileFlags = [mfAutoRename]);
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
function EnumByMask(const APath, AMask: string; Recursive: boolean = false): TFileList; overload;
var ABasePath: string;
begin
  SetLength(Result, 0);
  ABasePath := APath;
  if (ABasePath <> '') and (ABasePath[Length(ABasePath)] = '\') then
    delete(ABasePath, Length(ABasePath), 1);
  EnumByMask(Result, ABasePath, '', AMask, Recursive);
end;


{
BasePath: root folder where AFilename is located.
Filename: target subfolder and file
TargetPath: where to recreate subfolders and move file
}
constructor TMoveFile.Create(const ABasePath, AFilename, ATargetPath: string; AFlags: TMoveFileFlags = []);
begin
  inherited Create;
  FSourceBase := ABasePath;
  FFilename := AFilename;
  FTargetBase := ATargetPath;
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
// SourceFile: relative path and name of file or folder to move
// TargetPath: relative path where to place it
// TargetName: new filename to use
// Returns: LastError or 0
function TMoveFile.DoMove(const SourceFile, TargetPath, TargetName: string): integer;
var TargetFilename: string;
begin
  if TargetPath <> '' then
    TargetFilename := TargetPath+'\'+TargetName
  else
    TargetFilename := TargetName; //MoveFile dislikes double//slashes
  if not MoveFileWithProgress(
    PChar(FSourceBase+'\'+SourceFile),
    PChar(FTargetBase+'\'+TargetFilename),
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

//Moves one file or folder completely. Minds flags.
// SourceFile: relative path and name of file or folder to move
// TargetPath: relative path where to place it. May be different from SourceFile's
//   relative path due to auto-renaming of something in the middle
procedure TMoveFile.MoveFile(const SourceFile, TargetPath: string);
var err: integer;
  RenameIndex: integer;
  TargetFilename: string;
  ChildFilename: string;
begin
  SysUtils.ForceDirectories(ExtractFilePath(FTargetBase+'\'+TargetPath));

  RenameIndex := 1;
  TargetFilename := ExtractFilename(SourceFile);
  repeat
    err := DoMove(SourceFile, TargetPath, TargetFilename);
    case err of
      ERROR_SUCCESS: break;
      ERROR_FILE_EXISTS,
      ERROR_ALREADY_EXISTS:
        if (mfAutoMerge in FFlags)
        and (RenameIndex = 1) //makes no sense to merge into randomly renamed directories
        and DirectoryExists(FSourceBase+'\'+SourceFile)
        and DirectoryExists(FTargetBase+'\'+TargetPath+'\'+TargetFilename) then begin
          //Automatically merge the contents of the folder
          for ChildFilename in EnumByMask(FSourceBase+'\'+SourceFile, '*', false) do
            MoveFile(SourceFile+'\'+ChildFilename, TargetPath+'\'+ExtractFilename(SourceFile));
          //Delete the source folder
          if not RemoveDirectory(PChar(FSourceBase+'\'+SourceFile)) then
            ProgressHandler.ReportError(SourceFile+': Cannot remove folder.');
          exit;
        end else
        if mfAutoRename in FFlags then begin
          TargetFilename := CreateIndexedFilename(ExtractFilename(FFilename), RenameIndex);
          Inc(RenameIndex);
          continue; // retry
        end else begin
          ProgressHandler.ReportError(SourceFile+': file already exists');
          exit;
        end;
    else //unexpected errors
      break; //and raise error later
    end;
  until false;

  if err <> 0 then
    RaiseLastOsError(err);
end;

procedure TMoveFile.Run;
begin
  MoveFile(FFilename, ExtractFilePath(FFilename));
end;

procedure TUpgradeActionList.AddMoveFile(const ABasePath, AFilename, ATargetPath: string; AFlags: TMoveFileFlags);
begin
  if FileExists(ABasePath+'\'+AFilename)
  or DirectoryExists(ABasePath+'\'+AFilename) then
    Self.Add(TMoveFile.Create(ABasePath, AFilename, ATargetPath, AFlags));
end;

procedure TUpgradeActionList.AddMoveByMask(const ABasePath, AMask, ATargetPath: string; AFlags: TMoveFileFlags);
var Filename: string;
begin
  for Filename in EnumByMask(ABasePath, AMask) do
    Self.Add(TMoveFile.Create(ABasePath, Filename, ATargetPath, AFlags));
end;


//Builds a list of all upgrade actions in the order of their execution.
//Standalone/portable mode must be set before calling. Make sure to only add actions relevant to
//current mode.
function BuildUpgradeActionList: TUpgradeActionList;
var AppFolder: string;
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
    + Copy(AppFolder, Length(AppFolderDrive)+1, MaxInt); //AppFolder without Drive component

  //Older Wakans stored these in the application folder
  //We don't require standalone mode because there is a VirtualStore
  //for portable folder too - no harm checking
  Result.AddMoveByMask(VirtualStore, '*.usr', UserDataDir);
  Result.AddMoveByMask(VirtualStore, '*.bak', UserDataDir);
  Result.AddMoveByMask(VirtualStore, '*.cdt', UserDataDir);
  Result.AddMoveByMask(VirtualStore, '*.lay', UserDataDir);
  Result.AddMoveFile(VirtualStore, 'roma_problems.txt', UserDataDir);
  Result.AddMoveFile(VirtualStore, 'backup', UserDataDir, [mfAutoMerge, mfAutoRename]);

  Result.AddMoveByMask(AppFolder, '*.usr', UserDataDir);
  Result.AddMoveByMask(AppFolder, '*.bak', UserDataDir);
  Result.AddMoveByMask(AppFolder, '*.cdt', UserDataDir);
  Result.AddMoveByMask(AppFolder, '*.lay', UserDataDir);
  Result.AddMoveFile(AppFolder, 'roma_problems.txt', UserDataDir);
  Result.AddMoveFile(AppFolder, 'backup', UserDataDir, [mfAutoMerge, mfAutoRename]);
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
    Errors: TStringList;
  public
    sp: TSMPromptForm;
    constructor Create;
    destructor Destroy; override;
    procedure Start(Action: TUpgradeAction);
    function HandleProgress(const Total, Done: int64): boolean; override;
    procedure ReportError(ErrorText: string); override;
  end;

constructor TVisibleProgressHandler.Create;
begin
  inherited;
  Errors := TStringList.Create;
end;

destructor TVisibleProgressHandler.Destroy;
begin
  FreeAndNil(Errors);
  inherited;
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

procedure TVisibleProgressHandler.ReportError(ErrorText: string);
begin
  Errors.Add(ErrorText);
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

      if progress.Errors.Count > 0 then
        MessageBox(0, PChar(progress.Errors.Text),
          PChar('Not all actions were successfull'),
          MB_TASKMODAL + MB_ICONERROR);

    finally
      FreeAndNil(progress);
      FreeAndNil(sp);
    end;

  finally
    FreeAndNil(Actions);
  end;
end;


end.
