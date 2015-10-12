unit UpgradeFiles;
// Upgrades local data and moves data from obsolete to relevant locations.

interface

//TODO: Handle file already existing at target location (rename?)
//  At the very least crash gracefully (not to the common catcher at Init)

//TODO: Handle "cannot delete the file from source", run as Admin

//TODO: On init, ideally, no one else should know about the tricks with language pre-loading.
//  Move all this from main form into AppData.

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
    function ToString: string; virtual; abstract;
    procedure Run; virtual; abstract;
  end;

  TMoveToAppData = class(TUpgradeAction)
  protected
    FFilename: string;
  public
    constructor Create(const AFilename: string);
    function ToString: string; override;
    procedure Run; override;
  end;

  TUpgradeActionList = class(TObjectList<TUpgradeAction>)
  public
    procedure AddMoveFile(const Filename: string);
    procedure AddMoveByMask(const Path, Mask: string; Recursive: boolean);
    procedure AddMoveDirectory(const DirName: string; Recursive: boolean);
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
uses AppData, JWBLanguage, StdPrompt;

{$R *.dfm}

constructor TMoveToAppData.Create(const AFilename: string);
begin
  inherited Create;
  FFilename := AFilename;
end;

function TMoveToAppData.ToString: string;
begin
  Result := FFilename;
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
var Action: TMoveToAppData absolute lpData;
begin
  Result := PROGRESS_CONTINUE;
  if dwCallbackReason<>CALLBACK_CHUNK_FINISHED then
    exit;
  if Action.ProgressHandler <> nil then
    if not Action.ProgressHandler.HandleProgress(TotalFileSize, TotalBytesTransferred) then
      Result := PROGRESS_STOP; //Or PROGRESS_ABORT?
end;

procedure TMoveToAppData.Run;
begin
  if not FileExists(AppFolder+'\'+FFilename) then
    exit;
  ForceDirectories(ExtractFilePath(GetAppDataFolder+'\'+FFilename)); //filename might contain subdirs
  if not MoveFileWithProgress(
    PChar(AppFolder+'\'+FFilename),
    PChar(GetAppDataFolder+'\'+FFilename),
    @CopyProgress,
    pointer(Self),
    MOVEFILE_COPY_ALLOWED) then
    RaiseLastOsError();
end;

procedure TUpgradeActionList.AddMoveFile(const Filename: string);
begin
  if FileExists(AppFolder+'\'+filename) then
    Self.Add(TMoveToAppData.Create(filename));
end;

procedure TUpgradeActionList.AddMoveByMask(const Path, Mask: string; Recursive: boolean);
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
      Self.AddMoveByMask(path2+sr.Name, mask, recursive)
    else
    if (sr.Attr and faDirectory = 0) then
      Self.Add(TMoveToAppData.Create(path2+sr.Name));
    res := FindNext(sr);
  end;
  SysUtils.FindClose(sr);
end;

procedure TUpgradeActionList.AddMoveDirectory(const DirName: string; Recursive: boolean);
begin
  Self.AddMoveByMask(dirname, '*.*', recursive);
end;

//Builds a list of all upgrade actions in the order of their execution.
//Standalone/portable mode must be set before calling. Make sure to only add actions relevant to
//current mode.
function BuildUpgradeActionList: TUpgradeActionList;
begin
  Result := TUpgradeActionList.Create;
//  if PortabilityMode = pmStandalone then begin //TODO: Uncomment
    //Older Wakans stored these in the application folder
    Result.AddMoveFile('wakan.usr');
    Result.AddMoveFile('wakan.bak');
    Result.AddMoveFile('wakan.cdt');
    Result.AddMoveDirectory('backup', true);
//  end;

//Do not move dictionaries, they are currently always in Wakan folder
{  Result.AddMoveByMask('', '*.dic', false);
  Result.AddMoveFile('edict');
  Result.AddMoveFile('edict2');
  Result.AddMoveFile('cedict'); }
end;

class function TfUpgradeFiles.ConfirmUpgrade(AOwner: TComponent; ActionList: TUpgradeActionList): boolean;
var instance: TfUpgradeFiles;
  i: integer;
begin
  instance := TfUpgradeFiles.Create(nil);
  try
    for i := 0 to ActionList.Count-1 do
      instance.lbFiles.Items.Add(ActionList[i].ToString);

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
  sp.Caption :=  _l('#01024^eMove in progress');
  sp.SetMessage(_l('#01025^eMoving file %s...', [Action.ToString]));
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
      PChar(_l('#01022^eNot all files have been moved. This operation will continue if you restart Wakan.'#13
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
