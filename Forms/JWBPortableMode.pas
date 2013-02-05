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
  StdCtrls, Buttons, ExtCtrls;

//TODO: Localize this form

type
  TfPortableMode = class(TForm)
    lblQuestion: TLabel;
    lblPortableDescription: TLabel;
    lblStandaloneDescription: TLabel;
    btnPortable: TButton;
    btnStandalone: TButton;
  end;

var
  fPortableMode: TfPortableMode;

function AskIfPortableMode: boolean;
function AskIfMoveFiles: boolean;

procedure MoveFilesToAppData;

implementation
uses JWBStrings, JWBUnit, StdPrompt;

{$R *.DFM}

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
  if dwCallbackReason<>CALLBACK_CHUNK_FINISHED then exit;
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
      PChar(_l('#01019^eNot all files have been moved. This operation will continue if you restart Wakan.'#13
        +'Do you want to abort the operation?')),
      PChar(_l('#01020^eConfirm abort')),
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
      _l('Move in progress'),
      _l('Moving file %s...', [ExtractFilename(filename)]),
      100, //for starters
      {canCancel=}true);

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

//Asks the user to select a portability mode
function AskIfPortableMode: boolean;
var mr: TModalResult;
begin
  if not Application.MainForm.Visible then
    fPortableMode.Position := poScreenCenter
  else
    fPortableMode.Position := poOwnerFormCenter;

  with fPortableMode do begin
    lblQuestion.Caption := _l('#01009^eDo you want this copy of Wakan to be standalone?');
    btnPortable.Caption := _l('#01010^ePortable');
    lblPortableDescription.Caption := _l('#01011^eMove Wakan from a computer to a computer.'#13
      +'All settings are stored in the application folder.');
    btnStandalone.Caption := _l('#01012^eStandalone');
    lblStandaloneDescription.Caption := _l('#01013^eShare Wakan with all users of this computer.'#13
      +'Each user keeps their own settings.');
  end;

  mr := fPortableMode.ShowModal;
  case mr of
    1000: Result := false;
    1001: Result := true;
  else
    raise EAbort.Create('Loading aborted');
  end;
end;

//Asks the user if they want to move their files to AppData
function AskIfMoveFiles: boolean;
var mr: TModalResult;
begin
  if not Application.MainForm.Visible then
    fPortableMode.Position := poScreenCenter
  else
    fPortableMode.Position := poOwnerFormCenter;

  with fPortableMode do begin
    lblQuestion.Caption := _l('#01014^eWakan wants to move your files to AppData.'#13'This is recommended.');
    btnPortable.Caption := _l('#01015^eNope');
    lblPortableDescription.Caption := _l('#01016^eLeave my files in the Program Files folder.'#13
      +'I like to break the rules.');
    btnStandalone.Caption := _l('#01017^eSure');
    lblStandaloneDescription.Caption := _l('#01018^eMove my files where needed.'#13
      +'Just keep them safe.');
  end;

  mr := fPortableMode.ShowModal;
  case mr of
    1000: Result := false;
    1001: Result := true;
  else
    raise EAbort.Create('Loading aborted');
  end;
end;

{ Call to move all the possible files to the app data.
 It's okay to call this function multiple times, even across restarts;
 in fact you MUST do this until it finishes properly. }
procedure MoveFilesToAppData;
begin
  MoveToAppData('wakan.usr');
  MoveToAppData('wakan.cdt');
end;

end.