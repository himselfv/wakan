unit JWBDownloaderCommandLine;
{
 Command-line access to component downloader, useful for scripting.
}

interface
uses JWBCommandLine;

type
 { wakan download edict edict2
  returns 0 if all downloaded correctly }
  TDownloadCommand = class(TCommand)
  protected
    ComponentNames: array of string;
    Silent: boolean;
    Console: boolean;
  public
    function GetExecutionTime: TCommandExecutionTime; override;
    procedure Initialize; override;
    function AcceptParam(const Param: string): boolean; override;
    function AcceptFlag(const Flag: string): boolean; override;
    function Run: cardinal; override;
  end;

function ConsoleDownload(const AName: string): boolean;
function SilentDownload(const AName: string): boolean;

implementation
uses SysUtils, Windows, StdPrompt, JWBLanguage, JWBComponents, JWBDownloader, JWBJobs,
  JWBConsoleJobRunner;

function TDownloadCommand.GetExecutionTime: TCommandExecutionTime;
begin
  Result := etAfterCharData;
end;

procedure TDownloadCommand.Initialize;
begin
  SetLength(ComponentNames, 0);
  Silent := false;
  Console := false;
end;

function TDownloadCommand.AcceptParam(const Param: string): boolean;
begin
  SetLength(ComponentNames, Length(ComponentNames)+1);
  ComponentNames[Length(ComponentNames)-1] := Param;
  Result := true;
end;

function AttachConsole(dwProcessID: Integer): Boolean; stdcall; external kernel32;
const ATTACH_PARENT_PROCESS = -1;

function TDownloadCommand.AcceptFlag(const Flag: string): boolean;
begin
  if SameText(Flag, 'silent') then begin
    Silent := true;
    Result := true;
  end else
  if SameText(Flag, 'console') then begin
    if not AttachConsole(ATTACH_PARENT_PROCESS) then
      AllocConsole();
    Console := true;
    Result := true;
  end else
    Result := false;
end;

function TDownloadCommand.Run: cardinal;
var i: integer;
  succeeded: integer;
  dlResult: boolean;
begin
  succeeded := 0;
  for i := 0 to Length(ComponentNames)-1 do
    try
      if Silent then
        dlResult := SilentDownload(ComponentNames[i])
      else
      if Console then
        dlResult := ConsoleDownload(ComponentNames[i])
      else
        dlResult := DownloadComponent(ComponentNames[i]);

      if dlResult then
        Inc(succeeded);
    except
      on E: EComponentNotFound do
        if Console then
          writeln(ErrOutput, E.Message)
        else
          MessageBox(0, PChar(E.Message), PChar('Error'), MB_ICONERROR+MB_TASKMODAL);
    end;
  Result := Length(ComponentNames) - succeeded;
end;


//Executes a component download job silently
function SilentDownload(const AName: string): boolean;
var AComponent: PAppComponent;
  job: TComponentDownloadJob;
begin
  AComponent := AppComponents.FindByName(AName);
  if AComponent=nil then
    raise EComponentNotFound.Create('Cannot find component information for "'+AName+'".');

  job := TComponentDownloadJob.Create(AComponent);
  try
    RunInConsole(job, _l('01223^Downloading %s...', [AName]), true);
  finally
    FreeAndNil(job);
  end;
  Result := true;

end;

//Executes a component download job in a console runner
function ConsoleDownload(const AName: string): boolean;
var AComponent: PAppComponent;
  job: TComponentDownloadJob;
begin
  AComponent := AppComponents.FindByName(AName);
  if AComponent=nil then
    raise EComponentNotFound.Create('Cannot find component information for "'+AName+'".');
  job := TComponentDownloadJob.Create(AComponent);
  try
    RunInConsole(job, _l('01223^Downloading %s...', [AName]));
  finally
    FreeAndNil(job);
  end;
  Result := true;
end;


initialization
  RegisterCommand('download', TDownloadCommand.Create);

end.
