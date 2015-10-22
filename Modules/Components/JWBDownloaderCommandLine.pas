unit JWBDownloaderCommandLine;
{
 Command-line access to component downloader, useful for scripting.
}

interface
uses JWBCommandLine, JWBDownloader;

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

implementation
uses SysUtils, Windows, StdPrompt, JWBLanguage, JWBComponents;

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

function TDownloadCommand.AcceptFlag(const Flag: string): boolean;
begin
  if SameText(Flag, 'silent') then begin
    Silent := true;
    Result := true;
  end else
  if SameText(Flag, 'console') then begin
    Console := true;
    Result := true;
  end else
    Result := false;
end;

function TDownloadCommand.Run: cardinal;
var i: integer;
  succeeded: integer;
begin
  succeeded := 0;
  for i := 0 to Length(ComponentNames)-1 do
    try
      if Console then begin
        if ConsoleDownload(ComponentNames[i]) then
          Inc(succeeded);
      end else begin
        if DownloadComponent(ComponentNames[i]) then
          Inc(succeeded);
      end;
    except
      on E: EComponentNotFound do
        if Console then
          writeln(ErrOutput, E.Message)
        else
          MessageBox(0, PChar(E.Message), PChar('Error'), MB_ICONERROR+MB_TASKMODAL);
    end;
  Result := Length(ComponentNames) - succeeded;
end;

initialization
  RegisterCommand('download', TDownloadCommand.Create);

end.
