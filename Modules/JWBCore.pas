unit JWBCore;
{ Common stuff for Wakan which does not require any heavy dependencies.
 Paths, folders, backup, portability mode etc. }

interface
uses Types;

var
  AppFilename: string = '';
  AppFolder: string = ''; //path to program, set on load (no trail. slash)
  WakanVer: string = ''; //taken from resources on load

{$WRITEABLECONST ON}
const
  WakanAppName = 'WaKan - Japanese & Chinese Learning Tool';
  WakanCopyright = '(C) Filip Kabrt and others 2002-2013';
  AppUrl: string = 'https://bitbucket.org/himselfv/wakan';
  WikiUrlBase: string = 'https://bitbucket.org/himselfv/wakan/wiki/';

function WikiUrl(const APage: string = ''): string;

{ Some logging tools.
Define NOLOG to make sure that nothing in the application calls these. }
//{$DEFINE NOLOG}

{$IFNDEF NOLOG}
procedure Log(const msg: string); overload; {$IFNDEF DEBUG}inline;{$ENDIF} //inline in debug so that it's completely eliminated
procedure Log(const msg: string; args: array of const); overload;
procedure Profile(const msg: string); inline;
{$ENDIF}


{ Misc }

{ Min and max so we don't have to link Math.pas just for that }
function min(a, b: integer): integer; inline;
function max(a, b: integer): integer; inline;
function perc(i,j:integer):string;

procedure ShellOpen(const sCommand: string; const sParams: string = '');
procedure StartElevated(const Command: string; const Params: string; out ProcessHandle: THandle); overload;
procedure StartElevated(const Command: string; const Params: string = ''); overload;

function RunElevatedWorker(const Params: string = ''): cardinal;
function IsElevatedWorker: boolean;

function IsAdministrator: Boolean;
function IsElevated: Boolean;
procedure SetButtonElevated(const AButtonHandle: THandle);

procedure RegeditAtKey(const key: string);

implementation
uses SysUtils,
{$IFDEF MSWINDOWS}
  ShellAPI, Winapi.Windows, ShlObj,
{$ENDIF MSWINDOWS}
{$IFDEF POSIX}
  Posix.Stdlib
{$ENDIF POSIX}
  JWBStrings, Registry
;


{ Math }

function min(a, b: integer): integer;
begin
  if a<b then Result := a else Result := b;
end;

function max(a, b: integer): integer;
begin
  if a>b then Result := a else Result := b;
end;

function perc(i,j:integer):string;
begin
  if j>0 then
    Result := IntToStr(i)+' ('+IntToStr(Round(i/j*100))+'%)'
  else
    Result := IntToStr(i)+' (?%)';
end;

function WikiUrl(const APage: string = ''): string;
begin
  Result := WikiUrlBase + APage;
end;


{ Log }
{$IFNDEF NOLOG}

{$IFDEF DEBUG}
const
  BOM_UTF16BE: AnsiString = #254#255; //FE FF
  BOM_UTF16LE: AnsiString = #255#254; //FF FE
 //must be ansi or we'll get two unicode characters

function ForceFilePath(const Filename: string): boolean;
var Path: string;
begin
  Path := ExtractFilePath(Filename);
  Result := (Path='') {current dir always exists} or ForceDirectories(Path);
end;

{ Writes to UTF16BE, adding BOM to new file.
 Better employ critical sections if you use it from multiple threads. }
procedure WriteFileWithBom(const Filename: string; data: pointer; sz: cardinal);
var HFile: THandle;
  dwBytesWritten: cardinal;
begin
// if not ForceFilePath(Filename) then exit; --- do not waste time at first, try only if file/path is not found

 //Try open or create a file
  HFile := CreateFile(PChar(Filename), GENERIC_WRITE,
    FILE_SHARE_READ, nil, OPEN_ALWAYS, 0, 0);

 //The only problem we know how to solve is "path not found"
  if HFile = INVALID_HANDLE_VALUE then begin
    if GetLastError() <> ERROR_PATH_NOT_FOUND then exit;

   //Create directories and try again
    if not ForceFilePath(Filename) then exit;

    HFile := CreateFile(PChar(Filename), GENERIC_WRITE,
      FILE_SHARE_READ, nil, OPEN_ALWAYS, 0, 0);
    if HFile = INVALID_HANDLE_VALUE then exit;
  end;

 //If the file was present, scroll to the end
  if GetLastError() = ERROR_ALREADY_EXISTS then
    SetFilePointer(HFile, 0, nil, FILE_END)
  else //Write BOM
    WriteFile(HFile, BOM_UTF16LE[1], Length(BOM_UTF16LE), dwBytesWritten, nil);
 {Can be optimized: write BOM+first message at once. But the gain is too low.}

 //Anyway write the data
  WriteFile(HFile, data^, sz, dwBytesWritten, nil);
  CloseHandle(HFile);
end;
{$ENDIF}

procedure Log(const msg: string);
{$IFDEF DEBUG}
var tmp: string;
begin
//  OutputDebugString(PChar(msg));
  tmp:=IntToStr(GetTickCount)+': '+msg+#13#10;
  WriteFileWithBom('wakan.log', @tmp[1], Length(tmp)*SizeOf(char));
end;
{$ELSE}
begin
end;
{$ENDIF}

procedure Log(const msg: string; args: array of const); overload;
begin
{$IFDEF DEBUG}
  Log(Format(msg,args));
{$ENDIF}
end;

procedure Profile(const msg: string);
begin
{$IFDEF PROFILE}
  Log(msg);
{$ELSE}
 //Empty, so it's eliminated when inlined
{$ENDIF}
end;

{$ENDIF} //IFNDEF NOLOG


{ Start applications }

procedure ShellOpen(const sCommand: string; const sParams: string = '');
begin
{$IFDEF MSWINDOWS}
  ShellExecute(0, 'open', PChar(sCommand), PChar(sParams), '', SW_SHOW);
{$ENDIF MSWINDOWS}
{$IFDEF POSIX}
  if sParams<>'' then
    _system(PAnsiChar('open ' + AnsiString(sCommand)+' '+AnsiString(sParams)))
  else
    _system(PAnsiChar('open ' + AnsiString(sCommand)));
{$ENDIF POSIX}
end;

//Runs the specified process elevated. Makes sense only on Windows.
procedure StartElevated(const Command: string; const Params: string; out ProcessHandle: THandle);
{$IFDEF MSWINDOWS}
var sei: TShellExecuteInfo;
begin
  FillChar(sei, SizeOf(sei), 0);
  sei.cbSize := SizeOf(sei);
  sei.lpVerb := PChar('runas');
  sei.lpFile := PChar(Command);
  sei.lpParameters := PChar(Params);
  sei.nShow := SW_SHOW;
  sei.fMask := SEE_MASK_NOCLOSEPROCESS;
  if not ShellExecuteEx(@sei) then
    RaiseLastOsError();
  ProcessHandle := sei.hProcess;
end;
{$ENDIF MSWINDOWS}
{$IFDEF POSIX}
begin
  if sParams<>'' then
    _system(PAnsiChar(AnsiString(Command)+' '+AnsiString(Params)))
  else
    _system(PAnsiChar(AnsiString(Command)));
end;
{$ENDIF POSIX}

procedure StartElevated(const Command: string; const Params: string = '');
var ProcessHandle: THandle;
begin
  ProcessHandle := INVALID_HANDLE_VALUE;
  StartElevated(Command, Params, ProcessHandle);
  if (ProcessHandle <> INVALID_HANDLE_VALUE) and (ProcessHandle <> 0) then
    CloseHandle(ProcessHandle);
end;

//Runs Wakan elevated with the specified parameters and waits for it to return. Returns its exit code.
//Prevents recursion by adding /elevated command-line flag. If such a flag is detected, worker is not started.
function RunElevatedWorker(const Params: string = ''): cardinal;
var ProcessHandle: THandle;
begin
  if IsElevatedWorker() then
    raise Exception.Create('Permission denied.');
  StartElevated(AppFilename, Params+' /elevated', ProcessHandle);
  if (ProcessHandle <> INVALID_HANDLE_VALUE) and (ProcessHandle <> 0) then begin
    WaitForSingleObject(ProcessHandle, INFINITE);
    if not GetExitCodeProcess(ProcessHandle, Result) then
      Result := 0;
    CloseHandle(ProcessHandle);
  end else
    Result := 0; //we can't really tell, but at least it was started
end;

//True if we're an instance of Wakan designed to be an elevated worker.
//Elevated workers must usually do their job and exit.
function IsElevatedWorker: boolean;
begin
  Result := FindCmdLineSwitch('elevated')
end;


{$IFDEF MSWINDOWS}
function CheckTokenMembership(TokenHandle: THANDLE; SidToCheck: Pointer; var IsMember: BOOL): BOOL; stdcall; external advapi32;
{$ENDIF}

function IsAdministrator: Boolean;
{$IFDEF MSWINDOWS}
var
  psidAdmin: Pointer;
  B: BOOL;
const
  SECURITY_NT_AUTHORITY: TSidIdentifierAuthority = (Value: (0, 0, 0, 0, 0, 5));
  SECURITY_BUILTIN_DOMAIN_RID  = $00000020;
  DOMAIN_ALIAS_RID_ADMINS      = $00000220;
  SE_GROUP_USE_FOR_DENY_ONLY  = $00000010;
begin
  psidAdmin := nil;
  try
    // Create Admin group SID to test against
    if not AllocateAndInitializeSid(SECURITY_NT_AUTHORITY, 2,
      SECURITY_BUILTIN_DOMAIN_RID, DOMAIN_ALIAS_RID_ADMINS, 0, 0, 0, 0, 0, 0,
      psidAdmin) then
      RaiseLastOSError();

    // Check if we're in Admin group (minding all disabled SID checks)
    if CheckTokenMembership(0, psidAdmin, B) then
      Result := B
    else
      Result := False;
  finally
    if psidAdmin <> nil then
      FreeSid(psidAdmin);
  end;
end;
{$ELSE}
 {$ERROR 'Not implemented yet outside of Windows. Posix systems probably can support this too.'}
{$ENDIF}

function IsElevated: Boolean;
{$IFDEF MSWINDOWS}
const
  TokenElevation = TTokenInformationClass(20);
type
  TOKEN_ELEVATION = record
    TokenIsElevated: DWORD;
  end;
var
  TokenHandle: THandle;
  ResultLength: Cardinal;
  ATokenElevation: TOKEN_ELEVATION;
  HaveToken: Boolean;
begin
  if CheckWin32Version(6, 0) then
  begin
    TokenHandle := 0;
    HaveToken := OpenThreadToken(GetCurrentThread, TOKEN_QUERY, True, TokenHandle);
    if (not HaveToken) and (GetLastError = ERROR_NO_TOKEN) then
      HaveToken := OpenProcessToken(GetCurrentProcess, TOKEN_QUERY, TokenHandle);
    if HaveToken then
    begin
      try
        ResultLength := 0;
        if GetTokenInformation(TokenHandle, TokenElevation, @ATokenElevation, SizeOf(ATokenElevation), ResultLength) then
          Result := ATokenElevation.TokenIsElevated <> 0
        else
          Result := False;
      finally
        CloseHandle(TokenHandle);
      end;
    end
    else
      Result := False;
  end
  else
    Result := IsAdministrator;

(*
  HANDLE hToken;
  TOKEN_ELEVATION_TYPE elevationType;
  DWORD dwSize;

  OpenProcessToken(GetCurrentProcess(), TOKEN_QUERY, &hToken);
  GetTokenInformation(hToken, TokenElevationType, &elevationType, sizeof(elevationType), &dwSize);

  switch (elevationType) {
    case TokenElevationTypeDefault:
      wprintf(TEXT("\nTokenElevationTypeDefault - User is not using a split token.\n"));
      break;
    case TokenElevationTypeFull:
      wprintf(TEXT("\nTokenElevationTypeFull - User has a split token, and the process is running elevated.\n"));
      break;
    case TokenElevationTypeLimited:
      wprintf(TEXT("\nTokenElevationTypeLimited - User has a split token, but the process is not running elevated.\n"));
      break;
  }

  if (hToken) {
    CloseHandle(hToken);
*)
end;
{$ELSE}
 {$ERROR 'Not implemented yet outside of Windows. Posix systems probably can support this too.'}
{$ENDIF}

procedure SetButtonElevated(const AButtonHandle: THandle);
{$IFDEF MSWINDOWS}
const
  BCM_SETSHIELD = $160C;
var
  Required: BOOL;
begin
  if not CheckWin32Version(6, 0) then
    Exit;
  if IsElevated then
    Exit;

  Required := True;
  SendMessage(AButtonHandle, BCM_SETSHIELD, 0, LPARAM(Required));
end;
{$ELSE}
begin
end;
{$ENDIF}


//Opens registry editor at the specific key
procedure RegeditAtKey(const key: string);
var reg: TRegistry;
begin
  reg := TRegistry.Create;
  try
    reg.RootKey := HKEY_CURRENT_USER;
    reg.OpenKey('Software', true);
    reg.OpenKey('Microsoft', true);
    reg.OpenKey('Windows', true);
    reg.OpenKey('CurrentVersion', true);
    reg.OpenKey('Applets', true);
    reg.OpenKey('Regedit', true);
    reg.WriteString('Lastkey', key);
    WinExec(PAnsiChar(AnsiString('regedit.exe')), SW_SHOW);
  finally
    FreeAndNil(reg);
  end;
end;

initialization
  AppFilename := GetModuleFilenameStr(0);
  AppFolder := ExtractFileDir(AppFilename);
  WakanVer := GetFileVersionInfoStr(AppFilename)
    {$IFDEF UNICODE}+' unicode'{$ENDIF};

end.
