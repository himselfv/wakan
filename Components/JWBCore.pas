unit JWBCore;
{ Common stuff for Wakan which does not require any heavy dependencies.
 Paths, folders, backup, portability mode etc. }

interface
uses Types;

var
  AppFilename: string = '';
  AppFolder: string = ''; //path to program, set on load (no trail. slash)
  WakanVer: string = ''; //taken from resources on load

type
  TPortabilityMode = (pmStandalone, pmPortable, pmCompatible);

{$WRITEABLECONST ON}
const
  WakanAppName = 'WaKan - Japanese & Chinese Learning Tool';
  WakanCopyright = '(C) Filip Kabrt and others 2002-2013';
  WakanRegKey = 'Software\Labyrinth\Wakan';
  AppUrl: string = 'http://code.google.com/p/wakan/';
  WikiUrlBase: string = 'http://code.google.com/p/wakan/wiki/';

  PortabilityMode: TPortabilityMode = pmCompatible;

 //Set by SetPortabilityMode()
 //All paths have no trailing slashes
  ProgramDataDir: string = ''; //dictionaries, romanizations
  UserDataDir: string = '';  //wakan.usr, collections


procedure SetPortabilityMode(AMode: TPortabilityMode);
function GetAppDataFolder: string;
function DictionaryDir: string;
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

function BackupDir: string;
function GetBackupFilename(const AFilename: string): string;
function Backup(const filename: string): string;

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



{ Portable/standalone }

//Returns AppData\Wakan folder NO MATTER what mode is active.
//Do not use to get Wakan common/user data folders.
function GetAppDataFolder: string;
begin
  Result := GetSpecialFolderPath(CSIDL_APPDATA);
 //There's also CSIDL_LOCAL_APPDATA which might do if CSIDL_APPDATA is somehow not available.
 //But I don't think that can be the case!
  Assert(Result<>''); //just in case
  Result:=Result+'\Wakan';
  ForceDirectories(Result);
end;

//Absolutely the same as ProgramDataDir. Made just to prevent mistakes.
function CommonDataDir: string;
begin
  Result := ProgramDataDir;
end;

function WikiUrl(const APage: string = ''): string;
begin
  Result := WikiUrlBase + APage;
end;

procedure SetPortabilityMode(AMode: TPortabilityMode);
begin
  PortabilityMode := AMode;
  case AMode of
    pmStandalone: UserDataDir := GetAppDataFolder;
    pmPortable: UserDataDir := AppFolder+'\UserData';
    pmCompatible: UserDataDir := AppFolder;
  end;
  ProgramDataDir := AppFolder;
end;

function DictionaryDir: string;
begin
 //Dictionaries are always stored in application folder
  Result := ProgramDataDir;
end;

function BackupDir: string;
begin
  Result := UserDataDir+'\backup';
end;

//dir\wakan.usr --> wakan-20130111.usr
function GetBackupFilename(const AFilename: string): string;
begin
  Result := BackupDir+'\'+ChangeFileExt(ExtractFilename(AFilename),'')+'-'
    +FormatDateTime('yyyymmdd-hhnnss',now)+ExtractFileExt(AFilename);
end;

{ Universal backup function. Backups everything to the directory designated for backups.
 Returns filename of the backup or empty string on failure }
function Backup(const filename: string): string;
begin
 //For now works as it did in previous Wakan versions.
 //Has to be reworked to put backups into user folder.
  Result := GetBackupFilename(filename);
  ForceDirectories(ExtractFileDir(Result));
  if not CopyFile(PChar(filename),pchar(Result),false) then
    Result := '';
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
