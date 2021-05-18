unit AppData;
{
Stores application data and settings, known folders.
Wakan supports portable and standalone modes.

Standalone mode:
- Shared data in Program Files\Wakan
- User data in AppData\Roaming\Wakan
- Settings in registry

Portable mode:
- User data in Wakan folder
- Settings in wakan.ini

Older Wakans worked in mixed (compatible) mode:
- User data in Wakan folder
- Settings in registry

Wakan uses "wakan.ini" to determine the mode:
  wakan.ini present + Install=portable    =>  force portable or die if not writeable folder
  wakan.ini present + Install=standalone  =>  force standalone
  wakan.ini absent                        =>  ask the user, initialize wakan.ini

If the application needs to be installed in a specific mode, just put pre-configured
wakan.ini in the same directory.

Unsupported scenarios:
1. Configured as portable and non-writeable dir     =>  error!
2. No wakan.ini + non-writeable dir                 =>  error!
  Trying to be smart here leads to all sorts of problems.
}

interface
uses IniFiles;

type
  TPortabilityMode = (pmStandalone, pmPortable);

{$WRITEABLECONST ON}
const
  WakanRegKey = 'Software\Labyrinth\Wakan';
  PortabilityMode: TPortabilityMode = pmStandalone;

 {
  Main data folders. Set by SetPortabilityMode().
  All paths have no trailing slashes.
 }
  ProgramDataDir: string = ''; //dictionaries, romanizations
  UserDataDir: string = '';  //wakan.usr, collections


procedure InitLocalData;

procedure SetPortabilityMode(AMode: TPortabilityMode);
function DictionaryDir: string;

//Call Backup(filename) to make a backup of anything, before breakingly changing
function BackupDir: string;
function GetBackupFilename(const AFilename: string): string;
function Backup(const filename: string): string;
function BackupMove(const filename: string): string;

function CanWriteWakanIni: boolean;
function GetWakanIni: TCustomIniFile; //call GetSettingsStore instead
function GetSettingsStore: TCustomIniFile;
procedure FreeSettings;

implementation
uses SysUtils, Forms, ShlObj, Windows, Registry, JWBStrings, JWBCore, JWBCommandLine, JWBPortableMode,
  UpgradeFiles;

{ Portable/standalone }

//Returns AppData\Roaming\Wakan folder no matter what mode is active.
//Do not use to get Wakan common/user data folders.
function GetAppDataFolder: string;
begin
  Result := GetSpecialFolderPath(CSIDL_APPDATA);
  //There is also CSIDL_LOCAL_APPDATA which Wakan doesn't use
  Assert(Result<>''); //just in case
  Result:=Result+'\Wakan';
  ForceDirectories(Result);
end;

//Absolutely the same as ProgramDataDir. Made just to prevent mistakes.
function CommonDataDir: string;
begin
  Result := ProgramDataDir;
end;


{
               Standalone       Roaming
ProgramData    Wakan folder     Wakan folder
UserData       AppData\Wakan    Wakan\UserData
}
procedure SetPortabilityMode(AMode: TPortabilityMode);
begin
  PortabilityMode := AMode;
  ProgramDataDir := AppFolder;
  case AMode of
    pmStandalone: begin
      UserDataDir := GetAppDataFolder;
    end;
    pmPortable: begin
      UserDataDir := AppFolder+'\UserData';
    end;
  end;
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

//Same, but moves the given file to backup instead of making a copy
function BackupMove(const filename: string): string;
begin
  Result := GetBackupFilename(filename);
  ForceDirectories(ExtractFileDir(Result));
  if not MoveFile(PChar(filename), PChar(Result)) then
    Result := '';
end;

var
 { Shared settings. To avoid reading the same file twice, call GetSettingsStore
  to receive a shared copy. }
  FWakanIni: TCustomIniFile;
  FSettingsStore: TCustomIniFile;
  FPortabilityLoaded: boolean;


{ Settings store }

{ Opens wakan.ini file from the application directory which either contains
 the settings or an instruction to look in registry. }
function GetWakanIni: TCustomIniFile;
begin
  if FWakanIni<>nil then begin
    Result := FWakanIni;
    exit;
  end;
  Result := TMemIniFile.Create(AppFolder+'\wakan.ini', nil); //read everything, Ansi/UTF8/UTF16
  TMemIniFile(Result).Encoding := SysUtils.TEncoding.UTF8; //write UTF8 only
  FWakanIni := Result;
end;

{ Opens whatever settings store is configured for the application.
I.e.:
  portable mode => wakan.ini
  registry mode => registry key
If you're using this before proper LoadSettings, only read, do not write,
and be ready to receive nil }
function GetSettingsStore: TCustomIniFile;
var ini: TCustomIniFile;
  s: string;
begin
  if FSettingsStore<>nil then begin
    Result := FSettingsStore;
    exit;
  end;

 { If the settings has been loaded then the application is running in a
  particular mode and it doesn't matter what ini says anymore }
  if FPortabilityLoaded then begin
    case PortabilityMode of
      pmPortable: Result := GetWakanIni;
    else Result := TRegistryIniFile.Create(WakanRegKey);
    end;
    exit;
  end;

 { The settings has not been loaded yet, read those from ini }
  ini := GetWakanIni;

  s := LowerCase(ini.ReadString('General', 'Install', ''));
  if (s='') or (s='upgrade') or (s='compatible') or (s='standalone') then begin
    Result := TRegistryIniFile.Create(WakanRegKey);
  end else
  if (s='portable') then begin
    Result := ini;
  end else
    raise Exception.Create('Invalid installation mode configuration.');

  FSettingsStore := Result;
end;

{ Releases the shared settings objects. As things stand, there's no hurry as file
 is not kept locked. }
procedure FreeSettings;
begin
  if FSettingsStore=FWakanIni then
    FSettingsStore := nil; //same object
  FreeAndNil(FSettingsStore);
  FreeAndNil(FWakanIni);
end;

//True if the directory where Wakan resides is writeable for us
function CanWriteWakanIni: boolean;
begin
  //Usually it's better to just try and write, but VirtualStore muddifies the waters
  if FileExists(AppFolder+'\wakan.ini') then
    Result := CanAccess(AppFolder+'\wakan.ini', GENERIC_WRITE)
  else
    Result := CanAccess(AppFolder, FILE_ADD_FILE);
end;

// Saves portability settings to wakan.ini. May require administrator permissions.
procedure CommitPortabilityMode(const ModeString: string);
var ini: TCustomIniFile;
begin
  if CanWriteWakanIni then begin
    ini := GetWakanIni;
    ini.WriteString('General', 'Install', ModeString);
    ini.UpdateFile;
    UpgradeLocalData();
  end else begin
    RunElevatedWorker('writeportability ' + ModeString);
    FreeAndNil(FWakanIni); //Reload
  end;

  FreeAndNil(FSettingsStore); //settings location could have changed -- recreate later
end;


// Initializes Wakan in portable or standalone mode. Usually it's just a simple read from wakan.ini.
// In complex cases may need to ask user and store their preference, do data upgrades.
// See comments in JWBPortableMode.pas about Wakan modes.
procedure InitLocalData;
var ini: TCustomIniFile;
  s: string;
begin
  ini := GetWakanIni;
  s := LowerCase(ini.ReadString('General', 'Install', ''));

  if s='' then begin
    s := LowerCase(TfPortableMode.SelectMode(nil));
    CommitPortabilityMode(s);
  end else
  if (s='compatible') //This was a mixed mode emulating older Wakan. Not supported anymore.
  or (s='upgrade') //A mark from older Wakans that local data upgrade was aborted in the middle.
  then begin
    s := 'standalone';
    CommitPortabilityMode(s);
  end;

  if s='standalone' then begin
    SetPortabilityMode(pmStandalone);
  end else
  if s='portable' then begin
    SetPortabilityMode(pmPortable);
  end else
    raise Exception.Create('Invalid installation mode configuration.');
  FPortabilityLoaded := true;
end;



type
  TWritePortabilityCmd = class(TCommand)
  protected
    FModeString: string;
  public
    procedure Initialize; override;
    function AcceptParam(const Param: string): boolean; override;
    function Run: cardinal; override;
  end;

procedure TWritePortabilityCmd.Initialize;
begin
  FModeString := '';
end;

function TWritePortabilityCmd.AcceptParam(const Param: string): boolean;
begin
  if FModeString = '' then begin
    FModeString := Param;
    Result := true;
  end else
    Result := false;
end;

function TWritePortabilityCmd.Run: cardinal;
begin
  CommitPortabilityMode(FModeString);
  Result := 0;
end;

initialization
  RegisterCommand('writeportability', TWritePortabilityCmd.Create);

finalization
 {$IFDEF DEBUG}
  FreeSettings();
 {$ENDIF}

end.
