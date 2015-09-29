unit AppData;
// Stores application data and settings, known folders.

interface
uses IniFiles;

type
  TPortabilityMode = (pmStandalone, pmPortable, pmCompatible);

{$WRITEABLECONST ON}
const
  WakanRegKey = 'Software\Labyrinth\Wakan';
  PortabilityMode: TPortabilityMode = pmCompatible;

 //Set by SetPortabilityMode()
 //All paths have no trailing slashes
  ProgramDataDir: string = ''; //dictionaries, romanizations
  UserDataDir: string = '';  //wakan.usr, collections


procedure InitLocalData;

procedure SetPortabilityMode(AMode: TPortabilityMode);
function GetAppDataFolder: string;
function DictionaryDir: string;

function BackupDir: string;
function GetBackupFilename(const AFilename: string): string;
function Backup(const filename: string): string;

function GetWakanIni: TCustomIniFile;
function GetSettingsStore: TCustomIniFile;
procedure FreeSettings;

implementation
uses SysUtils, ShlObj, Windows, Registry, JWBStrings, JWBPortableMode, Forms;

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



//See comments in JWBPortableMode.pas about Wakan modes
//For now this sits here although should probably go somewhere else
procedure InitLocalData;
var ini: TCustomIniFile;
  fPortableMode: TfPortableMode;
  s: string;
begin
  ini := GetWakanIni;

  s := LowerCase(ini.ReadString('General', 'Install', ''));

  fPortableMode := nil;
  if s='' then begin
    fPortableMode := TfPortableMode.Create(Application);
    s := LowerCase(fPortableMode.Initialize(ini))
  end else
  if s='upgrade' then begin
    fPortableMode := TfPortableMode.Create(Application);
   //We cannot just copy some of the files. If we start this operation,
   //we have to continue it even after restart.
    s := LowerCase(fPortableMode.ContinueUpgrade(ini));
  end;
  if fPortableMode<>nil then
    FreeAndNil(FSettingsStore); //settings store location could have changed -- recreate later
  FreeAndNil(fPortableMode);

  if s='standalone' then begin
    SetPortabilityMode(pmStandalone);
  end else
  if s='compatible' then begin
    SetPortabilityMode(pmCompatible);
  end else
  if s='portable' then begin
    SetPortabilityMode(pmPortable);
  end else
    raise Exception.Create('Invalid installation mode configuration.');
  FPortabilityLoaded := true;
end;



initialization

finalization
  FreeSettings();

end.
