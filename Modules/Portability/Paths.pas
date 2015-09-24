unit Paths;

interface

type
  TPortabilityMode = (pmStandalone, pmPortable, pmCompatible);

  PortabilityMode: TPortabilityMode = pmCompatible;

 //Set by SetPortabilityMode()
 //All paths have no trailing slashes
  ProgramDataDir: string = ''; //dictionaries, romanizations
  UserDataDir: string = '';  //wakan.usr, collections


procedure SetPortabilityMode(AMode: TPortabilityMode);
function GetAppDataFolder: string;
function DictionaryDir: string;

function BackupDir: string;
function GetBackupFilename(const AFilename: string): string;
function Backup(const filename: string): string;

implementation

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


end.
