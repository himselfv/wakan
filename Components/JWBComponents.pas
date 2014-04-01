unit JWBComponents;

interface

type
  TSourceDicFormat = (sfEdict, sfCEdict, sfWakan);

  TAppComponent = record
    Category: string;
    Language: string;
    Name: string; //must be lowercase
    Description: string; //multiline
    URL: string;
    URL_Unpack: string; //lowercase
    TargetFilename: string; //local filename
    CheckPresent: string;
    IsDefault: boolean;
    Encoding: string; //lowercase
    Format: TSourceDicFormat;
    BaseLanguage: char;
    procedure Reset;
    function GetTarget: string;       //where to place/unpack the component
    function GetTargetDir: string;    //same, without filename
    function GetURLFilename: string;
    function GetCheckPresentFilename: string;
  end;
  PAppComponent = ^TAppComponent;

  TAppComponentArray = array of TAppComponent;

  TAppComponents = class
  protected
    FItems: TAppComponentArray;
    function GetCount: integer;
    function GetItemByIndex(Index: integer): PAppComponent;
    function AddNew: PAppComponent;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure LoadFromFile(const filename: string);
    function FindByName(const AName: string): PAppComponent;
    function FindByFile(const AFilename: string): PAppComponent;
    property Count: integer read GetCount;
    property Items[Index: integer]: PAppComponent read GetItemByIndex; default;
  end;

var
  AppComponents: TAppComponents; //populated on load


function IsComponentPresent(const ASource: PAppComponent): boolean;

{
Downloads and extracts this dependency from whatever is specified as a download source
}
function DownloadDependency(const depname: string): boolean;
function VerifyDependency(const filename, depname: string): boolean;

implementation
uses SysUtils, Classes, Forms, StrUtils, Windows, JWBStrings, JWBDownloaderCore,
  SevenZip, SevenZipUtils, JWBUnpackJob, StdPrompt, JWBCore, JWBLanguage;

procedure TAppComponent.Reset;
begin
  Category := '';
  Language := '';
  Name := '';
  Description := '';
  URL := '';
  URL_Unpack := '';
  TargetFilename := '';
  CheckPresent := '';
  IsDefault := false;
  Encoding := '';
  Format := sfEdict;
  BaseLanguage := #00;
end;

{
Returns the name for the unpacked file or the path where it needs to be placed.
1. If TargetFilename is defined, uses that.
2. Otherwise returns the target folder, and the name under which the file is
 being downloaded should be preserved.

The name under which the file is being downloaded:
1. Server-provided name in the headers (not always provided).
2. URL-provided name (not always available: download.php?id=759) -- see GetURLFilename.

If the downloaded file is archive, it should just be unpacked to GetTargetDir.
}
function TAppComponent.GetTarget: string;
begin
  if TargetFilename<>'' then
    Result := TargetFilename
  else
    Result := GetTargetDir;
end;

{ Returns the file system path where the component has to be placed, depending
 on its type.
 For some components there's no target path as they have to be downloaded to
 temporary folder and installed on the system. }
function TAppComponent.GetTargetDir: string;
begin
  if Category='base' then
    Result := AppFolder
  else
  if Category='dic' then
    Result := ProgramDataDir //TODO: ProgramDataDir + '/Dictionaries'
  else
  if Category='language' then
    Result := AppFolder //TODO: ProgramDataDir + '/UITranslations'
  else
  if Category='font' then
    Result := ProgramDataDir //TODO: ProgramDataDir + '/Fonts'... or can we use custom fonts? If not, then TempDir
  else
  if Category='romaji' then
    Result := ProgramDataDir //TODO: ProgramDataDir + '/RomajiSchemes'
  else
    Result := ProgramDataDir;
end;

{
URLFilename: provides static filename extracted from URL.
Not always available: some URLs provide no name (e.g. download.php?id=545).
}
function TAppComponent.GetURLFilename: string;
begin
  Result := ExtractFilenameURL(URL);
end;

{
File to use to check if the component is present.
1. If CheckPresent is defined, use that.
2. If TargetFilename is defined, use that.
3. Assuming the URL filename is kept when downloading (not alaways true --
 the server can override), use that.
}
function TAppComponent.GetCheckPresentFilename: string;
begin
  Result := CheckPresent;
  if Result='' then
    Result := TargetFilename;
  if Result='' then
    Result := GetURLFilename;
end;

constructor TAppComponents.Create;
begin
  inherited;
end;

destructor TAppComponents.Destroy;
begin
  inherited;
end;

procedure TAppComponents.Clear;
begin
  SetLength(FItems, 0);
end;

function TAppComponents.AddNew: PAppComponent;
begin
  SetLength(FItems, Length(FItems)+1);
  Result := @FItems[Length(FItems)-1];
  Result.Reset;
end;

function DecodeSpecSymbols(const AText: string): string;
var i: integer;
  flag_specSymbol: boolean;
begin
  Result := '';
  flag_specSymbol := false;
  for i := 1 to Length(AText) do
    if flag_specSymbol and (AText[i]='n') then begin
      Result := Result + #13#10;
      flag_specSymbol := false;
    end else
    if flag_specSymbol or (AText[i]<>'\') then begin
      Result := Result + AText[i];
      flag_specSymbol := false;
    end else
      flag_specSymbol := true;
end;

function StrToSourceDicFormat(const AText: string): TSourceDicFormat;
var tmp: string;
begin
  tmp := AnsiLowerCase(AText);
  if tmp='edict' then
    Result := sfEdict
  else
  if tmp='cedict' then
    Result := sfCEdict
  else
  if tmp='wakan' then
    Result := sfWakan
  else
    raise Exception.Create('Unknown source dictionary format: "'+AText+'"');
end;

procedure TAppComponents.LoadFromFile(const filename: string);
var sl: TStringList;
  i: integer;
  ln, param, tmp: string;
  ln_sep_pos: integer;
  item: PAppComponent;
begin
  sl := TStringList.Create;
  try
    sl.LoadFromFile(filename);
    item := nil;
    for i := 0 to sl.Count - 1 do begin
      ln := Trim(sl[i]);
      if ln='' then continue;
      if ln[1]=';' then continue;
      if ln[1]='[' then begin
        if ln[Length(ln)]<>']' then
         //Can't continue because we might merge two sections because of this
          raise Exception.Create('Invalid section start in '+filename+': missing terminating ]');
        ln := Trim(copy(ln,2,Length(ln)-2));
        item := AddNew;
        item.name := AnsiLowerCase(ln);
        continue;
      end;

      ln_sep_pos := pos('=', ln);
      if ln_sep_pos<=0 then
       //Can't continue because this might have been intended as a section name,
       //and we'll merge two sections
        raise Exception.Create('Invalid line in '+filename+': '+ln);

      param := AnsiLowerCase(copy(ln,1,ln_sep_pos-1));
      delete(ln,1,ln_sep_pos);

      if item=nil then
        raise Exception.Create('Invalid line in '+filename+': invalid parameter outside of section');

      if param='category' then
        item.Category := AnsiLowerCase(ln)
      else
      if param='language' then
        item.Language := AnsiLowerCase(ln)
      else
      if param='description' then
        item.Description := DecodeSpecSymbols(ln)
      else
      if param='url' then
        item.URL := ln
      else
      if param='url-unpack' then
        item.URL_Unpack := AnsiLowerCase(ln)
      else
      if param='targetfile' then
        item.TargetFilename := ln
      else
      if param='checkpresent' then
        item.CheckPresent := ln
      else
      if param='default' then
        item.IsDefault := SameText(ln, 'true')
      else
      if param='encoding' then
        item.Encoding := AnsiLowerCase(ln)
      else
      if param='format' then
        item.Format := StrToSourceDicFormat(ln)
      else
      if param='base-language' then begin
        tmp := AnsiLowerCase(ln);
        if tmp='jp' then
          item.BaseLanguage := 'j'
        else
        if tmp='cn' then
          item.BaseLanguage := 'c'
        else
          raise Exception.Create('Invalid base language: '+ln);
      end else
      begin
       //Skip params we don't understand -- allow for extensibility
      end;

    end;

  finally
    FreeAndNil(sl);
  end;
end;

function TAppComponents.FindByName(const AName: string): PAppComponent;
var l_name: string;
  i: integer;
begin
  Result := nil;
  l_name := AnsiLowerCase(AName);
  for i := 0 to Self.Count - 1 do
    if FItems[i].name=l_name then begin
      Result := @FItems[i];
      break;
    end;
end;

function TAppComponents.FindByFile(const AFilename: string): PAppComponent;
var l_name: string;
  i: integer;
begin
  Result := nil;
  l_name := AnsiLowerCase(AFilename);
  for i := 0 to Self.Count - 1 do
    if (FItems[i].TargetFilename<>'')
    and (AnsiLowerCase(FItems[i].TargetFilename)=l_name) then begin
      Result := @FItems[i];
      break;
    end;
end;

function TAppComponents.GetCount: integer;
begin
  Result := Length(FItems);
end;

function TAppComponents.GetItemByIndex(Index: integer): PAppComponent;
begin
  Result := @FItems[Index];
end;

function IsComponentPresent(const ASource: PAppComponent): boolean;
var TargetFile: string;
begin
  TargetFile := ASource.GetTargetDir + '\' + ASource.GetCheckPresentFilename;
  Result := FileExists(TargetFile);
end;

function DownloadDependency(const depname: string): boolean;
var dep: PAppComponent;
  tempDir: string;
  prog: TSMPromptForm;
begin
  dep := AppComponents.FindByName(depname);
  if dep=nil then begin
    Result := false;
    exit;
  end;

  prog:=SMProgressDlgCreate(_l('^eDownload'),'',100,{CanCancel=}true);
  if not Application.MainForm.Visible then
    prog.Position := poScreenCenter;
  prog.AppearModal;
  try

    tempDir := CreateRandomTempDirName();
    ForceDirectories(tempDir);

   //Also, return false if URL not accessible
    DownloadFile(dep.url, tempDir+'\'+depname, prog);

    if dep.url_unpack='' then
      CopyFile(PChar(tempDir+'\'+depname), PChar(ExtractFilename(dep.url)), false)
      //TODO: ExtractFilename is often meaningless for URLs ('dir/?get=hashcode') -- we have to get file name from HTTP headers
    else begin
      prog.SetMessage('Extracting...');
      Unpack(
        tempDir+'\'+depname,
        AppFolder,
        dep.url_unpack
      );
    end;

  finally
    FreeAndNil(prog);
  end;

  Result := true;
end;

function VerifyDependency(const filename, depname: string): boolean;
begin
  Result := FileExists(filename) or DownloadDependency(depname);
end;

procedure DownloadTest;
var tempDir: string;
  LastModified: TDatetime;
begin
  AppComponents.LoadFromFile('Dependencies.cfg');

  //Just a test
  tempDir := CreateRandomTempDirName();
  ForceDirectories(tempDir);
  //DownloadFile('http://ftp.monash.edu.au/pub/nihongo/edict2.gz', tempDir+'\EDICT2.gz');
  DownloadFileIfModified('http://ftp.monash.edu.au/pub/nihongo/edicthdr.txt', tempDir+'\edicthdr.txt',
    now, LastModified);

 //Download dependencies!
  VerifyDependency('WORDFREQ_CK', 'WORDFREQ_CK');
  VerifyDependency('UNICONV.exe', 'UNICONV');
end;

initialization
  AppComponents := TAppComponents.Create;

finalization
  FreeAndNil(AppComponents);

end.
