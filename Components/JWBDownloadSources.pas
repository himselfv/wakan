unit JWBDownloadSources;
{
Requires Unicode or we will not be able to load sources.cfg in UTF-16.
}

interface

type
  TSourceDicFormat = (sfEdict, sfCEdict, sfWakan);

  TDownloadSource = record
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
    BaseLanguage: string; //lowercase
    procedure Reset;
    function GetTargetDir: string;
    function GetURLFilename: string;
    function GetStaticTargetFilename: string;
    function GetCheckPresentFilename: string;
  end;
  PDownloadSource = ^TDownloadSource;

  TDownloadSourceArray = array of TDownloadSource;

  TDownloadSources = class
  protected
    FItems: TDownloadSourceArray;
    function GetCount: integer;
    function GetItemByIndex(Index: integer): PDownloadSource;
    function AddNew: PDownloadSource;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure LoadFromFile(const filename: string);
    function FindByName(const AName: string): PDownloadSource;
    property Count: integer read GetCount;
    property Items[Index: integer]: PDownloadSource read GetItemByIndex; default;
  end;

var
  DownloadSources: TDownloadSources; //populated on load


function IsComponentPresent(const ASource: PDownloadSource): boolean;

{
Downloads and extracts this dependency from whatever is specified as a download source
}
function DownloadDependency(const depname: string): boolean;
function VerifyDependency(const filename, depname: string): boolean;

implementation
uses SysUtils, Classes, Forms, StrUtils, Windows, JWBStrings, JWBDownloaderCore,
  SevenZip, SevenZipUtils, StdPrompt, JWBCore, JWBLanguage;

procedure TDownloadSource.Reset;
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
  BaseLanguage := '';
end;

{ Returns the file system path where the component has to be placed, depending
 on its type.
 For some components the returned path is temporary folder, from where they
 have to be installed on the system. }
function TDownloadSource.GetTargetDir: string;
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
Not always available. Some URLs provide no name (e.g. download.php?id=545)
and some servers may override the URL name with Content-Disposition header.
}
function TDownloadSource.GetURLFilename: string;
begin
  Result := ExtractFilenameURL(URL);
end;

{
TargetFilename: provides/overrides the name of the unpacked file.
Not all downloads require or even support TargetFilename. Archives may contain
multiple files with predefined names, and sometimes it's available by other
means:

1. If the download is archived and there are multiple files inside, ignore
  everything and just unpack it.
2. If TargetFilename is defined, use that.
3. If downloading, check if the server provides the name in headers.
 3a. If unpacking, auto-remove archive extensions.
4. Try to extract filename from URL (not always possible)
 4a. If unpacking, auto-remove archive extensions.

Static version:
1. If TargetFilename is defined, use that.
2. Try to extract filename from URL (not always possible)
 2a. If unpacking, auto-remove archive extensions.
}
function TDownloadSource.GetStaticTargetFilename: string;
var ext: string;
begin
  Result := TargetFilename;
  if Result='' then
    Result := ExtractFilenameURL(URL);
  ext := ExtractFileExt(Result);
  if (ext='.zip') or (ext='.gz') then
    Result := ChangeFileExt(Result, '');
end;

{
CheckPresent: file to use to check if the component is present.

1. If CheckPresent is defined, use that.
2. If TargetFilename is defined, use that.
3. Try to extract filename from URL (not always possible)
 3a. If unpacking, auto-remove archive extensions.
}
function TDownloadSource.GetCheckPresentFilename: string;
begin
  Result := CheckPresent;
  if Result='' then
    Result := GetStaticTargetFilename;
end;

constructor TDownloadSources.Create;
begin
  inherited;
end;

destructor TDownloadSources.Destroy;
begin
  inherited;
end;

procedure TDownloadSources.Clear;
begin
  SetLength(FItems, 0);
end;

function TDownloadSources.AddNew: PDownloadSource;
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

procedure TDownloadSources.LoadFromFile(const filename: string);
var sl: TStringList;
  i: integer;
  ln, param: string;
  ln_sep_pos: integer;
  item: PDownloadSource;
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
      if param='base-language' then
        item.BaseLanguage := AnsiLowerCase(ln)
      else
      begin
       //Skip params we don't understand -- allow for extensibility
      end;

    end;

  finally
    FreeAndNil(sl);
  end;
end;

function TDownloadSources.FindByName(const AName: string): PDownloadSource;
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

function TDownloadSources.GetCount: integer;
begin
  Result := Length(FItems);
end;

function TDownloadSources.GetItemByIndex(Index: integer): PDownloadSource;
begin
  Result := @FItems[Index];
end;

function IsComponentPresent(const ASource: PDownloadSource): boolean;
var TargetFile: string;
begin
  TargetFile := ASource.GetTargetDir + '\' + ASource.GetCheckPresentFilename;
  Result := FileExists(TargetFile);
end;

function DownloadDependency(const depname: string): boolean;
var dep: PDownloadSource;
  tempDir: string;
  zip: TSevenZipArchive;
  zname: string;
  i: integer;
  prog: TSMPromptForm;
begin
  dep := DownloadSources.FindByName(depname);
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

    zip := nil;
    if dep.url_unpack='zip' then
      zip := TSevenZipArchive.Create(CLSID_CFormatZip, tempDir+'\'+depname)
    else
    if dep.url_unpack='7z' then
      zip := TSevenZipArchive.Create(CLSID_CFormat7z, tempDir+'\'+depname)
    else
    if dep.url_unpack='rar' then
      zip := TSevenZipArchive.Create(CLSID_CFormatRar, tempDir+'\'+depname)
    else
    if dep.url_unpack='tar' then
      zip := TSevenZipArchive.Create(CLSID_CFormatTar, tempDir+'\'+depname)
    else
      CopyFile(PChar(tempDir+'\'+depname), PChar(ExtractFilename(dep.url)), false); //TODO: ExtractFilename is often meaningless for URLs ('dir/?get=hashcode') -- we have to get file name from HTTP headers

    prog.SetMessage('Extracting...');
    if zip<>nil then
      for i := 0 to zip.NumberOfItems - 1 do begin
        if zip.BoolProperty(i, kpidIsDir) then continue;
        with zip.ExtractFile(i) do try
          zname := zip.StrProperty(i, kpidPath);
          if zname='' then continue; //wtf?
          if ExtractFilePath(zname)<>'' then continue; //at least don't extract them to the root!
          zname := ExtractFileName(zname); //only file name
           //although in our case there's nothing EXCEPT the file name anyway, but that's now

          SaveToFile(zname);
        finally
          Free;
        end;
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
  DownloadSources.LoadFromFile('Dependencies.cfg');

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
  DownloadSources := TDownloadSources.Create;

finalization
  FreeAndNil(DownloadSources);

end.
