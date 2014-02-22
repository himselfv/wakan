unit JWBDownloadSources;
{
Requires Unicode or we will not be able to load sources.cfg in UTF-16.
}

interface

type
  TDownloadSource = record
    Category: string;
    Language: string;
    Name: string; //must be lowercase
    Description: string; //multiline
    URL: string;
    URL_Unpack: string; //lowercase
    Filename: string; //local filename
    IsDefault: boolean;
    IsDeprecated: boolean;
    Encoding: string; //lowercase
    Format: string; //lowercase
    BaseLanguage: string; //lowercase
    procedure Reset;
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

{
Downloads and extracts this dependency from whatever is specified as a download source
}
function DownloadDependency(const depname: string): boolean;
function VerifyDependency(const filename, depname: string): boolean;

implementation
uses SysUtils, Classes, Forms, StrUtils, Windows, JWBStrings, JWBDownloaderCore,
  SevenZip, SevenZipUtils, StdPrompt, JWBUnit;

procedure TDownloadSource.Reset;
begin
  Category := '';
  Language := '';
  Name := '';
  Description := '';
  URL := '';
  URL_Unpack := '';
  Filename := '';
  IsDefault := false;
  IsDeprecated := false;
  Encoding := '';
  Format := '';
  BaseLanguage := '';
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
      if param='file' then
        item.Filename := ln
      else
      if param='default' then
        item.IsDefault := SameText(ln, 'true')
      else
      if param='deprecated' then
        item.IsDeprecated := SameText(ln, 'true')
      else
      if param='encoding' then
        item.Encoding := AnsiLowerCase(ln)
      else
      if param='format' then
        item.Format := AnsiLowerCase(ln)
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
