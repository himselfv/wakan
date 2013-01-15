unit JWBDownloadSources;
{
Requires Unicode or we will not be able to load sources.cfg in UTF-16.
}

interface

type
  TDownloadSource = record
    name: string; //must be lowercase
    url: string;
    url_unpack: string;
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
    property Items[Index: integer]: PDownloadSource read GetItemByIndex;
  end;

var
  DownloadSources: TDownloadSources; //populated on load

{
Downloads and extracts this dependency from whatever is specified as a download source
}
function DownloadDependency(const depname: string): boolean;
function VerifyDependency(const filename, depname: string): boolean;

implementation
uses SysUtils, Classes, StrUtils, JWBDownloader;

procedure TDownloadSource.Reset;
begin
  name := '';
  url := '';
  url_unpack := '';
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

procedure TDownloadSources.LoadFromFile(const filename: string);
var sl: TStringList;
  i: integer;
  ln: string;
  ln_name: string;
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

      ln_name := AnsiLowerCase(copy(ln,1,ln_sep_pos-1));
      delete(ln,1,ln_sep_pos);

      if item=nil then
        raise Exception.Create('Invalid line in '+filename+': invalid parameter outside of section');

      if ln_name='url' then
        item.url := ln
      else
      if ln_name='url-unpack' then
        item.url_unpack := AnsiLowerCase(ln)
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
begin
  dep := DownloadSources.FindByName(depname);
  if dep=nil then begin
    Result := false;
    exit;
  end;

 //TODO: Wrong! Wrong! Download to tempdir, in any case.
 //Also, unpack if needed.
 //Also, return false if URL not accessible
  DownloadFile(dep.url, depname);
  Result := true;
end;

function VerifyDependency(const filename, depname: string): boolean;
begin
  Result := FileExists(filename) or DownloadDependency(depname);
end;

initialization
  DownloadSources := TDownloadSources.Create;

finalization
  FreeAndNil(DownloadSources);

end.
