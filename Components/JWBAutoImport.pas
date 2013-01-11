unit JWBAutoImport;
{ Covers EDICT auto import and auto upgrade }

interface
uses JWBStrings, JWBConvert, JWBDic, JWBDictImport;

type
  TKnownImportFormat = (ifEdict, ifCEdict);

  TKnownImportItem = record
    Filename: string;
    Name: string;
    Format: TKnownImportFormat;
    Encoding: integer; //see JWBConvert
    Language: char; //j or c
    Description: string;
    Copyright: string;
  end;
  PKnownImportItem = ^TKnownImportItem;

  TKnownImportList = record
    items: array of TKnownImportItem;
    procedure Clear;
    procedure Add(const s: string);
    function Count: integer;
  end;

var
  KnownDictSources: TKnownImportList; //loaded from wakan.cfg

procedure ParseKnownImportItem(const s: string; out item: TKnownImportItem);

procedure AutoImportDicts;
procedure AutoUpgrade(dic: TJaletDic);

implementation
uses SysUtils, Classes, Forms, Windows, MemSource, JWBUnit;

procedure TKnownImportList.Clear;
begin
  SetLength(items,0);
end;

procedure TKnownImportList.Add(const s: string);
begin
  SetLength(items,Length(items)+1);
  ParseKnownImportItem(s,items[Length(items)-1]); //and that's it
end;

function TKnownImportList.Count: integer;
begin
  Result := Length(items);
end;

procedure ParseKnownImportItem(const s: string; out item: TKnownImportItem);
var parts: TStringArray;
begin
  parts := SplitStr(s,7,',');
  item.Filename := parts[0];
  item.Name := parts[1];

  if (parts[2]='')
  or (lowercase(parts[2])='edict') then
    item.Format := ifEdict
  else
  if (lowercase(parts[2])='cedict') then
    item.Format := ifCEdict
  else
    raise Exception.Create('Unknown auto import format: "'+parts[2]+'"');

  if parts[3]='' then
    item.Encoding := FILETYPE_UNKNOWN
  else
  if lowercase(parts[3])='utf8' then
    item.Encoding := FILETYPE_UTF8
  else
  if (lowercase(parts[3])='utf16')
  or (lowercase(parts[3])='utf16-le') then
    item.Encoding := FILETYPE_UTF16LE
  else
  if (lowercase(parts[3])='utf16-be') then
    item.Encoding := FILETYPE_UTF16BE
  else
    raise Exception.Create('Unknown auto import encoding: "'+parts[3]+'"');

  if (parts[4]='')
  or (lowercase(parts[4])='j') then
    item.Language := 'j'
  else
  if (lowercase(parts[4])='c') then
    item.Language := 'c'
  else
    raise Exception.Create('Unknown auto import language: "'+parts[4]+'"');

  item.Description := parts[5];
  item.Copyright := parts[6];

  if item.Name='' then Item.Name := ChangeFileExt(ExtractFilename(item.Filename),'');
end;

{
sourceFname: file name
existingDt: UTC datetime of a last imported version of sourceFname (write time)
Returns:
  True, if we reasonably need to upgrade the file.
}
function NeedToUpgrade(const sourceFname: string; const existingDt: string): boolean;
const
  OneMinute: TDatetime = 1/(24*60);
var
  dt_ex, dt_new: TDatetime;
begin
  if not TryStrToDatetime(existingDt,dt_ex,DictFormatSettings) then
    dt_ex := 0;
  if not GetLastWriteTime(sourceFname,dt_new) then
    dt_new := 0;

  Result := ((dt_ex<OneMinute) and (dt_new>OneMinute))
         or ((dt_ex>OneMinute) and (dt_new>dt_ex+OneMinute));
end;

{
Automatically imports/upgrades all known dictionaries.
Import strategy is complicated:
1. If no source file is found, let the normal upgrade happen.
  Perhaps the .dic file exists and is built from other sources.
2. If no .dic file is found, just import the source file.

Assuming source and .dic are both present:
3. If .dic lacks source info, or
4. If .dic does not contain exactly one file with exactly same name as the source,
  Replace it completely and notify user.

5. Else proceed with the normal upgrade process.
}
procedure TryAutoImportItem(item: PKnownImportItem);
var targetFname: string;
  files: TFileList;
  flags: TImportDictFlags;
  info: TDictInfo;
  dic: TJaletDic; //if set, upgrade, else replace
  parts: TStringArray;
begin
  if not FileExists(item.Filename) then exit;
  targetFname := ExtractFilename(item.Filename)+'.dic';

  dic := nil;
  if FileExists(targetFname) then begin
    dic := TJaletDic.Create;
    dic.Offline := false;
    dic.LoadOnDemand := false;
    try
      dic.FillInfo(targetFname);
    except
      on E: EDictionaryException do begin
        Application.MessageBox(
          PChar('Cannot upgrade the dictionary "'+targetFname+'" because it cannot be loaded: '+E.Message),
          PChar('Auto-import error'),
          MB_ICONERROR+MB_OK);
        exit;
      end;
    end;

    if (dic.sources=nil)
    or (dic.sources.Count<>1) then
      FreeAndNil(dic) //replace
    else begin
      parts := SplitStr(dic.sources[0], 2, ',');
      if parts[0]<>ExtractFilename(item.Filename) then
        FreeAndNil(dic); //replace

      if not NeedToUpgrade(item.Filename, parts[1]) then begin
        FreeAndNil(dic);
        exit;
      end;
     //else continue while preserving dic
    end;
  end;

 //Finally, import!
  info.Name := item.Name;
  info.Description := item.Description;
  info.Copyright := item.Copyright;
  info.Version := '';
  info.Priority := 0;
  flags := [ifAddWordIndex, ifAddCharacterIndex, ifSilent];
  if fDictImport.SupportsFrequencyList then
    flags := flags + [ifAddFrequencyInfo];
  SetLength(files,1);
  files[0] := item.Filename;

  if FileExists(targetFname) then
    Backup(targetFname);

  fDictImport.ImportDictionary(targetFname, info, files, item.Language, flags);

  FreeAndNil(dic);
end;

procedure AutoImportDicts;
var i: integer;
begin
  for i := 0 to KnownDictSources.Count - 1 do
    TryAutoImportItem(@KnownDictSources.items[i]);
end;

{ Automatically upgrade the dictionary if all source files are present and
 at least one has been changed since last import.
 Uses WORDFREQ_CK, so call after any auto-conversions related to it. }
procedure AutoUpgrade(dic: TJaletDic);
var i: integer;
  import: boolean;
  fname: string;
  files: TFileList;
  flags: TImportDictFlags;
  info: TDictInfo;
  lang: char;
  parts: TStringArray;
  wasloaded: boolean;
begin
  import := false;
  if dic.sources=nil then exit; //sources not supported, can't upgrade

  SetLength(files, 0);
  for i := 0 to dic.sources.Count - 1 do begin
    parts := SplitStr(dic.sources[i], 2, ',');
    if (parts[0]='') or not FileExists(parts[0]) then
      exit; //can't do no upgrade if files are missing

    if NeedToUpgrade(parts[0], parts[1]) then
      import := true;

    AddFilename(files, parts[0]);
  end;

  if import then begin
    fname := dic.pname;
    info.Name := dic.name;
    info.Description := dic.description;
    info.Copyright := dic.copyright;
    info.Priority := dic.priority;
    info.Version := dic.version;
    lang := dic.language;
    flags := [ifAddWordIndex, ifAddCharacterIndex, ifSilent];
    if fDictImport.SupportsFrequencyList then
      flags := flags + [ifAddFrequencyInfo];

   //Unload the dictionary so that it doesn't block us,
   //and make a backup
    wasloaded := dic.loaded;
    if wasloaded then
      dic.Unload;
    Backup(fname);

    fDictImport.ImportDictionary(fname, info, files, lang, flags);

   //Load it again (I pray that it works...)
    if wasloaded then begin
      dic.FillInfo(fname);
      dic.Load;
    end;
  end;
end;

end.
