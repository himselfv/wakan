unit JWBAutoImport;
{
Covers EDICT auto import and auto update.
Call:
  1. AutoImportDicts -- to automatically import or update any of the known dictionaries.
  2. AutoUpdate(dic) -- to update the rest of the dictionaries.
}

interface
uses SysUtils, JWBStrings, JWBDic, JWBDictImport, JWBIO;

type
  TKnownImportFormat = (ifEdict, ifCEdict);

  TKnownImportItem = record
    Filename: string;
    Name: string;
    Format: TKnownImportFormat;
    Encoding: CEncoding; //see JWBIO
    Language: char; //j or c
    Description: string;
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
  ForceUpdates: boolean;
  ForceUpdateList: TFileList; //if empty + ForceUpdates, force for any file

procedure ParseKnownImportItem(const s: string; out item: TKnownImportItem);

{
Freshly imports all known dictionaries (EDICT/EDICT2/CEDICT/etc).
If it decides current version only needs update and not full reimport,
it leaves the work to AutoUpdate.
}
procedure AutoImportDicts;

{
Checks if a dictionary needs to be updated from sources and updates it.
No sources => okay, no updates
Some sources => message to the user
}
procedure AutoUpdate(dic: TJaletDic);

{
Updates all dictionaries from the list.
}
procedure AutoUpdateFiles(const list: TFilenameList);

{
In some cases we allow the user to pass filenames both with and without final ".DIC".
It's sometimes important to fix these filenames ahead of time.
}
procedure AutoFixFilenames(var list: TFilenameList);

const
  sAutoImport='#00988^eAuto-import';
  sAutoImportError='#00989^eAuto-import error';
  sCannotUpdateCantBeLoaded='#00990^eCannot update the dictionary "%s" because it cannot be loaded: %s';

implementation
uses Classes, Forms, Windows, MemSource, JWBCore, JWBLanguage, JWBCommandLine,
  JWBUnit;

var
 //Don't check the same dictionary twice
  AutoUpdateChecked: TFilenameList;
  AutoUpdateImported: TFilenameList;

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
  parts := SplitStr(s,6,',');
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
    item.Encoding := nil
  else
  if lowercase(parts[3])='utf8' then
    item.Encoding := TUTF8Encoding
  else
  if (lowercase(parts[3])='utf16')
  or (lowercase(parts[3])='utf16-le') then
    item.Encoding := TUTF16LEEncoding
  else
  if (lowercase(parts[3])='utf16-be') then
    item.Encoding := TUTF16BEEncoding
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

  item.Description := DecodeInfoField(parts[5]);

  if item.Name='' then Item.Name := ChangeFileExt(ExtractFilename(item.Filename),'');
end;

{
sourceFname: file name
existingDt: UTC datetime of a last imported version of sourceFname (write time)
Returns:
  True, if we reasonably need to update the file.
}
function NeedToUpdate(const sourceFname: string; const existingDt: string): boolean;
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

var
  fAutoImportForm: TfDictImport; { created on the first need, only used for auto-import.
    deleted with other forms by application }

{
Automatically imports/updates all known dictionaries:
Update strategy:
1. If no .dic file is found, just import the source file.

Assuming source and .dic are both present:
2. If .dic lacks source info, or
3. If .dic does not reference source file at all (referencing other files in addition is okay),
  Ask the user and replace it completely

4. Else do nothing and let the normal update process happen (which will update the file properly).
}
procedure TryAutoImportItem(item: PKnownImportItem);
var targetFname: string;
  files: TFileList;
  flags: TImportDictFlags;
  info: TDictInfo;
  dic: TJaletDic; //if set, update, else replace
  parts: TStringArray;
  qmsg: string; //how to ask about replacing the dictionary
  i: integer;
begin
  if not FileExists(item.Filename) then exit;
  targetFname := MakeDicFilename(item.Name);

  dic := nil;
  qmsg := '';
  if FileExists(targetFname) then begin
    dic := TJaletDic.Create;
    dic.Offline := false;
    dic.LoadOnDemand := false;
    try
      dic.FillInfo(targetFname);
    except
      on E: EDictionaryException do begin
        Application.MessageBox(
          PChar(_l(sCannotUpdateCantBeLoaded,[targetFname, E.Message])),
          PChar(_l(sAutoImportError)),
          MB_ICONERROR+MB_OK);
        exit;
      end;
    end;

    if (dic.sources=nil)
    or (dic.sources.Count<>1) then begin
     //Impossible to determine which sources went into the dictionary.
     //Replace.
      qmsg := _l(
        '#00991^e'
        +'Wakan wants to update your dictionary %0:s with the newest data from %1:s. '
        +'If %0:s was imported manually, any additional data will be lost.'#13
        +'Do you want to continue?'#13#13
        +'If you want to keep %0:s as it is, choose "No", close Wakan '
        +'and give %0:s some other name.',
        [targetFname, item.Filename]
      );
    end
    else begin
      if dic.language=item.Language then
       //If this dictionary has at least one reference to this source,
       //relegate the work to the normal update routine.
        for i := 0 to dic.sources.Count - 1 do begin
          parts := SplitStr(dic.sources[0], 2, ',');
          if lowercase(parts[0])=lowercase(ExtractFilename(item.Filename)) then begin
            FreeAndNil(dic);
            exit;
          end;
        end;

     //No references
      qmsg := _l(
        '#00992^e'
        +'Wakan wants to update your dictionary %0:s with the newest data from %1:s, '
        +'but it appears that %0:s was compiled from some other sources.'#13
        +'Do you want to replace it? It''s current contents will be lost.'#13#13
        +'If you want to keep %0:s as it is, choose "No", close Wakan '
        +'and give %0:s some other name.',
        [targetFname, item.Filename]
      );
    end;

    FreeAndNil(dic);

   //File exists but decided to replace
    if Application.MessageBox(
      PChar(qmsg),
      PChar(_l(sAutoImport)),
      MB_ICONQUESTION+MB_YESNO)<>ID_YES
    then
      exit;
  end;

 //Finally, import!
  info.Description := item.Description;
  flags := [ifSilent];
  if fAutoImportForm=nil then
    Application.CreateForm(TfDictImport, fAutoImportForm);
  if fAutoImportForm.SupportsFrequencyList then
    flags := flags + [ifAddFrequencyInfo];
  SetLength(files,1);
  files[0] := item.Filename;

  if FileExists(targetFname) then
    Backup(targetFname);

  try
    fAutoImportForm.ImportDictionary(targetFname, info, files, item.Language, flags);
    AddFilename(AutoUpdateChecked, targetFname);
    AddFilename(AutoUpdateImported, targetFname);
  except
    on E: EAbort do begin end; //do nothing, but the dictionary is not imported
  end;
end;

procedure AutoImportDicts;
var i: integer;
begin
  for i := 0 to KnownDictSources.Count - 1 do try
    TryAutoImportItem(@KnownDictSources.items[i]);
  except
    on E: EDictImportException do begin
      Application.MessageBox(
        PChar(E.Message),
        PChar(_l(sAutoImport)),
        MB_ICONERROR+MB_OK);
    end;
  end;
end;

{ Automatically update the dictionary if all source files are present and
 at least one has been changed since last import.
 Uses WORDFREQ_CK, so call after any auto-conversions related to it. }
procedure AutoUpdate(dic: TJaletDic);
var i: integer;
  fname: string;
  files: TFileList;
  missing: TFileList; //list of missing sources for current dic
  needupdate: TFileList; //list of sources which need update
  flags: TImportDictFlags;
  info: TDictInfo;
  lang: char;
  parts: TStringArray;
  wasloaded: boolean;
  pname_folder: string; //folder where dic is located -- look here first, if non-empty
begin
  if dic.sources=nil then exit; //sources not supported, can't update
   //this is silent, because the user doesn't expect us to even try and update older dics anyway
  if IsFileInList(dic.pname, AutoUpdateChecked) then exit;

  pname_folder := ExtractFilePath(dic.pname);
 //And trim, just in case
  while (Length(pname_folder)>1) and (pname_folder[1]='\') do
    pname_folder := copy(pname_folder,2,Length(pname_folder)-1);
  while (Length(pname_folder)>1) and (pname_folder[Length(pname_folder)-1]='\') do
    pname_folder := copy(pname_folder,1,Length(pname_folder)-1);
 //It's not the end of the world if something's left; we'll just check the same dir twice

  SetLength(files, 0);
  SetLength(missing, 0);
  SetLength(needUpdate, 0);
  for i := 0 to dic.sources.Count - 1 do begin
    parts := SplitStr(dic.sources[i], 2, ',');
    if parts[0]='' then begin
      AddFilename(missing, parts[0]);
      continue;
    end;

   //If the package was from another folder, first look in that another folder
    if (pname_folder<>'') and FileExists(pname_folder+'\'+parts[0]) then
      parts[0] := pname_folder+'\'+parts[0] //this fixed path will be added to file list
    else
    if not FileExists(parts[0]) then begin
      AddFilename(missing, parts[0]);
      continue;
    end;

    if NeedToUpdate(parts[0], parts[1]) then
      AddFilename(needUpdate, parts[0]);

    AddFilename(files, parts[0]);
  end;

  if ForceUpdates and ((Length(ForceUpdateList)<=0) or IsFileInList(dic.pname, ForceUpdateList)) then
    needUpdate := files; //everything needs update!

  AddFilename(AutoUpdateChecked, dic.pname);
  if Length(needUpdate)<=0 then
    exit; //nothing to update

  if Length(missing)>0 then begin
   //It's important we notify the user here, because AutoImportKnown relies
   //on this function for auto-update, and by convention we notify user about
   //any import problems with known dictionaries (they're important).
    Application.MessageBox(
      PChar(
        _l(
          '#00993^e'
          +'Wakan wants to rebuild your dictionary %0:s because these source '
          +'files have changed:'#13
          +'%1:s'#13#13
          +'Unfortunately, this cannot be done because some of the source files '
          +'which went into %0:s cannot be found:'#13
          +'%2:s'#13#13
          +'Please locate all of these files and place into Wakan folder next '
          +'time before you run Wakan.',
          [dic.pname,
           FilenameListToString(needUpdate, #13),
           FilenameListToString(missing, #13)]
        )),
      PChar(_l(sAutoImportError)),
      MB_ICONERROR+MB_OK);
    exit;
  end;

  fname := dic.pname; //dic.pname might be lost while playing with dic
  info.Description := dic.description;
  lang := dic.language;
  flags := [ifSilent];
  if fAutoImportForm=nil then
    Application.CreateForm(TfDictImport, fAutoImportForm);
  if fAutoImportForm.SupportsFrequencyList then
    flags := flags + [ifAddFrequencyInfo];

 //Unload the dictionary so that it doesn't block us,
 //and make a backup
  wasloaded := dic.loaded;
  if wasloaded then
    dic.Unload;
  Backup(fname);

  try
    fAutoImportForm.ImportDictionary(fname, info, files, lang, flags);
    AddFilename(AutoUpdateImported, fname);
  except
    on E: EAbort do begin end; //do nothing, but the dictionary is not updated
  end;

 //Load it again (I pray that it works...)
  if wasloaded then begin
    dic.FillInfo(fname);
    dic.Load;
  end;
end;

procedure AutoUpdateFiles(const list: TFilenameList);
var i: integer;
  dic: TJaletDic;
begin
  dic := nil;
  for i := 0 to Length(list) - 1 do try
    dic := TJaletDic.Create;
    dic.Offline := false;
    dic.LoadOnDemand := false;
    try
      dic.FillInfo(list[i]);
    except
      on E: EMemorySourceError do begin
        Application.MessageBox(
          PChar(_l(sCannotUpdateCantBeLoaded, [list[i], E.Message])),
          PChar(_l(sAutoImportError)),
          MB_ICONERROR+MB_OK);
        exit;
      end;
      on E: EDictionaryException do begin
        Application.MessageBox(
          PChar(_l(sCannotUpdateCantBeLoaded, [list[i], E.Message])),
          PChar(_l(sAutoImportError)),
          MB_ICONERROR+MB_OK);
        exit;
      end;
    end;
    AutoUpdate(dic)
  finally
    dic.Unload;
    FreeAndNil(dic);
  end;
end;

procedure AutoFixFilenames(var list: TFilenameList);
var missingFiles: TFileList;
  i: integer;
begin
  SetLength(missingFiles, 0);
  for i := 0 to Length(list) - 1 do
   //Prefer adding .DIC because there are often both "source" and "source.DIC" in the same folder!
   //In fact, we even auto-import dictionaries to this naming scheme, so it's COMMON.
    if FileExists(list[i]+'.dic') then list[i] := list[i]+'.dic' else
    if FileExists(list[i]) then continue else
    AddFilename(missingFiles, list[i]);

  if Length(missingFiles)>0 then
    Application.MessageBox(
      PChar(_l('#00994^e'
        +'Some dictionaries you specified weren''t found:'#13
        +'%0:s'+#13#13
        +'These dictionaries will not be updated.',
        [FilenameListToString(missingFiles, #13)]
      )),
      PChar(_l('#00989^eAuto-import error')),
      MB_ICONERROR+MB_OK
    );
end;

end.
