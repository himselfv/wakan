unit JWBDictionaries;
{ Program-wide dictionary store.
 Contains high-level procedures for managing the list of dictionaries }

{$DEFINE NODICLOADPROMPT}
{ Do not show "Loading dictionary..." window when hot-loading a dictionary.
 It was needed before when loading was slow, but I feel like now it only makes
 the interface feel sluggish. }

interface
uses SysUtils, JWBDic;

{ Dictionaries }

type
  TJaletDictionaryList = class(TDictionaryList)
  protected
   {$IFNDEF NODICLOADPROMPT}
    DicLoadPrompt: TSMPromptForm;
    procedure DicLoadStart(Sender: TObject);
    procedure DicLoadEnd(Sender: TObject);
   {$ENDIF}
    procedure DicLoadException(Sender: TObject; E: Exception);
  public
    function NewDict(const ADictFilename: string): TJaletDic;
    procedure Rescan(const AIncludeDisabled: boolean = false);
    procedure AutoUpgradeListed;
  end;

var
  dicts: TJaletDictionaryList; //Active dictionary list

implementation
uses Forms, Windows, AppData, JWBLanguage, JWBSettings, JWBUnit, JWBAutoImport;

{ Dicts }

{ Creates a new standardly configured dictionary object from a specified file.
Applies all default settings such as Offline/LoadOnDemand per dict settings. }
function TJaletDictionaryList.NewDict(const ADictFilename: string): TJaletDic;
var ADictName: string;
begin
  Result := TJaletDic.Create;
 {$IFNDEF NODICLOADPROMPT}
  Result.OnLoadStart := DicLoadStart;
  Result.OnLoadEnd := DicLoadEnd;
 {$ENDIF}
  Result.OnLoadException := DicLoadException;
  Result.LoadOnDemand := fSettings.CheckBox49.Checked;
  ADictName := ExtractFilename(ADictFilename);
  Result.Offline := dicts.IsInGroup(ADictName,GROUP_OFFLINE);
  try
    Result.FillInfo(ADictFilename);
  except
    Application.MessageBox(
      pchar(_l('#00321^eCannot register dictionary ')+ADictName+#13#13
        +(ExceptObject as Exception).Message),
      pchar(_l('#00020^eError')),
      MB_ICONERROR or MB_OK);
  end;
end;

{$IFNDEF NODICLOADPROMPT}
procedure TJaletDictionaryList.DicLoadStart(Sender: TObject);
begin
  DicLoadPrompt.Free; //just in case
  DicLoadPrompt := SMMessageDlg(
    _l('#00323^eDictionary loading'),
    _l('#00324^eLoading dictionary ')+TJaletDic(Sender).name+'...');
end;

procedure TJaletDictionaryList.DicLoadEnd(Sender: TObject);
begin
  FreeAndNil(DicLoadPrompt);
end;
{$ENDIF}

procedure TJaletDictionaryList.DicLoadException(Sender: TObject; E: Exception);
begin
  Application.MessageBox(
    pchar(_l('#00325^eCannot load dictionary ')+TJaletDic(Sender).name+#13#13+E.Message),
    pchar(_l('#00020^eError')),
    MB_ICONERROR or MB_OK);
end;

procedure TJaletDictionaryList.Rescan(const AIncludeDisabled: boolean = false);
var sr: TSearchRec;
  dic: TJaletDic;
  dicName: string;
begin
  Self.Clear; //unload+delete all
   //time can probably be saved on not unloading/reloadings dicts which are fine
  if FindFirst(DictionaryDir+'\*.dic', faAnyFile, sr)<>0 then
    exit;
  repeat
    if not AIncludeDisabled then begin
      dicName := ChangeFileExt(ExtractFilename(sr.Name), '');
      if Self.IsInGroup(dicName, GROUP_NOTUSED) then continue;
    end;

    dic := TJaletDictionaryList(dicts).NewDict(DictionaryDir+'\'+sr.name);
    if not dic.tested then begin
      dic.Free;
      continue; //some kind of invalid dict
    end;

    if Uppercase(ExtractFilename(dic.Filename))='JALET.DIC' then
      Application.MessageBox(
        pchar(_l('#00326^eIt is not recommended to use old style JALET.DIC dictionary.')),
        pchar(_l('#00090^eWarning')),
        MB_ICONWARNING or MB_OK);

    if curlang<>dic.language then begin
      dic.Free;
      continue;
    end;

    dicts.Add(dic);
    if not dicts.IsInGroup(dic, GROUP_NOTUSED) then
      dic.Load; //but maybe not actually DemandLoad()
  until FindNext(sr)<>0;
  SysUtils.FindClose(sr);
end;

procedure TJaletDictionaryList.AutoUpgradeListed;
var i: integer;
begin
  for i := 0 to Self.Count-1 do
    AutoUpdate(Self[i]);
end;

initialization
  dicts := TJaletDictionaryList.Create;

finalization
 {$IFDEF DEBUG}
  FreeAndNil(dicts);
 {$ENDIF}

end.
