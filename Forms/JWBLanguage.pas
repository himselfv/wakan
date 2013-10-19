unit JWBLanguage;
{
Everything related to loading, applying, choosing a translation.
Unicode-safe.
}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, TypInfo, ComCtrls, Menus, ExtCtrls, Registry;

type
  TLanguageFileInfo = record
    lName: string;
    lAuthor: string;
    lVersion: string;
  end;
  PLanguageFileInfo = ^TLanguageFileInfo;

  TFilenameArray = array of string;

  TfLanguage = class(TForm)
    Label1: TLabel;
    lbLanguages: TListBox;
    Label2: TLabel;
    Label3: TLabel;
    lbLanguageFile: TLabel;
    lbLanguageAuthor: TLabel;
    bbOk: TBitBtn;
    btnShowInfo: TButton;
    RxLabel1: TLabel;
    RxLabel2: TLabel;
    Bevel1: TBevel;
    procedure btnShowInfoClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure lbLanguagesClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  protected
   //Lists all loaded languages
    fnames: TFilenameArray; //without paths
    finfo: array of TLanguageFileInfo;

  public
    procedure LoadPerSettings; //when loading app
    procedure SaveSettings; //save changes

  protected
    procedure LocalizeEdictMarkers();
  public
    procedure LoadLanguage(fname:string); //and use for translation
    function TranslateString(id:string):string;
    function GetTlVar(id:string):string;
    procedure TranslateForm(f:TForm);
    procedure TranslateAllForms();
    procedure LocalizePropertyTypes();

  end;

var
  fLanguage: TfLanguage;

  TransDir: string; //path to translations
  curTransFile: string; //filename of active translation (without path)
  curTrans: TStringList; //active translation data
  curTransVars: TStringList;
  curTransInfo: TLanguageFileInfo;
  curGUILanguage: string; //same as curTransFile, only without an extension
    //used for stuff like wakan_LNG.chm

//Call at the start of the application
procedure InitLanguage;

implementation
uses JWBStrings, JWBEdictMarkers, JWBSettings, JWBCharData;

{$R *.DFM}

procedure InitLanguage;
begin
 //Load language or suggest to choose one
  fLanguage := TfLanguage.Create(Application);
  fLanguage.LoadPerSettings;
  fLanguage.TranslateAllForms;
end;

procedure TfLanguage.btnShowInfoClick(Sender: TObject);
begin
  Application.MessageBox(pchar('User interface translations are loaded from .LNG files'#13#13+
    'Please check WaKan website http://wakan.manga.cz'#13+
    'for new language files.'#13#13'If you want to volunteer for translating WaKan UI'#13+
    'into your language, please see WaKan forum for detailed'#13'information about how to do it.'#13#13+
    'All contributions will be greatly appreciated.'),
    'Info about UI translations',
    MB_ICONINFORMATION or MB_OK);
end;

//Adds files to array by mask
procedure ListFiles(mask: string; var files: TFilenameArray);
var sr: TSearchRec;
begin
  if FindFirst(mask,faAnyFile,sr)<>0 then exit;
  repeat
    SetLength(files, Length(files)+1);
    files[Length(files)-1] := sr.Name;
  until FindNext(sr)<>0;
  FindClose(sr);
end;

{
Loads language file into the container provided by the caller.
fname:
  Full path and file name of the translation file.
}
type
  TLoadLanguageFlag = (llOnlyInfo);
  TLoadLanguageFlags = set of TLoadLanguageFlag;

procedure LoadLanguageFile(fname: string; flags: TLoadLanguageFlags;
  out info: TLanguageFileInfo; data: TStringList; varData: TStringList = nil);
var t: TStringList; //temporary storage for .lng file contents
  i, j: integer;
  s, pref, tmp: string;
begin
  pref:='';
  if data<>nil then
    data.Clear;

  info.lName := '';
  info.lAuthor := '';
  info.lVersion := '';

  t := TStringList.Create;
  try
   //We support whatever encodings TStringList supports.
   //This includes at least Ansi and UTF-16LE and that's enough for us.
   //Don't encode translations in anything else.
    t.LoadFromFile(fname);

    for j := 0 to t.Count - 1 do begin
      s := t[j];
      tmp:=copy(s,1,6);
     { if it's a service field, read it }
      if tmp='#LANG>' then
        info.lName:=copy(s,7,length(s)-6)
      else
      if tmp='#AUTH>' then
        info.lAuthor:=copy(s,7,length(s)-6)
      else
      if tmp='#VERS>' then
        info.lVersion:=copy(s,7,length(s)-6)
      else
     { if we've been asked to read only info, skip the rest }
      if llOnlyInfo in flags then
        continue
      else
     { else it's 00016>some string }
      if length(s)<=0 then continue
      else
      if s[1]=';' then continue //comment
      else
      if (ord(s[1])>=ord('0')) and (ord(s[1])<=ord('9')) then begin
       { first char is a digit => 00042>text style line }
        if s[6]='+' then
          pref := pref+copy(s,7,length(s)-6)+#13
        else
        if s[6]='>' then
        begin
          if not TryStrToInt(copy(s,1,5), i) then
            i := 0;
          if i>0 then data.add(copy(s,1,6)+pref+copy(s,7,length(s)-6));
          pref:='';
        end;
      end else
      begin
      // first char is a letter => id=value style line --- copy as is
        if varData<>nil then
          varData.Add(s)
      end;

    end;
  finally
    t.Free;
  end;
end;

procedure TfLanguage.FormShow(Sender: TObject);
var i: integer;
begin
  lbLanguages.Items.Clear;

 //List translation files
  SetLength(fnames, 0);
  ListFiles(TransDir+'\*.lng', fnames);
  ListFiles(TransDir+'\lng\*.lng', fnames); //support this too
  if Length(fnames)<=0 then begin
   //We don't have to show this because it's perfectly okay not to have any tl files,
   //but let's be nice to the user.
    Application.MessageBox('No .LNG files were found.'#13
      +'These files must reside in Wakan folder.'#13#13
      +'Please make sure that your copy of Wakan is complete.',
      'Error',MB_ICONERROR or MB_OK);
  end;

 //Strip paths
  for i := 0 to Length(fnames) - 1 do begin
    fnames[i] := lowercase(ExtractFilename(fnames[i])); //lowercase for consistency
   //Pop English to the top
    if fnames[i]='en.lng' then begin
      fnames[i] := ''; //free string
      Move(fnames[0], fnames[1], sizeof(fnames[0])*i); //without refcounting
      pointer(fnames[0]) := nil; //without refcounting
      fnames[0]:='en.lng'; //with refcounting!
    end;
  end;

 //Load headers with info
  SetLength(finfo, Length(fnames));
  for i := 0 to Length(fnames) - 1 do begin
    if FileExists(TransDir+'\'+fnames[i]) then
      LoadLanguageFile(TransDir+'\'+fnames[i], [llOnlyInfo], finfo[i], nil)
    else
      LoadLanguageFile(TransDir+'\lng\'+fnames[i], [llOnlyInfo], finfo[i], nil);
    lbLanguages.Items.Add(finfo[i].lName+' ('+finfo[i].lVersion+')');
  end;

 //Select active language
  for i := 0 to Length(fnames) - 1 do
    if fnames[i]=curTransFile then begin
      lbLanguages.ItemIndex := i;
      lbLanguagesClick(lbLanguages);
      break;
    end;
end;

{
This one loads the language AND APPLIES IT TO THE PROGRAM.
So use with care.
fname:
  Translation file name (without path; has to be in TransDir)
}
procedure TfLanguage.LoadLanguage(fname:string);
var fullfname: string;
begin
  if FileExists(TransDir+'\'+fname) then
    fullfname := TransDir+'\'+fname
  else
  if FileExists(TransDir+'\lng\'+fname) then
    fullfname := TransDir+'\lng\'+fname
  else
  begin
    Application.MessageBox(pchar('GUI translation file '+fname+' was not found. '
      +'It should be located in Wakan folder.'#13'No translation has been loaded.'),
      'Error', MB_ICONWARNING or MB_OK);
    exit;
  end;

  curTransFile := fname;
  curGUILanguage := ChangeFileExt(lowercase(fname), '');
  LoadLanguageFile(fullfname, [], curTransInfo, curTrans, curTransVars);
  LocalizeEdictMarkers();
  LocalizePropertyTypes();
end;

procedure TfLanguage.lbLanguagesClick(Sender: TObject);
begin
  lbLanguageFile.Caption:=fnames[lbLanguages.ItemIndex];
  lbLanguageAuthor.Caption:=finfo[lbLanguages.ItemIndex].lAuthor;
end;

{
Reads registry settings and loads appropriate language.
If it's not configured, asks the user to choose one.
}
procedure TfLanguage.LoadPerSettings;
begin
  curTransFile := fSettings.GetTranslationFile;
  if curTransFile='' then begin
    Self.ShowModal;
    if curTransFile<>'' then //user cancelled
      curTransFile := '?'; //can't be a filename; means: "don't need tl"
    raise EAbort.Create('');
  end;
  if curTransFile <> '?' then
    Self.LoadLanguage(curTransFile);
end;

procedure TfLanguage.SaveSettings;
begin
  fSettings.SetTranslationFile(curTransFile);
end;

{
Save the choice and apply the language, OR -- cancel and exit.
}
procedure TfLanguage.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if not IsPositiveResult(ModalResult) then exit;

 //Else save the choice and load language
  if lbLanguages.ItemIndex>=0 then begin
    curTransFile := fnames[lbLanguages.ItemIndex];
    SaveSettings();
    LoadLanguage(curTransFile); //not really needed right now...

   //Currently there's no choice but to do this
    showmessage('Language has been changed. Please restart the application.');
    Application.Terminate;
  end;
end;

function TfLanguage.TranslateString(id:string):string;
var i,sk,m,l,r:integer;
    s:string;
begin
  result:=id;
  if (length(id)>6) and (id[1]='#') then
  begin
    sk:=0;
    while pos('&',id)>0 do delete(id,pos('&',id),1);
    try sk:=strtoint(copy(id,2,5)); except end;
    if sk>0 then
    begin
      l:=0;
      r:=curtrans.Count-1;
      while l<=r do
      begin
        m:=(l+r) div 2;
        try i:=strtoint(copy(curtrans[m],1,5)); except showmessage('.LNG file corrupted!'); end;
        if sk<i then r:=m-1 else if sk>i then l:=m+1 else break;
      end;
      if l<=r then result:=copy(curtrans[m],7,length(curtrans[m])-6);
    end;
  end;
  if (pos('^e',result)>0) then
  begin
    s:=result;
    result:='';
    if pos('^e',s)=0 then exit;
    i:=pos('^e',s);
    i:=i+2;
    while (i<=length(s)) do
    begin
      if s[i]='^'then break;
      result:=result+s[i];
      inc(i);
    end;
  end;
end;

function TfLanguage.GetTlVar(id:string):string;
var i: integer;
begin
  i := curTransVars.IndexOfName(id);
  if i<0 then
    Result := ''
  else
    Result := curTransVars.ValueFromIndex[i];
end;

function IsStringProperty(PropInfo: PPropInfo): Boolean;
var aPropInfo:TPropInfo;
    ppType:PPTypeInfo;
    pType:PTypeInfo;
    TypeInfo:TTypeInfo;
begin
  aPropInfo:=PropInfo^;
  ppType:=aPropInfo.PropType;
  pType:=ppType^;
  TypeInfo:=pType^;
  Result:=(TypeInfo.Kind=tkString)
    or (TypeInfo.Kind=tkLString)
    or (TypeInfo.Kind=tkWString)
  {$IFDEF UNICODE}
    or (TypeInfo.Kind=tkUString)
  {$ENDIF};
end;

procedure TfLanguage.TranslateForm(f:TForm);
var j,k:integer;
  a:TComponent;
  PropInfo:PPropInfo;

  procedure _set(a:TObject;prop:string);
  begin
    if a is TControl then
    begin
      PropInfo:=GetPropInfo(a.ClassInfo,prop);
      if (PropInfo<>nil) and IsStringProperty(PropInfo) then
        SetStrProp(a,PropInfo,TranslateString(GetStrProp(a,PropInfo)));
    end;
  end;

  procedure _setlist(a:TObject;prop:string);
  var ss:TStringList;
      i:integer;
  begin
    PropInfo:=GetPropInfo(a.ClassInfo,prop);
    if PropInfo<>nil then
    begin
      ss:=TStringList(GetOrdProp(a,PropInfo));
      for i:=0 to ss.Count-1 do
        ss[i]:=TranslateString(ss[i]);
    end;
  end;

  procedure _tlnode(a:TTreeNode);
  var i: integer;
  begin
    a.Text:=TranslateString(a.Text);
    for i := 0 to a.Count - 1 do
      _tlnode(a.Item[i]);
  end;

  procedure _tlnodes(a:TTreeView);
  var i: integer;
  begin
    a.Items.BeginUpdate;
    for i := 0 to a.Items.Count - 1 do
      _tlnode(a.Items[i]);
    a.Items.EndUpdate; //this also triggers scrollbar readjustment! need this.
  end;

  procedure _menuitem(mi:TMenuItem);
  var i:integer;
  begin
    for i:=0 to mi.Count-1 do _menuitem(mi.Items[i]);
    mi.Caption:=TranslateString(mi.Caption);
  end;

begin
  _set(f,'Caption');
  for j:=0 to f.ComponentCount-1 do
  begin
    a:=f.Components[j];
    _set(a,'Caption');
    _set(a,'Text');
    _set(a,'Hint');
    if (a is TCustomListBox) or
       (a is TCustomComboBox) or
       (a is TCustomMemo) or
       (a is TCustomRadioGroup)
    then begin
      _setlist(a,'Lines');
      _setlist(a,'Items');
    end;
    if a is TMenu then
    begin
      for k:=0 to (a as TMenu).Items.Count-1 do
        _menuitem((a as TMenu).Items[k]);
    end;
    if a is TTabControl then
      _setlist(a,'Tabs');
    if a is TCustomTreeView then
      _tlnodes(TTreeView(a));
  end;
end;

procedure TfLanguage.TranslateAllForms();
var i: integer;
begin
  for i := 0 to Screen.FormCount - 1 do
    Self.TranslateForm(Screen.Forms[i]);
end;

procedure TfLanguage.LocalizeEdictMarkers();
var i: integer;
begin
  for i := 0 to Length(EdictMarkers) - 1 do
    EdictMarkers[i].abl := GetTlVar('mark-'+EdictMarkers[i].m);
end;

procedure TfLanguage.LocalizePropertyTypes();
var i, i_pos: integer;
  tmp: string;
begin
  for i := 0 to Length(CharPropTypes) - 1 do begin
    tmp := GetTlVar('cprop-'+IntToStr(CharPropTypes[i].id));
    if tmp='' then continue;
    i_pos := pos(',',tmp);
    if i_pos<=0 then begin
      CharPropTypes[i].englishName := tmp;
      CharPropTypes[i].description := '';
    end else begin
      CharPropTypes[i].englishName := copy(tmp,1,i_pos-1);
      CharPropTypes[i].description := copy(tmp,i_pos+1,MaxInt);
    end;
  end;
end;


type
  CComponent = class of TComponent;
  TFormTranslated = class(TComponent);
  TFormHelper = class helper for TForm
    function FindComponent(const AClass: CComponent): integer;
  end;

function TFormHelper.FindComponent(const AClass: CComponent): integer;
var i: integer;
begin
  Result := -1;
  for i := 0 to ComponentCount-1 do
    if Components[i].ClassType=AClass then begin
      Result := i;
      break;
    end;
end;

initialization
  TransDir := ExtractFilePath(GetModuleFilenameStr(0));
  curTransFile := '';
  curTrans := TStringList.Create;
  curTransVars := TStringList.Create;
  FillChar(curTransInfo, SizeOf(curTransInfo), 00);
  curGUILanguage := '';

finalization
  curTrans.Free;

end.
