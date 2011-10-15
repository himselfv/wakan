unit JWBLanguage;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, TypInfo, ComCtrls, Menus, ExtCtrls, Registry, RXCtrls;

type
  TfLanguage = class(TForm)
    Label1: TLabel;
    ListBox1: TListBox;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    BitBtn1: TBitBtn;
    Button1: TButton;
    RxLabel1: TRxLabel;
    RxLabel2: TRxLabel;
    Bevel1: TBevel;
    procedure Button1Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ListBox1Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
  public
    lfiles:TStringList;
    curtrans:TStringList;
    curlanguage:string;
    function LoadLanguage(fname:string):string;
    function TranslateString(id:string):string;
    procedure TranslateForm(f:TForm);
    { Public declarations }
  end;

var
  fLanguage: TfLanguage;
  curGUILanguage: string;

implementation

{$R *.DFM}

procedure TfLanguage.Button1Click(Sender: TObject);
begin
  Application.MessageBox(pchar('User interface translations are loaded from .LNG files'#13+
    #13#13'Please check WaKan website http://wakan.manga.cz'#13+
    'for new language files.'#13#13'If you want to volunteer for translating WaKan UI'#13+
    'into your language, please see WaKan forum for detailed'#13'information about how to do it.'#13#13+
    'All contributions will be greatly appreciated.'),'Info about UI translations',MB_ICONINFORMATION or MB_OK);
end;

procedure TfLanguage.FormShow(Sender: TObject);
var sr:TSearchRec;
    t:textfile;
    s,lg,aut,ver:string;
    ind,i:integer;
begin
  ListBox1.Items.Clear;
  lfiles.Clear;
  ind:=0;
  if FindFirst('*.lng',faAnyFile,sr)<>0 then
  begin
    Application.MessageBox('No .LNG files were found.'#13'These files must reside in Wakan folder.'#13#13'Please ensure that your copy of Wakan is complete.',
      'Error',MB_ICONERROR or MB_OK);
    Application.Terminate;
    exit;
  end else
  repeat
    assignfile(t,sr.name);
    lg:='!';
    reset(t);
    while not eof(t) do
    begin
      readln(t,s);
      if copy(s,1,6)='#LANG>'then lg:=copy(s,7,length(s)-6);
      if copy(s,1,6)='#AUTH>'then aut:=copy(s,7,length(s)-6);
      if copy(s,1,6)='#VERS>'then ver:=copy(s,7,length(s)-6);
    end;
    closefile(t);
    if sr.name='en.lng'then
    begin
      ListBox1.Items.Insert(0,lg+' ('+ver+')');
      lfiles.Insert(0,sr.name);
    end else
    begin
      ListBox1.Items.Add(lg+' ('+ver+')');
      lfiles.Add(sr.name);
    end;
  until FindNext(sr)<>0;
  ind:=0;
  for i:=0 to lfiles.Count-1 do if sr.name=lfiles[i] then ind:=i;
  FindClose(sr);
  ListBox1.ItemIndex:=ind;
  ListBox1Click(sender);
end;

procedure TfLanguage.FormCreate(Sender: TObject);
begin
  lfiles:=TStringList.Create;
  curtrans:=TStringList.Create;
end;

function TfLanguage.LoadLanguage(fname:string):string;
var t:textfile;
  pref,s: string;
  i:integer;
  aut,ver,lang: string; { service fields }
begin
  if not fileexists(fname) then
  begin
    Application.MessageBox(pchar('GUI translation file '+fname+' was not found. '
      +'It should be located in Wakan folder.'#13'No translation has been loaded.'),
      'Error', MB_ICONWARNING or MB_OK);
    exit;
  end;
  curGUILanguage:=lowercase(fname);
  if pos('.',curGUILanguage)>0 then curGUILanguage:=copy(curGUILanguage,1,pos('.',curGUILanguage)-1);
  assignfile(t,fname);
  curtrans.Clear;
  pref:='';
  reset(t);
  while not eof(t) do
  begin
    readln(t,s);
   { if it's a service field, read it }
    if copy(s,1,6)='#LANG>' then
      lang := copy(s,7,length(s)-6)
    else
    if copy(s,1,6)='#AUTH>' then
      aut := copy(s,7,length(s)-6)
    else
    if copy(s,1,6)='#VERS>' then
      ver := copy(s,7,length(s)-6)
    else
   { else it's 00016>some string }
    if (length(s)>0) and (s[1]<>';') then
      if s[6]='+' then
        pref := pref+copy(s,7,length(s)-6)+#13
      else
      if s[6]='>' then
      begin
        i:=0;
        try i:=strtoint(copy(s,1,5)); except end;
        if i>0 then curtrans.add(copy(s,1,6)+pref+copy(s,7,length(s)-6));
        pref:='';
      end;
  end;
  closefile(t);
  result:=aut;
end;

procedure TfLanguage.ListBox1Click(Sender: TObject);
begin
  Label5.Caption:=LoadLanguage(lfiles[ListBox1.ItemIndex]);
  Label4.Caption:=lfiles[ListBox1.ItemIndex];
end;

procedure TfLanguage.FormDestroy(Sender: TObject);
begin
  lfiles.Free;
  curtrans.Free;
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

function StringProperty(PropInfo : PPropInfo) : Boolean;
var aPropInfo:TPropInfo;
    ppType:PPTypeInfo;
    pType:PTypeInfo;
    TypeInfo:TTypeInfo;
begin
  aPropInfo:=PropInfo^;
  ppType:=aPropInfo.PropType;
  pType:=ppType^;
  TypeInfo:=pType^;
  Result:=(TypeInfo.Kind=tkString) or (TypeInfo.Kind=tkLString) or
     (TypeInfo.Kind=tkWString);
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
    if (PropInfo<>nil) and (StringProperty(PropInfo)) then
    begin
      SetStrProp(a,PropInfo,TranslateString(GetStrProp(a,PropInfo)));
    end;
  end;
end;
procedure _setlist(a:TObject;prop:string);
var ss:TStringList;
    i:integer;
begin
  if (a is TCustomListBox) or
     (a is TCustomComboBox) or
     (a is TCustomMemo) or
     (a is TCustomRadioGroup)
     then
  begin
    PropInfo:=GetPropInfo(a.ClassInfo,prop);
    if (PropInfo<>nil) then
    begin
      ss:=TStringList(GetOrdProp(a,PropInfo));
      for i:=0 to ss.Count-1 do
        ss[i]:=TranslateString(ss[i]);
    end;
  end;
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
    _setlist(a,'Lines');
    _setlist(a,'Items');
    if a is TMenu then
    begin
      for k:=0 to (a as TMenu).Items.Count-1 do
        _menuitem((a as TMenu).Items[k]);
    end;
  end;
end;

procedure TfLanguage.FormClose(Sender: TObject; var Action: TCloseAction);
var reg:TRegIniFile;
begin
  curLanguage:=lfiles[ListBox1.ItemIndex];
  reg:=TRegIniFile.Create('Software\Labyrinth\Wakan');
  reg.WriteString('Language','LNGFile',curLanguage);
  reg.Free;
end;

end.
