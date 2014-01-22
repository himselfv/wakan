unit JWBCopyFormats;
{ Template language for copying dictionary results with Ctrl-C. }

interface
uses SysUtils, Classes, IniFiles, JWBDic, JWBDicSearch;

type
  TCopyFormat = class
  protected
    FName: string;
    FTemplates: TStringList;
    function ApplyTemplate(templ: string; const values: TStringList): string;
  public
    constructor Create;
    destructor Destroy; override;
    function FormatResult(const res: PSearchResult): string;
    property Name: string read FName;
    property Templates: TStringList read FTemplates;
  end;

  TCopyFormats = class
  protected
    FItems: array of TCopyFormat;
    function GetCount: integer; inline;
    function GetItem(const Index: integer): TCopyFormat; inline;
  public
    destructor Destroy; override;
    procedure Clear;
    procedure LoadFromIni(reg: TCustomIniFile);
    procedure SaveToIni(reg: TCustomIniFile);
    procedure LoadFromFile(const AFilename: string);
    procedure SaveToFile(const AFilename: string);
    function Find(const AFormat: string): integer;
    property Count: integer read GetCount;
    property Items[const Index: integer]: TCopyFormat read GetItem; default;
  end;

  ETemplateException = class(Exception);

var
  CopyFormats: TCopyFormats;

implementation
uses JWBStrings, JWBEdictMarkers, JWBLegacyMarkup;

type
  TStringListHelper = class helper for TStringList
  public
    function GetValueByNameDef(const AName: string; const ADefault: string = ''): string;
  end;

function TStringListHelper.GetValueByNameDef(const AName: string; const ADefault: string = ''): string;
var i: integer;
begin
  i := Self.IndexOfName(AName);
  if i>=0 then
    Result := Self.ValueFromIndex[i]
  else
    Result := ADefault;
end;

destructor TCopyFormats.Destroy;
begin
  Clear;
  inherited;
end;

function TCopyFormats.GetCount: integer;
begin
  Result := Length(FItems);
end;

function TCopyFormats.GetItem(const Index: integer): TCopyFormat;
begin
  Result := FItems[Index];
end;

procedure TCopyFormats.Clear;
var i: integer;
begin
  for i := 0 to Length(FItems)-1 do
    FreeAndNil(FItems[i]);
  SetLength(FItems, 0);
end;

procedure TCopyFormats.LoadFromIni(reg: TCustomIniFile);
var sections: TStringList;
  i, j: integer;
  item: TCopyFormat;
begin
  sections := TStringList.Create;
  try
    reg.ReadSections(sections);
    SetLength(FItems, sections.Count);
    for i := 0 to sections.Count-1 do begin
      item := TCopyFormat.Create;
      item.FName := sections[i];
      reg.ReadSection(item.FName, item.FTemplates);
      for j := 0 to item.FTemplates.Count-1 do
        item.FTemplates[j] := item.FTemplates[j]+'='+reg.ReadString(item.Name, item.FTemplates[j], '');
      FItems[i] := item;
    end;
  finally
    FreeAndNil(sections);
  end;
end;

procedure TCopyFormats.SaveToIni(reg: TCustomIniFile);
var i, j: integer;
  item: TCopyFormat;
begin
  for i := 0 to Count-1 do begin
    item := Items[i];
    for j := 0 to item.Templates.Count do
      reg.WriteString(item.Name, item.Templates.Names[j], item.Templates.ValueFromIndex[j]);
  end;
end;

procedure TCopyFormats.LoadFromFile(const AFilename: string);
var ini: TMemIniFile;
begin
  ini := TMemIniFile.Create(AFilename, nil);
  try
    LoadFromIni(ini);
  finally
    FreeAndNil(ini);
  end;
end;

procedure TCopyFormats.SaveToFile(const AFilename: string);
var ini: TMemIniFile;
begin
  ini := TMemIniFile.Create(AFilename, nil);
  try
    TMemIniFile(ini).Encoding := SysUtils.TEncoding.UTF8; //write UTF8 only
    SaveToIni(ini);
  finally
    FreeAndNil(ini);
  end;
end;

function TCopyFormats.Find(const AFormat: string): integer;
var i: integer;
begin
  Result := -1;
  for i := 0 to Count-1 do
    if Items[i].Name=AFormat then begin
      Result := i;
      break;
    end;
end;

constructor TCopyFormat.Create;
begin
  inherited Create;
  FTemplates := TStringList.Create;
end;

destructor TCopyFormat.Destroy;
begin
  FreeAndNil(FTemplates);
  inherited;
end;

function TCopyFormat.FormatResult(const res: PSearchResult): string;
var
  t_clause, t_gloss,
  t_marker, t_comment, t_group: string;
  art: PSearchResArticle;
  cla: PEntry;
  values: TStringList;
  i, j, k: integer;
  articles_text: string;
  clauses_text: string;
  glosses: TStringArray;
  clause: string;
  tmp: string;
begin
  t_clause := Templates.GetValueByNameDef('clause', '(%id) %text%{%last%?!; }');
  t_marker := Templates.GetValueByNameDef('marker', '<%text%>');
  t_comment := Templates.GetValueByNameDef('comment', '(%text%)');
  t_gloss := Templates.GetValueByNameDef('gloss', '%text%{%last%!?; }');
  t_group := Templates.GetValueByNameDef('group', '');

  values := TStringList.Create;
  try
    articles_text := '';
    for i := 0 to Length(res.articles)-1 do begin
      art := @res.articles[i];

      clauses_text := '';
      for j := 0 to art.entries.Count-1 do begin
        cla := @art.entries.items[j];

        clause := '';
        glosses := SplitGlosses(cla.text);
        for k := 0 to Length(glosses)-1 do begin
          values.Clear;
          values.Values['text'] := glosses[k];
          if k=0 then values.Values['first'] := 'true';
          if k=Length(glosses)-1 then values.Values['last'] := 'true';
          clause := clause + ApplyTemplate(t_gloss, values);
        end;

        clause := MatchMarkers(clause,
          function (const s: string): string
          var templ: string;
          begin
            if Length(s)<1 then begin
              Result := '';
              exit;
            end;

            Result := s;
            case Result[1] of
              '1','g': templ := t_marker;
              's': templ := t_comment;
              'l': templ := t_group;
            else exit; //not a marker
            end;

            delete(Result,1,1);
            values.Clear;
            values.Values['text'] := Result;
            values.Values['first'] := 'true';
            values.Values['last'] := 'true';
            Result := ApplyTemplate(templ, values);
          end
        );

        for k := 1 to Length(cla.markers) do begin
          values.Clear;
          tmp := GetMarkAbbr(cla.markers[k]);
          delete(tmp,1,1);
          values.Values['text'] := tmp;
          if k=1 then values.Values['first'] := 'true';
          if k=Length(cla.markers) then values.Values['last'] := 'true';
          clause := clause + ApplyTemplate(t_marker, values);
        end;

        values.Clear;
        values.Values['id'] := IntToStr(j+1);
        values.Values['text'] := clause;
        if j=0 then values.Values['first'] := 'true';
        if j=art.entries.Count-1 then values.Values['last'] := 'true';
        clauses_text := clauses_text + ApplyTemplate(Templates.Values['clause'], values);
      end;

      values.Clear;
      values.Values['clauses'] := clauses_text;
      values.Values['dict'] := art.dicname;
      if i=0 then values.Values['first'] := 'true';
      if i=Length(res.articles)-1 then values.Values['last'] := 'true';
      articles_text := articles_text + ApplyTemplate(Templates.Values['article'], values);
    end;

    values.Clear;
    values.Values['expr'] := res.kanji;
    values.Values['read'] := res.kana;
    values.Values['articles'] := articles_text;
    values.Values['last'] := 'true';
    Result := ApplyTemplate(Templates.Values['result'], values);

  finally
    FreeAndNil(Values);
  end;
end;

(*
Recieves a template and a set of values and fills the resulting string accordingly.
Format:
 %name% - insert variable
 {%name%?text} - insert text only if %name% is not empty
 {%name%!?text} - insert text only if %name% is empty
 #13 - character by code
 \ - escape character
*)
procedure Die(const msg: string);
begin
  raise ETemplateException.Create(msg);
end;

procedure Check(const AValue: boolean; const msg: string); inline;
begin
  if not AValue then Die(msg);
end;

function EatVarname(var pc: PChar): string;
var ps: PChar;
begin
  ps := pc;
  while IsLatinLetter(pc^) or IsDigit(pc^) do
    Inc(pc);
  Check(pc>ps, 'Empty variable name');
  Result := spancopy(ps,pc);
end;

function EatNumber(var pc: PChar): integer;
begin
  Result := 0;
  while IsDigit(pc^) do begin
    Result := Result*10 + (Ord(pc^)-Ord('0'));
    Inc(pc);
  end;
end;

function TCopyFormat.ApplyTemplate(templ: string; const values: TStringList): string;
var pc: PChar;
  bracket_lvl: integer;
  vname: string;
  vnot: boolean;
  num: integer;
begin
  Result := '';
  if templ='' then exit;

  bracket_lvl := 0;

  pc := PChar(templ);
  while pc^<>'' do begin

    if (pc^='#') and IsDigit(pc[1]) then begin
      Inc(pc);
      num := EatNumber(pc);
      Result := Result + Chr(num);
    end else

    if pc^='\' then begin
      Inc(pc);
      Check(pc^<>#00, 'Unterminated escape character');
      Result := Result + pc^;
      Inc(pc^);
    end else

    if pc^='%' then begin
      Inc(pc);
      vname := EatVarname(pc);
      Check(pc^='%', 'Unterminated variable name');
      Inc(pc);
      Result := Result + values.Values[vname];
    end else

    if pc^='{' then begin
      Inc(pc);
      Inc(bracket_lvl);
      Check(pc^='%', 'Conditional bracket without condition variable');
      Inc(pc);
      vname := EatVarname(pc);
      Check(pc^='%', 'Unterminated variable name');
      Inc(pc);
      if pc^='!' then begin
        vnot := true;
        Inc(pc);
      end else
        vnot := false;
      Check(pc^='?', 'Conditional bracket lacks question mark');
      Inc(pc);

      if vnot xor (values.Values[vname]='') then begin
       //Condition not satisfied; skip here and now
        num := bracket_lvl;
        while bracket_lvl>=num do begin
          Check(pc^<>#00, 'Invalid unterminated conditional bracket');
          if pc^='{' then
            Inc(bracket_lvl)
          else
          if pc^='}' then
            Dec(bracket_lvl);
          Inc(pc);
        end;
      end;

    end else

    if pc^='}' then begin
      Dec(bracket_lvl);
      Check(bracket_lvl>=0, 'Unexpected closing conditional bracket');
      Inc(pc);
    end else

    begin
      Result := Result + pc^;
      Inc(pc);
    end;
  end;

  Check(bracket_lvl=0, 'Invalid unterminated conditional bracket');
end;

initialization
  CopyFormats := TCopyFormats.Create;

finalization
 {$IFDEF CLEAN_DEINIT}
  FreeAndNil(CopyFormats);
 {$ENDIF}

end.
