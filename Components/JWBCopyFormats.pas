unit JWBCopyFormats;
{ Template language for copying dictionary results with Ctrl-C. }

interface
uses SysUtils, Classes, IniFiles, JWBDicSearch;

type
  TParsedFlags = array of string;
  TParsedClause = record
    id: string;
    text: string;
    flags: TParsedFlags;
  end;
  PParsedClause = ^TParsedClause;
  TParsedArticle = record
    clauses: array of TParsedClause;
    dict: string;
  end;
  PParsedArticle = ^TParsedArticle;
  TParsedResult = record
    expression: string;
    reading: string;
    articles: array of TParsedArticle;
  end;

  TCopyFormat = class
  protected
    FName: string;
    FTemplates: TStringList;
    function ParseResult(const res: PSearchResult): TParsedResult;
    function ParseArticle(const s: string): TParsedArticle;
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
    property Count: integer read GetCount;
    property Items[const Index: integer]: TCopyFormat read GetItem; default;
  end;

  ETemplateException = class(Exception);

var
  CopyFormats: TCopyFormats;

implementation
uses JWBStrings;

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
var pres: TParsedResult;
  art: PParsedArticle;
  cla: PParsedClause;
  values: TStringList;
  i, j, k: integer;
  articles_text: string;
  clauses_text: string;
  flags_text: string;
begin
  pres := ParseResult(res);

  values := TStringList.Create;
  try
    articles_text := '';
    for i := 0 to Length(pres.articles)-1 do begin
      art := @pres.articles[i];

      clauses_text := '';
      for j := 0 to Length(art.clauses)-1 do begin
        cla := @art.clauses[j];

        flags_text := '';
        for k := 0 to Length(cla.flags)-1 do begin
          values.Clear;
          values.Values['text'] := cla.flags[k];
          if k=0 then values.Values['first'] := 'true';
          if k=Length(cla.flags)-1 then values.Values['last'] := 'true';
          flags_text := flags_text + ApplyTemplate(Templates.Values['flag'], values);
        end;

        values.Clear;
        values.Values['id'] := cla.id;
        values.Values['text'] := cla.text;
        values.Values['flags'] := flags_text;
        if j=0 then values.Values['first'] := 'true';
        if j=Length(art.clauses)-1 then values.Values['last'] := 'true';
        clauses_text := clauses_text + ApplyTemplate(Templates.Values['clause'], values);
      end;

      values.Clear;
      values.Values['clauses'] := clauses_text;
      values.Values['dict'] := pres.articles[i].dict;
      if i=0 then values.Values['first'] := 'true';
      if i=Length(pres.articles)-1 then values.Values['last'] := 'true';
      articles_text := articles_text + ApplyTemplate(Templates.Values['article'], values);
    end;

    values.Clear;
    values.Values['expr'] := pres.expression;
    values.Values['read'] := pres.reading;
    values.Values['articles'] := articles_text;
    values.Values['last'] := 'true';
    Result := ApplyTemplate(Templates.Values['entry'], values);

  finally
    FreeAndNil(Values);
  end;
end;


{ Splits what we have from TSearchResult into more detailed parts. }
function TCopyFormat.ParseResult(const res: PSearchResult): TParsedResult;
var parts: TStringArray;
  tmp: string;
  i: integer;
begin
  Result.expression := res.kanji;
  Result.reading := res.kana;
  tmp := remexcl(replc(res.entry, '//', '/'));
//    if pos(' >> ',tmp)>0 then delete(tmp,1,pos(' >> ',tmp)+3); //not sure if needed
  parts := SplitStr(tmp, '/');
  SetLength(Result.articles, Length(parts));
  for i := 0 to Length(parts)-1 do begin
    parts[i] := Trim(parts[i]);
    Result.articles[i] := ParseArticle(parts[i]);
  end;
end;

function TCopyFormat.ParseArticle(const s: string): TParsedArticle;
var ps, pc: PChar;
  cla: PParsedClause;
  flags: TParsedFlags;
  ftype: char;

  procedure NeedClause;
  begin
    if cla<>nil then exit; //don't create clause for nothing
    SetLength(Result.clauses, Length(Result.clauses)+1);
    cla := @Result.clauses[Length(Result.clauses)-1];
    cla.id := '';
    cla.text := '';
    cla.flags := nil;
  end;

  procedure EndClause;
  begin
    if cla<>nil then
      cla.text := Trim(cla.text);
    cla := nil;
  end;

  procedure CommitText;
  begin
    if pc<=ps then exit;
    NeedClause;
    cla.text := cla.text + spancopy(ps,pc);
  end;

  procedure CommitTrimSpace;
  begin
   //We're going to cut something from this point on,
   //so maybe trim a space at the end of the previous block
    Dec(pc);
    if pc^=' ' then begin
      CommitText;
      Inc(pc);
    end else begin
      Inc(pc);
      CommitText;
    end;
  end;

  procedure CommitFlags;
  begin
    if Length(flags)<=0 then exit; //don't create clause for nothing
    NeedClause;
    cla.flags := flags;
    flags := nil;
  end;

  procedure IncTrimSpace(var pc: PChar);
  begin
   //We have cut something before this point in the string,
   //so maybe trim a space at the start of the next block
    Inc(pc);
    if pc^=' ' then begin
      Inc(pc);
      if pc^<>' ' then
        Dec(pc);
    end;
  end;

begin
  Result.clauses := nil;
  Result.dict := '';
  if s='' then exit;

  cla := nil;
  flags := nil;

  pc := PChar(s);
  ps := pc;
  while pc^<>#00 do

    if pc^=UH_LBEG then begin
      CommitTrimSpace;
      Inc(pc);
      ps := pc;
      while (pc^<>#00) and (pc^<>UH_LEND) do
        Inc(pc);
      if pc>ps then begin
       //First char of the flag is always its type
        ftype := ps^;
        Inc(ps);
      end else
        ftype := 's';
      if pc>ps then
        if ftype='d' then begin //dictionary
          Result.dict := spancopy(ps,pc);
        end else begin
          SetLength(flags, Length(flags)+1);
          flags[Length(flags)-1] := spancopy(ps,pc);
        end;
      IncTrimSpace(pc);
      ps := pc;
    end else

    if (pc^='(') and IsDigit(pc[1]) and (
      (pc[2]=')') or ( IsDigit(pc[2]) and (pc[3]=')')  )
    ) then begin
      CommitTrimSpace;
      CommitFlags;
      EndClause;
      NeedClause;
      if pc[2]=')' then begin
        cla.id := pc[1];
        Inc(pc,3);
      end else begin
        cla.id := pc[1]+pc[2];
        Inc(pc,4);
      end;
      if pc^=' ' then Inc(pc);
      ps := pc;
    end else

      Inc(pc); //any other char

  CommitText;
  CommitFlags;
  EndClause;
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
