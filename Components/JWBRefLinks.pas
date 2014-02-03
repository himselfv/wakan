unit JWBRefLinks;
{ Manages reference links for both kanji and expressions.
How to use:
  CharacterLinks.Add(TRefLink.FromString('link template from registry or config'))
  for i := 0 to ExpressionLinks.Count-1 do
    ...
    FormatReferenceLinkText(link.Caption, kanji);
    ShellOpen(FormatReferenceLinkText(link.URL, kanji));
}

interface
uses Classes, Generics.Collections, Menus,
{$IFDEF MSWINDOWS}
  ShellAPI, Winapi.Windows
{$ENDIF MSWINDOWS}
{$IFDEF POSIX}
  Posix.Stdlib
{$ENDIF POSIX}
;

type
  TRefLinkType = (ltJapaneseOnly, ltChineseOnly, ltAll);
  TRefLink = class
  public
    LinkType: TRefLinkType;
    Caption: string;
    Hint: string;
    URL: string;
    constructor FromString(const ADeclaration: string);
    function MatchesLang(const ACurLang: char): boolean;
  end;

  TRefLinkList = class(TObjectList<TRefLink>);

{ Pre-created lists. }
var
  CharacterLinks: TRefLinkList;
  ExpressionLinks: TRefLinkList;

{ Formats any of "Caption, Hint, URL", inserting actual character/expression }
function FormatReferenceLinkText(AText: string; AData: string): string;

type
 { TMenuItem tailored to open the specified link when clicked }
  TRefMenuItem = class(TMenuItem)
  protected
    FRefLink: TRefLink;
    FURL: string;
  public
    constructor Create(AOwner: TComponent; ARefLink: TRefLink; AData: string); reintroduce;
    procedure Click; override;
  end;

procedure ShellOpen(const sCommand: string);

implementation
uses SysUtils, JWBStrings;

resourcestring
  eInvalidReferenceLinkDeclaration = 'Invalid reference link declaration: %s';

constructor TRefLink.FromString(const ADeclaration: string);
var ps, pc: PChar;
  part_id: integer;
  part: string;
  flag_specsymbol: boolean;

  procedure CommitText;
  begin
    part := part + spancopy(ps, pc);
  end;

  procedure FinalizePart;
  begin
    CommitText;
    part := Trim(part);
    case part_id of
      0: begin
        if Length(part)<>1 then
          raise Exception.CreateFmt(eInvalidReferenceLinkDeclaration,[ADeclaration]);
        case UpperCase(part)[1] of
         'A': LinkType := ltAll;
         'J': LinkType := ltJapaneseOnly;
         'C': LinkType := ltChineseOnly;
        else
          raise Exception.CreateFmt(eInvalidReferenceLinkDeclaration,[ADeclaration]);
        end;
      end;

      1: Caption := part;
      2: Hint := part;
      3: URL := part;
    else
      raise Exception.CreateFmt(eInvalidReferenceLinkDeclaration,[ADeclaration]);
    end;
    part := '';
  end;

begin
  part_id := 0;
  part := '';
  flag_specsymbol := false;
  ps := PChar(ADeclaration);
  pc := ps;
  while (pc^<>#00) and (part_id<4) do
    if flag_specsymbol then begin
      part := part + pc^;
      flag_specsymbol := false;
      Inc(pc);
      ps := pc;
    end else
    if pc^='\' then begin
      Inc(pc);
      CommitText; //and keep the specsymbol opener, it's needed later
      flag_specsymbol := true;
      ps := pc;
    end else
    if pc^=',' then begin
      FinalizePart();
      Inc(part_id);
      Inc(pc);
      ps := pc;
    end else
      Inc(pc);
  if pc^<>#00 then
    raise Exception.CreateFmt(eInvalidReferenceLinkDeclaration, [ADeclaration]);
  FinalizePart();
end;

//True if this reference link should be shown for the specified target language
function TRefLink.MatchesLang(const ACurLang: char): boolean;
begin
  case LinkType of
    ltJapaneseOnly: Result := ACurLang='j';
    ltChineseOnly: Result := ACurLang='c';
    ltAll: Result := true;
  else Result := false;
  end;
end;

//Formats reference link text (Caption, Hint, Link), inserting data in the
//appropriate formats where needed
function FormatReferenceLinkText(AText: string; AData: string): string;
var ps, pc: PChar;
  flag_specsymbol: boolean;
  form: string;

  procedure CommitText;
  begin
    Result := Result + spancopy(ps,pc);
  end;

begin
  Result := '';
  AText := Trim(AText);
  if AText='' then exit;

  flag_specsymbol := false;
  ps := PChar(AText);
  pc := ps;
  while pc^<>#00 do begin
    if flag_specsymbol then begin
      Result := Result + pc^;
      flag_specsymbol := false;
      Inc(pc);
      ps := pc;
    end else
    if pc^='\' then begin
      CommitText; //until before the specsymbol opener
      flag_specsymbol := true;
      Inc(pc); //skip the opener
      ps := pc;
    end else
    if pc^='%' then begin
      CommitText;
      Inc(pc);

     {
      Two forms supported:
        %s                  only for s
        %s:urlencode%
     }
      if pc^<>'s' then exit; //invalid format string
      Inc(pc);
      if pc^=':' then begin
        Inc(pc);
        ps := pc;
        while (pc^<>#00) and (pc^<>'%') do
          Inc(pc);
        if pc^=#00 then exit; //invalid format string
        form := spancopy(ps,pc);
        Inc(pc);
      end else
        form := '';

      if form='' then
        Result := Result + AData
      else
      if form='hex' then
        Result := Result + UnicodeToHex(AData)
      else
      if form='urlencode' then
        Result := Result + string(URLEncode(AData, []))
      else
        exit; //unsupported format type

      ps := pc;
    end else
      Inc(pc);
  end;
  CommitText;

end;

constructor TRefMenuItem.Create(AOwner: TComponent; ARefLink: TRefLink; AData: string);
begin
  inherited Create(AOwner);
  FRefLink := ARefLink;
  Self.Caption := FormatReferenceLinkText(ARefLink.Caption, AData);
  Self.Hint := FormatReferenceLinkText(ARefLink.Hint, AData);
  Self.FURL := FormatReferenceLinkText(ARefLink.URL, AData);
end;

procedure ShellOpen(const sCommand: string);
begin
{$IFDEF MSWINDOWS}
  ShellExecute(0, 'OPEN', PChar(sCommand), '', '', SW_SHOWNORMAL);
{$ENDIF MSWINDOWS}
{$IFDEF POSIX}
  _system(PAnsiChar('open ' + AnsiString(sCommand)));
{$ENDIF POSIX}
end;

procedure TRefMenuItem.Click;
begin
  ShellOpen(FURL);
end;

initialization
  CharacterLinks := TRefLinkList.Create(true);
  ExpressionLinks := TRefLinkList.Create(true);

finalization
 {$IFDEF CLEAN_DEINIT}
  FreeAndNil(ExpressionLinks);
  FreeAndNil(CharacterLinks);
 {$ENDIF}

end.
