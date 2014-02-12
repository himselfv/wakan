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
uses Classes, Generics.Collections, Graphics, StdCtrls, Menus, Controls;

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

  TRefLabel = class(TLabel)
  protected
    FRefLink: TRefLink;
    FURL: string;
    procedure CMParentFontChanged(var Message: TCMParentFontChanged); message CM_PARENTFONTCHANGED;
  public
    constructor Create(AOwner: TComponent; ARefLink: TRefLink; AData: string); reintroduce;
    procedure Click; override;
  end;


implementation
uses SysUtils, UITypes, JWBStrings, JWBUtils, JWBIO, JWBCharData;

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

//Converts every character to hex and prepends with % sign:
//  %30%DF
function UrlBytes(AData: PByte; ALength: integer): UTF8String; overload;
const HexChars: UTF8String = '0123456789ABCDEF';
var i: integer;
begin
  SetLength(Result, ALength*3);
  for i := 1 to ALength do begin
    Result[(i-1)*3+1] := '%';
    Result[(i-1)*3+2] := HexChars[1+ AData^ shr 4];
    Result[(i-1)*3+3] := HexChars[1+ AData^ and $0F];
    Inc(AData);
  end;
end;

function UrlBytes(const AText: UTF8String): UTF8String; overload;
begin
  if AText<>'' then
    Result := UrlBytes(@AText[1], Length(AText))
  else
    Result := '';
end;

function UrlPairs(AData: PAnsiChar; ALength: integer): UTF8String; overload;
var i: integer;
begin
  SetLength(Result, (ALength div 2)*3);
  for i := 1 to ALength div 2 do begin
    Result[(i-1)*3+1] := '%';
    Result[(i-1)*3+2] := AData^;
    Inc(AData);
    Result[(i-1)*3+3] := AData^;
    Inc(AData);
  end;
end;

function UrlPairs(const AText: UTF8String): UTF8String; overload;
begin
  if AText<>'' then
    Result := UrlPairs(@AText[1], Length(AText))
  else
    Result := '';
end;

function EncodeStr(AString: string; const AOut: CEncoding): UTF8String;
var ss: TMemoryStream;
  enc: TEncoding;
  len: integer;
  newstr: UTF8String;
begin
  enc := nil;
  ss := TMemoryStream.Create;
  try
    enc := AOut.Create;
    enc.Write(ss, AString);
    FreeAndNil(enc); //Flush!
    ss.Seek(0, soBeginning);
    len := ss.Size;

   //Let's be extra safe with strings since Delphi does UniqueString pointlessly
   //and makes us break stuff (maybe)
    newstr := '';
    SetLength(newstr, len);
    ss.Read(newstr[1], len);

    Result := newstr;
  finally
    FreeAndNil(enc);
    FreeAndNil(ss);
  end;
end;

//Assuming the input string is a sequence of Unicode characters,
//converts it to a sequence of character property values from the database
//for the specified property type.
function ChProp(AString: UnicodeString; APropNo: integer): UnicodeString;
var i: integer;
begin
  Result := '';
  for i := 1 to Length(AString) do
    Result := Result + GetCharProp(AString[i], APropNo);
end;

//Formats reference link text (Caption, Hint, Link), inserting data in the
//appropriate formats where needed
function FormatReferenceLinkText(AText: string; AData: string): string;
var ps, pc: PChar;
  flag_specsymbol: boolean;
  form: string;
  tmp: UTF8String;
  propno: integer;

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

      tmp := UTF8String(AData);
     { Start from UTF16 data, but convert to UTF8 for handling. On conversions
      which expect "normal text" we convert back to UTF16 temporarily where
      required }
      while pc^=':' do begin
       //Read another conversion
        Inc(pc);
        ps := pc;
        while (pc^<>#00) and (pc^<>'%') and (pc^<>':') do
          Inc(pc);
        if pc^=#00 then exit; //invalid format string

        form := spancopy(ps,pc);

       //Convert
        if form='hex' then
          tmp := UTF8String(UnicodeToHex(UnicodeString(tmp)))
        else
        if form='urlbytes' then
          tmp := UrlBytes(tmp)
        else
        if form='urlpairs' then
          tmp := UrlPairs(tmp)
        else
        if form='urlencode' then
          tmp := UTF8String(URLEncode(tmp, []))
        else
        if form='uppercase' then
          tmp := UTF8String(Uppercase(UnicodeString(tmp)))
        else
        if form='lowercase' then
          tmp := UTF8String(Lowercase(UnicodeString(tmp)))
        else
        if form='utf8' then
          tmp := EncodeStr(UnicodeString(tmp), TUTF8Encoding)
        else
        if form='bigfive' then
          tmp := EncodeStr(UnicodeString(tmp), TBIG5Encoding)
        else
        if form='gb2312' then
          tmp := EncodeStr(UnicodeString(tmp), TGBEncoding)
        else
        if form='jis0208' then
          tmp := EncodeStr(UnicodeString(tmp), TJISEncoding)
        else
        if StartsStr('chprop(', form) then begin
          delete(form, 1, Length('chprop('));
          Assert(EndsStr(')', form));
          delete(form, Length(form), 1);
          if not TryStrToInt(form, propno) then
            exit; //invalid translation
          tmp := UTF8String(ChProp(UnicodeString(tmp), propno));
        end else
          exit; //unsupported format type

        if pc^='%' then
          Inc(pc);
        //leave :
      end;

      Result := Result + UnicodeString(tmp);
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

procedure TRefMenuItem.Click;
begin
  ShellOpen(FURL);
end;

constructor TRefLabel.Create(AOwner: TComponent; ARefLink: TRefLink; AData: string);
begin
  inherited Create(AOwner);
  FRefLink := ARefLink;
  Self.Caption := FormatReferenceLinkText(ARefLink.Caption, AData);
  Self.Hint := FormatReferenceLinkText(ARefLink.Hint, AData);
  Self.FURL := FormatReferenceLinkText(ARefLink.URL, AData);
  Self.Cursor := crHandPoint;
  Self.ShowHint := true;
end;

procedure TRefLabel.Click;
begin
  ShellOpen(FURL);
end;

procedure TRefLabel.CMParentFontChanged(var Message: TCMParentFontChanged);
begin
  inherited;
  Self.Font.Style := Self.Font.Style + [fsUnderline];
  Self.Font.Color := clBlue;
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
