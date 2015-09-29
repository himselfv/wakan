unit JWBRefLinks;
{ Manages reference links for both kanji and expressions.
How to use:
  for Filename in GetExpressionLinks do begin
    link := LoadLink(Filename);
    FormatReferenceLinkText(link.Caption, kanji);
    ShellOpen(FormatReferenceLinkText(link.URL, kanji));
    link.Destroy;
  end;
}

interface
uses Classes, Generics.Collections, Graphics, StdCtrls, Menus, Controls,
  Windows;

type
 //Descendants implement some specifics
  TRefLinkType = (ltJapaneseOnly, ltChineseOnly, ltAll);
  TRefLink = class
  public
    Filename: string;
    LinkType: TRefLinkType;
    Title: string;
    Hint: string;
    URL: string;
    WorkingDirectory: string;
    HotKey: integer;
    function MatchesLang(const ACurLang: char): boolean;
    function GetSmallIcon: TIcon; virtual;
    function GetSmallHIcon: HICON; virtual;
  end;

  //.url format file
  TUrlRefLink = class(TRefLink)
  public
    IconFile: string;
    IconIndex: integer;
    constructor Create(const AFilename: string);
    function GetSmallHIcon: HICON; override;
  end;

  //Basically anything which can be run with ShellOpen, but we can't customize
  //such things a lot.
  TShellRefLink = class(TRefLink)
  public
    constructor Create(const AFilename: string);
    function GetSmallHIcon: HICON; override;
  end;

//Formats any of "Caption, Hint, URL", inserting actual character/expression
function FormatReferenceLinkText(AText: string; AData: string): string;

//Escapes any characters which can be interpreted by FormatReferenceLinkText
function EscapeReferenceLinkText(AText: string): string;

type
  //TMenuItem tailored to open the specified link when clicked
  TRefMenuItem = class(TMenuItem)
  protected
    FURL: string;
  public
    constructor Create(AOwner: TComponent; ARefLink: TRefLink; AData: string); reintroduce;
    procedure Click; override;
  end;

  TRefLabel = class(TLabel)
  protected
    FURL: string;
    procedure CMParentFontChanged(var Message: TCMParentFontChanged); message CM_PARENTFONTCHANGED;
  public
    constructor Create(AOwner: TComponent; ARefLink: TRefLink; AData: string); reintroduce;
    procedure Click; override;
  end;

function GetCharacterLinksDir: string;
function GetExpressionLinksDir: string;
function GetCharacterLinks: TArray<string>;
function GetExpressionLinks: TArray<string>;
function LoadLink(const AFilename: string): TRefLink;

implementation
uses SysUtils, UITypes, JWBStrings, AppData, JWBCore, JWBIO, JWBCharData, IniFiles,
  ShellApi, ShlObj, ActiveX;

resourcestring
  eInvalidReferenceLinkDeclaration = 'Invalid reference link declaration: %s';

function LinkTypeFromStr(const AType: string): TRefLinkType;
begin
  if AType='' then
    raise Exception.CreateFmt(eInvalidReferenceLinkDeclaration,[AType])
  else
    case UpperCase(AType)[1] of
     'A': Result := ltAll;
     'J': Result := ltJapaneseOnly;
     'C': Result := ltChineseOnly;
    else
      raise Exception.CreateFmt(eInvalidReferenceLinkDeclaration,[AType]);
    end;
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

//Loads associated small icon, if any.
function TRefLink.GetSmallIcon: TIcon;
var hi: HICON;
begin
  hi := GetSmallHIcon();
  if hi<>0 then begin
    Result := TIcon.Create;
    Result.Handle := hi;
  end else
    Result := nil;
end;

function TRefLink.GetSmallHIcon: HICON;
begin
  Result := 0;
end;

constructor TUrlRefLink.Create(const AFilename: string);
var data: TMemIniFile;
  tmp: string;
begin
  inherited Create;
  Self.Filename := AFilename;
  data := TMemIniFile.Create(AFilename);
  try
    Self.URL := data.ReadString('InternetShortcut', 'URL', '');
    Self.Title := data.ReadString('InternetShortcut', 'Title', '');
    if Self.Title='' then
      Self.Title := ChangeFileExt(ExtractFilename(AFilename), '');
    Self.Hint := data.ReadString('InternetShortcut', 'Hint', '');
    Self.WorkingDirectory := data.ReadString('InternetShortcut', 'WorkingDirectory', '');
    Self.IconIndex := data.ReadInteger('InternetShortcut', 'IconIndex', 0);
    Self.IconFile := data.ReadString('InternetShortcut', 'IconFile', '');
    Self.HotKey := data.ReadInteger('InternetShortcut', 'HotKey', 0);
    tmp := data.ReadString('InternetShortcut', 'Language', '');
    if tmp='' then
      Self.LinkType := ltAll
    else
      Self.LinkType := LinkTypeFromStr(tmp);
  finally
    FreeAndNil(data);
  end;
end;

function TUrlRefLink.GetSmallHIcon: HICON;
var hLargeIcon, hSmallIcon: HICON;
  res: integer;
begin
  if Self.IconFile='' then begin
    Result := 0;
    exit;
  end;

  SetCurrentDir(ExtractFileDir(Self.Filename)); //Paths may be relative for all we know

  //Standard Windows way of handling the IconFile,IconIndex pair.
  //Only extracts 32x32 and 16x16 sizes but good enough for us.
  res := ExtractIconEx(PChar(Self.IconFile), Self.IconIndex,
    hLargeIcon, hSmallIcon, 1);
  //res=1 when generating both from ico, 2 when both are available
  if (res=1) or (res=2) then begin
    DestroyIcon(hLargeIcon); //thanks for declaring the param as var, Delphi
    Result := hSmallIcon; //handle is theirs to destroy
  end else
    Result := 0;
end;

//Result to be freed by CoTaskMemFree
function GetPathPIDL(const AParent: IShellFolder; const ARelativePath: string): PItemIDList;
var chEaten: cardinal;
  dwAttributes: cardinal;
begin
  if FAILED(AParent.ParseDisplayName(
    0, nil, PChar(ARelativePath), chEaten, Result, dwAttributes
  )) then
    Result := nil;
end;

function GetPathIShellFolder(const AAbsolutePath: string): IShellFolder;
var pDesktopFolder: IShellFolder;
  pidl: PItemIDList;
begin
  if FAILED(SHGetDesktopFolder(pDesktopFolder)) then begin
    Result := nil;
    exit;
  end;

  pidl := GetPathPIDL(pDesktopFolder, AAbsolutePath);
  if pidl=nil then begin
    Result := nil;
    exit;
  end;

  if FAILED(pDesktopFolder.BindToObject(pidl, nil, IShellFolder, Result)) then
    Result := nil;
  CoTaskMemFree(pidl);
end;

function SHGetInfoTip(AParent: IShellFolder; AItem: PItemIDList): string;
var qi: IQueryInfo;
  pTip: PWideChar;
begin
  if FAILED(AParent.GetUIObjectOf(0, 1, AItem, IQueryInfo, nil, qi)) then begin
    Result := '';
    exit;
  end;

  if FAILED(qi.GetInfoTip(QITIPF_DEFAULT, pTip)) then begin
    Result := '';
    exit;
  end;

  Result := pTip;
  CoTaskMemFree(pTip);
end;

constructor TShellRefLink.Create(const AFilename: string);
var AParent: IShellFolder;
  AItem: PItemIDList;
begin
  inherited Create;
  Self.Filename := AFilename;
  Self.URL := EscapeReferenceLinkText(Filename) + ' %s';
 { For generic "shell object" there's no way to store "parametrized target"
  like with URLs. It's possbible with .lnk files, but we get a bunch of other
  problems (template parser breaks on \ slashes) so let's not bother for now. }
  Self.Title := ChangeFileExt(ExtractFilename(AFilename), '');
  Self.Hint := '';

  //Ask shell to provide file hint. Need parent+immediate child
  //Absolute path is required in Filename.
  AParent := GetPathIShellFolder(ExtractFileDir(AFilename));
  if AParent<>nil then begin
    AItem := GetPathPIDL(AParent, ExtractFilename(AFilename));
    if (AItem<>nil) then begin
      Self.Hint := SHGetInfoTip(AParent, AItem);
      CoTaskMemFree(AItem);
    end;
  end;
end;

function TShellRefLink.GetSmallHIcon: HICON;
var fi: TSHFileInfo;
  res: DWORD_PTR;
begin
  res := SHGetFileInfo(PChar(Filename), 0, fi, SizeOf(fi),
    SHGFI_ICON or SHGFI_SMALLICON);
  if res=0 then begin
    Result := 0;
    exit;
  end;

  Result := fi.hIcon;
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
        //leave ":"
      end;

      Result := Result + UnicodeString(tmp);
      ps := pc;
    end else
      Inc(pc);
  end;
  CommitText;
end;

function EscapeReferenceLinkText(AText: string): string;
begin
  Result := AText.Replace('\', '\\').Replace('%', '\%');
end;

constructor TRefMenuItem.Create(AOwner: TComponent; ARefLink: TRefLink; AData: string);
var AIcon: TIcon;
begin
  inherited Create(AOwner);
  Self.Caption := FormatReferenceLinkText(ARefLink.Title, AData);
  Self.Hint := FormatReferenceLinkText(ARefLink.Hint, AData);
  Self.FURL := FormatReferenceLinkText(ARefLink.URL, AData);

  AIcon := ARefLink.GetSmallIcon;
  if AIcon<>nil then begin
    Self.Bitmap.Assign(ARefLink.GetSmallIcon);
    Self.Bitmap.Transparent := false; //disable "mask transparency", use alpha
    FreeAndNil(AIcon);
  end;
end;

procedure TRefMenuItem.Click;
begin
  ShellOpen(FURL);
end;

constructor TRefLabel.Create(AOwner: TComponent; ARefLink: TRefLink; AData: string);
begin
  inherited Create(AOwner);
  Self.Caption := FormatReferenceLinkText(ARefLink.Title, AData);
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


//Returns the full path to the folder where CharacterLinks are stored in this
//configuration
function GetCharacterLinksDir: string;
begin
  Result := UserDataDir + '\KanjiLinks';
end;

//Same for ExpressionLinks
function GetExpressionLinksDir: string;
begin
  Result := UserDataDir + '\ExprLinks';
end;

//Retrieves a list of all available link files in a folder
function GetLinks(const ADir: string): TArray<string>;
var sr: TSearchRec;
  res: integer;
  tmp: string;
begin
  SetLength(Result, 0);
  res := FindFirst(ADir+'\*.*', faAnyFile and not faDirectory, sr);
  try
    while res=0 do begin
      tmp := ExtractFileExt(sr.Name).ToLower;
      if (tmp='.url') or (tmp='.website') or (tmp='.lnk') then begin
        SetLength(Result, Length(Result)+1);
        Result[Length(Result)-1] := ADir + '\' + sr.Name;
      end;
      res := FindNext(sr);
    end;
  finally
    SysUtils.FindClose(sr);
  end;
end;

function GetCharacterLinks: TArray<string>;
begin
  Result := GetLinks(GetCharacterLinksDir);
end;

function GetExpressionLinks: TArray<string>;
begin
  Result := GetLinks(GetExpressionLinksDir);
end;

//Can in theory return nil if the file is not a supported link.
function LoadLink(const AFilename: string): TRefLink;
var ext: string;
begin
  ext := ExtractFileExt(AFilename);
  if (ext='.url') or (ext='.website') then
    Result := TUrlRefLink.Create(AFilename)
  else
  if (ext='.lnk') then
    Result := TShellRefLink.Create(AFilename)
  else
    Result := nil;
end;


end.
