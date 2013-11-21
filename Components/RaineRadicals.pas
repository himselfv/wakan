unit RaineRadicals;
{ Raine radicals in RADKFILE format.
 Usage:
   rad := TRaineRadicals.Create;
   rad.LoadFromRadKFile('RADKFILE');
   rad.LoadFromRadKFile('RADKFILE2');
   radicals := rad.GetCharRadicals(char);

 Throughout this file "radicals" mean "parts of characters". There could be
 several of these for a character.

 Unicode only. }

interface
uses SysUtils;

type
  TRadicalEntry = record
    Radical: char;
    StrokeCount: integer;
    Chars: string;
  end;
  PRadicalEntry = ^TRadicalEntry;

  TRaineRadicals = class
  protected
    FRadicals: array of TRadicalEntry;
    FRadicalsUsed: integer;
    function GetCount: integer;
    function GetItem(const AIndex: integer): PRadicalEntry;
    function AddRadical(const ARadical: char): integer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure LoadFromRadKFile(const AFilename: string);
    function FindRadical(const ARadical: char): integer;
    function HasRadical(const AChar: char; const ARadicalIndex: integer): boolean; overload;
    function HasRadical(const AChar: char; const ARadical: char): boolean; overload;
    function GetContainingChars(ARadicalIndex: integer): string; overload;
    function GetContainingChars(const ARadical: char): string; overload;
    function GetCharRadicals(const AChar: char): string;
    property Count: integer read GetCount;
    property Items[const Index: integer]: PRadicalEntry read GetItem; default;
  end;

  ERadKFileParsingException = class(Exception);

implementation
uses Classes, JWBIO;

constructor TRaineRadicals.Create;
begin
  inherited;
end;

destructor TRaineRadicals.Destroy;
begin
  Clear;
  inherited;
end;

procedure TRaineRadicals.Clear;
begin
  FRadicalsUsed := 0;
end;

function TRaineRadicals.GetCount: integer;
begin
  Result := FRadicalsUsed;
end;

function TRaineRadicals.GetItem(const AIndex: integer): PRadicalEntry;
begin
  Result := @FRadicals[AIndex];
end;

function TRaineRadicals.FindRadical(const ARadical: char): integer;
var i: integer;
begin
  Result := -1;
  for i := 0 to Count-1 do
    if Items[i].Radical=ARadical then begin
      Result := i;
      break;
    end;
end;

function TRaineRadicals.HasRadical(const AChar: char; const ARadicalIndex: integer): boolean;
var pc: PChar;
begin
  Result := false;
  pc := PChar(FRadicals[ARadicalIndex].Chars);
  if pc=nil then exit;
  while (pc^<>#00) and (pc^<>AChar) do
    Inc(pc);
  Result := (pc^<>#00);
end;

function TRaineRadicals.HasRadical(const AChar: char; const ARadical: char): boolean;
var AIndex: integer;
begin
  AIndex := FindRadical(ARadical);
  if AIndex<0 then
    Result := false
  else
    Result := HasRadical(AChar, AIndex);
end;

function TRaineRadicals.GetContainingChars(ARadicalIndex: integer): string;
begin
  Result := FRadicals[ARadicalIndex].Chars
end;

function TRaineRadicals.GetContainingChars(const ARadical: char): string;
var AIndex: integer;
begin
  AIndex := FindRadical(ARadical);
  if AIndex<0 then
    Result := ''
  else
    Result := GetContainingChars(AIndex);
end;

function TRaineRadicals.GetCharRadicals(const AChar: char): string;
var i: integer;
begin
  Result := '';
  for i := 0 to Count-1 do
    if HasRadical(AChar, i) then
      Result := Result + Items[i].Radical;
end;

function TRaineRadicals.AddRadical(const ARadical: char): integer;
begin
  if FRadicalsUsed>=Length(FRadicals) then
    SetLength(FRadicals,FRadicalsUsed*2+5);
  Result := FRadicalsUsed;
  FRadicals[Result].Radical := ARadical;
  FRadicals[Result].StrokeCount := 0;
  FRadicals[Result].Chars := '';
  Inc(FRadicalsUsed);
end;

procedure TRaineRadicals.LoadFromRadKFile(const AFilename: string);
const
  UH_RK_COMMENT: char = '#';
  UH_RK_RAD: char = '$';
var
  ln: string;
  lineno: integer;
  ch: char;
  ach: AnsiChar;
  i: integer;

  rad_idx: integer; //index to active radical record
  rad_char: WideChar;
  rad_scnt: integer; //stroke count

  conv: TStreamDecoder;

begin
 { See comments in TRaineRadicals for format }
  rad_idx := -1;

  lineno := 0;
  conv := OpenTextFile(AFilename, TEUCEncoding); //RADKFILEs are in EUC
  while not conv.EOF() do begin
    ln := conv.ReadLn();
    Inc(lineno);
    if Length(ln)<=0 then continue;

    ch := ln[1];
    if ch=UH_RK_COMMENT then
      continue;
    if ch=UH_RK_RAD then begin
      i := 2;
     //Skip spaces
      while (i<=Length(ln)) and (ln[i]=' ') do
        Inc(i);
      if i>Length(ln) then
        raise ERadKFileParsingException.Create('Bad RADKFILE: missing glyph char @ line '+IntToStr(lineno));
     //Read char
      rad_char := ln[i];
      Inc(i);
     //Skip spaces
      while (i<=Length(ln)) and (ln[i]=' ') do
        Inc(i);
      if i>Length(ln) then
        raise ERadKFileParsingException.Create('Bad RADKFILE: missing stroke count @ line '+IntToStr(lineno));
     //Read int
      rad_scnt := 0;
      while (i<=Length(ln)) and (ln[i]<>' ') do begin
        if not (ord(ln[i])>=ord('0'))
        or not (ord(ln[i])<=ord('9')) then
          raise ERadKFileParsingException.Create('Bad RADKFILE: invalid stroke count @ line '+IntToStr(lineno));
        rad_scnt := rad_scnt * 10 + (Ord(ach)-Ord('0'));
        Inc(i);
      end;
     //There could be jis code/image file name after this, but we're...
     //Done with this line
      rad_idx := FindRadical(rad_char);
      if rad_idx<0 then begin
        rad_idx := AddRadical(rad_char);
        FRadicals[rad_idx].StrokeCount := rad_scnt;
      end;
      continue;
    end;

   //Character line
    if rad_idx<0 then //we must have a rad_idx by this point
      raise Exception.Create('Bad RADKFILE: character data before any radical identification @ line '+IntToStr(lineno));
    FRadicals[rad_idx].Chars := FRadicals[rad_idx].Chars + ln;

   //we make no attempt to find duplicate kanjis since it's not in our use case.
   //RADKFILE/RADKFILE2 cover non-overlapping sets of kanjis
  end;
  FreeAndNil(conv);
end;

end.
