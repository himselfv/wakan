unit JWBIO;
{
Input/output
}

interface
uses SysUtils, Classes, Windows, JWBStrings;

type
  TCustomFileReader = class
  public
    function ReadLn(var s: string): boolean; overload; virtual; abstract;
    function ReadLn: string; overload; inline;
    function Eof: boolean; virtual; abstract;
  end;

  TAnsiFileReader = class(TCustomFileReader)
  protected
    f: textfile;
  public
    constructor Create(AFilename: string);
    destructor Destroy; override;
    function Eof: boolean; override;
    function ReadLn(var s: string): boolean; override;
  end;

  TUnicodeFileReader = class(TCustomFileReader)
  protected
    f: file;
    feof: boolean;
    buf: array[0..4095] of byte;
    bpos: integer;
    bsz: integer;
    procedure NewBlock;
    function PeekChar(var w: WideChar): boolean;
  public
    constructor Create(AFilename: string);
    destructor Destroy; override;
    procedure SkipBom;
    function Eof: boolean; override;
    function ReadLn(var s: string): boolean; override;
    function ReadWideChar(out w: WideChar): boolean;
  end;

  TConsoleReader = class(TCustomFileReader)
  protected
    FEOF: boolean;
  public
    function ReadLn(var s: string): boolean; override;
    function Eof: boolean; override;
  end;

  TUnicodeStreamReader = class(TCustomFileReader)
  protected
    FStream: TStream;
    FOwnsStream: boolean;
    FEOF: boolean;
  public
    constructor Create(AStream: TStream; AOwnsStream: boolean = false);
    destructor Destroy; override;
    function ReadLn(var s: string): boolean; override;
    function Eof: boolean; override;
  end;

 {$IFDEF UNICODE}
  TFileReader = TUnicodeFileReader;
 {$ELSE}
  TFileReader = TAnsiFileReader;
 {$ENDIF}

  TCustomFileWriter = class
  public
    procedure Writeln(const s: string); virtual; abstract;
  end;

  TAnsiFileWriter = class(TCustomFileWriter)
  protected
    f: textfile;
  public
    constructor Append(AFilename: string);
    constructor Rewrite(AFilename: string);
    destructor Destroy; override;
    procedure Writeln(const s: string); override;
  end;

  TUnicodeFileWriter = class(TCustomFileWriter)
  protected
    f: file;
    buf: array[0..4095] of byte;
    bpos: integer;
  public
    constructor Rewrite(AFilename: string);
    destructor Destroy; override;
    procedure Writeln(const s: string); override;
    procedure WriteWideChar(const w: WideChar);
    procedure WritelnUnicode(const s: UnicodeString);
    procedure Flush;
  end;

  TConsoleWriter = class(TCustomFileWriter)
  public
    procedure Writeln(const s: string); override;
  end;

  TConsoleUTF8Writer = class(TCustomFileWriter)
  public
    procedure Writeln(const s: string); override;
  end;

  TUnicodeStreamWriter = class(TCustomFileWriter)
  protected
    FStream: TStream;
    FOwnsStream: boolean;
    buf: array[0..4095] of byte;
    bpos: integer;
  public
    constructor Create(AStream: TStream; AOwnsStream: boolean = false);
    destructor Destroy; override;
    procedure Writeln(const s: string); override;
    procedure WriteWideChar(const w: WideChar);
    procedure WritelnUnicode(const s: UnicodeString);
    procedure Flush;
  end;

 {$IFDEF UNICODE}
  TFileWriter = TUnicodeFileWriter;
 {$ELSE}
  TFileWriter = TAnsiFileWriter;
 {$ENDIF}

implementation

function TCustomFileReader.ReadLn: string;
begin
  if not ReadLn(Result) then Result := '';
end;

constructor TAnsiFileReader.Create(AFilename: string);
begin
  inherited Create();
  assignfile(f,AFilename);
  reset(f);
end;

destructor TAnsiFileReader.Destroy;
begin
  closefile(f);
  inherited;
end;

function TAnsiFileReader.Eof: boolean;
begin
  Result := System.Eof(f);
end;

function TAnsiFileReader.ReadLn(var s: string): boolean;
begin
  Result := not Eof();
  if Result then
    System.Readln(f, s);
end;

constructor TUnicodeFileReader.Create(AFilename: string);
begin
  inherited Create;
  assignfile(f,AFilename);
  reset(f,1);
  feof := false;
  bpos := 0;
  bsz := 0;
end;

destructor TUnicodeFileReader.Destroy;
begin
  closefile(f);
  inherited;
end;

procedure TUnicodeFileReader.SkipBom;
var c: WideChar;
begin
  if PeekChar(c) and (c=#$FEFF) then
    ReadWideChar(c);
end;

function TUnicodeFileReader.Eof: boolean;
begin
  Result := feof and (bpos>=bsz);
end;

procedure TUnicodeFileReader.NewBlock;
begin
  BlockRead(f, buf, sizeof(buf), bsz);
  if bsz<sizeof(buf) then feof := true;
  bpos := 0;
end;

//Compare with ReadWideChar
function TUnicodeFileReader.PeekChar(var w: WideChar): boolean;
begin
  Result := false;
  if bpos>=bsz then begin
    if feof then exit;
    NewBlock;
  end;
  if bpos>bsz-2 then //don't have two bytes
    exit;
  w := PWideChar(@buf[bpos])^;
  Result := true;
end;

function TUnicodeFileReader.ReadWideChar(out w: WideChar): boolean;
begin
  Result := false;
  if bpos>=bsz then begin
    if feof then exit;
    NewBlock;
  end;
  if bpos>bsz-2 then //don't have two bytes
    exit;
  w := PWideChar(@buf[bpos])^;
  Inc(bpos, 2);
  Result := true;
end;

//Reads one line (maybe empty). False if there was no another line (not even empty one)
function TUnicodeFileReader.ReadLn(var s: string): boolean;
var w: WideChar;
begin
  s := '';
  if not ReadWideChar(w) then begin
    Result := false;
    exit;
  end;
  repeat
    if (w<>#$000D) and (w<>#$000A) then s := s + char(w);
  until (w=#$000A) or not ReadWideChar(w);
  Result := true;
end;

function TConsoleReader.ReadLn(var s: string): boolean;
begin
  Result := not EOF;
  System.Readln(s);
end;

function TConsoleReader.Eof: boolean;
begin
  Result := System.Eof(Input);
end;

constructor TUnicodeStreamReader.Create(AStream: TStream; AOwnsStream: boolean);
begin
  inherited Create();
  FStream := AStream;
  FOwnsStream := AOwnsStream;
end;

destructor TUnicodeStreamReader.Destroy;
begin
  if FOwnsStream then
    FreeAndNil(FStream);
  inherited;
end;

function TUnicodeStreamReader.ReadLn(var s: string): boolean;
var c: WideChar;
begin
  s := '';
  Result := not EOF;
  while FStream.Read(c, SizeOf(c))=SizeOf(c) do begin
    if c=#10 then exit;
    s := s + c;
  end;
  FEOF := true;
end;

function TUnicodeStreamReader.Eof: boolean;
begin
  Result := FEOF;
end;

constructor TAnsiFileWriter.Append(AFilename: string);
begin
  inherited Create();
  assignfile(f,AFilename);
  System.Append(f);
end;

constructor TAnsiFileWriter.Rewrite(AFilename: string);
begin
  inherited Create();
  assignfile(f,AFilename);
  System.rewrite(f);
end;

destructor TAnsiFileWriter.Destroy;
begin
  closefile(f);
  inherited;
end;

procedure TAnsiFileWriter.Writeln(const s: string);
begin
  System.writeln(f,s);
end;

constructor TUnicodeFileWriter.Rewrite(AFilename: string);
begin
  inherited Create();
  assignfile(f,AFilename);
  System.rewrite(f,1);
  bpos := 0;
end;

destructor TUnicodeFileWriter.Destroy;
begin
  Flush();
  closefile(f);
  inherited;
end;

procedure TUnicodeFileWriter.WriteWideChar(const w: WideChar);
begin
  if bpos>=sizeof(buf) then
    Flush();
  PWideChar(@buf[bpos])^ := w;
  Inc(bpos, 2);
end;

procedure TUnicodeFileWriter.Writeln(const s: string);
begin
  WritelnUnicode(UnicodeString(s));
end;

procedure TUnicodeFileWriter.WritelnUnicode(const s: UnicodeString);
var i: integer;
begin
  for i := 1 to Length(s) do
    WriteWideChar(s[i]);
  WriteWideChar(#$000D);
  WriteWideChar(#$000A);
end;

procedure TUnicodeFileWriter.Flush;
begin
  if bpos>0 then
    BlockWrite(f, buf, bpos);
  bpos := 0;
end;

procedure TConsoleWriter.Writeln(const s: string);
begin
  System.writeln(s);
end;

procedure TConsoleUTF8Writer.Writeln(const s: string);
begin
  System.writeln(UTF8Encode(s));
end;

constructor TUnicodeStreamWriter.Create(AStream: TStream; AOwnsStream: boolean);
begin
  inherited Create();
  FStream := AStream;
  FOwnsStream := AOwnsStream;
  bpos := 0;
end;

destructor TUnicodeStreamWriter.Destroy;
begin
  Flush();
  if FOwnsStream then
    FreeAndNil(FStream);
  inherited;
end;

procedure TUnicodeStreamWriter.WriteWideChar(const w: WideChar);
begin
  if bpos>=sizeof(buf) then
    Flush();
  PWideChar(@buf[bpos])^ := w;
  Inc(bpos, 2);
end;

procedure TUnicodeStreamWriter.Writeln(const s: string);
begin
  WritelnUnicode(UnicodeString(s));
end;

procedure TUnicodeStreamWriter.WritelnUnicode(const s: UnicodeString);
var i: integer;
begin
  for i := 1 to Length(s) do
    WriteWideChar(s[i]);
  WriteWideChar(#$000D);
  WriteWideChar(#$000A);
end;

procedure TUnicodeStreamWriter.Flush;
begin
  if bpos>0 then
    FStream.Write(buf, bpos);
  bpos := 0;
end;

end.


