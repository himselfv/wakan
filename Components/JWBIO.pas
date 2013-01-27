unit JWBIO;
{
Input/output
}

interface
uses JWBStrings;

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
  public
    constructor Create(AFilename: string);
    destructor Destroy; override;
    function Eof: boolean; override;
    function ReadLn(var s: string): boolean; override;
    function ReadWideChar(out w: WideChar): boolean;
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

end.


