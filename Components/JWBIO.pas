unit JWBIO;
{
Encodings and stream encoders/decoders.
How to use:
1. Detecting encodings:

  if not Conv_DetectType(filename, AEncoding) then
    AEncoding := Conv_ChooseType(AEncoding);

2. Reading file:

  AReader := TFileReader.Create(filename, TUTF8Encoding.Create);

  AReader := OpenTextFile(filename, TUTF8Encoding);
 //opens cached stream, may be useful when doing seeks/detection

  AReader := OpenTextFile(filename, nil);
 //best guess encoding and open

  while AReader.ReadChar(ch) do
    ProcessChar(ch);
  line := AReader.Readln();

3. Writing file:

  AWriter := CreateTextFile(filename, TUTF8Encoding);
  AWriter := AppendToTextFile(filename, TUTF8Encoding);
  AWriter.Writeln('Hello world');
}

interface
uses SysUtils, Classes, JWBStrings;

{ TODO: ACP (Active codepage, similar to ANSI but with local [128..255]),
 through buffering/QueryNextBlock }
{ TODO: RtlDecoder(TEncoding)
 We do not use Delphi's RTL TEncodings because those do not support converting
 only a part of the buffer.
 We may write a wrapper using some tricks (convert too much + drop ending) maybe. }
{ TODO: UTF16 surrogate pairs (LE and BE in different order) }

type
 { Encoding is created once and executed sequentially on a stream, starting with
  a legal position (not inside a surrogate).
  It may keep track of decoding state, so a new one is required for every stream.

  It is recommended to use TEncoding with a buffered, best effort stream.
  1. BOM checking and some routines require seeks.
  2. socket-like "read available bytes and return" behavior WILL break things.
   TStream has to block until requested number of bytes is available or the
   stream is over. }

  TEncodingLikelihood = (
    elIdentified, //data is self-marked as being in this encoding
    elLikely,     //data is likely to be in this encoding
    elUnlikely    //data is unlikely to be in this encoding
  );

  TEncoding = class
    constructor Create; virtual;
    class function GetBom: TBytes; virtual;
    class function ReadBom(AStream: TStream): boolean; virtual;
    procedure WriteBom(AStream: TStream); virtual;
    function Analyze(AStream: TStream): TEncodingLikelihood; virtual;
    function Read(AStream: TStream; MaxChars: integer): string; virtual; abstract;
   { MaxChars is given in 2 byte positions. Surrogate pairs may be broken }
    procedure Write(AStream: TStream; const AData: string); virtual; abstract;
   { Encoding descendants do not check the number of bytes written. If you care,
    raise exceptions in TStream descendant wrapper.  }
  end;
  CEncoding = class of TEncoding;

 { Simple encodings }
  TAsciiEncoding = class(TEncoding)
    function Read(AStream: TStream; MaxChars: integer): string; override;
    procedure Write(AStream: TStream; const AData: string); override;
  end;

  TACPEncoding = class(TAsciiEncoding); //for now

  TUTF8Encoding = class(TEncoding)
    class function GetBom: TBytes; override;
    function Read(AStream: TStream; MaxChars: integer): string; override;
    procedure Write(AStream: TStream; const AData: string); override;
  end;

  TUTF16LEEncoding = class(TEncoding)
    class function GetBom: TBytes; override;
    function Read(AStream: TStream; MaxChars: integer): string; override;
    procedure Write(AStream: TStream; const AData: string); override;
  end;
  TUTF16Encoding = TUTF16LEEncoding;
  TUnicodeEncoding = TUTF16LEEncoding;

  TUTF16BEEncoding = class(TEncoding)
    class function GetBom: TBytes; override;
    function Read(AStream: TStream; MaxChars: integer): string; override;
    procedure Write(AStream: TStream; const AData: string); override;
  end;

  TEUCEncoding = class(TEncoding)
    function Read(AStream: TStream; MaxChars: integer): string; override;
    procedure Write(AStream: TStream; const AData: string); override;
  end;

  TSJISEncoding = class(TEncoding)
    function Read(AStream: TStream; MaxChars: integer): string; override;
    procedure Write(AStream: TStream; const AData: string); override;
  end;

 { New-JIS, Old-JIS and NEC-JIS differ only by Start and End markers.
  Read() covers all of them while Write() depends on markers being set by descendants below }
  TBaseJISEncoding = class(TEncoding)
  protected
    intwobyte: boolean;
    StartMark: TBytes;
    EndMark: TBytes;
    procedure _fputstart(AStream: TStream);
    procedure _fputend(AStream: TStream);
  public
    function Read(AStream: TStream; MaxChars: integer): string; override;
    procedure Write(AStream: TStream; const AData: string); override;
  end;
  TJISEncoding = class(TBaseJISEncoding)
    constructor Create; override;
  end;
  TOldJISEncoding = class(TBaseJISEncoding)
    constructor Create; override;
  end;
  TNECJISEncoding = class(TBaseJISEncoding)
    constructor Create; override;
  end;

  TGBEncoding = class(TEncoding)
    function Read(AStream: TStream; MaxChars: integer): string; override;
    procedure Write(AStream: TStream; const AData: string); override;
  end;

  TBIG5Encoding = class(TEncoding)
    function Read(AStream: TStream; MaxChars: integer): string; override;
    procedure Write(AStream: TStream; const AData: string); override;
  end;

const
  INBUFSZ = 1024; //in characters
  OUTBUFSZ = 1024;

type
  TStreamDecoder = class
  protected
    FBuffer: string;
    FBufferPos: integer;
    FStream: TStream;
    FOwnsStream: boolean;
    FEncoding: TEncoding;
    FEOF: boolean; //set when you cannot read one more entry from the stream
    procedure NextBatch;
  public
    constructor Open(AStream: TStream; AEncoding: TEncoding;
      AOwnsStream: boolean = false); overload; virtual;
    constructor Open(const AFilename: string; AEncoding: TEncoding); overload;
    destructor Destroy; override;
    procedure DetachStream; //clears whatever caches the instance may have for the Stream
   { Since this may require backward seek, try not to TrySkipBom for sources where
    you do not really expect it (i.e. console) }
    procedure TrySkipBom;
    function EOF: boolean;
    function ReadChar(out ch: WideChar): boolean; overload;
   { If reading next position produces a surrogate pair, store it somewhere and
    return in two calls. }
    function ReadLn(out ln: UnicodeString): boolean; overload;
    function ReadChar: WideChar; overload; //#0000 if no char
    function ReadLn: UnicodeString; overload; //empty string if no string
   { Or should these raise exceptions? }
    property Stream: TStream read FStream;
    property OwnsStream: boolean read FOwnsStream;
    property Encoding: TEncoding read FEncoding;
  end;

  TStreamEncoder = class
  protected
    FBuffer: string;
    FBufferSize: integer;
    FStream: TStream;
    FOwnsStream: boolean;
    FEncoding: TEncoding;
  public
    constructor Open(AStream: TStream; AEncoding: TEncoding;
      AOwnsStream: boolean = false);
    constructor CreateNew(const AFilename: string; AEncoding: TEncoding);
    constructor Append(const AFilename: string; AEncoding: TEncoding);
    destructor Destroy; override;
    procedure Flush; //clears whatever caches instance may have for the stream
    procedure WriteBom;
    procedure WriteChar(const ch: WideChar);
    procedure Write(const ln: UnicodeString);
    procedure WriteLn(const ln: UnicodeString);
    property Stream: TStream read FStream;
    property OwnsStream: boolean read FOwnsStream;
    property Encoding: TEncoding read FEncoding;
    property BufferSize: integer read FBufferSize write FBufferSize;
  end;
  CStreamEncoder = class of TStreamDecoder;

function Conv_DetectType(AStream: TStream): CEncoding; overload;
function Conv_DetectType(AStream: TStream; out AEncoding: CEncoding): boolean; overload;
function Conv_DetectType(const AFilename: string): CEncoding; overload;
function Conv_DetectType(const AFilename: string; out AEncoding: CEncoding): boolean; overload;
function Conv_ChooseType(AChinese:boolean; ADefault: CEncoding): CEncoding;

function OpenTextFile(const AFilename: string; AEncoding: CEncoding = nil): TStreamDecoder;
function CreateTextFile(const AFilename: string; AEncoding: CEncoding): TStreamEncoder;
function AppendToTextFile(const AFilename: string; AEncoding: CEncoding = nil): TStreamEncoder;

{ Compatibility functions }
function AnsiFileReader(const AFilename: string): TStreamDecoder;
function UnicodeFileReader(const AFilename: string): TStreamDecoder;
function ConsoleReader(): TStreamDecoder;
function UnicodeStreamReader(AStream: TStream; AOwnsStream: boolean = false): TStreamDecoder;
function FileReader(const AFilename: string): TStreamDecoder; inline; //->Unicode on Unicode, ->Ansi on Ansi
function AnsiFileWriter(const AFilename: string): TStreamEncoder;
function UnicodeFileWriter(const AFilename: string): TStreamEncoder;
function ConsoleWriter(): TStreamEncoder;
function ConsoleUTF8Writer(): TStreamEncoder;
function UnicodeStreamWriter(AStream: TStream; AOwnsStream: boolean = false): TStreamEncoder;
function FileWriter(const AFilename: string): TStreamEncoder; inline; //->Unicode on Unicode, ->Ansi on Ansi

implementation
uses Controls, Windows, StreamUtils, JWBConvertTbl, JWBFileType;

{ Various helpers }

//Swaps bytes in a word
function _swapw(const w: word): word; inline;
begin
  Result :=
    (w and $00FF) shl 8 +
    (w and $FF00) shr 8;
{ or:
    (w mod $100) shl 8 +
    (w div $100);
  dunno which is faster }
end;


{ Encoding }

constructor TEncoding.Create;
begin
  inherited; { Inherit in descendants to initialize encoding }
end;

class function TEncoding.GetBom: TBytes;
begin
  Result := TBytes.Create();
end;

{ Reads out any of the BOMs supported by this encoding and returns true,
 or returns false and returns AStream to the previous position.
 Avoid calling for streams which do not support seeking when BOM is not really
 expected. }
class function TEncoding.ReadBom(AStream: TStream): boolean;
var BOM: TBytes;
  data: TBytes;
  read_sz: integer;
  i: integer;
begin
  BOM := GetBOM();
  if Length(BOM)<=0 then begin
    Result := false;
    exit;
  end;
 { Default implementation just checks for the main BOM }
  SetLength(data, Length(BOM));
  read_sz := AStream.Read(data[0], Length(data));
  Result := true;
  for i := 0 to Length(data)-1 do
    if BOM[i]<>data[i] then begin
      Result := false;
      break;
    end;
  if not Result then
    AStream.Seek(-read_sz, soCurrent);
end;

procedure TEncoding.WriteBom(AStream: TStream);
var BOM: TBytes;
begin
  BOM := GetBOM();
  if Length(BOM)>0 then
    AStream.Write(BOM[0], Length(BOM));
end;

function TEncoding.Analyze(AStream: TStream): TEncodingLikelihood;
begin
 { Default implementation only tests for BOM }
  if ReadBom(AStream) then
    Result := elIdentified
  else
    Result := elUnlikely;
end;

procedure _fwrite1(AStream: TStream; const b: byte); inline;
begin
  AStream.Write(b, 1);
end;

procedure _fwrite2(AStream: TStream; const w: word); inline;
begin
  AStream.Write(w, 2);
end;

procedure _fwrite3(AStream: TStream; const dw: cardinal); inline;
begin
  AStream.Write(dw, 3);
end;

procedure _fwrite4(AStream: TStream; const dw: cardinal); inline;
begin
  AStream.Write(dw, 4);
end;


{ Stream decoder }

constructor TStreamDecoder.Open(AStream: TStream; AEncoding: TEncoding;
  AOwnsStream: boolean = false);
begin
  inherited Create();
  FStream := AStream;
  FOwnsStream := AOwnsStream;
  FEOF := false;
  FEncoding := AEncoding;
  Self.TrySkipBom;
end;

constructor TStreamDecoder.Open(const AFilename: string; AEncoding: TEncoding);
var fs: TFileStream;
begin
  fs := TFileStream.Create(AFilename, fmOpenRead);
  Self.Open(fs, AEncoding, {OwnsStream=}true);
end;

destructor TStreamDecoder.Destroy;
begin
  if not FOwnsStream then
    DetachStream; //if the stream is ours, no point in restoring it's actual position
  if FOwnsStream then
    FreeAndNil(FStream);
  FreeAndNil(FEncoding);
  inherited;
end;

procedure TStreamDecoder.NextBatch;
var new_sz: integer;
  new_chunk: string;
begin
  new_sz := INBUFSZ-(Length(FBuffer)-FBufferPos);
  if new_sz<=0 then exit;
  new_chunk := FEncoding.Read(FStream, new_sz);
  FEOF := (length(new_chunk)<=0); { this way EOF may be cleared by some streams
    when data arrives, if you call NextBatch() again. Use at your own risk. }
  FBuffer := copy(FBuffer, FBufferPos+1, MaxInt)
    + new_chunk;
  FBufferPos := 0;
end;

procedure TStreamDecoder.DetachStream;
begin
 { Unfortunately there's no way now to know how many bytes we should roll back
  to position stream where we logically are (in characters). }
end;

procedure TStreamDecoder.TrySkipBom;
begin
  FEncoding.ReadBom(Stream);
end;

function TStreamDecoder.EOF: boolean;
begin
  Result := FEOF;
end;

function TStreamDecoder.ReadChar(out ch: WideChar): boolean;
begin
  if FBufferPos>=Length(FBuffer) then begin
    if FEOF then begin
      Result := false;
      exit;
    end;
    NextBatch;
    if FBufferPos>=Length(FBuffer) then begin
      Result := false;
      exit;
    end;
  end;
  ch := FBuffer[FBufferPos+1];
  Inc(FBufferPos);
  Result := true;
end;

function TStreamDecoder.ReadLn(out ln: UnicodeString): boolean;
var ch: WideChar;
begin
 { Reimplement in descendants if you have a better way }
  ln := '';
  Result := false;
  while ReadChar(ch) do begin
   { Maybe a more thorough CRLF/CR/LF handling is needed }
    if ch=#$000D then begin
     //ignore
    end else
    if ch=#$000A then
      break
    else
      ln := ln + ch;
    Result := true;
  end;
end;

function TStreamDecoder.ReadChar: WideChar;
begin
  if not ReadChar(Result) then
    Result := #0000;
end;

function TStreamDecoder.ReadLn: UnicodeString;
begin
  if not ReadLn(Result) then
    Result := '';
end;


{ Stream encoder }

constructor TStreamEncoder.Open(AStream: TStream; AEncoding: TEncoding;
  AOwnsStream: boolean = false);
begin
  inherited Create();
  FStream := AStream;
  FOwnsStream := AOwnsStream;
  FEncoding := AEncoding;
  FBufferSize := OUTBUFSZ;
end;

constructor TStreamEncoder.CreateNew(const AFilename: string; AEncoding: TEncoding);
var fs: TFileStream;
begin
  fs := TFileStream.Create(AFilename, fmCreate);
  Self.Open(fs,AEncoding,{OwnsStream=}true);
end;

constructor TStreamEncoder.Append(const AFilename: string; AEncoding: TEncoding);
var fs: TFileStream;
begin
  fs := TFileStream.Create(AFilename, fmCreate);
  fs.Seek(0,soFromEnd);
  Self.Open(fs,AEncoding,{OwnsStream=}true);
end;

destructor TStreamEncoder.Destroy;
begin
  Flush();
  if FOwnsStream then
    FreeAndNil(FStream);
  FreeAndNil(FEncoding);
  inherited;
end;

procedure TStreamEncoder.Flush;
begin
  if FBuffer<>'' then
    FEncoding.Write(FStream, FBuffer);
  FBuffer := '';
end;

procedure TStreamEncoder.WriteBom;
begin
  Flush;
  FEncoding.WriteBom(FStream);
end;

procedure TStreamEncoder.WriteChar(const ch: WideChar);
begin
  FBuffer := FBuffer + ch;
  if Length(FBuffer)>=FBufferSize then
    Flush;
end;

procedure TStreamEncoder.Write(const ln: UnicodeString);
begin
  FBuffer := FBuffer + ln;
  if Length(FBuffer)>=FBufferSize then
    Flush;
end;

procedure TStreamEncoder.WriteLn(const ln: UnicodeString);
begin
  Write(ln+#$000D+#$000A);
end;


{ Simple encodings }

function TAsciiEncoding.Read(AStream: TStream; MaxChars: integer): string;
var ac: AnsiChar;
begin
  Result := '';
  while (AStream.Read(ac,1)=1) and (MaxChars>0) do begin
    Result := Result + WideChar(ac);
    Dec(MaxChars);
  end;
end;

procedure TAsciiEncoding.Write(AStream: TStream; const AData: string);
var i: integer;
begin
  for i := 1 to Length(AData) do
    AStream.Write(AnsiChar(AData[i]), 1);
end;

class function TUTF8Encoding.GetBOM: TBytes;
begin
  Result := TBytes.Create($EF, $BB, $BF);
end;

function TUTF8Encoding.Read(AStream: TStream; MaxChars: integer): string;
var b1, b2, b3: byte;
  tmp: integer;
begin
  Result := '';
  while MaxChars>0 do begin
    if AStream.Read(b1,1)<>1 then exit;

    if (b1 and UTF8_MASK2)=UTF8_VALUE2 then begin
      if AStream.Read(b2,1)<>1 then exit;
      Result := Result + WideChar(((b1 and $1f) shl 6) or (b2 and $3f))
    end else
    if (b1 and UTF8_MASK3)=UTF8_VALUE3 then begin
      if (AStream.Read(b2,1)<>1)
      or (AStream.Read(b3,1)<>1) then exit;
      Result := Result + WideChar(((b1 and $0f) shl 12) or ((b2 and $3f) shl 6) or (b3 and $3f));
    end else
    if (b1 and UTF8_MASK4)=UTF8_VALUE4 then
    begin
      AStream.Read(tmp, 3); //and ignore
     { TODO: Decode and return a surrogate pair }
    end else
      Result := Result + WideChar(b1);

    Dec(MaxChars);
  end;
end;

procedure TUTF8Encoding.Write(AStream: TStream; const AData: string);
var i: integer;
  w: word;
begin
  for i := 1 to Length(AData) do begin
    w := Word(AData[i]);
    if (w and UTF8_WRITE1)=0 then
      _fwrite1(AStream, w mod 256)
    else
    if (w and UTF8_WRITE2)=0 then
      _fwrite2(AStream,
          (UTF8_VALUE2 or (w shr 6))
        + (UTF8_VALUEC or (w and $3f)) shl 8
      )
    else
      _fwrite3(AStream,
          (UTF8_VALUE3 or (w shr 12))
        + (UTF8_VALUEC or ((w shr 6) and $3f)) shl 8
        + (UTF8_VALUEC or (w and $3f)) shl 16
      );
  end;
end;

class function TUTF16LEEncoding.GetBOM: TBytes;
begin
  Result := TBytes.Create($FF, $FE);
end;

function TUTF16LEEncoding.Read(AStream: TStream; MaxChars: integer): string;
var read_sz: integer;
begin
  SetLength(Result, MaxChars);
  read_sz := AStream.Read(Result[1], MaxChars*SizeOf(WideChar));
  if read_sz<MaxChars*SizeOf(WideChar) then
    SetLength(Result, read_sz div SizeOf(WideChar));
end;

procedure TUTF16LEEncoding.Write(AStream: TStream; const AData: string);
begin
  if AData<>'' then
    AStream.Write(AData[1], Length(AData)*SizeOf(WideChar));
end;

class function TUTF16BEEncoding.GetBOM: TBytes;
begin
  Result := TBytes.Create($FE, $FF);
end;

function TUTF16BEEncoding.Read(AStream: TStream; MaxChars: integer): string;
var read_sz: integer;
  i: integer;
begin
  SetLength(Result, MaxChars);
  read_sz := AStream.Read(Result[1], MaxChars*SizeOf(WideChar));
  if read_sz<MaxChars*SizeOf(WideChar) then
    SetLength(Result, read_sz div SizeOf(WideChar));
  for i := 1 to Length(Result) do
    Result[i] := WideChar(_swapw(Word(Result[i])));
end;

procedure TUTF16BEEncoding.Write(AStream: TStream; const AData: string);
var i: integer;
begin
  for i := 1 to Length(AData) do
    _fwrite2(AStream, _swapw(Word(AData[i])));
end;


{ Wakan encodings }

const IS_EUC=1;
      IS_HALFKATA=2;
      IS_SJIS1=3;
      IS_SJIS2=4;
      IS_MARU=5;
      IS_NIGORI=6;
      IS_JIS=7;
      JIS_NL=10;          // New Line char.
      JIS_CR=13;         // Carrage Return.
      JIS_ESC=27;          // Escape.
      JIS_SS2=142;         // Half-width katakana marker.

type TUTFArray=array[0..3] of byte;

function SJIS2JIS(w:word):word;
var b1,b2,adjust, rowOffset, cellOffset:byte;
begin
  b1:=w div 256;
  b2:=w mod 256;
  if b2<159 then adjust:=1 else adjust:=0;
  if b1<160 then rowOffset:=112 else rowOffset:=176;
  if adjust=1 then begin if b2>127 then cellOffset:=32 else cellOffset:=31 end else cellOffset:=126;
  b1 := ((b1-rowOffset) shl 1) - adjust;
  b2 := b2-cellOffset;
  result:=b1*256+b2;
end;

function JIS2SJIS(w:word):word;
var b1,b2,b1n:byte;
begin
  b1:=w div 256;
  b2:=w mod 256;
  if b1 mod 2<>0 then begin if b2>95 then b2:=b2+32 else b2:=b2+31 end else b2:=b2+126;
  b1n:=((b1+1) shr 1);
  if b1<95 then b1n:=b1n+112 else b1n:=b1n+176;
  result:=b1n*256+b2;
end;

function JIS2Unicode(w:word):word;
begin
  result:=0;//default case
  case w of
    $0000..$007e:result:=w; // ascii
    $0080..$00ff:result:=Table_ExtASCII[w-128];
    $2330..$237a:result:=w-$2330+$ff10; // japanese ASCII
    $2421..$2473:result:=w-$2421+$3041; // hiragana
    $2521..$2576:result:=w-$2521+$30a1; // katakana
    $2621..$2658:if w<=$2631 then result:=w-$2621+$0391 else
                 if w<=$2638 then result:=w-$2621+$0392 else
                 if w< $2641 then result:=0 else
                 if w<=$2651 then result:=w-$2621+$0391 else
                 if w<=$2658 then result:=w-$2621+$0392; // greek
    $2721..$2771:if w<=$2726 then result:=w-$2721+$0410 else
                 if w= $2727 then result:=$0401 else
                 if w<=$2741 then result:=w-$2722+$0410 else
                 if w< $2751 then result:=0 else
                 if w<=$2756 then result:=w-$2751+$0430 else
                 if w= $2757 then result:=$0451 else
                 if w<=$2771 then result:=w-$2752+$0430; // cyrillic
    $3021..$7426:if ((w and $7f)<$21) or ((w and $7f)>$7e) then result:=0 else
                   result:=Table_Kanji[((w-$3021) div 256)*94+((w-$3021) mod 256)]; // kanji
    $2121..$217e:result:=Table_Misc[w-$2121];
    $2221..$227e:result:=Table_Misc[w-$2221+94];
    $2821..$2840:result:=Table_Misc[w-$2821+94+94];
  end;
end;

function Unicode2UTF(ch:word):TUTFArray;
begin
  if (ch and UTF8_WRITE1)=0 then
  begin
    result[0]:=1;
    result[1]:=ch;
    exit;
  end;
  if (ch and UTF8_WRITE2)=0 then
  begin
    result[0]:=2;
    result[1]:=(UTF8_VALUE2 or (ch shr 6));
    result[2]:=(UTF8_VALUEC or (ch and $3f));
  end;
  result[0]:=3;
  result[1]:=UTF8_VALUE3 or (ch shr 12);
  result[2]:=UTF8_VALUEC or ((ch shr 6) and $3f);
  result[3]:=UTF8_VALUEC or (ch and $3f);
end;

function Unicode2JIS(w:word):word;
var i:integer;
begin
  result:=0;
  case w of
    $0000..$007e:result:=w; // Ascii
    $3041..$3093:result:=w-$3041+$2421; // Hiragana
    $30a1..$30f6:result:=w-$30a1+$2521; // Katakana
    $0391..$03c9:if w<=$03a1 then result:=w-$0391+$2621 else
                 if w= $03a2 then result:=0 else
                 if w<=$03a9 then result:=w-$0392+$2621 else
                 if w< $03b1 then result:=0 else
                 if w<=$03c1 then result:=w-$0391+$2621 else
                 if w= $03c2 then result:=0 else
                 if w<=$03c9 then result:=w-$0392+$2621; // greek
    $0401       :result:=$2727;
    $0451       :result:=$2757;
    $0410..$044f:if w<=$0415 then result:=w-$0410+$2721 else
                 if w<=$042f then result:=w-$0416+$2728 else
                 if w<=$0435 then result:=w-$0430+$2751 else
                 if w<=$044f then result:=w-$0436+$2758; // cyrillic
    $ff10..$ff5a:result:=w-$ff10+$2330;
    $feff       :result:=w;
    $fffe       :result:=w;
  end;
  if result<>0 then exit;
  for i:=0 to NUMBER_KANJIUNICODE-1 do if Table_Kanji[i]=w then
  begin
    result:=i div 94;
    result:=(((result+$30) shl 8) or (i-result*94)+$21);
    exit;
  end;
  for i:=0 to NUMBER_MISCUNICODE-1 do if Table_Misc[i]=w then
  begin
    case i div 94 of
      0:result:=$2121+i;
      1:result:=$2221+i-94;
      2:result:=$2821+i-94-94;
    end;
    exit;
  end;
  for i:=0 to NUMBER_EXTUNICODE-1 do if Table_ExtASCII[i]=w then
  begin
    result:=i+$80;
    exit;
  end;
  result:=0;
end;

function UTF2Unicode(b1,b2,b3,b4:byte;var inc:byte):word;
begin
  if (b1 and UTF8_MASK1)=UTF8_VALUE1 then
  begin
    result:=b1;
    inc:=1;
    exit;
  end else if (b1 and UTF8_MASK2)=UTF8_VALUE2 then
  begin
    result:=((b1 and $1f) shl 6) or (b2 and $3f);
    inc:=2;
    exit;
  end else if (b1 and UTF8_MASK3)=UTF8_VALUE3 then
  begin
    result:=((b1 and $0f) shl 12) or ((b2 and $3f) shl 6) or (b3 and $3f);
    inc:=3;
    exit;
  end else if (b1 and UTF8_MASK4)=UTF8_VALUE4 then
  begin
    result:=$ffff;
    inc:=4;
  end else begin
    Result:=b1; //because we don't know what else to do
    inc:=1;
  end;
end;

function _is(b:word;cl:byte):boolean;
begin
  case cl of
    IS_EUC:result:=(b>=161) and (b<=254);
    IS_HALFKATA:result:=(b>=161) and (b<=223);
    IS_SJIS1:result:=((b>=129) and (b<=159)) or ((b>=224) and (b<=239));
    IS_SJIS2:result:=(b>=64) and (b<=252);
    IS_MARU:result:=(b>=202) and (b<=206);
    IS_NIGORI:result:=((b>=182) and (b<=196)) or ((b>=202) and (b<=206)) or (b=179);
    IS_JIS:result:=(b and $7f00)>0;
  else result:=false;
  end;
end;

function TEUCEncoding.Read(AStream: TStream; MaxChars: integer): string;
var b1, b2: byte;
begin
  Result := '';
  while MaxChars>0 do begin
    if AStream.Read(b1,1)<>1 then exit;

    if _is(b1,IS_EUC) then begin
      if AStream.Read(b2,1)<>1 then exit;
      Result := Result + WideChar(JIS2Unicode((b1*256+b2) and $7f7f));
    end else
      Result := Result + WideChar(b1);

    Dec(MaxChars);
  end;
end;

procedure TEUCEncoding.Write(AStream: TStream; const AData: string);
var i: integer;
  w: word;
begin
  for i := 1 to Length(AData) do begin
    w := Unicode2JIS(Word(AData[i]));
    if _is(w,IS_JIS) then
      _fwrite2(AStream,
          ((w shr 8) or $80)
        + ((w mod 256) or $80) shl 8
      )
    else if (w and $80)>0 then
      _fwrite2(AStream,
           JIS_SS2
        + (w and $7f) shl 8
      )
    else
      _fwrite1(AStream, w);
  end;
end;

function TSJISEncoding.Read(AStream: TStream; MaxChars: integer): string;
var b1, b2: byte;
begin
  Result := '';
  while MaxChars>0 do begin
    if AStream.Read(b1,1)<>1 then exit;

    if _is(b1,IS_SJIS1) then begin
      if AStream.Read(b2,1)<>1 then exit;
      if _is(b2,IS_SJIS2) then
        Result := Result + WideChar(JIS2Unicode(SJIS2JIS(b1*256+b2)))
      else
        Result := Result + WideChar(JIS2Unicode(b1*256+b2));
    end else
      Result := Result + WideChar(b1);

    Dec(MaxChars);
  end;
end;

procedure TSJISEncoding.Write(AStream: TStream; const AData: string);
var i: integer;
  w: word;
begin
  for i := 1 to Length(AData) do begin
    w:=Unicode2JIS(Word(AData[i]));
    if _is(w,IS_JIS) then
    begin
      w:=jis2sjis(w);
      _fwrite2(AStream, _swapw(w));
    end else
      _fwrite1(AStream, w);
  end;
end;

function TBaseJISEncoding.Read(AStream: TStream; MaxChars: integer): string;
var b1, b2: byte;
  inp_intwobyte: boolean;
begin
  Result := '';
  inp_intwobyte := false;
  while true do begin
    if AStream.Read(b1,1)<>1 then exit;

    if b1=JIS_ESC then
    begin
      if AStream.Read(b2,1)<>1 then exit;
      if (b2=ord('$')) or (b2=ord('(')) then AStream.Read(b1,1); //don't care about the result
      if (b2=ord('K')) or (b2=ord('$')) then inp_intwobyte:=true else inp_intwobyte:=false;
     //Do not exit, continue to the next char
    end else begin
      if (b1=JIS_NL) or (b1=JIS_CR) then
        Result := Result + WideChar(b1)
      else begin
        if AStream.Read(b2,1)<>1 then exit;
        if inp_intwobyte then
          Result := Result + WideChar(JIS2Unicode(b1*256+b2))
        else
          Result := Result + WideChar(b1);
      end;

      Dec(MaxChars);
      if MaxChars<=0 then break;
    end;

  end;
end;

procedure TBaseJISEncoding.Write(AStream: TStream; const AData: string);
var i: integer;
  w: word;
begin
  for i := 1 to Length(AData) do begin
    w := Word(AData[i]);

    if (w=13) or (w=10) then
    begin
      _fputend(AStream);
      _fwrite1(AStream, w);
    end else
    begin
      w:=Unicode2JIS(w);
      if _is(w,IS_JIS) then
      begin
        _fputstart(AStream);
        _fwrite2(AStream, _swapw(w));
      end else begin
        _fputend(AStream);
        _fwrite1(AStream, w);
      end;
    end;
  end;
end;

procedure TBaseJISEncoding._fputstart(AStream: TStream);
begin
  if intwobyte then exit;
  intwobyte:=true;
  AStream.Write(StartMark[0], Length(StartMark));
end;

procedure TBaseJISEncoding._fputend(AStream: TStream);
begin
  if not intwobyte then exit;
  intwobyte:=false;
  AStream.Write(EndMark[0], Length(EndMark));
end;

constructor TJISEncoding.Create;
begin
  inherited;
  StartMark := TBytes.Create(JIS_ESC, ord('B'), ord('$'));
  EndMark := TBytes.Create(JIS_ESC, ord('('), ord('J'));
end;

constructor TOldJISEncoding.Create;
begin
  inherited;
  StartMark := TBytes.Create(JIS_ESC, ord('@'), ord('$'));
  EndMark := TBytes.Create(JIS_ESC, ord('('), ord('J'));
end;

constructor TNECJISEncoding.Create;
begin
  inherited;
  StartMark := TBytes.Create(JIS_ESC, ord('K'));
  EndMark := TBytes.Create(JIS_ESC, ord('H'));
end;

function TGBEncoding.Read(AStream: TStream; MaxChars: integer): string;
var b1, b2: byte;
begin
  Result := '';
  while MaxChars>0 do begin
    if AStream.Read(b1,1)<>1 then exit;

    if (b1>=$a1) and (b1<=$fe) then
    begin
      if AStream.Read(b2,1)<>1 then exit;
      if (b2>=$a1) and (b2<=$fe) then
        Result := Result + WideChar(Table_GB[(b1-$a0)*96+(b2-$a0)])
      else
        Result := Result + WideChar(b1*256+b2);
    end else
      Result := Result + WideChar(b1);

    Dec(MaxChars);
  end;
end;

procedure TGBEncoding.Write(AStream: TStream; const AData: string);
var i,j: integer;
  w: word;
begin
  for j := 1 to Length(AData) do begin
    w := Word(AData[j]);
    if w<128 then
      _fwrite1(AStream, byte(w))
    else
    begin
      for i:=0 to 96*96-1 do if Table_GB[i]=w then begin
        _fwrite2(AStream,
            (i mod 96+$a0) shl 8
          + (i div 96+$a0)
        );
        exit;
      end;
      _fwrite2(AStream,
        (w mod 256) shl 8
        + (w div 256)
      );
    end;
  end;
end;

function TBIG5Encoding.Read(AStream: TStream; MaxChars: integer): string;
var b1, b2: byte;
begin
  Result := '';
  while MaxChars>0 do begin
    if AStream.Read(b1,1)<>1 then exit;

    if (b1>=$a1) and (b1<=$fe) then
    begin
      if AStream.Read(b2,1)<>1 then exit;
      if (b2>=$40) and (b2<=$7f) then
        Result := Result + WideChar(Table_Big5[(b1-$a0)*160+(b2-$40)])
      else
      if (b2>=$a1) and (b2<=$fe) then
        Result := Result + WideChar(Table_Big5[(b1-$a0)*160+(b2-$a0)])
      else
        Result := Result + WideChar(b1*256+b2);
    end else
      Result := Result + WideChar(b1);

    Dec(MaxChars);
  end;
end;

procedure TBIG5Encoding.Write(AStream: TStream; const AData: string);
var i, j: integer;
  w: word;
begin
  for j := 1 to Length(AData) do begin
    w := Word(AData[j]);
    if w<128 then
      _fwrite1(AStream, byte(w))
    else
    begin
      for i:=0 to 96*160-1 do if Table_GB[i]=w then begin
        _fwrite2(AStream,
            (i mod 96+$a0) shl 8
          + (i div 96+$a0)
        );
        exit;
      end;
      _fwrite2(AStream, _swapw(w));
    end;
  end;
end;

(*

constructor TRtlDecoder.Open(AStream: TStream; AOwnsStream: boolean;
  AEncoding: TEncoding; AOwnsEncoding: boolean);
begin
  inherited Open(AStream, AOwnsStream);
  FEncoding := TEncoding;
  FOwnsEncoding := AOwnsEncoding;
  bpos := 0;
  bsz := 0;
end;

constructor TRtlDecoder.Open(AStream: TStream; AOwnsStream: boolean;
  ACodePage: integer);
begin
  Open(AStream, AOwnsStream, TEncoding.GetEncoding(ACodePage), {OwnsEncoding=}true);
end;

constructor TRtlDecoder.Open(AStream: TStream; AOwnsStream: boolean;
  const AEncodingName: string);
begin
  Open(AStream, AOwnsStream, TEncoding.GetEncoding(AEncodingName), {OwnsEncoding=}true);
end;

constructor TRtlDecoder.Open(const AFilename: string; AEncoding: TEncoding;
  AOwnsEncoding: boolean = false);
var fs: TFileStream;
begin
  fs := TFileStream.Create(AFilename, fmOpenRead);
  Self.Open(fs, {OwnsStream=}true, AEncoding, AOwnsEncoding);
end;

constructor TRtlDecoder.Open(const AFilename: string; ACodePage: integer);
begin
  Open(AFilename, TEncoding.GetEncoding(ACodePage), {OwnsEncoding=}true);
end;

constructor TRtlDecoder.Open(const AFilename: string; const AEncodingName: string);
begin
  Open(AFilename, TEncoding.GetEncoding(AEncodingName), {OwnsEncoding=}true);
end;

destructor TRtlDecoder.Destroy;
begin
  if FOwnsEncoding then
    FreeAndNil(FEncoding);
  inherited;
end;

function TRtlDecoder.ReadChar(out ch: WideChar): boolean;
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
*)

function Conv_DetectType(AStream: TStream): CEncoding;
begin
  if not Conv_DetectType(AStream, Result) then
    Result := nil;
end;

{ Detects file encoding. Returns true if it's truly detected (i.e. through BOM),
 or false if it's a best guess. }
function Conv_DetectType(AStream: TStream; out AEncoding: CEncoding): boolean;
var i,b,j:integer;
    eucsjis:boolean;
    asciionly:boolean;
begin
  AStream.Seek(0, soBeginning);
  AEncoding := nil;
  Result := false;

  if TUTF16LEEncoding.ReadBom(AStream) then begin
    AEncoding := TUTF16LEEncoding;
    Result := true;
    exit;
  end;

  if TUTF16BEEncoding.ReadBom(AStream) then begin
    AEncoding := TUTF16BEEncoding;
    Result := true;
    exit;
  end;

  if TUTF8Encoding.ReadBom(AStream) then begin
    AEncoding := TUTF8Encoding;
    Result := true;
    exit;
  end;

 (*
   TODO: Move to WakanText.pas

    if i=$f1ff then begin
      tp:=FILETYPE_WTT;
      Result:=true;
      exit;
    end;

 *)

  AEncoding:=TUTF16LEEncoding;
  eucsjis:=true;
  i := 0; //zero higher bytes
  while (AStream.Read(i, 2)=2) and (AEncoding=TUTF16LEEncoding) do
  begin
    if Unicode2JIS(i)=0 then AEncoding:=nil;
    if (i and $8080)<>$8080 then eucsjis:=false;
  end;
  if eucsjis then AEncoding:=nil;
  if AEncoding<>nil then exit;

  AStream.Seek(0, soBeginning);
  asciionly:=true;
  AEncoding := TUTF8Encoding;
  i := 0; //zero higher bytes
  while (AStream.Read(i, 1)=1) and (AEncoding=TUTF8Encoding) do
  begin
    b:=0;
    if (i and UTF8_MASK1)=UTF8_VALUE1 then b:=0 else
    if (i and UTF8_MASK2)=UTF8_VALUE2 then b:=1 else
    if (i and UTF8_MASK3)=UTF8_VALUE3 then b:=2 else
    if (i and UTF8_MASK4)=UTF8_VALUE4 then b:=3 else AEncoding:=nil;
    if b>0 then asciionly:=false;
    for j:=0 to b-1 do
    begin
      if AStream.Read(i, 1)=1 then //else do not drop the encoding, tolerate missing bytes, stream might have been cut short
        if (i and $c0)<>$80 then AEncoding:=nil;
    end;
  end;
  if asciionly then AEncoding:=nil;
  if AEncoding<>nil then exit;


  AStream.Seek(0, soBeginning);
  AEncoding := TAsciiEncoding;
  eucsjis:=false;
  i:=0;
  while (AEncoding = TAsciiEncoding) or ((AEncoding=nil) and eucsjis) do
  begin
    if AStream.Read(i, 1)<>1 then break;

    if i=JIS_ESC then
    begin
      if AStream.Read(i, 1)<>1 then i:=-1;
      if i=ord('$') then
      begin
        if AStream.Read(i, 1)<>1 then i:=-1;
        if i=ord('B') then AEncoding:=TJISEncoding;
        if i=ord('@') then AEncoding:=TOldJISEncoding;
      end else
      if i=ord('K') then AEncoding:=TNECJISEncoding;
    end else if i=JIS_SS2 then
    begin
      if AStream.Read(i, 1)<>1 then i:=-1;
      if (i>=161) and (i<=223) then begin
        AEncoding:=nil;
        eucsjis:=true;
      end
      else if (i<>127) and (i>=64) and (i<=252) then AEncoding:=TSJISEncoding;
    end else if (i>=129) and (i<=159) then AEncoding:=TSJISEncoding
    else if (i>=161) and (i<=223) then
    begin
      if AStream.Read(i, 1)<>1 then i:=-1;
      if (i>=240) and (i<=254) then AEncoding:=TEUCEncoding
      else if (i>=161) and (i<=223) then begin
        AEncoding:=nil;
        eucsjis:=true;
      end
      else if (i>=224) and (i<=239) then
      begin
        AEncoding:=nil;
        eucsjis:=true;
        while ((i>=64) and (AEncoding=nil) and eucsjis) do
        begin
          if i>=129 then
          begin
            if (i<=141) or ((i>=143) and (i<=159)) then AEncoding:=TSJISEncoding else
            if (i>=253) and (i<=254) then AEncoding:=TEUCEncoding;
          end;
          if AStream.Read(i, 1)<>1 then break;
        end;
      end else if i<=159 then AEncoding:=TSJISEncoding;
    end else if (i>=240) and (i<=254) then AEncoding:=TEUCEncoding
    else if (i>=224) and (i<=239) then
    begin
      if AStream.Read(i, 1)<>1 then i:=-1;
      if ((i>=64) and (i<=126)) or ((i>=128) and (i<=160)) then AEncoding:=TSJISEncoding
      else if (i>=253) and (i<=254) then AEncoding:=TEUCEncoding
      else if (i>=161) and (i<=252) then begin
        AEncoding:=nil;
        eucsjis:=true;
      end;
    end;
  end;

  if (AEncoding=nil) and eucsjis then
    AEncoding:=TSJISEncoding;
end;

function Conv_DetectType(const AFilename: string): CEncoding;
var fsr: TStreamReader;
begin
  fsr := TStreamReader.Create(
    TFileStream.Create(AFilename, fmOpenRead), true);
  try
    Result := Conv_DetectType(fsr);
  finally
    FreeAndNil(fsr);
  end;
end;

function Conv_DetectType(const AFilename: string; out AEncoding: CEncoding): boolean;
var fsr: TStreamReader;
begin
  fsr := TStreamReader.Create(
    TFileStream.Create(AFilename, fmOpenRead), true);
  try
    Result := Conv_DetectType(fsr, AEncoding);
  finally
    FreeAndNil(fsr);
  end;
end;

function Conv_ChooseType(AChinese:boolean; ADefault: CEncoding): CEncoding;
var fFileType: TfFileType;
begin
  Result := nil;
  fFileType := TfFileType.Create(nil);
  try
    if AChinese then
    begin
      fFileType.rgType.Items.Clear;
      fFileType.rgType.Items.Add('Unicode (UCS2)');
      fFileType.rgType.Items.Add('UTF-8');
      fFileType.rgType.Items.Add('Big5');
      fFileType.rgType.Items.Add('GB2312');
      fFileType.rgType.Items.Add('Unicode (UCS2) reversed bytes');
      if ADefault=TUTF16LEEncoding then
        fFileType.rgType.ItemIndex:=0
      else
      if ADefault=TUTF8Encoding then
        fFileType.rgType.ItemIndex:=1
      else
      if ADefault=TBIG5Encoding then
        fFileType.rgType.ItemIndex:=2
      else
      if ADefault=TGBEncoding then
        fFileType.rgType.ItemIndex:=3
      else
      if ADefault=TUTF16BEEncoding then
        fFileType.rgType.ItemIndex:=4
      else
        fFileType.rgType.ItemIndex:=0;
      if fFileType.ShowModal=mrOK then
      case fFileType.rgType.ItemIndex of
        0: Result:=TUTF16LEEncoding;
        1: Result:=TUTF8Encoding;
        2: Result:=TBIG5Encoding;
        3: Result:=TUTF16BEEncoding;
        4: Result:=TUTF16BEEncoding;
      end else Result:=nil;
    end else
    begin
      fFileType.rgType.Items.Clear;
      fFileType.rgType.Items.Add('Unicode (UCS2)');
      fFileType.rgType.Items.Add('UTF-8');
      fFileType.rgType.Items.Add('Shift-JIS');
      fFileType.rgType.Items.Add('JIS');
      fFileType.rgType.Items.Add('Old JIS');
      fFileType.rgType.Items.Add('NEC JIS');
      fFileType.rgType.Items.Add('EUC');
      fFileType.rgType.Items.Add('Unicode (UCS2) reversed bytes');
      if ADefault=TUTF16LEEncoding then
        fFileType.rgType.ItemIndex:=0
      else
      if ADefault=TUTF8Encoding then
        fFileType.rgType.ItemIndex:=1
      else
      if ADefault=TSJISEncoding then
        fFileType.rgType.ItemIndex:=2
      else
      if ADefault=TJISEncoding then
        fFileType.rgType.ItemIndex:=3
      else
      if ADefault=TOldJISEncoding then
        fFileType.rgType.ItemIndex:=4
      else
      if ADefault=TNECJISEncoding then
        fFileType.rgType.ItemIndex:=5
      else
      if ADefault=TEUCEncoding then
        fFileType.rgType.ItemIndex:=6
      else
      if ADefault=TUTF16BEEncoding then
        fFileType.rgType.ItemIndex:=7
      else
        fFileType.rgType.ItemIndex:=0;
      if fFileType.ShowModal=mrOK then
      case fFileType.rgType.ItemIndex of
        0: Result:=TUTF16LEEncoding;
        1: Result:=TUTF8Encoding;
        2: Result:=TSJISEncoding;
        3: Result:=TJISEncoding;
        4: Result:=TOldJISEncoding;
        5: Result:=TNECJISEncoding;
        6: Result:=TEUCEncoding;
        7: Result:=TUTF16BEEncoding;
      end;
    end;
  finally
    FreeAndNil(fFileType);
  end;
end;

function OpenTextFile(const AFilename: string; AEncoding: CEncoding = nil): TStreamDecoder;
var fsr: TStreamReader;
begin
  fsr := TStreamReader.Create(
    TFileStream.Create(AFilename, fmOpenRead),
    {OwnsStream=}true
  );
  try
    if AEncoding=nil then
      if not Conv_DetectType(fsr, AEncoding) and (AEncoding=nil) {not even a best guess} then
        AEncoding := TAsciiEncoding;
    Result := TStreamDecoder.Open(fsr, AEncoding.Create, {OwnsStream=}true);
  except
    FreeAndNil(fsr);
    raise;
  end;
end;

function CreateTextFile(const AFilename: string; AEncoding: CEncoding): TStreamEncoder;
var fsr: TStreamReader;
begin
  fsr := TStreamReader.Create(
    TFileStream.Create(AFilename, fmCreate),
    {OwnsStream=}true
  );
  try
    Result := TStreamEncoder.Open(fsr, AEncoding.Create, {OwnsStream=}true);
  except
    FreeAndNil(fsr);
    raise;
  end;
end;

function AppendToTextFile(const AFilename: string; AEncoding: CEncoding = nil): TStreamEncoder;
var fsr: TStreamReader;
begin
  fsr := TStreamReader.Create(
    TFileStream.Create(AFilename, fmOpenReadWrite), //read is for encoding detection
    {OwnsStream=}true
  );
  try
    if AEncoding=nil then
      if not Conv_DetectType(fsr, AEncoding) and (AEncoding=nil) {not even a best guess} then
        AEncoding := TAsciiEncoding;
    fsr.Seek(0, soEnd);
    Result := TStreamEncoder.Open(fsr, AEncoding.Create, {OwnsStream=}true);
  except
    FreeAndNil(fsr);
    raise;
  end;
end;

{ Compatibility functions }

function AnsiFileReader(const AFilename: string): TStreamDecoder;
begin
  Result := OpenTextFile(AFilename, TAcpEncoding);
end;

function UnicodeFileReader(const AFilename: string): TStreamDecoder;
begin
  Result := OpenTextFile(AFilename, TUnicodeEncoding);
end;

function ConsoleReader(): TStreamDecoder;
begin
  Result := TStreamDecoder.Open(
    THandleStream.Create(GetStdHandle(STD_INPUT_HANDLE)),
    TUTF16LEEncoding.Create,
    {OwnsStream=}true
  );
end;

function UnicodeStreamReader(AStream: TStream; AOwnsStream: boolean = false): TStreamDecoder;
begin
  Result := TStreamDecoder.Open(AStream, TUnicodeEncoding.Create, AOwnsStream);
end;

function FileReader(const AFilename: string): TStreamDecoder;
begin
 {$IFDEF UNICODE}
  Result := UnicodeFileReader(AFilename);
 {$ELSE}
  Result := AnsiFileReader(AFilename);
 {$ENDIF}
end;

function AnsiFileWriter(const AFilename: string): TStreamEncoder;
begin
  Result := CreateTextFile(AFilename, TAcpEncoding);
end;

function UnicodeFileWriter(const AFilename: string): TStreamEncoder;
begin
  Result := CreateTextFile(AFilename, TUnicodeEncoding);
end;

function ConsoleWriter(): TStreamEncoder;
begin
  Result := TStreamEncoder.Open(
    THandleStream.Create(GetStdHandle(STD_OUTPUT_HANDLE)),
    TUTF16LEEncoding.Create,
    {OwnsStream=}true
  );
end;

function ConsoleUTF8Writer(): TStreamEncoder;
begin
  Result := TStreamEncoder.Open(
    THandleStream.Create(GetStdHandle(STD_OUTPUT_HANDLE)),
    TUTF8Encoding.Create,
    {OwnsStream=}true
  );
end;

function UnicodeStreamWriter(AStream: TStream; AOwnsStream: boolean = false): TStreamEncoder;
begin
  Result := TStreamEncoder.Open(AStream, TUnicodeEncoding.Create, AOwnsStream);
end;

function FileWriter(const AFilename: string): TStreamEncoder;
begin
 {$IFDEF UNICODE}
  Result := UnicodeFileWriter(AFilename);
 {$ELSE}
  Result := AnsiFileWriter(AFilename);
 {$ENDIF}
end;

end.
