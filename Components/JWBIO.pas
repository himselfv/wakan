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

4. Permanently storing encoding selection:

  WriteString(ChosenEncoding.ClassName);
  ChosenEncoding := FindEncoding(ReadString()).Create();
}

interface
uses SysUtils, Classes, JWBStrings;

{ TODO: Encodings with params?

  We have to choose between being able to parametrize encodings:
    enc := TEncoding.Create('cp1251');
  To do TRtlEncoding, as it requires passing an instance of TEncoding, which is
  impossible even with generics (and generics wouldn't let us specify a created
  TEncoding params anyway).

  And able to store encoding/decoding state, parts of surrogate pairs
    ch := enc.GetChar(stream); // -> Surrogate_part_1
    ch := enc.GetChar(stream); // -> Surrogate_part_2
  To add BOM support to single-base-multiple-codepages encodings such as
  TMultibyteEncoding.

  Over being able to characterize file encoding simply by the encoding class:
    if DetectEncoding('text.txt')=TUTF8Encoding then [...]
  To pass the class everywhere without much action:
    reader := TStreamReader.Create('text.txt', TUTF8Encoding)
  To run common routines (file type guessing, bom checking) without creating an
  instance of every encoding we want to check against.
  To auto-create detected encoding in a common way
  To register encodings by registering their class types (how do you register
  class+param_set?)

  In short, we have 3 options for TEncoding:
  1. Descriptor: class, Parser: class
  2. Descriptor: class, Parser: object
  3. Descriptor: object
  Where descriptor is something which contains a full set of properties which
  define encoding, and parser is something which is used on a particular text.

  We may also use a combination of approaches, e.g.:
  - Create all possible encodings at the start of the app
  - Use these for encoding properties/detection
  - Encodings are stateless
  - Encoding state is save in additional object, TEncodingState, which is returned
   by the encoding or passed to it and modified.
  But this would require us to have a state param for every encoding.
}

{ TODO: UTF16 surrogate pairs (LE and BE in different order) }
{ TODO: TRtlEncoding(SysUtils.TEncoding) }

{ TODO: Extended UTF16 LE/BE detection.
 Current algorithm may be unreliable and/or slow. There are some experiments
 hidden under GUESSUTF16 switch, but they suck.
 Check how it's done in other apps. }
//{$DEFINE GUESS_UTF16}

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
   { Implement *at least one* of Read/ReadChar.
    MaxChars is given in 2 byte positions. Surrogate pairs count as separate chars. }
    function Read(AStream: TStream; MaxChars: integer): string; virtual;
    function ReadChar(AStream: TStream; out AChar: WideChar): boolean; virtual;
   { Implement *at least one* of Write/WriteChar.
    Encoding descendants do not check the number of bytes written. If you care,
    wrap TStream and raise exceptions. }
    procedure Write(AStream: TStream; const AData: string); virtual;
    procedure WriteChar(AStream: TStream; AChar: WideChar); virtual;
  end;
  CEncoding = class of TEncoding;

 { Simple encodings }
  TAsciiEncoding = class(TEncoding)
    function Read(AStream: TStream; MaxChars: integer): string; override;
    procedure Write(AStream: TStream; const AData: string); override;
  end;

 {$IFDEF MSWINDOWS}
 { Encoding class based on MultiByteToWideChar/WideCharToMultiByte. Slowish but safe.
  As things are right now, it's better to use derived classes for actual encodings
  (e.g. TAcpEncoding) }
  TMultibyteEncoding = class(TEncoding)
  protected
    FCodepage: integer;
  public
    constructor Create(const ACodepage: integer); reintroduce; overload;
    function Read(AStream: TStream; MaxChars: integer): string; override;
    procedure Write(AStream: TStream; const AData: string); override;
  end;
  TAcpEncoding = class(TMultibyteEncoding)
    constructor Create; override;
  end;
 {$ENDIF}

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
    procedure Rewind(const ADontSkipBom: boolean = false);
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
    FBufferPos: integer;
    FStream: TStream;
    FOwnsStream: boolean;
    FEncoding: TEncoding;
    function GetBufferSize: integer;
    procedure SetBufferSize(const Value: integer);
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
    property BufferSize: integer read GetBufferSize write SetBufferSize;
  end;
  CStreamEncoder = class of TStreamDecoder;

{$IFDEF GUESS_UTF16}
function GuessUTF16(AStream: TStream; out AEncoding: CEncoding): boolean;
{$ENDIF}

function Conv_DetectType(AStream: TStream): CEncoding; overload;
function Conv_DetectType(AStream: TStream; out AEncoding: CEncoding): boolean; overload;
function Conv_DetectType(const AFilename: string): CEncoding; overload;
function Conv_DetectType(const AFilename: string; out AEncoding: CEncoding): boolean; overload;

function OpenStream(const AStream: TStream; AOwnsStream: boolean; AEncoding: CEncoding = nil): TStreamDecoder;
function OpenTextFile(const AFilename: string; AEncoding: CEncoding = nil): TStreamDecoder; inline;
function CreateTextFile(const AFilename: string; AEncoding: CEncoding): TStreamEncoder;
function AppendToTextFile(const AFilename: string; AEncoding: CEncoding = nil): TStreamEncoder;

//Finds a class by it's name. Good to store encoding selection in a permanent way.
function FindEncoding(const AClassName: string): CEncoding;

//Compares binary data in files
function CompareStreams(const AStream1, AStream2: TStream): boolean;
function CompareFiles(const AFilename1, AFilename2: string): boolean;

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
uses Windows, StreamUtils, JWBConvertTbl;

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

function TEncoding.Read(AStream: TStream; MaxChars: integer): string;
var pos: integer;
begin
  SetLength(Result, MaxChars);
  pos := 1;
  while MaxChars>0 do begin
    if not ReadChar(AStream, Result[pos]) then break;
    Inc(pos);
    Dec(MaxChars);
  end;
  if pos<Length(Result) then
    SetLength(Result, pos-1);
end;

function TEncoding.ReadChar(AStream: TStream; out AChar: WideChar): boolean;
var s: string;
begin
  s := Read(AStream, 1);
  Result := Length(s)>=1;
  if Result then AChar := s[1];
end;

procedure TEncoding.Write(AStream: TStream; const AData: string);
var i: integer;
begin
  for i := 1 to Length(AData) do
    WriteChar(AStream, AData[i]);
end;

procedure TEncoding.WriteChar(AStream: TStream; AChar: WideChar);
begin
  Write(AStream, AChar);
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

procedure TStreamDecoder.Rewind(const ADontSkipBom: boolean);
begin
  Stream.Seek(0, soBeginning);
  FBufferPos := Length(FBuffer)+1;
  FEOF := false;
  if not ADontSkipBom then
    TrySkipBom;
end;

{ True if there *already was* at least one ReadChar() which resulted in False,
 and no subsequent calls can be expected to succeed.
 Should not return true even if we're at EOF until someone tries to ReadChar()
 and finds it. }
function TStreamDecoder.EOF: boolean;
begin
  Result := FEOF;
end;

{ Reads another character from the stream or returns False to indicate that
 no more are available. }
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

{ Reads until next CRLF or the end of the stream.
 Last line always ends with the stream. If the file ends in CRLF, it'll be
 returned as a last, empty line.
 To reproduce the content when saving, always write last line without CRLF.
 As an exception, empty file is thought to have no lines. }
function TStreamDecoder.ReadLn(out ln: UnicodeString): boolean;
var ch: WideChar;
begin
  ln := '';
  Result := not EOF; //there may be no more characters, but at least we haven't found that out yet
  while ReadChar(ch) do begin
   { Maybe a more thorough CRLF/CR/LF handling is needed }
    if ch=#$000D then begin
     //ignore
    end else
    if ch=#$000A then
      break
    else
      ln := ln + ch;
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
  SetLength(FBuffer, OUTBUFSZ);
  FBufferPos := 0;
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

function TStreamEncoder.GetBufferSize: integer;
begin
  Result := Length(FBuffer);
end;

procedure TStreamEncoder.SetBufferSize(const Value: integer);
begin
  Flush;
  SetLength(FBuffer, Value);
end;

procedure TStreamEncoder.Flush;
begin
  if FBufferPos>=Length(FBuffer) then
    FEncoding.Write(FStream, FBuffer)
  else
  if FBufferPos>0 then
    FEncoding.Write(FStream, copy(FBuffer, 1, FBufferPos));
  FBufferPos := 0;
end;

procedure TStreamEncoder.WriteBom;
begin
  Flush;
  FEncoding.WriteBom(FStream);
end;

procedure TStreamEncoder.WriteChar(const ch: WideChar);
begin
  if Length(FBuffer)<=0 then
    FEncoding.WriteChar(FStream, ch)
  else begin
    Inc(FBufferPos);
    FBuffer[FBufferPos] := ch;
    if FBufferPos>=Length(FBuffer) then
      Flush();
  end;
end;

procedure TStreamEncoder.Write(const ln: UnicodeString);
var fst: integer;
begin
  if ln='' then exit;
  if Length(FBuffer)>0 then begin
   //Whatever fits into buffer
    fst := Length(FBuffer)-FBufferPos;
    if fst>Length(ln) then fst := Length(ln);

    Move(ln[1], FBuffer[FBufferPos+1], fst*SizeOf(WideChar));
    Inc(FBufferPos, fst);
    if FBufferPos>=Length(FBuffer) then
      Flush();

    Inc(fst);
  end else
    fst := 1;

 //Remainder
  if fst<Length(ln)+1 then
    FEncoding.Write(FStream, copy(ln, fst, MaxInt));
end;

{ Writes a line with CRLF in the end.
 Note that if you've read the lines with ReadLn, to reproduce the file content,
 last line has to be written without CRLF. See ReadLn. }
procedure TStreamEncoder.WriteLn(const ln: UnicodeString);
begin
  Write(ln+#$000D+#$000A);
end;


{ Simple encodings }

function TAsciiEncoding.Read(AStream: TStream; MaxChars: integer): string;
var pos: integer;
  ac: AnsiChar;
begin
  SetLength(Result, MaxChars);
  pos := 1;
  while (MaxChars>0) and (AStream.Read(ac,1)=1) do begin
    Result[pos] := WideChar(ac);
    Inc(pos);
    Dec(MaxChars);
  end;
  if pos<Length(Result) then
    SetLength(Result, pos-1);
end;

procedure TAsciiEncoding.Write(AStream: TStream; const AData: string);
var i: integer;
begin
  for i := 1 to Length(AData) do
    AStream.Write(AnsiChar(AData[i]), 1);
end;

{$IFDEF MSWINDOWS}
{ A note on parametrized encodings:
 They will work, but no with any special code which assumes one encoding == one type.
 You also cannot rely on comparisons like "if Encoding1==Encoding2". }
constructor TMultibyteEncoding.Create(const ACodepage: integer);
begin
  inherited Create;
  FCodepage := ACodepage;
end;

function TMultibyteEncoding.Read(AStream: TStream; MaxChars: integer): string;
var inp: array[0..4] of AnsiChar;
  i, pos: integer;
  found: boolean;
begin
  SetLength(Result, MaxChars);
  pos := 1;
  while MaxChars>0 do begin

   { Hold on to your seats guys, this is going to be slow!
    CP_* encodings on Windows can be multibyte and MultiByteToWideChar does not
    return the number of bytes it used (only the number of *resulting* chars).
    To know where to start next time we read data byte by byte, until there's
    enough bytes for MultiByteToWideChar to produce one char. }

    i := 0;
    found := false;
    while i<Length(inp) do begin
      if AStream.Read(inp[i],1)<>1 then break;

      if MultiByteToWideChar(CP_ACP, 0, @inp[0], i+1, @Result[pos], 1)>=1 then begin
        found := true;
        break;
      end;

      Inc(i);
    end;

    if not found then
      if i>=Length(inp) then begin
       { This was a byte we could not decode, so move to next byte and try again.
        Instead of putting '?' here, we could have just skipped Inc(pos)/Dec(maxchars),
        but it's important that we continue until we reach MaxChars of output or EOF }
        Result[pos] := '?';
        AStream.Seek(-i+1, soCurrent);
      end else begin
       { We've reached the end of file, break }
        if i>0 then AStream.Seek(-i, soCurrent);
        break;
      end;

    Inc(pos);
    Dec(MaxChars);
  end;

  if pos<Length(Result) then
    SetLength(Result, pos-1);
end;

procedure TMultibyteEncoding.Write(AStream: TStream; const AData: string);
const def: AnsiChar = '?';
var buf: AnsiString;
  written: integer;
begin
  if Length(AData)=0 then exit;
  SetLength(buf, Length(AData)*4); //there aren't any encodings with more than 4 byte chars
  written := WideCharToMultiByte(CP_ACP, 0, PWideChar(AData), Length(AData), PAnsiChar(buf), Length(buf), @def, nil);
  if written=0 then
    RaiseLastOsError();
  AStream.Write(buf[1], written);
end;

constructor TAcpEncoding.Create;
begin
  inherited Create();
  FCodepage := CP_ACP;
end;
{$ENDIF}

class function TUTF8Encoding.GetBOM: TBytes;
begin
  Result := TBytes.Create($EF, $BB, $BF);
end;

function TUTF8Encoding.Read(AStream: TStream; MaxChars: integer): string;
var b1, b2, b3: byte;
  tmp: integer;
  pos: integer;
begin
  SetLength(Result, MaxChars);
  pos := 1;
  while MaxChars>0 do begin
    if AStream.Read(b1,1)<>1 then break;

    if (b1 and UTF8_MASK2)=UTF8_VALUE2 then begin
      if AStream.Read(b2,1)<>1 then break;
      Result[pos] := WideChar(((b1 and $1f) shl 6) or (b2 and $3f))
    end else
    if (b1 and UTF8_MASK3)=UTF8_VALUE3 then begin
      if (AStream.Read(b2,1)<>1)
      or (AStream.Read(b3,1)<>1) then break;
      Result[pos] := WideChar(((b1 and $0f) shl 12) or ((b2 and $3f) shl 6) or (b3 and $3f));
    end else
    if (b1 and UTF8_MASK4)=UTF8_VALUE4 then
    begin
      AStream.Read(tmp, 3); //and ignore
     { TODO: Decode and return a surrogate pair }
    end else
      Result[pos] := WideChar(b1);

    Inc(pos);
    Dec(MaxChars);
  end;

  if pos<Length(Result) then
    SetLength(Result, pos-1);
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
  pos: integer;
begin
  SetLength(Result, MaxChars);
  pos := 1;
  while MaxChars>0 do begin
    if AStream.Read(b1,1)<>1 then break;

    if _is(b1,IS_EUC) then begin
      if AStream.Read(b2,1)<>1 then break;
      Result[pos] := WideChar(JIS2Unicode((b1*256+b2) and $7f7f));
    end else
      Result[pos] := WideChar(b1);

    Inc(pos);
    Dec(MaxChars);
  end;
  if pos<Length(Result) then
    SetLength(Result, pos-1);
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
  pos: integer;
begin
  SetLength(Result, MaxChars);
  pos := 1;
  while MaxChars>0 do begin
    if AStream.Read(b1,1)<>1 then break;

    if _is(b1,IS_SJIS1) then begin
      if AStream.Read(b2,1)<>1 then break;
      if _is(b2,IS_SJIS2) then
        Result[pos] := WideChar(JIS2Unicode(SJIS2JIS(b1*256+b2)))
      else
        Result[pos] := WideChar(JIS2Unicode(b1*256+b2));
    end else
      Result[pos] := WideChar(b1);

    Inc(pos);
    Dec(MaxChars);
  end;
  if pos<Length(Result) then
    SetLength(Result, pos-1);
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
  pos: integer;
begin
  SetLength(Result, MaxChars);
  pos := 1;
  inp_intwobyte := false;
  while true do begin
    if AStream.Read(b1,1)<>1 then break;

    if b1=JIS_ESC then
    begin
      if AStream.Read(b2,1)<>1 then break;
      if (b2=ord('$')) or (b2=ord('(')) then AStream.Read(b1,1); //don't care about the result
      if (b2=ord('K')) or (b2=ord('$')) then inp_intwobyte:=true else inp_intwobyte:=false;
     //Do not exit, continue to the next char
    end else begin
      if (b1=JIS_NL) or (b1=JIS_CR) then
        Result[pos] := WideChar(b1)
      else begin
        if AStream.Read(b2,1)<>1 then break;
        if inp_intwobyte then
          Result[pos] := WideChar(JIS2Unicode(b1*256+b2))
        else
          Result[pos] := WideChar(b1);
      end;

      Inc(pos);
      Dec(MaxChars);
      if MaxChars<=0 then break;
    end;
  end;
  if pos<Length(Result) then
    SetLength(Result, pos-1);
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
  pos: integer;
begin
  SetLength(Result, MaxChars);
  pos := 1;
  while MaxChars>0 do begin
    if AStream.Read(b1,1)<>1 then break;

    if (b1>=$a1) and (b1<=$fe) then
    begin
      if AStream.Read(b2,1)<>1 then break;
      if (b2>=$a1) and (b2<=$fe) then
        Result[pos] := WideChar(Table_GB[(b1-$a0)*96+(b2-$a0)])
      else
        Result[pos] := WideChar(b1*256+b2);
    end else
      Result[pos] := WideChar(b1);

    Inc(pos);
    Dec(MaxChars);
  end;
  if pos<Length(Result) then
    SetLength(Result, pos-1);
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
  pos: integer;
begin
  SetLength(Result, MaxChars);
  pos := 1;
  while MaxChars>0 do begin
    if AStream.Read(b1,1)<>1 then break;

    if (b1>=$a1) and (b1<=$fe) then
    begin
      if AStream.Read(b2,1)<>1 then break;
      if (b2>=$40) and (b2<=$7f) then
        Result[pos] := WideChar(Table_Big5[(b1-$a0)*160+(b2-$40)])
      else
      if (b2>=$a1) and (b2<=$fe) then
        Result[pos] := WideChar(Table_Big5[(b1-$a0)*160+(b2-$a0)])
      else
        Result[pos] := WideChar(b1*256+b2);
    end else
      Result[pos] := WideChar(b1);

    Inc(pos);
    Dec(MaxChars);
  end;
  if pos<Length(Result) then
    SetLength(Result, pos-1);
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
  w: word;
  eucsjis:boolean;
  asciionly:boolean;
  failed_le, failed_be: boolean;
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

 { UTF16LE/BE first try }
  failed_le:=false;
  failed_be:=false;
  eucsjis:=true;
  while (AStream.Read(w, 2)=2) and not (failed_be and failed_le) do
  begin
    if Unicode2JIS(w)=0 then failed_le:=true;
    if Unicode2JIS(_swapw(w))=0 then failed_be:=true;
    if (w and $8080)<>$8080 then eucsjis:=false;
  end;
  if eucsjis then
    AEncoding:=nil
  else
  if failed_be and not failed_le then
    AEncoding := TUTF16LEEncoding
  else
  if failed_le and not failed_be then
    AEncoding := TUTF16BEEncoding;
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

 {$IFDEF GUESS_UTF16}
  { UTF16 LE/BE second try }
  if AEncoding=nil then
    if GuessUTF16(AStream, AEncoding) and (AEncoding<>nil) then begin
      Result := true;
      exit;
    end;
 {$ENDIF}

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

{$IFDEF GUESS_UTF16}
{ Tries to guess the variety of UTF16 encoding used in the text heuristically.
  Return values:
    True, TUTF16LE --- "I think it may be UTF16LE"
    True, TUTF16BE --- "I think it may be UTF16BE"
    True, nil      --- "I don't think it's UTF16 at all"
    False          --- "I'm not sure" }
{ Methods used:
1. Count the number of leading and trailing surrogates, assuming it was LE or BE.
 If the guess is right, the number must be almost equal (spare for one surrogate
 being lost on the border of the sample block, or for somewhat broken text).
 Pros: If the number of surrogates is high enough (>4-5) AND their numbers are
   equal, this almost certainly means the guess is right.
 Cons: In most of the texts there are no surrogates at all, or sometimes one
   or two, which is too small because they can occur spontaneously in unrelated
   files.

2. Count the distribution of the values in the odd an in the even bytes in the
  text. Supposedly, higher position values will have just a bunch of highly
  popular values while lower position values will be distributed mostly randomly.
  Cons: It's not so simple in real life, and many EUC or JIS texts may trigger
  this heuristic to fire accidentally if you set the borderline checks low enough.
}
function GuessUTF16(AStream: TStream; out AEncoding: CEncoding): boolean;
var lead_le, lead_be,
  trail_le, trail_be: integer;
  evenb, oddb: array[0..255] of word;
  evenavg, oddavg: double;
  evencnt, oddcnt: integer;
  totalread: integer;
  failed_le, failed_be: boolean;
  i: integer;
  w: word;
begin
  lead_le := 0;
  lead_be := 0;
  trail_le := 0;
  trail_be := 0;

  for i := 0 to 255 do begin
    oddb[i] := 0;
    evenb[i] := 0;
  end;

 { Parse up to 32Kb of text }
  totalread := 0;
  AStream.Seek(0, soBeginning);
  while totalread < 32767 do begin
    if AStream.Read(w, 2)<>2 then break;

   { Build the distribution of leading and trailing byte values }
    Inc(evenb[w and $00FF]); //0, 2, 4...
    Inc(oddb[w shr 8]); //1, 3, 5...

   { Count the number of occurences of leading surrogates and trailing
    surrogates, looking through LE or BE glasses. }
    if (w>=$D800) and (w<=$DBFF) then Inc(lead_le);
    if (w>=$DC00) and (w<=$DFFF) then Inc(trail_le);
    w := _swapw(w);
    if (w>=$D800) and (w<=$DBFF) then Inc(lead_be);
    if (w>=$DC00) and (w<=$DFFF) then Inc(trail_be);

    Inc(totalread);
  end;

  failed_le := false;
  failed_be := false;

 { Test surrogates. The number of leading surrogates must match that of trailing. }

 { If either of the sum pairs is close to (0,0) we haven't gathered enough data
  to make a solid comparison. }
  if (lead_le<=2) and (trail_le<=2) then begin
    if Abs(lead_be-trail_be)>2 then
      failed_be := true;
   { Continue to the next test. }
  end else
  if (lead_be<=2) and (trail_be<=2) then begin
    if Abs(lead_le-trail_le)>2 then
      failed_le := true;
   { Continue to the next test. }
  end else
  if Abs(lead_le-trail_le)<=2 then
    if Abs(lead_be-trail_be)<=2 then begin
     { Both match, continue to the next test. }
    end else
    begin
     { BE fails, LE matches }
      AEncoding := TUTF16LEEncoding;
      Result := true;
      exit;
    end
  else
    if Abs(lead_be-trail_be)<=2 then begin
     { BE matches, LE fails }
      AEncoding := TUTF16BEEncoding;
      Result := true;
      exit;
    end
    else begin
     { Both invalid }
      AEncoding := nil;
      Result := true;
      exit;
    end;

 { Chars in text are usually grouped pretty tightly, i.e. kana is $30**,
  latin is $00** etc. Lower byte will be distributed evenly, while higher byte
  will have several spikes.
  We use this to figure which is the leading byte.
  One of the broadest cases, CJK United Ideographs covers $34-$4D which is only
  25 values for the higher byte out of 255 possible, less than 10%. }

 { This is pointless if we've had less than 255 characters of text }
{  if totalread<255 then begin
    AEncoding := nil;
    Result := false;
    exit;
  end;}

 { Calculate averages }
  evenavg := 0;
  oddavg := 0;
  for i := 0 to 255 do begin
    evenavg := evenavg + evenb[i];
    oddavg := oddavg + oddb[i];
  end;
  evenavg := evenavg / 255;
  oddavg := oddavg / 255;

 { Number of items higher than average }
  evencnt := 0;
  oddcnt := 0;
  for i := 0 to 255 do begin
    if evenb[i]>evenavg then Inc(evencnt);
    if oddb[i]>oddavg then Inc(oddcnt);
  end;

  if (evencnt>80) and (oddcnt>80) then begin
   //Both suck
    AEncoding := nil;
    Result := false;
    exit;
  end;

  if (evencnt<=30) and (oddcnt>30) then begin
    if failed_be then
      AEncoding := nil
    else
      AEncoding := TUTF16BEEncoding;
    Result := true;
    exit;
  end;

  if (evencnt>30) and (oddcnt<=30) then begin
    if failed_le then
      AEncoding := nil
    else
      AEncoding := TUTF16LEEncoding;
    Result := true;
    exit;
  end;

  Result := false;
end;
{$ENDIF}

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

function OpenStream(const AStream: TStream; AOwnsStream: boolean; AEncoding: CEncoding = nil): TStreamDecoder;
var fsr: TStreamReader;
begin
  fsr := TStreamReader.Create(AStream, AOwnsStream);
  try
    if AEncoding=nil then
      if not Conv_DetectType(fsr, AEncoding) and (AEncoding=nil) {not even a best guess} then
        AEncoding := TAsciiEncoding;
    fsr.Seek(0, soBeginning);
    Result := TStreamDecoder.Open(fsr, AEncoding.Create, {OwnsStream=}true);
  except
    FreeAndNil(fsr);
    raise;
  end;
end;

function OpenTextFile(const AFilename: string; AEncoding: CEncoding = nil): TStreamDecoder;
begin
  Result := OpenStream(
    TFileStream.Create(AFilename, fmOpenRead or fmShareDenyNone),
    {OwnsStream=}true
  );
end;

function CreateTextFile(const AFilename: string; AEncoding: CEncoding): TStreamEncoder;
var fsr: TStreamWriter;
begin
  fsr := TStreamWriter.Create(
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
var fsr: TStreamWriter;
begin
  fsr := TStreamWriter.Create(
    TFileStream.Create(AFilename, fmOpenReadWrite), //read is for encoding detection
    {OwnsStream=}true
  );
  try
    if AEncoding=nil then
     //Conv_DetectType by filename since TStreamWriter cannot read
      if not Conv_DetectType(AFilename, AEncoding) and (AEncoding=nil) {not even a best guess} then
        AEncoding := TAsciiEncoding;
    fsr.Seek(0, soEnd);
    Result := TStreamEncoder.Open(fsr, AEncoding.Create, {OwnsStream=}true);
  except
    FreeAndNil(fsr);
    raise;
  end;
end;


//Finds a class by it's name. Good to store encoding selection in a permanent way.
function FindEncoding(const AClassName: string): CEncoding;
begin
 //Stupid for now
  if AClassName='TAsciiEncoding' then
    Result := TAsciiEncoding
  else
 {$IFDEF MSWINDOWS}
  if AClassName='TAcpEncoding' then
    Result := TAcpEncoding
  else
 {$ENDIF}
  if AClassName='TUTF8Encoding' then
    Result := TUTF8Encoding
  else
  if AClassName='TUTF16LEEncoding' then
    Result := TUTF16LEEncoding
  else
  if AClassName='TUTF16BEEncoding' then
    Result := TUTF16BEEncoding
  else
  if AClassName='TEUCEncoding' then
    Result := TEUCEncoding
  else
  if AClassName='TSJISEncoding' then
    Result := TSJISEncoding
  else
  if AClassName='TJISEncoding' then
    Result := TJISEncoding
  else
  if AClassName='TOldJISEncoding' then
    Result := TOldJISEncoding
  else
  if AClassName='TNECJISEncoding' then
    Result := TNECJISEncoding
  else
  if AClassName='TGBEncoding' then
    Result := TGBEncoding
  else
  if AClassName='TBIG5Encoding' then
    Result := TBIG5Encoding
  else
    Result := nil;

end;


{ Compares binary data in streams }

function CompareStreams(const AStream1, AStream2: TStream): boolean;
var
  b1, b2: byte;
  r1, r2: boolean;
begin
 { Can be easily made somewhat faster by reading in dwords and comparing
  only the number of bytes read (e.g. case 1: 2: 3: 4: if (d1 & $000F) == etc.) }
  Result := true;
  while true do begin
    r1 := (AStream1.Read(b1,1)=1);
    r2 := (AStream2.Read(b2,1)=1);
    if r1 xor r2 then
      Result := false;
    if b1<>b2 then
      Result := false;
    if (not r1) or (not Result) then //not Result => diff; not r1 => both over
      break;
  end;
end;

function CompareFiles(const AFilename1, AFilename2: string): boolean;
var f1, f2: TStream;
begin
  f1 := nil;
  f2 := nil;
  try
    f1 := TStreamReader.Create(
      TFileStream.Create(AFilename1, fmOpenRead),
      true
    );
    f2 := TStreamReader.Create(
      TFileStream.Create(AFilename2, fmOpenRead),
      true
    );
    Result := CompareStreams(f1, f2);
  finally
    FreeAndNil(f2);
    FreeAndNil(f1);
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
 {$IFDEF MSWINDOWS}
    TUTF16Encoding.Create(),
 {$ELSE}
    TUTF8Encoding.Create(),
 {$ENDIF}
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
